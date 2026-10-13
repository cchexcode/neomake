use {
    crate::multiplex::tui::app::{
        App,
        LogStream,
        TaskState,
    },
    ratatui::{
        prelude::*,
        widgets::{
            Block,
            Borders,
            Clear,
            List,
            ListItem,
            ListState,
            Paragraph,
            Wrap,
        },
    },
};

pub fn render(f: &mut Frame, app: &mut App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Length(1), Constraint::Min(1), Constraint::Length(1)])
        .split(f.area());

    render_header(f, chunks[0], app);

    let body = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(40), Constraint::Percentage(60)])
        .split(chunks[1]);

    render_tree(f, body[0], app);
    render_logs(f, body[1], app);
    render_hotkeys(f, chunks[2]);

    if app.show_help {
        render_help(f);
    }
}

fn render_header(f: &mut Frame, area: Rect, app: &App) {
    let (done, total) = app.progress();
    let secs = app.elapsed().as_secs();
    let status = if app.quit_requested && !app.engine_finished {
        " STOPPING".to_owned()
    } else if app.aborted {
        " ABORTED".to_owned()
    } else if app.engine_finished {
        if app.failed == 0 {
            " DONE".to_owned()
        } else {
            format!(" DONE ({} failed)", app.failed)
        }
    } else {
        " RUNNING".to_owned()
    };
    let line = Line::from(vec![
        Span::styled(
            " neomake · multiplex ",
            Style::default().bg(Color::DarkGray).fg(Color::White),
        ),
        Span::raw(format!(" {}/{} tasks · {}s", done, total, secs)),
        Span::styled(status, Style::default().fg(status_color(app))),
    ]);
    f.render_widget(Paragraph::new(line), area);
}

fn status_color(app: &App) -> Color {
    if app.aborted || app.failed > 0 {
        Color::Red
    } else if app.engine_finished {
        Color::Green
    } else {
        Color::Yellow
    }
}

fn render_tree(f: &mut Frame, area: Rect, app: &mut App) {
    let items: Vec<ListItem> = app
        .order
        .iter()
        .map(|id| {
            let t = &app.tasks[id];
            let (glyph, style) = match t.state {
                | TaskState::Pending => ("●", Style::default().fg(Color::DarkGray)),
                | TaskState::Running => ("◐", Style::default().fg(Color::Yellow)),
                | TaskState::Success => ("✓", Style::default().fg(Color::Green)),
                | TaskState::Failed => ("✗", Style::default().fg(Color::Red)),
            };
            let duration = t.duration_ms.map(|ms| format!(" {}ms", ms)).unwrap_or_default();
            let label = short_cmd(&t.info.command);
            ListItem::new(Line::from(vec![
                Span::styled(format!("{:>3} ", id), Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{} ", glyph), style),
                Span::raw(label),
                Span::styled(duration, Style::default().fg(Color::DarkGray)),
            ]))
        })
        .collect();

    let list = List::new(items)
        .block(Block::default().borders(Borders::ALL).title("Tasks"))
        .highlight_style(Style::default().bg(Color::DarkGray))
        .highlight_symbol("▌");

    let mut state = ListState::default();
    state.select(Some(app.cursor));
    f.render_stateful_widget(list, area, &mut state);
}

fn render_logs(f: &mut Frame, area: Rect, app: &App) {
    let title = match app.selected_id() {
        | Some(id) => {
            let t = &app.tasks[&id];
            let mut s = format!("logs: [{}] {}", id, short_cmd(&t.info.command));
            if app.follow {
                s.push_str(" [follow]");
            }
            s
        },
        | None => "logs".to_owned(),
    };
    let block = Block::default().borders(Borders::ALL).title(title);

    let lines: Vec<Line> = match app.selected_id() {
        | Some(id) => {
            let t = &app.tasks[&id];
            let mut out = Vec::<Line>::new();
            out.push(Line::from(vec![
                Span::styled("$ ", Style::default().fg(Color::Cyan)),
                Span::raw(t.info.command.clone()),
            ]));
            for l in &t.logs {
                let style = match l.stream {
                    | LogStream::Stdout => Style::default(),
                    | LogStream::Stderr => Style::default().fg(Color::Red),
                };
                out.push(Line::from(Span::styled(l.text.clone(), style)));
            }
            if let Some(msg) = &t.failure {
                out.push(Line::from(Span::styled(
                    format!("! {}", msg),
                    Style::default().fg(Color::Red),
                )));
            }
            if t.logs.is_empty() && t.failure.is_none() && !matches!(t.state, TaskState::Running) {
                out.push(Line::from(Span::styled(
                    "(no output)",
                    Style::default().fg(Color::DarkGray),
                )));
            }
            out
        },
        | None => vec![Line::from("(no task selected)")],
    };

    let inner_height = area.height.saturating_sub(2) as usize;
    let max_scroll = lines.len().saturating_sub(inner_height) as u16;
    let scroll = if app.log_scroll == u16::MAX {
        max_scroll
    } else {
        app.log_scroll.min(max_scroll)
    };

    let p = Paragraph::new(lines)
        .block(block)
        .wrap(Wrap { trim: false })
        .scroll((scroll, 0));
    f.render_widget(p, area);
}

fn render_hotkeys(f: &mut Frame, area: Rect) {
    let line = Line::from(vec![
        Span::styled(" j/k ", Style::default().fg(Color::Cyan)),
        Span::raw("nav "),
        Span::styled("x/X ", Style::default().fg(Color::Cyan)),
        Span::raw("stop "),
        Span::styled("Ctrl+d/u ", Style::default().fg(Color::Cyan)),
        Span::raw("page "),
        Span::styled("f ", Style::default().fg(Color::Cyan)),
        Span::raw("follow "),
        Span::styled("? ", Style::default().fg(Color::Cyan)),
        Span::raw("help "),
        Span::styled("q ", Style::default().fg(Color::Cyan)),
        Span::raw("quit "),
    ]);
    f.render_widget(Paragraph::new(line), area);
}

fn render_help(f: &mut Frame) {
    let area = centered_rect(72, 70, f.area());
    let lines = vec![
        Line::from("Navigation"),
        Line::from("  j / k, Down / Up      move cursor"),
        Line::from("  g / G, Home / End     jump to top / bottom of logs"),
        Line::from("  Ctrl+d / Ctrl+u       page logs"),
        Line::from("  f                     toggle follow mode"),
        Line::from(""),
        Line::from("Execution"),
        Line::from("  x                     hard-stop selected task"),
        Line::from("  X                     hard-stop all tasks"),
        Line::from("  q                     quit (hard-stops; again forces exit)"),
        Line::from("  Ctrl+c                hard-stop all and quit"),
        Line::from(""),
        Line::from("  ?                     toggle this help"),
        Line::from("  Esc                   close help"),
    ];
    let block = Block::default().borders(Borders::ALL).title("Help");
    f.render_widget(Clear, area);
    f.render_widget(Paragraph::new(lines).block(block), area);
}

fn short_cmd(s: &str) -> String {
    let first = s.lines().next().unwrap_or("").trim();
    if first.len() > 80 {
        format!("{}…", &first[..80])
    } else {
        first.to_owned()
    }
}

fn centered_rect(pct_x: u16, pct_y: u16, r: Rect) -> Rect {
    let v = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage((100 - pct_y) / 2),
            Constraint::Percentage(pct_y),
            Constraint::Percentage((100 - pct_y) / 2),
        ])
        .split(r);
    Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage((100 - pct_x) / 2),
            Constraint::Percentage(pct_x),
            Constraint::Percentage((100 - pct_x) / 2),
        ])
        .split(v[1])[1]
}
