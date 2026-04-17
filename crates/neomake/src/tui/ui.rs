use {
    crate::tui::app::{
        App,
        BatchState,
        LogStream,
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
    render_hotkeys(f, chunks[2], app);

    if app.show_help {
        render_help(f);
    }
}

fn render_header(f: &mut Frame, area: Rect, app: &App) {
    let (done, total) = app.wave_progress();
    let secs = app.elapsed().as_secs();
    let status = if app.aborted {
        " ABORTED".to_owned()
    } else if app.exec_finished {
        if app.failed_batches == 0 {
            " DONE".to_owned()
        } else {
            format!(" DONE ({} failed)", app.failed_batches)
        }
    } else {
        format!(" wave {}/{}", app.wave_cursor + 1, app.wave_count.max(1))
    };
    let line = Line::from(vec![
        Span::styled(" neomake ", Style::default().bg(Color::DarkGray).fg(Color::White)),
        Span::raw(format!(" {}/{} batches · {}s", done, total, secs)),
        Span::styled(status, Style::default().fg(status_color(app))),
    ]);
    f.render_widget(Paragraph::new(line), area);
}

fn status_color(app: &App) -> Color {
    if app.aborted || app.failed_batches > 0 {
        Color::Red
    } else if app.exec_finished {
        Color::Green
    } else {
        Color::Yellow
    }
}

fn render_tree(f: &mut Frame, area: Rect, app: &mut App) {
    let items: Vec<ListItem> = app
        .order
        .iter()
        .map(|key| {
            let b = &app.batches[key];
            let (glyph, style) = match b.state {
                | BatchState::Pending => ("●", Style::default().fg(Color::DarkGray)),
                | BatchState::Running => ("◐", Style::default().fg(Color::Yellow)),
                | BatchState::Success => ("✓", Style::default().fg(Color::Green)),
                | BatchState::Failed => ("✗", Style::default().fg(Color::Red)),
            };
            let duration = b.duration_ms.map(|ms| format!(" {}ms", ms)).unwrap_or_default();
            let wave = format!("w{} ", key.wave);
            let label = key.label(&b.info);
            ListItem::new(Line::from(vec![
                Span::styled(wave, Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{} ", glyph), style),
                Span::raw(label),
                Span::styled(duration, Style::default().fg(Color::DarkGray)),
            ]))
        })
        .collect();

    let list = List::new(items)
        .block(Block::default().borders(Borders::ALL).title("Batches"))
        .highlight_style(Style::default().bg(Color::DarkGray))
        .highlight_symbol("▌");

    let mut state = ListState::default();
    state.select(Some(app.cursor));
    f.render_stateful_widget(list, area, &mut state);
}

fn render_logs(f: &mut Frame, area: Rect, app: &App) {
    let title = match app.selected_key() {
        | Some(key) => {
            let b = &app.batches[key];
            let mut t = format!("logs: {}", key.label(&b.info));
            if app.follow {
                t.push_str(" [follow]");
            }
            t
        },
        | None => "logs".to_owned(),
    };
    let block = Block::default().borders(Borders::ALL).title(title);

    let lines: Vec<Line> = match app.selected_key() {
        | Some(key) => {
            let b = &app.batches[key];
            let mut lines = Vec::<Line>::new();
            if let Some(cmd) = &b.current_command {
                lines.push(Line::from(vec![
                    Span::styled("$ ", Style::default().fg(Color::Cyan)),
                    Span::raw(cmd.clone()),
                ]));
            }
            for l in &b.logs {
                let style = match l.stream {
                    | LogStream::Stdout => Style::default(),
                    | LogStream::Stderr => Style::default().fg(Color::Red),
                };
                lines.push(Line::from(Span::styled(l.text.clone(), style)));
            }
            if let Some(msg) = &b.failure {
                lines.push(Line::from(Span::styled(
                    format!("! {}", msg),
                    Style::default().fg(Color::Red),
                )));
            }
            if b.logs.is_empty() && b.current_command.is_none() && b.failure.is_none() {
                lines.push(Line::from(Span::styled(
                    "(no output)",
                    Style::default().fg(Color::DarkGray),
                )));
            }
            lines
        },
        | None => vec![Line::from("(no batch selected)")],
    };

    let inner_height = area.height.saturating_sub(2) as usize;
    let scroll = if app.log_scroll == u16::MAX {
        lines.len().saturating_sub(inner_height) as u16
    } else {
        app.log_scroll.min(lines.len().saturating_sub(inner_height) as u16)
    };

    let p = Paragraph::new(lines)
        .block(block)
        .wrap(Wrap { trim: false })
        .scroll((scroll, 0));
    f.render_widget(p, area);
}

fn render_hotkeys(f: &mut Frame, area: Rect, _app: &App) {
    let hotkeys = Line::from(vec![
        Span::styled(" j/k ", Style::default().fg(Color::Cyan)),
        Span::raw("nav "),
        Span::styled("Ctrl+d/u ", Style::default().fg(Color::Cyan)),
        Span::raw("page "),
        Span::styled("g/G ", Style::default().fg(Color::Cyan)),
        Span::raw("top/bot "),
        Span::styled("f ", Style::default().fg(Color::Cyan)),
        Span::raw("follow "),
        Span::styled("? ", Style::default().fg(Color::Cyan)),
        Span::raw("help "),
        Span::styled("q ", Style::default().fg(Color::Cyan)),
        Span::raw("quit "),
    ]);
    f.render_widget(Paragraph::new(hotkeys), area);
}

fn render_help(f: &mut Frame) {
    let area = centered_rect(60, 50, f.area());
    let lines = vec![
        Line::from("Navigation"),
        Line::from("  j / k, Down / Up      move cursor"),
        Line::from("  g / G, Home / End     jump to top / bottom of logs"),
        Line::from("  Ctrl+d / Ctrl+u       page logs"),
        Line::from("  f                     toggle follow mode"),
        Line::from(""),
        Line::from("Execution"),
        Line::from("  q                     quit (cancels if still running)"),
        Line::from("  Ctrl+c                cancel execution"),
        Line::from(""),
        Line::from("  ?                     toggle this help"),
        Line::from("  Esc                   close help"),
    ];
    let block = Block::default().borders(Borders::ALL).title("Help");
    f.render_widget(Clear, area);
    f.render_widget(Paragraph::new(lines).block(block), area);
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
