pub mod app;
pub mod ui;

use {
    crate::exec::{
        Cancel,
        ExecEvent,
    },
    anyhow::Result,
    crossterm::{
        event::{
            Event,
            EventStream,
            KeyCode,
            KeyEvent,
            KeyEventKind,
            KeyModifiers,
        },
        execute,
        terminal::{
            disable_raw_mode,
            enable_raw_mode,
            EnterAlternateScreen,
            LeaveAlternateScreen,
        },
    },
    futures::StreamExt,
    ratatui::prelude::*,
    std::{
        io,
        time::Duration,
    },
    tokio::sync::mpsc::UnboundedReceiver,
};

/// Drive the execution TUI. Returns `true` when the plan completed without
/// failures, `false` when any batch failed or the user cancelled.
///
/// Rendering goes to stderr so stdout stays clean. Key events are read via
/// crossterm's `use-dev-tty` backend, which opens `/dev/tty` directly rather
/// than going through stdin; this lets the TUI work with a piped `plan`.
pub async fn run(rx: UnboundedReceiver<ExecEvent>, cancel: Cancel) -> Result<bool> {
    enable_raw_mode()?;
    let mut stderr = io::stderr();
    execute!(stderr, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stderr);
    let mut terminal = Terminal::new(backend)?;

    let result = event_loop(&mut terminal, rx, cancel).await;

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    result
}

/// Force the crossterm event-reader singleton to initialize without entering raw mode or
/// the alternate screen. Lets the caller detect `/dev/tty` / kqueue problems and fall back
/// to the plain driver before we've touched the terminal state.
pub fn probe_event_reader() -> std::io::Result<()> {
    crossterm::event::poll(std::time::Duration::from_millis(0)).map(|_| ())
}

async fn event_loop<B: Backend>(
    terminal: &mut Terminal<B>,
    mut rx: UnboundedReceiver<ExecEvent>,
    cancel: Cancel,
) -> Result<bool> {
    let mut app = app::App::new();
    let mut key_stream = EventStream::new();
    let tick = Duration::from_millis(100);

    loop {
        terminal.draw(|f| ui::render(f, &mut app))?;

        tokio::select! {
            biased;
            maybe_key = key_stream.next() => {
                if let Some(Ok(Event::Key(k))) = maybe_key {
                    handle_key(&mut app, k, &cancel);
                }
            }
            ev = rx.recv() => {
                match ev {
                    | Some(e) => app.apply(e),
                    | None => {
                        // Engine finished — render the final frame and wait for `q`.
                        terminal.draw(|f| ui::render(f, &mut app))?;
                        app.exec_finished = true;
                    }
                }
            }
            _ = tokio::time::sleep(tick) => {}
        }

        if app.quit {
            break;
        }
    }

    Ok(app.ok())
}

fn handle_key(app: &mut app::App, ev: KeyEvent, cancel: &Cancel) {
    if ev.kind != KeyEventKind::Press {
        return;
    }
    let ctrl = ev.modifiers.contains(KeyModifiers::CONTROL);
    match ev.code {
        | KeyCode::Char('c') if ctrl => {
            cancel.trip();
            app.request_quit_after_finish();
        },
        | KeyCode::Char('q') => {
            if app.exec_finished {
                app.quit = true;
            } else {
                cancel.trip();
                app.request_quit_after_finish();
            }
        },
        | KeyCode::Char('j') | KeyCode::Down => app.cursor_down(),
        | KeyCode::Char('k') | KeyCode::Up => app.cursor_up(),
        | KeyCode::Char('g') | KeyCode::Home => app.logs_top(),
        | KeyCode::Char('G') | KeyCode::End => app.logs_bottom(),
        | KeyCode::Char('d') if ctrl => app.logs_page_down(),
        | KeyCode::Char('u') if ctrl => app.logs_page_up(),
        | KeyCode::PageDown => app.logs_page_down(),
        | KeyCode::PageUp => app.logs_page_up(),
        | KeyCode::Char('f') => app.toggle_follow(),
        | KeyCode::Char('?') => app.toggle_help(),
        | KeyCode::Esc => app.dismiss_help(),
        | _ => {},
    }
}
