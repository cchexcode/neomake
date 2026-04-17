pub mod app;
pub mod ui;

use {
    crate::multiplex::{
        Cancel,
        MuxEvent,
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

/// Drive the multiplex TUI. Returns on user quit, engine completion, or
/// cancellation.
pub async fn run(rx: UnboundedReceiver<MuxEvent>, cancel: Cancel) -> Result<()> {
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

/// Force the crossterm event-reader singleton to initialize without entering
/// raw mode or the alternate screen. See the corresponding helper in
/// `tui::probe_event_reader`.
pub fn probe_event_reader() -> std::io::Result<()> {
    crossterm::event::poll(std::time::Duration::from_millis(0)).map(|_| ())
}

async fn event_loop<B: Backend>(
    terminal: &mut Terminal<B>,
    mut rx: UnboundedReceiver<MuxEvent>,
    cancel: Cancel,
) -> Result<()> {
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
                        terminal.draw(|f| ui::render(f, &mut app))?;
                        app.engine_finished = true;
                    }
                }
            }
            _ = tokio::time::sleep(tick) => {}
        }

        if app.quit {
            break;
        }
    }
    Ok(())
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
            if app.engine_finished {
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
