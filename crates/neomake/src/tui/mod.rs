pub mod app;
pub mod ui;

use {
    crate::{
        exec::ExecEvent,
        kill::{
            Control,
            FORCE_QUIT_AFTER,
        },
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
    tokio::sync::mpsc::{
        error::TryRecvError,
        UnboundedReceiver,
    },
};

/// Drive the execution TUI. Returns `true` when the plan completed without
/// failures, `false` when any batch failed or the user cancelled.
///
/// Rendering goes to stderr so stdout stays clean. Key events are read via
/// crossterm's `use-dev-tty` backend, which opens `/dev/tty` directly rather
/// than going through stdin; this lets the TUI work with a piped `plan`.
pub async fn run(rx: UnboundedReceiver<ExecEvent>, control: Control<crate::exec::BatchKey>) -> Result<bool> {
    enable_raw_mode()?;
    let mut stderr = io::stderr();
    execute!(stderr, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stderr);
    let mut terminal = Terminal::new(backend)?;

    let result = event_loop(&mut terminal, rx, control).await;

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    result
}

/// Force the crossterm event-reader singleton to initialize without entering
/// raw mode or the alternate screen. Lets the caller detect `/dev/tty` / kqueue
/// problems and fall back to the plain driver before we've touched the terminal
/// state.
pub fn probe_event_reader() -> std::io::Result<()> {
    crossterm::event::poll(std::time::Duration::from_millis(0)).map(|_| ())
}

async fn event_loop<B: Backend>(
    terminal: &mut Terminal<B>,
    mut rx: UnboundedReceiver<ExecEvent>,
    control: Control<crate::exec::BatchKey>,
) -> Result<bool> {
    let mut app = app::App::new();
    let mut key_stream = EventStream::new();
    let tick = Duration::from_millis(100);
    // Once the engine drops the sender, `recv` returns immediately. Leaving
    // that branch armed busy-spins the redraw and makes the TUI feel stuck.
    let mut engine_open = true;

    loop {
        if engine_open {
            engine_open = drain_exec(&mut app, &mut rx);
        }
        terminal.draw(|f| ui::render(f, &mut app))?;

        tokio::select! {
            biased;
            maybe_key = key_stream.next() => {
                if let Some(Ok(Event::Key(k))) = maybe_key {
                    handle_key(&mut app, k, &control);
                }
            }
            ev = rx.recv(), if engine_open => {
                match ev {
                    | Some(e) => {
                        app.apply(e);
                        engine_open = drain_exec(&mut app, &mut rx);
                    },
                    | None => {
                        engine_open = false;
                        app.note_engine_closed();
                    },
                }
            }
            _ = tokio::time::sleep(tick) => {}
        }

        poll_force_quit(&mut app, &control);
        if app.quit {
            break;
        }
    }

    Ok(app.ok())
}

/// Pull every event already queued so a log flood is one frame, not one draw
/// per line. Returns false when the engine has closed the channel.
fn drain_exec(app: &mut app::App, rx: &mut UnboundedReceiver<ExecEvent>) -> bool {
    loop {
        match rx.try_recv() {
            | Ok(ev) => app.apply(ev),
            | Err(TryRecvError::Empty) => return true,
            | Err(TryRecvError::Disconnected) => {
                app.note_engine_closed();
                return false;
            },
        }
    }
}

fn poll_force_quit(app: &mut app::App, control: &Control<crate::exec::BatchKey>) {
    if app.quit || app.exec_finished {
        return;
    }
    if let Some(at) = app.quit_requested_at {
        if at.elapsed() >= FORCE_QUIT_AFTER {
            control.force_quit();
            app.quit = true;
        }
    }
}

fn handle_key(app: &mut app::App, ev: KeyEvent, control: &Control<crate::exec::BatchKey>) {
    if ev.kind != KeyEventKind::Press {
        return;
    }
    let ctrl = ev.modifiers.contains(KeyModifiers::CONTROL);
    match ev.code {
        | KeyCode::Char('c') if ctrl => hard_stop_and_quit(app, control),
        | KeyCode::Char('q') => {
            if app.exec_finished {
                app.quit = true;
            } else {
                hard_stop_and_quit(app, control);
            }
        },
        | KeyCode::Char('x') => {
            if let Some(key) = app.selected_key().cloned() {
                control.stop_one(&key);
                app.note_stopped(&key);
            }
        },
        | KeyCode::Char('X') => {
            control.stop_all();
            app.note_stopped_all();
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

/// Hard-stop is synchronous. A second press closes the TUI without waiting
/// for the engine to notice.
fn hard_stop_and_quit(app: &mut app::App, control: &Control<crate::exec::BatchKey>) {
    if app.quit_requested {
        control.force_quit();
        app.quit = true;
        return;
    }
    control.stop_all();
    app.note_stopped_all();
    app.request_quit_after_finish();
}
