use {
    crate::{
        exec::{
            BatchInfo,
            BatchKey,
            BatchStatus,
            Cancel,
            ExecEngine,
            ExecEvent,
        },
        plan::ExecutionPlan,
    },
    anyhow::{
        anyhow,
        Result,
    },
    std::{
        collections::HashMap,
        io::IsTerminal,
    },
    tokio::sync::mpsc::unbounded_channel,
};

/// How output should be presented to the user.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TuiMode {
    /// TUI when stderr is a TTY, plain otherwise.
    Auto,
    /// Force the plain streaming driver.
    Off,
}

/// Whether children's stdout/stderr are forwarded in plain mode.
#[derive(Debug, Clone, Copy)]
pub struct PlainOutput {
    pub stdout: bool,
    pub stderr: bool,
}

/// Entry point used by `main.rs` for `neomake execute`.
///
/// Decides between the TUI and the plain streaming driver, spawns the engine,
/// and wires events between the two. Returns `Err` when any batch failed or the
/// user cancelled.
pub async fn execute(plan: ExecutionPlan, workers: usize, tui: TuiMode, plain: PlainOutput) -> Result<()> {
    // TUI renders to stderr and reads keys from /dev/tty (via crossterm's
    // use-dev-tty backend), so a piped stdin — the `plan | execute` flow — is
    // fine.
    let use_tui = matches!(tui, TuiMode::Auto) && std::io::stderr().is_terminal();

    let cancel = Cancel::new();
    let (tx, rx) = unbounded_channel::<ExecEvent>();

    let engine = ExecEngine::new(workers).with_cancel(cancel.clone());
    let plan_clone = plan.clone();
    let engine_handle = tokio::spawn(async move { engine.run(&plan_clone, tx).await });

    let result = if use_tui {
        // Probe first so a /dev/tty failure falls back to plain without losing the rx.
        if let Err(e) = crate::tui::probe_event_reader() {
            eprintln!("TUI disabled ({}); using plain driver.", e);
            run_plain(rx, plain).await
        } else {
            crate::tui::run(rx, cancel.clone()).await
        }
    } else {
        run_plain(rx, plain).await
    };

    let _ = engine_handle.await;

    match result {
        | Ok(true) => Ok(()),
        | Ok(false) => Err(anyhow!("execution failed")),
        | Err(e) => Err(e),
    }
}

/// Plain (non-TTY) driver: forwards line-buffered stdout/stderr with a `[node]`
/// prefix, prints a one-line summary per batch completion, and returns `false`
/// on any failure.
async fn run_plain(mut rx: tokio::sync::mpsc::UnboundedReceiver<ExecEvent>, output: PlainOutput) -> Result<bool> {
    use std::io::Write;

    let mut ok = true;
    let mut labels = HashMap::<BatchKey, String>::new();

    while let Some(ev) = rx.recv().await {
        match ev {
            | ExecEvent::Started { batches } => {
                for info in &batches {
                    labels.insert(info.key.clone(), prefix_label(info));
                }
            },
            | ExecEvent::WaveStarted { wave } => {
                let _ = writeln!(std::io::stderr(), "┌─ wave {} ─", wave);
                let _ = std::io::stderr().flush();
            },
            | ExecEvent::BatchStarted { .. } => {},
            | ExecEvent::Stdout { key, line } => {
                if output.stdout {
                    let prefix = labels.get(&key).cloned().unwrap_or_else(|| key.node.clone());
                    let _ = writeln!(std::io::stdout(), "[{}] {}", prefix, line);
                }
            },
            | ExecEvent::Stderr { key, line } => {
                if output.stderr {
                    let prefix = labels.get(&key).cloned().unwrap_or_else(|| key.node.clone());
                    let _ = writeln!(std::io::stderr(), "[{}] {}", prefix, line);
                }
            },
            | ExecEvent::BatchCompleted {
                key,
                status,
                duration_ms,
            } => {
                let prefix = labels.get(&key).cloned().unwrap_or_else(|| key.node.clone());
                match status {
                    | BatchStatus::Success => {
                        let _ = writeln!(std::io::stderr(), "✓ {} ({} ms)", prefix, duration_ms);
                    },
                    | BatchStatus::Failed { code, message } => {
                        ok = false;
                        let code = code.map(|c| c.to_string()).unwrap_or_else(|| "?".to_owned());
                        let _ = writeln!(
                            std::io::stderr(),
                            "✗ {} [exit {}] {} ({} ms)",
                            prefix,
                            code,
                            message,
                            duration_ms
                        );
                    },
                }
            },
            | ExecEvent::WaveCompleted { wave, failures } => {
                if failures == 0 {
                    let _ = writeln!(std::io::stderr(), "└─ wave {} ✓", wave);
                } else {
                    let _ = writeln!(std::io::stderr(), "└─ wave {} ✗ ({} failed)", wave, failures);
                }
            },
            | ExecEvent::Finished {
                failed_batches,
                aborted,
            } => {
                if aborted {
                    ok = false;
                    let _ = writeln!(std::io::stderr(), "aborted");
                } else if failed_batches > 0 {
                    let _ = writeln!(std::io::stderr(), "{} batch(es) failed", failed_batches);
                }
            },
        }
    }

    Ok(ok)
}

fn prefix_label(info: &BatchInfo) -> String {
    let cells = info
        .invocations
        .iter()
        .filter(|c| !c.is_empty())
        .map(|c| c.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","))
        .collect::<Vec<_>>();
    if cells.is_empty() {
        info.key.node.clone()
    } else if cells.len() == 1 {
        format!("{}[{}]", info.key.node, cells[0])
    } else {
        format!("{}[×{}]", info.key.node, cells.len())
    }
}
