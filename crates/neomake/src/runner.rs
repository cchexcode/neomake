use {
    crate::{
        cancel::Cancel,
        exec::{
            BatchKey,
            BatchStatus,
            ExecEngine,
            ExecEvent,
        },
        kill::Control,
        plan::ExecutionPlan,
    },
    anyhow::{
        anyhow,
        Context,
        Result,
    },
    std::{
        collections::HashMap,
        time::Instant,
    },
    tokio::sync::mpsc::unbounded_channel,
};

/// Whether children's stdout/stderr are forwarded in plain mode.
#[derive(Debug, Clone, Copy)]
pub struct PlainOutput {
    pub stdout: bool,
    pub stderr: bool,
}

/// Entry point used by `main.rs` for `neomake execute`.
///
/// Decides between the TUI and the plain streaming driver, spawns the engine,
/// collects timing events for the optional `--summary`, and wires events
/// between the two. Returns `Err` when any batch failed or the user cancelled.
pub async fn execute(plan: ExecutionPlan, workers: usize, tui: bool, plain: PlainOutput, summary: bool) -> Result<()> {
    if plan.version != env!("CARGO_PKG_VERSION") {
        anyhow::bail!(
            "execution plan version {} does not match neomake {}; re-run `neomake plan`",
            plan.version,
            env!("CARGO_PKG_VERSION")
        );
    }

    let control = Control::<BatchKey>::new(Cancel::new());
    crate::kill::arm_exit_signals(control.clone());
    let (tx, rx) = unbounded_channel::<ExecEvent>();

    // Tee events into the summary collector when requested. Both drivers and the
    // collector own their own receiver so no one waits on anyone else.
    let (rx_driver, rx_summary) = if summary {
        let (a_tx, a_rx) = unbounded_channel::<ExecEvent>();
        let (b_tx, b_rx) = unbounded_channel::<ExecEvent>();
        tokio::spawn(tee(rx, a_tx, b_tx));
        (a_rx, Some(b_rx))
    } else {
        (rx, None)
    };

    let engine = ExecEngine::new(workers)?.with_control(control.clone());
    let plan_for_engine = plan.clone();
    let engine_handle = tokio::spawn(async move { engine.run(&plan_for_engine, tx).await });

    let summary_handle = rx_summary.map(|r| tokio::spawn(collect_summary(r)));

    let result = if tui {
        if let Err(e) = crate::tui::probe_event_reader() {
            eprintln!("TUI disabled ({}); using plain driver.", e);
            run_plain(rx_driver, plain).await
        } else {
            crate::tui::run(rx_driver, control.clone()).await
        }
    } else {
        run_plain(rx_driver, plain).await
    };

    if control.forced() {
        control.stop_all();
        engine_handle.abort();
        if let Some(handle) = summary_handle {
            handle.abort();
        }
        return Err(anyhow!("execution aborted"));
    }

    if let Some(handle) = summary_handle {
        if let Ok(data) = handle.await {
            print_summary(&plan, &data);
        }
    }

    let driver_ok = result?;
    let engine_ok = engine_handle.await.context("execution engine panicked")??;
    if driver_ok && engine_ok {
        Ok(())
    } else {
        Err(anyhow!("execution failed"))
    }
}

/// Fan out a single receiver into two. Events are cloned for each side (cheap
/// for our variants — the payload is short strings).
async fn tee(
    mut rx: tokio::sync::mpsc::UnboundedReceiver<ExecEvent>,
    a: tokio::sync::mpsc::UnboundedSender<ExecEvent>,
    b: tokio::sync::mpsc::UnboundedSender<ExecEvent>,
) {
    while let Some(ev) = rx.recv().await {
        let _ = b.send(ev.clone());
        let _ = a.send(ev);
    }
}

/// Plain (non-TTY) driver.
async fn run_plain(mut rx: tokio::sync::mpsc::UnboundedReceiver<ExecEvent>, output: PlainOutput) -> Result<bool> {
    use std::io::Write;

    let mut ok = true;
    let mut labels = HashMap::<BatchKey, String>::new();

    while let Some(ev) = rx.recv().await {
        match ev {
            | ExecEvent::Started { batches } => {
                for info in &batches {
                    labels.insert(info.key.clone(), info.key.label(info));
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

// Summary collection and rendering.

struct SummaryData {
    started_at: Instant,
    finished_at: Option<Instant>,
    total_batches: usize,
    batches: HashMap<BatchKey, BatchSummary>,
    wave_starts: HashMap<usize, Instant>,
    wave_ends: HashMap<usize, Instant>,
    labels: HashMap<BatchKey, String>,
    aborted: bool,
}

struct BatchSummary {
    wave: usize,
    duration_ms: u64,
    ok: bool,
}

async fn collect_summary(mut rx: tokio::sync::mpsc::UnboundedReceiver<ExecEvent>) -> SummaryData {
    let mut data = SummaryData {
        started_at: Instant::now(),
        finished_at: None,
        total_batches: 0,
        batches: HashMap::new(),
        wave_starts: HashMap::new(),
        wave_ends: HashMap::new(),
        labels: HashMap::new(),
        aborted: false,
    };
    while let Some(ev) = rx.recv().await {
        match ev {
            | ExecEvent::Started { batches } => {
                data.started_at = Instant::now();
                data.total_batches = batches.len();
                for info in &batches {
                    data.labels.insert(info.key.clone(), info.key.label(info));
                }
            },
            | ExecEvent::WaveStarted { wave } => {
                data.wave_starts.insert(wave, Instant::now());
            },
            | ExecEvent::BatchCompleted {
                key,
                status,
                duration_ms,
            } => {
                let ok = matches!(status, BatchStatus::Success);
                data.batches.insert(key.clone(), BatchSummary {
                    wave: key.wave,
                    duration_ms,
                    ok,
                });
            },
            | ExecEvent::WaveCompleted { wave, .. } => {
                data.wave_ends.insert(wave, Instant::now());
            },
            | ExecEvent::Finished { aborted, .. } => {
                data.finished_at = Some(Instant::now());
                data.aborted = aborted;
            },
            | _ => {},
        }
    }
    data
}

fn print_summary(plan: &ExecutionPlan, data: &SummaryData) {
    use std::io::Write;
    let w = &mut std::io::stderr();

    let total = data
        .finished_at
        .map(|f| f.saturating_duration_since(data.started_at).as_millis())
        .unwrap_or(0);

    let _ = writeln!(w);
    let _ = writeln!(w, "── summary ────────────────────────────────");
    let _ = writeln!(
        w,
        "total: {} ms · {}/{} batches ok{}",
        total,
        data.batches.values().filter(|b| b.ok).count(),
        data.total_batches,
        if data.aborted { " · aborted" } else { "" }
    );

    // Per-wave durations.
    let mut waves: Vec<usize> = data.wave_starts.keys().copied().collect();
    waves.sort();
    for wave in &waves {
        let start = data.wave_starts.get(wave).copied();
        let end = data.wave_ends.get(wave).copied();
        let dur = match (start, end) {
            | (Some(s), Some(e)) => e.saturating_duration_since(s).as_millis(),
            | _ => 0,
        };
        let in_wave = data.batches.values().filter(|b| b.wave == *wave).count();
        let _ = writeln!(w, "  wave {}: {} ms ({} batches)", wave, dur, in_wave);
    }

    // Slowest batches (top 5).
    let mut ranked: Vec<(&BatchKey, &BatchSummary)> = data.batches.iter().collect();
    ranked.sort_by_key(|(_, b)| std::cmp::Reverse(b.duration_ms));
    if !ranked.is_empty() {
        let _ = writeln!(w, "slowest batches:");
        for (key, b) in ranked.iter().take(5) {
            let label = data.labels.get(key).cloned().unwrap_or_else(|| key.node.clone());
            let mark = if b.ok { "✓" } else { "✗" };
            let _ = writeln!(w, "  {} {} — {} ms (wave {})", mark, label, b.duration_ms, b.wave);
        }
    }

    let mut weights = HashMap::<String, u128>::new();
    for (key, batch) in &data.batches {
        let entry = weights.entry(key.node.clone()).or_insert(0);
        *entry = (*entry).max(u128::from(batch.duration_ms));
    }
    if let Some((path, path_ms)) = crate::plan::critical_path(plan, &weights) {
        let _ = writeln!(w, "critical path ({path_ms} ms):");
        for name in &path {
            let ms = weights.get(name).copied().unwrap_or(0);
            let _ = writeln!(w, "  → {name} ({ms} ms)");
        }
    }
    let _ = writeln!(w, "───────────────────────────────────────────");
}
