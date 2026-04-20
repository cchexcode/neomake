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
        time::Instant,
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
/// collects timing events for the optional `--summary`, and wires events
/// between the two. Returns `Err` when any batch failed or the user cancelled.
pub async fn execute(
    plan: ExecutionPlan,
    workers: usize,
    tui: TuiMode,
    plain: PlainOutput,
    summary: bool,
) -> Result<()> {
    let use_tui = matches!(tui, TuiMode::Auto) && std::io::stderr().is_terminal();

    let cancel = Cancel::new();
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

    let engine = ExecEngine::new(workers).with_cancel(cancel.clone());
    let plan_for_engine = plan.clone();
    let engine_handle = tokio::spawn(async move { engine.run(&plan_for_engine, tx).await });

    let summary_handle = rx_summary.map(|r| tokio::spawn(collect_summary(r)));

    let result = if use_tui {
        if let Err(e) = crate::tui::probe_event_reader() {
            eprintln!("TUI disabled ({}); using plain driver.", e);
            run_plain(rx_driver, plain).await
        } else {
            crate::tui::run(rx_driver, cancel.clone()).await
        }
    } else {
        run_plain(rx_driver, plain).await
    };

    let _ = engine_handle.await;

    if let Some(h) = summary_handle {
        if let Ok(data) = h.await {
            print_summary(&plan, &data);
        }
    }

    match result {
        | Ok(true) => Ok(()),
        | Ok(false) => Err(anyhow!("execution failed")),
        | Err(e) => Err(e),
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
        let cloned = clone_event(&ev);
        let _ = a.send(ev);
        let _ = b.send(cloned);
    }
}

fn clone_event(ev: &ExecEvent) -> ExecEvent {
    match ev {
        | ExecEvent::Started { batches } => {
            ExecEvent::Started {
                batches: batches.clone(),
            }
        },
        | ExecEvent::WaveStarted { wave } => ExecEvent::WaveStarted { wave: *wave },
        | ExecEvent::BatchStarted { key, command } => {
            ExecEvent::BatchStarted {
                key: key.clone(),
                command: command.clone(),
            }
        },
        | ExecEvent::Stdout { key, line } => {
            ExecEvent::Stdout {
                key: key.clone(),
                line: line.clone(),
            }
        },
        | ExecEvent::Stderr { key, line } => {
            ExecEvent::Stderr {
                key: key.clone(),
                line: line.clone(),
            }
        },
        | ExecEvent::BatchCompleted {
            key,
            status,
            duration_ms,
        } => {
            ExecEvent::BatchCompleted {
                key: key.clone(),
                status: status.clone(),
                duration_ms: *duration_ms,
            }
        },
        | ExecEvent::WaveCompleted { wave, failures } => {
            ExecEvent::WaveCompleted {
                wave: *wave,
                failures: *failures,
            }
        },
        | ExecEvent::Finished {
            failed_batches,
            aborted,
        } => {
            ExecEvent::Finished {
                failed_batches: *failed_batches,
                aborted: *aborted,
            }
        },
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

// ---- Summary collection & rendering
// ---------------------------------------------------

struct SummaryData {
    started_at: Instant,
    finished_at: Option<Instant>,
    total_batches: usize,
    batches: HashMap<BatchKey, BatchSummary>,
    wave_starts: HashMap<usize, Instant>,
    wave_ends: HashMap<usize, Instant>,
    labels: HashMap<BatchKey, String>,
    failed_batches: usize,
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
        failed_batches: 0,
        aborted: false,
    };
    while let Some(ev) = rx.recv().await {
        match ev {
            | ExecEvent::Started { batches } => {
                data.started_at = Instant::now();
                data.total_batches = batches.len();
                for info in &batches {
                    data.labels.insert(info.key.clone(), prefix_label(info));
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
            | ExecEvent::Finished {
                failed_batches,
                aborted,
            } => {
                data.finished_at = Some(Instant::now());
                data.failed_batches = failed_batches;
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

    // Critical path: longest chain through the plan's DAG weighted by observed
    // durations. For matrix nodes, use the max invocation duration across all
    // cells as the node's weight.
    if let Some((path, path_ms)) = critical_path(plan, data) {
        let _ = writeln!(w, "critical path ({} ms):", path_ms);
        for n in &path {
            let per_node_ms = data
                .batches
                .iter()
                .filter(|(k, _)| &k.node == n)
                .map(|(_, b)| b.duration_ms)
                .max()
                .unwrap_or(0);
            let _ = writeln!(w, "  → {} ({} ms)", n, per_node_ms);
        }
    }
    let _ = writeln!(w, "───────────────────────────────────────────");
}

/// Longest chain through the DAG weighted by per-node duration (max over matrix
/// cells). Uses the plan's stage structure to derive edges: a node depends on
/// every node that appears in an earlier wave AND is one of its declared
/// `inputs` or matches a `pre` — but we don't have the workflow here, so
/// approximate via the plan's `inputs` field plus topological position.
fn critical_path(plan: &ExecutionPlan, data: &SummaryData) -> Option<(Vec<String>, u128)> {
    let mut weight = HashMap::<String, u128>::new();
    let mut order = Vec::<String>::new();
    for stage in &plan.stages {
        for n in &stage.nodes {
            order.push(n.clone());
            let w = data
                .batches
                .iter()
                .filter(|(k, _)| &k.node == n)
                .map(|(_, b)| b.duration_ms as u128)
                .max()
                .unwrap_or(0);
            weight.insert(n.clone(), w);
        }
    }

    // Parents from `inputs` only — `pre` is ordering, not data flow, and we don't
    // have the original workflow here. This still produces a correct critical
    // path through the nodes whose data actually chained together.
    let mut best = HashMap::<String, (u128, Option<String>)>::new();
    for n in &order {
        let node = match plan.nodes.get(n) {
            | Some(x) => x,
            | None => continue,
        };
        let w = *weight.get(n).unwrap_or(&0);
        let mut this_best = (w, None);
        for parent in &node.inputs {
            if let Some((p_time, _)) = best.get(parent) {
                let candidate = *p_time + w;
                if candidate > this_best.0 {
                    this_best = (candidate, Some(parent.clone()));
                }
            }
        }
        best.insert(n.clone(), this_best);
    }

    let (end_node, end_total) = best
        .iter()
        .max_by_key(|(_, (t, _))| *t)
        .map(|(n, (t, _))| (n.clone(), *t))?;
    if end_total == 0 {
        return None;
    }

    // Walk backwards.
    let mut path = Vec::<String>::new();
    let mut cur = Some(end_node);
    while let Some(n) = cur {
        path.push(n.clone());
        cur = best.get(&n).and_then(|(_, p)| p.clone());
    }
    path.reverse();
    Some((path, end_total))
}
