use {
    crate::plan::ExecutionPlan,
    anyhow::{
        anyhow,
        Result,
    },
    std::{
        collections::HashMap,
        process::Stdio,
        sync::{
            atomic::{
                AtomicBool,
                Ordering,
            },
            Arc,
        },
        time::Instant,
    },
    tokio::{
        io::{
            AsyncBufReadExt,
            BufReader,
        },
        process::Command,
        sync::{
            mpsc::UnboundedSender,
            Semaphore,
        },
        task::JoinSet,
    },
};

/// Identifies one unit of work dispatched onto the worker pool.
///
/// A "batch" is the scheduling granularity:
///   - for a node whose matrix is `parallel = true`, each invocation (matrix
///     cell) becomes its own batch, so invocations run concurrently up to the
///     worker budget
///   - for `parallel = false` (or no matrix), all invocations × all tasks are
///     flattened into a single sequential batch
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BatchKey {
    pub wave: usize,
    pub node: String,
    pub index: usize,
}

impl BatchKey {
    pub fn label(&self, info: &BatchInfo) -> String {
        let effective = info.invocations.iter().filter(|c| !c.is_empty()).collect::<Vec<_>>();
        if effective.is_empty() {
            self.node.clone()
        } else if effective.len() == 1 {
            format!("{}[{}]", self.node, fmt_cell(effective[0]))
        } else {
            format!("{} (×{})", self.node, effective.len())
        }
    }
}

fn fmt_cell(cell: &[u8]) -> String {
    cell.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",")
}

/// Static description of a scheduled batch, sent upfront so observers (TUI,
/// plain driver) can render the full tree before any task starts.
#[derive(Debug, Clone)]
pub struct BatchInfo {
    pub key: BatchKey,
    pub invocations: Vec<Vec<u8>>,
}

#[derive(Debug, Clone)]
pub enum BatchStatus {
    Success,
    Failed { code: Option<i32>, message: String },
}

#[derive(Debug)]
pub enum ExecEvent {
    Started {
        batches: Vec<BatchInfo>,
    },
    WaveStarted {
        wave: usize,
    },
    BatchStarted {
        key: BatchKey,
        command: String,
    },
    Stdout {
        key: BatchKey,
        line: String,
    },
    Stderr {
        key: BatchKey,
        line: String,
    },
    BatchCompleted {
        key: BatchKey,
        status: BatchStatus,
        duration_ms: u64,
    },
    WaveCompleted {
        wave: usize,
        failures: usize,
    },
    Finished {
        failed_batches: usize,
        aborted: bool,
    },
}

#[derive(Debug, Clone)]
struct Work {
    shell: String,
    command: String,
    env: HashMap<String, String>,
    workdir: Option<String>,
}

/// Cooperative cancellation flag shared with the engine.
#[derive(Clone, Default)]
pub struct Cancel(Arc<AtomicBool>);

impl Cancel {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn trip(&self) {
        self.0.store(true, Ordering::SeqCst);
    }

    pub fn tripped(&self) -> bool {
        self.0.load(Ordering::SeqCst)
    }
}

/// DAG execution engine. Runs stages sequentially; within a stage each batch is
/// dispatched onto a tokio `JoinSet` with a `Semaphore` capping concurrency at
/// `workers`. All I/O is streamed out via `ExecEvent` so drivers (TUI / plain)
/// can observe and render without coupling to process spawning.
pub struct ExecEngine {
    pub workers: usize,
    pub cancel: Cancel,
}

impl ExecEngine {
    pub fn new(workers: usize) -> Self {
        Self {
            workers: workers.max(1),
            cancel: Cancel::new(),
        }
    }

    pub fn with_cancel(mut self, cancel: Cancel) -> Self {
        self.cancel = cancel;
        self
    }

    pub async fn run(self, plan: &ExecutionPlan, tx: UnboundedSender<ExecEvent>) -> Result<bool> {
        let all_batches = self.enumerate_batches(plan)?;
        let infos = all_batches.iter().map(|(info, _)| info.clone()).collect::<Vec<_>>();
        let _ = tx.send(ExecEvent::Started { batches: infos });

        let mut failed_batches = 0usize;
        let mut aborted = false;

        for (wave_idx, stage) in plan.stages.iter().enumerate() {
            if self.cancel.tripped() {
                aborted = true;
                break;
            }
            let _ = tx.send(ExecEvent::WaveStarted { wave: wave_idx });

            let mut joins = JoinSet::<bool>::new();
            let sem = Arc::new(Semaphore::new(self.workers));

            for (info, batch) in all_batches.iter().filter(|(info, _)| info.key.wave == wave_idx) {
                // Defensive — every batch in this wave must belong to a node the stage lists.
                if !stage.nodes.contains(&info.key.node) {
                    continue;
                }
                let permit = match sem.clone().acquire_owned().await {
                    | Ok(p) => p,
                    | Err(_) => break,
                };
                if self.cancel.tripped() {
                    drop(permit);
                    aborted = true;
                    break;
                }
                let key = info.key.clone();
                let batch = batch.clone();
                let tx = tx.clone();
                let cancel = self.cancel.clone();
                joins.spawn(async move {
                    let ok = run_batch(key, batch, tx, cancel).await;
                    drop(permit);
                    ok
                });
            }

            let mut wave_failures = 0usize;
            while let Some(res) = joins.join_next().await {
                match res {
                    | Ok(true) => {},
                    | Ok(false) => wave_failures += 1,
                    | Err(_) => wave_failures += 1,
                }
            }

            let _ = tx.send(ExecEvent::WaveCompleted {
                wave: wave_idx,
                failures: wave_failures,
            });
            failed_batches += wave_failures;

            if wave_failures > 0 {
                // A failing wave aborts the rest of the plan; already-spawned batches in the
                // current wave complete above.
                break;
            }
        }

        let _ = tx.send(ExecEvent::Finished {
            failed_batches,
            aborted,
        });
        Ok(failed_batches == 0 && !aborted)
    }

    fn enumerate_batches(&self, plan: &ExecutionPlan) -> Result<Vec<(BatchInfo, Vec<Work>)>> {
        let mut out = Vec::<(BatchInfo, Vec<Work>)>::new();
        for (wave_idx, stage) in plan.stages.iter().enumerate() {
            for node_name in &stage.nodes {
                let node = plan
                    .nodes
                    .get(node_name)
                    .ok_or_else(|| anyhow!("plan missing node: {}", node_name))?;

                let mut per_invocation = Vec::<Vec<Work>>::new();
                for invocation in &node.invocations {
                    let mut works = Vec::<Work>::new();
                    for task in &node.tasks {
                        let shell = task
                            .shell
                            .clone()
                            .or_else(|| node.shell.clone())
                            .unwrap_or_else(|| "/bin/sh -c".to_owned());
                        let workdir = task.workdir.clone().or_else(|| node.workdir.clone());
                        let mut env = plan.env.clone();
                        env.extend(node.env.clone());
                        env.extend(invocation.env.clone());
                        env.extend(task.env.clone());
                        works.push(Work {
                            shell,
                            command: task.cmd.clone(),
                            env,
                            workdir,
                        });
                    }
                    per_invocation.push(works);
                }

                let parallel = node.parallel && node.invocations.len() > 1;
                if parallel {
                    for (i, works) in per_invocation.iter().enumerate() {
                        let info = BatchInfo {
                            key: BatchKey {
                                wave: wave_idx,
                                node: node_name.clone(),
                                index: i,
                            },
                            invocations: vec![node.invocations.get(i).map(|inv| inv.cell.clone()).unwrap_or_default()],
                        };
                        out.push((info, works.clone()));
                    }
                } else {
                    let flat: Vec<Work> = per_invocation.into_iter().flatten().collect();
                    let cells: Vec<Vec<u8>> = node.invocations.iter().map(|inv| inv.cell.clone()).collect();
                    let info = BatchInfo {
                        key: BatchKey {
                            wave: wave_idx,
                            node: node_name.clone(),
                            index: 0,
                        },
                        invocations: if cells.is_empty() { vec![vec![]] } else { cells },
                    };
                    out.push((info, flat));
                }
            }
        }
        Ok(out)
    }
}

async fn run_batch(key: BatchKey, batch: Vec<Work>, tx: UnboundedSender<ExecEvent>, cancel: Cancel) -> bool {
    let start = Instant::now();
    for work in batch {
        if cancel.tripped() {
            let _ = tx.send(ExecEvent::BatchCompleted {
                key: key.clone(),
                status: BatchStatus::Failed {
                    code: None,
                    message: "cancelled".to_owned(),
                },
                duration_ms: start.elapsed().as_millis() as u64,
            });
            return false;
        }
        let _ = tx.send(ExecEvent::BatchStarted {
            key: key.clone(),
            command: work.command.clone(),
        });

        let mut parts = work.shell.split_whitespace();
        let program = match parts.next() {
            | Some(p) => p,
            | None => {
                let _ = tx.send(ExecEvent::BatchCompleted {
                    key: key.clone(),
                    status: BatchStatus::Failed {
                        code: None,
                        message: "empty shell specification".to_owned(),
                    },
                    duration_ms: start.elapsed().as_millis() as u64,
                });
                return false;
            },
        };
        let mut cmd = Command::new(program);
        for p in parts {
            cmd.arg(p);
        }
        cmd.envs(&work.env);
        if let Some(wd) = &work.workdir {
            cmd.current_dir(wd);
        }
        cmd.arg(&work.command)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true);

        let mut child = match cmd.spawn() {
            | Ok(c) => c,
            | Err(e) => {
                let _ = tx.send(ExecEvent::BatchCompleted {
                    key: key.clone(),
                    status: BatchStatus::Failed {
                        code: None,
                        message: format!("spawn failed: {}", e),
                    },
                    duration_ms: start.elapsed().as_millis() as u64,
                });
                return false;
            },
        };

        let stdout = child.stdout.take();
        let stderr = child.stderr.take();
        let out_tx = tx.clone();
        let err_tx = tx.clone();
        let out_key = key.clone();
        let err_key = key.clone();

        let stdout_task = tokio::spawn(async move {
            if let Some(s) = stdout {
                let mut lines = BufReader::new(s).lines();
                while let Ok(Some(line)) = lines.next_line().await {
                    let _ = out_tx.send(ExecEvent::Stdout {
                        key: out_key.clone(),
                        line,
                    });
                }
            }
        });
        let stderr_task = tokio::spawn(async move {
            if let Some(s) = stderr {
                let mut lines = BufReader::new(s).lines();
                while let Ok(Some(line)) = lines.next_line().await {
                    let _ = err_tx.send(ExecEvent::Stderr {
                        key: err_key.clone(),
                        line,
                    });
                }
            }
        });

        let status = tokio::select! {
            s = child.wait() => s,
            _ = wait_cancel(&cancel) => {
                let _ = child.kill().await;
                let _ = child.wait().await;
                Err(std::io::Error::other("cancelled"))
            },
        };
        let _ = stdout_task.await;
        let _ = stderr_task.await;

        match status {
            | Ok(s) if s.success() => {},
            | Ok(s) => {
                let code = s.code();
                let _ = tx.send(ExecEvent::BatchCompleted {
                    key: key.clone(),
                    status: BatchStatus::Failed {
                        code,
                        message: format!(
                            "exit {}",
                            code.map(|c| c.to_string()).unwrap_or_else(|| "signal".to_owned())
                        ),
                    },
                    duration_ms: start.elapsed().as_millis() as u64,
                });
                return false;
            },
            | Err(e) => {
                let _ = tx.send(ExecEvent::BatchCompleted {
                    key: key.clone(),
                    status: BatchStatus::Failed {
                        code: None,
                        message: format!("{}", e),
                    },
                    duration_ms: start.elapsed().as_millis() as u64,
                });
                return false;
            },
        }
    }

    let _ = tx.send(ExecEvent::BatchCompleted {
        key,
        status: BatchStatus::Success,
        duration_ms: start.elapsed().as_millis() as u64,
    });
    true
}

async fn wait_cancel(cancel: &Cancel) {
    loop {
        if cancel.tripped() {
            return;
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
}
