use {
    crate::{
        plan::{
            ExecutionPlan,
            ResolvedContainer,
        },
        workflow::{
            Backoff,
            ContainerRuntime,
            ContainerUser,
            RetryPolicy,
        },
    },
    anyhow::{
        anyhow,
        Result,
    },
    parking_lot::Mutex,
    std::{
        collections::{
            BTreeMap,
            HashMap,
        },
        process::Stdio,
        sync::{
            atomic::{
                AtomicBool,
                Ordering,
            },
            Arc,
        },
        time::{
            Duration,
            Instant,
        },
    },
    tokio::{
        io::{
            AsyncBufReadExt,
            AsyncReadExt,
            AsyncWriteExt,
            BufReader,
        },
        process::Command,
        sync::{
            mpsc::UnboundedSender,
            OnceCell,
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

/// Key used for a cell in the inputs-streaming JSON: comma-joined coordinates,
/// empty string for a cell with no coordinates (single-invocation producers).
fn cell_key(cell: &[u8]) -> String {
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
    retry: Option<RetryPolicy>,
    /// Which invocation of the owning node this work belongs to (index into the
    /// node's `invocations` vector).
    invocation_idx: usize,
    /// Fully-resolved container config for this work, or None to run on the
    /// host. Picked as `task.container.or(invocation.container)` in
    /// `enumerate_batches`.
    container: Option<ResolvedContainer>,
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

/// Collects successful per-invocation stdout for each node so downstream
/// consumers (declared via `inputs`) can read it from stdin when they're
/// spawned.
///
/// Keyed by node name → invocation index → accumulated stdout.
#[derive(Clone, Default)]
pub struct OutputCollector(Arc<Mutex<HashMap<String, BTreeMap<usize, String>>>>);

impl OutputCollector {
    pub fn new() -> Self {
        Self::default()
    }

    fn put(&self, node: &str, invocation_idx: usize, stdout: String) {
        let mut guard = self.0.lock();
        guard.entry(node.to_owned()).or_default().insert(invocation_idx, stdout);
    }

    /// Build the stdin JSON blob for a consumer with the given producer list.
    ///
    /// Shape: `{ producer_name: { cell_key: stdout, ... } }` where `cell_key`
    /// is the comma-joined matrix cell coordinates (e.g. `"0,1"`).
    /// Single-invocation producers use the empty string `""` as their key.
    /// Using a dict instead of an array structurally guarantees one entry
    /// per cell — duplicate cells are impossible.
    pub fn build_inputs_json(&self, plan: &ExecutionPlan, inputs: &[String]) -> Result<String> {
        let guard = self.0.lock();
        let mut root = serde_json::Map::<String, serde_json::Value>::new();
        for producer in inputs {
            let pnode = plan
                .nodes
                .get(producer)
                .ok_or_else(|| anyhow!("input producer \"{}\" not in plan", producer))?;
            let collected = guard.get(producer);
            let mut cells = serde_json::Map::<String, serde_json::Value>::new();
            for (inv_idx, invocation) in pnode.invocations.iter().enumerate() {
                let stdout = collected.and_then(|m| m.get(&inv_idx)).cloned().unwrap_or_default();
                let key = cell_key(&invocation.cell);
                cells.insert(key, serde_json::Value::String(stdout));
            }
            root.insert(producer.clone(), serde_json::Value::Object(cells));
        }
        Ok(serde_json::to_string(&serde_json::Value::Object(root))?)
    }
}

/// DAG execution engine. Runs stages sequentially; within a stage each batch is
/// dispatched onto a tokio `JoinSet` with a `Semaphore` capping concurrency at
/// `workers`. All I/O is streamed out via `ExecEvent` so drivers (TUI / plain)
/// can observe and render without coupling to process spawning.
pub struct ExecEngine {
    pub workers: usize,
    pub cancel: Cancel,
    /// Resolver for the container runtime binary; cloned into spawned tasks.
    runtime: RuntimeResolver,
}

/// Cheap-cloneable container-runtime resolver. Caches per-runtime availability
/// so a workflow with many containerized tasks probes each runtime's PATH at
/// most once per engine run.
#[derive(Clone, Default)]
pub struct RuntimeResolver {
    docker_ok: Arc<OnceCell<bool>>,
    podman_ok: Arc<OnceCell<bool>>,
}

impl RuntimeResolver {
    /// Resolve the declared runtime preference to its binary name. Errors when
    /// the declared runtime isn't on PATH — there is no auto-fallback, the
    /// user asked for a specific runtime.
    pub async fn resolve(&self, pref: &ContainerRuntime) -> Result<&'static str> {
        match pref {
            | ContainerRuntime::Docker { .. } => {
                let ok = *self.docker_ok.get_or_init(|| runtime_available("docker")).await;
                if ok {
                    Ok("docker")
                } else {
                    Err(anyhow!("container runtime `docker` not on PATH"))
                }
            },
            | ContainerRuntime::Podman { .. } => {
                let ok = *self.podman_ok.get_or_init(|| runtime_available("podman")).await;
                if ok {
                    Ok("podman")
                } else {
                    Err(anyhow!("container runtime `podman` not on PATH"))
                }
            },
        }
    }
}

impl ExecEngine {
    pub fn new(workers: usize) -> Self {
        Self {
            workers: workers.max(1),
            cancel: Cancel::new(),
            runtime: RuntimeResolver::default(),
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

        let collector = OutputCollector::new();
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

                // If the node has inputs declared, the producers all ran in a prior wave
                // (enforced by the compiler's ordering) so their output is already in the
                // collector. Snapshot the JSON once per batch and pipe it to each spawn.
                let node_inputs = plan
                    .nodes
                    .get(&info.key.node)
                    .map(|n| n.inputs.clone())
                    .unwrap_or_default();
                let stdin_blob = if node_inputs.is_empty() {
                    None
                } else {
                    Some(collector.build_inputs_json(plan, &node_inputs)?)
                };

                let key = info.key.clone();
                let batch = batch.clone();
                let tx = tx.clone();
                let cancel = self.cancel.clone();
                let collector = collector.clone();
                let runtime = self.runtime.clone();
                joins.spawn(async move {
                    let ok = run_batch(runtime, key, batch, stdin_blob, tx, cancel, collector).await;
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
                for (inv_idx, invocation) in node.invocations.iter().enumerate() {
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
                        // Task-level container wins over invocation-level (which already
                        // captured workflow+node+cells merged by the compiler).
                        let container = task.container.clone().or_else(|| invocation.container.clone());
                        works.push(Work {
                            shell,
                            command: task.cmd.clone(),
                            env,
                            workdir,
                            retry: task.retry.clone().or_else(|| node.retry.clone()),
                            invocation_idx: inv_idx,
                            container,
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

async fn run_batch(
    runtime: RuntimeResolver,
    key: BatchKey,
    batch: Vec<Work>,
    stdin_blob: Option<String>,
    tx: UnboundedSender<ExecEvent>,
    cancel: Cancel,
    collector: OutputCollector,
) -> bool {
    let start = Instant::now();
    let mut per_invocation_stdout = BTreeMap::<usize, String>::new();

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

        match run_work_with_retry(&runtime, &key, &work, stdin_blob.as_deref(), &tx, &cancel).await {
            | Ok(stdout) => {
                per_invocation_stdout
                    .entry(work.invocation_idx)
                    .or_default()
                    .push_str(&stdout);
            },
            | Err(failure) => {
                let _ = tx.send(ExecEvent::BatchCompleted {
                    key: key.clone(),
                    status: failure,
                    duration_ms: start.elapsed().as_millis() as u64,
                });
                return false;
            },
        }
    }

    for (inv, s) in per_invocation_stdout {
        collector.put(&key.node, inv, s);
    }

    let _ = tx.send(ExecEvent::BatchCompleted {
        key,
        status: BatchStatus::Success,
        duration_ms: start.elapsed().as_millis() as u64,
    });
    true
}

/// Run a single `Work` honoring its retry policy. Returns the work's captured
/// stdout on success, or a `BatchStatus::Failed` describing the last attempt on
/// exhaustion.
async fn run_work_with_retry(
    runtime: &RuntimeResolver,
    key: &BatchKey,
    work: &Work,
    stdin_blob: Option<&str>,
    tx: &UnboundedSender<ExecEvent>,
    cancel: &Cancel,
) -> std::result::Result<String, BatchStatus> {
    let attempts = work.retry.as_ref().map(|r| r.attempts.max(1)).unwrap_or(1);
    let mut last_failure: Option<BatchStatus> = None;

    for attempt in 1..=attempts {
        if cancel.tripped() {
            return Err(BatchStatus::Failed {
                code: None,
                message: "cancelled".to_owned(),
            });
        }

        match run_one(runtime, key, work, stdin_blob, tx, cancel).await {
            | Ok(stdout) => return Ok(stdout),
            | Err(status) => {
                last_failure = Some(status);
                if attempt < attempts {
                    if let Some(policy) = &work.retry {
                        let delay = backoff_delay(policy, attempt);
                        let _ = tx.send(ExecEvent::Stderr {
                            key: key.clone(),
                            line: format!(
                                "retrying (attempt {}/{}) after {}ms",
                                attempt + 1,
                                attempts,
                                delay.as_millis()
                            ),
                        });
                        tokio::time::sleep(delay).await;
                    }
                }
            },
        }
    }

    Err(last_failure.unwrap_or(BatchStatus::Failed {
        code: None,
        message: "retry budget exhausted".to_owned(),
    }))
}

fn backoff_delay(policy: &RetryPolicy, attempt_number: u32) -> Duration {
    let base = policy.delay_ms;
    let ms = match policy.backoff {
        | Backoff::Fixed => base,
        | Backoff::Linear => base.saturating_mul(attempt_number as u64),
        | Backoff::Exponential => base.saturating_mul(1u64 << (attempt_number.saturating_sub(1).min(16))),
    };
    Duration::from_millis(ms)
}

/// Single spawn-and-wait, no retry. Returns the captured stdout of the child on
/// success. Branches internally on `work.container`: host path inherits env +
/// cwd, container path rewrites the command into `<runtime> run …`.
async fn run_one(
    runtime: &RuntimeResolver,
    key: &BatchKey,
    work: &Work,
    stdin_blob: Option<&str>,
    tx: &UnboundedSender<ExecEvent>,
    cancel: &Cancel,
) -> std::result::Result<String, BatchStatus> {
    let _ = tx.send(ExecEvent::BatchStarted {
        key: key.clone(),
        command: work.command.clone(),
    });

    let mut parts = work.shell.split_whitespace();
    let shell_program = parts.next().ok_or_else(|| {
        BatchStatus::Failed {
            code: None,
            message: "empty shell specification".to_owned(),
        }
    })?;
    let shell_args: Vec<&str> = parts.collect();

    let mut cmd = match &work.container {
        | None => {
            // Host path — unchanged: spawn the user-declared shell directly, inherit env
            // and working directory.
            let mut c = Command::new(shell_program);
            for p in &shell_args {
                c.arg(*p);
            }
            c.envs(&work.env);
            if let Some(wd) = &work.workdir {
                c.current_dir(wd);
            }
            c.arg(&work.command);
            c
        },
        | Some(container) => {
            // Container path — rewrite into `<runtime> run --rm -i [-v host:cwd] -w wd
            // [--user U:G] [-e K=V …] [extras…] <image> <shell> <shell_args…> <command>`.
            let runtime_bin = runtime.resolve(&container.runtime).await.map_err(|e| {
                BatchStatus::Failed {
                    code: None,
                    message: format!("{}", e),
                }
            })?;
            build_container_command(runtime_bin, container, work, shell_program, &shell_args)
        },
    };
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).kill_on_drop(true);
    if stdin_blob.is_some() {
        cmd.stdin(Stdio::piped());
    } else {
        cmd.stdin(Stdio::null());
    }

    let mut child = cmd.spawn().map_err(|e| {
        BatchStatus::Failed {
            code: None,
            message: format!("spawn failed: {}", e),
        }
    })?;

    if let (Some(blob), Some(mut stdin)) = (stdin_blob, child.stdin.take()) {
        let bytes = blob.as_bytes().to_vec();
        let _ = tokio::spawn(async move {
            let _ = stdin.write_all(&bytes).await;
            let _ = stdin.shutdown().await;
        });
    }

    let stdout = child.stdout.take();
    let stderr = child.stderr.take();
    let out_tx = tx.clone();
    let err_tx = tx.clone();
    let out_key = key.clone();
    let err_key = key.clone();

    // stdout — read fully into a buffer (so we can hand it to `inputs` consumers)
    // while simultaneously emitting each line as an event for the driver.
    let stdout_task = tokio::spawn(async move {
        let mut collected = String::new();
        if let Some(mut s) = stdout {
            let mut raw = Vec::<u8>::new();
            if s.read_to_end(&mut raw).await.is_ok() {
                collected.push_str(&String::from_utf8_lossy(&raw));
                for line in collected.lines() {
                    let _ = out_tx.send(ExecEvent::Stdout {
                        key: out_key.clone(),
                        line: line.to_owned(),
                    });
                }
            }
        }
        collected
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
        _ = wait_cancel(cancel) => {
            let _ = child.kill().await;
            let _ = child.wait().await;
            Err(std::io::Error::other("cancelled"))
        },
    };
    let collected_stdout = stdout_task.await.unwrap_or_default();
    let _ = stderr_task.await;

    match status {
        | Ok(s) if s.success() => Ok(collected_stdout),
        | Ok(s) => {
            let code = s.code();
            Err(BatchStatus::Failed {
                code,
                message: format!(
                    "exit {}",
                    code.map(|c| c.to_string()).unwrap_or_else(|| "signal".to_owned())
                ),
            })
        },
        | Err(e) => {
            Err(BatchStatus::Failed {
                code: None,
                message: format!("{}", e),
            })
        },
    }
}

async fn wait_cancel(cancel: &Cancel) {
    loop {
        if cancel.tripped() {
            return;
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
}

/// Probe the host for a container runtime binary by invoking `<bin> --version`.
async fn runtime_available(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .await
        .map(|s| s.success())
        .unwrap_or(false)
}

/// Build the `<runtime> run …` command for a container-bound task. Env goes via
/// `-e` flags, workdir via `-w`, CWD via `-v` when `mount` is true.
/// Runtime-specific extras come from the matching sub-block (`docker` or
/// `podman`) in the resolved config.
fn build_container_command(
    runtime_bin: &str,
    container: &ResolvedContainer,
    work: &Work,
    shell_program: &str,
    shell_args: &[&str],
) -> Command {
    let mut c = Command::new(runtime_bin);
    c.arg("run").arg("--rm").arg("-i");

    // Workdir inside the container. Task-declared workdir overrides the resolved
    // default.
    let workdir = work.workdir.as_deref().unwrap_or(&container.workdir);
    c.arg("-w").arg(workdir);

    // Bind-mount the host CWD onto the container workdir. Use the captured CWD or
    // fall back to "." (docker will canonicalize).
    if container.mount {
        let host_cwd = std::env::current_dir()
            .map(|p| p.to_string_lossy().into_owned())
            .unwrap_or_else(|_| ".".to_owned());
        c.arg("-v").arg(format!("{}:{}", host_cwd, container.workdir));
    }

    match &container.user {
        | ContainerUser::Host => {
            // Match the host's primary UID:GID so container writes into the mount keep
            // correct ownership on the host side.
            let uid = unsafe { libc_getuid() };
            let gid = unsafe { libc_getgid() };
            c.arg("--user").arg(format!("{}:{}", uid, gid));
        },
        | ContainerUser::Root => {},
        | ContainerUser::Explicit(spec) => {
            c.arg("--user").arg(spec);
        },
    }

    for (k, v) in &work.env {
        c.arg("-e").arg(format!("{}={}", k, v));
    }

    // Runtime-specific extras come from the nested enum;
    // `container.runtime.extras()` is always the matching variant (we never
    // mis-apply docker extras to a podman run).
    let _ = runtime_bin; // runtime_bin is chosen by resolve; extras come from the enum itself.
    for a in &container.runtime.extras().extra_args {
        c.arg(a);
    }

    c.arg(&container.image);
    c.arg(shell_program);
    for p in shell_args {
        c.arg(*p);
    }
    c.arg(&work.command);
    c
}

// Thin libc wrappers so we don't need the `libc` crate. `getuid`/`getgid` on
// Linux/macOS are guaranteed-safe; they never fail and only read the process's
// own credentials.
extern "C" {
    fn getuid() -> u32;
    fn getgid() -> u32;
}
#[inline]
unsafe fn libc_getuid() -> u32 {
    getuid()
}
#[inline]
unsafe fn libc_getgid() -> u32 {
    getgid()
}
