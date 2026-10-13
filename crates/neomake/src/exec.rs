use {
    crate::{
        cancel::Cancel,
        kill::{
            arm,
            drive_child,
            ChildEnd,
            Control,
        },
        plan::{
            ExecutionPlan,
            PlannedTask,
            ResolvedContainer,
            ShellSpec,
        },
        workflow::{
            ContainerRuntime,
            ContainerUser,
            RetryPolicy,
        },
    },
    anyhow::{
        anyhow,
        bail,
        Context,
        Result,
    },
    std::{
        collections::{
            BTreeMap,
            HashMap,
        },
        path::PathBuf,
        process::Stdio,
        sync::{
            Arc,
            Mutex,
        },
        time::Instant,
    },
    tokio::{
        io::{
            AsyncBufReadExt,
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

/// One unit of work dispatched onto the worker pool.
///
/// A batch is the scheduling granularity. `parallel = true` with more than one
/// invocation makes each invocation its own batch. Otherwise every invocation
/// of the node runs sequentially in one batch.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BatchKey {
    pub wave: usize,
    pub node: String,
    pub index: usize,
}

impl BatchKey {
    pub fn label(&self, info: &BatchInfo) -> String {
        let cells: Vec<_> = info.invocations.iter().filter(|cell| !cell.is_empty()).collect();
        match cells.as_slice() {
            | [] => self.node.clone(),
            | [cell] => format!("{}[{}]", self.node, fmt_cell(cell)),
            | many => format!("{}[×{}]", self.node, many.len()),
        }
    }
}

fn fmt_cell(cell: &[u32]) -> String {
    cell.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",")
}

/// Key for one cell in the inputs JSON. Empty string for a non-matrix producer.
fn cell_key(cell: &[u32]) -> String {
    fmt_cell(cell)
}

/// Static description of a scheduled batch, sent before any task starts.
#[derive(Debug, Clone)]
pub struct BatchInfo {
    pub key: BatchKey,
    pub invocations: Vec<Vec<u32>>,
}

#[derive(Debug, Clone)]
pub enum BatchStatus {
    Success,
    Failed { code: Option<i32>, message: String },
}

#[derive(Debug, Clone)]
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
    shell: ShellSpec,
    command: String,
    env: BTreeMap<String, String>,
    workdir: Option<String>,
    retry: Option<RetryPolicy>,
    invocation_idx: usize,
    container: Option<ResolvedContainer>,
}

impl Work {
    fn from_task(task: &PlannedTask, invocation_idx: usize) -> Self {
        Self {
            shell: task.shell.clone(),
            command: task.cmd.clone(),
            env: task.env.clone(),
            workdir: task.workdir.clone(),
            retry: task.retry.clone(),
            invocation_idx,
            container: task.container.clone(),
        }
    }
}

/// Process identity captured once at engine construction.
///
/// Container commands must not read the cwd or uid later — a task that changes
/// directory, or a concurrent caller, must not change the mount identity.
#[derive(Clone)]
struct HostContext {
    cwd: PathBuf,
    uid: u32,
    gid: u32,
}

impl HostContext {
    fn capture() -> Result<Self> {
        Ok(Self {
            cwd: std::env::current_dir().context("failed to read current directory")?,
            // getuid/getgid only read this process's credentials and cannot fail.
            uid: unsafe { getuid() },
            gid: unsafe { getgid() },
        })
    }
}

/// Successful per-invocation stdout, keyed by node then invocation index, so
/// `inputs` consumers can read it on stdin.
#[derive(Clone, Default)]
pub struct OutputCollector(Arc<Mutex<HashMap<String, BTreeMap<usize, String>>>>);

impl OutputCollector {
    pub fn new() -> Self {
        Self::default()
    }

    fn put(&self, node: &str, invocation_idx: usize, stdout: String) {
        // A panicked holder should not sink the rest of the wave.
        let mut guard = self.0.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        guard.entry(node.to_owned()).or_default().insert(invocation_idx, stdout);
    }

    /// `{ producer: { cell_key: stdout } }`. `cell_key` is comma-joined
    /// coordinates, or `""` for a non-matrix producer. A map, not an array, so
    /// each cell has exactly one entry.
    pub fn build_inputs_json(&self, plan: &ExecutionPlan, inputs: &[String]) -> Result<String> {
        let guard = self.0.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        let mut root = serde_json::Map::<String, serde_json::Value>::new();
        for producer in inputs {
            let node = plan
                .nodes
                .get(producer)
                .ok_or_else(|| anyhow!("input producer \"{producer}\" not in plan"))?;
            let collected = guard.get(producer);
            let mut cells = serde_json::Map::<String, serde_json::Value>::new();
            for (inv_idx, invocation) in node.invocations.iter().enumerate() {
                let stdout = collected.and_then(|m| m.get(&inv_idx)).cloned().unwrap_or_default();
                cells.insert(cell_key(&invocation.cell), serde_json::Value::String(stdout));
            }
            root.insert(producer.clone(), serde_json::Value::Object(cells));
        }
        Ok(serde_json::to_string(&serde_json::Value::Object(root))?)
    }
}

/// DAG executor. Waves run in order. Batches inside a wave share a semaphore
/// of size `workers`. Observation is an `ExecEvent` stream; this type does not
/// print.
pub struct ExecEngine {
    workers: usize,
    control: Control<BatchKey>,
    runtime: RuntimeResolver,
    host: HostContext,
}

/// Caches per-runtime PATH probes so each binary is checked at most once.
#[derive(Clone, Default)]
pub struct RuntimeResolver {
    docker_ok: Arc<OnceCell<bool>>,
    podman_ok: Arc<OnceCell<bool>>,
}

impl RuntimeResolver {
    /// The declared runtime, or an error if it is not on PATH. No fallback.
    pub async fn resolve(&self, pref: &ContainerRuntime) -> Result<&'static str> {
        match pref {
            | ContainerRuntime::Docker { .. } => {
                let ok = *self.docker_ok.get_or_init(|| runtime_available("docker")).await;
                if ok {
                    Ok("docker")
                } else {
                    bail!("container runtime `docker` not on PATH")
                }
            },
            | ContainerRuntime::Podman { .. } => {
                let ok = *self.podman_ok.get_or_init(|| runtime_available("podman")).await;
                if ok {
                    Ok("podman")
                } else {
                    bail!("container runtime `podman` not on PATH")
                }
            },
        }
    }
}

impl ExecEngine {
    pub fn new(workers: usize) -> Result<Self> {
        Ok(Self {
            workers: workers.max(1),
            control: Control::new(Cancel::new()),
            runtime: RuntimeResolver::default(),
            host: HostContext::capture()?,
        })
    }

    pub fn with_control(mut self, control: Control<BatchKey>) -> Self {
        self.control = control;
        self
    }

    pub async fn run(self, plan: &ExecutionPlan, tx: UnboundedSender<ExecEvent>) -> Result<bool> {
        if plan.version != env!("CARGO_PKG_VERSION") {
            bail!(
                "execution plan version {} does not match neomake {}; re-run `neomake plan`",
                plan.version,
                env!("CARGO_PKG_VERSION")
            );
        }
        match self.run_inner(plan, &tx).await {
            | Ok((failed, aborted)) => {
                let _ = tx.send(ExecEvent::Finished {
                    failed_batches: failed,
                    aborted,
                });
                Ok(failed == 0 && !aborted)
            },
            | Err(err) => {
                let _ = tx.send(ExecEvent::Finished {
                    failed_batches: 1,
                    aborted: true,
                });
                Err(err)
            },
        }
    }

    async fn run_inner(&self, plan: &ExecutionPlan, tx: &UnboundedSender<ExecEvent>) -> Result<(usize, bool)> {
        let all_batches = self.enumerate_batches(plan)?;
        let infos = all_batches.iter().map(|(info, _)| info.clone()).collect::<Vec<_>>();
        let _ = tx.send(ExecEvent::Started { batches: infos });

        let collector = OutputCollector::new();
        let mut failed_batches = 0usize;
        let mut aborted = false;

        for (wave_idx, stage) in plan.stages.iter().enumerate() {
            if self.control.cancel.tripped() || self.control.stops.all_stopped() {
                aborted = true;
                break;
            }
            let _ = tx.send(ExecEvent::WaveStarted { wave: wave_idx });

            let mut scheduled = Vec::new();
            for (info, batch) in all_batches.iter().filter(|(info, _)| info.key.wave == wave_idx) {
                if !stage.nodes.contains(&info.key.node) {
                    continue;
                }
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
                scheduled.push((info.clone(), batch.clone(), stdin_blob));
            }

            let mut joins = JoinSet::<bool>::new();
            let sem = Arc::new(Semaphore::new(self.workers));
            for (info, batch, stdin_blob) in scheduled {
                let sem = sem.clone();
                let key = info.key.clone();
                let tx = tx.clone();
                let control = self.control.clone();
                let collector = collector.clone();
                let runtime = self.runtime.clone();
                let host = self.host.clone();
                joins.spawn(async move {
                    let stop_key = key.clone();
                    let permit = tokio::select! {
                        biased;
                        _ = control.cancel.cancelled() => {
                            emit_halted(&tx, key, &control);
                            return false;
                        }
                        _ = control.stops.stopped(&stop_key) => {
                            emit_halted(&tx, key, &control);
                            return false;
                        }
                        acquired = sem.acquire_owned() => acquired,
                    };
                    let Ok(_permit) = permit else {
                        emit_halted(&tx, key, &control);
                        return false;
                    };
                    if control.cancel.tripped() || control.stops.is_stopped(&key) {
                        emit_halted(&tx, key, &control);
                        return false;
                    }
                    run_batch(
                        RunCtx {
                            host,
                            runtime,
                            tx,
                            control,
                            collector,
                        },
                        key,
                        batch,
                        stdin_blob,
                    )
                    .await
                });
            }

            let mut wave_failures = 0usize;
            while let Some(res) = joins.join_next().await {
                if !matches!(res, Ok(true)) {
                    wave_failures += 1;
                }
            }
            let _ = tx.send(ExecEvent::WaveCompleted {
                wave: wave_idx,
                failures: wave_failures,
            });
            failed_batches += wave_failures;
            if self.control.cancel.tripped() || self.control.stops.all_stopped() {
                aborted = true;
            }
            if wave_failures > 0 || aborted {
                break;
            }
        }
        Ok((failed_batches, aborted))
    }

    fn enumerate_batches(&self, plan: &ExecutionPlan) -> Result<Vec<(BatchInfo, Vec<Work>)>> {
        let mut out = Vec::new();
        for (wave_idx, stage) in plan.stages.iter().enumerate() {
            for node_name in &stage.nodes {
                let node = plan
                    .nodes
                    .get(node_name)
                    .ok_or_else(|| anyhow!("plan missing node: {node_name}"))?;
                let parallel = node.parallel && node.invocations.len() > 1;
                if parallel {
                    for (index, invocation) in node.invocations.iter().enumerate() {
                        let works = invocation
                            .tasks
                            .iter()
                            .map(|task| Work::from_task(task, index))
                            .collect();
                        out.push((
                            BatchInfo {
                                key: BatchKey {
                                    wave: wave_idx,
                                    node: node_name.clone(),
                                    index,
                                },
                                invocations: vec![invocation.cell.clone()],
                            },
                            works,
                        ));
                    }
                } else {
                    let mut works = Vec::new();
                    let mut cells = Vec::new();
                    for (index, invocation) in node.invocations.iter().enumerate() {
                        cells.push(invocation.cell.clone());
                        works.extend(invocation.tasks.iter().map(|task| Work::from_task(task, index)));
                    }
                    out.push((
                        BatchInfo {
                            key: BatchKey {
                                wave: wave_idx,
                                node: node_name.clone(),
                                index: 0,
                            },
                            invocations: if cells.is_empty() { vec![vec![]] } else { cells },
                        },
                        works,
                    ));
                }
            }
        }
        Ok(out)
    }
}

fn emit_halted(tx: &UnboundedSender<ExecEvent>, key: BatchKey, control: &Control<BatchKey>) {
    let _ = tx.send(ExecEvent::BatchCompleted {
        key: key.clone(),
        status: BatchStatus::Failed {
            code: None,
            message: control.halt_message(&key).to_owned(),
        },
        duration_ms: 0,
    });
}

struct RunCtx {
    host: HostContext,
    runtime: RuntimeResolver,
    tx: UnboundedSender<ExecEvent>,
    control: Control<BatchKey>,
    collector: OutputCollector,
}

async fn run_batch(ctx: RunCtx, key: BatchKey, batch: Vec<Work>, stdin_blob: Option<String>) -> bool {
    let RunCtx {
        host,
        runtime,
        tx,
        control,
        collector,
    } = ctx;
    let start = Instant::now();
    let mut per_invocation_stdout = BTreeMap::<usize, String>::new();

    for work in batch {
        if control.cancel.tripped() || control.stops.is_stopped(&key) {
            emit_halted(&tx, key, &control);
            return false;
        }
        match run_work_with_retry(&host, &runtime, &key, &work, stdin_blob.as_deref(), &tx, &control).await {
            | Ok(stdout) => {
                per_invocation_stdout
                    .entry(work.invocation_idx)
                    .or_default()
                    .push_str(&stdout);
            },
            | Err(failure) => {
                let _ = tx.send(ExecEvent::BatchCompleted {
                    key,
                    status: failure,
                    duration_ms: elapsed_ms(start),
                });
                return false;
            },
        }
    }

    for (inv, stdout) in per_invocation_stdout {
        collector.put(&key.node, inv, stdout);
    }
    let _ = tx.send(ExecEvent::BatchCompleted {
        key,
        status: BatchStatus::Success,
        duration_ms: elapsed_ms(start),
    });
    true
}

fn elapsed_ms(start: Instant) -> u64 {
    u64::try_from(start.elapsed().as_millis()).unwrap_or(u64::MAX)
}

/// Run one work item, honoring its retry policy. Returns captured stdout.
async fn run_work_with_retry(
    host: &HostContext,
    runtime: &RuntimeResolver,
    key: &BatchKey,
    work: &Work,
    stdin_blob: Option<&str>,
    tx: &UnboundedSender<ExecEvent>,
    control: &Control<BatchKey>,
) -> std::result::Result<String, BatchStatus> {
    let attempts = work.retry.as_ref().map(|policy| policy.attempts.max(1)).unwrap_or(1);
    let mut last_failure = None;

    for attempt in 1..=attempts {
        if control.cancel.tripped() || control.stops.is_stopped(key) {
            return Err(BatchStatus::Failed {
                code: None,
                message: control.halt_message(key).to_owned(),
            });
        }
        match run_one(host, runtime, key, work, stdin_blob, tx, control).await {
            | Ok(stdout) => return Ok(stdout),
            | Err(status) => {
                last_failure = Some(status);
                if control.cancel.tripped() || control.stops.is_stopped(key) {
                    return Err(BatchStatus::Failed {
                        code: None,
                        message: control.halt_message(key).to_owned(),
                    });
                }
                if attempt < attempts {
                    if let Some(policy) = &work.retry {
                        let delay = policy.delay_after(attempt);
                        let _ = tx.send(ExecEvent::Stderr {
                            key: key.clone(),
                            line: format!(
                                "retrying (attempt {}/{attempts}) after {}ms",
                                attempt + 1,
                                delay.as_millis()
                            ),
                        });
                        tokio::select! {
                            biased;
                            _ = control.stops.stopped(key) => {
                                return Err(BatchStatus::Failed {
                                    code: None,
                                    message: "stopped".to_owned(),
                                });
                            }
                            _ = control.cancel.cancelled() => {
                                return Err(BatchStatus::Failed {
                                    code: None,
                                    message: control.halt_message(key).to_owned(),
                                });
                            }
                            _ = tokio::time::sleep(delay) => {}
                        }
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

/// Spawn one command and wait. Host tasks inherit env and cwd. Container tasks
/// are rewritten to `<runtime> run …` using the plan's resolved container.
async fn run_one(
    host: &HostContext,
    runtime: &RuntimeResolver,
    key: &BatchKey,
    work: &Work,
    stdin_blob: Option<&str>,
    tx: &UnboundedSender<ExecEvent>,
    control: &Control<BatchKey>,
) -> std::result::Result<String, BatchStatus> {
    let _ = tx.send(ExecEvent::BatchStarted {
        key: key.clone(),
        command: work.command.clone(),
    });

    let (shell_program, shell_args) = work.shell.split().map_err(|err| {
        BatchStatus::Failed {
            code: None,
            message: err.to_string(),
        }
    })?;

    let mut cmd = match &work.container {
        | None => {
            let mut cmd = Command::new(shell_program);
            for arg in &shell_args {
                cmd.arg(*arg);
            }
            cmd.envs(&work.env);
            if let Some(workdir) = &work.workdir {
                cmd.current_dir(workdir);
            }
            cmd.arg(&work.command);
            cmd
        },
        | Some(container) => {
            let runtime_bin = runtime.resolve(&container.runtime).await.map_err(|err| {
                BatchStatus::Failed {
                    code: None,
                    message: err.to_string(),
                }
            })?;
            build_container_command(runtime_bin, container, work, host, shell_program, &shell_args)
        },
    };
    arm(&mut cmd);
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).kill_on_drop(true);
    if stdin_blob.is_some() {
        cmd.stdin(Stdio::piped());
    } else {
        cmd.stdin(Stdio::null());
    }

    let mut child = cmd.spawn().map_err(|err| {
        BatchStatus::Failed {
            code: None,
            message: format!("spawn failed: {err}"),
        }
    })?;

    if let (Some(blob), Some(mut stdin)) = (stdin_blob, child.stdin.take()) {
        let bytes = blob.as_bytes().to_vec();
        tokio::spawn(async move {
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

    let stdout_task = tokio::spawn(async move { collect_stdout(stdout, out_tx, out_key).await });
    let stderr_task = tokio::spawn(async move {
        if let Some(stderr) = stderr {
            let mut lines = BufReader::new(stderr).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                let _ = err_tx.send(ExecEvent::Stderr {
                    key: err_key.clone(),
                    line,
                });
            }
        }
    });

    match drive_child(child, key.clone(), control, stdout_task, stderr_task).await {
        | ChildEnd::Stopped => {
            Err(BatchStatus::Failed {
                code: None,
                message: "stopped".to_owned(),
            })
        },
        | ChildEnd::Exited { status, stdout } => {
            match status {
                | Ok(status) if status.success() => Ok(stdout),
                | Ok(status) => {
                    let code = status.code();
                    Err(BatchStatus::Failed {
                        code,
                        message: format!(
                            "exit {}",
                            code.map(|c| c.to_string()).unwrap_or_else(|| "signal".to_owned())
                        ),
                    })
                },
                | Err(err) => {
                    Err(BatchStatus::Failed {
                        code: None,
                        message: err.to_string(),
                    })
                },
            }
        },
    }
}

/// Stream stdout line-by-line for the driver and keep the raw bytes for
/// `inputs`.
async fn collect_stdout(
    stdout: Option<tokio::process::ChildStdout>,
    tx: UnboundedSender<ExecEvent>,
    key: BatchKey,
) -> String {
    let Some(stdout) = stdout else {
        return String::new();
    };
    let mut reader = BufReader::new(stdout);
    let mut buf = Vec::new();
    let mut collected = Vec::new();
    loop {
        buf.clear();
        match reader.read_until(b'\n', &mut buf).await {
            | Ok(0) => break,
            | Ok(_) => emit_stdout_chunk(&buf, &mut collected, &tx, &key),
            | Err(_) => {
                if !buf.is_empty() {
                    emit_stdout_chunk(&buf, &mut collected, &tx, &key);
                }
                break;
            },
        }
    }
    String::from_utf8_lossy(&collected).into_owned()
}

fn emit_stdout_chunk(buf: &[u8], collected: &mut Vec<u8>, tx: &UnboundedSender<ExecEvent>, key: &BatchKey) {
    collected.extend_from_slice(buf);
    let text = String::from_utf8_lossy(buf);
    let line = text.trim_end_matches(['\r', '\n']);
    let _ = tx.send(ExecEvent::Stdout {
        key: key.clone(),
        line: line.to_owned(),
    });
}

async fn runtime_available(bin: &str) -> bool {
    Command::new(bin)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .await
        .map(|status| status.success())
        .unwrap_or(false)
}

/// `<runtime> run --rm -i [-v host:workdir] -w workdir [--user U:G] [-e K=V …]
/// [extras…] image shell… command`
fn build_container_command(
    runtime_bin: &str,
    container: &ResolvedContainer,
    work: &Work,
    host: &HostContext,
    shell_program: &str,
    shell_args: &[&str],
) -> Command {
    let mut cmd = Command::new(runtime_bin);
    cmd.arg("run").arg("--rm").arg("-i");

    let workdir = work.workdir.as_deref().unwrap_or(&container.workdir);
    cmd.arg("-w").arg(workdir);

    if container.mount {
        cmd.arg("-v")
            .arg(format!("{}:{}", host.cwd.display(), container.workdir));
    }

    match &container.user {
        | ContainerUser::Host => {
            cmd.arg("--user").arg(format!("{}:{}", host.uid, host.gid));
        },
        | ContainerUser::Root => {},
        | ContainerUser::Explicit(spec) => {
            cmd.arg("--user").arg(spec);
        },
    }

    for (key, value) in &work.env {
        cmd.arg("-e").arg(format!("{key}={value}"));
    }
    for arg in &container.runtime.extras().extra_args {
        cmd.arg(arg);
    }

    cmd.arg(&container.image);
    cmd.arg(shell_program);
    for arg in shell_args {
        cmd.arg(*arg);
    }
    cmd.arg(&work.command);
    cmd
}

extern "C" {
    fn getuid() -> u32;
    fn getgid() -> u32;
}

#[cfg(all(test, unix))]
mod test {
    use {
        super::{
            ExecEngine,
            ExecEvent,
        },
        crate::{
            cancel::Cancel,
            kill::Control,
            plan::{
                ExecutionPlan,
                PlannedInvocation,
                PlannedNode,
                PlannedTask,
                ShellSpec,
                Stage,
            },
        },
        std::{
            collections::BTreeMap,
            time::Duration,
        },
        tokio::sync::mpsc::unbounded_channel,
    };

    fn hang_plan() -> ExecutionPlan {
        ExecutionPlan {
            version: env!("CARGO_PKG_VERSION").to_owned(),
            stages: vec![Stage {
                nodes: vec!["hang".to_owned()],
            }],
            nodes: BTreeMap::from([("hang".to_owned(), PlannedNode {
                parallel: false,
                deps: Vec::new(),
                inputs: Vec::new(),
                invocations: vec![PlannedInvocation {
                    cell: Vec::new(),
                    tasks: vec![PlannedTask {
                        cmd: "sleep 60".to_owned(),
                        shell: ShellSpec::sh(),
                        env: BTreeMap::new(),
                        workdir: None,
                        retry: None,
                        container: None,
                    }],
                }],
            })]),
        }
    }

    #[tokio::test]
    async fn stop_one_unblocks_a_hung_batch() {
        let control = Control::new(Cancel::new());
        let engine = ExecEngine::new(1).expect("engine").with_control(control.clone());
        let plan = hang_plan();
        let (tx, mut rx) = unbounded_channel();
        let handle = tokio::spawn(async move { engine.run(&plan, tx).await });

        let key = loop {
            match rx.recv().await {
                | Some(ExecEvent::BatchStarted { key, .. }) => break key,
                | Some(ExecEvent::Finished { .. }) => panic!("finished before the batch started"),
                | Some(_) => {},
                | None => panic!("engine closed the event channel"),
            }
        };
        control.stop_one(&key);

        let joined = tokio::time::timeout(Duration::from_secs(3), handle)
            .await
            .expect("engine stayed blocked after stop");
        let ok = joined.expect("engine panicked").expect("engine error");
        assert!(!ok);
    }
}
