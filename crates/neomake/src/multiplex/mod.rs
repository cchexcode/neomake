pub mod tui;

use {
    crate::runner::TuiMode,
    anyhow::{
        anyhow,
        Result,
    },
    jiff::Timestamp,
    notify::{
        RecommendedWatcher,
        Watcher,
    },
    std::{
        collections::BTreeMap,
        io::{
            IsTerminal,
            Write,
        },
        path::Path,
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
            mpsc::{
                unbounded_channel,
                UnboundedReceiver,
                UnboundedSender,
            },
            Semaphore,
        },
        task::JoinSet,
    },
};

/// Static description of a scheduled multiplex task, sent upfront so observers
/// can render the tree before any task starts.
#[derive(Debug, Clone)]
pub struct TaskInfo {
    pub id: usize,
    pub command: String,
}

#[derive(Debug, Clone)]
pub enum TaskStatus {
    Success,
    Failed { code: Option<i32>, message: String },
}

#[derive(Debug)]
pub enum MuxEvent {
    Started {
        tasks: Vec<TaskInfo>,
    },
    TaskStarted {
        id: usize,
    },
    Stdout {
        id: usize,
        line: String,
    },
    Stderr {
        id: usize,
        line: String,
    },
    TaskCompleted {
        id: usize,
        status: TaskStatus,
        duration_ms: u64,
    },
    Finished {
        failed: usize,
        aborted: bool,
    },
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

/// Aggregated result emitted when `--stdout` is given.
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct MultiplexerResult {
    pub metadata: MultiplexerResultMetadata,
    pub tasks: BTreeMap<usize, MultiplexerResultDataTask>,
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct MultiplexerResultMetadata {
    pub started: Timestamp,
    pub ended: Timestamp,
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct MultiplexerResultDataTask {
    pub stdout: String,
}

/// Concurrent, budgeted runner for a batch of shell commands.
///
/// `program` is the shell invocation prefix, e.g. `["/bin/sh", "-c"]`. Each
/// command is appended as the final argv entry. Parallelism is bounded by a
/// tokio `Semaphore`. Progress and output are streamed as `MuxEvent`s on an
/// mpsc channel so drivers (TUI / plain) can observe and render without
/// coupling to process spawning.
pub struct Multiplexer {
    program: Vec<String>,
    commands: Vec<String>,
    parallelism: usize,
    pub cancel: Cancel,
}

impl Multiplexer {
    pub fn new(program: Vec<String>, commands: Vec<String>, parallelism: usize) -> Self {
        Self {
            program,
            commands,
            parallelism: parallelism.max(1),
            cancel: Cancel::new(),
        }
    }

    pub fn with_cancel(mut self, cancel: Cancel) -> Self {
        self.cancel = cancel;
        self
    }

    pub async fn run(self, tx: UnboundedSender<MuxEvent>) -> Result<MultiplexerResult> {
        if self.program.is_empty() {
            return Err(anyhow!("multiplex program is empty"));
        }
        let started = Timestamp::now();

        let tasks: Vec<TaskInfo> = self
            .commands
            .iter()
            .enumerate()
            .map(|(id, command)| {
                TaskInfo {
                    id,
                    command: command.clone(),
                }
            })
            .collect();
        let _ = tx.send(MuxEvent::Started { tasks: tasks.clone() });

        let budget = Arc::new(Semaphore::new(self.parallelism));
        let mut joins = JoinSet::<(usize, bool, String)>::new();

        for info in &tasks {
            if self.cancel.tripped() {
                break;
            }
            let id = info.id;
            let command_str = info.command.clone();
            let program = self.program.clone();
            let tx = tx.clone();
            let cancel = self.cancel.clone();
            let budget = budget.clone();

            joins.spawn(async move {
                let _permit = match budget.acquire_owned().await {
                    | Ok(p) => p,
                    | Err(_) => return (id, false, String::new()),
                };
                if cancel.tripped() {
                    let _ = tx.send(MuxEvent::TaskCompleted {
                        id,
                        status: TaskStatus::Failed {
                            code: None,
                            message: "cancelled".to_owned(),
                        },
                        duration_ms: 0,
                    });
                    return (id, false, String::new());
                }
                run_task(id, program, command_str, tx, cancel).await
            });
        }

        let mut stdout_per_task = BTreeMap::<usize, String>::new();
        let mut failed = 0usize;
        while let Some(res) = joins.join_next().await {
            match res {
                | Ok((id, ok, stdout)) => {
                    stdout_per_task.insert(id, stdout);
                    if !ok {
                        failed += 1;
                    }
                },
                | Err(_) => failed += 1,
            }
        }

        let _ = tx.send(MuxEvent::Finished {
            failed,
            aborted: self.cancel.tripped(),
        });

        let ended = Timestamp::now();
        let mut data = MultiplexerResult {
            metadata: MultiplexerResultMetadata { started, ended },
            tasks: BTreeMap::new(),
        };
        for info in tasks {
            let stdout = stdout_per_task.remove(&info.id).unwrap_or_default();
            data.tasks.insert(info.id, MultiplexerResultDataTask { stdout });
        }
        Ok(data)
    }
}

async fn run_task(
    id: usize,
    program: Vec<String>,
    command_str: String,
    tx: UnboundedSender<MuxEvent>,
    cancel: Cancel,
) -> (usize, bool, String) {
    let start = Instant::now();
    let _ = tx.send(MuxEvent::TaskStarted { id });

    let mut cmd = Command::new(&program[0]);
    for arg in &program[1..] {
        cmd.arg(arg);
    }
    cmd.arg(&command_str)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true);

    let mut child = match cmd.spawn() {
        | Ok(c) => c,
        | Err(e) => {
            let _ = tx.send(MuxEvent::TaskCompleted {
                id,
                status: TaskStatus::Failed {
                    code: None,
                    message: format!("spawn failed: {}", e),
                },
                duration_ms: start.elapsed().as_millis() as u64,
            });
            return (id, false, String::new());
        },
    };

    let stdout = child.stdout.take();
    let stderr = child.stderr.take();
    let out_tx = tx.clone();
    let err_tx = tx.clone();

    let stdout_task = tokio::spawn(async move {
        let mut collected = String::new();
        if let Some(s) = stdout {
            let mut lines = BufReader::new(s).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                collected.push_str(&line);
                collected.push('\n');
                let _ = out_tx.send(MuxEvent::Stdout { id, line });
            }
        }
        collected
    });
    let stderr_task = tokio::spawn(async move {
        if let Some(s) = stderr {
            let mut lines = BufReader::new(s).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                let _ = err_tx.send(MuxEvent::Stderr { id, line });
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
    let stdout_collected = stdout_task.await.unwrap_or_default();
    let _ = stderr_task.await;

    let (ok, outcome) = match status {
        | Ok(s) if s.success() => (true, TaskStatus::Success),
        | Ok(s) => {
            let code = s.code();
            (false, TaskStatus::Failed {
                code,
                message: format!(
                    "exit {}",
                    code.map(|c| c.to_string()).unwrap_or_else(|| "signal".to_owned())
                ),
            })
        },
        | Err(e) => {
            (false, TaskStatus::Failed {
                code: None,
                message: format!("{}", e),
            })
        },
    };

    let _ = tx.send(MuxEvent::TaskCompleted {
        id,
        status: outcome,
        duration_ms: start.elapsed().as_millis() as u64,
    });
    (id, ok, stdout_collected)
}

async fn wait_cancel(cancel: &Cancel) {
    loop {
        if cancel.tripped() {
            return;
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
}

/// Entry point used by `main.rs` for `neomake multiplex`.
///
/// Auto-selects the driver: the TUI when both stdin and stderr are TTYs and
/// `--no-tui` is not set, otherwise the plain streaming driver. In both paths,
/// `print_stdout` triggers the JSON aggregate on stdout at the end.
pub async fn run(
    program: Vec<String>,
    commands: Vec<String>,
    parallelism: Option<usize>,
    print_stdout: bool,
    stdout_pretty: bool,
    tui_mode: TuiMode,
) -> Result<()> {
    let parallelism = parallelism.unwrap_or_else(|| commands.len().max(1));
    let cancel = Cancel::new();
    let (tx, rx) = unbounded_channel::<MuxEvent>();
    let mux = Multiplexer::new(program, commands, parallelism).with_cancel(cancel.clone());
    let engine_handle = tokio::spawn(async move { mux.run(tx).await });

    let use_tui = matches!(tui_mode, TuiMode::Auto) && std::io::stderr().is_terminal();

    if use_tui {
        if let Err(e) = tui::probe_event_reader() {
            eprintln!("TUI disabled ({}); using plain driver.", e);
            run_plain(rx).await?;
        } else {
            tui::run(rx, cancel.clone()).await?;
        }
    } else {
        run_plain(rx).await?;
    }

    let result = match engine_handle.await {
        | Ok(r) => r?,
        | Err(e) => return Err(anyhow!("multiplex engine panicked: {}", e)),
    };
    if print_stdout {
        if stdout_pretty {
            println!("{}", serde_json::to_string_pretty(&result)?);
        } else {
            println!("{}", serde_json::to_string(&result)?);
        }
    }
    Ok(())
}

/// Plain (non-TTY) driver: prints a per-task summary and line-buffered output
/// with `[id]` prefixes.
async fn run_plain(mut rx: UnboundedReceiver<MuxEvent>) -> Result<()> {
    while let Some(ev) = rx.recv().await {
        match ev {
            | MuxEvent::Started { tasks } => {
                for t in &tasks {
                    let _ = writeln!(std::io::stderr(), "● [{}] {}", t.id, short_cmd(&t.command));
                }
            },
            | MuxEvent::TaskStarted { id } => {
                let _ = writeln!(std::io::stderr(), "◐ [{}] started", id);
            },
            | MuxEvent::Stdout { id, line } => {
                let _ = writeln!(std::io::stdout(), "[{}] {}", id, line);
            },
            | MuxEvent::Stderr { id, line } => {
                let _ = writeln!(std::io::stderr(), "[{}] {}", id, line);
            },
            | MuxEvent::TaskCompleted {
                id,
                status,
                duration_ms,
            } => {
                match status {
                    | TaskStatus::Success => {
                        let _ = writeln!(std::io::stderr(), "✓ [{}] ({} ms)", id, duration_ms);
                    },
                    | TaskStatus::Failed { code, message } => {
                        let code = code.map(|c| c.to_string()).unwrap_or_else(|| "?".to_owned());
                        let _ = writeln!(
                            std::io::stderr(),
                            "✗ [{}] [exit {}] {} ({} ms)",
                            id,
                            code,
                            message,
                            duration_ms
                        );
                    },
                }
            },
            | MuxEvent::Finished { failed, aborted } => {
                if aborted {
                    let _ = writeln!(std::io::stderr(), "aborted");
                } else if failed > 0 {
                    let _ = writeln!(std::io::stderr(), "{} task(s) failed", failed);
                }
            },
        }
    }
    Ok(())
}

fn short_cmd(s: &str) -> String {
    let first = s.lines().next().unwrap_or("").trim();
    if first.len() > 80 {
        format!("{}…", &first[..80])
    } else {
        first.to_owned()
    }
}

/// Watch `root` for filesystem events and re-run `commands` via the multiplexer
/// whenever an event matches `filter` (regex over `<event_kind>|<event_path>`).
/// Always uses the plain driver: a re-entering TUI across fs events would be
/// too jarring.
pub async fn watch(
    filter: String,
    root: String,
    program: Vec<String>,
    commands: Vec<String>,
    parallelism: Option<usize>,
) -> Result<()> {
    use signal_hook::{
        consts::{
            SIGINT,
            SIGTERM,
        },
        iterator::Signals,
    };

    let regex = fancy_regex::Regex::new(&filter)?;
    let trim_path = std::fs::canonicalize(&root)?.to_string_lossy().into_owned() + std::path::MAIN_SEPARATOR_STR;

    let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<String>();
    let pll = parallelism.unwrap_or(1);
    tokio::spawn(async move {
        while rx.recv().await.is_some() {
            let (ev_tx, ev_rx) = unbounded_channel::<MuxEvent>();
            let mux = Multiplexer::new(program.clone(), commands.clone(), pll);
            let h = tokio::spawn(async move { mux.run(ev_tx).await });
            let _ = run_plain(ev_rx).await;
            let _ = h.await;
        }
    });

    let mut watcher = RecommendedWatcher::new(
        move |res: Result<notify::Event, notify::Error>| {
            match res {
                | Ok(e) => {
                    let kind = format_event_kind(&e.kind);
                    if let Some(path) = e.paths.first() {
                        let p = path.to_string_lossy();
                        let rel = p.trim_start_matches(&trim_path);
                        let line = format!("{}|{}", kind, rel);
                        if regex.is_match(&line).unwrap_or(false) {
                            let _ = tx.send(line);
                        }
                    }
                },
                | Err(e) => eprintln!("watch error: {:?}", e),
            }
        },
        notify::Config::default(),
    )?;
    watcher.watch(Path::new(&root), notify::RecursiveMode::Recursive)?;
    Signals::new([SIGINT, SIGTERM])?.wait();
    Ok(())
}

fn format_event_kind(k: &notify::EventKind) -> &'static str {
    use notify::{
        event::{
            AccessKind,
            AccessMode,
            CreateKind,
            DataChange,
            MetadataKind,
            ModifyKind,
            RemoveKind,
            RenameMode,
        },
        EventKind::*,
    };
    match k {
        | Create(v) => {
            match v {
                | CreateKind::Any => "created/any",
                | CreateKind::File => "created/file",
                | CreateKind::Folder => "created/folder",
                | CreateKind::Other => "created/other",
            }
        },
        | Modify(v) => {
            match v {
                | ModifyKind::Any => "modified/any",
                | ModifyKind::Data(d) => {
                    match d {
                        | DataChange::Any => "modified/data/any",
                        | DataChange::Size => "modified/data/size",
                        | DataChange::Content => "modified/data/content",
                        | DataChange::Other => "modified/data/other",
                    }
                },
                | ModifyKind::Metadata(m) => {
                    match m {
                        | MetadataKind::Any => "modified/metadata/any",
                        | MetadataKind::AccessTime => "modified/metadata/accesstime",
                        | MetadataKind::WriteTime => "modified/metadata/writetime",
                        | MetadataKind::Permissions => "modified/metadata/permissions",
                        | MetadataKind::Ownership => "modified/metadata/ownership",
                        | MetadataKind::Extended => "modified/metadata/extended",
                        | MetadataKind::Other => "modified/metadata/other",
                    }
                },
                | ModifyKind::Name(r) => {
                    match r {
                        | RenameMode::Any => "modified/name/any",
                        | RenameMode::To => "modified/name/to",
                        | RenameMode::From => "modified/name/from",
                        | RenameMode::Both => "modified/name/both",
                        | RenameMode::Other => "modified/name/other",
                    }
                },
                | ModifyKind::Other => "modified/other",
            }
        },
        | Remove(v) => {
            match v {
                | RemoveKind::Any => "removed/any",
                | RemoveKind::File => "removed/file",
                | RemoveKind::Folder => "removed/folder",
                | RemoveKind::Other => "removed/other",
            }
        },
        | Access(v) => {
            match v {
                | AccessKind::Any => "access/any",
                | AccessKind::Read => "access/read",
                | AccessKind::Open(m) => {
                    match m {
                        | AccessMode::Any => "access/open/any",
                        | AccessMode::Execute => "access/open/execute",
                        | AccessMode::Read => "access/open/read",
                        | AccessMode::Write => "access/open/write",
                        | AccessMode::Other => "access/open/other",
                    }
                },
                | AccessKind::Close(m) => {
                    match m {
                        | AccessMode::Any => "access/close/any",
                        | AccessMode::Execute => "access/close/execute",
                        | AccessMode::Read => "access/close/read",
                        | AccessMode::Write => "access/close/write",
                        | AccessMode::Other => "access/close/other",
                    }
                },
                | AccessKind::Other => "access/other",
            }
        },
        | Other => "other",
        | Any => "any",
    }
}
