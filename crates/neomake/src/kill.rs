use {
    crate::cancel::Cancel,
    std::{
        collections::{
            HashMap,
            HashSet,
        },
        hash::Hash,
        process::ExitStatus,
        sync::{
            atomic::{
                AtomicBool,
                Ordering,
            },
            Arc,
            Mutex,
        },
    },
    tokio::{
        process::Child,
        sync::Notify,
        task::JoinHandle,
    },
};

/// How long `q` / Ctrl+C wait for the engine to notice a hard stop before the
/// TUI closes anyway. The stop itself does not wait.
pub const FORCE_QUIT_AFTER: std::time::Duration = std::time::Duration::from_secs(1);

/// Put `cmd` in its own process group so a later hard stop can signal the
/// whole tree, not just the direct child.
pub fn arm(cmd: &mut tokio::process::Command) {
    #[cfg(unix)]
    {
        cmd.process_group(0);
    }
    #[cfg(not(unix))]
    {
        let _ = cmd;
    }
}

/// SIGKILL the process group led by `pid`, then the pid itself.
///
/// Returns immediately. Does not wait for the process to exit. `pid == 0` is
/// ignored: `kill(0, …)` would signal our own group.
pub fn hard_kill(pid: u32) {
    if pid == 0 {
        return;
    }
    #[cfg(unix)]
    unsafe {
        const SIGKILL: i32 = 9;
        let pid = pid as i32;
        // Negative pid is the process group. Children spawned with `arm`
        // inherit that group unless they call setsid.
        let _ = kill(-pid, SIGKILL);
        let _ = kill(pid, SIGKILL);
    }
}

#[cfg(unix)]
extern "C" {
    fn kill(pid: i32, sig: i32) -> i32;
}

/// Live process groups plus per-key stop requests.
///
/// `stop` and `stop_all` are synchronous. A TUI event loop can call them
/// without waiting on a hung child.
#[derive(Clone)]
pub struct KillBoard<K> {
    inner: Arc<BoardInner<K>>,
}

struct BoardInner<K> {
    requested: Mutex<HashSet<K>>,
    live: Mutex<HashMap<K, u32>>,
    stop_all: AtomicBool,
    notify: Notify,
}

impl<K> Default for KillBoard<K> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K> KillBoard<K> {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(BoardInner {
                requested: Mutex::new(HashSet::new()),
                live: Mutex::new(HashMap::new()),
                stop_all: AtomicBool::new(false),
                notify: Notify::new(),
            }),
        }
    }
}

impl<K> KillBoard<K>
where K: Clone+Eq+Hash
{
    pub fn stop(&self, key: &K) {
        lock(&self.inner.requested).insert(key.clone());
        let pid = lock(&self.inner.live).get(key).copied();
        if let Some(pid) = pid {
            hard_kill(pid);
        }
        self.inner.notify.notify_waiters();
    }

    pub fn stop_all(&self) {
        self.inner.stop_all.store(true, Ordering::SeqCst);
        let pids: Vec<u32> = lock(&self.inner.live).values().copied().collect();
        for pid in pids {
            hard_kill(pid);
        }
        self.inner.notify.notify_waiters();
    }

    pub fn is_stopped(&self, key: &K) -> bool {
        self.inner.stop_all.load(Ordering::SeqCst) || lock(&self.inner.requested).contains(key)
    }

    pub fn all_stopped(&self) -> bool {
        self.inner.stop_all.load(Ordering::SeqCst)
    }

    /// Resolves when `key` has been stopped or `stop_all` has been called.
    /// Safe against the notify/check race: the waiter is registered first.
    pub async fn stopped(&self, key: &K) {
        loop {
            let notified = self.inner.notify.notified();
            tokio::pin!(notified);
            if self.is_stopped(key) {
                return;
            }
            notified.await;
        }
    }

    /// Register a live child. If this key is already stopped, it is killed
    /// before this returns. Dropping the guard unregisters the pid; it does
    /// not signal.
    pub fn attach(&self, key: K, pid: u32) -> LiveChild<K> {
        if pid != 0 {
            lock(&self.inner.live).insert(key.clone(), pid);
            if self.is_stopped(&key) {
                hard_kill(pid);
            }
        }
        LiveChild {
            board: self.clone(),
            key,
            pid,
        }
    }
}

pub struct LiveChild<K: Eq+Hash> {
    board: KillBoard<K>,
    key: K,
    pid: u32,
}

impl<K: Eq+Hash> Drop for LiveChild<K> {
    fn drop(&mut self) {
        if self.pid == 0 {
            return;
        }
        let mut live = lock(&self.board.inner.live);
        if live.get(&self.key) == Some(&self.pid) {
            live.remove(&self.key);
        }
    }
}

/// Shared halt handle for an engine and its TUI.
///
/// Individual stop does not cancel the rest of the run. `stop_all` does.
#[derive(Clone)]
pub struct Control<K> {
    pub cancel: Cancel,
    pub stops: KillBoard<K>,
    forced: Arc<AtomicBool>,
}

impl<K> Control<K> {
    pub fn new(cancel: Cancel) -> Self {
        Self {
            cancel,
            stops: KillBoard::new(),
            forced: Arc::new(AtomicBool::new(false)),
        }
    }

    pub fn forced(&self) -> bool {
        self.forced.load(Ordering::SeqCst)
    }
}

impl<K> Control<K>
where K: Clone+Eq+Hash
{
    /// Hard-stop one task. Other tasks keep running. Does not wait.
    pub fn stop_one(&self, key: &K) {
        self.stops.stop(key);
    }

    /// Hard-stop every live task and cancel work that has not started.
    /// Does not wait.
    pub fn stop_all(&self) {
        self.stops.stop_all();
        self.cancel.trip();
    }

    /// `stop_all`, and tell the driver not to wait for the engine.
    pub fn force_quit(&self) {
        self.stop_all();
        self.forced.store(true, Ordering::SeqCst);
    }

    pub fn halt_message(&self, key: &K) -> &'static str {
        if self.stops.is_stopped(key) {
            "stopped"
        } else {
            "cancelled"
        }
    }
}

pub enum ChildEnd {
    Exited {
        status: std::io::Result<ExitStatus>,
        stdout: String,
    },
    Stopped,
}

/// Wait for `child`, or hard-stop it.
///
/// On stop, output tasks are aborted and the reap is detached. A grandchild
/// holding the pipes, or a child stuck in an uninterruptible sleep, cannot
/// pin the caller.
pub async fn drive_child<K>(
    mut child: Child,
    key: K,
    control: &Control<K>,
    mut stdout_task: JoinHandle<String>,
    mut stderr_task: JoinHandle<()>,
) -> ChildEnd
where
    K: Clone+Eq+Hash,
{
    let pid = child.id().unwrap_or(0);
    let _live = control.stops.attach(key.clone(), pid);

    let waited = tokio::select! {
        biased;
        _ = control.stops.stopped(&key) => None,
        _ = control.cancel.cancelled() => None,
        status = child.wait() => Some(status),
    };

    let Some(status) = waited else {
        detach_child(pid, child, true, &mut stdout_task, &mut stderr_task).await;
        return ChildEnd::Stopped;
    };
    if control.stops.is_stopped(&key) || control.cancel.tripped() {
        detach_child(pid, child, false, &mut stdout_task, &mut stderr_task).await;
        return ChildEnd::Stopped;
    }

    let stdout = tokio::select! {
        biased;
        _ = control.stops.stopped(&key) => {
            detach_child(pid, child, false, &mut stdout_task, &mut stderr_task).await;
            return ChildEnd::Stopped;
        }
        _ = control.cancel.cancelled() => {
            detach_child(pid, child, false, &mut stdout_task, &mut stderr_task).await;
            return ChildEnd::Stopped;
        }
        joined = &mut stdout_task => joined.unwrap_or_default(),
    };

    let stderr_stopped = tokio::select! {
        biased;
        _ = control.stops.stopped(&key) => true,
        _ = control.cancel.cancelled() => true,
        _ = &mut stderr_task => false,
    };
    if stderr_stopped {
        detach_child(pid, child, false, &mut stdout_task, &mut stderr_task).await;
        return ChildEnd::Stopped;
    }

    ChildEnd::Exited { status, stdout }
}

/// Signal the tree and abort pipe readers. When the child has not been waited
/// on, reap it on a detached task so an uninterruptible sleep cannot pin the
/// caller. `start_kill` covers hosts without process-group kill.
async fn detach_child(
    pid: u32,
    mut child: Child,
    needs_reap: bool,
    stdout_task: &mut JoinHandle<String>,
    stderr_task: &mut JoinHandle<()>,
) {
    hard_kill(pid);
    let _ = child.start_kill();
    stdout_task.abort();
    stderr_task.abort();
    let _ = stdout_task.await;
    let _ = stderr_task.await;
    if needs_reap {
        tokio::spawn(async move {
            let _ = tokio::time::timeout(std::time::Duration::from_secs(2), child.wait()).await;
        });
    }
}

/// Install a detached SIGINT/SIGTERM handler.
///
/// The first signal hard-stops every tracked child and cancels the run. A
/// second SIGINT exits even if the runtime is wedged. Uses a plain thread so
/// the tokio runtime does not wait on it at shutdown.
pub fn arm_exit_signals<K>(control: Control<K>)
where K: Clone+Eq+Hash+Send+'static {
    std::thread::spawn(move || {
        use signal_hook::{
            consts::{
                SIGINT,
                SIGTERM,
            },
            iterator::Signals,
        };

        let Ok(mut signals) = Signals::new([SIGINT, SIGTERM]) else {
            return;
        };
        let mut interrupts = 0u8;
        for sig in signals.forever() {
            control.stop_all();
            if sig == SIGINT {
                interrupts = interrupts.saturating_add(1);
                if interrupts >= 2 {
                    restore_terminal();
                    std::process::exit(130);
                }
            }
        }
    });
}

fn restore_terminal() {
    let _ = crossterm::terminal::disable_raw_mode();
    let mut err = std::io::stderr();
    let _ = crossterm::execute!(err, crossterm::terminal::LeaveAlternateScreen);
}

fn lock<T>(mutex: &Mutex<T>) -> std::sync::MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|err| err.into_inner())
}

#[cfg(test)]
mod test {
    use {
        super::{
            hard_kill,
            KillBoard,
        },
        std::{
            path::PathBuf,
            time::Duration,
        },
        tokio::io::AsyncBufReadExt,
    };

    #[test]
    fn stop_is_sticky_and_does_not_block() {
        let board = KillBoard::<&str>::new();
        assert!(!board.is_stopped(&"a"));
        let started = std::time::Instant::now();
        board.stop(&"a");
        board.stop_all();
        assert!(started.elapsed() < Duration::from_millis(50));
        assert!(board.is_stopped(&"a"));
        assert!(board.is_stopped(&"never-seen"));
        assert!(board.all_stopped());
    }

    #[tokio::test]
    async fn stopped_wakes_without_a_poll_loop() {
        let board = KillBoard::<u8>::new();
        let wait = tokio::spawn({
            let board = board.clone();
            async move {
                board.stopped(&7).await;
            }
        });
        tokio::task::yield_now().await;
        board.stop(&7);
        tokio::time::timeout(Duration::from_secs(1), wait)
            .await
            .expect("stop did not wake waiter")
            .expect("waiter panicked");
    }

    #[cfg(unix)]
    struct KillOnDrop(u32);

    #[cfg(unix)]
    impl Drop for KillOnDrop {
        fn drop(&mut self) {
            hard_kill(self.0);
        }
    }

    #[cfg(unix)]
    struct DirGuard(PathBuf);

    #[cfg(unix)]
    impl Drop for DirGuard {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    /// A background subshell keeps writing after the leader is gone unless the
    /// whole process group is signalled.
    #[cfg(unix)]
    #[tokio::test]
    async fn hard_kill_stops_background_subshell() {
        let dir = std::env::temp_dir().join(format!(
            "neomake-kill-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_nanos()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        let _dir = DirGuard(dir.clone());
        let marker = dir.join("tick");
        let script = format!(
            "(while true; do echo x >> '{}'; sleep 0.05; done) &\necho go\nwait\n",
            marker.display()
        );

        let mut cmd = tokio::process::Command::new("/bin/sh");
        cmd.arg("-c")
            .arg(&script)
            .process_group(0)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null())
            .kill_on_drop(true);
        let mut child = cmd.spawn().expect("spawn");
        let pid = child.id().expect("pid");
        let _cleanup = KillOnDrop(pid);

        let stdout = child.stdout.take().expect("stdout");
        let mut lines = tokio::io::BufReader::new(stdout).lines();
        let started = tokio::time::timeout(Duration::from_secs(2), lines.next_line()).await;
        assert!(matches!(started, Ok(Ok(Some(_)))), "leader did not report start");

        let wrote = tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                if std::fs::metadata(&marker).map(|m| m.len()).unwrap_or(0) > 0 {
                    break;
                }
                tokio::time::sleep(Duration::from_millis(20)).await;
            }
        })
        .await;
        assert!(wrote.is_ok(), "background subshell did not write");

        let kill_at = std::time::Instant::now();
        hard_kill(pid);
        assert!(kill_at.elapsed() < Duration::from_millis(200));

        let reaped = tokio::time::timeout(Duration::from_secs(2), child.wait()).await;
        assert!(reaped.is_ok(), "leader was not reaped after hard_kill");

        tokio::time::sleep(Duration::from_millis(200)).await;
        let size = std::fs::metadata(&marker).map(|m| m.len()).unwrap_or(0);
        tokio::time::sleep(Duration::from_millis(300)).await;
        let later = std::fs::metadata(&marker).map(|m| m.len()).unwrap_or(0);
        assert_eq!(size, later, "background subshell kept writing after hard_kill");
    }
}
