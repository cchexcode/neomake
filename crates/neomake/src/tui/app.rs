use {
    crate::exec::{
        BatchInfo,
        BatchKey,
        BatchStatus,
        ExecEvent,
    },
    std::{
        collections::{
            HashMap,
            VecDeque,
        },
        time::Instant,
    },
};

/// Per-batch status tracked for rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BatchState {
    Pending,
    Running,
    Success,
    Failed,
}

pub struct BatchEntry {
    pub info: BatchInfo,
    pub state: BatchState,
    pub duration_ms: Option<u64>,
    pub failure: Option<String>,
    pub logs: VecDeque<LogLine>,
    pub current_command: Option<String>,
}

#[derive(Debug, Clone)]
pub struct LogLine {
    pub stream: LogStream,
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogStream {
    Stdout,
    Stderr,
}

pub struct App {
    /// Every batch in plan order — the render tree is built from this.
    pub order: Vec<BatchKey>,
    pub batches: HashMap<BatchKey, BatchEntry>,
    pub cursor: usize,

    pub log_scroll: u16,
    pub follow: bool,
    pub wave_count: usize,
    pub wave_cursor: usize,
    pub started_at: Instant,
    pub finished_at: Option<Instant>,
    pub failed_batches: usize,
    pub aborted: bool,
    pub exec_finished: bool,

    pub show_help: bool,
    pub quit: bool,
    pub quit_requested: bool,
}

impl App {
    pub fn new() -> Self {
        Self {
            order: Vec::new(),
            batches: HashMap::new(),
            cursor: 0,
            log_scroll: 0,
            follow: true,
            wave_count: 0,
            wave_cursor: 0,
            started_at: Instant::now(),
            finished_at: None,
            failed_batches: 0,
            aborted: false,
            exec_finished: false,
            show_help: false,
            quit: false,
            quit_requested: false,
        }
    }

    pub fn ok(&self) -> bool {
        self.failed_batches == 0 && !self.aborted
    }

    pub fn apply(&mut self, ev: ExecEvent) {
        match ev {
            | ExecEvent::Started { batches } => {
                self.order.clear();
                self.batches.clear();
                self.wave_count = batches.iter().map(|b| b.key.wave).max().unwrap_or(0) + 1;
                for info in batches {
                    self.order.push(info.key.clone());
                    self.batches.insert(info.key.clone(), BatchEntry {
                        info,
                        state: BatchState::Pending,
                        duration_ms: None,
                        failure: None,
                        logs: VecDeque::new(),
                        current_command: None,
                    });
                }
            },
            | ExecEvent::WaveStarted { wave } => {
                self.wave_cursor = wave;
            },
            | ExecEvent::BatchStarted { key, command } => {
                if let Some(b) = self.batches.get_mut(&key) {
                    b.state = BatchState::Running;
                    b.current_command = Some(command);
                }
            },
            | ExecEvent::Stdout { key, line } => {
                if let Some(b) = self.batches.get_mut(&key) {
                    push_capped(&mut b.logs, LogLine {
                        stream: LogStream::Stdout,
                        text: line,
                    });
                }
                if self.follow {
                    self.log_scroll = u16::MAX;
                }
            },
            | ExecEvent::Stderr { key, line } => {
                if let Some(b) = self.batches.get_mut(&key) {
                    push_capped(&mut b.logs, LogLine {
                        stream: LogStream::Stderr,
                        text: line,
                    });
                }
                if self.follow {
                    self.log_scroll = u16::MAX;
                }
            },
            | ExecEvent::BatchCompleted {
                key,
                status,
                duration_ms,
            } => {
                if let Some(b) = self.batches.get_mut(&key) {
                    b.duration_ms = Some(duration_ms);
                    b.current_command = None;
                    match status {
                        | BatchStatus::Success => b.state = BatchState::Success,
                        | BatchStatus::Failed { message, .. } => {
                            b.state = BatchState::Failed;
                            b.failure = Some(message);
                        },
                    }
                }
            },
            | ExecEvent::WaveCompleted { .. } => {},
            | ExecEvent::Finished {
                failed_batches,
                aborted,
            } => {
                self.failed_batches = failed_batches;
                self.aborted = aborted;
                self.finished_at = Some(Instant::now());
                self.exec_finished = true;
                if self.quit_requested {
                    self.quit = true;
                }
            },
        }
    }

    pub fn selected_key(&self) -> Option<&BatchKey> {
        self.order.get(self.cursor)
    }

    pub fn cursor_down(&mut self) {
        if self.cursor + 1 < self.order.len() {
            self.cursor += 1;
            self.log_scroll = if self.follow { u16::MAX } else { 0 };
        }
    }

    pub fn cursor_up(&mut self) {
        if self.cursor > 0 {
            self.cursor -= 1;
            self.log_scroll = if self.follow { u16::MAX } else { 0 };
        }
    }

    pub fn logs_top(&mut self) {
        self.log_scroll = 0;
        self.follow = false;
    }

    pub fn logs_bottom(&mut self) {
        self.log_scroll = u16::MAX;
        self.follow = true;
    }

    pub fn logs_page_down(&mut self) {
        self.follow = false;
        self.log_scroll = self.log_scroll.saturating_add(10);
    }

    pub fn logs_page_up(&mut self) {
        self.follow = false;
        self.log_scroll = self.log_scroll.saturating_sub(10);
    }

    pub fn toggle_follow(&mut self) {
        self.follow = !self.follow;
        if self.follow {
            self.log_scroll = u16::MAX;
        }
    }

    pub fn toggle_help(&mut self) {
        self.show_help = !self.show_help;
    }

    pub fn dismiss_help(&mut self) {
        self.show_help = false;
    }

    pub fn request_quit_after_finish(&mut self) {
        self.quit_requested = true;
        if self.exec_finished {
            self.quit = true;
        }
    }

    pub fn elapsed(&self) -> std::time::Duration {
        self.finished_at.unwrap_or_else(Instant::now) - self.started_at
    }

    pub fn wave_progress(&self) -> (usize, usize) {
        let completed = self
            .batches
            .values()
            .filter(|b| matches!(b.state, BatchState::Success | BatchState::Failed))
            .count();
        (completed, self.order.len())
    }
}

fn push_capped(buf: &mut VecDeque<LogLine>, line: LogLine) {
    const CAP: usize = 5000;
    if buf.len() >= CAP {
        buf.pop_front();
    }
    buf.push_back(line);
}
