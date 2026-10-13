use {
    crate::multiplex::{
        MuxEvent,
        TaskInfo,
        TaskStatus,
    },
    std::{
        collections::{
            HashMap,
            VecDeque,
        },
        time::Instant,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskState {
    Pending,
    Running,
    Success,
    Failed,
}

pub struct TaskEntry {
    pub info: TaskInfo,
    pub state: TaskState,
    pub duration_ms: Option<u64>,
    pub failure: Option<String>,
    pub logs: VecDeque<LogLine>,
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
    pub order: Vec<usize>,
    pub tasks: HashMap<usize, TaskEntry>,
    pub cursor: usize,

    pub log_scroll: u16,
    pub follow: bool,
    pub started_at: Instant,
    pub finished_at: Option<Instant>,
    pub failed: usize,
    pub aborted: bool,
    pub engine_finished: bool,

    pub show_help: bool,
    pub quit: bool,
    pub quit_requested: bool,
}

impl App {
    pub fn new() -> Self {
        Self {
            order: Vec::new(),
            tasks: HashMap::new(),
            cursor: 0,
            log_scroll: 0,
            follow: true,
            started_at: Instant::now(),
            finished_at: None,
            failed: 0,
            aborted: false,
            engine_finished: false,
            show_help: false,
            quit: false,
            quit_requested: false,
        }
    }

    pub fn apply(&mut self, ev: MuxEvent) {
        match ev {
            | MuxEvent::Started { tasks } => {
                self.order.clear();
                self.tasks.clear();
                for info in tasks {
                    self.order.push(info.id);
                    self.tasks.insert(info.id, TaskEntry {
                        info,
                        state: TaskState::Pending,
                        duration_ms: None,
                        failure: None,
                        logs: VecDeque::new(),
                    });
                }
            },
            | MuxEvent::TaskStarted { id } => {
                if let Some(t) = self.tasks.get_mut(&id) {
                    t.state = TaskState::Running;
                }
            },
            | MuxEvent::Stdout { id, line } => {
                if let Some(t) = self.tasks.get_mut(&id) {
                    push_capped(&mut t.logs, LogLine {
                        stream: LogStream::Stdout,
                        text: line,
                    });
                }
                if self.follow {
                    self.log_scroll = u16::MAX;
                }
            },
            | MuxEvent::Stderr { id, line } => {
                if let Some(t) = self.tasks.get_mut(&id) {
                    push_capped(&mut t.logs, LogLine {
                        stream: LogStream::Stderr,
                        text: line,
                    });
                }
                if self.follow {
                    self.log_scroll = u16::MAX;
                }
            },
            | MuxEvent::TaskCompleted {
                id,
                status,
                duration_ms,
            } => {
                if let Some(t) = self.tasks.get_mut(&id) {
                    t.duration_ms = Some(duration_ms);
                    match status {
                        | TaskStatus::Success => t.state = TaskState::Success,
                        | TaskStatus::Failed { message, .. } => {
                            t.state = TaskState::Failed;
                            t.failure = Some(message);
                        },
                    }
                }
            },
            | MuxEvent::Finished { failed, aborted } => {
                self.failed = failed;
                self.aborted = aborted;
                self.finished_at = Some(Instant::now());
                self.engine_finished = true;
                if self.quit_requested {
                    self.quit = true;
                }
            },
        }
    }

    pub fn selected_id(&self) -> Option<usize> {
        self.order.get(self.cursor).copied()
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
        if self.engine_finished {
            self.quit = true;
        }
    }

    pub fn elapsed(&self) -> std::time::Duration {
        self.finished_at.unwrap_or_else(Instant::now) - self.started_at
    }

    pub fn progress(&self) -> (usize, usize) {
        let completed = self
            .tasks
            .values()
            .filter(|t| matches!(t.state, TaskState::Success | TaskState::Failed))
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
