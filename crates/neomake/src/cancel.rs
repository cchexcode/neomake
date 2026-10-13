use {
    std::sync::{
        atomic::{
            AtomicBool,
            Ordering,
        },
        Arc,
    },
    tokio::sync::Notify,
};

/// Cooperative cancellation shared by an engine and its drivers.
///
/// `trip` wakes every task waiting in `cancelled` without a poll loop.
#[derive(Clone, Debug)]
pub struct Cancel {
    flag: Arc<AtomicBool>,
    notify: Arc<Notify>,
}

impl Default for Cancel {
    fn default() -> Self {
        Self {
            flag: Arc::new(AtomicBool::new(false)),
            notify: Arc::new(Notify::new()),
        }
    }
}

impl Cancel {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn trip(&self) {
        self.flag.store(true, Ordering::SeqCst);
        self.notify.notify_waiters();
    }

    pub fn tripped(&self) -> bool {
        self.flag.load(Ordering::SeqCst)
    }

    /// Resolves when `trip` has been called. Safe against the notify/check
    /// race: the waiter is registered before the flag is read.
    pub async fn cancelled(&self) {
        loop {
            let notified = self.notify.notified();
            tokio::pin!(notified);
            if self.tripped() {
                return;
            }
            notified.await;
        }
    }
}
