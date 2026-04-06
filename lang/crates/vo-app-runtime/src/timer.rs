use std::collections::HashMap;
use std::convert::TryFrom;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};
use std::time::Duration;

pub(crate) struct TimerControl {
    stop: AtomicBool,
    pending_event: AtomicBool,
}

impl TimerControl {
    fn new() -> Self {
        Self {
            stop: AtomicBool::new(false),
            pending_event: AtomicBool::new(false),
        }
    }

    fn request_stop(&self) {
        self.stop.store(true, Ordering::Relaxed);
    }

    fn should_stop(&self) -> bool {
        self.stop.load(Ordering::Relaxed)
    }

    fn queue_pending_event(&self) -> bool {
        !self.pending_event.swap(true, Ordering::Relaxed)
    }

    pub(crate) fn take_pending_event(&self) -> bool {
        self.pending_event.swap(false, Ordering::Relaxed)
    }
}

type TimerCallback = Arc<dyn Fn(i32, Arc<TimerControl>) + Send + Sync + 'static>;

struct NativeTimerProviderInner {
    callback: TimerCallback,
    timeouts: Mutex<HashMap<i32, Arc<TimerControl>>>,
    intervals: Mutex<HashMap<i32, Arc<TimerControl>>>,
}

impl NativeTimerProviderInner {
    fn remove_timeout_if_current(&self, id: i32, control: &Arc<TimerControl>) {
        let mut timeouts = self.timeouts.lock().unwrap();
        if let Some(current) = timeouts.get(&id) {
            if Arc::ptr_eq(current, control) {
                timeouts.remove(&id);
            }
        }
    }

    fn remove_interval_if_current(&self, id: i32, control: &Arc<TimerControl>) {
        let mut intervals = self.intervals.lock().unwrap();
        if let Some(current) = intervals.get(&id) {
            if Arc::ptr_eq(current, control) {
                intervals.remove(&id);
            }
        }
    }
}

#[derive(Clone)]
pub(crate) struct NativeTimerProvider {
    inner: Arc<NativeTimerProviderInner>,
}

impl NativeTimerProvider {
    pub(crate) fn new<F>(callback: F) -> Self
    where
        F: Fn(i32, Arc<TimerControl>) + Send + Sync + 'static,
    {
        Self {
            inner: Arc::new(NativeTimerProviderInner {
                callback: Arc::new(callback),
                timeouts: Mutex::new(HashMap::new()),
                intervals: Mutex::new(HashMap::new()),
            }),
        }
    }

    fn weak_inner(&self) -> Weak<NativeTimerProviderInner> {
        Arc::downgrade(&self.inner)
    }
}

impl vo_ext::host::timer::TimerProvider for NativeTimerProvider {
    fn start_timeout(&self, id: i32, ms: i32) {
        let control = Arc::new(TimerControl::new());
        if let Some(existing) = self.inner.timeouts.lock().unwrap().insert(id, Arc::clone(&control)) {
            existing.request_stop();
        }
        let inner = self.weak_inner();
        std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(u64::try_from(ms).unwrap()));
            let Some(inner) = inner.upgrade() else {
                return;
            };
            if control.should_stop() {
                inner.remove_timeout_if_current(id, &control);
                return;
            }
            control.queue_pending_event();
            inner.remove_timeout_if_current(id, &control);
            (inner.callback)(id, Arc::clone(&control));
        });
    }

    fn clear_timeout(&self, id: i32) {
        if let Some(control) = self.inner.timeouts.lock().unwrap().remove(&id) {
            control.request_stop();
        }
    }

    fn start_interval(&self, id: i32, ms: i32) {
        let control = Arc::new(TimerControl::new());
        if let Some(existing) = self.inner.intervals.lock().unwrap().insert(id, Arc::clone(&control)) {
            existing.request_stop();
        }
        let inner = self.weak_inner();
        std::thread::spawn(move || {
            let interval = Duration::from_millis(u64::try_from(ms).unwrap());
            loop {
                std::thread::sleep(interval);
                let Some(inner) = inner.upgrade() else {
                    return;
                };
                if control.should_stop() {
                    inner.remove_interval_if_current(id, &control);
                    return;
                }
                if control.queue_pending_event() {
                    (inner.callback)(id, Arc::clone(&control));
                }
            }
        });
    }

    fn clear_interval(&self, id: i32) {
        if let Some(control) = self.inner.intervals.lock().unwrap().remove(&id) {
            control.request_stop();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TimerControl;

    #[test]
    fn timer_control_coalesces_pending_events_until_consumed() {
        let control = TimerControl::new();

        assert!(control.queue_pending_event());
        assert!(!control.queue_pending_event());
        assert!(control.take_pending_event());
        assert!(!control.take_pending_event());
        assert!(control.queue_pending_event());
    }
}
