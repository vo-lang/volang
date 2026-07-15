use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, Weak};
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

fn lock_recover<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    match mutex.lock() {
        Ok(guard) => guard,
        Err(poisoned) => {
            mutex.clear_poison();
            poisoned.into_inner()
        }
    }
}

struct NativeTimerProviderInner {
    callback: TimerCallback,
    timeouts: Mutex<HashMap<i32, Arc<TimerControl>>>,
    intervals: Mutex<HashMap<i32, Arc<TimerControl>>>,
}

impl NativeTimerProviderInner {
    fn remove_timeout_if_current(&self, id: i32, control: &Arc<TimerControl>) {
        let mut timeouts = lock_recover(&self.timeouts);
        if let Some(current) = timeouts.get(&id) {
            if Arc::ptr_eq(current, control) {
                timeouts.remove(&id);
            }
        }
    }

    fn remove_interval_if_current(&self, id: i32, control: &Arc<TimerControl>) {
        let mut intervals = lock_recover(&self.intervals);
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
        let Ok(delay_ms) = u64::try_from(ms) else {
            return;
        };
        let control = Arc::new(TimerControl::new());
        if let Some(existing) = lock_recover(&self.inner.timeouts).insert(id, Arc::clone(&control))
        {
            existing.request_stop();
        }
        let inner = self.weak_inner();
        std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(delay_ms));
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
        if let Some(control) = lock_recover(&self.inner.timeouts).remove(&id) {
            control.request_stop();
        }
    }

    fn start_interval(&self, id: i32, ms: i32) {
        let Ok(delay_ms) = u64::try_from(ms) else {
            return;
        };
        let control = Arc::new(TimerControl::new());
        if let Some(existing) = lock_recover(&self.inner.intervals).insert(id, Arc::clone(&control))
        {
            existing.request_stop();
        }
        let inner = self.weak_inner();
        std::thread::spawn(move || {
            // A zero-duration interval would become an unbounded busy loop.
            // Keep zero valid at the API boundary while imposing a minimal
            // scheduler-friendly native cadence.
            let interval = Duration::from_millis(delay_ms.max(1));
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
        if let Some(control) = lock_recover(&self.inner.intervals).remove(&id) {
            control.request_stop();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::{lock_recover, NativeTimerProvider, TimerControl};
    use vo_ext::host::timer::TimerProvider;

    #[test]
    fn timer_control_coalesces_pending_events_until_consumed() {
        let control = TimerControl::new();

        assert!(control.queue_pending_event());
        assert!(!control.queue_pending_event());
        assert!(control.take_pending_event());
        assert!(!control.take_pending_event());
        assert!(control.queue_pending_event());
    }

    #[test]
    fn timer_provider_rejects_negative_delays_without_spawning_or_registering() {
        let provider = NativeTimerProvider::new(|_, _| panic!("negative timer must not fire"));

        provider.start_timeout(1, -1);
        provider.start_interval(2, -1);

        assert!(lock_recover(&provider.inner.timeouts).is_empty());
        assert!(lock_recover(&provider.inner.intervals).is_empty());
    }

    #[test]
    fn timer_provider_recovers_poisoned_registries() {
        let provider = NativeTimerProvider::new(|_, _| {});
        let inner = Arc::clone(&provider.inner);
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
            let _timeouts = inner.timeouts.lock().expect("initial timeout lock");
            panic!("poison timeout registry");
        }));
        assert!(result.is_err());

        provider.start_timeout(7, -1);
        provider.clear_timeout(7);
        assert!(!provider.inner.timeouts.is_poisoned());
        assert!(lock_recover(&provider.inner.timeouts).is_empty());
    }
}
