//! Timer capabilities: timeout and interval scheduling.

/// Provider trait for timer capabilities.
///
/// Hosts that support timer scheduling implement this trait and register it
/// via [`super::HostBridge::with_timer`].
pub trait TimerProvider: Send + Sync + 'static {
    fn start_timeout(&self, id: i32, ms: i32);
    fn clear_timeout(&self, id: i32);
    fn start_interval(&self, id: i32, ms: i32);
    fn clear_interval(&self, id: i32);
}

pub fn start_timeout(id: i32, ms: i32) {
    super::with_bridge(|b| {
        if let Some(t) = &b.timer {
            t.start_timeout(id, ms);
        }
    });
}

pub fn clear_timeout(id: i32) {
    super::with_bridge(|b| {
        if let Some(t) = &b.timer {
            t.clear_timeout(id);
        }
    });
}

pub fn start_interval(id: i32, ms: i32) {
    super::with_bridge(|b| {
        if let Some(t) = &b.timer {
            t.start_interval(id, ms);
        }
    });
}

pub fn clear_interval(id: i32) {
    super::with_bridge(|b| {
        if let Some(t) = &b.timer {
            t.clear_interval(id);
        }
    });
}
