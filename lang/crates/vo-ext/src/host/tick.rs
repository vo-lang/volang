//! Tick loop capabilities: game/animation frame scheduling.

/// Provider trait for tick loop capabilities.
///
/// Hosts that support periodic tick loops (e.g. game loops) implement this
/// trait and register it via [`super::HostBridge::with_tick`].
pub trait TickProvider: Send + Sync + 'static {
    fn start_tick_loop(&self, id: i32);
    fn stop_tick_loop(&self, id: i32);
}

pub fn start_tick_loop(id: i32) {
    super::with_bridge(|b| {
        if let Some(t) = &b.tick {
            t.start_tick_loop(id);
        }
    });
}

pub fn stop_tick_loop(id: i32) {
    super::with_bridge(|b| {
        if let Some(t) = &b.tick {
            t.stop_tick_loop(id);
        }
    });
}
