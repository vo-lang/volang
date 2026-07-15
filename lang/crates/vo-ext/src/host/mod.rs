//! Host capability bridge for native extensions.
//!
//! A host builds a [`HostBridge`] and installs it on its VM. Extensions access
//! the services through the submodule APIs:
//!
//! - [`timer`] — timeout and interval scheduling
//! - [`tick`] — game/animation tick loops
//! - [`capability`] — capability queries
//!
//! Dynamic calls receive a VM-scoped, versioned C callback table. Rust trait
//! objects, collections, and allocator ownership stay in the host image.
//! Child islands inherit the VM's shared owner. [`install`] remains available
//! only as an explicit same-image, current-thread fallback for direct helper
//! calls outside a VM.

pub mod capability;
pub mod tick;
pub mod timer;

use std::sync::Arc;

/// Central host bridge that holds all registered capabilities.
///
/// Constructed via the builder pattern:
/// ```ignore
/// let bridge = HostBridge::new()
///     .with_capability("render_island_host")
///     .with_tick(Box::new(my_tick_provider));
/// vo_ext::host::install(bridge);
/// ```
pub struct HostBridge {
    pub(crate) timer: Option<Box<dyn timer::TimerProvider>>,
    pub(crate) tick: Option<Box<dyn tick::TickProvider>>,
    pub(crate) capabilities: Vec<String>,
}

impl Default for HostBridge {
    fn default() -> Self {
        Self::new()
    }
}

impl HostBridge {
    pub fn new() -> Self {
        Self {
            timer: None,
            tick: None,
            capabilities: Vec::new(),
        }
    }

    pub fn with_timer(mut self, provider: Box<dyn timer::TimerProvider>) -> Self {
        self.timer = Some(provider);
        self
    }

    pub fn with_tick(mut self, provider: Box<dyn tick::TickProvider>) -> Self {
        self.tick = Some(provider);
        self
    }

    pub fn with_capability(mut self, name: impl Into<String>) -> Self {
        self.capabilities.push(name.into());
        self
    }
}

impl vo_runtime::host_services::HostServices for HostBridge {
    fn has_capability(&self, name: &str) -> bool {
        self.capabilities
            .iter()
            .any(|capability| capability == name)
    }

    fn start_timeout(&self, id: i32, ms: i32) -> bool {
        if ms < 0 {
            return false;
        }
        let Some(provider) = &self.timer else {
            return false;
        };
        provider.start_timeout(id, ms);
        true
    }

    fn clear_timeout(&self, id: i32) -> bool {
        let Some(provider) = &self.timer else {
            return false;
        };
        provider.clear_timeout(id);
        true
    }

    fn start_interval(&self, id: i32, ms: i32) -> bool {
        if ms < 0 {
            return false;
        }
        let Some(provider) = &self.timer else {
            return false;
        };
        provider.start_interval(id, ms);
        true
    }

    fn clear_interval(&self, id: i32) -> bool {
        let Some(provider) = &self.timer else {
            return false;
        };
        provider.clear_interval(id);
        true
    }

    fn start_tick_loop(&self, id: i32) -> bool {
        let Some(provider) = &self.tick else {
            return false;
        };
        provider.start_tick_loop(id);
        true
    }

    fn stop_tick_loop(&self, id: i32) -> bool {
        let Some(provider) = &self.tick else {
            return false;
        };
        provider.stop_tick_loop(id);
        true
    }
}

/// Install an owned same-image fallback for the current thread.
///
/// VM hosts should store an `Arc<HostBridge>` through their VM host-service
/// API so calls remain isolated per VM and propagate to child islands.
pub fn install(bridge: HostBridge) {
    vo_runtime::host_services::install_local(Arc::new(bridge));
}

/// Clear the same-image fallback for the current thread.
pub fn clear() {
    vo_runtime::host_services::clear_local();
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Arc, Mutex};

    type TimerCall = (&'static str, i32, i32);

    struct Timer(Arc<Mutex<Vec<TimerCall>>>);

    impl timer::TimerProvider for Timer {
        fn start_timeout(&self, id: i32, ms: i32) {
            self.0.lock().unwrap().push(("start_timeout", id, ms));
        }

        fn clear_timeout(&self, id: i32) {
            self.0.lock().unwrap().push(("clear_timeout", id, 0));
        }

        fn start_interval(&self, id: i32, ms: i32) {
            self.0.lock().unwrap().push(("start_interval", id, ms));
        }

        fn clear_interval(&self, id: i32) {
            self.0.lock().unwrap().push(("clear_interval", id, 0));
        }
    }

    struct Tick(Arc<Mutex<Vec<(&'static str, i32)>>>);

    impl tick::TickProvider for Tick {
        fn start_tick_loop(&self, id: i32) {
            self.0.lock().unwrap().push(("start", id));
        }

        fn stop_tick_loop(&self, id: i32) {
            self.0.lock().unwrap().push(("stop", id));
        }
    }

    #[test]
    fn same_image_fallback_delegates_without_raw_bridge_pointers() {
        let timer_calls = Arc::new(Mutex::new(Vec::new()));
        let tick_calls = Arc::new(Mutex::new(Vec::new()));
        install(
            HostBridge::new()
                .with_capability("render")
                .with_timer(Box::new(Timer(Arc::clone(&timer_calls))))
                .with_tick(Box::new(Tick(Arc::clone(&tick_calls)))),
        );

        assert!(capability::has("render"));
        assert!(!capability::has("missing"));
        timer::start_timeout(1, 2);
        timer::clear_timeout(1);
        timer::start_interval(3, 4);
        timer::clear_interval(3);
        tick::start_tick_loop(5);
        tick::stop_tick_loop(5);
        clear();

        assert_eq!(
            timer_calls.lock().unwrap().as_slice(),
            [
                ("start_timeout", 1, 2),
                ("clear_timeout", 1, 0),
                ("start_interval", 3, 4),
                ("clear_interval", 3, 0),
            ]
        );
        assert_eq!(
            tick_calls.lock().unwrap().as_slice(),
            [("start", 5), ("stop", 5)]
        );
        assert!(!capability::has("render"));
    }

    #[test]
    fn host_bridge_rejects_negative_timer_delays_before_provider_dispatch() {
        let calls = Arc::new(Mutex::new(Vec::new()));
        let bridge = HostBridge::new().with_timer(Box::new(Timer(Arc::clone(&calls))));

        assert!(!vo_runtime::host_services::HostServices::start_timeout(
            &bridge, 1, -1
        ));
        assert!(!vo_runtime::host_services::HostServices::start_interval(
            &bridge, 2, -1
        ));
        assert!(calls.lock().expect("timer calls").is_empty());
    }
}
