#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct WakeGeneration {
    current: u64,
    active: bool,
}

impl Default for WakeGeneration {
    fn default() -> Self {
        Self {
            current: 0,
            active: false,
        }
    }
}

impl WakeGeneration {
    pub fn register(&mut self) -> u64 {
        self.current = next_generation(self.current);
        self.active = true;
        self.current
    }

    pub const fn accepts(&self, generation: u64) -> bool {
        self.active && generation == self.current
    }

    pub fn release(&mut self, generation: u64) -> bool {
        if !self.accepts(generation) {
            return false;
        }
        self.active = false;
        true
    }
}

fn next_generation(value: u64) -> u64 {
    let next = value.wrapping_add(1);
    if next == 0 {
        1
    } else {
        next
    }
}

#[cfg(feature = "std")]
#[derive(Debug, Default)]
pub struct WakeCoalescer {
    pending: core::sync::atomic::AtomicBool,
}

#[cfg(feature = "std")]
impl WakeCoalescer {
    pub fn try_mark_pending(&self) -> bool {
        self.pending
            .compare_exchange(
                false,
                true,
                core::sync::atomic::Ordering::AcqRel,
                core::sync::atomic::Ordering::Acquire,
            )
            .is_ok()
    }

    pub fn consume(&self) {
        self.pending
            .store(false, core::sync::atomic::Ordering::Release);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn released_and_replaced_registrations_reject_stale_wakes() {
        let mut wake = WakeGeneration::default();
        let first = wake.register();
        assert!(wake.accepts(first));
        assert!(wake.release(first));
        assert!(!wake.accepts(first));
        let second = wake.register();
        assert_ne!(first, second);
        assert!(!wake.accepts(first));
        assert!(wake.accepts(second));
    }

    #[cfg(feature = "std")]
    #[test]
    fn coalescer_allows_at_most_one_pending_wake() {
        let wake = WakeCoalescer::default();
        assert!(wake.try_mark_pending());
        assert!(!wake.try_mark_pending());
        wake.consume();
        assert!(wake.try_mark_pending());
    }
}
