/// Abstraction for host-side event scheduling.
///
/// Implementations are responsible for waking host events after the requested
/// delay. The runtime calls `schedule` when a new pending host event is
/// produced by a VM step, and `cancel` when the event is no longer needed
/// (e.g. the session is shutting down).
pub trait HostEventScheduler {
    fn schedule(&self, token: u64, delay_ms: u32);
    fn cancel(&self, token: u64);
}
