//! Shared GUI host state — render bytes and event synchronization.
//!
//! Decouples the vogui library from the host (playground, studio):
//!   - vogui FFI writes render bytes and stores event state here.
//!   - The host reads render bytes and resolves event tokens from here.
//!
//! This lives in vo-runtime because both vogui and every host already
//! depend on vo-runtime, avoiding any new cross-crate dependencies.

use std::cell::RefCell;
use std::sync::atomic::{AtomicU64, Ordering};

// =============================================================================
// Pending render bytes
// =============================================================================

thread_local! {
    static PENDING_RENDER: RefCell<Option<Vec<u8>>> = RefCell::new(None);
}

/// Called by `emitRenderBinary` FFI to store render bytes for the host.
pub fn set_pending_render(bytes: Vec<u8>) {
    PENDING_RENDER.with(|r| *r.borrow_mut() = Some(bytes));
}

/// Take the pending render bytes (if `emitRenderBinary` was called).
pub fn take_pending_render_bytes() -> Option<Vec<u8>> {
    PENDING_RENDER.with(|r| r.borrow_mut().take())
}

/// Clear any pending render bytes.
pub fn clear_pending_render() {
    PENDING_RENDER.with(|r| *r.borrow_mut() = None);
}

// =============================================================================
// Event synchronization
// =============================================================================

/// Pending event data: (handler_id, payload).
/// Written by the host before waking the fiber; read by `waitForEvent` on replay.
pub struct PendingEvent {
    pub handler_id: i32,
    pub payload: String,
}

/// Monotonic token counter for `HostEventWaitAndReplay`.
static EVENT_TOKEN_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Generate a unique host event token.
pub fn next_event_token() -> u64 {
    EVENT_TOKEN_COUNTER.fetch_add(1, Ordering::Relaxed)
}

thread_local! {
    /// Event data for the blocked main fiber (set by host, consumed by waitForEvent extern).
    static PENDING_EVENT: RefCell<Option<PendingEvent>> = RefCell::new(None);

    /// The host event token used by the main fiber's `waitForEvent` block.
    /// Set when `waitForEvent` first suspends; used by `send_event` to wake the fiber.
    static EVENT_WAIT_TOKEN: RefCell<Option<u64>> = RefCell::new(None);
}

/// Called by the `waitForEvent` FFI on first call to store the fiber's token.
pub fn store_event_wait_token(token: u64) {
    EVENT_WAIT_TOKEN.with(|s| *s.borrow_mut() = Some(token));
}

/// Take the pending event (called by `waitForEvent` FFI on replay).
pub fn take_pending_event() -> Option<PendingEvent> {
    PENDING_EVENT.with(|s| s.borrow_mut().take())
}

/// Store an event for the blocked main fiber and return the token to wake it.
/// Returns `None` if `waitForEvent` hasn't been called yet (fiber not blocked).
pub fn send_event(handler_id: i32, payload: String) -> Option<u64> {
    let token = EVENT_WAIT_TOKEN.with(|s| s.borrow().as_ref().copied())?;
    PENDING_EVENT.with(|s| *s.borrow_mut() = Some(PendingEvent { handler_id, payload }));
    Some(token)
}

/// Clear all event-loop state (called on stop/reset).
pub fn clear_event_state() {
    PENDING_EVENT.with(|s| *s.borrow_mut() = None);
    EVENT_WAIT_TOKEN.with(|s| *s.borrow_mut() = None);
}
