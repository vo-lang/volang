//! Port-specific operations for cross-island communication.
//!
//! Ports are cross-island channels that require std for Arc<Mutex<>> synchronization.
//! In no_std mode, port operations return errors instead of panicking.

pub use super::queue_state::{SendResult, RecvResult};

// Re-export WaiterInfo from queue_state (std) or define stub (no_std)
#[cfg(feature = "std")]
pub use super::queue_state::WaiterInfo;

#[cfg(not(feature = "std"))]
#[derive(Debug, Clone, Copy)]
pub struct WaiterInfo {
    pub island_id: u32,
    pub fiber_id: u64,
}

// Conditionally include std implementation
#[cfg(feature = "std")]
mod std_impl;
#[cfg(feature = "std")]
pub use std_impl::*;

// no_std stubs - return errors rather than panic
#[cfg(not(feature = "std"))]
mod nostd_stub;
#[cfg(not(feature = "std"))]
pub use nostd_stub::*;

/// # Safety
/// port must be a valid Port GcRef.
#[cfg(not(feature = "std"))]
pub unsafe fn drop_inner(_port: GcRef) {
    // No-op in no_std - ports not supported
}
