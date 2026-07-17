//! Platform-specific native module-cache mutation capabilities.
//!
//! Unix keeps descriptor-relative `openat`/`renameat` implementations in the
//! portable module. Windows has a handle-identity implementation with Win32
//! byte-range leases and NT handle-relative rename operations. Targets that
//! cannot provide either contract retain the fail-closed stubs from the
//! portable implementation.

#[cfg(not(windows))]
#[path = "mutation_lock_portable.rs"]
mod platform;

#[cfg(windows)]
#[path = "mutation_lock_windows.rs"]
mod platform;

pub(crate) use platform::*;
