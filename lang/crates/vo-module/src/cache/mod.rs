//! Installed module cache abstraction.
//!
//! This module provides platform-agnostic cache layout, inspection, and
//! validation for installed Vo modules.  All functions are generic over the
//! `FileSystem` trait so they work identically on native `RealFs`, in-memory
//! `MemoryFs`, and browser `WasmVfs`.

use std::path::Path;

use crate::Error;

pub mod install;
pub mod layout;
pub(crate) mod mutation_lock;
mod source_integrity;
pub mod validate;

/// A shared native-cache lease. Hold this value for the complete interval in
/// which source files or artifacts may be read; cache cleanup waits until all
/// leases have been dropped.
#[derive(Debug)]
pub struct CacheReadLease {
    _lock: mutation_lock::CacheMutationLock,
}

/// Acquire a shared lease over a native module-cache root.
pub fn acquire_read_lease(cache_root: &Path) -> Result<CacheReadLease, Error> {
    Ok(CacheReadLease {
        _lock: mutation_lock::CacheMutationLock::shared(cache_root)?,
    })
}
