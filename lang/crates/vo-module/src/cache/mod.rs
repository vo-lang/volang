//! Installed module cache abstraction.
//!
//! This module provides platform-agnostic cache layout, inspection, and
//! validation for installed Vo modules.  All functions are generic over the
//! `FileSystem` trait so they work identically on native `RealFs`, in-memory
//! `MemoryFs`, and browser `WasmVfs`.

use std::path::Path;

use crate::Error;

/// Generation of the owned on-disk module-cache layout.
///
/// Native defaults include this as their final path component. Incompatible
/// layout changes must advance it so older cache roots remain untouched.
pub const CACHE_LAYOUT_GENERATION: &str = "v2";

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

/// An opaque, process-safe coordinator for one native build identity.
///
/// The guard also retains a shared cache lease, so cache cleanup cannot remove
/// the bounded identity-lock pool while a compiler is using it.
#[derive(Debug)]
pub struct NativeBuildCoordinator {
    _lock: mutation_lock::CacheMutationLock,
}

/// Acquire a shared lease over a native module-cache root.
pub fn acquire_read_lease(cache_root: &Path) -> Result<CacheReadLease, Error> {
    Ok(CacheReadLease {
        _lock: mutation_lock::CacheMutationLock::shared(cache_root)?,
    })
}

/// Serialize native builds for a stable adapter identity.
///
/// The implementation reuses the cache-owned, descriptor-validated bounded
/// lock pool beneath `.vo-staging`. Hash collisions only serialize unrelated
/// identities; they cannot weaken mutual exclusion.
pub fn acquire_native_build_coordinator(
    cache_root: &Path,
    identity: &str,
) -> Result<NativeBuildCoordinator, Error> {
    let cache_lease = mutation_lock::CacheMutationLock::shared(cache_root)?;
    let identity_lock = cache_lease.identity_lock(identity)?;
    Ok(NativeBuildCoordinator {
        _lock: identity_lock,
    })
}
