//! Island data structure for multi-VM concurrency.
//!
//! An Island represents an independent VM instance with:
//! - Its own GC/heap
//! - Its own fiber scheduler
//! - Communication via ports (cross-island channels)
//!
//! Each island runs on a dedicated OS thread.

use crate::gc::{Gc, GcRef};
use crate::objects::impl_gc_object;
use crate::pack::PackedValue;
use crate::slot::SLOT_BYTES;
use vo_common_core::types::{ValueKind, ValueMeta};

/// GC object layout for island handle.
/// This is what Vo code sees as `island` type.
/// 
/// Note: Command channels are managed by VM, not stored in GC objects.
#[repr(C)]
pub struct IslandData {
    /// Island ID (0 = main island)
    pub id: u32,
    /// Padding for alignment
    _pad: u32,
}

pub const DATA_SLOTS: u16 = 1;
const _: () = assert!(core::mem::size_of::<IslandData>() == DATA_SLOTS as usize * SLOT_BYTES);

impl_gc_object!(IslandData);

/// Commands that can be sent to an island from other islands.
#[derive(Debug)]
pub enum IslandCommand {
    /// Spawn a new fiber with packed closure data
    SpawnFiber {
        closure_data: PackedValue,
        capture_slots: u16,
    },
    /// Wake a blocked fiber (no PC modification - blocker sets resume PC)
    WakeFiber { fiber_id: u32 },
    /// Request island shutdown
    Shutdown,
}

/// Create a new island handle.
/// Note: Command channels are managed by VM, not stored here.
pub fn create(gc: &mut Gc, island_id: u32) -> GcRef {
    let handle = gc.alloc(ValueMeta::new(0, ValueKind::Island), DATA_SLOTS);
    let data = IslandData::as_mut(handle);
    data.id = island_id;
    data._pad = 0;
    handle
}

/// Create an island handle for the main island (island 0).
pub fn create_main(gc: &mut Gc) -> GcRef {
    create(gc, 0)
}

#[inline]
pub fn id(island: GcRef) -> u32 {
    IslandData::as_ref(island).id
}
