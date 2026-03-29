//! Island data structure for multi-VM concurrency.
//!
//! An Island represents an independent VM instance with:
//! - Its own GC/heap
//! - Its own fiber scheduler
//! - Communication via channels (cross-island)
//!
//! Each island runs on a dedicated OS thread.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

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
    SpawnFiber { closure_data: PackedValue },
    /// Wake a blocked fiber (no PC modification - blocker sets resume PC)
    WakeFiber { fiber_id: u32 },
    /// Request island shutdown
    Shutdown,
    /// Request from a remote island to the home island (where ChannelState lives).
    EndpointRequest {
        endpoint_id: u64,
        kind: EndpointRequestKind,
        from_island: u32,
        fiber_id: u64,
    },
    /// Response from the home island back to the requesting remote island.
    EndpointResponse {
        endpoint_id: u64,
        kind: EndpointResponseKind,
        fiber_id: u64,
    },
}

/// Kind of channel request (remote → home).
#[derive(Debug)]
pub enum EndpointRequestKind {
    /// Send data to the channel.
    Send { data: Vec<u8> },
    /// Receive data from the channel.
    Recv,
    /// Close the channel.
    Close,
    /// Notify home that a new peer island has received a proxy.
    Transfer { new_peer: u32 },
}

/// Kind of channel response (home → remote).
#[derive(Debug)]
pub enum EndpointResponseKind {
    /// Acknowledgment of a send operation.
    SendAck { closed: bool },
    /// Data delivered to a receiver (or closed indication).
    RecvData { data: Vec<u8>, closed: bool },
    /// Broadcast: channel was closed by someone else.
    Closed,
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
