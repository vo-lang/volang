#![allow(clippy::missing_safety_doc)]
//! Island data structure for multi-VM concurrency.
//!
//! An Island represents an independent VM instance with:
//! - Its own GC/heap
//! - Its own fiber scheduler
//! - Communication via channels (cross-island)
//!
//! # Safety contract
//! Unsafe accessors require a canonical live `ValueKind::Island` allocation.
//!
//! Each island runs on a dedicated OS thread.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use core::num::NonZeroU64;

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
    /// Start a certified generated entry factory with owned init bytes.
    StartEntry {
        launch_token: u64,
        function_id: u32,
        init: Vec<u8>,
    },
    /// Resume a target-island fiber waiting on a HostServices completion.
    WakeHostEvent { token: u64, data: Vec<u8> },
    /// Request island shutdown
    Shutdown,
    /// Request from a remote island to the home island (where ChannelState lives).
    EndpointRequest {
        endpoint_id: u64,
        kind: EndpointRequestKind,
    },
    /// Response from the home island back to the requesting remote island.
    EndpointResponse {
        endpoint_id: u64,
        kind: EndpointResponseKind,
    },
}

/// Authenticated transport envelope for a command delivered to an island.
///
/// The envelope source is the sole transport-owned source identity. Endpoint
/// command payloads deliberately omit a second source field, so receivers cannot
/// observe conflicting transport and semantic identities.
#[derive(Debug)]
pub struct IslandCommandEnvelope {
    pub source_island_id: u32,
    pub command: IslandCommand,
}

impl IslandCommandEnvelope {
    pub fn new(source_island_id: u32, command: IslandCommand) -> Self {
        Self {
            source_island_id,
            command,
        }
    }
}

/// Identity of a fiber waiting for an endpoint operation to complete.
///
/// Keeping the wait ID non-zero makes fire-and-forget endpoint messages
/// structurally distinct from operations that must receive a response.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EndpointWaitKey {
    fiber_key: u64,
    wait_id: NonZeroU64,
}

impl EndpointWaitKey {
    #[inline]
    pub const fn new(fiber_key: u64, wait_id: NonZeroU64) -> Self {
        Self { fiber_key, wait_id }
    }

    #[inline]
    pub const fn try_new(fiber_key: u64, wait_id: u64) -> Option<Self> {
        match NonZeroU64::new(wait_id) {
            Some(wait_id) => Some(Self::new(fiber_key, wait_id)),
            None => None,
        }
    }

    #[inline]
    pub const fn fiber_key(self) -> u64 {
        self.fiber_key
    }

    #[inline]
    pub const fn wait_id(self) -> NonZeroU64 {
        self.wait_id
    }
}

/// Kind of channel request (remote → home).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EndpointRequestKind {
    /// Send data to the channel.
    Send {
        data: Vec<u8>,
        wait_key: EndpointWaitKey,
    },
    /// Receive data from the channel.
    Recv { wait_key: EndpointWaitKey },
    /// Close the channel.
    Close,
    /// Notify home that a new peer island has received a proxy.
    Transfer { new_peer: u32 },
}

impl EndpointRequestKind {
    /// Returns the wait identity exactly when the request expects a response.
    #[inline]
    pub const fn wait_key(&self) -> Option<EndpointWaitKey> {
        match self {
            Self::Send { wait_key, .. } | Self::Recv { wait_key } => Some(*wait_key),
            Self::Close | Self::Transfer { .. } => None,
        }
    }
}

/// Kind of channel response (home → remote).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EndpointResponseKind {
    /// Acknowledgment of a send operation.
    SendAck {
        closed: bool,
        wait_key: EndpointWaitKey,
    },
    /// Data delivered to a receiver (or closed indication).
    RecvData {
        data: Vec<u8>,
        closed: bool,
        wait_key: EndpointWaitKey,
    },
    /// Receive failed before the home queue state was consumed.
    RecvError { wait_key: EndpointWaitKey },
    /// Broadcast: channel was closed by someone else.
    Closed,
}

impl EndpointResponseKind {
    /// Returns the targeted wait identity, excluding untargeted close broadcasts.
    #[inline]
    pub const fn wait_key(&self) -> Option<EndpointWaitKey> {
        match self {
            Self::SendAck { wait_key, .. }
            | Self::RecvData { wait_key, .. }
            | Self::RecvError { wait_key } => Some(*wait_key),
            Self::Closed => None,
        }
    }
}

/// Create a new island handle.
/// Note: Command channels are managed by VM, not stored here.
pub fn create(gc: &mut Gc, island_id: u32) -> GcRef {
    let handle = gc.alloc(ValueMeta::new(0, ValueKind::Island), DATA_SLOTS);
    if handle.is_null() {
        return handle;
    }
    // Safety: `handle` is freshly allocated and not visible to the collector yet.
    let data = unsafe { IslandData::as_mut(handle) };
    data.id = island_id;
    data._pad = 0;
    handle
}

/// Create an island handle for the main island (island 0).
pub fn create_main(gc: &mut Gc) -> GcRef {
    create(gc, 0)
}

#[inline]
pub unsafe fn id(island: GcRef) -> u32 {
    unsafe { IslandData::as_ref(island) }.id
}
