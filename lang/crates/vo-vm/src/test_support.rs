//! Trusted raw-object adapters for VM unit tests.
//!
//! Production code must validate `GcRef` handles at VM/JIT/FFI boundaries.
//! Unit tests in this crate build every referenced object from a local `Gc`, so
//! this facade keeps fixture code compact while the production API remains
//! explicitly unsafe.
#![allow(dead_code, unused_imports)]

pub(crate) mod queue {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::queue as raw;
    use vo_runtime::objects::queue_state::{
        HomeInfo, LocalQueueState, QueueMessage, QueueWaiter, RemoteProxy,
    };

    pub use raw::{
        create, create_remote_proxy, BlockingSendResult, HomeInfoSnapshot, ResolvedSendResult,
        SendResult,
    };

    pub fn local_state(chan: GcRef) -> &'static mut LocalQueueState {
        unsafe { raw::local_state(chan) }
    }

    pub fn remote_proxy(chan: GcRef) -> &'static RemoteProxy {
        unsafe { raw::remote_proxy(chan) }
    }

    pub fn home_info(chan: GcRef) -> Option<&'static HomeInfo> {
        unsafe { raw::home_info(chan) }
    }

    pub fn home_info_snapshot(chan: GcRef) -> Option<HomeInfoSnapshot> {
        unsafe { raw::home_info_snapshot(chan) }
    }

    pub fn install_home_info(chan: GcRef, endpoint_id: u64, home_island: u32) {
        unsafe { raw::install_home_info(chan, endpoint_id, home_island) }
    }

    pub fn add_home_peer(chan: GcRef, peer_island: u32) -> u64 {
        unsafe { raw::add_home_peer(chan, peer_island) }.expect("test port HomeInfo")
    }

    pub fn with_local_state<T>(chan: GcRef, f: impl FnOnce(&mut LocalQueueState) -> T) -> T {
        unsafe { raw::with_local_state(chan, f) }
    }

    pub fn len(chan: GcRef) -> usize {
        unsafe { raw::len(chan) }
    }

    pub fn is_closed(chan: GcRef) -> bool {
        unsafe { raw::is_closed(chan) }
    }

    pub fn close(chan: GcRef) {
        unsafe { raw::close(chan) }
    }

    pub fn try_send(chan: GcRef, value: QueueMessage) -> SendResult<QueueWaiter, QueueMessage> {
        unsafe { raw::try_send(chan, value) }
    }

    pub fn register_sender(chan: GcRef, waiter: QueueWaiter, value: QueueMessage) {
        unsafe { raw::register_sender(chan, waiter, value) }
    }

    pub fn register_receiver(chan: GcRef, waiter: QueueWaiter) {
        unsafe { raw::register_receiver(chan, waiter) }
    }

    pub fn send_or_block_resolved(
        chan: GcRef,
        value: QueueMessage,
        waiter: QueueWaiter,
        local_island: u32,
    ) -> ResolvedSendResult<QueueWaiter, QueueMessage> {
        unsafe { raw::send_or_block_resolved(chan, value, waiter, local_island) }
    }

    pub fn take_waiting_receivers(chan: GcRef) -> Vec<QueueWaiter> {
        unsafe { raw::take_waiting_receivers(chan) }
    }

    pub fn take_waiting_senders(chan: GcRef) -> Vec<(QueueWaiter, QueueMessage)> {
        unsafe { raw::take_waiting_senders(chan) }
    }
}

pub(crate) mod queue_state {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::queue_state as raw;

    pub use raw::{
        LocalQueueState, QueueBacking, QueueData, QueueKind, QueueMessage, QueueWaiter,
        SelectWaitKind, SendResult,
    };

    pub fn capacity(queue: GcRef) -> usize {
        unsafe { raw::capacity(queue) }
    }
}

pub(crate) mod array {
    use vo_runtime::gc::{Gc, GcRef};
    use vo_runtime::objects::array as raw;
    use vo_runtime::{ValueKind, ValueMeta};

    pub use raw::{ArrayHeader, HEADER_SLOTS};

    pub fn create(gc: &mut Gc, elem_meta: ValueMeta, elem_bytes: usize, length: usize) -> GcRef {
        raw::create(gc, elem_meta, elem_bytes, length)
    }

    pub fn len(array: GcRef) -> usize {
        unsafe { raw::len(array) }
    }

    pub fn elem_meta(array: GcRef) -> ValueMeta {
        unsafe { raw::elem_meta(array) }
    }

    pub fn elem_kind(array: GcRef) -> ValueKind {
        unsafe { raw::elem_kind(array) }
    }

    pub fn data_ptr_bytes(array: GcRef) -> *mut u8 {
        unsafe { raw::data_ptr_bytes(array) }
    }

    pub fn set(array: GcRef, index: usize, value: u64, elem_bytes: usize) {
        unsafe { raw::set(array, index, value, elem_bytes) }
    }
}

pub(crate) mod slice {
    use vo_runtime::gc::{Gc, GcRef};
    use vo_runtime::objects::slice as raw;
    use vo_runtime::ValueMeta;

    pub use raw::{SliceData, DATA_SLOTS};

    pub fn create(
        gc: &mut Gc,
        elem_meta: ValueMeta,
        elem_bytes: usize,
        length: usize,
        capacity: usize,
    ) -> GcRef {
        raw::create(gc, elem_meta, elem_bytes, length, capacity)
    }

    pub fn from_array_range(gc: &mut Gc, array: GcRef, start: usize, length: usize) -> GcRef {
        unsafe { raw::from_array_range(gc, array, start, length) }
    }

    pub fn array_ref(slice: GcRef) -> GcRef {
        unsafe { raw::array_ref(slice) }
    }

    pub fn elem_meta(slice: GcRef) -> ValueMeta {
        unsafe { raw::elem_meta(slice) }
    }

    pub fn len(slice: GcRef) -> usize {
        unsafe { raw::len(slice) }
    }

    pub fn get(slice: GcRef, index: usize, elem_bytes: usize) -> u64 {
        unsafe { raw::get(slice, index, elem_bytes) }
    }

    pub fn set(slice: GcRef, index: usize, value: u64, elem_bytes: usize) {
        unsafe { raw::set(slice, index, value, elem_bytes) }
    }
}
