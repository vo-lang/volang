//! Trusted adapters for unit tests that construct every object from a local GC.
#![allow(dead_code, unused_imports)]

pub(crate) mod array {
    use crate::gc::{Gc, GcRef};
    use crate::objects::array as raw;
    use crate::{ValueKind, ValueMeta};

    pub use raw::{ArrayHeader, HEADER_SLOTS};

    pub fn create(gc: &mut Gc, meta: ValueMeta, width: usize, len: usize) -> GcRef {
        raw::create(gc, meta, width, len)
    }
    pub fn len(value: GcRef) -> usize {
        unsafe { raw::len(value) }
    }
    pub fn elem_meta(value: GcRef) -> ValueMeta {
        unsafe { raw::elem_meta(value) }
    }
    pub fn elem_bytes(value: GcRef) -> usize {
        unsafe { raw::elem_bytes(value) }
    }
    pub fn data_ptr_bytes(value: GcRef) -> *mut u8 {
        unsafe { raw::data_ptr_bytes(value) }
    }
    pub fn set(value: GcRef, index: usize, slot: u64, width: usize) {
        unsafe { raw::set(value, index, slot, width) }
    }
    pub fn get(value: GcRef, index: usize, width: usize) -> u64 {
        unsafe { raw::get(value, index, width) }
    }
    pub fn set_n(value: GcRef, index: usize, slots: &[u64], width: usize) {
        unsafe { raw::set_n(value, index, slots, width) }
    }
}

pub(crate) mod slice {
    use crate::gc::{Gc, GcRef};
    use crate::objects::slice as raw;
    use crate::ValueMeta;

    pub use raw::{SliceData, DATA_SLOTS};

    pub fn create(gc: &mut Gc, meta: ValueMeta, width: usize, len: usize, cap: usize) -> GcRef {
        raw::create(gc, meta, width, len, cap)
    }
    pub fn from_array_range(gc: &mut Gc, array: GcRef, start: usize, len: usize) -> GcRef {
        unsafe { raw::from_array_range(gc, array, start, len) }
    }
    pub fn from_array_range_with_cap(
        gc: &mut Gc,
        array: GcRef,
        start: usize,
        len: usize,
        cap: usize,
    ) -> GcRef {
        unsafe { raw::from_array_range_with_cap(gc, array, start, len, cap) }
    }
    pub fn array_ref(value: GcRef) -> GcRef {
        unsafe { raw::array_ref(value) }
    }
    pub fn len(value: GcRef) -> usize {
        unsafe { raw::len(value) }
    }
    pub fn elem_meta(value: GcRef) -> ValueMeta {
        unsafe { raw::elem_meta(value) }
    }
    pub fn data_ptr(value: GcRef) -> *mut u8 {
        unsafe { raw::data_ptr(value) }
    }
    pub fn get(value: GcRef, index: usize, width: usize) -> u64 {
        unsafe { raw::get(value, index, width) }
    }
    pub fn set(value: GcRef, index: usize, slot: u64, width: usize) {
        unsafe { raw::set(value, index, slot, width) }
    }
    pub fn try_append(
        gc: &mut Gc,
        meta: ValueMeta,
        width: usize,
        value: GcRef,
        slots: &[u64],
        module: Option<&crate::Module>,
    ) -> Result<GcRef, crate::gc_types::TypedWriteBarrierByMetaError> {
        unsafe { raw::try_append(gc, meta, width, value, slots, module) }
    }
    pub fn with_new_len(gc: &mut Gc, value: GcRef, len: usize) -> GcRef {
        unsafe { raw::with_new_len(gc, value, len) }
    }
}

pub(crate) mod queue {
    use crate::gc::GcRef;
    use crate::objects::queue as raw;
    use crate::objects::queue_state::{HomeInfo, QueueMessage, QueueWaiter, RemoteProxy};

    pub use raw::{create, create_remote_proxy, BlockingSendResult, SendResult};

    pub fn install_home_info(value: GcRef, endpoint: u64, island: u32) {
        unsafe { raw::install_home_info(value, endpoint, island) }
    }
    pub fn is_remote(value: GcRef) -> bool {
        unsafe { raw::is_remote(value) }
    }
    pub fn is_port(value: GcRef) -> bool {
        unsafe { raw::is_port(value) }
    }
    pub fn remote_proxy(value: GcRef) -> &'static RemoteProxy {
        unsafe { raw::remote_proxy(value) }
    }
    pub fn try_send(value: GcRef, message: QueueMessage) -> SendResult<QueueWaiter, QueueMessage> {
        unsafe { raw::try_send(value, message) }
    }
    pub fn send_or_block(
        value: GcRef,
        message: QueueMessage,
        waiter: QueueWaiter,
    ) -> BlockingSendResult<QueueWaiter, QueueMessage> {
        unsafe { raw::send_or_block(value, message, waiter) }
    }
}

pub(crate) mod pack {
    use crate::bytecode::{NamedTypeMeta, StructMeta};
    use crate::gc::{Gc, GcRef};
    use crate::pack::{self as raw, PackTypeContext, PackedValue, QueueHandleInfo};
    use crate::{Module, RuntimeType, ValueMeta};

    pub fn pack_slots(
        gc: &Gc,
        src: &[u64],
        meta: ValueMeta,
        structs: &[StructMeta],
        runtime: &[RuntimeType],
    ) -> PackedValue {
        unsafe { raw::pack_slots(gc, src, meta, structs, runtime) }
    }
    pub fn pack_slots_with_named_type_metas(
        gc: &Gc,
        src: &[u64],
        meta: ValueMeta,
        structs: &[StructMeta],
        named: &[NamedTypeMeta],
        runtime: &[RuntimeType],
    ) -> PackedValue {
        unsafe { raw::pack_slots_with_named_type_metas(gc, src, meta, structs, named, runtime) }
    }
    pub fn unpack_slots(
        gc: &mut Gc,
        packed: &PackedValue,
        dst: &mut [u64],
        structs: &[StructMeta],
        runtime: &[RuntimeType],
    ) {
        unsafe { raw::unpack_slots(gc, packed, dst, structs, runtime) }
    }
    pub fn unpack_slots_with_named_type_metas(
        gc: &mut Gc,
        packed: &PackedValue,
        dst: &mut [u64],
        structs: &[StructMeta],
        named: &[NamedTypeMeta],
        runtime: &[RuntimeType],
    ) {
        unsafe { raw::unpack_slots_with_named_type_metas(gc, packed, dst, structs, named, runtime) }
    }
    #[allow(clippy::too_many_arguments)]
    pub fn unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas<F>(
        gc: &mut Gc,
        packed: &PackedValue,
        dst: &mut [u64],
        expected: ValueMeta,
        structs: &[StructMeta],
        named: &[NamedTypeMeta],
        runtime: &[RuntimeType],
        resolver: F,
    ) where
        F: FnMut(&mut Gc, QueueHandleInfo) -> GcRef,
    {
        unsafe {
            raw::unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas(
                gc, packed, dst, expected, structs, named, runtime, resolver,
            )
        }
    }
}

pub(crate) fn scan_object<'a, F>(
    gc: &mut crate::gc::Gc,
    object: crate::gc::GcRef,
    structs: &[crate::bytecode::StructMeta],
    closure_layout: &F,
) where
    F: Fn(u32) -> crate::gc_types::ClosureScanLayout<'a> + ?Sized,
{
    unsafe { crate::gc_types::scan_object(gc, object, structs, closure_layout) }
}

pub(crate) fn trace_object_children_with_context<'a, F, V>(
    object: crate::gc::GcRef,
    context: crate::gc_types::GcScanContext<'_>,
    closure_layout: &F,
    visit: V,
) where
    F: Fn(u32) -> crate::gc_types::ClosureScanLayout<'a> + ?Sized,
    V: FnMut(crate::gc::GcRef),
{
    unsafe {
        crate::gc_types::trace_object_children_with_context(object, context, closure_layout, visit)
    }
}
