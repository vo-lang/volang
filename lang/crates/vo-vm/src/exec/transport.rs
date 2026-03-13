//! Cross-island transport message helpers for channel data packing/unpacking.

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};

use vo_runtime::gc::Gc;
use vo_runtime::gc::GcRef;
use vo_runtime::objects::queue_state::QueueMessage;
use vo_runtime::pack::{
    pack_slots, unpack_slots_with_queue_handle_resolver, PackedValue, QueueHandleInfo,
};
use vo_runtime::ValueMeta;
use vo_common_core::bytecode::StructMeta;
use vo_common_core::RuntimeType;

pub fn pack_transport_message(
    gc: &Gc,
    src: &[u64],
    elem_meta: ValueMeta,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
) -> Vec<u8> {
    pack_slots(gc, src, elem_meta, struct_metas, runtime_types).into_data()
}

pub fn resolve_unpacked_queue_handle(
    gc: &mut Gc,
    handle: QueueHandleInfo,
    endpoint_registry: &mut crate::vm::EndpointRegistry,
) -> GcRef {
    if let Some(existing) = endpoint_registry.get_live(handle.endpoint_id) {
        return existing;
    }
    let queue_ref = vo_runtime::objects::queue::create_remote_proxy_with_closed(
        gc,
        handle.kind,
        handle.endpoint_id,
        handle.home_island,
        handle.cap,
        handle.elem_meta,
        handle.elem_rttid,
        handle.elem_slots,
        handle.closed,
    );
    endpoint_registry.register_live(handle.endpoint_id, queue_ref);
    queue_ref
}

pub fn unpack_transport_message(
    gc: &mut Gc,
    data: &[u8],
    elem_slots: usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    endpoint_registry: &mut crate::vm::EndpointRegistry,
) -> QueueMessage {
    if elem_slots == 0 {
        return Vec::new().into_boxed_slice();
    }
    let packed = PackedValue::from_data(data.to_vec());
    let mut dst: Vec<u64> = vec![0; elem_slots];
    unpack_slots_with_queue_handle_resolver(
        gc,
        &packed,
        &mut dst,
        struct_metas,
        runtime_types,
        |gc, handle| resolve_unpacked_queue_handle(gc, handle, endpoint_registry),
    );
    dst.into_boxed_slice()
}
