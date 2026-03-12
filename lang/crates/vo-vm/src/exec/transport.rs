//! Cross-island transport message helpers for channel data packing/unpacking.

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};

use vo_runtime::gc::Gc;
#[cfg(feature = "std")]
use vo_runtime::gc::GcRef;
use vo_runtime::objects::queue_state::ChannelMessage;
use vo_runtime::pack::{
    pack_slots, unpack_slots, unpack_slots_with_chan_resolver, ChanHandleInfo, PackedValue,
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

#[cfg(feature = "std")]
pub fn resolve_unpacked_chan_handle(
    gc: &mut Gc,
    handle: ChanHandleInfo,
    endpoint_registry: &mut crate::vm::EndpointRegistry,
) -> GcRef {
    if let Some(existing) = endpoint_registry.get_live(handle.endpoint_id) {
        return existing;
    }
    let chan = vo_runtime::objects::channel::create_remote_proxy_with_closed(
        gc,
        handle.endpoint_id,
        handle.home_island,
        handle.cap,
        handle.elem_meta,
        handle.elem_slots,
        handle.closed,
    );
    endpoint_registry.register_live(handle.endpoint_id, chan);
    chan
}

pub fn unpack_transport_message(
    gc: &mut Gc,
    data: &[u8],
    elem_slots: usize,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    #[cfg(feature = "std")] endpoint_registry: Option<&mut crate::vm::EndpointRegistry>,
) -> ChannelMessage {
    if elem_slots == 0 {
        return Vec::new().into_boxed_slice();
    }
    let packed = PackedValue::from_data(data.to_vec());
    let mut dst: Vec<u64> = vec![0; elem_slots];
    #[cfg(feature = "std")]
    if let Some(endpoint_registry) = endpoint_registry {
        unpack_slots_with_chan_resolver(
            gc,
            &packed,
            &mut dst,
            struct_metas,
            runtime_types,
            |gc, handle| resolve_unpacked_chan_handle(gc, handle, endpoint_registry),
        );
        return dst.into_boxed_slice();
    }
    unpack_slots(gc, &packed, &mut dst, struct_metas, runtime_types);
    dst.into_boxed_slice()
}
