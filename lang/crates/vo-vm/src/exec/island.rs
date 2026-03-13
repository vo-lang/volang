//! Island instructions: IslandNew, GoIsland

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::island;
use vo_runtime::objects::closure;
use vo_runtime::slot::Slot;
use vo_common_core::TransferType;

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};
use hashbrown::HashSet;


/// Result of go @(island) - spawn fiber on remote island.
/// 
/// Contains raw capture and argument data. The VM coordinator will pack these
/// with proper type metadata from FunctionDef before sending.
pub struct GoIslandResult {
    pub island: GcRef,
    pub func_id: u32,
    pub capture_data: Vec<Slot>,
    pub arg_data: Vec<Slot>,
}

/// Create a new island (no_std: dummy main island handle).
#[inline]
pub fn exec_island_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    next_island_id: u32,
) -> GcRef {
    let handle = island::create(gc, next_island_id);
    stack_set(stack, bp + inst.a as usize, handle as u64);
    handle
}

/// Start a goroutine on a specific island.
/// 
/// GoIsland instruction: a=island, b=closure, c=args_start, flags=arg_slots
/// 
/// Note: Closures themselves are NOT sendable. This function:
/// 1. Extracts func_id from the closure
/// 2. Reads raw capture slots (GcRefs to escaped variables)
/// 3. Reads raw argument slots from stack
/// 4. Returns data for VM coordinator to handle packing with proper type info
pub fn exec_go_island(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
) -> GoIslandResult {
    let island_handle = stack_get(stack, bp + inst.a as usize) as GcRef;
    let closure_ref = stack_get(stack, bp + inst.b as usize) as GcRef;
    let args_start = bp + inst.c as usize;
    let arg_slots = inst.flags as usize;

    // Extract function ID and capture count from closure itself
    let func_id = closure::func_id(closure_ref);
    let capture_count = closure::capture_count(closure_ref);
    
    // Read raw capture slots
    let mut capture_data = Vec::with_capacity(capture_count);
    for i in 0..capture_count {
        capture_data.push(closure::get_capture(closure_ref, i));
    }
    
    // Read raw argument slots
    let mut arg_data = Vec::with_capacity(arg_slots);
    for i in 0..arg_slots {
        arg_data.push(stack_get(stack, args_start + i));
    }

    GoIslandResult {
        island: island_handle,
        func_id,
        capture_data,
        arg_data,
    }
}

/// Prepare all channels in a GoIsland closure for cross-island transfer.
/// Scans captures and args for channels, installs HomeInfo on LOCAL channels,
/// adds target_island to peers, and registers them in endpoint_registry.
/// Must be called BEFORE pack_closure_for_island (which calls encode_spawn_payload
/// which calls detect_chan_typed which expects HomeInfo to be installed).
pub fn prepare_queue_handles_for_transfer(
    result: &GoIslandResult,
    target_island: u32,
    capture_types: &[TransferType],
    param_types: &[TransferType],
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
) {
    let mut notified_remote_endpoints = HashSet::new();

    for (i, &slot) in result.capture_data.iter().enumerate() {
        let Some(transfer_type) = capture_types.get(i) else {
            break;
        };
        if slot == 0 {
            continue;
        }
        let value_meta = vo_runtime::ValueMeta::from_raw(transfer_type.meta_raw);
        if !may_contain_queue_handle(value_meta.value_kind()) {
            continue;
        }
        let box_ref = slot as GcRef;
        let mut capture_slots = vec![0u64; transfer_type.slots as usize];
        for j in 0..transfer_type.slots as usize {
            capture_slots[j] = unsafe { Gc::read_slot(box_ref, j) };
        }
        prepare_value_queue_handles_for_transfer_inner(
            &capture_slots,
            value_meta,
            target_island,
            struct_metas,
            runtime_types,
            state,
            &mut notified_remote_endpoints,
        );
    }

    let mut arg_slot_idx = 0usize;
    for transfer_type in param_types {
        let slots_usize = transfer_type.slots as usize;
        if arg_slot_idx + slots_usize > result.arg_data.len() {
            break;
        }
        let value_meta = vo_runtime::ValueMeta::from_raw(transfer_type.meta_raw);
        if may_contain_queue_handle(value_meta.value_kind()) {
            prepare_value_queue_handles_for_transfer_inner(
                &result.arg_data[arg_slot_idx..arg_slot_idx + slots_usize],
                value_meta,
                target_island,
                struct_metas,
                runtime_types,
                state,
                &mut notified_remote_endpoints,
            );
        }
        arg_slot_idx += slots_usize;
    }
}

/// Recursively prepare any channels nested inside a value for cross-island transfer.
/// Walks the value tree based on metadata, finds Channel-typed slots, and installs
/// HomeInfo on LOCAL channels (or notifies home for REMOTE re-transfers).
/// Must be called BEFORE pack_slots when the value may contain nested channels.
pub fn prepare_value_queue_handles_for_transfer(
    slots: &[u64],
    value_meta: vo_runtime::ValueMeta,
    target_island: u32,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
) {
    let mut notified_remote_endpoints = HashSet::new();
    prepare_value_queue_handles_for_transfer_inner(
        slots,
        value_meta,
        target_island,
        struct_metas,
        runtime_types,
        state,
        &mut notified_remote_endpoints,
    );
}

fn prepare_value_queue_handles_for_transfer_inner(
    slots: &[u64],
    value_meta: vo_runtime::ValueMeta,
    target_island: u32,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
    notified_remote_endpoints: &mut HashSet<u64>,
) {
    use vo_runtime::ValueKind;
    use vo_runtime::gc::{Gc, GcRef};

    let vk = value_meta.value_kind();
    match vk {
        kind if kind.is_queue() => {
            let chan_ref = slots[0] as GcRef;
            if chan_ref.is_null() { return; }
            prepare_single_queue_handle(chan_ref, target_island, state, notified_remote_endpoints);
        }

        ValueKind::Struct => {
            let meta_id = value_meta.meta_id() as usize;
            if meta_id >= struct_metas.len() { return; }
            let meta = &struct_metas[meta_id];
            for field in &meta.fields {
                let fvk = field.type_info.value_kind();
                if !may_contain_queue_handle(fvk) { continue; }
                let offset = field.offset as usize;
                let fslots = field.slot_count as usize;
                if offset + fslots > slots.len() { continue; }
                let field_meta = if fvk == ValueKind::Struct {
                    let nested_meta_id = lookup_struct_meta_id_safe(field.type_info.rttid(), runtime_types);
                    vo_runtime::ValueMeta::new(nested_meta_id, fvk)
                } else {
                    vo_runtime::ValueMeta::new(0, fvk)
                };
                prepare_value_queue_handles_for_transfer_inner(
                    &slots[offset..offset + fslots],
                    field_meta,
                    target_island,
                    struct_metas,
                    runtime_types,
                    state,
                    notified_remote_endpoints,
                );
            }
        }

        ValueKind::Pointer => {
            let ptr_ref = slots[0] as GcRef;
            if ptr_ref.is_null() { return; }
            let header = Gc::header(ptr_ref);
            let obj_meta = header.value_meta();
            if !may_contain_queue_handle(obj_meta.value_kind()) { return; }
            let obj_slots_count = header.slots as usize;
            let mut obj_slots = vec![0u64; obj_slots_count];
            for i in 0..obj_slots_count {
                obj_slots[i] = unsafe { Gc::read_slot(ptr_ref, i) };
            }
            prepare_value_queue_handles_for_transfer_inner(
                &obj_slots,
                obj_meta,
                target_island,
                struct_metas,
                runtime_types,
                state,
                notified_remote_endpoints,
            );
        }

        ValueKind::Slice => {
            let slice_ref = slots[0] as GcRef;
            if slice_ref.is_null() { return; }
            let elem_meta = vo_runtime::objects::slice::elem_meta(slice_ref);
            if !may_contain_queue_handle(elem_meta.value_kind()) { return; }
            let length = vo_runtime::objects::slice::len(slice_ref);
            let elem_bytes = vo_runtime::objects::array::elem_bytes(
                vo_runtime::objects::slice::array_ref(slice_ref),
            );
            if elem_bytes == 0 { return; }
            let elem_slot_count = (elem_bytes + 7) / 8;
            let data_ptr = vo_runtime::objects::slice::data_ptr(slice_ref);
            let mut elem_buf = vec![0u64; elem_slot_count];
            for i in 0..length {
                read_element_raw(data_ptr, i, elem_bytes, &mut elem_buf);
                prepare_value_queue_handles_for_transfer(
                    &elem_buf, elem_meta, target_island, struct_metas, runtime_types, state,
                );
            }
        }

        ValueKind::Array => {
            let arr_ref = slots[0] as GcRef;
            if arr_ref.is_null() { return; }
            let elem_meta = vo_runtime::objects::array::elem_meta(arr_ref);
            if !may_contain_queue_handle(elem_meta.value_kind()) { return; }
            let length = vo_runtime::objects::array::len(arr_ref);
            let elem_bytes = vo_runtime::objects::array::elem_bytes(arr_ref);
            if elem_bytes == 0 { return; }
            let elem_slot_count = (elem_bytes + 7) / 8;
            let data_ptr = vo_runtime::objects::array::data_ptr_bytes(arr_ref);
            let mut elem_buf = vec![0u64; elem_slot_count];
            for i in 0..length {
                read_element_raw(data_ptr, i, elem_bytes, &mut elem_buf);
                prepare_value_queue_handles_for_transfer(
                    &elem_buf, elem_meta, target_island, struct_metas, runtime_types, state,
                );
            }
        }

        ValueKind::Map => {
            let map_ref = slots[0] as GcRef;
            if map_ref.is_null() { return; }
            let key_meta = vo_runtime::objects::map::key_meta(map_ref);
            let val_meta = vo_runtime::objects::map::val_meta(map_ref);
            let scan_keys = may_contain_queue_handle(key_meta.value_kind());
            let scan_vals = may_contain_queue_handle(val_meta.value_kind());
            if !scan_keys && !scan_vals { return; }
            let mut iter = vo_runtime::objects::map::iter_init(map_ref);
            while let Some((k, v)) = vo_runtime::objects::map::iter_next(&mut iter) {
                if scan_keys {
                    prepare_value_queue_handles_for_transfer_inner(
                        k,
                        key_meta,
                        target_island,
                        struct_metas,
                        runtime_types,
                        state,
                        notified_remote_endpoints,
                    );
                }
                if scan_vals {
                    prepare_value_queue_handles_for_transfer_inner(
                        v,
                        val_meta,
                        target_island,
                        struct_metas,
                        runtime_types,
                        state,
                        notified_remote_endpoints,
                    );
                }
            }
        }

        // Scalars, strings — cannot contain channels
        _ => {}
    }
}

pub fn prepare_remote_send_value_if_needed(
    ch: vo_runtime::gc::GcRef,
    slots: &[u64],
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
) {
    if ch.is_null() || !vo_runtime::objects::queue::is_remote(ch) {
        return;
    }
    let elem_meta = vo_runtime::objects::queue_state::elem_meta(ch);
    if !elem_meta.value_kind().may_contain_gc_refs() {
        return;
    }
    let target_island = vo_runtime::objects::queue::remote_proxy(ch).home_island;
    prepare_value_queue_handles_for_transfer(
        slots,
        elem_meta,
        target_island,
        struct_metas,
        runtime_types,
        state,
    );
}

fn prepare_single_queue_handle(
    chan_ref: vo_runtime::gc::GcRef,
    target_island: u32,
    state: &mut crate::vm::VmState,
    notified_remote_endpoints: &mut HashSet<u64>,
) {
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::{BACKING_LOCAL, BACKING_REMOTE};
    use vo_runtime::island::{EndpointRequestKind, IslandCommand};

    let backing = vo_runtime::objects::queue_state::QueueData::as_ref(chan_ref).backing;
    match backing {
        BACKING_LOCAL => {
            if queue::home_info(chan_ref).is_none() {
                let eid = state.allocate_endpoint_id();
                queue::install_home_info(chan_ref, eid, state.current_island_id);
            }
            if let Some(info) = queue::home_info_mut(chan_ref) {
                info.peers.insert(target_island);
                state.endpoint_registry.ensure_live(info.endpoint_id, chan_ref);
            }
        }
        BACKING_REMOTE => {
            let proxy = queue::remote_proxy(chan_ref);
            if notified_remote_endpoints.insert(proxy.endpoint_id) {
                state.send_to_island(proxy.home_island, IslandCommand::EndpointRequest {
                    endpoint_id: proxy.endpoint_id,
                    kind: EndpointRequestKind::Transfer { new_peer: target_island },
                    from_island: state.current_island_id,
                    fiber_id: 0,
                });
            }
        }
        _ => {}
    }
}

fn may_contain_queue_handle(vk: vo_runtime::ValueKind) -> bool {
    matches!(vk,
        vo_runtime::ValueKind::Channel
        | vo_runtime::ValueKind::Port
        | vo_runtime::ValueKind::Struct
        | vo_runtime::ValueKind::Pointer
        | vo_runtime::ValueKind::Slice
        | vo_runtime::ValueKind::Array
        | vo_runtime::ValueKind::Map
        | vo_runtime::ValueKind::Interface
    )
}

fn lookup_struct_meta_id_safe(rttid: u32, runtime_types: &[vo_common_core::RuntimeType]) -> u32 {
    if let Some(rt) = runtime_types.get(rttid as usize) {
        if let vo_common_core::RuntimeType::Struct { meta_id, .. } = rt {
            return *meta_id;
        }
        if let vo_common_core::RuntimeType::Named { struct_meta_id, .. } = rt {
            if let Some(id) = struct_meta_id {
                return *id;
            }
        }
    }
    0 // Fallback — struct without meta
}

fn read_element_raw(base_ptr: *mut u8, idx: usize, elem_bytes: usize, dst: &mut [u64]) {
    let ptr = unsafe { base_ptr.add(idx * elem_bytes) };
    match elem_bytes {
        1 => dst[0] = unsafe { *ptr } as u64,
        2 => dst[0] = unsafe { *(ptr as *const u16) } as u64,
        4 => dst[0] = unsafe { *(ptr as *const u32) } as u64,
        8 => dst[0] = unsafe { *(ptr as *const u64) },
        _ => {
            let dst_bytes = dst.as_mut_ptr() as *mut u8;
            unsafe { core::ptr::copy_nonoverlapping(ptr, dst_bytes, elem_bytes) };
        }
    }
}

/// Pack closure data for cross-island transfer with proper type serialization.
/// Uses type info from FunctionDef to correctly serialize all sendable types.
pub fn pack_closure_for_island(
    gc: &vo_runtime::gc::Gc,
    result: &GoIslandResult,
    capture_types: &[TransferType],
    param_types: &[TransferType],
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    runtime_types: &[vo_common_core::RuntimeType],
) -> Vec<u8> {
    vo_runtime::island_msg::encode_spawn_payload(
        gc,
        result.func_id,
        &result.capture_data,
        capture_types,
        &result.arg_data,
        param_types,
        struct_metas,
        runtime_types,
    )
}
