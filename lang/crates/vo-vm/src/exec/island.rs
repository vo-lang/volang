//! Island instructions: IslandNew, GoIsland

use vo_common_core::TransferType;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::island;
use vo_runtime::objects::closure;
use vo_runtime::slot::Slot;

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[cfg(not(feature = "std"))]
use alloc::{
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use hashbrown::HashSet;
#[cfg(feature = "std")]
use std::{
    string::{String, ToString},
    vec::Vec,
};

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

pub type QueueTransferResult = Result<(), String>;

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
pub fn exec_go_island(stack: *const Slot, bp: usize, inst: &Instruction) -> GoIslandResult {
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
) -> QueueTransferResult {
    let mut notified_remote_endpoints = HashSet::new();

    for (i, &slot) in result.capture_data.iter().enumerate() {
        let Some(transfer_type) = capture_types.get(i) else {
            return Err(format!(
                "GoIsland capture {i} missing transfer metadata for func_id {}",
                result.func_id
            ));
        };
        if slot == 0 {
            continue;
        }
        let value_meta = vo_runtime::ValueMeta::from_raw(transfer_type.meta_raw);
        ensure_transfer_kind_is_sendable(
            value_meta.value_kind(),
            &format!("GoIsland capture {i}"),
        )?;
        if !may_contain_queue_handle(value_meta.value_kind()) {
            continue;
        }
        let box_ref = slot as GcRef;
        let mut capture_slots = vec![0u64; transfer_type.slots as usize];
        for (j, slot) in capture_slots.iter_mut().enumerate() {
            *slot = unsafe { Gc::read_slot(box_ref, j) };
        }
        prepare_value_queue_handles_for_transfer_inner(
            &capture_slots,
            value_meta,
            target_island,
            struct_metas,
            runtime_types,
            state,
            &mut notified_remote_endpoints,
        )?;
    }

    let mut arg_slot_idx = 0usize;
    for (i, transfer_type) in param_types.iter().enumerate() {
        let slots_usize = transfer_type.slots as usize;
        if arg_slot_idx + slots_usize > result.arg_data.len() {
            return Err(format!(
                "GoIsland arg {i} metadata requires slots {}..{} but only {} arg slots exist",
                arg_slot_idx,
                arg_slot_idx + slots_usize,
                result.arg_data.len()
            ));
        }
        let value_meta = vo_runtime::ValueMeta::from_raw(transfer_type.meta_raw);
        ensure_transfer_kind_is_sendable(value_meta.value_kind(), &format!("GoIsland arg {i}"))?;
        if may_contain_queue_handle(value_meta.value_kind()) {
            prepare_value_queue_handles_for_transfer_inner(
                &result.arg_data[arg_slot_idx..arg_slot_idx + slots_usize],
                value_meta,
                target_island,
                struct_metas,
                runtime_types,
                state,
                &mut notified_remote_endpoints,
            )?;
        }
        arg_slot_idx += slots_usize;
    }
    if arg_slot_idx != result.arg_data.len() {
        return Err(format!(
            "GoIsland transfer metadata covers {arg_slot_idx} arg slots but closure passed {}",
            result.arg_data.len()
        ));
    }
    Ok(())
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
) -> QueueTransferResult {
    let mut notified_remote_endpoints = HashSet::new();
    prepare_value_queue_handles_for_transfer_inner(
        slots,
        value_meta,
        target_island,
        struct_metas,
        runtime_types,
        state,
        &mut notified_remote_endpoints,
    )
}

fn prepare_value_queue_handles_for_transfer_inner(
    slots: &[u64],
    value_meta: vo_runtime::ValueMeta,
    target_island: u32,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
    notified_remote_endpoints: &mut HashSet<u64>,
) -> QueueTransferResult {
    use vo_runtime::gc::{Gc, GcRef};
    use vo_runtime::ValueKind;

    let vk = value_meta.value_kind();
    match vk {
        kind if kind.is_queue() => {
            ensure_transfer_kind_is_sendable(kind, "queue transfer queue value")?;
            if slots.is_empty() {
                return Err(format!(
                    "queue transfer value for {:?} has no slots",
                    value_meta.value_kind()
                ));
            }
            let chan_ref = slots[0] as GcRef;
            if chan_ref.is_null() {
                return Ok(());
            }
            prepare_single_queue_handle(chan_ref, target_island, state, notified_remote_endpoints);
        }

        ValueKind::Struct => {
            let meta_id = value_meta.meta_id() as usize;
            let Some(meta) = struct_metas.get(meta_id) else {
                return Err(format!(
                    "missing StructMeta id {meta_id} during queue transfer"
                ));
            };
            for field in &meta.fields {
                let fvk = field.type_info.value_kind();
                ensure_transfer_kind_is_sendable(
                    fvk,
                    &format!("field {} during queue transfer", field.name),
                )?;
                if !may_contain_queue_handle(fvk) {
                    continue;
                }
                let offset = field.offset as usize;
                let fslots = field.slot_count as usize;
                if offset + fslots > slots.len() {
                    return Err(format!(
                        "field {} layout {}..{} exceeds value slots {} during queue transfer",
                        field.name,
                        offset,
                        offset + fslots,
                        slots.len()
                    ));
                }
                let field_meta = if fvk == ValueKind::Struct {
                    let nested_meta_id = lookup_struct_meta_id(field.type_info.rttid(), runtime_types)
                        .ok_or_else(|| {
                            format!(
                                "missing struct runtime type rttid {} for field {} during queue transfer",
                                field.type_info.rttid(),
                                field.name
                            )
                        })?;
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
                )?;
            }
        }

        ValueKind::Pointer => {
            if slots.is_empty() {
                return Err("queue transfer pointer value has no slots".to_string());
            }
            let ptr_ref = slots[0] as GcRef;
            if ptr_ref.is_null() {
                return Ok(());
            }
            let Some(_) = state.gc.canonicalize_ref(ptr_ref) else {
                return Err(format!(
                    "queue transfer pointer {:p} is not a GC object",
                    ptr_ref
                ));
            };
            let meta_id = value_meta.meta_id() as usize;
            let Some(meta) = struct_metas.get(meta_id) else {
                return Err(format!(
                    "missing pointee StructMeta id {meta_id} during queue transfer"
                ));
            };
            let obj_meta = vo_runtime::ValueMeta::new(value_meta.meta_id(), ValueKind::Struct);
            let obj_slots_count = meta.slot_types.len();
            let mut obj_slots = vec![0u64; obj_slots_count];
            for (i, slot) in obj_slots.iter_mut().enumerate() {
                *slot = unsafe { Gc::read_slot(ptr_ref, i) };
            }
            prepare_value_queue_handles_for_transfer_inner(
                &obj_slots,
                obj_meta,
                target_island,
                struct_metas,
                runtime_types,
                state,
                notified_remote_endpoints,
            )?;
        }

        ValueKind::Slice => {
            if slots.is_empty() {
                return Err("queue transfer slice value has no slots".to_string());
            }
            let slice_ref = slots[0] as GcRef;
            if slice_ref.is_null() {
                return Ok(());
            }
            let elem_meta = vo_runtime::objects::slice::elem_meta(slice_ref);
            ensure_transfer_kind_is_sendable(
                elem_meta.value_kind(),
                "slice element during queue transfer",
            )?;
            if !may_contain_queue_handle(elem_meta.value_kind()) {
                return Ok(());
            }
            let length = vo_runtime::objects::slice::len(slice_ref);
            let elem_bytes = vo_runtime::objects::array::elem_bytes(
                vo_runtime::objects::slice::array_ref(slice_ref),
            );
            if elem_bytes == 0 {
                validate_zero_byte_sequence_transfer(elem_meta, struct_metas)?;
                return Ok(());
            }
            let elem_slot_count =
                sequence_elem_slots_for_transfer(elem_meta, elem_bytes, struct_metas)?;
            let data_ptr = vo_runtime::objects::slice::data_ptr(slice_ref);
            let mut elem_buf = vec![0u64; elem_slot_count];
            for i in 0..length {
                read_element_raw(data_ptr, i, elem_bytes, &mut elem_buf);
                prepare_value_queue_handles_for_transfer_inner(
                    &elem_buf,
                    elem_meta,
                    target_island,
                    struct_metas,
                    runtime_types,
                    state,
                    notified_remote_endpoints,
                )?;
            }
        }

        ValueKind::Array => {
            if slots.is_empty() {
                return Err("queue transfer array value has no slots".to_string());
            }
            let arr_ref = slots[0] as GcRef;
            if arr_ref.is_null() {
                return Ok(());
            }
            let elem_meta = vo_runtime::objects::array::elem_meta(arr_ref);
            ensure_transfer_kind_is_sendable(
                elem_meta.value_kind(),
                "array element during queue transfer",
            )?;
            if !may_contain_queue_handle(elem_meta.value_kind()) {
                return Ok(());
            }
            let length = vo_runtime::objects::array::len(arr_ref);
            let elem_bytes = vo_runtime::objects::array::elem_bytes(arr_ref);
            if elem_bytes == 0 {
                validate_zero_byte_sequence_transfer(elem_meta, struct_metas)?;
                return Ok(());
            }
            let elem_slot_count =
                sequence_elem_slots_for_transfer(elem_meta, elem_bytes, struct_metas)?;
            let data_ptr = vo_runtime::objects::array::data_ptr_bytes(arr_ref);
            let mut elem_buf = vec![0u64; elem_slot_count];
            for i in 0..length {
                read_element_raw(data_ptr, i, elem_bytes, &mut elem_buf);
                prepare_value_queue_handles_for_transfer_inner(
                    &elem_buf,
                    elem_meta,
                    target_island,
                    struct_metas,
                    runtime_types,
                    state,
                    notified_remote_endpoints,
                )?;
            }
        }

        ValueKind::Map => {
            if slots.is_empty() {
                return Err("queue transfer map value has no slots".to_string());
            }
            let map_ref = slots[0] as GcRef;
            if map_ref.is_null() {
                return Ok(());
            }
            let key_meta = vo_runtime::objects::map::key_meta(map_ref);
            let val_meta = vo_runtime::objects::map::val_meta(map_ref);
            ensure_transfer_kind_is_sendable(
                key_meta.value_kind(),
                "map key during queue transfer",
            )?;
            ensure_transfer_kind_is_sendable(
                val_meta.value_kind(),
                "map value during queue transfer",
            )?;
            let scan_keys = may_contain_queue_handle(key_meta.value_kind());
            let scan_vals = may_contain_queue_handle(val_meta.value_kind());
            if !scan_keys && !scan_vals {
                return Ok(());
            }
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
                    )?;
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
                    )?;
                }
            }
        }

        // Scalars, strings — cannot contain channels
        _ => {}
    }
    Ok(())
}

pub fn prepare_remote_send_value_if_needed(
    ch: vo_runtime::gc::GcRef,
    slots: &[u64],
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
) -> QueueTransferResult {
    if ch.is_null() || !vo_runtime::objects::queue::is_remote(ch) {
        return Ok(());
    }
    let elem_meta = vo_runtime::objects::queue_state::elem_meta(ch);
    ensure_transfer_kind_is_sendable(elem_meta.value_kind(), "remote queue element")?;
    if !elem_meta.value_kind().may_contain_gc_refs() {
        return Ok(());
    }
    let target_island = vo_runtime::objects::queue::remote_proxy(ch).home_island;
    prepare_value_queue_handles_for_transfer(
        slots,
        elem_meta,
        target_island,
        struct_metas,
        runtime_types,
        state,
    )
}

fn prepare_single_queue_handle(
    chan_ref: vo_runtime::gc::GcRef,
    target_island: u32,
    state: &mut crate::vm::VmState,
    notified_remote_endpoints: &mut HashSet<u64>,
) {
    use vo_runtime::island::{EndpointRequestKind, IslandCommand};
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::QueueBacking;

    match vo_runtime::objects::queue_state::backing(chan_ref) {
        QueueBacking::Local => {
            if queue::home_info(chan_ref).is_none() {
                let eid = state.allocate_endpoint_id();
                queue::install_home_info(chan_ref, eid, state.current_island_id);
            }
            let endpoint_id = queue::add_home_peer(chan_ref, target_island);
            state.endpoint_registry.ensure_live(endpoint_id, chan_ref);
            state.mark_gc_all_roots_dirty();
        }
        QueueBacking::Remote => {
            let proxy = queue::remote_proxy(chan_ref);
            if notified_remote_endpoints.insert(proxy.endpoint_id) {
                state.send_to_island(
                    proxy.home_island,
                    IslandCommand::EndpointRequest {
                        endpoint_id: proxy.endpoint_id,
                        kind: EndpointRequestKind::Transfer {
                            new_peer: target_island,
                        },
                        from_island: state.current_island_id,
                        fiber_id: 0,
                    },
                );
            }
        }
    }
}

fn may_contain_queue_handle(vk: vo_runtime::ValueKind) -> bool {
    matches!(
        vk,
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

fn ensure_transfer_kind_is_sendable(
    vk: vo_runtime::ValueKind,
    context: &str,
) -> QueueTransferResult {
    match vk {
        vo_runtime::ValueKind::Void
        | vo_runtime::ValueKind::Bool
        | vo_runtime::ValueKind::Int
        | vo_runtime::ValueKind::Int8
        | vo_runtime::ValueKind::Int16
        | vo_runtime::ValueKind::Int32
        | vo_runtime::ValueKind::Int64
        | vo_runtime::ValueKind::Uint
        | vo_runtime::ValueKind::Uint8
        | vo_runtime::ValueKind::Uint16
        | vo_runtime::ValueKind::Uint32
        | vo_runtime::ValueKind::Uint64
        | vo_runtime::ValueKind::Float32
        | vo_runtime::ValueKind::Float64
        | vo_runtime::ValueKind::Array
        | vo_runtime::ValueKind::Struct
        | vo_runtime::ValueKind::String
        | vo_runtime::ValueKind::Slice
        | vo_runtime::ValueKind::Map
        | vo_runtime::ValueKind::Pointer
        | vo_runtime::ValueKind::Port => Ok(()),
        vo_runtime::ValueKind::Channel
        | vo_runtime::ValueKind::Closure
        | vo_runtime::ValueKind::Island
        | vo_runtime::ValueKind::Interface => Err(format!(
            "{context}: non-sendable {:?} metadata cannot be packed for island transfer",
            vk
        )),
    }
}

fn lookup_struct_meta_id(rttid: u32, runtime_types: &[vo_common_core::RuntimeType]) -> Option<u32> {
    if let Some(rt) = runtime_types.get(rttid as usize) {
        if let vo_common_core::RuntimeType::Struct { meta_id, .. } = rt {
            return Some(*meta_id);
        }
        if let vo_common_core::RuntimeType::Named {
            struct_meta_id: Some(id),
            ..
        } = rt
        {
            return Some(*id);
        }
    }
    None
}

fn sequence_elem_slots_for_transfer(
    elem_meta: vo_runtime::ValueMeta,
    elem_bytes: usize,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
) -> Result<usize, String> {
    if elem_meta.value_kind() == vo_runtime::ValueKind::Struct {
        let meta_id = elem_meta.meta_id() as usize;
        return struct_metas
            .get(meta_id)
            .map(|meta| meta.slot_types.len())
            .ok_or_else(|| {
                format!(
                    "missing StructMeta id {meta_id} for sequence element during queue transfer"
                )
            });
    }
    Ok(elem_bytes.div_ceil(8))
}

fn validate_zero_byte_sequence_transfer(
    elem_meta: vo_runtime::ValueMeta,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
) -> QueueTransferResult {
    if elem_meta.value_kind() != vo_runtime::ValueKind::Struct {
        return Ok(());
    }
    let meta_id = elem_meta.meta_id() as usize;
    let Some(meta) = struct_metas.get(meta_id) else {
        return Err(format!(
            "missing StructMeta id {meta_id} for zero-byte sequence element during queue transfer"
        ));
    };
    if !meta.fields.is_empty() {
        return Err(format!(
            "zero-byte sequence element StructMeta id {meta_id} has {} field(s) during queue transfer",
            meta.fields.len()
        ));
    }
    Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use vo_common_core::bytecode::{FieldMeta, StructMeta};
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{SlotType, ValueKind, ValueMeta, ValueRttid};

    #[test]
    fn missing_nested_struct_runtime_type_does_not_guess_meta_zero() {
        let mut state = crate::vm::VmState::new();
        let ch = queue::create(
            &mut state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
            1,
            0,
        );
        let struct_metas = vec![
            StructMeta {
                slot_types: vec![SlotType::GcRef],
                fields: vec![FieldMeta {
                    name: "ch".to_string(),
                    offset: 0,
                    slot_count: 1,
                    type_info: ValueRttid::new(0, ValueKind::Port),
                    embedded: false,
                    tag: None,
                }],
                field_index: HashMap::new(),
            },
            StructMeta {
                slot_types: vec![SlotType::Value],
                fields: vec![FieldMeta {
                    name: "nested".to_string(),
                    offset: 0,
                    slot_count: 1,
                    type_info: ValueRttid::new(999, ValueKind::Struct),
                    embedded: false,
                    tag: None,
                }],
                field_index: HashMap::new(),
            },
        ];

        let err = prepare_value_queue_handles_for_transfer(
            &[ch as u64],
            ValueMeta::new(1, ValueKind::Struct),
            7,
            &struct_metas,
            &[],
            &mut state,
        )
        .expect_err("missing nested struct metadata must be a transfer contract error");

        assert!(err.contains("missing struct runtime type"), "{err}");
        assert!(queue::home_info(ch).is_none());
    }

    #[test]
    fn go_island_transfer_rejects_non_sendable_metadata_before_pack() {
        let mut state = crate::vm::VmState::new();
        let result = GoIslandResult {
            island: core::ptr::null_mut(),
            func_id: 7,
            capture_data: Vec::new(),
            arg_data: vec![0, 0],
        };
        let param_types = vec![TransferType {
            meta_raw: ValueMeta::new(0, ValueKind::Interface).to_raw(),
            rttid_raw: ValueRttid::new(0, ValueKind::Interface).to_raw(),
            slots: 2,
        }];

        let err =
            prepare_queue_handles_for_transfer(&result, 3, &[], &param_types, &[], &[], &mut state)
                .expect_err("non-sendable transfer metadata must be rejected before pack");

        assert!(err.contains("non-sendable"), "{err}");
        assert!(err.contains("Interface"), "{err}");
    }

    #[test]
    fn queue_handle_transfer_has_no_metadata_skip_paths() {
        let source = include_str!("island.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("island source should contain tests section");
        let normalized = source.split_whitespace().collect::<String>();

        assert!(
            source.contains("pub type QueueTransferResult = Result<(), String>;"),
            "queue-handle transfer must expose metadata/layout failures as Result"
        );
        assert!(
            !normalized.contains("ifmeta_id>=struct_metas.len(){return;}"),
            "missing struct metadata must not be silently skipped"
        );
        assert!(
            !normalized.contains("ifoffset+fslots>slots.len(){continue;}"),
            "field layout extending past the value must not be silently skipped"
        );
        assert!(
            !normalized.contains("else{continue;}"),
            "nested struct metadata lookup must not silently continue"
        );
    }
}
