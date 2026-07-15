//! Island instructions: IslandNew, GoIsland

use vo_common_core::bytecode::{FunctionDef, MethodInfo, Module, RuntimeTypeResolver};
use vo_common_core::types::ValueRttid;
use vo_common_core::{RuntimeType, SlotType, TransferType, ValueKind};
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::island;
use vo_runtime::slot::Slot;

use crate::frame_call::ValidClosureTarget;
use crate::instruction::Instruction;
use crate::runtime_boundary::{IslandCommandEffect, RuntimeRollback};
use crate::vm::helpers::{stack_get, stack_set};

#[cfg(not(feature = "std"))]
use alloc::{
    format,
    string::{String, ToString},
    sync::Arc,
    vec,
    vec::Vec,
};
use hashbrown::HashSet;
#[cfg(feature = "std")]
use std::{
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};

/// Result of go @(island) - spawn fiber on remote island.
///
/// Contains raw capture and argument data. The VM coordinator will pack these
/// with proper type metadata from FunctionDef before sending.
pub struct GoIslandResult {
    pub island: GcRef,
    pub func_id: u32,
    pub receiver_capture_slots: u16,
    pub capture_data: Vec<Slot>,
    pub arg_data: Vec<Slot>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DirectMethodReceiverTransfer {
    pub transfer_type: TransferType,
    pub raw_capture_slots: u16,
}

pub type QueueTransferResult = Result<(), String>;

#[derive(Debug, Default)]
pub struct QueueTransferCommit {
    state: QueueTransferCommitState,
    home_info_snapshots: Vec<(GcRef, Option<vo_runtime::objects::queue::HomeInfoSnapshot>)>,
}

#[derive(Debug, Default)]
enum QueueTransferCommitState {
    #[default]
    Uncommitted,
    Committed {
        endpoint_registry: crate::vm::EndpointRegistrySnapshot,
    },
}

impl QueueTransferCommit {
    #[inline]
    pub fn requires_terminal_commit(&self) -> bool {
        matches!(self.state, QueueTransferCommitState::Committed { .. })
    }

    fn snapshot_local_endpoint_state(&mut self, state: &crate::vm::VmState, chan_ref: GcRef) {
        if matches!(self.state, QueueTransferCommitState::Uncommitted) {
            self.state = QueueTransferCommitState::Committed {
                endpoint_registry: state.endpoint_registry.snapshot(),
            };
        }
        if !self
            .home_info_snapshots
            .iter()
            .any(|(existing, _)| *existing == chan_ref)
        {
            // Safety: transfer preflight stores only canonical live queue handles.
            self.home_info_snapshots.push((chan_ref, unsafe {
                vo_runtime::objects::queue::home_info_snapshot(chan_ref)
            }));
        }
    }

    fn commit_local_endpoint_state(&mut self, state: &crate::vm::VmState, chan_ref: GcRef) {
        self.snapshot_local_endpoint_state(state, chan_ref);
    }

    pub(crate) fn absorb(&mut self, other: Self) {
        let QueueTransferCommitState::Committed { endpoint_registry } = other.state else {
            return;
        };
        if matches!(self.state, QueueTransferCommitState::Uncommitted) {
            self.state = QueueTransferCommitState::Committed { endpoint_registry };
        }
        for (chan_ref, snapshot) in other.home_info_snapshots {
            if !self
                .home_info_snapshots
                .iter()
                .any(|(existing, _)| *existing == chan_ref)
            {
                self.home_info_snapshots.push((chan_ref, snapshot));
            }
        }
    }

    pub(crate) fn restore_committed_local_endpoint_state(self, state: &mut crate::vm::VmState) {
        let QueueTransferCommitState::Committed { endpoint_registry } = self.state else {
            return;
        };
        for (chan_ref, snapshot) in self.home_info_snapshots {
            // Safety: snapshots retain their live queue handle until rollback.
            unsafe { vo_runtime::objects::queue::restore_home_info_snapshot(chan_ref, snapshot) };
        }
        state.endpoint_registry.restore(endpoint_registry);
        state.mark_gc_all_roots_dirty();
    }

    pub(crate) fn into_runtime_rollback(self) -> Option<RuntimeRollback> {
        let QueueTransferCommitState::Committed { endpoint_registry } = self.state else {
            return None;
        };
        Some(RuntimeRollback::endpoint_transfer(
            endpoint_registry,
            self.home_info_snapshots,
        ))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum QueueTransferPass {
    Validate,
    Commit,
}

impl QueueTransferPass {
    #[inline]
    fn commits(self) -> bool {
        matches!(self, Self::Commit)
    }
}

fn preflight_queue_transfer_route(
    chan_ref: vo_runtime::gc::GcRef,
    target_island: u32,
    state: &crate::vm::VmState,
) -> Result<(), String> {
    #[cfg(feature = "std")]
    {
        if target_island != state.current_island_id {
            state
                .can_route_to_island(target_island)
                .map_err(|error| error.to_string())?;
        }
        if unsafe { vo_runtime::objects::queue::is_remote(chan_ref) } {
            let home_island =
                unsafe { vo_runtime::objects::queue::remote_proxy(chan_ref) }.home_island;
            if home_island != state.current_island_id {
                state
                    .can_route_to_island(home_island)
                    .map_err(|error| error.to_string())?;
            }
        }
    }
    #[cfg(not(feature = "std"))]
    {
        let _ = (chan_ref, target_island, state);
    }
    Ok(())
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
/// GoIsland instruction: a=island, b=closure, c=args_start. CallLayout owns arg_slots.
///
/// Note: Closures themselves are NOT sendable. This function:
/// 1. Extracts func_id from the closure
/// 2. Reads raw capture slots (GcRefs to escaped variables)
/// 3. Reads raw argument slots from stack
/// 4. Returns data for VM coordinator to handle packing with proper type info
pub(crate) fn exec_go_island(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    arg_slots: usize,
    island_handle: GcRef,
    closure_target: &ValidClosureTarget<'_>,
) -> GoIslandResult {
    let args_start = bp + inst.c as usize;

    // Read raw capture slots
    let capture_count = closure_target.capture_count();
    let mut capture_data = Vec::with_capacity(capture_count);
    for i in 0..capture_count {
        capture_data.push(closure_target.capture(i));
    }

    // Read raw argument slots
    let mut arg_data = Vec::with_capacity(arg_slots);
    for i in 0..arg_slots {
        arg_data.push(stack_get(stack, args_start + i));
    }

    GoIslandResult {
        island: island_handle,
        func_id: closure_target.func_id,
        receiver_capture_slots: closure_target.layout.receiver_capture_count as u16,
        capture_data,
        arg_data,
    }
}

pub fn direct_method_receiver_transfer_type(
    module: &Module,
    func_id: u32,
    func: &FunctionDef,
    receiver_capture_slots: u16,
) -> Result<TransferType, String> {
    direct_method_receiver_transfer_plan(module, func_id, func, receiver_capture_slots)
        .map(|plan| plan.transfer_type)
}

pub fn direct_method_receiver_transfer_plan(
    module: &Module,
    func_id: u32,
    func: &FunctionDef,
    receiver_capture_slots: u16,
) -> Result<DirectMethodReceiverTransfer, String> {
    if receiver_capture_slots == 0 || receiver_capture_slots != func.recv_slots {
        return Err(format!(
            "direct method receiver capture slots {} do not match recv_slots={} for func_id {}",
            receiver_capture_slots, func.recv_slots, func_id
        ));
    }

    let mut found = None;
    for (named_rttid, runtime_type) in module.runtime_types.iter().enumerate() {
        let RuntimeType::Named { id, .. } = runtime_type else {
            continue;
        };
        let Some(named) = module.named_type_metas.get(*id as usize) else {
            continue;
        };
        let Some((method_name, method)) = named.methods.iter().find(|(method_name, method)| {
            direct_method_receiver_entry_matches(module, func_id, func, method_name, method)
        }) else {
            continue;
        };
        let named_value_rttid =
            ValueRttid::new(named_rttid as u32, named.underlying_rttid.value_kind());
        let Some(frame_receiver_layout) = func.slot_types.get(..receiver_capture_slots as usize)
        else {
            continue;
        };
        let promoted_boxed_receiver_wrapper =
            direct_method_promoted_wrapper_matches(&func.name, method_name)
                && receiver_capture_slots == 1
                && frame_receiver_layout == [SlotType::GcRef];
        let receiver_rttid = if promoted_boxed_receiver_wrapper {
            pointer_rttid_for_target(module, named_value_rttid).ok_or_else(|| {
                format!(
                    "promoted method receiver for func_id {func_id} requires pointer RTTID targeting named type {}",
                    named.name
                )
            })?
        } else if method.is_pointer_receiver {
            pointer_rttid_for_target(module, named_value_rttid).ok_or_else(|| {
                format!(
                    "direct method receiver for func_id {func_id} is a pointer receiver but no pointer RTTID targets named type {}",
                    named.name
                )
            })?
        } else {
            named_value_rttid
        };

        let Some(meta) = module.canonical_value_meta_for_value_rttid(receiver_rttid) else {
            continue;
        };
        let Some(layout) = module.slot_layout_for_value_rttid(receiver_rttid) else {
            continue;
        };
        let boxed_value_receiver_wrapper = method.receiver_is_iface_boxed
            && !method.is_pointer_receiver
            && !promoted_boxed_receiver_wrapper
            && matches!(
                named.underlying_rttid.value_kind(),
                ValueKind::Struct | ValueKind::Array
            )
            && receiver_capture_slots == 1
            && frame_receiver_layout == [SlotType::GcRef];
        let raw_capture_slots = if promoted_boxed_receiver_wrapper {
            receiver_capture_slots
        } else if boxed_value_receiver_wrapper {
            0
        } else {
            if layout.len() != receiver_capture_slots as usize || layout != frame_receiver_layout {
                continue;
            }
            receiver_capture_slots
        };
        let transfer_type = TransferType {
            meta_raw: meta.to_raw(),
            rttid_raw: receiver_rttid.to_raw(),
            slots: layout.len() as u16,
        };
        let plan = DirectMethodReceiverTransfer {
            transfer_type,
            raw_capture_slots,
        };
        if let Some(previous) = found {
            if previous != plan {
                return Err(format!(
                    "direct method receiver metadata for func_id {func_id} is ambiguous"
                ));
            }
        } else {
            found = Some(plan);
        }
    }

    found.ok_or_else(|| {
        format!(
            "direct method receiver metadata missing for func_id {func_id} ({})",
            func.name
        )
    })
}

fn direct_method_promoted_wrapper_matches(func_name: &str, method_name: &str) -> bool {
    func_name
        .rsplit('.')
        .next()
        .and_then(|short| short.strip_suffix("$promoted"))
        == Some(method_name)
}

fn direct_method_receiver_entry_matches(
    module: &Module,
    func_id: u32,
    func: &FunctionDef,
    method_name: &str,
    method: &MethodInfo,
) -> bool {
    if method.func_id == func_id {
        return true;
    }
    if method_name != func.name || method.is_pointer_receiver || !method.receiver_is_iface_boxed {
        return false;
    }
    module
        .functions
        .get(method.func_id as usize)
        .is_some_and(|wrapper| wrapper.name == format!("{method_name}$iface"))
}

fn transfer_type_slot_count(transfer_types: &[TransferType]) -> usize {
    transfer_types
        .iter()
        .map(|transfer_type| transfer_type.slots as usize)
        .sum()
}

pub fn go_island_sender_param_transfer_types(
    module: &Module,
    func_id: u32,
    func: &FunctionDef,
    arg_slots: usize,
) -> Result<Vec<TransferType>, String> {
    let declared_slots = transfer_type_slot_count(&func.param_types);
    if declared_slots == arg_slots {
        return Ok(func.param_types.clone());
    }
    let recv_slots = func.recv_slots as usize;
    if recv_slots > 0 && declared_slots + recv_slots == arg_slots {
        let plan = direct_method_receiver_transfer_plan(module, func_id, func, func.recv_slots)?;
        if plan.raw_capture_slots != func.recv_slots {
            return Err(format!(
                "GoIsland method-expression receiver for func_id {func_id} ({}) requires receiver-inclusive param_types",
                func.name
            ));
        }
        let mut param_types = Vec::with_capacity(func.param_types.len() + 1);
        param_types.push(plan.transfer_type);
        param_types.extend_from_slice(&func.param_types);
        return Ok(param_types);
    }
    Err(format!(
        "GoIsland arg transfer metadata covers {declared_slots} slots but closure passed {arg_slots} slots for func_id {func_id} ({})",
        func.name
    ))
}

pub fn go_island_payload_param_transfer_types(
    module: &Module,
    func_id: u32,
    func: &FunctionDef,
    wire_arg_count: usize,
) -> Result<Vec<TransferType>, String> {
    if func.param_types.len() == wire_arg_count {
        return Ok(func.param_types.clone());
    }
    if func.recv_slots > 0 && func.param_types.len() + 1 == wire_arg_count {
        let plan = direct_method_receiver_transfer_plan(module, func_id, func, func.recv_slots)?;
        if plan.raw_capture_slots != func.recv_slots {
            return Err(format!(
                "GoIsland spawn payload receiver for func_id {func_id} ({}) requires receiver-inclusive param_types",
                func.name
            ));
        }
        let mut param_types = Vec::with_capacity(func.param_types.len() + 1);
        param_types.push(plan.transfer_type);
        param_types.extend_from_slice(&func.param_types);
        return Ok(param_types);
    }
    Err(format!(
        "GoIsland spawn payload arg transfer count {wire_arg_count} does not match param_types count {} for func_id {func_id} ({})",
        func.param_types.len(),
        func.name
    ))
}

pub fn apply_direct_method_receiver_transfer_plan(
    mut result: GoIslandResult,
    plan: DirectMethodReceiverTransfer,
) -> GoIslandResult {
    result.receiver_capture_slots = plan.raw_capture_slots;
    result
}

fn pointer_rttid_for_target(module: &Module, target: ValueRttid) -> Option<ValueRttid> {
    module
        .runtime_types
        .iter()
        .enumerate()
        .find_map(|(idx, runtime_type)| match runtime_type {
            RuntimeType::Pointer(elem) if *elem == target => {
                Some(ValueRttid::new(idx as u32, ValueKind::Pointer))
            }
            _ => None,
        })
}

/// Prepare all channels in a GoIsland closure for cross-island transfer.
/// Scans captures and args for channels, installs HomeInfo on LOCAL channels,
/// adds target_island to peers, and registers them in endpoint_registry.
/// Must be called before pack_closure_for_island so packed queue handles observe
/// installed HomeInfo and registered endpoints.
pub fn prepare_queue_handles_for_transfer(
    result: &GoIslandResult,
    target_island: u32,
    capture_types: &[TransferType],
    param_types: &[TransferType],
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
    island_effects: &mut Vec<IslandCommandEffect>,
) -> Result<QueueTransferCommit, String> {
    let mut validation_notified_remote_endpoints = HashSet::new();
    let mut validation_island_effects = Vec::new();
    let mut validation_commit = QueueTransferCommit::default();
    prepare_queue_handles_for_transfer_pass(
        result,
        target_island,
        capture_types,
        param_types,
        struct_metas,
        named_type_metas,
        runtime_types,
        state,
        &mut validation_notified_remote_endpoints,
        &mut validation_island_effects,
        &mut validation_commit,
        QueueTransferPass::Validate,
    )?;

    let mut notified_remote_endpoints = HashSet::new();
    let mut commit = QueueTransferCommit::default();
    prepare_queue_handles_for_transfer_pass(
        result,
        target_island,
        capture_types,
        param_types,
        struct_metas,
        named_type_metas,
        runtime_types,
        state,
        &mut notified_remote_endpoints,
        island_effects,
        &mut commit,
        QueueTransferPass::Commit,
    )?;
    Ok(commit)
}

fn prepare_queue_handles_for_transfer_pass(
    result: &GoIslandResult,
    target_island: u32,
    capture_types: &[TransferType],
    param_types: &[TransferType],
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
    notified_remote_endpoints: &mut HashSet<u64>,
    island_effects: &mut Vec<IslandCommandEffect>,
    commit: &mut QueueTransferCommit,
    pass: QueueTransferPass,
) -> QueueTransferResult {
    let mut visited_pointers = HashSet::new();
    if result.receiver_capture_slots > 0 {
        if capture_types.len() != 1
            || capture_types[0].slots != result.receiver_capture_slots
            || result.capture_data.len() != result.receiver_capture_slots as usize
        {
            return Err(format!(
                "GoIsland direct receiver capture metadata mismatch for func_id {}: raw slots {}, capture slots {}, transfer entries {}",
                result.func_id,
                result.receiver_capture_slots,
                result.capture_data.len(),
                capture_types.len()
            ));
        }
        let transfer_type = capture_types[0];
        let value_meta = checked_transfer_value_meta(
            transfer_type,
            struct_metas,
            named_type_metas,
            runtime_types,
            "GoIsland direct receiver capture",
        )?;
        ensure_transfer_kind_is_sendable(
            value_meta.value_kind(),
            "GoIsland direct receiver capture",
        )?;
        prepare_value_queue_handles_for_transfer_inner(
            &result.capture_data,
            value_meta,
            target_island,
            struct_metas,
            named_type_metas,
            runtime_types,
            state,
            notified_remote_endpoints,
            island_effects,
            commit,
            &mut visited_pointers,
            pass,
        )?;
    } else {
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
            let value_meta = checked_transfer_value_meta(
                *transfer_type,
                struct_metas,
                named_type_metas,
                runtime_types,
                &format!("GoIsland capture {i}"),
            )?;
            ensure_transfer_kind_is_sendable(
                value_meta.value_kind(),
                &format!("GoIsland capture {i}"),
            )?;
            let capture_slots = capture_value_slots_for_transfer(
                &state.gc,
                slot as GcRef,
                *transfer_type,
                struct_metas,
                named_type_metas,
                runtime_types,
                &format!("GoIsland capture {i} box"),
            )?;
            prepare_value_queue_handles_for_transfer_inner(
                &capture_slots.slots,
                value_meta,
                target_island,
                struct_metas,
                named_type_metas,
                runtime_types,
                state,
                notified_remote_endpoints,
                island_effects,
                commit,
                &mut visited_pointers,
                pass,
            )?;
        }
    }

    let mut arg_slot_idx = 0usize;
    for (i, transfer_type) in param_types.iter().enumerate() {
        let slots_usize = transfer_type.slots as usize;
        let arg_slot_end = arg_slot_idx
            .checked_add(slots_usize)
            .ok_or_else(|| format!("GoIsland arg {i} slot range overflows addressable memory"))?;
        if arg_slot_end > result.arg_data.len() {
            return Err(format!(
                "GoIsland arg {i} metadata requires slots {}..{} but only {} arg slots exist",
                arg_slot_idx,
                arg_slot_end,
                result.arg_data.len()
            ));
        }
        let value_meta = checked_transfer_value_meta(
            *transfer_type,
            struct_metas,
            named_type_metas,
            runtime_types,
            &format!("GoIsland arg {i}"),
        )?;
        ensure_transfer_kind_is_sendable(value_meta.value_kind(), &format!("GoIsland arg {i}"))?;
        prepare_value_queue_handles_for_transfer_inner(
            &result.arg_data[arg_slot_idx..arg_slot_end],
            value_meta,
            target_island,
            struct_metas,
            named_type_metas,
            runtime_types,
            state,
            notified_remote_endpoints,
            island_effects,
            commit,
            &mut visited_pointers,
            pass,
        )?;
        arg_slot_idx = arg_slot_end;
    }
    if arg_slot_idx != result.arg_data.len() {
        return Err(format!(
            "GoIsland transfer metadata covers {arg_slot_idx} arg slots but closure passed {}",
            result.arg_data.len()
        ));
    }
    Ok(())
}

/// Prepare any channels nested inside a value for cross-island transfer.
/// Walks the value graph based on metadata, finds Channel-typed slots, and installs
/// HomeInfo on LOCAL channels (or notifies home for REMOTE re-transfers).
/// Must be called BEFORE pack_slots when the value may contain nested channels.
pub fn prepare_value_queue_handles_for_transfer(
    slots: &[u64],
    value_meta: vo_runtime::ValueMeta,
    target_island: u32,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
    island_effects: &mut Vec<IslandCommandEffect>,
) -> QueueTransferResult {
    prepare_value_queue_handles_for_transfer_with_commit(
        slots,
        value_meta,
        target_island,
        struct_metas,
        named_type_metas,
        runtime_types,
        state,
        island_effects,
    )
    .map(|_| ())
}

pub fn prepare_value_queue_handles_for_transfer_with_commit(
    slots: &[u64],
    value_meta: vo_runtime::ValueMeta,
    target_island: u32,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
    island_effects: &mut Vec<IslandCommandEffect>,
) -> Result<QueueTransferCommit, String> {
    validate_value_queue_handles_for_transfer(
        slots,
        value_meta,
        target_island,
        struct_metas,
        named_type_metas,
        runtime_types,
        state,
    )?;

    let original_island_effects_len = island_effects.len();
    let mut notified_remote_endpoints = HashSet::new();
    let mut commit = QueueTransferCommit::default();
    let mut visited_pointers = HashSet::new();
    let result = prepare_value_queue_handles_for_transfer_inner(
        slots,
        value_meta,
        target_island,
        struct_metas,
        named_type_metas,
        runtime_types,
        state,
        &mut notified_remote_endpoints,
        island_effects,
        &mut commit,
        &mut visited_pointers,
        QueueTransferPass::Commit,
    );
    if let Err(error) = result {
        commit.restore_committed_local_endpoint_state(state);
        island_effects.truncate(original_island_effects_len);
        return Err(error);
    }
    Ok(commit)
}

pub fn validate_value_queue_handles_for_transfer(
    slots: &[u64],
    value_meta: vo_runtime::ValueMeta,
    target_island: u32,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
) -> QueueTransferResult {
    let mut validation_notified_remote_endpoints = HashSet::new();
    let mut validation_island_effects = Vec::new();
    let mut validation_commit = QueueTransferCommit::default();
    let mut visited_pointers = HashSet::new();
    prepare_value_queue_handles_for_transfer_inner(
        slots,
        value_meta,
        target_island,
        struct_metas,
        named_type_metas,
        runtime_types,
        state,
        &mut validation_notified_remote_endpoints,
        &mut validation_island_effects,
        &mut validation_commit,
        &mut visited_pointers,
        QueueTransferPass::Validate,
    )
}

fn validate_transfer_object_ref(
    gc: &vo_runtime::gc::Gc,
    obj_ref: vo_runtime::gc::GcRef,
    expected: vo_runtime::ValueKind,
    context: &str,
) -> Result<vo_runtime::gc::GcRef, String> {
    let Some(base) = gc.canonicalize_ref(obj_ref) else {
        return Err(format!("{context} {:p} is not a GC object", obj_ref));
    };
    if base != obj_ref {
        return Err(format!("{context} must be an object base"));
    }
    let actual = unsafe { vo_runtime::gc::Gc::header(base) }.kind();
    if actual != expected {
        return Err(format!(
            "{context} expected {:?} object, got {:?}",
            expected, actual
        ));
    }
    Ok(base)
}

fn validate_pointer_pointee_range(
    gc: &vo_runtime::gc::Gc,
    ptr_ref: vo_runtime::gc::GcRef,
    expected_slots: usize,
    context: &str,
) -> Result<vo_runtime::gc::GcRef, String> {
    let Some((_, offset_bytes, data_bytes)) = gc.ref_data_range(ptr_ref) else {
        return Err(format!("{context} {:p} is not a GC object", ptr_ref));
    };
    if offset_bytes % vo_runtime::slot::SLOT_BYTES != 0 {
        return Err(format!("{context} is not slot-aligned"));
    }
    let byte_width = expected_slots
        .checked_mul(vo_runtime::slot::SLOT_BYTES)
        .ok_or_else(|| format!("{context} pointee byte width overflow"))?;
    let end = offset_bytes
        .checked_add(byte_width)
        .ok_or_else(|| format!("{context} pointee range overflow"))?;
    if end > data_bytes {
        return Err(format!(
            "{context} pointee layout exceeds allocation: offset {offset_bytes}, width {byte_width}, allocation {data_bytes}"
        ));
    }
    Ok(ptr_ref)
}

fn validate_transfer_object_layout(
    gc: &vo_runtime::gc::Gc,
    obj_ref: vo_runtime::gc::GcRef,
    expected_meta: vo_runtime::ValueMeta,
    context: &str,
) -> Result<(vo_runtime::gc::GcRef, usize), String> {
    let obj_ref = validate_transfer_object_ref(gc, obj_ref, expected_meta.value_kind(), context)?;
    let header = unsafe { vo_runtime::gc::Gc::header(obj_ref) };
    if header.value_meta() != expected_meta {
        return Err(format!(
            "{context} {:?} layout metadata mismatch: expected {:?}, got {:?}",
            expected_meta.value_kind(),
            expected_meta,
            header.value_meta()
        ));
    }
    let Some(data_bytes) = gc.allocated_data_size_bytes(obj_ref) else {
        return Err(format!(
            "{context} {:?} layout is missing allocation size",
            expected_meta.value_kind()
        ));
    };
    if data_bytes % vo_runtime::slot::SLOT_BYTES != 0 {
        return Err(format!(
            "{context} {:?} layout data size {data_bytes} is not slot-aligned",
            expected_meta.value_kind()
        ));
    }
    let allocated_slots = data_bytes / vo_runtime::slot::SLOT_BYTES;
    if header.slots != 0 && header.slots as usize > allocated_slots {
        return Err(format!(
            "{context} {:?} layout header slots {} exceed allocation slots {allocated_slots}",
            expected_meta.value_kind(),
            header.slots
        ));
    }
    Ok((obj_ref, allocated_slots))
}

fn validate_fixed_transfer_object_layout(
    gc: &vo_runtime::gc::Gc,
    obj_ref: vo_runtime::gc::GcRef,
    expected_meta: vo_runtime::ValueMeta,
    expected_slots: usize,
    context: &str,
) -> Result<vo_runtime::gc::GcRef, String> {
    let (obj_ref, allocated_slots) =
        validate_transfer_object_layout(gc, obj_ref, expected_meta, context)?;
    let header_slots = unsafe { vo_runtime::gc::Gc::header(obj_ref) }.slots as usize;
    if header_slots != expected_slots || allocated_slots != expected_slots {
        return Err(format!(
            "{context} {:?} layout slot count mismatch: expected {expected_slots}, header {header_slots}, allocation {allocated_slots}",
            expected_meta.value_kind()
        ));
    }
    Ok(obj_ref)
}

#[derive(Clone, Copy)]
struct HeapArrayTransferLayout {
    len: usize,
    elem_meta: vo_runtime::ValueMeta,
    elem_bytes: usize,
    data_start: usize,
    data_end: usize,
}

fn validate_heap_array_transfer_layout(
    gc: &vo_runtime::gc::Gc,
    arr_ref: vo_runtime::gc::GcRef,
    context: &str,
) -> Result<HeapArrayTransferLayout, String> {
    let expected_meta = vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Array);
    let (arr_ref, allocated_slots) =
        validate_transfer_object_layout(gc, arr_ref, expected_meta, context)?;
    if allocated_slots < vo_runtime::objects::array::HEADER_SLOTS {
        return Err(format!(
            "{context} Array layout has {allocated_slots} allocation slots, expected at least {}",
            vo_runtime::objects::array::HEADER_SLOTS
        ));
    }

    // Safety: `validate_transfer_object_layout` established a live array object.
    let len = unsafe { vo_runtime::objects::array::len(arr_ref) };
    let elem_meta = unsafe { vo_runtime::objects::array::elem_meta(arr_ref) };
    let elem_bytes = unsafe { vo_runtime::objects::array::elem_bytes(arr_ref) };
    let data_bytes = len.checked_mul(elem_bytes).ok_or_else(|| {
        format!("{context} Array layout data byte count overflow during queue transfer")
    })?;
    let data_slots = vo_runtime::slot::slots_for_bytes(data_bytes);
    let expected_slots = vo_runtime::objects::array::HEADER_SLOTS
        .checked_add(data_slots)
        .ok_or_else(|| format!("{context} Array layout slot count overflow"))?;
    let header_slots = unsafe { vo_runtime::gc::Gc::header(arr_ref) }.slots as usize;
    if header_slots != 0 && header_slots != expected_slots {
        return Err(format!(
            "{context} Array layout header slots {header_slots} do not match payload slots {expected_slots}"
        ));
    }
    if allocated_slots != expected_slots {
        return Err(format!(
            "{context} Array layout allocation slots {allocated_slots} do not match payload slots {expected_slots}"
        ));
    }

    let data_start = unsafe { vo_runtime::objects::array::data_ptr_bytes(arr_ref) } as usize;
    let data_end = data_start
        .checked_add(data_bytes)
        .ok_or_else(|| format!("{context} Array layout data pointer overflow"))?;
    Ok(HeapArrayTransferLayout {
        len,
        elem_meta,
        elem_bytes,
        data_start,
        data_end,
    })
}

struct SliceTransferLayout {
    slice_ref: vo_runtime::gc::GcRef,
    len: usize,
    elem_meta: vo_runtime::ValueMeta,
    elem_bytes: usize,
}

fn validate_slice_transfer_layout(
    gc: &vo_runtime::gc::Gc,
    slice_ref: vo_runtime::gc::GcRef,
    context: &str,
) -> Result<SliceTransferLayout, String> {
    let expected_meta = vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Slice);
    let slice_ref = validate_fixed_transfer_object_layout(
        gc,
        slice_ref,
        expected_meta,
        vo_runtime::objects::slice::DATA_SLOTS as usize,
        context,
    )?;
    // Safety: fixed-layout validation established a live slice object.
    let owner = unsafe { vo_runtime::objects::slice::owner_ref(slice_ref) };
    let len = unsafe { vo_runtime::objects::slice::len(slice_ref) };
    let cap = unsafe { vo_runtime::objects::slice::cap(slice_ref) };
    let elem_meta = unsafe { vo_runtime::objects::slice::elem_meta(slice_ref) };
    let elem_bytes = unsafe { vo_runtime::objects::slice::elem_bytes(slice_ref) };
    let storage_stride = unsafe { vo_runtime::objects::slice::storage_stride(slice_ref) };
    let flat_storage = unsafe { vo_runtime::objects::slice::uses_flat_slot_storage(slice_ref) };
    let backing_len = unsafe { vo_runtime::objects::slice::backing_len(slice_ref) };
    let backing_ptr = unsafe { vo_runtime::objects::slice::backing_ptr(slice_ref) };
    let data_ptr = unsafe { vo_runtime::objects::slice::data_ptr(slice_ref) };
    if len > cap {
        return Err(format!(
            "{context} Slice layout len/cap mismatch: len {len}, cap {cap}"
        ));
    }
    if storage_stride != 0 {
        let backing_bytes = backing_len.checked_mul(storage_stride).ok_or_else(|| {
            format!("{context} Slice layout backing byte count overflow during queue transfer")
        })?;
        if backing_bytes != 0 && backing_ptr.is_null() {
            return Err(format!("{context} Slice layout has null backing pointer"));
        }
        let read_bytes = len.checked_mul(storage_stride).ok_or_else(|| {
            format!("{context} Slice layout read byte count overflow during queue transfer")
        })?;
        if read_bytes != 0 && data_ptr.is_null() {
            return Err(format!("{context} Slice layout has null data pointer"));
        }
        let data_addr = data_ptr as usize;
        let read_end = data_addr
            .checked_add(read_bytes)
            .ok_or_else(|| format!("{context} Slice layout data pointer overflow"))?;
        let backing_start = backing_ptr as usize;
        let backing_end = backing_start
            .checked_add(backing_bytes)
            .ok_or_else(|| format!("{context} Slice layout backing pointer overflow"))?;
        if data_addr < backing_start || read_end > backing_end {
            return Err(format!(
                "{context} Slice layout data range is outside its backing storage"
            ));
        }
        let elem_offset = data_addr - backing_start;
        if !elem_offset.is_multiple_of(storage_stride) {
            return Err(format!(
                "{context} Slice layout data pointer is not on an element boundary"
            ));
        }
        let start = elem_offset / storage_stride;
        if start > backing_len || cap > backing_len - start {
            return Err(format!(
                "{context} Slice layout range exceeds backing storage: start {start}, cap {cap}, backing len {backing_len}"
            ));
        }
        if !owner.is_null() {
            let owner_base = gc.canonicalize_ref(owner).ok_or_else(|| {
                format!("{context} Slice layout owner is not a live GC allocation")
            })?;
            let owner_bytes = gc.allocated_data_size_bytes(owner_base).ok_or_else(|| {
                format!("{context} Slice layout owner allocation size is unavailable")
            })?;
            let owner_start = owner_base as usize;
            let owner_end = owner_start
                .checked_add(owner_bytes)
                .ok_or_else(|| format!("{context} Slice layout owner range overflow"))?;
            if backing_start < owner_start || backing_end > owner_end {
                return Err(format!(
                    "{context} Slice backing range is outside its owning GC allocation"
                ));
            }
        }
    } else if len > cap || cap > backing_len {
        return Err(format!(
            "{context} zero-width Slice layout range exceeds backing length {backing_len}"
        ));
    }
    if flat_storage {
        if !storage_stride.is_multiple_of(vo_runtime::slot::SLOT_BYTES) {
            return Err(format!(
                "{context} flat Slice storage stride {storage_stride} is not slot-aligned"
            ));
        }
    } else if storage_stride != elem_bytes {
        return Err(format!(
            "{context} packed Slice stride {storage_stride} differs from element width {elem_bytes}"
        ));
    }
    Ok(SliceTransferLayout {
        slice_ref,
        len,
        elem_meta,
        elem_bytes,
    })
}

fn validate_string_transfer_layout(
    gc: &vo_runtime::gc::Gc,
    str_ref: vo_runtime::gc::GcRef,
    context: &str,
) -> QueueTransferResult {
    let expected_meta = vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::String);
    let str_ref = validate_fixed_transfer_object_layout(
        gc,
        str_ref,
        expected_meta,
        vo_runtime::objects::slice::DATA_SLOTS as usize,
        context,
    )?;
    // Safety: fixed-layout validation established a live string object.
    let arr_ref = unsafe { vo_runtime::objects::slice::owner_ref(str_ref) };
    if arr_ref.is_null() {
        return Err(format!(
            "{context} String layout is missing underlying Array"
        ));
    }
    let array_layout =
        validate_heap_array_transfer_layout(gc, arr_ref, "queue transfer String underlying Array")?;
    let byte_meta = vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Uint8);
    let descriptor_meta = unsafe { vo_runtime::objects::slice::elem_meta(str_ref) };
    let descriptor_bytes = unsafe { vo_runtime::objects::slice::elem_bytes(str_ref) };
    if array_layout.elem_meta != byte_meta
        || array_layout.elem_bytes != 1
        || descriptor_meta != byte_meta
        || descriptor_bytes != 1
    {
        return Err(format!(
            "{context} String layout expects Uint8 backing, got {:?} width {}",
            array_layout.elem_meta, array_layout.elem_bytes
        ));
    }
    let len = unsafe { vo_runtime::objects::slice::len(str_ref) };
    let cap = unsafe { vo_runtime::objects::slice::cap(str_ref) };
    if len > cap || cap > array_layout.len {
        return Err(format!(
            "{context} String layout len/cap mismatch: len {len}, cap {cap}, array len {}",
            array_layout.len
        ));
    }
    let data_ptr = unsafe { vo_runtime::objects::slice::data_ptr(str_ref) };
    if len != 0 && data_ptr.is_null() {
        return Err(format!("{context} String layout has null data pointer"));
    }
    let data_addr = data_ptr as usize;
    let read_end = data_addr
        .checked_add(len)
        .ok_or_else(|| format!("{context} String layout data pointer overflow"))?;
    if len != 0 && (data_addr < array_layout.data_start || read_end > array_layout.data_end) {
        return Err(format!(
            "{context} String layout data range is outside the underlying Array"
        ));
    }
    Ok(())
}

fn validate_map_transfer_layout(
    gc: &vo_runtime::gc::Gc,
    map_ref: vo_runtime::gc::GcRef,
    context: &str,
) -> Result<vo_runtime::gc::GcRef, String> {
    let expected_meta = vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Map);
    let map_ref = validate_fixed_transfer_object_layout(
        gc,
        map_ref,
        expected_meta,
        vo_runtime::objects::map::DATA_SLOTS as usize,
        context,
    )?;
    if unsafe { vo_runtime::objects::map::MapData::as_ref(map_ref) }.inner == 0 {
        return Err(format!("{context} Map layout is missing backing storage"));
    }
    Ok(map_ref)
}

fn validate_capture_box_ref(
    gc: &vo_runtime::gc::Gc,
    box_ref: vo_runtime::gc::GcRef,
    value_meta: vo_runtime::ValueMeta,
    expected_slots: usize,
    context: &str,
) -> Result<vo_runtime::gc::GcRef, String> {
    let expected_meta = vo_runtime::island_msg::capture_box_meta(value_meta);
    validate_fixed_transfer_object_layout(gc, box_ref, expected_meta, expected_slots, context)
}

fn read_object_slots(
    obj_ref: vo_runtime::gc::GcRef,
    slots: usize,
    context: &str,
) -> Result<Vec<Slot>, String> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(slots)
        .map_err(|_| format!("{context} cannot reserve {slots} capture slots"))?;
    for idx in 0..slots {
        values.push(unsafe { Gc::read_slot(obj_ref, idx) });
    }
    Ok(values)
}

fn read_value_slot_capture_box(
    gc: &vo_runtime::gc::Gc,
    box_ref: vo_runtime::gc::GcRef,
    value_meta: vo_runtime::ValueMeta,
    expected_slots: usize,
    context: &str,
) -> Result<Vec<Slot>, String> {
    let box_ref =
        validate_fixed_transfer_object_layout(gc, box_ref, value_meta, expected_slots, context)?;
    read_object_slots(box_ref, expected_slots, context)
}

fn read_heap_array_capture_slots(
    gc: &vo_runtime::gc::Gc,
    arr_ref: vo_runtime::gc::GcRef,
    value_meta: vo_runtime::ValueMeta,
    expected_slots: usize,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    context: &str,
) -> Result<Vec<Slot>, String> {
    let heap_layout = validate_heap_array_transfer_layout(gc, arr_ref, context)?;
    let array_layout = array_transfer_layout(
        value_meta,
        None,
        struct_metas,
        named_type_metas,
        runtime_types,
    )?
    .ok_or_else(|| format!("{context} missing Array runtime type layout"))?;
    if heap_layout.len != array_layout.len {
        return Err(format!(
            "{context} Array length mismatch: expected {}, got {}",
            array_layout.len, heap_layout.len
        ));
    }
    if heap_layout.elem_meta != array_layout.elem_meta {
        return Err(format!(
            "{context} Array element metadata mismatch: expected {:?}, got {:?}",
            array_layout.elem_meta, heap_layout.elem_meta
        ));
    }
    let elem_layout = sequence_elem_layout_for_transfer(
        heap_layout.elem_meta,
        heap_layout.elem_bytes,
        struct_metas,
        named_type_metas,
        runtime_types,
    )?;
    if elem_layout.logical_slots != array_layout.elem_slots {
        return Err(format!(
            "{context} Array element slot mismatch: expected {}, got {}",
            array_layout.elem_slots, elem_layout.logical_slots
        ));
    }
    let total_slots = array_layout
        .len
        .checked_mul(array_layout.elem_slots)
        .ok_or_else(|| format!("{context} Array capture slot count overflow"))?;
    if total_slots != expected_slots {
        return Err(format!(
            "{context} Array capture slot count mismatch: expected {expected_slots}, layout {total_slots}"
        ));
    }

    let mut slots = Vec::new();
    slots
        .try_reserve_exact(total_slots)
        .map_err(|_| format!("{context} cannot reserve {total_slots} Array capture slots"))?;
    slots.resize(total_slots, 0);
    unsafe { vo_runtime::objects::array::read_value_flat(arr_ref, &mut slots) }
        .map_err(|detail| format!("{context} cannot materialize Array capture: {detail}"))?;
    Ok(slots)
}

fn checked_transfer_value_meta(
    transfer_type: TransferType,
    _struct_metas: &[vo_common_core::bytecode::StructMeta],
    _named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    _runtime_types: &[vo_common_core::RuntimeType],
    context: &str,
) -> Result<vo_runtime::ValueMeta, String> {
    let value_meta = vo_runtime::ValueMeta::try_from_raw(transfer_type.meta_raw)
        .ok_or_else(|| format!("{context} has invalid value metadata"))?;
    let value_rttid = vo_runtime::ValueRttid::try_from_raw(transfer_type.rttid_raw)
        .ok_or_else(|| format!("{context} has invalid runtime type metadata"))?;
    if value_meta.try_value_kind() != value_rttid.try_value_kind() {
        return Err(format!("{context} value and runtime metadata kinds differ"));
    }
    Ok(value_meta)
}

struct CaptureTransferValue {
    slots: Vec<Slot>,
    storage: vo_runtime::island_msg::SpawnCaptureStorage,
}

fn capture_value_slots_for_transfer(
    gc: &vo_runtime::gc::Gc,
    box_ref: vo_runtime::gc::GcRef,
    transfer_type: TransferType,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    context: &str,
) -> Result<CaptureTransferValue, String> {
    let expected_slots = transfer_type.slots as usize;
    let value_meta = checked_transfer_value_meta(
        transfer_type,
        struct_metas,
        named_type_metas,
        runtime_types,
        context,
    )?;
    let Some(base) = gc.canonicalize_ref(box_ref) else {
        return Err(format!("{context} {:p} is not a GC object", box_ref));
    };
    if base != box_ref {
        return Err(format!("{context} must be an object base"));
    }

    let header = unsafe { vo_runtime::gc::Gc::header(base) };
    if header.is_value_slots_object() {
        return Ok(CaptureTransferValue {
            slots: read_value_slot_capture_box(gc, base, value_meta, expected_slots, context)?,
            storage: vo_runtime::island_msg::SpawnCaptureStorage::ValueSlots,
        });
    }
    if value_meta.value_kind() == vo_runtime::ValueKind::Array
        && header.kind() == vo_runtime::ValueKind::Array
    {
        return Ok(CaptureTransferValue {
            slots: read_heap_array_capture_slots(
                gc,
                base,
                value_meta,
                expected_slots,
                struct_metas,
                named_type_metas,
                runtime_types,
                context,
            )?,
            storage: vo_runtime::island_msg::SpawnCaptureStorage::HeapArray,
        });
    }
    if value_meta.value_kind() == vo_runtime::ValueKind::Array
        && header.kind() == vo_runtime::ValueKind::Struct
    {
        let array_rttid =
            vo_runtime::ValueRttid::new(value_meta.meta_id(), vo_runtime::ValueKind::Array);
        let expected_layout =
            RuntimeTypeResolver::new(struct_metas, named_type_metas, runtime_types)
                .slot_layout_for_value_rttid(array_rttid)
                .ok_or_else(|| {
                    format!(
                        "{context} missing, cyclic, or oversized Array runtime type slot layout"
                    )
                })?;
        if expected_layout.len() != expected_slots {
            return Err(format!(
                "{context} Array capture slot count mismatch: expected {expected_slots}, layout {}",
                expected_layout.len()
            ));
        }
        let struct_meta = header.value_meta();
        let meta_id = struct_meta.meta_id() as usize;
        let Some(meta) = struct_metas.get(meta_id) else {
            return Err(format!(
                "{context} synthetic Array capture box references missing StructMeta id {meta_id}"
            ));
        };
        if meta.slot_types != expected_layout {
            return Err(format!(
                "{context} synthetic Array capture box layout {:?} does not match Array slot layout {:?}",
                meta.slot_types, expected_layout
            ));
        }
        let box_ref =
            validate_fixed_transfer_object_layout(gc, base, struct_meta, expected_slots, context)?;
        return Ok(CaptureTransferValue {
            slots: read_object_slots(box_ref, expected_slots, context)?,
            storage: vo_runtime::island_msg::SpawnCaptureStorage::ValueSlots,
        });
    }

    let box_ref = validate_capture_box_ref(gc, base, value_meta, expected_slots, context)?;
    Ok(CaptureTransferValue {
        slots: read_object_slots(box_ref, expected_slots, context)?,
        storage: vo_runtime::island_msg::SpawnCaptureStorage::ValueSlots,
    })
}

#[derive(Clone, Copy, Debug)]
struct ArrayTransferLayout {
    len: usize,
    elem_meta: vo_runtime::ValueMeta,
    elem_slots: usize,
}

#[derive(Clone)]
struct QueueTransferSlots {
    data: Arc<[u64]>,
    start: usize,
    end: usize,
}

impl QueueTransferSlots {
    fn from_slice(slots: &[u64]) -> Self {
        Self::from_vec(slots.to_vec())
    }

    fn from_vec(slots: Vec<u64>) -> Self {
        let end = slots.len();
        Self {
            data: Arc::from(slots),
            start: 0,
            end,
        }
    }

    fn as_slice(&self) -> &[u64] {
        &self.data[self.start..self.end]
    }

    fn len(&self) -> usize {
        self.end - self.start
    }

    fn subspan(&self, offset: usize, len: usize) -> Option<Self> {
        let start = self.start.checked_add(offset)?;
        let end = start.checked_add(len)?;
        if end > self.end {
            return None;
        }
        Some(Self {
            data: Arc::clone(&self.data),
            start,
            end,
        })
    }
}

fn array_transfer_layout(
    value_meta: vo_runtime::ValueMeta,
    validated_total_slots: Option<usize>,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
) -> Result<Option<ArrayTransferLayout>, String> {
    if value_meta.value_kind() != vo_runtime::ValueKind::Array {
        return Ok(None);
    }
    let array_rttid =
        vo_runtime::ValueRttid::new(value_meta.meta_id(), vo_runtime::ValueKind::Array);
    let resolver = RuntimeTypeResolver::new(struct_metas, named_type_metas, runtime_types);
    let Some((
        _,
        vo_common_core::RuntimeType::Array {
            len,
            elem: elem_rttid,
        },
    )) = resolver.resolve_value_rttid(array_rttid)
    else {
        return Ok(None);
    };
    let total_slots = match validated_total_slots {
        Some(total_slots) => total_slots,
        None => resolver
            .slot_count_for_value_rttid(array_rttid)
            .ok_or_else(|| {
                format!(
                    "missing, cyclic, or oversized slot layout for array runtime type {} during queue transfer",
                    array_rttid.rttid()
                )
            })?,
    };
    let Some(elem_meta) = resolver.canonical_value_meta_for_value_rttid(*elem_rttid) else {
        return Err(format!(
            "missing element metadata for array runtime type {} during queue transfer",
            elem_rttid.rttid()
        ));
    };
    let len = usize::try_from(*len)
        .map_err(|_| "array length does not fit usize during queue transfer".to_string())?;
    let elem_slots = if len == 0 {
        if total_slots != 0 {
            return Err(format!(
                "zero-length array runtime type {} has {total_slots} resolved slots",
                array_rttid.rttid()
            ));
        }
        0
    } else {
        if !total_slots.is_multiple_of(len) {
            return Err(format!(
                "array runtime type {} slot layout {total_slots} is not divisible by length {len}",
                array_rttid.rttid()
            ));
        }
        total_slots / len
    };
    Ok(Some(ArrayTransferLayout {
        len,
        elem_meta,
        elem_slots,
    }))
}

fn prepare_value_queue_handles_for_transfer_inner(
    slots: &[u64],
    value_meta: vo_runtime::ValueMeta,
    target_island: u32,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
    notified_remote_endpoints: &mut HashSet<u64>,
    island_effects: &mut Vec<IslandCommandEffect>,
    commit: &mut QueueTransferCommit,
    visited_pointers: &mut HashSet<usize>,
    pass: QueueTransferPass,
) -> QueueTransferResult {
    use vo_runtime::gc::{Gc, GcRef};
    use vo_runtime::objects::map::MapIterator;
    use vo_runtime::{ValueKind, ValueMeta};

    enum QueueTransferWork {
        Value {
            meta: ValueMeta,
            slots: QueueTransferSlots,
            layout_validated: bool,
        },
        StructFields {
            meta_id: usize,
            slots: QueueTransferSlots,
            field_index: usize,
        },
        SliceElements {
            slice_ref: GcRef,
            index: usize,
            len: usize,
            elem_meta: ValueMeta,
            elem_slots: usize,
        },
        InlineArrayElements {
            slots: QueueTransferSlots,
            index: usize,
            len: usize,
            elem_meta: ValueMeta,
            elem_slots: usize,
        },
        HeapArrayElements {
            data_ptr: *mut u8,
            index: usize,
            len: usize,
            elem_bytes: usize,
            elem_meta: ValueMeta,
            elem_slots: usize,
        },
        MapEntries {
            iter: MapIterator,
            key_meta: ValueMeta,
            val_meta: ValueMeta,
            key_slots: usize,
            val_slots: usize,
        },
    }

    let type_resolver = RuntimeTypeResolver::new(struct_metas, named_type_metas, runtime_types);
    let mut pending = vec![QueueTransferWork::Value {
        meta: value_meta,
        slots: QueueTransferSlots::from_slice(slots),
        layout_validated: false,
    }];
    while let Some(work) = pending.pop() {
        let (value_meta, owned_slots, layout_validated) = match work {
            QueueTransferWork::Value {
                meta,
                slots,
                layout_validated,
            } => (meta, slots, layout_validated),
            QueueTransferWork::StructFields {
                meta_id,
                slots,
                field_index,
            } => {
                let Some(meta) = struct_metas.get(meta_id) else {
                    return Err(format!(
                        "missing StructMeta id {meta_id} during queue transfer"
                    ));
                };
                let Some(field) = meta.fields.get(field_index) else {
                    continue;
                };
                let fvk = field.type_info.value_kind();
                ensure_transfer_kind_is_sendable(
                    fvk,
                    &format!("field {} during queue transfer", field.name),
                )?;
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
                let field_meta = type_resolver
                    .canonical_value_meta_for_value_rttid(field.type_info)
                    .ok_or_else(|| {
                        format!(
                        "missing runtime type metadata for field {} rttid {} during queue transfer",
                        field.name,
                        field.type_info.rttid()
                    )
                    })?;
                let field_slots = slots.subspan(offset, fslots).ok_or_else(|| {
                    format!(
                        "field {} layout {}..{} exceeds value slots {} during queue transfer",
                        field.name,
                        offset,
                        offset + fslots,
                        slots.as_slice().len()
                    )
                })?;
                pending.push(QueueTransferWork::StructFields {
                    meta_id,
                    slots,
                    field_index: field_index + 1,
                });
                pending.push(QueueTransferWork::Value {
                    meta: field_meta,
                    slots: field_slots,
                    layout_validated: false,
                });
                continue;
            }
            QueueTransferWork::SliceElements {
                slice_ref,
                index,
                len,
                elem_meta,
                elem_slots,
            } => {
                if index >= len {
                    continue;
                }
                let mut elem_buf = vec![0u64; elem_slots];
                unsafe {
                    vo_runtime::objects::slice::read_logical_slots(slice_ref, index, &mut elem_buf);
                }
                pending.push(QueueTransferWork::SliceElements {
                    slice_ref,
                    index: index + 1,
                    len,
                    elem_meta,
                    elem_slots,
                });
                pending.push(QueueTransferWork::Value {
                    meta: elem_meta,
                    slots: QueueTransferSlots::from_vec(elem_buf),
                    layout_validated: true,
                });
                continue;
            }
            QueueTransferWork::InlineArrayElements {
                slots,
                index,
                len,
                elem_meta,
                elem_slots,
            } => {
                if index >= len {
                    continue;
                }
                let start = index
                    .checked_mul(elem_slots)
                    .ok_or_else(|| "inline Array element offset overflow".to_string())?;
                let elem = slots.subspan(start, elem_slots).ok_or_else(|| {
                    "inline Array element range exceeds validated value slots".to_string()
                })?;
                pending.push(QueueTransferWork::InlineArrayElements {
                    slots,
                    index: index + 1,
                    len,
                    elem_meta,
                    elem_slots,
                });
                pending.push(QueueTransferWork::Value {
                    meta: elem_meta,
                    slots: elem,
                    layout_validated: true,
                });
                continue;
            }
            QueueTransferWork::HeapArrayElements {
                data_ptr,
                index,
                len,
                elem_bytes,
                elem_meta,
                elem_slots,
            } => {
                if index >= len {
                    continue;
                }
                let mut elem_buf = vec![0u64; elem_slots];
                read_element_raw(data_ptr, index, elem_bytes, &mut elem_buf);
                pending.push(QueueTransferWork::HeapArrayElements {
                    data_ptr,
                    index: index + 1,
                    len,
                    elem_bytes,
                    elem_meta,
                    elem_slots,
                });
                pending.push(QueueTransferWork::Value {
                    meta: elem_meta,
                    slots: QueueTransferSlots::from_vec(elem_buf),
                    layout_validated: true,
                });
                continue;
            }
            QueueTransferWork::MapEntries {
                mut iter,
                key_meta,
                val_meta,
                key_slots,
                val_slots,
            } => {
                let mut key = vec![0; key_slots];
                let mut val = vec![0; val_slots];
                let has_entry = unsafe {
                    vo_runtime::objects::map::iter_next_into(&mut iter, &mut key, &mut val)
                }
                .map_err(|_| "queue transfer map iterator layout mismatch".to_string())?;
                if !has_entry {
                    continue;
                }
                pending.push(QueueTransferWork::MapEntries {
                    iter,
                    key_meta,
                    val_meta,
                    key_slots,
                    val_slots,
                });
                pending.push(QueueTransferWork::Value {
                    meta: val_meta,
                    slots: QueueTransferSlots::from_vec(val),
                    layout_validated: false,
                });
                pending.push(QueueTransferWork::Value {
                    meta: key_meta,
                    slots: QueueTransferSlots::from_vec(key),
                    layout_validated: false,
                });
                continue;
            }
        };
        let slots = owned_slots.as_slice();
        let vk = value_meta.value_kind();
        ensure_transfer_kind_is_sendable(vk, "queue transfer value")?;
        match vk {
            kind if kind.is_queue() => {
                if slots.len() != 1 {
                    return Err(format!(
                        "queue transfer value for {:?} requires exactly 1 slot, got {}",
                        value_meta.value_kind(),
                        slots.len()
                    ));
                }
                let chan_ref = slots[0] as GcRef;
                if chan_ref.is_null() {
                    continue;
                }
                let chan_ref = super::queue::validate_queue_handle(
                    &state.gc,
                    chan_ref,
                    "queue transfer value",
                )?;
                let expected_kind =
                    vo_runtime::objects::queue_state::QueueKind::from_value_kind(kind);
                let actual_kind = unsafe { vo_runtime::objects::queue_state::kind(chan_ref) };
                if actual_kind != expected_kind {
                    return Err(format!(
                        "queue transfer value kind mismatch: metadata says {:?}, handle is {:?}",
                        expected_kind, actual_kind
                    ));
                }
                preflight_queue_transfer_route(chan_ref, target_island, state)?;
                if pass.commits() {
                    prepare_single_queue_handle(
                        chan_ref,
                        target_island,
                        state,
                        notified_remote_endpoints,
                        island_effects,
                        commit,
                    )?;
                }
            }

            ValueKind::Struct => {
                let meta_id = value_meta.meta_id() as usize;
                let Some(meta) = struct_metas.get(meta_id) else {
                    return Err(format!(
                        "missing StructMeta id {meta_id} during queue transfer"
                    ));
                };
                if slots.len() != meta.slot_types.len() {
                    return Err(format!(
                        "struct value layout requires exactly {} slots, got {} during queue transfer",
                        meta.slot_types.len(),
                        slots.len()
                    ));
                }
                pending.push(QueueTransferWork::StructFields {
                    meta_id,
                    slots: owned_slots,
                    field_index: 0,
                });
            }

            ValueKind::Pointer => {
                if slots.len() != 1 {
                    return Err(format!(
                        "queue transfer pointer value requires exactly 1 slot, got {}",
                        slots.len()
                    ));
                }
                let ptr_ref = slots[0] as GcRef;
                if ptr_ref.is_null() {
                    continue;
                }
                let meta_id = value_meta.meta_id() as usize;
                let Some(meta) = struct_metas.get(meta_id) else {
                    return Err(format!(
                        "missing pointee StructMeta id {meta_id} during queue transfer"
                    ));
                };
                let obj_meta = vo_runtime::ValueMeta::new(value_meta.meta_id(), ValueKind::Struct);
                let obj_slots_count = meta.slot_types.len();
                let ptr_ref = validate_pointer_pointee_range(
                    &state.gc,
                    ptr_ref,
                    obj_slots_count,
                    "queue transfer Pointer value",
                )?;
                if !visited_pointers.insert(ptr_ref as usize) {
                    continue;
                }
                let mut obj_slots = vec![0u64; obj_slots_count];
                for (i, slot) in obj_slots.iter_mut().enumerate() {
                    *slot = unsafe { Gc::read_slot(ptr_ref, i) };
                }
                pending.push(QueueTransferWork::Value {
                    meta: obj_meta,
                    slots: QueueTransferSlots::from_vec(obj_slots),
                    layout_validated: false,
                });
            }

            ValueKind::String => {
                if slots.len() != 1 {
                    return Err(format!(
                        "queue transfer string value requires exactly 1 slot, got {}",
                        slots.len()
                    ));
                }
                let str_ref = slots[0] as GcRef;
                if str_ref.is_null() {
                    continue;
                }
                validate_string_transfer_layout(&state.gc, str_ref, "queue transfer string value")?;
            }

            ValueKind::Slice => {
                if slots.len() != 1 {
                    return Err(format!(
                        "queue transfer slice value requires exactly 1 slot, got {}",
                        slots.len()
                    ));
                }
                let slice_ref = slots[0] as GcRef;
                if slice_ref.is_null() {
                    continue;
                }
                let slice_layout = validate_slice_transfer_layout(
                    &state.gc,
                    slice_ref,
                    "queue transfer slice value",
                )?;
                if !visited_pointers.insert(slice_ref as usize) {
                    continue;
                }
                let elem_meta = slice_layout.elem_meta;
                ensure_transfer_kind_is_sendable(
                    elem_meta.value_kind(),
                    "slice element during queue transfer",
                )?;
                let length = slice_layout.len;
                let elem_bytes = slice_layout.elem_bytes;
                let elem_layout = sequence_elem_layout_for_transfer(
                    elem_meta,
                    elem_bytes,
                    struct_metas,
                    named_type_metas,
                    runtime_types,
                )?;
                if elem_bytes == 0 || !requires_transfer_preflight(elem_meta.value_kind()) {
                    continue;
                }
                pending.push(QueueTransferWork::SliceElements {
                    slice_ref: slice_layout.slice_ref,
                    index: 0,
                    len: length,
                    elem_meta,
                    elem_slots: elem_layout.logical_slots,
                });
            }

            ValueKind::Array => {
                if let Some(layout) = array_transfer_layout(
                    value_meta,
                    layout_validated.then_some(slots.len()),
                    struct_metas,
                    named_type_metas,
                    runtime_types,
                )? {
                    ensure_transfer_kind_is_sendable(
                        layout.elem_meta.value_kind(),
                        "array element during queue transfer",
                    )?;
                    let expected_slots =
                        layout.len.checked_mul(layout.elem_slots).ok_or_else(|| {
                            "array slot count overflow during queue transfer".to_string()
                        })?;
                    if slots.len() != expected_slots {
                        return Err(format!(
                            "array value layout requires exactly {expected_slots} slots, got {} during queue transfer",
                            slots.len()
                        ));
                    }
                    if layout.elem_slots == 0 {
                        continue;
                    }
                    if !requires_transfer_preflight(layout.elem_meta.value_kind()) {
                        continue;
                    }
                    pending.push(QueueTransferWork::InlineArrayElements {
                        slots: owned_slots,
                        index: 0,
                        len: layout.len,
                        elem_meta: layout.elem_meta,
                        elem_slots: layout.elem_slots,
                    });
                    continue;
                }
                if slots.len() != 1 {
                    return Err(format!(
                        "queue transfer heap array value requires exactly 1 slot, got {}",
                        slots.len()
                    ));
                }
                let arr_ref = slots[0] as GcRef;
                if arr_ref.is_null() {
                    continue;
                }
                let array_layout = validate_heap_array_transfer_layout(
                    &state.gc,
                    arr_ref,
                    "queue transfer array value",
                )?;
                if !visited_pointers.insert(arr_ref as usize) {
                    continue;
                }
                let elem_meta = array_layout.elem_meta;
                ensure_transfer_kind_is_sendable(
                    elem_meta.value_kind(),
                    "array element during queue transfer",
                )?;
                let length = array_layout.len;
                let elem_bytes = array_layout.elem_bytes;
                let elem_layout = sequence_elem_layout_for_transfer(
                    elem_meta,
                    elem_bytes,
                    struct_metas,
                    named_type_metas,
                    runtime_types,
                )?;
                if elem_bytes == 0 || !requires_transfer_preflight(elem_meta.value_kind()) {
                    continue;
                }
                let data_ptr = array_layout.data_start as *mut u8;
                pending.push(QueueTransferWork::HeapArrayElements {
                    data_ptr,
                    index: 0,
                    len: length,
                    elem_bytes,
                    elem_meta,
                    elem_slots: elem_layout.logical_slots,
                });
            }

            ValueKind::Map => {
                if slots.len() != 1 {
                    return Err(format!(
                        "queue transfer map value requires exactly 1 slot, got {}",
                        slots.len()
                    ));
                }
                let map_ref = slots[0] as GcRef;
                if map_ref.is_null() {
                    continue;
                }
                let map_ref =
                    validate_map_transfer_layout(&state.gc, map_ref, "queue transfer map value")?;
                if !visited_pointers.insert(map_ref as usize) {
                    continue;
                }
                let key_meta = unsafe { vo_runtime::objects::map::key_meta(map_ref) };
                let val_meta = unsafe { vo_runtime::objects::map::val_meta(map_ref) };
                ensure_transfer_kind_is_sendable(
                    key_meta.value_kind(),
                    "map key during queue transfer",
                )?;
                ensure_transfer_kind_is_sendable(
                    val_meta.value_kind(),
                    "map value during queue transfer",
                )?;
                let key_slots = unsafe { vo_runtime::objects::map::key_slots(map_ref) } as usize;
                let val_slots = unsafe { vo_runtime::objects::map::val_slots(map_ref) } as usize;
                pending.push(QueueTransferWork::MapEntries {
                    iter: unsafe { vo_runtime::objects::map::iter_init(map_ref) },
                    key_meta,
                    val_meta,
                    key_slots,
                    val_slots,
                });
            }

            // Scalars cannot contain queue handles, but preflight still owns their
            // slot-width contract so pack never discovers malformed layouts later.
            _ => {
                let expected_slots = if vk == ValueKind::Void {
                    0
                } else {
                    vo_runtime::slot::slots_for_bytes(vk.elem_bytes())
                };
                if slots.len() != expected_slots {
                    return Err(format!(
                        "queue transfer {:?} value requires exactly {expected_slots} slot(s), got {}",
                        vk,
                        slots.len()
                    ));
                }
            }
        }
    }
    Ok(())
}

pub fn prepare_remote_send_value_if_needed(
    ch: vo_runtime::gc::GcRef,
    slots: &[u64],
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
    state: &mut crate::vm::VmState,
    island_effects: &mut Vec<IslandCommandEffect>,
) -> Result<QueueTransferCommit, String> {
    if ch.is_null() {
        return Ok(QueueTransferCommit::default());
    }
    let ch = super::queue::validate_queue_handle(&state.gc, ch, "remote queue send")?;
    if !unsafe { vo_runtime::objects::queue::is_remote(ch) } {
        return Ok(QueueTransferCommit::default());
    }
    let elem_meta = unsafe { vo_runtime::objects::queue_state::elem_meta(ch) };
    ensure_transfer_kind_is_sendable(elem_meta.value_kind(), "remote queue element")?;
    let target_island = unsafe { vo_runtime::objects::queue::remote_proxy(ch) }.home_island;
    prepare_value_queue_handles_for_transfer_with_commit(
        slots,
        elem_meta,
        target_island,
        struct_metas,
        named_type_metas,
        runtime_types,
        state,
        island_effects,
    )
}

fn prepare_single_queue_handle(
    chan_ref: vo_runtime::gc::GcRef,
    target_island: u32,
    state: &mut crate::vm::VmState,
    notified_remote_endpoints: &mut HashSet<u64>,
    island_effects: &mut Vec<IslandCommandEffect>,
    commit: &mut QueueTransferCommit,
) -> Result<(), String> {
    use vo_runtime::objects::queue;
    use vo_runtime::objects::queue_state::QueueBacking;

    match unsafe { vo_runtime::objects::queue_state::backing(chan_ref) } {
        QueueBacking::Local => {
            commit.commit_local_endpoint_state(state, chan_ref);
            if unsafe { queue::home_info(chan_ref) }.is_none() {
                let eid = state
                    .allocate_endpoint_id()
                    .map_err(|error| error.to_string())?;
                unsafe { queue::install_home_info(chan_ref, eid, state.current_island_id) };
            }
            let endpoint_id = unsafe { queue::add_home_peer(chan_ref, target_island) }
                .map_err(|_| "local port transfer lost its HomeInfo".to_string())?;
            state.endpoint_registry.ensure_live(endpoint_id, chan_ref);
            state.mark_gc_all_roots_dirty();
        }
        QueueBacking::Remote => {
            let proxy = unsafe { queue::remote_proxy(chan_ref) };
            if notified_remote_endpoints.insert(proxy.endpoint_id) {
                island_effects.push(IslandCommandEffect::endpoint_transfer_request(
                    proxy.home_island,
                    proxy.endpoint_id,
                    target_island,
                    state.current_island_id,
                ));
            }
        }
    }
    Ok(())
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

fn requires_transfer_preflight(vk: vo_runtime::ValueKind) -> bool {
    vk == vo_runtime::ValueKind::String || may_contain_queue_handle(vk)
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

fn sequence_elem_layout_for_transfer(
    elem_meta: vo_runtime::ValueMeta,
    elem_bytes: usize,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
) -> Result<vo_runtime::pack::SequenceElementLayout, String> {
    vo_runtime::pack::sequence_element_layout(
        elem_meta,
        elem_bytes,
        struct_metas,
        named_type_metas,
        runtime_types,
    )
    .map_err(|err| {
        format!(
            "sequence {:?} element layout byte width mismatch during queue transfer: expected {expected_bytes}, got {elem_bytes}",
            elem_meta.value_kind(),
            expected_bytes = err.expected_bytes,
            elem_bytes = err.actual_bytes,
        )
    })
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
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_common_core::RuntimeType],
) -> Result<Vec<u8>, String> {
    if result.receiver_capture_slots > 0 {
        if capture_types.len() != 1
            || capture_types[0].slots != result.receiver_capture_slots
            || result.capture_data.len() != result.receiver_capture_slots as usize
        {
            return Err(format!(
                "GoIsland direct receiver capture metadata mismatch for func_id {}: raw slots {}, capture slots {}, transfer entries {}",
                result.func_id,
                result.receiver_capture_slots,
                result.capture_data.len(),
                capture_types.len()
            ));
        }
        return vo_runtime::island_msg::encode_spawn_payload_from_raw_capture_slots(
            gc,
            result.func_id,
            &result.capture_data,
            capture_types[0],
            &result.arg_data,
            param_types,
            struct_metas,
            named_type_metas,
            runtime_types,
        )
        .map_err(|error| format!("GoIsland spawn payload encode failed: {error}"));
    }

    if result.capture_data.len() != capture_types.len() {
        return Err(format!(
            "GoIsland capture metadata mismatch for func_id {}: capture slots {}, transfer entries {}",
            result.func_id,
            result.capture_data.len(),
            capture_types.len()
        ));
    }

    let mut capture_values = Vec::new();
    capture_values
        .try_reserve_exact(result.capture_data.len())
        .map_err(|_| {
            format!(
                "GoIsland capture plan allocation failed for {} entries",
                result.capture_data.len()
            )
        })?;
    for (i, &slot) in result.capture_data.iter().enumerate() {
        let Some(transfer_type) = capture_types.get(i) else {
            capture_values.push(None);
            continue;
        };
        if slot == 0 {
            capture_values.push(None);
            continue;
        }
        let capture_slots = capture_value_slots_for_transfer(
            gc,
            slot as vo_runtime::gc::GcRef,
            *transfer_type,
            struct_metas,
            named_type_metas,
            runtime_types,
            &format!("GoIsland capture {i} box"),
        )?;
        capture_values.push(Some(capture_slots));
    }
    let mut capture_descriptors = Vec::new();
    capture_descriptors
        .try_reserve_exact(capture_values.len())
        .map_err(|_| {
            format!(
                "GoIsland capture descriptor allocation failed for {} entries",
                capture_values.len()
            )
        })?;
    for capture in &capture_values {
        capture_descriptors.push(match capture {
            Some(capture) => vo_runtime::island_msg::SpawnCaptureValue {
                slots: Some(&capture.slots),
                storage: capture.storage,
            },
            None => vo_runtime::island_msg::SpawnCaptureValue::value_slots(None),
        });
    }
    vo_runtime::island_msg::encode_spawn_payload_from_capture_descriptors(
        gc,
        result.func_id,
        &capture_descriptors,
        capture_types,
        &result.arg_data,
        param_types,
        struct_metas,
        named_type_metas,
        runtime_types,
    )
    .map_err(|error| format!("GoIsland spawn payload encode failed: {error}"))
}

#[cfg(test)]
mod tests;
