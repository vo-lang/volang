//! VM-owned call-frame construction helpers.

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_common_core::runtime_type::RuntimeType;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::itab::ItabCache;
use vo_runtime::objects::closure;
use vo_runtime::{SlotType, ValueKind, ValueMeta, ValueRttid};

use crate::bytecode::{FunctionDef, JitInstructionMetadata, Module, TransferType};
use crate::exec::direct_method_receiver_transfer_plan;
use crate::fiber::{Fiber, TypedSlotPayload};
use crate::vm::helpers::{closure_call_layout, runtime_trap, stack_set, ClosureCallLayout};
use crate::vm::{ExecResult, RuntimeTrapKind};

pub(crate) struct FrameCallBuilder<'a> {
    gc: &'a mut Gc,
    fiber: &'a mut Fiber,
    module: &'a Module,
    itab_cache: Option<&'a ItabCache>,
}

pub(crate) struct ValidClosureTarget<'a> {
    pub(crate) func_id: u32,
    pub(crate) closure_gcref: GcRef,
    pub(crate) func: &'a FunctionDef,
    pub(crate) layout: ClosureCallLayout,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum CallFrameShapeError {
    ParamSlotsExceedLocals { param_slots: u16, local_slots: u16 },
    ScanSlotsExceedLocals { scan_slots: u16, local_slots: u16 },
}

impl CallFrameShapeError {
    pub(crate) fn message(self, context: &str) -> String {
        match self {
            CallFrameShapeError::ParamSlotsExceedLocals {
                param_slots,
                local_slots,
            } => format!("{context}: param_slots {param_slots} exceed local_slots {local_slots}"),
            CallFrameShapeError::ScanSlotsExceedLocals {
                scan_slots,
                local_slots,
            } => format!("{context}: gc_scan_slots {scan_slots} exceed local_slots {local_slots}"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct CallReturnWindowError {
    pub(crate) ret_reg: u16,
    pub(crate) ret_slots: u16,
    pub(crate) local_slots: u16,
}

impl CallReturnWindowError {
    pub(crate) fn message(self, context: &str) -> String {
        format!(
            "{context}: ret_reg {} + ret_slots {} exceed caller local_slots {}",
            self.ret_reg, self.ret_slots, self.local_slots
        )
    }
}

pub(crate) fn validate_call_frame_shape(func: &FunctionDef) -> Result<(), CallFrameShapeError> {
    if func.param_slots > func.local_slots {
        return Err(CallFrameShapeError::ParamSlotsExceedLocals {
            param_slots: func.param_slots,
            local_slots: func.local_slots,
        });
    }
    if func.gc_scan_slots > func.local_slots {
        return Err(CallFrameShapeError::ScanSlotsExceedLocals {
            scan_slots: func.gc_scan_slots,
            local_slots: func.local_slots,
        });
    }
    Ok(())
}

pub(crate) fn validate_call_return_window(
    caller_func: &FunctionDef,
    ret_reg: u16,
    ret_slots: u16,
) -> Result<(), CallReturnWindowError> {
    if ret_reg as usize + ret_slots as usize > caller_func.local_slots as usize {
        return Err(CallReturnWindowError {
            ret_reg,
            ret_slots,
            local_slots: caller_func.local_slots,
        });
    }
    Ok(())
}

impl ValidClosureTarget<'_> {
    #[inline]
    pub(crate) fn capture_count(&self) -> usize {
        // Safety: `ValidClosureTarget` construction validates and roots this closure.
        unsafe { closure::capture_count(self.closure_gcref) }
    }

    #[inline]
    pub(crate) fn capture(&self, index: usize) -> u64 {
        unsafe { closure::get_capture(self.closure_gcref, index) }
    }

    pub(crate) fn user_arg_slots(&self, context: &str) -> Result<usize, String> {
        (self.func.param_slots as usize)
            .checked_sub(self.layout.arg_offset)
            .ok_or_else(|| {
                format!(
                    "{context} layout arg_offset {} exceeds param_slots {} for func_id={} name={}",
                    self.layout.arg_offset, self.func.param_slots, self.func_id, self.func.name
                )
            })
    }
}

impl<'a> FrameCallBuilder<'a> {
    pub(crate) fn new(gc: &'a mut Gc, fiber: &'a mut Fiber, module: &'a Module) -> Self {
        Self {
            gc,
            fiber,
            module,
            itab_cache: None,
        }
    }

    pub(crate) fn new_with_itab_cache(
        gc: &'a mut Gc,
        fiber: &'a mut Fiber,
        module: &'a Module,
        itab_cache: &'a ItabCache,
    ) -> Self {
        Self {
            gc,
            fiber,
            module,
            itab_cache: Some(itab_cache),
        }
    }

    pub(crate) fn call_closure_borrowed(
        &mut self,
        closure_value: u64,
        arg_start: usize,
    ) -> ExecResult {
        let stack = self.fiber.stack_ptr();
        if closure_value == 0 {
            return runtime_trap(
                self.gc,
                self.fiber,
                stack,
                self.module,
                RuntimeTrapKind::NilFuncCall,
            );
        }

        let target = match self.validate_closure_target(closure_value, "CallClosure") {
            Ok(target) => target,
            Err(result) => return result,
        };
        if arg_start < target.layout.arg_offset {
            return ExecResult::JitError(format!(
                "CallClosure ABI underflow: arg_start={} arg_offset={} func_id={} name={}",
                arg_start, target.layout.arg_offset, target.func_id, target.func.name
            ));
        }

        let borrowed_start = (arg_start - target.layout.arg_offset) as u16;
        let caller_frame = match self.fiber.frames.last().copied() {
            Some(frame) => frame,
            None => {
                return ExecResult::JitError(
                    "CallClosure requested without an active caller frame".to_string(),
                );
            }
        };
        let Some(caller_func) = self.module.functions.get(caller_frame.func_id as usize) else {
            return ExecResult::JitError(format!(
                "CallClosure requested from missing caller function id {}",
                caller_frame.func_id
            ));
        };
        let callsite_pc = match caller_frame.pc.checked_sub(1) {
            Some(pc) => pc,
            None => {
                return ExecResult::JitError(
                    "CallClosure requested before caller pc advanced".to_string(),
                );
            }
        };
        let (callsite_arg_layout, callsite_ret_layout) =
            match call_layout_for_callsite(caller_func, callsite_pc, "CallClosure") {
                Ok(layout) => layout,
                Err(err) => return ExecResult::JitError(err),
            };
        let expected_user_arg_slots = match target.user_arg_slots("CallClosure") {
            Ok(slots) => slots,
            Err(err) => return ExecResult::JitError(err),
        };
        if let Err(result) = validate_dynamic_call_shape(
            "CallClosure",
            callsite_arg_layout.len(),
            callsite_ret_layout.len(),
            expected_user_arg_slots,
            target.func.ret_slots,
            target.func_id,
            &target.func.name,
        ) {
            return result;
        }
        if let Err(err) = validate_closure_callsite_layout(
            "CallClosure",
            &target,
            callsite_arg_layout,
            callsite_ret_layout,
        ) {
            return ExecResult::JitError(err);
        }
        let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(borrowed_start);
        let ret_reg = match checked_borrowed_return_reg(
            "CallClosure",
            borrowed_start,
            target.func.param_slots as usize,
            target.func_id,
            &target.func.name,
        ) {
            Ok(ret_reg) => ret_reg,
            Err(result) => return result,
        };
        if let Err(err) = validate_call_frame_shape(target.func) {
            return ExecResult::JitError(err.message("CallClosure callee frame shape"));
        }
        if let Err(err) = validate_call_return_window(caller_func, ret_reg, target.func.ret_slots) {
            return ExecResult::JitError(err.message("CallClosure caller return window"));
        }

        let new_bp = match self.fiber.try_push_borrowed_call_frame(
            target.func_id,
            borrowed_start,
            ret_reg,
            target.func.ret_slots,
            caller_scan_slots,
            target.func.local_slots,
            target.func.gc_scan_slots,
        ) {
            Ok(bp) => bp,
            Err(_) => {
                return runtime_trap(
                    self.gc,
                    self.fiber,
                    stack,
                    self.module,
                    RuntimeTrapKind::StackOverflow,
                );
            }
        };
        self.fiber.zero_slots_tail_at(
            new_bp,
            target.func.gc_scan_slots as usize,
            target.func.param_slots as usize,
        );
        for i in 0..target.layout.receiver_capture_count {
            let stack = self.fiber.stack_ptr();
            stack_set(stack, new_bp + i, unsafe {
                closure::get_capture(target.closure_gcref, i)
            });
        }
        if let Some(slot0_val) = target.layout.slot0 {
            let stack = self.fiber.stack_ptr();
            stack_set(stack, new_bp, slot0_val);
        }

        ExecResult::FrameChanged
    }

    pub(crate) fn call_extern_replay_closure(
        &mut self,
        closure_ref: GcRef,
        mut args: TypedSlotPayload,
    ) -> ExecResult {
        let stack = self.fiber.stack_ptr();
        if closure_ref.is_null() {
            return ExecResult::JitError(
                "CallExtern closure replay requested nil closure reference".to_string(),
            );
        }

        let target =
            match self.validate_closure_target(closure_ref as u64, "CallExtern closure replay") {
                Ok(target) => target,
                Err(result) => return result,
            };
        if let Err(err) = validate_call_frame_shape(target.func) {
            return ExecResult::JitError(err.message("CallExtern closure replay"));
        }
        let expected_arg_slots = match target.user_arg_slots("CallExtern closure replay") {
            Ok(slots) => slots,
            Err(err) => return ExecResult::JitError(err),
        };
        if args.values.len() != expected_arg_slots {
            return ExecResult::JitError(format!(
                "CallExtern closure replay arg slot count {} does not match expected {} for func_id={} name={}",
                args.values.len(), expected_arg_slots, target.func_id, target.func.name
            ));
        }
        let expected_arg_end = target.layout.arg_offset + args.values.len();
        let Some(expected_slot_types) = target
            .func
            .slot_types
            .get(target.layout.arg_offset..expected_arg_end)
        else {
            return ExecResult::JitError(format!(
                "CallExtern closure replay missing slot metadata for func_id={} name={} arg slot range {}..{} actual slot_types={}",
                target.func_id,
                target.func.name,
                target.layout.arg_offset,
                expected_arg_end,
                target.func.slot_types.len()
            ));
        };
        if args.slot_types.as_slice() != expected_slot_types {
            return ExecResult::JitError(format!(
                "CallExtern closure replay arg slot metadata mismatch for func_id={} name={}",
                target.func_id, target.func.name
            ));
        }
        if let Err(err) = validate_gc_visible_payload_values(
            self.gc,
            &mut args.values,
            &args.slot_types,
            "CallExtern closure replay arg",
            target.func_id,
            &target.func.name,
        ) {
            return ExecResult::JitError(err);
        }
        if let Err(err) = validate_extern_replay_transfer_args(
            self.gc,
            self.module,
            self.itab_cache,
            &mut args.values,
            &args.slot_types,
            &target,
        ) {
            return ExecResult::JitError(err);
        }
        let initialized_slots = target.layout.arg_offset + args.values.len();
        let local_slots = target.func.local_slots as usize;
        if initialized_slots > local_slots {
            return ExecResult::JitError(format!(
                "CallExtern closure replay initialized slots {} exceed local_slots {} for func_id={} name={}",
                initialized_slots, target.func.local_slots, target.func_id, target.func.name
            ));
        }

        let new_bp = self.fiber.sp;
        if self
            .fiber
            .try_reserve_call_window(new_bp, local_slots)
            .is_err()
        {
            self.fiber.closure_replay.finish_extern_terminal();
            return runtime_trap(
                self.gc,
                self.fiber,
                stack,
                self.module,
                RuntimeTrapKind::StackOverflow,
            );
        }

        self.fiber
            .zero_slots_tail_at(new_bp, target.func.gc_scan_slots as usize, 0);
        let fstack = self.fiber.stack_ptr();
        for i in 0..target.layout.receiver_capture_count {
            stack_set(fstack, new_bp + i, unsafe {
                closure::get_capture(target.closure_gcref, i)
            });
        }
        if let Some(slot0_val) = target.layout.slot0 {
            stack_set(fstack, new_bp, slot0_val);
        }
        let initialized_prefix = target
            .layout
            .receiver_capture_count
            .max(usize::from(target.layout.slot0.is_some()));
        for slot in initialized_prefix..target.layout.arg_offset {
            stack_set(fstack, new_bp + slot, 0);
        }
        self.fiber
            .copy_slots_from_slice(new_bp + target.layout.arg_offset, &args.values);

        if self
            .fiber
            .try_push_call_frame(
                target.func_id,
                new_bp,
                0,
                target.func.ret_slots,
                target.func.gc_scan_slots,
            )
            .is_err()
        {
            self.fiber.closure_replay.finish_extern_terminal();
            return runtime_trap(
                self.gc,
                self.fiber,
                stack,
                self.module,
                RuntimeTrapKind::StackOverflow,
            );
        }

        self.fiber
            .closure_replay
            .push_depth(self.fiber.frames.len());
        ExecResult::FrameChanged
    }

    fn validate_closure_target(
        &self,
        raw_ref: u64,
        context: &str,
    ) -> Result<ValidClosureTarget<'a>, ExecResult> {
        validate_closure_target(self.gc, self.module, raw_ref, context)
            .map_err(ExecResult::JitError)
    }
}

pub(crate) fn typed_extern_replay_args(
    gc: &Gc,
    module: &Module,
    itab_cache: &ItabCache,
    closure_ref: GcRef,
    mut args: Vec<u64>,
) -> Result<TypedSlotPayload, String> {
    if closure_ref.is_null() {
        return Err("CallExtern closure replay requested nil closure reference".to_string());
    }
    let target =
        validate_closure_target(gc, module, closure_ref as u64, "CallExtern closure replay")?;
    let expected_arg_slots = target.user_arg_slots("CallExtern closure replay")?;
    validate_function_arg_shape_with_expected(
        "CallExtern closure replay",
        target.func_id,
        target.func,
        args.len(),
        expected_arg_slots,
    )?;
    let end = target.layout.arg_offset + args.len();
    let slot_types = target
        .func
        .slot_types
        .get(target.layout.arg_offset..end)
        .ok_or_else(|| {
            format!(
                "CallExtern closure replay missing slot metadata for func_id={} name={} arg slot range {}..{} actual slot_types={}",
                target.func_id,
                target.func.name,
                target.layout.arg_offset,
                end,
                target.func.slot_types.len()
            )
        })?
        .to_vec();
    validate_gc_visible_payload_values(
        gc,
        &mut args,
        &slot_types,
        "CallExtern closure replay arg",
        target.func_id,
        &target.func.name,
    )?;
    validate_extern_replay_transfer_args(
        gc,
        module,
        Some(itab_cache),
        &mut args,
        &slot_types,
        &target,
    )?;
    TypedSlotPayload::try_new(args, slot_types)
}

fn validate_extern_replay_transfer_args(
    gc: &Gc,
    module: &Module,
    itab_cache: Option<&ItabCache>,
    values: &mut [u64],
    slot_types: &[SlotType],
    target: &ValidClosureTarget<'_>,
) -> Result<(), String> {
    let Some(plan) =
        extern_replay_user_arg_transfer_types(module, target, values.len(), slot_types)?
    else {
        return Ok(());
    };
    let mut slot_idx = plan.value_slot_offset;
    for transfer in &plan.transfers {
        let width = transfer.slots as usize;
        let end = slot_idx.checked_add(width).ok_or_else(|| {
            format!(
                "CallExtern closure replay arg transfer slot overflow for func_id={} name={}",
                target.func_id, target.func.name
            )
        })?;
        if end > values.len() {
            return Err(format!(
                "CallExtern closure replay arg transfer exceeds values for func_id={} name={}: transfer_end={} values={}",
                target.func_id,
                target.func.name,
                end,
                values.len()
            ));
        }
        let transfer_meta =
            validate_extern_replay_transfer_layout(module, slot_types, slot_idx, transfer, target)?;
        if transfer_meta.value_kind() == ValueKind::Interface {
            let Some(itab_cache) = itab_cache else {
                return Err(format!(
                    "CallExtern closure replay interface param requires itab cache for func_id={} name={} slot={}",
                    target.func_id, target.func.name, slot_idx
                ));
            };
            validate_extern_replay_interface_arg(
                gc, module, itab_cache, values, slot_idx, transfer, target,
            )?;
        } else {
            validate_extern_replay_concrete_arg(gc, values, slot_idx, transfer_meta, target)?;
        }
        slot_idx = end;
    }
    if slot_idx != plan.required_end_slot {
        return Err(format!(
            "CallExtern closure replay param_types consumed {} slots but transfer plan requires {} for func_id={} name={}",
            slot_idx,
            plan.required_end_slot,
            target.func_id,
            target.func.name
        ));
    }
    Ok(())
}

struct ExternReplayTransferPlan {
    value_slot_offset: usize,
    required_end_slot: usize,
    transfers: Vec<TransferType>,
}

fn extern_replay_user_arg_transfer_types(
    module: &Module,
    target: &ValidClosureTarget<'_>,
    arg_slots: usize,
    slot_types: &[SlotType],
) -> Result<Option<ExternReplayTransferPlan>, String> {
    let explicit_receiver_slots = explicit_receiver_arg_prefix_slots(target, arg_slots);
    if target.func.param_types.is_empty() {
        if explicit_receiver_slots != 0 {
            let metadata_slots = slot_types.get(explicit_receiver_slots..).ok_or_else(|| {
                format!(
                    "CallExtern closure replay explicit receiver prefix {} exceeds args {} for func_id={} name={}",
                    explicit_receiver_slots, arg_slots, target.func_id, target.func.name
                )
            })?;
            if extern_replay_slot_types_require_transfer_metadata(metadata_slots) {
                return Err(format!(
                    "CallExtern closure replay missing param_types for GC-visible args func_id={} name={}",
                    target.func_id, target.func.name
                ));
            }
            let receiver = direct_method_receiver_transfer_plan(
                module,
                target.func_id,
                target.func,
                target.func.recv_slots,
            )?;
            // Interface value-receiver wrappers receive a single boxed data
            // reference whose logical aggregate transfer spans multiple slots.
            // The raw GcRef has already passed payload validation; it has no
            // standalone canonical TransferType and must be skipped here.
            let (value_slot_offset, transfers) = if receiver.raw_capture_slots == 0 {
                (explicit_receiver_slots, Vec::new())
            } else {
                (0, vec![receiver.transfer_type])
            };
            return Ok(Some(ExternReplayTransferPlan {
                value_slot_offset,
                required_end_slot: explicit_receiver_slots,
                transfers,
            }));
        }
        let metadata_slots = slot_types
            .get(explicit_receiver_slots..)
            .ok_or_else(|| {
                format!(
                    "CallExtern closure replay explicit receiver prefix {} exceeds args {} for func_id={} name={}",
                    explicit_receiver_slots, arg_slots, target.func_id, target.func.name
                )
            })?;
        if extern_replay_slot_types_require_transfer_metadata(metadata_slots) {
            return Err(format!(
                "CallExtern closure replay missing param_types for GC-visible args func_id={} name={}",
                target.func_id, target.func.name
            ));
        }
        return Ok(None);
    }
    let total = target
        .func
        .param_types
        .iter()
        .try_fold(0usize, |acc, transfer| {
            acc.checked_add(transfer.slots as usize)
        })
        .ok_or_else(|| {
            format!(
                "CallExtern closure replay param_types slot count overflow for func_id={} name={}",
                target.func_id, target.func.name
            )
        })?;
    let (skip_slots, value_slot_offset, receiver_transfer) = if total == arg_slots {
        (0, 0, None)
    } else if total == arg_slots + target.layout.arg_offset {
        (target.layout.arg_offset, 0, None)
    } else if explicit_receiver_slots != 0 && total + explicit_receiver_slots == arg_slots {
        let receiver = direct_method_receiver_transfer_plan(
            module,
            target.func_id,
            target.func,
            target.func.recv_slots,
        )?;
        if receiver.raw_capture_slots == 0 {
            (0, explicit_receiver_slots, None)
        } else {
            (0, 0, Some(receiver.transfer_type))
        }
    } else {
        return Err(format!(
            "CallExtern closure replay param_types slots {} do not match args {}, receiver-inclusive args {}, or explicit receiver prefix {} for func_id={} name={}",
            total,
            arg_slots,
            arg_slots + target.layout.arg_offset,
            explicit_receiver_slots,
            target.func_id,
            target.func.name
        ));
    };

    let mut skipped = 0usize;
    let mut transfers = Vec::new();
    if let Some(receiver_transfer) = receiver_transfer {
        transfers.push(receiver_transfer);
    }
    for transfer in &target.func.param_types {
        let width = transfer.slots as usize;
        if skipped < skip_slots {
            let next = skipped + width;
            if next > skip_slots {
                return Err(format!(
                    "CallExtern closure replay param_types receiver boundary splits a transfer for func_id={} name={}",
                    target.func_id, target.func.name
                ));
            }
            skipped = next;
            continue;
        }
        transfers.push(*transfer);
    }
    Ok(Some(ExternReplayTransferPlan {
        value_slot_offset,
        required_end_slot: arg_slots,
        transfers,
    }))
}

fn explicit_receiver_arg_prefix_slots(target: &ValidClosureTarget<'_>, arg_slots: usize) -> usize {
    let recv_slots = target.func.recv_slots as usize;
    if target.layout.arg_offset == 0 && recv_slots != 0 && arg_slots >= recv_slots {
        recv_slots
    } else {
        0
    }
}

fn extern_replay_slot_types_require_transfer_metadata(slot_types: &[SlotType]) -> bool {
    slot_types.iter().any(|slot| {
        matches!(
            slot,
            SlotType::GcRef | SlotType::Interface0 | SlotType::Interface1
        )
    })
}

fn validate_extern_replay_transfer_layout(
    module: &Module,
    slot_types: &[SlotType],
    slot_idx: usize,
    transfer: &TransferType,
    target: &ValidClosureTarget<'_>,
) -> Result<ValueMeta, String> {
    let expected_meta = ValueMeta::from_raw(transfer.meta_raw);
    let expected_rttid = ValueRttid::from_raw(transfer.rttid_raw);
    if expected_meta.value_kind() != expected_rttid.value_kind() {
        return Err(format!(
            "CallExtern closure replay param metadata kind {:?} does not match RTTID kind {:?} for func_id={} name={} slot={}",
            expected_meta.value_kind(),
            expected_rttid.value_kind(),
            target.func_id,
            target.func.name,
            slot_idx
        ));
    }
    let Some(canonical_meta) = module.canonical_value_meta_for_value_rttid(expected_rttid) else {
        return Err(format!(
            "CallExtern closure replay param RTTID cannot be resolved for func_id={} name={} slot={}",
            target.func_id, target.func.name, slot_idx
        ));
    };
    if expected_meta != canonical_meta {
        return Err(format!(
            "CallExtern closure replay param metadata raw 0x{:x} does not match RTTID canonical raw 0x{:x} for func_id={} name={} slot={}",
            expected_meta.to_raw(),
            canonical_meta.to_raw(),
            target.func_id,
            target.func.name,
            slot_idx
        ));
    }
    let Some(expected_layout) = module.slot_layout_for_value_rttid(expected_rttid) else {
        return Err(format!(
            "CallExtern closure replay param RTTID cannot resolve slot layout for func_id={} name={} slot={}",
            target.func_id, target.func.name, slot_idx
        ));
    };
    if transfer.slots as usize != expected_layout.len() {
        return Err(format!(
            "CallExtern closure replay param transfer has {} slots but RTTID layout has {} for func_id={} name={} slot={}",
            transfer.slots,
            expected_layout.len(),
            target.func_id,
            target.func.name,
            slot_idx
        ));
    }
    let end = slot_idx + expected_layout.len();
    if slot_types.get(slot_idx..end) != Some(expected_layout.as_slice()) {
        return Err(format!(
            "CallExtern closure replay param slot layout mismatch for func_id={} name={} slot={}",
            target.func_id, target.func.name, slot_idx
        ));
    }
    Ok(expected_meta)
}

fn validate_extern_replay_concrete_arg(
    gc: &Gc,
    values: &mut [u64],
    slot_idx: usize,
    expected_meta: ValueMeta,
    target: &ValidClosureTarget<'_>,
) -> Result<(), String> {
    let value_kind = expected_meta.value_kind();
    let Some(expected_header_kind) = replay_heap_header_kind_for_value_kind(value_kind) else {
        return Ok(());
    };
    let raw = values[slot_idx];
    if raw == 0 {
        return Ok(());
    }
    let Some(canonical) = gc.canonicalize_ref(raw as GcRef) else {
        let (in_all, in_index, index_len) = gc.debug_ref_membership(raw as GcRef);
        return Err(format!(
            "CallExtern closure replay param invalid GcRef func_id={} name={} slot={} raw=0x{:016x} in_all_objects={} in_object_index={} object_index_len={}",
            target.func_id,
            target.func.name,
            slot_idx,
            raw,
            in_all,
            in_index,
            index_len
        ));
    };
    values[slot_idx] = canonical as u64;
    let header = unsafe { Gc::header(canonical) };
    if header.kind() != expected_header_kind {
        return Err(format!(
            "CallExtern closure replay param object kind {:?} does not match expected {:?} for value kind {:?} func_id={} name={} slot={}",
            header.kind(),
            expected_header_kind,
            value_kind,
            target.func_id,
            target.func.name,
            slot_idx
        ));
    }
    if value_kind == ValueKind::Pointer && header.meta_id() != expected_meta.meta_id() {
        return Err(format!(
            "CallExtern closure replay pointer param meta_id {} does not match expected {} for func_id={} name={} slot={}",
            header.meta_id(),
            expected_meta.meta_id(),
            target.func_id,
            target.func.name,
            slot_idx
        ));
    }
    Ok(())
}

fn replay_heap_header_kind_for_value_kind(value_kind: ValueKind) -> Option<ValueKind> {
    match value_kind {
        ValueKind::String
        | ValueKind::Slice
        | ValueKind::Map
        | ValueKind::Channel
        | ValueKind::Port
        | ValueKind::Closure
        | ValueKind::Island => Some(value_kind),
        ValueKind::Pointer => Some(ValueKind::Struct),
        _ => None,
    }
}

fn validate_extern_replay_interface_arg(
    gc: &Gc,
    module: &Module,
    itab_cache: &ItabCache,
    values: &mut [u64],
    slot_idx: usize,
    transfer: &TransferType,
    target: &ValidClosureTarget<'_>,
) -> Result<(), String> {
    use vo_runtime::objects::interface;

    if transfer.slots != 2 {
        return Err(format!(
            "CallExtern closure replay interface param has {} slots for func_id={} name={} slot={}",
            transfer.slots, target.func_id, target.func.name, slot_idx
        ));
    }
    let expected_meta = ValueMeta::from_raw(transfer.meta_raw);
    if expected_meta.value_kind() != ValueKind::Interface {
        return Err(format!(
            "CallExtern closure replay interface param metadata kind {:?} for func_id={} name={} slot={}",
            expected_meta.value_kind(),
            target.func_id,
            target.func.name,
            slot_idx
        ));
    }
    let expected_rttid = ValueRttid::from_raw(transfer.rttid_raw);
    let Some(canonical_meta) = module.canonical_value_meta_for_value_rttid(expected_rttid) else {
        return Err(format!(
            "CallExtern closure replay interface param RTTID cannot be resolved for func_id={} name={} slot={}",
            target.func_id, target.func.name, slot_idx
        ));
    };
    if canonical_meta != expected_meta {
        return Err(format!(
            "CallExtern closure replay interface param metadata raw 0x{:x} does not match RTTID canonical raw 0x{:x} for func_id={} name={} slot={}",
            expected_meta.to_raw(),
            canonical_meta.to_raw(),
            target.func_id,
            target.func.name,
            slot_idx
        ));
    }
    let expected_iface_meta_id = expected_meta.meta_id();
    let Some(expected_iface) = module.interface_metas.get(expected_iface_meta_id as usize) else {
        return Err(format!(
            "CallExtern closure replay interface param references missing interface {} for func_id={} name={} slot={}",
            expected_iface_meta_id, target.func_id, target.func.name, slot_idx
        ));
    };
    let slot0 = values[slot_idx];
    let slot1 = values[slot_idx + 1];
    let value_kind = interface::unpack_value_kind(slot0);
    if value_kind == ValueKind::Void {
        if slot1 != 0 {
            return Err(format!(
                "CallExtern closure replay nil interface arg has nonzero data for func_id={} name={} slot={}",
                target.func_id, target.func.name, slot_idx
            ));
        }
        return Ok(());
    }
    if value_kind == ValueKind::Interface {
        return Err(format!(
            "CallExtern closure replay raw interface-kind arg for func_id={} name={} slot={}",
            target.func_id, target.func.name, slot_idx
        ));
    }
    let rttid = interface::unpack_rttid(slot0);
    if module
        .canonical_value_meta_for_value_rttid(ValueRttid::new(rttid, value_kind))
        .is_none()
    {
        return Err(format!(
            "CallExtern closure replay interface arg has non-canonical RTTID/kind for func_id={} name={} slot={} rttid={} kind={:?}",
            target.func_id, target.func.name, slot_idx, rttid, value_kind
        ));
    }
    validate_extern_replay_interface_data_object(
        gc, module, values, slot_idx, slot0, slot1, rttid, value_kind, target,
    )?;
    let itab_id = interface::unpack_itab_id(slot0);
    if expected_iface.methods.is_empty() {
        return Ok(());
    }
    if itab_id == 0 {
        return Err(format!(
            "CallExtern closure replay interface arg missing itab for func_id={} name={} slot={} iface_meta_id={}",
            target.func_id, target.func.name, slot_idx, expected_iface_meta_id
        ));
    }
    let Some(named_type_id) = named_type_id_from_replay_interface_value(module, rttid, value_kind)
    else {
        return Err(format!(
            "CallExtern closure replay interface arg is not a named value for func_id={} name={} slot={} rttid={} kind={:?}",
            target.func_id, target.func.name, slot_idx, rttid, value_kind
        ));
    };
    let Some(expected_methods) = vo_runtime::itab::expected_interface_itab_methods(
        named_type_id,
        expected_iface_meta_id,
        value_kind == ValueKind::Pointer,
        &module.named_type_metas,
        &module.interface_metas,
    ) else {
        return Err(format!(
            "CallExtern closure replay interface arg does not implement expected interface for func_id={} name={} slot={} named_type_id={} iface_meta_id={}",
            target.func_id, target.func.name, slot_idx, named_type_id, expected_iface_meta_id
        ));
    };
    let Some(actual_itab) = itab_cache.get_itab(itab_id) else {
        return Err(format!(
            "CallExtern closure replay interface arg references missing itab {} for func_id={} name={} slot={}",
            itab_id, target.func_id, target.func.name, slot_idx
        ));
    };
    if actual_itab.iface_meta_id != expected_iface_meta_id
        || actual_itab.methods != expected_methods
    {
        return Err(format!(
            "CallExtern closure replay interface arg itab {} does not match expected interface for func_id={} name={} slot={} iface_meta_id={}",
            itab_id, target.func_id, target.func.name, slot_idx, expected_iface_meta_id
        ));
    }
    Ok(())
}

fn validate_extern_replay_interface_data_object(
    gc: &Gc,
    module: &Module,
    values: &mut [u64],
    slot_idx: usize,
    slot0: u64,
    slot1: u64,
    rttid: u32,
    value_kind: ValueKind,
    target: &ValidClosureTarget<'_>,
) -> Result<(), String> {
    use vo_runtime::objects::interface;

    if !interface::data_is_gc_ref(slot0) {
        return Ok(());
    }
    if slot1 == 0 {
        if matches!(value_kind, ValueKind::Struct | ValueKind::Array) {
            return Err(format!(
                "CallExtern closure replay interface arg data missing object for aggregate value kind {:?} func_id={} name={} slot={}",
                value_kind,
                target.func_id,
                target.func.name,
                slot_idx + 1
            ));
        }
        return Ok(());
    }
    let Some(canonical) = gc.canonicalize_ref(slot1 as GcRef) else {
        let (in_all, in_index, index_len) = gc.debug_ref_membership(slot1 as GcRef);
        return Err(format!(
            "CallExtern closure replay interface arg data invalid GcRef func_id={} name={} slot={} raw=0x{:016x} in_all_objects={} in_object_index={} object_index_len={}",
            target.func_id,
            target.func.name,
            slot_idx + 1,
            slot1,
            in_all,
            in_index,
            index_len
        ));
    };
    values[slot_idx + 1] = canonical as u64;
    let header = unsafe { Gc::header(canonical) };
    let Some(expected_meta) =
        module.canonical_value_meta_for_value_rttid(ValueRttid::new(rttid, value_kind))
    else {
        return Err(format!(
            "CallExtern closure replay interface arg data RTTID cannot be resolved for func_id={} name={} slot={} rttid={} kind={:?}",
            target.func_id, target.func.name, slot_idx + 1, rttid, value_kind
        ));
    };
    match value_kind {
        ValueKind::Struct | ValueKind::Pointer => {
            validate_extern_replay_interface_data_kind(
                header.kind(),
                ValueKind::Struct,
                value_kind,
                target,
                slot_idx,
            )?;
            if header.meta_id() != expected_meta.meta_id() {
                return Err(format!(
                    "CallExtern closure replay interface arg data meta_id {} does not match expected {} for func_id={} name={} slot={}",
                    header.meta_id(),
                    expected_meta.meta_id(),
                    target.func_id,
                    target.func.name,
                    slot_idx + 1
                ));
            }
            validate_extern_replay_struct_data_slots(
                module,
                header.meta_id(),
                header.slots as usize,
                target,
                slot_idx,
            )?;
        }
        ValueKind::Array => {
            validate_extern_replay_interface_array_data(
                module, canonical, header, rttid, target, slot_idx,
            )?;
        }
        _ => {
            if let Some(expected_kind) = interface_data_heap_kind_for_value_kind(value_kind) {
                validate_extern_replay_interface_data_kind(
                    header.kind(),
                    expected_kind,
                    value_kind,
                    target,
                    slot_idx,
                )?;
            }
        }
    }
    Ok(())
}

fn interface_data_heap_kind_for_value_kind(value_kind: ValueKind) -> Option<ValueKind> {
    match value_kind {
        ValueKind::String
        | ValueKind::Slice
        | ValueKind::Map
        | ValueKind::Channel
        | ValueKind::Port
        | ValueKind::Closure
        | ValueKind::Island => Some(value_kind),
        _ => None,
    }
}

fn validate_extern_replay_interface_data_kind(
    actual: ValueKind,
    expected: ValueKind,
    value_kind: ValueKind,
    target: &ValidClosureTarget<'_>,
    slot_idx: usize,
) -> Result<(), String> {
    if actual != expected {
        return Err(format!(
            "CallExtern closure replay interface arg data object kind {:?} does not match expected {:?} for value kind {:?} func_id={} name={} slot={}",
            actual,
            expected,
            value_kind,
            target.func_id,
            target.func.name,
            slot_idx + 1
        ));
    }
    Ok(())
}

fn validate_extern_replay_struct_data_slots(
    module: &Module,
    struct_meta_id: u32,
    actual_slots: usize,
    target: &ValidClosureTarget<'_>,
    slot_idx: usize,
) -> Result<(), String> {
    let Some(struct_meta) = module.struct_metas.get(struct_meta_id as usize) else {
        return Err(format!(
            "CallExtern closure replay interface arg data references missing StructMeta id {} for func_id={} name={} slot={}",
            struct_meta_id,
            target.func_id,
            target.func.name,
            slot_idx + 1
        ));
    };
    validate_extern_replay_data_slot_width(
        actual_slots,
        struct_meta.slot_types.len(),
        target,
        slot_idx,
    )
}

fn validate_extern_replay_data_slot_width(
    actual_slots: usize,
    expected_slots: usize,
    target: &ValidClosureTarget<'_>,
    slot_idx: usize,
) -> Result<(), String> {
    if actual_slots != expected_slots {
        return Err(format!(
            "CallExtern closure replay interface arg data allocation slots {} do not match expected {} for func_id={} name={} slot={}",
            actual_slots,
            expected_slots,
            target.func_id,
            target.func.name,
            slot_idx + 1
        ));
    }
    Ok(())
}

fn validate_extern_replay_interface_array_data(
    module: &Module,
    array_ref: GcRef,
    header: &vo_runtime::gc::GcHeader,
    rttid: u32,
    target: &ValidClosureTarget<'_>,
    slot_idx: usize,
) -> Result<(), String> {
    use vo_runtime::objects::array;

    let value_rttid = ValueRttid::new(rttid, ValueKind::Array);
    let Some((expected_len, expected_elem_rttid)) =
        interface_array_runtime_type(module, value_rttid)
    else {
        return Err(format!(
            "CallExtern closure replay interface arg array data layout missing for func_id={} name={} slot={} rttid={}",
            target.func_id,
            target.func.name,
            slot_idx + 1,
            rttid
        ));
    };
    let Some(expected_elem_meta) = module.canonical_value_meta_for_value_rttid(expected_elem_rttid)
    else {
        return Err(format!(
            "CallExtern closure replay interface arg array data element RTTID cannot be resolved for func_id={} name={} slot={} rttid={}",
            target.func_id,
            target.func.name,
            slot_idx + 1,
            expected_elem_rttid.rttid()
        ));
    };
    let Some(expected_elem_bytes) = sequence_element_physical_bytes(module, expected_elem_rttid)
    else {
        return Err(format!(
            "CallExtern closure replay interface arg array data element layout missing for func_id={} name={} slot={} rttid={}",
            target.func_id,
            target.func.name,
            slot_idx + 1,
            expected_elem_rttid.rttid()
        ));
    };
    match header.kind() {
        ValueKind::Array => {}
        ValueKind::Struct => {
            return validate_extern_replay_interface_array_value_slot_box(
                module, header, rttid, target, slot_idx,
            );
        }
        actual => {
            return Err(format!(
                "CallExtern closure replay interface arg data object kind {:?} does not match expected Array or Struct for value kind Array func_id={} name={} slot={}",
                actual,
                target.func_id,
                target.func.name,
                slot_idx + 1
            ));
        }
    }
    // Safety: the replay validator canonicalized the object and checked its array kind.
    let actual_len = unsafe { array::len(array_ref) };
    let actual_elem_meta = unsafe { array::elem_meta(array_ref) };
    let actual_elem_bytes = unsafe { array::elem_bytes(array_ref) };
    if actual_len != expected_len
        || actual_elem_meta != expected_elem_meta
        || actual_elem_bytes != expected_elem_bytes
    {
        return Err(format!(
            "CallExtern closure replay interface arg array data layout mismatch for func_id={} name={} slot={}: len {} expected {}, elem_meta 0x{:x} expected 0x{:x}, elem_bytes {} expected {}",
            target.func_id,
            target.func.name,
            slot_idx + 1,
            actual_len,
            expected_len,
            actual_elem_meta.to_raw(),
            expected_elem_meta.to_raw(),
            actual_elem_bytes,
            expected_elem_bytes
        ));
    }
    Ok(())
}

fn validate_extern_replay_interface_array_value_slot_box(
    module: &Module,
    header: &vo_runtime::gc::GcHeader,
    rttid: u32,
    target: &ValidClosureTarget<'_>,
    slot_idx: usize,
) -> Result<(), String> {
    let Some(expected_layout) =
        module.slot_layout_for_value_rttid(ValueRttid::new(rttid, ValueKind::Array))
    else {
        return Err(format!(
            "CallExtern closure replay interface arg array data layout missing for func_id={} name={} slot={} rttid={}",
            target.func_id,
            target.func.name,
            slot_idx + 1,
            rttid
        ));
    };
    let Some(struct_meta) = module.struct_metas.get(header.meta_id() as usize) else {
        return Err(format!(
            "CallExtern closure replay interface arg array data value-slot box references missing StructMeta id {} for func_id={} name={} slot={}",
            header.meta_id(),
            target.func_id,
            target.func.name,
            slot_idx + 1
        ));
    };
    if struct_meta.slot_types != expected_layout {
        return Err(format!(
            "CallExtern closure replay interface arg array data value-slot box layout {:?} does not match Array slot layout {:?} for func_id={} name={} slot={}",
            struct_meta.slot_types,
            expected_layout,
            target.func_id,
            target.func.name,
            slot_idx + 1
        ));
    }
    validate_extern_replay_data_slot_width(
        header.slots as usize,
        expected_layout.len(),
        target,
        slot_idx,
    )
}

fn interface_array_runtime_type(
    module: &Module,
    value_rttid: ValueRttid,
) -> Option<(usize, ValueRttid)> {
    let mut current = value_rttid;
    let limit = module.runtime_types.len() + module.named_type_metas.len() + 1;
    for _ in 0..limit {
        match module.runtime_types.get(current.rttid() as usize)? {
            RuntimeType::Array { len, elem } if current.value_kind() == ValueKind::Array => {
                return Some((*len as usize, *elem));
            }
            RuntimeType::Named { id, .. } => {
                let named = module.named_type_metas.get(*id as usize)?;
                if named.underlying_rttid.value_kind() != ValueKind::Array {
                    return None;
                }
                current = named.underlying_rttid;
            }
            _ => return None,
        }
    }
    None
}

fn sequence_element_physical_bytes(module: &Module, value_rttid: ValueRttid) -> Option<usize> {
    match value_rttid.value_kind() {
        ValueKind::Void => Some(0),
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => Some(1),
        ValueKind::Int16 | ValueKind::Uint16 => Some(2),
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => Some(4),
        _ => module
            .slot_layout_for_value_rttid(value_rttid)
            .and_then(|layout| layout.len().checked_mul(vo_runtime::slot::SLOT_BYTES)),
    }
}

fn named_type_id_from_replay_interface_value(
    module: &Module,
    rttid: u32,
    value_kind: ValueKind,
) -> Option<u32> {
    match module.runtime_types.get(rttid as usize)? {
        RuntimeType::Named { id, .. } => Some(*id),
        RuntimeType::Pointer(elem) if value_kind == ValueKind::Pointer => {
            match module.runtime_types.get(elem.rttid() as usize)? {
                RuntimeType::Named { id, .. } => Some(*id),
                _ => None,
            }
        }
        _ => None,
    }
}

pub(crate) fn validate_gc_visible_payload_values(
    gc: &Gc,
    values: &mut [u64],
    slot_types: &[SlotType],
    context: &'static str,
    func_id: u32,
    func_name: &str,
) -> Result<(), String> {
    if values.len() != slot_types.len() {
        return Err(format!(
            "{context} width mismatch for func_id={} name={}: values={} slot_types={}",
            func_id,
            func_name,
            values.len(),
            slot_types.len()
        ));
    }

    let mut slot_idx = 0usize;
    while slot_idx < slot_types.len() {
        match slot_types[slot_idx] {
            SlotType::GcRef => {
                let raw = values[slot_idx];
                if raw != 0 {
                    let Some(canonical) = gc.canonicalize_ref(raw as GcRef) else {
                        let (in_all, in_index, index_len) = gc.debug_ref_membership(raw as GcRef);
                        return Err(format!(
                            "{context} invalid GcRef func_id={} name={} slot={} raw=0x{:016x} in_all_objects={} in_object_index={} object_index_len={}",
                            func_id,
                            func_name,
                            slot_idx,
                            raw,
                            in_all,
                            in_index,
                            index_len
                        ));
                    };
                    values[slot_idx] = canonical as u64;
                }
                slot_idx += 1;
            }
            SlotType::Interface0 => {
                if slot_idx + 1 >= slot_types.len()
                    || slot_types[slot_idx + 1] != SlotType::Interface1
                {
                    return Err(format!(
                        "{context} interface metadata truncated for func_id={} name={} slot={}",
                        func_id, func_name, slot_idx
                    ));
                }
                let slot0 = values[slot_idx];
                let raw = values[slot_idx + 1];
                if vo_runtime::objects::interface::data_is_gc_ref(slot0) && raw != 0 {
                    let Some(canonical) = gc.canonicalize_ref(raw as GcRef) else {
                        let (in_all, in_index, index_len) = gc.debug_ref_membership(raw as GcRef);
                        return Err(format!(
                            "{context} invalid interface GcRef func_id={} name={} slot={} raw=0x{:016x} in_all_objects={} in_object_index={} object_index_len={}",
                            func_id,
                            func_name,
                            slot_idx + 1,
                            raw,
                            in_all,
                            in_index,
                            index_len
                        ));
                    };
                    values[slot_idx + 1] = canonical as u64;
                }
                slot_idx += 2;
            }
            _ => {
                slot_idx += 1;
            }
        }
    }
    Ok(())
}

pub(crate) fn validate_closure_target<'a>(
    gc: &Gc,
    module: &'a Module,
    raw_ref: u64,
    context: &str,
) -> Result<ValidClosureTarget<'a>, String> {
    let Some(canonical_ref) = gc.canonicalize_ref(raw_ref as GcRef) else {
        return Err(format!(
            "{context} requested invalid closure reference {:p}",
            raw_ref as GcRef
        ));
    };
    let kind = unsafe { Gc::header(canonical_ref) }.kind();
    if kind != ValueKind::Closure {
        return Err(format!(
            "{context} requested non-closure object kind {:?} at {:p}",
            kind, canonical_ref
        ));
    }
    let Some(data_bytes) = gc.allocated_data_size_bytes(canonical_ref) else {
        return Err(format!(
            "{context} closure layout is missing allocation size at {:p}",
            canonical_ref
        ));
    };
    if data_bytes % vo_runtime::slot::SLOT_BYTES != 0 {
        return Err(format!(
            "{context} closure layout data size {data_bytes} is not slot-aligned"
        ));
    }
    let allocated_slots = data_bytes / vo_runtime::slot::SLOT_BYTES;
    if allocated_slots < closure::HEADER_SLOTS {
        return Err(format!(
            "{context} closure layout has {allocated_slots} allocation slots, expected at least {}",
            closure::HEADER_SLOTS
        ));
    }
    // Safety: the object was canonicalized and checked as a closure above.
    let func_id = unsafe { closure::func_id(canonical_ref) };
    let capture_count = unsafe { closure::capture_count(canonical_ref) };
    let expected_slots = closure::HEADER_SLOTS
        .checked_add(capture_count)
        .ok_or_else(|| format!("{context} closure layout slot count overflow"))?;
    let header_slots = unsafe { Gc::header(canonical_ref) }.slots as usize;
    if header_slots != expected_slots || allocated_slots != expected_slots {
        return Err(format!(
            "{context} closure layout slot count mismatch for func_id={func_id}: expected {expected_slots}, header {header_slots}, allocation {allocated_slots}"
        ));
    }
    let Some(func) = module.functions.get(func_id as usize) else {
        return Err(format!("{context} missing function id {func_id}"));
    };
    let expected_capture_count = if func.recv_slots > 0 && capture_count > 0 {
        func.recv_slots as usize
    } else {
        func.capture_slot_types.len()
    };
    if capture_count != expected_capture_count {
        return Err(format!(
            "{context} closure capture count {capture_count} does not match expected {expected_capture_count} for func_id={func_id} name={}",
            func.name
        ));
    }
    // Safety: canonical_ref passed closure kind, allocation, and capture-count validation above.
    let layout = unsafe {
        closure_call_layout(
            canonical_ref as u64,
            canonical_ref,
            func.recv_slots as usize,
            func.is_closure,
        )
    }
    .map_err(|err| {
        format!(
            "{context} invalid closure layout for func_id={func_id} name={}: {}",
            func.name,
            err.message()
        )
    })?;
    Ok(ValidClosureTarget {
        func_id,
        closure_gcref: canonical_ref,
        func,
        layout,
    })
}

pub(crate) fn validate_island_handle(
    gc: &Gc,
    raw_ref: u64,
    context: &str,
) -> Result<GcRef, String> {
    let Some(canonical_ref) = gc.canonicalize_ref(raw_ref as GcRef) else {
        return Err(format!(
            "{context} requested invalid island reference {:p}",
            raw_ref as GcRef
        ));
    };
    let kind = unsafe { Gc::header(canonical_ref) }.kind();
    if kind != ValueKind::Island {
        return Err(format!(
            "{context} requested non-island object kind {:?} at {:p}",
            kind, canonical_ref
        ));
    }
    Ok(canonical_ref)
}

pub(crate) fn validate_dynamic_call_shape(
    opcode: &str,
    callsite_arg_slots: usize,
    callsite_ret_slots: usize,
    expected_user_arg_slots: usize,
    expected_ret_slots: u16,
    func_id: u32,
    func_name: &str,
) -> Result<(), ExecResult> {
    if callsite_arg_slots != expected_user_arg_slots {
        return Err(ExecResult::JitError(format!(
            "{opcode} arg slot count {} does not match target {} for func_id={} name={}",
            callsite_arg_slots, expected_user_arg_slots, func_id, func_name
        )));
    }
    if callsite_ret_slots != usize::from(expected_ret_slots) {
        return Err(ExecResult::JitError(format!(
            "{opcode} return slot count {} does not match target {} for func_id={} name={}",
            callsite_ret_slots, expected_ret_slots, func_id, func_name
        )));
    }
    Ok(())
}

pub(crate) fn call_layout_for_callsite<'a>(
    func: &'a FunctionDef,
    pc: usize,
    context: &str,
) -> Result<(&'a [SlotType], &'a [SlotType]), String> {
    let metadata = func.jit_metadata.get(pc).ok_or_else(|| {
        format!(
            "{context} missing CallLayout metadata for caller {} pc {}",
            func.name, pc
        )
    })?;
    match metadata {
        JitInstructionMetadata::CallLayout {
            arg_layout,
            ret_layout,
        } => Ok((arg_layout.as_slice(), ret_layout.as_slice())),
        other => Err(format!(
            "{context} expected CallLayout metadata for caller {} pc {}, got {other:?}",
            func.name, pc
        )),
    }
}

pub(crate) fn call_iface_layout_for_callsite<'a>(
    func: &'a FunctionDef,
    pc: usize,
    context: &str,
) -> Result<(u32, u32, &'a [SlotType], &'a [SlotType]), String> {
    let Some(metadata) = func.jit_metadata.get(pc) else {
        return Err(format!(
            "{context} missing CallIfaceLayout metadata for caller {} pc {}",
            func.name, pc
        ));
    };
    match metadata {
        JitInstructionMetadata::CallIfaceLayout {
            iface_meta_id,
            method_idx,
            arg_layout,
            ret_layout,
        } => Ok((
            *iface_meta_id,
            *method_idx,
            arg_layout.as_slice(),
            ret_layout.as_slice(),
        )),
        other => Err(format!(
            "{context} expected CallIfaceLayout metadata for caller {} pc {}, got {other:?}",
            func.name, pc
        )),
    }
}

fn validate_slot_layout(
    context: &str,
    func_id: u32,
    func_name: &str,
    role: &str,
    callsite_layout: &[SlotType],
    target_layout: &[SlotType],
) -> Result<(), String> {
    if callsite_layout != target_layout {
        return Err(format!(
            "{context} {role} slot metadata mismatch for func_id={func_id} name={func_name}: callsite={callsite_layout:?} target={target_layout:?}"
        ));
    }
    Ok(())
}

pub(crate) fn validate_function_callsite_layout(
    context: &str,
    func_id: u32,
    func: &FunctionDef,
    target_arg_start: usize,
    target_arg_slots: usize,
    callsite_arg_layout: &[SlotType],
    callsite_ret_layout: &[SlotType],
) -> Result<(), String> {
    validate_function_callsite_arg_layout(
        context,
        func_id,
        func,
        target_arg_start,
        target_arg_slots,
        callsite_arg_layout,
    )?;
    validate_slot_layout(
        context,
        func_id,
        &func.name,
        "return",
        callsite_ret_layout,
        &func.ret_slot_types,
    )
}

pub(crate) fn validate_function_callsite_arg_layout(
    context: &str,
    func_id: u32,
    func: &FunctionDef,
    target_arg_start: usize,
    target_arg_slots: usize,
    callsite_arg_layout: &[SlotType],
) -> Result<(), String> {
    let arg_end = target_arg_start
        .checked_add(target_arg_slots)
        .ok_or_else(|| format!("{context} target arg slot range overflow"))?;
    let target_arg_layout = func
        .slot_types
        .get(target_arg_start..arg_end)
        .ok_or_else(|| {
            format!(
                "{context} missing target arg slot metadata for func_id={} name={} range {}..{} actual slot_types={}",
                func_id,
                func.name,
                target_arg_start,
                arg_end,
                func.slot_types.len()
            )
        })?;
    validate_slot_layout(
        context,
        func_id,
        &func.name,
        "arg",
        callsite_arg_layout,
        target_arg_layout,
    )
}

pub(crate) fn validate_closure_callsite_layout(
    context: &str,
    target: &ValidClosureTarget<'_>,
    callsite_arg_layout: &[SlotType],
    callsite_ret_layout: &[SlotType],
) -> Result<(), String> {
    let expected_arg_slots = target.user_arg_slots(context)?;
    validate_function_callsite_layout(
        context,
        target.func_id,
        target.func,
        target.layout.arg_offset,
        expected_arg_slots,
        callsite_arg_layout,
        callsite_ret_layout,
    )
}

pub(crate) fn validate_closure_callsite_arg_layout(
    context: &str,
    target: &ValidClosureTarget<'_>,
    callsite_arg_layout: &[SlotType],
) -> Result<(), String> {
    let expected_arg_slots = target.user_arg_slots(context)?;
    validate_function_callsite_arg_layout(
        context,
        target.func_id,
        target.func,
        target.layout.arg_offset,
        expected_arg_slots,
        callsite_arg_layout,
    )
}

pub(crate) fn validate_closure_arg_shape(
    context: &str,
    target: &ValidClosureTarget<'_>,
    supplied_arg_slots: usize,
) -> Result<(), String> {
    let expected_arg_slots = target.user_arg_slots(context)?;
    validate_function_arg_shape_with_expected(
        context,
        target.func_id,
        target.func,
        supplied_arg_slots,
        expected_arg_slots,
    )
}

pub(crate) fn validate_function_arg_shape(
    context: &str,
    func_id: u32,
    func: &FunctionDef,
    supplied_arg_slots: usize,
) -> Result<(), String> {
    validate_function_arg_shape_with_expected(
        context,
        func_id,
        func,
        supplied_arg_slots,
        func.param_slots as usize,
    )
}

fn validate_function_arg_shape_with_expected(
    context: &str,
    func_id: u32,
    func: &FunctionDef,
    supplied_arg_slots: usize,
    expected_arg_slots: usize,
) -> Result<(), String> {
    if supplied_arg_slots != expected_arg_slots {
        return Err(format!(
            "{context} arg slot count {} does not match target {} for func_id={} name={}",
            supplied_arg_slots, expected_arg_slots, func_id, func.name
        ));
    }
    if validate_call_frame_shape(func).is_err() {
        return Err(format!(
            "{context} invalid target frame shape for func_id={} name={}: param_slots={} gc_scan_slots={} local_slots={}",
            func_id,
            func.name,
            func.param_slots,
            func.gc_scan_slots,
            func.local_slots
        ));
    }
    Ok(())
}

fn checked_borrowed_return_reg(
    opcode: &str,
    borrowed_start: u16,
    arg_slots: usize,
    func_id: u32,
    func_name: &str,
) -> Result<u16, ExecResult> {
    let arg_slots = u16::try_from(arg_slots).map_err(|_| {
        ExecResult::JitError(format!(
            "{opcode} arg slot count {arg_slots} exceeds u16 for func_id={func_id} name={func_name}"
        ))
    })?;
    borrowed_start.checked_add(arg_slots).ok_or_else(|| {
        ExecResult::JitError(format!(
            "{opcode} return offset overflow: borrowed_start={borrowed_start} arg_slots={arg_slots} func_id={func_id} name={func_name}"
        ))
    })
}

#[cfg(test)]
mod tests;
