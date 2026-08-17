//! VM-owned call-frame construction helpers.

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(test)]
use vo_common_core::runtime_type::RuntimeType;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::itab::{validate_interface_itab, ItabCache};
use vo_runtime::objects::closure;
use vo_runtime::value_layout::{
    canonicalize_concrete_heap_value, validate_interface_value, validate_transfer_layout,
    ValidatedInterfaceValue,
};
#[cfg(test)]
use vo_runtime::ValueRttid;
use vo_runtime::{SlotType, ValueKind, ValueMeta};

use crate::bytecode::{FunctionDef, InstructionMetadata, Module, TransferType};
use crate::exec::direct_method_receiver_transfer_plan;
use crate::fiber::{Fiber, TypedSlotPayload};
use crate::instruction::Instruction;
use crate::vm::helpers::{closure_call_layout, runtime_trap, stack_set, ClosureCallLayout};
use crate::vm::{ExecResult, RuntimeTrapKind};

pub(crate) struct FrameCallBuilder<'a> {
    gc: &'a mut Gc,
    fiber: &'a mut Fiber,
    module: &'a Module,
    frame_root_maps: Option<&'a vo_common_core::FrameRootMaps>,
    itab_cache: Option<&'a ItabCache>,
}

#[derive(Clone, Copy)]
enum ClosureReferenceProof {
    Unverified,
    ExactBase,
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
}

impl CallFrameShapeError {
    pub(crate) fn message(self, context: &str) -> String {
        match self {
            CallFrameShapeError::ParamSlotsExceedLocals {
                param_slots,
                local_slots,
            } => format!("{context}: param_slots {param_slots} exceed local_slots {local_slots}"),
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
            frame_root_maps: None,
            itab_cache: None,
        }
    }

    pub(crate) fn new_loaded(
        gc: &'a mut Gc,
        fiber: &'a mut Fiber,
        loaded_module: &'a vo_runtime::bytecode::LoadedModule,
    ) -> Self {
        Self {
            gc,
            fiber,
            module: loaded_module.module(),
            frame_root_maps: Some(loaded_module.frame_root_maps()),
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
            frame_root_maps: None,
            itab_cache: Some(itab_cache),
        }
    }

    #[inline]
    fn initialize_frame_root_locals(&mut self, bp: usize, target: &ValidClosureTarget<'_>) {
        if let Some(root_maps) = self.frame_root_maps {
            let roots = root_maps
                .function(target.func_id)
                .expect("verified closure target owns frame-root facts")
                .initialization_roots_to_clear();
            if let Some(roots) = roots {
                self.fiber.zero_frame_root_locals_at(bp, roots);
            }
        } else {
            self.fiber.zero_function_root_locals_at(bp, target.func);
        }
    }

    pub(crate) fn call_closure_borrowed(
        &mut self,
        closure_value: u64,
        arg_start: usize,
    ) -> ExecResult {
        self.call_closure_borrowed_impl(
            closure_value,
            arg_start,
            None,
            ClosureReferenceProof::Unverified,
        )
    }

    /// Execute a closure call with the shared dynamic-call proof cache.
    /// Object identity and closure layout are validated before the cache is
    /// consulted, so the cache only elides immutable module-target proofs.
    pub(crate) fn call_closure_borrowed_cached(
        &mut self,
        closure_value: u64,
        arg_start: usize,
        ic_entry: &mut vo_runtime::DynCallIC,
    ) -> ExecResult {
        self.call_closure_borrowed_impl(
            closure_value,
            arg_start,
            Some(ic_entry),
            ClosureReferenceProof::Unverified,
        )
    }

    /// Execute a closure call whose callee slot is `GcBase` in a loaded,
    /// verified module.  The slot proof makes the allocation header directly
    /// accessible while all module-specific target checks remain unchanged.
    pub(crate) fn call_verified_closure_borrowed_cached(
        &mut self,
        closure_value: u64,
        arg_start: usize,
        ic_entry: &mut vo_runtime::DynCallIC,
    ) -> ExecResult {
        self.call_closure_borrowed_impl(
            closure_value,
            arg_start,
            Some(ic_entry),
            ClosureReferenceProof::ExactBase,
        )
    }

    fn call_closure_borrowed_impl(
        &mut self,
        closure_value: u64,
        arg_start: usize,
        mut ic_entry: Option<&mut vo_runtime::DynCallIC>,
        reference_proof: ClosureReferenceProof,
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

        let validation = match reference_proof {
            ClosureReferenceProof::Unverified => {
                closure::validate_object(self.gc, closure_value as GcRef)
            }
            // Safety: the only caller of this mode consumes a loaded module;
            // its verifier requires CallClosure's callee slot to be GcBase.
            ClosureReferenceProof::ExactBase => unsafe {
                closure::validate_exact_base(closure_value as GcRef)
            },
        };
        let closure_object = match validation {
            Ok(object) => object,
            Err(error) => {
                return ExecResult::JitError(format!(
                    "CallClosure requested invalid closure object at {:p}: {error}",
                    closure_value as GcRef
                ));
            }
        };
        let dispatch_key = closure_object.dispatch_key();
        let cached = ic_entry
            .as_deref()
            .and_then(|entry| entry.probe(dispatch_key))
            .filter(|target| target.func_id == closure_object.func_id);
        let (target, proof_cache_hit, fill_ic_after_validation) = match cached {
            Some(cached) => {
                let target =
                    match trusted_cached_closure_target(self.module, closure_object, cached) {
                        Ok(target) => target,
                        Err(err) => return ExecResult::JitError(err),
                    };
                (target, true, false)
            }
            None => {
                let target = match validate_closure_target_object(
                    self.module,
                    closure_object,
                    "CallClosure",
                ) {
                    Ok(target) => target,
                    Err(error) => return ExecResult::JitError(error),
                };
                (target, false, ic_entry.is_some())
            }
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
        if !proof_cache_hit {
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
        }
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
        if !proof_cache_hit {
            if let Err(err) = validate_call_frame_shape(target.func) {
                return ExecResult::JitError(err.message("CallClosure callee frame shape"));
            }
            if let Err(err) =
                validate_call_return_window(caller_func, ret_reg, target.func.ret_slots)
            {
                return ExecResult::JitError(err.message("CallClosure caller return window"));
            }
            if fill_ic_after_validation {
                let entry = ic_entry
                    .as_deref_mut()
                    .expect("closure cache miss must retain its destination entry");
                entry.publish_interpreter_target(
                    dispatch_key,
                    vo_runtime::DynamicCallTarget {
                        func_id: target.func_id,
                        local_slots: target.func.local_slots,
                    },
                );
            }
        }

        let new_bp = match self.fiber.try_push_borrowed_call_frame(
            target.func_id,
            borrowed_start,
            ret_reg,
            target.func.ret_slots,
            target.func.local_slots,
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
        self.initialize_frame_root_locals(new_bp, &target);
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

    pub(crate) fn call_extern_replay_closure_at(
        &mut self,
        closure_ref: GcRef,
        mut args: TypedSlotPayload,
        replay_pc: usize,
    ) -> ExecResult {
        let Some(suspended_pc) = replay_pc.checked_add(1) else {
            return ExecResult::JitError(
                "CallExtern closure replay pc exceeds the host address domain".to_string(),
            );
        };
        if self.fiber.frames.is_empty() {
            return ExecResult::JitError(
                "CallExtern closure replay requested without a caller frame".to_string(),
            );
        }
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
        let reservation = match self.fiber.try_reserve_call_window(new_bp, local_slots) {
            Ok(reservation) => reservation,
            Err(_) => {
                self.fiber.closure_replay.finish_extern_terminal();
                return runtime_trap(
                    self.gc,
                    self.fiber,
                    stack,
                    self.module,
                    RuntimeTrapKind::StackOverflow,
                );
            }
        };

        self.initialize_frame_root_locals(new_bp, &target);

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

        self.fiber.commit_reserved_call_frame(
            reservation,
            target.func_id,
            new_bp,
            0,
            target.func.ret_slots,
        );

        let parent_index = self.fiber.frames.len() - 2;
        self.fiber.frames[parent_index].pc = suspended_pc;
        self.fiber
            .closure_replay
            .push_boundary(self.fiber.frames.len(), replay_pc);
        ExecResult::FrameChanged
    }

    #[cfg(test)]
    pub(crate) fn call_extern_replay_closure(
        &mut self,
        closure_ref: GcRef,
        args: TypedSlotPayload,
    ) -> ExecResult {
        let replay_pc = self
            .fiber
            .current_frame()
            .map_or(0, |frame| frame.pc.saturating_sub(1));
        self.call_extern_replay_closure_at(closure_ref, args, replay_pc)
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
        let transfer_meta = validate_transfer_layout(module, slot_types, slot_idx, transfer)
            .map_err(|err| {
                format!(
                    "CallExtern closure replay param {err} for func_id={} name={} slot={}",
                    target.func_id, target.func.name, slot_idx
                )
            })?;
        if transfer_meta.value_kind() == ValueKind::Interface {
            let Some(itab_cache) = itab_cache else {
                return Err(format!(
                    "CallExtern closure replay interface param requires itab cache for func_id={} name={} slot={}",
                    target.func_id, target.func.name, slot_idx
                ));
            };
            validate_extern_replay_interface_arg(
                gc,
                module,
                itab_cache,
                values,
                slot_idx,
                transfer_meta,
                target,
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
            SlotType::GcBase | SlotType::GcRef | SlotType::Interface0 | SlotType::Interface1
        )
    })
}

fn validate_extern_replay_concrete_arg(
    gc: &Gc,
    values: &mut [u64],
    slot_idx: usize,
    expected_meta: ValueMeta,
    target: &ValidClosureTarget<'_>,
) -> Result<(), String> {
    let raw = values[slot_idx];
    let canonical = canonicalize_concrete_heap_value(gc, raw, expected_meta).map_err(|err| {
        format!(
            "CallExtern closure replay param {err} for func_id={} name={} slot={}",
            target.func_id, target.func.name, slot_idx
        )
    })?;
    if let Some(canonical) = canonical {
        values[slot_idx] = canonical as u64;
    }
    Ok(())
}

fn validate_extern_replay_interface_arg(
    gc: &Gc,
    module: &Module,
    itab_cache: &ItabCache,
    values: &mut [u64],
    slot_idx: usize,
    expected_meta: ValueMeta,
    target: &ValidClosureTarget<'_>,
) -> Result<(), String> {
    let expected_iface_meta_id = expected_meta.meta_id();
    let slot0 = values[slot_idx];
    let slot1 = values[slot_idx + 1];
    let validated = validate_interface_value(gc, module, slot0, slot1).map_err(|err| {
        format!(
            "CallExtern closure replay interface arg {err} for func_id={} name={} slot={}",
            target.func_id, target.func.name, slot_idx
        )
    })?;
    let (value_rttid, canonical_data) = match validated {
        ValidatedInterfaceValue::Nil => (None, None),
        ValidatedInterfaceValue::Concrete {
            value_rttid,
            canonical_data,
        } => (Some(value_rttid), canonical_data),
    };
    if let Some(canonical) = canonical_data {
        values[slot_idx + 1] = canonical as u64;
    }
    let itab_id = vo_runtime::objects::interface::unpack_itab_id(slot0);
    validate_interface_itab(
        module,
        itab_cache,
        expected_iface_meta_id,
        itab_id,
        value_rttid,
    )
    .map_err(|err| {
        format!(
            "CallExtern closure replay interface arg {err} for func_id={} name={} slot={} itab_id={} iface_meta_id={}",
            target.func_id, target.func.name, slot_idx, itab_id, expected_iface_meta_id
        )
    })
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
            SlotType::GcBase => {
                let raw = values[slot_idx];
                if raw != 0 {
                    let Some(canonical) = gc.canonicalize_ref(raw as GcRef) else {
                        return Err(format!(
                            "{context} invalid GcBase func_id={} name={} slot={} raw=0x{:016x}",
                            func_id, func_name, slot_idx, raw
                        ));
                    };
                    if canonical as u64 != raw {
                        return Err(format!(
                            "{context} interior pointer in GcBase func_id={} name={} slot={} raw=0x{:016x}",
                            func_id, func_name, slot_idx, raw
                        ));
                    }
                }
                slot_idx += 1;
            }
            SlotType::GcRef => {
                let raw = values[slot_idx];
                if raw != 0 {
                    let Some(canonical) = gc.canonicalize_ref(raw as GcRef) else {
                        return Err(format!(
                            "{context} invalid GcRef func_id={} name={} slot={} raw=0x{:016x}",
                            func_id, func_name, slot_idx, raw
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
                let value_kind = vo_runtime::objects::interface::try_unpack_value_kind(slot0)
                    .ok_or_else(|| {
                        format!(
                            "{context} interface has invalid value-kind tag {} for func_id={} name={} slot={}",
                            slot0 as u8, func_id, func_name, slot_idx
                        )
                    })?;
                if value_kind.may_contain_gc_refs() && raw != 0 {
                    let Some(canonical) = gc.canonicalize_ref(raw as GcRef) else {
                        return Err(format!(
                            "{context} invalid interface GcRef func_id={} name={} slot={} raw=0x{:016x}",
                            func_id,
                            func_name,
                            slot_idx + 1,
                            raw
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
    let object = closure::validate_object(gc, raw_ref as GcRef).map_err(|error| {
        format!(
            "{context} requested invalid closure object at {:p}: {error}",
            raw_ref as GcRef
        )
    })?;
    validate_closure_target_object(module, object, context)
}

fn validate_closure_target_object<'a>(
    module: &'a Module,
    object: closure::ValidatedClosureObject,
    context: &str,
) -> Result<ValidClosureTarget<'a>, String> {
    let canonical_ref = object.reference;
    let func_id = object.func_id;
    let capture_count = object.capture_count;
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

/// Reconstruct a closure call target from an allocation proof and a
/// module-specific call proof published for the same dispatch identity.
fn trusted_cached_closure_target<'a>(
    module: &'a Module,
    object: closure::ValidatedClosureObject,
    cached: vo_runtime::DynamicCallTarget,
) -> Result<ValidClosureTarget<'a>, String> {
    if cached.func_id != object.func_id {
        return Err(format!(
            "CallClosure cached function id {} does not match closure function id {}",
            cached.func_id, object.func_id
        ));
    }
    let closure_gcref = object.reference;
    let Some(func) = module.functions.get(cached.func_id as usize) else {
        return Err(format!(
            "CallClosure cached function id {} is out of bounds",
            cached.func_id
        ));
    };
    let layout = unsafe {
        closure_call_layout(
            closure_gcref as u64,
            closure_gcref,
            func.recv_slots as usize,
            func.is_closure,
        )
    }
    .map_err(|err| {
        format!(
            "CallClosure cached layout is invalid for func_id={} name={}: {}",
            cached.func_id,
            func.name,
            err.message()
        )
    })?;
    Ok(ValidClosureTarget {
        func_id: cached.func_id,
        closure_gcref,
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
    let metadata = func.instruction_metadata.get(pc).ok_or_else(|| {
        format!(
            "{context} missing CallLayout metadata for caller {} pc {}",
            func.name, pc
        )
    })?;
    match metadata {
        InstructionMetadata::CallLayout {
            arg_layout,
            ret_layout,
        } => Ok((arg_layout.as_slice(), ret_layout.as_slice())),
        other => Err(format!(
            "{context} expected CallLayout metadata for caller {} pc {}, got {other:?}",
            func.name, pc
        )),
    }
}

pub(crate) fn shared_call_arg_layout_for_callsite<'a>(
    caller: &'a FunctionDef,
    module: &'a Module,
    pc: usize,
    inst: &Instruction,
    context: &str,
) -> Result<&'a [SlotType], String> {
    if inst.call_shape_is_closure() {
        let (args, returns) = call_layout_for_callsite(caller, pc, context)?;
        if !returns.is_empty() {
            return Err(format!(
                "{context} closure callsite must not declare return slots"
            ));
        }
        return Ok(args);
    }
    let func_id = inst.call_shape_static_func_id();
    let callee = module
        .functions
        .get(func_id as usize)
        .ok_or_else(|| format!("{context} references missing function {func_id}"))?;
    callee
        .slot_types
        .get(..usize::from(callee.param_slots))
        .ok_or_else(|| {
            format!(
                "{context} target {} has param_slots={} with only {} slot types",
                callee.name,
                callee.param_slots,
                callee.slot_types.len()
            )
        })
}

pub(crate) fn call_iface_layout_for_callsite<'a>(
    func: &'a FunctionDef,
    pc: usize,
    context: &str,
) -> Result<(u32, u32, &'a [SlotType], &'a [SlotType]), String> {
    let Some(metadata) = func.instruction_metadata.get(pc) else {
        return Err(format!(
            "{context} missing CallIfaceLayout metadata for caller {} pc {}",
            func.name, pc
        ));
    };
    match metadata {
        InstructionMetadata::CallIfaceLayout {
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
            "{context} invalid target frame shape for func_id={} name={}: param_slots={} local_slots={}",
            func_id,
            func.name,
            func.param_slots,
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
