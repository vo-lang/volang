//! JIT callbacks for goroutine spawning.

use vo_runtime::jit_api::{
    set_jit_infra_error, set_jit_infra_error_with_message, JitContext, JitResult,
    JitRuntimeTrapKind, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};

use crate::fiber::{Fiber, JitExternSuspend};
use crate::frame_call::{
    call_layout_for_callsite, validate_closure_arg_shape, validate_closure_callsite_arg_layout,
    validate_closure_target, validate_function_arg_shape, validate_function_callsite_arg_layout,
    validate_island_handle, ValidClosureTarget,
};
use crate::runtime_boundary::{
    IslandCommandEffect, PendingTransitionTerminalPolicy, ResumePolicy, RuntimeBoundary,
    RuntimeTransition,
};
use crate::vm::{helpers, GcRootEffect, Vm};

use super::helpers::{record_runtime_trap, set_jit_trap, validate_callback_raw_slots};

fn reject_invalid_object_kind(ctx: &mut JitContext, raw_ref: u64) -> JitResult {
    set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, raw_ref)
}

fn reject_invalid_callback_state(ctx: &mut JitContext, detail: u64) -> JitResult {
    set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, detail)
}

fn validate_regular_go_start_abi(
    ctx: &mut JitContext,
    func_id: u32,
    func: &vo_runtime::bytecode::FunctionDef,
    args_ptr: *const u64,
    arg_slots: u32,
) -> Result<(), JitResult> {
    let validated_arg_slots = validate_callback_raw_slots(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        func_id as u64,
        args_ptr,
        arg_slots,
    )?;
    if u32::try_from(validated_arg_slots).ok() != Some(arg_slots)
        || validate_function_arg_shape("JIT GoStart", func_id, func, validated_arg_slots).is_err()
    {
        return Err(reject_invalid_callback_state(ctx, func_id as u64));
    }
    Ok(())
}

fn validate_closure_go_abi<'a>(
    ctx: &mut JitContext,
    target: &'a ValidClosureTarget<'a>,
    args_ptr: *const u64,
    arg_slots: u32,
) -> Result<usize, JitResult> {
    let supplied_arg_slots = validate_callback_raw_slots(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        target.func_id as u64,
        args_ptr,
        arg_slots,
    )?;
    if validate_closure_arg_shape("jit go closure", target, supplied_arg_slots).is_err() {
        return Err(reject_invalid_callback_state(ctx, target.func_id as u64));
    }
    Ok(supplied_arg_slots)
}

fn jit_callsite_arg_layout<'a>(
    ctx: &mut JitContext,
    module: &'a vo_runtime::bytecode::Module,
    context: &str,
    detail: u64,
) -> Result<&'a [vo_runtime::SlotType], JitResult> {
    let callsite_pc = ctx.runtime_trap_pc;
    if callsite_pc == u32::MAX {
        return Err(reject_invalid_callback_state(ctx, detail));
    }
    let fiber = unsafe { (ctx.fiber as *const Fiber).as_ref() }
        .ok_or_else(|| reject_invalid_callback_state(ctx, detail))?;
    let caller_frame = fiber
        .current_frame()
        .ok_or_else(|| reject_invalid_callback_state(ctx, detail))?;
    let caller_func = module
        .functions
        .get(caller_frame.func_id as usize)
        .ok_or_else(|| reject_invalid_callback_state(ctx, caller_frame.func_id as u64))?;
    let (arg_layout, ret_layout) =
        call_layout_for_callsite(caller_func, callsite_pc as usize, context)
            .map_err(|_| reject_invalid_callback_state(ctx, callsite_pc as u64))?;
    if !ret_layout.is_empty() {
        return Err(reject_invalid_callback_state(ctx, detail));
    }
    Ok(arg_layout)
}

fn validate_jit_closure_go_callsite_layout(
    ctx: &mut JitContext,
    context: &str,
    target: &ValidClosureTarget<'_>,
    arg_layout: &[vo_runtime::SlotType],
) -> Result<(), JitResult> {
    validate_closure_callsite_arg_layout(context, target, arg_layout)
        .map_err(|_| reject_invalid_callback_state(ctx, target.func_id as u64))
}

fn validate_jit_static_go_callsite(
    ctx: &mut JitContext,
    module: &vo_runtime::bytecode::Module,
    context: &str,
    func_id: u32,
    func: &vo_runtime::bytecode::FunctionDef,
) -> Result<(), JitResult> {
    let arg_layout = jit_callsite_arg_layout(ctx, module, context, func_id as u64)?;
    validate_function_callsite_arg_layout(
        context,
        func_id,
        func,
        0,
        func.param_slots as usize,
        arg_layout,
    )
    .map_err(|_| reject_invalid_callback_state(ctx, func_id as u64))
}

/// Create a new fiber from a closure and spawn it on the scheduler.
///
/// Handles: func_id resolution, Fiber creation, push_frame, call_layout arg copy, and spawn.
unsafe fn build_closure_fiber(
    vm: &mut Vm,
    module: &vo_runtime::bytecode::Module,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) -> Result<Fiber, helpers::ClosureFiberBuildError> {
    let new_fiber = helpers::try_build_closure_fiber_from_args_ptr(
        &vm.state.gc,
        module,
        vm.scheduler.fibers.len() as u32,
        closure_ref,
        args_ptr,
        arg_slots,
    )?;
    Ok(new_fiber)
}

fn commit_go_transition(
    ctx: &mut JitContext,
    vm: &mut Vm,
    mut transition: RuntimeTransition,
    terminal_policy: PendingTransitionTerminalPolicy,
) -> JitResult {
    transition.set_pending_terminal_policy(terminal_policy);
    vm.push_pending_runtime_transition(transition);
    suspend_after_go_boundary(ctx)
}

fn suspend_after_go_boundary(ctx: &mut JitContext) -> JitResult {
    let Some(resume_pc) = ctx.runtime_trap_pc.checked_add(1) else {
        return reject_invalid_callback_state(ctx, ctx.runtime_trap_pc as u64);
    };
    let fiber = unsafe { &mut *(ctx.fiber as *mut Fiber) };
    fiber.jit_extern_suspend = Some(JitExternSuspend::Yield { resume_pc });
    JitResult::ExternSuspend
}

fn commit_go_spawn(ctx: &mut JitContext, vm: &mut Vm, fiber: Fiber) -> JitResult {
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    transition.spawns.push(fiber);
    commit_go_transition(
        ctx,
        vm,
        transition,
        PendingTransitionTerminalPolicy::CommitOnLanguagePanic,
    )
}

fn commit_go_island_commands(
    ctx: &mut JitContext,
    vm: &mut Vm,
    island_commands: Vec<IslandCommandEffect>,
    terminal_policy: PendingTransitionTerminalPolicy,
    rollback: Option<crate::runtime_boundary::RuntimeRollback>,
) -> JitResult {
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    transition.island_commands = island_commands;
    if let Some(rollback) = rollback {
        transition.set_rollback(rollback);
    }
    commit_go_transition(ctx, vm, transition, terminal_policy)
}

/// JIT callback to spawn a new goroutine.
/// This is fire-and-forget - creates a new fiber and adds it to the scheduler.
pub extern "C" fn jit_go_start(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure_call: u32,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let module = unsafe { &*(ctx.module) };
    let raw_arg_detail = if is_closure_call != 0 {
        closure_ref
    } else {
        func_id as u64
    };
    if let Err(result) = validate_callback_raw_slots(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        raw_arg_detail,
        args_ptr,
        arg_slots,
    ) {
        return result;
    }

    if is_closure_call != 0 {
        let arg_layout = match jit_callsite_arg_layout(ctx, module, "JIT GoStart", raw_arg_detail) {
            Ok(arg_layout) => arg_layout,
            Err(result) => return result,
        };
        if closure_ref != 0 {
            let gc = unsafe { &*ctx.gc };
            let closure_target =
                match validate_closure_target(gc, module, closure_ref, "jit_go_start") {
                    Ok(target) => target,
                    Err(_) => return reject_invalid_object_kind(ctx, closure_ref),
                };
            if let Err(result) = validate_closure_go_abi(ctx, &closure_target, args_ptr, arg_slots)
            {
                return result;
            }
            if let Err(result) = validate_jit_closure_go_callsite_layout(
                ctx,
                "JIT GoStart",
                &closure_target,
                arg_layout,
            ) {
                return result;
            }
        }
        match unsafe { build_closure_fiber(vm, module, closure_ref, args_ptr, arg_slots) } {
            Ok(new_fiber) => commit_go_spawn(ctx, vm, new_fiber),
            Err(helpers::ClosureFiberBuildError::Trap(kind)) => {
                let trap = match kind {
                    crate::vm::RuntimeTrapKind::StackOverflow => JitRuntimeTrapKind::StackOverflow,
                    _ => JitRuntimeTrapKind::NilFuncCall,
                };
                record_runtime_trap(ctx, trap, ctx.runtime_trap_pc);
                JitResult::Panic
            }
            Err(helpers::ClosureFiberBuildError::Malformed(_)) => {
                reject_invalid_callback_state(ctx, closure_ref)
            }
        }
    } else {
        // Regular function: args start at reg[0]
        let Some(func) = module.functions.get(func_id as usize) else {
            return vo_runtime::jit_api::set_jit_infra_error(
                ctx,
                vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                func_id as u64,
            );
        };
        if let Err(result) = validate_regular_go_start_abi(ctx, func_id, func, args_ptr, arg_slots)
        {
            return result;
        }
        if let Err(result) =
            validate_jit_static_go_callsite(ctx, module, "JIT GoStart", func_id, func)
        {
            return result;
        }
        let next_id = vm.scheduler.fibers.len() as u32;
        let mut new_fiber = Fiber::new(next_id);
        if let Err(err) =
            new_fiber.try_push_frame(func_id, func.local_slots, func.gc_scan_slots, 0, 0)
        {
            record_runtime_trap(ctx, JitRuntimeTrapKind::StackOverflow, ctx.runtime_trap_pc);
            let current = unsafe { &mut *(ctx.fiber as *mut Fiber) };
            return set_jit_trap(
                &mut vm.state.gc,
                current,
                crate::vm::RuntimeTrapKind::StackOverflow,
                &err.message(),
            );
        }
        let new_stack = new_fiber.stack_ptr();
        for i in 0..arg_slots as usize {
            unsafe { *new_stack.add(i) = *args_ptr.add(i) };
        }
        commit_go_spawn(ctx, vm, new_fiber)
    }
}

/// JIT callback to spawn a goroutine on a specific island.
/// If island_id == 0, spawns locally. Otherwise sends to remote island via command channel.
pub extern "C" fn jit_go_island(
    ctx: *mut JitContext,
    island: u64,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    let vm = unsafe { &mut *(ctx.vm as *mut Vm) };
    let module = unsafe { &*(ctx.module) };
    if let Err(result) = validate_callback_raw_slots(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        closure_ref,
        args_ptr,
        arg_slots,
    ) {
        return result;
    }
    let arg_layout = match jit_callsite_arg_layout(ctx, module, "JIT GoIsland", closure_ref) {
        Ok(arg_layout) => arg_layout,
        Err(result) => return result,
    };

    if island == 0 {
        record_runtime_trap(
            ctx,
            JitRuntimeTrapKind::NilPointerDereference,
            ctx.runtime_trap_pc,
        );
        return JitResult::Panic;
    }
    if closure_ref == 0 {
        record_runtime_trap(ctx, JitRuntimeTrapKind::NilFuncCall, ctx.runtime_trap_pc);
        return JitResult::Panic;
    }

    let gc = unsafe { &*ctx.gc };
    let island_handle = match validate_island_handle(gc, island, "jit_go_island") {
        Ok(island_handle) => island_handle,
        Err(_) => return reject_invalid_object_kind(ctx, island),
    };
    let closure_target = match validate_closure_target(gc, module, closure_ref, "jit_go_island") {
        Ok(target) => target,
        Err(_) => return reject_invalid_object_kind(ctx, closure_ref),
    };
    let island_id = vo_runtime::island::id(island_handle);
    let supplied_arg_slots =
        match validate_closure_go_abi(ctx, &closure_target, args_ptr, arg_slots) {
            Ok(supplied_arg_slots) => supplied_arg_slots,
            Err(result) => return result,
        };
    if let Err(result) =
        validate_jit_closure_go_callsite_layout(ctx, "JIT GoIsland", &closure_target, arg_layout)
    {
        return result;
    }
    if island_id != vm.state.current_island_id {
        if let Err(msg) =
            crate::exec::preflight_island_route(&vm.state, island_id, "GoIsland spawn route")
        {
            return set_jit_infra_error_with_message(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                island_id as u64,
                msg,
            );
        }
    }

    if island_id == vm.state.current_island_id {
        match unsafe { build_closure_fiber(vm, module, closure_ref, args_ptr, arg_slots) } {
            Ok(new_fiber) => commit_go_spawn(ctx, vm, new_fiber),
            Err(helpers::ClosureFiberBuildError::Trap(kind)) => {
                let trap = match kind {
                    crate::vm::RuntimeTrapKind::StackOverflow => JitRuntimeTrapKind::StackOverflow,
                    _ => JitRuntimeTrapKind::NilFuncCall,
                };
                record_runtime_trap(ctx, trap, ctx.runtime_trap_pc);
                JitResult::Panic
            }
            Err(helpers::ClosureFiberBuildError::Malformed(_)) => {
                reject_invalid_callback_state(ctx, closure_ref)
            }
        }
    } else {
        let func_id = closure_target.func_id;
        let capture_count = closure_target.capture_count();
        let func_def = closure_target.func;

        let mut capture_data = Vec::with_capacity(capture_count);
        for i in 0..capture_count {
            capture_data.push(closure_target.capture(i));
        }

        let mut arg_data = Vec::with_capacity(supplied_arg_slots);
        for i in 0..supplied_arg_slots {
            arg_data.push(unsafe { *args_ptr.add(i) });
        }

        let result = crate::exec::GoIslandResult {
            island: island_handle,
            func_id,
            receiver_capture_slots: closure_target.layout.receiver_capture_count as u16,
            capture_data,
            arg_data,
        };

        let (result, capture_types) = if result.receiver_capture_slots == 0 {
            (result, func_def.capture_types.clone())
        } else {
            match crate::exec::direct_method_receiver_transfer_plan(
                module,
                result.func_id,
                func_def,
                result.receiver_capture_slots,
            ) {
                Ok(plan) => (
                    crate::exec::apply_direct_method_receiver_transfer_plan(result, plan),
                    vec![plan.transfer_type],
                ),
                Err(_) => {
                    return vo_runtime::jit_api::set_jit_infra_error(
                        ctx,
                        vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                        func_id as u64,
                    );
                }
            }
        };

        let param_types = match crate::exec::go_island_sender_param_transfer_types(
            module,
            result.func_id,
            func_def,
            result.arg_data.len(),
        ) {
            Ok(param_types) => param_types,
            Err(_) => {
                return vo_runtime::jit_api::set_jit_infra_error(
                    ctx,
                    vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    func_id as u64,
                );
            }
        };
        let mut island_effects = Vec::new();
        let transfer_commit = match crate::exec::prepare_queue_handles_for_transfer(
            &result,
            island_id,
            &capture_types,
            &param_types,
            &module.struct_metas,
            &module.named_type_metas,
            &module.runtime_types,
            &mut vm.state,
            &mut island_effects,
        ) {
            Ok(commit) => commit,
            Err(_) => {
                return vo_runtime::jit_api::set_jit_infra_error(
                    ctx,
                    vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    func_id as u64,
                );
            }
        };
        let data = crate::exec::pack_closure_for_island(
            &vm.state.gc,
            &result,
            &capture_types,
            &param_types,
            &module.struct_metas,
            &module.named_type_metas,
            &module.runtime_types,
        );
        let data = match data {
            Ok(data) => data,
            Err(_) => {
                return vo_runtime::jit_api::set_jit_infra_error(
                    ctx,
                    vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    func_id as u64,
                );
            }
        };
        let closure_data = vo_runtime::pack::PackedValue::from_data(data);
        island_effects.push(IslandCommandEffect::spawn_fiber(island_id, closure_data));
        let terminal_policy = if transfer_commit.requires_terminal_commit() {
            PendingTransitionTerminalPolicy::CommitOnAnyTerminal
        } else {
            PendingTransitionTerminalPolicy::CommitOnLanguagePanic
        };
        let rollback = transfer_commit.into_runtime_rollback();
        commit_go_island_commands(ctx, vm, island_effects, terminal_policy, rollback)
    }
}

#[cfg(test)]
mod tests;
