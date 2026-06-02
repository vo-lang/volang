//! JIT callbacks for goroutine spawning.

use vo_runtime::gc::GcRef;
use vo_runtime::jit_api::{
    set_jit_infra_error, JitContext, JitResult, JitRuntimeTrapKind,
    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};
use vo_runtime::objects::closure;
use vo_runtime::ValueKind;

use crate::fiber::Fiber;
use crate::vm::{helpers, Vm};

use super::helpers::{record_runtime_trap, set_jit_trap};

fn canonical_object_with_kind(
    ctx: &JitContext,
    raw_ref: u64,
    expected: ValueKind,
) -> Option<GcRef> {
    let gc = unsafe { &*ctx.gc };
    let obj = gc.canonicalize_ref(raw_ref as GcRef)?;
    (vo_runtime::gc::Gc::header(obj).kind() == expected).then_some(obj)
}

fn reject_invalid_object_kind(ctx: &mut JitContext, raw_ref: u64) -> JitResult {
    set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, raw_ref)
}

/// Create a new fiber from a closure and spawn it on the scheduler.
///
/// Handles: func_id resolution, Fiber creation, push_frame, call_layout arg copy, and spawn.
unsafe fn spawn_closure_fiber(
    vm: &mut Vm,
    module: &vo_runtime::bytecode::Module,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) -> Result<(), crate::vm::RuntimeTrapKind> {
    let new_fiber = helpers::try_build_closure_fiber_from_args_ptr(
        &module.functions,
        vm.scheduler.fibers.len() as u32,
        closure_ref,
        args_ptr,
        arg_slots,
    )?;
    vm.scheduler.spawn(new_fiber);
    Ok(())
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

    if is_closure_call != 0 {
        if closure_ref != 0 {
            let Some(closure_gcref) =
                canonical_object_with_kind(ctx, closure_ref, ValueKind::Closure)
            else {
                return reject_invalid_object_kind(ctx, closure_ref);
            };
            let func_id = closure::func_id(closure_gcref);
            if module.functions.get(func_id as usize).is_none() {
                return vo_runtime::jit_api::set_jit_infra_error(
                    ctx,
                    vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    func_id as u64,
                );
            }
        }
        if let Err(kind) =
            unsafe { spawn_closure_fiber(vm, module, closure_ref, args_ptr, arg_slots) }
        {
            let trap = match kind {
                crate::vm::RuntimeTrapKind::StackOverflow => JitRuntimeTrapKind::StackOverflow,
                _ => JitRuntimeTrapKind::NilFuncCall,
            };
            record_runtime_trap(ctx, trap, ctx.runtime_trap_pc);
            return JitResult::Panic;
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
        vm.scheduler.spawn(new_fiber);
    }
    JitResult::Ok
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

    let Some(island_handle) = canonical_object_with_kind(ctx, island, ValueKind::Island) else {
        return reject_invalid_object_kind(ctx, island);
    };
    let Some(closure) = canonical_object_with_kind(ctx, closure_ref, ValueKind::Closure) else {
        return reject_invalid_object_kind(ctx, closure_ref);
    };
    let island_id = vo_runtime::island::id(island_handle);

    if island_id == vm.state.current_island_id {
        if closure_ref != 0 {
            let func_id = closure::func_id(closure);
            if module.functions.get(func_id as usize).is_none() {
                return vo_runtime::jit_api::set_jit_infra_error(
                    ctx,
                    vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    func_id as u64,
                );
            }
        }
        if let Err(kind) =
            unsafe { spawn_closure_fiber(vm, module, closure_ref, args_ptr, arg_slots) }
        {
            let trap = match kind {
                crate::vm::RuntimeTrapKind::StackOverflow => JitRuntimeTrapKind::StackOverflow,
                _ => JitRuntimeTrapKind::NilFuncCall,
            };
            record_runtime_trap(ctx, trap, ctx.runtime_trap_pc);
            return JitResult::Panic;
        }
    } else {
        let func_id = closure::func_id(closure);
        let capture_count = closure::capture_count(closure);

        let mut capture_data = Vec::with_capacity(capture_count);
        for i in 0..capture_count {
            capture_data.push(closure::get_capture(closure, i));
        }

        let mut arg_data = Vec::with_capacity(arg_slots as usize);
        for i in 0..arg_slots as usize {
            arg_data.push(unsafe { *args_ptr.add(i) });
        }

        let Some(func_def) = module.functions.get(func_id as usize) else {
            return vo_runtime::jit_api::set_jit_infra_error(
                ctx,
                vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                func_id as u64,
            );
        };
        let result = crate::exec::GoIslandResult {
            island: island_handle,
            func_id,
            capture_data,
            arg_data,
        };

        if crate::exec::prepare_queue_handles_for_transfer(
            &result,
            island_id,
            &func_def.capture_types,
            &func_def.param_types,
            &module.struct_metas,
            &module.runtime_types,
            &mut vm.state,
        )
        .is_err()
        {
            return vo_runtime::jit_api::set_jit_infra_error(
                ctx,
                vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                func_id as u64,
            );
        }
        let data = crate::exec::pack_closure_for_island(
            &vm.state.gc,
            &result,
            &func_def.capture_types,
            &func_def.param_types,
            &module.struct_metas,
            &module.runtime_types,
        );
        let closure_data = vo_runtime::pack::PackedValue::from_data(data);
        vm.state.send_spawn_fiber_to_island(island_id, closure_data);
    }
    JitResult::Ok
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::ffi::c_void;
    use vo_runtime::bytecode::{FunctionDef, Module};
    use vo_runtime::ffi::SentinelErrorCache;
    use vo_runtime::itab::ItabCache;
    use vo_runtime::jit_api::{
        JitRuntimeTrapKind, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, JIT_INFRA_ERROR_SENTINEL,
    };
    use vo_runtime::objects::interface::InterfaceSlot;
    use vo_runtime::output::CaptureSink;
    use vo_runtime::SlotType;

    fn minimal_func() -> FunctionDef {
        FunctionDef {
            name: "callee".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 1,
            gc_scan_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: Vec::new(),
            jit_metadata: Vec::new(),
            slot_types: vec![SlotType::Value],
            borrowed_scan_slots_prefix: vec![0, 0],
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn test_context<'a>(
        vm: &'a mut Vm,
        module: &'a Module,
        fiber: &'a mut Fiber,
        itab_cache: &'a mut ItabCache,
        safepoint_flag: &'a bool,
        panic_flag: &'a mut bool,
        is_user_panic: &'a mut bool,
        panic_msg: &'a mut InterfaceSlot,
        program_args: &'a Vec<String>,
        sentinel_errors: &'a mut SentinelErrorCache,
        output: &'a CaptureSink,
        host_output: &'a mut Option<Vec<u8>>,
    ) -> JitContext {
        JitContext {
            gc: &mut vm.state.gc,
            globals: core::ptr::null_mut(),
            safepoint_flag,
            panic_flag,
            is_user_panic,
            panic_msg,
            user_panic_pc: u32::MAX,
            runtime_trap_kind: JitRuntimeTrapKind::None as u8,
            runtime_trap_arg0: 0,
            runtime_trap_arg1: 0,
            runtime_trap_pc: u32::MAX,
            vm: vm as *mut Vm as *mut c_void,
            fiber: fiber as *mut Fiber as *mut c_void,
            itab_cache,
            extern_registry: core::ptr::null(),
            call_extern_fn: None,
            module,
            jit_func_table: core::ptr::null(),
            jit_func_count: 0,
            direct_call_table: core::ptr::null(),
            direct_call_count: 0,
            program_args,
            sentinel_errors,
            output: output as *const dyn vo_runtime::output::OutputSink,
            host_output,
            #[cfg(feature = "std")]
            io: core::ptr::null_mut(),
            call_func_id: 0,
            call_arg_start: 0,
            call_resume_pc: 0,
            call_ret_slots: 0,
            call_ret_reg: 0,
            call_kind: 0,
            #[cfg(feature = "std")]
            wait_io_token: 0,
            loop_exit_pc: 0,
            stack_ptr: core::ptr::null_mut(),
            stack_cap: 0,
            stack_limit: 0,
            call_depth: 0,
            call_depth_limit: 64,
            jit_bp: 0,
            fiber_sp: 0,
            push_frame_fn: None,
            pop_frame_fn: None,
            stack_overflow_fn: None,
            push_resume_point_fn: None,
            create_island_fn: None,
            queue_close_fn: None,
            queue_send_fn: None,
            queue_recv_fn: None,
            go_start_fn: None,
            go_island_fn: None,
            defer_push_fn: None,
            recover_fn: None,
            select_begin_fn: None,
            select_send_fn: None,
            select_recv_fn: None,
            select_exec_fn: None,
            is_error_return: 0,
            ret_gcref_start: 0,
            ret_is_heap: 0,
            ret_start: 0,
            prepare_closure_call_fn: None,
            prepare_iface_call_fn: None,
            ic_table: core::ptr::null_mut(),
        }
    }

    #[test]
    fn jit_go_island_rejects_nil_island_before_object_header_read() {
        let module = Module::new("test".to_string());
        let mut vm = Vm::new();
        let mut fiber = Fiber::new(0);
        let mut itab_cache = ItabCache::new();
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut is_user_panic = false;
        let mut panic_msg = InterfaceSlot::nil();
        let program_args = Vec::new();
        let mut sentinel_errors = SentinelErrorCache::new();
        let output = CaptureSink::new();
        let mut host_output = None;
        let mut ctx = test_context(
            &mut vm,
            &module,
            &mut fiber,
            &mut itab_cache,
            &safepoint_flag,
            &mut panic_flag,
            &mut is_user_panic,
            &mut panic_msg,
            &program_args,
            &mut sentinel_errors,
            &output,
            &mut host_output,
        );

        let result = jit_go_island(&mut ctx, 0, 0, core::ptr::null(), 0);

        assert_eq!(result, JitResult::Panic);
        assert_eq!(
            ctx.runtime_trap_kind,
            JitRuntimeTrapKind::NilPointerDereference as u8
        );
    }

    #[test]
    fn jit_go_island_rejects_nil_closure_before_object_header_read() {
        let module = Module::new("test".to_string());
        let mut vm = Vm::new();
        let island = vo_runtime::island::create(&mut vm.state.gc, vm.state.current_island_id);
        let mut fiber = Fiber::new(0);
        let mut itab_cache = ItabCache::new();
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut is_user_panic = false;
        let mut panic_msg = InterfaceSlot::nil();
        let program_args = Vec::new();
        let mut sentinel_errors = SentinelErrorCache::new();
        let output = CaptureSink::new();
        let mut host_output = None;
        let mut ctx = test_context(
            &mut vm,
            &module,
            &mut fiber,
            &mut itab_cache,
            &safepoint_flag,
            &mut panic_flag,
            &mut is_user_panic,
            &mut panic_msg,
            &program_args,
            &mut sentinel_errors,
            &output,
            &mut host_output,
        );

        let result = jit_go_island(&mut ctx, island as u64, 0, core::ptr::null(), 0);

        assert_eq!(result, JitResult::Panic);
        assert_eq!(ctx.runtime_trap_kind, JitRuntimeTrapKind::NilFuncCall as u8);
    }

    #[test]
    fn jit_go_island_rejects_non_island_gcref_before_island_header_read() {
        let mut module = Module::new("test".to_string());
        module.functions.push(minimal_func());
        let mut vm = Vm::new();
        let wrong_island = closure::create(&mut vm.state.gc, 0, 0);
        let closure_ref = closure::create(&mut vm.state.gc, 0, 0);
        let mut fiber = Fiber::new(0);
        let mut itab_cache = ItabCache::new();
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut is_user_panic = false;
        let mut panic_msg = InterfaceSlot::nil();
        let program_args = Vec::new();
        let mut sentinel_errors = SentinelErrorCache::new();
        let output = CaptureSink::new();
        let mut host_output = None;
        let mut ctx = test_context(
            &mut vm,
            &module,
            &mut fiber,
            &mut itab_cache,
            &safepoint_flag,
            &mut panic_flag,
            &mut is_user_panic,
            &mut panic_msg,
            &program_args,
            &mut sentinel_errors,
            &output,
            &mut host_output,
        );

        let result = jit_go_island(
            &mut ctx,
            wrong_island as u64,
            closure_ref as u64,
            core::ptr::null(),
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(
            ctx.runtime_trap_arg1,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
        );
    }

    #[test]
    fn jit_go_island_rejects_non_closure_gcref_before_closure_header_read() {
        let mut module = Module::new("test".to_string());
        module.functions.push(minimal_func());
        let mut vm = Vm::new();
        let island = vo_runtime::island::create(&mut vm.state.gc, vm.state.current_island_id);
        let wrong_closure =
            vo_runtime::island::create(&mut vm.state.gc, vm.state.current_island_id);
        let mut fiber = Fiber::new(0);
        let mut itab_cache = ItabCache::new();
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut is_user_panic = false;
        let mut panic_msg = InterfaceSlot::nil();
        let program_args = Vec::new();
        let mut sentinel_errors = SentinelErrorCache::new();
        let output = CaptureSink::new();
        let mut host_output = None;
        let mut ctx = test_context(
            &mut vm,
            &module,
            &mut fiber,
            &mut itab_cache,
            &safepoint_flag,
            &mut panic_flag,
            &mut is_user_panic,
            &mut panic_msg,
            &program_args,
            &mut sentinel_errors,
            &output,
            &mut host_output,
        );

        let result = jit_go_island(
            &mut ctx,
            island as u64,
            wrong_closure as u64,
            core::ptr::null(),
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(
            ctx.runtime_trap_arg1,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
        );
    }
}
