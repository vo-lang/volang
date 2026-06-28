//! JIT callbacks for defer/recover operations.

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::jit_api::{
    set_jit_infra_error, JitContext, JitResult, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};
use vo_runtime::InterfaceSlot;
use vo_runtime::{ValueKind, ValueMeta};

use crate::fiber::{DeferArgLayout, DeferEntry, Fiber};
use crate::frame_call::validate_closure_target;

use super::helpers::{
    validate_callback_raw_slot_span, validate_callback_raw_slots, validate_callback_slot_count,
};

const JIT_RECOVER_INVALID_RESULT_PTR: u64 = 1;

/// Push a defer entry from JIT code.
///
/// Called by JIT-compiled code when executing DeferPush or ErrDeferPush instructions.
/// The defer entry is stored in fiber.defer_stack with frame_depth = fiber.frames.len().
pub extern "C" fn jit_defer_push(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure: u32,
    closure_ref: u64,
    arg_start: u32,
    args_ptr: *const u64,
    arg_count: u32,
    is_errdefer: u32,
) -> JitResult {
    let ctx_ref = unsafe { &*ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    let gc = unsafe { &mut *ctx_ref.gc };
    let module = unsafe { &*ctx_ref.module };

    let is_closure = is_closure != 0;
    let arg_slots = match validate_callback_raw_slots(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        func_id as u64,
        args_ptr,
        arg_count,
    ) {
        Ok(arg_slots) => arg_slots as u16,
        Err(result) => return result,
    };
    let arg_start = match validate_callback_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        arg_start as u64,
        arg_start,
    ) {
        Ok(arg_start) => arg_start,
        Err(result) => return result,
    };
    let frame_depth = fiber.frames.len();
    let generation = fiber.effective_defer_generation();
    let Some(caller_frame) = fiber.frames.last().copied() else {
        return set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, 0);
    };
    let Some(caller_func) = module.functions.get(caller_frame.func_id as usize) else {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            caller_frame.func_id as u64,
        );
    };
    let Ok(arg_layout) = DeferArgLayout::try_from_caller_slot_types(
        &caller_func.slot_types,
        caller_frame.func_id,
        caller_frame.pc.saturating_sub(1) as u32,
        arg_start,
        arg_slots,
    ) else {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(arg_start),
        );
    };

    // Match VM push_defer_entry semantics (exec/defer.rs)
    let (fid, closure): (u32, GcRef) = if is_closure {
        let closure = if closure_ref == 0 {
            core::ptr::null_mut()
        } else {
            match validate_closure_target(gc, module, closure_ref, "JIT DeferPush closure") {
                Ok(target) => target.closure_gcref,
                Err(_) => {
                    return set_jit_infra_error(
                        ctx,
                        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                        closure_ref,
                    );
                }
            }
        };
        (0, closure)
    } else {
        (func_id, core::ptr::null_mut())
    };

    let args: GcRef = if arg_slots > 0 {
        let args_ref = gc.alloc(ValueMeta::new(0, ValueKind::Void), arg_slots);
        for i in 0..arg_slots {
            let val = unsafe { *args_ptr.add(i as usize) };
            unsafe { Gc::write_slot(args_ref, i as usize, val) };
        }
        args_ref
    } else {
        core::ptr::null_mut()
    };

    fiber.defer_stack.push(DeferEntry {
        frame_depth,
        func_id: fid,
        closure,
        args,
        arg_layout,
        is_closure,
        is_errdefer: is_errdefer != 0,
        registered_at_generation: generation,
    });
    JitResult::Ok
}

/// Execute recover() from JIT code.
///
/// Called by JIT-compiled code when executing Recover instruction.
/// Result (interface{}) is written to result_ptr (2 slots).
pub extern "C" fn jit_recover(ctx: *mut JitContext, result_ptr: *mut u64) -> JitResult {
    if let Err(result) = validate_callback_raw_slot_span(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_RECOVER_INVALID_RESULT_PTR,
        result_ptr,
        2,
    ) {
        return result;
    }

    let ctx_ref = unsafe { &*ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };

    // Check if in valid recover context
    if !fiber.is_direct_defer_context() {
        // Not in direct defer context - return nil without consuming panic
        unsafe {
            *result_ptr = 0;
            *result_ptr.add(1) = 0;
        }
        return JitResult::Ok;
    }

    // Try to take the panic value
    let recovered = fiber.take_recoverable_panic();
    let val = recovered.unwrap_or(InterfaceSlot::nil());

    // Store result (interface{} = 2 slots)
    unsafe {
        *result_ptr = val.slot0;
        *result_ptr.add(1) = val.slot1;
    }

    // If recover succeeded, switch from Panic to Return mode
    if recovered.is_some() {
        fiber.switch_panic_to_return_mode();
        JitResult::Ok
    } else {
        JitResult::Ok
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL;
    use vo_runtime::SlotType;

    fn caller_module() -> Module {
        let mut module = Module::new("jit-callback-abi-defer".to_string());
        module.functions.push(vo_runtime::bytecode::FunctionDef {
            name: "caller".to_string(),
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
            has_defer: true,
            has_calls: false,
            has_call_extern: false,
            code: Vec::new(),
            jit_metadata: Vec::new(),
            slot_types: vec![SlotType::Value],
            borrowed_scan_slots_prefix: vec![0, 0],
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        });
        module
    }

    fn assert_invalid_callback_state(ctx: &JitContext) {
        assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(
            ctx.runtime_trap_arg1,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
        );
    }

    #[test]
    fn vm_jit_callback_abi_defer_push_rejects_arg_count_overflow_before_truncation() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let module = caller_module();
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 1, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        let args = [99_u64];

        let result = jit_defer_push(
            ctx.as_ptr(),
            0,
            0,
            0,
            0,
            args.as_ptr(),
            u32::from(u16::MAX) + 1,
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert_invalid_callback_state(&ctx.ctx);
        assert!(fiber.defer_stack.is_empty());
    }

    #[test]
    fn vm_jit_callback_abi_defer_push_rejects_null_non_empty_args_before_defer_entry() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let module = caller_module();
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 1, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        let result = jit_defer_push(ctx.as_ptr(), 0, 0, 0, 0, core::ptr::null(), 1, 0);

        assert_eq!(result, JitResult::JitError);
        assert_invalid_callback_state(&ctx.ctx);
        assert!(fiber.defer_stack.is_empty());
    }

    #[test]
    fn vm_jit_callback_abi_defer_push_rejects_arg_start_width_drift_before_defer_entry() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let module = caller_module();
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 1, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        let args = [99_u64];

        let result = jit_defer_push(
            ctx.as_ptr(),
            0,
            0,
            0,
            u32::from(u16::MAX) + 1,
            args.as_ptr(),
            1,
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert_invalid_callback_state(&ctx.ctx);
        assert!(fiber.defer_stack.is_empty());
    }

    #[test]
    fn vm_jit_defer_closure_kind_062_rejects_non_closure_before_defer_publication() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let non_closure = vm.state.gc.alloc(ValueMeta::new(0, ValueKind::String), 1);
        let module = caller_module();
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 1, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        let result = jit_defer_push(ctx.as_ptr(), 0, 1, non_closure as u64, 0, [].as_ptr(), 0, 0);

        assert_eq!(result, JitResult::JitError);
        assert_invalid_callback_state(&ctx.ctx);
        assert!(fiber.defer_stack.is_empty());
    }

    #[test]
    fn vm_jit_defer_closure_kind_062_preserves_nil_defer_registration_for_recover() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let module = caller_module();
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 1, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        let result = jit_defer_push(ctx.as_ptr(), 0, 1, 0, 0, [].as_ptr(), 0, 0);

        assert_eq!(result, JitResult::Ok);
        assert_eq!(fiber.defer_stack.len(), 1);
        assert!(fiber.defer_stack[0].is_closure);
        assert!(fiber.defer_stack[0].closure.is_null());
    }

    #[test]
    fn vm_jit_defer_closure_kind_062_source_validates_before_entry_publication() {
        let source = crate::source_contract::production_source_without_test_modules(include_str!(
            "defer.rs"
        ));
        let callback_body = source
            .split("pub extern \"C\" fn jit_defer_push")
            .nth(1)
            .expect("jit_defer_push source");
        assert!(
            vo_source_contract::compact_pattern_before(
                callback_body,
                "validate_closure_target(",
                "fiber.defer_stack.push(DeferEntry"
            ),
            "JIT defer registration must validate closure target before DeferEntry publication"
        );
    }

    #[test]
    fn vm_jit_defer_closure_kind_062_source_order_ignores_comment_spoofed_validator() {
        let probe = r#"
            pub extern "C" fn jit_defer_push() {
                /* validate_closure_target( */
                fiber.defer_stack.push(DeferEntry {
                    frame_depth,
                });
            }
        "#;

        assert!(
            !vo_source_contract::compact_pattern_before(
                probe,
                "validate_closure_target(",
                "fiber.defer_stack.push(DeferEntry"
            ),
            "comments must not satisfy JIT defer validator-before-publication source contracts"
        );
    }

    #[test]
    fn vm_jit_callback_boundary_001_recover_rejects_null_result_pointer() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let module = caller_module();
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 1, 0, 0, 0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        let result = jit_recover(ctx.as_ptr(), core::ptr::null_mut());

        assert_eq!(result, JitResult::JitError);
        assert_invalid_callback_state(&ctx.ctx);
    }
}
