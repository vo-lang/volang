use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, FuncRef, InstBuilder, MemFlags, StackSlot, Value};

use vo_runtime::jit_api::JitContext;

use crate::translator::IrEmitter;

use super::{
    emit_checked_jit_result_indirect_callback_call, emit_raw_jit_context_callback_call,
    NON_OK_SLOW_PATH_PUSH_FRAME_CALLSITE, NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE,
};

// JitResult constants for readability in lowering code.
pub const JIT_RESULT_OK: i32 = 0;
pub const JIT_RESULT_CALL: i32 = 2;

/// Emit a helper/callback wrapper that returns `JitResult`, and route every
/// non-Ok result back to the VM before local execution can continue.
pub fn emit_checked_jit_result_helper_call<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    func: FuncRef,
    args: &[Value],
    spill_vars: bool,
) -> Value {
    let call = crate::translator::emit_funcref_call(emitter, func, args);
    let result = emitter.builder().inst_results(call)[0];
    check_call_result(emitter, result, spill_vars);
    result
}

/// Parameters for the non-OK slow path (shared by direct/indirect/self-recursive calls).
///
/// When a JIT callee returns non-OK (Call/WaitIo/Panic), the caller must:
/// 1. Restore ctx.jit_bp and ctx.fiber_sp to caller's values
/// 2. Spill SSA variables to fiber.stack
/// 3. push_frame to materialize callee frame
/// 4. Optionally copy args from native stack to fiber.stack
/// 5. push_resume_point for frame chain
/// 6. Return the JIT result
pub struct NonOkSlowPathParams {
    pub jit_result: Value,
    pub ctx: Value,
    pub caller_bp: Value,
    pub old_fiber_sp: Value,
    /// CALLEE's func_id, used in push_resume_point to create CallFrame(callee_func_id, callee_bp).
    pub callee_func_id_val: Value,
    pub local_slots_val: Value,
    pub ret_reg_val: Value,
    pub ret_slots_val: Value,
    pub caller_resume_pc_val: Value,
    /// Optional: (args_slot, arg_count) to copy args from native stack to fiber.stack after push_frame.
    pub copy_args: Option<(StackSlot, usize)>,
}

/// Emit the non-OK slow path: restore ctx, spill, push_frame, push_resume_point, return.
///
/// Caller is responsible for creating/switching to the non-OK block before calling this.
pub fn emit_non_ok_slow_path<'a, E: IrEmitter<'a>>(emitter: &mut E, p: NonOkSlowPathParams) {
    let ctx = p.ctx;

    // The inline update set fiber_sp = old_fiber_sp + callee_local_slots; push_frame
    // uses fiber_sp as new_bp, so restore ctx to the caller window first.
    emitter.builder().ins().store(
        MemFlags::trusted(),
        p.caller_bp,
        ctx,
        JitContext::OFFSET_JIT_BP,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        p.old_fiber_sp,
        ctx,
        JitContext::OFFSET_FIBER_SP,
    );

    emitter.spill_all_vars();

    let push_frame_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_PUSH_FRAME_FN,
    );
    let callee_fiber_args_ptr = emit_raw_jit_context_callback_call(
        emitter,
        NON_OK_SLOW_PATH_PUSH_FRAME_CALLSITE,
        push_frame_fn_ptr,
        &[
            ctx,
            p.callee_func_id_val,
            p.local_slots_val,
            p.ret_reg_val,
            p.ret_slots_val,
            p.caller_resume_pc_val,
        ],
    )
    .unwrap_or_else(|| panic!("push_frame_fn returns callee args pointer"));
    crate::contract::emit_return_if_runtime_trap_recorded(emitter);

    // Self-recursive calls pass args via native stack slot. Regular/indirect
    // calls already have callee state spilled into fiber.stack.
    if let Some((args_slot, arg_count)) = p.copy_args {
        for i in 0..arg_count {
            let val = emitter
                .builder()
                .ins()
                .stack_load(types::I64, args_slot, (i * 8) as i32);
            emitter.builder().ins().store(
                MemFlags::trusted(),
                val,
                callee_fiber_args_ptr,
                (i * 8) as i32,
            );
        }
    }

    let push_resume_point_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_PUSH_RESUME_POINT_FN,
    );
    let callee_bp = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_JIT_BP,
    );
    emit_checked_jit_result_indirect_callback_call(
        emitter,
        NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE,
        push_resume_point_fn_ptr,
        &[
            ctx,
            p.callee_func_id_val,
            p.caller_resume_pc_val,
            callee_bp,
            p.caller_bp,
            p.ret_reg_val,
            p.ret_slots_val,
        ],
        true,
    );

    emitter.builder().ins().return_(&[p.jit_result]);
}

/// Check call result and handle non-Ok cases.
///
/// JitResult: Ok=0, Panic=1, Call=2, WaitIo=3, WaitQueue=4
pub fn check_call_result<'a, E: IrEmitter<'a>>(emitter: &mut E, result: Value, spill_vars: bool) {
    let ok_block = emitter.builder().create_block();
    let non_ok_block = emitter.builder().create_block();

    let ok_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter.builder().ins().icmp(IntCC::Equal, result, ok_val);
    emitter
        .builder()
        .ins()
        .brif(is_ok, ok_block, &[], non_ok_block, &[]);

    emitter.builder().switch_to_block(non_ok_block);
    emitter.builder().seal_block(non_ok_block);

    if spill_vars {
        emitter.spill_all_vars();
    }

    emitter.builder().ins().return_(&[result]);

    emitter.builder().switch_to_block(ok_block);
    emitter.builder().seal_block(ok_block);
}

#[cfg(test)]
mod tests {
    #[test]
    fn result_flow_is_not_inlined_back_into_call_helpers_root() {
        let root = include_str!("../call_helpers.rs");
        assert!(
            root.contains("mod result_flow;"),
            "call_helpers root must delegate JitResult flow to result_flow"
        );
        assert!(
            !root.contains("pub struct NonOkSlowPathParams {"),
            "non-Ok materialization belongs in result_flow, not the lowering root"
        );
        assert!(
            !root.contains("pub fn check_call_result"),
            "common JitResult checks belong in result_flow, not the lowering root"
        );
    }
}
