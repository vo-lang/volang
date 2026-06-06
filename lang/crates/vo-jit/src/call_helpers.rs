//! Shared call emission helpers for FunctionCompiler and LoopCompiler.
//!
//! The root module owns only shared call ABI/frame guard helpers plus public
//! exports. Concrete lowering lives in focused submodules:
//! dynamic inline-cache calls, prepared-call dispatch, extern calls, and VM call
//! materialization for ordinary bytecode calls.

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, InstBuilder, MemFlags, SigRef, Value};

use vo_runtime::jit_api::JitContext;

use crate::translator::IrEmitter;

mod callback_abi;
mod dynamic;
mod externs;
mod plan;
mod prepared;
mod result_flow;
mod vm_materialization;

pub use callback_abi::{
    emit_checked_jit_result_indirect_callback_call, emit_raw_jit_context_callback_call,
    emit_returning_jit_result_indirect_callback_call, jit_context_callback_callsites,
    JitContextCallbackCallKind, CALL_DEPTH_OVERFLOW_CALLSITE, NON_OK_SLOW_PATH_PUSH_FRAME_CALLSITE,
    NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE, PREPARED_CALL_POP_FRAME_CALLSITE,
    PREPARED_CALL_PUSH_RESUME_POINT_CALLSITE, PREPARE_CLOSURE_CALLSITE, PREPARE_IFACE_CALLSITE,
    STACK_LIMIT_OVERFLOW_CALLSITE,
};
pub use dynamic::{emit_call_closure, emit_call_iface};
pub use externs::{emit_call_extern, CallExternConfig};
pub use plan::{
    CallPlan, CallRoute, CallViaVmConfig, DynamicCallPlan, JitCallWithVmMaterializationConfig,
};
pub use result_flow::{
    check_call_result, emit_checked_jit_result_helper_call, emit_non_ok_slow_path,
    NonOkSlowPathParams, JIT_RESULT_CALL, JIT_RESULT_OK,
};
pub use vm_materialization::{emit_call_via_vm, emit_jit_call_with_vm_materialization};

/// Maximum callee local_slots for direct static JIT calls from a function body.
/// Larger frames use the VM path so fiber stack limits fire before host stack exhaustion.
pub const MAX_DIRECT_JIT_NATIVE_FRAME_SLOTS: usize = 512;

fn current_call_conv<'a, E: IrEmitter<'a>>(emitter: &mut E) -> cranelift_codegen::isa::CallConv {
    emitter.builder().func.signature.call_conv
}

pub fn emit_stack_limit_guard<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ctx: Value,
    new_sp: Value,
) -> Result<(), crate::JitError> {
    let limit = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_STACK_LIMIT,
    );
    let overflow = emitter
        .builder()
        .ins()
        .icmp(IntCC::UnsignedGreaterThan, new_sp, limit);

    let overflow_block = emitter.builder().create_block();
    let ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(overflow, overflow_block, &[], ok_block, &[]);

    emitter.builder().switch_to_block(overflow_block);
    emitter.builder().seal_block(overflow_block);
    mark_stack_overflow_pc(emitter, ctx);
    let stack_overflow_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_STACK_OVERFLOW_FN,
    );
    emit_returning_jit_result_indirect_callback_call(
        emitter,
        STACK_LIMIT_OVERFLOW_CALLSITE,
        stack_overflow_fn_ptr,
        &[ctx],
    )?;

    emitter.builder().switch_to_block(ok_block);
    emitter.builder().seal_block(ok_block);
    Ok(())
}

fn mark_stack_overflow_pc<'a, E: IrEmitter<'a>>(emitter: &mut E, ctx: Value) {
    let current_pc = emitter.current_pc() as i64;
    let pc_val = emitter.builder().ins().iconst(types::I32, current_pc);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        pc_val,
        ctx,
        JitContext::OFFSET_RUNTIME_TRAP_PC,
    );
}

pub fn emit_stack_capacity_check<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ctx: Value,
    new_sp: Value,
) -> Result<(Block, Block), crate::JitError> {
    emit_stack_limit_guard(emitter, ctx, new_sp)?;

    let capacity = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_STACK_CAP,
    );
    let exceeds_capacity =
        emitter
            .builder()
            .ins()
            .icmp(IntCC::UnsignedGreaterThan, new_sp, capacity);

    let materialize_block = emitter.builder().create_block();
    let ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(exceeds_capacity, materialize_block, &[], ok_block, &[]);

    Ok((materialize_block, ok_block))
}

pub fn emit_call_depth_enter<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ctx: Value,
) -> Result<Value, crate::JitError> {
    let depth = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_CALL_DEPTH,
    );
    let limit = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_CALL_DEPTH_LIMIT,
    );
    let overflow = emitter
        .builder()
        .ins()
        .icmp(IntCC::UnsignedGreaterThanOrEqual, depth, limit);

    let overflow_block = emitter.builder().create_block();
    let ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(overflow, overflow_block, &[], ok_block, &[]);

    emitter.builder().switch_to_block(overflow_block);
    emitter.builder().seal_block(overflow_block);
    mark_stack_overflow_pc(emitter, ctx);
    let stack_overflow_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_STACK_OVERFLOW_FN,
    );
    emit_returning_jit_result_indirect_callback_call(
        emitter,
        CALL_DEPTH_OVERFLOW_CALLSITE,
        stack_overflow_fn_ptr,
        &[ctx],
    )?;

    emitter.builder().switch_to_block(ok_block);
    emitter.builder().seal_block(ok_block);
    let next_depth = emitter.builder().ins().iadd_imm(depth, 1);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        next_depth,
        ctx,
        JitContext::OFFSET_CALL_DEPTH,
    );
    Ok(depth)
}

pub fn emit_call_depth_leave<'a, E: IrEmitter<'a>>(emitter: &mut E, ctx: Value, old_depth: Value) {
    emitter.builder().ins().store(
        MemFlags::trusted(),
        old_depth,
        ctx,
        JitContext::OFFSET_CALL_DEPTH,
    );
}

/// Create signature for JIT function: (ctx, args_ptr, ret_ptr) -> JitResult
pub fn import_jit_func_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    let call_conv = current_call_conv(emitter);
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(call_conv);
        sig.params
            .push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params
            .push(cranelift_codegen::ir::AbiParam::new(types::I64)); // args_ptr
        sig.params
            .push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ret_ptr
        sig.returns
            .push(cranelift_codegen::ir::AbiParam::new(types::I32)); // JitResult
        sig
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use cranelift_codegen::ir::FuncRef;
    use vo_runtime::bytecode::FunctionDef;
    use vo_runtime::instruction::Instruction;

    fn func(local_slots: u16, has_defer: bool) -> FunctionDef {
        FunctionDef {
            name: "callee".to_string(),
            param_count: 1,
            param_slots: 1,
            local_slots,
            gc_scan_slots: local_slots,
            ret_slots: 1,
            ret_slot_types: vec![vo_runtime::SlotType::Value],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer,
            has_calls: false,
            has_call_extern: false,
            code: Vec::new(),
            jit_metadata: Vec::new(),
            slot_types: Vec::new(),
            borrowed_scan_slots_prefix: Vec::new(),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[test]
    fn gc_materialize_call_plan_routes_full_function_call_shapes() {
        let self_plan = CallPlan::new(7, 2, &func(8, false), None);
        assert_eq!(
            self_plan.route_for_full_function(7),
            CallRoute::VmCallMaterialization,
            "self recursion must use VM frames so stack overflow remains recoverable"
        );

        let defer_self = CallPlan::new(7, 2, &func(8, true), None);
        assert_eq!(
            defer_self.route_for_full_function(7),
            CallRoute::VmCallMaterialization
        );

        let large = CallPlan::new(
            7,
            2,
            &func((MAX_DIRECT_JIT_NATIVE_FRAME_SLOTS + 1) as u16, false),
            None,
        );
        assert_eq!(
            large.route_for_full_function(7),
            CallRoute::VmCallMaterialization
        );
        assert_eq!(large.route_for_loop(), CallRoute::VmCallMaterialization);

        let direct = CallPlan::new(8, 2, &func(8, false), Some(FuncRef::from_u32(3)));
        assert_eq!(direct.route_for_full_function(7), CallRoute::KnownDirectJit);
        assert_eq!(direct.route_for_loop(), CallRoute::KnownDirectJit);

        let dynamic = CallPlan::new(8, 2, &func(8, false), None);
        assert_eq!(
            dynamic.route_for_full_function(7),
            CallRoute::DynamicJitTable
        );
        assert_eq!(dynamic.route_for_loop(), CallRoute::DynamicJitTable);

        let mut allocating = func(8, false);
        allocating.code = vec![Instruction::new(
            vo_runtime::instruction::Opcode::PtrNew,
            0,
            1,
            1,
        )];
        let allocating_plan = CallPlan::new(8, 2, &allocating, Some(FuncRef::from_u32(4)));
        assert_eq!(
            allocating_plan.route_for_full_function(7),
            CallRoute::VmCallMaterialization,
            "allocating callees may still JIT, but must use a materialized VM frame"
        );
    }

    #[test]
    fn dynamic_call_plan_uses_packed_call_operands() {
        let inst = Instruction::with_flags(
            vo_runtime::instruction::Opcode::CallClosure,
            0,
            4,
            10,
            (3 << 8) | 2,
        );
        let plan = DynamicCallPlan::new(&inst, 41);
        assert_eq!(plan.arg_start, 10);
        assert_eq!(plan.arg_slots, 3);
        assert_eq!(plan.ret_slots, 2);
        assert_eq!(plan.ret_reg, 13);
        assert_eq!(plan.resume_pc, 42);
        assert_eq!(plan.route, CallRoute::DynamicInlineCache);
    }
}
