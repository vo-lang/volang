//! Shared call emission helpers for FunctionCompiler and LoopCompiler.
//!
//! The root module owns only shared call ABI/frame guard helpers plus public
//! exports. Concrete lowering lives in focused submodules:
//! dynamic inline-cache calls, prepared-call dispatch, extern calls, and VM call
//! materialization for ordinary bytecode calls.

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, InstBuilder, SigRef, Value};

use vo_runtime::jit_api::JitContextField;

use crate::translator::IrEmitter;

mod callback_abi;
mod dynamic;
mod externs;
mod leaf_inline;
mod plan;
mod prepared;
mod result_flow;
mod vm_materialization;

pub use callback_abi::{
    emit_checked_jit_result_indirect_callback_call, emit_raw_jit_context_callback_call,
    emit_returning_jit_result_indirect_callback_call, CALL_DEPTH_OVERFLOW_CALLSITE,
    LINK_FUNCTION_CALLSITE, NON_OK_SLOW_PATH_PUSH_FRAME_CALLSITE,
    NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE, PREPARED_CALL_POP_FRAME_CALLSITE,
    PREPARED_CALL_PUSH_RESUME_POINT_CALLSITE, PREPARE_CLOSURE_CALLSITE, PREPARE_IFACE_CALLSITE,
    STACK_LIMIT_OVERFLOW_CALLSITE,
};
pub use dynamic::{emit_call_closure, emit_call_iface};
pub use externs::{emit_call_extern, CallExternConfig};
pub(crate) use leaf_inline::{SmallPureLeafInline, SMALL_LEAF_INLINE_BUDGET};
pub use plan::{CallPlan, CallRoute, CallViaVmConfig, DynamicCallPlan};
pub use result_flow::{
    check_call_result, emit_checked_jit_result_helper_call, emit_non_ok_slow_path,
    NonOkSlowPathParams, JIT_RESULT_CALL, JIT_RESULT_OK, JIT_RESULT_REPLAY,
};
pub use vm_materialization::{emit_call_via_vm, emit_jit_call_with_vm_materialization};

/// Maximum callee local-slot width for direct static JIT calls.
///
/// Wider frames use the VM route so direct-call shadow windows and callee
/// prologue initialization stay bounded independently from the guest stack.
pub const MAX_DIRECT_JIT_FRAME_SLOTS: usize = 512;

fn current_call_conv<'a, E: IrEmitter<'a>>(emitter: &mut E) -> cranelift_codegen::isa::CallConv {
    emitter.builder().func.signature.call_conv
}

pub fn emit_stack_limit_guard<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ctx: Value,
    new_sp: Value,
) -> Result<(), crate::JitError> {
    let limit = emitter.load_context_field(types::I32, JitContextField::StackLimit);
    let overflow = emitter
        .builder()
        .ins()
        .icmp(IntCC::UnsignedGreaterThan, new_sp, limit);

    let overflow_block = crate::compile_common::cold_block(emitter.builder());
    let ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(overflow, overflow_block, &[], ok_block, &[]);

    emitter.builder().switch_to_block(overflow_block);
    emitter.builder().seal_block(overflow_block);
    mark_stack_overflow_pc(emitter);
    let stack_overflow_fn_ptr =
        emitter.load_context_field(types::I64, JitContextField::StackOverflowFn);
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

fn mark_stack_overflow_pc<'a, E: IrEmitter<'a>>(emitter: &mut E) {
    let current_pc = emitter.current_pc() as i64;
    let pc_val = emitter.builder().ins().iconst(types::I32, current_pc);
    emitter.store_context_field(pc_val, JitContextField::RuntimeTrapPc);
}

fn load_current_func_id<'a, E: IrEmitter<'a>>(emitter: &mut E) -> Value {
    emitter.load_context_field(types::I32, JitContextField::CurrentFuncId)
}

/// Resolve a cold callee and reload its native dispatch pointer. The callback
/// keeps the current native root chain linked and publishes through the same
/// generation-tracked table used by inline caches.
pub(super) fn emit_native_link<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    func_id: Value,
) -> Result<Value, crate::JitError> {
    let ctx = emitter.ctx_param();
    let link_fn = emitter.load_context_field(types::I64, JitContextField::LinkFunctionFn);
    emit_checked_jit_result_indirect_callback_call(
        emitter,
        LINK_FUNCTION_CALLSITE,
        link_fn,
        &[ctx, func_id],
        true,
    )?;

    let table = emitter.load_context_field(types::I64, JitContextField::JitFuncTable);
    let func_id_i64 = emitter.builder().ins().uextend(types::I64, func_id);
    let offset = emitter.builder().ins().imul_imm_u(
        func_id_i64,
        vo_runtime::jit_api::JitDispatchEntry::SIZE as i64,
    );
    let entry = emitter.builder().ins().iadd(table, offset);
    Ok(emitter.builder().ins().load(
        types::I64,
        cranelift_codegen::ir::MemFlagsData::trusted(),
        entry,
        vo_runtime::jit_api::JitDispatchEntry::OFFSET_NATIVE,
    ))
}

fn restore_caller_execution_context<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    caller_bp: Value,
    old_fiber_sp: Value,
    caller_func_id: Value,
) {
    emitter.store_context_field(caller_bp, JitContextField::JitBp);
    emitter.store_context_field(old_fiber_sp, JitContextField::FiberSp);
    emitter.store_context_field(caller_func_id, JitContextField::CurrentFuncId);
}

pub fn emit_stack_capacity_check<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ctx: Value,
    new_sp: Value,
) -> Result<(Block, Block), crate::JitError> {
    emit_stack_limit_guard(emitter, ctx, new_sp)?;

    let capacity = emitter.load_context_field(types::I32, JitContextField::StackCap);
    let exceeds_capacity =
        emitter
            .builder()
            .ins()
            .icmp(IntCC::UnsignedGreaterThan, new_sp, capacity);

    let materialize_block = crate::compile_common::cold_block(emitter.builder());
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
    let depth = emitter.load_context_field(types::I32, JitContextField::CallDepth);
    let limit = emitter.load_context_field(types::I32, JitContextField::CallDepthLimit);
    let overflow = emitter
        .builder()
        .ins()
        .icmp(IntCC::UnsignedGreaterThanOrEqual, depth, limit);

    let overflow_block = crate::compile_common::cold_block(emitter.builder());
    let ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(overflow, overflow_block, &[], ok_block, &[]);

    emitter.builder().switch_to_block(overflow_block);
    emitter.builder().seal_block(overflow_block);
    mark_stack_overflow_pc(emitter);
    let stack_overflow_fn_ptr =
        emitter.load_context_field(types::I64, JitContextField::StackOverflowFn);
    emit_returning_jit_result_indirect_callback_call(
        emitter,
        CALL_DEPTH_OVERFLOW_CALLSITE,
        stack_overflow_fn_ptr,
        &[ctx],
    )?;

    emitter.builder().switch_to_block(ok_block);
    emitter.builder().seal_block(ok_block);
    let next_depth = emitter.builder().ins().iadd_imm_s(depth, 1);
    emitter.store_context_field(next_depth, JitContextField::CallDepth);
    Ok(depth)
}

pub fn emit_call_depth_leave<'a, E: IrEmitter<'a>>(emitter: &mut E, old_depth: Value) {
    emitter.store_context_field(old_depth, JitContextField::CallDepth);
}

/// Create the uniform internal native signature. Raw argument lanes keep this
/// signature valid for every Vo function and allow static callers to pass SSA
/// values without first serializing them to a frame window.
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
        for _ in 0..crate::NATIVE_ARG_LANES {
            sig.params
                .push(cranelift_codegen::ir::AbiParam::new(types::I64));
        }
        sig.returns
            .push(cranelift_codegen::ir::AbiParam::new(types::I32)); // JitResult
        sig
    })
}

#[derive(Clone, Copy)]
pub(super) enum JitCallGcMode {
    Never,
    MayGc,
    Dynamic(Value),
}

pub(super) struct JitCallOperands<'a> {
    pub(super) ctx: Value,
    pub(super) args_ptr: Value,
    pub(super) ret_ptr: Value,
    pub(super) arg_lanes: &'a [Value; crate::NATIVE_ARG_LANES],
}

/// Emit one native JIT call and attach a precise root map only on paths whose
/// selected target can enter a managed-heap safepoint.
pub(super) fn emit_effect_aware_jit_call<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    jit_func_sig: SigRef,
    jit_func_ptr: Value,
    operands: JitCallOperands<'_>,
    mode: JitCallGcMode,
) -> Value {
    let emit_call = |emitter: &mut E, attach_roots: bool| {
        let native_roots = attach_roots.then(|| emitter.spill_native_roots()).flatten();
        let mut args = Vec::with_capacity(3 + crate::NATIVE_ARG_LANES);
        args.extend_from_slice(&[operands.ctx, operands.args_ptr, operands.ret_ptr]);
        args.extend_from_slice(operands.arg_lanes);
        let call = emitter
            .builder()
            .ins()
            .call_indirect(jit_func_sig, jit_func_ptr, &args);
        if attach_roots {
            emitter.attach_native_roots(call, native_roots);
            emitter.clear_dead_native_roots();
        }
        emitter.builder().inst_results(call)[0]
    };

    match mode {
        JitCallGcMode::Never => emit_call(emitter, false),
        JitCallGcMode::MayGc => emit_call(emitter, true),
        JitCallGcMode::Dynamic(may_gc) => {
            let zero = emitter.builder().ins().iconst(types::I32, 0);
            let needs_roots = emitter.builder().ins().icmp(IntCC::NotEqual, may_gc, zero);
            let gc_block = emitter.builder().create_block();
            let no_gc_block = emitter.builder().create_block();
            let merge_block = emitter.builder().create_block();
            emitter
                .builder()
                .append_block_param(merge_block, types::I32);
            emitter
                .builder()
                .ins()
                .brif(needs_roots, gc_block, &[], no_gc_block, &[]);

            emitter.builder().switch_to_block(gc_block);
            emitter.builder().seal_block(gc_block);
            let gc_result = emit_call(emitter, true);
            emitter
                .builder()
                .ins()
                .jump(merge_block, &[gc_result.into()]);

            emitter.builder().switch_to_block(no_gc_block);
            emitter.builder().seal_block(no_gc_block);
            let no_gc_result = emit_call(emitter, false);
            emitter
                .builder()
                .ins()
                .jump(merge_block, &[no_gc_result.into()]);

            emitter.builder().switch_to_block(merge_block);
            emitter.builder().seal_block(merge_block);
            emitter.builder().block_params(merge_block)[0]
        }
    }
}

/// Direct-symbol counterpart used by stable self-recursive optimizing code.
pub(super) fn emit_effect_aware_direct_jit_call<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    jit_func_ref: cranelift_codegen::ir::FuncRef,
    ctx: Value,
    args_ptr: Value,
    ret_ptr: Value,
    arg_lanes: &[Value; crate::NATIVE_ARG_LANES],
    mode: JitCallGcMode,
) -> Value {
    let emit_call = |emitter: &mut E, attach_roots: bool| {
        let native_roots = attach_roots.then(|| emitter.spill_native_roots()).flatten();
        let mut args = Vec::with_capacity(3 + crate::NATIVE_ARG_LANES);
        args.extend_from_slice(&[ctx, args_ptr, ret_ptr]);
        args.extend_from_slice(arg_lanes);
        let call = emitter.builder().ins().call(jit_func_ref, &args);
        if attach_roots {
            emitter.attach_native_roots(call, native_roots);
            emitter.clear_dead_native_roots();
        }
        emitter.builder().inst_results(call)[0]
    };

    match mode {
        JitCallGcMode::Never => emit_call(emitter, false),
        JitCallGcMode::MayGc => emit_call(emitter, true),
        JitCallGcMode::Dynamic(_) => {
            unreachable!("direct static calls have a compile-time GC contract")
        }
    }
}

/// Load raw argument words from a validated frame window. Callers guarantee
/// that `available_slots` words are addressable; unused lanes are zero-filled.
pub(super) fn load_native_arg_lanes<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    frame_ptr: Value,
    available_slots: usize,
) -> [Value; crate::NATIVE_ARG_LANES] {
    std::array::from_fn(|lane| {
        if lane < available_slots {
            emitter.builder().ins().load(
                types::I64,
                cranelift_codegen::ir::MemFlagsData::trusted(),
                frame_ptr,
                (lane * 8) as i32,
            )
        } else {
            emitter.builder().ins().iconst(types::I64, 0)
        }
    })
}

/// Dynamic counterpart used by prepared calls. Each load is control-dependent
/// on the verified target frame width, so a narrow frame never incurs a
/// speculative out-of-bounds read.
pub(super) fn load_native_arg_lanes_dynamic<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    frame_ptr: Value,
    available_slots: Value,
) -> [Value; crate::NATIVE_ARG_LANES] {
    std::array::from_fn(|lane| {
        let load_block = emitter.builder().create_block();
        let zero_block = emitter.builder().create_block();
        let merge_block = emitter.builder().create_block();
        emitter
            .builder()
            .append_block_param(merge_block, types::I64);
        let in_bounds = emitter.builder().ins().icmp_imm_u(
            IntCC::UnsignedGreaterThan,
            available_slots,
            lane as i64,
        );
        emitter
            .builder()
            .ins()
            .brif(in_bounds, load_block, &[], zero_block, &[]);

        emitter.builder().switch_to_block(load_block);
        emitter.builder().seal_block(load_block);
        let loaded = emitter.builder().ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlagsData::trusted(),
            frame_ptr,
            (lane * 8) as i32,
        );
        let loaded_arg = [loaded.into()];
        emitter.builder().ins().jump(merge_block, &loaded_arg);

        emitter.builder().switch_to_block(zero_block);
        emitter.builder().seal_block(zero_block);
        let zero = emitter.builder().ins().iconst(types::I64, 0);
        let zero_arg = [zero.into()];
        emitter.builder().ins().jump(merge_block, &zero_arg);

        emitter.builder().switch_to_block(merge_block);
        emitter.builder().seal_block(merge_block);
        emitter.builder().block_params(merge_block)[0]
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::{FunctionDef, Module, ResolvedExternTable};
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
            instruction_metadata: Vec::new(),
            slot_types: Vec::new(),
            borrowed_scan_slots_prefix: Vec::new(),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[test]
    fn gc_materialize_call_plan_routes_full_function_call_shapes() {
        let self_plan = CallPlan::new(7, 2, &func(8, false));
        assert_eq!(
            self_plan.route_for_full_function(7),
            CallRoute::DynamicJitTable,
            "self recursion uses the guarded JIT table path once its entry is published"
        );

        let defer_self = CallPlan::new(7, 2, &func(8, true));
        assert_eq!(
            defer_self.route_for_full_function(7),
            CallRoute::VmCallMaterialization
        );

        let large = CallPlan::new(7, 2, &func((MAX_DIRECT_JIT_FRAME_SLOTS + 1) as u16, false));
        assert_eq!(
            large.route_for_full_function(7),
            CallRoute::VmCallMaterialization
        );
        assert_eq!(large.route_for_loop(), CallRoute::VmCallMaterialization);

        let dynamic = CallPlan::new(8, 2, &func(8, false));
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
        let allocating_plan = CallPlan::new(8, 2, &allocating);
        assert_eq!(
            allocating_plan.route_for_full_function(7),
            CallRoute::DynamicJitTable,
            "allocation-only callees use the guarded JIT table; their allocation helpers own the GC safepoint"
        );

        let mut trapping = func(8, false);
        trapping.code = vec![Instruction::new(
            vo_runtime::instruction::Opcode::IndexCheck,
            0,
            1,
            0,
        )];
        let trapping_plan = CallPlan::new(8, 2, &trapping);
        assert_eq!(
            trapping_plan.route_for_full_function(7),
            CallRoute::PreparedJitTable,
            "runtime traps use the prepared shadow route"
        );
    }

    #[test]
    fn static_call_plan_refines_known_nonzero_division_to_frame_elided_route() {
        let mut callee = func(3, false);
        callee.code = vec![
            Instruction::new(vo_runtime::instruction::Opcode::LoadInt, 2, 2, 0),
            Instruction::new(vo_runtime::instruction::Opcode::DivI, 0, 1, 2),
            Instruction::new(vo_runtime::instruction::Opcode::Return, 0, 0, 0),
        ];
        callee.slot_types = vec![vo_runtime::SlotType::Value; 3];
        let mut module = Module::new("known-divisor-route".to_string());
        module.functions.push(callee.clone());
        let externs = ResolvedExternTable::empty();
        let plan = CallPlan::new_in_env(
            0,
            0,
            &callee,
            &module,
            crate::JitCompileEnv {
                externs: &externs,
                backend_caps: Default::default(),
            },
        );

        assert_eq!(plan.route_for_full_function(1), CallRoute::DynamicJitTable);
    }

    #[test]
    fn dynamic_call_plan_uses_authoritative_metadata_counts() {
        let inst =
            Instruction::with_flags(vo_runtime::instruction::Opcode::CallClosure, 0, 4, 10, 0);
        let plan = DynamicCallPlan::new(&inst, 41, 300, 257);
        assert_eq!(plan.arg_start, 10);
        assert_eq!(plan.arg_slots, 300);
        assert_eq!(plan.ret_slots, 257);
        assert_eq!(plan.ret_reg, 310);
        assert_eq!(plan.resume_pc, 42);
    }
}
