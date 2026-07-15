use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder};
use vo_runtime::bytecode::JitInstructionMetadata;
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::call_helpers::emit_checked_jit_result_helper_call;
use crate::translate::{emit_runtime_trap_if, mark_runtime_trap_pc, require_helper};
use crate::translator::RuntimeOpsEmitter;
use crate::JitError;

pub(in crate::translate) fn island_new<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let island_new_func = require_helper(e.helpers().island_new, "island_new")?;
    let ctx = e.ctx_param();
    let out_ptr = e.var_addr(inst.a);

    emit_checked_jit_result_helper_call(e, island_new_func, &[ctx, out_ptr], true);
    e.sync_written_slots(inst.a, 1)?;
    Ok(())
}

pub(in crate::translate) fn go_start<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let go_start_func = require_helper(e.helpers().go_start, "go_start")?;
    let ctx = e.ctx_param();
    let is_closure_call = inst.call_shape_is_closure();
    let func_id = if is_closure_call {
        0
    } else {
        inst.call_shape_static_func_id()
    };
    let func_id_val = e.builder().ins().iconst(types::I32, func_id as i64);
    let is_closure_val = e
        .builder()
        .ins()
        .iconst(types::I32, if is_closure_call { 1 } else { 0 });
    let closure_ref = if is_closure_call {
        let closure_ref = e.read_var(inst.a);
        crate::contract::emit_nil_func_trap_if(e, closure_ref);
        closure_ref
    } else {
        e.builder().ins().iconst(types::I64, 0)
    };
    let args_ptr = e.var_addr(inst.b);
    let arg_slots = e.builder().ins().iconst(types::I32, inst.c as i64);
    mark_runtime_trap_pc(e);
    emit_checked_jit_result_helper_call(
        e,
        go_start_func,
        &[
            ctx,
            func_id_val,
            is_closure_val,
            closure_ref,
            args_ptr,
            arg_slots,
        ],
        true,
    );
    Ok(())
}

pub(in crate::translate) fn go_island<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let arg_slots = match e.current_jit_metadata() {
        Some(JitInstructionMetadata::CallLayout {
            arg_layout,
            ret_layout,
        }) if ret_layout.is_empty() => arg_layout.len(),
        _ => {
            return Err(JitError::Internal(format!(
                "GoIsland missing authoritative argument metadata at pc {}",
                e.current_pc()
            )))
        }
    };
    let arg_slots = u32::try_from(arg_slots)
        .map_err(|_| JitError::Internal("GoIsland argument layout exceeds u32".to_string()))?;
    let go_island_func = require_helper(e.helpers().go_island, "go_island")?;
    let ctx = e.ctx_param();
    let island = e.read_var(inst.a);
    let zero = e.builder().ins().iconst(types::I64, 0);
    let island_is_nil = e.builder().ins().icmp(IntCC::Equal, island, zero);
    emit_runtime_trap_if(
        e,
        island_is_nil,
        JitRuntimeTrapKind::NilPointerDereference,
        None,
        None,
    );
    let closure_ref = e.read_var(inst.b);
    crate::contract::emit_nil_func_trap_if(e, closure_ref);
    let args_ptr = e.var_addr(inst.c);
    let arg_slots = e.builder().ins().iconst(types::I32, i64::from(arg_slots));
    mark_runtime_trap_pc(e);
    emit_checked_jit_result_helper_call(
        e,
        go_island_func,
        &[ctx, island, closure_ref, args_ptr, arg_slots],
        true,
    );
    Ok(())
}

pub(in crate::translate) fn defer_push<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
    is_errdefer: bool,
) -> Result<(), JitError> {
    let defer_push_func = require_helper(e.helpers().defer_push, "defer_push")?;
    let ctx = e.ctx_param();
    let is_closure_call = inst.call_shape_is_closure();
    let func_id = if is_closure_call {
        0
    } else {
        inst.call_shape_static_func_id()
    };
    let func_id_val = e.builder().ins().iconst(types::I32, func_id as i64);
    let is_closure_val = e
        .builder()
        .ins()
        .iconst(types::I32, if is_closure_call { 1 } else { 0 });
    let closure_ref = if is_closure_call {
        e.read_var(inst.a)
    } else {
        e.builder().ins().iconst(types::I64, 0)
    };
    let arg_start_val = e.builder().ins().iconst(types::I32, inst.b as i64);
    let args_ptr = e.var_addr(inst.b);
    let arg_count = e.builder().ins().iconst(types::I32, inst.c as i64);
    let is_errdefer_val = e
        .builder()
        .ins()
        .iconst(types::I32, if is_errdefer { 1 } else { 0 });
    emit_checked_jit_result_helper_call(
        e,
        defer_push_func,
        &[
            ctx,
            func_id_val,
            is_closure_val,
            closure_ref,
            arg_start_val,
            args_ptr,
            arg_count,
            is_errdefer_val,
        ],
        true,
    );
    Ok(())
}

pub(in crate::translate) fn recover<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let recover_func = require_helper(e.helpers().recover, "recover")?;
    let ctx = e.ctx_param();
    let result_ptr = e.var_addr(inst.a);
    emit_checked_jit_result_helper_call(e, recover_func, &[ctx, result_ptr], true);
    e.sync_written_slots(inst.a, 2)?;
    Ok(())
}
