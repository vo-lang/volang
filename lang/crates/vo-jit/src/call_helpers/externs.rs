use cranelift_codegen::ir::{types, InstBuilder, MemFlags, StackSlotData, StackSlotKind};

use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitContext;

use crate::intrinsics;
use crate::translator::IrEmitter;

use super::emit_checked_jit_result_helper_call;

/// Configuration for extern call emission.
pub struct CallExternConfig {
    /// Current PC (for resume_pc setting).
    pub current_pc: usize,
}

/// Emit an extern function call instruction.
///
/// Both FunctionCompiler and LoopCompiler should use this to ensure consistent behavior.
pub fn emit_call_extern<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
    config: CallExternConfig,
) -> Result<(), crate::JitError> {
    // Fast path: emit intrinsic instruction directly, skip FFI entirely.
    if intrinsics::try_emit_for_extern(emitter, inst) {
        return Ok(());
    }

    let call_extern_func =
        crate::translate::require_helper(emitter.helpers().call_extern, "call_extern")?;

    let dst = inst.a as usize;
    let extern_id = inst.b as u32;
    let arg_start = inst.c as usize;
    let arg_count = inst.flags as usize;

    let extern_def = emitter
        .vo_module()
        .externs
        .get(extern_id as usize)
        .ok_or_else(|| {
            crate::JitError::Internal(format!("CallExtern missing extern {extern_id}"))
        })?;
    let extern_ret_slots = extern_def.ret_slots as usize;
    let buffer_size = arg_count.max(extern_ret_slots).max(1);

    let copy_back_slots = if extern_ret_slots == 0 {
        0
    } else {
        let available_vars = emitter.local_slot_count().checked_sub(dst).ok_or_else(|| {
            crate::JitError::Internal(format!(
                "CallExtern destination slot {dst} is outside local slots"
            ))
        })?;
        if extern_ret_slots > available_vars {
            return Err(crate::JitError::Internal(format!(
                "CallExtern return slots exceed local slots: ret_slots={extern_ret_slots}, available={available_vars}"
            )));
        }
        extern_ret_slots
    };

    let builder = emitter.builder();
    let slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (buffer_size * 8) as u32,
        8,
    ));

    for i in 0..arg_count {
        let val = emitter.read_var((arg_start + i) as u16);
        emitter
            .builder()
            .ins()
            .stack_store(val, slot, (i * 8) as i32);
    }

    let ctx = emitter.ctx_param();
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, slot, 0);
    let extern_id_val = emitter.builder().ins().iconst(types::I32, extern_id as i64);
    let arg_count_val = emitter.builder().ins().iconst(types::I32, arg_count as i64);
    let ret_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, extern_ret_slots as i64);

    // Set resume_pc before the call so VM can re-execute CallExtern on exit.
    // Any extern can return Replay (CallClosure) or WaitIo, both need resume_pc.
    let resume_pc_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.current_pc as i64);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContext::OFFSET_CALL_RESUME_PC,
    );
    // Extern panics are user panics at the CallExtern bytecode pc. Publish the
    // panic location through user_panic_pc; call_resume_pc is only a resume ABI.
    emitter.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContext::OFFSET_USER_PANIC_PC,
    );

    emit_checked_jit_result_helper_call(
        emitter,
        call_extern_func,
        &[
            ctx,
            extern_id_val,
            args_ptr,
            arg_count_val,
            args_ptr,
            ret_slots_val,
        ],
        true,
    );

    let no_user_panic_pc = emitter.builder().ins().iconst(types::I32, -1);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        no_user_panic_pc,
        ctx,
        JitContext::OFFSET_USER_PANIC_PC,
    );

    for i in 0..copy_back_slots {
        let val = emitter
            .builder()
            .ins()
            .stack_load(types::I64, slot, (i * 8) as i32);
        emitter.write_var((dst + i) as u16, val);
    }
    Ok(())
}
