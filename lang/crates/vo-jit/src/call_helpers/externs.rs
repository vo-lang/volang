use cranelift_codegen::ir::{types, InstBuilder};

use vo_runtime::bytecode::{ExternJitRoute, InstructionMetadata};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitContextField;

use crate::intrinsics;
use crate::translator::{HelperKind, IrEmitter, NativeScratchKind};

use super::{emit_checked_jit_result_helper_call, JIT_RESULT_REPLAY};

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
) -> Result<bool, crate::JitError> {
    let dst = inst.a as usize;
    let extern_id = inst.b as u32;
    let arg_start = inst.c as usize;
    let (arg_count, callsite_ret_slots) = match emitter.current_instruction_metadata() {
        Some(InstructionMetadata::CallExternLayout {
            arg_layout,
            ret_layout,
        }) => (arg_layout.len(), ret_layout.len()),
        _ => {
            return Err(crate::JitError::Internal(format!(
                "CallExtern missing authoritative metadata at pc {}",
                emitter.current_pc()
            )))
        }
    };
    let resolved = emitter.resolved_extern(extern_id)?;
    let jit_route = resolved.jit_route;
    let resolved_name = resolved.name.clone();
    let arg_slots_u16 = u16::try_from(arg_count).map_err(|_| {
        crate::JitError::Internal(format!(
            "CallExtern arg slot count {arg_count} exceeds u16 ABI range"
        ))
    })?;
    if !resolved.params.accepts_slots(arg_slots_u16) {
        return Err(crate::JitError::Internal(format!(
            "CallExtern arg slot count {arg_count} does not match resolved extern '{}' params {}",
            resolved.name,
            resolved.params.display_name()
        )));
    }
    if callsite_ret_slots != usize::from(resolved.returns.slots) {
        return Err(crate::JitError::Internal(format!(
            "CallExtern return slot count {callsite_ret_slots} does not match resolved extern '{}' returns {}",
            resolved.name, resolved.returns.slots
        )));
    }

    let _extern_def = emitter
        .vo_module()
        .externs
        .get(extern_id as usize)
        .ok_or_else(|| {
            crate::JitError::Internal(format!("CallExtern missing extern {extern_id}"))
        })?;
    let extern_ret_slots = callsite_ret_slots;

    if matches!(jit_route, ExternJitRoute::Intrinsic) {
        intrinsics::emit_resolved_intrinsic(emitter, inst, &resolved_name)?;
        return Ok(false);
    }

    let call_extern_func = emitter.helper(HelperKind::call_extern);

    let current_pc_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.current_pc as i64);
    if matches!(jit_route, ExternJitRoute::VmMaterializeBeforeCall) {
        emitter.store_context_field(current_pc_val, JitContextField::CallResumePc);
        emitter.spill_all_vars();
        let replay_result = emitter
            .builder()
            .ins()
            .iconst(types::I32, JIT_RESULT_REPLAY as i64);
        emitter.builder().ins().return_(&[replay_result]);
        return Ok(true);
    }

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

    let args_slot =
        emitter.native_scratch_slot(NativeScratchKind::ExternArgs, arg_count.max(1) * 8);
    let ret_slot = emitter.native_scratch_slot(
        NativeScratchKind::ExternReturns,
        extern_ret_slots.max(1) * 8,
    );

    for i in 0..arg_count {
        let val = emitter.read_var((arg_start + i) as u16);
        emitter
            .builder()
            .ins()
            .stack_store(types::I64, val, args_slot, (i * 8) as i32);
    }

    let ctx = emitter.ctx_param();
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, args_slot, 0);
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);
    let extern_id_val = emitter.builder().ins().iconst(types::I32, extern_id as i64);
    let arg_count_val = emitter.builder().ins().iconst(types::I32, arg_count as i64);
    let ret_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, extern_ret_slots as i64);

    // Publish the current pc before the helper call. Direct-helper extern
    // suspends travel through VM-owned ExternSuspend payloads; materialize-only
    // routes use Replay before calling the extern.
    emitter.store_context_field(current_pc_val, JitContextField::CallResumePc);
    // Extern panics are user panics at the CallExtern bytecode pc. Publish the
    // panic location through user_panic_pc; call_resume_pc is only a resume ABI.
    emitter.store_context_field(current_pc_val, JitContextField::UserPanicPc);

    emit_checked_jit_result_helper_call(
        emitter,
        call_extern_func,
        &[
            ctx,
            extern_id_val,
            args_ptr,
            arg_count_val,
            ret_ptr,
            ret_slots_val,
        ],
    );
    emitter.refresh_stack_base_after_reallocation();

    let no_user_panic_pc = emitter.builder().ins().iconst(types::I32, -1);
    emitter.store_context_field(no_user_panic_pc, JitContextField::UserPanicPc);

    for i in 0..copy_back_slots {
        let val =
            emitter
                .builder()
                .ins()
                .stack_load(types::I64, types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((dst + i) as u16, val);
    }
    Ok(false)
}
