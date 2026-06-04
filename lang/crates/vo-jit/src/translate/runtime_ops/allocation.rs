use cranelift_codegen::ir::{types, InstBuilder, StackSlotData, StackSlotKind};
use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::Instruction;

use crate::translate::require_helper;
use crate::translator::{emit_funcref_call, RuntimeOpsEmitter};
use crate::JitError;

pub(in crate::translate) fn ptr_new<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().gc_alloc, "gc_alloc")?;
    let gc_ptr = e.gc_ptr();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let slots_i32 = e.builder().ins().iconst(types::I32, inst.c as i64);
    let call = emit_funcref_call(e, func, &[gc_ptr, meta_i32, slots_i32]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn str_new<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_new, "str_new")?;
    let const_idx = inst.b as usize;
    let bytes: Vec<u8> = match e.vo_module().constants.get(const_idx) {
        Some(Constant::String(s)) => s.as_bytes().to_vec(),
        Some(other) => {
            return Err(JitError::Internal(format!(
                "StrNew constant at pc {} must be String, got {other:?}",
                e.current_pc()
            )));
        }
        None => {
            return Err(JitError::Internal(format!(
                "StrNew constant index {const_idx} missing at pc {}",
                e.current_pc()
            )));
        }
    };
    let len = bytes.len();
    if len == 0 {
        let zero = e.builder().ins().iconst(types::I64, 0);
        e.write_var(inst.a, zero);
    } else {
        let gc_ptr = e.gc_ptr();
        let stack_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            len as u32,
            0,
        ));
        for (i, &b) in bytes.iter().enumerate() {
            let byte_val = e.builder().ins().iconst(types::I8, b as i64);
            e.builder()
                .ins()
                .stack_store(byte_val, stack_slot, i as i32);
        }
        let data_ptr = e.builder().ins().stack_addr(types::I64, stack_slot, 0);
        let len_val = e.builder().ins().iconst(types::I64, len as i64);
        let call = emit_funcref_call(e, func, &[gc_ptr, data_ptr, len_val]);
        let result = e.builder().inst_results(call)[0];
        e.write_var(inst.a, result);
    }
    Ok(())
}
