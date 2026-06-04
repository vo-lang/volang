use cranelift_codegen::ir::{types, InstBuilder, MemFlags};
use vo_runtime::instruction::Instruction;

use crate::translate::require_helper;
use crate::translator::{emit_funcref_call, RuntimeOpsEmitter};
use crate::JitError;

pub(in crate::translate) fn closure_new<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().closure_new, "closure_new")?;
    let gc_ptr = e.gc_ptr();
    let func_id = inst.closure_new_func_id();
    let capture_count = inst.c as u32;
    let func_id_i32 = e.builder().ins().iconst(types::I32, func_id as i64);
    let capture_count_i32 = e.builder().ins().iconst(types::I32, capture_count as i64);
    let call = emit_funcref_call(e, func, &[gc_ptr, func_id_i32, capture_count_i32]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn closure_get<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) {
    use vo_runtime::objects::closure::HEADER_SLOTS;
    let closure = e.read_var(0);
    let capture_idx = inst.b as usize;
    let offset = ((HEADER_SLOTS + capture_idx) * 8) as i32;
    let val = e
        .builder()
        .ins()
        .load(types::I64, MemFlags::trusted(), closure, offset);
    e.write_var(inst.a, val);
}
