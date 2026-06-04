use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags, Value};
use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::{JitResult, JIT_HELPER_U64_ERROR};

use crate::translator::CollectionEmitter;
use crate::JitError;

/// Resolve elem_bytes from instruction flags or per-PC JIT metadata.
/// When flags==0, verifier requires metadata; register constants are not a
/// layout authority.
/// Returns (elem_bytes, needs_sign_extend).
pub(in crate::translate) fn resolve_elem_bytes<'a>(
    e: &impl CollectionEmitter<'a>,
    opcode: Opcode,
    flags: u8,
    eb_reg: u16,
) -> Result<(usize, bool), JitError> {
    let layout = e
        .elem_layout(flags, eb_reg)
        .ok_or(JitError::MissingJitLayout {
            pc: e.current_pc(),
            opcode,
            layout: "ElemLayout",
        })?;
    Ok((layout.bytes, layout.needs_sign_extend))
}

/// Load a single element (1/2/4/8 bytes) from memory address, with optional sign extension.
pub(in crate::translate) fn load_element<'a>(
    e: &mut impl CollectionEmitter<'a>,
    addr: Value,
    elem_bytes: usize,
    needs_sext: bool,
) -> Value {
    match elem_bytes {
        1 => {
            let v = e
                .builder()
                .ins()
                .load(types::I8, MemFlags::trusted(), addr, 0);
            if needs_sext {
                e.builder().ins().sextend(types::I64, v)
            } else {
                e.builder().ins().uextend(types::I64, v)
            }
        }
        2 => {
            let v = e
                .builder()
                .ins()
                .load(types::I16, MemFlags::trusted(), addr, 0);
            if needs_sext {
                e.builder().ins().sextend(types::I64, v)
            } else {
                e.builder().ins().uextend(types::I64, v)
            }
        }
        4 => {
            let v = e
                .builder()
                .ins()
                .load(types::I32, MemFlags::trusted(), addr, 0);
            if needs_sext {
                e.builder().ins().sextend(types::I64, v)
            } else {
                e.builder().ins().uextend(types::I64, v)
            }
        }
        _ => e
            .builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), addr, 0),
    }
}

/// Store a single element (1/2/4/8 bytes) to memory address.
pub(in crate::translate) fn store_element<'a>(
    e: &mut impl CollectionEmitter<'a>,
    addr: Value,
    val: Value,
    elem_bytes: usize,
) {
    match elem_bytes {
        1 => {
            let v = e.builder().ins().ireduce(types::I8, val);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        2 => {
            let v = e.builder().ins().ireduce(types::I16, val);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        4 => {
            let v = e.builder().ins().ireduce(types::I32, val);
            e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
        }
        _ => {
            e.builder().ins().store(MemFlags::trusted(), val, addr, 0);
        }
    }
}

pub(in crate::translate) fn emit_elem_bytes_i32<'a>(
    e: &mut impl CollectionEmitter<'a>,
    opcode: Opcode,
    flags: u8,
    eb_reg: u16,
) -> Result<Value, JitError> {
    let (elem_bytes, _) = resolve_elem_bytes(e, opcode, flags, eb_reg)?;
    Ok(e.builder().ins().iconst(types::I32, elem_bytes as i64))
}

pub(in crate::translate) fn emit_return_if_u64_jit_error<'a>(
    e: &mut impl CollectionEmitter<'a>,
    result: Value,
) {
    let sentinel = e
        .builder()
        .ins()
        .iconst(types::I64, JIT_HELPER_U64_ERROR as i64);
    let is_error = e.builder().ins().icmp(IntCC::Equal, result, sentinel);
    let error_block = e.builder().create_block();
    let ok_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(is_error, error_block, &[], ok_block, &[]);

    e.builder().switch_to_block(error_block);
    e.builder().seal_block(error_block);
    e.spill_all_vars();
    let jit_error = e
        .builder()
        .ins()
        .iconst(types::I32, JitResult::JitError as i64);
    e.builder().ins().return_(&[jit_error]);

    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
}
