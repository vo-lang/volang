use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, Value};

/// Emit ForLoop step: idx += step; return (next_idx, continue_condition)
///
/// Shared by FuncCompiler and LoopCompiler.
/// flags: bit0=unsigned, bit1=decrement, bit2=inclusive
pub fn emit_forloop_step(
    builder: &mut cranelift_frontend::FunctionBuilder,
    idx: Value,
    limit: Value,
    is_decrement: bool,
    is_unsigned: bool,
    is_inclusive: bool,
) -> (Value, Value) {
    let one = builder.ins().iconst(types::I64, 1);
    let next_idx = if is_decrement {
        builder.ins().isub(idx, one)
    } else {
        builder.ins().iadd(idx, one)
    };

    let cc = match (is_decrement, is_unsigned, is_inclusive) {
        // Increment exclusive: i < limit
        (false, false, false) => IntCC::SignedLessThan,
        (false, true, false) => IntCC::UnsignedLessThan,
        // Increment inclusive: i <= limit
        (false, false, true) => IntCC::SignedLessThanOrEqual,
        (false, true, true) => IntCC::UnsignedLessThanOrEqual,
        // Decrement exclusive: i > limit
        (true, false, false) => IntCC::SignedGreaterThan,
        (true, true, false) => IntCC::UnsignedGreaterThan,
        // Decrement inclusive: i >= limit
        (true, false, true) => IntCC::SignedGreaterThanOrEqual,
        (true, true, true) => IntCC::UnsignedGreaterThanOrEqual,
    };
    let continue_loop = builder.ins().icmp(cc, next_idx, limit);

    (next_idx, continue_loop)
}
