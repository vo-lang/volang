//! Binary32 operations on the portable slot bit representation.
use crate::translator::ScalarEmitter;
use cranelift_codegen::ir::condcodes::FloatCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlagsData as MemFlags, Value};
use cranelift_frontend::FunctionBuilder;
use vo_runtime::instruction::{Instruction, Opcode};

fn unpack(builder: &mut FunctionBuilder<'_>, bits: Value) -> Value {
    let bits = builder.ins().ireduce(types::I32, bits);
    builder.ins().bitcast(types::F32, MemFlags::new(), bits)
}

/// Emit one operation, preserving its binary32 rounding boundary. Shared with
/// leaf recipes so inlining uses the same width and unordered comparisons.
pub(crate) fn emit_float32_bits(
    builder: &mut FunctionBuilder<'_>,
    opcode: Opcode,
    left: Value,
    right: Option<Value>,
) -> Value {
    let left = unpack(builder, left);
    let right = right.map(|value| unpack(builder, value));
    let value = match opcode {
        Opcode::AddF32 => builder.ins().fadd(left, right.unwrap()),
        Opcode::SubF32 => builder.ins().fsub(left, right.unwrap()),
        Opcode::MulF32 => builder.ins().fmul(left, right.unwrap()),
        Opcode::DivF32 => builder.ins().fdiv(left, right.unwrap()),
        Opcode::NegF32 => builder.ins().fneg(left),
        Opcode::EqF32
        | Opcode::NeF32
        | Opcode::LtF32
        | Opcode::LeF32
        | Opcode::GtF32
        | Opcode::GeF32 => {
            let cc = match opcode {
                Opcode::EqF32 => FloatCC::Equal,
                Opcode::NeF32 => FloatCC::NotEqual,
                Opcode::LtF32 => FloatCC::LessThan,
                Opcode::LeF32 => FloatCC::LessThanOrEqual,
                Opcode::GtF32 => FloatCC::GreaterThan,
                Opcode::GeF32 => FloatCC::GreaterThanOrEqual,
                _ => unreachable!(),
            };
            let compared = builder.ins().fcmp(cc, left, right.unwrap());
            return builder.ins().uextend(types::I64, compared);
        }
        _ => unreachable!("binary32 opcode admitted by the caller"),
    };
    let bits = builder.ins().bitcast(types::I32, MemFlags::new(), value);
    builder.ins().uextend(types::I64, bits)
}

pub(super) fn float32<'a>(emitter: &mut impl ScalarEmitter<'a>, instruction: &Instruction) {
    let left = emitter.read_var(instruction.b);
    let right = (instruction.opcode() != Opcode::NegF32).then(|| emitter.read_var(instruction.c));
    let value = emit_float32_bits(emitter.builder(), instruction.opcode(), left, right);
    emitter.write_var(instruction.a, value);
}
