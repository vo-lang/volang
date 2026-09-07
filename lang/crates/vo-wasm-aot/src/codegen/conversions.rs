//! Shared integer-width conversion and saturation rules.
use super::*;

pub(super) fn emit_integer_width(body: &mut Function, bits: u8, signed: bool) {
    match (bits, signed) {
        (8, true) => {
            body.instruction(&W::I64Extend8S);
        }
        (16, true) => {
            body.instruction(&W::I64Extend16S);
        }
        (32, true) => {
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64ExtendI32S);
        }
        (8, false) => {
            body.instruction(&W::I64Const(0xff)).instruction(&W::I64And);
        }
        (16, false) => {
            body.instruction(&W::I64Const(0xffff))
                .instruction(&W::I64And);
        }
        (32, false) => {
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64ExtendI32U);
        }
        _ => {}
    }
}

/// Clamp a saturating f64-to-i64 conversion to the final language integer
/// width. Narrow Rust casts saturate at the narrow type's bounds; truncating
/// the already-saturated i64 would wrap and change that contract.
pub(super) fn emit_saturating_integer_width(
    body: &mut Function,
    bits: u8,
    signed: bool,
    temp: u32,
) {
    if bits == 64 {
        return;
    }
    body.instruction(&W::LocalSet(temp));
    if signed {
        let (minimum, maximum) = match bits {
            8 => (i64::from(i8::MIN), i64::from(i8::MAX)),
            16 => (i64::from(i16::MIN), i64::from(i16::MAX)),
            32 => (i64::from(i32::MIN), i64::from(i32::MAX)),
            _ => unreachable!("verified ConvF2I signed width"),
        };
        body.instruction(&W::LocalGet(temp))
            .instruction(&W::I64Const(minimum))
            .instruction(&W::I64LtS)
            .instruction(&W::If(BlockType::Result(ValType::I64)))
            .instruction(&W::I64Const(minimum))
            .instruction(&W::Else)
            .instruction(&W::LocalGet(temp))
            .instruction(&W::I64Const(maximum))
            .instruction(&W::I64GtS)
            .instruction(&W::If(BlockType::Result(ValType::I64)))
            .instruction(&W::I64Const(maximum))
            .instruction(&W::Else)
            .instruction(&W::LocalGet(temp))
            .instruction(&W::End)
            .instruction(&W::End);
    } else {
        let maximum = match bits {
            8 => u64::from(u8::MAX),
            16 => u64::from(u16::MAX),
            32 => u64::from(u32::MAX),
            _ => unreachable!("verified ConvF2I unsigned width"),
        };
        body.instruction(&W::LocalGet(temp))
            .instruction(&W::I64Const(maximum as i64))
            .instruction(&W::I64GtU)
            .instruction(&W::If(BlockType::Result(ValType::I64)))
            .instruction(&W::I64Const(maximum as i64))
            .instruction(&W::Else)
            .instruction(&W::LocalGet(temp))
            .instruction(&W::End);
    }
}
