//! Shared scalar semantics for typed locals, cached locals, and frame memory.
use super::*;

#[derive(Clone, Copy)]
pub(super) enum ScalarStorage<'a> {
    Typed(TypedFunctionLocals),
    Cached(&'a ScalarLocals),
    Frame,
}

#[derive(Clone, Copy)]
enum ScalarOperand {
    Local(u32),
    Frame(u16),
}

impl ScalarOperand {
    fn load(self, body: &mut Function) {
        match self {
            Self::Local(local) => {
                body.instruction(&W::LocalGet(local));
            }
            Self::Frame(slot) => load_slot(body, slot),
        }
    }
    fn prefix(self, body: &mut Function) {
        if let Self::Frame(slot) = self {
            store_prefix(body, slot);
        }
    }
    fn store(self, body: &mut Function) {
        body.instruction(&match self {
            Self::Local(local) => W::LocalSet(local),
            Self::Frame(_) => W::I64Store(memarg(0)),
        });
    }
}

impl ScalarStorage<'_> {
    fn operand(self, slot: u16) -> Option<ScalarOperand> {
        match self {
            Self::Typed(locals) => Some(ScalarOperand::Local(locals.slot(slot))),
            Self::Cached(locals) => locals.get(slot).map(ScalarOperand::Local),
            Self::Frame => Some(ScalarOperand::Frame(slot)),
        }
    }
}

/// Admission is complete before emission, so cached-local fallback cannot
/// leave partial instructions. Each tier supplies its own unwind publication.
pub(super) fn emit_scalar_arithmetic(
    body: &mut Function,
    instruction: vo_common_core::Instruction,
    storage: ScalarStorage<'_>,
    mut panic_handler: impl FnMut(&mut Function, i32),
) -> bool {
    let opcode = instruction.opcode();
    let binary = match opcode {
        Opcode::NegI
        | Opcode::Not
        | Opcode::BoolNot
        | Opcode::NegF
        | Opcode::NegF32
        | Opcode::ConvI2F
        | Opcode::ConvF2I
        | Opcode::ConvF64F32
        | Opcode::ConvF32F64
        | Opcode::Trunc => false,
        Opcode::AddF
        | Opcode::AddF32
        | Opcode::AddI
        | Opcode::And
        | Opcode::AndNot
        | Opcode::DivF
        | Opcode::DivF32
        | Opcode::DivI
        | Opcode::DivU
        | Opcode::EqF
        | Opcode::EqF32
        | Opcode::EqI
        | Opcode::GeF
        | Opcode::GeF32
        | Opcode::GeI
        | Opcode::GeU
        | Opcode::GtF
        | Opcode::GtF32
        | Opcode::GtI
        | Opcode::GtU
        | Opcode::LeF
        | Opcode::LeF32
        | Opcode::LeI
        | Opcode::LeU
        | Opcode::LtF
        | Opcode::LtF32
        | Opcode::LtI
        | Opcode::LtU
        | Opcode::ModI
        | Opcode::ModU
        | Opcode::MulF
        | Opcode::MulF32
        | Opcode::MulI
        | Opcode::NeF
        | Opcode::NeF32
        | Opcode::NeI
        | Opcode::Or
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU
        | Opcode::SubF
        | Opcode::SubF32
        | Opcode::SubI
        | Opcode::Xor => true,
        _ => return false,
    };
    let (Some(destination), Some(left)) = (
        storage.operand(instruction.a),
        storage.operand(instruction.b),
    ) else {
        return false;
    };
    let right = if binary {
        let Some(right) = storage.operand(instruction.c) else {
            return false;
        };
        Some(right)
    } else {
        None
    };
    let scratch = match destination {
        ScalarOperand::Local(local) => local,
        ScalarOperand::Frame(_) => PACKED_LOCAL,
    };
    destination.prefix(body);
    match opcode {
        Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU => {
            if matches!(opcode, Opcode::Shl | Opcode::ShrS | Opcode::ShrU)
                && instruction.flags & SHIFT_FLAG_RHS_UNSIGNED == 0
            {
                right.expect("binary operand admitted above").load(body);
                body.instruction(&W::I64Const(0))
                    .instruction(&W::I64LtS)
                    .instruction(&W::If(BlockType::Empty));
                panic_handler(body, STATUS_NEGATIVE_SHIFT);
                body.instruction(&W::End);
            }
            if matches!(opcode, Opcode::Shl | Opcode::ShrS | Opcode::ShrU) {
                right.expect("binary operand admitted above").load(body);
                body.instruction(&W::I64Const(64))
                    .instruction(&W::I64GeU)
                    .instruction(&W::If(BlockType::Result(ValType::I64)));
                if opcode == Opcode::ShrS {
                    left.load(body);
                    body.instruction(&W::I64Const(63)).instruction(&W::I64ShrS);
                } else {
                    body.instruction(&W::I64Const(0));
                }
                body.instruction(&W::Else);
                left.load(body);
                right.expect("binary operand admitted above").load(body);
                body.instruction(&match opcode {
                    Opcode::Shl => W::I64Shl,
                    Opcode::ShrS => W::I64ShrS,
                    Opcode::ShrU => W::I64ShrU,
                    _ => unreachable!(),
                })
                .instruction(&W::End);
            } else {
                left.load(body);
                right.expect("binary operand admitted above").load(body);
                if opcode == Opcode::AndNot {
                    body.instruction(&W::I64Const(-1)).instruction(&W::I64Xor);
                }
                body.instruction(&match opcode {
                    Opcode::AddI => W::I64Add,
                    Opcode::SubI => W::I64Sub,
                    Opcode::MulI => W::I64Mul,
                    Opcode::And | Opcode::AndNot => W::I64And,
                    Opcode::Or => W::I64Or,
                    Opcode::Xor => W::I64Xor,
                    _ => unreachable!(),
                });
            }
            destination.store(body);
        }
        Opcode::DivI | Opcode::DivU | Opcode::ModI | Opcode::ModU => {
            right.expect("binary operand admitted above").load(body);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            panic_handler(body, STATUS_DIVISION_BY_ZERO);
            body.instruction(&W::End);
            if opcode == Opcode::DivI {
                left.load(body);
                body.instruction(&W::I64Const(i64::MIN))
                    .instruction(&W::I64Eq);
                right.expect("binary operand admitted above").load(body);
                body.instruction(&W::I64Const(-1))
                    .instruction(&W::I64Eq)
                    .instruction(&W::I32And)
                    .instruction(&W::If(BlockType::Result(ValType::I64)))
                    .instruction(&W::I64Const(i64::MIN))
                    .instruction(&W::Else);
                left.load(body);
                right.expect("binary operand admitted above").load(body);
                body.instruction(&W::I64DivS).instruction(&W::End);
            } else {
                left.load(body);
                right.expect("binary operand admitted above").load(body);
                body.instruction(&match opcode {
                    Opcode::DivU => W::I64DivU,
                    Opcode::ModI => W::I64RemS,
                    Opcode::ModU => W::I64RemU,
                    _ => unreachable!(),
                });
            }
            destination.store(body);
        }
        Opcode::NegI | Opcode::Not | Opcode::BoolNot => {
            if opcode == Opcode::NegI {
                body.instruction(&W::I64Const(0));
                left.load(body);
                body.instruction(&W::I64Sub);
            } else if opcode == Opcode::Not {
                left.load(body);
                body.instruction(&W::I64Const(-1)).instruction(&W::I64Xor);
            } else {
                left.load(body);
                body.instruction(&W::I64Eqz).instruction(&W::I64ExtendI32U);
            }
            destination.store(body);
        }
        Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LeI
        | Opcode::GtI
        | Opcode::GeI
        | Opcode::LtU
        | Opcode::LeU
        | Opcode::GtU
        | Opcode::GeU => {
            left.load(body);
            right.expect("binary operand admitted above").load(body);
            body.instruction(&match opcode {
                Opcode::EqI => W::I64Eq,
                Opcode::NeI => W::I64Ne,
                Opcode::LtI => W::I64LtS,
                Opcode::LeI => W::I64LeS,
                Opcode::GtI => W::I64GtS,
                Opcode::GeI => W::I64GeS,
                Opcode::LtU => W::I64LtU,
                Opcode::LeU => W::I64LeU,
                Opcode::GtU => W::I64GtU,
                Opcode::GeU => W::I64GeU,
                _ => unreachable!(),
            })
            .instruction(&W::I64ExtendI32U);
            destination.store(body);
        }
        Opcode::AddF32
        | Opcode::SubF32
        | Opcode::MulF32
        | Opcode::DivF32
        | Opcode::NegF32
        | Opcode::EqF32
        | Opcode::NeF32
        | Opcode::LtF32
        | Opcode::LeF32
        | Opcode::GtF32
        | Opcode::GeF32 => {
            left.load(body);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::F32ReinterpretI32);
            if let Some(right) = right {
                right.load(body);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::F32ReinterpretI32);
            }
            body.instruction(&match opcode {
                Opcode::AddF32 => W::F32Add,
                Opcode::SubF32 => W::F32Sub,
                Opcode::MulF32 => W::F32Mul,
                Opcode::DivF32 => W::F32Div,
                Opcode::NegF32 => W::F32Neg,
                Opcode::EqF32 => W::F32Eq,
                Opcode::NeF32 => W::F32Ne,
                Opcode::LtF32 => W::F32Lt,
                Opcode::LeF32 => W::F32Le,
                Opcode::GtF32 => W::F32Gt,
                Opcode::GeF32 => W::F32Ge,
                _ => unreachable!(),
            });
            if matches!(
                opcode,
                Opcode::AddF32 | Opcode::SubF32 | Opcode::MulF32 | Opcode::DivF32 | Opcode::NegF32
            ) {
                body.instruction(&W::I32ReinterpretF32);
            }
            body.instruction(&W::I64ExtendI32U);
            destination.store(body);
        }
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF => {
            left.load(body);
            body.instruction(&W::F64ReinterpretI64);
            right.expect("binary operand admitted above").load(body);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&match opcode {
                    Opcode::AddF => W::F64Add,
                    Opcode::SubF => W::F64Sub,
                    Opcode::MulF => W::F64Mul,
                    Opcode::DivF => W::F64Div,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ReinterpretF64);
            destination.store(body);
        }
        Opcode::NegF => {
            left.load(body);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&W::F64Neg)
                .instruction(&W::I64ReinterpretF64);
            destination.store(body);
        }
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF => {
            left.load(body);
            body.instruction(&W::F64ReinterpretI64);
            right.expect("binary operand admitted above").load(body);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&match opcode {
                    Opcode::EqF => W::F64Eq,
                    Opcode::NeF => W::F64Ne,
                    Opcode::LtF => W::F64Lt,
                    Opcode::LeF => W::F64Le,
                    Opcode::GtF => W::F64Gt,
                    Opcode::GeF => W::F64Ge,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ExtendI32U);
            destination.store(body);
        }
        Opcode::ConvI2F => {
            left.load(body);
            if instruction.flags & CONV_FLAG_FLOAT32 != 0 {
                body.instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::F32ConvertI64U
                } else {
                    W::F32ConvertI64S
                })
                .instruction(&W::I32ReinterpretF32)
                .instruction(&W::I64ExtendI32U);
            } else {
                body.instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::F64ConvertI64U
                } else {
                    W::F64ConvertI64S
                })
                .instruction(&W::I64ReinterpretF64);
            }
            destination.store(body);
        }
        Opcode::ConvF2I => {
            left.load(body);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&if instruction.flags & CONV_FLAG_UNSIGNED != 0 {
                    W::I64TruncSatF64U
                } else {
                    W::I64TruncSatF64S
                });
            emit_saturating_integer_width(
                body,
                conv_f2i_width_bits(instruction.flags),
                instruction.flags & CONV_FLAG_UNSIGNED == 0,
                scratch,
            );
            destination.store(body);
        }
        Opcode::ConvF64F32 => {
            left.load(body);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&W::F32DemoteF64)
                .instruction(&W::I32ReinterpretF32)
                .instruction(&W::I64ExtendI32U);
            destination.store(body);
        }
        Opcode::ConvF32F64 => {
            left.load(body);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::F32ReinterpretI32)
                .instruction(&W::F64PromoteF32)
                .instruction(&W::I64ReinterpretF64);
            destination.store(body);
        }
        Opcode::Trunc => {
            left.load(body);
            emit_integer_width(
                body,
                (instruction.flags & 0x7f) * 8,
                instruction.flags & 0x80 != 0,
            );
            destination.store(body);
        }
        _ => unreachable!("scalar opcode admitted above"),
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_encoder::{Encode, MemorySection};

    #[test]
    fn scalar_tiers_validate_with_aliases_and_panic_returns() {
        let cached = ScalarLocals {
            by_slot: vec![Some(12), Some(13), Some(14)],
            count: 3,
        };
        let typed = TypedFunctionLocals {
            param_slots: 0,
            block: 1,
            status: 2,
            address: 3,
            first_non_param_slot: 12,
        };
        let opcodes = [
            Opcode::AddI,
            Opcode::SubI,
            Opcode::MulI,
            Opcode::DivI,
            Opcode::DivU,
            Opcode::ModI,
            Opcode::ModU,
            Opcode::And,
            Opcode::Or,
            Opcode::Xor,
            Opcode::AndNot,
            Opcode::Shl,
            Opcode::ShrS,
            Opcode::ShrU,
            Opcode::NegI,
            Opcode::Not,
            Opcode::BoolNot,
            Opcode::EqI,
            Opcode::NeI,
            Opcode::LtI,
            Opcode::LeI,
            Opcode::GtI,
            Opcode::GeI,
            Opcode::LtU,
            Opcode::LeU,
            Opcode::GtU,
            Opcode::GeU,
            Opcode::AddF32,
            Opcode::SubF32,
            Opcode::MulF32,
            Opcode::DivF32,
            Opcode::NegF32,
            Opcode::EqF32,
            Opcode::NeF32,
            Opcode::LtF32,
            Opcode::LeF32,
            Opcode::GtF32,
            Opcode::GeF32,
            Opcode::AddF,
            Opcode::SubF,
            Opcode::MulF,
            Opcode::DivF,
            Opcode::NegF,
            Opcode::EqF,
            Opcode::NeF,
            Opcode::LtF,
            Opcode::LeF,
            Opcode::GtF,
            Opcode::GeF,
            Opcode::ConvI2F,
            Opcode::ConvF2I,
            Opcode::ConvF64F32,
            Opcode::ConvF32F64,
            Opcode::Trunc,
        ];
        for storage in [
            ScalarStorage::Typed(typed),
            ScalarStorage::Cached(&cached),
            ScalarStorage::Frame,
        ] {
            for opcode in opcodes {
                // A destination may alias either operand. Conversions also use
                // the destination as scratch, and traps return through a stack
                // that can already contain the frame destination address.
                for destination in 0..3 {
                    let flags: &[u8] = match opcode {
                        Opcode::ConvI2F => &[0, 1, 8, 9],
                        Opcode::ConvF2I => &[0, 1, 2, 3, 4, 5, 6, 7],
                        Opcode::Trunc => &[1, 2, 4, 8, 0x81, 0x82, 0x84, 0x88],
                        Opcode::Shl | Opcode::ShrS | Opcode::ShrU => &[0, 1],
                        _ => &[0],
                    };
                    for &flags in flags {
                        let mut instruction =
                            vo_common_core::Instruction::new(opcode, destination, 0, 1);
                        instruction.flags = flags;
                        let mut body = Function::new([(10, ValType::I32), (5, ValType::I64)]);
                        assert!(emit_scalar_arithmetic(
                            &mut body,
                            instruction,
                            storage,
                            |body, status| {
                                body.instruction(&W::I64Const(i64::from(status)))
                                    .instruction(&W::Return);
                            }
                        ));
                        storage.operand(destination).unwrap().load(&mut body);
                        body.instruction(&W::End);
                        let mut types = TypeSection::new();
                        types.ty().function([], [ValType::I64]);
                        let mut functions = FunctionSection::new();
                        functions.function(0);
                        let mut memories = MemorySection::new();
                        memories.memory(MemoryType {
                            minimum: 1,
                            maximum: Some(1),
                            memory64: false,
                            shared: false,
                            page_size_log2: None,
                        });
                        let mut code = CodeSection::new();
                        code.function(&body);
                        let mut module = Module::new();
                        module
                            .section(&types)
                            .section(&functions)
                            .section(&memories)
                            .section(&code);
                        wasmparser::Validator::new()
                            .validate_all(&module.finish())
                            .unwrap_or_else(|error| {
                                panic!(
                                    "{opcode:?}, destination={destination}, flags={flags}: {error}"
                                )
                            });
                    }
                }
            }
        }
    }

    #[test]
    fn cached_scalar_fallback_does_not_emit_partial_wasm() {
        for missing in 0..3 {
            let mut cached = ScalarLocals {
                by_slot: vec![Some(12), Some(13), Some(14)],
                count: 2,
            };
            cached.by_slot[missing] = None;
            let mut body = Function::new([]);
            body.instruction(&W::Nop);
            let mut before = Vec::new();
            body.encode(&mut before);
            assert!(!emit_scalar_arithmetic(
                &mut body,
                vo_common_core::Instruction::new(Opcode::AddI, 2, 0, 1),
                ScalarStorage::Cached(&cached),
                |_, _| panic!("fallback cannot emit a trap")
            ));
            let mut after = Vec::new();
            body.encode(&mut after);
            assert_eq!(before, after);
        }
    }
}
