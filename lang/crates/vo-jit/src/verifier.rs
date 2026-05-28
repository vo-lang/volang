//! Verifier for bytecode facts consumed by the JIT.

use std::fmt;

use vo_common_core::instruction::HINT_LOOP;
use vo_runtime::bytecode::{FunctionDef, JitInstructionMetadata, Module as VoModule};
use vo_runtime::instruction::Opcode;

use crate::effects::{EffectError, SlotRangeError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JitMetadataError {
    LengthMismatch {
        func: String,
        code_len: usize,
        metadata_len: usize,
    },
    InvalidOpcode {
        func: String,
        pc: usize,
        raw: u8,
    },
    WrongMetadataKind {
        func: String,
        pc: usize,
        opcode: Opcode,
        metadata: &'static str,
    },
    InvalidElemLayout {
        func: String,
        pc: usize,
        elem_bytes: u32,
    },
    InconsistentElemLayout {
        func: String,
        pc: usize,
        flags: u8,
    },
    MissingLayout {
        func: String,
        pc: usize,
        opcode: Opcode,
        layout: &'static str,
    },
    MissingExtern {
        func: String,
        pc: usize,
        extern_id: u16,
    },
    MissingLoopEnd {
        func: String,
        pc: usize,
    },
    InvalidLoopEnd {
        func: String,
        pc: usize,
        begin_pc: usize,
        end_pc: usize,
        code_len: usize,
    },
    InconsistentLoopEnd {
        func: String,
        pc: usize,
        encoded_end_pc: usize,
        metadata_end_pc: usize,
    },
    InvalidLoopEndBackEdge {
        func: String,
        pc: usize,
        begin_pc: usize,
        end_pc: usize,
    },
    SlotRangeOverflow {
        func: String,
        pc: usize,
        start: u16,
        count: u16,
        access: &'static str,
    },
    SlotOutOfRange {
        func: String,
        pc: usize,
        slot: u16,
        local_slots: u16,
        access: &'static str,
    },
}

impl fmt::Display for JitMetadataError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LengthMismatch {
                func,
                code_len,
                metadata_len,
            } => write!(
                f,
                "JIT metadata length mismatch in {func}: code={code_len}, metadata={metadata_len}"
            ),
            Self::InvalidOpcode { func, pc, raw } => {
                write!(f, "invalid opcode {raw} in {func} at pc {pc}")
            }
            Self::WrongMetadataKind {
                func,
                pc,
                opcode,
                metadata,
            } => write!(
                f,
                "wrong JIT metadata kind {metadata} for {opcode:?} in {func} at pc {pc}"
            ),
            Self::InvalidElemLayout {
                func,
                pc,
                elem_bytes,
            } => write!(
                f,
                "invalid JIT elem layout in {func} at pc {pc}: elem_bytes={elem_bytes}"
            ),
            Self::InconsistentElemLayout { func, pc, flags } => write!(
                f,
                "JIT elem layout does not match bytecode flags 0x{flags:02x} in {func} at pc {pc}"
            ),
            Self::MissingLayout {
                func,
                pc,
                opcode,
                layout,
            } => write!(
                f,
                "missing JIT {layout} layout for {opcode:?} in {func} at pc {pc}"
            ),
            Self::MissingExtern {
                func,
                pc,
                extern_id,
            } => write!(
                f,
                "JIT CallExtern references missing extern {extern_id} in {func} at pc {pc}"
            ),
            Self::MissingLoopEnd { func, pc } => {
                write!(f, "missing JIT LoopEnd metadata for HINT_LOOP in {func} at pc {pc}")
            }
            Self::InvalidLoopEnd {
                func,
                pc,
                begin_pc,
                end_pc,
                code_len,
            } => write!(
                f,
                "invalid JIT LoopEnd in {func} at pc {pc}: begin_pc={begin_pc}, end_pc={end_pc}, code_len={code_len}"
            ),
            Self::InconsistentLoopEnd {
                func,
                pc,
                encoded_end_pc,
                metadata_end_pc,
            } => write!(
                f,
                "JIT LoopEnd metadata does not match HINT_LOOP offset in {func} at pc {pc}: encoded end_pc={encoded_end_pc}, metadata end_pc={metadata_end_pc}"
            ),
            Self::InvalidLoopEndBackEdge {
                func,
                pc,
                begin_pc,
                end_pc,
            } => write!(
                f,
                "JIT LoopEnd in {func} at pc {pc} points at end_pc={end_pc}, which is not a back-edge to begin_pc={begin_pc}"
            ),
            Self::SlotRangeOverflow {
                func,
                pc,
                start,
                count,
                access,
            } => write!(
                f,
                "JIT {access} slot range starting at {start} with {count} slots overflows u16 in {func} at pc {pc}"
            ),
            Self::SlotOutOfRange {
                func,
                pc,
                slot,
                local_slots,
                access,
            } => write!(
                f,
                "JIT {access} slot {slot} out of range for {func} at pc {pc} (local_slots={local_slots})"
            ),
        }
    }
}

impl std::error::Error for JitMetadataError {}

impl JitMetadataError {
    pub(crate) fn slot_range(func: &FunctionDef, pc: usize, err: SlotRangeError) -> Self {
        Self::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: err.start,
            count: err.count,
            access: err.access,
        }
    }

    pub(crate) fn effect(func: &FunctionDef, pc: usize, err: EffectError) -> Self {
        match err {
            EffectError::SlotRange(err) => Self::slot_range(func, pc, err),
            EffectError::MissingLayout { opcode, layout } => Self::MissingLayout {
                func: func.name.clone(),
                pc,
                opcode,
                layout,
            },
            EffectError::MissingExtern { extern_id } => Self::MissingExtern {
                func: func.name.clone(),
                pc,
                extern_id,
            },
        }
    }
}

pub fn verify_jit_metadata(
    func: &FunctionDef,
    vo_module: &VoModule,
) -> Result<(), JitMetadataError> {
    if func.code.len() != func.jit_metadata.len() {
        return Err(JitMetadataError::LengthMismatch {
            func: func.name.clone(),
            code_len: func.code.len(),
            metadata_len: func.jit_metadata.len(),
        });
    }

    for (pc, inst) in func.code.iter().enumerate() {
        let opcode = inst.opcode();
        if opcode == Opcode::Invalid {
            return Err(JitMetadataError::InvalidOpcode {
                func: func.name.clone(),
                pc,
                raw: inst.op,
            });
        }
        verify_metadata_kind(
            func,
            pc,
            opcode,
            func.code[pc].flags,
            &func.jit_metadata[pc],
        )?;
    }

    let analysis = crate::analysis::FunctionAnalysis::for_function(func, vo_module)?;
    for (pc, effects) in analysis.effects.iter().enumerate() {
        for &slot in &effects.reads {
            verify_slot(func, pc, slot, "read")?;
        }
        for &slot in &effects.writes {
            verify_slot(func, pc, slot, "write")?;
        }
    }

    Ok(())
}

fn verify_metadata_kind(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    flags: u8,
    metadata: &JitInstructionMetadata,
) -> Result<(), JitMetadataError> {
    match *metadata {
        JitInstructionMetadata::None => {
            if let Some(layout) = required_layout(opcode, flags) {
                return Err(JitMetadataError::MissingLayout {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    layout,
                });
            }
            if opcode == Opcode::Hint && flags == HINT_LOOP {
                let end_offset = loop_end_offset(func.code[pc]);
                if end_offset == 0 {
                    return Err(JitMetadataError::MissingLoopEnd {
                        func: func.name.clone(),
                        pc,
                    });
                }
                validate_loop_end_backedge(func, pc, pc + end_offset)?;
            }
            Ok(())
        }
        JitInstructionMetadata::ElemLayout { elem_bytes, .. } => {
            if !matches!(
                opcode,
                Opcode::ArrayNew
                    | Opcode::ArrayGet
                    | Opcode::ArraySet
                    | Opcode::ArrayAddr
                    | Opcode::SliceNew
                    | Opcode::SliceGet
                    | Opcode::SliceSet
                    | Opcode::SliceAddr
                    | Opcode::SliceAppend
            ) {
                return Err(wrong_kind(func, pc, opcode, "ElemLayout"));
            }
            let Some(layout) = crate::metadata::elem_layout_from_instruction(metadata) else {
                return Err(JitMetadataError::InvalidElemLayout {
                    func: func.name.clone(),
                    pc,
                    elem_bytes,
                });
            };
            if func.code[pc].flags != 0 {
                let from_flags = crate::metadata::elem_layout_from_flags(func.code[pc].flags);
                if from_flags != layout {
                    return Err(JitMetadataError::InconsistentElemLayout {
                        func: func.name.clone(),
                        pc,
                        flags: func.code[pc].flags,
                    });
                }
            }
            Ok(())
        }
        JitInstructionMetadata::MapGet { .. } => (opcode == Opcode::MapGet)
            .then_some(())
            .ok_or_else(|| wrong_kind(func, pc, opcode, "MapGet")),
        JitInstructionMetadata::MapSet { .. } => (opcode == Opcode::MapSet)
            .then_some(())
            .ok_or_else(|| wrong_kind(func, pc, opcode, "MapSet")),
        JitInstructionMetadata::MapDelete { .. } => (opcode == Opcode::MapDelete)
            .then_some(())
            .ok_or_else(|| wrong_kind(func, pc, opcode, "MapDelete")),
        JitInstructionMetadata::LoopEnd { end_pc } => {
            if opcode != Opcode::Hint || flags != HINT_LOOP {
                return Err(wrong_kind(func, pc, opcode, "LoopEnd"));
            }
            let end_pc = end_pc as usize;
            let begin_pc = pc + 1;
            if begin_pc >= func.code.len() || end_pc >= func.code.len() || begin_pc > end_pc {
                return Err(JitMetadataError::InvalidLoopEnd {
                    func: func.name.clone(),
                    pc,
                    begin_pc,
                    end_pc,
                    code_len: func.code.len(),
                });
            }
            let encoded_end_offset = loop_end_offset(func.code[pc]);
            if encoded_end_offset > 0 {
                let encoded_end_pc = pc + encoded_end_offset;
                if encoded_end_pc != end_pc {
                    return Err(JitMetadataError::InconsistentLoopEnd {
                        func: func.name.clone(),
                        pc,
                        encoded_end_pc,
                        metadata_end_pc: end_pc,
                    });
                }
            }
            validate_loop_end_backedge(func, pc, end_pc)?;
            Ok(())
        }
    }
}

fn validate_loop_end_backedge(
    func: &FunctionDef,
    pc: usize,
    end_pc: usize,
) -> Result<(), JitMetadataError> {
    let begin_pc = pc + 1;
    let Some(inst) = func.code.get(end_pc) else {
        return Err(JitMetadataError::InvalidLoopEnd {
            func: func.name.clone(),
            pc,
            begin_pc,
            end_pc,
            code_len: func.code.len(),
        });
    };
    let targets_begin = match inst.opcode() {
        Opcode::Jump => jump_target(end_pc, inst.imm32()) == Some(begin_pc),
        Opcode::ForLoop => inst.forloop_target(end_pc) == begin_pc,
        _ => false,
    };
    if targets_begin {
        Ok(())
    } else {
        Err(JitMetadataError::InvalidLoopEndBackEdge {
            func: func.name.clone(),
            pc,
            begin_pc,
            end_pc,
        })
    }
}

fn jump_target(pc: usize, offset: i32) -> Option<usize> {
    let target = pc as i64 + offset as i64;
    (target >= 0).then_some(target as usize)
}

fn loop_end_offset(inst: vo_runtime::instruction::Instruction) -> usize {
    ((inst.a >> 8) & 0xFF) as usize
}

fn required_layout(opcode: Opcode, flags: u8) -> Option<&'static str> {
    match opcode {
        Opcode::ArrayNew
        | Opcode::ArrayGet
        | Opcode::ArraySet
        | Opcode::ArrayAddr
        | Opcode::SliceNew
        | Opcode::SliceGet
        | Opcode::SliceSet
        | Opcode::SliceAddr
        | Opcode::SliceAppend
            if flags == 0 =>
        {
            Some("ElemLayout")
        }
        Opcode::MapGet => Some("MapGet"),
        Opcode::MapSet => Some("MapSet"),
        Opcode::MapDelete => Some("MapDelete"),
        _ => None,
    }
}

fn wrong_kind(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    metadata: &'static str,
) -> JitMetadataError {
    JitMetadataError::WrongMetadataKind {
        func: func.name.clone(),
        pc,
        opcode,
        metadata,
    }
}

fn verify_slot(
    func: &FunctionDef,
    pc: usize,
    slot: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    if slot >= func.local_slots {
        return Err(JitMetadataError::SlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot,
            local_slots: func.local_slots,
            access,
        });
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::Module as VoModule;
    use vo_runtime::instruction::Instruction;
    use vo_runtime::SlotType;

    fn make_func(
        code: Vec<Instruction>,
        jit_metadata: Vec<JitInstructionMetadata>,
        local_slots: u16,
    ) -> FunctionDef {
        let slot_types = vec![SlotType::Value; local_slots as usize];
        let borrowed_scan_slots_prefix =
            FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types);
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&code);
        FunctionDef {
            name: "verify".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
            ret_slots: 0,
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls,
            has_call_extern,
            code,
            jit_metadata,
            slot_types,
            borrowed_scan_slots_prefix,
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    fn hint_loop(end_offset: u8) -> Instruction {
        Instruction::with_flags(Opcode::Hint, HINT_LOOP, (end_offset as u16) << 8, 0, 0)
    }

    fn jump(offset: i32) -> Instruction {
        let encoded = offset as u32;
        Instruction::new(Opcode::Jump, 0, encoded as u16, (encoded >> 16) as u16)
    }

    #[test]
    fn rejects_metadata_length_mismatch() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            Vec::new(),
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::LengthMismatch { .. })
        ));
    }

    #[test]
    fn rejects_wrong_metadata_kind_for_opcode() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::LoadInt, 0, 1, 0)],
            vec![JitInstructionMetadata::MapDelete { key_slots: 1 }],
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::WrongMetadataKind { .. })
        ));
    }

    #[test]
    fn rejects_invalid_opcode() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction {
                op: 254,
                flags: 0,
                a: 0,
                b: 0,
                c: 0,
            }],
            vec![JitInstructionMetadata::None],
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidOpcode { raw: 254, .. })
        ));
    }

    #[test]
    fn accepts_zero_elem_layout_metadata() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2)],
            vec![JitInstructionMetadata::ElemLayout {
                elem_bytes: 0,
                needs_sign_extend: false,
            }],
            4,
        ));

        verify_jit_metadata(&module.functions[0], &module).expect("zero-size elements are valid");
    }

    #[test]
    fn rejects_zero_elem_layout_with_sign_extension() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2)],
            vec![JitInstructionMetadata::ElemLayout {
                elem_bytes: 0,
                needs_sign_extend: true,
            }],
            4,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidElemLayout { elem_bytes: 0, .. })
        ));
    }

    #[test]
    fn rejects_inconsistent_elem_layout_metadata() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::SliceGet, 0x81, 0, 1, 2)],
            vec![JitInstructionMetadata::ElemLayout {
                elem_bytes: 8,
                needs_sign_extend: false,
            }],
            4,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InconsistentElemLayout { flags: 0x81, .. })
        ));
    }

    #[test]
    fn rejects_slot_effects_outside_locals() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::Copy, 0, 3, 0)],
            vec![JitInstructionMetadata::None],
            2,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotOutOfRange { access: "read", .. })
        ));
    }

    #[test]
    fn rejects_slot_range_overflow_without_wrapping() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::CopyN, 0, u16::MAX, 2)],
            vec![JitInstructionMetadata::None],
            4,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotRangeOverflow { access: "read", .. })
        ));
    }

    #[test]
    fn rejects_operand_offset_overflow_without_wrapping() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::ArrayNew, 0, 1, u16::MAX)],
            vec![JitInstructionMetadata::ElemLayout {
                elem_bytes: 24,
                needs_sign_extend: false,
            }],
            4,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotRangeOverflow { access: "read", .. })
        ));
    }

    #[test]
    fn rejects_missing_required_dynamic_layout() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2)],
            vec![JitInstructionMetadata::None],
            4,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::MissingLayout {
                layout: "ElemLayout",
                ..
            })
        ));
    }

    #[test]
    fn rejects_missing_array_addr_dynamic_layout() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::ArrayAddr, 0, 0, 1, 2)],
            vec![JitInstructionMetadata::None],
            4,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::MissingLayout {
                opcode: Opcode::ArrayAddr,
                layout: "ElemLayout",
                ..
            })
        ));
    }

    #[test]
    fn rejects_missing_slice_addr_dynamic_layout() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::SliceAddr, 0, 0, 1, 2)],
            vec![JitInstructionMetadata::None],
            4,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::MissingLayout {
                opcode: Opcode::SliceAddr,
                layout: "ElemLayout",
                ..
            })
        ));
    }

    #[test]
    fn rejects_wrong_slice_addr_layout_metadata_kind() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::SliceAddr, 0, 0, 1, 2)],
            vec![JitInstructionMetadata::MapDelete { key_slots: 1 }],
            4,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::WrongMetadataKind {
                opcode: Opcode::SliceAddr,
                ..
            })
        ));
    }

    #[test]
    fn rejects_inconsistent_array_addr_layout_metadata() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::ArrayAddr, 0x82, 0, 1, 2)],
            vec![JitInstructionMetadata::ElemLayout {
                elem_bytes: 4,
                needs_sign_extend: false,
            }],
            4,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InconsistentElemLayout { flags: 0x82, .. })
        ));
    }

    #[test]
    fn accepts_packed_addr_layout_metadata() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![
                Instruction::with_flags(Opcode::ArrayAddr, 0x82, 0, 1, 2),
                Instruction::with_flags(Opcode::SliceAddr, 0x44, 3, 4, 5),
            ],
            vec![
                JitInstructionMetadata::ElemLayout {
                    elem_bytes: 2,
                    needs_sign_extend: true,
                },
                JitInstructionMetadata::ElemLayout {
                    elem_bytes: 4,
                    needs_sign_extend: false,
                },
            ],
            6,
        ));

        verify_jit_metadata(&module.functions[0], &module).unwrap();
    }

    #[test]
    fn rejects_map_get_output_range_overflow() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::MapGet, 0, 1, 2)],
            vec![JitInstructionMetadata::MapGet {
                key_slots: 1,
                val_slots: u16::MAX,
                has_ok: true,
            }],
            4,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotRangeOverflow {
                access: "write",
                ..
            })
        ));
    }

    #[test]
    fn accepts_dynamic_map_layout_metadata() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::MapGet, 4, 1, 2)],
            vec![JitInstructionMetadata::MapGet {
                key_slots: 2,
                val_slots: 2,
                has_ok: true,
            }],
            8,
        ));

        verify_jit_metadata(&module.functions[0], &module).expect("valid metadata");
    }

    #[test]
    fn accepts_loop_end_metadata_with_real_backedge() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![
                hint_loop(0),
                Instruction::new(Opcode::LoadInt, 0, 1, 0),
                jump(-1),
            ],
            vec![
                JitInstructionMetadata::LoopEnd { end_pc: 2 },
                JitInstructionMetadata::None,
                JitInstructionMetadata::None,
            ],
            1,
        ));

        verify_jit_metadata(&module.functions[0], &module).expect("valid loop end");
    }

    #[test]
    fn rejects_loop_end_metadata_without_real_backedge() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![
                hint_loop(0),
                Instruction::new(Opcode::LoadInt, 0, 1, 0),
                Instruction::new(Opcode::LoadInt, 0, 2, 0),
            ],
            vec![
                JitInstructionMetadata::LoopEnd { end_pc: 2 },
                JitInstructionMetadata::None,
                JitInstructionMetadata::None,
            ],
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidLoopEndBackEdge {
                begin_pc: 1,
                end_pc: 2,
                ..
            })
        ));
    }

    #[test]
    fn rejects_compact_loop_end_without_real_backedge() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![
                hint_loop(2),
                Instruction::new(Opcode::LoadInt, 0, 1, 0),
                Instruction::new(Opcode::LoadInt, 0, 2, 0),
            ],
            vec![
                JitInstructionMetadata::None,
                JitInstructionMetadata::None,
                JitInstructionMetadata::None,
            ],
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidLoopEndBackEdge {
                begin_pc: 1,
                end_pc: 2,
                ..
            })
        ));
    }
}
