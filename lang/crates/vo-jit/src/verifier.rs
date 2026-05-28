//! Verifier for bytecode facts consumed by the JIT.

use std::fmt;

use vo_common_core::bytecode::RETURN_FLAG_HEAP_RETURNS;
use vo_common_core::instruction::HINT_LOOP;
use vo_common_core::types::ValueKind;
use vo_runtime::bytecode::{FunctionDef, JitInstructionMetadata, Module as VoModule};
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

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
    SlotTypeMismatch {
        func: String,
        pc: usize,
        opcode: Opcode,
        access: &'static str,
        slot: u16,
        expected: Vec<SlotType>,
        actual: Vec<SlotType>,
    },
    InvalidInterfaceLayout {
        func: String,
        pc: usize,
        opcode: Opcode,
        access: &'static str,
        slot: u16,
        actual: Vec<SlotType>,
    },
    MissingFunction {
        func: String,
        pc: usize,
        callee_id: u32,
    },
    CallShapeMismatch {
        func: String,
        pc: usize,
        opcode: Opcode,
        detail: String,
    },
    GlobalSlotOutOfRange {
        func: String,
        pc: usize,
        slot: u16,
        global_slots: usize,
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
            Self::SlotTypeMismatch {
                func,
                pc,
                opcode,
                access,
                slot,
                expected,
                actual,
            } => write!(
                f,
                "JIT {access} slot layout mismatch for {opcode:?} in {func} at pc {pc}, slot {slot}: expected {expected:?}, actual {actual:?}"
            ),
            Self::InvalidInterfaceLayout {
                func,
                pc,
                opcode,
                access,
                slot,
                actual,
            } => write!(
                f,
                "JIT {access} interface layout mismatch for {opcode:?} in {func} at pc {pc}, slot {slot}: expected [Interface0, Interface1], actual {actual:?}"
            ),
            Self::MissingFunction {
                func,
                pc,
                callee_id,
            } => write!(
                f,
                "JIT call references missing function {callee_id} in {func} at pc {pc}"
            ),
            Self::CallShapeMismatch {
                func,
                pc,
                opcode,
                detail,
            } => write!(
                f,
                "JIT call shape mismatch for {opcode:?} in {func} at pc {pc}: {detail}"
            ),
            Self::GlobalSlotOutOfRange {
                func,
                pc,
                slot,
                global_slots,
                access,
            } => write!(
                f,
                "JIT {access} global slot {slot} out of range for {func} at pc {pc} (global_slots={global_slots})"
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
        verify_slot_contract(func, vo_module, pc)?;
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

fn verify_slot_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
) -> Result<(), JitMetadataError> {
    let inst = func.code[pc];
    let opcode = inst.opcode();
    match opcode {
        Opcode::Copy => Ok(()),
        Opcode::CopyN => verify_copy_n_contract(func, pc, opcode, inst),
        Opcode::IfaceAssign => {
            verify_interface_or_raw_pair(func, pc, opcode, inst.a, "IfaceAssign destination")?;
            verify_iface_assign_source(func, pc, opcode, inst.b, inst.flags)
        }
        Opcode::Return => verify_return_contract(func, pc, inst),
        Opcode::Call => verify_static_call_contract(func, vo_module, pc, inst),
        Opcode::CallClosure => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "CallClosure callee",
            )?;
            verify_structural_range(
                func,
                pc,
                opcode,
                inst.b,
                inst.packed_arg_slots(),
                "CallClosure args",
            )?;
            verify_structural_range(
                func,
                pc,
                opcode,
                inst.packed_call_ret_start(),
                inst.packed_ret_slots(),
                "CallClosure returns",
            )
        }
        Opcode::CallIface => {
            verify_interface_pair(func, pc, opcode, inst.a, "CallIface receiver")?;
            verify_structural_range(
                func,
                pc,
                opcode,
                inst.b,
                inst.packed_arg_slots(),
                "CallIface args",
            )?;
            verify_structural_range(
                func,
                pc,
                opcode,
                inst.packed_call_ret_start(),
                inst.packed_ret_slots(),
                "CallIface returns",
            )
        }
        Opcode::CallExtern => verify_call_extern_contract(func, vo_module, pc, inst),
        Opcode::PtrSet => verify_ptr_set_contract(func, pc, opcode, inst.a, inst.c, inst.flags),
        Opcode::PtrSetN => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrSetN pointer",
            )?;
            let source = local_layout(func, pc, inst.c, inst.flags as u16, "PtrSetN source")?;
            if source
                .iter()
                .any(|st| matches!(st, SlotType::GcRef | SlotType::Interface1))
            {
                return Err(JitMetadataError::SlotTypeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    access: "PtrSetN source requires typed write barriers",
                    slot: inst.c,
                    expected: source
                        .iter()
                        .map(|st| match st {
                            SlotType::GcRef | SlotType::Interface1 => SlotType::Value,
                            other => *other,
                        })
                        .collect(),
                    actual: source.to_vec(),
                });
            }
            Ok(())
        }
        Opcode::GlobalSet => {
            verify_global_set_contract(func, vo_module, pc, opcode, inst.a, inst.b, 1)
        }
        Opcode::GlobalSetN => verify_global_set_contract(
            func,
            vo_module,
            pc,
            opcode,
            inst.a,
            inst.b,
            inst.flags as u16,
        ),
        Opcode::SlotSet => verify_slot_set_contract(func, pc, opcode, inst.a, inst.b, inst.c, 1),
        Opcode::SlotSetN => {
            verify_slot_set_contract(func, pc, opcode, inst.a, inst.b, inst.c, inst.flags as u16)
        }
        Opcode::Panic => verify_interface_pair(func, pc, opcode, inst.a, "Panic payload"),
        _ => Ok(()),
    }
}

fn verify_return_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::Return;
    if (inst.flags & RETURN_FLAG_HEAP_RETURNS) != 0 {
        if inst.b != func.heap_ret_gcref_count {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "heap return count {} does not match function heap_ret_gcref_count {}",
                    inst.b, func.heap_ret_gcref_count
                ),
            });
        }
        let expected = vec![SlotType::GcRef; inst.b as usize];
        verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &expected,
            "Return heap named returns",
        )?;
        return Ok(());
    }

    if inst.b > func.ret_slots {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "return slot count {} exceeds function ret_slots {}",
                inst.b, func.ret_slots
            ),
        });
    }
    verify_structural_range(func, pc, opcode, inst.a, inst.b, "Return values")?;

    if func.error_ret_slot >= 0 {
        let error_offset = func.error_ret_slot as u16;
        if error_offset + 1 < inst.b {
            verify_interface_pair(func, pc, opcode, inst.a + error_offset, "Return error slot")?;
        }
    }
    Ok(())
}

fn verify_static_call_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::Call;
    let callee_id = inst.static_call_func_id();
    let callee = vo_module.functions.get(callee_id as usize).ok_or_else(|| {
        JitMetadataError::MissingFunction {
            func: func.name.clone(),
            pc,
            callee_id,
        }
    })?;

    if callee.param_slots <= u8::MAX as u16
        && callee.ret_slots <= u8::MAX as u16
        && inst.packed_arg_slots() != callee.param_slots
    {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "encoded arg slots {} do not match callee {} param_slots {}",
                inst.packed_arg_slots(),
                callee.name,
                callee.param_slots
            ),
        });
    }
    if callee.param_slots <= u8::MAX as u16
        && callee.ret_slots <= u8::MAX as u16
        && inst.packed_ret_slots() != callee.ret_slots
    {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "encoded ret slots {} do not match callee {} ret_slots {}",
                inst.packed_ret_slots(),
                callee.name,
                callee.ret_slots
            ),
        });
    }

    let expected_args = callee
        .slot_types
        .get(..callee.param_slots as usize)
        .ok_or_else(|| JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "callee {} has {} slot_types but param_slots={}",
                callee.name,
                callee.slot_types.len(),
                callee.param_slots
            ),
        })?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        expected_args,
        "Call argument buffer",
    )?;
    let ret_start = inst.b.checked_add(callee.param_slots).ok_or_else(|| {
        JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: inst.b,
            count: callee.param_slots,
            access: "Call return buffer",
        }
    })?;
    verify_structural_range(
        func,
        pc,
        opcode,
        ret_start,
        callee.ret_slots,
        "Call return buffer",
    )
}

fn verify_call_extern_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::CallExtern;
    let extern_def =
        vo_module
            .externs
            .get(inst.b as usize)
            .ok_or_else(|| JitMetadataError::MissingExtern {
                func: func.name.clone(),
                pc,
                extern_id: inst.b,
            })?;
    if !extern_def.param_kinds.is_empty() && extern_def.param_kinds.len() != inst.flags as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "extern {} has {} param_kinds but instruction encodes {} arg slots",
                extern_def.name,
                extern_def.param_kinds.len(),
                inst.flags
            ),
        });
    }
    verify_range(func, pc, inst.c, inst.flags as u16, "CallExtern args")?;
    verify_structural_range(
        func,
        pc,
        opcode,
        inst.a,
        extern_def.ret_slots,
        "CallExtern returns",
    )
}

fn verify_ptr_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    ptr_slot: u16,
    src_slot: u16,
    flags: u8,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        ptr_slot,
        &[SlotType::GcRef],
        "PtrSet pointer",
    )?;
    let source = local_layout(func, pc, src_slot, 1, "PtrSet source")?;
    let requires_barrier = matches!(source[0], SlotType::GcRef | SlotType::Interface1);
    let has_barrier = (flags & 1) != 0;
    if requires_barrier && !has_barrier {
        return Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access: "PtrSet missing write barrier",
            slot: src_slot,
            expected: vec![SlotType::GcRef],
            actual: source.to_vec(),
        });
    }
    Ok(())
}

fn verify_global_set_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    global_start: u16,
    src_start: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let globals = flattened_global_slot_types(vo_module);
    let end =
        global_start
            .checked_add(count)
            .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
                func: func.name.clone(),
                pc,
                start: global_start,
                count,
                access: "global write",
            })? as usize;
    if end > globals.len() {
        return Err(JitMetadataError::GlobalSlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: global_start,
            global_slots: globals.len(),
            access: "write",
        });
    }
    let expected = &globals[global_start as usize..end];
    verify_structural_layout(func, pc, opcode, global_start, expected, "GlobalSet target")?;
    verify_structural_range(func, pc, opcode, src_start, count, "GlobalSet source")
}

fn verify_slot_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    base_start: u16,
    index_slot: u16,
    src_start: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        index_slot,
        &[SlotType::Value],
        "SlotSet index",
    )?;
    verify_structural_range(func, pc, opcode, base_start, count, "SlotSet element")?;
    verify_structural_range(func, pc, opcode, src_start, count, "SlotSet source")
}

fn verify_copy_n_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let count = inst.copy_n_count();
    verify_structural_range(func, pc, opcode, inst.a, count, "CopyN destination")?;
    verify_structural_range(func, pc, opcode, inst.b, count, "CopyN source")
}

fn verify_iface_assign_source(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    src_slot: u16,
    raw_vk: u8,
) -> Result<(), JitMetadataError> {
    let vk = ValueKind::from_u8(raw_vk);
    match vk {
        ValueKind::Interface => {
            verify_interface_pair(func, pc, opcode, src_slot, "IfaceAssign source")
        }
        ValueKind::Array
        | ValueKind::Struct
        | ValueKind::String
        | ValueKind::Slice
        | ValueKind::Map
        | ValueKind::Channel
        | ValueKind::Closure
        | ValueKind::Pointer
        | ValueKind::Port
        | ValueKind::Island => verify_layout(
            func,
            pc,
            opcode,
            src_slot,
            &[SlotType::GcRef],
            "IfaceAssign source",
        ),
        ValueKind::Float32 | ValueKind::Float64 => verify_layout(
            func,
            pc,
            opcode,
            src_slot,
            &[SlotType::Float],
            "IfaceAssign source",
        ),
        _ => verify_layout(
            func,
            pc,
            opcode,
            src_slot,
            &[SlotType::Value],
            "IfaceAssign source",
        ),
    }
}

fn verify_interface_pair(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, slot, 2, access)?;
    if actual == [SlotType::Interface0, SlotType::Interface1] {
        Ok(())
    } else {
        Err(JitMetadataError::InvalidInterfaceLayout {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot,
            actual: actual.to_vec(),
        })
    }
}

fn verify_interface_or_raw_pair(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, slot, 2, access)?;
    if actual == [SlotType::Interface0, SlotType::Interface1]
        || actual == [SlotType::Value, SlotType::Value]
    {
        Ok(())
    } else {
        Err(JitMetadataError::InvalidInterfaceLayout {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot,
            actual: actual.to_vec(),
        })
    }
}

fn verify_structural_range(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let layout = local_layout(func, pc, start, count, access)?;
    verify_structural_layout(func, pc, opcode, start, layout, access)
}

fn verify_structural_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    layout: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let mut i = 0usize;
    while i < layout.len() {
        match layout[i] {
            SlotType::Interface0 => {
                if layout.get(i + 1) != Some(&SlotType::Interface1) {
                    return Err(JitMetadataError::InvalidInterfaceLayout {
                        func: func.name.clone(),
                        pc,
                        opcode,
                        access,
                        slot: start + i as u16,
                        actual: layout[i..(i + 1).min(layout.len())].to_vec(),
                    });
                }
                i += 2;
            }
            SlotType::Interface1 => {
                return Err(JitMetadataError::InvalidInterfaceLayout {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    access,
                    slot: start + i as u16,
                    actual: vec![SlotType::Interface1],
                });
            }
            _ => i += 1,
        }
    }
    Ok(())
}

fn verify_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

fn local_layout<'a>(
    func: &'a FunctionDef,
    pc: usize,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<&'a [SlotType], JitMetadataError> {
    verify_range(func, pc, start, count, access)?;
    let start = start as usize;
    let end = start + count as usize;
    Ok(&func.slot_types[start..end])
}

fn verify_range(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    if count == 0 {
        return Ok(());
    }
    let end = start
        .checked_add(count - 1)
        .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start,
            count,
            access,
        })?;
    if end >= func.local_slots || end as usize >= func.slot_types.len() {
        return Err(JitMetadataError::SlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: end,
            local_slots: func.local_slots,
            access,
        });
    }
    Ok(())
}

fn flattened_global_slot_types(vo_module: &VoModule) -> Vec<SlotType> {
    vo_module
        .globals
        .iter()
        .flat_map(|global| global.slot_types.iter().copied())
        .collect()
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
    use vo_runtime::bytecode::{ExternDef, GlobalDef, Module as VoModule};
    use vo_runtime::instruction::Instruction;
    use vo_runtime::SlotType;

    fn make_func(
        code: Vec<Instruction>,
        jit_metadata: Vec<JitInstructionMetadata>,
        local_slots: u16,
    ) -> FunctionDef {
        let slot_types = vec![SlotType::Value; local_slots as usize];
        make_func_with_shape(code, jit_metadata, slot_types, 0, 0, -1)
    }

    fn make_func_with_slot_types(
        code: Vec<Instruction>,
        jit_metadata: Vec<JitInstructionMetadata>,
        slot_types: Vec<SlotType>,
        ret_slots: u16,
    ) -> FunctionDef {
        make_func_with_shape(code, jit_metadata, slot_types, 0, ret_slots, -1)
    }

    fn make_func_with_shape(
        code: Vec<Instruction>,
        jit_metadata: Vec<JitInstructionMetadata>,
        slot_types: Vec<SlotType>,
        param_slots: u16,
        ret_slots: u16,
        error_ret_slot: i16,
    ) -> FunctionDef {
        let local_slots = slot_types.len() as u16;
        let borrowed_scan_slots_prefix =
            FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types);
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&code);
        FunctionDef {
            name: "verify".to_string(),
            param_count: 0,
            param_slots,
            local_slots,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
            ret_slots,
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot,
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
    fn rejects_copyn_partial_interface_layout() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::CopyN, 0, 2, 2)],
            vec![JitInstructionMetadata::None],
            vec![
                SlotType::Value,
                SlotType::Value,
                SlotType::Interface0,
                SlotType::Value,
            ],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::CopyN,
                ..
            })
        ));
    }

    #[test]
    fn rejects_iface_assign_and_panic_without_interface_pairs() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::IfaceAssign, 2, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::Value],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::Panic, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::Value],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::IfaceAssign,
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::Panic,
                ..
            })
        ));
    }

    #[test]
    fn rejects_return_error_slot_without_interface_layout() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_shape(
            vec![Instruction::with_flags(Opcode::Return, 1, 0, 2, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::Value],
            0,
            2,
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::Return,
                ..
            })
        ));
    }

    #[test]
    fn rejects_call_slot_contract_mismatches() {
        let callee = make_func_with_shape(
            vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Interface0, SlotType::Interface1],
            2,
            0,
            -1,
        );
        let caller = make_func_with_slot_types(
            vec![Instruction::new(Opcode::Call, 1, 0, 2 << 8)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::Value],
            0,
        );
        let closure = make_func_with_slot_types(
            vec![Instruction::new(Opcode::CallClosure, 0, 1, 1 << 8)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Value, SlotType::Value],
            0,
        );
        let iface = make_func_with_slot_types(
            vec![Instruction::new(Opcode::CallIface, 0, 2, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::Value],
            0,
        );
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(caller);
        module.functions.push(callee);
        module.functions.push(closure);
        module.functions.push(iface);

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::Call,
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[2], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::CallClosure,
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[3], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::CallIface,
                ..
            })
        ));
    }

    #[test]
    fn rejects_call_extern_param_kind_mismatch() {
        let mut module = VoModule::new("verify".to_string());
        module.externs.push(ExternDef {
            name: "host".to_string(),
            param_slots: 0,
            ret_slots: 0,
            is_blocking: false,
            param_kinds: vec![vo_runtime::bytecode::ExtSlotKind::Value],
        });
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::CallExtern, 2, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            2,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::CallShapeMismatch {
                opcode: Opcode::CallExtern,
                ..
            })
        ));
    }

    #[test]
    fn rejects_store_slot_contract_mismatches() {
        let mut module = VoModule::new("verify".to_string());
        module.globals.push(GlobalDef {
            name: "g".to_string(),
            slots: 2,
            value_kind: 0,
            meta_id: 0,
            slot_types: vec![SlotType::Interface0, SlotType::Interface1],
        });
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::GlobalSetN, 2, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Interface0, SlotType::Value],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::PtrSet, 0, 0, 0, 1)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::GcRef],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::SlotSetN, 2, 0, 2, 3)],
            vec![JitInstructionMetadata::None],
            vec![
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Interface0,
                SlotType::Value,
            ],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::GlobalSetN,
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::PtrSet,
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[2], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::SlotSetN,
                ..
            })
        ));
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
            Err(JitMetadataError::SlotRangeOverflow {
                access: "CopyN source",
                ..
            })
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
