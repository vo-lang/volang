//! Verifier for bytecode facts consumed by the JIT.

#![allow(clippy::too_many_arguments)]

use std::fmt;

use vo_common_core::bytecode::RETURN_FLAG_HEAP_RETURNS;
use vo_common_core::instruction::HINT_LOOP;
use vo_common_core::types::ValueKind;
use vo_runtime::bytecode::{Constant, FunctionDef, JitInstructionMetadata, Module as VoModule};
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

use crate::effects::{EffectError, SlotRangeError};
#[cfg(test)]
use crate::semantics::JitMetadataRequirement;

const RAW_I64_SLOTS: &[SlotType] = &[
    SlotType::Value,
    SlotType::GcRef,
    SlotType::Interface0,
    SlotType::Interface1,
];
const ANY_SINGLE_SLOT: &[SlotType] = &[
    SlotType::Value,
    SlotType::GcRef,
    SlotType::Interface0,
    SlotType::Interface1,
    SlotType::Float,
];
const FLOAT_STORAGE_SLOTS: &[SlotType] = &[SlotType::Float, SlotType::Value];

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
    UnsupportedLegacyMetadata {
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
    MissingConstant {
        func: String,
        pc: usize,
        const_id: u16,
    },
    ConstantKindMismatch {
        func: String,
        pc: usize,
        opcode: Opcode,
        const_id: u16,
        expected: &'static str,
        actual: &'static str,
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
    InvalidBranchTarget {
        func: String,
        pc: usize,
        opcode: Opcode,
        target: i64,
        code_len: usize,
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
    FunctionInvariant {
        func: String,
        detail: String,
    },
    InvalidValueKind {
        func: String,
        pc: usize,
        opcode: Opcode,
        raw: u8,
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
            Self::UnsupportedLegacyMetadata {
                func,
                pc,
                opcode,
                metadata,
            } => write!(
                f,
                "legacy JIT metadata kind {metadata} is not supported by strict {opcode:?} verification in {func} at pc {pc}"
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
            Self::MissingConstant { func, pc, const_id } => write!(
                f,
                "JIT instruction references missing constant {const_id} in {func} at pc {pc}"
            ),
            Self::ConstantKindMismatch {
                func,
                pc,
                opcode,
                const_id,
                expected,
                actual,
            } => write!(
                f,
                "JIT constant kind mismatch for {opcode:?} in {func} at pc {pc}, const {const_id}: expected {expected}, actual {actual}"
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
            Self::InvalidBranchTarget {
                func,
                pc,
                opcode,
                target,
                code_len,
            } => write!(
                f,
                "JIT branch target {target} for {opcode:?} in {func} at pc {pc} is outside code length {code_len}"
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
            Self::FunctionInvariant { func, detail } => {
                write!(f, "JIT function metadata invariant failed in {func}: {detail}")
            }
            Self::InvalidValueKind {
                func,
                pc,
                opcode,
                raw,
            } => write!(
                f,
                "invalid ValueKind tag {raw} for {opcode:?} in {func} at pc {pc}"
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
    verify_function_invariants(func)?;

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

fn verify_function_invariants(func: &FunctionDef) -> Result<(), JitMetadataError> {
    let invariant = |detail: String| JitMetadataError::FunctionInvariant {
        func: func.name.clone(),
        detail,
    };

    if func.local_slots as usize != func.slot_types.len() {
        return Err(invariant(format!(
            "local_slots={} but slot_types.len()={}",
            func.local_slots,
            func.slot_types.len()
        )));
    }
    if func.param_slots > func.local_slots {
        return Err(invariant(format!(
            "param_slots={} exceeds local_slots={}",
            func.param_slots, func.local_slots
        )));
    }
    if func.recv_slots > func.param_slots {
        return Err(invariant(format!(
            "recv_slots={} exceeds param_slots={}",
            func.recv_slots, func.param_slots
        )));
    }
    if func.ret_slot_types.len() != func.ret_slots as usize {
        return Err(invariant(format!(
            "ret_slot_types.len()={} but ret_slots={}",
            func.ret_slot_types.len(),
            func.ret_slots
        )));
    }
    if func.gc_scan_slots != FunctionDef::compute_gc_scan_slots(&func.slot_types) {
        return Err(invariant(format!(
            "gc_scan_slots={} but computed={}",
            func.gc_scan_slots,
            FunctionDef::compute_gc_scan_slots(&func.slot_types)
        )));
    }
    let expected_prefix = FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
    if func.borrowed_scan_slots_prefix != expected_prefix {
        return Err(invariant(format!(
            "borrowed_scan_slots_prefix.len()={} but computed len={}",
            func.borrowed_scan_slots_prefix.len(),
            expected_prefix.len()
        )));
    }
    let has_defer = func
        .code
        .iter()
        .any(|inst| matches!(inst.opcode(), Opcode::DeferPush | Opcode::ErrDeferPush));
    if func.has_defer != has_defer {
        return Err(invariant(format!(
            "has_defer={} but bytecode has_defer={}",
            func.has_defer, has_defer
        )));
    }
    let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&func.code);
    if func.has_calls != has_calls {
        return Err(invariant(format!(
            "has_calls={} but bytecode has_calls={}",
            func.has_calls, has_calls
        )));
    }
    if func.has_call_extern != has_call_extern {
        return Err(invariant(format!(
            "has_call_extern={} but bytecode has_call_extern={}",
            func.has_call_extern, has_call_extern
        )));
    }
    if func.heap_ret_slots.len() != func.heap_ret_gcref_count as usize {
        return Err(invariant(format!(
            "heap_ret_slots.len()={} but heap_ret_gcref_count={}",
            func.heap_ret_slots.len(),
            func.heap_ret_gcref_count
        )));
    }
    if func.heap_ret_gcref_count > 0 {
        let end = func
            .heap_ret_gcref_start
            .checked_add(func.heap_ret_gcref_count)
            .ok_or_else(|| {
                invariant(format!(
                    "heap return range {}..+{} overflows",
                    func.heap_ret_gcref_start, func.heap_ret_gcref_count
                ))
            })?;
        if end > func.local_slots {
            return Err(invariant(format!(
                "heap return range {}..{} exceeds local_slots={}",
                func.heap_ret_gcref_start, end, func.local_slots
            )));
        }
        for slot in func.heap_ret_gcref_start..end {
            if func.slot_types[slot as usize] != SlotType::GcRef {
                return Err(invariant(format!(
                    "heap return slot {slot} must be GcRef, got {:?}",
                    func.slot_types[slot as usize]
                )));
            }
        }
    }
    if func.error_ret_slot >= 0 && (func.error_ret_slot as u16).saturating_add(1) >= func.ret_slots
    {
        return Err(invariant(format!(
            "error_ret_slot={} is not a two-slot interface inside ret_slots={}",
            func.error_ret_slot, func.ret_slots
        )));
    }
    if func.is_closure {
        if func.param_slots == 0 || func.slot_types.first() != Some(&SlotType::GcRef) {
            return Err(invariant(
                "closure functions must reserve GcRef slot 0".to_string(),
            ));
        }
    }
    for (idx, transfer) in func.capture_types.iter().enumerate() {
        validate_transfer_type_kinds(func, idx, "capture_types", transfer)?;
    }
    for (idx, transfer) in func.param_types.iter().enumerate() {
        validate_transfer_type_kinds(func, idx, "param_types", transfer)?;
    }

    Ok(())
}

fn validate_transfer_type_kinds(
    func: &FunctionDef,
    idx: usize,
    access: &'static str,
    transfer: &vo_common_core::TransferType,
) -> Result<(), JitMetadataError> {
    let invariant = |detail: String| JitMetadataError::FunctionInvariant {
        func: func.name.clone(),
        detail,
    };
    let meta_kind = transfer.meta_raw as u8;
    if ValueKind::try_from(meta_kind).is_err() {
        return Err(invariant(format!(
            "{access}[{idx}] has invalid ValueMeta kind tag {meta_kind}"
        )));
    }
    let rttid_kind = transfer.rttid_raw as u8;
    if ValueKind::try_from(rttid_kind).is_err() {
        return Err(invariant(format!(
            "{access}[{idx}] has invalid ValueRttid kind tag {rttid_kind}"
        )));
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
        Opcode::LoadInt => verify_load_int_contract(func, pc, inst),
        Opcode::LoadConst => verify_load_const_contract(func, vo_module, pc, inst),
        Opcode::StrNew => verify_str_new_contract(func, vo_module, pc, inst),
        Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::DivI
        | Opcode::DivU
        | Opcode::ModI
        | Opcode::ModU
        | Opcode::LtI
        | Opcode::LtU
        | Opcode::LeI
        | Opcode::LeU
        | Opcode::GtI
        | Opcode::GtU
        | Opcode::GeI
        | Opcode::GeU
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU => verify_binary_slot_contract(
            func,
            pc,
            opcode,
            inst,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            scalar_destination_access(opcode),
            "scalar lhs",
            "scalar rhs",
        ),
        Opcode::EqI | Opcode::NeI | Opcode::And | Opcode::Or | Opcode::Xor | Opcode::AndNot => {
            verify_binary_one_of_slot_contract(
                func,
                pc,
                opcode,
                inst,
                &[SlotType::Value],
                RAW_I64_SLOTS,
                RAW_I64_SLOTS,
                scalar_destination_access(opcode),
                "raw lhs",
                "raw rhs",
            )
        }
        Opcode::NegI | Opcode::BoolNot => verify_unary_slot_contract(
            func,
            pc,
            opcode,
            inst,
            SlotType::Value,
            SlotType::Value,
            scalar_destination_access(opcode),
            "scalar source",
        ),
        Opcode::Not => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            &[SlotType::Value],
            RAW_I64_SLOTS,
            scalar_destination_access(opcode),
            "raw source",
        ),
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF => {
            verify_binary_one_of_slot_contract(
                func,
                pc,
                opcode,
                inst,
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                scalar_destination_access(opcode),
                "float lhs",
                "float rhs",
            )
        }
        Opcode::NegF => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            FLOAT_STORAGE_SLOTS,
            scalar_destination_access(opcode),
            "float source",
        ),
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF => {
            verify_binary_one_of_slot_contract(
                func,
                pc,
                opcode,
                inst,
                &[SlotType::Value],
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                scalar_destination_access(opcode),
                "float lhs",
                "float rhs",
            )
        }
        Opcode::ConvI2F => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            &[SlotType::Value],
            "ConvI2F destination",
            "ConvI2F source",
        ),
        Opcode::ConvF2I => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            &[SlotType::Value],
            FLOAT_STORAGE_SLOTS,
            "ConvF2I destination",
            "ConvF2I source",
        ),
        Opcode::ConvF64F32 => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            FLOAT_STORAGE_SLOTS,
            "ConvF64F32 destination",
            "ConvF64F32 source",
        ),
        Opcode::ConvF32F64 => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            &[SlotType::Value, SlotType::Float],
            "ConvF32F64 destination",
            "ConvF32F64 source",
        ),
        Opcode::Trunc => verify_unary_slot_contract(
            func,
            pc,
            opcode,
            inst,
            SlotType::Value,
            SlotType::Value,
            "Trunc destination",
            "Trunc source",
        ),
        Opcode::IndexCheck => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "IndexCheck index",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "IndexCheck length",
            )
        }
        Opcode::Jump => {
            verify_jump_target_contract(func, pc, opcode, jump_target_i64(pc, inst.imm32()))
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                if opcode == Opcode::JumpIf {
                    "JumpIf condition"
                } else {
                    "JumpIfNot condition"
                },
            )?;
            verify_jump_target_contract(func, pc, opcode, jump_target_i64(pc, inst.imm32()))
        }
        Opcode::ForLoop => {
            if inst.flags & !0x07 != 0 {
                return Err(JitMetadataError::CallShapeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    detail: format!("unsupported ForLoop flags 0x{:02x}", inst.flags),
                });
            }
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "ForLoop index",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "ForLoop limit",
            )?;
            verify_jump_target_contract(func, pc, opcode, forloop_target_i64(pc, inst.c as i16))
        }
        Opcode::Copy => verify_copy_contract(func, pc, opcode, inst),
        Opcode::CopyN => verify_copy_n_contract(func, pc, opcode, inst),
        Opcode::SlotGet => verify_slot_get_contract(func, pc, opcode, inst.a, inst.b, inst.c, 1),
        Opcode::SlotGetN => {
            verify_slot_get_contract(func, pc, opcode, inst.a, inst.b, inst.c, inst.flags as u16)
        }
        Opcode::IfaceAssign => {
            verify_interface_or_raw_pair(func, pc, opcode, inst.a, "IfaceAssign destination")?;
            let packed =
                verify_iface_assign_metadata_constant(func, vo_module, pc, opcode, inst.c)?;
            let value_kind = verify_value_kind_tag(func, pc, opcode, inst.flags)?;
            verify_iface_assign_metadata_schema(func, vo_module, pc, opcode, value_kind, packed)?;
            verify_iface_assign_source(func, pc, opcode, inst.b, value_kind)
        }
        Opcode::ClosureNew => verify_closure_new_contract(func, vo_module, pc, inst),
        Opcode::ClosureGet => verify_closure_get_contract(func, pc, inst),
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
            let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
            if arg_layout.len() != inst.packed_arg_slots() as usize
                || ret_layout.len() != inst.packed_ret_slots() as usize
            {
                return Err(JitMetadataError::CallShapeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    detail: format!(
                        "CallClosure metadata layout slots args={} returns={} do not match encoded args={} returns={}",
                        arg_layout.len(),
                        ret_layout.len(),
                        inst.packed_arg_slots(),
                        inst.packed_ret_slots()
                    ),
                });
            }
            verify_local_layout_matches(func, pc, opcode, inst.b, &arg_layout, "CallClosure args")?;
            verify_local_layout_matches(
                func,
                pc,
                opcode,
                inst.packed_call_ret_start(),
                &ret_layout,
                "CallClosure returns",
            )
        }
        Opcode::CallIface => {
            verify_interface_pair(func, pc, opcode, inst.a, "CallIface receiver")?;
            let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
            if arg_layout.len() != inst.packed_arg_slots() as usize
                || ret_layout.len() != inst.packed_ret_slots() as usize
            {
                return Err(JitMetadataError::CallShapeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    detail: format!(
                        "CallIface metadata layout slots args={} returns={} do not match encoded args={} returns={}",
                        arg_layout.len(),
                        ret_layout.len(),
                        inst.packed_arg_slots(),
                        inst.packed_ret_slots()
                    ),
                });
            }
            verify_local_layout_matches(func, pc, opcode, inst.b, &arg_layout, "CallIface args")?;
            verify_local_layout_matches(
                func,
                pc,
                opcode,
                inst.packed_call_ret_start(),
                &ret_layout,
                "CallIface returns",
            )
        }
        Opcode::CallExtern => verify_call_extern_contract(func, vo_module, pc, inst),
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush => {
            verify_shared_call_shape_contract(func, vo_module, pc, opcode, inst)
        }
        Opcode::GoIsland => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "GoIsland island",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "GoIsland closure",
            )?;
            let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
            if !ret_layout.is_empty() || arg_layout.len() != inst.flags as usize {
                return Err(JitMetadataError::CallShapeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    detail: format!(
                        "GoIsland metadata layout slots args={} returns={} do not match encoded args={}",
                        arg_layout.len(),
                        ret_layout.len(),
                        inst.flags
                    ),
                });
            }
            verify_local_layout_matches(func, pc, opcode, inst.c, &arg_layout, "GoIsland args")
        }
        Opcode::GlobalGet => {
            verify_global_get_contract(func, vo_module, pc, opcode, inst.b, inst.a, 1)
        }
        Opcode::GlobalGetN => verify_global_get_contract(
            func,
            vo_module,
            pc,
            opcode,
            inst.b,
            inst.a,
            inst.flags as u16,
        ),
        Opcode::PtrNew => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "PtrNew metadata",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrNew destination",
            )
        }
        Opcode::PtrGet => verify_ptr_get_contract(func, pc, opcode, inst.a, inst.b, 1),
        Opcode::PtrGetN => {
            verify_ptr_get_contract(func, pc, opcode, inst.a, inst.b, inst.flags as u16)
        }
        Opcode::PtrAdd => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrAdd destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "PtrAdd pointer",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.c,
                &[SlotType::Value],
                "PtrAdd offset",
            )
        }
        Opcode::PtrSet => verify_ptr_set_contract(func, pc, opcode, inst.a, inst.c, inst.flags),
        Opcode::PtrSetN => {
            let value_layout = ptr_value_layout(func, pc, opcode)?;
            if value_layout.len() != inst.flags as usize {
                return Err(JitMetadataError::CallShapeMismatch {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    detail: format!(
                        "PtrSetN metadata layout slots {} do not match encoded count {}",
                        value_layout.len(),
                        inst.flags
                    ),
                });
            }
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "PtrSetN pointer",
            )?;
            let source = local_layout(func, pc, inst.c, inst.flags as u16, "PtrSetN source")?;
            verify_local_layout_matches(func, pc, opcode, inst.c, &value_layout, "PtrSetN source")?;
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
        Opcode::ArrayNew => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "ArrayNew destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "ArrayNew metadata",
            )?;
            verify_value_range(
                func,
                pc,
                opcode,
                inst.c,
                if inst.flags == 0 { 2 } else { 1 },
                "ArrayNew length/elem_bytes",
            )
        }
        Opcode::ArrayGet => {
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Array source")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Array index")?;
            verify_indexed_layout_contract(func, pc, opcode, inst, inst.a, "ArrayGet destination")
        }
        Opcode::ArrayAddr => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "ArrayAddr destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Array source")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Array index")
        }
        Opcode::ArraySet => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "ArraySet target",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "ArraySet index",
            )?;
            verify_indexed_layout_contract(func, pc, opcode, inst, inst.c, "ArraySet source")
        }
        Opcode::SliceNew => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "SliceNew destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "SliceNew metadata",
            )?;
            verify_value_range(
                func,
                pc,
                opcode,
                inst.c,
                if inst.flags == 0 { 3 } else { 2 },
                "SliceNew len/cap/elem_bytes",
            )
        }
        Opcode::SliceGet => {
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Slice source")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Slice index")?;
            verify_indexed_layout_contract(func, pc, opcode, inst, inst.a, "SliceGet destination")
        }
        Opcode::SliceAddr => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "SliceAddr destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Slice source")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "Slice index")
        }
        Opcode::SliceSet => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "SliceSet target",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "SliceSet index",
            )?;
            verify_indexed_layout_contract(func, pc, opcode, inst, inst.c, "SliceSet source")
        }
        Opcode::SliceAppend => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "SliceAppend destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "SliceAppend source",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.c,
                &[SlotType::Value],
                "SliceAppend metadata",
            )?;
            let value_start = checked_slot_offset_for_verifier(
                func,
                pc,
                inst.c,
                if inst.flags == 0 { 2 } else { 1 },
                "SliceAppend value",
            )?;
            verify_indexed_layout_contract(func, pc, opcode, inst, value_start, "SliceAppend value")
        }
        Opcode::SliceLen | Opcode::SliceCap => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "slice length/capacity result",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "Slice source")
        }
        Opcode::StrLen => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "StrLen destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "string")
        }
        Opcode::StrIndex | Opcode::StrDecodeRune => {
            if opcode == Opcode::StrDecodeRune {
                verify_layout(
                    func,
                    pc,
                    opcode,
                    inst.a,
                    &[SlotType::Value, SlotType::Value],
                    "StrDecodeRune destination",
                )?;
            } else {
                verify_layout(
                    func,
                    pc,
                    opcode,
                    inst.a,
                    &[SlotType::Value],
                    "StrIndex destination",
                )?;
            }
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "string")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "string index")
        }
        Opcode::StrConcat => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "StrConcat destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "string lhs")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::GcRef], "string rhs")
        }
        Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "string comparison destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "string lhs")?;
            verify_layout(func, pc, opcode, inst.c, &[SlotType::GcRef], "string rhs")
        }
        Opcode::StrSlice => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "StrSlice destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "string")?;
            verify_value_range(func, pc, opcode, inst.c, 2, "string slice bounds")
        }
        Opcode::SliceSlice => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "SliceSlice destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "SliceSlice source",
            )?;
            let bound_slots = if (inst.flags & 0b10) != 0 { 3 } else { 2 };
            verify_value_range(func, pc, opcode, inst.c, bound_slots, "SliceSlice bounds")
        }
        Opcode::MapNew => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcRef],
                "MapNew destination",
            )?;
            verify_value_range(func, pc, opcode, inst.b, 2, "MapNew metadata")
        }
        Opcode::MapGet => verify_map_get_contract(func, pc, inst),
        Opcode::MapSet => verify_map_set_contract(func, pc, inst),
        Opcode::MapDelete => verify_map_delete_contract(func, pc, inst),
        Opcode::MapLen => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "MapLen destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "map")
        }
        Opcode::MapIterInit => {
            verify_local_layout_matches(
                func,
                pc,
                opcode,
                inst.a,
                &vo_runtime::objects::map::MAP_ITER_SLOT_TYPES,
                "MapIterInit iterator",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "map")
        }
        Opcode::MapIterNext => verify_map_iter_next_contract(func, pc, opcode, inst),
        Opcode::Panic => verify_interface_pair(func, pc, opcode, inst.a, "Panic payload"),
        Opcode::QueueNew => verify_queue_new_contract(func, pc, inst),
        Opcode::QueueSend => verify_queue_send_contract(func, pc, inst),
        Opcode::QueueRecv => verify_recv_contract(
            func,
            pc,
            opcode,
            inst.b,
            inst.a,
            inst.flags,
            false,
            "QueueRecv",
        ),
        Opcode::QueueLen | Opcode::QueueCap | Opcode::QueueClose => {
            verify_queue_ref_contract(func, pc, opcode, inst)?;
            if opcode == Opcode::QueueLen || opcode == Opcode::QueueCap {
                verify_layout(func, pc, opcode, inst.a, &[SlotType::Value], "queue result")?;
            }
            Ok(())
        }
        Opcode::SelectBegin => Ok(()),
        Opcode::SelectSend => verify_select_send_contract(func, pc, inst),
        Opcode::SelectRecv => verify_recv_contract(
            func,
            pc,
            opcode,
            inst.b,
            inst.a,
            inst.flags,
            true,
            "SelectRecv",
        ),
        Opcode::SelectExec => verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::Value],
            "SelectExec result",
        ),
        Opcode::Recover => verify_interface_pair(func, pc, opcode, inst.a, "Recover destination"),
        Opcode::IfaceAssert => verify_iface_assert_contract(func, pc, inst),
        Opcode::IfaceEq => verify_iface_eq_contract(func, pc, inst),
        Opcode::IslandNew => verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::GcRef],
            "IslandNew destination",
        ),
        Opcode::Hint => Ok(()),
        Opcode::Invalid => Err(JitMetadataError::InvalidOpcode {
            func: func.name.clone(),
            pc,
            raw: inst.op,
        }),
    }
}

fn verify_map_iter_next_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        inst.b,
        &vo_runtime::objects::map::MAP_ITER_SLOT_TYPES,
        "MapIterNext iterator",
    )?;
    let key_slots = inst.map_iter_key_slots();
    let val_slots = inst.map_iter_val_slots();
    let (key_layout, val_layout) = map_iter_layout(func, pc, opcode)?;
    if key_layout.len() != key_slots as usize || val_layout.len() != val_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "MapIterNext metadata layout slots key={} value={} do not match encoded key={} value={}",
                key_layout.len(),
                val_layout.len(),
                key_slots,
                val_slots
            ),
        });
    }
    let kv_slots = key_slots
        .checked_add(val_slots)
        .ok_or(JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: inst.a,
            count: key_slots,
            access: "MapIterNext key/value",
        })?;
    let mut kv_layout = key_layout;
    kv_layout.extend(val_layout);
    debug_assert_eq!(kv_layout.len(), kv_slots as usize);
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        inst.a,
        &kv_layout,
        "MapIterNext key/value",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "MapIterNext ok",
    )
}

fn verify_queue_new_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::QueueNew;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "QueueNew destination",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "QueueNew element metadata",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "QueueNew capacity",
    )
}

fn verify_queue_ref_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let slot = if opcode == Opcode::QueueClose {
        inst.a
    } else {
        inst.b
    };
    verify_layout(func, pc, opcode, slot, &[SlotType::GcRef], "queue")
}

fn verify_queue_send_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::QueueSend;
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != inst.flags as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "QueueSend metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                inst.flags
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "QueueSend queue",
    )?;
    verify_local_layout_matches(func, pc, opcode, inst.b, &elem_layout, "QueueSend value")
}

fn verify_select_send_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::SelectSend;
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    let elem_slots = if inst.flags == 0 {
        1
    } else {
        inst.flags as u16
    };
    if elem_layout.len() != elem_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "SelectSend metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                elem_slots
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "SelectSend queue",
    )?;
    verify_local_layout_matches(func, pc, opcode, inst.b, &elem_layout, "SelectSend value")
}

fn verify_recv_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    queue_slot: u16,
    dst_start: u16,
    flags: u8,
    normalize_zero_elem_slots: bool,
    access_prefix: &'static str,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        queue_slot,
        &[SlotType::GcRef],
        "recv queue",
    )?;

    let inst = vo_runtime::instruction::Instruction::with_flags(opcode, flags, 0, 0, 0);
    let mut elem_slots = inst.recv_elem_slots();
    if normalize_zero_elem_slots && elem_slots == 0 {
        elem_slots = 1;
    }
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != elem_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "{access_prefix} metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                elem_slots
            ),
        });
    }
    if elem_slots > 0 {
        verify_local_layout_matches(func, pc, opcode, dst_start, &elem_layout, access_prefix)?;
    }
    if inst.recv_has_ok() {
        let ok_slot =
            checked_slot_offset_for_verifier(func, pc, dst_start, elem_slots, access_prefix)?;
        verify_layout(
            func,
            pc,
            opcode,
            ok_slot,
            &[SlotType::Value],
            if opcode == Opcode::QueueRecv {
                "QueueRecv ok"
            } else {
                "SelectRecv ok"
            },
        )?;
    }
    Ok(())
}

fn verify_iface_assert_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::IfaceAssert;
    verify_interface_pair(func, pc, opcode, inst.b, "IfaceAssert source")?;

    let assert_kind = inst.flags & 0x03;
    if assert_kind > 1 {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!("unsupported IfaceAssert kind {assert_kind}"),
        });
    }
    let has_ok = ((inst.flags >> 2) & 0x01) != 0;
    let target_slots = (inst.flags >> 3) as u16;
    let result_layout = iface_assert_result_layout(func, pc, opcode)?;
    let dst_slots = if assert_kind == 1 {
        2
    } else {
        target_slots.max(1)
    };
    if result_layout.len() != dst_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "IfaceAssert metadata layout slots {} do not match encoded destination slots {}",
                result_layout.len(),
                dst_slots
            ),
        });
    }
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        inst.a,
        &result_layout,
        "IfaceAssert destination",
    )?;
    if has_ok {
        let ok_slot =
            checked_slot_offset_for_verifier(func, pc, inst.a, dst_slots, "IfaceAssert ok")?;
        verify_layout(
            func,
            pc,
            opcode,
            ok_slot,
            &[SlotType::Value],
            "IfaceAssert ok",
        )?;
    }
    Ok(())
}

fn verify_iface_eq_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::IfaceEq;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::Value],
        "IfaceEq result",
    )?;
    verify_interface_pair(func, pc, opcode, inst.b, "IfaceEq lhs")?;
    verify_interface_pair(func, pc, opcode, inst.c, "IfaceEq rhs")
}

fn verify_binary_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
    dst: SlotType,
    lhs: SlotType,
    rhs: SlotType,
    dst_access: &'static str,
    lhs_access: &'static str,
    rhs_access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_layout(func, pc, opcode, inst.a, &[dst], dst_access)?;
    verify_layout(func, pc, opcode, inst.b, &[lhs], lhs_access)?;
    verify_layout(func, pc, opcode, inst.c, &[rhs], rhs_access)
}

fn verify_unary_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
    dst: SlotType,
    src: SlotType,
    dst_access: &'static str,
    src_access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_layout(func, pc, opcode, inst.a, &[dst], dst_access)?;
    verify_layout(func, pc, opcode, inst.b, &[src], src_access)
}

fn verify_binary_one_of_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
    dst_any: &[SlotType],
    lhs_any: &[SlotType],
    rhs_any: &[SlotType],
    dst_access: &'static str,
    lhs_access: &'static str,
    rhs_access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_one_of_single_slot_layout(func, pc, opcode, inst.a, dst_any, dst_access)?;
    verify_one_of_single_slot_layout(func, pc, opcode, inst.b, lhs_any, lhs_access)?;
    verify_one_of_single_slot_layout(func, pc, opcode, inst.c, rhs_any, rhs_access)
}

fn verify_unary_one_of_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
    dst_any: &[SlotType],
    src_any: &[SlotType],
    dst_access: &'static str,
    src_access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_one_of_single_slot_layout(func, pc, opcode, inst.a, dst_any, dst_access)?;
    verify_one_of_single_slot_layout(func, pc, opcode, inst.b, src_any, src_access)
}

fn scalar_destination_access(opcode: Opcode) -> &'static str {
    match opcode {
        Opcode::AddI => "AddI destination",
        Opcode::SubI => "SubI destination",
        Opcode::MulI => "MulI destination",
        Opcode::DivI => "DivI destination",
        Opcode::DivU => "DivU destination",
        Opcode::ModI => "ModI destination",
        Opcode::ModU => "ModU destination",
        Opcode::NegI => "NegI destination",
        Opcode::AddF => "AddF destination",
        Opcode::SubF => "SubF destination",
        Opcode::MulF => "MulF destination",
        Opcode::DivF => "DivF destination",
        Opcode::NegF => "NegF destination",
        Opcode::EqI => "EqI destination",
        Opcode::NeI => "NeI destination",
        Opcode::LtI => "LtI destination",
        Opcode::LtU => "LtU destination",
        Opcode::LeI => "LeI destination",
        Opcode::LeU => "LeU destination",
        Opcode::GtI => "GtI destination",
        Opcode::GtU => "GtU destination",
        Opcode::GeI => "GeI destination",
        Opcode::GeU => "GeU destination",
        Opcode::EqF => "EqF destination",
        Opcode::NeF => "NeF destination",
        Opcode::LtF => "LtF destination",
        Opcode::LeF => "LeF destination",
        Opcode::GtF => "GtF destination",
        Opcode::GeF => "GeF destination",
        Opcode::And => "And destination",
        Opcode::Or => "Or destination",
        Opcode::Xor => "Xor destination",
        Opcode::AndNot => "AndNot destination",
        Opcode::Not => "Not destination",
        Opcode::Shl => "Shl destination",
        Opcode::ShrS => "ShrS destination",
        Opcode::ShrU => "ShrU destination",
        Opcode::BoolNot => "BoolNot destination",
        _ => "scalar destination",
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
        verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &vec![SlotType::GcRef; inst.b as usize],
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
    if func.ret_slot_types.len() != func.ret_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "function has {} ret_slot_types but ret_slots={}",
                func.ret_slot_types.len(),
                func.ret_slots
            ),
        });
    }
    let expected = &func.ret_slot_types[..inst.b as usize];
    verify_local_layout_matches(func, pc, opcode, inst.a, expected, "Return values")?;

    if func.error_ret_slot >= 0 {
        let error_offset = func.error_ret_slot as u16;
        if error_offset + 1 < inst.b {
            verify_interface_pair(func, pc, opcode, inst.a + error_offset, "Return error slot")?;
        }
    }
    Ok(())
}

fn verify_jump_target_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    target: i64,
) -> Result<(), JitMetadataError> {
    if target >= 0 && (target as usize) < func.code.len() {
        Ok(())
    } else {
        Err(JitMetadataError::InvalidBranchTarget {
            func: func.name.clone(),
            pc,
            opcode,
            target,
            code_len: func.code.len(),
        })
    }
}

fn jump_target_i64(pc: usize, offset: i32) -> i64 {
    pc as i64 + offset as i64
}

fn forloop_target_i64(pc: usize, offset: i16) -> i64 {
    pc as i64 + 1 + i64::from(offset)
}

fn verify_load_int_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    if inst.imm32() == 0 {
        verify_one_of_single_slot_layout(
            func,
            pc,
            Opcode::LoadInt,
            inst.a,
            ANY_SINGLE_SLOT,
            "LoadInt destination",
        )
    } else {
        verify_layout(
            func,
            pc,
            Opcode::LoadInt,
            inst.a,
            &[SlotType::Value],
            "LoadInt destination",
        )
    }
}

fn verify_load_const_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let constant = constant_at(func, vo_module, pc, inst.b)?;
    let expected_slot = match constant {
        Constant::String(_) => {
            return Err(JitMetadataError::ConstantKindMismatch {
                func: func.name.clone(),
                pc,
                opcode: Opcode::LoadConst,
                const_id: inst.b,
                expected: "non-string constant; use StrNew for string allocation",
                actual: constant_kind(constant),
            });
        }
        Constant::Float(_) => {
            return verify_one_of_single_slot_layout(
                func,
                pc,
                Opcode::LoadConst,
                inst.a,
                FLOAT_STORAGE_SLOTS,
                "LoadConst destination",
            );
        }
        Constant::Nil => {
            return verify_one_of_single_slot_layout(
                func,
                pc,
                Opcode::LoadConst,
                inst.a,
                ANY_SINGLE_SLOT,
                "LoadConst destination",
            );
        }
        Constant::Bool(_) | Constant::Int(_) => SlotType::Value,
    };
    verify_layout(
        func,
        pc,
        Opcode::LoadConst,
        inst.a,
        &[expected_slot],
        "LoadConst destination",
    )
}

fn verify_str_new_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        Opcode::StrNew,
        inst.a,
        &[SlotType::GcRef],
        "StrNew destination",
    )?;
    let constant = constant_at(func, vo_module, pc, inst.b)?;
    if !matches!(constant, Constant::String(_)) {
        return Err(JitMetadataError::ConstantKindMismatch {
            func: func.name.clone(),
            pc,
            opcode: Opcode::StrNew,
            const_id: inst.b,
            expected: "String",
            actual: constant_kind(constant),
        });
    }
    Ok(())
}

fn verify_iface_assign_metadata_constant(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    const_id: u16,
) -> Result<i64, JitMetadataError> {
    let constant = constant_at(func, vo_module, pc, const_id)?;
    if let Constant::Int(packed) = constant {
        Ok(*packed)
    } else {
        Err(JitMetadataError::ConstantKindMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            const_id,
            expected: "Int",
            actual: constant_kind(constant),
        })
    }
}

fn verify_value_kind_tag(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    raw: u8,
) -> Result<ValueKind, JitMetadataError> {
    ValueKind::try_from(raw).map_err(|_| JitMetadataError::InvalidValueKind {
        func: func.name.clone(),
        pc,
        opcode,
        raw,
    })
}

fn verify_iface_assign_metadata_schema(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    value_kind: ValueKind,
    packed: i64,
) -> Result<(), JitMetadataError> {
    let packed = packed as u64;
    let high = (packed >> 32) as u32;
    let low = (packed & 0xFFFF_FFFF) as u32;
    if value_kind == ValueKind::Interface {
        if high != 0 {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "interface source metadata must store target iface id in low word only, got high word {high}"
                ),
            });
        }
        if low != 0 && low as usize >= vo_module.interface_metas.len() {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "target interface meta id {low} exceeds interface metadata count {}",
                    vo_module.interface_metas.len()
                ),
            });
        }
    } else if low != 0 && low as usize >= vo_module.itabs.len() {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!("itab id {low} exceeds itab count {}", vo_module.itabs.len()),
        });
    }
    Ok(())
}

fn constant_at<'a>(
    func: &FunctionDef,
    vo_module: &'a VoModule,
    pc: usize,
    const_id: u16,
) -> Result<&'a Constant, JitMetadataError> {
    vo_module
        .constants
        .get(const_id as usize)
        .ok_or_else(|| JitMetadataError::MissingConstant {
            func: func.name.clone(),
            pc,
            const_id,
        })
}

fn constant_kind(constant: &Constant) -> &'static str {
    match constant {
        Constant::Nil => "Nil",
        Constant::Bool(_) => "Bool",
        Constant::Int(_) => "Int",
        Constant::Float(_) => "Float",
        Constant::String(_) => "String",
    }
}

fn verify_closure_new_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        Opcode::ClosureNew,
        inst.a,
        &[SlotType::GcRef],
        "ClosureNew destination",
    )?;
    let callee_id = inst.closure_new_func_id();
    vo_module.functions.get(callee_id as usize).ok_or_else(|| {
        JitMetadataError::MissingFunction {
            func: func.name.clone(),
            pc,
            callee_id,
        }
    })?;
    Ok(())
}

fn verify_closure_get_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        Opcode::ClosureGet,
        0,
        &[SlotType::GcRef],
        "ClosureGet closure",
    )?;
    let capture_slot = inst.b as usize;
    let Some(expected) = func.capture_slot_types.get(capture_slot).copied() else {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode: Opcode::ClosureGet,
            detail: format!(
                "capture slot {} out of range for {} capture slots",
                inst.b,
                func.capture_slot_types.len()
            ),
        });
    };
    verify_layout(
        func,
        pc,
        Opcode::ClosureGet,
        inst.a,
        &[expected],
        "ClosureGet destination",
    )
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
    if callee.ret_slot_types.len() != callee.ret_slots as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "callee {} has {} ret_slot_types but ret_slots={}",
                callee.name,
                callee.ret_slot_types.len(),
                callee.ret_slots
            ),
        });
    }
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        ret_start,
        &callee.ret_slot_types,
        "Call return buffer",
    )
}

fn verify_shared_call_shape_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    if inst.call_shape_is_closure() {
        verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::GcRef],
            "closure callee",
        )?;
        let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
        if !ret_layout.is_empty() || arg_layout.len() != inst.c as usize {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "{opcode:?} closure metadata layout slots args={} returns={} do not match encoded args={}",
                    arg_layout.len(),
                    ret_layout.len(),
                    inst.c
                ),
            });
        }
        verify_local_layout_matches(func, pc, opcode, inst.b, &arg_layout, "closure call args")
    } else {
        let callee_id = inst.call_shape_static_func_id();
        let callee = vo_module.functions.get(callee_id as usize).ok_or_else(|| {
            JitMetadataError::MissingFunction {
                func: func.name.clone(),
                pc,
                callee_id,
            }
        })?;
        if inst.c != callee.param_slots {
            return Err(JitMetadataError::CallShapeMismatch {
                func: func.name.clone(),
                pc,
                opcode,
                detail: format!(
                    "encoded arg slots {} do not match callee {} param_slots {}",
                    inst.c, callee.name, callee.param_slots
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
        verify_layout(func, pc, opcode, inst.b, expected_args, "static call args")?;
        Ok(())
    }
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
    let (arg_layout, ret_layout) = call_extern_layout(func, pc, opcode)?;
    if arg_layout.len() != inst.flags as usize || ret_layout.len() != extern_def.ret_slots as usize
    {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "CallExtern metadata layout slots args={} returns={} do not match encoded args={} extern returns={}",
                arg_layout.len(),
                ret_layout.len(),
                inst.flags,
                extern_def.ret_slots
            ),
        });
    }
    verify_local_layout_matches(func, pc, opcode, inst.c, &arg_layout, "CallExtern args")?;
    verify_local_layout_matches(func, pc, opcode, inst.a, &ret_layout, "CallExtern returns")
}

fn verify_indexed_layout_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
    start: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let layout = indexed_elem_layout(func, pc, opcode, inst)?;
    if layout.is_empty() {
        return Ok(());
    }
    verify_local_layout_matches(func, pc, opcode, start, &layout, access)
}

fn indexed_elem_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<Vec<SlotType>, JitMetadataError> {
    let metadata = func
        .jit_metadata
        .get(pc)
        .ok_or_else(|| JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "ElemLayout",
        })?;
    let layout = crate::metadata::elem_layout_from_instruction(metadata).ok_or_else(|| {
        JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "ElemLayout",
        }
    })?;
    debug_assert_eq!(layout.slot_layout.len(), layout.slots as usize);
    let from_flags = if inst.flags == 0 {
        None
    } else {
        Some(crate::metadata::elem_layout_from_flags(inst.flags))
    };
    if let Some(from_flags) = from_flags {
        if from_flags.bytes != layout.bytes
            || from_flags.needs_sign_extend != layout.needs_sign_extend
        {
            return Err(JitMetadataError::InconsistentElemLayout {
                func: func.name.clone(),
                pc,
                flags: inst.flags,
            });
        }
    }
    Ok(layout.slot_layout)
}

fn verify_map_get_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::MapGet;
    let layout = map_get_layout(func, pc, opcode)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "MapGet map")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "MapGet metadata",
    )?;
    let key_start = checked_slot_offset_for_verifier(func, pc, inst.c, 1, "MapGet key")?;
    verify_layout(
        func,
        pc,
        opcode,
        key_start,
        &layout.key_layout,
        "MapGet key",
    )?;
    let output_slots =
        layout
            .output_slots()
            .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
                func: func.name.clone(),
                pc,
                start: inst.a,
                count: layout.val_slots,
                access: "MapGet destination",
            })?;
    let mut output_layout = layout.val_layout.clone();
    if layout.has_ok {
        output_layout.push(SlotType::Value);
    }
    debug_assert_eq!(output_layout.len(), output_slots as usize);
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &output_layout,
        "MapGet destination",
    )
}

fn verify_map_set_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::MapSet;
    let layout = map_set_layout(func, pc, opcode)?;
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcRef], "MapSet map")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "MapSet metadata",
    )?;
    let key_start = checked_slot_offset_for_verifier(func, pc, inst.b, 1, "MapSet key")?;
    verify_layout(
        func,
        pc,
        opcode,
        key_start,
        &layout.key_layout,
        "MapSet key",
    )?;
    verify_layout(func, pc, opcode, inst.c, &layout.val_layout, "MapSet value")
}

fn verify_map_delete_contract(
    func: &FunctionDef,
    pc: usize,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let opcode = Opcode::MapDelete;
    let key_layout = map_delete_key_layout(func, pc, opcode)?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "MapDelete map",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "MapDelete metadata",
    )?;
    let key_start = checked_slot_offset_for_verifier(func, pc, inst.b, 1, "MapDelete key")?;
    verify_layout(func, pc, opcode, key_start, &key_layout, "MapDelete key")
}

fn map_get_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<crate::metadata::MapGetLayout, JitMetadataError> {
    func.jit_metadata
        .get(pc)
        .and_then(crate::metadata::map_get_layout_from_instruction)
        .ok_or_else(|| JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "MapGet",
        })
}

fn map_set_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<crate::metadata::MapSetLayout, JitMetadataError> {
    func.jit_metadata
        .get(pc)
        .and_then(crate::metadata::map_set_layout_from_instruction)
        .ok_or_else(|| JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "MapSet",
        })
}

fn map_delete_key_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    func.jit_metadata
        .get(pc)
        .and_then(crate::metadata::map_delete_key_layout_from_instruction)
        .ok_or_else(|| JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "MapDelete",
        })
}

fn ptr_value_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::PtrLayout { value_layout }) => Ok(value_layout.clone()),
        _ => Err(JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "PtrLayout",
        }),
    }
}

fn slot_elem_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::SlotLayout { elem_layout }) => Ok(elem_layout.clone()),
        _ => Err(JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "SlotLayout",
        }),
    }
}

fn call_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), JitMetadataError> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::CallLayout {
            arg_layout,
            ret_layout,
        }) => Ok((arg_layout.clone(), ret_layout.clone())),
        _ => Err(JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "CallLayout",
        }),
    }
}

fn call_extern_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), JitMetadataError> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::CallExternLayout {
            arg_layout,
            ret_layout,
        }) => Ok((arg_layout.clone(), ret_layout.clone())),
        _ => Err(JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "CallExternLayout",
        }),
    }
}

fn queue_elem_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::QueueLayout { elem_layout }) => Ok(elem_layout.clone()),
        _ => Err(JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "QueueLayout",
        }),
    }
}

fn map_iter_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), JitMetadataError> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::MapIterNext {
            key_layout,
            val_layout,
        }) => Ok((key_layout.clone(), val_layout.clone())),
        _ => Err(JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "MapIterNext",
        }),
    }
}

fn iface_assert_result_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, JitMetadataError> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::IfaceAssertLayout { result_layout }) => {
            Ok(result_layout.clone())
        }
        _ => Err(JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout: "IfaceAssertLayout",
        }),
    }
}

fn checked_slot_offset_for_verifier(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    offset: u16,
    access: &'static str,
) -> Result<u16, JitMetadataError> {
    start
        .checked_add(offset)
        .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start,
            count: offset.saturating_add(1),
            access,
        })
}

fn verify_ptr_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    ptr_slot: u16,
    src_slot: u16,
    flags: u8,
) -> Result<(), JitMetadataError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    if value_layout.len() != 1 {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "PtrSet metadata layout slots {} do not match encoded count 1",
                value_layout.len()
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        ptr_slot,
        &[SlotType::GcRef],
        "PtrSet pointer",
    )?;
    let source = local_layout(func, pc, src_slot, 1, "PtrSet source")?;
    verify_raw_or_exact_layout_matches(func, pc, opcode, src_slot, &value_layout, "PtrSet source")?;
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
    verify_local_layout_matches(func, pc, opcode, src_start, expected, "GlobalSet source")
}

fn verify_global_get_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
    opcode: Opcode,
    global_start: u16,
    dst_start: u16,
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
                access: "global read",
            })? as usize;
    if end > globals.len() {
        return Err(JitMetadataError::GlobalSlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: global_start,
            global_slots: globals.len(),
            access: "read",
        });
    }
    let expected = &globals[global_start as usize..end];
    verify_structural_layout(func, pc, opcode, global_start, expected, "GlobalGet source")?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        dst_start,
        expected,
        "GlobalGet destination",
    )
}

fn verify_ptr_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    dst_start: u16,
    ptr_slot: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    if value_layout.len() != count as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "PtrGet metadata layout slots {} do not match encoded count {}",
                value_layout.len(),
                count
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        ptr_slot,
        &[SlotType::GcRef],
        "PtrGet pointer",
    )?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        dst_start,
        &value_layout,
        "PtrGet destination",
    )
}

fn verify_slot_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    dst_start: u16,
    base_start: u16,
    index_slot: u16,
    count: u16,
) -> Result<(), JitMetadataError> {
    let elem_layout = slot_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != count as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "SlotGet metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                count
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        index_slot,
        &[SlotType::Value],
        "SlotGet index",
    )?;
    if count == 1 {
        verify_raw_or_exact_layout_matches(
            func,
            pc,
            opcode,
            base_start,
            &elem_layout,
            "SlotGet element",
        )?;
        verify_raw_or_exact_layout_matches(
            func,
            pc,
            opcode,
            dst_start,
            &elem_layout,
            "SlotGet destination",
        )
    } else {
        verify_local_layout_matches(
            func,
            pc,
            opcode,
            base_start,
            &elem_layout,
            "SlotGet element",
        )?;
        verify_local_layout_matches(
            func,
            pc,
            opcode,
            dst_start,
            &elem_layout,
            "SlotGet destination",
        )
    }
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
    let elem_layout = slot_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != count as usize {
        return Err(JitMetadataError::CallShapeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            detail: format!(
                "SlotSet metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                count
            ),
        });
    }
    verify_layout(
        func,
        pc,
        opcode,
        index_slot,
        &[SlotType::Value],
        "SlotSet index",
    )?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        base_start,
        &elem_layout,
        "SlotSet element",
    )?;
    verify_local_layout_matches(func, pc, opcode, src_start, &elem_layout, "SlotSet source")
}

fn verify_copy_n_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let count = inst.copy_n_count();
    let source = local_layout(func, pc, inst.b, count, "CopyN source")?;
    verify_structural_layout(func, pc, opcode, inst.b, source, "CopyN source")?;
    verify_local_layout_matches(func, pc, opcode, inst.a, source, "CopyN destination")
}

fn verify_copy_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: vo_runtime::instruction::Instruction,
) -> Result<(), JitMetadataError> {
    let source = local_layout(func, pc, inst.b, 1, "Copy source")?;
    let actual = local_layout(func, pc, inst.a, 1, "Copy destination")?;
    if actual == source {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access: "Copy destination",
            slot: inst.a,
            expected: source.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

fn verify_iface_assign_source(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    src_slot: u16,
    vk: ValueKind,
) -> Result<(), JitMetadataError> {
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

fn verify_value_range(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        start,
        &vec![SlotType::Value; count as usize],
        access,
    )
}

fn verify_local_layout_matches(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    verify_structural_layout(func, pc, opcode, start, actual, access)?;
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

fn verify_raw_or_exact_layout_matches(
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
        verify_structural_layout(func, pc, opcode, start, actual, access)?;
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

fn verify_one_of_single_slot_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected_any: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, 1, access)?;
    if expected_any.contains(&actual[0]) {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected_any.to_vec(),
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
    match metadata {
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
                    elem_bytes: *elem_bytes,
                });
            };
            if func.code[pc].flags != 0 {
                let from_flags = crate::metadata::elem_layout_from_flags(func.code[pc].flags);
                if from_flags.bytes != layout.bytes
                    || from_flags.needs_sign_extend != layout.needs_sign_extend
                {
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
        JitInstructionMetadata::PtrLayout { .. } => matches!(
            opcode,
            Opcode::PtrGet | Opcode::PtrGetN | Opcode::PtrSet | Opcode::PtrSetN
        )
        .then_some(())
        .ok_or_else(|| wrong_kind(func, pc, opcode, "PtrLayout")),
        JitInstructionMetadata::SlotLayout { .. } => matches!(
            opcode,
            Opcode::SlotGet | Opcode::SlotGetN | Opcode::SlotSet | Opcode::SlotSetN
        )
        .then_some(())
        .ok_or_else(|| wrong_kind(func, pc, opcode, "SlotLayout")),
        JitInstructionMetadata::CallLayout { .. } => (matches!(
            opcode,
            Opcode::CallClosure | Opcode::CallIface | Opcode::GoIsland
        ) || (matches!(
            opcode,
            Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush
        ) && (flags & 1) != 0))
            .then_some(())
            .ok_or_else(|| wrong_kind(func, pc, opcode, "CallLayout")),
        JitInstructionMetadata::CallExternLayout { .. } => (opcode == Opcode::CallExtern)
            .then_some(())
            .ok_or_else(|| wrong_kind(func, pc, opcode, "CallExternLayout")),
        JitInstructionMetadata::QueueLayout { .. } => matches!(
            opcode,
            Opcode::QueueSend | Opcode::QueueRecv | Opcode::SelectSend | Opcode::SelectRecv
        )
        .then_some(())
        .ok_or_else(|| wrong_kind(func, pc, opcode, "QueueLayout")),
        JitInstructionMetadata::MapIterNext { .. } => (opcode == Opcode::MapIterNext)
            .then_some(())
            .ok_or_else(|| wrong_kind(func, pc, opcode, "MapIterNext")),
        JitInstructionMetadata::IfaceAssertLayout { .. } => (opcode == Opcode::IfaceAssert)
            .then_some(())
            .ok_or_else(|| wrong_kind(func, pc, opcode, "IfaceAssertLayout")),
        JitInstructionMetadata::LegacyMapGet { .. } => Err(unsupported_legacy_metadata(
            func,
            pc,
            opcode,
            "LegacyMapGet",
        )),
        JitInstructionMetadata::LegacyMapSet { .. } => Err(unsupported_legacy_metadata(
            func,
            pc,
            opcode,
            "LegacyMapSet",
        )),
        JitInstructionMetadata::LegacyMapDelete { .. } => Err(unsupported_legacy_metadata(
            func,
            pc,
            opcode,
            "LegacyMapDelete",
        )),
        JitInstructionMetadata::LoopEnd { end_pc } => {
            if opcode != Opcode::Hint || flags != HINT_LOOP {
                return Err(wrong_kind(func, pc, opcode, "LoopEnd"));
            }
            let end_pc = *end_pc as usize;
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
        Opcode::ForLoop => forloop_target_i64(end_pc, inst.c as i16) == begin_pc as i64,
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
        | Opcode::SliceAppend => Some("ElemLayout"),
        Opcode::PtrGet | Opcode::PtrGetN | Opcode::PtrSet | Opcode::PtrSetN => Some("PtrLayout"),
        Opcode::SlotGet | Opcode::SlotGetN | Opcode::SlotSet | Opcode::SlotSetN => {
            Some("SlotLayout")
        }
        Opcode::CallClosure | Opcode::CallIface | Opcode::GoIsland => Some("CallLayout"),
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush if (flags & 1) != 0 => {
            Some("CallLayout")
        }
        Opcode::CallExtern => Some("CallExternLayout"),
        Opcode::QueueSend | Opcode::QueueRecv | Opcode::SelectSend | Opcode::SelectRecv => {
            Some("QueueLayout")
        }
        Opcode::MapIterNext => Some("MapIterNext"),
        Opcode::IfaceAssert => Some("IfaceAssertLayout"),
        Opcode::MapGet => Some("MapGet"),
        Opcode::MapSet => Some("MapSet"),
        Opcode::MapDelete => Some("MapDelete"),
        _ => None,
    }
}

#[cfg(test)]
pub(crate) fn jit_metadata_requirement(opcode: Opcode, _flags: u8) -> JitMetadataRequirement {
    match opcode {
        Opcode::ArrayNew
        | Opcode::ArrayGet
        | Opcode::ArraySet
        | Opcode::ArrayAddr
        | Opcode::SliceNew
        | Opcode::SliceGet
        | Opcode::SliceSet
        | Opcode::SliceAddr
        | Opcode::SliceAppend => JitMetadataRequirement::ElemLayoutWhenFlagsZero,
        Opcode::PtrGet | Opcode::PtrGetN | Opcode::PtrSet | Opcode::PtrSetN => {
            JitMetadataRequirement::PtrLayout
        }
        Opcode::SlotGet | Opcode::SlotGetN | Opcode::SlotSet | Opcode::SlotSetN => {
            JitMetadataRequirement::SlotLayout
        }
        Opcode::CallClosure | Opcode::CallIface | Opcode::GoIsland => {
            JitMetadataRequirement::CallLayout
        }
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush => {
            JitMetadataRequirement::CallLayoutWhenClosureShape
        }
        Opcode::CallExtern => JitMetadataRequirement::CallExternLayout,
        Opcode::QueueSend | Opcode::QueueRecv | Opcode::SelectSend | Opcode::SelectRecv => {
            JitMetadataRequirement::QueueLayout
        }
        Opcode::MapIterNext => JitMetadataRequirement::MapIterNext,
        Opcode::MapGet => JitMetadataRequirement::MapGet,
        Opcode::MapSet => JitMetadataRequirement::MapSet,
        Opcode::MapDelete => JitMetadataRequirement::MapDelete,
        Opcode::IfaceAssert => JitMetadataRequirement::IfaceAssertLayout,
        Opcode::Hint => JitMetadataRequirement::LoopEndForHintLoop,
        _ => JitMetadataRequirement::None,
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

fn unsupported_legacy_metadata(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    metadata: &'static str,
) -> JitMetadataError {
    JitMetadataError::UnsupportedLegacyMetadata {
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
        let has_defer = code
            .iter()
            .any(|inst| matches!(inst.opcode(), Opcode::DeferPush | Opcode::ErrDeferPush));
        FunctionDef {
            name: "verify".to_string(),
            param_count: 0,
            param_slots,
            local_slots,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
            ret_slots,
            ret_slot_types: vec![SlotType::Value; ret_slots as usize],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot,
            has_defer,
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

    fn refresh_function_metadata(func: &mut FunctionDef) {
        func.local_slots = func.slot_types.len() as u16;
        func.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&func.slot_types);
        func.borrowed_scan_slots_prefix =
            FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
        func.has_defer = func
            .code
            .iter()
            .any(|inst| matches!(inst.opcode(), Opcode::DeferPush | Opcode::ErrDeferPush));
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&func.code);
        func.has_calls = has_calls;
        func.has_call_extern = has_call_extern;
    }

    #[test]
    fn rejects_jump_target_out_of_bounds() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![jump(2)],
            vec![JitInstructionMetadata::None],
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidBranchTarget {
                opcode: Opcode::Jump,
                target: 2,
                code_len: 1,
                ..
            })
        ));
    }

    #[test]
    fn rejects_forloop_target_out_of_bounds_without_wrapping() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(
                Opcode::ForLoop,
                0,
                0,
                1,
                (-3i16) as u16,
            )],
            vec![JitInstructionMetadata::None],
            2,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidBranchTarget {
                opcode: Opcode::ForLoop,
                target: -2,
                code_len: 1,
                ..
            })
        ));
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
    fn rejects_copyn_source_destination_layout_mismatch() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::CopyN, 0, 2, 2)],
            vec![JitInstructionMetadata::None],
            vec![
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::GcRef,
            ],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::CopyN,
                ..
            })
        ));
    }

    #[test]
    fn rejects_function_def_invariant_mismatches_before_instruction_review() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_shape(
            vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Value],
            2,
            0,
            -1,
        ));
        module.functions[0].local_slots = 3;

        let err = verify_jit_metadata(&module.functions[0], &module)
            .expect_err("local_slots/slot_types drift must fail before JIT lowering");
        assert!(
            matches!(err, JitMetadataError::FunctionInvariant { .. }),
            "unexpected error: {err:?}"
        );
    }

    #[test]
    fn rejects_function_def_derived_flag_drift() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::CallExtern, 0, 0, 0)],
            vec![JitInstructionMetadata::CallExternLayout {
                arg_layout: Vec::new(),
                ret_layout: Vec::new(),
            }],
            vec![SlotType::Value],
            0,
        ));
        module.functions[0].has_call_extern = false;

        let err = verify_jit_metadata(&module.functions[0], &module)
            .expect_err("derived call flags must be recomputed at the verifier boundary");
        assert!(
            matches!(err, JitMetadataError::FunctionInvariant { .. }),
            "unexpected error: {err:?}"
        );
    }

    #[test]
    fn rejects_single_copy_source_destination_layout_mismatch() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::Copy, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::Value],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::Copy,
                ..
            })
        ));
    }

    #[test]
    fn rejects_dynamic_closure_and_island_operands_outside_gcref_slots() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::CallClosure, 0, 1, 0)],
            vec![JitInstructionMetadata::CallLayout {
                arg_layout: Vec::new(),
                ret_layout: Vec::new(),
            }],
            vec![SlotType::Value],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::GoIsland, 1, 0, 1, 2)],
            vec![JitInstructionMetadata::CallLayout {
                arg_layout: vec![SlotType::Value],
                ret_layout: Vec::new(),
            }],
            vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::CallClosure,
                access: "CallClosure callee",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::GoIsland,
                access: "GoIsland island",
                ..
            })
        ));
    }

    #[test]
    fn rejects_integer_control_and_len_ops_on_root_typed_slots() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::JumpIf, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(
                Opcode::ForLoop,
                0,
                0,
                1,
                (-1i16) as u16,
            )],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::Value],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::MapLen, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::GcRef],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::JumpIf,
                access: "JumpIf condition",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::ForLoop,
                access: "ForLoop index",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[2], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::MapLen,
                access: "MapLen destination",
                ..
            })
        ));
    }

    #[test]
    fn rejects_iface_assign_invalid_flags_and_reference_source_drift() {
        let mut module = VoModule::new("verify".to_string());
        module.constants.push(Constant::Int(0));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::IfaceAssign, 250, 0, 2, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Interface0, SlotType::Interface1, SlotType::Value],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(
                Opcode::IfaceAssign,
                ValueKind::String as u8,
                0,
                2,
                0,
            )],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Interface0, SlotType::Interface1, SlotType::Value],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidValueKind {
                opcode: Opcode::IfaceAssign,
                raw: 250,
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::IfaceAssign,
                access: "IfaceAssign source",
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
        module.functions[0].ret_slot_types = vec![SlotType::GcRef, SlotType::Value];

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
            vec![JitInstructionMetadata::CallLayout {
                arg_layout: vec![SlotType::Interface0],
                ret_layout: Vec::new(),
            }],
            vec![SlotType::GcRef, SlotType::Interface1],
            0,
        );
        let iface = make_func_with_slot_types(
            vec![Instruction::new(Opcode::CallIface, 0, 2, 0)],
            vec![JitInstructionMetadata::CallLayout {
                arg_layout: Vec::new(),
                ret_layout: Vec::new(),
            }],
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
            Err(JitMetadataError::InvalidInterfaceLayout {
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
    fn rejects_shared_closure_call_without_precise_arg_layout() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::GoStart, 1, 0, 1, 2)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::Value, SlotType::GcRef],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::DeferPush, 1, 0, 1, 2)],
            vec![JitInstructionMetadata::CallLayout {
                arg_layout: vec![SlotType::Value, SlotType::GcRef],
                ret_layout: Vec::new(),
            }],
            vec![SlotType::GcRef, SlotType::Value, SlotType::Value],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::MissingLayout {
                opcode: Opcode::GoStart,
                layout: "CallLayout",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::DeferPush,
                access: "closure call args",
                ..
            })
        ));
    }

    #[test]
    fn accepts_shared_closure_call_with_precise_arg_layout() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::GoStart, 1, 0, 1, 2)],
            vec![JitInstructionMetadata::CallLayout {
                arg_layout: vec![SlotType::Value, SlotType::GcRef],
                ret_layout: Vec::new(),
            }],
            vec![SlotType::GcRef, SlotType::Value, SlotType::GcRef],
            0,
        ));

        verify_jit_metadata(&module.functions[0], &module).unwrap();
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
            vec![JitInstructionMetadata::CallExternLayout {
                arg_layout: Vec::new(),
                ret_layout: Vec::new(),
            }],
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
    fn rejects_scalar_slot_contract_mismatches() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::AddI, 0, 1, 2)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Float, SlotType::Value, SlotType::Value],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::AddF, 0, 1, 2)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::Float, SlotType::Float],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::EqF, 0, 1, 2)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Float, SlotType::Float, SlotType::Float],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::IndexCheck, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Float, SlotType::Value],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::AddI,
                access: "AddI destination",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::AddF,
                access: "AddF destination",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[2], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::EqF,
                access: "EqF destination",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[3], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::IndexCheck,
                access: "IndexCheck index",
                ..
            })
        ));
    }

    #[test]
    fn rejects_conversion_slot_contract_mismatches() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::ConvI2F, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef, SlotType::Value],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::ConvF2I, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Value, SlotType::GcRef],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::ConvF64F32, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Float, SlotType::GcRef],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::ConvF32F64, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Float, SlotType::GcRef],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::Trunc, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Float, SlotType::Value],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::ConvI2F,
                access: "ConvI2F destination",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::ConvF2I,
                access: "ConvF2I source",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[2], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::ConvF64F32,
                access: "ConvF64F32 source",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[3], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::ConvF32F64,
                access: "ConvF32F64 source",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[4], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::Trunc,
                access: "Trunc destination",
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
            vec![JitInstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::GcRef],
            }],
            vec![SlotType::GcRef, SlotType::GcRef],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::SlotSetN, 2, 0, 2, 3)],
            vec![JitInstructionMetadata::SlotLayout {
                elem_layout: vec![SlotType::Value, SlotType::Value],
            }],
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
    fn verifier_slot_contract_has_no_wildcard_allowlist() {
        let src = include_str!("verifier.rs");
        let start = src.find("fn verify_slot_contract").unwrap();
        let end = src[start..]
            .find("\nfn verify_binary_slot_contract")
            .map(|offset| start + offset)
            .unwrap();
        let body = &src[start..end];

        assert!(
            !body.contains("_ => Ok(())"),
            "verify_slot_contract must explicitly name every opcode it accepts; wildcard Ok lets new opcodes bypass JIT contract review"
        );
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
            vec![JitInstructionMetadata::MapDelete {
                key_layout: vec![SlotType::Value],
            }],
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::WrongMetadataKind { .. })
        ));
    }

    #[test]
    fn accepts_zero_loads_into_gcref_slots_for_nil_references() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::LoadInt, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef],
            0,
        ));
        module.constants.push(Constant::Nil);
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::LoadConst, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef],
            0,
        ));

        verify_jit_metadata(&module.functions[0], &module).expect("zero LoadInt can nil a GcRef");
        verify_jit_metadata(&module.functions[1], &module).expect("nil LoadConst can nil a GcRef");
    }

    #[test]
    fn accepts_raw_reference_slots_for_equality_and_bit_tests() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![
                Instruction::new(Opcode::EqI, 0, 1, 2),
                Instruction::new(Opcode::And, 3, 4, 5),
            ],
            vec![JitInstructionMetadata::None, JitInstructionMetadata::None],
            vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Interface0,
                SlotType::Value,
            ],
            0,
        ));

        verify_jit_metadata(&module.functions[0], &module)
            .expect("raw reference/header equality and bit tests are valid lowering inputs");
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
    fn rejects_missing_load_const_constant() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::LoadConst, 0, 7, 0)],
            vec![JitInstructionMetadata::None],
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::MissingConstant { const_id: 7, .. })
        ));
    }

    #[test]
    fn rejects_string_constant_loaded_without_str_new() {
        let mut module = VoModule::new("verify".to_string());
        module.constants.push(Constant::String("bad".to_string()));
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::LoadConst, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::ConstantKindMismatch {
                opcode: Opcode::LoadConst,
                expected,
                actual: "String",
                ..
            }) if expected.contains("StrNew")
        ));
    }

    #[test]
    fn rejects_str_new_missing_or_non_string_constant() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::StrNew, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef],
            0,
        ));
        module.constants.push(Constant::Int(42));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::StrNew, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::MissingConstant { const_id: 1, .. })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::ConstantKindMismatch {
                opcode: Opcode::StrNew,
                expected: "String",
                actual: "Int",
                ..
            })
        ));
    }

    #[test]
    fn rejects_iface_assign_missing_or_non_int_metadata_constant() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::IfaceAssign, 2, 0, 0, 9)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Interface0, SlotType::Interface1],
            0,
        ));
        module
            .constants
            .push(Constant::String("not metadata".to_string()));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::IfaceAssign, 2, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Interface0, SlotType::Interface1],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::MissingConstant { const_id: 9, .. })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::ConstantKindMismatch {
                opcode: Opcode::IfaceAssign,
                expected: "Int",
                actual: "String",
                ..
            })
        ));
    }

    #[test]
    fn rejects_global_get_range_out_of_bounds() {
        let mut module = VoModule::new("verify".to_string());
        module.globals.push(GlobalDef {
            name: "g".to_string(),
            slots: 1,
            value_kind: 0,
            meta_id: 0,
            slot_types: vec![SlotType::Value],
        });
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::GlobalGet, 0, 1, 0)],
            vec![JitInstructionMetadata::None],
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::GlobalSlotOutOfRange { access: "read", .. })
        ));
    }

    #[test]
    fn rejects_global_get_destination_layout_mismatch() {
        let mut module = VoModule::new("verify".to_string());
        module.globals.push(GlobalDef {
            name: "g".to_string(),
            slots: 2,
            value_kind: 0,
            meta_id: 0,
            slot_types: vec![SlotType::GcRef, SlotType::Float],
        });
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::GlobalGetN, 2, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Value, SlotType::Float],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::GlobalGetN,
                access: "GlobalGet destination",
                expected,
                actual,
                ..
            }) if expected == vec![SlotType::GcRef, SlotType::Float]
                && actual == vec![SlotType::Value, SlotType::Float]
        ));
    }

    #[test]
    fn rejects_global_set_source_layout_mismatch() {
        let mut module = VoModule::new("verify".to_string());
        module.globals.push(GlobalDef {
            name: "g".to_string(),
            slots: 2,
            value_kind: 0,
            meta_id: 0,
            slot_types: vec![SlotType::GcRef, SlotType::Float],
        });
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::GlobalSetN, 2, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Value, SlotType::Float],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::GlobalSetN,
                access: "GlobalSet source",
                expected,
                actual,
                ..
            }) if expected == vec![SlotType::GcRef, SlotType::Float]
                && actual == vec![SlotType::Value, SlotType::Float]
        ));
    }

    #[test]
    fn rejects_closure_new_missing_function() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::ClosureNew, 0, 0, 3, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::GcRef],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::MissingFunction { callee_id: 3, .. })
        ));
    }

    #[test]
    fn rejects_static_go_and_defer_missing_function() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::GoStart, 0, 4, 0, 0)],
            vec![JitInstructionMetadata::None],
            1,
        ));
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::DeferPush, 0, 4, 0, 0)],
            vec![JitInstructionMetadata::None],
            1,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::MissingFunction { callee_id: 4, .. })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::MissingFunction { callee_id: 4, .. })
        ));
    }

    #[test]
    fn rejects_slot_get_contract_mismatches() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::SlotGetN, 2, 2, 0, 1)],
            vec![JitInstructionMetadata::SlotLayout {
                elem_layout: vec![SlotType::Interface0, SlotType::Interface1],
            }],
            vec![
                SlotType::Interface0,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::SlotGetN,
                ..
            })
        ));
    }

    #[test]
    fn accepts_single_slot_raw_transfers_from_interface_pair_slots() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::SlotGet, 2, 0, 1)],
            vec![JitInstructionMetadata::SlotLayout {
                elem_layout: vec![SlotType::Interface0],
            }],
            vec![SlotType::Interface0, SlotType::Value, SlotType::Interface0],
            0,
        ));

        assert!(verify_jit_metadata(&module.functions[0], &module).is_ok());
    }

    #[test]
    fn rejects_pointer_reads_from_non_root_slots() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::PtrGet, 0, 1, 0)],
            vec![JitInstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::Value],
            }],
            vec![SlotType::Value, SlotType::Value],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::PtrGet,
                access: "PtrGet pointer",
                ..
            })
        ));
    }

    #[test]
    fn accepts_pointer_reads_from_gc_root_slots() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::PtrGet, 0, 1, 0)],
            vec![JitInstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::Value],
            }],
            vec![SlotType::Value, SlotType::GcRef],
            0,
        ));

        assert!(verify_jit_metadata(&module.functions[0], &module).is_ok());
    }

    #[test]
    fn rejects_interior_pointer_destinations_outside_gc_root_slots() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::PtrAdd, 0, 1, 2)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::SliceAddr, 0x08, 0, 1, 2)],
            vec![JitInstructionMetadata::ElemLayout {
                elem_bytes: 8,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Value],
            }],
            vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::PtrAdd,
                access: "PtrAdd destination",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::SliceAddr,
                access: "SliceAddr destination",
                ..
            })
        ));
    }

    #[test]
    fn accepts_zero_elem_layout_metadata() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2)],
            vec![JitInstructionMetadata::ElemLayout {
                elem_bytes: 0,
                needs_sign_extend: false,
                slot_layout: Vec::new(),
            }],
            vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
            ],
            0,
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
                slot_layout: Vec::new(),
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
                slot_layout: vec![SlotType::Value],
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
            Err(JitMetadataError::SlotOutOfRange {
                access: "Copy source",
                ..
            })
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
                slot_layout: vec![SlotType::Value; 3],
            }],
            4,
        ));
        module.functions[0].slot_types[0] = SlotType::GcRef;
        refresh_function_metadata(&mut module.functions[0]);

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotRangeOverflow {
                access: "ArrayNew length/elem_bytes",
                ..
            })
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
    fn rejects_indexed_metadata_layout_slot_mismatches() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::ArrayGet, 0, 0, 2, 3)],
            vec![JitInstructionMetadata::ElemLayout {
                elem_bytes: 16,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Value; 2],
            }],
            vec![
                SlotType::Interface0,
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
            ],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::ArraySet, 0, 1, 4, 20)],
            vec![JitInstructionMetadata::ElemLayout {
                elem_bytes: 16,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Value; 2],
            }],
            vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
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
                opcode: Opcode::ArrayGet,
                access: "ArrayGet destination",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::ArraySet,
                access: "ArraySet source",
                ..
            })
        ));
    }

    #[test]
    fn rejects_map_metadata_layout_slot_mismatches() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::MapGet, 8, 1, 4)],
            vec![JitInstructionMetadata::MapGet {
                key_layout: vec![SlotType::Interface0, SlotType::Interface1],
                val_layout: vec![SlotType::Value, SlotType::GcRef],
                has_ok: true,
            }],
            vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Interface0,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::MapSet, 1, 4, 8)],
            vec![JitInstructionMetadata::MapSet {
                key_layout: vec![SlotType::Value],
                val_layout: vec![SlotType::Interface0, SlotType::Interface1],
            }],
            vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Interface0,
                SlotType::Value,
            ],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::MapDelete, 1, 4, 0)],
            vec![JitInstructionMetadata::MapDelete {
                key_layout: vec![SlotType::Interface0, SlotType::Interface1],
            }],
            vec![
                SlotType::Value,
                SlotType::GcRef,
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
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::MapGet,
                access: "MapGet key",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::MapSet,
                access: "MapSet value",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[2], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::MapDelete,
                access: "MapDelete key",
                ..
            })
        ));
    }

    #[test]
    fn rejects_queue_select_iface_contract_mismatches() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::QueueSend, 2, 0, 1, 0)],
            vec![JitInstructionMetadata::QueueLayout {
                elem_layout: vec![SlotType::Interface0, SlotType::Interface1],
            }],
            vec![SlotType::GcRef, SlotType::Interface0, SlotType::Value],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::QueueRecv, 3, 0, 2, 0)],
            vec![JitInstructionMetadata::QueueLayout {
                elem_layout: vec![SlotType::GcRef],
            }],
            vec![SlotType::GcRef, SlotType::Interface1, SlotType::GcRef],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(Opcode::SelectSend, 2, 0, 1, 0)],
            vec![JitInstructionMetadata::QueueLayout {
                elem_layout: vec![SlotType::Interface0, SlotType::Interface1],
            }],
            vec![SlotType::GcRef, SlotType::Interface0, SlotType::Value],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(
                Opcode::IfaceAssert,
                0b0010_1001,
                0,
                1,
                0,
            )],
            vec![JitInstructionMetadata::IfaceAssertLayout {
                result_layout: vec![SlotType::Interface0, SlotType::Interface1],
            }],
            vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
            ],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::IfaceEq, 0, 1, 3)],
            vec![JitInstructionMetadata::None],
            vec![
                SlotType::Value,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Interface0,
                SlotType::Interface1,
            ],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::new(Opcode::Recover, 0, 0, 0)],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Value, SlotType::GcRef],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::QueueSend,
                access: "QueueSend value",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::QueueRecv,
                access: "QueueRecv ok",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[2], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::SelectSend,
                access: "SelectSend value",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[3], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::IfaceAssert,
                access: "IfaceAssert source",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[4], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::IfaceEq,
                access: "IfaceEq lhs",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[5], &module),
            Err(JitMetadataError::InvalidInterfaceLayout {
                opcode: Opcode::Recover,
                access: "Recover destination",
                ..
            })
        ));
    }

    #[test]
    fn rejects_iface_assert_missing_or_mismatched_result_layout() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(
                Opcode::IfaceAssert,
                1 << 3,
                0,
                1,
                0,
            )],
            vec![JitInstructionMetadata::None],
            vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(
                Opcode::IfaceAssert,
                1 << 3,
                0,
                1,
                0,
            )],
            vec![JitInstructionMetadata::IfaceAssertLayout {
                result_layout: vec![SlotType::Value, SlotType::Value],
            }],
            vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
            0,
        ));
        module.functions.push(make_func_with_slot_types(
            vec![Instruction::with_flags(
                Opcode::IfaceAssert,
                1 << 3,
                0,
                1,
                0,
            )],
            vec![JitInstructionMetadata::IfaceAssertLayout {
                result_layout: vec![SlotType::GcRef],
            }],
            vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
            0,
        ));

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::MissingLayout {
                opcode: Opcode::IfaceAssert,
                layout: "IfaceAssertLayout",
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[1], &module),
            Err(JitMetadataError::CallShapeMismatch {
                opcode: Opcode::IfaceAssert,
                ..
            })
        ));
        assert!(matches!(
            verify_jit_metadata(&module.functions[2], &module),
            Err(JitMetadataError::SlotTypeMismatch {
                opcode: Opcode::IfaceAssert,
                access: "IfaceAssert destination",
                ..
            })
        ));
    }

    #[test]
    fn rejects_wrong_slice_addr_layout_metadata_kind() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::with_flags(Opcode::SliceAddr, 0, 0, 1, 2)],
            vec![JitInstructionMetadata::MapDelete {
                key_layout: vec![SlotType::Value],
            }],
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
                slot_layout: vec![SlotType::Value],
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
        module.functions.push(make_func_with_slot_types(
            vec![
                Instruction::with_flags(Opcode::ArrayAddr, 0x82, 0, 1, 2),
                Instruction::with_flags(Opcode::SliceAddr, 0x44, 3, 4, 5),
            ],
            vec![
                JitInstructionMetadata::ElemLayout {
                    elem_bytes: 2,
                    needs_sign_extend: true,
                    slot_layout: vec![SlotType::Value],
                },
                JitInstructionMetadata::ElemLayout {
                    elem_bytes: 4,
                    needs_sign_extend: false,
                    slot_layout: vec![SlotType::Float],
                },
            ],
            vec![
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::Value,
            ],
            0,
        ));

        verify_jit_metadata(&module.functions[0], &module).unwrap();
    }

    #[test]
    fn rejects_map_get_output_range_overflow() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::MapGet, 0, 1, 2)],
            vec![JitInstructionMetadata::MapGet {
                key_layout: vec![SlotType::Value],
                val_layout: vec![SlotType::Value; u16::MAX as usize],
                has_ok: true,
            }],
            4,
        ));
        module.functions[0].slot_types[1] = SlotType::GcRef;
        refresh_function_metadata(&mut module.functions[0]);

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::SlotRangeOverflow {
                access: "MapGet destination",
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
                key_layout: vec![SlotType::Value, SlotType::Value],
                val_layout: vec![SlotType::Value, SlotType::Value],
                has_ok: true,
            }],
            8,
        ));
        module.functions[0].slot_types[1] = SlotType::GcRef;
        refresh_function_metadata(&mut module.functions[0]);

        verify_jit_metadata(&module.functions[0], &module).expect("valid metadata");
    }

    #[test]
    fn rejects_legacy_map_metadata_without_precise_slot_layouts() {
        let mut module = VoModule::new("verify".to_string());
        module.functions.push(make_func(
            vec![Instruction::new(Opcode::MapSet, 1, 2, 4)],
            vec![JitInstructionMetadata::LegacyMapSet {
                key_slots: 1,
                val_slots: 1,
            }],
            6,
        ));
        module.functions[0].slot_types[1] = SlotType::GcRef;
        refresh_function_metadata(&mut module.functions[0]);

        assert!(matches!(
            verify_jit_metadata(&module.functions[0], &module),
            Err(JitMetadataError::UnsupportedLegacyMetadata {
                opcode: Opcode::MapSet,
                metadata: "LegacyMapSet",
                ..
            })
        ));
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
