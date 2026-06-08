use std::fmt;

use vo_runtime::bytecode::FunctionDef;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

use crate::effects::{EffectError, SlotRangeError};
use vo_common_core::verifier::ModuleVerificationError;

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
    InvalidInstructionFlags {
        func: String,
        pc: usize,
        opcode: Opcode,
        flags: u8,
        allowed: u8,
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
            Self::InvalidInstructionFlags {
                func,
                pc,
                opcode,
                flags,
                allowed,
            } => write!(
                f,
                "invalid flags 0x{flags:02x} for {opcode:?} in {func} at pc {pc}; allowed mask is 0x{allowed:02x}"
            ),
        }
    }
}

impl std::error::Error for JitMetadataError {}

impl From<ModuleVerificationError> for JitMetadataError {
    fn from(err: ModuleVerificationError) -> Self {
        match err {
            ModuleVerificationError::LengthMismatch {
                func,
                code_len,
                metadata_len,
            } => Self::LengthMismatch {
                func,
                code_len,
                metadata_len,
            },
            ModuleVerificationError::InvalidOpcode { func, pc, raw } => {
                Self::InvalidOpcode { func, pc, raw }
            }
            ModuleVerificationError::MissingLayout {
                func,
                pc,
                opcode,
                layout,
            } => Self::MissingLayout {
                func,
                pc,
                opcode,
                layout,
            },
            ModuleVerificationError::MissingExtern {
                func,
                pc,
                extern_id,
            } => Self::MissingExtern {
                func,
                pc,
                extern_id,
            },
            ModuleVerificationError::MissingConstant { func, pc, const_id } => {
                Self::MissingConstant { func, pc, const_id }
            }
            ModuleVerificationError::ConstantKindMismatch {
                func,
                pc,
                opcode,
                const_id,
                expected,
                actual,
            } => Self::ConstantKindMismatch {
                func,
                pc,
                opcode,
                const_id,
                expected,
                actual,
            },
            ModuleVerificationError::InvalidBranchTarget {
                func,
                pc,
                opcode,
                target,
                code_len,
            } => Self::InvalidBranchTarget {
                func,
                pc,
                opcode,
                target,
                code_len,
            },
            ModuleVerificationError::SlotRangeOverflow {
                func,
                pc,
                start,
                count,
                access,
            } => Self::SlotRangeOverflow {
                func,
                pc,
                start,
                count,
                access: normalize_common_range_access(access),
            },
            ModuleVerificationError::SlotOutOfRange {
                func,
                pc,
                slot,
                local_slots,
                access,
            } => Self::SlotOutOfRange {
                func,
                pc,
                slot,
                local_slots,
                access: normalize_common_range_access(access),
            },
            ModuleVerificationError::SlotTypeMismatch {
                func,
                pc,
                opcode,
                access,
                slot,
                expected,
                actual,
            } => Self::SlotTypeMismatch {
                func,
                pc,
                opcode,
                access,
                slot,
                expected,
                actual,
            },
            ModuleVerificationError::InvalidInterfaceLayout {
                func,
                pc,
                opcode,
                access,
                slot,
                actual,
            } => Self::InvalidInterfaceLayout {
                func,
                pc,
                opcode,
                access,
                slot,
                actual,
            },
            ModuleVerificationError::MissingFunction {
                func,
                pc,
                callee_id,
            } => Self::MissingFunction {
                func,
                pc,
                callee_id,
            },
            ModuleVerificationError::CallShapeMismatch {
                func,
                pc,
                opcode,
                detail,
            } => Self::CallShapeMismatch {
                func,
                pc,
                opcode,
                detail,
            },
            ModuleVerificationError::GlobalSlotOutOfRange {
                func,
                pc,
                slot,
                global_slots,
                access,
            } => Self::GlobalSlotOutOfRange {
                func,
                pc,
                slot,
                global_slots,
                access,
            },
            ModuleVerificationError::FunctionInvariant { func, detail } => {
                Self::FunctionInvariant { func, detail }
            }
            ModuleVerificationError::InvalidValueKind {
                func,
                pc,
                opcode,
                raw,
            } => Self::InvalidValueKind {
                func,
                pc,
                opcode,
                raw,
            },
            ModuleVerificationError::InvalidInstructionFlags {
                func,
                pc,
                opcode,
                flags,
                allowed,
            } => Self::InvalidInstructionFlags {
                func,
                pc,
                opcode,
                flags,
                allowed,
            },
            ModuleVerificationError::ModuleInvariant { detail }
            | ModuleVerificationError::GcLayout { detail } => Self::FunctionInvariant {
                func: "<module>".to_string(),
                detail,
            },
        }
    }
}

fn normalize_common_range_access(access: &'static str) -> &'static str {
    if access == "MapGet ok"
        || access.contains("destination")
        || access.contains("Destination")
        || access.contains("result")
        || access.contains("Result")
        || access.contains("returns")
        || access.contains("Return")
        || access.contains("ok")
    {
        "write"
    } else {
        "read"
    }
}

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
