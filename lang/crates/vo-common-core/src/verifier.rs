//! VM-shared bytecode and module verifier.
//!
//! This verifier owns checks that are true before any backend runs: module
//! indices, bytecode PC/slot ranges, call and extern shapes, GC layouts, write
//! barrier requirements, and derived `FunctionDef` fields. Strict JIT-specific
//! checks such as lowering capability, helper ABI, OSR metadata, side exits, and
//! frame materialization remain in `vo-jit`.

#[cfg(not(feature = "std"))]
use alloc::{
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{
    string::{String, ToString},
    vec,
    vec::Vec,
};

use core::fmt;

use crate::bytecode::{
    Constant, FunctionDef, JitInstructionMetadata, Module, ReturnFlags, TransferType,
};
use crate::instruction::{Instruction, Opcode, HINT_LOOP};
use crate::types::{SlotType, ValueKind};

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
pub enum ModuleVerificationError {
    ModuleInvariant {
        detail: String,
    },
    FunctionInvariant {
        func: String,
        detail: String,
    },
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
    MissingLayout {
        func: String,
        pc: usize,
        opcode: Opcode,
        layout: &'static str,
    },
    MissingFunction {
        func: String,
        pc: usize,
        callee_id: u32,
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
    GlobalSlotOutOfRange {
        func: String,
        pc: usize,
        slot: u16,
        global_slots: usize,
        access: &'static str,
    },
    CallShapeMismatch {
        func: String,
        pc: usize,
        opcode: Opcode,
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
    GcLayout {
        detail: String,
    },
}

impl fmt::Display for ModuleVerificationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ModuleInvariant { detail } => write!(f, "module invariant failed: {detail}"),
            Self::FunctionInvariant { func, detail } => {
                write!(f, "function metadata invariant failed in {func}: {detail}")
            }
            Self::LengthMismatch {
                func,
                code_len,
                metadata_len,
            } => write!(
                f,
                "instruction metadata length mismatch in {func}: code={code_len}, metadata={metadata_len}"
            ),
            Self::InvalidOpcode { func, pc, raw } => {
                write!(f, "invalid opcode {raw} in {func} at pc {pc}")
            }
            Self::MissingLayout {
                func,
                pc,
                opcode,
                layout,
            } => write!(
                f,
                "missing {layout} layout for {opcode:?} in {func} at pc {pc}"
            ),
            Self::MissingFunction {
                func,
                pc,
                callee_id,
            } => write!(
                f,
                "instruction references missing function {callee_id} in {func} at pc {pc}"
            ),
            Self::MissingExtern {
                func,
                pc,
                extern_id,
            } => write!(
                f,
                "CallExtern references missing extern {extern_id} in {func} at pc {pc}"
            ),
            Self::MissingConstant { func, pc, const_id } => write!(
                f,
                "instruction references missing constant {const_id} in {func} at pc {pc}"
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
                "constant kind mismatch for {opcode:?} in {func} at pc {pc}, const {const_id}: expected {expected}, actual {actual}"
            ),
            Self::InvalidBranchTarget {
                func,
                pc,
                opcode,
                target,
                code_len,
            } => write!(
                f,
                "branch target {target} for {opcode:?} in {func} at pc {pc} is outside code length {code_len}"
            ),
            Self::SlotRangeOverflow {
                func,
                pc,
                start,
                count,
                access,
            } => write!(
                f,
                "{access} slot range starting at {start} with {count} slots overflows u16 in {func} at pc {pc}"
            ),
            Self::SlotOutOfRange {
                func,
                pc,
                slot,
                local_slots,
                access,
            } => write!(
                f,
                "{access} slot {slot} out of range for {func} at pc {pc} (local_slots={local_slots})"
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
                "{access} slot layout mismatch for {opcode:?} in {func} at pc {pc}, slot {slot}: expected {expected:?}, actual {actual:?}"
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
                "{access} interface layout mismatch for {opcode:?} in {func} at pc {pc}, slot {slot}: expected [Interface0, Interface1], actual {actual:?}"
            ),
            Self::GlobalSlotOutOfRange {
                func,
                pc,
                slot,
                global_slots,
                access,
            } => write!(
                f,
                "{access} global slot {slot} out of range for {func} at pc {pc} (global_slots={global_slots})"
            ),
            Self::CallShapeMismatch {
                func,
                pc,
                opcode,
                detail,
            } => write!(
                f,
                "call shape mismatch for {opcode:?} in {func} at pc {pc}: {detail}"
            ),
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
            Self::GcLayout { detail } => write!(f, "{detail}"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ModuleVerificationError {}

#[derive(Debug, Clone, Copy)]
pub struct VerifiedModule<'m> {
    module: &'m Module,
}

impl<'m> VerifiedModule<'m> {
    pub fn module(self) -> &'m Module {
        self.module
    }

    pub fn matches(self, module: &Module) -> bool {
        core::ptr::eq(self.module, module)
    }
}

pub struct ModuleVerifier<'m> {
    module: &'m Module,
}

impl<'m> ModuleVerifier<'m> {
    pub fn new(module: &'m Module) -> Self {
        Self { module }
    }

    pub fn verify(self) -> Result<VerifiedModule<'m>, ModuleVerificationError> {
        verify_module_invariants(self.module)?;
        validate_module_gc_layout(self.module)?;
        for (idx, func) in self.module.functions.iter().enumerate() {
            verify_function_at(self.module, idx, func)?;
        }
        Ok(VerifiedModule {
            module: self.module,
        })
    }
}

pub fn verify_module(module: &Module) -> Result<VerifiedModule<'_>, ModuleVerificationError> {
    ModuleVerifier::new(module).verify()
}

pub fn verify_function(func: &FunctionDef, module: &Module) -> Result<(), ModuleVerificationError> {
    verify_function_common(func, module)
}

fn verify_module_invariants(module: &Module) -> Result<(), ModuleVerificationError> {
    let invariant = |detail: String| ModuleVerificationError::ModuleInvariant { detail };

    if module.entry_func as usize >= module.functions.len() {
        return Err(invariant(format!(
            "entry_func={} exceeds function count {}",
            module.entry_func,
            module.functions.len()
        )));
    }
    if module.island_init_func as usize >= module.functions.len() {
        return Err(invariant(format!(
            "island_init_func={} exceeds function count {}",
            module.island_init_func,
            module.functions.len()
        )));
    }
    for (idx, itab) in module.itabs.iter().enumerate() {
        for &func_id in &itab.methods {
            if func_id as usize >= module.functions.len() {
                return Err(invariant(format!(
                    "itab {idx} references missing function {func_id}"
                )));
            }
        }
    }
    for (idx, named) in module.named_type_metas.iter().enumerate() {
        for (name, method) in &named.methods {
            if method.func_id as usize >= module.functions.len() {
                return Err(invariant(format!(
                    "named_type_metas[{idx}] method {name} references missing function {}",
                    method.func_id
                )));
            }
        }
    }
    Ok(())
}

fn verify_function_at(
    module: &Module,
    idx: usize,
    func: &FunctionDef,
) -> Result<(), ModuleVerificationError> {
    let _ = idx;
    verify_function_common(func, module)
}

fn verify_function_common(
    func: &FunctionDef,
    module: &Module,
) -> Result<(), ModuleVerificationError> {
    verify_function_invariants(func)?;

    if func.code.len() != func.jit_metadata.len() {
        return Err(ModuleVerificationError::LengthMismatch {
            func: func.name.clone(),
            code_len: func.code.len(),
            metadata_len: func.jit_metadata.len(),
        });
    }

    for (pc, inst) in func.code.iter().copied().enumerate() {
        let opcode = inst.opcode();
        if opcode == Opcode::Invalid {
            return Err(ModuleVerificationError::InvalidOpcode {
                func: func.name.clone(),
                pc,
                raw: inst.op,
            });
        }
        verify_instruction_contract(func, module, pc, inst, opcode)?;
    }

    Ok(())
}

fn verify_function_invariants(func: &FunctionDef) -> Result<(), ModuleVerificationError> {
    let invariant = |detail: String| ModuleVerificationError::FunctionInvariant {
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
    if func.is_closure
        && (func.param_slots == 0 || func.slot_types.first() != Some(&SlotType::GcRef))
    {
        return Err(invariant(
            "closure functions must reserve GcRef slot 0".to_string(),
        ));
    }
    for (idx, transfer) in func.capture_types.iter().enumerate() {
        validate_transfer_type_kinds(func, idx, "capture_types", transfer)?;
    }
    for (idx, transfer) in func.param_types.iter().enumerate() {
        validate_transfer_type_kinds(func, idx, "param_types", transfer)?;
    }
    validate_transfer_shape_invariants(func)?;

    Ok(())
}

fn validate_transfer_shape_invariants(func: &FunctionDef) -> Result<(), ModuleVerificationError> {
    let invariant = |detail: String| ModuleVerificationError::FunctionInvariant {
        func: func.name.clone(),
        detail,
    };

    if func.capture_types.len() != func.capture_slot_types.len() {
        return Err(invariant(format!(
            "capture_types.len()={} but capture_slot_types.len()={}",
            func.capture_types.len(),
            func.capture_slot_types.len()
        )));
    }

    if !func.param_types.is_empty() {
        let implicit_param_slots = func
            .recv_slots
            .checked_add(u16::from(func.is_closure))
            .ok_or_else(|| {
                invariant(format!(
                    "implicit param slots overflow recv_slots={} is_closure={}",
                    func.recv_slots, func.is_closure
                ))
            })?;
        if implicit_param_slots > func.param_slots {
            return Err(invariant(format!(
                "implicit param slots {} exceed param_slots={}",
                implicit_param_slots, func.param_slots
            )));
        }
        let expected_without_receiver = func.param_slots - implicit_param_slots;
        let expected_with_receiver = func
            .param_slots
            .checked_sub(u16::from(func.is_closure))
            .ok_or_else(|| {
                invariant(format!(
                    "closure self slot exceeds param_slots={}",
                    func.param_slots
                ))
            })?;
        let actual_transfer_slots = func
            .param_types
            .iter()
            .try_fold(0u16, |acc, transfer| acc.checked_add(transfer.slots));
        let Some(actual_transfer_slots) = actual_transfer_slots else {
            return Err(invariant(
                "param_types total slots overflow u16".to_string(),
            ));
        };
        if actual_transfer_slots != expected_without_receiver
            && (func.recv_slots == 0 || actual_transfer_slots != expected_with_receiver)
        {
            return Err(invariant(format!(
                "param_types total slots {} but expected {} without receiver or {} with explicit receiver from param_slots={} recv_slots={} is_closure={}",
                actual_transfer_slots,
                expected_without_receiver,
                expected_with_receiver,
                func.param_slots,
                func.recv_slots,
                func.is_closure
            )));
        }
    }

    Ok(())
}

fn validate_transfer_type_kinds(
    func: &FunctionDef,
    idx: usize,
    access: &'static str,
    transfer: &TransferType,
) -> Result<(), ModuleVerificationError> {
    let invariant = |detail: String| ModuleVerificationError::FunctionInvariant {
        func: func.name.clone(),
        detail,
    };
    let meta_kind = transfer.meta_raw as u8;
    let meta_kind = ValueKind::try_from(meta_kind).map_err(|_| {
        invariant(format!(
            "{access}[{idx}] has invalid ValueMeta kind tag {meta_kind}"
        ))
    })?;
    let rttid_kind = transfer.rttid_raw as u8;
    let rttid_kind = ValueKind::try_from(rttid_kind).map_err(|_| {
        invariant(format!(
            "{access}[{idx}] has invalid ValueRttid kind tag {rttid_kind}"
        ))
    })?;
    if meta_kind != rttid_kind {
        return Err(invariant(format!(
            "{access}[{idx}] ValueMeta kind {:?} does not match ValueRttid kind {:?}",
            meta_kind, rttid_kind
        )));
    }
    Ok(())
}

pub fn validate_module_gc_layout(module: &Module) -> Result<(), ModuleVerificationError> {
    let mut total_global_slots = 0usize;
    for (idx, global) in module.globals.iter().enumerate() {
        total_global_slots = total_global_slots
            .checked_add(global.slots as usize)
            .ok_or_else(|| ModuleVerificationError::GcLayout {
                detail: "global slot count overflows usize".to_string(),
            })?;
        validate_slot_layout(
            &format!("global {idx} ({})", global.name),
            global.slots as usize,
            &global.slot_types,
        )?;
    }
    let _ = total_global_slots;

    for (idx, meta) in module.struct_metas.iter().enumerate() {
        let label = format!("struct_meta {idx}");
        validate_slot_layout(&label, meta.slot_types.len(), &meta.slot_types)?;
        for (field_idx, field) in meta.fields.iter().enumerate() {
            let end = (field.offset as usize)
                .checked_add(field.slot_count as usize)
                .ok_or_else(|| ModuleVerificationError::GcLayout {
                    detail: format!(
                        "{label} field {field_idx} ({}) slot range overflows",
                        field.name
                    ),
                })?;
            if end > meta.slot_types.len() {
                return Err(ModuleVerificationError::GcLayout {
                    detail: format!(
                        "{label} field {field_idx} ({}) slot range {}..{} exceeds struct slots {}",
                        field.name,
                        field.offset,
                        end,
                        meta.slot_types.len()
                    ),
                });
            }
        }
    }

    for (idx, func) in module.functions.iter().enumerate() {
        validate_function_gc_layout(idx, func)?;
    }

    Ok(())
}

pub fn validate_slot_layout(
    label: &str,
    slots: usize,
    slot_types: &[SlotType],
) -> Result<(), ModuleVerificationError> {
    if slot_types.len() != slots {
        return Err(ModuleVerificationError::GcLayout {
            detail: format!(
                "{label} slot_types len {} does not match slots {}",
                slot_types.len(),
                slots
            ),
        });
    }
    validate_interface_pairs(label, slot_types)
}

pub fn validate_interface_pairs(
    label: &str,
    slot_types: &[SlotType],
) -> Result<(), ModuleVerificationError> {
    for (slot_idx, slot_type) in slot_types.iter().enumerate() {
        match slot_type {
            SlotType::Interface0 => {
                if slot_types.get(slot_idx + 1) != Some(&SlotType::Interface1) {
                    return Err(ModuleVerificationError::GcLayout {
                        detail: format!(
                            "{label} Interface0 slot {slot_idx} is not followed by Interface1"
                        ),
                    });
                }
            }
            SlotType::Interface1 => {
                if slot_idx == 0 || slot_types.get(slot_idx - 1) != Some(&SlotType::Interface0) {
                    return Err(ModuleVerificationError::GcLayout {
                        detail: format!(
                            "{label} Interface1 slot {slot_idx} is not preceded by Interface0"
                        ),
                    });
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn validate_function_gc_layout(
    idx: usize,
    func: &FunctionDef,
) -> Result<(), ModuleVerificationError> {
    let label = format!("function {idx} ({})", func.name);
    validate_slot_layout(&label, func.local_slots as usize, &func.slot_types)?;

    if func.gc_scan_slots as usize > func.slot_types.len() {
        return Err(ModuleVerificationError::GcLayout {
            detail: format!(
                "{label} gc_scan_slots {} exceeds slot_types len {}",
                func.gc_scan_slots,
                func.slot_types.len()
            ),
        });
    }

    let expected_scan_slots = FunctionDef::compute_gc_scan_slots(&func.slot_types);
    if func.gc_scan_slots != expected_scan_slots {
        return Err(ModuleVerificationError::GcLayout {
            detail: format!(
                "{label} gc_scan_slots {} does not match slot_types; expected {}",
                func.gc_scan_slots, expected_scan_slots
            ),
        });
    }

    let expected_prefix = FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
    if func.borrowed_scan_slots_prefix != expected_prefix {
        return Err(ModuleVerificationError::GcLayout {
            detail: format!("{label} borrowed_scan_slots_prefix does not match slot_types"),
        });
    }

    validate_slot_layout(
        &format!("{label} return slots"),
        func.ret_slots as usize,
        &func.ret_slot_types,
    )?;

    if !func.capture_slot_types.is_empty() {
        validate_interface_pairs(&format!("{label} capture slots"), &func.capture_slot_types)?;
    }

    if !func.heap_ret_slots.is_empty()
        && func.heap_ret_slots.len() != func.heap_ret_gcref_count as usize
    {
        return Err(ModuleVerificationError::GcLayout {
            detail: format!(
                "{label} heap_ret_slots len {} does not match heap_ret_gcref_count {}",
                func.heap_ret_slots.len(),
                func.heap_ret_gcref_count
            ),
        });
    }
    let heap_ret_end = (func.heap_ret_gcref_start as usize)
        .checked_add(func.heap_ret_gcref_count as usize)
        .ok_or_else(|| ModuleVerificationError::GcLayout {
            detail: format!("{label} heap return GcRef range overflows"),
        })?;
    if heap_ret_end > func.local_slots as usize {
        return Err(ModuleVerificationError::GcLayout {
            detail: format!(
                "{label} heap return GcRef range {}..{} exceeds local_slots {}",
                func.heap_ret_gcref_start, heap_ret_end, func.local_slots
            ),
        });
    }

    Ok(())
}

fn verify_instruction_contract(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    inst: Instruction,
    opcode: Opcode,
) -> Result<(), ModuleVerificationError> {
    match opcode {
        Opcode::Hint => verify_hint(func, pc, inst),
        Opcode::LoadInt => verify_load_int_contract(func, pc, inst),
        Opcode::LoadConst => verify_load_const_contract(func, module, pc, inst),
        Opcode::Copy => verify_copy_contract(func, pc, opcode, inst),
        Opcode::CopyN => verify_copy_n_contract(func, pc, opcode, inst),
        Opcode::SlotGet => verify_slot_get_contract(func, pc, opcode, inst.a, inst.b, inst.c, 1),
        Opcode::SlotGetN => {
            verify_slot_get_contract(func, pc, opcode, inst.a, inst.b, inst.c, inst.flags as u16)
        }
        Opcode::SlotSet => verify_slot_set_contract(func, pc, opcode, inst.a, inst.b, inst.c, 1),
        Opcode::SlotSetN => {
            verify_slot_set_contract(func, pc, opcode, inst.a, inst.b, inst.c, inst.flags as u16)
        }
        Opcode::GlobalGet => {
            verify_global_get_contract(func, module, pc, opcode, inst.b, inst.a, 1)
        }
        Opcode::GlobalGetN => {
            verify_global_get_contract(func, module, pc, opcode, inst.b, inst.a, inst.flags as u16)
        }
        Opcode::GlobalSet => {
            verify_global_set_contract(func, module, pc, opcode, inst.a, inst.b, 1)
        }
        Opcode::GlobalSetN => {
            verify_global_set_contract(func, module, pc, opcode, inst.a, inst.b, inst.flags as u16)
        }
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
        Opcode::PtrSet => verify_ptr_set_contract(func, pc, opcode, inst.a, inst.c, inst.flags),
        Opcode::PtrSetN => verify_ptr_set_n_contract(func, pc, opcode, inst),
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
        Opcode::Call => verify_static_call_contract(func, module, pc, inst),
        Opcode::CallExtern => verify_call_extern_contract(func, module, pc, inst),
        Opcode::CallClosure => verify_dynamic_call_contract(func, pc, opcode, inst, true),
        Opcode::CallIface => verify_dynamic_call_contract(func, pc, opcode, inst, false),
        Opcode::Return => verify_return_contract(func, pc, inst),
        Opcode::StrNew => verify_str_new_contract(func, module, pc, inst),
        Opcode::StrLen => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "StrLen destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "StrLen source",
            )
        }
        Opcode::StrIndex | Opcode::StrDecodeRune => {
            verify_str_index_contract(func, pc, opcode, inst)
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
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "StrConcat lhs",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.c,
                &[SlotType::GcRef],
                "StrConcat rhs",
            )
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
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "StrSlice source",
            )?;
            verify_value_range(func, pc, opcode, inst.c, 2, "StrSlice bounds")
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
                "string compare destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "string compare lhs",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.c,
                &[SlotType::GcRef],
                "string compare rhs",
            )
        }
        Opcode::ArrayNew => verify_array_new_contract(func, pc, opcode, inst),
        Opcode::ArrayGet => verify_indexed_get_contract(
            func,
            pc,
            opcode,
            inst,
            "Array source",
            "Array index",
            "ArrayGet destination",
        ),
        Opcode::ArraySet => verify_indexed_set_contract(
            func,
            pc,
            opcode,
            inst,
            "ArraySet target",
            "ArraySet index",
            "ArraySet source",
        ),
        Opcode::ArrayAddr => verify_indexed_addr_contract(
            func,
            pc,
            opcode,
            inst,
            "ArrayAddr destination",
            "Array source",
            "Array index",
        ),
        Opcode::SliceNew => verify_slice_new_contract(func, pc, opcode, inst),
        Opcode::SliceGet => verify_indexed_get_contract(
            func,
            pc,
            opcode,
            inst,
            "Slice source",
            "Slice index",
            "SliceGet destination",
        ),
        Opcode::SliceSet => verify_indexed_set_contract(
            func,
            pc,
            opcode,
            inst,
            "SliceSet target",
            "SliceSet index",
            "SliceSet source",
        ),
        Opcode::SliceAddr => verify_indexed_addr_contract(
            func,
            pc,
            opcode,
            inst,
            "SliceAddr destination",
            "Slice source",
            "Slice index",
        ),
        Opcode::SliceLen | Opcode::SliceCap => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "Slice len/cap destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "Slice len/cap source",
            )
        }
        Opcode::SliceSlice => verify_slice_slice_contract(func, pc, opcode, inst),
        Opcode::SliceAppend => verify_slice_append_contract(func, pc, opcode, inst),
        Opcode::MapNew => verify_map_new_contract(func, pc, opcode, inst),
        Opcode::MapGet => verify_map_get_contract(func, pc, opcode, inst),
        Opcode::MapSet => verify_map_set_contract(func, pc, opcode, inst),
        Opcode::MapDelete => verify_map_delete_contract(func, pc, opcode, inst),
        Opcode::MapLen => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "MapLen destination",
            )?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "MapLen map")
        }
        Opcode::MapIterInit => {
            verify_range(func, pc, inst.a, 7, "MapIterInit iterator")?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "MapIterInit map",
            )
        }
        Opcode::MapIterNext => verify_map_iter_next_contract(func, pc, opcode, inst),
        Opcode::QueueNew => verify_queue_new_contract(func, pc, opcode, inst),
        Opcode::QueueSend => verify_queue_send_contract(func, pc, opcode, inst),
        Opcode::QueueRecv => verify_queue_recv_contract(func, pc, opcode, inst),
        Opcode::QueueLen | Opcode::QueueCap | Opcode::QueueClose => {
            let dst = if opcode == Opcode::QueueClose {
                None
            } else {
                Some(inst.a)
            };
            if let Some(dst) = dst {
                verify_layout(
                    func,
                    pc,
                    opcode,
                    dst,
                    &[SlotType::Value],
                    "Queue query destination",
                )?;
            }
            let queue_slot = if opcode == Opcode::QueueClose {
                inst.a
            } else {
                inst.b
            };
            verify_layout(func, pc, opcode, queue_slot, &[SlotType::GcRef], "queue")
        }
        Opcode::SelectBegin => Ok(()),
        Opcode::SelectSend => verify_select_send_contract(func, pc, opcode, inst),
        Opcode::SelectRecv => verify_select_recv_contract(func, pc, opcode, inst),
        Opcode::SelectExec => verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::Value],
            "SelectExec destination",
        ),
        Opcode::ClosureNew => verify_closure_new_contract(func, module, pc, inst),
        Opcode::ClosureGet => verify_closure_get_contract(func, pc, inst),
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush => {
            verify_shared_call_shape_contract(func, module, pc, opcode, inst)
        }
        Opcode::Panic => verify_interface_pair(func, pc, opcode, inst.a, "Panic payload"),
        Opcode::Recover => verify_interface_pair(func, pc, opcode, inst.a, "Recover destination"),
        Opcode::IfaceAssign => verify_iface_assign_contract(func, module, pc, inst),
        Opcode::IfaceAssert => verify_iface_assert_contract(func, module, pc, inst),
        Opcode::IfaceEq => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "IfaceEq destination",
            )?;
            verify_interface_pair(func, pc, opcode, inst.b, "IfaceEq lhs")?;
            verify_interface_pair(func, pc, opcode, inst.c, "IfaceEq rhs")
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
        Opcode::IslandNew => verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::GcRef],
            "IslandNew destination",
        ),
        Opcode::GoIsland => verify_go_island_contract(func, pc, opcode, inst),
        Opcode::ForLoop => {
            if inst.flags & !0x07 != 0 {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!("unsupported ForLoop flags 0x{:02x}", inst.flags),
                ));
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
        Opcode::Invalid => Err(ModuleVerificationError::InvalidOpcode {
            func: func.name.clone(),
            pc,
            raw: inst.op,
        }),
    }
}

fn verify_hint(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    if inst.flags == HINT_LOOP {
        let encoded_end_offset = ((inst.a >> 8) & 0xFF) as usize;
        if encoded_end_offset > 0 {
            verify_jump_target_contract(func, pc, Opcode::Hint, (pc + encoded_end_offset) as i64)?;
        }
    }
    Ok(())
}

fn constant_at<'a>(
    func: &FunctionDef,
    module: &'a Module,
    pc: usize,
    const_id: u16,
) -> Result<&'a Constant, ModuleVerificationError> {
    module.constants.get(const_id as usize).ok_or_else(|| {
        ModuleVerificationError::MissingConstant {
            func: func.name.clone(),
            pc,
            const_id,
        }
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

fn verify_function_index(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    callee_id: u32,
) -> Result<(), ModuleVerificationError> {
    module
        .functions
        .get(callee_id as usize)
        .map(|_| ())
        .ok_or_else(|| ModuleVerificationError::MissingFunction {
            func: func.name.clone(),
            pc,
            callee_id,
        })
}

fn verify_extern_index(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    extern_id: u16,
) -> Result<(), ModuleVerificationError> {
    module
        .externs
        .get(extern_id as usize)
        .map(|_| ())
        .ok_or_else(|| ModuleVerificationError::MissingExtern {
            func: func.name.clone(),
            pc,
            extern_id,
        })
}

fn local_layout<'a>(
    func: &'a FunctionDef,
    pc: usize,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<&'a [SlotType], ModuleVerificationError> {
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
) -> Result<(), ModuleVerificationError> {
    if count == 0 {
        return Ok(());
    }
    if count == 1 && (start >= func.local_slots || start as usize >= func.slot_types.len()) {
        return Err(ModuleVerificationError::SlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: start,
            local_slots: func.local_slots,
            access,
        });
    }
    let end =
        start
            .checked_add(count)
            .ok_or_else(|| ModuleVerificationError::SlotRangeOverflow {
                func: func.name.clone(),
                pc,
                start,
                count,
                access,
            })?;
    if end > func.local_slots || end as usize > func.slot_types.len() {
        return Err(ModuleVerificationError::SlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: end.saturating_sub(1),
            local_slots: func.local_slots,
            access,
        });
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
) -> Result<(), ModuleVerificationError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        Ok(())
    } else {
        Err(ModuleVerificationError::SlotTypeMismatch {
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

fn verify_value_range(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    verify_layout(
        func,
        pc,
        opcode,
        start,
        &vec![SlotType::Value; count as usize],
        access,
    )
}

fn verify_one_of_single_slot_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected_any: &[SlotType],
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let actual = local_layout(func, pc, start, 1, access)?;
    if expected_any.contains(&actual[0]) {
        Ok(())
    } else {
        Err(ModuleVerificationError::SlotTypeMismatch {
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

fn verify_local_layout_matches(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        Ok(())
    } else {
        if matches!(
            actual.first(),
            Some(SlotType::Interface0 | SlotType::Interface1)
        ) {
            verify_structural_layout(func, pc, opcode, start, actual, access)?;
        }
        Err(ModuleVerificationError::SlotTypeMismatch {
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

fn verify_storage_layout_compatible(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        return Ok(());
    }
    verify_structural_layout(func, pc, opcode, start, expected, access)?;
    verify_structural_layout(func, pc, opcode, start, actual, access)?;

    // VM collection copies are raw slot moves; typed barriers and container
    // metadata decide which copied words are references. Allow metadata that
    // describes raw Value storage width to be backed by structural interface
    // pairs, while keeping Float, GcRef, and non-raw expectations strict.
    if raw_value_storage_accepts_actual(expected, actual) {
        Ok(())
    } else {
        Err(ModuleVerificationError::SlotTypeMismatch {
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

fn raw_value_storage_accepts_actual(expected: &[SlotType], actual: &[SlotType]) -> bool {
    if !expected.iter().all(|slot| *slot == SlotType::Value) {
        return false;
    }

    let mut idx = 0usize;
    let mut saw_structural_interface = false;
    while idx < actual.len() {
        match actual[idx] {
            SlotType::Value => idx += 1,
            SlotType::Interface0 if actual.get(idx + 1) == Some(&SlotType::Interface1) => {
                saw_structural_interface = true;
                idx += 2;
            }
            _ => return false,
        }
    }
    saw_structural_interface
}

fn verify_raw_or_exact_layout_matches(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        Ok(())
    } else {
        verify_structural_layout(func, pc, opcode, start, actual, access)?;
        Err(ModuleVerificationError::SlotTypeMismatch {
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
) -> Result<(), ModuleVerificationError> {
    let mut i = 0usize;
    while i < layout.len() {
        match layout[i] {
            SlotType::Interface0 => {
                if layout.get(i + 1) != Some(&SlotType::Interface1) {
                    return Err(ModuleVerificationError::InvalidInterfaceLayout {
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
                return Err(ModuleVerificationError::InvalidInterfaceLayout {
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

fn verify_interface_pair(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let actual = local_layout(func, pc, slot, 2, access)?;
    if actual == [SlotType::Interface0, SlotType::Interface1] {
        Ok(())
    } else {
        Err(ModuleVerificationError::InvalidInterfaceLayout {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot,
            actual: actual.to_vec(),
        })
    }
}

fn checked_slot_offset(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    offset: u16,
    access: &'static str,
) -> Result<u16, ModuleVerificationError> {
    start
        .checked_add(offset)
        .ok_or_else(|| ModuleVerificationError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start,
            count: offset.saturating_add(1),
            access,
        })
}

fn flattened_global_slot_types(module: &Module) -> Vec<SlotType> {
    module
        .globals
        .iter()
        .flat_map(|global| global.slot_types.iter().copied())
        .collect()
}

fn call_shape_mismatch(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    detail: String,
) -> ModuleVerificationError {
    ModuleVerificationError::CallShapeMismatch {
        func: func.name.clone(),
        pc,
        opcode,
        detail,
    }
}

fn missing_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    layout: &'static str,
) -> ModuleVerificationError {
    ModuleVerificationError::MissingLayout {
        func: func.name.clone(),
        pc,
        opcode,
        layout,
    }
}

fn decode_metadata_layout<T>(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    layout: &'static str,
    decode: impl FnOnce(&JitInstructionMetadata) -> Option<T>,
) -> Result<T, ModuleVerificationError> {
    func.jit_metadata
        .get(pc)
        .and_then(decode)
        .ok_or_else(|| missing_layout(func, pc, opcode, layout))
}

fn elem_layout_from_flags(flags: u8) -> (usize, Vec<SlotType>, bool) {
    let (bytes, sign) = match flags {
        0 => (64usize, false),
        0x81 => (1, true),
        0x82 => (2, true),
        0x84 => (4, true),
        0x44 => (4, false),
        f => (f as usize, false),
    };
    let slot = if (flags & crate::ELEM_FLAG_FLOAT_BIT) != 0 {
        SlotType::Float
    } else {
        SlotType::Value
    };
    let slots = bytes.div_ceil(8);
    (bytes, vec![slot; slots], sign)
}

fn elem_layout_from_instruction(metadata: &JitInstructionMetadata) -> Option<Vec<SlotType>> {
    match metadata {
        JitInstructionMetadata::ElemLayout {
            elem_bytes,
            slot_layout,
            ..
        } => {
            let slots = (*elem_bytes as usize).div_ceil(8);
            (slot_layout.len() == slots).then(|| slot_layout.clone())
        }
        _ => None,
    }
}

fn elem_layout_for_indexed(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    flags: u8,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    if flags == 0 {
        decode_metadata_layout(func, pc, opcode, "ElemLayout", elem_layout_from_instruction)
    } else {
        Ok(elem_layout_from_flags(flags).1)
    }
}

fn ptr_value_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "PtrLayout", |metadata| match metadata {
        JitInstructionMetadata::PtrLayout { value_layout } => Some(value_layout.clone()),
        _ => None,
    })
}

fn slot_elem_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "SlotLayout", |metadata| match metadata {
        JitInstructionMetadata::SlotLayout { elem_layout } => Some(elem_layout.clone()),
        _ => None,
    })
}

fn call_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "CallLayout", |metadata| match metadata {
        JitInstructionMetadata::CallLayout {
            arg_layout,
            ret_layout,
        } => Some((arg_layout.clone(), ret_layout.clone())),
        _ => None,
    })
}

fn call_extern_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "CallExternLayout",
        |metadata| match metadata {
            JitInstructionMetadata::CallExternLayout {
                arg_layout,
                ret_layout,
            } => Some((arg_layout.clone(), ret_layout.clone())),
            _ => None,
        },
    )
}

fn queue_elem_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "QueueLayout", |metadata| match metadata {
        JitInstructionMetadata::QueueLayout { elem_layout } => Some(elem_layout.clone()),
        _ => None,
    })
}

fn map_get_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>, bool), ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "MapGet", |metadata| match metadata {
        JitInstructionMetadata::MapGet {
            key_layout,
            val_layout,
            has_ok,
        } => Some((key_layout.clone(), val_layout.clone(), *has_ok)),
        _ => None,
    })
}

fn map_set_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "MapSet", |metadata| match metadata {
        JitInstructionMetadata::MapSet {
            key_layout,
            val_layout,
        } => Some((key_layout.clone(), val_layout.clone())),
        _ => None,
    })
}

fn map_delete_key_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "MapDelete", |metadata| match metadata {
        JitInstructionMetadata::MapDelete { key_layout } => Some(key_layout.clone()),
        _ => None,
    })
}

fn map_iter_next_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "MapIterNext", |metadata| match metadata {
        JitInstructionMetadata::MapIterNext {
            key_layout,
            val_layout,
        } => Some((key_layout.clone(), val_layout.clone())),
        _ => None,
    })
}

fn iface_assert_result_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "IfaceAssertLayout",
        |metadata| match metadata {
            JitInstructionMetadata::IfaceAssertLayout { result_layout } => {
                Some(result_layout.clone())
            }
            _ => None,
        },
    )
}

fn verify_binary_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    dst: SlotType,
    lhs: SlotType,
    rhs: SlotType,
    dst_access: &'static str,
    lhs_access: &'static str,
    rhs_access: &'static str,
) -> Result<(), ModuleVerificationError> {
    verify_layout(func, pc, opcode, inst.a, &[dst], dst_access)?;
    verify_layout(func, pc, opcode, inst.b, &[lhs], lhs_access)?;
    verify_layout(func, pc, opcode, inst.c, &[rhs], rhs_access)
}

fn verify_unary_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    dst: SlotType,
    src: SlotType,
    dst_access: &'static str,
    src_access: &'static str,
) -> Result<(), ModuleVerificationError> {
    verify_layout(func, pc, opcode, inst.a, &[dst], dst_access)?;
    verify_layout(func, pc, opcode, inst.b, &[src], src_access)
}

fn verify_binary_one_of_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    dst_any: &[SlotType],
    lhs_any: &[SlotType],
    rhs_any: &[SlotType],
    dst_access: &'static str,
    lhs_access: &'static str,
    rhs_access: &'static str,
) -> Result<(), ModuleVerificationError> {
    verify_one_of_single_slot_layout(func, pc, opcode, inst.a, dst_any, dst_access)?;
    verify_one_of_single_slot_layout(func, pc, opcode, inst.b, lhs_any, lhs_access)?;
    verify_one_of_single_slot_layout(func, pc, opcode, inst.c, rhs_any, rhs_access)
}

fn verify_unary_one_of_slot_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    dst_any: &[SlotType],
    src_any: &[SlotType],
    dst_access: &'static str,
    src_access: &'static str,
) -> Result<(), ModuleVerificationError> {
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

fn verify_load_int_contract(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
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
    module: &Module,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let constant = constant_at(func, module, pc, inst.b)?;
    let expected_slot = match constant {
        Constant::String(_) => {
            return Err(ModuleVerificationError::ConstantKindMismatch {
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

fn verify_copy_n_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    if inst.c == 0 && inst.flags != 0 {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "CopyN count must be encoded in c, got c=0 flags={}",
                inst.flags
            ),
        ));
    }
    let count = inst.c;
    let source = local_layout(func, pc, inst.b, count, "CopyN source")?;
    verify_structural_layout(func, pc, opcode, inst.b, source, "CopyN source")?;
    verify_local_layout_matches(func, pc, opcode, inst.a, source, "CopyN destination")
}

fn verify_copy_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let source = local_layout(func, pc, inst.b, 1, "Copy source")?;
    let actual = local_layout(func, pc, inst.a, 1, "Copy destination")?;
    if actual == source {
        Ok(())
    } else {
        Err(ModuleVerificationError::SlotTypeMismatch {
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

fn verify_slot_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    dst_start: u16,
    base_start: u16,
    index_slot: u16,
    count: u16,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = slot_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != count as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "SlotGet metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                count
            ),
        ));
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
) -> Result<(), ModuleVerificationError> {
    let elem_layout = slot_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != count as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "SlotSet metadata layout slots {} do not match encoded count {}",
                elem_layout.len(),
                count
            ),
        ));
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

fn verify_global_get_contract(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    opcode: Opcode,
    global_start: u16,
    dst_start: u16,
    count: u16,
) -> Result<(), ModuleVerificationError> {
    let globals = flattened_global_slot_types(module);
    let end = global_start.checked_add(count).ok_or_else(|| {
        ModuleVerificationError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: global_start,
            count,
            access: "global read",
        }
    })? as usize;
    if end > globals.len() {
        return Err(ModuleVerificationError::GlobalSlotOutOfRange {
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

fn verify_global_set_contract(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    opcode: Opcode,
    global_start: u16,
    src_start: u16,
    count: u16,
) -> Result<(), ModuleVerificationError> {
    let globals = flattened_global_slot_types(module);
    let end = global_start.checked_add(count).ok_or_else(|| {
        ModuleVerificationError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: global_start,
            count,
            access: "global write",
        }
    })? as usize;
    if end > globals.len() {
        return Err(ModuleVerificationError::GlobalSlotOutOfRange {
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

fn verify_ptr_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    dst_start: u16,
    ptr_slot: u16,
    count: u16,
) -> Result<(), ModuleVerificationError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    if value_layout.len() != count as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "PtrGet metadata layout slots {} do not match encoded count {}",
                value_layout.len(),
                count
            ),
        ));
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

fn verify_ptr_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    ptr_slot: u16,
    src_slot: u16,
    flags: u8,
) -> Result<(), ModuleVerificationError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    if value_layout.len() != 1 {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "PtrSet metadata layout slots {} do not match encoded count 1",
                value_layout.len()
            ),
        ));
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
        return Err(ModuleVerificationError::SlotTypeMismatch {
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

fn verify_ptr_set_n_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    if value_layout.len() != inst.flags as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "PtrSetN metadata layout slots {} do not match encoded count {}",
                value_layout.len(),
                inst.flags
            ),
        ));
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
        return Err(ModuleVerificationError::SlotTypeMismatch {
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

fn verify_jump_target_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    target: i64,
) -> Result<(), ModuleVerificationError> {
    if target >= 0 && (target as usize) < func.code.len() {
        Ok(())
    } else {
        Err(ModuleVerificationError::InvalidBranchTarget {
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

fn verify_return_contract(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let opcode = Opcode::Return;
    let flags = ReturnFlags::from_bits(inst.flags).ok_or_else(|| {
        ModuleVerificationError::InvalidInstructionFlags {
            func: func.name.clone(),
            pc,
            opcode,
            flags: inst.flags,
            allowed: ReturnFlags::ALLOWED_BITS,
        }
    })?;
    if flags.has_heap_returns() {
        if func.heap_ret_gcref_count == 0 {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                "heap return flag set but function has no heap return GcRefs".to_string(),
            ));
        }
        if inst.b != func.heap_ret_gcref_count {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "heap return count {} does not match function heap_ret_gcref_count {}",
                    inst.b, func.heap_ret_gcref_count
                ),
            ));
        }
        if inst.a != func.heap_ret_gcref_start {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "heap return start {} does not match function heap_ret_gcref_start {}",
                    inst.a, func.heap_ret_gcref_start
                ),
            ));
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

    if inst.b != func.ret_slots {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "return slot count {} does not match function ret_slots {}",
                inst.b, func.ret_slots
            ),
        ));
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

fn verify_static_call_contract(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let opcode = Opcode::Call;
    let callee_id = inst.static_call_func_id();
    let callee = module.functions.get(callee_id as usize).ok_or_else(|| {
        ModuleVerificationError::MissingFunction {
            func: func.name.clone(),
            pc,
            callee_id,
        }
    })?;

    if callee.param_slots <= u8::MAX as u16 && callee.ret_slots <= u8::MAX as u16 {
        if inst.packed_arg_slots() != callee.param_slots {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "encoded arg slots {} do not match callee {} param_slots {}",
                    inst.packed_arg_slots(),
                    callee.name,
                    callee.param_slots
                ),
            ));
        }
        if inst.packed_ret_slots() != callee.ret_slots {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "encoded ret slots {} do not match callee {} ret_slots {}",
                    inst.packed_ret_slots(),
                    callee.name,
                    callee.ret_slots
                ),
            ));
        }
    } else if inst.c != 0 {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "large static call to {} must use zero packed shape mirror, got 0x{:04x}",
                callee.name, inst.c
            ),
        ));
    }

    let expected_args = callee
        .slot_types
        .get(..callee.param_slots as usize)
        .ok_or_else(|| {
            call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "callee {} has {} slot_types but param_slots={}",
                    callee.name,
                    callee.slot_types.len(),
                    callee.param_slots
                ),
            )
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
        ModuleVerificationError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: inst.b,
            count: callee.param_slots,
            access: "Call return buffer",
        }
    })?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        ret_start,
        &callee.ret_slot_types,
        "Call return buffer",
    )
}

fn verify_dynamic_call_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    is_closure: bool,
) -> Result<(), ModuleVerificationError> {
    if is_closure {
        verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::GcRef],
            "CallClosure callee",
        )?;
    } else {
        verify_interface_pair(func, pc, opcode, inst.a, "CallIface receiver")?;
        if inst.b == 0 {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                "CallIface ABI requires a hidden receiver prefix slot before arg_start".to_string(),
            ));
        }
    }
    let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
    if arg_layout.len() != inst.packed_arg_slots() as usize
        || ret_layout.len() != inst.packed_ret_slots() as usize
    {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{:?} metadata layout slots args={} returns={} do not match encoded args={} returns={}",
                opcode,
                arg_layout.len(),
                ret_layout.len(),
                inst.packed_arg_slots(),
                inst.packed_ret_slots()
            ),
        ));
    }
    verify_local_layout_matches(func, pc, opcode, inst.b, &arg_layout, "dynamic call args")?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        inst.packed_call_ret_start(),
        &ret_layout,
        "dynamic call returns",
    )
}

fn verify_call_extern_contract(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let opcode = Opcode::CallExtern;
    verify_extern_index(func, module, pc, inst.b)?;
    let extern_def = &module.externs[inst.b as usize];
    if !extern_def.param_kinds.is_empty() && extern_def.param_kinds.len() != inst.flags as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "extern {} has {} param_kinds but instruction encodes {} arg slots",
                extern_def.name,
                extern_def.param_kinds.len(),
                inst.flags
            ),
        ));
    }
    if extern_def.param_slots != 0 && extern_def.param_slots != inst.flags as u16 {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "CallExtern arg slot count {} does not match extern {} param_slots {}",
                inst.flags, extern_def.name, extern_def.param_slots
            ),
        ));
    }
    let (arg_layout, ret_layout) = call_extern_layout(func, pc, opcode)?;
    if arg_layout.len() != inst.flags as usize || ret_layout.len() != extern_def.ret_slots as usize
    {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "CallExtern metadata layout slots args={} returns={} do not match encoded args={} extern returns={}",
                arg_layout.len(),
                ret_layout.len(),
                inst.flags,
                extern_def.ret_slots
            ),
        ));
    }
    verify_local_layout_matches(func, pc, opcode, inst.c, &arg_layout, "CallExtern args")?;
    verify_local_layout_matches(func, pc, opcode, inst.a, &ret_layout, "CallExtern returns")
}

fn verify_str_new_contract(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let constant = constant_at(func, module, pc, inst.b)?;
    if !matches!(constant, Constant::String(_)) {
        return Err(ModuleVerificationError::ConstantKindMismatch {
            func: func.name.clone(),
            pc,
            opcode: Opcode::StrNew,
            const_id: inst.b,
            expected: "String",
            actual: constant_kind(constant),
        });
    }
    verify_layout(
        func,
        pc,
        Opcode::StrNew,
        inst.a,
        &[SlotType::GcRef],
        "StrNew destination",
    )
}

fn verify_str_index_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::GcRef],
        "string source",
    )?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], "string index")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::Value],
        "string result",
    )?;
    if opcode == Opcode::StrDecodeRune {
        verify_layout(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.a, 1, "StrDecodeRune width")?,
            &[SlotType::Value],
            "StrDecodeRune width",
        )?;
    }
    Ok(())
}

fn verify_array_new_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
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
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "ArrayNew length",
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

fn verify_slice_new_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
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
    verify_value_range(func, pc, opcode, inst.c, 2, "SliceNew len/cap")?;
    verify_value_range(
        func,
        pc,
        opcode,
        inst.c,
        if inst.flags == 0 { 3 } else { 2 },
        "SliceNew len/cap/elem_bytes",
    )
}

fn verify_indexed_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    source_access: &'static str,
    index_access: &'static str,
    dst_access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = elem_layout_for_indexed(func, pc, opcode, inst.flags)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], source_access)?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], index_access)?;
    if elem_layout.is_empty() {
        Ok(())
    } else {
        verify_storage_layout_compatible(func, pc, opcode, inst.a, &elem_layout, dst_access)
    }
}

fn verify_indexed_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    target_access: &'static str,
    index_access: &'static str,
    source_access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = elem_layout_for_indexed(func, pc, opcode, inst.flags)?;
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcRef], target_access)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::Value], index_access)?;
    if elem_layout.is_empty() {
        Ok(())
    } else {
        verify_storage_layout_compatible(func, pc, opcode, inst.c, &elem_layout, source_access)
    }
}

fn verify_indexed_addr_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    dst_access: &'static str,
    source_access: &'static str,
    index_access: &'static str,
) -> Result<(), ModuleVerificationError> {
    if inst.flags == 0 {
        let _ = elem_layout_for_indexed(func, pc, opcode, inst.flags)?;
    }
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcRef], dst_access)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], source_access)?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], index_access)
}

fn verify_slice_slice_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    if inst.flags & !0b11 != 0 {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("unsupported SliceSlice flags 0x{:02x}", inst.flags),
        ));
    }
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
    let bound_count = if (inst.flags & 0b10) != 0 { 3 } else { 2 };
    verify_value_range(func, pc, opcode, inst.c, bound_count, "SliceSlice bounds")
}

fn verify_slice_append_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = elem_layout_for_indexed(func, pc, opcode, inst.flags)?;
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
        "SliceAppend slice",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "SliceAppend elem metadata",
    )?;
    let elem_offset = if inst.flags == 0 { 2 } else { 1 };
    if inst.flags == 0 {
        verify_layout(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.c, 1, "SliceAppend elem bytes")?,
            &[SlotType::Value],
            "SliceAppend elem bytes",
        )?;
    }
    verify_storage_layout_compatible(
        func,
        pc,
        opcode,
        checked_slot_offset(func, pc, inst.c, elem_offset, "SliceAppend element")?,
        &elem_layout,
        "SliceAppend element",
    )
}

fn verify_map_new_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcRef],
        "MapNew destination",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value, SlotType::Value],
        "MapNew metadata/key RTTID",
    )
}

fn verify_map_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let (key_layout, val_layout, has_ok) = map_get_layout(func, pc, opcode)?;
    let val_slots = u16::try_from(val_layout.len()).map_err(|_| {
        ModuleVerificationError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: inst.a,
            count: u16::MAX,
            access: "MapGet value",
        }
    })?;
    if has_ok && val_slots.checked_add(1).is_none() {
        return Err(ModuleVerificationError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: inst.a,
            count: u16::MAX,
            access: "MapGet ok",
        });
    }
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "MapGet map")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "MapGet metadata",
    )?;
    let key_start = checked_slot_offset(func, pc, inst.c, 1, "MapGet key")?;
    verify_storage_layout_compatible(func, pc, opcode, key_start, &key_layout, "MapGet key")?;
    verify_storage_layout_compatible(func, pc, opcode, inst.a, &val_layout, "MapGet value")?;
    if has_ok {
        verify_layout(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.a, val_layout.len() as u16, "MapGet ok")?,
            &[SlotType::Value],
            "MapGet ok",
        )?;
    }
    Ok(())
}

fn verify_map_set_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let (key_layout, val_layout) = map_set_layout(func, pc, opcode)?;
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcRef], "MapSet map")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "MapSet metadata",
    )?;
    let key_start = checked_slot_offset(func, pc, inst.b, 1, "MapSet key")?;
    verify_storage_layout_compatible(func, pc, opcode, key_start, &key_layout, "MapSet key")?;
    verify_storage_layout_compatible(func, pc, opcode, inst.c, &val_layout, "MapSet value")
}

fn verify_map_delete_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
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
    let key_start = checked_slot_offset(func, pc, inst.b, 1, "MapDelete key")?;
    verify_storage_layout_compatible(func, pc, opcode, key_start, &key_layout, "MapDelete key")
}

fn verify_map_iter_next_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let (key_layout, val_layout) = map_iter_next_layout(func, pc, opcode)?;
    if key_layout.len() != inst.map_iter_key_slots() as usize
        || val_layout.len() != inst.map_iter_val_slots() as usize
    {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "MapIterNext metadata layout key={} val={} does not match encoded key={} val={}",
                key_layout.len(),
                val_layout.len(),
                inst.map_iter_key_slots(),
                inst.map_iter_val_slots()
            ),
        ));
    }
    verify_layout(func, pc, opcode, inst.a, &key_layout, "MapIterNext key")?;
    verify_range(func, pc, inst.b, 7, "MapIterNext iterator")?;
    verify_layout(
        func,
        pc,
        opcode,
        checked_slot_offset(
            func,
            pc,
            inst.a,
            key_layout.len() as u16,
            "MapIterNext value",
        )?,
        &val_layout,
        "MapIterNext value",
    )
}

fn verify_queue_new_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
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

fn verify_queue_send_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != inst.flags as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "QueueSend metadata layout slots {} do not match encoded elem slots {}",
                elem_layout.len(),
                inst.flags
            ),
        ));
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

fn verify_queue_recv_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != inst.recv_elem_slots() as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "QueueRecv metadata layout slots {} do not match encoded elem slots {}",
                elem_layout.len(),
                inst.recv_elem_slots()
            ),
        ));
    }
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::GcRef],
        "QueueRecv queue",
    )?;
    verify_local_layout_matches(func, pc, opcode, inst.a, &elem_layout, "QueueRecv value")?;
    if inst.recv_has_ok() {
        verify_layout(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.a, elem_layout.len() as u16, "QueueRecv ok")?,
            &[SlotType::Value],
            "QueueRecv ok",
        )?;
    }
    Ok(())
}

fn verify_select_send_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    let elem_slots = if inst.flags == 0 {
        1
    } else {
        inst.flags as u16
    };
    if elem_layout.len() != elem_slots as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "SelectSend metadata layout slots {} do not match encoded elem slots {}",
                elem_layout.len(),
                elem_slots
            ),
        ));
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

fn verify_select_recv_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != inst.recv_elem_slots() as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "SelectRecv metadata layout slots {} do not match encoded elem slots {}",
                elem_layout.len(),
                inst.recv_elem_slots()
            ),
        ));
    }
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::GcRef],
        "SelectRecv queue",
    )?;
    verify_storage_layout_compatible(func, pc, opcode, inst.a, &elem_layout, "SelectRecv value")?;
    if inst.recv_has_ok() {
        verify_layout(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.a, elem_layout.len() as u16, "SelectRecv ok")?,
            &[SlotType::Value],
            "SelectRecv ok",
        )?;
    }
    Ok(())
}

fn verify_closure_new_contract(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    verify_layout(
        func,
        pc,
        Opcode::ClosureNew,
        inst.a,
        &[SlotType::GcRef],
        "ClosureNew destination",
    )?;
    verify_function_index(func, module, pc, inst.closure_new_func_id())
}

fn verify_closure_get_contract(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
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
        return Err(call_shape_mismatch(
            func,
            pc,
            Opcode::ClosureGet,
            format!(
                "capture slot {} out of range for {} capture slots",
                inst.b,
                func.capture_slot_types.len()
            ),
        ));
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

fn verify_shared_call_shape_contract(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
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
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "{opcode:?} closure metadata layout slots args={} returns={} do not match encoded args={}",
                    arg_layout.len(),
                    ret_layout.len(),
                    inst.c
                ),
            ));
        }
        verify_local_layout_matches(func, pc, opcode, inst.b, &arg_layout, "closure call args")
    } else {
        let callee_id = inst.call_shape_static_func_id();
        let callee = module.functions.get(callee_id as usize).ok_or_else(|| {
            ModuleVerificationError::MissingFunction {
                func: func.name.clone(),
                pc,
                callee_id,
            }
        })?;
        if inst.c != callee.param_slots {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "encoded arg slots {} do not match callee {} param_slots {}",
                    inst.c, callee.name, callee.param_slots
                ),
            ));
        }
        let expected_args = callee
            .slot_types
            .get(..callee.param_slots as usize)
            .ok_or_else(|| {
                call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "callee {} has {} slot_types but param_slots={}",
                        callee.name,
                        callee.slot_types.len(),
                        callee.param_slots
                    ),
                )
            })?;
        verify_layout(func, pc, opcode, inst.b, expected_args, "static call args")
    }
}

fn verify_iface_assign_contract(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let opcode = Opcode::IfaceAssign;
    let kind =
        ValueKind::try_from(inst.flags).map_err(|_| ModuleVerificationError::InvalidValueKind {
            func: func.name.clone(),
            pc,
            opcode,
            raw: inst.flags,
        })?;
    let constant = constant_at(func, module, pc, inst.c)?;
    let packed = if let Constant::Int(packed) = constant {
        *packed as u64
    } else {
        return Err(ModuleVerificationError::ConstantKindMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            const_id: inst.c,
            expected: "Int",
            actual: constant_kind(constant),
        });
    };
    verify_iface_assign_metadata_schema(func, module, pc, opcode, kind, packed)?;
    verify_interface_or_raw_pair(func, pc, opcode, inst.a, "IfaceAssign destination")?;
    verify_iface_assign_source(func, pc, opcode, inst.b, kind)
}

fn verify_iface_assert_contract(
    func: &FunctionDef,
    _module: &Module,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let opcode = Opcode::IfaceAssert;
    verify_interface_pair(func, pc, opcode, inst.b, "IfaceAssert source")?;
    let assert_kind = inst.flags & 0x03;
    if assert_kind > 1 {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("unsupported IfaceAssert kind {assert_kind}"),
        ));
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
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "IfaceAssert metadata layout slots {} do not match encoded destination slots {}",
                result_layout.len(),
                dst_slots
            ),
        ));
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
        verify_layout(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.a, dst_slots, "IfaceAssert ok")?,
            &[SlotType::Value],
            "IfaceAssert ok",
        )?;
    }
    Ok(())
}

fn verify_iface_assign_metadata_schema(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    opcode: Opcode,
    value_kind: ValueKind,
    packed: u64,
) -> Result<(), ModuleVerificationError> {
    let high = (packed >> 32) as u32;
    let low = (packed & 0xFFFF_FFFF) as u32;
    if value_kind == ValueKind::Interface {
        if high != 0 {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "interface source metadata must store target iface id in low word only, got high word {high}"
                ),
            ));
        }
        if low != 0 && low as usize >= module.interface_metas.len() {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "target interface meta id {low} exceeds interface metadata count {}",
                    module.interface_metas.len()
                ),
            ));
        }
    } else if low != 0 && low as usize >= module.itabs.len() {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("itab id {low} exceeds itab count {}", module.itabs.len()),
        ));
    }
    Ok(())
}

fn verify_iface_assign_source(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    src_slot: u16,
    value_kind: ValueKind,
) -> Result<(), ModuleVerificationError> {
    match value_kind {
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

fn verify_interface_or_raw_pair(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let actual = local_layout(func, pc, slot, 2, access)?;
    if actual == [SlotType::Interface0, SlotType::Interface1]
        || actual == [SlotType::Value, SlotType::Value]
    {
        Ok(())
    } else {
        Err(ModuleVerificationError::InvalidInterfaceLayout {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot,
            actual: actual.to_vec(),
        })
    }
}

fn verify_go_island_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
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
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "GoIsland metadata layout slots args={} returns={} do not match encoded args={}",
                arg_layout.len(),
                ret_layout.len(),
                inst.flags
            ),
        ));
    }
    verify_local_layout_matches(func, pc, opcode, inst.c, &arg_layout, "GoIsland args")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::{FieldMeta, GlobalDef, StructMeta};
    use crate::types::{ValueKind, ValueRttid};

    fn function_with_slot_types(slot_types: Vec<SlotType>) -> FunctionDef {
        FunctionDef {
            name: "f".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: slot_types.len() as u16,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: Vec::new(),
            jit_metadata: Vec::new(),
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(
                &slot_types,
            ),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
            slot_types,
        }
    }

    #[test]
    fn gc_layout_rejects_global_width_mismatch() {
        let mut module = Module::new("test".to_string());
        module.globals.push(GlobalDef {
            name: "g".to_string(),
            slots: 1,
            value_kind: ValueKind::String as u8,
            meta_id: 0,
            slot_types: Vec::new(),
        });
        module.functions.push(function_with_slot_types(Vec::new()));

        let err = validate_module_gc_layout(&module).unwrap_err();
        assert!(err
            .to_string()
            .contains("global 0 (g) slot_types len 0 does not match slots 1"));
    }

    #[test]
    fn gc_layout_rejects_struct_field_width_mismatch() {
        let mut module = Module::new("test".to_string());
        module.functions.push(function_with_slot_types(Vec::new()));
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::Value],
            fields: vec![FieldMeta {
                name: "field".to_string(),
                offset: 0,
                slot_count: 2,
                type_info: ValueRttid::from_raw(0),
                embedded: false,
                tag: None,
            }],
            field_index: Default::default(),
        });

        let err = validate_module_gc_layout(&module).unwrap_err();
        assert!(err
            .to_string()
            .contains("field 0 (field) slot range 0..2 exceeds struct slots 1"));
    }

    #[test]
    fn gc_layout_rejects_orphan_interface1() {
        let err = validate_interface_pairs("layout", &[SlotType::Interface1]).unwrap_err();
        assert!(err
            .to_string()
            .contains("Interface1 slot 0 is not preceded by Interface0"));
    }

    #[test]
    fn module_verifier_rejects_invalid_opcode() {
        let mut module = Module::new("bad-op".to_string());
        let mut func = function_with_slot_types(vec![SlotType::Value]);
        func.code = vec![Instruction {
            op: 254,
            flags: 0,
            a: 0,
            b: 0,
            c: 0,
        }];
        func.jit_metadata = vec![JitInstructionMetadata::None];
        module.functions.push(func);

        assert!(matches!(
            verify_module(&module),
            Err(ModuleVerificationError::InvalidOpcode { raw: 254, .. })
        ));
    }
}
