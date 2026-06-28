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
    ext_slot_kind_matches_slot_type, ext_slot_kinds_for_slot_types,
    known_builtin_extern_fixed_return_slot_types, known_builtin_extern_param_slot_types,
    known_builtin_extern_requires_precise_return_layout, known_builtin_extern_return_slot_count,
    slot_type_for_value_kind, validate_ext_param_kinds_with_label, Constant, ExtSlotKind,
    ExternDef, FunctionDef, JitInstructionMetadata, Module, ParamShape, ReturnFlags, TransferType,
    IFACE_ASSIGN_NO_ITAB, MAP_ITER_SLOTS, MAP_ITER_SLOT_TYPES,
};
use crate::instruction::{
    iface_assert_result_slots_from_flags, Instruction, Opcode, HINT_LOOP, HINT_NOP,
    LOOP_FLAG_HAS_DEFER, LOOP_FLAG_HAS_LABELED_BREAK, LOOP_FLAG_HAS_LABELED_CONTINUE,
    MAP_GET_MAX_VALUE_SLOTS, MAP_SET_MAX_KEY_VAL_SLOTS,
};
use crate::runtime_type::RuntimeType;
use crate::types::{SlotType, ValueKind, ValueMeta, ValueRttid, META_ID_MASK};

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

#[derive(Clone, Copy)]
struct InstructionVerifierContext<'a> {
    func: &'a FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
}

#[derive(Clone, Copy)]
struct VerifierAnalyses<'a> {
    module: &'a Module,
    constant_facts: &'a ConstantFactAnalysis,
    index_check_facts: &'a IndexCheckAnalysis,
    container_layout_facts: &'a ContainerLayoutAnalysis,
}

#[derive(Clone, Copy)]
struct LocalSlotRange {
    access: &'static str,
    start: u16,
    count: usize,
}

#[derive(Clone, Copy)]
struct IndexedAccessLabels {
    base: &'static str,
    index: &'static str,
    value: &'static str,
}

#[derive(Clone, Copy)]
struct MapLayoutExpectation<'a> {
    key_layout: &'a [SlotType],
    val_layout: &'a [SlotType],
}

#[derive(Clone, Copy)]
struct ItabReceiverContract<'a> {
    rttid: u32,
    value_kind: ValueKind,
    itab_id: u32,
    itab: &'a crate::bytecode::Itab,
}

#[derive(Clone, Copy)]
enum SlotExpectation<'a> {
    Exact(SlotType),
    OneOf(&'a [SlotType]),
}

impl SlotExpectation<'_> {
    fn verify(
        self,
        ctx: InstructionVerifierContext<'_>,
        slot: u16,
        access: &'static str,
    ) -> Result<(), ModuleVerificationError> {
        match self {
            SlotExpectation::Exact(expected) => {
                verify_layout(ctx.func, ctx.pc, ctx.opcode, slot, &[expected], access)
            }
            SlotExpectation::OneOf(expected_any) => verify_one_of_single_slot_layout(
                ctx.func,
                ctx.pc,
                ctx.opcode,
                slot,
                expected_any,
                access,
            ),
        }
    }
}

#[derive(Clone, Copy)]
struct BinarySlotContract<'a> {
    dst: SlotExpectation<'a>,
    lhs: SlotExpectation<'a>,
    rhs: SlotExpectation<'a>,
    dst_access: &'static str,
    lhs_access: &'static str,
    rhs_access: &'static str,
}

impl BinarySlotContract<'_> {
    fn exact(
        dst: SlotType,
        lhs: SlotType,
        rhs: SlotType,
        dst_access: &'static str,
        lhs_access: &'static str,
        rhs_access: &'static str,
    ) -> Self {
        Self {
            dst: SlotExpectation::Exact(dst),
            lhs: SlotExpectation::Exact(lhs),
            rhs: SlotExpectation::Exact(rhs),
            dst_access,
            lhs_access,
            rhs_access,
        }
    }
}

impl<'a> BinarySlotContract<'a> {
    fn one_of(
        dst: &'a [SlotType],
        lhs: &'a [SlotType],
        rhs: &'a [SlotType],
        dst_access: &'static str,
        lhs_access: &'static str,
        rhs_access: &'static str,
    ) -> Self {
        Self {
            dst: SlotExpectation::OneOf(dst),
            lhs: SlotExpectation::OneOf(lhs),
            rhs: SlotExpectation::OneOf(rhs),
            dst_access,
            lhs_access,
            rhs_access,
        }
    }
}

#[derive(Clone, Copy)]
struct UnarySlotContract<'a> {
    dst: SlotExpectation<'a>,
    src: SlotExpectation<'a>,
    dst_access: &'static str,
    src_access: &'static str,
}

impl UnarySlotContract<'_> {
    fn exact(
        dst: SlotType,
        src: SlotType,
        dst_access: &'static str,
        src_access: &'static str,
    ) -> Self {
        Self {
            dst: SlotExpectation::Exact(dst),
            src: SlotExpectation::Exact(src),
            dst_access,
            src_access,
        }
    }
}

impl<'a> UnarySlotContract<'a> {
    fn one_of(
        dst: &'a [SlotType],
        src: &'a [SlotType],
        dst_access: &'static str,
        src_access: &'static str,
    ) -> Self {
        Self {
            dst: SlotExpectation::OneOf(dst),
            src: SlotExpectation::OneOf(src),
            dst_access,
            src_access,
        }
    }
}

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
    for (idx, extern_def) in module.externs.iter().enumerate() {
        if crate::bytecode::ExternEffects::from_bits(extern_def.allowed_effects.bits()).is_none() {
            return Err(invariant(format!(
                "externs[{idx}] ({}) has invalid allowed_effects bits 0x{:x}",
                extern_def.name,
                extern_def.allowed_effects.bits()
            )));
        }
        extern_def
            .returns
            .validate_with_label(&format!("externs[{idx}] ({})", extern_def.name))
            .map_err(invariant)?;
        for (slot_idx, iface_meta_id) in extern_def.returns.interface_metas.iter().enumerate() {
            let Some(iface_meta_id) = iface_meta_id else {
                continue;
            };
            if *iface_meta_id as usize >= module.interface_metas.len() {
                return Err(invariant(format!(
                    "externs[{idx}] ({}) return slot {slot_idx} expected interface meta id {} exceeds interface metadata count {}",
                    extern_def.name,
                    iface_meta_id,
                    module.interface_metas.len()
                )));
            }
        }
        validate_ext_param_kinds_with_label(
            &extern_def.params,
            &extern_def.param_kinds,
            &format!("externs[{idx}] ({})", extern_def.name),
        )
        .map_err(invariant)?;
        if let Some(expected_layout) = known_builtin_extern_param_slot_types(&extern_def.name) {
            let expected_params = ParamShape::Exact {
                slots: expected_layout.len() as u16,
            };
            if extern_def.params != expected_params {
                return Err(invariant(format!(
                    "externs[{idx}] ({}) builtin params must be {}, got {}",
                    extern_def.name,
                    expected_params.display_name(),
                    extern_def.params.display_name()
                )));
            }
            if !extern_def.param_kinds.is_empty() {
                let expected_kinds = ext_slot_kinds_for_slot_types(expected_layout);
                if extern_def.param_kinds != expected_kinds {
                    return Err(invariant(format!(
                        "externs[{idx}] ({}) builtin param_kinds do not match builtin layout",
                        extern_def.name
                    )));
                }
            }
        }
        if let Some(expected_slots) = known_builtin_extern_return_slot_count(&extern_def.name) {
            if extern_def.returns.slots != expected_slots {
                return Err(invariant(format!(
                    "externs[{idx}] ({}) builtin returns must be fixed({expected_slots}), got {}",
                    extern_def.name, extern_def.returns.slots
                )));
            }
        }
        if let Some(expected_layout) =
            known_builtin_extern_fixed_return_slot_types(&extern_def.name)
        {
            if extern_def.returns.slot_types != expected_layout {
                return Err(invariant(format!(
                    "externs[{idx}] ({}) builtin return slot_types must match fixed layout",
                    extern_def.name
                )));
            }
        }
        if known_builtin_extern_requires_precise_return_layout(&extern_def.name)
            && extern_def.returns.slot_types.is_empty()
        {
            return Err(invariant(format!(
                "externs[{idx}] ({}) builtin returns require precise return slot_types",
                extern_def.name
            )));
        }
        validate_dynamic_extern_param_contract(idx, extern_def).map_err(invariant)?;
    }
    validate_same_name_extern_abi_shapes(module).map_err(invariant)?;
    for (idx, itab) in module.itabs.iter().enumerate() {
        if idx == 0 && !itab.methods.is_empty() {
            return Err(invariant(
                "itab 0 is reserved for empty-interface no-itab values".to_string(),
            ));
        }
        if idx != 0 {
            let Some(iface_meta) = module.interface_metas.get(itab.iface_meta_id as usize) else {
                return Err(invariant(format!(
                    "itab {idx} target interface meta id {} exceeds interface metadata count {}",
                    itab.iface_meta_id,
                    module.interface_metas.len()
                )));
            };
            if itab.methods.len() != iface_meta.methods.len() {
                return Err(invariant(format!(
                    "itab {idx} method count {} does not match interface {} method count {}",
                    itab.methods.len(),
                    itab.iface_meta_id,
                    iface_meta.methods.len()
                )));
            }
        }
        for (method_idx, &func_id) in itab.methods.iter().enumerate() {
            if func_id as usize >= module.functions.len() {
                return Err(invariant(format!(
                    "itab {idx} references missing function {func_id}"
                )));
            }
            validate_call_iface_itab_target(module, idx, method_idx, func_id)?;
        }
    }
    for (idx, named) in module.named_type_metas.iter().enumerate() {
        validate_value_meta_ref(
            module,
            named.underlying_meta,
            &format!("named_type_metas[{idx}] underlying_meta"),
        )?;
        validate_value_rttid_ref(
            module,
            named.underlying_rttid,
            &format!("named_type_metas[{idx}] underlying_rttid"),
        )?;
        validate_named_underlying_meta(module, idx, named)?;
        for (name, method) in &named.methods {
            if method.func_id as usize >= module.functions.len() {
                return Err(invariant(format!(
                    "named_type_metas[{idx}] method {name} references missing function {}",
                    method.func_id
                )));
            }
            validate_named_method_receiver_abi(module, idx, name, named, method)?;
            validate_signature_rttid(
                module,
                method.signature_rttid,
                &format!("named_type_metas[{idx}] method {name} signature_rttid"),
            )?;
        }
    }
    validate_struct_metadata_refs(module)?;
    validate_interface_metadata_refs(module)?;
    validate_runtime_type_refs(module)?;
    validate_global_metadata_refs(module)?;
    validate_well_known_types(module)?;
    validate_debug_info_refs(module)?;
    Ok(())
}

fn module_invariant(detail: String) -> ModuleVerificationError {
    ModuleVerificationError::ModuleInvariant { detail }
}

fn validate_same_name_extern_abi_shapes(module: &Module) -> Result<(), String> {
    for idx in 0..module.externs.len() {
        let current = &module.externs[idx];
        for prev_idx in 0..idx {
            let previous = &module.externs[prev_idx];
            if previous.name != current.name {
                continue;
            }
            if previous.params == current.params
                && previous.returns == current.returns
                && previous.param_kinds == current.param_kinds
            {
                continue;
            }
            if is_vm_owned_variable_shape_extern(&current.name) {
                continue;
            }
            return Err(format!(
                "same-name extern {} has incompatible ABI shapes between externs[{prev_idx}] and externs[{idx}]",
                current.name
            ));
        }
    }
    Ok(())
}

fn is_vm_owned_variable_shape_extern(name: &str) -> bool {
    matches!(name, "dyn_call" | "dyn_method" | "dyn_field" | "dyn_index")
}

fn dynamic_call_extern_param_prefix(name: &str) -> Option<&'static [SlotType]> {
    match name {
        "dyn_call" => Some(&[
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::GcRef,
            SlotType::Value,
        ]),
        "dyn_method" => Some(&[
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::Value,
        ]),
        _ => None,
    }
}

fn validate_dynamic_call_extern_layout(name: &str, layout: &[SlotType]) -> Result<(), String> {
    let Some(prefix) = dynamic_call_extern_param_prefix(name) else {
        return Ok(());
    };
    let suffix_len = layout.len().checked_sub(prefix.len()).ok_or_else(|| {
        format!(
            "dynamic extern {name} argument layout has {} slots but requires at least {}",
            layout.len(),
            prefix.len()
        )
    })?;
    if !layout.starts_with(prefix) {
        return Err(format!(
            "dynamic extern {name} argument layout does not match required protocol prefix"
        ));
    }
    if suffix_len % 2 != 0
        || layout[prefix.len()..]
            .iter()
            .any(|slot_type| *slot_type != SlotType::Value)
    {
        return Err(format!(
            "dynamic extern {name} argument layout must encode value metadata/is-any pairs after the protocol prefix"
        ));
    }
    Ok(())
}

fn validate_dynamic_extern_param_contract(
    idx: usize,
    extern_def: &ExternDef,
) -> Result<(), String> {
    let Some(prefix) = dynamic_call_extern_param_prefix(&extern_def.name) else {
        return Ok(());
    };
    let Some(param_slots) = extern_def.params.exact_slots() else {
        return Ok(());
    };
    let suffix_len = (param_slots as usize)
        .checked_sub(prefix.len())
        .ok_or_else(|| {
            format!(
                "externs[{idx}] ({}) dynamic extern argument layout has {} slots but requires at least {}",
                extern_def.name,
                param_slots,
                prefix.len()
            )
        })?;
    if suffix_len % 2 != 0 {
        return Err(format!(
            "externs[{idx}] ({}) dynamic extern argument layout must encode value metadata/is-any pairs after the protocol prefix",
            extern_def.name
        ));
    }
    if !extern_def.param_kinds.is_empty() {
        let mut expected = ext_slot_kinds_for_slot_types(prefix);
        expected.extend((0..suffix_len).map(|_| ExtSlotKind::Value));
        if extern_def.param_kinds != expected {
            return Err(format!(
                "externs[{idx}] ({}) dynamic extern param_kinds do not match required protocol layout",
                extern_def.name
            ));
        }
    }
    Ok(())
}

fn validate_named_method_receiver_abi(
    module: &Module,
    named_idx: usize,
    method_name: &str,
    named: &crate::bytecode::NamedTypeMeta,
    method: &crate::bytecode::MethodInfo,
) -> Result<(), ModuleVerificationError> {
    if !method.receiver_is_iface_boxed {
        return Ok(());
    }
    if method.is_pointer_receiver {
        return Err(module_invariant(format!(
            "named_type_metas[{named_idx}] method {method_name} cannot mark pointer receiver as interface-boxed"
        )));
    }
    if !named.underlying_meta.value_kind().needs_boxing() {
        return Err(module_invariant(format!(
            "named_type_metas[{named_idx}] method {method_name} marks non-boxed {:?} receiver as interface-boxed",
            named.underlying_meta.value_kind()
        )));
    }
    let func = &module.functions[method.func_id as usize];
    if func.recv_slots != 1 || func.slot_types.first() != Some(&SlotType::GcRef) {
        return Err(module_invariant(format!(
            "named_type_metas[{named_idx}] method {method_name} interface-boxed receiver target {} ({}) must have one GcRef receiver slot, got recv_slots={} first_slot={:?}",
            method.func_id,
            func.name,
            func.recv_slots,
            func.slot_types.first()
        )));
    }
    Ok(())
}

fn validate_call_iface_itab_target(
    module: &Module,
    itab_idx: usize,
    method_idx: usize,
    func_id: u32,
) -> Result<(), ModuleVerificationError> {
    let func = module.functions.get(func_id as usize).ok_or_else(|| {
        module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} references missing function {func_id}"
        ))
    })?;
    if func.recv_slots != 1 {
        return Err(module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} target function {func_id} ({}) must have exactly one receiver slot, got recv_slots={}",
            func.name, func.recv_slots
        )));
    }
    if func.param_slots < func.recv_slots {
        return Err(module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} target function {func_id} ({}) has param_slots={} below recv_slots={}",
            func.name, func.param_slots, func.recv_slots
        )));
    }
    if func.slot_types.len() < func.recv_slots as usize {
        return Err(module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} target function {func_id} ({}) has {} slot_types but recv_slots={}",
            func.name,
            func.slot_types.len(),
            func.recv_slots
        )));
    }
    validate_call_iface_itab_target_signature(module, itab_idx, method_idx, func_id, &func.name)?;
    Ok(())
}

fn validate_call_iface_itab_target_signature(
    module: &Module,
    itab_idx: usize,
    method_idx: usize,
    func_id: u32,
    func_name: &str,
) -> Result<(), ModuleVerificationError> {
    let itab = module.itabs.get(itab_idx).ok_or_else(|| {
        module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} references missing itab"
        ))
    })?;
    let iface_meta = module
        .interface_metas
        .get(itab.iface_meta_id as usize)
        .ok_or_else(|| {
            module_invariant(format!(
                "CallIface itab {itab_idx} target interface meta id {} is missing",
                itab.iface_meta_id
            ))
        })?;
    let iface_method = iface_meta.methods.get(method_idx).ok_or_else(|| {
        module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} exceeds interface {} method count {}",
            itab.iface_meta_id,
            iface_meta.methods.len()
        ))
    })?;

    let mut saw_same_name_target = false;
    let mut saw_target_function = false;
    let mut first_signature_mismatch: Option<u32> = None;
    for named in &module.named_type_metas {
        for (method_name, method) in &named.methods {
            if method.func_id != func_id {
                continue;
            }
            saw_target_function = true;
            if method_name != &iface_method.name {
                continue;
            }
            saw_same_name_target = true;
            if method.signature_rttid == iface_method.signature_rttid {
                return Ok(());
            }
            first_signature_mismatch.get_or_insert(method.signature_rttid);
        }
    }

    if let Some(actual_signature) = first_signature_mismatch {
        return Err(module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} target function {func_id} ({func_name}) signature_rttid={actual_signature} does not match interface method {} signature_rttid={}",
            iface_method.name,
            iface_method.signature_rttid
        )));
    }
    if saw_target_function && !saw_same_name_target {
        return Err(module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} expected interface method {} but itab references {func_id} ({func_name}) registered under a different method name",
            iface_method.name
        )));
    }
    Err(module_invariant(format!(
        "CallIface itab {itab_idx} method {method_idx} target function {func_id} ({func_name}) is not registered as a named method"
    )))
}

fn validate_value_kind_tag(raw: u8, label: &str) -> Result<ValueKind, ModuleVerificationError> {
    ValueKind::try_from(raw)
        .map_err(|_| module_invariant(format!("{label} has invalid ValueKind tag {raw}")))
}

fn validate_value_meta_ref(
    module: &Module,
    value_meta: ValueMeta,
    label: &str,
) -> Result<ValueKind, ModuleVerificationError> {
    let kind = validate_value_kind_tag(value_meta.to_raw() as u8, label)?;
    let meta_id = value_meta.meta_id() as usize;
    match kind {
        ValueKind::Struct if meta_id >= module.struct_metas.len() => {
            Err(module_invariant(format!(
                "{label} references missing struct metadata {}",
                value_meta.meta_id()
            )))
        }
        ValueKind::Pointer if meta_id != 0 && meta_id >= module.struct_metas.len() => {
            Err(module_invariant(format!(
                "{label} references missing pointer target struct metadata {}",
                value_meta.meta_id()
            )))
        }
        ValueKind::Interface if meta_id >= module.interface_metas.len() => {
            Err(module_invariant(format!(
                "{label} references missing interface metadata {}",
                value_meta.meta_id()
            )))
        }
        ValueKind::Array => {
            if meta_id >= module.runtime_types.len() {
                return Err(module_invariant(format!(
                    "{label} references missing array runtime type {}",
                    value_meta.meta_id()
                )));
            }
            let expected = expected_value_kind_for_rttid(module, meta_id, label)?;
            if expected != ValueKind::Array {
                return Err(module_invariant(format!(
                    "{label} references runtime_types[{}] with kind {expected:?}, expected Array",
                    value_meta.meta_id()
                )));
            }
            Ok(kind)
        }
        _ => Ok(kind),
    }
}

fn validate_value_rttid_ref(
    module: &Module,
    value_rttid: ValueRttid,
    label: &str,
) -> Result<ValueKind, ModuleVerificationError> {
    let kind = validate_value_kind_tag(value_rttid.to_raw() as u8, label)?;
    let rttid = value_rttid.rttid() as usize;
    if rttid >= module.runtime_types.len() {
        return Err(module_invariant(format!(
            "{label} references missing runtime type {}",
            value_rttid.rttid()
        )));
    }
    let expected = expected_value_kind_for_rttid(module, rttid, label)?;
    if kind != expected {
        return Err(module_invariant(format!(
            "{label} ValueKind {kind:?} does not match runtime_types[{}] expected {expected:?}",
            value_rttid.rttid()
        )));
    }
    Ok(kind)
}

fn expected_value_kind_for_rttid(
    module: &Module,
    rttid: usize,
    label: &str,
) -> Result<ValueKind, ModuleVerificationError> {
    match &module.runtime_types[rttid] {
        RuntimeType::Basic(kind) => Ok(*kind),
        RuntimeType::Named { id, .. } => {
            let Some(named) = module.named_type_metas.get(*id as usize) else {
                return Err(module_invariant(format!(
                    "{label} references runtime_types[{rttid}] Named with missing \
                     named_type_metas[{id}]"
                )));
            };
            Ok(named.underlying_rttid.value_kind())
        }
        RuntimeType::Pointer(_) => Ok(ValueKind::Pointer),
        RuntimeType::Array { .. } => Ok(ValueKind::Array),
        RuntimeType::Slice(_) => Ok(ValueKind::Slice),
        RuntimeType::Map { .. } => Ok(ValueKind::Map),
        RuntimeType::Chan { .. } => Ok(ValueKind::Channel),
        RuntimeType::Port { .. } => Ok(ValueKind::Port),
        RuntimeType::Func { .. } => Ok(ValueKind::Closure),
        RuntimeType::Struct { .. } => Ok(ValueKind::Struct),
        RuntimeType::Interface { .. } => Ok(ValueKind::Interface),
        RuntimeType::Tuple(_) => Ok(ValueKind::Void),
        RuntimeType::Island => Ok(ValueKind::Island),
    }
}

fn validate_named_underlying_meta(
    module: &Module,
    idx: usize,
    named: &crate::bytecode::NamedTypeMeta,
) -> Result<(), ModuleVerificationError> {
    let Some(canonical) = module.canonical_value_meta_for_value_rttid(named.underlying_rttid)
    else {
        return Err(module_invariant(format!(
            "named_type_metas[{idx}] underlying_rttid cannot be resolved to canonical metadata"
        )));
    };
    if named.underlying_meta != canonical {
        return Err(module_invariant(format!(
            "named_type_metas[{idx}] underlying_meta raw 0x{:x} does not match canonical raw 0x{:x} from underlying_rttid {}",
            named.underlying_meta.to_raw(),
            canonical.to_raw(),
            named.underlying_rttid.rttid()
        )));
    }
    Ok(())
}

fn validate_signature_rttid(
    module: &Module,
    rttid: u32,
    label: &str,
) -> Result<(), ModuleVerificationError> {
    let Some(runtime_type) = module.runtime_types.get(rttid as usize) else {
        return Err(module_invariant(format!(
            "{label} references missing runtime type {rttid}"
        )));
    };
    if !matches!(runtime_type, RuntimeType::Func { .. }) {
        return Err(module_invariant(format!(
            "{label} must reference a function runtime type, got {runtime_type:?}"
        )));
    }
    Ok(())
}

fn validate_struct_metadata_refs(module: &Module) -> Result<(), ModuleVerificationError> {
    for (idx, meta) in module.struct_metas.iter().enumerate() {
        for (field_idx, field) in meta.fields.iter().enumerate() {
            validate_value_rttid_ref(
                module,
                field.type_info,
                &format!("struct_metas[{idx}] field {field_idx} type_info"),
            )?;
        }
        for (name, &field_idx) in &meta.field_index {
            let Some(field) = meta.fields.get(field_idx) else {
                return Err(module_invariant(format!(
                    "struct_metas[{idx}] field_index entry {name} references missing field {field_idx}"
                )));
            };
            if field.name != *name {
                return Err(module_invariant(format!(
                    "struct_metas[{idx}] field_index entry {name} points to field named {}",
                    field.name
                )));
            }
        }
    }
    Ok(())
}

fn validate_interface_metadata_refs(module: &Module) -> Result<(), ModuleVerificationError> {
    for (idx, meta) in module.interface_metas.iter().enumerate() {
        if meta.method_names.len() != meta.methods.len() {
            return Err(module_invariant(format!(
                "interface_metas[{idx}] method_names.len()={} but methods.len()={}",
                meta.method_names.len(),
                meta.methods.len()
            )));
        }
        for (method_idx, method) in meta.methods.iter().enumerate() {
            if meta.method_names.get(method_idx) != Some(&method.name) {
                return Err(module_invariant(format!(
                    "interface_metas[{idx}] method {method_idx} name {} does not match method_names",
                    method.name
                )));
            }
            validate_signature_rttid(
                module,
                method.signature_rttid,
                &format!(
                    "interface_metas[{idx}] method {} signature_rttid",
                    method.name
                ),
            )?;
        }
    }
    Ok(())
}

fn validate_runtime_type_refs(module: &Module) -> Result<(), ModuleVerificationError> {
    for (idx, runtime_type) in module.runtime_types.iter().enumerate() {
        let label = format!("runtime_types[{idx}]");
        match runtime_type {
            RuntimeType::Basic(_) | RuntimeType::Island => {}
            RuntimeType::Named { id, struct_meta_id } => {
                if *id as usize >= module.named_type_metas.len() {
                    return Err(module_invariant(format!(
                        "{label} Named references missing named_type_metas[{id}]"
                    )));
                }
                let expected_struct_meta_id = module
                    .named_type_metas
                    .get(*id as usize)
                    .filter(|meta| meta.underlying_meta.value_kind() == ValueKind::Struct)
                    .map(|meta| meta.underlying_meta.meta_id());
                if *struct_meta_id != expected_struct_meta_id {
                    return Err(module_invariant(format!(
                        "{label} Named struct_meta_id {struct_meta_id:?} does not match named_type_metas[{id}] canonical struct_meta_id {expected_struct_meta_id:?}"
                    )));
                }
                if let Some(struct_meta_id) = struct_meta_id {
                    if *struct_meta_id as usize >= module.struct_metas.len() {
                        return Err(module_invariant(format!(
                            "{label} Named references missing struct metadata {struct_meta_id}"
                        )));
                    }
                }
            }
            RuntimeType::Pointer(elem)
            | RuntimeType::Slice(elem)
            | RuntimeType::Chan { elem, .. }
            | RuntimeType::Port { elem, .. } => {
                validate_value_rttid_ref(module, *elem, &format!("{label} element"))?;
            }
            RuntimeType::Array { elem, .. } => {
                validate_value_rttid_ref(module, *elem, &format!("{label} element"))?;
            }
            RuntimeType::Map { key, val } => {
                validate_value_rttid_ref(module, *key, &format!("{label} key"))?;
                validate_value_rttid_ref(module, *val, &format!("{label} value"))?;
            }
            RuntimeType::Func {
                params, results, ..
            } => {
                for (param_idx, param) in params.iter().enumerate() {
                    validate_value_rttid_ref(
                        module,
                        *param,
                        &format!("{label} param {param_idx}"),
                    )?;
                }
                for (result_idx, result) in results.iter().enumerate() {
                    validate_value_rttid_ref(
                        module,
                        *result,
                        &format!("{label} result {result_idx}"),
                    )?;
                }
            }
            RuntimeType::Struct { fields, meta_id } => {
                if *meta_id as usize >= module.struct_metas.len() {
                    return Err(module_invariant(format!(
                        "{label} Struct references missing struct metadata {meta_id}"
                    )));
                }
                for (field_idx, field) in fields.iter().enumerate() {
                    validate_value_rttid_ref(
                        module,
                        field.typ,
                        &format!("{label} field {field_idx} type"),
                    )?;
                }
            }
            RuntimeType::Interface { methods, meta_id } => {
                if *meta_id as usize >= module.interface_metas.len() {
                    return Err(module_invariant(format!(
                        "{label} Interface references missing interface metadata {meta_id}"
                    )));
                }
                for (method_idx, method) in methods.iter().enumerate() {
                    validate_value_rttid_ref(
                        module,
                        method.sig,
                        &format!("{label} method {method_idx} signature"),
                    )?;
                    validate_signature_rttid(
                        module,
                        method.sig.rttid(),
                        &format!("{label} method {method_idx} signature"),
                    )?;
                }
            }
            RuntimeType::Tuple(elems) => {
                for (elem_idx, elem) in elems.iter().enumerate() {
                    validate_value_rttid_ref(
                        module,
                        *elem,
                        &format!("{label} tuple element {elem_idx}"),
                    )?;
                }
            }
        }
    }
    Ok(())
}

fn validate_global_metadata_refs(module: &Module) -> Result<(), ModuleVerificationError> {
    for (idx, global) in module.globals.iter().enumerate() {
        let kind = validate_value_kind_tag(
            global.value_kind,
            &format!("globals[{idx}] ({}) value_kind", global.name),
        )?;
        validate_value_meta_ref(
            module,
            ValueMeta::new(global.meta_id, kind),
            &format!("globals[{idx}] ({}) metadata", global.name),
        )?;
    }
    Ok(())
}

fn validate_well_known_types(module: &Module) -> Result<(), ModuleVerificationError> {
    let well_known = &module.well_known;
    validate_optional_table_ref(
        "well_known.error_named_type_id",
        well_known.error_named_type_id,
        module.named_type_metas.len(),
        "named_type_metas",
    )?;
    validate_optional_table_ref(
        "well_known.error_iface_meta_id",
        well_known.error_iface_meta_id,
        module.interface_metas.len(),
        "interface_metas",
    )?;
    validate_optional_table_ref(
        "well_known.error_struct_meta_id",
        well_known.error_struct_meta_id,
        module.struct_metas.len(),
        "struct_metas",
    )?;
    if let Some(rttid) = well_known.error_ptr_rttid {
        let Some(runtime_type) = module.runtime_types.get(rttid as usize) else {
            return Err(module_invariant(format!(
                "well_known.error_ptr_rttid references missing runtime type {rttid}"
            )));
        };
        if !matches!(runtime_type, RuntimeType::Pointer(_)) {
            return Err(module_invariant(format!(
                "well_known.error_ptr_rttid must reference a pointer runtime type, got {runtime_type:?}"
            )));
        }
    }
    if let Some(offsets) = well_known.error_field_offsets {
        let Some(struct_id) = well_known.error_struct_meta_id else {
            return Err(module_invariant(
                "well_known.error_field_offsets present without error_struct_meta_id".to_string(),
            ));
        };
        let Some(struct_meta) = module.struct_metas.get(struct_id as usize) else {
            return Err(module_invariant(format!(
                "well_known.error_field_offsets references missing struct_metas[{struct_id}]"
            )));
        };
        for (idx, offset) in offsets.iter().enumerate() {
            if *offset as usize >= struct_meta.slot_types.len() {
                return Err(module_invariant(format!(
                    "well_known.error_field_offsets[{idx}]={offset} exceeds error struct slots {}",
                    struct_meta.slot_types.len()
                )));
            }
        }
    }
    validate_optional_table_ref(
        "well_known.attr_object_iface_id",
        well_known.attr_object_iface_id,
        module.interface_metas.len(),
        "interface_metas",
    )?;
    validate_optional_table_ref(
        "well_known.set_attr_object_iface_id",
        well_known.set_attr_object_iface_id,
        module.interface_metas.len(),
        "interface_metas",
    )?;
    validate_optional_table_ref(
        "well_known.index_object_iface_id",
        well_known.index_object_iface_id,
        module.interface_metas.len(),
        "interface_metas",
    )?;
    validate_optional_table_ref(
        "well_known.set_index_object_iface_id",
        well_known.set_index_object_iface_id,
        module.interface_metas.len(),
        "interface_metas",
    )?;
    validate_optional_table_ref(
        "well_known.call_object_iface_id",
        well_known.call_object_iface_id,
        module.interface_metas.len(),
        "interface_metas",
    )?;
    Ok(())
}

fn validate_optional_table_ref(
    label: &'static str,
    id: Option<u32>,
    len: usize,
    table: &'static str,
) -> Result<(), ModuleVerificationError> {
    if let Some(id) = id {
        if id as usize >= len {
            return Err(module_invariant(format!(
                "{label} references missing {table}[{id}]"
            )));
        }
    }
    Ok(())
}

fn validate_debug_info_refs(module: &Module) -> Result<(), ModuleVerificationError> {
    if module.debug_info.funcs.len() > module.functions.len() {
        return Err(module_invariant(format!(
            "debug_info has {} function entries but module has {} functions",
            module.debug_info.funcs.len(),
            module.functions.len()
        )));
    }
    for (func_id, debug_func) in module.debug_info.funcs.iter().enumerate() {
        let code_len = module.functions[func_id].code.len();
        let mut previous_pc = None;
        for (entry_idx, entry) in debug_func.entries.iter().enumerate() {
            if entry.file_id as usize >= module.debug_info.files.len() {
                return Err(module_invariant(format!(
                    "debug_info.funcs[{func_id}].entries[{entry_idx}] references missing file {}",
                    entry.file_id
                )));
            }
            if entry.pc as usize >= code_len {
                return Err(module_invariant(format!(
                    "debug_info.funcs[{func_id}].entries[{entry_idx}] pc {} exceeds code length {}",
                    entry.pc, code_len
                )));
            }
            if previous_pc.is_some_and(|prev| entry.pc < prev) {
                return Err(module_invariant(format!(
                    "debug_info.funcs[{func_id}] entries are not sorted by pc"
                )));
            }
            previous_pc = Some(entry.pc);
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
    verify_function_invariants(func, module)?;

    if func.code.len() != func.jit_metadata.len() {
        return Err(ModuleVerificationError::LengthMismatch {
            func: func.name.clone(),
            code_len: func.code.len(),
            metadata_len: func.jit_metadata.len(),
        });
    }
    for (pc, metadata) in func.jit_metadata.iter().enumerate() {
        validate_instruction_metadata_shape(func, pc, metadata)?;
    }
    verify_select_case_structure(func)?;
    let constant_facts = ConstantFactAnalysis::analyze(func, module);
    let index_check_facts = IndexCheckAnalysis::analyze(func, module, &constant_facts);
    let container_layout_facts = ContainerLayoutAnalysis::analyze(func, module);
    let analyses = VerifierAnalyses {
        module,
        constant_facts: &constant_facts,
        index_check_facts: &index_check_facts,
        container_layout_facts: &container_layout_facts,
    };

    for (pc, inst) in func.code.iter().copied().enumerate() {
        let opcode = inst.opcode();
        if opcode == Opcode::Invalid {
            return Err(ModuleVerificationError::InvalidOpcode {
                func: func.name.clone(),
                pc,
                raw: inst.op,
            });
        }
        verify_instruction_contract(func, analyses, pc, inst, opcode)?;
    }

    Ok(())
}

struct PendingSelectCases {
    begin_pc: usize,
    expected: u16,
    seen: u16,
    has_default: bool,
    source_indices: Vec<u16>,
}

fn verify_select_case_structure(func: &FunctionDef) -> Result<(), ModuleVerificationError> {
    let invariant = |detail: String| ModuleVerificationError::FunctionInvariant {
        func: func.name.clone(),
        detail,
    };
    let mut pending: Option<PendingSelectCases> = None;

    for (pc, inst) in func.code.iter().copied().enumerate() {
        let opcode = inst.opcode();
        match opcode {
            Opcode::SelectBegin => {
                if inst.flags & !0x01 != 0 {
                    return Err(ModuleVerificationError::InvalidInstructionFlags {
                        func: func.name.clone(),
                        pc,
                        opcode,
                        flags: inst.flags,
                        allowed: 0x01,
                    });
                }
                if let Some(select) = pending.as_ref() {
                    return Err(invariant(format!(
                        "SelectBegin at pc {pc} nested before SelectExec for SelectBegin at pc {}",
                        select.begin_pc
                    )));
                }
                pending = Some(PendingSelectCases {
                    begin_pc: pc,
                    expected: inst.a,
                    seen: 0,
                    has_default: inst.flags & 0x01 != 0,
                    source_indices: Vec::new(),
                });
            }
            Opcode::SelectSend | Opcode::SelectRecv => {
                let Some(select) = pending.as_mut() else {
                    return Err(invariant(format!(
                        "{opcode:?} at pc {pc} without active SelectBegin"
                    )));
                };
                if select.seen >= select.expected {
                    return Err(invariant(format!(
                        "SelectBegin declared {} cases but saw extra {opcode:?} at pc {pc}",
                        select.expected
                    )));
                }
                let source_domain = select.expected as usize + usize::from(select.has_default);
                if inst.c as usize >= source_domain {
                    return Err(invariant(format!(
                        "{opcode:?} at pc {pc} has source case index {} outside valid domain 0..{source_domain}",
                        inst.c
                    )));
                }
                if select.source_indices.contains(&inst.c) {
                    return Err(invariant(format!(
                        "{opcode:?} at pc {pc} has duplicate source case index {}",
                        inst.c
                    )));
                }
                select.source_indices.push(inst.c);
                select.seen += 1;
            }
            Opcode::SelectExec => {
                let Some(select) = pending.take() else {
                    return Err(invariant(format!(
                        "SelectExec at pc {pc} without active SelectBegin"
                    )));
                };
                if select.seen != select.expected {
                    return Err(invariant(format!(
                        "SelectBegin declared {} cases but SelectExec saw {}",
                        select.expected, select.seen
                    )));
                }
            }
            _ if pending.is_some() => {
                let select = pending.as_ref().expect("checked pending select");
                return Err(invariant(format!(
                    "non-select opcode {opcode:?} while SelectBegin at pc {} is pending",
                    select.begin_pc
                )));
            }
            _ => {}
        }
    }

    if let Some(select) = pending {
        return Err(invariant(format!(
            "SelectBegin at pc {} declared {} cases but has no SelectExec",
            select.begin_pc, select.expected
        )));
    }

    Ok(())
}

fn validate_instruction_metadata_shape(
    func: &FunctionDef,
    pc: usize,
    metadata: &JitInstructionMetadata,
) -> Result<(), ModuleVerificationError> {
    match metadata {
        JitInstructionMetadata::None | JitInstructionMetadata::LoopEnd { .. } => Ok(()),
        JitInstructionMetadata::ElemLayout {
            elem_bytes,
            needs_sign_extend,
            slot_layout,
        } => {
            if *elem_bytes == 0 {
                if *needs_sign_extend {
                    return Err(instruction_metadata_invariant(
                        func,
                        pc,
                        "zero-byte ElemLayout cannot request sign extension".to_string(),
                    ));
                }
                if !slot_layout.is_empty() && slot_layout.as_slice() != [SlotType::Value] {
                    return Err(instruction_metadata_invariant(
                        func,
                        pc,
                        format!(
                            "zero-byte ElemLayout logical slot_layout must be empty or [Value], got {slot_layout:?}"
                        ),
                    ));
                }
                return Ok(());
            }

            let expected_slots = (*elem_bytes as usize).div_ceil(8);
            if slot_layout.len() != expected_slots {
                return Err(instruction_metadata_invariant(
                    func,
                    pc,
                    format!(
                        "ElemLayout slot_layout.len()={} but elem_bytes={} requires {} slots",
                        slot_layout.len(),
                        elem_bytes,
                        expected_slots
                    ),
                ));
            }
            Ok(())
        }
        JitInstructionMetadata::MapNew { .. }
        | JitInstructionMetadata::MapGet { .. }
        | JitInstructionMetadata::MapSet { .. }
        | JitInstructionMetadata::MapDelete { .. }
        | JitInstructionMetadata::PtrLayout { .. }
        | JitInstructionMetadata::SlotLayout { .. }
        | JitInstructionMetadata::CallLayout { .. }
        | JitInstructionMetadata::CallIfaceLayout { .. }
        | JitInstructionMetadata::CallExternLayout { .. }
        | JitInstructionMetadata::QueueLayout { .. }
        | JitInstructionMetadata::MapIterNext { .. }
        | JitInstructionMetadata::IfaceAssertLayout { .. } => {
            // Metadata kind and interface-pair semantics are enforced by
            // opcode-specific VM/JIT contracts. The common shape check only
            // rejects self-inconsistent byte/slot metadata that every backend
            // would misinterpret.
            Ok(())
        }
    }
}

fn instruction_metadata_invariant(
    func: &FunctionDef,
    pc: usize,
    detail: String,
) -> ModuleVerificationError {
    ModuleVerificationError::FunctionInvariant {
        func: func.name.clone(),
        detail: format!("instruction metadata at pc {pc}: {detail}"),
    }
}

fn verify_function_invariants(
    func: &FunctionDef,
    module: &Module,
) -> Result<(), ModuleVerificationError> {
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
        let heap_ret_slot_sum = func.heap_ret_slots.iter().try_fold(0u16, |sum, slots| {
            sum.checked_add(*slots)
                .ok_or_else(|| invariant("heap_ret_slots sum overflows u16".to_string()))
        })?;
        if heap_ret_slot_sum != func.ret_slots {
            return Err(invariant(format!(
                "heap_ret_slots sum {heap_ret_slot_sum} but ret_slots={}",
                func.ret_slots
            )));
        }
        let mut ret_start = 0usize;
        for (idx, &width) in func.heap_ret_slots.iter().enumerate() {
            let width = width as usize;
            let ret_end = ret_start.checked_add(width).ok_or_else(|| {
                invariant("heap_ret_slots partition range overflows usize".to_string())
            })?;
            let Some(partition) = func.ret_slot_types.get(ret_start..ret_end) else {
                return Err(invariant(format!(
                    "heap return partition {idx} range {ret_start}..{ret_end} exceeds ret_slot_types len {}",
                    func.ret_slot_types.len()
                )));
            };
            validate_interface_pairs(
                &format!("function {} heap return partition {idx}", func.name),
                partition,
            )?;
            ret_start = ret_end;
        }
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
    if func.error_ret_slot >= 0 {
        let error_ret_slot = func.error_ret_slot as u16;
        if error_ret_slot.checked_add(2) != Some(func.ret_slots) {
            return Err(invariant(format!(
                "error_ret_slot={} must be the final two return slots of ret_slots={}",
                func.error_ret_slot, func.ret_slots
            )));
        }
        let error_ret_slot = error_ret_slot as usize;
        if func.ret_slot_types[error_ret_slot] != SlotType::Interface0
            || func.ret_slot_types[error_ret_slot + 1] != SlotType::Interface1
        {
            return Err(invariant(format!(
                "error_ret_slot={} must have Interface0/Interface1 layout, got {:?}/{:?}",
                func.error_ret_slot,
                func.ret_slot_types[error_ret_slot],
                func.ret_slot_types[error_ret_slot + 1]
            )));
        }
        if func.heap_ret_gcref_count > 0 {
            let last_width = func.heap_ret_slots.last().copied().unwrap_or(0);
            let last_start = func
                .heap_ret_slots
                .iter()
                .take(func.heap_ret_slots.len().saturating_sub(1))
                .try_fold(0u16, |sum, slots| {
                    sum.checked_add(*slots).ok_or_else(|| {
                        invariant("heap_ret_slots prefix sum overflows u16".to_string())
                    })
                })?;
            if last_start != func.error_ret_slot as u16 || last_width != 2 {
                return Err(invariant(format!(
                    "heap error return partition must start at error_ret_slot={} with width 2, got start={} width={}",
                    func.error_ret_slot, last_start, last_width
                )));
            }
        }
    }
    if func.is_closure
        && (func.param_slots == 0 || func.slot_types.first() != Some(&SlotType::GcRef))
    {
        return Err(invariant(
            "closure functions must reserve GcRef slot 0".to_string(),
        ));
    }
    if func.is_closure && func.recv_slots > 0 {
        return Err(invariant(format!(
            "closure functions cannot declare receiver slots (recv_slots={})",
            func.recv_slots
        )));
    }
    if func.recv_slots > 0
        && (!func.capture_types.is_empty() || !func.capture_slot_types.is_empty())
    {
        return Err(invariant(
            "receiver functions cannot declare ordinary closure capture metadata".to_string(),
        ));
    }
    for (idx, transfer) in func.capture_types.iter().enumerate() {
        let _ = validate_transfer_type_layout(module, func, idx, "capture_types", transfer)?;
    }
    let mut param_transfer_layout = Vec::new();
    for (idx, transfer) in func.param_types.iter().enumerate() {
        param_transfer_layout.extend(validate_transfer_type_layout(
            module,
            func,
            idx,
            "param_types",
            transfer,
        )?);
    }
    validate_transfer_shape_invariants(func, &param_transfer_layout)?;

    Ok(())
}

fn validate_transfer_shape_invariants(
    func: &FunctionDef,
    param_transfer_layout: &[SlotType],
) -> Result<(), ModuleVerificationError> {
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
        let actual_transfer_slots = param_transfer_layout.len();
        if actual_transfer_slots != expected_without_receiver as usize
            && (func.recv_slots == 0 || actual_transfer_slots != expected_with_receiver as usize)
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
        let param_end = func.param_slots as usize;
        let without_receiver_start = implicit_param_slots as usize;
        let Some(expected_without_receiver_layout) =
            func.slot_types.get(without_receiver_start..param_end)
        else {
            return Err(invariant(format!(
                "param_types frame range {}..{} is outside slot_types.len()={}",
                without_receiver_start,
                param_end,
                func.slot_types.len()
            )));
        };
        let expected_with_receiver_layout = if func.recv_slots == 0 {
            None
        } else {
            let with_receiver_start = usize::from(func.is_closure);
            Some(
                func.slot_types
                    .get(with_receiver_start..param_end)
                    .ok_or_else(|| {
                        invariant(format!(
                            "param_types receiver frame range {}..{} is outside slot_types.len()={}",
                            with_receiver_start,
                            param_end,
                            func.slot_types.len()
                        ))
                    })?,
            )
        };
        if param_transfer_layout != expected_without_receiver_layout
            && expected_with_receiver_layout != Some(param_transfer_layout)
        {
            return Err(invariant(format!(
                "param_types slot layout {:?} does not match frame parameter layout {:?}{}",
                param_transfer_layout,
                expected_without_receiver_layout,
                expected_with_receiver_layout
                    .map(|expected| format!(" or receiver-inclusive layout {expected:?}"))
                    .unwrap_or_default()
            )));
        }
    }

    Ok(())
}

fn validate_transfer_type_layout(
    module: &Module,
    func: &FunctionDef,
    idx: usize,
    access: &'static str,
    transfer: &TransferType,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
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
    let value_meta = ValueMeta::from_raw(transfer.meta_raw);
    let value_rttid = ValueRttid::from_raw(transfer.rttid_raw);
    let Some(canonical_meta) = module.canonical_value_meta_for_value_rttid(value_rttid) else {
        return Err(invariant(format!(
            "{access}[{idx}] ValueRttid {} cannot be resolved to canonical metadata",
            value_rttid.rttid()
        )));
    };
    if value_meta != canonical_meta {
        return Err(invariant(format!(
            "{access}[{idx}] ValueMeta raw 0x{:x} does not match canonical raw 0x{:x} from ValueRttid {}",
            value_meta.to_raw(),
            canonical_meta.to_raw(),
            value_rttid.rttid()
        )));
    }
    let Some(expected_layout) = module.slot_layout_for_value_rttid(value_rttid) else {
        return Err(invariant(format!(
            "{access}[{idx}] ValueRttid {} cannot be resolved to slot layout",
            value_rttid.rttid()
        )));
    };
    let expected_slots = expected_layout.len();
    if expected_slots > u16::MAX as usize {
        return Err(invariant(format!(
            "{access}[{idx}] rttid slot width {expected_slots} exceeds u16"
        )));
    }
    if transfer.slots as usize != expected_slots {
        return Err(invariant(format!(
            "{access}[{idx}] slots {} do not match rttid slot width {expected_slots}",
            transfer.slots
        )));
    }
    Ok(expected_layout)
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
    if slots > u16::MAX as usize {
        return Err(ModuleVerificationError::GcLayout {
            detail: format!("{label} slot count {slots} exceeds u16::MAX"),
        });
    }
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
    analyses: VerifierAnalyses<'_>,
    pc: usize,
    inst: Instruction,
    opcode: Opcode,
) -> Result<(), ModuleVerificationError> {
    let VerifierAnalyses {
        module,
        constant_facts,
        index_check_facts,
        container_layout_facts,
    } = analyses;
    let ctx = InstructionVerifierContext {
        func,
        pc,
        opcode,
        inst,
    };

    match opcode {
        Opcode::Hint => verify_hint(func, pc, inst),
        Opcode::LoadInt => verify_load_int_contract(func, pc, inst),
        Opcode::LoadConst => verify_load_const_contract(func, module, pc, inst),
        Opcode::Copy => verify_copy_contract(func, pc, opcode, inst),
        Opcode::CopyN => verify_copy_n_contract(func, pc, opcode, inst),
        Opcode::SlotGet => {
            verify_slot_get_contract(ctx, index_check_facts, inst.a, inst.b, inst.c, 1)
        }
        Opcode::SlotGetN => verify_slot_get_contract(
            ctx,
            index_check_facts,
            inst.a,
            inst.b,
            inst.c,
            inst.flags as u16,
        ),
        Opcode::SlotSet => {
            verify_slot_set_contract(ctx, index_check_facts, inst.a, inst.b, inst.c, 1)
        }
        Opcode::SlotSetN => verify_slot_set_contract(
            ctx,
            index_check_facts,
            inst.a,
            inst.b,
            inst.c,
            inst.flags as u16,
        ),
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
        Opcode::PtrNew => verify_ptr_new_contract(func, module, constant_facts, pc, opcode, inst),
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
            ctx,
            BinarySlotContract::exact(
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                scalar_destination_access(opcode),
                "scalar lhs",
                "scalar rhs",
            ),
        ),
        Opcode::EqI | Opcode::NeI | Opcode::And | Opcode::Or | Opcode::Xor | Opcode::AndNot => {
            verify_binary_one_of_slot_contract(
                ctx,
                BinarySlotContract::one_of(
                    &[SlotType::Value],
                    RAW_I64_SLOTS,
                    RAW_I64_SLOTS,
                    scalar_destination_access(opcode),
                    "raw lhs",
                    "raw rhs",
                ),
            )
        }
        Opcode::NegI | Opcode::BoolNot => verify_unary_slot_contract(
            ctx,
            UnarySlotContract::exact(
                SlotType::Value,
                SlotType::Value,
                scalar_destination_access(opcode),
                "scalar source",
            ),
        ),
        Opcode::Not => verify_unary_one_of_slot_contract(
            ctx,
            UnarySlotContract::one_of(
                &[SlotType::Value],
                RAW_I64_SLOTS,
                scalar_destination_access(opcode),
                "raw source",
            ),
        ),
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF => {
            verify_binary_one_of_slot_contract(
                ctx,
                BinarySlotContract::one_of(
                    FLOAT_STORAGE_SLOTS,
                    FLOAT_STORAGE_SLOTS,
                    FLOAT_STORAGE_SLOTS,
                    scalar_destination_access(opcode),
                    "float lhs",
                    "float rhs",
                ),
            )
        }
        Opcode::NegF => verify_unary_one_of_slot_contract(
            ctx,
            UnarySlotContract::one_of(
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                scalar_destination_access(opcode),
                "float source",
            ),
        ),
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF => {
            verify_binary_one_of_slot_contract(
                ctx,
                BinarySlotContract::one_of(
                    &[SlotType::Value],
                    FLOAT_STORAGE_SLOTS,
                    FLOAT_STORAGE_SLOTS,
                    scalar_destination_access(opcode),
                    "float lhs",
                    "float rhs",
                ),
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
        Opcode::CallClosure => verify_dynamic_call_contract(func, module, pc, opcode, inst, true),
        Opcode::CallIface => verify_dynamic_call_contract(func, module, pc, opcode, inst, false),
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
        Opcode::ArrayNew => {
            verify_array_new_contract(func, module, constant_facts, pc, opcode, inst)
        }
        Opcode::ArrayGet => verify_indexed_get_contract(
            ctx,
            constant_facts,
            IndexedAccessLabels {
                base: "Array source",
                index: "Array index",
                value: "ArrayGet destination",
            },
        ),
        Opcode::ArraySet => verify_indexed_set_contract(
            ctx,
            constant_facts,
            IndexedAccessLabels {
                base: "ArraySet target",
                index: "ArraySet index",
                value: "ArraySet source",
            },
        ),
        Opcode::ArrayAddr => verify_indexed_addr_contract(
            ctx,
            constant_facts,
            IndexedAccessLabels {
                base: "Array source",
                index: "Array index",
                value: "ArrayAddr destination",
            },
        ),
        Opcode::SliceNew => {
            verify_slice_new_contract(func, module, constant_facts, pc, opcode, inst)
        }
        Opcode::SliceGet => verify_indexed_get_contract(
            ctx,
            constant_facts,
            IndexedAccessLabels {
                base: "Slice source",
                index: "Slice index",
                value: "SliceGet destination",
            },
        ),
        Opcode::SliceSet => verify_indexed_set_contract(
            ctx,
            constant_facts,
            IndexedAccessLabels {
                base: "SliceSet target",
                index: "SliceSet index",
                value: "SliceSet source",
            },
        ),
        Opcode::SliceAddr => verify_indexed_addr_contract(
            ctx,
            constant_facts,
            IndexedAccessLabels {
                base: "Slice source",
                index: "Slice index",
                value: "SliceAddr destination",
            },
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
        Opcode::SliceAppend => {
            verify_slice_append_contract(func, module, constant_facts, pc, opcode, inst)
        }
        Opcode::MapNew => verify_map_new_contract(func, module, constant_facts, pc, opcode, inst),
        Opcode::MapGet => verify_map_get_contract(
            func,
            container_layout_facts,
            constant_facts,
            pc,
            opcode,
            inst,
        ),
        Opcode::MapSet => verify_map_set_contract(
            func,
            container_layout_facts,
            constant_facts,
            pc,
            opcode,
            inst,
        ),
        Opcode::MapDelete => verify_map_delete_contract(
            func,
            container_layout_facts,
            constant_facts,
            pc,
            opcode,
            inst,
        ),
        Opcode::MapLen => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "MapLen destination",
            )?;
            verify_known_map_object(func, container_layout_facts, pc, opcode, inst.b, "MapLen")?;
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], "MapLen map")
        }
        Opcode::MapIterInit => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &MAP_ITER_SLOT_TYPES,
                "MapIterInit iterator",
            )?;
            verify_known_map_object(
                func,
                container_layout_facts,
                pc,
                opcode,
                inst.b,
                "MapIterInit",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcRef],
                "MapIterInit map",
            )
        }
        Opcode::MapIterNext => {
            verify_map_iter_next_contract(func, container_layout_facts, pc, opcode, inst)
        }
        Opcode::QueueNew => {
            verify_queue_new_contract(func, module, constant_facts, pc, opcode, inst)
        }
        Opcode::QueueSend => {
            verify_queue_send_contract(func, container_layout_facts, pc, opcode, inst)
        }
        Opcode::QueueRecv => {
            verify_queue_recv_contract(func, container_layout_facts, pc, opcode, inst)
        }
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
            let access = match opcode {
                Opcode::QueueLen => "QueueLen",
                Opcode::QueueCap => "QueueCap",
                Opcode::QueueClose => "QueueClose",
                _ => "Queue",
            };
            verify_known_queue_object(
                func,
                container_layout_facts,
                pc,
                opcode,
                queue_slot,
                access,
            )?;
            verify_layout(func, pc, opcode, queue_slot, &[SlotType::GcRef], "queue")
        }
        Opcode::SelectBegin => Ok(()),
        Opcode::SelectSend => {
            verify_select_send_contract(func, container_layout_facts, pc, opcode, inst)
        }
        Opcode::SelectRecv => {
            verify_select_recv_contract(func, container_layout_facts, pc, opcode, inst)
        }
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
            ctx,
            UnarySlotContract::one_of(
                FLOAT_STORAGE_SLOTS,
                &[SlotType::Value],
                "ConvI2F destination",
                "ConvI2F source",
            ),
        ),
        Opcode::ConvF2I => verify_unary_one_of_slot_contract(
            ctx,
            UnarySlotContract::one_of(
                &[SlotType::Value],
                FLOAT_STORAGE_SLOTS,
                "ConvF2I destination",
                "ConvF2I source",
            ),
        ),
        Opcode::ConvF64F32 => verify_unary_one_of_slot_contract(
            ctx,
            UnarySlotContract::one_of(
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                "ConvF64F32 destination",
                "ConvF64F32 source",
            ),
        ),
        Opcode::ConvF32F64 => verify_unary_one_of_slot_contract(
            ctx,
            UnarySlotContract::one_of(
                FLOAT_STORAGE_SLOTS,
                &[SlotType::Value, SlotType::Float],
                "ConvF32F64 destination",
                "ConvF32F64 source",
            ),
        ),
        Opcode::Trunc => verify_trunc_contract(ctx),
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
    match inst.flags {
        HINT_NOP => Ok(()),
        HINT_LOOP => {
            let loop_flags = (inst.a & 0x0F) as u8;
            const ALLOWED_LOOP_FLAGS: u8 =
                LOOP_FLAG_HAS_DEFER | LOOP_FLAG_HAS_LABELED_BREAK | LOOP_FLAG_HAS_LABELED_CONTINUE;
            if loop_flags & !ALLOWED_LOOP_FLAGS != 0 {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    Opcode::Hint,
                    format!("unsupported HINT_LOOP flags 0x{loop_flags:02x}"),
                ));
            }
            let encoded_end_offset = ((inst.a >> 8) & 0xFF) as usize;
            if encoded_end_offset > 0 {
                verify_jump_target_contract(
                    func,
                    pc,
                    Opcode::Hint,
                    (pc + encoded_end_offset) as i64,
                )?;
            }
            let exit_pc = inst.imm32_unsigned() as usize;
            if exit_pc != 0 && exit_pc >= func.code.len() {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    Opcode::Hint,
                    format!(
                        "Hint exit_pc {exit_pc} outside function length {}",
                        func.code.len()
                    ),
                ));
            }
            Ok(())
        }
        flags => Err(call_shape_mismatch(
            func,
            pc,
            Opcode::Hint,
            format!("unsupported Hint flags 0x{flags:02x}"),
        )),
    }
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ConstantFact {
    Unknown,
    Int(i128),
    Conflict,
}

struct ConstantFactAnalysis {
    slots: Vec<u16>,
    before: Vec<Option<Vec<ConstantFact>>>,
}

impl ConstantFactAnalysis {
    fn analyze(func: &FunctionDef, module: &Module) -> Self {
        let slots = tracked_constant_slots(func, module);
        if slots.is_empty() || func.code.is_empty() {
            return Self {
                slots,
                before: vec![None; func.code.len()],
            };
        }

        let mut before = vec![None; func.code.len()];
        before[0] = Some(vec![ConstantFact::Unknown; slots.len()]);
        let mut worklist = vec![0usize];

        while let Some(pc) = worklist.pop() {
            let Some(mut out) = before[pc].clone() else {
                continue;
            };
            apply_constant_fact_transfer(func, module, pc, &slots, &mut out);
            for succ in instruction_successors(func, pc) {
                if succ >= func.code.len() {
                    continue;
                }
                if merge_constant_state(&mut before[succ], &out) {
                    worklist.push(succ);
                }
            }
        }

        Self { slots, before }
    }

    fn fact_for_slot(&self, pc: usize, slot: u16) -> Option<ConstantFact> {
        let idx = self.slots.iter().position(|candidate| *candidate == slot)?;
        self.before
            .get(pc)
            .and_then(|state| state.as_ref())
            .map(|state| state[idx])
    }

    fn is_reachable(&self, pc: usize) -> bool {
        self.before.get(pc).is_some_and(Option::is_some)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IndexCheckFact {
    Unknown,
    Checked { len: u16 },
    Conflict,
}

struct IndexCheckAnalysis {
    slots: Vec<u16>,
    before: Vec<Option<Vec<IndexCheckFact>>>,
}

impl IndexCheckAnalysis {
    fn analyze(func: &FunctionDef, module: &Module, constant_facts: &ConstantFactAnalysis) -> Self {
        let slots = tracked_index_check_slots(func, module);
        if slots.is_empty() || func.code.is_empty() {
            return Self {
                slots,
                before: vec![None; func.code.len()],
            };
        }

        let mut before = vec![None; func.code.len()];
        before[0] = Some(vec![IndexCheckFact::Unknown; slots.len()]);
        let mut worklist = vec![0usize];

        while let Some(pc) = worklist.pop() {
            let Some(mut out) = before[pc].clone() else {
                continue;
            };
            apply_index_check_transfer(func, module, constant_facts, pc, &slots, &mut out);
            for succ in instruction_successors(func, pc) {
                if succ >= func.code.len() {
                    continue;
                }
                if merge_index_check_state(&mut before[succ], &out) {
                    worklist.push(succ);
                }
            }
        }

        Self { slots, before }
    }

    fn fact_for_slot(&self, pc: usize, slot: u16) -> Option<IndexCheckFact> {
        let idx = self.slots.iter().position(|candidate| *candidate == slot)?;
        self.before
            .get(pc)
            .and_then(|state| state.as_ref())
            .map(|state| state[idx])
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ContainerLayoutFact {
    Unknown,
    Conflict,
    Map {
        key_layout: Vec<SlotType>,
        val_layout: Vec<SlotType>,
    },
    MapIter {
        key_layout: Vec<SlotType>,
        val_layout: Vec<SlotType>,
    },
    Queue {
        elem_layout: Vec<SlotType>,
    },
}

struct ContainerLayoutAnalysis {
    slots: Vec<u16>,
    before: Vec<Option<Vec<ContainerLayoutFact>>>,
}

impl ContainerLayoutAnalysis {
    fn analyze(func: &FunctionDef, module: &Module) -> Self {
        let slots = tracked_container_slots(func, module);
        if slots.is_empty() || func.code.is_empty() {
            return Self {
                slots,
                before: vec![None; func.code.len()],
            };
        }

        let mut before = vec![None; func.code.len()];
        before[0] = Some(initial_container_state(func, module, &slots));
        let mut worklist = vec![0usize];

        while let Some(pc) = worklist.pop() {
            let Some(mut out) = before[pc].clone() else {
                continue;
            };
            apply_container_layout_transfer(func, module, pc, &slots, &mut out);
            for succ in instruction_successors(func, pc) {
                if succ >= func.code.len() {
                    continue;
                }
                if merge_container_state(&mut before[succ], &out) {
                    worklist.push(succ);
                }
            }
        }

        Self { slots, before }
    }

    fn fact_for_slot(&self, pc: usize, slot: u16) -> Option<&ContainerLayoutFact> {
        let idx = self.slots.iter().position(|candidate| *candidate == slot)?;
        self.before
            .get(pc)
            .and_then(|state| state.as_ref())
            .map(|state| &state[idx])
    }
}

fn tracked_container_slots(func: &FunctionDef, module: &Module) -> Vec<u16> {
    let mut slots = Vec::new();
    for inst in func.code.iter().copied() {
        match inst.opcode() {
            Opcode::MapNew => push_unique_slot(&mut slots, inst.a),
            Opcode::MapGet => push_unique_slot(&mut slots, inst.b),
            Opcode::MapSet | Opcode::MapDelete => push_unique_slot(&mut slots, inst.a),
            Opcode::MapIterInit => {
                push_unique_slot(&mut slots, inst.a);
                push_unique_slot(&mut slots, inst.b);
            }
            Opcode::MapIterNext => push_unique_slot(&mut slots, inst.b),
            Opcode::MapLen => push_unique_slot(&mut slots, inst.b),
            Opcode::QueueNew => push_unique_slot(&mut slots, inst.a),
            Opcode::QueueSend | Opcode::QueueClose | Opcode::SelectSend => {
                push_unique_slot(&mut slots, inst.a);
            }
            Opcode::QueueRecv | Opcode::SelectRecv => push_unique_slot(&mut slots, inst.b),
            Opcode::QueueLen | Opcode::QueueCap => push_unique_slot(&mut slots, inst.b),
            _ => {}
        }
    }
    loop {
        let snapshot = slots.clone();
        let mut changed = false;
        for (pc, inst) in func.code.iter().copied().enumerate() {
            for dependency in
                container_fact_dependencies_for_tracked_write(func, module, pc, inst, &snapshot)
            {
                let len = slots.len();
                push_unique_slot(&mut slots, dependency);
                changed |= slots.len() != len;
            }
        }
        if !changed {
            break;
        }
    }
    slots
}

fn container_fact_dependencies_for_tracked_write(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    inst: Instruction,
    tracked_slots: &[u16],
) -> Vec<u16> {
    let mut dependencies = Vec::new();
    for slot in tracked_slots.iter().copied() {
        if !instruction_writes_slot(Some(module), func, pc, inst, slot) {
            continue;
        }
        match inst.opcode() {
            Opcode::Copy if inst.a == slot => push_unique_slot(&mut dependencies, inst.b),
            Opcode::CopyN if slot_in_range(slot, inst.a, inst.c as usize) => {
                let offset = slot.wrapping_sub(inst.a);
                if let Some(source) = inst.b.checked_add(offset) {
                    push_unique_slot(&mut dependencies, source);
                }
            }
            Opcode::MapIterInit if inst.a == slot => push_unique_slot(&mut dependencies, inst.b),
            _ => {}
        }
    }
    dependencies
}

fn initial_container_state(
    func: &FunctionDef,
    module: &Module,
    slots: &[u16],
) -> Vec<ContainerLayoutFact> {
    let mut state = vec![ContainerLayoutFact::Unknown; slots.len()];
    seed_param_container_layout_facts(func, module, slots, &mut state);
    state
}

fn seed_param_container_layout_facts(
    func: &FunctionDef,
    module: &Module,
    slots: &[u16],
    state: &mut [ContainerLayoutFact],
) {
    if func.param_types.is_empty() {
        return;
    }
    let transfer_slots = func
        .param_types
        .iter()
        .try_fold(0u16, |acc, transfer| acc.checked_add(transfer.slots));
    let Some(transfer_slots) = transfer_slots else {
        return;
    };
    let implicit_param_slots = match func.recv_slots.checked_add(u16::from(func.is_closure)) {
        Some(slots) => slots,
        None => return,
    };
    let start = if func
        .param_slots
        .checked_sub(implicit_param_slots)
        .is_some_and(|expected| expected == transfer_slots)
    {
        implicit_param_slots
    } else if func.recv_slots > 0
        && func
            .param_slots
            .checked_sub(u16::from(func.is_closure))
            .is_some_and(|expected| expected == transfer_slots)
    {
        u16::from(func.is_closure)
    } else {
        return;
    };

    let mut cursor = start;
    for transfer in &func.param_types {
        if let Some(fact) = container_fact_for_transfer_type(module, transfer) {
            if let Some(idx) = slots.iter().position(|slot| *slot == cursor) {
                state[idx] = fact;
            }
        }
        let Some(next) = cursor.checked_add(transfer.slots) else {
            return;
        };
        cursor = next;
    }
}

fn container_fact_for_transfer_type(
    module: &Module,
    transfer: &TransferType,
) -> Option<ContainerLayoutFact> {
    let value_rttid = ValueRttid::from_raw(transfer.rttid_raw);
    match module.runtime_types.get(value_rttid.rttid() as usize)? {
        RuntimeType::Map { key, val } => Some(ContainerLayoutFact::Map {
            key_layout: module.slot_layout_for_value_rttid(*key)?,
            val_layout: module.slot_layout_for_value_rttid(*val)?,
        }),
        RuntimeType::Chan { elem, .. } | RuntimeType::Port { elem, .. } => {
            Some(ContainerLayoutFact::Queue {
                elem_layout: module.slot_layout_for_value_rttid(*elem)?,
            })
        }
        _ => None,
    }
}

fn merge_container_state(
    dst: &mut Option<Vec<ContainerLayoutFact>>,
    incoming: &[ContainerLayoutFact],
) -> bool {
    let Some(current) = dst else {
        *dst = Some(incoming.to_vec());
        return true;
    };

    let mut changed = false;
    for (current, incoming) in current.iter_mut().zip(incoming.iter()) {
        let merged = merge_container_fact(current, incoming);
        if merged != *current {
            *current = merged;
            changed = true;
        }
    }
    changed
}

fn merge_container_fact(
    current: &ContainerLayoutFact,
    incoming: &ContainerLayoutFact,
) -> ContainerLayoutFact {
    match (current, incoming) {
        (ContainerLayoutFact::Conflict, _) | (_, ContainerLayoutFact::Conflict) => {
            ContainerLayoutFact::Conflict
        }
        (ContainerLayoutFact::Unknown, _) | (_, ContainerLayoutFact::Unknown) => {
            ContainerLayoutFact::Unknown
        }
        (lhs, rhs) if lhs == rhs => lhs.clone(),
        _ => ContainerLayoutFact::Conflict,
    }
}

fn apply_container_layout_transfer(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    slots: &[u16],
    state: &mut [ContainerLayoutFact],
) {
    let inst = func.code[pc];
    let input = state.to_vec();
    for (idx, slot) in slots.iter().copied().enumerate() {
        if !instruction_writes_slot(Some(module), func, pc, inst, slot) {
            continue;
        }
        state[idx] = container_fact_written_to_slot(func, pc, inst, slot, slots, &input);
    }
}

fn container_fact_written_to_slot(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
    slot: u16,
    tracked_slots: &[u16],
    input: &[ContainerLayoutFact],
) -> ContainerLayoutFact {
    match inst.opcode() {
        Opcode::MapNew if inst.a == slot => map_new_layout(func, pc, Opcode::MapNew)
            .map(|(key_layout, val_layout)| ContainerLayoutFact::Map {
                key_layout,
                val_layout,
            })
            .unwrap_or(ContainerLayoutFact::Unknown),
        Opcode::QueueNew if inst.a == slot => queue_elem_layout(func, pc, Opcode::QueueNew)
            .map(|elem_layout| ContainerLayoutFact::Queue { elem_layout })
            .unwrap_or(ContainerLayoutFact::Unknown),
        Opcode::MapIterInit if inst.a == slot => {
            match fact_for_tracked_container_source(inst.b, tracked_slots, input) {
                ContainerLayoutFact::Map {
                    key_layout,
                    val_layout,
                }
                | ContainerLayoutFact::MapIter {
                    key_layout,
                    val_layout,
                } => ContainerLayoutFact::MapIter {
                    key_layout,
                    val_layout,
                },
                ContainerLayoutFact::Conflict => ContainerLayoutFact::Conflict,
                ContainerLayoutFact::Unknown | ContainerLayoutFact::Queue { .. } => {
                    ContainerLayoutFact::Unknown
                }
            }
        }
        Opcode::MapIterNext if slot_in_range(slot, inst.b, MAP_ITER_SLOTS) => {
            match fact_for_tracked_container_source(inst.b, tracked_slots, input) {
                ContainerLayoutFact::MapIter {
                    key_layout,
                    val_layout,
                } => ContainerLayoutFact::MapIter {
                    key_layout,
                    val_layout,
                },
                ContainerLayoutFact::Conflict => ContainerLayoutFact::Conflict,
                ContainerLayoutFact::Unknown
                | ContainerLayoutFact::Map { .. }
                | ContainerLayoutFact::Queue { .. } => ContainerLayoutFact::Unknown,
            }
        }
        Opcode::Copy if inst.a == slot => {
            fact_for_tracked_container_source(inst.b, tracked_slots, input)
        }
        Opcode::CopyN if slot_in_range(slot, inst.a, inst.c as usize) => {
            let offset = slot.wrapping_sub(inst.a);
            let Some(source) = inst.b.checked_add(offset) else {
                return ContainerLayoutFact::Unknown;
            };
            fact_for_tracked_container_source(source, tracked_slots, input)
        }
        _ => ContainerLayoutFact::Unknown,
    }
}

fn fact_for_tracked_container_source(
    source: u16,
    tracked_slots: &[u16],
    input: &[ContainerLayoutFact],
) -> ContainerLayoutFact {
    tracked_slots
        .iter()
        .position(|slot| *slot == source)
        .map(|idx| input[idx].clone())
        .unwrap_or(ContainerLayoutFact::Unknown)
}

fn tracked_constant_slots(func: &FunctionDef, module: &Module) -> Vec<u16> {
    let mut slots = Vec::new();
    for (pc, inst) in func.code.iter().copied().enumerate() {
        if let Some(slot) = dynamic_elem_bytes_fact_slot(func, pc, inst) {
            push_unique_slot(&mut slots, slot);
        }
        match inst.opcode() {
            Opcode::MapNew => {
                push_unique_slot(&mut slots, inst.b);
                if let Some(key_rttid_slot) = inst.b.checked_add(1) {
                    push_unique_slot(&mut slots, key_rttid_slot);
                }
            }
            Opcode::ArrayNew | Opcode::SliceNew => push_unique_slot(&mut slots, inst.b),
            Opcode::SliceAppend => push_unique_slot(&mut slots, inst.c),
            Opcode::PtrNew => push_unique_slot(&mut slots, inst.b),
            Opcode::QueueNew => push_unique_slot(&mut slots, inst.b),
            Opcode::MapGet => push_unique_slot(&mut slots, inst.c),
            Opcode::MapSet | Opcode::MapDelete => push_unique_slot(&mut slots, inst.b),
            Opcode::IndexCheck => push_unique_slot(&mut slots, inst.b),
            _ => {}
        }
    }
    loop {
        let snapshot = slots.clone();
        let mut changed = false;
        for (pc, inst) in func.code.iter().copied().enumerate() {
            for dependency in
                constant_fact_dependencies_for_tracked_write(func, module, pc, inst, &snapshot)
            {
                let len = slots.len();
                push_unique_slot(&mut slots, dependency);
                changed |= slots.len() != len;
            }
        }
        if !changed {
            break;
        }
    }
    slots
}

fn constant_fact_dependencies_for_tracked_write(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    inst: Instruction,
    tracked_slots: &[u16],
) -> Vec<u16> {
    let mut dependencies = Vec::new();
    for slot in tracked_slots.iter().copied() {
        if !instruction_writes_slot(Some(module), func, pc, inst, slot) {
            continue;
        }
        match inst.opcode() {
            Opcode::Copy if inst.a == slot => push_unique_slot(&mut dependencies, inst.b),
            Opcode::CopyN if slot_in_range(slot, inst.a, inst.c as usize) => {
                let offset = slot.wrapping_sub(inst.a);
                if let Some(source) = inst.b.checked_add(offset) {
                    push_unique_slot(&mut dependencies, source);
                }
            }
            Opcode::Shl | Opcode::Or if inst.a == slot => {
                push_unique_slot(&mut dependencies, inst.b);
                push_unique_slot(&mut dependencies, inst.c);
            }
            _ => {}
        }
    }
    dependencies
}

fn push_unique_slot(slots: &mut Vec<u16>, slot: u16) {
    if !slots.contains(&slot) {
        slots.push(slot);
    }
}

fn tracked_index_check_slots(func: &FunctionDef, module: &Module) -> Vec<u16> {
    let mut slots = Vec::new();
    for inst in func.code.iter().copied() {
        match inst.opcode() {
            Opcode::SlotGet | Opcode::SlotGetN => push_unique_slot(&mut slots, inst.c),
            Opcode::SlotSet | Opcode::SlotSetN => push_unique_slot(&mut slots, inst.b),
            _ => {}
        }
    }
    loop {
        let snapshot = slots.clone();
        let mut changed = false;
        for (pc, inst) in func.code.iter().copied().enumerate() {
            for dependency in
                index_check_dependencies_for_tracked_write(func, module, pc, inst, &snapshot)
            {
                let len = slots.len();
                push_unique_slot(&mut slots, dependency);
                changed |= slots.len() != len;
            }
        }
        if !changed {
            break;
        }
    }
    slots
}

fn index_check_dependencies_for_tracked_write(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    inst: Instruction,
    tracked_slots: &[u16],
) -> Vec<u16> {
    let mut dependencies = Vec::new();
    for slot in tracked_slots.iter().copied() {
        if !instruction_writes_slot(Some(module), func, pc, inst, slot) {
            continue;
        }
        match inst.opcode() {
            Opcode::Copy if inst.a == slot => push_unique_slot(&mut dependencies, inst.b),
            Opcode::CopyN if slot_in_range(slot, inst.a, inst.c as usize) => {
                let offset = slot.wrapping_sub(inst.a);
                if let Some(source) = inst.b.checked_add(offset) {
                    push_unique_slot(&mut dependencies, source);
                }
            }
            _ => {}
        }
    }
    dependencies
}

fn dynamic_elem_bytes_fact_slot(func: &FunctionDef, pc: usize, inst: Instruction) -> Option<u16> {
    if inst.flags != 0 {
        return None;
    }
    match inst.opcode() {
        Opcode::ArrayNew => checked_slot_offset(func, pc, inst.c, 1, "ArrayNew elem bytes").ok(),
        Opcode::SliceNew => checked_slot_offset(func, pc, inst.c, 2, "SliceNew elem bytes").ok(),
        Opcode::ArrayGet | Opcode::SliceGet | Opcode::ArrayAddr | Opcode::SliceAddr => {
            checked_slot_offset(func, pc, inst.c, 1, "indexed elem bytes").ok()
        }
        Opcode::ArraySet | Opcode::SliceSet => {
            checked_slot_offset(func, pc, inst.b, 1, "indexed elem bytes").ok()
        }
        Opcode::SliceAppend => {
            checked_slot_offset(func, pc, inst.c, 1, "SliceAppend elem bytes").ok()
        }
        _ => None,
    }
}

fn instruction_successors(func: &FunctionDef, pc: usize) -> Vec<usize> {
    let inst = func.code[pc];
    let mut successors = Vec::with_capacity(2);
    match inst.opcode() {
        Opcode::Jump => {
            push_valid_successor(func, &mut successors, jump_target_i64(pc, inst.imm32()));
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            push_fallthrough_successor(func, pc, &mut successors);
            push_valid_successor(func, &mut successors, jump_target_i64(pc, inst.imm32()));
        }
        Opcode::ForLoop => {
            push_fallthrough_successor(func, pc, &mut successors);
            push_valid_successor(func, &mut successors, forloop_target_i64(pc, inst.c as i16));
        }
        Opcode::Return | Opcode::Panic => {}
        _ => push_fallthrough_successor(func, pc, &mut successors),
    }
    successors
}

fn push_fallthrough_successor(func: &FunctionDef, pc: usize, successors: &mut Vec<usize>) {
    let next = pc + 1;
    if next < func.code.len() {
        successors.push(next);
    }
}

fn push_valid_successor(func: &FunctionDef, successors: &mut Vec<usize>, target: i64) {
    if target >= 0 {
        let target = target as usize;
        if target < func.code.len() && !successors.contains(&target) {
            successors.push(target);
        }
    }
}

fn merge_constant_state(dst: &mut Option<Vec<ConstantFact>>, incoming: &[ConstantFact]) -> bool {
    let Some(current) = dst else {
        *dst = Some(incoming.to_vec());
        return true;
    };

    let mut changed = false;
    for (current, incoming) in current.iter_mut().zip(incoming.iter().copied()) {
        let merged = merge_constant_fact(*current, incoming);
        if merged != *current {
            *current = merged;
            changed = true;
        }
    }
    changed
}

fn merge_constant_fact(a: ConstantFact, b: ConstantFact) -> ConstantFact {
    match (a, b) {
        (ConstantFact::Conflict, _) | (_, ConstantFact::Conflict) => ConstantFact::Conflict,
        (ConstantFact::Unknown, _) | (_, ConstantFact::Unknown) => ConstantFact::Unknown,
        (ConstantFact::Int(lhs), ConstantFact::Int(rhs)) if lhs == rhs => ConstantFact::Int(lhs),
        (ConstantFact::Int(_), ConstantFact::Int(_)) => ConstantFact::Conflict,
    }
}

fn apply_constant_fact_transfer(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    slots: &[u16],
    state: &mut [ConstantFact],
) {
    let inst = func.code[pc];
    let input = state.to_vec();
    for (idx, slot) in slots.iter().copied().enumerate() {
        if !instruction_writes_slot(Some(module), func, pc, inst, slot) {
            continue;
        }
        state[idx] = constant_fact_written_to_slot(module, inst, slot, slots, &input);
    }
}

fn constant_fact_written_to_slot(
    module: &Module,
    inst: Instruction,
    slot: u16,
    tracked_slots: &[u16],
    input: &[ConstantFact],
) -> ConstantFact {
    match inst.opcode() {
        Opcode::LoadInt if inst.a == slot => ConstantFact::Int(inst.imm32() as i128),
        Opcode::LoadConst if inst.a == slot => match module.constants.get(inst.b as usize) {
            Some(Constant::Int(value)) => ConstantFact::Int(*value as i128),
            _ => ConstantFact::Unknown,
        },
        Opcode::Copy if inst.a == slot => fact_for_tracked_source(inst.b, tracked_slots, input),
        Opcode::CopyN if slot_in_range(slot, inst.a, inst.c as usize) => {
            let offset = slot.wrapping_sub(inst.a);
            let Some(source) = inst.b.checked_add(offset) else {
                return ConstantFact::Unknown;
            };
            fact_for_tracked_source(source, tracked_slots, input)
        }
        Opcode::Shl if inst.a == slot => {
            fold_u64_constant_binary(inst.b, inst.c, tracked_slots, input, |lhs, rhs| {
                let shift = u32::try_from(rhs).ok()?;
                if shift >= 64 {
                    return None;
                }
                lhs.checked_shl(shift)
            })
        }
        Opcode::Or if inst.a == slot => {
            fold_u64_constant_binary(inst.b, inst.c, tracked_slots, input, |lhs, rhs| {
                Some(lhs | rhs)
            })
        }
        _ => ConstantFact::Unknown,
    }
}

fn fold_u64_constant_binary(
    lhs_slot: u16,
    rhs_slot: u16,
    tracked_slots: &[u16],
    input: &[ConstantFact],
    fold: impl FnOnce(u64, u64) -> Option<u64>,
) -> ConstantFact {
    let Some(lhs) = u64_fact_for_tracked_source(lhs_slot, tracked_slots, input) else {
        return ConstantFact::Unknown;
    };
    let Some(rhs) = u64_fact_for_tracked_source(rhs_slot, tracked_slots, input) else {
        return ConstantFact::Unknown;
    };
    fold(lhs, rhs)
        .map(|value| ConstantFact::Int(value as i128))
        .unwrap_or(ConstantFact::Unknown)
}

fn u64_fact_for_tracked_source(
    source: u16,
    tracked_slots: &[u16],
    input: &[ConstantFact],
) -> Option<u64> {
    match fact_for_tracked_source(source, tracked_slots, input) {
        ConstantFact::Int(value) => u64::try_from(value).ok(),
        ConstantFact::Unknown | ConstantFact::Conflict => None,
    }
}

fn fact_for_tracked_source(
    source: u16,
    tracked_slots: &[u16],
    input: &[ConstantFact],
) -> ConstantFact {
    tracked_slots
        .iter()
        .position(|slot| *slot == source)
        .map(|idx| input[idx])
        .unwrap_or(ConstantFact::Unknown)
}

fn merge_index_check_state(
    dst: &mut Option<Vec<IndexCheckFact>>,
    incoming: &[IndexCheckFact],
) -> bool {
    let Some(current) = dst else {
        *dst = Some(incoming.to_vec());
        return true;
    };

    let mut changed = false;
    for (current, incoming) in current.iter_mut().zip(incoming.iter().copied()) {
        let merged = merge_index_check_fact(*current, incoming);
        if merged != *current {
            *current = merged;
            changed = true;
        }
    }
    changed
}

fn merge_index_check_fact(a: IndexCheckFact, b: IndexCheckFact) -> IndexCheckFact {
    match (a, b) {
        (IndexCheckFact::Conflict, _) | (_, IndexCheckFact::Conflict) => IndexCheckFact::Conflict,
        (IndexCheckFact::Unknown, _) | (_, IndexCheckFact::Unknown) => IndexCheckFact::Unknown,
        (IndexCheckFact::Checked { len: lhs }, IndexCheckFact::Checked { len: rhs })
            if lhs == rhs =>
        {
            IndexCheckFact::Checked { len: lhs }
        }
        (IndexCheckFact::Checked { .. }, IndexCheckFact::Checked { .. }) => {
            IndexCheckFact::Conflict
        }
    }
}

fn apply_index_check_transfer(
    func: &FunctionDef,
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    slots: &[u16],
    state: &mut [IndexCheckFact],
) {
    let inst = func.code[pc];
    let input = state.to_vec();
    for (idx, slot) in slots.iter().copied().enumerate() {
        if !index_check_instruction_writes_slot(func, module, pc, inst, slot, slots, &input) {
            continue;
        }
        state[idx] = index_check_fact_written_to_slot(inst, slot, slots, &input);
    }

    if inst.opcode() != Opcode::IndexCheck {
        return;
    }

    let fact = match constant_facts.fact_for_slot(pc, inst.b) {
        Some(ConstantFact::Int(value)) => match u16::try_from(value) {
            Ok(len) => IndexCheckFact::Checked { len },
            Err(_) => IndexCheckFact::Conflict,
        },
        Some(ConstantFact::Conflict) => IndexCheckFact::Conflict,
        Some(ConstantFact::Unknown) | None => IndexCheckFact::Unknown,
    };
    for (idx, slot) in slots.iter().copied().enumerate() {
        if slot == inst.a {
            state[idx] = fact;
        }
    }
}

fn index_check_instruction_writes_slot(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    inst: Instruction,
    slot: u16,
    tracked_slots: &[u16],
    input: &[IndexCheckFact],
) -> bool {
    match inst.opcode() {
        Opcode::SlotSet | Opcode::SlotSetN => {
            index_check_slot_set_writes_slot(inst, slot, tracked_slots, input)
                .unwrap_or_else(|| instruction_writes_slot(Some(module), func, pc, inst, slot))
        }
        _ => instruction_writes_slot(Some(module), func, pc, inst, slot),
    }
}

fn index_check_slot_set_writes_slot(
    inst: Instruction,
    slot: u16,
    tracked_slots: &[u16],
    input: &[IndexCheckFact],
) -> Option<bool> {
    let elem_slots = match inst.opcode() {
        Opcode::SlotSet => 1usize,
        Opcode::SlotSetN => inst.flags as usize,
        _ => return None,
    };
    let IndexCheckFact::Checked { len } =
        index_check_fact_for_tracked_source(inst.b, tracked_slots, input)
    else {
        return None;
    };
    let span_slots = elem_slots.checked_mul(usize::from(len))?;
    Some(slot_in_range(slot, inst.a, span_slots))
}

fn index_check_fact_written_to_slot(
    inst: Instruction,
    slot: u16,
    tracked_slots: &[u16],
    input: &[IndexCheckFact],
) -> IndexCheckFact {
    match inst.opcode() {
        Opcode::Copy if inst.a == slot => {
            index_check_fact_for_tracked_source(inst.b, tracked_slots, input)
        }
        Opcode::CopyN if slot_in_range(slot, inst.a, inst.c as usize) => {
            let offset = slot.wrapping_sub(inst.a);
            let Some(source) = inst.b.checked_add(offset) else {
                return IndexCheckFact::Unknown;
            };
            index_check_fact_for_tracked_source(source, tracked_slots, input)
        }
        _ => IndexCheckFact::Unknown,
    }
}

fn index_check_fact_for_tracked_source(
    source: u16,
    tracked_slots: &[u16],
    input: &[IndexCheckFact],
) -> IndexCheckFact {
    tracked_slots
        .iter()
        .position(|slot| *slot == source)
        .map(|idx| input[idx])
        .unwrap_or(IndexCheckFact::Unknown)
}

fn instruction_writes_slot(
    module: Option<&Module>,
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
    slot: u16,
) -> bool {
    let opcode = inst.opcode();
    match opcode {
        Opcode::Hint
        | Opcode::GlobalSet
        | Opcode::GlobalSetN
        | Opcode::PtrSet
        | Opcode::PtrSetN
        | Opcode::Jump
        | Opcode::JumpIf
        | Opcode::JumpIfNot
        | Opcode::Return
        | Opcode::ArraySet
        | Opcode::SliceSet
        | Opcode::MapSet
        | Opcode::MapDelete
        | Opcode::QueueSend
        | Opcode::QueueClose
        | Opcode::SelectBegin
        | Opcode::SelectSend
        | Opcode::GoStart
        | Opcode::DeferPush
        | Opcode::ErrDeferPush
        | Opcode::Panic
        | Opcode::IndexCheck
        | Opcode::GoIsland
        | Opcode::Invalid => false,
        Opcode::SlotSet => slot >= inst.a,
        Opcode::SlotSetN => inst.flags != 0 && slot >= inst.a,
        Opcode::CopyN => slot_in_range(slot, inst.a, inst.c as usize),
        Opcode::SlotGetN | Opcode::GlobalGetN | Opcode::PtrGetN => {
            slot_in_range(slot, inst.a, inst.flags as usize)
        }
        Opcode::Call => {
            let Some(callee) =
                module.and_then(|module| module.functions.get(inst.static_call_func_id() as usize))
            else {
                let Some(ret_start) = packed_call_ret_start_option(inst) else {
                    return false;
                };
                return slot_in_range(slot, ret_start, inst.packed_ret_slots() as usize);
            };
            let Some(ret_start) = inst.b.checked_add(callee.param_slots) else {
                return false;
            };
            slot_in_range(slot, ret_start, callee.ret_slots as usize)
        }
        Opcode::CallClosure | Opcode::CallIface => {
            let Some(ret_start) = packed_call_ret_start_option(inst) else {
                return false;
            };
            slot_in_range(slot, ret_start, inst.packed_ret_slots() as usize)
        }
        Opcode::CallExtern => {
            let count = match &func.jit_metadata.get(pc) {
                Some(JitInstructionMetadata::CallExternLayout { ret_layout, .. }) => {
                    ret_layout.len()
                }
                _ => 1,
            };
            slot_in_range(slot, inst.a, count)
        }
        Opcode::ArrayGet | Opcode::SliceGet => {
            let count = elem_metadata_for_instruction(func, pc, opcode)
                .map(|(_, _, layout)| layout.len())
                .unwrap_or(1);
            slot_in_range(slot, inst.a, count)
        }
        Opcode::MapGet => {
            let count = match &func.jit_metadata.get(pc) {
                Some(JitInstructionMetadata::MapGet {
                    val_layout, has_ok, ..
                }) => val_layout.len() + usize::from(*has_ok),
                _ => 1,
            };
            slot_in_range(slot, inst.a, count)
        }
        Opcode::MapIterInit => slot_in_range(slot, inst.a, MAP_ITER_SLOTS),
        Opcode::MapIterNext => {
            slot_in_range(slot, inst.b, MAP_ITER_SLOTS)
                || slot_in_range(
                    slot,
                    inst.a,
                    usize::from(inst.map_iter_key_slots() + inst.map_iter_val_slots()),
                )
                || slot == inst.c
        }
        Opcode::QueueRecv => slot_in_range(
            slot,
            inst.a,
            usize::from(inst.recv_elem_slots() + u16::from(inst.recv_has_ok())),
        ),
        Opcode::SelectRecv => {
            let elem_slots = inst.recv_elem_slots();
            slot_in_range(
                slot,
                inst.a,
                usize::from(elem_slots + u16::from(inst.recv_has_ok())),
            )
        }
        Opcode::IfaceAssign | Opcode::Recover => slot_in_range(slot, inst.a, 2),
        Opcode::IfaceAssert => {
            let assert_kind = inst.flags & 0x03;
            let has_ok = ((inst.flags >> 2) & 0x01) != 0;
            let target_slots = (inst.flags >> 3) as u16;
            let dst_slots = if assert_kind == 1 {
                2
            } else {
                target_slots.max(1)
            };
            slot_in_range(slot, inst.a, usize::from(dst_slots + u16::from(has_ok)))
        }
        Opcode::StrDecodeRune => slot_in_range(slot, inst.a, 2),
        _ => slot == inst.a,
    }
}

fn slot_in_range(slot: u16, start: u16, count: usize) -> bool {
    if count == 0 || slot < start {
        return false;
    }
    let offset = usize::from(slot - start);
    offset < count
}

fn packed_call_ret_start_option(inst: Instruction) -> Option<u16> {
    inst.b.checked_add(inst.packed_arg_slots())
}

fn checked_packed_call_ret_start(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
    access: &'static str,
) -> Result<u16, ModuleVerificationError> {
    inst.b.checked_add(inst.packed_arg_slots()).ok_or_else(|| {
        ModuleVerificationError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: inst.b,
            count: inst.packed_arg_slots(),
            access,
        }
    })
}

fn constant_int_for_slot_before(
    facts: &ConstantFactAnalysis,
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<i128, ModuleVerificationError> {
    match facts.fact_for_slot(pc, slot) {
        Some(ConstantFact::Int(value)) => Ok(value),
        Some(ConstantFact::Conflict) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} register r{slot} has conflicting constants before use"),
        )),
        Some(ConstantFact::Unknown) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} register r{slot} is not a constant on every path"),
        )),
        None => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} register r{slot} is not reachable with a constant fact"),
        )),
    }
}

fn constant_u64_for_slot_before(
    facts: &ConstantFactAnalysis,
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<u64, ModuleVerificationError> {
    let value = constant_int_for_slot_before(facts, func, pc, opcode, slot, access)?;
    u64::try_from(value).map_err(|_| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} constant {value} must be non-negative"),
        )
    })
}

fn index_checked_len_before(
    facts: &IndexCheckAnalysis,
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<u16, ModuleVerificationError> {
    match facts.fact_for_slot(pc, slot) {
        Some(IndexCheckFact::Checked { len }) => Ok(len),
        Some(IndexCheckFact::Conflict) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} index r{slot} has conflicting checked lengths before use"),
        )),
        Some(IndexCheckFact::Unknown) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{access} index r{slot} is not proven by IndexCheck with a constant length on every path"
            ),
        )),
        None => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} index r{slot} is not tracked by the verifier"),
        )),
    }
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
        if start > func.local_slots || start as usize > func.slot_types.len() {
            return Err(ModuleVerificationError::SlotOutOfRange {
                func: func.name.clone(),
                pc,
                slot: start,
                local_slots: func.local_slots,
                access,
            });
        }
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

fn verify_disjoint_local_ranges(
    ctx: InstructionVerifierContext<'_>,
    lhs: LocalSlotRange,
    rhs: LocalSlotRange,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let lhs_end = local_range_end(func, pc, lhs.start, lhs.count, lhs.access)?;
    let rhs_end = local_range_end(func, pc, rhs.start, rhs.count, rhs.access)?;
    let lhs_start = usize::from(lhs.start);
    let rhs_start = usize::from(rhs.start);
    if lhs_start < rhs_end && rhs_start < lhs_end {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{} aliases {}: {lhs_start}..{lhs_end} overlaps {rhs_start}..{rhs_end}",
                lhs.access, rhs.access
            ),
        ));
    }
    Ok(())
}

fn local_range_end(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    count: usize,
    access: &'static str,
) -> Result<usize, ModuleVerificationError> {
    let count = u16::try_from(count).map_err(|_| ModuleVerificationError::SlotRangeOverflow {
        func: func.name.clone(),
        pc,
        start,
        count: u16::MAX,
        access,
    })?;
    verify_range(func, pc, start, count, access)?;
    Ok(usize::from(start) + usize::from(count))
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
            if *elem_bytes == 0 {
                return Some(Vec::new());
            }
            let slots = (*elem_bytes as usize).div_ceil(8);
            (slot_layout.len() == slots).then(|| slot_layout.clone())
        }
        _ => None,
    }
}

fn elem_runtime_layout_from_instruction(
    metadata: &JitInstructionMetadata,
) -> Option<Vec<SlotType>> {
    match metadata {
        JitInstructionMetadata::ElemLayout { slot_layout, .. } => Some(slot_layout.clone()),
        _ => None,
    }
}

fn elem_metadata_for_instruction(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(u32, bool, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "ElemLayout", |metadata| match metadata {
        JitInstructionMetadata::ElemLayout {
            elem_bytes,
            needs_sign_extend,
            slot_layout,
        } => Some((*elem_bytes, *needs_sign_extend, slot_layout.clone())),
        _ => None,
    })
}

fn elem_metadata_matches_flags(metadata: &JitInstructionMetadata, flags: u8) -> bool {
    if flags == 0 {
        return true;
    }
    let JitInstructionMetadata::ElemLayout {
        elem_bytes,
        needs_sign_extend,
        ..
    } = metadata
    else {
        return true;
    };
    let (flag_bytes, _, flag_needs_sign_extend) = elem_layout_from_flags(flags);
    *elem_bytes as usize == flag_bytes && *needs_sign_extend == flag_needs_sign_extend
}

fn elem_layout_for_indexed(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    flags: u8,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    if let Some(metadata) = func.jit_metadata.get(pc) {
        if let Some(layout) = elem_layout_from_instruction(metadata) {
            if !elem_metadata_matches_flags(metadata, flags) {
                return Err(instruction_metadata_invariant(
                    func,
                    pc,
                    format!("ElemLayout is inconsistent with {opcode:?} flags {flags:#04x}"),
                ));
            }
            return Ok(layout);
        }
    }
    if flags == 0 {
        return decode_metadata_layout(
            func,
            pc,
            opcode,
            "ElemLayout",
            elem_layout_from_instruction,
        );
    }
    Ok(elem_layout_from_flags(flags).1)
}

fn elem_runtime_layout_for_indexed(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    flags: u8,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    if let Some(metadata) = func.jit_metadata.get(pc) {
        if let Some(layout) = elem_runtime_layout_from_instruction(metadata) {
            if !elem_metadata_matches_flags(metadata, flags) {
                return Err(instruction_metadata_invariant(
                    func,
                    pc,
                    format!("ElemLayout is inconsistent with {opcode:?} flags {flags:#04x}"),
                ));
            }
            return Ok(layout);
        }
    }
    if flags == 0 {
        return decode_metadata_layout(
            func,
            pc,
            opcode,
            "ElemLayout",
            elem_runtime_layout_from_instruction,
        );
    }
    Ok(elem_layout_from_flags(flags).1)
}

fn verify_dynamic_elem_bytes_contract(
    constant_facts: &ConstantFactAnalysis,
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    flags: u8,
    elem_bytes_slot: u16,
) -> Result<(), ModuleVerificationError> {
    if flags != 0 {
        return Ok(());
    }
    if !constant_facts.is_reachable(pc) {
        return Ok(());
    }
    let (metadata_bytes, _, _) = elem_metadata_for_instruction(func, pc, opcode)?;
    let constant = constant_u64_for_slot_before(
        constant_facts,
        func,
        pc,
        opcode,
        elem_bytes_slot,
        "dynamic elem_bytes",
    )?;
    if constant == metadata_bytes as u64 {
        return Ok(());
    }
    Err(call_shape_mismatch(
        func,
        pc,
        opcode,
        format!(
            "{opcode:?} dynamic elem_bytes constant {constant} does not match metadata {metadata_bytes}"
        ),
    ))
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

fn verify_ptr_new_contract(
    func: &FunctionDef,
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    if value_layout.len() != inst.c as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "PtrNew metadata layout slots {} do not match allocation slots {}",
                value_layout.len(),
                inst.c
            ),
        ));
    }
    verify_ptr_new_runtime_metadata(
        func,
        module,
        constant_facts,
        pc,
        opcode,
        inst,
        &value_layout,
    )?;
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

fn verify_ptr_new_runtime_metadata(
    func: &FunctionDef,
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    value_layout: &[SlotType],
) -> Result<(), ModuleVerificationError> {
    if !constant_facts.is_reachable(pc) {
        return Ok(());
    }
    let meta_raw =
        constant_u64_for_slot_before(constant_facts, func, pc, opcode, inst.b, "PtrNew metadata")?;
    let meta_raw = u32::try_from(meta_raw).map_err(|_| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("PtrNew metadata raw 0x{meta_raw:x} exceeds u32::MAX"),
        )
    })?;
    let value_meta = ValueMeta::from_raw(meta_raw);
    let runtime_layout = value_meta_slot_layout(module, value_meta, "PtrNew value metadata")?;
    if runtime_layout != value_layout {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "PtrNew value metadata layout {runtime_layout:?} does not match JIT metadata {value_layout:?}"
            ),
        ));
    }
    Ok(())
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

fn call_iface_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(u32, Vec<SlotType>, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "CallIfaceLayout",
        |metadata| match metadata {
            JitInstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                arg_layout,
                ret_layout,
            } => Some((*iface_meta_id, arg_layout.clone(), ret_layout.clone())),
            _ => None,
        },
    )
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

fn map_new_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "MapNew", |metadata| match metadata {
        JitInstructionMetadata::MapNew {
            key_layout,
            val_layout,
        } => Some((key_layout.clone(), val_layout.clone())),
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
    ctx: InstructionVerifierContext<'_>,
    contract: BinarySlotContract<'_>,
) -> Result<(), ModuleVerificationError> {
    contract.dst.verify(ctx, ctx.inst.a, contract.dst_access)?;
    contract.lhs.verify(ctx, ctx.inst.b, contract.lhs_access)?;
    contract.rhs.verify(ctx, ctx.inst.c, contract.rhs_access)
}

fn verify_unary_slot_contract(
    ctx: InstructionVerifierContext<'_>,
    contract: UnarySlotContract<'_>,
) -> Result<(), ModuleVerificationError> {
    contract.dst.verify(ctx, ctx.inst.a, contract.dst_access)?;
    contract.src.verify(ctx, ctx.inst.b, contract.src_access)
}

fn verify_binary_one_of_slot_contract(
    ctx: InstructionVerifierContext<'_>,
    contract: BinarySlotContract<'_>,
) -> Result<(), ModuleVerificationError> {
    verify_binary_slot_contract(ctx, contract)
}

fn verify_unary_one_of_slot_contract(
    ctx: InstructionVerifierContext<'_>,
    contract: UnarySlotContract<'_>,
) -> Result<(), ModuleVerificationError> {
    verify_unary_slot_contract(ctx, contract)
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
    ctx: InstructionVerifierContext<'_>,
    index_check_facts: &IndexCheckAnalysis,
    dst_start: u16,
    base_start: u16,
    index_slot: u16,
    count: u16,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
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
    verify_dynamic_slot_span(
        ctx,
        index_check_facts,
        base_start,
        index_slot,
        &elem_layout,
        "SlotGet element span",
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

fn verify_slot_set_contract(
    ctx: InstructionVerifierContext<'_>,
    index_check_facts: &IndexCheckAnalysis,
    base_start: u16,
    index_slot: u16,
    src_start: u16,
    count: u16,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
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
    verify_dynamic_slot_span(
        ctx,
        index_check_facts,
        base_start,
        index_slot,
        &elem_layout,
        "SlotSet element span",
    )?;
    verify_local_layout_matches(func, pc, opcode, src_start, &elem_layout, "SlotSet source")
}

fn verify_dynamic_slot_span(
    ctx: InstructionVerifierContext<'_>,
    index_check_facts: &IndexCheckAnalysis,
    base_start: u16,
    index_slot: u16,
    elem_layout: &[SlotType],
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let checked_len =
        index_checked_len_before(index_check_facts, func, pc, opcode, index_slot, access)?;
    let Some(total_slots) = elem_layout.len().checked_mul(usize::from(checked_len)) else {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} slot count overflows usize"),
        ));
    };
    let total_slots = u16::try_from(total_slots).map_err(|_| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{access} covers {} element slots, exceeding addressable local slot range",
                total_slots
            ),
        )
    })?;

    let actual = local_layout(func, pc, base_start, total_slots, access)?;
    let mut expected = Vec::with_capacity(total_slots as usize);
    for _ in 0..checked_len {
        expected.extend_from_slice(elem_layout);
    }
    if actual == expected.as_slice() {
        Ok(())
    } else {
        Err(ModuleVerificationError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: base_start,
            expected,
            actual: actual.to_vec(),
        })
    }
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

fn verify_trunc_contract(
    ctx: InstructionVerifierContext<'_>,
) -> Result<(), ModuleVerificationError> {
    let bytes = ctx.inst.flags & 0x7F;
    if !matches!(bytes, 1 | 2 | 4) {
        return Err(call_shape_mismatch(
            ctx.func,
            ctx.pc,
            ctx.opcode,
            format!("unsupported Trunc flags 0x{:02x}", ctx.inst.flags),
        ));
    }
    verify_unary_slot_contract(
        ctx,
        UnarySlotContract::exact(
            SlotType::Value,
            SlotType::Value,
            "Trunc destination",
            "Trunc source",
        ),
    )
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
    if flags.is_error_return() && func.error_ret_slot < 0 {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            "error return flag set but function has no error_ret_slot".to_string(),
        ));
    }
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
    module: &Module,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    is_closure: bool,
) -> Result<(), ModuleVerificationError> {
    let (arg_layout, ret_layout) = if is_closure {
        verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::GcRef],
            "CallClosure callee",
        )?;
        call_layout(func, pc, opcode)?
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
        let (iface_meta_id, arg_layout, ret_layout) = call_iface_layout(func, pc, opcode)?;
        let Some(iface_meta) = module.interface_metas.get(iface_meta_id as usize) else {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("CallIface metadata references missing interface meta id {iface_meta_id}"),
            ));
        };
        if inst.flags as usize >= iface_meta.methods.len() {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "CallIface method_idx {} out of bounds for callsite interface {} method count {}",
                    inst.flags,
                    iface_meta_id,
                    iface_meta.methods.len()
                ),
            ));
        }
        (arg_layout, ret_layout)
    };
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
    let ret_start = checked_packed_call_ret_start(func, pc, inst, "dynamic call returns")?;
    verify_local_layout_matches(func, pc, opcode, inst.b, &arg_layout, "dynamic call args")?;
    verify_local_layout_matches(
        func,
        pc,
        opcode,
        ret_start,
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
    if let Some(param_slots) = extern_def.params.exact_slots() {
        if param_slots != inst.flags as u16 {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "CallExtern arg slot count {} does not match extern {} params {}",
                    inst.flags,
                    extern_def.name,
                    extern_def.params.display_name()
                ),
            ));
        }
    }
    let (arg_layout, ret_layout) = call_extern_layout(func, pc, opcode)?;
    if arg_layout.len() != inst.flags as usize
        || ret_layout.len() != extern_def.returns.slots as usize
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
                extern_def.returns.slots
            ),
        ));
    }
    validate_dynamic_call_extern_layout(&extern_def.name, &arg_layout)
        .map_err(|detail| call_shape_mismatch(func, pc, opcode, detail))?;
    if extern_def.returns.slot_types.is_empty() {
        if ret_layout
            .iter()
            .any(|slot_type| !matches!(slot_type, SlotType::Value))
        {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "CallExtern return layout for extern {} requires precise return slot_types",
                    extern_def.name
                ),
            ));
        }
    } else if extern_def.returns.slot_types != ret_layout {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "CallExtern return layout for extern {} does not match resolved declaration",
                extern_def.name
            ),
        ));
    }
    if let Some(expected_layout) = known_builtin_extern_param_slot_types(&extern_def.name) {
        if arg_layout != expected_layout {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "CallExtern argument layout for builtin extern {} does not match builtin ABI",
                    extern_def.name
                ),
            ));
        }
    }
    for (idx, (kind, slot_type)) in extern_def.param_kinds.iter().zip(&arg_layout).enumerate() {
        if !ext_slot_kind_matches_slot_type(*kind, *slot_type) {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "CallExtern parameter layout for extern {} slot {idx} has {:?} but param_kinds expects {:?}",
                    extern_def.name, slot_type, kind
                ),
            ));
        }
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
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = elem_runtime_layout_for_indexed(func, pc, opcode, inst.flags)?;
    verify_indexed_new_runtime_metadata(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        module,
        constant_facts,
        inst.b,
        "ArrayNew element metadata",
        &elem_layout,
    )?;
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
    )?;
    let elem_bytes_slot = checked_slot_offset(func, pc, inst.c, 1, "ArrayNew elem bytes")?;
    verify_dynamic_elem_bytes_contract(
        constant_facts,
        func,
        pc,
        opcode,
        inst.flags,
        elem_bytes_slot,
    )
}

fn verify_slice_new_contract(
    func: &FunctionDef,
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = elem_runtime_layout_for_indexed(func, pc, opcode, inst.flags)?;
    verify_indexed_new_runtime_metadata(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        module,
        constant_facts,
        inst.b,
        "SliceNew element metadata",
        &elem_layout,
    )?;
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
    )?;
    let elem_bytes_slot = checked_slot_offset(func, pc, inst.c, 2, "SliceNew elem bytes")?;
    verify_dynamic_elem_bytes_contract(
        constant_facts,
        func,
        pc,
        opcode,
        inst.flags,
        elem_bytes_slot,
    )
}

fn verify_indexed_new_runtime_metadata(
    ctx: InstructionVerifierContext<'_>,
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    meta_slot: u16,
    label: &'static str,
    elem_layout: &[SlotType],
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    if !constant_facts.is_reachable(pc) {
        return Ok(());
    }
    let raw = constant_u64_for_slot_before(constant_facts, func, pc, opcode, meta_slot, label)?;
    let raw = u32::try_from(raw).map_err(|_| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{label} raw 0x{raw:x} exceeds u32::MAX"),
        )
    })?;
    let elem_meta = ValueMeta::from_raw(raw);
    let elem_meta_layout = value_meta_slot_layout(module, elem_meta, label)?;
    if elem_meta_layout == elem_layout {
        return Ok(());
    }
    Err(call_shape_mismatch(
        func,
        pc,
        opcode,
        format!("{label} layout {elem_meta_layout:?} does not match JIT metadata {elem_layout:?}"),
    ))
}

fn verify_indexed_get_contract(
    ctx: InstructionVerifierContext<'_>,
    constant_facts: &ConstantFactAnalysis,
    access: IndexedAccessLabels,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let inst = ctx.inst;
    let elem_layout = elem_layout_for_indexed(func, pc, opcode, inst.flags)?;
    let elem_bytes_slot = checked_slot_offset(func, pc, inst.c, 1, access.value)?;
    verify_dynamic_elem_bytes_contract(
        constant_facts,
        func,
        pc,
        opcode,
        inst.flags,
        elem_bytes_slot,
    )?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], access.base)?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], access.index)?;
    if elem_layout.is_empty() {
        Ok(())
    } else {
        verify_storage_layout_compatible(func, pc, opcode, inst.a, &elem_layout, access.value)
    }
}

fn verify_indexed_set_contract(
    ctx: InstructionVerifierContext<'_>,
    constant_facts: &ConstantFactAnalysis,
    access: IndexedAccessLabels,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let inst = ctx.inst;
    let elem_layout = elem_layout_for_indexed(func, pc, opcode, inst.flags)?;
    let elem_bytes_slot = checked_slot_offset(func, pc, inst.b, 1, access.value)?;
    verify_dynamic_elem_bytes_contract(
        constant_facts,
        func,
        pc,
        opcode,
        inst.flags,
        elem_bytes_slot,
    )?;
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcRef], access.base)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::Value], access.index)?;
    if elem_layout.is_empty() {
        Ok(())
    } else {
        verify_storage_layout_compatible(func, pc, opcode, inst.c, &elem_layout, access.value)
    }
}

fn verify_indexed_addr_contract(
    ctx: InstructionVerifierContext<'_>,
    constant_facts: &ConstantFactAnalysis,
    access: IndexedAccessLabels,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let inst = ctx.inst;
    if inst.flags == 0 {
        let _ = elem_layout_for_indexed(func, pc, opcode, inst.flags)?;
        let elem_bytes_slot = checked_slot_offset(func, pc, inst.c, 1, access.value)?;
        verify_dynamic_elem_bytes_contract(
            constant_facts,
            func,
            pc,
            opcode,
            inst.flags,
            elem_bytes_slot,
        )?;
    }
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcRef], access.value)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcRef], access.base)?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], access.index)
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
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = elem_layout_for_indexed(func, pc, opcode, inst.flags)?;
    let elem_runtime_layout = elem_runtime_layout_for_indexed(func, pc, opcode, inst.flags)?;
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
    verify_indexed_new_runtime_metadata(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        module,
        constant_facts,
        inst.c,
        "SliceAppend element metadata",
        &elem_runtime_layout,
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
        let elem_bytes_slot = checked_slot_offset(func, pc, inst.c, 1, "SliceAppend elem bytes")?;
        verify_dynamic_elem_bytes_contract(
            constant_facts,
            func,
            pc,
            opcode,
            inst.flags,
            elem_bytes_slot,
        )?;
    }
    if elem_layout.is_empty() {
        Ok(())
    } else {
        verify_storage_layout_compatible(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.c, elem_offset, "SliceAppend element")?,
            &elem_layout,
            "SliceAppend element",
        )
    }
}

fn verify_map_new_contract(
    func: &FunctionDef,
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let (key_layout, val_layout) = map_new_layout(func, pc, opcode)?;
    if key_layout.len() != inst.map_new_key_slots() as usize
        || val_layout.len() != inst.map_new_val_slots() as usize
    {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "MapNew metadata layout key={} val={} does not match encoded key={} val={}",
                key_layout.len(),
                val_layout.len(),
                inst.map_new_key_slots(),
                inst.map_new_val_slots()
            ),
        ));
    }
    verify_map_new_runtime_metadata(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        module,
        constant_facts,
        MapLayoutExpectation {
            key_layout: &key_layout,
            val_layout: &val_layout,
        },
    )?;
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

fn verify_map_new_runtime_metadata(
    ctx: InstructionVerifierContext<'_>,
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    expected: MapLayoutExpectation<'_>,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let inst = ctx.inst;
    if !constant_facts.is_reachable(pc) {
        return Ok(());
    }
    let packed =
        constant_u64_for_slot_before(constant_facts, func, pc, opcode, inst.b, "MapNew metadata")?;
    let key_meta = ValueMeta::from_raw((packed >> 32) as u32);
    let val_meta = ValueMeta::from_raw(packed as u32);
    let key_meta_layout = value_meta_slot_layout(module, key_meta, "MapNew key metadata")?;
    let val_meta_layout = value_meta_slot_layout(module, val_meta, "MapNew value metadata")?;
    verify_map_new_meta_layout_matches(
        func,
        pc,
        opcode,
        "key",
        &key_meta_layout,
        expected.key_layout,
    )?;
    verify_map_new_meta_layout_matches(
        func,
        pc,
        opcode,
        "value",
        &val_meta_layout,
        expected.val_layout,
    )?;

    let key_rttid_slot = checked_slot_offset(func, pc, inst.b, 1, "MapNew key RTTID")?;
    let key_rttid_raw = constant_u64_for_slot_before(
        constant_facts,
        func,
        pc,
        opcode,
        key_rttid_slot,
        "MapNew key RTTID",
    )?;
    let key_rttid_raw = u32::try_from(key_rttid_raw).map_err(|_| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("MapNew key RTTID 0x{key_rttid_raw:x} exceeds u32::MAX"),
        )
    })?;
    if key_rttid_raw > META_ID_MASK {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("MapNew bare key RTTID {key_rttid_raw} exceeds packed ValueRttid width"),
        ));
    }
    let key_rttid = ValueRttid::new(key_rttid_raw, key_meta.value_kind());
    validate_value_rttid_ref(module, key_rttid, "MapNew key RTTID")?;
    let Some(canonical_key_meta) = module.canonical_value_meta_for_value_rttid(key_rttid) else {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            "MapNew key RTTID cannot be resolved to canonical metadata".to_string(),
        ));
    };
    if canonical_key_meta != key_meta {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "MapNew key metadata raw 0x{:x} does not match key RTTID canonical raw 0x{:x}",
                key_meta.to_raw(),
                canonical_key_meta.to_raw()
            ),
        ));
    }
    Ok(())
}

fn verify_map_new_meta_layout_matches(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    access: &'static str,
    runtime_layout: &[SlotType],
    metadata_layout: &[SlotType],
) -> Result<(), ModuleVerificationError> {
    if runtime_layout == metadata_layout {
        return Ok(());
    }
    Err(call_shape_mismatch(
        func,
        pc,
        opcode,
        format!(
            "MapNew {access} metadata layout {runtime_layout:?} does not match JIT metadata {metadata_layout:?}"
        ),
    ))
}

fn verify_queue_new_runtime_metadata(
    func: &FunctionDef,
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
    elem_layout: &[SlotType],
) -> Result<(), ModuleVerificationError> {
    if !constant_facts.is_reachable(pc) {
        return Ok(());
    }
    let packed = constant_u64_for_slot_before(
        constant_facts,
        func,
        pc,
        opcode,
        inst.b,
        "QueueNew element metadata",
    )?;
    let elem_meta = ValueMeta::from_raw(packed as u32);
    let elem_rttid_raw = u32::try_from(packed >> 32).map_err(|_| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "QueueNew element RTTID 0x{:x} exceeds u32::MAX",
                packed >> 32
            ),
        )
    })?;
    let elem_rttid = ValueRttid::from_raw(elem_rttid_raw);
    validate_value_rttid_ref(module, elem_rttid, "QueueNew element RTTID")?;
    let Some(canonical_elem_meta) = module.canonical_value_meta_for_value_rttid(elem_rttid) else {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            "QueueNew element RTTID cannot be resolved to canonical metadata".to_string(),
        ));
    };
    if canonical_elem_meta != elem_meta {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "QueueNew element metadata raw 0x{:x} does not match element RTTID canonical raw 0x{:x}",
                elem_meta.to_raw(),
                canonical_elem_meta.to_raw()
            ),
        ));
    }
    let elem_meta_layout = value_meta_slot_layout(module, elem_meta, "QueueNew element metadata")?;
    if elem_meta_layout != elem_layout {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "QueueNew element metadata layout {elem_meta_layout:?} does not match JIT metadata {elem_layout:?}"
            ),
        ));
    }
    Ok(())
}

fn value_meta_slot_layout(
    module: &Module,
    value_meta: ValueMeta,
    label: &str,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    let kind = validate_value_meta_ref(module, value_meta, label)?;
    match kind {
        ValueKind::Struct => module
            .struct_metas
            .get(value_meta.meta_id() as usize)
            .map(|meta| meta.slot_types.clone())
            .ok_or_else(|| {
                module_invariant(format!(
                    "{label} references missing struct metadata {}",
                    value_meta.meta_id()
                ))
            }),
        ValueKind::Array => module
            .slot_layout_for_value_rttid(ValueRttid::new(value_meta.meta_id(), ValueKind::Array))
            .ok_or_else(|| {
                module_invariant(format!(
                    "{label} array runtime type {} has no slot layout",
                    value_meta.meta_id()
                ))
            }),
        ValueKind::Interface => Ok(vec![SlotType::Interface0, SlotType::Interface1]),
        kind => Ok(vec![slot_type_for_value_kind(kind)]),
    }
}

fn verify_known_map_layout(
    ctx: InstructionVerifierContext<'_>,
    facts: &ContainerLayoutAnalysis,
    slot: u16,
    access: &'static str,
    expected: MapLayoutExpectation<'_>,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    match facts.fact_for_slot(pc, slot) {
        None | Some(ContainerLayoutFact::Unknown) => Ok(()),
        Some(ContainerLayoutFact::Conflict) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} map layout has conflicting known layouts"),
        )),
        Some(ContainerLayoutFact::Map {
            key_layout: known_key,
            val_layout: known_val,
        })
        | Some(ContainerLayoutFact::MapIter {
            key_layout: known_key,
            val_layout: known_val,
        }) => {
            if expected.key_layout != known_key {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "{access} key layout {:?} does not match known map key layout {known_key:?}",
                        expected.key_layout
                    ),
                ));
            }
            if expected.val_layout != known_val {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "{access} value layout {:?} does not match known map value layout {known_val:?}",
                        expected.val_layout
                    ),
                ));
            }
            Ok(())
        }
        Some(ContainerLayoutFact::Queue { elem_layout }) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{access} expected map layout but known queue element layout is {elem_layout:?}"
            ),
        )),
    }
}

fn verify_known_map_key_layout(
    func: &FunctionDef,
    facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
    key_layout: &[SlotType],
) -> Result<(), ModuleVerificationError> {
    match facts.fact_for_slot(pc, slot) {
        None | Some(ContainerLayoutFact::Unknown) => Ok(()),
        Some(ContainerLayoutFact::Conflict) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} map layout has conflicting known layouts"),
        )),
        Some(ContainerLayoutFact::Map {
            key_layout: known_key,
            ..
        })
        | Some(ContainerLayoutFact::MapIter {
            key_layout: known_key,
            ..
        }) => {
            if key_layout != known_key {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "{access} key layout {key_layout:?} does not match known map key layout {known_key:?}"
                    ),
                ));
            }
            Ok(())
        }
        Some(ContainerLayoutFact::Queue { elem_layout }) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{access} expected map layout but known queue element layout is {elem_layout:?}"
            ),
        )),
    }
}

fn verify_known_map_iter_layout(
    ctx: InstructionVerifierContext<'_>,
    facts: &ContainerLayoutAnalysis,
    slot: u16,
    access: &'static str,
    expected: MapLayoutExpectation<'_>,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    match facts.fact_for_slot(pc, slot) {
        None | Some(ContainerLayoutFact::Unknown) => Ok(()),
        Some(ContainerLayoutFact::MapIter {
            key_layout: known_key,
            val_layout: known_val,
        }) => {
            if expected.key_layout != known_key {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "{access} key layout {:?} does not match known map key layout {known_key:?}",
                        expected.key_layout
                    ),
                ));
            }
            if expected.val_layout != known_val {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "{access} value layout {:?} does not match known map value layout {known_val:?}",
                        expected.val_layout
                    ),
                ));
            }
            Ok(())
        }
        Some(ContainerLayoutFact::Conflict) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} iterator layout has conflicting known layouts"),
        )),
        Some(ContainerLayoutFact::Map { .. }) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} expected map iterator layout but found map object layout"),
        )),
        Some(ContainerLayoutFact::Queue { elem_layout }) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{access} expected map iterator layout but known queue element layout is {elem_layout:?}"
            ),
        )),
    }
}

fn verify_known_queue_layout(
    func: &FunctionDef,
    facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
    elem_layout: &[SlotType],
) -> Result<(), ModuleVerificationError> {
    match facts.fact_for_slot(pc, slot) {
        None | Some(ContainerLayoutFact::Unknown) => Ok(()),
        Some(ContainerLayoutFact::Conflict) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} queue layout has conflicting known layouts"),
        )),
        Some(ContainerLayoutFact::Queue {
            elem_layout: known_elem,
        }) => {
            if elem_layout != known_elem {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "{access} element layout {elem_layout:?} does not match known queue element layout {known_elem:?}"
                    ),
                ));
            }
            Ok(())
        }
        Some(ContainerLayoutFact::Map {
            key_layout,
            val_layout,
        })
        | Some(ContainerLayoutFact::MapIter {
            key_layout,
            val_layout,
        }) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{access} expected queue layout but known map layout is key {key_layout:?} value {val_layout:?}"
            ),
        )),
    }
}

fn verify_known_map_object(
    func: &FunctionDef,
    facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    match facts.fact_for_slot(pc, slot) {
        None | Some(ContainerLayoutFact::Unknown) => Ok(()),
        Some(ContainerLayoutFact::Conflict) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} map layout has conflicting known layouts"),
        )),
        Some(ContainerLayoutFact::Map { .. }) => Ok(()),
        Some(ContainerLayoutFact::MapIter { .. }) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} expected map layout but found map iterator layout"),
        )),
        Some(ContainerLayoutFact::Queue { elem_layout }) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{access} expected map layout but known queue element layout is {elem_layout:?}"
            ),
        )),
    }
}

fn verify_known_queue_object(
    func: &FunctionDef,
    facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    match facts.fact_for_slot(pc, slot) {
        None | Some(ContainerLayoutFact::Unknown) => Ok(()),
        Some(ContainerLayoutFact::Conflict) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} queue layout has conflicting known layouts"),
        )),
        Some(ContainerLayoutFact::Queue { .. }) => Ok(()),
        Some(ContainerLayoutFact::Map {
            key_layout,
            val_layout,
        })
        | Some(ContainerLayoutFact::MapIter {
            key_layout,
            val_layout,
        }) => Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{access} expected queue layout but known map layout is key {key_layout:?} value {val_layout:?}"
            ),
        )),
    }
}

fn verify_map_get_contract(
    func: &FunctionDef,
    container_layout_facts: &ContainerLayoutAnalysis,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let (key_layout, val_layout, has_ok) = map_get_layout(func, pc, opcode)?;
    verify_known_map_layout(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        container_layout_facts,
        inst.b,
        "MapGet",
        MapLayoutExpectation {
            key_layout: &key_layout,
            val_layout: &val_layout,
        },
    )?;
    verify_metadata_layout_len_at_most(
        func,
        pc,
        opcode,
        "MapGet value",
        val_layout.len(),
        MAP_GET_MAX_VALUE_SLOTS,
    )?;
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
    if constant_facts.is_reachable(pc) {
        let packed = constant_u64_for_slot_before(
            constant_facts,
            func,
            pc,
            opcode,
            inst.c,
            "MapGet metadata",
        )?;
        if packed >> 32 != 0 {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("MapGet packed metadata 0x{packed:x} contains unsupported high bits"),
            ));
        }
        let packed_key_slots = ((packed >> 16) & 0xFFFF) as usize;
        let packed_val_slots = ((packed >> 1) & 0x7FFF) as usize;
        let packed_has_ok = (packed & 1) != 0;
        if key_layout.len() != packed_key_slots
            || val_layout.len() != packed_val_slots
            || has_ok != packed_has_ok
        {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "MapGet metadata layout key={} val={} ok={} does not match packed key={} val={} ok={}",
                    key_layout.len(),
                    val_layout.len(),
                    has_ok,
                    packed_key_slots,
                    packed_val_slots,
                    packed_has_ok
                ),
            ));
        }
    }
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
    container_layout_facts: &ContainerLayoutAnalysis,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let (key_layout, val_layout) = map_set_layout(func, pc, opcode)?;
    verify_known_map_layout(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        container_layout_facts,
        inst.a,
        "MapSet",
        MapLayoutExpectation {
            key_layout: &key_layout,
            val_layout: &val_layout,
        },
    )?;
    verify_metadata_layout_len_at_most(
        func,
        pc,
        opcode,
        "MapSet key",
        key_layout.len(),
        MAP_SET_MAX_KEY_VAL_SLOTS,
    )?;
    verify_metadata_layout_len_at_most(
        func,
        pc,
        opcode,
        "MapSet value",
        val_layout.len(),
        MAP_SET_MAX_KEY_VAL_SLOTS,
    )?;
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcRef], "MapSet map")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::Value],
        "MapSet metadata",
    )?;
    if constant_facts.is_reachable(pc) {
        let packed = constant_u64_for_slot_before(
            constant_facts,
            func,
            pc,
            opcode,
            inst.b,
            "MapSet metadata",
        )?;
        if packed >> 16 != 0 {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("MapSet packed metadata 0x{packed:x} contains unsupported high bits"),
            ));
        }
        let packed_key_slots = ((packed >> 8) & 0xFF) as usize;
        let packed_val_slots = (packed & 0xFF) as usize;
        if key_layout.len() != packed_key_slots || val_layout.len() != packed_val_slots {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "MapSet metadata layout key={} val={} does not match packed key={} val={}",
                    key_layout.len(),
                    val_layout.len(),
                    packed_key_slots,
                    packed_val_slots
                ),
            ));
        }
    }
    let key_start = checked_slot_offset(func, pc, inst.b, 1, "MapSet key")?;
    verify_storage_layout_compatible(func, pc, opcode, key_start, &key_layout, "MapSet key")?;
    verify_storage_layout_compatible(func, pc, opcode, inst.c, &val_layout, "MapSet value")
}

fn verify_metadata_layout_len_at_most(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    access: &'static str,
    len: usize,
    max: u16,
) -> Result<(), ModuleVerificationError> {
    if len <= max as usize {
        return Ok(());
    }
    Err(call_shape_mismatch(
        func,
        pc,
        opcode,
        format!("{access} metadata layout slots {len} exceed packed ABI max {max}"),
    ))
}

fn verify_map_delete_contract(
    func: &FunctionDef,
    container_layout_facts: &ContainerLayoutAnalysis,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let key_layout = map_delete_key_layout(func, pc, opcode)?;
    verify_known_map_key_layout(
        func,
        container_layout_facts,
        pc,
        opcode,
        inst.a,
        "MapDelete",
        &key_layout,
    )?;
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
    if constant_facts.is_reachable(pc) {
        let packed = constant_u64_for_slot_before(
            constant_facts,
            func,
            pc,
            opcode,
            inst.b,
            "MapDelete metadata",
        )?;
        if packed > u16::MAX as u64 {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("MapDelete packed metadata 0x{packed:x} exceeds u16::MAX"),
            ));
        }
        let packed_key_slots = packed as usize;
        if key_layout.len() != packed_key_slots {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "MapDelete metadata layout key={} does not match packed key={}",
                    key_layout.len(),
                    packed_key_slots
                ),
            ));
        }
    }
    let key_start = checked_slot_offset(func, pc, inst.b, 1, "MapDelete key")?;
    verify_storage_layout_compatible(func, pc, opcode, key_start, &key_layout, "MapDelete key")
}

fn verify_map_iter_next_contract(
    func: &FunctionDef,
    container_layout_facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let (key_layout, val_layout) = map_iter_next_layout(func, pc, opcode)?;
    verify_known_map_iter_layout(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        container_layout_facts,
        inst.b,
        "MapIterNext",
        MapLayoutExpectation {
            key_layout: &key_layout,
            val_layout: &val_layout,
        },
    )?;
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
    let key_start = inst.a;
    let value_start = checked_slot_offset(
        func,
        pc,
        key_start,
        key_layout.len() as u16,
        "MapIterNext value",
    )?;
    verify_layout(func, pc, opcode, key_start, &key_layout, "MapIterNext key")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &MAP_ITER_SLOT_TYPES,
        "MapIterNext iterator",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        value_start,
        &val_layout,
        "MapIterNext value",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.c,
        &[SlotType::Value],
        "MapIterNext ok",
    )?;
    verify_disjoint_local_ranges(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        LocalSlotRange {
            access: "MapIterNext key",
            start: key_start,
            count: key_layout.len(),
        },
        LocalSlotRange {
            access: "iterator state",
            start: inst.b,
            count: MAP_ITER_SLOTS,
        },
    )?;
    verify_disjoint_local_ranges(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        LocalSlotRange {
            access: "MapIterNext ok",
            start: inst.c,
            count: 1,
        },
        LocalSlotRange {
            access: "MapIterNext key",
            start: key_start,
            count: key_layout.len(),
        },
    )?;
    verify_disjoint_local_ranges(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        LocalSlotRange {
            access: "MapIterNext ok",
            start: inst.c,
            count: 1,
        },
        LocalSlotRange {
            access: "MapIterNext value",
            start: value_start,
            count: val_layout.len(),
        },
    )?;
    verify_disjoint_local_ranges(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        LocalSlotRange {
            access: "MapIterNext value",
            start: value_start,
            count: val_layout.len(),
        },
        LocalSlotRange {
            access: "iterator state",
            start: inst.b,
            count: MAP_ITER_SLOTS,
        },
    )?;
    verify_disjoint_local_ranges(
        InstructionVerifierContext {
            func,
            pc,
            opcode,
            inst,
        },
        LocalSlotRange {
            access: "MapIterNext ok",
            start: inst.c,
            count: 1,
        },
        LocalSlotRange {
            access: "iterator state",
            start: inst.b,
            count: MAP_ITER_SLOTS,
        },
    )
}

fn verify_queue_new_contract(
    func: &FunctionDef,
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    if elem_layout.len() != inst.queue_new_elem_slots() as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "QueueNew metadata layout slots {} do not match encoded elem slots {}",
                elem_layout.len(),
                inst.queue_new_elem_slots()
            ),
        ));
    }
    verify_queue_new_runtime_metadata(
        func,
        module,
        constant_facts,
        pc,
        opcode,
        inst,
        &elem_layout,
    )?;
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
    container_layout_facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    verify_known_queue_layout(
        func,
        container_layout_facts,
        pc,
        opcode,
        inst.a,
        "QueueSend",
        &elem_layout,
    )?;
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
    container_layout_facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    verify_known_queue_layout(
        func,
        container_layout_facts,
        pc,
        opcode,
        inst.b,
        "QueueRecv",
        &elem_layout,
    )?;
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
    container_layout_facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    verify_known_queue_layout(
        func,
        container_layout_facts,
        pc,
        opcode,
        inst.a,
        "SelectSend",
        &elem_layout,
    )?;
    let elem_slots = inst.flags as u16;
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
    container_layout_facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    verify_known_queue_layout(
        func,
        container_layout_facts,
        pc,
        opcode,
        inst.b,
        "SelectRecv",
        &elem_layout,
    )?;
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
    verify_local_layout_matches(func, pc, opcode, inst.a, &elem_layout, "SelectRecv value")?;
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
    let target_func_id = inst.closure_new_func_id();
    let target = module
        .functions
        .get(target_func_id as usize)
        .ok_or_else(|| ModuleVerificationError::MissingFunction {
            func: func.name.clone(),
            pc,
            callee_id: target_func_id,
        })?;
    if inst.c as usize != target.capture_slot_types.len() {
        return Err(call_shape_mismatch(
            func,
            pc,
            Opcode::ClosureNew,
            format!(
                "ClosureNew encoded capture count {} does not match target {} capture slots {}",
                inst.c,
                target_func_id,
                target.capture_slot_types.len()
            ),
        ));
    }
    verify_layout(
        func,
        pc,
        Opcode::ClosureNew,
        inst.a,
        &[SlotType::GcRef],
        "ClosureNew destination",
    )?;
    Ok(())
}

fn verify_closure_get_contract(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    if !func.is_closure {
        return Err(call_shape_mismatch(
            func,
            pc,
            Opcode::ClosureGet,
            "ClosureGet is only valid in closure-shaped functions".to_string(),
        ));
    }
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
        if opcode == Opcode::GoStart {
            let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
            if !ret_layout.is_empty() || arg_layout.as_slice() != expected_args {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "GoStart metadata layout slots args={:?} returns={:?} do not match callee {} args={:?}",
                        arg_layout,
                        ret_layout,
                        callee.name,
                        expected_args
                    ),
                ));
            }
        }
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
    verify_interface_pair(func, pc, opcode, inst.a, "IfaceAssign destination")?;
    verify_iface_assign_source(func, pc, opcode, inst.b, kind)
}

fn verify_iface_assert_contract(
    func: &FunctionDef,
    module: &Module,
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
    let dst_slots =
        iface_assert_result_slots_from_flags(u16::from(inst.flags)).ok_or_else(|| {
            call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "IfaceAssert encoded target slots {} are invalid for assert kind {assert_kind}",
                    inst.flags >> 3
                ),
            )
        })?;
    let result_layout = iface_assert_result_layout(func, pc, opcode)?;
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
    let expected_layout = if assert_kind == 1 {
        let target_id = inst.c as usize;
        if target_id >= module.interface_metas.len() {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("IfaceAssert target interface meta {target_id} is missing"),
            ));
        }
        vec![SlotType::Interface0, SlotType::Interface1]
    } else {
        let target_id = inst.c as usize;
        if target_id >= module.runtime_types.len() {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("IfaceAssert target runtime type {target_id} is missing"),
            ));
        }
        let target_kind =
            expected_value_kind_for_rttid(module, target_id, "IfaceAssert target runtime type")?;
        if target_kind == ValueKind::Interface {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "IfaceAssert target runtime type {target_id} has Interface kind; interface targets must use assert_kind=1"
                ),
            ));
        }
        module
            .slot_layout_for_value_rttid(ValueRttid::new(inst.c as u32, target_kind))
            .ok_or_else(|| {
                call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "IfaceAssert target runtime type {target_id} cannot be resolved to a slot layout"
                    ),
                )
            })?
    };
    if result_layout != expected_layout {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "IfaceAssert metadata layout {:?} does not match target layout {:?}",
                result_layout, expected_layout
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
    if value_kind == ValueKind::Void {
        if packed != 0 {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("nil interface source metadata must be zero, got 0x{packed:x}"),
            ));
        }
    } else if value_kind == ValueKind::Interface {
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
    } else {
        validate_value_rttid_ref(
            module,
            ValueRttid::new(high, value_kind),
            "IfaceAssign source",
        )
        .map_err(|err| call_shape_mismatch(func, pc, opcode, err.to_string()))?;
        if low == IFACE_ASSIGN_NO_ITAB {
            return Ok(());
        }
        if low == 0 {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                "itab id 0 is reserved; empty-interface assignments must use IFACE_ASSIGN_NO_ITAB"
                    .to_string(),
            ));
        }
        if low as usize >= module.itabs.len() {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("itab id {low} exceeds itab count {}", module.itabs.len()),
            ));
        }
        let itab = &module.itabs[low as usize];
        verify_iface_assign_itab_receiver_layout(
            func,
            module,
            pc,
            opcode,
            ItabReceiverContract {
                rttid: high,
                value_kind,
                itab_id: low,
                itab,
            },
        )?;
    }
    Ok(())
}

fn named_type_id_for_rttid(module: &Module, rttid: u32) -> Option<u32> {
    match module.runtime_types.get(rttid as usize)? {
        RuntimeType::Named { id, .. } => Some(*id),
        RuntimeType::Pointer(inner) => named_type_id_for_rttid(module, inner.rttid()),
        _ => None,
    }
}

fn verify_iface_assign_itab_receiver_layout(
    func: &FunctionDef,
    module: &Module,
    pc: usize,
    opcode: Opcode,
    contract: ItabReceiverContract<'_>,
) -> Result<(), ModuleVerificationError> {
    let rttid = contract.rttid;
    let value_kind = contract.value_kind;
    let itab_id = contract.itab_id;
    let itab = contract.itab;
    if value_kind == ValueKind::Interface {
        return Ok(());
    }
    let Some(named_type_id) = named_type_id_for_rttid(module, rttid) else {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("IfaceAssign source RTTID {rttid} is not a named type"),
        ));
    };
    let Some(named_type) = module.named_type_metas.get(named_type_id as usize) else {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("IfaceAssign source named type id {named_type_id} is missing"),
        ));
    };
    let Some(iface_meta) = module.interface_metas.get(itab.iface_meta_id as usize) else {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "IfaceAssign itab {itab_id} target interface meta id {} is missing",
                itab.iface_meta_id
            ),
        ));
    };
    if itab.methods.len() != iface_meta.methods.len() {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "IfaceAssign itab {itab_id} method count {} does not match interface {} method count {}",
                itab.methods.len(),
                itab.iface_meta_id,
                iface_meta.methods.len()
            ),
        ));
    }
    for (method_idx, target_id) in itab.methods.iter().copied().enumerate() {
        let target = module.functions.get(target_id as usize).ok_or_else(|| {
            call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "IfaceAssign itab {itab_id} method {method_idx} references missing function {target_id}"
                ),
            )
        })?;
        let iface_method = &iface_meta.methods[method_idx];
        let method = named_type
            .methods
            .get(&iface_method.name)
            .ok_or_else(|| {
                call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "IfaceAssign itab {itab_id} method {method_idx} expected interface method {} is not implemented by receiver source RTTID {rttid}",
                        iface_method.name
                    ),
                )
            })?;
        if method.signature_rttid != iface_method.signature_rttid {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "IfaceAssign itab {itab_id} method {method_idx} expected interface method {} signature_rttid={} but receiver method signature_rttid={}",
                    iface_method.name,
                    iface_method.signature_rttid,
                    method.signature_rttid
                ),
            ));
        }
        if method.func_id != target_id {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "IfaceAssign itab {itab_id} method {method_idx} expected interface method {} target function {} but itab references {}",
                    iface_method.name,
                    method.func_id,
                    target_id
                ),
            ));
        }
        let expected = method
            .iface_receiver_slot_type_for_source_kind(value_kind)
            .map_err(|reason| {
                call_shape_mismatch(
                    func,
                    pc,
                    opcode,
                    format!(
                        "IfaceAssign itab {itab_id} method {method_idx} target function {target_id} ({}) violates receiver ownership: {reason} for source kind {:?}",
                        target.name, value_kind
                    ),
                )
            })?;
        if target.recv_slots != 1 || target.slot_types.first() != Some(&expected) {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "IfaceAssign itab {itab_id} method {method_idx} target function {target_id} ({}) receiver layout {:?} does not match source kind {:?} storage {:?}",
                    target.name,
                    target.slot_types.first(),
                    value_kind,
                    expected
                ),
            ));
        }
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
mod tests;
