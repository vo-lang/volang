//! VM-shared bytecode and module verifier.
//!
//! This verifier owns checks that are true before any backend runs: module
//! indices, bytecode PC/slot ranges, call and extern shapes, GC layouts, write
//! barrier requirements, and derived `FunctionDef` fields. Strict JIT-specific
//! checks such as lowering capability, helper ABI, OSR metadata, side exits, and
//! frame materialization remain in `vo-jit`.

#[cfg(not(feature = "std"))]
use alloc::{
    collections::{BTreeMap, BTreeSet},
    format,
    string::{String, ToString},
    sync::Arc,
    vec,
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{
    collections::{BTreeMap, BTreeSet},
    string::{String, ToString},
    sync::Arc,
    vec,
    vec::Vec,
};

use core::{fmt, mem::size_of};

use crate::bytecode::{
    ext_slot_kind_matches_slot_type, ext_slot_kinds_for_slot_types,
    known_builtin_extern_fixed_return_slot_types, known_builtin_extern_param_slot_types,
    known_builtin_extern_requires_precise_return_layout, known_builtin_extern_return_slot_count,
    slot_type_for_value_kind, validate_ext_param_kinds_with_label, Constant, ExtSlotKind,
    ExternDef, FunctionDef, InstructionMetadata, LoadedModule, Module, ParamShape, ReturnFlags,
    RuntimeTypeFacts, SelectCaseLayout, TransferType, IFACE_ASSIGN_NO_ITAB, MAP_ITER_SLOTS,
    MAP_ITER_SLOT_TYPES, MAX_CLOSURE_CAPTURE_SLOTS,
};
use crate::instruction::{
    Instruction, Opcode, CONV_F2I_ALLOWED_FLAGS, CONV_I2F_ALLOWED_FLAGS, HINT_LOOP, HINT_NOP,
    IFACE_ASSERT_HAS_OK_FLAG, QUEUE_KIND_PORT_FLAG, QUEUE_RECV_HAS_OK_FLAG, SHIFT_ALLOWED_FLAGS,
    SLICE_SLICE_ALLOWED_FLAGS, SLICE_SLICE_FLAG_ARRAY, SLICE_SLICE_FLAG_HAS_MAX,
    SLICE_SLICE_FLAG_INLINE_ARRAY_VIEW,
};
use crate::runtime_type::RuntimeType;
use crate::types::{SlotType, ValueKind, ValueMeta, ValueRttid, INVALID_META_ID};

const RAW_I64_SLOTS: &[SlotType] = &[
    SlotType::Value,
    SlotType::GcBase,
    SlotType::GcRef,
    SlotType::Interface0,
    SlotType::Interface1,
];
const ANY_SINGLE_SLOT: &[SlotType] = &[
    SlotType::Value,
    SlotType::GcBase,
    SlotType::GcRef,
    SlotType::Interface0,
    SlotType::Interface1,
    SlotType::Float,
];
const FLOAT_STORAGE_SLOTS: &[SlotType] = &[SlotType::Float, SlotType::Value];

// Verification runs on untrusted modules before the VM or JIT may execute
// them.  Keep derived data and fixed-point work bounded independently from the
// encoded module size: a tiny instruction can otherwise fan out into one fact
// per tracked slot at every program counter.
const MAX_VERIFIER_DERIVED_BYTES: usize = 256 * 1024 * 1024;
const MAX_VERIFIER_WORK_UNITS: usize = 256 * 1024 * 1024;

struct VerifierResources {
    derived_bytes_left: usize,
    work_left: usize,
}

impl VerifierResources {
    fn new() -> Self {
        Self {
            derived_bytes_left: MAX_VERIFIER_DERIVED_BYTES,
            work_left: MAX_VERIFIER_WORK_UNITS,
        }
    }

    fn charge_bytes<T>(
        &mut self,
        func: &FunctionDef,
        count: usize,
        resource: &'static str,
    ) -> Result<(), ModuleVerificationError> {
        let requested = count.checked_mul(size_of::<T>()).ok_or_else(|| {
            verifier_resource_limit(func, resource, usize::MAX, MAX_VERIFIER_DERIVED_BYTES)
        })?;
        if requested > self.derived_bytes_left {
            return Err(verifier_resource_limit(
                func,
                resource,
                requested,
                self.derived_bytes_left,
            ));
        }
        self.derived_bytes_left -= requested;
        Ok(())
    }

    fn charge_work(
        &mut self,
        func: &FunctionDef,
        units: usize,
        resource: &'static str,
    ) -> Result<(), ModuleVerificationError> {
        if units > self.work_left {
            return Err(verifier_resource_limit(
                func,
                resource,
                units,
                self.work_left,
            ));
        }
        self.work_left -= units;
        Ok(())
    }
}

fn verifier_resource_limit(
    func: &FunctionDef,
    resource: &'static str,
    requested: usize,
    remaining: usize,
) -> ModuleVerificationError {
    ModuleVerificationError::ResourceLimit {
        func: func.name.clone(),
        resource,
        requested,
        remaining,
    }
}

fn try_none_vec<T>(
    func: &FunctionDef,
    len: usize,
    resource: &'static str,
) -> Result<Vec<Option<T>>, ModuleVerificationError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(len)
        .map_err(|_| verifier_resource_limit(func, resource, len, MAX_VERIFIER_DERIVED_BYTES))?;
    values.resize_with(len, || None);
    Ok(values)
}

fn try_filled_vec<T: Clone>(
    func: &FunctionDef,
    len: usize,
    value: T,
    resource: &'static str,
) -> Result<Vec<T>, ModuleVerificationError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(len)
        .map_err(|_| verifier_resource_limit(func, resource, len, MAX_VERIFIER_DERIVED_BYTES))?;
    values.resize(len, value);
    Ok(values)
}

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
    WrongMetadataKind {
        func: String,
        pc: usize,
        opcode: Opcode,
        metadata: &'static str,
    },
    InvalidLoopEnd {
        func: String,
        pc: usize,
        begin_pc: usize,
        end_pc: usize,
        code_len: usize,
    },
    InvalidLoopEndBackEdge {
        func: String,
        pc: usize,
        begin_pc: usize,
        end_pc: usize,
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
    ResourceLimit {
        func: String,
        resource: &'static str,
        requested: usize,
        remaining: usize,
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
            Self::WrongMetadataKind {
                func,
                pc,
                opcode,
                metadata,
            } => write!(
                f,
                "wrong instruction metadata kind {metadata} for {opcode:?} in {func} at pc {pc}"
            ),
            Self::InvalidLoopEnd {
                func,
                pc,
                begin_pc,
                end_pc,
                code_len,
            } => write!(
                f,
                "invalid LoopEnd in {func} at pc {pc}: begin_pc={begin_pc}, end_pc={end_pc}, code_len={code_len}"
            ),
            Self::InvalidLoopEndBackEdge {
                func,
                pc,
                begin_pc,
                end_pc,
            } => write!(
                f,
                "LoopEnd in {func} at pc {pc} points at end_pc={end_pc}, which is not a back-edge to begin_pc={begin_pc}"
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
            Self::ResourceLimit {
                func,
                resource,
                requested,
                remaining,
            } => write!(
                f,
                "verifier resource limit exceeded in {func} while building {resource}: requested {requested}, remaining {remaining}"
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

impl LoadedModule {
    /// Reborrow the common-verifier certificate owned by this loaded image.
    #[inline]
    pub fn verified_module(&self) -> VerifiedModule<'_> {
        VerifiedModule {
            module: self.module(),
        }
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
        let (verified, _) = self.verify_with_runtime_type_facts()?;
        Ok(verified)
    }

    pub fn verify_with_runtime_type_facts(
        self,
    ) -> Result<(VerifiedModule<'m>, RuntimeTypeFacts), ModuleVerificationError> {
        let runtime_type_facts = verify_module_invariants(self.module)?;
        validate_module_gc_layout(self.module)?;
        let mut resources = VerifierResources::new();
        for (idx, func) in self.module.functions.iter().enumerate() {
            verify_function_at(self.module, idx, func, &mut resources)?;
        }
        Ok((
            VerifiedModule {
                module: self.module,
            },
            runtime_type_facts,
        ))
    }
}

pub fn verify_module(module: &Module) -> Result<VerifiedModule<'_>, ModuleVerificationError> {
    ModuleVerifier::new(module).verify()
}

/// Verify an owned immutable module and bind all derived facts to that exact
/// image for VM-family sharing.
pub fn verify_loaded_module(module: Module) -> Result<LoadedModule, ModuleVerificationError> {
    let runtime_type_facts = {
        let (_, facts) = ModuleVerifier::new(&module).verify_with_runtime_type_facts()?;
        facts
    };
    let frame_root_maps = crate::frame_roots::FrameRootMaps::build(&module).map_err(|error| {
        let func = module
            .functions
            .get(error.function)
            .map_or("<missing>", |func| func.name.as_str());
        ModuleVerificationError::FunctionInvariant {
            func: func.to_string(),
            detail: format!(
                "frame root analysis failed at pc {} while building {}",
                error.pc, error.detail
            ),
        }
    })?;
    Ok(LoadedModule::new(
        module,
        runtime_type_facts,
        frame_root_maps,
    ))
}

pub fn verify_function(func: &FunctionDef, module: &Module) -> Result<(), ModuleVerificationError> {
    verify_function_common(func, module, &mut VerifierResources::new())
}

fn verify_module_invariants(module: &Module) -> Result<RuntimeTypeFacts, ModuleVerificationError> {
    let invariant = |detail: String| ModuleVerificationError::ModuleInvariant { detail };

    validate_metadata_table_lengths(
        module.struct_metas.len(),
        module.interface_metas.len(),
        module.named_type_metas.len(),
        module.runtime_types.len(),
    )?;

    let mut expected_dynamic_callsite_index = 0usize;
    for (func_id, function) in module.functions.iter().enumerate() {
        for (pc, instruction) in function.code.iter().enumerate() {
            if !matches!(
                instruction.opcode(),
                Opcode::CallClosure | Opcode::CallIface
            ) {
                continue;
            }
            let actual = instruction.dynamic_callsite_index() as usize;
            if actual != expected_dynamic_callsite_index {
                return Err(invariant(format!(
                    "dynamic callsite in function {func_id} at pc {pc} has index {actual}, expected {expected_dynamic_callsite_index}"
                )));
            }
            expected_dynamic_callsite_index += 1;
        }
    }

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
        crate::extern_key::classify_extern_name(&extern_def.name).map_err(|error| {
            invariant(format!(
                "externs[{idx}] ({}) has an invalid extern identity: {error}",
                extern_def.name
            ))
        })?;
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
        if idx == 0 && itab.iface_meta_id != 0 {
            return Err(invariant(format!(
                "itab 0 is reserved for empty-interface metadata 0, got iface_meta_id={}",
                itab.iface_meta_id
            )));
        }
        if idx != 0 {
            if itab.iface_meta_id == 0 {
                return Err(invariant(format!(
                    "itab {idx} targets the canonical empty interface; empty-interface values must use itab 0"
                )));
            }
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
    let mut named_type_identities = BTreeSet::new();
    for (idx, named) in module.named_type_metas.iter().enumerate() {
        validate_named_type_identity(&named.name, &format!("named_type_metas[{idx}] name"))?;
        if !named_type_identities.insert(named.name.as_str()) {
            return Err(invariant(format!(
                "named_type_metas[{idx}] duplicates named type identity {:?}",
                named.name
            )));
        }
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
            validate_method_identity(name, &format!("named_type_metas[{idx}] method identity"))?;
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
    let runtime_type_facts = validate_runtime_type_refs(module)?;
    validate_global_metadata_refs(module)?;
    validate_well_known_types(module)?;
    validate_debug_info_refs(module)?;
    Ok(runtime_type_facts)
}

fn validate_metadata_table_lengths(
    struct_metas: usize,
    interface_metas: usize,
    named_type_metas: usize,
    runtime_types: usize,
) -> Result<(), ModuleVerificationError> {
    let max_len = INVALID_META_ID as usize;
    for (table, len) in [
        ("struct_metas", struct_metas),
        ("interface_metas", interface_metas),
        ("named_type_metas", named_type_metas),
        ("runtime_types", runtime_types),
    ] {
        if len > max_len {
            return Err(module_invariant(format!(
                "{table} length {len} exceeds 24-bit addressable table limit {max_len}; id 0x{INVALID_META_ID:06x} is reserved"
            )));
        }
    }
    Ok(())
}

fn module_invariant(detail: String) -> ModuleVerificationError {
    ModuleVerificationError::ModuleInvariant { detail }
}

fn validate_source_declaration_identifier(
    name: &str,
    label: &str,
    allow_blank: bool,
) -> Result<(), ModuleVerificationError> {
    if (allow_blank && name == "_") || crate::identifier::is_named_declaration_identifier(name) {
        return Ok(());
    }
    Err(module_invariant(format!(
        "{label} {name:?} is not a Unicode 16 Vo declaration identifier{}",
        if allow_blank {
            " or the blank identifier"
        } else {
            ""
        }
    )))
}

/// Validate the identity string stored for a named source type.
///
/// Production source types use `<canonical-package>.<declaration>` or the
/// compiler-owned local identity grammar. The exact raw spelling `error` is
/// reserved for the predeclared language type. Function and interface-meta
/// display names are intentionally outside this gate: those tables also carry
/// compiler-generated names such as wrappers and anonymous interfaces.
fn validate_named_type_identity(
    identity: &str,
    label: &str,
) -> Result<(), ModuleVerificationError> {
    if identity.len() > crate::identifier::MAX_NAMED_TYPE_IDENTITY_BYTES {
        return Err(module_invariant(format!(
            "{label} is {} bytes, exceeding the {}-byte named type identity limit",
            identity.len(),
            crate::identifier::MAX_NAMED_TYPE_IDENTITY_BYTES
        )));
    }
    if identity == "error" {
        return Ok(());
    }
    if identity.contains(crate::identifier::LOCAL_TYPE_IDENTITY_MARKER) {
        if crate::identifier::is_local_type_identity(identity) {
            return Ok(());
        }
        return Err(module_invariant(format!(
            "{label} {identity:?} is not a canonical compiler-generated local type identity"
        )));
    }
    let Some((package, name)) = identity.rsplit_once('.') else {
        return Err(module_invariant(format!(
            "{label} {identity:?} is not a named type identity"
        )));
    };
    validate_source_declaration_identifier(name, label, false)?;
    crate::extern_key::validate_canonical_package_path(package).map_err(|error| {
        module_invariant(format!(
            "{label} {identity:?} has a non-canonical package path: {error}"
        ))
    })
}

/// Method sets store exported declarations by their raw name and private
/// declarations by `<canonical-package>.<name>` so package-private methods do
/// not alias across packages.
fn validate_method_identity(identity: &str, label: &str) -> Result<(), ModuleVerificationError> {
    if crate::identifier::is_named_declaration_identifier(identity) {
        if crate::identifier::is_exported_name(identity) {
            return Ok(());
        }
        return Err(module_invariant(format!(
            "{label} {identity:?} is private and must include its canonical package path"
        )));
    }
    let Some((package, name)) = identity.rsplit_once('.') else {
        return Err(module_invariant(format!(
            "{label} {identity:?} is not a method identity"
        )));
    };
    validate_source_declaration_identifier(name, label, false)?;
    if crate::identifier::is_exported_name(name) {
        return Err(module_invariant(format!(
            "{label} {identity:?} qualifies an exported method"
        )));
    }
    crate::extern_key::validate_canonical_package_path(package).map_err(|error| {
        module_invariant(format!(
            "{label} {identity:?} has a non-canonical package path: {error}"
        ))
    })
}

fn validate_same_name_extern_abi_shapes(module: &Module) -> Result<(), String> {
    let mut first_by_name = BTreeMap::<&str, (usize, &ExternDef)>::new();
    for (idx, current) in module.externs.iter().enumerate() {
        if is_vm_owned_variable_shape_extern(&current.name) {
            continue;
        }
        if let Some((prev_idx, previous)) = first_by_name.get(current.name.as_str()).copied() {
            if previous.params != current.params
                || previous.returns != current.returns
                || previous.param_kinds != current.param_kinds
                || previous.allowed_effects != current.allowed_effects
            {
                return Err(format!(
                    "same-name extern {} has incompatible ABI contracts between externs[{prev_idx}] and externs[{idx}]",
                    current.name
                ));
            }
        } else {
            first_by_name.insert(current.name.as_str(), (idx, current));
        }
    }
    Ok(())
}

fn is_vm_owned_variable_shape_extern(name: &str) -> bool {
    crate::extern_key::is_vm_variable_shape_extern_name(name)
}

fn dynamic_call_extern_param_prefix(name: &str) -> Option<&'static [SlotType]> {
    match name {
        "dyn_call" => Some(&[
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::GcBase,
            SlotType::Value,
        ]),
        "dyn_method" => Some(&[
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::GcBase,
            SlotType::GcBase,
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
    let expected_receiver = if method.is_pointer_receiver {
        if method.receiver_is_iface_boxed {
            return Err(module_invariant(format!(
                "named_type_metas[{named_idx}] method {method_name} cannot mark pointer receiver as interface-boxed"
            )));
        }
        vec![SlotType::GcRef]
    } else if method.receiver_is_iface_boxed {
        if !named.underlying_meta.value_kind().needs_boxing() {
            return Err(module_invariant(format!(
                "named_type_metas[{named_idx}] method {method_name} marks non-boxed {:?} receiver as interface-boxed",
                named.underlying_meta.value_kind()
            )));
        }
        vec![SlotType::GcBase]
    } else {
        module
            .slot_layout_for_value_rttid(named.underlying_rttid)
            .ok_or_else(|| {
                module_invariant(format!(
                    "named_type_metas[{named_idx}] method {method_name} receiver layout cannot be resolved"
                ))
            })?
    };
    let expected_recv_slots = u16::try_from(expected_receiver.len()).map_err(|_| {
        module_invariant(format!(
            "named_type_metas[{named_idx}] method {method_name} receiver layout exceeds u16"
        ))
    })?;
    let func = &module.functions[method.func_id as usize];
    let actual_receiver = func
        .slot_types
        .get(..usize::from(func.recv_slots))
        .unwrap_or(&[]);
    if func.recv_slots != expected_recv_slots || actual_receiver != expected_receiver {
        return Err(module_invariant(format!(
            "named_type_metas[{named_idx}] method {method_name} receiver target {} ({}) must have layout {:?}, got recv_slots={} layout={:?}",
            method.func_id,
            func.name,
            expected_receiver,
            func.recv_slots,
            actual_receiver
        )));
    }
    if func.param_slots < func.recv_slots {
        return Err(module_invariant(format!(
            "named_type_metas[{named_idx}] method {method_name} target {} ({}) has param_slots={} below recv_slots={}",
            method.func_id, func.name, func.param_slots, func.recv_slots
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
                return validate_call_iface_itab_target_function_layout(
                    module,
                    itab_idx,
                    method_idx,
                    func_id,
                    func_name,
                    iface_method,
                );
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

fn validate_call_iface_itab_target_function_layout(
    module: &Module,
    itab_idx: usize,
    method_idx: usize,
    func_id: u32,
    func_name: &str,
    iface_method: &crate::bytecode::InterfaceMethodMeta,
) -> Result<(), ModuleVerificationError> {
    let func = &module.functions[func_id as usize];
    let (expected_args, expected_returns) =
        function_signature_slot_layouts(module, iface_method.signature_rttid).map_err(|detail| {
            module_invariant(format!(
                "CallIface itab {itab_idx} method {method_idx} interface signature cannot be resolved: {detail}"
            ))
        })?;
    let arg_start = func.recv_slots as usize;
    let arg_end = func.param_slots as usize;
    let Some(actual_args) = func.slot_types.get(arg_start..arg_end) else {
        return Err(module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} target function {func_id} ({func_name}) has invalid parameter slot range {arg_start}..{arg_end}"
        )));
    };
    if actual_args != expected_args {
        return Err(module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} target function {func_id} ({func_name}) non-receiver parameter layout {actual_args:?} does not match interface method {} signature layout {expected_args:?}",
            iface_method.name
        )));
    }
    if func.ret_slot_types != expected_returns {
        return Err(module_invariant(format!(
            "CallIface itab {itab_idx} method {method_idx} target function {func_id} ({func_name}) return layout {:?} does not match interface method {} signature layout {expected_returns:?}",
            func.ret_slot_types, iface_method.name
        )));
    }
    Ok(())
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
    if value_meta.meta_id() >= INVALID_META_ID {
        return Err(module_invariant(format!(
            "{label} metadata id {} uses reserved id 0x{INVALID_META_ID:06x}",
            value_meta.meta_id()
        )));
    }
    let meta_id = value_meta.meta_id() as usize;
    match kind {
        ValueKind::Struct if meta_id >= module.struct_metas.len() => {
            Err(module_invariant(format!(
                "{label} references missing struct metadata {}",
                value_meta.meta_id()
            )))
        }
        ValueKind::Pointer if meta_id >= module.struct_metas.len() => {
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
        ValueKind::Struct | ValueKind::Pointer | ValueKind::Interface => Ok(kind),
        _ if meta_id != 0 => Err(module_invariant(format!(
            "{label} has non-canonical metadata id {}; {kind:?} values require metadata id 0",
            value_meta.meta_id()
        ))),
        _ => Ok(kind),
    }
}

fn validate_value_rttid_ref(
    module: &Module,
    value_rttid: ValueRttid,
    label: &str,
) -> Result<ValueKind, ModuleVerificationError> {
    let kind = validate_value_kind_tag(value_rttid.to_raw() as u8, label)?;
    if value_rttid.rttid() >= INVALID_META_ID {
        return Err(module_invariant(format!(
            "{label} runtime type id {} uses reserved id 0x{INVALID_META_ID:06x}",
            value_rttid.rttid()
        )));
    }
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
            validate_value_kind_tag(
                named.underlying_rttid.to_raw() as u8,
                &format!("{label} named_type_metas[{id}] underlying_rttid"),
            )
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

/// Expand the ABI-visible parameters and results of a function signature.
///
/// Interface method signatures never include the receiver. `CallIface` carries
/// that receiver separately in its interface pair and hidden prefix slot, so
/// these argument slots correspond exactly to `CallIfaceLayout::arg_layout`.
fn function_signature_slot_layouts(
    module: &Module,
    signature_rttid: u32,
) -> Result<(Vec<SlotType>, Vec<SlotType>), String> {
    let Some(RuntimeType::Func {
        params, results, ..
    }) = module.runtime_types.get(signature_rttid as usize)
    else {
        return Err(format!(
            "signature_rttid {signature_rttid} does not reference a function runtime type"
        ));
    };
    Ok((
        flatten_signature_value_layouts(module, signature_rttid, "parameter", params)?,
        flatten_signature_value_layouts(module, signature_rttid, "result", results)?,
    ))
}

fn flatten_signature_value_layouts(
    module: &Module,
    signature_rttid: u32,
    value_role: &'static str,
    values: &[ValueRttid],
) -> Result<Vec<SlotType>, String> {
    let mut flattened = Vec::new();
    for (idx, value_rttid) in values.iter().copied().enumerate() {
        let Some(value_layout) = module.slot_layout_for_value_rttid(value_rttid) else {
            return Err(format!(
                "signature_rttid {signature_rttid} {value_role} {idx} runtime type {} has an invalid, cyclic, or over-wide slot layout",
                value_rttid.rttid()
            ));
        };
        let Some(total) = flattened.len().checked_add(value_layout.len()) else {
            return Err(format!(
                "signature_rttid {signature_rttid} {value_role} layout slot count overflows usize"
            ));
        };
        if total > u16::MAX as usize {
            return Err(format!(
                "signature_rttid {signature_rttid} {value_role} layout has {total} slots, exceeding u16::MAX"
            ));
        }
        flattened.extend_from_slice(&value_layout);
    }
    Ok(flattened)
}

fn validate_struct_metadata_refs(module: &Module) -> Result<(), ModuleVerificationError> {
    for (idx, meta) in module.struct_metas.iter().enumerate() {
        let mut selectable_names = BTreeSet::new();
        let mut expected_offset = 0usize;
        for (field_idx, field) in meta.fields.iter().enumerate() {
            validate_source_declaration_identifier(
                &field.name,
                &format!("struct_metas[{idx}] field {field_idx} name"),
                true,
            )?;
            validate_value_rttid_ref(
                module,
                field.type_info,
                &format!("struct_metas[{idx}] field {field_idx} type_info"),
            )?;
            let field_label = format!("struct_metas[{idx}] field {field_idx} ({})", field.name);
            let expected_layout = module
                .slot_layout_for_value_rttid(field.type_info)
                .ok_or_else(|| {
                    module_invariant(format!(
                        "{field_label} type_info has an invalid, cyclic, or over-wide canonical slot layout"
                    ))
                })?;
            if usize::from(field.slot_count) != expected_layout.len() {
                return Err(module_invariant(format!(
                    "{field_label} slot_count={} does not match canonical type layout width {}",
                    field.slot_count,
                    expected_layout.len()
                )));
            }
            if usize::from(field.offset) != expected_offset {
                return Err(module_invariant(format!(
                    "{field_label} offset={} does not match canonical contiguous offset {expected_offset}; overlapping fields and layout gaps are invalid",
                    field.offset
                )));
            }
            let field_end = expected_offset
                .checked_add(expected_layout.len())
                .ok_or_else(|| {
                    module_invariant(format!(
                        "{field_label} canonical slot range overflows usize"
                    ))
                })?;
            let actual_layout = meta
                .slot_types
                .get(expected_offset..field_end)
                .ok_or_else(|| {
                    module_invariant(format!(
                        "{field_label} canonical slot range {expected_offset}..{field_end} exceeds struct layout width {}",
                        meta.slot_types.len()
                    ))
                })?;
            if actual_layout != expected_layout {
                return Err(module_invariant(format!(
                    "{field_label} physical slot layout {actual_layout:?} does not match canonical type layout {expected_layout:?}"
                )));
            }
            expected_offset = field_end;
            if field.name != "_" {
                if !selectable_names.insert(field.name.as_str()) {
                    return Err(module_invariant(format!(
                        "struct_metas[{idx}] has duplicate selectable field name {}",
                        field.name
                    )));
                }
                if meta.field_index.get(&field.name) != Some(&field_idx) {
                    return Err(module_invariant(format!(
                        "struct_metas[{idx}] field {field_idx} ({}) is missing its canonical field_index entry",
                        field.name
                    )));
                }
            }
        }
        let has_zero_size_workaround = expected_offset == 0
            && meta.slot_types.as_slice() == [SlotType::Value]
            && !meta.fields.is_empty();
        if !meta.fields.is_empty()
            && expected_offset != meta.slot_types.len()
            && !has_zero_size_workaround
        {
            return Err(module_invariant(format!(
                "struct_metas[{idx}] fields cover {expected_offset} slots but physical layout has {} slots; trailing layout gaps are invalid",
                meta.slot_types.len()
            )));
        }
        for (name, &field_idx) in &meta.field_index {
            if name == "_" {
                return Err(module_invariant(format!(
                    "struct_metas[{idx}] field_index must not expose the blank identifier"
                )));
            }
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
        if idx == 0 && (!meta.method_names.is_empty() || !meta.methods.is_empty()) {
            return Err(module_invariant(
                "interface_metas[0] is reserved for the canonical empty interface".to_string(),
            ));
        }
        if meta.method_names.len() != meta.methods.len() {
            return Err(module_invariant(format!(
                "interface_metas[{idx}] method_names.len()={} but methods.len()={}",
                meta.method_names.len(),
                meta.methods.len()
            )));
        }
        if idx != 0 && meta.methods.is_empty() {
            return Err(module_invariant(format!(
                "interface_metas[{idx}] duplicates the canonical empty interface at index 0"
            )));
        }
        let mut canonical_method_identities = BTreeSet::new();
        for (method_idx, method) in meta.methods.iter().enumerate() {
            validate_method_identity(
                &method.name,
                &format!("interface_metas[{idx}] method {method_idx} identity"),
            )?;
            if meta.method_names.get(method_idx) != Some(&method.name) {
                return Err(module_invariant(format!(
                    "interface_metas[{idx}] method {method_idx} name {} does not match method_names",
                    method.name
                )));
            }
            if !canonical_method_identities.insert(method.name.as_str()) {
                return Err(module_invariant(format!(
                    "interface_metas[{idx}] contains duplicate method {}",
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

fn validate_runtime_type_refs(
    module: &Module,
) -> Result<RuntimeTypeFacts, ModuleVerificationError> {
    for (idx, runtime_type) in module.runtime_types.iter().enumerate() {
        let label = format!("runtime_types[{idx}]");
        match runtime_type {
            RuntimeType::Basic(kind) => {
                if !ValueKind::BASIC.contains(kind) {
                    return Err(module_invariant(format!(
                        "{label} RuntimeType::Basic contains non-basic ValueKind {kind:?}"
                    )));
                }
            }
            RuntimeType::Island => {}
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
                let Some(struct_meta) = module.struct_metas.get(*meta_id as usize) else {
                    return Err(module_invariant(format!(
                        "{label} Struct references missing struct metadata {meta_id}"
                    )));
                };
                if fields.len() != struct_meta.fields.len() {
                    return Err(module_invariant(format!(
                        "{label} Struct has {} identity fields but struct_metas[{meta_id}] has {} physical fields",
                        fields.len(),
                        struct_meta.fields.len()
                    )));
                }
                for (field_idx, field) in fields.iter().enumerate() {
                    let field_label = format!("{label} identity field {field_idx}");
                    if field.pkg.is_empty() {
                        if !crate::identifier::is_exported_name(&field.name) {
                            return Err(module_invariant(format!(
                                "{field_label} ({:?}) is private and must include its canonical package path",
                                field.name
                            )));
                        }
                    } else {
                        crate::extern_key::validate_canonical_package_path(&field.pkg).map_err(
                            |error| {
                                module_invariant(format!(
                                    "{field_label} ({:?}) has a non-canonical package path {:?}: {error}",
                                    field.name, field.pkg
                                ))
                            },
                        )?;
                        if crate::identifier::is_exported_name(&field.name) {
                            return Err(module_invariant(format!(
                                "{field_label} ({:?}) is exported and must have an empty package identity",
                                field.name
                            )));
                        }
                    }
                    validate_value_rttid_ref(
                        module,
                        field.typ,
                        &format!("{label} field {field_idx} type"),
                    )?;
                    let physical = &struct_meta.fields[field_idx];
                    if field.name != physical.name
                        || field.typ != physical.type_info
                        || field.embedded != physical.embedded
                        || field.tag != physical.tag.as_deref().unwrap_or("")
                    {
                        return Err(module_invariant(format!(
                            "{label} identity field {field_idx} ({}, {:?}, embedded={}, tag={:?}) does not match struct_metas[{meta_id}] physical field ({}, {:?}, embedded={}, tag={:?})",
                            field.name,
                            field.typ,
                            field.embedded,
                            field.tag,
                            physical.name,
                            physical.type_info,
                            physical.embedded,
                            physical.tag.as_deref().unwrap_or("")
                        )));
                    }
                }
            }
            RuntimeType::Interface { methods, meta_id } => {
                let Some(interface_meta) = module.interface_metas.get(*meta_id as usize) else {
                    return Err(module_invariant(format!(
                        "{label} Interface references missing interface metadata {meta_id}"
                    )));
                };
                if methods.len() != interface_meta.methods.len() {
                    return Err(module_invariant(format!(
                        "{label} Interface has {} identity methods but interface_metas[{meta_id}] has {} dispatch methods",
                        methods.len(),
                        interface_meta.methods.len()
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
                    if !interface_meta.methods.iter().any(|physical| {
                        physical.name == method.name
                            && physical.signature_rttid == method.sig.rttid()
                    }) {
                        return Err(module_invariant(format!(
                            "{label} identity method {} with signature {} is absent from interface_metas[{meta_id}]",
                            method.name,
                            method.sig.rttid()
                        )));
                    }
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
    validate_runtime_value_containment_graph(module)
}

/// Validate the graph of types that are stored inline in another value.
///
/// Pointer, slice, map, channel, port, closure, interface, string, and island
/// edges terminate because their payload is represented by one or two slots.
/// Named wrappers, arrays, tuples, and struct fields retain their child value
/// inline, so a cycle there would make packing/comparison/transfer non-
/// terminating even when a forged `StructMeta` supplied a finite slot vector.
fn validate_runtime_value_containment_graph(
    module: &Module,
) -> Result<RuntimeTypeFacts, ModuleVerificationError> {
    let facts = RuntimeTypeFacts::from_module_parts(
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
    )
    .map_err(|error| module_invariant(error.to_string()))?;

    for (id, runtime_type) in module.runtime_types.iter().enumerate() {
        let RuntimeType::Func {
            params,
            results,
            variadic,
        } = runtime_type
        else {
            continue;
        };
        for (role, values) in [("parameter", params), ("result", results)] {
            let total = values.iter().try_fold(0usize, |total, value| {
                let value_width = facts
                    .get(*value)
                    .ok_or_else(|| {
                        module_invariant(format!(
                            "runtime_types[{id}] function {role} width was not resolved"
                        ))
                    })?
                    .bounded_slot_count();
                Ok::<_, ModuleVerificationError>(
                    total.saturating_add(value_width).min(u16::MAX as usize + 1),
                )
            })?;
            if total > u16::MAX as usize {
                return Err(module_invariant(format!(
                    "runtime_types[{id}] function {role} layout width {total} exceeds the u16 slot domain"
                )));
            }
        }
        if *variadic {
            let Some(last) = params.last() else {
                return Err(module_invariant(format!(
                    "runtime_types[{id}] variadic function has no final slice parameter"
                )));
            };
            let Some((_, RuntimeType::Slice(_))) =
                module.runtime_type_resolver().resolve_value_rttid(*last)
            else {
                return Err(module_invariant(format!(
                    "runtime_types[{id}] variadic function final parameter is not a slice"
                )));
            };
        }
    }

    Ok(facts)
}

fn validate_global_metadata_refs(module: &Module) -> Result<(), ModuleVerificationError> {
    for (idx, global) in module.globals.iter().enumerate() {
        validate_source_declaration_identifier(
            &global.name,
            &format!("globals[{idx}] name"),
            false,
        )?;
        let kind = validate_value_kind_tag(
            global.value_kind,
            &format!("globals[{idx}] ({}) value_kind", global.name),
        )?;
        let value_meta = ValueMeta::try_new(global.meta_id, kind).ok_or_else(|| {
            module_invariant(format!(
                "globals[{idx}] ({}) metadata id {} exceeds the 24-bit domain or uses reserved id 0x{INVALID_META_ID:06x}",
                global.name, global.meta_id
            ))
        })?;
        let label = format!("globals[{idx}] ({}) metadata", global.name);
        let canonical_layout = if matches!(kind, ValueKind::Array | ValueKind::Struct) {
            // Addressable aggregate globals own stable typed allocations. The
            // metadata identifies their logical value while the global frame
            // stores and scans exactly one canonical object reference.
            validate_value_meta_ref(module, value_meta, &label)?;
            vec![SlotType::GcBase]
        } else {
            value_meta_slot_layout(module, value_meta, &label)?
        };
        if usize::from(global.slots) != canonical_layout.len() {
            return Err(module_invariant(format!(
                "globals[{idx}] ({}) slots={} does not match canonical value layout width {}",
                global.name,
                global.slots,
                canonical_layout.len()
            )));
        }
        if global.slot_types != canonical_layout {
            return Err(module_invariant(format!(
                "globals[{idx}] ({}) slot_types {:?} do not match canonical value layout {:?}",
                global.name, global.slot_types, canonical_layout
            )));
        }
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
    validate_error_well_known_contract(module)?;
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

fn validate_error_well_known_contract(module: &Module) -> Result<(), ModuleVerificationError> {
    let well_known = &module.well_known;
    let present = [
        well_known.error_named_type_id.is_some(),
        well_known.error_iface_meta_id.is_some(),
        well_known.error_ptr_rttid.is_some(),
        well_known.error_struct_meta_id.is_some(),
        well_known.error_field_offsets.is_some(),
    ]
    .into_iter()
    .filter(|present| *present)
    .count();
    if present == 0 {
        return Ok(());
    }
    if present != 5 {
        return Err(module_invariant(format!(
            "well_known error metadata must be absent or complete; {present} of 5 fields are present"
        )));
    }

    let named_id = well_known
        .error_named_type_id
        .expect("complete error metadata has named id");
    let iface_id = well_known
        .error_iface_meta_id
        .expect("complete error metadata has interface id");
    let ptr_rttid = well_known
        .error_ptr_rttid
        .expect("complete error metadata has pointer RTTID");
    let struct_id = well_known
        .error_struct_meta_id
        .expect("complete error metadata has struct id");
    let offsets = well_known
        .error_field_offsets
        .expect("complete error metadata has field offsets");

    let named = &module.named_type_metas[named_id as usize];
    let expected_struct_meta =
        ValueMeta::try_new(struct_id, ValueKind::Struct).ok_or_else(|| {
            module_invariant(format!(
                "well_known.error_struct_meta_id {struct_id} is not representable"
            ))
        })?;
    if named.underlying_meta != expected_struct_meta {
        return Err(module_invariant(format!(
            "well_known.error_named_type_id {named_id} underlying metadata {:?} does not identify error struct metadata {struct_id}",
            named.underlying_meta
        )));
    }

    let RuntimeType::Pointer(pointer_elem) = &module.runtime_types[ptr_rttid as usize] else {
        unreachable!("error_ptr_rttid kind was checked above");
    };
    if module.named_type_id_for_rttid(pointer_elem.rttid()) != Some(named_id) {
        return Err(module_invariant(format!(
            "well_known.error_ptr_rttid {ptr_rttid} does not point to error named type {named_id}"
        )));
    }
    if module.canonical_value_meta_for_value_rttid(*pointer_elem) != Some(expected_struct_meta) {
        return Err(module_invariant(format!(
            "well_known.error_ptr_rttid {ptr_rttid} pointee does not resolve to error struct metadata {struct_id}"
        )));
    }

    let iface = &module.interface_metas[iface_id as usize];
    if iface.methods.is_empty() {
        return Err(module_invariant(
            "well_known.error_iface_meta_id must identify a non-empty error interface".to_string(),
        ));
    }
    for iface_method in &iface.methods {
        let Some(named_method) = named.methods.get(&iface_method.name) else {
            return Err(module_invariant(format!(
                "well_known error named type {named_id} does not implement interface method {}",
                iface_method.name
            )));
        };
        if named_method.signature_rttid != iface_method.signature_rttid {
            return Err(module_invariant(format!(
                "well_known error method {} signature {} does not match interface signature {}",
                iface_method.name, named_method.signature_rttid, iface_method.signature_rttid
            )));
        }
    }

    let struct_meta = &module.struct_metas[struct_id as usize];
    let msg_field = struct_meta.get_field("msg").ok_or_else(|| {
        module_invariant("well_known error struct is missing field msg".to_string())
    })?;
    let cause_field = struct_meta.get_field("cause").ok_or_else(|| {
        module_invariant("well_known error struct is missing field cause".to_string())
    })?;
    if offsets != [msg_field.offset, cause_field.offset] {
        return Err(module_invariant(format!(
            "well_known.error_field_offsets {offsets:?} do not match msg/cause field offsets [{}, {}]",
            msg_field.offset, cause_field.offset
        )));
    }
    let msg_layout = module
        .slot_layout_for_value_rttid(msg_field.type_info)
        .ok_or_else(|| {
            module_invariant("well_known error msg field has no canonical layout".to_string())
        })?;
    if msg_field.type_info.try_value_kind() != Some(ValueKind::String)
        || msg_layout != [SlotType::GcBase]
    {
        return Err(module_invariant(format!(
            "well_known error msg field must be string/GcBase, got kind {:?} layout {msg_layout:?}",
            msg_field.type_info.try_value_kind()
        )));
    }
    let cause_layout = module
        .slot_layout_for_value_rttid(cause_field.type_info)
        .ok_or_else(|| {
            module_invariant("well_known error cause field has no canonical layout".to_string())
        })?;
    let expected_cause_meta =
        ValueMeta::try_new(iface_id, ValueKind::Interface).ok_or_else(|| {
            module_invariant(format!(
                "well_known.error_iface_meta_id {iface_id} is not representable"
            ))
        })?;
    if module.canonical_value_meta_for_value_rttid(cause_field.type_info)
        != Some(expected_cause_meta)
        || cause_layout != [SlotType::Interface0, SlotType::Interface1]
    {
        return Err(module_invariant(format!(
            "well_known error cause field must use interface metadata {iface_id} with Interface0/Interface1 layout"
        )));
    }
    let cause_end = usize::from(cause_field.offset)
        .checked_add(cause_layout.len())
        .ok_or_else(|| {
            module_invariant("well_known error cause field range overflows usize".to_string())
        })?;
    if cause_end > struct_meta.slot_types.len() {
        return Err(module_invariant(format!(
            "well_known error cause field range {}..{cause_end} exceeds struct width {}",
            cause_field.offset,
            struct_meta.slot_types.len()
        )));
    }
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
    resources: &mut VerifierResources,
) -> Result<(), ModuleVerificationError> {
    let _ = idx;
    verify_function_common(func, module, resources)
}

fn verify_function_common(
    func: &FunctionDef,
    module: &Module,
    resources: &mut VerifierResources,
) -> Result<(), ModuleVerificationError> {
    verify_function_invariants(func, module)?;

    if func.code.len() != func.instruction_metadata.len() {
        return Err(ModuleVerificationError::LengthMismatch {
            func: func.name.clone(),
            code_len: func.code.len(),
            metadata_len: func.instruction_metadata.len(),
        });
    }
    for (pc, metadata) in func.instruction_metadata.iter().enumerate() {
        validate_instruction_metadata_shape(func, pc, metadata)?;
    }
    verify_select_case_structure(func)?;
    let cfg = FunctionCfg::build(func, resources)?;
    let dependencies = FactDependencyGraph::build(func, resources)?;
    let constant_facts =
        ConstantFactAnalysis::analyze(func, module, &cfg, &dependencies, resources)?;
    let index_check_facts = IndexCheckAnalysis::analyze(
        func,
        module,
        &cfg,
        &dependencies,
        &constant_facts,
        resources,
    )?;
    let container_layout_facts =
        ContainerLayoutAnalysis::analyze(func, module, &cfg, &dependencies, resources)?;
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
        validate_instruction_metadata_contract(
            func,
            pc,
            opcode,
            inst.flags,
            &func.instruction_metadata[pc],
        )?;
        verify_instruction_contract(func, analyses, pc, inst, opcode)?;
        validate_loop_end_contract(func, pc, &func.instruction_metadata[pc])?;
    }

    let Some(last) = func.code.last() else {
        return Err(ModuleVerificationError::FunctionInvariant {
            func: func.name.clone(),
            detail: "bytecode is empty and has no terminating control transfer".to_string(),
        });
    };
    let last_opcode = last.opcode();
    if !matches!(last_opcode, Opcode::Jump | Opcode::Return | Opcode::Panic) {
        return Err(ModuleVerificationError::FunctionInvariant {
            func: func.name.clone(),
            detail: format!(
                "final {last_opcode:?} instruction falls through beyond code length {}",
                func.code.len()
            ),
        });
    }

    Ok(())
}

struct PendingSelectCases {
    begin_pc: usize,
    expected: u16,
    seen: u16,
    has_default: bool,
    source_indices: Vec<u16>,
    layouts: Vec<SelectCaseLayout>,
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
                    layouts: Vec::new(),
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
                let elem_slots = match func.instruction_metadata.get(pc) {
                    Some(InstructionMetadata::QueueLayout { elem_layout }) => {
                        u16::try_from(elem_layout.len()).map_err(|_| {
                            invariant(format!(
                                "{opcode:?} at pc {pc} element layout exceeds the register domain"
                            ))
                        })?
                    }
                    _ => {
                        return Err(invariant(format!(
                            "{opcode:?} at pc {pc} is missing QueueLayout metadata"
                        )))
                    }
                };
                select.layouts.push(match opcode {
                    Opcode::SelectSend => SelectCaseLayout::Send {
                        queue: inst.a,
                        value: inst.b,
                        elem_slots,
                    },
                    Opcode::SelectRecv => SelectCaseLayout::Recv {
                        destination: inst.a,
                        queue: inst.b,
                        elem_slots,
                        has_ok: inst.recv_has_ok(),
                    },
                    _ => unreachable!("select case opcode was matched above"),
                });
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
                let Some(InstructionMetadata::SelectExecLayout { cases }) =
                    func.instruction_metadata.get(pc)
                else {
                    return Err(invariant(format!(
                        "SelectExec at pc {pc} is missing SelectExecLayout metadata"
                    )));
                };
                if cases != &select.layouts {
                    return Err(invariant(format!(
                        "SelectExec at pc {pc} layout does not match its declared cases"
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
    metadata: &InstructionMetadata,
) -> Result<(), ModuleVerificationError> {
    match metadata {
        InstructionMetadata::None | InstructionMetadata::LoopEnd { .. } => Ok(()),
        InstructionMetadata::ElemLayout {
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
        InstructionMetadata::MapNew { .. }
        | InstructionMetadata::MapGet { .. }
        | InstructionMetadata::MapSet { .. }
        | InstructionMetadata::MapDelete { .. }
        | InstructionMetadata::PtrLayout { .. }
        | InstructionMetadata::SlotLayout { .. }
        | InstructionMetadata::CallLayout { .. }
        | InstructionMetadata::CallIfaceLayout { .. }
        | InstructionMetadata::CallExternLayout { .. }
        | InstructionMetadata::QueueLayout { .. }
        | InstructionMetadata::SelectExecLayout { .. }
        | InstructionMetadata::MapIterNext { .. }
        | InstructionMetadata::IfaceAssertLayout { .. } => {
            // Metadata kind and interface-pair semantics are enforced by
            // opcode-specific VM/JIT contracts. The common shape check only
            // rejects self-inconsistent byte/slot metadata that every backend
            // would misinterpret.
            Ok(())
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum InstructionMetadataKind {
    None,
    ElemLayout,
    MapNew,
    MapGet,
    MapSet,
    MapDelete,
    PtrLayout,
    SlotLayout,
    CallLayout,
    CallIfaceLayout,
    CallExternLayout,
    QueueLayout,
    SelectExecLayout,
    MapIterNext,
    IfaceAssertLayout,
    LoopEnd,
}

impl InstructionMetadataKind {
    fn of(metadata: &InstructionMetadata) -> Self {
        match metadata {
            InstructionMetadata::None => Self::None,
            InstructionMetadata::ElemLayout { .. } => Self::ElemLayout,
            InstructionMetadata::MapNew { .. } => Self::MapNew,
            InstructionMetadata::MapGet { .. } => Self::MapGet,
            InstructionMetadata::MapSet { .. } => Self::MapSet,
            InstructionMetadata::MapDelete { .. } => Self::MapDelete,
            InstructionMetadata::PtrLayout { .. } => Self::PtrLayout,
            InstructionMetadata::SlotLayout { .. } => Self::SlotLayout,
            InstructionMetadata::CallLayout { .. } => Self::CallLayout,
            InstructionMetadata::CallIfaceLayout { .. } => Self::CallIfaceLayout,
            InstructionMetadata::CallExternLayout { .. } => Self::CallExternLayout,
            InstructionMetadata::QueueLayout { .. } => Self::QueueLayout,
            InstructionMetadata::SelectExecLayout { .. } => Self::SelectExecLayout,
            InstructionMetadata::MapIterNext { .. } => Self::MapIterNext,
            InstructionMetadata::IfaceAssertLayout { .. } => Self::IfaceAssertLayout,
            InstructionMetadata::LoopEnd { .. } => Self::LoopEnd,
        }
    }

    fn name(self) -> &'static str {
        match self {
            Self::None => "None",
            Self::ElemLayout => "ElemLayout",
            Self::MapNew => "MapNew",
            Self::MapGet => "MapGet",
            Self::MapSet => "MapSet",
            Self::MapDelete => "MapDelete",
            Self::PtrLayout => "PtrLayout",
            Self::SlotLayout => "SlotLayout",
            Self::CallLayout => "CallLayout",
            Self::CallIfaceLayout => "CallIfaceLayout",
            Self::CallExternLayout => "CallExternLayout",
            Self::QueueLayout => "QueueLayout",
            Self::SelectExecLayout => "SelectExecLayout",
            Self::MapIterNext => "MapIterNext",
            Self::IfaceAssertLayout => "IfaceAssertLayout",
            Self::LoopEnd => "LoopEnd",
        }
    }
}

fn required_instruction_metadata_kind(opcode: Opcode, flags: u8) -> InstructionMetadataKind {
    match opcode {
        Opcode::Hint if flags == HINT_LOOP => InstructionMetadataKind::LoopEnd,
        Opcode::SlotGet | Opcode::SlotSet | Opcode::SlotGetN | Opcode::SlotSetN => {
            InstructionMetadataKind::SlotLayout
        }
        Opcode::PtrNew | Opcode::PtrGet | Opcode::PtrSet | Opcode::PtrGetN | Opcode::PtrSetN => {
            InstructionMetadataKind::PtrLayout
        }
        Opcode::CallExtern => InstructionMetadataKind::CallExternLayout,
        Opcode::CallClosure | Opcode::GoIsland => InstructionMetadataKind::CallLayout,
        Opcode::CallIface => InstructionMetadataKind::CallIfaceLayout,
        Opcode::ArrayNew
        | Opcode::ArrayGet
        | Opcode::ArraySet
        | Opcode::ArrayAddr
        | Opcode::SliceNew
        | Opcode::SliceGet
        | Opcode::SliceSet
        | Opcode::SliceAppend
        | Opcode::SliceAddr => InstructionMetadataKind::ElemLayout,
        Opcode::MapNew => InstructionMetadataKind::MapNew,
        Opcode::MapGet => InstructionMetadataKind::MapGet,
        Opcode::MapSet => InstructionMetadataKind::MapSet,
        Opcode::MapDelete => InstructionMetadataKind::MapDelete,
        Opcode::MapIterNext => InstructionMetadataKind::MapIterNext,
        Opcode::QueueNew
        | Opcode::QueueSend
        | Opcode::QueueRecv
        | Opcode::SelectSend
        | Opcode::SelectRecv => InstructionMetadataKind::QueueLayout,
        Opcode::SelectExec => InstructionMetadataKind::SelectExecLayout,
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush if flags & 1 != 0 => {
            InstructionMetadataKind::CallLayout
        }
        Opcode::IfaceAssert => InstructionMetadataKind::IfaceAssertLayout,
        _ => InstructionMetadataKind::None,
    }
}

fn validate_instruction_metadata_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    flags: u8,
    metadata: &InstructionMetadata,
) -> Result<(), ModuleVerificationError> {
    let required = required_instruction_metadata_kind(opcode, flags);
    let actual = InstructionMetadataKind::of(metadata);
    if actual == InstructionMetadataKind::None && required != InstructionMetadataKind::None {
        return Err(missing_layout(func, pc, opcode, required.name()));
    }
    if actual != required {
        return Err(ModuleVerificationError::WrongMetadataKind {
            func: func.name.clone(),
            pc,
            opcode,
            metadata: actual.name(),
        });
    }
    Ok(())
}

fn validate_loop_end_contract(
    func: &FunctionDef,
    pc: usize,
    metadata: &InstructionMetadata,
) -> Result<(), ModuleVerificationError> {
    let InstructionMetadata::LoopEnd { end_pc } = metadata else {
        return Ok(());
    };
    let begin_pc = pc + 1;
    let end_pc = *end_pc as usize;
    if begin_pc >= func.code.len() || end_pc >= func.code.len() || begin_pc > end_pc {
        return Err(ModuleVerificationError::InvalidLoopEnd {
            func: func.name.clone(),
            pc,
            begin_pc,
            end_pc,
            code_len: func.code.len(),
        });
    }
    let end = func.code[end_pc];
    let targets_begin = match end.opcode() {
        Opcode::Jump => {
            let target = end_pc as i64 + i64::from(end.imm32());
            target == begin_pc as i64
        }
        Opcode::ForLoop => end_pc as i64 + 1 + i64::from(end.c as i16) == begin_pc as i64,
        _ => false,
    };
    if !targets_begin {
        return Err(ModuleVerificationError::InvalidLoopEndBackEdge {
            func: func.name.clone(),
            pc,
            begin_pc,
            end_pc,
        });
    }
    Ok(())
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
            if func.slot_types[slot as usize] != SlotType::GcBase {
                return Err(invariant(format!(
                    "heap return slot {slot} must be GcBase, got {:?}",
                    func.slot_types[slot as usize]
                )));
            }
        }
    }
    if func.error_ret_slot < -1 {
        return Err(invariant(format!(
            "error_ret_slot={} uses an invalid negative sentinel; expected -1",
            func.error_ret_slot
        )));
    }
    if func.error_ret_slot >= 0 {
        let error_ret_slot = u16::try_from(func.error_ret_slot).map_err(|_| {
            invariant(format!(
                "error_ret_slot={} exceeds the u16 slot-address domain",
                func.error_ret_slot
            ))
        })?;
        if error_ret_slot.checked_add(2) != Some(func.ret_slots) {
            return Err(invariant(format!(
                "error_ret_slot={} must be the final two return slots of ret_slots={}",
                func.error_ret_slot, func.ret_slots
            )));
        }
        let error_ret_index = usize::from(error_ret_slot);
        if func.ret_slot_types[error_ret_index] != SlotType::Interface0
            || func.ret_slot_types[error_ret_index + 1] != SlotType::Interface1
        {
            return Err(invariant(format!(
                "error_ret_slot={} must have Interface0/Interface1 layout, got {:?}/{:?}",
                func.error_ret_slot,
                func.ret_slot_types[error_ret_index],
                func.ret_slot_types[error_ret_index + 1]
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
            if last_start != error_ret_slot || last_width != 2 {
                return Err(invariant(format!(
                    "heap error return partition must start at error_ret_slot={} with width 2, got start={} width={}",
                    func.error_ret_slot, last_start, last_width
                )));
            }
        }
    }
    if func.is_closure
        && (func.param_slots == 0 || func.slot_types.first() != Some(&SlotType::GcBase))
    {
        return Err(invariant(
            "closure functions must reserve GcBase slot 0".to_string(),
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
    if func.capture_slot_types.len() > MAX_CLOSURE_CAPTURE_SLOTS
        || func.capture_types.len() > MAX_CLOSURE_CAPTURE_SLOTS
    {
        return Err(invariant(format!(
            "closure capture metadata exceeds maximum {MAX_CLOSURE_CAPTURE_SLOTS}: capture_slot_types={} capture_types={}",
            func.capture_slot_types.len(),
            func.capture_types.len()
        )));
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
    if total_global_slots > u16::MAX as usize {
        return Err(ModuleVerificationError::GcLayout {
            detail: format!(
                "global slot count {total_global_slots} exceeds the u16 bytecode address domain"
            ),
        });
    }

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
        Opcode::SlotGet | Opcode::SlotGetN => {
            verify_slot_get_contract(ctx, index_check_facts, inst.a, inst.b, inst.c)
        }
        Opcode::SlotSet | Opcode::SlotSetN => {
            verify_slot_set_contract(ctx, index_check_facts, inst.a, inst.b, inst.c)
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
        Opcode::PtrNew => verify_ptr_new_contract(func, module, constant_facts, pc, opcode, inst),
        Opcode::PtrGet | Opcode::PtrGetN => {
            verify_ptr_get_contract(func, pc, opcode, inst.a, inst.b, inst.flags)
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
            verify_one_of_single_slot_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcBase, SlotType::GcRef],
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
        | Opcode::GeU => verify_binary_slot_contract(
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
        Opcode::Shl | Opcode::ShrS | Opcode::ShrU => {
            verify_shift_flags(ctx)?;
            verify_binary_slot_contract(
                ctx,
                BinarySlotContract::exact(
                    SlotType::Value,
                    SlotType::Value,
                    SlotType::Value,
                    scalar_destination_access(opcode),
                    "scalar lhs",
                    "scalar rhs",
                ),
            )
        }
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
                &[SlotType::GcBase],
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
                &[SlotType::GcBase],
                "StrConcat destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcBase],
                "StrConcat lhs",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.c,
                &[SlotType::GcBase],
                "StrConcat rhs",
            )
        }
        Opcode::StrSlice => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::GcBase],
                "StrSlice destination",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::GcBase],
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
                &[SlotType::GcBase],
                "string compare lhs",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.c,
                &[SlotType::GcBase],
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
                &[SlotType::GcBase],
                "Slice len/cap source",
            )
        }
        Opcode::SliceSlice => verify_slice_slice_contract(func, pc, opcode, inst),
        Opcode::SliceAppend => {
            verify_slice_append_contract(func, module, constant_facts, pc, opcode, inst)
        }
        Opcode::MapNew => verify_map_new_contract(func, module, constant_facts, pc, opcode, inst),
        Opcode::MapGet => verify_map_get_contract(func, container_layout_facts, pc, opcode, inst),
        Opcode::MapSet => verify_map_set_contract(func, container_layout_facts, pc, opcode, inst),
        Opcode::MapDelete => {
            verify_map_delete_contract(func, container_layout_facts, pc, opcode, inst)
        }
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
            verify_layout(func, pc, opcode, inst.b, &[SlotType::GcBase], "MapLen map")
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
                &[SlotType::GcBase],
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
            verify_layout(func, pc, opcode, queue_slot, &[SlotType::GcBase], "queue")
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
        Opcode::ConvI2F => {
            verify_conversion_flags(ctx)?;
            verify_unary_one_of_slot_contract(
                ctx,
                UnarySlotContract::one_of(
                    FLOAT_STORAGE_SLOTS,
                    &[SlotType::Value],
                    "ConvI2F destination",
                    "ConvI2F source",
                ),
            )
        }
        Opcode::ConvF2I => {
            verify_conversion_flags(ctx)?;
            verify_unary_one_of_slot_contract(
                ctx,
                UnarySlotContract::one_of(
                    &[SlotType::Value],
                    FLOAT_STORAGE_SLOTS,
                    "ConvF2I destination",
                    "ConvF2I source",
                ),
            )
        }
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
            &[SlotType::GcBase],
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
            if inst.a != 0 {
                return Err(call_shape_mismatch(
                    func,
                    pc,
                    Opcode::Hint,
                    format!("Hint loop reserved operand a must be zero, got {}", inst.a),
                ));
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
    fn analyze(
        func: &FunctionDef,
        module: &Module,
        cfg: &FunctionCfg,
        dependencies: &FactDependencyGraph,
        resources: &mut VerifierResources,
    ) -> Result<Self, ModuleVerificationError> {
        let slots = tracked_constant_slots(func, dependencies, resources)?;
        if slots.is_empty() || func.code.is_empty() {
            return Ok(Self {
                slots,
                before: try_none_vec(func, func.code.len(), "constant fact states")?,
            });
        }

        resources.charge_bytes::<ConstantFact>(
            func,
            func.code.len().saturating_mul(slots.len()),
            "constant fact matrix",
        )?;
        let mut before = try_none_vec(func, func.code.len(), "constant fact states")?;
        before[0] = Some(try_filled_vec(
            func,
            slots.len(),
            ConstantFact::Unknown,
            "constant entry facts",
        )?);
        let mut worklist = try_filled_vec(func, 1, 0usize, "constant fact worklist")?;

        while let Some(pc) = worklist.pop() {
            resources.charge_work(func, slots.len(), "constant fact propagation")?;
            let Some(mut out) = before[pc].clone() else {
                continue;
            };
            apply_constant_fact_transfer(func, module, pc, &slots, &mut out);
            for succ in cfg.successors(pc) {
                if merge_constant_state(func, &mut before[succ], &out)? {
                    worklist.push(succ);
                }
            }
        }

        Ok(Self { slots, before })
    }

    fn fact_for_slot(&self, pc: usize, slot: u16) -> Option<ConstantFact> {
        let idx = self.slots.binary_search(&slot).ok()?;
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
    fn analyze(
        func: &FunctionDef,
        module: &Module,
        cfg: &FunctionCfg,
        dependencies: &FactDependencyGraph,
        constant_facts: &ConstantFactAnalysis,
        resources: &mut VerifierResources,
    ) -> Result<Self, ModuleVerificationError> {
        let slots = tracked_index_check_slots(func, dependencies, resources)?;
        if slots.is_empty() || func.code.is_empty() {
            return Ok(Self {
                slots,
                before: try_none_vec(func, func.code.len(), "index fact states")?,
            });
        }

        resources.charge_bytes::<IndexCheckFact>(
            func,
            func.code.len().saturating_mul(slots.len()),
            "index fact matrix",
        )?;
        let mut before = try_none_vec(func, func.code.len(), "index fact states")?;
        before[0] = Some(try_filled_vec(
            func,
            slots.len(),
            IndexCheckFact::Unknown,
            "index entry facts",
        )?);
        let mut worklist = try_filled_vec(func, 1, 0usize, "index fact worklist")?;

        while let Some(pc) = worklist.pop() {
            resources.charge_work(func, slots.len(), "index fact propagation")?;
            let Some(mut out) = before[pc].clone() else {
                continue;
            };
            apply_index_check_transfer(func, module, constant_facts, pc, &slots, &mut out);
            for succ in cfg.successors(pc) {
                if merge_index_check_state(func, &mut before[succ], &out)? {
                    worklist.push(succ);
                }
            }
        }

        Ok(Self { slots, before })
    }

    fn fact_for_slot(&self, pc: usize, slot: u16) -> Option<IndexCheckFact> {
        let idx = self.slots.binary_search(&slot).ok()?;
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
        key_layout: Arc<[SlotType]>,
        val_layout: Arc<[SlotType]>,
    },
    MapIter {
        key_layout: Arc<[SlotType]>,
        val_layout: Arc<[SlotType]>,
    },
    Queue {
        elem_layout: Arc<[SlotType]>,
    },
}

struct ContainerLayoutAnalysis {
    slots: Vec<u16>,
    before: Vec<Option<Vec<ContainerLayoutFact>>>,
}

impl ContainerLayoutAnalysis {
    fn analyze(
        func: &FunctionDef,
        module: &Module,
        cfg: &FunctionCfg,
        dependencies: &FactDependencyGraph,
        resources: &mut VerifierResources,
    ) -> Result<Self, ModuleVerificationError> {
        let slots = tracked_container_slots(func, dependencies, resources)?;
        if slots.is_empty() || func.code.is_empty() {
            return Ok(Self {
                slots,
                before: try_none_vec(func, func.code.len(), "container fact states")?,
            });
        }

        resources.charge_bytes::<ContainerLayoutFact>(
            func,
            func.code.len().saturating_mul(slots.len()),
            "container fact matrix",
        )?;
        let mut before = try_none_vec(func, func.code.len(), "container fact states")?;
        before[0] = Some(initial_container_state(func, module, &slots, resources)?);
        let instruction_facts = container_instruction_facts(func, resources)?;
        let mut worklist = try_filled_vec(func, 1, 0usize, "container fact worklist")?;

        while let Some(pc) = worklist.pop() {
            resources.charge_work(func, slots.len(), "container fact propagation")?;
            let Some(mut out) = before[pc].clone() else {
                continue;
            };
            apply_container_layout_transfer(func, module, pc, &slots, &instruction_facts, &mut out);
            for succ in cfg.successors(pc) {
                if merge_container_state(func, &mut before[succ], &out)? {
                    worklist.push(succ);
                }
            }
        }

        Ok(Self { slots, before })
    }

    fn fact_for_slot(&self, pc: usize, slot: u16) -> Option<&ContainerLayoutFact> {
        let idx = self.slots.binary_search(&slot).ok()?;
        self.before
            .get(pc)
            .and_then(|state| state.as_ref())
            .map(|state| &state[idx])
    }
}

const CONSTANT_DEPENDENCY: u8 = 1 << 0;
const INDEX_DEPENDENCY: u8 = 1 << 1;
const CONTAINER_DEPENDENCY: u8 = 1 << 2;
const COPY_DEPENDENCIES: u8 = CONSTANT_DEPENDENCY | INDEX_DEPENDENCY | CONTAINER_DEPENDENCY;

#[derive(Clone, Copy)]
struct FactDependency {
    source: u16,
    kinds: u8,
}

struct FactDependencyGraph {
    by_destination: BTreeMap<u16, Vec<FactDependency>>,
}

impl FactDependencyGraph {
    fn build(
        func: &FunctionDef,
        resources: &mut VerifierResources,
    ) -> Result<Self, ModuleVerificationError> {
        let mut graph = Self {
            by_destination: BTreeMap::new(),
        };
        for inst in func.code.iter().copied() {
            match inst.opcode() {
                Opcode::Copy => {
                    graph.push(func, inst.a, inst.b, COPY_DEPENDENCIES, resources)?;
                }
                Opcode::CopyN => {
                    resources.charge_work(
                        func,
                        inst.copy_n_count() as usize,
                        "fact dependency graph",
                    )?;
                    for offset in 0..inst.copy_n_count() {
                        let (Some(destination), Some(source)) =
                            (inst.a.checked_add(offset), inst.b.checked_add(offset))
                        else {
                            break;
                        };
                        graph.push(func, destination, source, COPY_DEPENDENCIES, resources)?;
                    }
                }
                Opcode::Shl | Opcode::Or => {
                    graph.push(func, inst.a, inst.b, CONSTANT_DEPENDENCY, resources)?;
                    graph.push(func, inst.a, inst.c, CONSTANT_DEPENDENCY, resources)?;
                }
                Opcode::MapIterInit => {
                    graph.push(func, inst.a, inst.b, CONTAINER_DEPENDENCY, resources)?;
                }
                _ => {}
            }
        }
        Ok(graph)
    }

    fn push(
        &mut self,
        func: &FunctionDef,
        destination: u16,
        source: u16,
        kinds: u8,
        resources: &mut VerifierResources,
    ) -> Result<(), ModuleVerificationError> {
        if destination >= func.local_slots || source >= func.local_slots {
            return Ok(());
        }
        resources.charge_bytes::<FactDependency>(func, 1, "fact dependency graph")?;
        let edges = self.by_destination.entry(destination).or_default();
        edges.try_reserve(1).map_err(|_| {
            verifier_resource_limit(
                func,
                "fact dependency graph",
                size_of::<FactDependency>(),
                MAX_VERIFIER_DERIVED_BYTES,
            )
        })?;
        edges.push(FactDependency { source, kinds });
        Ok(())
    }

    fn expand(
        &self,
        func: &FunctionDef,
        seeds: impl IntoIterator<Item = u16>,
        kind: u8,
        resources: &mut VerifierResources,
    ) -> Result<Vec<u16>, ModuleVerificationError> {
        let local_slots = func.local_slots as usize;
        resources.charge_bytes::<bool>(func, local_slots, "tracked fact slots")?;
        resources.charge_bytes::<u16>(func, local_slots, "tracked fact worklist")?;
        let mut marked = try_filled_vec(func, local_slots, false, "tracked fact slots")?;
        let mut worklist = Vec::new();
        worklist
            .try_reserve(local_slots.min(256))
            .map_err(|_| verifier_resource_limit(func, "tracked fact worklist", local_slots, 0))?;
        for slot in seeds {
            let Some(mark) = marked.get_mut(slot as usize) else {
                continue;
            };
            if !*mark {
                *mark = true;
                worklist.push(slot);
            }
        }
        while let Some(destination) = worklist.pop() {
            let Some(edges) = self.by_destination.get(&destination) else {
                continue;
            };
            resources.charge_work(func, edges.len(), "tracked fact closure")?;
            for edge in edges {
                if edge.kinds & kind == 0 || marked[edge.source as usize] {
                    continue;
                }
                marked[edge.source as usize] = true;
                worklist.push(edge.source);
            }
        }
        let tracked_count = marked.iter().filter(|marked| **marked).count();
        let mut slots = Vec::new();
        slots
            .try_reserve_exact(tracked_count)
            .map_err(|_| verifier_resource_limit(func, "tracked fact result", tracked_count, 0))?;
        slots.extend(
            marked
                .into_iter()
                .enumerate()
                .filter_map(|(slot, marked)| marked.then_some(slot as u16)),
        );
        Ok(slots)
    }
}

fn tracked_container_slots(
    func: &FunctionDef,
    dependencies: &FactDependencyGraph,
    resources: &mut VerifierResources,
) -> Result<Vec<u16>, ModuleVerificationError> {
    let mut seeds = Vec::new();
    for inst in func.code.iter().copied() {
        match inst.opcode() {
            Opcode::MapNew => seeds.push(inst.a),
            Opcode::MapGet => seeds.push(inst.b),
            Opcode::MapSet | Opcode::MapDelete => seeds.push(inst.a),
            Opcode::MapIterInit => {
                seeds.push(inst.a);
                seeds.push(inst.b);
            }
            Opcode::MapIterNext => seeds.push(inst.b),
            Opcode::MapLen => seeds.push(inst.b),
            Opcode::QueueNew => seeds.push(inst.a),
            Opcode::QueueSend | Opcode::QueueClose | Opcode::SelectSend => {
                seeds.push(inst.a);
            }
            Opcode::QueueRecv | Opcode::SelectRecv => seeds.push(inst.b),
            Opcode::QueueLen | Opcode::QueueCap => seeds.push(inst.b),
            _ => {}
        }
    }
    dependencies.expand(func, seeds, CONTAINER_DEPENDENCY, resources)
}

fn initial_container_state(
    func: &FunctionDef,
    module: &Module,
    slots: &[u16],
    resources: &mut VerifierResources,
) -> Result<Vec<ContainerLayoutFact>, ModuleVerificationError> {
    let mut state = try_filled_vec(
        func,
        slots.len(),
        ContainerLayoutFact::Unknown,
        "container entry facts",
    )?;
    seed_param_container_layout_facts(func, module, slots, &mut state, resources)?;
    Ok(state)
}

fn seed_param_container_layout_facts(
    func: &FunctionDef,
    module: &Module,
    slots: &[u16],
    state: &mut [ContainerLayoutFact],
    resources: &mut VerifierResources,
) -> Result<(), ModuleVerificationError> {
    if func.param_types.is_empty() {
        return Ok(());
    }
    let transfer_slots = func
        .param_types
        .iter()
        .try_fold(0u16, |acc, transfer| acc.checked_add(transfer.slots));
    let Some(transfer_slots) = transfer_slots else {
        return Ok(());
    };
    let implicit_param_slots = match func.recv_slots.checked_add(u16::from(func.is_closure)) {
        Some(slots) => slots,
        None => return Ok(()),
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
        return Ok(());
    };

    let mut cursor = start;
    for transfer in &func.param_types {
        if let Some(fact) = container_fact_for_transfer_type(func, module, transfer, resources)? {
            if let Ok(idx) = slots.binary_search(&cursor) {
                state[idx] = fact;
            }
        }
        let Some(next) = cursor.checked_add(transfer.slots) else {
            return Ok(());
        };
        cursor = next;
    }
    Ok(())
}

fn container_fact_for_transfer_type(
    func: &FunctionDef,
    module: &Module,
    transfer: &TransferType,
    resources: &mut VerifierResources,
) -> Result<Option<ContainerLayoutFact>, ModuleVerificationError> {
    let value_rttid = ValueRttid::from_raw(transfer.rttid_raw);
    let Some(runtime_type) = module.runtime_types.get(value_rttid.rttid() as usize) else {
        return Ok(None);
    };
    match runtime_type {
        RuntimeType::Map { key, val } => {
            let Some(key_layout) = module.slot_layout_for_value_rttid(*key) else {
                return Ok(None);
            };
            let Some(val_layout) = module.slot_layout_for_value_rttid(*val) else {
                return Ok(None);
            };
            Ok(Some(ContainerLayoutFact::Map {
                key_layout: shared_container_layout(
                    func,
                    &key_layout,
                    "parameter map key layout",
                    resources,
                )?,
                val_layout: shared_container_layout(
                    func,
                    &val_layout,
                    "parameter map value layout",
                    resources,
                )?,
            }))
        }
        RuntimeType::Chan { elem, .. } | RuntimeType::Port { elem, .. } => {
            let Some(elem_layout) = module.slot_layout_for_value_rttid(*elem) else {
                return Ok(None);
            };
            Ok(Some(ContainerLayoutFact::Queue {
                elem_layout: shared_container_layout(
                    func,
                    &elem_layout,
                    "parameter queue element layout",
                    resources,
                )?,
            }))
        }
        _ => Ok(None),
    }
}

fn shared_container_layout(
    func: &FunctionDef,
    layout: &[SlotType],
    resource: &'static str,
    resources: &mut VerifierResources,
) -> Result<Arc<[SlotType]>, ModuleVerificationError> {
    resources.charge_bytes::<SlotType>(func, layout.len(), resource)?;
    let mut owned = Vec::new();
    owned.try_reserve_exact(layout.len()).map_err(|_| {
        verifier_resource_limit(
            func,
            resource,
            layout.len().saturating_mul(size_of::<SlotType>()),
            0,
        )
    })?;
    owned.extend_from_slice(layout);
    Ok(Arc::from(owned))
}

fn container_instruction_facts(
    func: &FunctionDef,
    resources: &mut VerifierResources,
) -> Result<Vec<Option<ContainerLayoutFact>>, ModuleVerificationError> {
    let mut facts = try_none_vec(func, func.code.len(), "container instruction facts")?;
    for (pc, (inst, metadata)) in func
        .code
        .iter()
        .zip(func.instruction_metadata.iter())
        .enumerate()
    {
        facts[pc] = match (inst.opcode(), metadata) {
            (
                Opcode::MapNew,
                InstructionMetadata::MapNew {
                    key_layout,
                    val_layout,
                },
            ) => Some({
                Ok(ContainerLayoutFact::Map {
                    key_layout: shared_container_layout(
                        func,
                        key_layout,
                        "MapNew key layout fact",
                        resources,
                    )?,
                    val_layout: shared_container_layout(
                        func,
                        val_layout,
                        "MapNew value layout fact",
                        resources,
                    )?,
                })
            }),
            (Opcode::QueueNew, InstructionMetadata::QueueLayout { elem_layout }) => Some({
                Ok(ContainerLayoutFact::Queue {
                    elem_layout: shared_container_layout(
                        func,
                        elem_layout,
                        "QueueNew element layout fact",
                        resources,
                    )?,
                })
            }),
            _ => None,
        }
        .transpose()?;
    }
    Ok(facts)
}

fn merge_container_state(
    func: &FunctionDef,
    dst: &mut Option<Vec<ContainerLayoutFact>>,
    incoming: &[ContainerLayoutFact],
) -> Result<bool, ModuleVerificationError> {
    let Some(current) = dst else {
        let mut state = Vec::new();
        state.try_reserve_exact(incoming.len()).map_err(|_| {
            verifier_resource_limit(
                func,
                "container fact state",
                incoming.len(),
                MAX_VERIFIER_DERIVED_BYTES,
            )
        })?;
        state.extend_from_slice(incoming);
        *dst = Some(state);
        return Ok(true);
    };

    let mut changed = false;
    for (current, incoming) in current.iter_mut().zip(incoming.iter()) {
        let merged = merge_container_fact(current, incoming);
        if merged != *current {
            *current = merged;
            changed = true;
        }
    }
    Ok(changed)
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
    instruction_facts: &[Option<ContainerLayoutFact>],
    state: &mut [ContainerLayoutFact],
) {
    let inst = func.code[pc];
    let input = state.to_vec();
    for (idx, slot) in slots.iter().copied().enumerate() {
        if !instruction_writes_slot(Some(module), func, pc, inst, slot) {
            continue;
        }
        state[idx] = container_fact_written_to_slot(
            inst,
            slot,
            slots,
            &input,
            instruction_facts.get(pc).and_then(Option::as_ref),
        );
    }
}

fn container_fact_written_to_slot(
    inst: Instruction,
    slot: u16,
    tracked_slots: &[u16],
    input: &[ContainerLayoutFact],
    instruction_fact: Option<&ContainerLayoutFact>,
) -> ContainerLayoutFact {
    match inst.opcode() {
        Opcode::MapNew | Opcode::QueueNew if inst.a == slot => instruction_fact
            .cloned()
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
        .binary_search(&source)
        .ok()
        .map(|idx| input[idx].clone())
        .unwrap_or(ContainerLayoutFact::Unknown)
}

fn tracked_constant_slots(
    func: &FunctionDef,
    dependencies: &FactDependencyGraph,
    resources: &mut VerifierResources,
) -> Result<Vec<u16>, ModuleVerificationError> {
    let mut seeds = Vec::new();
    for inst in func.code.iter().copied() {
        match inst.opcode() {
            Opcode::MapNew => {
                seeds.push(inst.b);
                if let Some(key_rttid_slot) = inst.b.checked_add(1) {
                    seeds.push(key_rttid_slot);
                }
            }
            Opcode::ArrayNew | Opcode::SliceNew => seeds.push(inst.b),
            Opcode::SliceAppend => seeds.push(inst.c),
            Opcode::PtrNew => seeds.push(inst.b),
            Opcode::QueueNew => seeds.push(inst.b),
            Opcode::IndexCheck => seeds.push(inst.b),
            _ => {}
        }
    }
    dependencies.expand(func, seeds, CONSTANT_DEPENDENCY, resources)
}

fn tracked_index_check_slots(
    func: &FunctionDef,
    dependencies: &FactDependencyGraph,
    resources: &mut VerifierResources,
) -> Result<Vec<u16>, ModuleVerificationError> {
    let mut seeds = Vec::new();
    for inst in func.code.iter().copied() {
        match inst.opcode() {
            Opcode::SlotGet | Opcode::SlotGetN => seeds.push(inst.c),
            Opcode::SlotSet | Opcode::SlotSetN => seeds.push(inst.b),
            _ => {}
        }
    }
    dependencies.expand(func, seeds, INDEX_DEPENDENCY, resources)
}

#[derive(Clone, Copy)]
struct InstructionSuccessors {
    pcs: [usize; 2],
    len: u8,
}

impl InstructionSuccessors {
    const fn new() -> Self {
        Self {
            pcs: [0; 2],
            len: 0,
        }
    }

    fn push(&mut self, pc: usize) {
        if !self.pcs[..self.len as usize].contains(&pc) {
            self.pcs[self.len as usize] = pc;
            self.len += 1;
        }
    }

    fn as_slice(&self) -> &[usize] {
        &self.pcs[..self.len as usize]
    }
}

struct FunctionCfg {
    successors: Vec<InstructionSuccessors>,
}

impl FunctionCfg {
    fn build(
        func: &FunctionDef,
        resources: &mut VerifierResources,
    ) -> Result<Self, ModuleVerificationError> {
        resources.charge_bytes::<InstructionSuccessors>(
            func,
            func.code.len(),
            "control-flow graph",
        )?;
        resources.charge_work(func, func.code.len(), "control-flow graph")?;
        let mut successors = try_filled_vec(
            func,
            func.code.len(),
            InstructionSuccessors::new(),
            "control-flow graph",
        )?;
        for (pc, inst) in func.code.iter().copied().enumerate() {
            let entry = &mut successors[pc];
            match inst.opcode() {
                Opcode::Jump => {
                    push_valid_successor(func, entry, jump_target_i64(pc, inst.imm32()));
                }
                Opcode::JumpIf | Opcode::JumpIfNot => {
                    push_fallthrough_successor(func, pc, entry);
                    push_valid_successor(func, entry, jump_target_i64(pc, inst.imm32()));
                }
                Opcode::ForLoop => {
                    push_fallthrough_successor(func, pc, entry);
                    push_valid_successor(func, entry, forloop_target_i64(pc, inst.c as i16));
                }
                Opcode::Return | Opcode::Panic => {}
                _ => push_fallthrough_successor(func, pc, entry),
            }
        }
        Ok(Self { successors })
    }

    fn successors(&self, pc: usize) -> impl Iterator<Item = usize> + '_ {
        self.successors[pc].as_slice().iter().copied()
    }
}

fn push_fallthrough_successor(
    func: &FunctionDef,
    pc: usize,
    successors: &mut InstructionSuccessors,
) {
    let next = pc + 1;
    if next < func.code.len() {
        successors.push(next);
    }
}

fn push_valid_successor(func: &FunctionDef, successors: &mut InstructionSuccessors, target: i64) {
    if target >= 0 {
        let target = target as usize;
        if target < func.code.len() {
            successors.push(target);
        }
    }
}

fn merge_constant_state(
    func: &FunctionDef,
    dst: &mut Option<Vec<ConstantFact>>,
    incoming: &[ConstantFact],
) -> Result<bool, ModuleVerificationError> {
    let Some(current) = dst else {
        let mut state = Vec::new();
        state.try_reserve_exact(incoming.len()).map_err(|_| {
            verifier_resource_limit(
                func,
                "constant fact state",
                incoming.len(),
                MAX_VERIFIER_DERIVED_BYTES,
            )
        })?;
        state.extend_from_slice(incoming);
        *dst = Some(state);
        return Ok(true);
    };

    let mut changed = false;
    for (current, incoming) in current.iter_mut().zip(incoming.iter().copied()) {
        let merged = merge_constant_fact(*current, incoming);
        if merged != *current {
            *current = merged;
            changed = true;
        }
    }
    Ok(changed)
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
        .binary_search(&source)
        .ok()
        .map(|idx| input[idx])
        .unwrap_or(ConstantFact::Unknown)
}

fn merge_index_check_state(
    func: &FunctionDef,
    dst: &mut Option<Vec<IndexCheckFact>>,
    incoming: &[IndexCheckFact],
) -> Result<bool, ModuleVerificationError> {
    let Some(current) = dst else {
        let mut state = Vec::new();
        state.try_reserve_exact(incoming.len()).map_err(|_| {
            verifier_resource_limit(
                func,
                "index fact state",
                incoming.len(),
                MAX_VERIFIER_DERIVED_BYTES,
            )
        })?;
        state.extend_from_slice(incoming);
        *dst = Some(state);
        return Ok(true);
    };

    let mut changed = false;
    for (current, incoming) in current.iter_mut().zip(incoming.iter().copied()) {
        let merged = merge_index_check_fact(*current, incoming);
        if merged != *current {
            *current = merged;
            changed = true;
        }
    }
    Ok(changed)
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
            index_check_slot_set_writes_slot(func, pc, inst, slot, tracked_slots, input)
                .unwrap_or_else(|| instruction_writes_slot(Some(module), func, pc, inst, slot))
        }
        _ => instruction_writes_slot(Some(module), func, pc, inst, slot),
    }
}

fn index_check_slot_set_writes_slot(
    func: &FunctionDef,
    pc: usize,
    inst: Instruction,
    slot: u16,
    tracked_slots: &[u16],
    input: &[IndexCheckFact],
) -> Option<bool> {
    let elem_slots = match inst.opcode() {
        Opcode::SlotSet => 1usize,
        Opcode::SlotSetN => match func.instruction_metadata.get(pc) {
            Some(InstructionMetadata::SlotLayout { elem_layout, .. }) => elem_layout.len(),
            _ => inst.flags as usize,
        },
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
        .binary_search(&source)
        .ok()
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
    match inst.opcode() {
        Opcode::SlotSet => return slot >= inst.a,
        Opcode::SlotSetN => {
            let has_elements = matches!(
                func.instruction_metadata.get(pc),
                Some(InstructionMetadata::SlotLayout { elem_layout, .. }) if !elem_layout.is_empty()
            );
            return has_elements && slot >= inst.a;
        }
        _ => {}
    }

    let (externs, functions) = module
        .map(|module| (module.externs.as_slice(), module.functions.as_slice()))
        .unwrap_or((&[], &[]));
    let mut writes = false;
    let result = crate::instruction_effects::visit_instruction_register_writes(
        &inst,
        func.instruction_metadata.get(pc),
        externs,
        functions,
        |start, count| {
            writes |= crate::instruction_effects::register_range_contains(slot, start, count);
        },
    );
    writes || result.is_err()
}

fn slot_in_range(slot: u16, start: u16, count: usize) -> bool {
    if count == 0 || slot < start {
        return false;
    }
    let offset = usize::from(slot - start);
    offset < count
}

fn checked_metadata_call_ret_start(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    args_start: u16,
    arg_slots: usize,
    access: &'static str,
) -> Result<u16, ModuleVerificationError> {
    let count = u16::try_from(arg_slots).map_err(|_| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} argument layout has {arg_slots} slots, exceeding u16::MAX"),
        )
    })?;
    args_start
        .checked_add(count)
        .ok_or_else(|| ModuleVerificationError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: args_start,
            count,
            access,
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
    let count = checked_layout_slot_count(func, pc, start, expected.len(), access)?;
    let actual = local_layout(func, pc, start, count, access)?;
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

fn layout_flows_to(source: &[SlotType], target: &[SlotType]) -> bool {
    source.len() == target.len()
        && source
            .iter()
            .zip(target)
            .all(|(source, target)| source.can_flow_to(*target))
}

/// Verify that values read from a local range can flow into `target` storage.
fn verify_local_layout_flows_to(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    target: &[SlotType],
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let count = checked_layout_slot_count(func, pc, start, target.len(), access)?;
    let actual = local_layout(func, pc, start, count, access)?;
    if layout_flows_to(actual, target) {
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
            expected: target.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

/// Verify that values with `source` layout can flow into a local destination.
fn verify_layout_flows_to_local(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    source: &[SlotType],
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let count = checked_layout_slot_count(func, pc, start, source.len(), access)?;
    let actual = local_layout(func, pc, start, count, access)?;
    if layout_flows_to(source, actual) {
        return Ok(());
    }
    verify_structural_layout(func, pc, opcode, start, source, access)?;
    verify_structural_layout(func, pc, opcode, start, actual, access)?;
    Err(ModuleVerificationError::SlotTypeMismatch {
        func: func.name.clone(),
        pc,
        opcode,
        access,
        slot: start,
        expected: source.to_vec(),
        actual: actual.to_vec(),
    })
}

fn checked_layout_slot_count(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    count: usize,
    access: &'static str,
) -> Result<u16, ModuleVerificationError> {
    u16::try_from(count).map_err(|_| ModuleVerificationError::SlotRangeOverflow {
        func: func.name.clone(),
        pc,
        start,
        count: u16::MAX,
        access,
    })
}

fn verify_structural_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    layout: &[SlotType],
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    checked_layout_slot_count(func, pc, start, layout.len(), access)?;
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

fn verify_reserved_zero(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    value: u64,
    field: &'static str,
) -> Result<(), ModuleVerificationError> {
    if value == 0 {
        Ok(())
    } else {
        Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("reserved {field} must be zero, got {value}"),
        ))
    }
}

fn verify_allowed_flags(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    flags: u8,
    allowed: u8,
) -> Result<(), ModuleVerificationError> {
    let unsupported = flags & !allowed;
    if unsupported == 0 {
        Ok(())
    } else {
        Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("unsupported flags 0x{unsupported:02x}"),
        ))
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
    decode: impl FnOnce(&InstructionMetadata) -> Option<T>,
) -> Result<T, ModuleVerificationError> {
    func.instruction_metadata
        .get(pc)
        .and_then(decode)
        .ok_or_else(|| missing_layout(func, pc, opcode, layout))
}

fn elem_layout_from_instruction(metadata: &InstructionMetadata) -> Option<Vec<SlotType>> {
    match metadata {
        InstructionMetadata::ElemLayout {
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

fn elem_runtime_layout_from_instruction(metadata: &InstructionMetadata) -> Option<Vec<SlotType>> {
    match metadata {
        InstructionMetadata::ElemLayout { slot_layout, .. } => Some(slot_layout.clone()),
        _ => None,
    }
}

fn elem_layout_for_indexed(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "ElemLayout", elem_layout_from_instruction)
}

fn elem_runtime_layout_for_indexed(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "ElemLayout",
        elem_runtime_layout_from_instruction,
    )
}

fn ptr_value_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<Vec<SlotType>, ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "PtrLayout", |metadata| match metadata {
        InstructionMetadata::PtrLayout { value_layout } => Some(value_layout.clone()),
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
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    verify_reserved_zero(func, pc, opcode, inst.c.into(), "c")?;
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
    verify_one_of_single_slot_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcBase, SlotType::GcRef],
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
                "PtrNew value metadata layout {runtime_layout:?} does not match instruction metadata {value_layout:?}"
            ),
        ));
    }
    Ok(())
}

fn slot_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(u16, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "SlotLayout", |metadata| match metadata {
        InstructionMetadata::SlotLayout {
            array_len,
            elem_layout,
        } => Some((*array_len, elem_layout.clone())),
        _ => None,
    })
}

fn checked_metadata_layout_slots(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    layout: &[SlotType],
    access: &'static str,
) -> Result<u16, ModuleVerificationError> {
    u16::try_from(layout.len()).map_err(|_| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("{access} slot count {} exceeds u16::MAX", layout.len()),
        )
    })
}

fn call_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "CallLayout", |metadata| match metadata {
        InstructionMetadata::CallLayout {
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
) -> Result<(u32, u32, Vec<SlotType>, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "CallIfaceLayout",
        |metadata| match metadata {
            InstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                method_idx,
                arg_layout,
                ret_layout,
            } => Some((
                *iface_meta_id,
                *method_idx,
                arg_layout.clone(),
                ret_layout.clone(),
            )),
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
            InstructionMetadata::CallExternLayout {
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
        InstructionMetadata::QueueLayout { elem_layout } => Some(elem_layout.clone()),
        _ => None,
    })
}

fn checked_queue_elem_slots(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    elem_layout: &[SlotType],
) -> Result<u16, ModuleVerificationError> {
    u16::try_from(elem_layout.len()).map_err(|_| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "QueueLayout element slot count {} exceeds u16::MAX",
                elem_layout.len()
            ),
        )
    })
}

fn map_new_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "MapNew", |metadata| match metadata {
        InstructionMetadata::MapNew {
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
        InstructionMetadata::MapGet {
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
        InstructionMetadata::MapSet {
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
        InstructionMetadata::MapDelete { key_layout } => Some(key_layout.clone()),
        _ => None,
    })
}

fn map_iter_next_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(Vec<SlotType>, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(func, pc, opcode, "MapIterNext", |metadata| match metadata {
        InstructionMetadata::MapIterNext {
            key_layout,
            val_layout,
        } => Some((key_layout.clone(), val_layout.clone())),
        _ => None,
    })
}

fn iface_assert_metadata(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
) -> Result<(u8, u32, Vec<SlotType>), ModuleVerificationError> {
    decode_metadata_layout(
        func,
        pc,
        opcode,
        "IfaceAssertLayout",
        |metadata| match metadata {
            InstructionMetadata::IfaceAssertLayout {
                assert_kind,
                target_id,
                result_layout,
            } => Some((*assert_kind, *target_id, result_layout.clone())),
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
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    let count = inst.c;
    let source = local_layout(func, pc, inst.b, count, "CopyN source")?;
    verify_structural_layout(func, pc, opcode, inst.b, source, "CopyN source")?;
    verify_layout_flows_to_local(func, pc, opcode, inst.a, source, "CopyN destination")
}

fn verify_copy_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let source = local_layout(func, pc, inst.b, 1, "Copy source")?;
    let actual = local_layout(func, pc, inst.a, 1, "Copy destination")?;
    if layout_flows_to(source, actual) {
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
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let (array_len, elem_layout) = slot_layout(func, pc, opcode)?;
    verify_reserved_zero(func, pc, opcode, ctx.inst.flags.into(), "flags")?;
    if (opcode == Opcode::SlotGet) != (elem_layout.len() == 1) {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{opcode:?} requires {} element slots, metadata has {}",
                if opcode == Opcode::SlotGet {
                    "one"
                } else {
                    "zero or multiple"
                },
                elem_layout.len()
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
        array_len,
        &elem_layout,
        "SlotGet element span",
    )?;
    verify_layout_flows_to_local(
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
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let (array_len, elem_layout) = slot_layout(func, pc, opcode)?;
    verify_reserved_zero(func, pc, opcode, ctx.inst.flags.into(), "flags")?;
    if (opcode == Opcode::SlotSet) != (elem_layout.len() == 1) {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{opcode:?} requires {} element slots, metadata has {}",
                if opcode == Opcode::SlotSet {
                    "one"
                } else {
                    "zero or multiple"
                },
                elem_layout.len()
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
        array_len,
        &elem_layout,
        "SlotSet element span",
    )?;
    verify_local_layout_flows_to(func, pc, opcode, src_start, &elem_layout, "SlotSet source")
}

fn verify_dynamic_slot_span(
    ctx: InstructionVerifierContext<'_>,
    index_check_facts: &IndexCheckAnalysis,
    base_start: u16,
    index_slot: u16,
    declared_len: u16,
    elem_layout: &[SlotType],
    access: &'static str,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let checked_len =
        index_checked_len_before(index_check_facts, func, pc, opcode, index_slot, access)?;
    if checked_len != declared_len {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{access} checked length {checked_len} does not match SlotLayout array length {declared_len}"
            ),
        ));
    }
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
    verify_layout_flows_to_local(
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
    verify_local_layout_flows_to(func, pc, opcode, src_start, expected, "GlobalSet source")
}

fn verify_ptr_get_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    dst_start: u16,
    ptr_slot: u16,
    flags: u8,
) -> Result<(), ModuleVerificationError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    verify_reserved_zero(func, pc, opcode, flags.into(), "flags")?;
    if (opcode == Opcode::PtrGet) != (value_layout.len() == 1) {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "{opcode:?} requires {} value slots, metadata has {}",
                if opcode == Opcode::PtrGet {
                    "one"
                } else {
                    "zero or multiple"
                },
                value_layout.len()
            ),
        ));
    }
    verify_one_of_single_slot_layout(
        func,
        pc,
        opcode,
        ptr_slot,
        &[SlotType::GcBase, SlotType::GcRef],
        "PtrGet pointer",
    )?;
    verify_layout_flows_to_local(
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
    verify_reserved_zero(func, pc, opcode, flags.into(), "flags")?;
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
    verify_one_of_single_slot_layout(
        func,
        pc,
        opcode,
        ptr_slot,
        &[SlotType::GcBase, SlotType::GcRef],
        "PtrSet pointer",
    )?;
    verify_local_layout_flows_to(func, pc, opcode, src_slot, &value_layout, "PtrSet source")?;
    Ok(())
}

fn verify_ptr_set_n_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let value_layout = ptr_value_layout(func, pc, opcode)?;
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    verify_one_of_single_slot_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcBase, SlotType::GcRef],
        "PtrSetN pointer",
    )?;
    let count =
        checked_metadata_layout_slots(func, pc, opcode, &value_layout, "PtrSetN source layout")?;
    let source = local_layout(func, pc, inst.c, count, "PtrSetN source")?;
    verify_local_layout_flows_to(func, pc, opcode, inst.c, &value_layout, "PtrSetN source")?;
    if source.iter().any(|slot| slot.needs_write_barrier()) {
        return Err(ModuleVerificationError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access: "PtrSetN source requires typed write barriers",
            slot: inst.c,
            expected: source
                .iter()
                .map(|slot| {
                    if slot.needs_write_barrier() {
                        SlotType::Value
                    } else {
                        *slot
                    }
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

fn verify_conversion_flags(
    ctx: InstructionVerifierContext<'_>,
) -> Result<(), ModuleVerificationError> {
    let allowed = match ctx.opcode {
        Opcode::ConvI2F => CONV_I2F_ALLOWED_FLAGS,
        Opcode::ConvF2I => CONV_F2I_ALLOWED_FLAGS,
        _ => 0,
    };
    if ctx.inst.flags & !allowed != 0 {
        return Err(ModuleVerificationError::InvalidInstructionFlags {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            opcode: ctx.opcode,
            flags: ctx.inst.flags,
            allowed,
        });
    }
    Ok(())
}

fn verify_shift_flags(ctx: InstructionVerifierContext<'_>) -> Result<(), ModuleVerificationError> {
    if ctx.inst.flags & !SHIFT_ALLOWED_FLAGS != 0 {
        return Err(ModuleVerificationError::InvalidInstructionFlags {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            opcode: ctx.opcode,
            flags: ctx.inst.flags,
            allowed: SHIFT_ALLOWED_FLAGS,
        });
    }
    Ok(())
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
            &vec![SlotType::GcBase; inst.b as usize],
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
    verify_local_layout_flows_to(func, pc, opcode, inst.a, expected, "Return values")?;

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

    verify_reserved_zero(func, pc, opcode, inst.c.into(), "c")?;

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
    verify_local_layout_flows_to(
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
    verify_layout_flows_to_local(
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
            &[SlotType::GcBase],
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
        let (iface_meta_id, method_idx, arg_layout, ret_layout) =
            call_iface_layout(func, pc, opcode)?;
        let Some(iface_meta) = module.interface_metas.get(iface_meta_id as usize) else {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("CallIface metadata references missing interface meta id {iface_meta_id}"),
            ));
        };
        if method_idx as usize >= iface_meta.methods.len() {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "CallIface method_idx {} out of bounds for callsite interface {} method count {}",
                    method_idx,
                    iface_meta_id,
                    iface_meta.methods.len()
                ),
            ));
        }
        let iface_method = &iface_meta.methods[method_idx as usize];
        let (signature_args, signature_returns) =
            function_signature_slot_layouts(module, iface_method.signature_rttid).map_err(
                |detail| {
                    call_shape_mismatch(
                        func,
                        pc,
                        opcode,
                        format!(
                    "CallIface interface {} method {} signature cannot be resolved: {detail}",
                    iface_meta_id, iface_method.name
                ),
                    )
                },
            )?;
        if arg_layout != signature_args {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "CallIface metadata argument layout {arg_layout:?} does not match interface {} method {} non-receiver signature argument layout {signature_args:?}",
                    iface_meta_id, iface_method.name
                ),
            ));
        }
        if ret_layout != signature_returns {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "CallIface metadata return layout {ret_layout:?} does not match interface {} method {} signature return layout {signature_returns:?}",
                    iface_meta_id, iface_method.name
                ),
            ));
        }
        (arg_layout, ret_layout)
    };
    let ret_start = checked_metadata_call_ret_start(
        func,
        pc,
        opcode,
        inst.b,
        arg_layout.len(),
        "dynamic call returns",
    )?;
    verify_local_layout_flows_to(func, pc, opcode, inst.b, &arg_layout, "dynamic call args")?;
    verify_layout_flows_to_local(
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
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    verify_extern_index(func, module, pc, inst.b)?;
    let extern_def = &module.externs[inst.b as usize];
    let (arg_layout, ret_layout) = call_extern_layout(func, pc, opcode)?;
    if !extern_def.param_kinds.is_empty() && extern_def.param_kinds.len() != arg_layout.len() {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "extern {} has {} param_kinds but callsite metadata declares {} arg slots",
                extern_def.name,
                extern_def.param_kinds.len(),
                arg_layout.len()
            ),
        ));
    }
    if let Some(param_slots) = extern_def.params.exact_slots() {
        if usize::from(param_slots) != arg_layout.len() {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "CallExtern arg slot count {} does not match extern {} params {}",
                    arg_layout.len(),
                    extern_def.name,
                    extern_def.params.display_name()
                ),
            ));
        }
    }
    if ret_layout.len() != extern_def.returns.slots as usize {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "CallExtern metadata layout slots args={} returns={} do not match extern returns={}",
                arg_layout.len(),
                ret_layout.len(),
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
    let arg_start = usize::from(inst.c);
    let arg_end = arg_start + arg_layout.len();
    let ret_start = usize::from(inst.a);
    let ret_end = ret_start + ret_layout.len();
    if arg_start < ret_end && ret_start < arg_end {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "CallExtern argument slots {arg_start}..{arg_end} overlap return slots {ret_start}..{ret_end}"
            ),
        ));
    }
    verify_local_layout_flows_to(func, pc, opcode, inst.c, &arg_layout, "CallExtern args")?;
    verify_layout_flows_to_local(func, pc, opcode, inst.a, &ret_layout, "CallExtern returns")
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
        &[SlotType::GcBase],
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
        &[SlotType::GcBase],
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
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    let elem_layout = elem_runtime_layout_for_indexed(func, pc, opcode)?;
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
        &[SlotType::GcBase],
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
    Ok(())
}

fn verify_slice_new_contract(
    func: &FunctionDef,
    module: &Module,
    constant_facts: &ConstantFactAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    let elem_layout = elem_runtime_layout_for_indexed(func, pc, opcode)?;
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
        &[SlotType::GcBase],
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
    verify_value_range(func, pc, opcode, inst.c, 2, "SliceNew len/cap")
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
        format!("{label} layout {elem_meta_layout:?} does not match instruction metadata {elem_layout:?}"),
    ))
}

fn verify_indexed_get_contract(
    ctx: InstructionVerifierContext<'_>,
    _constant_facts: &ConstantFactAnalysis,
    access: IndexedAccessLabels,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let inst = ctx.inst;
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    let elem_layout = elem_layout_for_indexed(func, pc, opcode)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcBase], access.base)?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], access.index)?;
    if elem_layout.is_empty() {
        Ok(())
    } else {
        verify_layout_flows_to_local(func, pc, opcode, inst.a, &elem_layout, access.value)
    }
}

fn verify_indexed_set_contract(
    ctx: InstructionVerifierContext<'_>,
    _constant_facts: &ConstantFactAnalysis,
    access: IndexedAccessLabels,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let inst = ctx.inst;
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    let elem_layout = elem_layout_for_indexed(func, pc, opcode)?;
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcBase], access.base)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::Value], access.index)?;
    if elem_layout.is_empty() {
        Ok(())
    } else {
        verify_local_layout_flows_to(func, pc, opcode, inst.c, &elem_layout, access.value)
    }
}

fn verify_indexed_addr_contract(
    ctx: InstructionVerifierContext<'_>,
    _constant_facts: &ConstantFactAnalysis,
    access: IndexedAccessLabels,
) -> Result<(), ModuleVerificationError> {
    let func = ctx.func;
    let pc = ctx.pc;
    let opcode = ctx.opcode;
    let inst = ctx.inst;
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    let _ = elem_layout_for_indexed(func, pc, opcode)?;
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcRef], access.value)?;
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcBase], access.base)?;
    verify_layout(func, pc, opcode, inst.c, &[SlotType::Value], access.index)
}

fn verify_slice_slice_contract(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    if inst.flags & !SLICE_SLICE_ALLOWED_FLAGS != 0 {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("unsupported SliceSlice flags 0x{:02x}", inst.flags),
        ));
    }
    let inline_view = inst.flags & SLICE_SLICE_FLAG_INLINE_ARRAY_VIEW != 0;
    if inline_view && inst.flags & SLICE_SLICE_FLAG_ARRAY == 0 {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            "inline array view mode requires the array source flag".to_string(),
        ));
    }
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcBase],
        "SliceSlice destination",
    )?;
    if inline_view {
        verify_layout(
            func,
            pc,
            opcode,
            inst.b,
            &[
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ],
            "SliceSlice inline array view",
        )?;
    } else {
        verify_layout(
            func,
            pc,
            opcode,
            inst.b,
            &[SlotType::GcBase],
            "SliceSlice source",
        )?;
    }
    let bound_count = if (inst.flags & SLICE_SLICE_FLAG_HAS_MAX) != 0 {
        3
    } else {
        2
    };
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
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    let elem_layout = elem_layout_for_indexed(func, pc, opcode)?;
    let elem_runtime_layout = elem_runtime_layout_for_indexed(func, pc, opcode)?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcBase],
        "SliceAppend destination",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::GcBase],
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
    if elem_layout.is_empty() {
        Ok(())
    } else {
        verify_local_layout_flows_to(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.c, 1, "SliceAppend element")?,
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
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    verify_reserved_zero(func, pc, opcode, inst.c.into(), "c")?;
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
        &[SlotType::GcBase],
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
    if key_rttid_raw >= INVALID_META_ID {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "MapNew bare key RTTID {key_rttid_raw} exceeds the packed ValueRttid domain or uses reserved id 0x{INVALID_META_ID:06x}"
            ),
        ));
    }
    let key_rttid = ValueRttid::try_new(key_rttid_raw, key_meta.value_kind()).ok_or_else(|| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("MapNew bare key RTTID {key_rttid_raw} is not representable"),
        )
    })?;
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
            "MapNew {access} metadata layout {runtime_layout:?} does not match instruction metadata {metadata_layout:?}"
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
                "QueueNew element metadata layout {elem_meta_layout:?} does not match instruction metadata {elem_layout:?}"
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
        ValueKind::Array => {
            let value_rttid = ValueRttid::try_new(value_meta.meta_id(), ValueKind::Array)
                .ok_or_else(|| {
                    module_invariant(format!(
                        "{label} array runtime type {} is not representable",
                        value_meta.meta_id()
                    ))
                })?;
            module
                .slot_layout_for_value_rttid(value_rttid)
                .ok_or_else(|| {
                    module_invariant(format!(
                        "{label} array runtime type {} has no slot layout",
                        value_meta.meta_id()
                    ))
                })
        }
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
            if expected.key_layout != known_key.as_ref() {
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
            if expected.val_layout != known_val.as_ref() {
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
            if key_layout != known_key.as_ref() {
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
            if expected.key_layout != known_key.as_ref() {
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
            if expected.val_layout != known_val.as_ref() {
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
            if elem_layout != known_elem.as_ref() {
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
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
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
    let val_slots =
        checked_metadata_layout_slots(func, pc, opcode, &val_layout, "MapGet value layout")?;
    if has_ok && val_slots.checked_add(1).is_none() {
        return Err(ModuleVerificationError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start: inst.a,
            count: u16::MAX,
            access: "MapGet ok",
        });
    }
    verify_layout(func, pc, opcode, inst.b, &[SlotType::GcBase], "MapGet map")?;
    verify_local_layout_flows_to(func, pc, opcode, inst.c, &key_layout, "MapGet key")?;
    verify_layout_flows_to_local(func, pc, opcode, inst.a, &val_layout, "MapGet value")?;
    if has_ok {
        verify_layout(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.a, val_slots, "MapGet ok")?,
            &[SlotType::Value],
            "MapGet ok",
        )?;
    }
    Ok(())
}

fn verify_map_set_contract(
    func: &FunctionDef,
    container_layout_facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
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
    verify_layout(func, pc, opcode, inst.a, &[SlotType::GcBase], "MapSet map")?;
    verify_local_layout_flows_to(func, pc, opcode, inst.b, &key_layout, "MapSet key")?;
    verify_local_layout_flows_to(func, pc, opcode, inst.c, &val_layout, "MapSet value")
}

fn verify_map_delete_contract(
    func: &FunctionDef,
    container_layout_facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    verify_reserved_zero(func, pc, opcode, inst.c.into(), "c")?;
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
        &[SlotType::GcBase],
        "MapDelete map",
    )?;
    verify_local_layout_flows_to(func, pc, opcode, inst.b, &key_layout, "MapDelete key")
}

fn verify_map_iter_next_contract(
    func: &FunctionDef,
    container_layout_facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
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
    let key_slots =
        checked_metadata_layout_slots(func, pc, opcode, &key_layout, "MapIterNext key layout")?;
    checked_metadata_layout_slots(func, pc, opcode, &val_layout, "MapIterNext value layout")?;
    let key_start = inst.a;
    let value_start = checked_slot_offset(func, pc, key_start, key_slots, "MapIterNext value")?;
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
    verify_allowed_flags(func, pc, opcode, inst.flags, QUEUE_KIND_PORT_FLAG)?;
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
        &[SlotType::GcBase],
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
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    verify_known_queue_layout(
        func,
        container_layout_facts,
        pc,
        opcode,
        inst.a,
        "QueueSend",
        &elem_layout,
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcBase],
        "QueueSend queue",
    )?;
    verify_local_layout_flows_to(func, pc, opcode, inst.b, &elem_layout, "QueueSend value")
}

fn verify_queue_recv_contract(
    func: &FunctionDef,
    container_layout_facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    let elem_slots = checked_queue_elem_slots(func, pc, opcode, &elem_layout)?;
    verify_allowed_flags(func, pc, opcode, inst.flags, QUEUE_RECV_HAS_OK_FLAG)?;
    verify_known_queue_layout(
        func,
        container_layout_facts,
        pc,
        opcode,
        inst.b,
        "QueueRecv",
        &elem_layout,
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::GcBase],
        "QueueRecv queue",
    )?;
    verify_layout_flows_to_local(func, pc, opcode, inst.a, &elem_layout, "QueueRecv value")?;
    if inst.recv_has_ok() {
        verify_layout(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.a, elem_slots, "QueueRecv ok")?,
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
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    verify_known_queue_layout(
        func,
        container_layout_facts,
        pc,
        opcode,
        inst.a,
        "SelectSend",
        &elem_layout,
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcBase],
        "SelectSend queue",
    )?;
    verify_local_layout_flows_to(func, pc, opcode, inst.b, &elem_layout, "SelectSend value")
}

fn verify_select_recv_contract(
    func: &FunctionDef,
    container_layout_facts: &ContainerLayoutAnalysis,
    pc: usize,
    opcode: Opcode,
    inst: Instruction,
) -> Result<(), ModuleVerificationError> {
    let elem_layout = queue_elem_layout(func, pc, opcode)?;
    let elem_slots = checked_queue_elem_slots(func, pc, opcode, &elem_layout)?;
    verify_allowed_flags(func, pc, opcode, inst.flags, QUEUE_RECV_HAS_OK_FLAG)?;
    verify_known_queue_layout(
        func,
        container_layout_facts,
        pc,
        opcode,
        inst.b,
        "SelectRecv",
        &elem_layout,
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::GcBase],
        "SelectRecv queue",
    )?;
    verify_layout_flows_to_local(func, pc, opcode, inst.a, &elem_layout, "SelectRecv value")?;
    if inst.recv_has_ok() {
        verify_layout(
            func,
            pc,
            opcode,
            checked_slot_offset(func, pc, inst.a, elem_slots, "SelectRecv ok")?,
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
    if inst.c as usize > MAX_CLOSURE_CAPTURE_SLOTS {
        return Err(call_shape_mismatch(
            func,
            pc,
            Opcode::ClosureNew,
            format!(
                "ClosureNew capture count {} exceeds allocation maximum {MAX_CLOSURE_CAPTURE_SLOTS}",
                inst.c
            ),
        ));
    }
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
        &[SlotType::GcBase],
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
        &[SlotType::GcBase],
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
    verify_layout_flows_to_local(
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
    verify_reserved_zero(func, pc, opcode, inst.c.into(), "c")?;
    if inst.call_shape_is_closure() {
        verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::GcBase],
            "closure callee",
        )?;
        let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
        if !ret_layout.is_empty() {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "{opcode:?} closure metadata requires an empty return layout; args={} returns={}",
                    arg_layout.len(),
                    ret_layout.len()
                ),
            ));
        }
        verify_local_layout_flows_to(func, pc, opcode, inst.b, &arg_layout, "closure call args")
    } else {
        let callee_id = inst.call_shape_static_func_id();
        let callee = module.functions.get(callee_id as usize).ok_or_else(|| {
            ModuleVerificationError::MissingFunction {
                func: func.name.clone(),
                pc,
                callee_id,
            }
        })?;
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
    let (assert_kind, target_id, result_layout) = iface_assert_metadata(func, pc, opcode)?;
    verify_allowed_flags(func, pc, opcode, inst.flags, IFACE_ASSERT_HAS_OK_FLAG)?;
    verify_reserved_zero(func, pc, opcode, inst.c.into(), "c")?;
    if assert_kind > 1 {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!("unsupported IfaceAssert kind {assert_kind}"),
        ));
    }
    let has_ok = (inst.flags & IFACE_ASSERT_HAS_OK_FLAG) != 0;
    let dst_slots = u16::try_from(result_layout.len()).map_err(|_| {
        call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "IfaceAssert metadata result layout exceeds u16 slot space: {}",
                result_layout.len()
            ),
        )
    })?;
    let expected_layout = if assert_kind == 1 {
        let target_index = usize::try_from(target_id).map_err(|_| {
            call_shape_mismatch(
                func,
                pc,
                opcode,
                "IfaceAssert interface target id overflow".to_string(),
            )
        })?;
        if target_index >= module.interface_metas.len() {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("IfaceAssert target interface meta {target_id} is missing"),
            ));
        }
        vec![SlotType::Interface0, SlotType::Interface1]
    } else {
        let target_index = usize::try_from(target_id).map_err(|_| {
            call_shape_mismatch(
                func,
                pc,
                opcode,
                "IfaceAssert runtime target id overflow".to_string(),
            )
        })?;
        if target_index >= module.runtime_types.len() {
            return Err(call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("IfaceAssert target runtime type {target_id} is missing"),
            ));
        }
        let target_kind =
            expected_value_kind_for_rttid(module, target_index, "IfaceAssert target runtime type")?;
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
        let target_value_rttid = ValueRttid::try_new(target_id, target_kind).ok_or_else(|| {
            call_shape_mismatch(
                func,
                pc,
                opcode,
                format!("IfaceAssert target runtime type {target_id} is not representable"),
            )
        })?;
        module
            .slot_layout_for_value_rttid(target_value_rttid)
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
    verify_layout_flows_to_local(
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
        let source_value_rttid = ValueRttid::try_new(high, value_kind).ok_or_else(|| {
            call_shape_mismatch(
                func,
                pc,
                opcode,
                format!(
                    "IfaceAssign source runtime type {high} exceeds the 24-bit domain or uses reserved id 0x{INVALID_META_ID:06x}"
                ),
            )
        })?;
        validate_value_rttid_ref(module, source_value_rttid, "IfaceAssign source")
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
    let Some(named_type_id) = module.named_type_id_for_rttid(rttid) else {
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
        | ValueKind::Port
        | ValueKind::Island => verify_layout(
            func,
            pc,
            opcode,
            src_slot,
            &[SlotType::GcBase],
            "IfaceAssign source",
        ),
        ValueKind::Pointer => verify_layout(
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
    verify_reserved_zero(func, pc, opcode, inst.flags.into(), "flags")?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.a,
        &[SlotType::GcBase],
        "GoIsland island",
    )?;
    verify_layout(
        func,
        pc,
        opcode,
        inst.b,
        &[SlotType::GcBase],
        "GoIsland closure",
    )?;
    let (arg_layout, ret_layout) = call_layout(func, pc, opcode)?;
    if !ret_layout.is_empty() {
        return Err(call_shape_mismatch(
            func,
            pc,
            opcode,
            format!(
                "GoIsland metadata layout slots args={} returns={} requires an empty return layout",
                arg_layout.len(),
                ret_layout.len()
            ),
        ));
    }
    verify_local_layout_flows_to(func, pc, opcode, inst.c, &arg_layout, "GoIsland args")
}

#[cfg(test)]
mod tests;
