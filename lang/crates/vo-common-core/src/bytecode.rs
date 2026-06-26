//! Bytecode module definition.

/// Return instruction flag: this return carries a non-nil error return path.
pub const RETURN_FLAG_ERROR_RETURN: u8 = 0x01;
/// Return instruction flag: heap-allocated named returns (need GcRef dereference)
pub const RETURN_FLAG_HEAP_RETURNS: u8 = 0x02;
/// All currently valid Return instruction flag bits.
pub const RETURN_FLAGS_ALLOWED: u8 = RETURN_FLAG_ERROR_RETURN | RETURN_FLAG_HEAP_RETURNS;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ReturnFlags(u8);

impl ReturnFlags {
    pub const NONE: Self = Self(0);
    pub const ERROR_RETURN: Self = Self(RETURN_FLAG_ERROR_RETURN);
    pub const HEAP_RETURNS: Self = Self(RETURN_FLAG_HEAP_RETURNS);
    pub const ALLOWED_BITS: u8 = RETURN_FLAGS_ALLOWED;

    #[inline]
    pub fn from_bits(bits: u8) -> Option<Self> {
        if bits & !Self::ALLOWED_BITS == 0 {
            Some(Self(bits))
        } else {
            None
        }
    }

    #[inline]
    pub const fn stack_return(is_error_return: bool) -> Self {
        if is_error_return {
            Self::ERROR_RETURN
        } else {
            Self::NONE
        }
    }

    #[inline]
    pub const fn heap_returns(is_error_return: bool) -> Self {
        if is_error_return {
            Self(RETURN_FLAG_HEAP_RETURNS | RETURN_FLAG_ERROR_RETURN)
        } else {
            Self::HEAP_RETURNS
        }
    }

    #[inline]
    pub const fn bits(self) -> u8 {
        self.0
    }

    #[inline]
    pub const fn is_error_return(self) -> bool {
        self.0 & RETURN_FLAG_ERROR_RETURN != 0
    }

    #[inline]
    pub const fn has_heap_returns(self) -> bool {
        self.0 & RETURN_FLAG_HEAP_RETURNS != 0
    }
}

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::{collections::BTreeMap, string::String, vec::Vec};

#[cfg(not(feature = "std"))]
use hashbrown::HashMap;
#[cfg(feature = "std")]
use std::collections::{BTreeMap, HashMap};

use crate::debug_info::DebugInfo;
use crate::instruction::Instruction;
use crate::types::{SlotType, ValueKind, ValueMeta, ValueRttid};
use crate::RuntimeType;

pub const MAP_ITER_SLOTS: usize = 7;

/// Bytecode-visible slot layout for the opaque map iterator state.
///
/// Runtime writes this state as raw slots, so the verifier must enforce the
/// hidden GC-bearing slots before the precise stack scanner can trust it.
pub const MAP_ITER_SLOT_TYPES: [SlotType; MAP_ITER_SLOTS] = [
    SlotType::Value,
    SlotType::Value,
    SlotType::GcRef,
    SlotType::Value,
    SlotType::Value,
    SlotType::Value,
    SlotType::GcRef,
];

/// IfaceAssign concrete-source metadata low word meaning "no itab".
///
/// The runtime interface slot still stores `itab_id = 0` for empty-interface
/// values. This sentinel lives only in bytecode metadata so concrete
/// assignments to `interface{}` cannot be confused with a valid compile-time
/// `itab[0]`.
pub const IFACE_ASSIGN_NO_ITAB: u32 = u32::MAX;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(transparent)]
pub struct ExternEffects(u64);

impl ExternEffects {
    pub const NONE: Self = Self(0);
    pub const MAY_YIELD: Self = Self(1 << 0);
    pub const MAY_QUEUE_BLOCK: Self = Self(1 << 1);
    pub const MAY_WAIT_IO_REPLAY: Self = Self(1 << 2);
    pub const MAY_HOST_WAIT: Self = Self(1 << 3);
    pub const MAY_HOST_REPLAY: Self = Self(1 << 4);
    pub const MAY_CALL_CLOSURE_REPLAY: Self = Self(1 << 5);
    pub const UNKNOWN_CONTROL: Self = Self(1 << 6);

    pub const ALLOWED_BITS: u64 = Self::MAY_YIELD.bits()
        | Self::MAY_QUEUE_BLOCK.bits()
        | Self::MAY_WAIT_IO_REPLAY.bits()
        | Self::MAY_HOST_WAIT.bits()
        | Self::MAY_HOST_REPLAY.bits()
        | Self::MAY_CALL_CLOSURE_REPLAY.bits()
        | Self::UNKNOWN_CONTROL.bits();

    #[inline]
    pub const fn from_bits(bits: u64) -> Option<Self> {
        if bits & !Self::ALLOWED_BITS != 0 {
            return None;
        }
        let effects = Self(bits);
        if effects.contains(Self::UNKNOWN_CONTROL) && bits != Self::UNKNOWN_CONTROL.bits() {
            return None;
        }
        Some(effects)
    }

    #[inline]
    pub const fn bits(self) -> u64 {
        self.0
    }

    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    #[inline]
    pub const fn is_subset_of(self, allowed: Self) -> bool {
        allowed.contains(Self::UNKNOWN_CONTROL) || self.0 & !allowed.0 == 0
    }

    #[inline]
    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    #[inline]
    pub const fn intersects(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }

    pub fn names(self) -> impl Iterator<Item = &'static str> {
        [
            (Self::MAY_YIELD, "yield"),
            (Self::MAY_QUEUE_BLOCK, "queue_block"),
            (Self::MAY_WAIT_IO_REPLAY, "wait_io_replay"),
            (Self::MAY_HOST_WAIT, "host_wait"),
            (Self::MAY_HOST_REPLAY, "host_replay"),
            (Self::MAY_CALL_CLOSURE_REPLAY, "call_closure_replay"),
            (Self::UNKNOWN_CONTROL, "unknown_control"),
        ]
        .into_iter()
        .filter_map(move |(effect, name)| self.contains(effect).then_some(name))
    }
}

impl core::ops::BitOr for ExternEffects {
    type Output = Self;

    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl core::ops::BitOrAssign for ExternEffects {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisteredExternSource {
    Builtin,
    Stdlib,
    LinkmeExtension,
    NativeExtension,
    WasmHost,
    WasmExtensionBridge,
    Manual,
    Test,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternJitRoute {
    Intrinsic,
    DirectHelper,
    VmMaterializeBeforeCall,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParamShape {
    Exact { slots: u16 },
    CallSiteVariadic,
}

impl ParamShape {
    #[inline]
    pub const fn exact(slots: u16) -> Self {
        Self::Exact { slots }
    }

    #[inline]
    pub const fn call_site_variadic() -> Self {
        Self::CallSiteVariadic
    }

    #[inline]
    pub const fn exact_slots(&self) -> Option<u16> {
        match self {
            Self::Exact { slots } => Some(*slots),
            Self::CallSiteVariadic => None,
        }
    }

    #[inline]
    pub const fn accepts_slots(&self, slots: u16) -> bool {
        match self {
            Self::Exact { slots: expected } => *expected == slots,
            Self::CallSiteVariadic => true,
        }
    }

    pub fn display_name(&self) -> String {
        match self {
            Self::Exact { slots } => format!("exact({slots})"),
            Self::CallSiteVariadic => "call-site-variadic".to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProviderTrust {
    RuntimeInternal,
    IntrinsicEligible,
    Untrusted,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnShape {
    pub slots: u16,
    pub kinds: Vec<ExtSlotKind>,
    pub slot_types: Vec<SlotType>,
    pub interface_metas: Vec<Option<u32>>,
}

impl ReturnShape {
    pub fn new(slots: u16, kinds: Vec<ExtSlotKind>, slot_types: Vec<SlotType>) -> Self {
        Self {
            slots,
            kinds,
            slot_types,
            interface_metas: Vec::new(),
        }
    }

    pub fn try_new(
        slots: u16,
        kinds: Vec<ExtSlotKind>,
        slot_types: Vec<SlotType>,
    ) -> Result<Self, String> {
        let shape = Self {
            slots,
            kinds,
            slot_types,
            interface_metas: Vec::new(),
        };
        shape.validate_with_label("ReturnShape")?;
        Ok(shape)
    }

    pub fn try_with_slot_types_and_interface_metas(
        slot_types: Vec<SlotType>,
        interface_metas: Vec<Option<u32>>,
    ) -> Result<Self, String> {
        let slots = u16::try_from(slot_types.len()).map_err(|_| {
            format!(
                "ReturnShape slot count exceeds u16::MAX: {} slots",
                slot_types.len()
            )
        })?;
        let shape = Self {
            slots,
            kinds: Vec::new(),
            slot_types,
            interface_metas,
        };
        shape.validate_with_label("ReturnShape")?;
        Ok(shape)
    }

    pub fn slots(slots: u16) -> Self {
        Self {
            slots,
            kinds: Vec::new(),
            slot_types: Vec::new(),
            interface_metas: Vec::new(),
        }
    }

    pub fn with_slot_types(slot_types: Vec<SlotType>) -> Self {
        Self::try_with_slot_types(slot_types).unwrap_or_else(|error| panic!("{error}"))
    }

    pub fn try_with_slot_types(slot_types: Vec<SlotType>) -> Result<Self, String> {
        let slots = u16::try_from(slot_types.len()).map_err(|_| {
            format!(
                "ReturnShape slot count exceeds u16::MAX: {} slots",
                slot_types.len()
            )
        })?;
        Self::try_new(slots, Vec::new(), slot_types)
    }

    pub fn validate_with_label(&self, label: &str) -> Result<(), String> {
        if !self.kinds.is_empty() && self.kinds.len() != self.slots as usize {
            return Err(format!(
                "{label} return slots {} but return kinds has {} slots",
                self.slots,
                self.kinds.len()
            ));
        }
        if !self.slot_types.is_empty() && self.slot_types.len() != self.slots as usize {
            return Err(format!(
                "{label} return slots {} but return slot_types has {} slots",
                self.slots,
                self.slot_types.len()
            ));
        }
        if !self.interface_metas.is_empty() && self.interface_metas.len() != self.slots as usize {
            return Err(format!(
                "{label} return slots {} but return interface_metas has {} slots",
                self.slots,
                self.interface_metas.len()
            ));
        }
        if !self.interface_metas.is_empty() && self.slot_types.is_empty() {
            return Err(format!(
                "{label} return interface metadata requires return slot_types"
            ));
        }
        let has_interface_slots = self
            .slot_types
            .iter()
            .any(|slot_type| matches!(slot_type, SlotType::Interface0 | SlotType::Interface1));
        if has_interface_slots && self.interface_metas.is_empty() {
            return Err(format!(
                "{label} return interface slots must carry expected interface metadata"
            ));
        }
        for (idx, slot_type) in self.slot_types.iter().enumerate() {
            match slot_type {
                SlotType::Interface0 => {
                    if !self.interface_metas.is_empty() && self.interface_metas[idx].is_none() {
                        return Err(format!(
                            "{label} return Interface0 slot {idx} must carry expected interface metadata"
                        ));
                    }
                    if self.slot_types.get(idx + 1) != Some(&SlotType::Interface1) {
                        return Err(format!(
                            "{label} return Interface0 slot {idx} is not followed by Interface1"
                        ));
                    }
                    if !self.interface_metas.is_empty()
                        && self
                            .interface_metas
                            .get(idx + 1)
                            .copied()
                            .flatten()
                            .is_some()
                    {
                        return Err(format!(
                            "{label} return Interface1 slot {} must not carry interface metadata",
                            idx + 1
                        ));
                    }
                }
                SlotType::Interface1 => {
                    if !self.interface_metas.is_empty()
                        && self.interface_metas.get(idx).copied().flatten().is_some()
                    {
                        return Err(format!(
                            "{label} return Interface1 slot {idx} must not carry interface metadata"
                        ));
                    }
                    if idx == 0 || self.slot_types.get(idx - 1) != Some(&SlotType::Interface0) {
                        return Err(format!(
                            "{label} return Interface1 slot {idx} is not preceded by Interface0"
                        ));
                    }
                }
                _ => {
                    if !self.interface_metas.is_empty()
                        && self.interface_metas.get(idx).copied().flatten().is_some()
                    {
                        return Err(format!(
                            "{label} return non-interface slot {idx} must not carry interface metadata"
                        ));
                    }
                }
            }
        }
        Ok(())
    }
}

pub fn ext_slot_kind_matches_slot_type(kind: ExtSlotKind, slot_type: SlotType) -> bool {
    match kind {
        ExtSlotKind::Value => slot_type != SlotType::GcRef,
        ExtSlotKind::Bytes => slot_type == SlotType::GcRef,
    }
}

pub fn ext_slot_kinds_for_slot_types(slot_types: &[SlotType]) -> Vec<ExtSlotKind> {
    slot_types
        .iter()
        .map(|slot_type| {
            if *slot_type == SlotType::GcRef {
                ExtSlotKind::Bytes
            } else {
                ExtSlotKind::Value
            }
        })
        .collect()
}

pub fn known_builtin_extern_param_slot_types(name: &str) -> Option<&'static [SlotType]> {
    match name {
        "dyn_field" => Some(&[
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ]),
        "dyn_index" => Some(&[
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::Value,
            SlotType::Value,
        ]),
        "dyn_set_field" => Some(&[
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::GcRef,
            SlotType::Interface0,
            SlotType::Interface1,
        ]),
        "dyn_set_index_unified" => Some(&[
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::Interface0,
            SlotType::Interface1,
        ]),
        "panic_with_error" => Some(&[SlotType::Interface0, SlotType::Interface1]),
        "vo_copy" => Some(&[SlotType::GcRef, SlotType::GcRef]),
        "vo_slice_append_slice" => Some(&[SlotType::GcRef, SlotType::GcRef, SlotType::Value]),
        "vo_conv_int_str" => Some(&[SlotType::Value]),
        "vo_conv_bytes_str" | "vo_conv_runes_str" | "vo_conv_str_bytes" | "vo_conv_str_runes"
        | "vo_string_to_bytes" | "vo_bytes_to_string" => Some(&[SlotType::GcRef]),
        _ => None,
    }
}

pub fn known_builtin_extern_return_slot_count(name: &str) -> Option<u16> {
    if let Some(slot_types) = known_builtin_extern_fixed_return_slot_types(name) {
        return u16::try_from(slot_types.len()).ok();
    }
    match name {
        "dyn_field" | "dyn_index" => Some(4),
        "dyn_set_field" | "dyn_set_index_unified" => Some(2),
        _ => None,
    }
}

pub fn known_builtin_extern_fixed_return_slot_types(name: &str) -> Option<&'static [SlotType]> {
    match name {
        "vo_copy" => Some(&[SlotType::Value]),
        "vo_slice_append_slice" => Some(&[SlotType::GcRef]),
        "vo_conv_int_str" => Some(&[SlotType::GcRef]),
        "vo_conv_bytes_str" | "vo_conv_runes_str" | "vo_conv_str_bytes" | "vo_conv_str_runes"
        | "vo_string_to_bytes" | "vo_bytes_to_string" => Some(&[SlotType::GcRef]),
        _ => None,
    }
}

pub fn known_builtin_extern_requires_precise_return_layout(name: &str) -> bool {
    known_builtin_extern_fixed_return_slot_types(name).is_some()
        || matches!(
            name,
            "dyn_field" | "dyn_index" | "dyn_set_field" | "dyn_set_index_unified"
        )
}

pub fn validate_ext_param_kinds_with_label(
    params: &ParamShape,
    param_kinds: &[ExtSlotKind],
    label: &str,
) -> Result<(), String> {
    if param_kinds.is_empty() {
        return Ok(());
    }
    let Some(param_slots) = params.exact_slots() else {
        return Err(format!(
            "{label} has param_kinds but params are call-site variadic"
        ));
    };
    if param_kinds.len() != param_slots as usize {
        return Err(format!(
            "{label} exact params {param_slots} but param_kinds has {} slots",
            param_kinds.len()
        ));
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedExternAbi {
    pub id: u32,
    pub name: String,
    pub params: ParamShape,
    pub returns: ReturnShape,
    pub param_kinds: Vec<ExtSlotKind>,
    pub allowed_effects: ExternEffects,
    pub provider_effects: ExternEffects,
    pub effective_effects: ExternEffects,
    pub source: RegisteredExternSource,
    pub provider_identity: u64,
    pub abi_fingerprint: u64,
    pub trust: ProviderTrust,
    pub jit_route: ExternJitRoute,
}

pub type ResolvedExtern = ResolvedExternAbi;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ResolvedExternTable {
    entries: Vec<ResolvedExtern>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedExternTableError {
    message: String,
}

impl ResolvedExternTableError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl core::fmt::Display for ResolvedExternTableError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(&self.message)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ResolvedExternTableError {}

impl ResolvedExternTable {
    pub fn try_new(entries: Vec<ResolvedExtern>) -> Result<Self, ResolvedExternTableError> {
        for (idx, entry) in entries.iter().enumerate() {
            if entry.id as usize != idx {
                return Err(ResolvedExternTableError::new(format!(
                    "resolved extern table entry at index {idx} has id {}",
                    entry.id
                )));
            }
            if let Err(message) = validate_ext_param_kinds_with_label(
                &entry.params,
                &entry.param_kinds,
                &format!("resolved extern '{}'", entry.name),
            ) {
                return Err(ResolvedExternTableError::new(message));
            }
            if let Err(message) = entry
                .returns
                .validate_with_label(&format!("resolved extern '{}'", entry.name))
            {
                return Err(ResolvedExternTableError::new(message));
            }
        }
        Ok(Self { entries })
    }

    pub fn empty() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn get(&self, id: u32) -> Option<&ResolvedExtern> {
        self.entries.get(id as usize)
    }

    pub fn entries(&self) -> &[ResolvedExtern] {
        &self.entries
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TransferType {
    pub meta_raw: u32,
    pub rttid_raw: u32,
    pub slots: u16,
}

/// Per-instruction metadata attached to a bytecode instruction.
///
/// The instruction stream remains the VM source of truth. This table gives the
/// VM and JIT typed, per-PC layout facts that are awkward to recover from
/// temporary metadata registers after optimization and control-flow merging.
/// Strict JIT-only policy for metadata kind, lowering capability, loop/OSR
/// facts, and helper ABI stays in `vo-jit`.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum JitInstructionMetadata {
    #[default]
    None,
    ElemLayout {
        elem_bytes: u32,
        needs_sign_extend: bool,
        slot_layout: Vec<SlotType>,
    },
    MapGet {
        key_layout: Vec<SlotType>,
        val_layout: Vec<SlotType>,
        has_ok: bool,
    },
    MapNew {
        key_layout: Vec<SlotType>,
        val_layout: Vec<SlotType>,
    },
    MapSet {
        key_layout: Vec<SlotType>,
        val_layout: Vec<SlotType>,
    },
    MapDelete {
        key_layout: Vec<SlotType>,
    },
    PtrLayout {
        value_layout: Vec<SlotType>,
    },
    SlotLayout {
        elem_layout: Vec<SlotType>,
    },
    CallLayout {
        arg_layout: Vec<SlotType>,
        ret_layout: Vec<SlotType>,
    },
    CallIfaceLayout {
        iface_meta_id: u32,
        arg_layout: Vec<SlotType>,
        ret_layout: Vec<SlotType>,
    },
    CallExternLayout {
        arg_layout: Vec<SlotType>,
        ret_layout: Vec<SlotType>,
    },
    QueueLayout {
        elem_layout: Vec<SlotType>,
    },
    MapIterNext {
        key_layout: Vec<SlotType>,
        val_layout: Vec<SlotType>,
    },
    IfaceAssertLayout {
        result_layout: Vec<SlotType>,
    },
    LoopEnd {
        end_pc: u32,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub param_count: u16,
    pub param_slots: u16,
    pub local_slots: u16,
    pub gc_scan_slots: u16,
    pub ret_slots: u16,
    pub ret_slot_types: Vec<SlotType>,
    /// Receiver slots for methods (0 for functions, >0 for methods)
    /// Used by CallIface to know how many slots to copy from interface data
    pub recv_slots: u16,
    /// Number of GcRefs for heap-allocated named returns (0 = no heap returns).
    /// Used by panic recovery to return named return values after recover().
    pub heap_ret_gcref_count: u16,
    /// Starting slot for heap-allocated named return GcRefs.
    /// GcRefs are in slots heap_ret_gcref_start..heap_ret_gcref_start+heap_ret_gcref_count.
    pub heap_ret_gcref_start: u16,
    /// Slot count for each heap-allocated named return (parallel to GcRefs).
    /// Length equals heap_ret_gcref_count. Empty if no heap returns.
    pub heap_ret_slots: Vec<u16>,
    /// True if this is a closure (anonymous function) that expects closure ref in slot 0.
    /// Named functions wrapped as closures have this set to false.
    pub is_closure: bool,
    /// Slot offset of error return value within return slots, or -1 if function doesn't return error.
    /// Used for errdefer runtime check: errdefer runs when returning non-nil error.
    pub error_ret_slot: i16,
    /// True if function contains DeferPush or ErrDeferPush instructions.
    /// Used by JIT to route calls through VM (ensuring real CallFrame exists for defer).
    pub has_defer: bool,
    /// True if function contains Call, CallClosure, or CallIface instructions.
    /// Used by JIT to determine call routing (direct JIT vs VM fallback) and
    /// IC leaf optimization: a function is leaf IFF !has_calls && !has_call_extern.
    pub has_calls: bool,
    /// True if function contains CallExtern instructions.
    /// CallExtern can leave JIT through materialization or VM-owned extern
    /// suspend payloads, so IC leaf optimization must NOT skip
    /// ctx.jit_bp/fiber_sp update for such functions.
    pub has_call_extern: bool,
    pub code: Vec<Instruction>,
    /// Optional per-instruction metadata for JIT lowering.
    ///
    /// Length must match `code.len()` for versioned bytecode that carries the
    /// table. Pre-metadata bytecode deserializes with a `None`-filled table.
    pub jit_metadata: Vec<JitInstructionMetadata>,
    pub slot_types: Vec<SlotType>,
    pub borrowed_scan_slots_prefix: Vec<u16>,
    /// Capture types for cross-island transfer (closures only).
    /// Each entry: (ValueMeta raw, slot_count) for the captured variable's inner type.
    /// Empty for non-closure functions.
    pub capture_types: Vec<TransferType>,
    /// SlotTypes for closure captures, used by GC to scan closure objects.
    /// Length = total slots across all captures (e.g., 2 for an interface capture).
    /// For regular closures, all entries are GcRef (escape boxes).
    /// For method value wrappers, may contain Interface0/Interface1.
    pub capture_slot_types: Vec<SlotType>,
    /// Parameter types for cross-island transfer.
    /// Each entry: (ValueMeta raw, slot_count) for one parameter.
    /// Empty if function has no parameters or types not needed.
    pub param_types: Vec<TransferType>,
}

impl FunctionDef {
    /// Scan bytecode to compute (has_calls, has_call_extern).
    /// Used during construction and deserialization to avoid duplicating this logic.
    pub fn compute_call_flags(code: &[Instruction]) -> (bool, bool) {
        use crate::instruction::Opcode;
        let mut has_calls = false;
        let mut has_call_extern = false;
        for inst in code {
            match inst.opcode() {
                Opcode::Call | Opcode::CallClosure | Opcode::CallIface => has_calls = true,
                Opcode::CallExtern => has_call_extern = true,
                _ => {}
            }
            if has_calls && has_call_extern {
                break;
            }
        }
        (has_calls, has_call_extern)
    }

    pub fn compute_gc_scan_slots(slot_types: &[SlotType]) -> u16 {
        let mut scan_slots = 0usize;
        for (idx, slot_type) in slot_types.iter().enumerate() {
            match slot_type {
                SlotType::GcRef => scan_slots = idx + 1,
                SlotType::Interface0 => scan_slots = idx + 2,
                _ => {}
            }
        }
        scan_slots as u16
    }

    pub fn compute_borrowed_scan_slots_prefix(slot_types: &[SlotType]) -> Vec<u16> {
        let mut prefix = Vec::with_capacity(slot_types.len() + 1);
        let mut scan_slots = 0u16;
        prefix.push(0);
        for (idx, slot_type) in slot_types.iter().enumerate() {
            match slot_type {
                SlotType::GcRef => scan_slots = (idx + 1) as u16,
                SlotType::Interface0 => scan_slots = (idx + 2) as u16,
                _ => {}
            }
            prefix.push(scan_slots);
        }
        prefix
    }

    #[inline]
    pub fn scan_slots_before_borrowed_start(&self, borrowed_start: u16) -> u16 {
        let end = borrowed_start as usize;
        assert!(
            end <= self.slot_types.len(),
            "scan_slots_before_borrowed_start: borrowed_start {} exceeds slot layout length {}",
            borrowed_start,
            self.slot_types.len()
        );
        self.borrowed_scan_slots_prefix[end]
    }
}

/// How a single slot in an ext function boundary is encoded across the WASM ext-bridge.
///
/// Input (Vo → WASM): Value = 8-byte LE u64; Bytes = [u32 len][data].
/// Output (WASM → Vo): self-describing tagged stream (see ext_bridge.rs for tag constants).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ExtSlotKind {
    /// Raw 64-bit integer / float / bool / uint32.  Serialised as 8 bytes LE.
    Value = 0,
    /// GC reference to string or []byte.  Serialised as [u32 LE len][bytes].
    Bytes = 1,
}

impl ExtSlotKind {
    #[inline]
    pub fn from_u8(v: u8) -> Self {
        if v == 1 {
            Self::Bytes
        } else {
            Self::Value
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternDef {
    pub name: String,
    pub params: ParamShape,
    pub returns: ReturnShape,
    /// Upper bound of control-flow effects this bytecode extern permits.
    pub allowed_effects: ExternEffects,
    /// Ext-bridge encoding kind for each parameter slot.
    /// Non-empty only for WASM ext externs; empty for native/builtin externs.
    /// When non-empty, the ext_bridge uses tagged binary protocol for both
    /// input (using these kinds) and output (self-describing tagged stream).
    pub param_kinds: Vec<ExtSlotKind>,
}

impl ExternDef {
    pub fn new(
        name: String,
        params: ParamShape,
        returns: ReturnShape,
        allowed_effects: ExternEffects,
        param_kinds: Vec<ExtSlotKind>,
    ) -> Self {
        Self {
            name,
            params,
            returns,
            allowed_effects,
            param_kinds,
        }
    }

    pub fn call_site_variadic(
        name: String,
        ret_slots: u16,
        allowed_effects: ExternEffects,
        param_kinds: Vec<ExtSlotKind>,
    ) -> Self {
        Self::new(
            name,
            ParamShape::CallSiteVariadic,
            ReturnShape::slots(ret_slots),
            allowed_effects,
            param_kinds,
        )
    }

    pub fn exact(
        name: String,
        param_slots: u16,
        returns: ReturnShape,
        allowed_effects: ExternEffects,
        param_kinds: Vec<ExtSlotKind>,
    ) -> Self {
        Self::new(
            name,
            ParamShape::Exact { slots: param_slots },
            returns,
            allowed_effects,
            param_kinds,
        )
    }

    #[inline]
    pub fn ret_slots(&self) -> u16 {
        self.returns.slots
    }
}

#[derive(Debug, Clone)]
pub struct GlobalDef {
    pub name: String,
    pub slots: u16,
    pub value_kind: u8,
    pub meta_id: u32,
    pub slot_types: Vec<SlotType>,
}

/// Metadata for a single struct field.
#[derive(Debug, Clone)]
pub struct FieldMeta {
    pub name: String,
    pub offset: u16,
    pub slot_count: u16,
    pub type_info: ValueRttid,
    /// Whether this field is embedded (anonymous struct).
    pub embedded: bool,
    /// The field tag (e.g. `json:"name" toml:"other"`), if any.
    pub tag: Option<String>,
}

#[derive(Debug, Clone)]
pub struct StructMeta {
    pub slot_types: Vec<SlotType>,
    pub fields: Vec<FieldMeta>,
    /// Field name -> field index for O(1) lookup.
    pub field_index: HashMap<String, usize>,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub func_id: u32,
    pub is_pointer_receiver: bool,
    pub receiver_is_iface_boxed: bool,
    pub signature_rttid: u32,
}

impl MethodInfo {
    pub fn iface_receiver_slot_type_for_source_kind(
        &self,
        source_kind: ValueKind,
    ) -> Result<SlotType, &'static str> {
        if self.is_pointer_receiver && source_kind != ValueKind::Pointer {
            return Err("pointer receiver target requires pointer interface receiver");
        }
        if self.receiver_is_iface_boxed {
            Ok(SlotType::GcRef)
        } else {
            Ok(slot_type_for_value_kind(source_kind))
        }
    }
}

#[derive(Debug, Clone)]
pub struct NamedTypeMeta {
    pub name: String,
    pub underlying_meta: ValueMeta,
    pub underlying_rttid: ValueRttid,
    pub methods: BTreeMap<String, MethodInfo>,
}

impl StructMeta {
    #[inline]
    pub fn slot_count(&self) -> u16 {
        u16::try_from(self.slot_types.len()).expect("verified StructMeta slot count must fit u16")
    }

    /// Get field by name (O(1) lookup).
    #[inline]
    pub fn get_field(&self, name: &str) -> Option<&FieldMeta> {
        self.field_index.get(name).map(|&idx| &self.fields[idx])
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceMethodMeta {
    pub name: String,
    pub signature_rttid: u32,
}

#[derive(Debug, Clone)]
pub struct InterfaceMeta {
    pub name: String,
    pub method_names: Vec<String>,
    pub methods: Vec<InterfaceMethodMeta>,
}

/// Itab: interface method table for a specific target interface.
#[derive(Debug, Clone, Default)]
pub struct Itab {
    pub iface_meta_id: u32,
    pub methods: Vec<u32>,
}

/// Pre-computed type IDs for well-known types (errors.Error, etc.)
/// Filled at codegen time to avoid runtime lookups.
#[derive(Debug, Clone, Default)]
pub struct WellKnownTypes {
    /// errors.Error named_type_id
    pub error_named_type_id: Option<u32>,
    /// error interface meta_id
    pub error_iface_meta_id: Option<u32>,
    /// *errors.Error rttid
    pub error_ptr_rttid: Option<u32>,
    /// errors.Error struct_meta_id
    pub error_struct_meta_id: Option<u32>,
    /// Field offsets in errors.Error: [msg, cause]
    pub error_field_offsets: Option<[u16; 2]>,

    // Builtin protocol interface meta IDs for dynamic access
    /// AttrObject protocol iface_meta_id
    pub attr_object_iface_id: Option<u32>,
    /// SetAttrObject protocol iface_meta_id
    pub set_attr_object_iface_id: Option<u32>,
    /// IndexObject protocol iface_meta_id
    pub index_object_iface_id: Option<u32>,
    /// SetIndexObject protocol iface_meta_id
    pub set_index_object_iface_id: Option<u32>,
    /// CallObject protocol iface_meta_id
    pub call_object_iface_id: Option<u32>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub struct_metas: Vec<StructMeta>,
    pub interface_metas: Vec<InterfaceMeta>,
    pub named_type_metas: Vec<NamedTypeMeta>,
    pub runtime_types: Vec<RuntimeType>, // rttid -> RuntimeType
    pub itabs: Vec<Itab>,                // compile-time built itabs
    pub well_known: WellKnownTypes,      // pre-computed type IDs
    pub constants: Vec<Constant>,
    pub globals: Vec<GlobalDef>,
    pub functions: Vec<FunctionDef>,
    pub externs: Vec<ExternDef>,
    pub entry_func: u32,
    pub island_init_func: u32,
    pub debug_info: DebugInfo,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            struct_metas: Vec::new(),
            interface_metas: Vec::new(),
            named_type_metas: Vec::new(),
            runtime_types: Vec::new(),
            itabs: Vec::new(),
            well_known: WellKnownTypes::default(),
            constants: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            externs: Vec::new(),
            entry_func: 0,
            island_init_func: 0,
            debug_info: DebugInfo::new(),
        }
    }

    pub fn canonical_value_meta_for_value_rttid(
        &self,
        value_rttid: ValueRttid,
    ) -> Option<ValueMeta> {
        let kind = value_rttid.value_kind();
        let limit = self.runtime_types.len() + self.named_type_metas.len() + 1;
        if self.expected_value_kind_for_value_rttid(value_rttid, 0, limit)? != kind {
            return None;
        }
        match kind {
            ValueKind::Struct => self
                .canonical_struct_meta_id(value_rttid, 0, limit)
                .map(|meta_id| ValueMeta::new(meta_id, kind)),
            ValueKind::Pointer => self
                .canonical_pointer_meta_id(value_rttid, 0, limit)
                .map(|meta_id| ValueMeta::new(meta_id, kind)),
            ValueKind::Interface => self
                .canonical_interface_meta_id(value_rttid, 0, limit)
                .map(|meta_id| ValueMeta::new(meta_id, kind)),
            ValueKind::Array => Some(ValueMeta::new(value_rttid.rttid(), kind)),
            _ => Some(ValueMeta::new(0, kind)),
        }
    }

    pub fn slot_count_for_value_rttid(&self, value_rttid: ValueRttid) -> Option<usize> {
        self.slot_layout_for_value_rttid(value_rttid)
            .map(|layout| layout.len())
    }

    pub fn slot_layout_for_value_rttid(&self, value_rttid: ValueRttid) -> Option<Vec<SlotType>> {
        let limit = self.runtime_types.len() + self.named_type_metas.len() + 1;
        self.slot_layout_for_value_rttid_inner(value_rttid, 0, limit)
    }

    fn slot_layout_for_value_rttid_inner(
        &self,
        value_rttid: ValueRttid,
        depth: usize,
        limit: usize,
    ) -> Option<Vec<SlotType>> {
        if depth > limit {
            return None;
        }
        if self.expected_value_kind_for_value_rttid(value_rttid, depth, limit)?
            != value_rttid.value_kind()
        {
            return None;
        }
        match self.runtime_types.get(value_rttid.rttid() as usize)? {
            RuntimeType::Basic(kind) => {
                let mut layout = Vec::with_capacity(1);
                layout.push(slot_type_for_value_kind(*kind));
                Some(layout)
            }
            RuntimeType::Pointer(elem) => {
                self.canonical_struct_meta_id(*elem, depth + 1, limit)?;
                let mut layout = Vec::with_capacity(1);
                layout.push(SlotType::GcRef);
                Some(layout)
            }
            RuntimeType::Slice(_)
            | RuntimeType::Map { .. }
            | RuntimeType::Chan { .. }
            | RuntimeType::Port { .. }
            | RuntimeType::Func { .. }
            | RuntimeType::Island => {
                let mut layout = Vec::with_capacity(1);
                layout.push(SlotType::GcRef);
                Some(layout)
            }
            RuntimeType::Interface { .. } => {
                let mut layout = Vec::with_capacity(2);
                layout.push(SlotType::Interface0);
                layout.push(SlotType::Interface1);
                Some(layout)
            }
            RuntimeType::Struct { meta_id, .. } => self
                .struct_metas
                .get(*meta_id as usize)
                .map(|meta| meta.slot_types.clone()),
            RuntimeType::Array { len, elem } => {
                let elem_layout =
                    self.slot_layout_for_value_rttid_inner(*elem, depth + 1, limit)?;
                let total = (*len as usize).checked_mul(elem_layout.len())?;
                let mut layout = Vec::with_capacity(total);
                for _ in 0..*len {
                    layout.extend_from_slice(&elem_layout);
                }
                Some(layout)
            }
            RuntimeType::Named { id, .. } => {
                let named = self.named_type_metas.get(*id as usize)?;
                self.slot_layout_for_value_rttid_inner(named.underlying_rttid, depth + 1, limit)
            }
            RuntimeType::Tuple(elems) => {
                let mut layout = Vec::new();
                for elem in elems {
                    let elem_layout =
                        self.slot_layout_for_value_rttid_inner(*elem, depth + 1, limit)?;
                    layout.extend_from_slice(&elem_layout);
                }
                Some(layout)
            }
        }
    }

    fn canonical_struct_meta_id(
        &self,
        value_rttid: ValueRttid,
        depth: usize,
        limit: usize,
    ) -> Option<u32> {
        if depth > limit {
            return None;
        }
        if self.expected_value_kind_for_value_rttid(value_rttid, depth, limit)?
            != value_rttid.value_kind()
        {
            return None;
        }
        match self.runtime_types.get(value_rttid.rttid() as usize)? {
            RuntimeType::Struct { meta_id, .. } => Some(*meta_id),
            RuntimeType::Named { id, .. } => {
                let named = self.named_type_metas.get(*id as usize)?;
                self.canonical_struct_meta_id(named.underlying_rttid, depth + 1, limit)
            }
            _ => None,
        }
    }

    fn canonical_pointer_meta_id(
        &self,
        value_rttid: ValueRttid,
        depth: usize,
        limit: usize,
    ) -> Option<u32> {
        if depth > limit {
            return None;
        }
        if self.expected_value_kind_for_value_rttid(value_rttid, depth, limit)?
            != value_rttid.value_kind()
        {
            return None;
        }
        match self.runtime_types.get(value_rttid.rttid() as usize)? {
            RuntimeType::Pointer(elem) => self.canonical_struct_meta_id(*elem, depth + 1, limit),
            RuntimeType::Named { id, .. } => {
                let named = self.named_type_metas.get(*id as usize)?;
                self.canonical_pointer_meta_id(named.underlying_rttid, depth + 1, limit)
            }
            _ => None,
        }
    }

    fn canonical_interface_meta_id(
        &self,
        value_rttid: ValueRttid,
        depth: usize,
        limit: usize,
    ) -> Option<u32> {
        if depth > limit {
            return None;
        }
        if self.expected_value_kind_for_value_rttid(value_rttid, depth, limit)?
            != value_rttid.value_kind()
        {
            return None;
        }
        match self.runtime_types.get(value_rttid.rttid() as usize)? {
            RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
            RuntimeType::Named { id, .. } => {
                let named = self.named_type_metas.get(*id as usize)?;
                self.canonical_interface_meta_id(named.underlying_rttid, depth + 1, limit)
            }
            _ => None,
        }
    }

    fn expected_value_kind_for_value_rttid(
        &self,
        value_rttid: ValueRttid,
        depth: usize,
        limit: usize,
    ) -> Option<ValueKind> {
        if depth > limit {
            return None;
        }
        match self.runtime_types.get(value_rttid.rttid() as usize)? {
            RuntimeType::Basic(kind) => Some(*kind),
            RuntimeType::Named { id, .. } => {
                let named = self.named_type_metas.get(*id as usize)?;
                self.expected_value_kind_for_value_rttid(named.underlying_rttid, depth + 1, limit)
            }
            RuntimeType::Pointer(_) => Some(ValueKind::Pointer),
            RuntimeType::Array { .. } => Some(ValueKind::Array),
            RuntimeType::Slice(_) => Some(ValueKind::Slice),
            RuntimeType::Map { .. } => Some(ValueKind::Map),
            RuntimeType::Chan { .. } => Some(ValueKind::Channel),
            RuntimeType::Port { .. } => Some(ValueKind::Port),
            RuntimeType::Func { .. } => Some(ValueKind::Closure),
            RuntimeType::Struct { .. } => Some(ValueKind::Struct),
            RuntimeType::Interface { .. } => Some(ValueKind::Interface),
            RuntimeType::Tuple(_) => Some(ValueKind::Void),
            RuntimeType::Island => Some(ValueKind::Island),
        }
    }
}

#[inline]
pub fn slot_type_for_value_kind(kind: ValueKind) -> SlotType {
    match kind {
        ValueKind::Float32 | ValueKind::Float64 => SlotType::Float,
        ValueKind::String
        | ValueKind::Slice
        | ValueKind::Map
        | ValueKind::Closure
        | ValueKind::Channel
        | ValueKind::Port
        | ValueKind::Pointer
        | ValueKind::Island => SlotType::GcRef,
        ValueKind::Interface => SlotType::Interface0,
        _ => SlotType::Value,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn function_with_slot_types(slot_types: Vec<SlotType>) -> FunctionDef {
        FunctionDef {
            name: "test".to_string(),
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
    #[should_panic(
        expected = "scan_slots_before_borrowed_start: borrowed_start 2 exceeds slot layout length 1"
    )]
    fn scan_slots_before_borrowed_start_rejects_out_of_layout_start() {
        let func = function_with_slot_types(vec![SlotType::Value]);
        let _ = func.scan_slots_before_borrowed_start(2);
    }

    #[test]
    fn return_flags_reject_unknown_bits() {
        let strict = ReturnFlags::heap_returns(true);
        assert_eq!(
            strict.bits(),
            RETURN_FLAG_HEAP_RETURNS | RETURN_FLAG_ERROR_RETURN
        );
        assert!(strict.has_heap_returns());
        assert!(strict.is_error_return());

        assert!(ReturnFlags::from_bits(0x04).is_none());
    }

    #[test]
    fn extern_effects_are_explicit_and_validated() {
        let effects = ExternEffects::MAY_WAIT_IO_REPLAY.union(ExternEffects::MAY_HOST_REPLAY);
        assert_eq!(
            effects.names().collect::<Vec<_>>(),
            vec!["wait_io_replay", "host_replay"]
        );
        assert!(ExternEffects::MAY_WAIT_IO_REPLAY.is_subset_of(effects));
        assert!(!ExternEffects::MAY_HOST_WAIT.is_subset_of(effects));
        assert!(ExternEffects::MAY_HOST_WAIT.is_subset_of(ExternEffects::UNKNOWN_CONTROL));

        assert!(ExternEffects::from_bits(ExternEffects::ALLOWED_BITS << 1).is_none());
        assert!(ExternEffects::from_bits(
            ExternEffects::UNKNOWN_CONTROL.bits() | ExternEffects::MAY_YIELD.bits()
        )
        .is_none());
    }

    #[test]
    fn return_shape_rejects_slot_type_width_overflow() {
        let err = ReturnShape::try_with_slot_types(vec![SlotType::Value; 65_536])
            .expect_err("wide return shape must not truncate");
        assert_eq!(err, "ReturnShape slot count exceeds u16::MAX: 65536 slots");
    }

    #[test]
    fn return_shape_constructor_rejects_invalid_interface_slot_pairs_048() {
        let err = ReturnShape::try_with_slot_types_and_interface_metas(
            vec![SlotType::Interface0],
            vec![Some(0)],
        )
        .expect_err("ReturnShape constructor must enforce interface pair layout");
        assert!(err.contains("Interface0 slot 0 is not followed by Interface1"));

        let err = ReturnShape::try_with_slot_types_and_interface_metas(
            vec![SlotType::Interface1],
            vec![None],
        )
        .expect_err("ReturnShape constructor must reject orphan Interface1");
        assert!(err.contains("Interface1 slot 0 is not preceded by Interface0"));
    }

    #[test]
    fn return_shape_constructor_rejects_invalid_interface_metadata_060() {
        let err =
            ReturnShape::try_with_slot_types(vec![SlotType::Interface0, SlotType::Interface1])
                .expect_err("layout-only interface returns must be rejected");
        assert!(err.contains("return interface slots must carry expected interface metadata"));

        let err = ReturnShape::try_with_slot_types_and_interface_metas(
            vec![SlotType::Interface0, SlotType::Interface1],
            vec![None, None],
        )
        .expect_err("Interface0 return slots must carry expected interface metadata");
        assert!(err.contains("Interface0 slot 0 must carry expected interface metadata"));

        let err = ReturnShape::try_with_slot_types_and_interface_metas(
            vec![SlotType::Interface0, SlotType::Interface1],
            vec![Some(0), Some(0)],
        )
        .expect_err("Interface1 return slots must not carry interface metadata");
        assert!(err.contains("Interface1 slot 1 must not carry interface metadata"));

        let err = ReturnShape::try_with_slot_types_and_interface_metas(
            vec![SlotType::Value],
            vec![Some(0)],
        )
        .expect_err("non-interface return slots must not carry interface metadata");
        assert!(err.contains("non-interface slot 0 must not carry interface metadata"));

        let mut shape = ReturnShape::slots(2);
        shape.interface_metas = vec![Some(0), None];
        let err = shape
            .validate_with_label("ReturnShape")
            .expect_err("slots-only returns must not carry interface metadata");
        assert!(err.contains("return interface metadata requires return slot_types"));
    }

    #[test]
    fn resolved_extern_table_rejects_non_indexed_ids() {
        let entries = vec![ResolvedExtern {
            id: 1,
            name: "x".to_string(),
            params: ParamShape::Exact { slots: 0 },
            returns: ReturnShape::slots(0),
            param_kinds: Vec::new(),
            allowed_effects: ExternEffects::NONE,
            provider_effects: ExternEffects::NONE,
            effective_effects: ExternEffects::NONE,
            source: RegisteredExternSource::Test,
            provider_identity: 0,
            abi_fingerprint: 0,
            trust: ProviderTrust::Untrusted,
            jit_route: ExternJitRoute::DirectHelper,
        }];

        let err = ResolvedExternTable::try_new(entries).unwrap_err();

        assert!(err.message().contains("index 0 has id 1"));
    }
}
