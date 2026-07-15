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
use alloc::{
    collections::BTreeMap,
    string::{String, ToString},
    vec,
    vec::Vec,
};

#[cfg(not(feature = "std"))]
use hashbrown::{HashMap, HashSet};
#[cfg(feature = "std")]
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::debug_info::DebugInfo;
use crate::instruction::Instruction;
use crate::types::{SlotType, ValueKind, ValueMeta, ValueRttid, INVALID_META_ID};
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
    /// The provider may terminate the entire guest session with an exit code.
    pub const MAY_EXIT: Self = Self(1 << 7);

    pub const ALLOWED_BITS: u64 = Self::MAY_YIELD.bits()
        | Self::MAY_QUEUE_BLOCK.bits()
        | Self::MAY_WAIT_IO_REPLAY.bits()
        | Self::MAY_HOST_WAIT.bits()
        | Self::MAY_HOST_REPLAY.bits()
        | Self::MAY_CALL_CLOSURE_REPLAY.bits()
        | Self::UNKNOWN_CONTROL.bits()
        | Self::MAY_EXIT.bits();

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
            (Self::MAY_EXIT, "exit"),
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
    if let Some(layout) = known_math_intrinsic_param_slot_types(name) {
        return Some(layout);
    }
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
        "vo_conv_bytes_str" | "vo_conv_runes_str" | "vo_conv_str_bytes" | "vo_conv_str_runes" => {
            Some(&[SlotType::GcRef])
        }
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
    if known_math_intrinsic_param_slot_types(name).is_some() {
        return Some(&[SlotType::Float]);
    }
    match name {
        "vo_copy" => Some(&[SlotType::Value]),
        "vo_slice_append_slice" => Some(&[SlotType::GcRef]),
        "vo_conv_int_str" => Some(&[SlotType::GcRef]),
        "vo_conv_bytes_str" | "vo_conv_runes_str" | "vo_conv_str_bytes" | "vo_conv_str_runes" => {
            Some(&[SlotType::GcRef])
        }
        _ => None,
    }
}

fn known_math_intrinsic_param_slot_types(name: &str) -> Option<&'static [SlotType]> {
    let crate::extern_key::ExternNameClass::Canonical(key) =
        crate::extern_key::classify_extern_name(name).ok()?
    else {
        return None;
    };
    match (key.package(), key.function()) {
        ("math", "Sqrt" | "Floor" | "Ceil" | "Trunc") => Some(&[SlotType::Float]),
        ("math", "FMA") => Some(&[SlotType::Float, SlotType::Float, SlotType::Float]),
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
    /// Exact authoritative `vo.mod` ModulePath for extension-owned providers.
    /// Runtime/stdlib/host providers have no extension module owner.
    pub provider_module_owner: Option<String>,
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
        let mut first_by_name = BTreeMap::<&str, &ResolvedExtern>::new();
        let mut name_by_provider_identity = BTreeMap::<u64, &str>::new();
        for (idx, entry) in entries.iter().enumerate() {
            let expected_id = u32::try_from(idx).map_err(|_| {
                ResolvedExternTableError::new(format!(
                    "resolved extern table index {idx} exceeds the u32 extern id domain"
                ))
            })?;
            if entry.id != expected_id {
                return Err(ResolvedExternTableError::new(format!(
                    "resolved extern table entry at index {idx} has id {}",
                    entry.id
                )));
            }
            let name_class =
                crate::extern_key::classify_extern_name(&entry.name).map_err(|error| {
                    ResolvedExternTableError::new(format!(
                        "resolved extern '{}' has invalid bytecode identity: {error}",
                        entry.name
                    ))
                })?;
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
            for (field, effects) in [
                ("allowed_effects", entry.allowed_effects),
                ("provider_effects", entry.provider_effects),
                ("effective_effects", entry.effective_effects),
            ] {
                if ExternEffects::from_bits(effects.bits()).is_none() {
                    return Err(ResolvedExternTableError::new(format!(
                        "resolved extern '{}' has invalid {field} bits 0x{:x}",
                        entry.name,
                        effects.bits()
                    )));
                }
            }
            if !entry.provider_effects.is_subset_of(entry.allowed_effects) {
                return Err(ResolvedExternTableError::new(format!(
                    "resolved extern '{}' provider effects 0x{:x} exceed allowed effects 0x{:x}",
                    entry.name,
                    entry.provider_effects.bits(),
                    entry.allowed_effects.bits()
                )));
            }
            // Resolution accepts a provider only when its complete effect set
            // fits the declaration, so the intersection authority is exactly
            // the provider set. A caller-supplied narrower or broader value
            // would make interpreter/JIT outcome classification disagree.
            if entry.effective_effects != entry.provider_effects {
                return Err(ResolvedExternTableError::new(format!(
                    "resolved extern '{}' effective effects 0x{:x} do not equal provider effects 0x{:x}",
                    entry.name,
                    entry.effective_effects.bits(),
                    entry.provider_effects.bits()
                )));
            }
            match entry.jit_route {
                ExternJitRoute::Intrinsic => {
                    if entry.trust != ProviderTrust::IntrinsicEligible {
                        return Err(ResolvedExternTableError::new(format!(
                            "resolved extern '{}' selects the intrinsic JIT route without intrinsic-eligible trust",
                            entry.name
                        )));
                    }
                    if !entry.effective_effects.is_empty() {
                        return Err(ResolvedExternTableError::new(format!(
                            "resolved extern '{}' selects the intrinsic JIT route with effects 0x{:x}",
                            entry.name,
                            entry.effective_effects.bits()
                        )));
                    }
                    if entry.source != RegisteredExternSource::Builtin {
                        return Err(ResolvedExternTableError::new(format!(
                            "resolved extern '{}' selects the intrinsic JIT route from non-builtin source {:?}",
                            entry.name, entry.source
                        )));
                    }
                    if !resolved_intrinsic_abi_matches(name_class, entry) {
                        return Err(ResolvedExternTableError::new(format!(
                            "resolved extern '{}' selects the intrinsic JIT route with an unsupported name or ABI",
                            entry.name
                        )));
                    }
                }
                ExternJitRoute::DirectHelper
                    if entry
                        .effective_effects
                        .contains(ExternEffects::UNKNOWN_CONTROL) =>
                {
                    return Err(ResolvedExternTableError::new(format!(
                        "resolved extern '{}' with unknown control effects must materialize before JIT dispatch",
                        entry.name
                    )));
                }
                ExternJitRoute::DirectHelper | ExternJitRoute::VmMaterializeBeforeCall => {}
            }
            if entry.provider_identity == 0 {
                return Err(ResolvedExternTableError::new(format!(
                    "resolved extern '{}' has reserved provider identity 0",
                    entry.name
                )));
            }
            if let Some(previous_name) =
                name_by_provider_identity.insert(entry.provider_identity, &entry.name)
            {
                if previous_name != entry.name {
                    return Err(ResolvedExternTableError::new(format!(
                        "resolved externs '{}' and '{}' reuse provider identity {}",
                        previous_name, entry.name, entry.provider_identity
                    )));
                }
            }
            let requires_module_owner = matches!(
                entry.source,
                RegisteredExternSource::NativeExtension
                    | RegisteredExternSource::LinkmeExtension
                    | RegisteredExternSource::WasmExtensionBridge
            );
            match entry.provider_module_owner.as_deref() {
                None if requires_module_owner => {
                    return Err(ResolvedExternTableError::new(format!(
                        "resolved extern '{}' from {:?} is missing its provider module owner",
                        entry.name, entry.source
                    )));
                }
                Some(owner) if !requires_module_owner => {
                    return Err(ResolvedExternTableError::new(format!(
                        "resolved extern '{}' from {:?} cannot declare extension module owner '{}'",
                        entry.name, entry.source, owner
                    )));
                }
                Some(owner) => {
                    crate::extern_key::validate_canonical_module_owner(owner).map_err(|error| {
                        ResolvedExternTableError::new(format!(
                            "resolved extern '{}' has invalid provider module owner '{}': {}",
                            entry.name, owner, error
                        ))
                    })?;
                    let crate::extern_key::ExternNameClass::Canonical(key) = name_class else {
                        return Err(ResolvedExternTableError::new(format!(
                            "resolved extern '{}' owned by extension module '{}' must use a canonical source identity",
                            entry.name, owner
                        )));
                    };
                    if !key.is_owned_by_module(owner) {
                        return Err(ResolvedExternTableError::new(format!(
                            "resolved extern '{}' is outside provider module owner '{}'",
                            entry.name, owner
                        )));
                    }
                }
                None => {}
            }

            if let Some(previous) = first_by_name.get(entry.name.as_str()).copied() {
                if previous.source != entry.source
                    || previous.provider_module_owner != entry.provider_module_owner
                    || previous.provider_identity != entry.provider_identity
                    || previous.abi_fingerprint != entry.abi_fingerprint
                    || previous.provider_effects != entry.provider_effects
                    || previous.effective_effects != entry.effective_effects
                    || previous.trust != entry.trust
                    || previous.jit_route != entry.jit_route
                {
                    return Err(ResolvedExternTableError::new(format!(
                        "same-name resolved extern '{}' has inconsistent provider authority",
                        entry.name
                    )));
                }
                if !crate::extern_key::is_vm_variable_shape_extern_name(&entry.name)
                    && (previous.params != entry.params
                        || previous.returns != entry.returns
                        || previous.param_kinds != entry.param_kinds
                        || previous.allowed_effects != entry.allowed_effects)
                {
                    return Err(ResolvedExternTableError::new(format!(
                        "same-name resolved extern '{}' has inconsistent module ABI contracts",
                        entry.name
                    )));
                }
            } else {
                first_by_name.insert(entry.name.as_str(), entry);
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
        self.entries.get(usize::try_from(id).ok()?)
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

fn resolved_intrinsic_abi_matches(
    name_class: crate::extern_key::ExternNameClass<'_>,
    entry: &ResolvedExtern,
) -> bool {
    let crate::extern_key::ExternNameClass::Canonical(key) = name_class else {
        return false;
    };
    let expected_params = match (key.package(), key.function()) {
        ("math", "Sqrt" | "Floor" | "Ceil" | "Trunc") => 1,
        ("math", "FMA") => 3,
        _ => return false,
    };
    matches!(
        &entry.params,
        ParamShape::Exact { slots } if *slots == expected_params
    ) && entry.returns.slots == 1
        && entry.returns.slot_types.as_slice() == [SlotType::Float]
        && (entry.param_kinds.is_empty()
            || entry
                .param_kinds
                .iter()
                .all(|kind| *kind == ExtSlotKind::Value))
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
        /// Full-width interface method index. The instruction flags contain
        /// only an optional u8 mirror; zero is also the overflow sentinel.
        method_idx: u32,
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
        /// 0 = concrete runtime type id, 1 = interface metadata id.
        assert_kind: u8,
        /// Full-width target identity. The instruction's `c` operand is only
        /// an optional u16 mirror/sentinel.
        target_id: u32,
        result_layout: Vec<SlotType>,
    },
    LoopEnd {
        end_pc: u32,
    },
}

/// Closure payloads reserve one slot for `ClosureHeader`; the full allocation
/// width is encoded in the GC header's `u16` slot field.
pub const CLOSURE_HEADER_SLOTS: usize = 1;
pub const MAX_CLOSURE_CAPTURE_SLOTS: usize = u16::MAX as usize - CLOSURE_HEADER_SLOTS;

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
    ///
    /// Return layouts use the full `u16` slot-address domain. This field is
    /// therefore wider than `i16` so offsets above 32767 remain representable
    /// while retaining the negative sentinel.
    /// Used for errdefer runtime check: errdefer runs when returning non-nil error.
    pub error_ret_slot: i32,
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
        Self::try_compute_gc_scan_slots(slot_types)
            .expect("slot layout exceeds the u16 GC scan domain")
    }

    /// Compute the GC scan prefix without narrowing an unverified layout.
    ///
    /// `None` means the layout is wider than the bytecode slot domain, or an
    /// `Interface0` header would require an unrepresentable trailing data
    /// slot. Pairing itself is checked by the module verifier.
    pub fn try_compute_gc_scan_slots(slot_types: &[SlotType]) -> Option<u16> {
        let mut scan_slots = 0usize;
        for (idx, slot_type) in slot_types.iter().enumerate() {
            match slot_type {
                SlotType::GcRef => scan_slots = idx.checked_add(1)?,
                SlotType::Interface0 => scan_slots = idx.checked_add(2)?,
                _ => {}
            }
        }
        u16::try_from(scan_slots).ok()
    }

    pub fn compute_borrowed_scan_slots_prefix(slot_types: &[SlotType]) -> Vec<u16> {
        Self::try_compute_borrowed_scan_slots_prefix(slot_types)
            .expect("slot layout exceeds the u16 borrowed-scan domain")
    }

    /// Compute every borrowed-frame scan prefix without silent narrowing.
    pub fn try_compute_borrowed_scan_slots_prefix(slot_types: &[SlotType]) -> Option<Vec<u16>> {
        if slot_types.len() > u16::MAX as usize {
            return None;
        }
        let mut prefix = Vec::with_capacity(slot_types.len().checked_add(1)?);
        let mut scan_slots = 0u16;
        prefix.push(0);
        for (idx, slot_type) in slot_types.iter().enumerate() {
            match slot_type {
                SlotType::GcRef => scan_slots = u16::try_from(idx.checked_add(1)?).ok()?,
                SlotType::Interface0 => scan_slots = u16::try_from(idx.checked_add(2)?).ok()?,
                _ => {}
            }
            prefix.push(scan_slots);
        }
        Some(prefix)
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
        Self::try_from_u8(v)
            .unwrap_or_else(|| panic!("invalid ExtSlotKind tag {v}; expected 0..=1"))
    }

    #[inline]
    pub const fn try_from_u8(v: u8) -> Option<Self> {
        match v {
            0 => Some(Self::Value),
            1 => Some(Self::Bytes),
            _ => None,
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
        self.runtime_type_resolver()
            .canonical_value_meta_for_value_rttid(value_rttid)
    }

    pub fn slot_count_for_value_rttid(&self, value_rttid: ValueRttid) -> Option<usize> {
        self.runtime_type_resolver()
            .slot_count_for_value_rttid(value_rttid)
    }

    pub fn slot_layout_for_value_rttid(&self, value_rttid: ValueRttid) -> Option<Vec<SlotType>> {
        self.runtime_type_resolver()
            .slot_layout_for_value_rttid(value_rttid)
    }

    pub fn value_rttid_for_rttid(&self, rttid: u32) -> Option<ValueRttid> {
        self.runtime_type_resolver().value_rttid_for_rttid(rttid)
    }

    /// Find the named type reached by following zero or more pointer types.
    pub fn named_type_id_for_rttid(&self, rttid: u32) -> Option<u32> {
        self.runtime_type_resolver().named_type_id_for_rttid(rttid)
    }

    pub fn runtime_type_resolver(&self) -> RuntimeTypeResolver<'_> {
        RuntimeTypeResolver::new(
            &self.struct_metas,
            &self.named_type_metas,
            &self.runtime_types,
        )
    }
}

/// Cycle-aware resolver for bytecode runtime-type metadata.
///
/// The resolver accepts metadata slices so runtimes that already borrowed a
/// module's tables can share the exact verifier rules without cloning a
/// `Module`. Physical layouts are capped at `u16::MAX` slots before allocation,
/// matching the bytecode slot-index domain.
#[derive(Clone, Copy)]
pub struct RuntimeTypeResolver<'a> {
    struct_metas: &'a [StructMeta],
    named_type_metas: &'a [NamedTypeMeta],
    runtime_types: &'a [RuntimeType],
}

impl<'a> RuntimeTypeResolver<'a> {
    pub const MAX_LAYOUT_SLOTS: usize = u16::MAX as usize;

    pub fn new(
        struct_metas: &'a [StructMeta],
        named_type_metas: &'a [NamedTypeMeta],
        runtime_types: &'a [RuntimeType],
    ) -> Self {
        Self {
            struct_metas,
            named_type_metas,
            runtime_types,
        }
    }

    /// Find the named type reached by following zero or more pointer types.
    ///
    /// A valid chain cannot visit more runtime-type entries than the table
    /// contains. The table-sized bound rejects malformed pointer cycles while
    /// keeping this hot metadata lookup allocation-free.
    pub fn named_type_id_for_rttid(&self, rttid: u32) -> Option<u32> {
        if rttid >= INVALID_META_ID {
            return None;
        }
        let mut current = rttid;
        for _ in 0..=self.runtime_types.len() {
            match self.runtime_types.get(current as usize)? {
                RuntimeType::Named { id, .. } => {
                    self.named_type_metas.get(*id as usize)?;
                    return Some(*id);
                }
                RuntimeType::Pointer(inner) => {
                    let inner = ValueRttid::try_from_raw(inner.to_raw())?;
                    let inner_kind = match self.runtime_types.get(inner.rttid() as usize)? {
                        RuntimeType::Named { id, .. } => ValueRttid::try_from_raw(
                            self.named_type_metas
                                .get(*id as usize)?
                                .underlying_rttid
                                .to_raw(),
                        )?
                        .value_kind(),
                        runtime_type => runtime_type_value_kind(runtime_type)?,
                    };
                    if inner_kind != inner.value_kind() {
                        return None;
                    }
                    current = inner.rttid();
                }
                _ => return None,
            }
        }
        None
    }

    /// Resolve all named wrappers and return the terminal runtime type.
    ///
    /// Every wrapper must preserve the embedded `ValueKind`. Missing metadata,
    /// kind drift, and named-type cycles return `None`.
    pub fn resolve_value_rttid(
        &self,
        value_rttid: ValueRttid,
    ) -> Option<(ValueRttid, &'a RuntimeType)> {
        let value_rttid = ValueRttid::try_from_raw(value_rttid.to_raw())?;
        let expected_kind = value_rttid.value_kind();
        let mut current = value_rttid;
        let mut seen = HashSet::new();

        loop {
            current = ValueRttid::try_from_raw(current.to_raw())?;
            let current_kind = current.value_kind();
            if current_kind != expected_kind || !seen.insert(current.rttid()) {
                return None;
            }
            let runtime_type = self.runtime_types.get(current.rttid() as usize)?;
            let RuntimeType::Named { id, .. } = runtime_type else {
                if runtime_type_value_kind(runtime_type)? != expected_kind {
                    return None;
                }
                return Some((current, runtime_type));
            };
            current = self.named_type_metas.get(*id as usize)?.underlying_rttid;
        }
    }

    /// Reconstruct a validated `ValueRttid` from a bare runtime-type id.
    ///
    /// Named runtime types derive their physical kind from
    /// `NamedTypeMeta::underlying_rttid`; other variants carry their own kind.
    pub fn value_rttid_for_rttid(&self, rttid: u32) -> Option<ValueRttid> {
        if rttid >= INVALID_META_ID {
            return None;
        }
        let runtime_type = self.runtime_types.get(rttid as usize)?;
        let kind = match runtime_type {
            RuntimeType::Named { id, .. } => {
                let raw = self
                    .named_type_metas
                    .get(*id as usize)?
                    .underlying_rttid
                    .to_raw() as u8;
                ValueKind::try_from(raw).ok()?
            }
            _ => runtime_type_value_kind(runtime_type)?,
        };
        let value_rttid = ValueRttid::try_new(rttid, kind)?;
        self.resolve_value_rttid(value_rttid)?;
        Some(value_rttid)
    }

    pub fn canonical_value_meta_for_value_rttid(
        &self,
        value_rttid: ValueRttid,
    ) -> Option<ValueMeta> {
        let value_rttid = ValueRttid::try_from_raw(value_rttid.to_raw())?;
        let kind = value_rttid.value_kind();
        let (_, runtime_type) = self.resolve_value_rttid(value_rttid)?;
        let meta_id = match (kind, runtime_type) {
            (ValueKind::Struct, RuntimeType::Struct { meta_id, .. }) => {
                self.struct_metas.get(*meta_id as usize)?;
                *meta_id
            }
            (ValueKind::Pointer, RuntimeType::Pointer(elem)) => {
                self.canonical_struct_meta_id(*elem)?
            }
            (ValueKind::Interface, RuntimeType::Interface { meta_id, .. }) => *meta_id,
            (ValueKind::Array, RuntimeType::Array { .. }) => value_rttid.rttid(),
            (
                ValueKind::Struct | ValueKind::Pointer | ValueKind::Interface | ValueKind::Array,
                _,
            ) => {
                return None;
            }
            _ => 0,
        };
        ValueMeta::try_new(meta_id, kind)
    }

    pub fn slot_count_for_value_rttid(&self, value_rttid: ValueRttid) -> Option<usize> {
        self.slot_layout_for_value_rttid(value_rttid)
            .map(|layout| layout.len())
    }

    /// Expand a value into its physical bytecode slot layout without recursion.
    ///
    /// Arrays and tuples are rejected as soon as their accumulated layout would
    /// exceed `u16::MAX`; an untrusted array length therefore cannot request an
    /// unbounded allocation.
    pub fn slot_layout_for_value_rttid(&self, value_rttid: ValueRttid) -> Option<Vec<SlotType>> {
        enum Task {
            Visit(ValueRttid),
            FinishArray { len: u64, elem_start: usize },
            Exit(u32),
        }

        let mut layout = Vec::new();
        let mut active = HashSet::new();
        let mut tasks = vec![Task::Visit(value_rttid)];

        while let Some(task) = tasks.pop() {
            match task {
                Task::Visit(value_rttid) => {
                    let (resolved_rttid, runtime_type) = self.resolve_value_rttid(value_rttid)?;
                    let resolved_id = resolved_rttid.rttid();
                    if !active.insert(resolved_id) {
                        return None;
                    }

                    match runtime_type {
                        RuntimeType::Basic(kind) => {
                            push_layout_slot(&mut layout, slot_type_for_value_kind(*kind))?;
                            active.remove(&resolved_id);
                        }
                        RuntimeType::Pointer(elem) => {
                            self.canonical_struct_meta_id(*elem)?;
                            push_layout_slot(&mut layout, SlotType::GcRef)?;
                            active.remove(&resolved_id);
                        }
                        RuntimeType::Slice(_)
                        | RuntimeType::Map { .. }
                        | RuntimeType::Chan { .. }
                        | RuntimeType::Port { .. }
                        | RuntimeType::Func { .. }
                        | RuntimeType::Island => {
                            push_layout_slot(&mut layout, SlotType::GcRef)?;
                            active.remove(&resolved_id);
                        }
                        RuntimeType::Interface { .. } => {
                            extend_slot_layout(
                                &mut layout,
                                &[SlotType::Interface0, SlotType::Interface1],
                            )?;
                            active.remove(&resolved_id);
                        }
                        RuntimeType::Struct { meta_id, .. } => {
                            let struct_layout =
                                &self.struct_metas.get(*meta_id as usize)?.slot_types;
                            extend_slot_layout(&mut layout, struct_layout)?;
                            active.remove(&resolved_id);
                        }
                        RuntimeType::Array { len, elem } => {
                            tasks.push(Task::Exit(resolved_id));
                            tasks.push(Task::FinishArray {
                                len: *len,
                                elem_start: layout.len(),
                            });
                            tasks.push(Task::Visit(*elem));
                        }
                        RuntimeType::Tuple(elems) => {
                            tasks.push(Task::Exit(resolved_id));
                            tasks.extend(elems.iter().rev().copied().map(Task::Visit));
                        }
                        RuntimeType::Named { .. } => return None,
                    }
                }
                Task::FinishArray { len, elem_start } => {
                    let elem_slots = layout.len().checked_sub(elem_start)?;
                    let total_slots = len.checked_mul(u64::try_from(elem_slots).ok()?)?;
                    if total_slots > Self::MAX_LAYOUT_SLOTS as u64 {
                        return None;
                    }
                    let total_slots = usize::try_from(total_slots).ok()?;
                    let final_slots = elem_start.checked_add(total_slots)?;
                    if final_slots > Self::MAX_LAYOUT_SLOTS {
                        return None;
                    }
                    if total_slots == 0 {
                        layout.truncate(elem_start);
                        continue;
                    }
                    let repetitions = usize::try_from(len).ok()?;
                    if repetitions == 1 {
                        continue;
                    }
                    let elem_layout = layout.get(elem_start..)?.to_vec();
                    layout.reserve(total_slots.checked_sub(elem_slots)?);
                    for _ in 1..repetitions {
                        layout.extend_from_slice(&elem_layout);
                    }
                }
                Task::Exit(resolved_id) => {
                    if !active.remove(&resolved_id) {
                        return None;
                    }
                }
            }
        }
        Some(layout)
    }

    fn canonical_struct_meta_id(&self, value_rttid: ValueRttid) -> Option<u32> {
        let (_, runtime_type) = self.resolve_value_rttid(value_rttid)?;
        let RuntimeType::Struct { meta_id, .. } = runtime_type else {
            return None;
        };
        self.struct_metas.get(*meta_id as usize)?;
        Some(*meta_id)
    }
}

fn runtime_type_value_kind(runtime_type: &RuntimeType) -> Option<ValueKind> {
    match runtime_type {
        RuntimeType::Basic(kind) if ValueKind::BASIC.contains(kind) => Some(*kind),
        RuntimeType::Basic(_) | RuntimeType::Named { .. } => None,
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

fn push_layout_slot(layout: &mut Vec<SlotType>, slot: SlotType) -> Option<()> {
    if layout.len() == RuntimeTypeResolver::MAX_LAYOUT_SLOTS {
        return None;
    }
    layout.push(slot);
    Some(())
}

fn extend_slot_layout(layout: &mut Vec<SlotType>, extension: &[SlotType]) -> Option<()> {
    let total = layout.len().checked_add(extension.len())?;
    if total > RuntimeTypeResolver::MAX_LAYOUT_SLOTS {
        return None;
    }
    layout.extend_from_slice(extension);
    Some(())
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

    #[test]
    fn ext_slot_kind_rejects_unknown_tags() {
        assert_eq!(ExtSlotKind::try_from_u8(0), Some(ExtSlotKind::Value));
        assert_eq!(ExtSlotKind::try_from_u8(1), Some(ExtSlotKind::Bytes));
        assert_eq!(ExtSlotKind::try_from_u8(2), None);
        assert!(std::panic::catch_unwind(|| ExtSlotKind::from_u8(2)).is_err());
    }

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
        let effects = ExternEffects::MAY_WAIT_IO_REPLAY
            .union(ExternEffects::MAY_HOST_REPLAY)
            .union(ExternEffects::MAY_EXIT);
        assert_eq!(
            effects.names().collect::<Vec<_>>(),
            vec!["wait_io_replay", "host_replay", "exit"]
        );
        assert!(ExternEffects::MAY_WAIT_IO_REPLAY.is_subset_of(effects));
        assert!(!ExternEffects::MAY_HOST_WAIT.is_subset_of(effects));
        assert!(ExternEffects::MAY_EXIT.is_subset_of(effects));
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
    fn runtime_type_resolver_handles_deep_named_chains_without_recursion() {
        const DEPTH: usize = 8_192;

        let mut module = Module::new("deep-named-runtime-types".to_string());
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Int64));
        for named_id in 0..DEPTH {
            module.named_type_metas.push(NamedTypeMeta {
                name: format!("N{named_id}"),
                underlying_meta: ValueMeta::new(0, ValueKind::Int64),
                underlying_rttid: ValueRttid::new(named_id as u32, ValueKind::Int64),
                methods: BTreeMap::new(),
            });
            module.runtime_types.push(RuntimeType::Named {
                id: named_id as u32,
                struct_meta_id: None,
            });
        }

        let outer = ValueRttid::new(DEPTH as u32, ValueKind::Int64);
        assert_eq!(
            module.canonical_value_meta_for_value_rttid(outer),
            Some(ValueMeta::new(0, ValueKind::Int64))
        );
        assert_eq!(
            module.slot_layout_for_value_rttid(outer),
            Some(vec![SlotType::Value])
        );
    }

    #[test]
    fn runtime_type_resolver_follows_deep_pointer_chains_and_rejects_cycles() {
        const DEPTH: usize = 8_192;

        let mut module = Module::new("deep-pointer-named-type".to_string());
        module.named_type_metas.push(NamedTypeMeta {
            name: "Named".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
            methods: BTreeMap::new(),
        });
        module.runtime_types.push(RuntimeType::Named {
            id: 0,
            struct_meta_id: None,
        });
        for rttid in 0..DEPTH {
            let elem_kind = if rttid == 0 {
                ValueKind::Int64
            } else {
                ValueKind::Pointer
            };
            module
                .runtime_types
                .push(RuntimeType::Pointer(ValueRttid::new(
                    rttid as u32,
                    elem_kind,
                )));
        }
        assert_eq!(module.named_type_id_for_rttid(DEPTH as u32), Some(0));

        module.runtime_types[1] = RuntimeType::Pointer(ValueRttid::new(0, ValueKind::Pointer));
        assert_eq!(module.named_type_id_for_rttid(DEPTH as u32), None);

        let mut cyclic = Module::new("cyclic-pointer-named-type".to_string());
        cyclic
            .runtime_types
            .push(RuntimeType::Pointer(ValueRttid::new(1, ValueKind::Pointer)));
        cyclic
            .runtime_types
            .push(RuntimeType::Pointer(ValueRttid::new(0, ValueKind::Pointer)));
        assert_eq!(cyclic.named_type_id_for_rttid(0), None);
    }

    #[test]
    fn runtime_type_resolver_reconstructs_named_value_rttids_and_rejects_missing_metadata() {
        let mut module = Module::new("runtime-type-kind-reconstruction".to_string());
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::String));
        module.named_type_metas.push(NamedTypeMeta {
            name: "Name".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::String),
            underlying_rttid: ValueRttid::new(0, ValueKind::String),
            methods: BTreeMap::new(),
        });
        module.runtime_types.push(RuntimeType::Named {
            id: 0,
            struct_meta_id: None,
        });
        module.runtime_types.push(RuntimeType::Named {
            id: 99,
            struct_meta_id: None,
        });

        assert_eq!(
            module.value_rttid_for_rttid(1),
            Some(ValueRttid::new(1, ValueKind::String))
        );
        assert_eq!(module.value_rttid_for_rttid(2), None);
        assert_eq!(module.value_rttid_for_rttid(99), None);
    }

    #[test]
    fn runtime_type_resolver_rejects_reserved_packed_ids_without_panicking() {
        let mut module = Module::new("reserved-runtime-type-ids".to_string());
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Int64));
        assert_eq!(module.value_rttid_for_rttid(INVALID_META_ID), None);
        assert_eq!(module.value_rttid_for_rttid(u32::MAX), None);

        let reserved =
            ValueRttid::from_raw((INVALID_META_ID << 8) | u32::from(ValueKind::Int64 as u8));
        assert_eq!(
            module.runtime_type_resolver().resolve_value_rttid(reserved),
            None
        );
        assert_eq!(module.canonical_value_meta_for_value_rttid(reserved), None);
        assert_eq!(module.slot_layout_for_value_rttid(reserved), None);

        let invalid_kind = ValueRttid::from_raw(0xff);
        assert_eq!(
            module
                .runtime_type_resolver()
                .resolve_value_rttid(invalid_kind),
            None
        );
        assert_eq!(
            module.canonical_value_meta_for_value_rttid(invalid_kind),
            None
        );
        assert_eq!(module.slot_layout_for_value_rttid(invalid_kind), None);

        module.runtime_types[0] = RuntimeType::Interface {
            methods: Vec::new(),
            meta_id: INVALID_META_ID,
        };
        assert_eq!(
            module.canonical_value_meta_for_value_rttid(ValueRttid::new(0, ValueKind::Interface,)),
            None
        );
        module.runtime_types[0] = RuntimeType::Interface {
            methods: Vec::new(),
            meta_id: u32::MAX,
        };
        assert_eq!(
            module.canonical_value_meta_for_value_rttid(ValueRttid::new(0, ValueKind::Interface,)),
            None
        );
    }

    #[test]
    fn runtime_type_resolver_handles_deep_array_tuple_chains_without_recursion() {
        const DEPTH: usize = 8_192;

        let mut module = Module::new("deep-array-tuple-runtime-types".to_string());
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Int64));
        let mut current = ValueRttid::new(0, ValueKind::Int64);
        for depth in 0..DEPTH {
            let runtime_type = if depth % 2 == 0 {
                RuntimeType::Array {
                    len: 1,
                    elem: current,
                }
            } else {
                RuntimeType::Tuple(vec![current])
            };
            let kind = if depth % 2 == 0 {
                ValueKind::Array
            } else {
                ValueKind::Void
            };
            module.runtime_types.push(runtime_type);
            current = ValueRttid::new(depth as u32 + 1, kind);
        }

        assert_eq!(
            module.slot_layout_for_value_rttid(current),
            Some(vec![SlotType::Value])
        );
    }

    #[test]
    fn runtime_type_resolver_does_not_recopy_wide_layouts_through_unit_arrays() {
        const DEPTH: usize = 2_048;
        const WIDTH: usize = 32_768;

        let mut module = Module::new("deep-wide-unit-arrays".to_string());
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::Value; WIDTH],
            fields: Vec::new(),
            field_index: HashMap::new(),
        });
        module.runtime_types.push(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        });
        let mut current = ValueRttid::new(0, ValueKind::Struct);
        for depth in 0..DEPTH {
            module.runtime_types.push(RuntimeType::Array {
                len: 1,
                elem: current,
            });
            current = ValueRttid::new(depth as u32 + 1, ValueKind::Array);
        }

        let layout = module
            .slot_layout_for_value_rttid(current)
            .expect("unit-length Array wrappers must preserve the wide leaf layout");
        assert_eq!(layout.len(), WIDTH);
        assert!(layout.iter().all(|slot| *slot == SlotType::Value));
    }

    #[test]
    fn runtime_type_resolver_rejects_named_and_layout_cycles() {
        let mut named_cycle = Module::new("named-runtime-type-cycle".to_string());
        named_cycle.named_type_metas.push(NamedTypeMeta {
            name: "A".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: ValueRttid::new(1, ValueKind::Int64),
            methods: BTreeMap::new(),
        });
        named_cycle.named_type_metas.push(NamedTypeMeta {
            name: "B".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
            methods: BTreeMap::new(),
        });
        named_cycle.runtime_types.push(RuntimeType::Named {
            id: 0,
            struct_meta_id: None,
        });
        named_cycle.runtime_types.push(RuntimeType::Named {
            id: 1,
            struct_meta_id: None,
        });
        let named = ValueRttid::new(0, ValueKind::Int64);
        assert_eq!(named_cycle.slot_layout_for_value_rttid(named), None);
        assert_eq!(
            named_cycle.canonical_value_meta_for_value_rttid(named),
            None
        );

        let mut layout_cycle = Module::new("array-tuple-runtime-type-cycle".to_string());
        layout_cycle.runtime_types.push(RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new(1, ValueKind::Void),
        });
        layout_cycle
            .runtime_types
            .push(RuntimeType::Tuple(vec![ValueRttid::new(
                0,
                ValueKind::Array,
            )]));
        assert_eq!(
            layout_cycle.slot_layout_for_value_rttid(ValueRttid::new(0, ValueKind::Array)),
            None
        );
    }

    #[test]
    fn runtime_type_resolver_caps_array_and_tuple_layouts_before_allocation() {
        let mut module = Module::new("runtime-type-layout-width-cap".to_string());
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Int64));
        module.runtime_types.push(RuntimeType::Array {
            len: u16::MAX as u64,
            elem: ValueRttid::new(0, ValueKind::Int64),
        });
        module.runtime_types.push(RuntimeType::Array {
            len: u16::MAX as u64 + 1,
            elem: ValueRttid::new(0, ValueKind::Int64),
        });
        module.runtime_types.push(RuntimeType::Array {
            len: u64::MAX,
            elem: ValueRttid::new(0, ValueKind::Int64),
        });
        module.runtime_types.push(RuntimeType::Array {
            len: 32_768,
            elem: ValueRttid::new(0, ValueKind::Int64),
        });
        module.runtime_types.push(RuntimeType::Tuple(vec![
            ValueRttid::new(4, ValueKind::Array),
            ValueRttid::new(4, ValueKind::Array),
        ]));
        module.struct_metas.push(StructMeta {
            slot_types: Vec::new(),
            fields: Vec::new(),
            field_index: HashMap::new(),
        });
        module.runtime_types.push(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        });
        module.runtime_types.push(RuntimeType::Array {
            len: u64::MAX,
            elem: ValueRttid::new(6, ValueKind::Struct),
        });

        assert_eq!(
            module
                .slot_layout_for_value_rttid(ValueRttid::new(1, ValueKind::Array))
                .map(|layout| layout.len()),
            Some(u16::MAX as usize)
        );
        assert_eq!(
            module.slot_layout_for_value_rttid(ValueRttid::new(2, ValueKind::Array)),
            None
        );
        assert_eq!(
            module.slot_layout_for_value_rttid(ValueRttid::new(3, ValueKind::Array)),
            None
        );
        assert_eq!(
            module.slot_layout_for_value_rttid(ValueRttid::new(5, ValueKind::Void)),
            None
        );
        assert_eq!(
            module.slot_layout_for_value_rttid(ValueRttid::new(7, ValueKind::Array)),
            Some(Vec::new()),
            "a huge zero-slot array must resolve without iterating over its logical length"
        );
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
            provider_module_owner: None,
            provider_identity: 0,
            abi_fingerprint: 0,
            trust: ProviderTrust::Untrusted,
            jit_route: ExternJitRoute::DirectHelper,
        }];

        let err = ResolvedExternTable::try_new(entries).unwrap_err();

        assert!(err.message().contains("index 0 has id 1"));
    }

    #[test]
    fn resolved_extern_table_rejects_invalid_bytecode_identities() {
        let mut entry = resolved_extension_entry("github.com/acme/demo");
        entry.source = RegisteredExternSource::Test;
        entry.provider_module_owner = None;
        entry.name = "legacy_flattened_name".to_string();

        let err = ResolvedExternTable::try_new(vec![entry])
            .expect_err("resolved tables must preserve the verifier's name invariant");
        assert!(err.message().contains("invalid bytecode identity"));
    }

    fn resolved_extension_entry(package: &str) -> ResolvedExtern {
        ResolvedExtern {
            id: 0,
            name: crate::extern_key::ExternKeyRef::new(package, "Run")
                .encode()
                .expect("canonical extern name"),
            params: ParamShape::Exact { slots: 0 },
            returns: ReturnShape::slots(0),
            param_kinds: Vec::new(),
            allowed_effects: ExternEffects::NONE,
            provider_effects: ExternEffects::NONE,
            effective_effects: ExternEffects::NONE,
            source: RegisteredExternSource::NativeExtension,
            provider_module_owner: Some("github.com/acme/demo".to_string()),
            provider_identity: 1,
            abi_fingerprint: 2,
            trust: ProviderTrust::Untrusted,
            jit_route: ExternJitRoute::DirectHelper,
        }
    }

    fn resolved_math_intrinsic(function: &str, param_slots: u16) -> ResolvedExtern {
        ResolvedExtern {
            id: 0,
            name: crate::extern_key::ExternKeyRef::new("math", function)
                .encode()
                .expect("canonical math extern name"),
            params: ParamShape::Exact { slots: param_slots },
            returns: ReturnShape::try_with_slot_types(vec![SlotType::Float])
                .expect("one float is a valid return shape"),
            param_kinds: Vec::new(),
            allowed_effects: ExternEffects::NONE,
            provider_effects: ExternEffects::NONE,
            effective_effects: ExternEffects::NONE,
            source: RegisteredExternSource::Builtin,
            provider_module_owner: None,
            provider_identity: 1,
            abi_fingerprint: 2,
            trust: ProviderTrust::IntrinsicEligible,
            jit_route: ExternJitRoute::Intrinsic,
        }
    }

    #[test]
    fn resolved_extern_table_accepts_exact_extension_owner_metadata() {
        let entry = resolved_extension_entry("github.com/acme/demo/渲染器");

        let table = ResolvedExternTable::try_new(vec![entry]).expect("valid extension owner");

        assert_eq!(
            table
                .get(0)
                .and_then(|entry| entry.provider_module_owner.as_deref()),
            Some("github.com/acme/demo")
        );
    }

    #[test]
    fn resolved_extern_table_rejects_missing_or_spurious_extension_owner_metadata() {
        let mut missing = resolved_extension_entry("github.com/acme/demo/pkg");
        missing.provider_module_owner = None;
        let err = ResolvedExternTable::try_new(vec![missing])
            .expect_err("extension sources require an authoritative module owner");
        assert!(err.message().contains("missing its provider module owner"));

        let mut spurious = resolved_extension_entry("github.com/acme/demo/pkg");
        spurious.source = RegisteredExternSource::Builtin;
        let err = ResolvedExternTable::try_new(vec![spurious])
            .expect_err("host-image sources cannot claim extension ownership");
        assert!(err
            .message()
            .contains("cannot declare extension module owner"));
    }

    #[test]
    fn resolved_extern_table_rejects_invalid_or_foreign_extension_owner_metadata() {
        let mut invalid = resolved_extension_entry("github.com/acme/demo/pkg");
        invalid.provider_module_owner = Some("github.com/acme/demo/../other".to_string());
        let err = ResolvedExternTable::try_new(vec![invalid])
            .expect_err("non-canonical owner must be rejected");
        assert!(err.message().contains("invalid provider module owner"));

        let foreign = resolved_extension_entry("github.com/acme/other/pkg");
        let err = ResolvedExternTable::try_new(vec![foreign])
            .expect_err("extern package must stay within the provider module");
        assert!(err.message().contains("outside provider module owner"));
    }

    #[test]
    fn resolved_extern_table_rejects_effect_and_jit_authority_drift() {
        let mut outside_contract = resolved_extension_entry("github.com/acme/demo/pkg");
        outside_contract.provider_effects = ExternEffects::MAY_YIELD;
        outside_contract.effective_effects = ExternEffects::MAY_YIELD;
        let error = ResolvedExternTable::try_new(vec![outside_contract])
            .expect_err("provider effects outside the declaration must fail");
        assert!(error.message().contains("exceed allowed effects"));

        let mut wrong_intersection = resolved_extension_entry("github.com/acme/demo/pkg");
        wrong_intersection.allowed_effects = ExternEffects::MAY_YIELD;
        wrong_intersection.provider_effects = ExternEffects::MAY_YIELD;
        let error = ResolvedExternTable::try_new(vec![wrong_intersection])
            .expect_err("effective effects must preserve the resolved provider authority");
        assert!(error.message().contains("do not equal provider effects"));

        let mut unknown_direct = resolved_extension_entry("github.com/acme/demo/pkg");
        unknown_direct.allowed_effects = ExternEffects::UNKNOWN_CONTROL;
        unknown_direct.provider_effects = ExternEffects::UNKNOWN_CONTROL;
        unknown_direct.effective_effects = ExternEffects::UNKNOWN_CONTROL;
        let error = ResolvedExternTable::try_new(vec![unknown_direct])
            .expect_err("unknown control cannot use direct JIT dispatch");
        assert!(error.message().contains("must materialize"));

        let mut untrusted_intrinsic = resolved_extension_entry("github.com/acme/demo/pkg");
        untrusted_intrinsic.jit_route = ExternJitRoute::Intrinsic;
        let error = ResolvedExternTable::try_new(vec![untrusted_intrinsic])
            .expect_err("intrinsic dispatch requires explicit trust");
        assert!(error.message().contains("intrinsic-eligible trust"));

        let mut effectful_intrinsic = resolved_extension_entry("github.com/acme/demo/pkg");
        effectful_intrinsic.allowed_effects = ExternEffects::MAY_YIELD;
        effectful_intrinsic.provider_effects = ExternEffects::MAY_YIELD;
        effectful_intrinsic.effective_effects = ExternEffects::MAY_YIELD;
        effectful_intrinsic.trust = ProviderTrust::IntrinsicEligible;
        effectful_intrinsic.jit_route = ExternJitRoute::Intrinsic;
        let error = ResolvedExternTable::try_new(vec![effectful_intrinsic])
            .expect_err("effectful providers cannot use intrinsic dispatch");
        assert!(error.message().contains("intrinsic JIT route with effects"));

        ResolvedExternTable::try_new(vec![resolved_math_intrinsic("Sqrt", 1)])
            .expect("the frozen builtin math intrinsic contract is valid");

        let mut non_builtin_intrinsic = resolved_math_intrinsic("Sqrt", 1);
        non_builtin_intrinsic.source = RegisteredExternSource::Test;
        let error = ResolvedExternTable::try_new(vec![non_builtin_intrinsic])
            .expect_err("non-builtin providers cannot bypass dispatch through an intrinsic");
        assert!(error.message().contains("non-builtin source"));

        let unsupported_intrinsic = resolved_math_intrinsic("Round", 1);
        let error = ResolvedExternTable::try_new(vec![unsupported_intrinsic])
            .expect_err("the intrinsic route must use an exact supported name");
        assert!(error.message().contains("unsupported name or ABI"));

        let wrong_intrinsic_abi = resolved_math_intrinsic("FMA", 1);
        let error = ResolvedExternTable::try_new(vec![wrong_intrinsic_abi])
            .expect_err("the intrinsic route must freeze its exact parameter ABI");
        assert!(error.message().contains("unsupported name or ABI"));

        let mut wrong_intrinsic_param_kind = resolved_math_intrinsic("Sqrt", 1);
        wrong_intrinsic_param_kind.param_kinds = vec![ExtSlotKind::Bytes];
        let error = ResolvedExternTable::try_new(vec![wrong_intrinsic_param_kind])
            .expect_err("the intrinsic route must reject reference-shaped parameters");
        assert!(error.message().contains("unsupported name or ABI"));

        let mut zero_identity = resolved_extension_entry("github.com/acme/demo/pkg");
        zero_identity.provider_identity = 0;
        let error = ResolvedExternTable::try_new(vec![zero_identity])
            .expect_err("zero is reserved by the process identity allocator");
        assert!(error.message().contains("reserved provider identity 0"));
    }

    #[test]
    fn resolved_extern_table_rejects_provider_identity_and_same_name_drift() {
        let first = resolved_extension_entry("github.com/acme/demo/pkg");
        let mut reused_identity = first.clone();
        reused_identity.id = 1;
        reused_identity.name =
            crate::extern_key::ExternKeyRef::new("github.com/acme/demo/pkg", "Other")
                .encode()
                .unwrap();
        let error = ResolvedExternTable::try_new(vec![first.clone(), reused_identity])
            .expect_err("one process provider identity cannot name two providers");
        assert!(error.message().contains("reuse provider identity"));

        let mut authority_drift = first.clone();
        authority_drift.id = 1;
        authority_drift.provider_identity = 2;
        let error = ResolvedExternTable::try_new(vec![first.clone(), authority_drift])
            .expect_err("same-name entries must freeze one provider authority");
        assert!(error.message().contains("inconsistent provider authority"));

        let mut abi_drift = first.clone();
        abi_drift.id = 1;
        abi_drift.params = ParamShape::Exact { slots: 1 };
        let error = ResolvedExternTable::try_new(vec![first, abi_drift])
            .expect_err("ordinary same-name externs must freeze one ABI contract");
        assert!(error
            .message()
            .contains("inconsistent module ABI contracts"));
    }

    #[test]
    fn resolved_extern_table_keeps_vm_variable_shape_exception_narrow() {
        let entry = |id, params| ResolvedExtern {
            id,
            name: "dyn_call".to_string(),
            params,
            returns: ReturnShape::slots(0),
            param_kinds: Vec::new(),
            allowed_effects: ExternEffects::NONE,
            provider_effects: ExternEffects::NONE,
            effective_effects: ExternEffects::NONE,
            source: RegisteredExternSource::Builtin,
            provider_module_owner: None,
            provider_identity: 7,
            abi_fingerprint: 11,
            trust: ProviderTrust::RuntimeInternal,
            jit_route: ExternJitRoute::DirectHelper,
        };

        let table = ResolvedExternTable::try_new(vec![
            entry(0, ParamShape::CallSiteVariadic),
            entry(1, ParamShape::Exact { slots: 4 }),
        ])
        .expect("only the exact VM variable-shape whitelist may vary its ABI");
        assert_eq!(table.len(), 2);
    }
}
