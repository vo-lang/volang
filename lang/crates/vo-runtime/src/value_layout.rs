//! Canonical physical-layout facts shared by runtime boundaries.

use crate::bytecode::{ModuleRuntimeMetadata, TransferType};
use crate::gc::{Gc, GcRef};
use crate::objects::{array, interface};
use crate::{Module, RuntimeType, SlotType, ValueKind, ValueMeta, ValueRttid};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueSlotLayoutError {
    MissingModule(ValueKind),
    MissingStructMetadata(u32),
    MissingArrayLayout(u32),
}

impl core::fmt::Display for ValueSlotLayoutError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::MissingModule(kind) => {
                write!(f, "missing module metadata for {kind:?} slot layout")
            }
            Self::MissingStructMetadata(meta_id) => {
                write!(f, "missing StructMeta id {meta_id}")
            }
            Self::MissingArrayLayout(rttid) => {
                write!(f, "array runtime type {rttid} has no slot layout")
            }
        }
    }
}

/// Match a runtime value descriptor against canonical physical slots.
///
/// Loaded-module array matching uses verified compact type facts and allocates
/// no flattened temporary layout.
pub fn value_meta_layout_matches(
    meta: ValueMeta,
    actual: &[SlotType],
    module: Option<ModuleRuntimeMetadata<'_>>,
) -> Result<bool, ValueSlotLayoutError> {
    match meta.value_kind() {
        ValueKind::Struct => {
            let module = module.ok_or(ValueSlotLayoutError::MissingModule(ValueKind::Struct))?;
            let meta_id = meta.meta_id();
            let expected = module
                .struct_metas
                .get(meta_id as usize)
                .ok_or(ValueSlotLayoutError::MissingStructMetadata(meta_id))?;
            Ok(actual == expected.slot_types.as_slice())
        }
        ValueKind::Array => {
            let module = module.ok_or(ValueSlotLayoutError::MissingModule(ValueKind::Array))?;
            let rttid = meta.meta_id();
            let value_rttid = ValueRttid::new(rttid, ValueKind::Array);
            module
                .value_rttid_layout_matches(value_rttid, actual)
                .ok_or(ValueSlotLayoutError::MissingArrayLayout(rttid))
        }
        ValueKind::Interface => Ok(actual == [SlotType::Interface0, SlotType::Interface1]),
        ValueKind::Float32 | ValueKind::Float64 => {
            Ok(actual.iter().all(|slot| *slot == SlotType::Float))
        }
        ValueKind::Pointer => Ok(actual.iter().all(|slot| *slot == SlotType::GcRef)),
        kind if kind.may_contain_gc_refs() => {
            Ok(actual.iter().all(|slot| *slot == SlotType::GcBase))
        }
        _ => Ok(actual.iter().all(|slot| *slot == SlotType::Value)),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransferLayoutError {
    InvalidMetadata(u32),
    InvalidRttid(u32),
    KindMismatch {
        metadata: ValueKind,
        rttid: ValueKind,
    },
    UnresolvedRttid,
    NonCanonicalMetadata {
        actual: u32,
        canonical: u32,
    },
    MissingSlotLayout,
    SlotCountMismatch {
        transfer: usize,
        layout: usize,
    },
    SlotRangeOverflow,
    SlotLayoutMismatch,
}

impl core::fmt::Display for TransferLayoutError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::InvalidMetadata(raw) => write!(f, "metadata raw 0x{raw:x} is invalid"),
            Self::InvalidRttid(raw) => write!(f, "RTTID raw 0x{raw:x} is invalid"),
            Self::KindMismatch { metadata, rttid } => write!(
                f,
                "metadata kind {metadata:?} does not match RTTID kind {rttid:?}"
            ),
            Self::UnresolvedRttid => write!(f, "RTTID cannot be resolved"),
            Self::NonCanonicalMetadata { actual, canonical } => write!(
                f,
                "metadata raw 0x{actual:x} does not match RTTID canonical raw 0x{canonical:x}"
            ),
            Self::MissingSlotLayout => write!(f, "RTTID cannot resolve slot layout"),
            Self::SlotCountMismatch { transfer, layout } => write!(
                f,
                "transfer has {transfer} slots but RTTID layout has {layout}"
            ),
            Self::SlotRangeOverflow => write!(f, "slot range overflows the address domain"),
            Self::SlotLayoutMismatch => write!(f, "slot layout mismatch"),
        }
    }
}

/// Validate serialized transfer metadata against the canonical module layout.
pub fn validate_transfer_layout(
    module: &Module,
    slot_types: &[SlotType],
    slot_idx: usize,
    transfer: &TransferType,
) -> Result<ValueMeta, TransferLayoutError> {
    let expected_meta = ValueMeta::try_from_raw(transfer.meta_raw)
        .ok_or(TransferLayoutError::InvalidMetadata(transfer.meta_raw))?;
    let expected_rttid = ValueRttid::try_from_raw(transfer.rttid_raw)
        .ok_or(TransferLayoutError::InvalidRttid(transfer.rttid_raw))?;
    if expected_meta.value_kind() != expected_rttid.value_kind() {
        return Err(TransferLayoutError::KindMismatch {
            metadata: expected_meta.value_kind(),
            rttid: expected_rttid.value_kind(),
        });
    }
    let canonical_meta = module
        .canonical_value_meta_for_value_rttid(expected_rttid)
        .ok_or(TransferLayoutError::UnresolvedRttid)?;
    if expected_meta != canonical_meta {
        return Err(TransferLayoutError::NonCanonicalMetadata {
            actual: expected_meta.to_raw(),
            canonical: canonical_meta.to_raw(),
        });
    }
    let expected_layout = module
        .slot_layout_for_value_rttid(expected_rttid)
        .ok_or(TransferLayoutError::MissingSlotLayout)?;
    if transfer.slots as usize != expected_layout.len() {
        return Err(TransferLayoutError::SlotCountMismatch {
            transfer: transfer.slots as usize,
            layout: expected_layout.len(),
        });
    }
    let end = slot_idx
        .checked_add(expected_layout.len())
        .ok_or(TransferLayoutError::SlotRangeOverflow)?;
    if slot_types.get(slot_idx..end) != Some(expected_layout.as_slice()) {
        return Err(TransferLayoutError::SlotLayoutMismatch);
    }
    Ok(expected_meta)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConcreteHeapValueError {
    InvalidReference {
        raw: u64,
    },
    ObjectKindMismatch {
        actual: ValueKind,
        expected: ValueKind,
        value_kind: ValueKind,
    },
    PointerMetadataMismatch {
        actual: u32,
        expected: u32,
    },
}

impl core::fmt::Display for ConcreteHeapValueError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::InvalidReference { raw } => {
                write!(f, "invalid GcRef raw=0x{raw:016x}")
            }
            Self::ObjectKindMismatch {
                actual,
                expected,
                value_kind,
            } => write!(
                f,
                "object kind {actual:?} does not match expected {expected:?} for value kind {value_kind:?}"
            ),
            Self::PointerMetadataMismatch { actual, expected } => write!(
                f,
                "pointer meta_id {actual} does not match expected {expected}"
            ),
        }
    }
}

/// Canonicalize and validate a direct, single-slot heap value.
///
/// Scalar and inline aggregate values return `None` without inspecting `raw`.
/// Nil heap values also return `None`.
#[inline]
pub fn canonicalize_concrete_heap_value(
    gc: &Gc,
    raw: u64,
    expected_meta: ValueMeta,
) -> Result<Option<GcRef>, ConcreteHeapValueError> {
    let value_kind = expected_meta.value_kind();
    let Some(expected_kind) = heap_object_kind(value_kind) else {
        return Ok(None);
    };
    if raw == 0 {
        return Ok(None);
    }
    let Some(canonical) = gc.canonicalize_ref(raw as GcRef) else {
        return Err(ConcreteHeapValueError::InvalidReference { raw });
    };
    let header = unsafe { Gc::header(canonical) };
    if header.kind() != expected_kind {
        return Err(ConcreteHeapValueError::ObjectKindMismatch {
            actual: header.kind(),
            expected: expected_kind,
            value_kind,
        });
    }
    if value_kind == ValueKind::Pointer && header.meta_id() != expected_meta.meta_id() {
        return Err(ConcreteHeapValueError::PointerMetadataMismatch {
            actual: header.meta_id(),
            expected: expected_meta.meta_id(),
        });
    }
    Ok(Some(canonical))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValidatedInterfaceValue {
    Nil,
    Concrete {
        value_rttid: ValueRttid,
        canonical_data: Option<GcRef>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InterfaceValueError {
    InvalidValueKind(u8),
    NonCanonicalNil {
        slot0: u64,
        slot1: u64,
    },
    RawInterfaceKind,
    InvalidRttid {
        rttid: u32,
        value_kind: ValueKind,
    },
    UnresolvedRttid {
        rttid: u32,
        value_kind: ValueKind,
    },
    MissingAggregateObject(ValueKind),
    InvalidReference {
        raw: u64,
    },
    ObjectKindMismatch {
        actual: ValueKind,
        expected: ValueKind,
        value_kind: ValueKind,
    },
    MetadataMismatch {
        actual: u32,
        expected: u32,
    },
    MissingStructMetadata(u32),
    SlotCountMismatch {
        actual: usize,
        expected: usize,
    },
    MissingArrayLayout(u32),
    MissingArrayElementMetadata(u32),
    MissingArrayElementLayout(u32),
    ArrayObjectKindMismatch(ValueKind),
    ArrayLayoutMismatch {
        actual_len: usize,
        expected_len: usize,
        actual_elem_meta: ValueMeta,
        expected_elem_meta: ValueMeta,
        actual_elem_bytes: usize,
        expected_elem_bytes: usize,
    },
    MissingArrayBoxStructMetadata(u32),
    ArrayBoxLayoutMismatch,
}

impl core::fmt::Display for InterfaceValueError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::InvalidValueKind(tag) => write!(f, "has invalid value-kind tag {tag}"),
            Self::NonCanonicalNil { slot0, slot1 } => write!(
                f,
                "has non-canonical nil interface representation slot0=0x{slot0:016x} slot1=0x{slot1:016x}"
            ),
            Self::RawInterfaceKind => write!(f, "contains a raw interface-kind value"),
            Self::InvalidRttid { rttid, value_kind } => write!(
                f,
                "has reserved interface RTTID {rttid} for value kind {value_kind:?}"
            ),
            Self::UnresolvedRttid { rttid, value_kind } => write!(
                f,
                "has non-canonical RTTID/kind rttid={rttid} kind={value_kind:?}"
            ),
            Self::MissingAggregateObject(value_kind) => write!(
                f,
                "data missing object for aggregate value kind {value_kind:?}"
            ),
            Self::InvalidReference { raw } => {
                write!(f, "data invalid GcRef raw=0x{raw:016x}")
            }
            Self::ObjectKindMismatch {
                actual,
                expected,
                value_kind,
            } => write!(
                f,
                "data object kind {actual:?} does not match expected {expected:?} for value kind {value_kind:?}"
            ),
            Self::MetadataMismatch { actual, expected } => write!(
                f,
                "data meta_id {actual} does not match expected {expected}"
            ),
            Self::MissingStructMetadata(meta_id) => {
                write!(f, "data references missing StructMeta id {meta_id}")
            }
            Self::SlotCountMismatch { actual, expected } => write!(
                f,
                "data allocation slots {actual} do not match expected {expected}"
            ),
            Self::MissingArrayLayout(rttid) => {
                write!(f, "array data layout missing for RTTID {rttid}")
            }
            Self::MissingArrayElementMetadata(rttid) => write!(
                f,
                "array data element RTTID {rttid} cannot resolve canonical metadata"
            ),
            Self::MissingArrayElementLayout(rttid) => {
                write!(f, "array data element layout missing for RTTID {rttid}")
            }
            Self::ArrayObjectKindMismatch(actual) => write!(
                f,
                "data object kind {actual:?} does not match expected Array or Struct for value kind Array"
            ),
            Self::ArrayLayoutMismatch {
                actual_len,
                expected_len,
                actual_elem_meta,
                expected_elem_meta,
                actual_elem_bytes,
                expected_elem_bytes,
            } => write!(
                f,
                "array data layout mismatch: len {actual_len} expected {expected_len}, elem_meta 0x{:x} expected 0x{:x}, elem_bytes {actual_elem_bytes} expected {expected_elem_bytes}",
                actual_elem_meta.to_raw(),
                expected_elem_meta.to_raw()
            ),
            Self::MissingArrayBoxStructMetadata(meta_id) => write!(
                f,
                "array data value-slot box references missing StructMeta id {meta_id}"
            ),
            Self::ArrayBoxLayoutMismatch => write!(
                f,
                "array data value-slot box layout does not match the canonical Array slot layout"
            ),
        }
    }
}

/// Validate the concrete payload carried by an interface boundary value.
///
/// This is the shared fail-closed path for VM spawn, extern replay, and native
/// extern returns. It canonicalizes live references and verifies every heap
/// layout fact before callers publish the value into VM-owned state.
#[inline]
pub fn validate_interface_value(
    gc: &Gc,
    module: &Module,
    slot0: u64,
    slot1: u64,
) -> Result<ValidatedInterfaceValue, InterfaceValueError> {
    let value_kind = interface::try_unpack_value_kind(slot0)
        .ok_or(InterfaceValueError::InvalidValueKind(slot0 as u8))?;
    if value_kind == ValueKind::Void {
        return if slot0 == 0 && slot1 == 0 {
            Ok(ValidatedInterfaceValue::Nil)
        } else {
            Err(InterfaceValueError::NonCanonicalNil { slot0, slot1 })
        };
    }
    if value_kind == ValueKind::Interface {
        return Err(InterfaceValueError::RawInterfaceKind);
    }
    let rttid = interface::unpack_rttid(slot0);
    let value_rttid = ValueRttid::try_new(rttid, value_kind)
        .ok_or(InterfaceValueError::InvalidRttid { rttid, value_kind })?;
    let expected_meta = module
        .canonical_value_meta_for_value_rttid(value_rttid)
        .ok_or(InterfaceValueError::UnresolvedRttid { rttid, value_kind })?;
    let canonical_data = if value_kind.may_contain_gc_refs() {
        validate_interface_data_object(gc, module, slot1, value_rttid, expected_meta)?
    } else {
        None
    };
    Ok(ValidatedInterfaceValue::Concrete {
        value_rttid,
        canonical_data,
    })
}

#[inline]
fn validate_interface_data_object(
    gc: &Gc,
    module: &Module,
    raw: u64,
    value_rttid: ValueRttid,
    expected_meta: ValueMeta,
) -> Result<Option<GcRef>, InterfaceValueError> {
    let value_kind = value_rttid.value_kind();
    if raw == 0 {
        return if matches!(value_kind, ValueKind::Struct | ValueKind::Array) {
            Err(InterfaceValueError::MissingAggregateObject(value_kind))
        } else {
            Ok(None)
        };
    }
    let Some(canonical) = gc.canonicalize_ref(raw as GcRef) else {
        return Err(InterfaceValueError::InvalidReference { raw });
    };
    let header = unsafe { Gc::header(canonical) };
    match value_kind {
        ValueKind::Struct | ValueKind::Pointer => {
            validate_interface_data_kind(header.kind(), ValueKind::Struct, value_kind)?;
            if header.meta_id() != expected_meta.meta_id() {
                return Err(InterfaceValueError::MetadataMismatch {
                    actual: header.meta_id(),
                    expected: expected_meta.meta_id(),
                });
            }
            let struct_meta = module
                .struct_metas
                .get(header.meta_id() as usize)
                .ok_or(InterfaceValueError::MissingStructMetadata(header.meta_id()))?;
            validate_interface_data_slot_count(
                header.slots as usize,
                struct_meta.slot_types.len(),
            )?;
        }
        ValueKind::Array => {
            validate_interface_array_data(module, canonical, header, value_rttid)?;
        }
        _ => {
            if let Some(expected_kind) = heap_object_kind(value_kind) {
                validate_interface_data_kind(header.kind(), expected_kind, value_kind)?;
            }
        }
    }
    Ok(Some(canonical))
}

#[inline]
fn validate_interface_data_kind(
    actual: ValueKind,
    expected: ValueKind,
    value_kind: ValueKind,
) -> Result<(), InterfaceValueError> {
    if actual != expected {
        return Err(InterfaceValueError::ObjectKindMismatch {
            actual,
            expected,
            value_kind,
        });
    }
    Ok(())
}

#[inline]
fn validate_interface_data_slot_count(
    actual: usize,
    expected: usize,
) -> Result<(), InterfaceValueError> {
    if actual != expected {
        return Err(InterfaceValueError::SlotCountMismatch { actual, expected });
    }
    Ok(())
}

#[inline]
fn validate_interface_array_data(
    module: &Module,
    array_ref: GcRef,
    header: &crate::gc::GcHeader,
    value_rttid: ValueRttid,
) -> Result<(), InterfaceValueError> {
    let (expected_len, expected_elem_rttid) = array_runtime_type(module, value_rttid)
        .ok_or(InterfaceValueError::MissingArrayLayout(value_rttid.rttid()))?;
    let expected_elem_meta = module
        .canonical_value_meta_for_value_rttid(expected_elem_rttid)
        .ok_or(InterfaceValueError::MissingArrayElementMetadata(
            expected_elem_rttid.rttid(),
        ))?;
    let expected_elem_bytes = sequence_element_physical_bytes(module, expected_elem_rttid).ok_or(
        InterfaceValueError::MissingArrayElementLayout(expected_elem_rttid.rttid()),
    )?;
    match header.kind() {
        ValueKind::Array => {}
        ValueKind::Struct => {
            return validate_interface_array_value_slot_box(module, header, value_rttid)
        }
        actual => return Err(InterfaceValueError::ArrayObjectKindMismatch(actual)),
    }
    // Safety: `array_ref` is canonical and its header kind is Array.
    let actual_len = unsafe { array::len(array_ref) };
    let actual_elem_meta = unsafe { array::elem_meta(array_ref) };
    let actual_elem_bytes = unsafe { array::elem_bytes(array_ref) };
    if actual_len != expected_len
        || actual_elem_meta != expected_elem_meta
        || actual_elem_bytes != expected_elem_bytes
    {
        return Err(InterfaceValueError::ArrayLayoutMismatch {
            actual_len,
            expected_len,
            actual_elem_meta,
            expected_elem_meta,
            actual_elem_bytes,
            expected_elem_bytes,
        });
    }
    Ok(())
}

#[inline]
fn validate_interface_array_value_slot_box(
    module: &Module,
    header: &crate::gc::GcHeader,
    value_rttid: ValueRttid,
) -> Result<(), InterfaceValueError> {
    let expected_layout = module
        .slot_layout_for_value_rttid(value_rttid)
        .ok_or(InterfaceValueError::MissingArrayLayout(value_rttid.rttid()))?;
    let struct_meta = module.struct_metas.get(header.meta_id() as usize).ok_or(
        InterfaceValueError::MissingArrayBoxStructMetadata(header.meta_id()),
    )?;
    if struct_meta.slot_types != expected_layout {
        return Err(InterfaceValueError::ArrayBoxLayoutMismatch);
    }
    validate_interface_data_slot_count(header.slots as usize, expected_layout.len())
}

/// Heap header kind used by a non-nil runtime value of `value_kind`.
#[inline]
pub fn heap_object_kind(value_kind: ValueKind) -> Option<ValueKind> {
    match value_kind {
        ValueKind::String
        | ValueKind::Slice
        | ValueKind::Map
        | ValueKind::Channel
        | ValueKind::Port
        | ValueKind::Closure
        | ValueKind::Island => Some(value_kind),
        ValueKind::Pointer => Some(ValueKind::Struct),
        _ => None,
    }
}

/// Resolve named wrappers around a fixed array and return its logical shape.
#[inline]
pub fn array_runtime_type(module: &Module, value_rttid: ValueRttid) -> Option<(usize, ValueRttid)> {
    let (_, runtime_type) = module
        .runtime_type_resolver()
        .resolve_value_rttid(value_rttid)?;
    let RuntimeType::Array { len, elem } = runtime_type else {
        return None;
    };
    Some((usize::try_from(*len).ok()?, *elem))
}

/// Physical byte stride used by runtime arrays and sequence containers.
#[inline]
pub fn sequence_element_physical_bytes(module: &Module, value_rttid: ValueRttid) -> Option<usize> {
    match value_rttid.value_kind() {
        ValueKind::Void => Some(0),
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => Some(1),
        ValueKind::Int16 | ValueKind::Uint16 => Some(2),
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => Some(4),
        _ => module
            .slot_layout_for_value_rttid(value_rttid)
            .and_then(|layout| layout.len().checked_mul(crate::slot::SLOT_BYTES)),
    }
}

#[cfg(test)]
mod tests {
    use alloc::collections::BTreeMap;

    use super::*;
    use vo_common_core::bytecode::NamedTypeMeta;
    use vo_common_core::types::ValueMeta;

    #[test]
    fn shared_layout_facts_resolve_named_arrays_and_scalar_strides() {
        let mut module = Module::new("value-layout-facts".to_string());
        let elem = ValueRttid::new(0, ValueKind::Uint16);
        let array = ValueRttid::new(1, ValueKind::Array);
        module.runtime_types.extend([
            RuntimeType::Basic(ValueKind::Uint16),
            RuntimeType::Array { len: 3, elem },
            RuntimeType::Named {
                id: 0,
                struct_meta_id: None,
            },
        ]);
        module.named_type_metas.push(NamedTypeMeta {
            name: "Words".to_string(),
            underlying_meta: ValueMeta::new(1, ValueKind::Array),
            underlying_rttid: array,
            methods: BTreeMap::new(),
        });

        assert_eq!(
            array_runtime_type(&module, ValueRttid::new(2, ValueKind::Array)),
            Some((3, elem))
        );
        assert_eq!(sequence_element_physical_bytes(&module, elem), Some(2));
        assert_eq!(
            heap_object_kind(ValueKind::Pointer),
            Some(ValueKind::Struct)
        );

        let named_array = ValueRttid::new(2, ValueKind::Array);
        let transfer = TransferType {
            meta_raw: ValueMeta::new(2, ValueKind::Array).to_raw(),
            rttid_raw: named_array.to_raw(),
            slots: 3,
        };
        assert_eq!(
            validate_transfer_layout(
                &module,
                &[SlotType::Value, SlotType::Value, SlotType::Value],
                0,
                &transfer,
            ),
            Ok(ValueMeta::new(2, ValueKind::Array))
        );
        assert_eq!(
            validate_transfer_layout(
                &module,
                &[SlotType::Value, SlotType::Value, SlotType::Value],
                0,
                &TransferType {
                    meta_raw: 0xff,
                    ..transfer
                },
            ),
            Err(TransferLayoutError::InvalidMetadata(0xff))
        );
    }

    #[test]
    fn shared_boundary_value_validation_is_fallible_and_canonical() {
        let mut module = Module::new("value-boundary-validation".to_string());
        module.runtime_types.extend([
            RuntimeType::Basic(ValueKind::String),
            RuntimeType::Basic(ValueKind::Uint16),
            RuntimeType::Array {
                len: 3,
                elem: ValueRttid::new(1, ValueKind::Uint16),
            },
        ]);
        let mut gc = Gc::new();
        let text = crate::objects::string::new_from_string(&mut gc, "value".to_string());
        let text_slot0 = interface::pack_slot0(0, 0, ValueKind::String);

        assert_eq!(
            validate_interface_value(&gc, &module, text_slot0, text as u64),
            Ok(ValidatedInterfaceValue::Concrete {
                value_rttid: ValueRttid::new(0, ValueKind::String),
                canonical_data: Some(text),
            })
        );

        let array = array::create(&mut gc, ValueMeta::new(0, ValueKind::Uint16), 2, 3);
        assert_eq!(
            validate_interface_value(
                &gc,
                &module,
                interface::pack_slot0(0, 2, ValueKind::Array),
                array as u64,
            ),
            Ok(ValidatedInterfaceValue::Concrete {
                value_rttid: ValueRttid::new(2, ValueKind::Array),
                canonical_data: Some(array),
            })
        );

        let wrong_kind = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
        assert!(matches!(
            canonicalize_concrete_heap_value(
                &gc,
                wrong_kind as u64,
                ValueMeta::new(0, ValueKind::String),
            ),
            Err(ConcreteHeapValueError::ObjectKindMismatch { .. })
        ));
        assert!(matches!(
            canonicalize_concrete_heap_value(
                &gc,
                wrong_kind as u64,
                ValueMeta::new(1, ValueKind::Pointer),
            ),
            Err(ConcreteHeapValueError::PointerMetadataMismatch {
                actual: 0,
                expected: 1
            })
        ));

        assert_eq!(
            validate_interface_value(&gc, &module, 0xff, 0),
            Err(InterfaceValueError::InvalidValueKind(0xff))
        );
        let reserved_rttid_slot0 =
            ((vo_common_core::types::INVALID_META_ID as u64) << 8) | ValueKind::String as u64;
        assert!(matches!(
            validate_interface_value(&gc, &module, reserved_rttid_slot0, 0),
            Err(InterfaceValueError::InvalidRttid { .. })
        ));
        assert!(matches!(
            validate_interface_value(&gc, &module, 1_u64 << 32, 0),
            Err(InterfaceValueError::NonCanonicalNil { .. })
        ));
    }
}
