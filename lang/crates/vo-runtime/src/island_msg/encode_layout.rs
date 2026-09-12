//! Positive validation facts owned by one unpublished Island packet.
//!
//! The source metadata tables are borrowed for the validator's entire lifetime.
//! A complete transfer descriptor is the cache key; roots sharing a numeric type
//! ID cannot borrow a different kind's or physical width's validation result.
use super::IslandMessageEncodeError;
use crate::{RuntimeType, ValueKind, ValueMeta, ValueRttid};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
use hashbrown::HashMap;
use vo_common_core::bytecode::{NamedTypeMeta, RuntimeTypeResolver, StructMeta, TransferType};

type Key = (u32, u32, u16);
fn key(transfer: TransferType) -> Key {
    (transfer.meta_raw, transfer.rttid_raw, transfer.slots)
}

pub(super) struct TransferLayoutValidator<'a> {
    struct_metas: &'a [StructMeta],
    named_type_metas: &'a [NamedTypeMeta],
    runtime_types: &'a [RuntimeType],
    // Homogeneous packets need no cache allocation after their first check.
    last: Option<(Key, ValueMeta)>,
    other: HashMap<Key, ValueMeta>,
    #[cfg(test)]
    validations: usize,
}

impl<'a> TransferLayoutValidator<'a> {
    // One inline entry plus a soft-bounded optional table.
    const MAX_ENTRIES: usize = 4096;

    pub(super) fn new(
        struct_metas: &'a [StructMeta],
        named_type_metas: &'a [NamedTypeMeta],
        runtime_types: &'a [RuntimeType],
    ) -> Self {
        Self {
            struct_metas,
            named_type_metas,
            runtime_types,
            last: None,
            other: HashMap::new(),
            #[cfg(test)]
            validations: 0,
        }
    }

    pub(super) fn meta(
        &mut self,
        transfer: TransferType,
        field: &'static str,
    ) -> Result<ValueMeta, IslandMessageEncodeError> {
        let key = key(transfer);
        if let Some((previous, meta)) = self.last {
            if previous == key {
                return Ok(meta);
            }
        }
        if let Some(&meta) = self.other.get(&key) {
            return Ok(meta);
        }
        #[cfg(test)]
        {
            self.validations += 1;
        }
        let meta = checked_encode_transfer_meta(
            transfer,
            field,
            self.struct_metas,
            self.named_type_metas,
            self.runtime_types,
        )?;
        if let Some((previous, meta)) = self.last {
            if self.other.len() < Self::MAX_ENTRIES - 1 && self.other.try_reserve(1).is_ok() {
                self.other.insert(previous, meta);
            }
        }
        self.last = Some((key, meta));
        Ok(meta)
    }

    pub(super) fn metas(
        &mut self,
        transfers: &[TransferType],
        field: &'static str,
    ) -> Result<Vec<ValueMeta>, IslandMessageEncodeError> {
        let mut metas = Vec::new();
        metas.try_reserve_exact(transfers.len()).map_err(|_| {
            IslandMessageEncodeError::AllocationFailed {
                field,
                requested: transfers.len(),
            }
        })?;
        for &transfer in transfers {
            metas.push(self.meta(transfer, field)?);
        }
        Ok(metas)
    }
}

pub(super) fn checked_encode_transfer_meta(
    transfer_type: TransferType,
    field: &'static str,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Result<ValueMeta, IslandMessageEncodeError> {
    let value_meta = ValueMeta::try_from_raw(transfer_type.meta_raw)
        .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
    let value_rttid = ValueRttid::try_from_raw(transfer_type.rttid_raw)
        .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
    if value_meta.try_value_kind() != value_rttid.try_value_kind() {
        return Err(IslandMessageEncodeError::InvalidLayout { field });
    }
    if matches!(
        value_meta.try_value_kind(),
        Some(ValueKind::Channel | ValueKind::Closure | ValueKind::Interface | ValueKind::Island)
    ) {
        return Err(IslandMessageEncodeError::InvalidLayout { field });
    }
    let resolver = RuntimeTypeResolver::new(struct_metas, named_type_metas, runtime_types);
    if resolver.canonical_value_meta_for_value_rttid(value_rttid) != Some(value_meta)
        || resolver.slot_count_for_value_rttid(value_rttid) != Some(transfer_type.slots as usize)
    {
        return Err(IslandMessageEncodeError::InvalidLayout { field });
    }
    validate_sendable_type_graph(value_rttid, field, struct_metas, runtime_types, resolver)?;
    Ok(value_meta)
}

pub(super) fn validate_sendable_type_graph(
    root: ValueRttid,
    field: &'static str,
    struct_metas: &[StructMeta],
    runtime_types: &[RuntimeType],
    resolver: RuntimeTypeResolver<'_>,
) -> Result<(), IslandMessageEncodeError> {
    let root = ValueRttid::try_from_raw(root.to_raw())
        .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
    // Canonical basic types have no graph edges. Keep their complete identity
    // checks, without allocating traversal state proportional to the program.
    if matches!(runtime_types.get(root.rttid() as usize), Some(RuntimeType::Basic(kind)) if ValueKind::BASIC.contains(kind))
    {
        return if resolver
            .canonical_value_meta_for_value_rttid(root)
            .is_some()
            && resolver.slot_count_for_value_rttid(root).is_some()
        {
            Ok(())
        } else {
            Err(IslandMessageEncodeError::InvalidLayout { field })
        };
    }

    fn enqueue(
        value_rttid: ValueRttid,
        field: &'static str,
        runtime_types: &[RuntimeType],
        seen: &mut [Option<ValueKind>],
        pending: &mut Vec<ValueRttid>,
    ) -> Result<(), IslandMessageEncodeError> {
        let value_rttid = ValueRttid::try_from_raw(value_rttid.to_raw())
            .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
        let index = value_rttid.rttid() as usize;
        runtime_types
            .get(index)
            .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
        // Deduplication must include the kind embedded in each edge. A
        // previously queued type ID cannot validate a differently tagged edge.
        match seen[index] {
            Some(kind) if kind != value_rttid.value_kind() => {
                return Err(IslandMessageEncodeError::InvalidLayout { field });
            }
            Some(_) => {}
            None => {
                seen[index] = Some(value_rttid.value_kind());
                pending.push(value_rttid);
            }
        }
        Ok(())
    }

    let type_count = runtime_types.len();
    let mut seen = Vec::new();
    seen.try_reserve_exact(type_count)
        .map_err(|_| IslandMessageEncodeError::AllocationFailed {
            field,
            requested: type_count,
        })?;
    seen.resize(type_count, None);
    let mut pending = Vec::new();
    pending.try_reserve_exact(type_count).map_err(|_| {
        IslandMessageEncodeError::AllocationFailed {
            field,
            requested: type_count,
        }
    })?;
    enqueue(root, field, runtime_types, &mut seen, &mut pending)?;

    while let Some(value_rttid) = pending.pop() {
        if resolver
            .canonical_value_meta_for_value_rttid(value_rttid)
            .is_none()
            || resolver.slot_count_for_value_rttid(value_rttid).is_none()
        {
            return Err(IslandMessageEncodeError::InvalidLayout { field });
        }
        let (resolved_rttid, runtime_type) = resolver
            .resolve_value_rttid(value_rttid)
            .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
        let resolved_index = resolved_rttid.rttid() as usize;
        if let Some(resolved_seen) = seen.get_mut(resolved_index) {
            if resolved_seen.is_some_and(|kind| kind != resolved_rttid.value_kind()) {
                return Err(IslandMessageEncodeError::InvalidLayout { field });
            }
            *resolved_seen = Some(resolved_rttid.value_kind());
        } else {
            return Err(IslandMessageEncodeError::InvalidLayout { field });
        }

        let mut push = |nested| enqueue(nested, field, runtime_types, &mut seen, &mut pending);
        match runtime_type {
            RuntimeType::Basic(kind) => {
                if matches!(
                    kind,
                    ValueKind::Channel
                        | ValueKind::Closure
                        | ValueKind::Interface
                        | ValueKind::Island
                ) {
                    return Err(IslandMessageEncodeError::InvalidLayout { field });
                }
            }
            RuntimeType::Pointer(elem)
            | RuntimeType::Slice(elem)
            | RuntimeType::Array { elem, .. } => push(*elem)?,
            RuntimeType::Port { dir, elem } => {
                if *dir != vo_common_core::ChanDir::Send {
                    return Err(IslandMessageEncodeError::InvalidLayout { field });
                }
                push(*elem)?;
            }
            RuntimeType::Map { key, val } => {
                push(*key)?;
                push(*val)?;
            }
            RuntimeType::Struct { fields, meta_id } => {
                let physical = struct_metas
                    .get(*meta_id as usize)
                    .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
                if fields.len() != physical.fields.len() {
                    return Err(IslandMessageEncodeError::InvalidLayout { field });
                }
                let mut expected_offset = 0usize;
                for (identity_field, physical_field) in fields.iter().zip(&physical.fields) {
                    if identity_field.name != physical_field.name
                        || identity_field.typ != physical_field.type_info
                        || identity_field.embedded != physical_field.embedded
                        || identity_field.tag != physical_field.tag.as_deref().unwrap_or("")
                    {
                        return Err(IslandMessageEncodeError::InvalidLayout { field });
                    }
                    let field_layout = resolver
                        .slot_layout_for_value_rttid(physical_field.type_info)
                        .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
                    if physical_field.offset as usize != expected_offset
                        || physical_field.slot_count as usize != field_layout.len()
                    {
                        return Err(IslandMessageEncodeError::InvalidLayout { field });
                    }
                    let field_end = expected_offset
                        .checked_add(field_layout.len())
                        .ok_or(IslandMessageEncodeError::InvalidLayout { field })?;
                    if physical.slot_types.get(expected_offset..field_end)
                        != Some(field_layout.as_slice())
                    {
                        return Err(IslandMessageEncodeError::InvalidLayout { field });
                    }
                    expected_offset = field_end;
                    push(identity_field.typ)?;
                }
                let zero_size_workaround = expected_offset == 0
                    && physical.slot_types.as_slice() == [vo_common_core::SlotType::Value]
                    && !physical.fields.is_empty();
                if !physical.fields.is_empty()
                    && expected_offset != physical.slot_types.len()
                    && !zero_size_workaround
                {
                    return Err(IslandMessageEncodeError::InvalidLayout { field });
                }
            }
            RuntimeType::Chan { .. }
            | RuntimeType::Func { .. }
            | RuntimeType::Interface { .. }
            | RuntimeType::Tuple(_)
            | RuntimeType::Island
            | RuntimeType::Named { .. } => {
                return Err(IslandMessageEncodeError::InvalidLayout { field });
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ValueKind, ValueRttid};

    fn descriptor(id: u32, kind: ValueKind, slots: u16) -> TransferType {
        TransferType {
            meta_raw: ValueMeta::new(id, kind).to_raw(),
            rttid_raw: ValueRttid::new(id, kind).to_raw(),
            slots,
        }
    }

    #[test]
    fn basic_roots_preserve_type_identity_and_reject_non_sendable_kinds() {
        for stored in ValueKind::BASIC {
            let types = [RuntimeType::Basic(stored)];
            let resolver = RuntimeTypeResolver::new(&[], &[], &types);
            for tag in 0..=ValueKind::Island as u8 {
                let kind = ValueKind::from_u8(tag);
                let result = validate_sendable_type_graph(
                    ValueRttid::new(0, kind),
                    "basic root",
                    &[],
                    &types,
                    resolver,
                );
                assert_eq!(
                    result.is_ok(),
                    kind == stored,
                    "stored {stored:?}, tag {kind:?}"
                );
            }
            assert!(validate_sendable_type_graph(
                ValueRttid::new(1, stored),
                "missing root",
                &[],
                &types,
                resolver,
            )
            .is_err());
        }
        for kind in [
            ValueKind::Channel,
            ValueKind::Closure,
            ValueKind::Interface,
            ValueKind::Island,
        ] {
            let types = [RuntimeType::Basic(kind)];
            let resolver = RuntimeTypeResolver::new(&[], &[], &types);
            assert!(validate_sendable_type_graph(
                ValueRttid::new(0, kind),
                "non-sendable root",
                &[],
                &types,
                resolver,
            )
            .is_err());
        }
    }

    #[test]
    fn homogeneous_capture_and_argument_lists_share_one_validation_without_a_hash_table() {
        let types = [RuntimeType::Basic(ValueKind::Int64)];
        let mut validator = TransferLayoutValidator::new(&[], &[], &types);
        let transfer = descriptor(0, ValueKind::Int64, 1);
        let expected = ValueMeta::new(0, ValueKind::Int64);
        assert_eq!(
            validator.metas(&[transfer; 128], "captures").unwrap(),
            [expected; 128]
        );
        assert_eq!(
            validator.metas(&[transfer; 128], "arguments").unwrap(),
            [expected; 128]
        );
        assert_eq!(validator.validations, 1);
        assert_eq!(validator.other.capacity(), 0);
    }

    #[test]
    fn cached_success_cannot_validate_a_different_tag_width_or_program() {
        let types = [RuntimeType::Basic(ValueKind::Int64)];
        let mut validator = TransferLayoutValidator::new(&[], &[], &types);
        let valid = descriptor(0, ValueKind::Int64, 1);
        validator.meta(valid, "captures").unwrap();
        for invalid in [
            descriptor(0, ValueKind::Uint64, 1),
            descriptor(0, ValueKind::Channel, 1),
            descriptor(0, ValueKind::Int64, 2),
            TransferType {
                meta_raw: u32::MAX,
                ..valid
            },
            TransferType {
                rttid_raw: u32::MAX,
                ..valid
            },
        ] {
            assert_eq!(
                validator.meta(invalid, "arguments"),
                Err(IslandMessageEncodeError::InvalidLayout { field: "arguments" })
            );
        }
        assert_eq!(
            validator.meta(valid, "arguments").unwrap(),
            ValueMeta::new(0, ValueKind::Int64)
        );
        let other_types = [RuntimeType::Basic(ValueKind::Uint64)];
        let mut other = TransferLayoutValidator::new(&[], &[], &other_types);
        assert!(other.meta(valid, "different program").is_err());
    }

    #[test]
    fn saturation_keeps_valid_new_shapes_encodable_and_reuses_old_facts() {
        let count = TransferLayoutValidator::MAX_ENTRIES + 3;
        let mut types = vec![RuntimeType::Basic(ValueKind::Int64)];
        types.extend((0..count).map(|_| RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new(0, ValueKind::Int64),
        }));
        let mut validator = TransferLayoutValidator::new(&[], &[], &types);
        for id in 1..=count {
            assert_eq!(
                validator
                    .meta(descriptor(id as u32, ValueKind::Array, 1), "array")
                    .unwrap(),
                ValueMeta::new(id as u32, ValueKind::Array)
            );
        }
        assert_eq!(
            validator.other.len(),
            TransferLayoutValidator::MAX_ENTRIES - 1
        );
        let validations = validator.validations;
        validator
            .meta(descriptor(1, ValueKind::Array, 1), "cached")
            .unwrap();
        validator
            .meta(descriptor(count as u32, ValueKind::Array, 1), "last")
            .unwrap();
        assert_eq!(validator.validations, validations);
        validator
            .meta(
                descriptor((count - 1) as u32, ValueKind::Array, 1),
                "uncached",
            )
            .unwrap();
        assert_eq!(validator.validations, validations + 1);
    }
}
