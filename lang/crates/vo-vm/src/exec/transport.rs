//! Cross-island transport message helpers for channel data packing/unpacking.

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};

use vo_common_core::bytecode::{NamedTypeMeta, StructMeta};
use vo_common_core::RuntimeType;
use vo_runtime::gc::Gc;
use vo_runtime::gc::GcRef;
use vo_runtime::objects::queue_state::{QueueBacking, QueueKind, QueueMessage};
use vo_runtime::objects::{queue, queue_state};
use vo_runtime::pack::{
    pack_slots_with_named_type_metas,
    unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas,
    validate_packed_slots_expected_with_named_type_metas, PackedValue, QueueHandleInfo,
};
use vo_runtime::{ValueMeta, ValueRttid};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueueHandleMismatchField {
    Kind,
    Backing,
    HomeIsland,
    Closed,
    Capacity,
    ElemMeta,
    ElemRttid,
    ElemSlots,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueueHandleValidationError {
    MalformedPayload,
    EndpointRecvRejected,
    UnsupportedKind {
        endpoint_id: u64,
        kind: QueueKind,
    },
    TombstonedEndpoint {
        endpoint_id: u64,
    },
    MissingEndpointContract {
        endpoint_id: u64,
    },
    LiveEndpointMismatch {
        endpoint_id: u64,
        field: QueueHandleMismatchField,
    },
}

impl core::fmt::Display for QueueHandleValidationError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::MalformedPayload => write!(f, "transport payload did not match receiver layout"),
            Self::EndpointRecvRejected => write!(f, "endpoint receive payload was rejected"),
            Self::UnsupportedKind { endpoint_id, kind } => write!(
                f,
                "transport queue handle endpoint {} has unsupported kind {:?}",
                endpoint_id, kind
            ),
            Self::TombstonedEndpoint { endpoint_id } => write!(
                f,
                "transport queue handle endpoint {} is tombstoned",
                endpoint_id
            ),
            Self::MissingEndpointContract { endpoint_id } => write!(
                f,
                "transport queue handle endpoint {} has no live endpoint contract",
                endpoint_id
            ),
            Self::LiveEndpointMismatch { endpoint_id, field } => write!(
                f,
                "transport queue handle endpoint {} mismatched {:?}",
                endpoint_id, field
            ),
        }
    }
}

impl core::error::Error for QueueHandleValidationError {}

pub type QueueHandleValidationResult<T> = Result<T, QueueHandleValidationError>;

pub unsafe fn pack_transport_message(
    gc: &Gc,
    src: &[u64],
    elem_meta: ValueMeta,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
) -> Vec<u8> {
    pack_slots_with_named_type_metas(
        gc,
        src,
        elem_meta,
        struct_metas,
        named_type_metas,
        runtime_types,
    )
    .into_data()
}

fn remote_handle_closed_state(ch: GcRef) -> bool {
    unsafe { queue::remote_proxy(ch) }.closed
}

fn local_handle_closed_state(ch: GcRef) -> bool {
    // Safety: callers validate live endpoint handles before inspecting state.
    unsafe { queue::is_closed(ch) && queue::len(ch) == 0 }
}

fn endpoint_home_island(ch: GcRef) -> QueueHandleValidationResult<u32> {
    // Safety: callers validate live endpoint handles before inspecting state.
    match unsafe { queue_state::backing(ch) } {
        QueueBacking::Remote => Ok(unsafe { queue::remote_proxy(ch) }.home_island),
        QueueBacking::Local => unsafe { queue::home_info(ch) }
            .map(|info| info.home_island)
            .ok_or(QueueHandleValidationError::MissingEndpointContract { endpoint_id: 0 }),
    }
}

fn endpoint_closed_state(ch: GcRef) -> bool {
    // Safety: callers validate live endpoint handles before inspecting state.
    match unsafe { queue_state::backing(ch) } {
        QueueBacking::Remote => remote_handle_closed_state(ch),
        QueueBacking::Local => local_handle_closed_state(ch),
    }
}

fn validate_live_endpoint_handle(
    gc: &Gc,
    existing: GcRef,
    handle: QueueHandleInfo,
) -> QueueHandleValidationResult<()> {
    let existing = super::queue::validate_queue_handle(gc, existing, "transport queue handle")
        .map_err(|_| QueueHandleValidationError::LiveEndpointMismatch {
            endpoint_id: handle.endpoint_id,
            field: QueueHandleMismatchField::Kind,
        })?;
    if unsafe { queue_state::kind(existing) } != handle.kind {
        return Err(QueueHandleValidationError::LiveEndpointMismatch {
            endpoint_id: handle.endpoint_id,
            field: QueueHandleMismatchField::Kind,
        });
    }

    let endpoint_id_matches = match unsafe { queue_state::backing(existing) } {
        QueueBacking::Remote => {
            unsafe { queue::remote_proxy(existing) }.endpoint_id == handle.endpoint_id
        }
        QueueBacking::Local => unsafe { queue::home_info(existing) }
            .map(|info| info.endpoint_id == handle.endpoint_id)
            .ok_or(QueueHandleValidationError::MissingEndpointContract {
                endpoint_id: handle.endpoint_id,
            })?,
    };
    if !endpoint_id_matches {
        return Err(QueueHandleValidationError::LiveEndpointMismatch {
            endpoint_id: handle.endpoint_id,
            field: QueueHandleMismatchField::Backing,
        });
    }

    if endpoint_home_island(existing).map_err(|_| {
        QueueHandleValidationError::MissingEndpointContract {
            endpoint_id: handle.endpoint_id,
        }
    })? != handle.home_island
    {
        return Err(QueueHandleValidationError::LiveEndpointMismatch {
            endpoint_id: handle.endpoint_id,
            field: QueueHandleMismatchField::HomeIsland,
        });
    }

    if endpoint_closed_state(existing) != handle.closed {
        return Err(QueueHandleValidationError::LiveEndpointMismatch {
            endpoint_id: handle.endpoint_id,
            field: QueueHandleMismatchField::Closed,
        });
    }

    if unsafe { queue_state::capacity(existing) } as u64 != handle.cap {
        return Err(QueueHandleValidationError::LiveEndpointMismatch {
            endpoint_id: handle.endpoint_id,
            field: QueueHandleMismatchField::Capacity,
        });
    }
    if unsafe { queue_state::elem_meta(existing) } != handle.elem_meta {
        return Err(QueueHandleValidationError::LiveEndpointMismatch {
            endpoint_id: handle.endpoint_id,
            field: QueueHandleMismatchField::ElemMeta,
        });
    }
    if unsafe { queue_state::elem_rttid(existing) } != handle.elem_rttid {
        return Err(QueueHandleValidationError::LiveEndpointMismatch {
            endpoint_id: handle.endpoint_id,
            field: QueueHandleMismatchField::ElemRttid,
        });
    }
    if unsafe { queue_state::elem_slots(existing) } != handle.elem_slots {
        return Err(QueueHandleValidationError::LiveEndpointMismatch {
            endpoint_id: handle.endpoint_id,
            field: QueueHandleMismatchField::ElemSlots,
        });
    }

    Ok(())
}

pub fn try_resolve_unpacked_queue_handle(
    gc: &mut Gc,
    handle: QueueHandleInfo,
    endpoint_registry: &mut crate::vm::EndpointRegistry,
) -> QueueHandleValidationResult<GcRef> {
    if let Some(existing) = endpoint_registry.get_live(handle.endpoint_id) {
        validate_live_endpoint_handle(gc, existing, handle)?;
        return Ok(existing);
    }
    if endpoint_registry.is_tombstone(handle.endpoint_id) {
        return Err(QueueHandleValidationError::TombstonedEndpoint {
            endpoint_id: handle.endpoint_id,
        });
    }

    if handle.kind != QueueKind::Port {
        return Err(QueueHandleValidationError::UnsupportedKind {
            endpoint_id: handle.endpoint_id,
            kind: handle.kind,
        });
    }

    let queue_ref = queue::create_remote_proxy_with_closed(
        gc,
        handle.endpoint_id,
        handle.home_island,
        handle.cap,
        handle.elem_meta,
        handle.elem_rttid,
        handle.elem_slots,
        handle.closed,
    );
    endpoint_registry.register_live(handle.endpoint_id, queue_ref);
    Ok(queue_ref)
}

pub fn resolve_unpacked_queue_handle(
    gc: &mut Gc,
    handle: QueueHandleInfo,
    endpoint_registry: &mut crate::vm::EndpointRegistry,
) -> GcRef {
    try_resolve_unpacked_queue_handle(gc, handle, endpoint_registry)
        .unwrap_or(core::ptr::null_mut())
}

pub fn unpack_transport_message(
    gc: &mut Gc,
    data: &[u8],
    elem_meta: ValueMeta,
    elem_rttid: ValueRttid,
    elem_slots: usize,
    struct_metas: &[StructMeta],
    named_type_metas: &[NamedTypeMeta],
    runtime_types: &[RuntimeType],
    endpoint_registry: &mut crate::vm::EndpointRegistry,
) -> QueueHandleValidationResult<QueueMessage> {
    let packed = PackedValue::from_data(data.to_vec());
    let mut dst: Vec<u64> = vec![0; elem_slots];
    let mut error = None;
    let registry_snapshot = endpoint_registry.snapshot();
    if validate_packed_slots_expected_with_named_type_metas(
        packed.data(),
        elem_meta,
        elem_rttid,
        struct_metas,
        named_type_metas,
        runtime_types,
    )
    .is_err()
    {
        endpoint_registry.restore(registry_snapshot);
        return Err(QueueHandleValidationError::MalformedPayload);
    }
    // Safety: the packed payload was fully validated immediately above and
    // `dst` matches the declared element slot width.
    unsafe {
        unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas(
            gc,
            &packed,
            &mut dst,
            elem_meta,
            struct_metas,
            named_type_metas,
            runtime_types,
            |gc, handle| match try_resolve_unpacked_queue_handle(gc, handle, endpoint_registry) {
                Ok(queue_ref) => queue_ref,
                Err(err) => {
                    if error.is_none() {
                        error = Some(err);
                    }
                    core::ptr::null_mut()
                }
            },
        )
    };
    match error {
        Some(err) => {
            endpoint_registry.restore(registry_snapshot);
            Err(err)
        }
        None => Ok(dst.into_boxed_slice()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::EndpointRegistry;
    use vo_common_core::bytecode::FieldMeta;
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{SlotType, ValueKind, ValueRttid};

    fn base_handle() -> QueueHandleInfo {
        QueueHandleInfo {
            kind: QueueKind::Port,
            endpoint_id: 42,
            home_island: 7,
            cap: 3,
            elem_meta: ValueMeta::new(0, ValueKind::Int64),
            elem_rttid: ValueRttid::new(0, ValueKind::Int64),
            elem_slots: 1,
            closed: false,
        }
    }

    #[test]
    fn vm_transport_handle_validation_001_live_endpoint_mismatch_must_not_alias() {
        let mut gc = Gc::new();
        let mut endpoint_registry = EndpointRegistry::new();

        let existing =
            resolve_unpacked_queue_handle(&mut gc, base_handle(), &mut endpoint_registry);

        let mut mismatched = base_handle();
        mismatched.home_island = 8;
        let resolved = resolve_unpacked_queue_handle(&mut gc, mismatched, &mut endpoint_registry);

        assert_ne!(
            resolved, existing,
            "live endpoint handle metadata mismatch must be rejected, not aliased"
        );
    }

    fn assert_live_endpoint_mismatch(
        mutate: impl FnOnce(&mut QueueHandleInfo),
        field: QueueHandleMismatchField,
    ) {
        let mut gc = Gc::new();
        let mut endpoint_registry = EndpointRegistry::new();
        let existing =
            try_resolve_unpacked_queue_handle(&mut gc, base_handle(), &mut endpoint_registry)
                .expect("base handle should create a remote proxy");

        let mut mismatched = base_handle();
        mutate(&mut mismatched);
        let err = try_resolve_unpacked_queue_handle(&mut gc, mismatched, &mut endpoint_registry)
            .expect_err("mismatched live endpoint handle should be rejected");

        assert_eq!(
            err,
            QueueHandleValidationError::LiveEndpointMismatch {
                endpoint_id: base_handle().endpoint_id,
                field,
            }
        );
        assert_eq!(
            endpoint_registry.get_live(base_handle().endpoint_id),
            Some(existing)
        );
    }

    #[test]
    fn vm_transport_handle_validation_001_rejects_all_live_endpoint_mismatches() {
        assert_live_endpoint_mismatch(
            |handle| handle.kind = QueueKind::Chan,
            QueueHandleMismatchField::Kind,
        );
        assert_live_endpoint_mismatch(
            |handle| handle.home_island += 1,
            QueueHandleMismatchField::HomeIsland,
        );
        assert_live_endpoint_mismatch(
            |handle| handle.closed = !handle.closed,
            QueueHandleMismatchField::Closed,
        );
        assert_live_endpoint_mismatch(|handle| handle.cap += 1, QueueHandleMismatchField::Capacity);
        assert_live_endpoint_mismatch(
            |handle| handle.elem_meta = ValueMeta::new(0, ValueKind::Uint64),
            QueueHandleMismatchField::ElemMeta,
        );
        assert_live_endpoint_mismatch(
            |handle| handle.elem_rttid = ValueRttid::new(1, ValueKind::Int64),
            QueueHandleMismatchField::ElemRttid,
        );
        assert_live_endpoint_mismatch(
            |handle| handle.elem_slots += 1,
            QueueHandleMismatchField::ElemSlots,
        );
    }

    #[test]
    fn vm_transport_handle_validation_001_rejects_new_non_port_handle() {
        let mut gc = Gc::new();
        let mut endpoint_registry = EndpointRegistry::new();
        let mut handle = base_handle();
        handle.kind = QueueKind::Chan;

        let err = try_resolve_unpacked_queue_handle(&mut gc, handle, &mut endpoint_registry)
            .expect_err("new non-port queue handle should be rejected");

        assert_eq!(
            err,
            QueueHandleValidationError::UnsupportedKind {
                endpoint_id: base_handle().endpoint_id,
                kind: QueueKind::Chan,
            }
        );
        assert_eq!(endpoint_registry.get_live(base_handle().endpoint_id), None);
    }

    #[test]
    fn vm_transport_handle_validation_062_rejects_tombstoned_endpoint_rebind() {
        let mut gc = Gc::new();
        let mut endpoint_registry = EndpointRegistry::new();
        let handle = base_handle();
        endpoint_registry
            .mark_tombstone_with_response_source(handle.endpoint_id, Some(handle.home_island));

        let result = try_resolve_unpacked_queue_handle(&mut gc, handle, &mut endpoint_registry);

        assert!(
            result.is_err(),
            "unpacked queue handles must not overwrite endpoint tombstones while response authority may still be live"
        );
        assert_eq!(endpoint_registry.get_live(handle.endpoint_id), None);
        assert_eq!(
            endpoint_registry.tombstone_response_source(handle.endpoint_id),
            Some(Some(handle.home_island)),
            "rejected unpack must preserve tombstone response authority"
        );
    }

    fn push_u16(data: &mut Vec<u8>, value: u16) {
        data.extend_from_slice(&value.to_le_bytes());
    }

    fn push_u32(data: &mut Vec<u8>, value: u32) {
        data.extend_from_slice(&value.to_le_bytes());
    }

    fn push_u64(data: &mut Vec<u8>, value: u64) {
        data.extend_from_slice(&value.to_le_bytes());
    }

    fn push_queue_handle(data: &mut Vec<u8>, handle: QueueHandleInfo) {
        data.push(handle.kind.value_kind() as u8);
        data.push(1);
        data.push(handle.kind.value_kind() as u8);
        push_u64(data, handle.endpoint_id);
        push_u32(data, handle.home_island);
        push_u64(data, handle.cap);
        push_u32(data, handle.elem_meta.to_raw());
        push_u32(data, handle.elem_rttid.to_raw());
        push_u16(data, handle.elem_slots);
        data.push(handle.closed as u8);
    }

    #[test]
    fn vm_transport_handle_validation_002_unpack_error_rolls_back_new_endpoint_handles() {
        let mut first = base_handle();
        first.endpoint_id = 42;
        let mut second = base_handle();
        second.endpoint_id = 43;
        second.kind = QueueKind::Chan;

        let struct_metas = vec![StructMeta {
            slot_types: vec![SlotType::GcRef, SlotType::GcRef],
            fields: vec![
                FieldMeta {
                    name: "first".to_string(),
                    offset: 0,
                    slot_count: 1,
                    type_info: ValueRttid::new(0, ValueKind::Port),
                    embedded: false,
                    tag: None,
                },
                FieldMeta {
                    name: "second".to_string(),
                    offset: 1,
                    slot_count: 1,
                    type_info: ValueRttid::new(0, ValueKind::Channel),
                    embedded: false,
                    tag: None,
                },
            ],
            field_index: Default::default(),
        }];

        let mut data = Vec::new();
        data.push(ValueKind::Struct as u8);
        push_u32(&mut data, 0);
        push_u32(&mut data, 2);
        push_queue_handle(&mut data, first);
        push_queue_handle(&mut data, second);

        let mut gc = Gc::new();
        let mut endpoint_registry = EndpointRegistry::new();

        let err = unpack_transport_message(
            &mut gc,
            &data,
            ValueMeta::new(0, ValueKind::Struct),
            ValueRttid::new(0, ValueKind::Struct),
            2,
            &struct_metas,
            &[],
            &[],
            &mut endpoint_registry,
        )
        .expect_err("later invalid handle should reject the whole transport message");

        assert_eq!(
            err,
            QueueHandleValidationError::UnsupportedKind {
                endpoint_id: 43,
                kind: QueueKind::Chan,
            }
        );
        assert_eq!(endpoint_registry.get_live(42), None);
        assert_eq!(endpoint_registry.get_live(43), None);
    }

    #[test]
    fn vm_transport_handle_validation_002_unpack_cache_revalidates_same_endpoint_metadata() {
        let mut first = base_handle();
        first.endpoint_id = 42;
        let mut second = first;
        second.home_island = 99;

        let struct_metas = vec![StructMeta {
            slot_types: vec![SlotType::GcRef, SlotType::GcRef],
            fields: vec![
                FieldMeta {
                    name: "first".to_string(),
                    offset: 0,
                    slot_count: 1,
                    type_info: ValueRttid::new(0, ValueKind::Port),
                    embedded: false,
                    tag: None,
                },
                FieldMeta {
                    name: "second".to_string(),
                    offset: 1,
                    slot_count: 1,
                    type_info: ValueRttid::new(0, ValueKind::Port),
                    embedded: false,
                    tag: None,
                },
            ],
            field_index: Default::default(),
        }];

        let mut data = Vec::new();
        data.push(ValueKind::Struct as u8);
        push_u32(&mut data, 0);
        push_u32(&mut data, 2);
        push_queue_handle(&mut data, first);
        push_queue_handle(&mut data, second);

        let mut gc = Gc::new();
        let mut endpoint_registry = EndpointRegistry::new();

        let err = unpack_transport_message(
            &mut gc,
            &data,
            ValueMeta::new(0, ValueKind::Struct),
            ValueRttid::new(0, ValueKind::Struct),
            2,
            &struct_metas,
            &[],
            &[],
            &mut endpoint_registry,
        )
        .expect_err("same endpoint metadata drift must be revalidated");

        assert_eq!(
            err,
            QueueHandleValidationError::LiveEndpointMismatch {
                endpoint_id: 42,
                field: QueueHandleMismatchField::HomeIsland,
            }
        );
        assert_eq!(endpoint_registry.get_live(42), None);
    }

    #[test]
    fn vm_transport_unpack_layout_006_rejects_sequence_fixed_array_byte_width_drift() {
        let runtime_types = vec![
            RuntimeType::Port {
                dir: vo_common_core::ChanDir::Both,
                elem: ValueRttid::new(0, ValueKind::Int64),
            },
            RuntimeType::Array {
                len: 1,
                elem: ValueRttid::new(0, ValueKind::Port),
            },
        ];
        let mut data = Vec::new();
        data.push(ValueKind::Slice as u8);
        data.push(1);
        push_u64(&mut data, 1);
        push_u32(&mut data, ValueMeta::new(1, ValueKind::Array).to_raw());
        push_u32(&mut data, 1);

        let mut gc = Gc::new();
        let mut endpoint_registry = EndpointRegistry::new();
        let err = unpack_transport_message(
            &mut gc,
            &data,
            ValueMeta::new(0, ValueKind::Slice),
            ValueRttid::new(0, ValueKind::Slice),
            1,
            &[],
            &[],
            &runtime_types,
            &mut endpoint_registry,
        )
        .expect_err("sequence elem byte-width drift must reject before allocation");

        assert_eq!(err, QueueHandleValidationError::MalformedPayload);
        assert_eq!(endpoint_registry.get_live(42), None);
    }

    #[test]
    fn vm_transport_zero_slot_001_rejects_wire_kind_drift() {
        let struct_metas = vec![StructMeta {
            slot_types: Vec::new(),
            fields: Vec::new(),
            field_index: Default::default(),
        }];
        let mut data = Vec::new();
        data.push(ValueKind::Int64 as u8);
        push_u64(&mut data, 42);

        let mut gc = Gc::new();
        let mut endpoint_registry = EndpointRegistry::new();
        let err = unpack_transport_message(
            &mut gc,
            &data,
            ValueMeta::new(0, ValueKind::Struct),
            ValueRttid::new(0, ValueKind::Struct),
            0,
            &struct_metas,
            &[],
            &[RuntimeType::Struct {
                fields: Vec::new(),
                meta_id: 0,
            }],
            &mut endpoint_registry,
        )
        .expect_err("zero-slot values still require a matching packed envelope");

        assert_eq!(err, QueueHandleValidationError::MalformedPayload);
        assert_eq!(endpoint_registry.get_live(42), None);
    }

    #[test]
    fn vm_transport_queue_handle_layout_006_rejects_forged_elem_metadata_before_register() {
        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::String),
            RuntimeType::Port {
                dir: vo_common_core::ChanDir::Both,
                elem: ValueRttid::new(0, ValueKind::String),
            },
        ];
        let mut forged = base_handle();
        forged.endpoint_id = 99;
        forged.elem_meta = ValueMeta::new(0, ValueKind::Int64);
        forged.elem_rttid = ValueRttid::new(2, ValueKind::Int64);
        forged.elem_slots = 1;
        let mut data = Vec::new();
        push_queue_handle(&mut data, forged);

        let mut gc = Gc::new();
        let mut endpoint_registry = EndpointRegistry::new();
        let err = unpack_transport_message(
            &mut gc,
            &data,
            ValueMeta::new(0, ValueKind::Port),
            ValueRttid::new(1, ValueKind::Port),
            1,
            &[],
            &[],
            &runtime_types,
            &mut endpoint_registry,
        )
        .expect_err("forged queue handle element metadata must reject before registration");

        assert_eq!(err, QueueHandleValidationError::MalformedPayload);
        assert_eq!(endpoint_registry.get_live(99), None);
    }
}
