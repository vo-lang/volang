//! Image metadata and allocation descriptor construction.
use super::*;

pub(super) fn interface_array_assertion_layout(
    module: &VoModule,
    target_rttid: u32,
    result_slots: u16,
) -> Result<Option<InterfaceArrayAssertionLayout>, WasmAotError> {
    let value_rttid = module.value_rttid_for_rttid(target_rttid).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "interface assertion target runtime type {target_rttid} cannot be resolved"
        ))
    })?;
    if value_rttid.value_kind() != ValueKind::Array {
        return Ok(None);
    }
    let (_, runtime_type) = module
        .runtime_type_resolver()
        .resolve_value_rttid(value_rttid)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "interface array assertion target runtime type {target_rttid} cannot be resolved"
            ))
        })?;
    let RuntimeType::Array { len, elem } = runtime_type else {
        return Err(WasmAotError::InvalidModule(format!(
            "interface assertion target runtime type {target_rttid} has array value kind without array metadata"
        )));
    };
    let elem_layout = module.slot_layout_for_value_rttid(*elem).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "interface array assertion target runtime type {target_rttid} has no element layout"
        ))
    })?;
    let expected_slots = usize::try_from(*len)
        .ok()
        .and_then(|len| len.checked_mul(elem_layout.len()))
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "interface array assertion target runtime type {target_rttid} exceeds the slot domain"
            ))
        })?;
    if expected_slots != usize::from(result_slots) {
        return Err(WasmAotError::InvalidModule(format!(
            "interface array assertion target runtime type {target_rttid} has {expected_slots} logical slots, metadata declares {result_slots}"
        )));
    }
    if expected_slots == 0 {
        return Ok(Some(InterfaceArrayAssertionLayout {
            len: 0,
            elem_bytes: 0,
            needs_sign_extend: false,
        }));
    }
    let len = u16::try_from(*len).map_err(|_| {
        WasmAotError::InvalidModule(format!(
            "interface array assertion target runtime type {target_rttid} exceeds the slot domain"
        ))
    })?;
    let (elem_bytes, needs_sign_extend) = match elem.value_kind() {
        ValueKind::Bool | ValueKind::Uint8 => (1, false),
        ValueKind::Int8 => (1, true),
        ValueKind::Uint16 => (2, false),
        ValueKind::Int16 => (2, true),
        ValueKind::Uint32 | ValueKind::Float32 => (4, false),
        ValueKind::Int32 => (4, true),
        _ => {
            let bytes = elem_layout.len().checked_mul(8).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "interface array assertion target runtime type {target_rttid} element layout overflows"
                ))
            })?;
            let bytes = u32::try_from(bytes).map_err(|_| {
                WasmAotError::InvalidModule(format!(
                    "interface array assertion target runtime type {target_rttid} element layout is too wide"
                ))
            })?;
            (bytes, false)
        }
    };
    if elem_bytes < 8 && elem_layout.len() != 1 {
        return Err(WasmAotError::InvalidModule(format!(
            "interface array assertion target runtime type {target_rttid} has an invalid packed element layout"
        )));
    }
    Ok(Some(InterfaceArrayAssertionLayout {
        len,
        elem_bytes,
        needs_sign_extend,
    }))
}

pub(super) fn encoded_slot_types(layout: &[vo_common_core::SlotType]) -> Vec<u8> {
    layout.iter().map(|slot| *slot as u8).collect()
}

pub(super) fn sequence_descriptor(
    layout: &[vo_common_core::SlotType],
    elem_bytes: u32,
    needs_sign_extend: bool,
) -> AllocationDescriptor {
    AllocationDescriptor::Sequence {
        elem_slot_types: encoded_slot_types(layout),
        elem_bytes,
        needs_sign_extend,
    }
}

pub(super) fn sequence_element_storage(kind: ValueKind, logical_slots: usize) -> (u32, bool) {
    match kind {
        ValueKind::Void => (0, false),
        ValueKind::Bool | ValueKind::Uint8 => (1, false),
        ValueKind::Int8 => (1, true),
        ValueKind::Uint16 => (2, false),
        ValueKind::Int16 => (2, true),
        ValueKind::Uint32 | ValueKind::Float32 => (4, false),
        ValueKind::Int32 => (4, true),
        _ => ((logical_slots as u32) * 8, false),
    }
}

pub(super) fn build_allocation_descriptors(
    module: &VoModule,
    reachable: &[u32],
) -> Result<AllocationDescriptors, WasmAotError> {
    let mut requested =
        BTreeMap::<(u32, usize), (AllocationDescriptor, Option<AllocationDescriptor>)>::new();
    let panic_context_descriptor = AllocationDescriptor::Fixed {
        slot_types: vec![
            vo_common_core::SlotType::Interface0 as u8,
            vo_common_core::SlotType::Interface1 as u8,
            vo_common_core::SlotType::Value as u8,
            vo_common_core::SlotType::GcRef as u8,
        ],
    };
    let mut island_state_slots = vec![vo_common_core::SlotType::Value as u8];
    island_state_slots.extend(
        module
            .globals
            .iter()
            .flat_map(|global| encoded_slot_types(&global.slot_types)),
    );
    let island_state_descriptor = AllocationDescriptor::Fixed {
        slot_types: island_state_slots,
    };
    let mut unique = BTreeSet::from([
        AllocationDescriptor::None,
        AllocationDescriptor::Frame,
        panic_context_descriptor.clone(),
        island_state_descriptor.clone(),
    ]);
    let generic_sequence_layouts = [
        (ValueKind::Void, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Bool, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Int, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Int8, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Int16, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Int32, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Int64, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Uint, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Uint8, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Uint16, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Uint32, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Uint64, vec![vo_common_core::SlotType::Value]),
        (ValueKind::Float32, vec![vo_common_core::SlotType::Float]),
        (ValueKind::Float64, vec![vo_common_core::SlotType::Float]),
        (
            ValueKind::Interface,
            vec![
                vo_common_core::SlotType::Interface0,
                vo_common_core::SlotType::Interface1,
            ],
        ),
        (ValueKind::String, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Slice, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Map, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Channel, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Closure, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Pointer, vec![vo_common_core::SlotType::GcRef]),
        (ValueKind::Port, vec![vo_common_core::SlotType::GcBase]),
        (ValueKind::Island, vec![vo_common_core::SlotType::GcBase]),
    ];
    let mut requested_sequence_by_kind = BTreeMap::new();
    for (kind, layout) in generic_sequence_layouts {
        let (elem_bytes, needs_sign_extend) = sequence_element_storage(kind, layout.len());
        let descriptor = sequence_descriptor(&layout, elem_bytes, needs_sign_extend);
        unique.insert(descriptor.clone());
        requested_sequence_by_kind.insert(kind as u8, descriptor);
    }
    let mut requested_sequence_by_meta = BTreeMap::new();
    let mut requested_sequence_by_value = BTreeMap::new();
    let mut requested_fixed_by_struct_meta = BTreeMap::new();
    let mut requested_fixed_by_value = BTreeMap::new();
    let mut requested_map_by_value = BTreeMap::new();
    let mut requested_closure_by_function = BTreeMap::new();
    for (meta_id, metadata) in module.struct_metas.iter().enumerate() {
        let meta_id: u32 = meta_id.try_into().map_err(|_| {
            WasmAotError::InvalidModule("struct metadata index exceeds wasm32".into())
        })?;
        let value_meta = ValueMeta::try_new(meta_id, ValueKind::Struct).ok_or_else(|| {
            WasmAotError::InvalidModule("struct metadata exceeds the packed type domain".into())
        })?;
        let descriptor = sequence_descriptor(
            &metadata.slot_types,
            (metadata.slot_types.len() as u32) * 8,
            false,
        );
        unique.insert(descriptor.clone());
        requested_sequence_by_meta.insert(value_meta.to_raw(), descriptor);
        let fixed_descriptor = AllocationDescriptor::Fixed {
            slot_types: encoded_slot_types(&metadata.slot_types),
        };
        unique.insert(fixed_descriptor.clone());
        requested_fixed_by_struct_meta.insert(meta_id, fixed_descriptor);
    }
    for rttid in 0..module.runtime_types.len() {
        let rttid: u32 = rttid
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("runtime type index exceeds wasm32".into()))?;
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let value_layout = module
            .slot_layout_for_value_rttid(value_rttid)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime value type {rttid} has no physical slot layout"
                ))
            })?;
        let fixed_descriptor = AllocationDescriptor::Fixed {
            slot_types: encoded_slot_types(&value_layout),
        };
        unique.insert(fixed_descriptor.clone());
        requested_fixed_by_value.insert(value_rttid.to_raw(), fixed_descriptor);
        let (elem_bytes, needs_sign_extend) =
            sequence_element_storage(value_rttid.value_kind(), value_layout.len());
        let sequence = sequence_descriptor(&value_layout, elem_bytes, needs_sign_extend);
        unique.insert(sequence.clone());
        requested_sequence_by_value.insert(value_rttid.to_raw(), sequence);
        if let Some((_, RuntimeType::Map { key, val })) = module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
        {
            let key_layout = module.slot_layout_for_value_rttid(*key).ok_or_else(|| {
                WasmAotError::InvalidModule(format!("map runtime type {rttid} has no key layout"))
            })?;
            let value_layout = module.slot_layout_for_value_rttid(*val).ok_or_else(|| {
                WasmAotError::InvalidModule(format!("map runtime type {rttid} has no value layout"))
            })?;
            let key_slot_types = encoded_slot_types(&key_layout);
            let value_slot_types = encoded_slot_types(&value_layout);
            let map = AllocationDescriptor::Map {
                key_slot_types: key_slot_types.clone(),
                value_slot_types: value_slot_types.clone(),
            };
            let entries = AllocationDescriptor::MapEntries {
                key_slot_types,
                value_slot_types,
            };
            unique.insert(map.clone());
            unique.insert(entries.clone());
            requested_map_by_value.insert(value_rttid.to_raw(), (map, entries));
        }
        if value_rttid.value_kind() != ValueKind::Array {
            continue;
        }
        let layout = module
            .slot_layout_for_value_rttid(value_rttid)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "array runtime type {rttid} has no physical slot layout"
                ))
            })?;
        let value_meta = ValueMeta::try_new(rttid, ValueKind::Array).ok_or_else(|| {
            WasmAotError::InvalidModule("array runtime type exceeds the packed type domain".into())
        })?;
        let descriptor = sequence_descriptor(&layout, (layout.len() as u32) * 8, false);
        unique.insert(descriptor.clone());
        requested_sequence_by_meta.insert(value_meta.to_raw(), descriptor);
    }
    for method in module
        .named_type_metas
        .iter()
        .flat_map(|named| named.methods.values())
    {
        let target = module
            .functions
            .get(method.func_id as usize)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "dynamic method metadata references missing function {}",
                    method.func_id
                ))
            })?;
        let receiver = target
            .slot_types
            .get(..usize::from(target.recv_slots))
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "dynamic method {} receiver layout is truncated",
                    target.name
                ))
            })?;
        let mut slot_types = vec![vo_common_core::SlotType::Value as u8];
        slot_types.extend(encoded_slot_types(receiver));
        let descriptor = AllocationDescriptor::Fixed { slot_types };
        unique.insert(descriptor.clone());
        requested_closure_by_function.insert(method.func_id, descriptor);
    }
    for function_id in reachable {
        let function = &module.functions[*function_id as usize];
        for (pc, instruction) in function.code.iter().enumerate() {
            let metadata = function.instruction_metadata.get(pc).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "function {function_id} pc {pc} is missing instruction metadata"
                ))
            })?;
            let descriptor = match instruction.opcode() {
                Opcode::PtrNew => Some((
                    AllocationDescriptor::Fixed {
                        slot_types: encoded_slot_types(metadata.ptr_value_layout().ok_or_else(
                            || {
                                WasmAotError::InvalidModule(format!(
                                    "{} pc {pc} is missing PtrLayout metadata",
                                    function.name
                                ))
                            },
                        )?),
                    },
                    None,
                )),
                Opcode::StrConcat => Some((AllocationDescriptor::None, None)),
                Opcode::StrSlice => Some((
                    AllocationDescriptor::Fixed {
                        slot_types: vec![
                            vo_common_core::SlotType::Value as u8,
                            vo_common_core::SlotType::GcRef as u8,
                        ],
                    },
                    None,
                )),
                Opcode::ArrayNew | Opcode::SliceNew | Opcode::SliceAppend => {
                    let layout = metadata.elem_slot_layout().ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} is missing ElemLayout metadata",
                            function.name
                        ))
                    })?;
                    let element = metadata.elem_layout().ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} has invalid ElemLayout metadata",
                            function.name
                        ))
                    })?;
                    Some((
                        sequence_descriptor(
                            layout,
                            element.bytes as u32,
                            element.needs_sign_extend,
                        ),
                        None,
                    ))
                }
                Opcode::SliceSlice => Some((
                    AllocationDescriptor::Fixed {
                        slot_types: vec![
                            vo_common_core::SlotType::GcRef as u8,
                            vo_common_core::SlotType::Value as u8,
                            vo_common_core::SlotType::Value as u8,
                            vo_common_core::SlotType::Value as u8,
                        ],
                    },
                    None,
                )),
                Opcode::MapNew => {
                    let (key_layout, value_layout) =
                        metadata.map_new_layout_slices().ok_or_else(|| {
                            WasmAotError::InvalidModule(format!(
                                "{} pc {pc} is missing MapNew metadata",
                                function.name
                            ))
                        })?;
                    let key_slot_types = encoded_slot_types(key_layout);
                    let value_slot_types = encoded_slot_types(value_layout);
                    Some((
                        AllocationDescriptor::Map {
                            key_slot_types: key_slot_types.clone(),
                            value_slot_types: value_slot_types.clone(),
                        },
                        Some(AllocationDescriptor::MapEntries {
                            key_slot_types,
                            value_slot_types,
                        }),
                    ))
                }
                Opcode::QueueNew => Some((
                    AllocationDescriptor::Queue {
                        elem_slot_types: encoded_slot_types(
                            metadata.queue_elem_layout().ok_or_else(|| {
                                WasmAotError::InvalidModule(format!(
                                    "{} pc {pc} is missing QueueLayout metadata",
                                    function.name
                                ))
                            })?,
                        ),
                    },
                    None,
                )),
                Opcode::ClosureNew => {
                    let target = instruction.closure_new_func_id();
                    let target = module.functions.get(target as usize).ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} references missing closure target {target}",
                            function.name
                        ))
                    })?;
                    let mut slot_types = vec![vo_common_core::SlotType::Value as u8];
                    slot_types.extend(encoded_slot_types(&target.capture_slot_types));
                    Some((AllocationDescriptor::Fixed { slot_types }, None))
                }
                Opcode::DeferPush | Opcode::ErrDeferPush => {
                    let arg_layout = if instruction.call_shape_is_closure() {
                        metadata
                            .call_layout_slices()
                            .map(|layouts| layouts.0)
                            .ok_or_else(|| {
                                WasmAotError::InvalidModule(format!(
                                    "{} pc {pc} is missing closure defer CallLayout metadata",
                                    function.name
                                ))
                            })?
                    } else {
                        let target = instruction.call_shape_static_func_id();
                        let callee = module.functions.get(target as usize).ok_or_else(|| {
                            WasmAotError::InvalidModule(format!(
                                "{} pc {pc} defers missing function {target}",
                                function.name
                            ))
                        })?;
                        callee
                            .slot_types
                            .get(..usize::from(callee.param_slots))
                            .ok_or_else(|| {
                                WasmAotError::InvalidModule(format!(
                                    "{} pc {pc} callee {target} parameter layout is truncated",
                                    function.name
                                ))
                            })?
                    };
                    let mut slot_types = vec![
                        vo_common_core::SlotType::GcRef as u8,
                        vo_common_core::SlotType::Value as u8,
                        vo_common_core::SlotType::GcRef as u8,
                        vo_common_core::SlotType::Value as u8,
                        vo_common_core::SlotType::Value as u8,
                        vo_common_core::SlotType::Value as u8,
                        vo_common_core::SlotType::Value as u8,
                    ];
                    slot_types.extend(encoded_slot_types(arg_layout));
                    Some((AllocationDescriptor::Fixed { slot_types }, None))
                }
                _ => None,
            };
            if let Some(descriptor) = descriptor {
                unique.insert(descriptor.0.clone());
                if let Some(secondary) = &descriptor.1 {
                    unique.insert(secondary.clone());
                }
                requested.insert((*function_id, pc), descriptor);
            }
        }
    }
    let entries: Vec<_> = unique.into_iter().collect();
    let ids: BTreeMap<_, _> = entries
        .iter()
        .cloned()
        .enumerate()
        .map(|(id, descriptor)| (descriptor, id as u32))
        .collect();
    let frame = *ids
        .get(&AllocationDescriptor::Frame)
        .expect("frame descriptor is always registered");
    let panic_context = ids[&panic_context_descriptor];
    let island_state = ids[&island_state_descriptor];
    let sequence_by_kind = requested_sequence_by_kind
        .into_iter()
        .map(|(kind, descriptor)| (kind, ids[&descriptor]))
        .collect();
    let sequence_by_meta = requested_sequence_by_meta
        .into_iter()
        .map(|(value_meta, descriptor)| (value_meta, ids[&descriptor]))
        .collect();
    let sequence_by_value = requested_sequence_by_value
        .into_iter()
        .map(|(value_rttid, descriptor)| (value_rttid, ids[&descriptor]))
        .collect();
    let fixed_by_struct_meta = requested_fixed_by_struct_meta
        .into_iter()
        .map(|(meta_id, descriptor)| (meta_id, ids[&descriptor]))
        .collect();
    let fixed_by_value = requested_fixed_by_value
        .into_iter()
        .map(|(value_rttid, descriptor)| (value_rttid, ids[&descriptor]))
        .collect();
    let map_by_value = requested_map_by_value
        .into_iter()
        .map(|(value_rttid, (map, entries))| (value_rttid, (ids[&map], ids[&entries])))
        .collect();
    let closure_by_function = requested_closure_by_function
        .into_iter()
        .map(|(function_id, descriptor)| (function_id, ids[&descriptor]))
        .collect();
    let sites = requested
        .into_iter()
        .map(|(site, (primary, secondary))| {
            let primary = ids[&primary];
            let secondary = secondary.map(|descriptor| ids[&descriptor]);
            (site, (primary, secondary))
        })
        .collect();
    Ok(AllocationDescriptors {
        entries,
        sites,
        sequence_by_kind,
        sequence_by_meta,
        sequence_by_value,
        fixed_by_struct_meta,
        fixed_by_value,
        map_by_value,
        closure_by_function,
        frame,
        panic_context,
        island_state,
    })
}

pub(super) fn align_up(value: u32, alignment: u32) -> Result<u32, WasmAotError> {
    value
        .checked_add(alignment - 1)
        .map(|value| value & !(alignment - 1))
        .ok_or_else(|| WasmAotError::InvalidModule("WebAssembly memory layout overflow".into()))
}

pub(super) fn build_static_data(module: &VoModule) -> Result<StaticData, WasmAotError> {
    fn push_string(bytes: &mut Vec<u8>, value: &str) -> Result<u32, WasmAotError> {
        if value.is_empty() {
            return Ok(0);
        }
        while (STATIC_DATA_START as usize + bytes.len()) & 7 != 0 {
            bytes.push(0);
        }
        let header = STATIC_DATA_START
            .checked_add(bytes.len() as u32)
            .ok_or_else(|| WasmAotError::InvalidModule("static string offset overflow".into()))?;
        let data_ptr = header
            .checked_add(16)
            .ok_or_else(|| WasmAotError::InvalidModule("static string data overflow".into()))?;
        bytes.extend_from_slice(&(value.len() as u64).to_le_bytes());
        bytes.extend_from_slice(&u64::from(data_ptr).to_le_bytes());
        bytes.extend_from_slice(value.as_bytes());
        Ok(header)
    }

    let mut bytes = Vec::new();
    let mut string_refs = Vec::with_capacity(module.constants.len());
    for constant in &module.constants {
        let Constant::String(value) = constant else {
            string_refs.push(0);
            continue;
        };
        string_refs.push(push_string(&mut bytes, value)?);
    }
    let runtime_messages = [
        "",
        "runtime error: integer divide by zero",
        "runtime error: negative shift amount",
        "runtime error: index out of range",
        "runtime error: out of memory",
        "",
        "runtime error: send on closed channel",
        "",
        "runtime error: hash of unhashable type",
        "runtime error: stack overflow",
        "runtime error: comparing uncomparable type in interface value",
        "runtime error: interface conversion: interface is nil, not",
        "",
        "",
        "",
    ];
    let mut runtime_panic_refs = [0; 15];
    for (index, message) in runtime_messages.into_iter().enumerate() {
        runtime_panic_refs[index] = push_string(&mut bytes, message)?;
    }
    let nil_reference_panic_ref =
        push_string(&mut bytes, "runtime error: nil pointer dereference")?;
    let nil_map_write_panic_ref =
        push_string(&mut bytes, "runtime error: assignment to entry in nil map")?;
    let makeslice_negative_len_panic_ref =
        push_string(&mut bytes, "runtime error: makeslice: len out of range")?;
    let makeslice_cap_panic_ref =
        push_string(&mut bytes, "runtime error: makeslice: cap out of range")?;
    let makeslice_len_gt_cap_panic_ref =
        push_string(&mut bytes, "runtime error: makeslice: len larger than cap")?;
    let makechan_panic_ref = push_string(&mut bytes, "runtime error: makechan: size out of range")?;
    let makeport_panic_ref = push_string(&mut bytes, "runtime error: makeport: size out of range")?;
    let index_panic_prefix_ref = push_string(&mut bytes, "runtime error: index out of range [")?;
    let index_panic_middle_ref = push_string(&mut bytes, "] with length ")?;
    let mut dynamic_strings = BTreeSet::from([
        "dynamic access: unknown error".to_string(),
        "dynamic access: base value is nil".to_string(),
        "dynamic access: field does not exist".to_string(),
        "dynamic access: invalid index type".to_string(),
        "dynamic access: index out of bounds".to_string(),
        "dynamic access: cannot call value".to_string(),
        "dynamic access: signature mismatch".to_string(),
        "dynamic access: type mismatch".to_string(),
        "cannot access field on nil".to_string(),
        "cannot access field on nil map".to_string(),
        "cannot index nil".to_string(),
        "cannot index nil slice".to_string(),
        "cannot index nil map".to_string(),
        "cannot set field on nil".to_string(),
        "cannot set field on nil map".to_string(),
        "cannot set index on nil".to_string(),
        "cannot set index on nil slice".to_string(),
        "cannot set index on nil map".to_string(),
        "field not found".to_string(),
        "map key not found".to_string(),
        "map key type mismatch".to_string(),
        "map key is not hashable".to_string(),
        "nil pointer in embedding path".to_string(),
        "index must be integer".to_string(),
        "array index out of bounds".to_string(),
        "slice index out of bounds".to_string(),
        "string index out of bounds".to_string(),
        "dynamic target type mismatch".to_string(),
        "spread arg must be slice".to_string(),
        "dynamic packed argument layout is invalid".to_string(),
        "dynamic packed argument length exceeds wasm32".to_string(),
        "cannot call nil".to_string(),
        "cannot call method on nil".to_string(),
        "call target contains an invalid value-kind tag".to_string(),
        "cannot call value".to_string(),
        "closure is null".to_string(),
        "dynamic call panicked".to_string(),
        "invalid closure signature".to_string(),
        "return count mismatch: hint: adjust LHS variable count to match function signature"
            .to_string(),
        "dynamic return type mismatch".to_string(),
        "parameter count mismatch".to_string(),
        "argument type mismatch".to_string(),
        "method not found".to_string(),
        "method lookup returned a non-callable value".to_string(),
        "CallObject only supports single return".to_string(),
        "CallObject return type mismatch".to_string(),
        "type does not support this access".to_string(),
        "type does not support this assignment".to_string(),
    ]);
    for metadata in &module.struct_metas {
        for field in &metadata.fields {
            dynamic_strings.insert(field.name.clone());
            if let Some(name) = dynamic_field_name(field) {
                dynamic_strings.insert(name.to_string());
            }
        }
    }
    for named in &module.named_type_metas {
        dynamic_strings.extend(named.methods.keys().cloned());
    }
    let mut dynamic_string_refs = BTreeMap::new();
    for value in dynamic_strings {
        dynamic_string_refs.insert(value.clone(), push_string(&mut bytes, &value)?);
    }
    let mut dynamic_dispatch = BTreeMap::new();
    for (function_id, function) in module.functions.iter().enumerate() {
        for (pc, instruction) in function.code.iter().enumerate() {
            let (kind, entries): (DynamicDispatchKind, BTreeMap<u64, (u32, u32)>) =
                match instruction.opcode() {
                    Opcode::CallClosure => (
                        DynamicDispatchKind::Closure,
                        closure_callsite_targets(module, function, pc, ClosureResultUse::Consumed)?
                            .into_iter()
                            .map(|target| {
                                (
                                    target.encoded_identity() as u64,
                                    (target.function_id, closure_prefix_code(target.abi.prefix)),
                                )
                            })
                            .collect(),
                    ),
                    Opcode::CallIface => {
                        let Some(InstructionMetadata::CallIfaceLayout {
                            iface_meta_id,
                            method_idx,
                            ..
                        }) = function.instruction_metadata.get(pc)
                        else {
                            return Err(WasmAotError::InvalidModule(format!(
                                "{} pc {pc} is missing CallIfaceLayout metadata",
                                function.name
                            )));
                        };
                        (
                            DynamicDispatchKind::Interface,
                            interface_implementations(module, *iface_meta_id)?
                                .into_iter()
                                .filter_map(|(value_rttid, methods)| {
                                    let target = *methods.get(*method_idx as usize)?;
                                    Some((
                                        u64::from(value_rttid),
                                        (
                                            target,
                                            u32::from(module.functions[target as usize].recv_slots),
                                        ),
                                    ))
                                })
                                .collect(),
                        )
                    }
                    _ => continue,
                };
            if !entries.is_empty() && entries.len() <= INLINE_DYNAMIC_DISPATCH_LIMIT {
                continue;
            }
            while (STATIC_DATA_START as usize + bytes.len()) & 7 != 0 {
                bytes.push(0);
            }
            let address = STATIC_DATA_START
                .checked_add(bytes.len() as u32)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("dynamic dispatch data overflow".into())
                })?;
            for (identity, (target, abi_data)) in &entries {
                bytes.extend_from_slice(&identity.to_le_bytes());
                bytes.extend_from_slice(&target.to_le_bytes());
                bytes.extend_from_slice(&abi_data.to_le_bytes());
            }
            dynamic_dispatch.insert(
                (function_id as u32, pc, kind),
                DynamicDispatchTable {
                    address,
                    entries: entries.len() as u32,
                },
            );
        }
    }
    let static_end = STATIC_DATA_START
        .checked_add(bytes.len() as u32)
        .ok_or_else(|| WasmAotError::InvalidModule("static data exceeds wasm32".into()))?;
    let stack_base = align_up(
        static_end.max(WASM_PAGE_BYTES as u32),
        WASM_PAGE_BYTES as u32,
    )?;
    let entry = module
        .functions
        .get(module.entry_func as usize)
        .ok_or_else(|| {
            WasmAotError::InvalidModule("entry function is missing from the module".into())
        })?;
    let entry_bytes = u32::from(entry.local_slots)
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(FRAME_STATE_BYTES))
        .ok_or_else(|| WasmAotError::InvalidModule("entry frame exceeds wasm32".into()))?;
    if entry_bytes > STACK_RESERVE_BYTES {
        return Err(WasmAotError::InvalidModule(
            "entry frame exceeds the Core-Wasm stack budget".into(),
        ));
    }
    let root_stack_end = stack_base
        .checked_add(entry_bytes)
        .and_then(|end| end.checked_add(SHADOW_STACK_BASE_CHUNK_BYTES))
        .ok_or_else(|| WasmAotError::InvalidModule("AOT root stack exceeds wasm32".into()))?;
    let allocation_index_base = align_up(root_stack_end, WASM_PAGE_BYTES as u32)?;
    let heap_base = allocation_index_base
        .checked_add(ALLOCATION_INDEX_BYTES)
        .ok_or_else(|| WasmAotError::InvalidModule("AOT allocation index exceeds wasm32".into()))?;
    let required_bytes = heap_base
        .checked_add(WASM_PAGE_BYTES as u32)
        .ok_or_else(|| WasmAotError::InvalidModule("AOT heap base exceeds wasm32".into()))?;
    let memory_pages = required_bytes.div_ceil(WASM_PAGE_BYTES as u32);
    Ok(StaticData {
        bytes,
        string_refs,
        dynamic_string_refs,
        runtime_panic_refs,
        nil_reference_panic_ref,
        nil_map_write_panic_ref,
        makeslice_negative_len_panic_ref,
        makeslice_cap_panic_ref,
        makeslice_len_gt_cap_panic_ref,
        makechan_panic_ref,
        makeport_panic_ref,
        index_panic_prefix_ref,
        index_panic_middle_ref,
        stack_base,
        allocation_index_base,
        memory_pages,
        dynamic_dispatch,
        dynamic_lookup_function: 0,
    })
}

pub(super) fn extern_source_tag(source: RegisteredExternSource) -> u8 {
    match source {
        RegisteredExternSource::Builtin => 0,
        RegisteredExternSource::Stdlib => 1,
        RegisteredExternSource::LinkmeExtension => 2,
        RegisteredExternSource::NativeExtension => 3,
        RegisteredExternSource::WasmHost => 4,
        RegisteredExternSource::WasmExtensionBridge => 5,
        RegisteredExternSource::Manual => 6,
        RegisteredExternSource::Test => 7,
    }
}

pub(super) fn encode_extern_manifest(
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    required_externs: &BTreeSet<u32>,
) -> Result<Vec<u8>, WasmAotError> {
    let mut bytes = Vec::new();
    bytes.extend_from_slice(b"VOEXT003");
    bytes.extend_from_slice(&(module.externs.len() as u32).to_le_bytes());
    for (extern_id, external) in module.externs.iter().enumerate() {
        let name = external.name.as_bytes();
        let len: u16 = name
            .len()
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("extern name exceeds u16".into()))?;
        bytes.extend_from_slice(&len.to_le_bytes());
        bytes.extend_from_slice(name);
        let resolved = resolved_externs.get(extern_id as u32);
        let params = resolved.map_or(&external.params, |entry| &entry.params);
        let returns = resolved.map_or(&external.returns, |entry| &entry.returns);
        let required = required_externs.contains(&(extern_id as u32));
        bytes.extend_from_slice(&u16::from(required).to_le_bytes());
        match params {
            ParamShape::Exact { slots } => {
                bytes.push(0);
                bytes.extend_from_slice(&slots.to_le_bytes());
            }
            ParamShape::CallSiteVariadic => {
                bytes.push(1);
                bytes.extend_from_slice(&0u16.to_le_bytes());
            }
        }
        bytes.extend_from_slice(&returns.slots.to_le_bytes());
        let allowed_effects =
            resolved.map_or(external.allowed_effects, |entry| entry.allowed_effects);
        let effective_effects = resolved.map_or(allowed_effects, |entry| entry.effective_effects);
        bytes.extend_from_slice(&allowed_effects.bits().to_le_bytes());
        bytes.extend_from_slice(&effective_effects.bits().to_le_bytes());
        bytes.extend_from_slice(
            &resolved
                .map_or(0, |entry| entry.abi_fingerprint)
                .to_le_bytes(),
        );
        bytes.extend_from_slice(
            &resolved
                .map_or(0, |entry| entry.provider_identity)
                .to_le_bytes(),
        );
        bytes.push(resolved.map_or(0xff, |entry| extern_source_tag(entry.source)));
        bytes.push(0);
        let slot_type_count: u16 =
            returns.slot_types.len().try_into().map_err(|_| {
                WasmAotError::InvalidModule("extern return layout exceeds u16".into())
            })?;
        bytes.extend_from_slice(&slot_type_count.to_le_bytes());
        bytes.extend(returns.slot_types.iter().map(|slot| *slot as u8));
    }
    Ok(bytes)
}

pub(super) fn runtime_storage_bytes(
    module: &VoModule,
    value: ValueRttid,
) -> Result<u32, WasmAotError> {
    let bytes = match value.value_kind() {
        ValueKind::Void => 0usize,
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
        ValueKind::Int16 | ValueKind::Uint16 => 2,
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
        ValueKind::Interface => 16,
        ValueKind::Struct | ValueKind::Array => module
            .slot_layout_for_value_rttid(value)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime type {} has no physical layout",
                    value.rttid()
                ))
            })?
            .len()
            .checked_mul(8)
            .ok_or_else(|| WasmAotError::InvalidModule("runtime layout overflows".into()))?,
        _ => 8,
    };
    bytes
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("runtime storage exceeds wasm32".into()))
}

pub(super) fn encode_runtime_metadata(
    module: &VoModule,
    descriptors: &AllocationDescriptors,
) -> Result<Vec<u8>, WasmAotError> {
    let resolver = module.runtime_type_resolver();
    let runtime_type_count: u32 = module
        .runtime_types
        .len()
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("runtime type count exceeds u32".into()))?;
    let runtime_values: Vec<ValueRttid> = (0..runtime_type_count)
        .filter_map(|rttid| resolver.value_rttid_for_rttid(rttid))
        .collect();
    let descriptor_count: u32 = descriptors.entries.len().try_into().map_err(|_| {
        WasmAotError::InvalidModule("allocation descriptor count exceeds u32".into())
    })?;
    let type_count: u32 = runtime_values
        .len()
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("runtime type count exceeds u32".into()))?;
    let struct_count: u32 = module
        .struct_metas
        .len()
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("struct metadata count exceeds u32".into()))?;

    let error_value = module
        .well_known
        .error_ptr_rttid
        .and_then(|rttid| resolver.value_rttid_for_rttid(rttid));
    let error_struct_meta = module.well_known.error_struct_meta_id;
    let error_descriptor =
        error_struct_meta.and_then(|meta| descriptors.fixed_by_struct_meta.get(&meta).copied());
    let error_slots = error_struct_meta
        .and_then(|meta| module.struct_metas.get(meta as usize))
        .map(StructMeta::slot_count)
        .unwrap_or(0);
    let error_offsets = module.well_known.error_field_offsets.unwrap_or([0, 0]);

    let mut bytes = Vec::new();
    bytes.extend_from_slice(b"VORT0001");
    bytes.extend_from_slice(&descriptor_count.to_le_bytes());
    bytes.extend_from_slice(&type_count.to_le_bytes());
    bytes.extend_from_slice(&struct_count.to_le_bytes());
    bytes.extend_from_slice(
        &error_value
            .map(ValueRttid::to_raw)
            .unwrap_or(RUNTIME_METADATA_NONE)
            .to_le_bytes(),
    );
    bytes.extend_from_slice(
        &error_descriptor
            .unwrap_or(RUNTIME_METADATA_NONE)
            .to_le_bytes(),
    );
    bytes.extend_from_slice(&error_slots.to_le_bytes());
    bytes.extend_from_slice(&error_offsets[0].to_le_bytes());
    bytes.extend_from_slice(&error_offsets[1].to_le_bytes());
    bytes.extend_from_slice(&0u16.to_le_bytes());

    for value in runtime_values {
        let type_name = module
            .named_type_id_for_rttid(value.rttid())
            .and_then(|id| module.named_type_metas.get(id as usize))
            .map(|metadata| metadata.name.as_bytes())
            .unwrap_or_default();
        let type_name_len: u16 = type_name.len().try_into().map_err(|_| {
            WasmAotError::InvalidModule(format!("runtime type {} name exceeds u16", value.rttid()))
        })?;
        let (_, runtime_type) = resolver.resolve_value_rttid(value).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "runtime type {} has an invalid named-type chain",
                value.rttid()
            ))
        })?;
        let (tag, first, second, length) = match runtime_type {
            RuntimeType::Basic(_) => (0u8, 0, 0, 0),
            RuntimeType::Pointer(elem) => (1, elem.to_raw(), 0, 0),
            RuntimeType::Array { len, elem } => (2, elem.to_raw(), 0, *len),
            RuntimeType::Slice(elem) => (3, elem.to_raw(), 0, 0),
            RuntimeType::Map { key, val } => (4, key.to_raw(), val.to_raw(), 0),
            RuntimeType::Struct { meta_id, .. } => (5, *meta_id, 0, 0),
            RuntimeType::Interface { meta_id, .. } => (6, *meta_id, 0, 0),
            RuntimeType::Chan { .. } => (7, 0, 0, 0),
            RuntimeType::Port { .. } => (8, 0, 0, 0),
            RuntimeType::Func { .. } => (9, 0, 0, 0),
            RuntimeType::Island => (10, 0, 0, 0),
            RuntimeType::Tuple(_) | RuntimeType::Named { .. } => {
                return Err(WasmAotError::InvalidModule(format!(
                    "runtime type {} did not resolve to a value representation",
                    value.rttid()
                )));
            }
        };
        let slot_count: u32 = resolver
            .slot_count_for_value_rttid(value)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime type {} has no finite slot layout",
                    value.rttid()
                ))
            })?
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("runtime slot count exceeds u32".into()))?;
        let canonical_meta = module
            .canonical_value_meta_for_value_rttid(value)
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "runtime type {} has no canonical value metadata",
                    value.rttid()
                ))
            })?
            .to_raw();
        let fixed = descriptors
            .fixed_by_value
            .get(&value.to_raw())
            .copied()
            .unwrap_or(RUNTIME_METADATA_NONE);
        let sequence = descriptors
            .sequence_by_value
            .get(&value.to_raw())
            .copied()
            .unwrap_or(RUNTIME_METADATA_NONE);
        let (map, map_entries) = descriptors
            .map_by_value
            .get(&value.to_raw())
            .copied()
            .unwrap_or((RUNTIME_METADATA_NONE, RUNTIME_METADATA_NONE));
        bytes.extend_from_slice(&value.to_raw().to_le_bytes());
        bytes.extend_from_slice(&canonical_meta.to_le_bytes());
        bytes.push(value.value_kind() as u8);
        bytes.push(tag);
        // The formerly reserved u16 carries an optional canonical named-type
        // identity. A zero length keeps older unnamed records byte-for-byte
        // compatible while serialization hosts can retain semantic string
        // types such as encoding/toml.LocalDate.
        bytes.extend_from_slice(&type_name_len.to_le_bytes());
        bytes.extend_from_slice(&slot_count.to_le_bytes());
        bytes.extend_from_slice(&runtime_storage_bytes(module, value)?.to_le_bytes());
        bytes.extend_from_slice(&fixed.to_le_bytes());
        bytes.extend_from_slice(&sequence.to_le_bytes());
        bytes.extend_from_slice(&map.to_le_bytes());
        bytes.extend_from_slice(&map_entries.to_le_bytes());
        bytes.extend_from_slice(&first.to_le_bytes());
        bytes.extend_from_slice(&second.to_le_bytes());
        bytes.extend_from_slice(&length.to_le_bytes());
        bytes.extend_from_slice(type_name);
    }

    for metadata in &module.struct_metas {
        let slot_count = metadata.slot_count();
        let field_count: u16 =
            metadata.fields.len().try_into().map_err(|_| {
                WasmAotError::InvalidModule("struct field count exceeds u16".into())
            })?;
        bytes.extend_from_slice(&slot_count.to_le_bytes());
        bytes.extend_from_slice(&field_count.to_le_bytes());
        for field in &metadata.fields {
            let name = field.name.as_bytes();
            let tag = field.tag.as_deref().unwrap_or("").as_bytes();
            let name_len: u32 = name
                .len()
                .try_into()
                .map_err(|_| WasmAotError::InvalidModule("struct field name exceeds u32".into()))?;
            let tag_len: u32 = tag
                .len()
                .try_into()
                .map_err(|_| WasmAotError::InvalidModule("struct field tag exceeds u32".into()))?;
            bytes.extend_from_slice(&name_len.to_le_bytes());
            bytes.extend_from_slice(&tag_len.to_le_bytes());
            bytes.extend_from_slice(&field.offset.to_le_bytes());
            bytes.extend_from_slice(&field.slot_count.to_le_bytes());
            bytes.extend_from_slice(&field.type_info.to_raw().to_le_bytes());
            let flags = u8::from(field.embedded) | (u8::from(is_exported_name(&field.name)) << 1);
            bytes.push(flags);
            bytes.extend_from_slice(&[0, 0, 0]);
            bytes.extend_from_slice(name);
            bytes.extend_from_slice(tag);
        }
    }
    Ok(bytes)
}

pub(super) fn encode_debug_metadata(module: &VoModule) -> Result<Vec<u8>, WasmAotError> {
    let file_count: u32 = module
        .debug_info
        .files
        .len()
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("debug file count exceeds u32".into()))?;
    let function_count: u32 = module
        .debug_info
        .funcs
        .len()
        .try_into()
        .map_err(|_| WasmAotError::InvalidModule("debug function count exceeds u32".into()))?;
    let mut bytes = Vec::new();
    bytes.extend_from_slice(b"VODBG002");
    bytes.extend_from_slice(&file_count.to_le_bytes());
    bytes.extend_from_slice(&function_count.to_le_bytes());
    // runtime.Caller is implemented by the host because file paths and line
    // mappings live in this section. Publish the private frame fields it must
    // walk alongside those mappings so a frame-layout change cannot silently
    // desynchronize an otherwise ABI-compatible host.
    bytes.extend_from_slice(&FRAME_STATE_BYTES.to_le_bytes());
    bytes.extend_from_slice(&(FRAME_FUNCTION_ID_OFFSET as u32).to_le_bytes());
    bytes.extend_from_slice(&(FRAME_PARENT_OFFSET as u32).to_le_bytes());
    bytes.extend_from_slice(&(FRAME_DEBUG_PC_OFFSET as u32).to_le_bytes());
    for file in &module.debug_info.files {
        let encoded = file.as_bytes();
        let length: u32 = encoded
            .len()
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("debug file path exceeds u32".into()))?;
        bytes.extend_from_slice(&length.to_le_bytes());
        bytes.extend_from_slice(encoded);
    }
    for function in &module.debug_info.funcs {
        // DebugInfo::lookup resolves duplicate PCs to the last recorded span.
        // Preserve that canonical meaning while giving the public AOT section
        // a strictly increasing PC table that every host can binary-search.
        let mut canonical_entries = Vec::with_capacity(function.entries.len());
        for entry in &function.entries {
            if canonical_entries
                .last()
                .is_some_and(|previous: &&vo_common_core::DebugLoc| previous.pc > entry.pc)
            {
                return Err(WasmAotError::InvalidModule(
                    "debug locations are not sorted by bytecode PC".into(),
                ));
            }
            if canonical_entries
                .last()
                .is_some_and(|previous: &&vo_common_core::DebugLoc| previous.pc == entry.pc)
            {
                *canonical_entries
                    .last_mut()
                    .expect("duplicate entry has a predecessor") = entry;
            } else {
                canonical_entries.push(entry);
            }
        }
        let entry_count: u32 = canonical_entries
            .len()
            .try_into()
            .map_err(|_| WasmAotError::InvalidModule("debug location count exceeds u32".into()))?;
        bytes.extend_from_slice(&entry_count.to_le_bytes());
        for entry in canonical_entries {
            if entry.file_id >= file_count {
                return Err(WasmAotError::InvalidModule(
                    "debug location references a missing file".into(),
                ));
            }
            bytes.extend_from_slice(&entry.pc.to_le_bytes());
            bytes.extend_from_slice(&entry.file_id.to_le_bytes());
            bytes.extend_from_slice(&entry.line.to_le_bytes());
            bytes.extend_from_slice(&entry.col.to_le_bytes());
            bytes.extend_from_slice(&entry.len.to_le_bytes());
        }
    }
    Ok(bytes)
}
