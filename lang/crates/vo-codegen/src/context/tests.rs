use super::CodegenContext;
use std::collections::{BTreeMap, HashMap};
use vo_analysis::arena::ArenaKey;
use vo_common_core::JitInstructionMetadata;
use vo_runtime::bytecode::{
    ExtSlotKind, FunctionDef, GlobalDef, InterfaceMeta, NamedTypeMeta, ParamShape, ReturnShape,
    StructMeta,
};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::{RuntimeType, SlotType, ValueKind, ValueMeta, ValueRttid};

fn minimal_function(code_len: usize) -> FunctionDef {
    let code = (0..code_len)
        .map(|_| Instruction::new(Opcode::Return, 0, 0, 0))
        .collect::<Vec<_>>();
    let slot_types = Vec::new();
    FunctionDef {
        name: "test".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 0,
        gc_scan_slots: 0,
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
        jit_metadata: vec![JitInstructionMetadata::None; code_len],
        code,
        slot_types,
        borrowed_scan_slots_prefix: vec![0],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}

#[test]
fn variable_ret_externs_are_keyed_by_ret_slots() {
    let mut ctx = CodegenContext::new("extern-ret-slots");

    let effects = vo_runtime::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY;
    let typed_call = ctx.get_or_register_variable_ret_extern_with_effects("dyn_call", 4, effects);
    let any_call = ctx.get_or_register_variable_ret_extern_with_effects("dyn_call", 6, effects);
    let typed_call_again =
        ctx.get_or_register_variable_ret_extern_with_effects("dyn_call", 4, effects);

    assert_ne!(typed_call, any_call);
    assert_eq!(typed_call, typed_call_again);
    assert_eq!(ctx.module().externs[typed_call as usize].returns.slots, 4);
    assert_eq!(ctx.module().externs[any_call as usize].returns.slots, 6);
    assert_eq!(ctx.module().externs[typed_call as usize].name, "dyn_call");
    assert_eq!(ctx.module().externs[any_call as usize].name, "dyn_call");
}

#[test]
fn variable_ret_externs_are_keyed_by_precise_return_layout_058() {
    let mut ctx = CodegenContext::new("extern-ret-layout");

    let effects = vo_runtime::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY;
    let value_layout = vec![
        vo_runtime::SlotType::Value,
        vo_runtime::SlotType::Interface0,
        vo_runtime::SlotType::Interface1,
    ];
    let gc_layout = vec![
        vo_runtime::SlotType::GcRef,
        vo_runtime::SlotType::Interface0,
        vo_runtime::SlotType::Interface1,
    ];
    let value_returns = ReturnShape::try_with_slot_types_and_interface_metas(
        value_layout.clone(),
        vec![None, Some(0), None],
    )
    .expect("test return shape should be valid");
    let gc_returns = ReturnShape::try_with_slot_types_and_interface_metas(
        gc_layout.clone(),
        vec![None, Some(0), None],
    )
    .expect("test return shape should be valid");
    let value_call = ctx.get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
        "dyn_call",
        value_returns.clone(),
        Vec::new(),
        effects,
    );
    let gc_call = ctx.get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
        "dyn_call",
        gc_returns,
        Vec::new(),
        effects,
    );
    let value_call_again = ctx
        .get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
            "dyn_call",
            value_returns,
            Vec::new(),
            effects,
        );

    assert_ne!(value_call, gc_call);
    assert_eq!(value_call, value_call_again);
    assert_eq!(
        ctx.module().externs[value_call as usize].returns.slot_types,
        value_layout
    );
    assert_eq!(
        ctx.module().externs[gc_call as usize].returns.slot_types,
        gc_layout
    );
}

#[test]
fn variable_ret_dynamic_externs_preserve_exact_parameter_abi_058() {
    let mut ctx = CodegenContext::new("dynamic-extern-param-abi");

    let effects = vo_runtime::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY;
    let dynamic_returns = ReturnShape::try_with_slot_types_and_interface_metas(
        vec![
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Interface1,
        ],
        vec![None, None, Some(0), None],
    )
    .expect("test return shape should be valid");
    let dyn_field = ctx.get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
        "dyn_field",
        dynamic_returns.clone(),
        vec![
            ExtSlotKind::Value,
            ExtSlotKind::Value,
            ExtSlotKind::Bytes,
            ExtSlotKind::Value,
            ExtSlotKind::Value,
        ],
        effects,
    );
    let dyn_index = ctx.get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
        "dyn_index",
        dynamic_returns,
        vec![ExtSlotKind::Value; 6],
        effects,
    );

    let dyn_field = &ctx.module().externs[dyn_field as usize];
    assert_eq!(dyn_field.params, ParamShape::Exact { slots: 5 });
    assert_eq!(
        dyn_field.param_kinds,
        vec![
            ExtSlotKind::Value,
            ExtSlotKind::Value,
            ExtSlotKind::Bytes,
            ExtSlotKind::Value,
            ExtSlotKind::Value,
        ]
    );

    let dyn_index = &ctx.module().externs[dyn_index as usize];
    assert_eq!(dyn_index.params, ParamShape::Exact { slots: 6 });
    assert_eq!(dyn_index.param_kinds, vec![ExtSlotKind::Value; 6]);
}

#[test]
fn extern_return_shape_merge_rejects_slot_count_layout_drift_048() {
    let mut ctx = CodegenContext::new("extern-return-shape-drift");

    let id = ctx.get_or_register_extern_with_slots_and_effects(
        "pkg_F",
        ReturnShape::slots(2),
        ParamShape::CallSiteVariadic,
        Vec::new(),
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    assert_eq!(ctx.module().externs[id as usize].returns.slots, 2);
    assert!(ctx.module().externs[id as usize]
        .returns
        .slot_types
        .is_empty());

    ctx.get_or_register_extern_with_slots_and_effects(
        "pkg_F",
        ReturnShape::with_slot_types(vec![SlotType::GcRef]),
        ParamShape::CallSiteVariadic,
        Vec::new(),
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    assert_eq!(
        ctx.check_layout_errors().unwrap_err(),
        "extern 'pkg_F' registered with incompatible return slot layout"
    );
}

#[test]
fn declared_extern_rejects_same_name_different_precise_return_layout_048() {
    let mut ctx = CodegenContext::new("extern-precise-return-shape-drift");

    ctx.get_or_register_declared_extern_with_return_layout(
        "pkg_F",
        vec![SlotType::GcRef],
        Vec::new(),
    );
    ctx.get_or_register_declared_extern_with_return_layout(
        "pkg_F",
        vec![SlotType::Value, SlotType::Value],
        Vec::new(),
    );
    assert_eq!(
        ctx.check_layout_errors().unwrap_err(),
        "extern 'pkg_F' registered with incompatible return slot layout"
    );
}

#[test]
fn declared_extern_rejects_same_name_different_return_interface_metadata_060() {
    let mut ctx = CodegenContext::new("extern-interface-return-shape-drift");

    let returns_iface_1 = ReturnShape::try_with_slot_types_and_interface_metas(
        vec![SlotType::Interface0, SlotType::Interface1],
        vec![Some(1), None],
    )
    .expect("test return shape should be valid");
    let returns_iface_2 = ReturnShape::try_with_slot_types_and_interface_metas(
        vec![SlotType::Interface0, SlotType::Interface1],
        vec![Some(2), None],
    )
    .expect("test return shape should be valid");

    ctx.get_or_register_declared_extern_with_return_shape("pkg_F", returns_iface_1, Vec::new());
    ctx.get_or_register_declared_extern_with_return_shape("pkg_F", returns_iface_2, Vec::new());
    assert_eq!(
        ctx.check_layout_errors().unwrap_err(),
        "extern 'pkg_F' registered with incompatible return interface metadata"
    );
}

#[test]
fn declared_extern_rejects_same_name_different_parameter_kinds_048() {
    let mut ctx = CodegenContext::new("extern-parameter-shape-drift");

    ctx.get_or_register_extern_with_slots_and_effects(
        "pkg_F",
        ReturnShape::slots(0),
        ParamShape::Exact { slots: 1 },
        vec![ExtSlotKind::Bytes],
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    ctx.get_or_register_extern_with_slots_and_effects(
        "pkg_F",
        ReturnShape::slots(0),
        ParamShape::Exact { slots: 1 },
        vec![ExtSlotKind::Value],
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    assert_eq!(
        ctx.check_layout_errors().unwrap_err(),
        "extern 'pkg_F' registered with incompatible parameter ABI"
    );
}

#[test]
fn declared_extern_rejects_same_name_different_exact_parameter_slots_048() {
    let mut ctx = CodegenContext::new("extern-parameter-slot-drift");

    ctx.get_or_register_extern_with_slots_and_effects(
        "pkg_F",
        ReturnShape::slots(0),
        ParamShape::Exact { slots: 1 },
        vec![ExtSlotKind::Value],
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    ctx.get_or_register_extern_with_slots_and_effects(
        "pkg_F",
        ReturnShape::slots(0),
        ParamShape::Exact { slots: 2 },
        vec![ExtSlotKind::Value, ExtSlotKind::Value],
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    assert_eq!(
        ctx.check_layout_errors().unwrap_err(),
        "extern 'pkg_F' registered with incompatible parameter ABI"
    );
}

#[test]
fn declared_zero_arg_extern_preserves_exact_param_shape_051() {
    let mut ctx = CodegenContext::new("extern-zero-arg-param-shape");

    let id = ctx.get_or_register_declared_extern_with_return_layout(
        "time_nowUnixNano",
        vec![SlotType::Value],
        Vec::new(),
    );

    assert_eq!(
        ctx.module().externs[id as usize].params,
        ParamShape::Exact { slots: 0 },
        "declared zero-arg externs must not degrade to call-site variadic ABI"
    );
}

#[test]
fn vm_codegen_global_slot_width_023_rejects_unencodable_total_width() {
    let mut ctx = CodegenContext::new("global-slot-width");
    ctx.register_global(
        vo_analysis::objects::ObjKey::from_usize(0),
        GlobalDef {
            name: "wide".to_string(),
            slots: u16::MAX,
            value_kind: ValueKind::Int64 as u8,
            meta_id: 0,
            slot_types: vec![SlotType::Value; usize::from(u16::MAX)],
        },
    );
    ctx.register_global(
        vo_analysis::objects::ObjKey::from_usize(1),
        GlobalDef {
            name: "overflow".to_string(),
            slots: 2,
            value_kind: ValueKind::Int64 as u8,
            meta_id: 0,
            slot_types: vec![SlotType::Value; 2],
        },
    );

    let err = ctx
        .check_layout_errors()
        .expect_err("global offsets beyond the VM operand domain must be rejected");
    assert_eq!(
        err,
        "global slot count exceeds u16 operand width: 65537 slots"
    );
}

#[test]
fn transfer_metadata_slot_width_047_uses_slot_count_contract() {
    let mut ctx = CodegenContext::new("transfer-slot-width");
    let struct_rttid = ctx.type_interner.intern(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 1,
    });
    ctx.module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value; usize::from(u16::MAX) + 1],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    ctx.module.runtime_types = ctx.type_interner.types().to_vec();

    let err = ctx
        .canonical_transfer_type_for_rttid(ValueRttid::new(struct_rttid, ValueKind::Struct), "type")
        .expect_err("canonical transfer metadata must reject unencodable slot counts");

    assert!(err.contains("type slot count exceeds u16::MAX: 65536 slots"));
}

#[test]
fn extern_allowed_effects_merge_unknown_without_invalid_bit_mix() {
    let mut ctx = CodegenContext::new("extern-effects");

    let id = ctx.get_or_register_extern_with_slots_and_effects(
        "pkg_F",
        ReturnShape::slots(0),
        ParamShape::CallSiteVariadic,
        Vec::new(),
        vo_runtime::bytecode::ExternEffects::MAY_HOST_REPLAY,
    );
    let same = ctx.get_or_register_extern_with_slots_and_effects(
        "pkg_F",
        ReturnShape::slots(0),
        ParamShape::CallSiteVariadic,
        Vec::new(),
        vo_runtime::bytecode::ExternEffects::UNKNOWN_CONTROL,
    );

    assert_eq!(id, same);
    assert_eq!(
        ctx.module().externs[id as usize].allowed_effects,
        vo_runtime::bytecode::ExternEffects::UNKNOWN_CONTROL
    );
}

#[test]
fn declared_extern_effects_use_manifest_for_known_names_only() {
    let mut ctx = CodegenContext::new("declared-extern-effects");

    let file_read =
        ctx.get_or_register_declared_extern_with_slots("os_blocking_fileRead", 3, Vec::new());
    let unknown =
        ctx.get_or_register_declared_extern_with_slots("extension_doThing", 0, Vec::new());

    assert_eq!(
        ctx.module().externs[file_read as usize].allowed_effects,
        vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY
    );
    assert_eq!(
        ctx.module().externs[unknown as usize].allowed_effects,
        vo_runtime::bytecode::ExternEffects::UNKNOWN_CONTROL
    );
}

#[test]
fn finalize_named_type_underlying_meta_rewrites_to_metadata_table_ids() {
    let mut ctx = CodegenContext::new("named-meta");
    let struct_rttid = ctx.type_interner.intern(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 1,
    });
    let interface_rttid = ctx.type_interner.intern(RuntimeType::Interface {
        methods: Vec::new(),
        meta_id: 1,
    });
    let named_struct_rttid = ctx.type_interner.intern(RuntimeType::Named {
        id: 0,
        struct_meta_id: Some(1),
    });
    let pointer_rttid = ctx
        .type_interner
        .intern(RuntimeType::Pointer(ValueRttid::new(
            named_struct_rttid,
            ValueKind::Struct,
        )));
    let dummy_struct = StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: HashMap::new(),
    };
    ctx.module
        .struct_metas
        .resize_with(struct_rttid as usize + 1, || dummy_struct.clone());
    ctx.module.struct_metas[1] = StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: HashMap::new(),
    };
    let dummy_iface = InterfaceMeta {
        name: "dummy".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    };
    ctx.module
        .interface_metas
        .resize_with(interface_rttid as usize + 1, || dummy_iface.clone());
    ctx.module.interface_metas[1] = InterfaceMeta {
        name: "Iface".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    };
    ctx.module.named_type_metas.push(NamedTypeMeta {
        name: "S".to_string(),
        underlying_meta: ValueMeta::new(struct_rttid, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(struct_rttid, ValueKind::Struct),
        methods: BTreeMap::new(),
    });
    ctx.module.named_type_metas.push(NamedTypeMeta {
        name: "I".to_string(),
        underlying_meta: ValueMeta::new(interface_rttid, ValueKind::Interface),
        underlying_rttid: ValueRttid::new(interface_rttid, ValueKind::Interface),
        methods: BTreeMap::new(),
    });
    ctx.module.named_type_metas.push(NamedTypeMeta {
        name: "SPtr".to_string(),
        underlying_meta: ValueMeta::new(pointer_rttid, ValueKind::Pointer),
        underlying_rttid: ValueRttid::new(pointer_rttid, ValueKind::Pointer),
        methods: BTreeMap::new(),
    });

    ctx.finalize_named_type_underlying_meta();

    assert_eq!(ctx.module.named_type_metas[0].underlying_meta.meta_id(), 1);
    assert_eq!(ctx.module.named_type_metas[1].underlying_meta.meta_id(), 1);
    assert_eq!(ctx.module.named_type_metas[2].underlying_meta.meta_id(), 1);
    assert_eq!(
        ctx.module.named_type_metas[2].underlying_meta.value_kind(),
        ValueKind::Pointer
    );
}

#[test]
fn finalize_debug_info_drops_entries_past_function_code() {
    let mut ctx = CodegenContext::new("debug-info");
    ctx.module.functions.push(minimal_function(2));
    ctx.module.debug_info.add_loc(0, 2, "test.vo", 3, 1, 1);
    ctx.module.debug_info.add_loc(0, 1, "test.vo", 2, 1, 1);
    ctx.module.debug_info.add_loc(0, 0, "test.vo", 1, 1, 1);
    ctx.module.debug_info.add_loc(1, 0, "test.vo", 4, 1, 1);

    ctx.finalize_debug_info();

    let pcs = ctx.module.debug_info.funcs[0]
        .entries
        .iter()
        .map(|entry| entry.pc)
        .collect::<Vec<_>>();
    assert_eq!(pcs, vec![0, 1]);
    assert!(ctx.module.debug_info.funcs[1].entries.is_empty());
}

#[test]
fn vm_ptr_new_boxing_paths_use_physical_boxing_meta_060() {
    let sources = [
        ("assign.rs", include_str!("../assign.rs")),
        ("expr/builtin.rs", include_str!("../expr/builtin.rs")),
        ("expr/pointer.rs", include_str!("../expr/pointer.rs")),
    ];
    for (path, source) in sources {
        let mut rest = source;
        while let Some(ptr_new_idx) = rest.find("emit_ptr_new") {
            let prefix = &rest[..ptr_new_idx];
            let start = prefix.len().saturating_sub(260);
            let window = &prefix[start..];
            assert!(
                    !window.contains("get_or_create_value_meta("),
                    "{path} must use get_boxing_meta before emit_ptr_new so boxed values carry physical object metadata"
                );
            rest = &rest[ptr_new_idx + "emit_ptr_new".len()..];
        }
    }
}
