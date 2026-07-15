use super::{
    builtin_extern_ret_slots, checked_next_function_id, checked_next_u32_table_id,
    exact_param_shape_from_kinds, CodegenContext, MAX_ENCODED_FUNCTION_ID,
};
use std::collections::{BTreeMap, HashMap};
use vo_analysis::arena::ArenaKey;
use vo_common::{SourceMap, Span};
use vo_common_core::JitInstructionMetadata;
use vo_runtime::bytecode::{
    ExtSlotKind, FunctionDef, GlobalDef, InterfaceMeta, NamedTypeMeta, ParamShape, ReturnShape,
    StructMeta,
};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::{RuntimeType, SlotType, ValueKind, ValueMeta, ValueRttid};

const TEST_EXTERN_NAME: &str = vo_runtime::vo_extern_name!("codegen_tests", "F");

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
fn empty_interface_registration_reuses_metadata_zero() {
    let mut context = CodegenContext::new("empty-interface");
    let type_key = vo_analysis::objects::TypeKey::from_usize(0);

    let id = context
        .register_interface_meta(
            type_key,
            InterfaceMeta {
                name: "NamedEmpty".to_string(),
                method_names: Vec::new(),
                methods: Vec::new(),
            },
        )
        .unwrap();

    assert_eq!(id, 0);
    assert_eq!(context.get_interface_meta_id(type_key), Some(0));
    assert_eq!(context.module.interface_metas.len(), 1);
}

#[test]
fn canonical_any_registration_reuses_metadata_zero_before_interface_completion() {
    let tc_objs = vo_analysis::objects::TCObjects::new();
    let any_type = tc_objs.universe().any_type();
    let interner = vo_common::SymbolInterner::new();
    let mut context = CodegenContext::new("canonical-any");

    let id = context.get_or_create_interface_meta_id(any_type, &tc_objs, &interner);

    assert_eq!(id, 0);
    assert_eq!(context.get_interface_meta_id(any_type), Some(0));
    assert_eq!(context.module.interface_metas.len(), 1);
}

#[test]
fn imported_plain_main_function_cannot_replace_entry_package_main() {
    use vo_analysis::objects::ObjKey;
    use vo_common::Symbol;

    let mut ctx = CodegenContext::new("main-entry-owner");
    ctx.declare_func(
        None,
        false,
        Symbol::from_raw(1),
        ObjKey::from_usize(0),
        "main",
        true,
    );
    let entry_id = ctx.main_func_id().expect("entry package main registered");

    ctx.declare_func(
        None,
        false,
        Symbol::from_raw(2),
        ObjKey::from_usize(1),
        "main",
        false,
    );
    assert_eq!(ctx.main_func_id(), Some(entry_id));
    assert_eq!(ctx.module().functions.len(), 2);
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
fn variable_ret_registration_is_reserved_for_vm_internal_externs() {
    let effects = vo_runtime::bytecode::ExternEffects::NONE;

    let mut slot_ctx = CodegenContext::new("canonical-variable-ret-slots");
    let id =
        slot_ctx.get_or_register_variable_ret_extern_with_effects(TEST_EXTERN_NAME, 1, effects);
    assert_eq!(id, 0);
    assert!(slot_ctx.module().externs.is_empty());
    let error = slot_ctx
        .check_layout_errors()
        .expect_err("canonical extern cannot use VM variable-return registration");
    assert!(
        error.contains("reserved for VM-internal helpers"),
        "{error}"
    );

    let mut shape_ctx = CodegenContext::new("canonical-variable-ret-shape");
    let id = shape_ctx.get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
        TEST_EXTERN_NAME,
        ReturnShape::slots(1),
        Vec::new(),
        effects,
    );
    assert_eq!(id, 0);
    assert!(shape_ctx.module().externs.is_empty());
    let error = shape_ctx
        .check_layout_errors()
        .expect_err("canonical extern cannot use VM variable-return registration");
    assert!(
        error.contains("reserved for VM-internal helpers"),
        "{error}"
    );
}

#[test]
fn canonical_dyn_source_externs_keep_declared_shapes_and_effects() {
    use vo_runtime::bytecode::ExternEffects;

    let cases = [
        ("getDynErrors", 16, Vec::new(), ExternEffects::NONE),
        (
            "GetAttr",
            4,
            vec![ExtSlotKind::Value, ExtSlotKind::Value, ExtSlotKind::Bytes],
            ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        ),
        (
            "GetIndex",
            4,
            vec![ExtSlotKind::Value; 4],
            ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        ),
        (
            "SetAttr",
            2,
            vec![
                ExtSlotKind::Value,
                ExtSlotKind::Value,
                ExtSlotKind::Bytes,
                ExtSlotKind::Value,
                ExtSlotKind::Value,
            ],
            ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        ),
        (
            "SetIndex",
            2,
            vec![ExtSlotKind::Value; 6],
            ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        ),
    ];

    let mut ctx = CodegenContext::new("canonical-dyn-source-externs");
    for (function, return_slots, param_kinds, effects) in cases {
        let name = vo_common::abi::try_abi_lookup_name("dyn", function).unwrap();
        assert_eq!(builtin_extern_ret_slots(&name), return_slots);

        let id = ctx.get_or_register_declared_extern_with_return_shape(
            &name,
            ReturnShape::slots(return_slots),
            param_kinds.clone(),
        );
        let external = &ctx.module().externs[id as usize];
        assert_eq!(
            external.params,
            ParamShape::Exact {
                slots: param_kinds.len() as u16
            },
            "{function} parameter contract"
        );
        assert_eq!(external.param_kinds, param_kinds, "{function} slot kinds");
        assert_eq!(external.returns.slots, return_slots, "{function} returns");
        assert_eq!(external.allowed_effects, effects, "{function} effects");
    }

    let foreign = vo_common::abi::try_abi_lookup_name("github.com/acme/dyn", "GetAttr").unwrap();
    let foreign_id =
        ctx.get_or_register_declared_extern_with_slots(&foreign, 4, vec![ExtSlotKind::Value; 3]);
    assert_eq!(
        ctx.module().externs[foreign_id as usize].allowed_effects,
        ExternEffects::UNKNOWN_CONTROL,
        "logical package identity must participate in builtin recognition"
    );
}

#[test]
fn constant_pool_overflow_is_reported_before_operand_wraparound() {
    let mut ctx = CodegenContext::new("constant-pool-width");
    for value in 0..=u16::MAX {
        assert_eq!(ctx.const_int(i64::from(value)), value);
    }

    assert_eq!(ctx.const_int(i64::from(u16::MAX) + 1), 0);
    let error = ctx
        .check_layout_errors()
        .expect_err("the 65537th constant must exceed the bytecode operand width");
    assert!(error.contains("constant pool exceeds u16 operand width"));
    assert_eq!(ctx.module().constants.len(), usize::from(u16::MAX) + 1);
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
        TEST_EXTERN_NAME,
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
        TEST_EXTERN_NAME,
        ReturnShape::with_slot_types(vec![SlotType::GcRef]),
        ParamShape::CallSiteVariadic,
        Vec::new(),
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    assert_eq!(
        ctx.check_layout_errors().unwrap_err(),
        format!("extern '{TEST_EXTERN_NAME}' registered with incompatible return slot layout")
    );
}

#[test]
fn codegen_rejects_noncanonical_extern_identity_before_module_insertion() {
    for invalid in ["legacy_flattened_F", "vo1:01:x:1:F"] {
        let mut ctx = CodegenContext::new("invalid-extern-identity");
        let id = ctx.get_or_register_extern_with_slots_and_effects(
            invalid,
            ReturnShape::slots(0),
            ParamShape::Exact { slots: 0 },
            Vec::new(),
            vo_runtime::bytecode::ExternEffects::NONE,
        );

        assert_eq!(id, 0);
        assert!(ctx.module().externs.is_empty());
        let error = ctx
            .check_layout_errors()
            .expect_err("invalid extern identity must be a codegen error");
        assert!(error.contains("invalid bytecode identity"), "{error}");
    }
}

#[test]
fn declared_extern_rejects_same_name_different_precise_return_layout_048() {
    let mut ctx = CodegenContext::new("extern-precise-return-shape-drift");

    ctx.get_or_register_declared_extern_with_return_layout(
        TEST_EXTERN_NAME,
        vec![SlotType::GcRef],
        Vec::new(),
    );
    ctx.get_or_register_declared_extern_with_return_layout(
        TEST_EXTERN_NAME,
        vec![SlotType::Value, SlotType::Value],
        Vec::new(),
    );
    assert_eq!(
        ctx.check_layout_errors().unwrap_err(),
        format!("extern '{TEST_EXTERN_NAME}' registered with incompatible return slot layout")
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

    ctx.get_or_register_declared_extern_with_return_shape(
        TEST_EXTERN_NAME,
        returns_iface_1,
        Vec::new(),
    );
    ctx.get_or_register_declared_extern_with_return_shape(
        TEST_EXTERN_NAME,
        returns_iface_2,
        Vec::new(),
    );
    assert_eq!(
        ctx.check_layout_errors().unwrap_err(),
        format!(
            "extern '{TEST_EXTERN_NAME}' registered with incompatible return interface metadata"
        )
    );
}

#[test]
fn declared_extern_rejects_same_name_different_parameter_kinds_048() {
    let mut ctx = CodegenContext::new("extern-parameter-shape-drift");

    ctx.get_or_register_extern_with_slots_and_effects(
        TEST_EXTERN_NAME,
        ReturnShape::slots(0),
        ParamShape::Exact { slots: 1 },
        vec![ExtSlotKind::Bytes],
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    ctx.get_or_register_extern_with_slots_and_effects(
        TEST_EXTERN_NAME,
        ReturnShape::slots(0),
        ParamShape::Exact { slots: 1 },
        vec![ExtSlotKind::Value],
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    assert_eq!(
        ctx.check_layout_errors().unwrap_err(),
        format!("extern '{TEST_EXTERN_NAME}' registered with incompatible parameter ABI")
    );
}

#[test]
fn declared_extern_rejects_same_name_different_exact_parameter_slots_048() {
    let mut ctx = CodegenContext::new("extern-parameter-slot-drift");

    ctx.get_or_register_extern_with_slots_and_effects(
        TEST_EXTERN_NAME,
        ReturnShape::slots(0),
        ParamShape::Exact { slots: 1 },
        vec![ExtSlotKind::Value],
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    ctx.get_or_register_extern_with_slots_and_effects(
        TEST_EXTERN_NAME,
        ReturnShape::slots(0),
        ParamShape::Exact { slots: 2 },
        vec![ExtSlotKind::Value, ExtSlotKind::Value],
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    assert_eq!(
        ctx.check_layout_errors().unwrap_err(),
        format!("extern '{TEST_EXTERN_NAME}' registered with incompatible parameter ABI")
    );
}

#[test]
fn declared_zero_arg_extern_preserves_exact_param_shape_051() {
    let mut ctx = CodegenContext::new("extern-zero-arg-param-shape");

    let id = ctx.get_or_register_declared_extern_with_return_layout(
        vo_runtime::vo_extern_name!("time", "nowUnixNano"),
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
    let struct_rttid = ctx
        .type_interner
        .intern(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 1,
        })
        .unwrap();
    ctx.module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value; usize::from(u16::MAX) + 1],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    ctx.module.runtime_types = ctx.type_interner.types().to_vec();

    let err = ctx
        .canonical_transfer_type_for_rttid(ValueRttid::new(struct_rttid, ValueKind::Struct), "type")
        .expect_err("canonical transfer metadata must reject unencodable slot counts");

    assert!(err.contains("exceeds the VM u16 slot-address domain"));
}

#[test]
fn extern_allowed_effects_merge_unknown_without_invalid_bit_mix() {
    let mut ctx = CodegenContext::new("extern-effects");

    let id = ctx.get_or_register_extern_with_slots_and_effects(
        TEST_EXTERN_NAME,
        ReturnShape::slots(0),
        ParamShape::CallSiteVariadic,
        Vec::new(),
        vo_runtime::bytecode::ExternEffects::MAY_HOST_REPLAY,
    );
    let same = ctx.get_or_register_extern_with_slots_and_effects(
        TEST_EXTERN_NAME,
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

    let file_read = ctx.get_or_register_declared_extern_with_slots(
        vo_runtime::vo_extern_name!("os", "blocking_fileRead"),
        3,
        Vec::new(),
    );
    let exit = ctx.get_or_register_declared_extern_with_slots(
        vo_runtime::vo_extern_name!("os", "nativeExit"),
        0,
        Vec::new(),
    );
    let unknown = ctx.get_or_register_declared_extern_with_slots(
        vo_runtime::vo_extern_name!("codegen_tests", "DoThing"),
        0,
        Vec::new(),
    );

    assert_eq!(
        ctx.module().externs[file_read as usize].allowed_effects,
        vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY
    );
    assert_eq!(
        ctx.module().externs[exit as usize].allowed_effects,
        vo_runtime::bytecode::ExternEffects::MAY_EXIT
    );
    assert_eq!(
        ctx.module().externs[unknown as usize].allowed_effects,
        vo_runtime::bytecode::ExternEffects::UNKNOWN_CONTROL
    );
}

#[test]
fn finalize_named_type_underlying_meta_rewrites_to_metadata_table_ids() {
    let mut ctx = CodegenContext::new("named-meta");
    let struct_rttid = ctx
        .type_interner
        .intern(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 1,
        })
        .unwrap();
    let interface_rttid = ctx
        .type_interner
        .intern(RuntimeType::Interface {
            methods: Vec::new(),
            meta_id: 1,
        })
        .unwrap();
    let named_struct_rttid = ctx
        .type_interner
        .intern(RuntimeType::Named {
            id: 0,
            struct_meta_id: Some(1),
        })
        .unwrap();
    let pointer_rttid = ctx
        .type_interner
        .intern(RuntimeType::Pointer(ValueRttid::new(
            named_struct_rttid,
            ValueKind::Struct,
        )))
        .unwrap();
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

    ctx.finalize_debug_info().expect("finalize debug info");

    let pcs = ctx.module.debug_info.funcs[0]
        .entries
        .iter()
        .map(|entry| entry.pc)
        .collect::<Vec<_>>();
    assert_eq!(pcs, vec![0, 1]);
    assert!(ctx.module.debug_info.funcs[1].entries.is_empty());
}

#[test]
fn debug_locations_require_a_complete_single_file_span() {
    let mut source_map = SourceMap::new();
    source_map.add_file("empty.vo", "");
    source_map.add_file("test.vo", "abc");

    let mut ctx = CodegenContext::new("debug-span-validation");
    ctx.add_debug_loc_from_span(0, 0, Span::from_u32(1, 3), &source_map);
    ctx.add_debug_loc_from_span(0, 1, Span::dummy(), &source_map);
    ctx.add_debug_loc_from_span(0, 2, Span::from_u32(3, 2), &source_map);
    ctx.add_debug_loc_from_span(0, 3, Span::from_u32(0, 1), &source_map);

    let entries = &ctx.module.debug_info.funcs[0].entries;
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].pc, 0);
    assert_eq!(
        ctx.module.debug_info.files[entries[0].file_id as usize],
        "test.vo"
    );
    assert_eq!(entries[0].line, 1);
    assert_eq!(entries[0].col, 1);
    assert_eq!(entries[0].len, 2);
}

#[test]
fn runtime_debug_locations_preserve_the_source_path_when_available() {
    let mut source_map = SourceMap::new();
    source_map
        .try_add_file_with_path(
            "main.vo",
            std::path::PathBuf::from("project/cmd/app/main.vo"),
            "call()",
        )
        .expect("source file");

    let mut ctx = CodegenContext::new("debug-source-path");
    ctx.add_debug_loc_from_span(0, 0, Span::from_u32(0, 6), &source_map);

    let entry = &ctx.module.debug_info.funcs[0].entries[0];
    assert_eq!(
        ctx.module.debug_info.files[entry.file_id as usize],
        "project/cmd/app/main.vo"
    );
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

#[test]
fn exact_extern_param_shape_checks_u16_boundary() {
    let max = vec![ExtSlotKind::Value; u16::MAX as usize];
    assert_eq!(
        exact_param_shape_from_kinds(&max).unwrap(),
        ParamShape::Exact { slots: u16::MAX }
    );

    let too_wide = vec![ExtSlotKind::Value; u16::MAX as usize + 1];
    assert!(exact_param_shape_from_kinds(&too_wide)
        .unwrap_err()
        .contains("exceeds u16::MAX"));
}

#[test]
fn oversized_extern_param_layout_records_error_without_truncated_contract() {
    let mut ctx = CodegenContext::new("wide-extern-params");
    let too_wide = vec![ExtSlotKind::Value; u16::MAX as usize + 1];
    let id = ctx.get_or_register_declared_extern_with_slots(TEST_EXTERN_NAME, 0, too_wide);

    assert!(ctx
        .check_layout_errors()
        .unwrap_err()
        .contains("exceeds u16::MAX"));
    let extern_def = &ctx.module.externs[id as usize];
    assert_eq!(extern_def.params, ParamShape::CallSiteVariadic);
    assert!(extern_def.param_kinds.is_empty());
}

#[test]
fn function_and_serialized_table_id_boundaries_are_checked_without_allocations() {
    assert_eq!(
        checked_next_function_id((MAX_ENCODED_FUNCTION_ID - 1) as usize),
        Ok(MAX_ENCODED_FUNCTION_ID - 1)
    );
    assert_eq!(
        checked_next_function_id(MAX_ENCODED_FUNCTION_ID as usize),
        Ok(MAX_ENCODED_FUNCTION_ID)
    );
    assert!(checked_next_function_id(MAX_ENCODED_FUNCTION_ID as usize + 1).is_err());

    assert_eq!(
        checked_next_u32_table_id("test", u32::MAX as usize - 1),
        Ok(u32::MAX - 1)
    );
    assert!(checked_next_u32_table_id("test", u32::MAX as usize).is_err());
}

#[test]
fn well_known_error_metadata_is_absent_when_only_the_predeclared_interface_exists() {
    let mut ctx = CodegenContext::new("error-interface-only");
    ctx.module.interface_metas.push(InterfaceMeta {
        name: "error".to_string(),
        method_names: vec!["Error".to_string()],
        methods: Vec::new(),
    });

    ctx.fill_well_known_types().unwrap();

    let known = &ctx.module.well_known;
    assert!(known.error_named_type_id.is_none());
    assert!(known.error_iface_meta_id.is_none());
    assert!(known.error_ptr_rttid.is_none());
    assert!(known.error_struct_meta_id.is_none());
    assert!(known.error_field_offsets.is_none());
}

#[test]
fn incomplete_registered_error_type_is_rejected_before_module_publication() {
    let mut ctx = CodegenContext::new("incomplete-error-type");
    ctx.module.interface_metas.push(InterfaceMeta {
        name: "error".to_string(),
        method_names: vec!["Error".to_string()],
        methods: Vec::new(),
    });
    ctx.module.named_type_metas.push(NamedTypeMeta {
        name: "errors.Error".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(0, ValueKind::Struct),
        methods: BTreeMap::new(),
    });

    let error = ctx.fill_well_known_types().unwrap_err();
    assert!(error.contains("*errors.Error runtime type is missing"));
}
