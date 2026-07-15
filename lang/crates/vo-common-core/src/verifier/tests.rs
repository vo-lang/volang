use super::*;
use crate::bytecode::{
    ExtSlotKind, ExternDef, ExternEffects, FieldMeta, GlobalDef, InterfaceMeta,
    InterfaceMethodMeta, Itab, MethodInfo, NamedTypeMeta, ParamShape, ReturnShape, StructMeta,
    IFACE_ASSIGN_NO_ITAB,
};
use crate::debug_info::{DebugLoc, FuncDebugInfo};
use crate::runtime_type::{RuntimeType, StructField};
use crate::types::{ValueKind, ValueMeta, ValueRttid};
use std::collections::BTreeMap;

mod call_iface;
mod call_widths;
mod containers;
mod extern_calls;
mod iface_asserts;
mod metadata_refs;
mod transfers_and_iface;

fn canonical_test_extern_name(function: &str) -> String {
    crate::extern_key::ExternKeyRef::new("github.com/volang/verifier-tests", function)
        .encode()
        .expect("verifier test extern identity must be canonical")
}

fn single_method_interface_meta(method_name: &str, signature_rttid: u32) -> InterfaceMeta {
    InterfaceMeta {
        name: format!("iface_{method_name}"),
        method_names: vec![method_name.to_string()],
        methods: vec![InterfaceMethodMeta {
            name: method_name.to_string(),
            signature_rttid,
        }],
    }
}

fn canonical_empty_interface_meta() -> InterfaceMeta {
    InterfaceMeta {
        name: "interface{}".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    }
}

fn push_non_empty_test_interface_meta(module: &mut Module, meta: InterfaceMeta) -> u32 {
    assert!(
        !meta.methods.is_empty(),
        "non-empty test interface helper requires at least one method"
    );
    if module.interface_metas.is_empty() {
        module
            .interface_metas
            .push(canonical_empty_interface_meta());
    }
    assert!(module.interface_metas[0].methods.is_empty());
    let id = u32::try_from(module.interface_metas.len()).expect("test interface id fits u32");
    module.interface_metas.push(meta);
    id
}

fn function_with_slot_types(slot_types: Vec<SlotType>) -> FunctionDef {
    FunctionDef {
        name: "f".to_string(),
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
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
        slot_types,
    }
}

fn iface_assert_layout_module(
    name: &str,
    assert_kind: u8,
    target_id: u16,
    target_slots: u16,
    slot_types: Vec<SlotType>,
    result_layout: Vec<SlotType>,
    runtime_type: RuntimeType,
) -> Module {
    let mut module = Module::new(name.to_string());
    module.runtime_types.push(runtime_type);
    if assert_kind == 1 {
        module.interface_metas.push(InterfaceMeta {
            name: "I".to_string(),
            method_names: Vec::new(),
            methods: Vec::new(),
        });
    }
    let flags = crate::instruction::pack_iface_assert_flags(assert_kind, false, target_slots)
        .expect("valid IfaceAssert flags");
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![Instruction::with_flags(
        Opcode::IfaceAssert,
        flags,
        0,
        2,
        target_id,
    )];
    func.jit_metadata = vec![JitInstructionMetadata::IfaceAssertLayout {
        assert_kind,
        target_id: u32::from(target_id),
        result_layout,
    }];
    module.functions.push(func);
    module
}

fn map_iter_next_module_039(
    name: &str,
    slot_types: Vec<SlotType>,
    next_inst: Instruction,
) -> Module {
    let mut module = Module::new(name.to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    module.constants.push(Constant::Int(
        ((int_meta.to_raw() as i64) << 32) | int_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));

    let mut func = function_with_slot_types(slot_types);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
        Instruction::new(Opcode::MapIterInit, 3, 0, 0),
        next_inst,
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapIterNext {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);
    module
}

fn verifier_fuzz_base_module(case_id: usize) -> Module {
    let mut module = Module::new(format!("verifier-fuzz-{case_id}"));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Float,
    ]);
    func.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);
    module
}

fn verifier_fuzz_case(case_id: usize) -> Module {
    let mut module = verifier_fuzz_base_module(case_id);
    match case_id % 10 {
        0 => {}
        1 => module.functions[0].jit_metadata.clear(),
        2 => {
            module.functions[0].code[0] = Instruction {
                op: 254,
                flags: 0,
                a: 0,
                b: 0,
                c: 0,
            }
        }
        3 => {
            module.functions[0].slot_types = vec![SlotType::Interface1];
            module.functions[0].local_slots = 1;
        }
        4 => module.functions[0].local_slots = 99,
        5 => {
            module.functions[0].gc_scan_slots = module.functions[0].gc_scan_slots.saturating_add(1)
        }
        6 => module.globals.push(GlobalDef {
            name: "g".to_string(),
            slots: 1,
            value_kind: 255,
            meta_id: 0,
            slot_types: vec![SlotType::Value],
        }),
        7 => module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::Interface0],
            fields: Vec::new(),
            field_index: Default::default(),
        }),
        8 => module
            .runtime_types
            .push(RuntimeType::Slice(ValueRttid::new(99, ValueKind::Struct))),
        _ => {
            module.functions[0].code = vec![Instruction::new(Opcode::LoadConst, 0, 99, 0)];
            module.functions[0].jit_metadata = vec![JitInstructionMetadata::None];
        }
    }
    module
}

fn finish_test_function(mut func: FunctionDef) -> FunctionDef {
    let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&func.code);
    func.has_calls = has_calls;
    func.has_call_extern = has_call_extern;
    func.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
    func.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&func.slot_types);
    func
}

fn assert_zero_slot_range_rejected(module: Module, access: &'static str) {
    let err = verify_module(&module).expect_err("zero-slot out-of-frame range must reject");
    match err {
        ModuleVerificationError::SlotOutOfRange { access: actual, .. } => {
            assert_eq!(actual, access)
        }
        other => panic!("expected SlotOutOfRange for {access}, got {other:?}"),
    }
}

fn one_slot_struct_transfer() -> crate::bytecode::TransferType {
    crate::bytecode::TransferType {
        meta_raw: ValueMeta::new(1, ValueKind::Struct).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Struct).to_raw(),
        slots: 1,
    }
}

fn struct_key_map_new_module(key_meta: ValueMeta, key_rttid_const: i64) -> Module {
    let mut module = Module::new("map-new-bare-key-rttid".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![FieldMeta {
            name: "x".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(1, ValueKind::Int64),
            embedded: false,
            tag: None,
        }],
        field_index: [("x".to_string(), 0usize)].into_iter().collect(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: vec![StructField {
            name: "x".to_string(),
            typ: ValueRttid::new(1, ValueKind::Int64),
            tag: String::new(),
            embedded: false,
            pkg: "test".to_string(),
        }],
        meta_id: 0,
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));

    let val_meta = ValueMeta::new(0, ValueKind::Int64).to_raw() as i64;
    module
        .constants
        .push(Constant::Int(((key_meta.to_raw() as i64) << 32) | val_meta));
    module.constants.push(Constant::Int(key_rttid_const));

    let mut func =
        function_with_slot_types(vec![SlotType::GcRef, SlotType::Value, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);
    module
}
