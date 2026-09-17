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

/// Most verifier unit fixtures isolate one unrelated contract and historically
/// omitted the trailing control transfer. Complete those synthetic functions
/// here so each test still reaches the invariant it was built to exercise.
/// Production modules go directly through `super::verify_module` unchanged.
fn verify_module(module: &Module) -> Result<(), ModuleVerificationError> {
    let mut fixture = module.clone();
    for func in &mut fixture.functions {
        if !func.code.last().is_some_and(|inst| {
            matches!(inst.opcode(), Opcode::Jump | Opcode::Return | Opcode::Panic)
        }) {
            let terminal = if func.heap_ret_gcref_count > 0 {
                Instruction::with_flags(
                    Opcode::Return,
                    ReturnFlags::heap_returns(false).bits(),
                    func.heap_ret_gcref_start,
                    func.heap_ret_gcref_count,
                    0,
                )
            } else {
                Instruction::new(Opcode::Return, 0, func.ret_slots, 0)
            };
            func.code.push(terminal);
            func.instruction_metadata.push(InstructionMetadata::None);
        }
    }
    super::verify_module(&fixture).map(|_| ())
}

#[test]
fn production_verifier_rejects_empty_function_bytecode() {
    let mut module = Module::new("empty-function".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));

    let error = super::verify_module(&module).expect_err("empty function must be rejected");
    assert!(matches!(
        error,
        ModuleVerificationError::FunctionInvariant { detail, .. }
            if detail.contains("bytecode is empty")
    ));
}

#[test]
fn production_verifier_rejects_final_fallthrough() {
    let mut module = Module::new("final-fallthrough".to_string());
    let mut func = function_with_slot_types(Vec::new());
    func.code.push(Instruction::new(Opcode::Hint, 0, 0, 0));
    func.instruction_metadata.push(InstructionMetadata::None);
    module.functions.push(func);

    let error = super::verify_module(&module).expect_err("final fallthrough must be rejected");
    assert!(matches!(
        error,
        ModuleVerificationError::FunctionInvariant { detail, .. }
            if detail.contains("final Hint instruction") && detail.contains("falls through")
    ));
}

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
        instruction_metadata: Vec::new(),
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
    _target_slots: u16,
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
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![Instruction::new(Opcode::IfaceAssert, 0, 2, 0)];
    func.instruction_metadata = vec![InstructionMetadata::IfaceAssertLayout {
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
        Instruction::new(Opcode::MapNew, 0, 1, 0),
        Instruction::new(Opcode::MapIterInit, 3, 0, 0),
        next_inst,
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
        InstructionMetadata::None,
        InstructionMetadata::MapIterNext {
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
    func.instruction_metadata = vec![InstructionMetadata::None];
    module.functions.push(func);
    module
}

fn verifier_fuzz_case(case_id: usize) -> Module {
    let mut module = verifier_fuzz_base_module(case_id);
    match case_id % 10 {
        0 => {}
        1 => module.functions[0].instruction_metadata.clear(),
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
        5 => {}
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
            module.functions[0].instruction_metadata = vec![InstructionMetadata::None];
        }
    }
    module
}

fn finish_test_function(mut func: FunctionDef) -> FunctionDef {
    let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&func.code);
    func.has_calls = has_calls;
    func.has_call_extern = has_call_extern;
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
        function_with_slot_types(vec![SlotType::GcBase, SlotType::Value, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, 0),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);
    module
}

#[test]
fn float32_opcodes_require_scalar_bits_and_boolean_comparison_results() {
    for opcode in [
        Opcode::AddF32,
        Opcode::SubF32,
        Opcode::MulF32,
        Opcode::DivF32,
        Opcode::NegF32,
        Opcode::EqF32,
        Opcode::NeF32,
        Opcode::LtF32,
        Opcode::LeF32,
        Opcode::GtF32,
        Opcode::GeF32,
    ] {
        let comparison = matches!(
            opcode,
            Opcode::EqF32
                | Opcode::NeF32
                | Opcode::LtF32
                | Opcode::LeF32
                | Opcode::GtF32
                | Opcode::GeF32
        );
        for operand in [SlotType::Value, SlotType::Float] {
            let destination = if comparison { SlotType::Value } else { operand };
            let mut function = function_with_slot_types(vec![operand, operand, destination]);
            function.code = vec![
                Instruction::new(opcode, 2, 0, 1),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ];
            function.instruction_metadata = vec![InstructionMetadata::None; 2];
            let mut module = Module::new("f32-slot-contract".into());
            module.functions.push(function);
            super::verify_module(&module).unwrap();
            for slot in if opcode == Opcode::NegF32 {
                &[0, 2][..]
            } else {
                &[0, 1, 2][..]
            } {
                let mut invalid = module.clone();
                invalid.functions[0].slot_types[*slot] = SlotType::GcRef;
                assert!(
                    super::verify_module(&invalid).is_err(),
                    "{opcode:?} slot {slot}"
                );
            }
            if comparison {
                module.functions[0].slot_types[2] = SlotType::Float;
                assert!(
                    super::verify_module(&module).is_err(),
                    "{opcode:?} boolean destination"
                );
            }
        }
    }
}

#[test]
fn transfer_scratch_reads_overlapping_copies_from_the_complete_input() {
    let module = Module::new("overlapping-transfer".into());
    let mut func = function_with_slot_types(vec![SlotType::Value; 3]);
    func.code.push(Instruction::new(Opcode::CopyN, 1, 0, 2));
    func.instruction_metadata.push(InstructionMetadata::None);
    let slots = [0, 1, 2];
    let input = [
        ConstantFact::Int(11),
        ConstantFact::Int(22),
        ConstantFact::Int(33),
    ];
    let mut output = [ConstantFact::Conflict; 3];
    apply_constant_fact_transfer(&func, &module, 0, &slots, &input, &mut output);
    assert_eq!(
        output,
        [
            ConstantFact::Int(11),
            ConstantFact::Int(11),
            ConstantFact::Int(22)
        ]
    );
    assert_eq!(
        input,
        [
            ConstantFact::Int(11),
            ConstantFact::Int(22),
            ConstantFact::Int(33)
        ]
    );

    let constants = ConstantFactAnalysis {
        slots: Vec::new(),
        before: Vec::new(),
    };
    let input = [
        IndexCheckFact::Checked { len: 11 },
        IndexCheckFact::Checked { len: 22 },
        IndexCheckFact::Checked { len: 33 },
    ];
    let mut output = [IndexCheckFact::Conflict; 3];
    apply_index_check_transfer(&func, &module, &constants, 0, &slots, &input, &mut output);
    assert_eq!(
        output,
        [
            IndexCheckFact::Checked { len: 11 },
            IndexCheckFact::Checked { len: 11 },
            IndexCheckFact::Checked { len: 22 }
        ]
    );

    let queue = ContainerLayoutFact::Queue {
        elem_layout: Arc::from([SlotType::Value]),
    };
    let map = ContainerLayoutFact::Map {
        key_layout: Arc::from([SlotType::Value]),
        val_layout: Arc::from([SlotType::GcRef]),
    };
    let input = [queue.clone(), map.clone(), ContainerLayoutFact::Unknown];
    let mut output = core::array::from_fn::<_, 3, _>(|_| ContainerLayoutFact::Conflict);
    apply_container_layout_transfer(&func, &module, 0, &slots, &[None], &input, &mut output);
    assert_eq!(output, [queue.clone(), queue, map]);

    // A reused row must overwrite stale facts even for an instruction with no writes.
    func.code[0] = Instruction::new(Opcode::Hint, 0, 0, 0);
    apply_container_layout_transfer(&func, &module, 0, &slots, &[None], &input, &mut output);
    assert_eq!(output, input);
}
