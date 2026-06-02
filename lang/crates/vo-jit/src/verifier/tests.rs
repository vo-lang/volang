use super::*;
use vo_common_core::bytecode::ReturnFlags;
use vo_common_core::instruction::HINT_LOOP;
use vo_common_core::types::ValueKind;
use vo_runtime::bytecode::{
    Constant, ExternDef, GlobalDef, JitInstructionMetadata, Module as VoModule,
};
use vo_runtime::instruction::Instruction;
use vo_runtime::SlotType;

fn make_func(
    code: Vec<Instruction>,
    jit_metadata: Vec<JitInstructionMetadata>,
    local_slots: u16,
) -> FunctionDef {
    crate::test_fixtures::JitFunctionBuilder::new(code)
        .name("verify")
        .local_slots(local_slots)
        .jit_metadata(jit_metadata)
        .build()
}

fn make_func_with_slot_types(
    code: Vec<Instruction>,
    jit_metadata: Vec<JitInstructionMetadata>,
    slot_types: Vec<SlotType>,
    ret_slots: u16,
) -> FunctionDef {
    crate::test_fixtures::JitFunctionBuilder::new(code)
        .name("verify")
        .slot_types(slot_types)
        .signature(0, 0, ret_slots)
        .jit_metadata(jit_metadata)
        .build()
}

fn make_func_with_shape(
    code: Vec<Instruction>,
    jit_metadata: Vec<JitInstructionMetadata>,
    slot_types: Vec<SlotType>,
    param_slots: u16,
    ret_slots: u16,
    error_ret_slot: i16,
) -> FunctionDef {
    crate::test_fixtures::function_with_shape(
        code,
        jit_metadata,
        slot_types,
        param_slots,
        ret_slots,
        error_ret_slot,
    )
}

fn hint_loop(end_offset: u8) -> Instruction {
    Instruction::with_flags(Opcode::Hint, HINT_LOOP, (end_offset as u16) << 8, 0, 0)
}

fn jump(offset: i32) -> Instruction {
    let encoded = offset as u32;
    Instruction::new(Opcode::Jump, 0, encoded as u16, (encoded >> 16) as u16)
}

fn refresh_function_metadata(func: &mut FunctionDef) {
    crate::test_fixtures::refresh_derived_function_fields(func);
}

fn transfer_int(slots: u16) -> vo_runtime::bytecode::TransferType {
    vo_runtime::bytecode::TransferType {
        meta_raw: vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int).to_raw(),
        rttid_raw: vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Int).to_raw(),
        slots,
    }
}

#[test]
fn verify_module_rejects_invalid_function_through_shared_strict_entry() {
    let good = crate::test_fixtures::JitFunctionBuilder::new(vec![Instruction::new(
        Opcode::Return,
        0,
        0,
        0,
    )])
    .name("good")
    .local_slots(1)
    .build();
    let bad = crate::test_fixtures::JitFunctionBuilder::new(vec![Instruction::new(
        Opcode::Return,
        0,
        0,
        0,
    )])
    .name("bad")
    .local_slots(1)
    .jit_metadata(Vec::new())
    .build();
    let module = crate::test_fixtures::module_with_functions("verify", vec![good, bad]);

    assert!(matches!(
        verify_module(&module),
        Err(JitMetadataError::LengthMismatch {
            func,
            code_len: 1,
            metadata_len: 0,
        }) if func == "bad"
    ));
}

#[test]
fn verified_module_token_detects_metadata_mutation() {
    let func = crate::test_fixtures::JitFunctionBuilder::new(vec![Instruction::new(
        Opcode::Return,
        0,
        0,
        0,
    )])
    .name("verified")
    .local_slots(1)
    .build();
    let mut module = crate::test_fixtures::module_with_functions("verify", vec![func]);
    let verified = verify_module(&module).expect("valid module verifies");

    assert!(verified.matches(&module));

    module.functions[0].jit_metadata[0] = JitInstructionMetadata::LoopEnd { end_pc: 1 };

    assert!(!verified.matches(&module));
    assert!(matches!(
        verify_module(&module),
        Err(JitMetadataError::WrongMetadataKind { func, pc: 0, .. }) if func == "verified"
    ));
}

#[test]
fn rejects_param_transfer_slot_drift_before_jit_load() {
    let mut module = VoModule::new("verify".to_string());
    let mut func = make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Value],
        2,
        0,
        -1,
    );
    func.param_count = 1;
    func.param_types = vec![transfer_int(1)];
    module.functions.push(func);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::FunctionInvariant { detail, .. })
            if detail.contains("param_types total slots")
    ));
}

#[test]
fn accepts_method_expression_transfer_metadata_with_explicit_receiver() {
    let mut module = VoModule::new("verify".to_string());
    let mut func = make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Value, SlotType::Value],
        3,
        0,
        -1,
    );
    func.recv_slots = 2;
    func.param_types = vec![transfer_int(2), transfer_int(1)];
    module.functions.push(func);

    verify_jit_metadata(&module.functions[0], &module)
        .expect("method-expression wrappers may encode receiver in param_types");
}

#[test]
fn rejects_capture_transfer_shape_drift_before_closure_gc_layout() {
    let mut module = VoModule::new("verify".to_string());
    let mut func = make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        1,
        0,
        -1,
    );
    func.is_closure = true;
    func.capture_types = Vec::new();
    func.capture_slot_types = vec![SlotType::GcRef];
    module.functions.push(func);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::FunctionInvariant { detail, .. })
            if detail.contains("capture_types.len()")
    ));
}

#[test]
fn rejects_transfer_value_kind_drift_before_island_payload_lowering() {
    let mut module = VoModule::new("verify".to_string());
    let mut func = make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value],
        1,
        0,
        -1,
    );
    func.param_count = 1;
    func.param_types = vec![vo_runtime::bytecode::TransferType {
        meta_raw: vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int).to_raw(),
        rttid_raw: vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Float64).to_raw(),
        slots: 1,
    }];
    module.functions.push(func);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::FunctionInvariant { detail, .. })
            if detail.contains("ValueMeta kind")
    ));
}

#[test]
fn rejects_jump_target_out_of_bounds() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![jump(2)],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidBranchTarget {
            opcode: Opcode::Jump,
            target: 2,
            code_len: 1,
            ..
        })
    ));
}

#[test]
fn rejects_forloop_target_out_of_bounds_without_wrapping() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(
            Opcode::ForLoop,
            0,
            0,
            1,
            (-3i16) as u16,
        )],
        vec![JitInstructionMetadata::None],
        2,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidBranchTarget {
            opcode: Opcode::ForLoop,
            target: -2,
            code_len: 1,
            ..
        })
    ));
}

#[test]
fn rejects_copyn_partial_interface_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::CopyN, 0, 2, 2)],
        vec![JitInstructionMetadata::None],
        vec![
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Value,
        ],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::CopyN,
            ..
        })
    ));
}

#[test]
fn rejects_copyn_source_destination_layout_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::CopyN, 0, 2, 2)],
        vec![JitInstructionMetadata::None],
        vec![
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
        ],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::CopyN,
            ..
        })
    ));
}

#[test]
fn rejects_function_def_invariant_mismatches_before_instruction_review() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value],
        2,
        0,
        -1,
    ));
    module.functions[0].local_slots = 3;

    let err = verify_jit_metadata(&module.functions[0], &module)
        .expect_err("local_slots/slot_types drift must fail before JIT lowering");
    assert!(
        matches!(err, JitMetadataError::FunctionInvariant { .. }),
        "unexpected error: {err:?}"
    );
}

#[test]
fn rejects_function_def_derived_flag_drift() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::CallExtern, 0, 0, 0)],
        vec![JitInstructionMetadata::CallExternLayout {
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }],
        vec![SlotType::Value],
        0,
    ));
    module.functions[0].has_call_extern = false;

    let err = verify_jit_metadata(&module.functions[0], &module)
        .expect_err("derived call flags must be recomputed at the verifier boundary");
    assert!(
        matches!(err, JitMetadataError::FunctionInvariant { .. }),
        "unexpected error: {err:?}"
    );
}

#[test]
fn rejects_single_copy_source_destination_layout_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::Copy, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::Copy,
            ..
        })
    ));
}

#[test]
fn rejects_dynamic_closure_and_island_operands_outside_gcref_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::CallClosure, 0, 1, 0)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }],
        vec![SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GoIsland, 1, 0, 1, 2)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value],
            ret_layout: Vec::new(),
        }],
        vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::CallClosure,
            access: "CallClosure callee",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::GoIsland,
            access: "GoIsland island",
            ..
        })
    ));
}

#[test]
fn rejects_integer_control_and_len_ops_on_root_typed_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::JumpIf, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::ForLoop,
            0,
            0,
            1,
            (-1i16) as u16,
        )],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::MapLen, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::GcRef],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::JumpIf,
            access: "JumpIf condition",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ForLoop,
            access: "ForLoop index",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::MapLen,
            access: "MapLen destination",
            ..
        })
    ));
}

#[test]
fn rejects_iface_assign_invalid_flags_and_reference_source_drift() {
    let mut module = VoModule::new("verify".to_string());
    module.constants.push(Constant::Int(0));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::IfaceAssign, 250, 0, 2, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Interface1, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::IfaceAssign,
            ValueKind::String as u8,
            0,
            2,
            0,
        )],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Interface1, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidValueKind {
            opcode: Opcode::IfaceAssign,
            raw: 250,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::IfaceAssign,
            access: "IfaceAssign source",
            ..
        })
    ));
}

#[test]
fn rejects_iface_assign_and_panic_without_interface_pairs() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::IfaceAssign, 2, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::Panic, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::IfaceAssign,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::Panic,
            ..
        })
    ));
}

#[test]
fn rejects_return_error_slot_without_interface_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_shape(
        vec![Instruction::with_flags(Opcode::Return, 1, 0, 2, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
        2,
        0,
    ));
    module.functions[0].ret_slot_types = vec![SlotType::GcRef, SlotType::Value];

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::Return,
            ..
        })
    ));
}

#[test]
fn rejects_return_slot_count_drift_before_jit_lowering() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Value],
        0,
        2,
        -1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::CallShapeMismatch {
            opcode: Opcode::Return,
            ..
        })
    ));
}

#[test]
fn rejects_return_unknown_flags_before_jit_lowering() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_shape(
        vec![Instruction::with_flags(Opcode::Return, 0x04, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value],
        0,
        1,
        -1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInstructionFlags {
            opcode: Opcode::Return,
            flags: 0x04,
            allowed: ReturnFlags::ALLOWED_BITS,
            ..
        })
    ));
}

#[test]
fn rejects_heap_return_gcref_start_drift_before_jit_lowering() {
    let mut module = VoModule::new("verify".to_string());
    let mut func = make_func_with_shape(
        vec![Instruction::with_flags(
            Opcode::Return,
            ReturnFlags::HEAP_RETURNS.bits(),
            0,
            1,
            0,
        )],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::GcRef],
        0,
        1,
        -1,
    );
    func.heap_ret_gcref_count = 1;
    func.heap_ret_gcref_start = 1;
    func.heap_ret_slots = vec![1];
    module.functions.push(func);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::CallShapeMismatch {
            opcode: Opcode::Return,
            ..
        })
    ));
}

#[test]
fn rejects_call_slot_contract_mismatches() {
    let callee = make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Interface1],
        2,
        0,
        -1,
    );
    let caller = make_func_with_slot_types(
        vec![Instruction::new(Opcode::Call, 1, 0, 2 << 8)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    );
    let closure = make_func_with_slot_types(
        vec![Instruction::new(Opcode::CallClosure, 0, 1, 1 << 8)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Interface0],
            ret_layout: Vec::new(),
        }],
        vec![SlotType::GcRef, SlotType::Interface1],
        0,
    );
    let iface = make_func_with_slot_types(
        vec![Instruction::new(Opcode::CallIface, 0, 2, 0)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    );
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(caller);
    module.functions.push(callee);
    module.functions.push(closure);
    module.functions.push(iface);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::Call,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::CallClosure,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[3], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::CallIface,
            ..
        })
    ));
}

#[test]
fn rejects_large_static_call_with_nonzero_legacy_shape_mirror() {
    let callee = make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 300, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value; 301],
        300,
        1,
        -1,
    );
    let caller = make_func_with_slot_types(
        vec![Instruction::new(Opcode::Call, 1, 0, (1 << 8) | 1)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value; 301],
        0,
    );
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(caller);
    module.functions.push(callee);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::CallShapeMismatch {
            opcode: Opcode::Call,
            ..
        })
    ));
}

#[test]
fn rejects_shared_closure_call_without_precise_arg_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GoStart, 1, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::DeferPush, 1, 0, 1, 2)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value, SlotType::GcRef],
            ret_layout: Vec::new(),
        }],
        vec![SlotType::GcRef, SlotType::Value, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingLayout {
            opcode: Opcode::GoStart,
            layout: "CallLayout",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::DeferPush,
            access: "closure call args",
            ..
        })
    ));
}

#[test]
fn accepts_shared_closure_call_with_precise_arg_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GoStart, 1, 0, 1, 2)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value, SlotType::GcRef],
            ret_layout: Vec::new(),
        }],
        vec![SlotType::GcRef, SlotType::Value, SlotType::GcRef],
        0,
    ));

    verify_jit_metadata(&module.functions[0], &module).unwrap();
}

#[test]
fn rejects_call_extern_param_kind_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.externs.push(ExternDef {
        name: "host".to_string(),
        param_slots: 0,
        ret_slots: 0,
        is_blocking: false,
        param_kinds: vec![vo_runtime::bytecode::ExtSlotKind::Value],
    });
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::CallExtern, 2, 0, 0, 0)],
        vec![JitInstructionMetadata::CallExternLayout {
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }],
        2,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::CallShapeMismatch {
            opcode: Opcode::CallExtern,
            ..
        })
    ));
}

#[test]
fn rejects_scalar_slot_contract_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::AddI, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::Value, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::AddF, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Float, SlotType::Float],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::EqF, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::Float, SlotType::Float],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::IndexCheck, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::AddI,
            access: "AddI destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::AddF,
            access: "AddF destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::EqF,
            access: "EqF destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[3], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::IndexCheck,
            access: "IndexCheck index",
            ..
        })
    ));
}

#[test]
fn rejects_conversion_slot_contract_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::ConvI2F, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::ConvF2I, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::ConvF64F32, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::ConvF32F64, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::Trunc, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ConvI2F,
            access: "ConvI2F destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ConvF2I,
            access: "ConvF2I source",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ConvF64F32,
            access: "ConvF64F32 source",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[3], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ConvF32F64,
            access: "ConvF32F64 source",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[4], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::Trunc,
            access: "Trunc destination",
            ..
        })
    ));
}

#[test]
fn rejects_store_slot_contract_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 2,
        value_kind: 0,
        meta_id: 0,
        slot_types: vec![SlotType::Interface0, SlotType::Interface1],
    });
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GlobalSetN, 2, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::PtrSet, 0, 0, 0, 1)],
        vec![JitInstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::GcRef],
        }],
        vec![SlotType::GcRef, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SlotSetN, 2, 0, 2, 3)],
        vec![JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::Value, SlotType::Value],
        }],
        vec![
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Value,
        ],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::GlobalSetN,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::PtrSet,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::SlotSetN,
            ..
        })
    ));
}

#[test]
fn verifier_slot_contract_has_no_wildcard_allowlist() {
    let src = include_str!("instruction_contracts.rs");
    let start = src.find("fn verify_slot_contract").unwrap();
    let end = src[start..]
        .find("\nfn verify_map_iter_next_contract")
        .map(|offset| start + offset)
        .unwrap();
    let body = &src[start..end];

    assert!(
        !body.contains("_ => Ok(())"),
        "verify_slot_contract must explicitly name every opcode it accepts; wildcard Ok lets new opcodes bypass JIT contract review"
    );
}

#[test]
fn verifier_metadata_layouts_use_typed_accessors() {
    let src = include_str!("instruction_contracts.rs");
    assert!(
        src.contains("decode_metadata_layout("),
        "verifier must route metadata payloads through its typed accessor gate"
    );
    assert!(
        !src.contains("Some(JitInstructionMetadata::")
            && !src.contains("match func.jit_metadata.get(pc)"),
        "verifier must not maintain a second metadata enum decoding table"
    );
}

#[test]
fn rejects_metadata_length_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::LengthMismatch { .. })
    ));
}

#[test]
fn rejects_wrong_metadata_kind_for_opcode() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadInt, 0, 1, 0)],
        vec![JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value],
        }],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::WrongMetadataKind { .. })
    ));
}

#[test]
fn accepts_zero_loads_into_gcref_slots_for_nil_references() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::LoadInt, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));
    module.constants.push(Constant::Nil);
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::LoadConst, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));

    verify_jit_metadata(&module.functions[0], &module).expect("zero LoadInt can nil a GcRef");
    verify_jit_metadata(&module.functions[1], &module).expect("nil LoadConst can nil a GcRef");
}

#[test]
fn accepts_raw_reference_slots_for_equality_and_bit_tests() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![
            Instruction::new(Opcode::EqI, 0, 1, 2),
            Instruction::new(Opcode::And, 3, 4, 5),
        ],
        vec![JitInstructionMetadata::None, JitInstructionMetadata::None],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Value,
        ],
        0,
    ));

    verify_jit_metadata(&module.functions[0], &module)
        .expect("raw reference/header equality and bit tests are valid lowering inputs");
}

#[test]
fn rejects_invalid_opcode() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction {
            op: 254,
            flags: 0,
            a: 0,
            b: 0,
            c: 0,
        }],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidOpcode { raw: 254, .. })
    ));
}

#[test]
fn rejects_missing_load_const_constant() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadConst, 0, 7, 0)],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingConstant { const_id: 7, .. })
    ));
}

#[test]
fn rejects_string_constant_loaded_without_str_new() {
    let mut module = VoModule::new("verify".to_string());
    module.constants.push(Constant::String("bad".to_string()));
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadConst, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::ConstantKindMismatch {
            opcode: Opcode::LoadConst,
            expected,
            actual: "String",
            ..
        }) if expected.contains("StrNew")
    ));
}

#[test]
fn rejects_str_new_missing_or_non_string_constant() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::StrNew, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));
    module.constants.push(Constant::Int(42));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::StrNew, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingConstant { const_id: 1, .. })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::ConstantKindMismatch {
            opcode: Opcode::StrNew,
            expected: "String",
            actual: "Int",
            ..
        })
    ));
}

#[test]
fn rejects_iface_assign_missing_or_non_int_metadata_constant() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::IfaceAssign, 2, 0, 0, 9)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Interface1],
        0,
    ));
    module
        .constants
        .push(Constant::String("not metadata".to_string()));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::IfaceAssign, 2, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Interface1],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingConstant { const_id: 9, .. })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::ConstantKindMismatch {
            opcode: Opcode::IfaceAssign,
            expected: "Int",
            actual: "String",
            ..
        })
    ));
}

#[test]
fn rejects_global_get_range_out_of_bounds() {
    let mut module = VoModule::new("verify".to_string());
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 1,
        value_kind: 0,
        meta_id: 0,
        slot_types: vec![SlotType::Value],
    });
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::GlobalGet, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::GlobalSlotOutOfRange { access: "read", .. })
    ));
}

#[test]
fn rejects_global_get_destination_layout_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 2,
        value_kind: 0,
        meta_id: 0,
        slot_types: vec![SlotType::GcRef, SlotType::Float],
    });
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GlobalGetN, 2, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Float],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::GlobalGetN,
            access: "GlobalGet destination",
            expected,
            actual,
            ..
        }) if expected == vec![SlotType::GcRef, SlotType::Float]
            && actual == vec![SlotType::Value, SlotType::Float]
    ));
}

#[test]
fn rejects_global_set_source_layout_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 2,
        value_kind: 0,
        meta_id: 0,
        slot_types: vec![SlotType::GcRef, SlotType::Float],
    });
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GlobalSetN, 2, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Float],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::GlobalSetN,
            access: "GlobalSet source",
            expected,
            actual,
            ..
        }) if expected == vec![SlotType::GcRef, SlotType::Float]
            && actual == vec![SlotType::Value, SlotType::Float]
    ));
}

#[test]
fn rejects_closure_new_missing_function() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::ClosureNew, 0, 0, 3, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingFunction { callee_id: 3, .. })
    ));
}

#[test]
fn rejects_static_go_and_defer_missing_function() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::GoStart, 0, 4, 0, 0)],
        vec![JitInstructionMetadata::None],
        1,
    ));
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::DeferPush, 0, 4, 0, 0)],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingFunction { callee_id: 4, .. })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::MissingFunction { callee_id: 4, .. })
    ));
}

#[test]
fn rejects_slot_get_contract_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SlotGetN, 2, 2, 0, 1)],
        vec![JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![
            SlotType::Interface0,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::SlotGetN,
            ..
        })
    ));
}

#[test]
fn accepts_single_slot_raw_transfers_from_interface_pair_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::SlotGet, 2, 0, 1)],
        vec![JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::Interface0],
        }],
        vec![SlotType::Interface0, SlotType::Value, SlotType::Interface0],
        0,
    ));

    assert!(verify_jit_metadata(&module.functions[0], &module).is_ok());
}

#[test]
fn rejects_pointer_reads_from_non_root_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::PtrGet, 0, 1, 0)],
        vec![JitInstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Value],
        }],
        vec![SlotType::Value, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::PtrGet,
            access: "PtrGet pointer",
            ..
        })
    ));
}

#[test]
fn accepts_pointer_reads_from_gc_root_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::PtrGet, 0, 1, 0)],
        vec![JitInstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Value],
        }],
        vec![SlotType::Value, SlotType::GcRef],
        0,
    ));

    assert!(verify_jit_metadata(&module.functions[0], &module).is_ok());
}

#[test]
fn rejects_interior_pointer_destinations_outside_gc_root_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::PtrAdd, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SliceAddr, 0x08, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        }],
        vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::PtrAdd,
            access: "PtrAdd destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::SliceAddr,
            access: "SliceAddr destination",
            ..
        })
    ));
}

#[test]
fn accepts_zero_elem_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 0,
            needs_sign_extend: false,
            slot_layout: Vec::new(),
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));

    verify_jit_metadata(&module.functions[0], &module).expect("zero-size elements are valid");
}

#[test]
fn rejects_zero_elem_layout_with_sign_extension() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 0,
            needs_sign_extend: true,
            slot_layout: Vec::new(),
        }],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidElemLayout { elem_bytes: 0, .. })
    ));
}

#[test]
fn rejects_inconsistent_elem_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::SliceGet, 0x81, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        }],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InconsistentElemLayout { flags: 0x81, .. })
    ));
}

#[test]
fn rejects_slot_effects_outside_locals() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Copy, 0, 3, 0)],
        vec![JitInstructionMetadata::None],
        2,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotOutOfRange {
            access: "Copy source",
            ..
        })
    ));
}

#[test]
fn rejects_slot_range_overflow_without_wrapping() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::CopyN, 0, u16::MAX, 2)],
        vec![JitInstructionMetadata::None],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotRangeOverflow {
            access: "CopyN source",
            ..
        })
    ));
}

#[test]
fn rejects_operand_offset_overflow_without_wrapping() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::ArrayNew, 0, 1, u16::MAX)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 3],
        }],
        4,
    ));
    module.functions[0].slot_types[0] = SlotType::GcRef;
    refresh_function_metadata(&mut module.functions[0]);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotRangeOverflow {
            access: "ArrayNew length/elem_bytes",
            ..
        })
    ));
}

#[test]
fn rejects_missing_required_dynamic_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingLayout {
            layout: "ElemLayout",
            ..
        })
    ));
}

#[test]
fn rejects_missing_array_addr_dynamic_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::ArrayAddr, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingLayout {
            opcode: Opcode::ArrayAddr,
            layout: "ElemLayout",
            ..
        })
    ));
}

#[test]
fn rejects_missing_slice_addr_dynamic_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::SliceAddr, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingLayout {
            opcode: Opcode::SliceAddr,
            layout: "ElemLayout",
            ..
        })
    ));
}

#[test]
fn rejects_indexed_metadata_layout_slot_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::ArrayGet, 0, 0, 2, 3)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 16,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 2],
        }],
        vec![
            SlotType::Interface0,
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::ArraySet, 0, 1, 4, 20)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 16,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 2],
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Value,
        ],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::ArrayGet,
            access: "ArrayGet destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::ArraySet,
            access: "ArraySet source",
            ..
        })
    ));
}

#[test]
fn rejects_map_metadata_layout_slot_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::MapGet, 8, 1, 4)],
        vec![JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Interface0, SlotType::Interface1],
            val_layout: vec![SlotType::Value, SlotType::GcRef],
            has_ok: true,
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::MapSet, 1, 4, 8)],
        vec![JitInstructionMetadata::MapSet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::MapDelete, 1, 4, 0)],
        vec![JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Value,
        ],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::MapGet,
            access: "MapGet key",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::MapSet,
            access: "MapSet value",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::MapDelete,
            access: "MapDelete key",
            ..
        })
    ));
}

#[test]
fn rejects_queue_select_iface_contract_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::QueueSend, 2, 0, 1, 0)],
        vec![JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![SlotType::GcRef, SlotType::Interface0, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::QueueRecv, 3, 0, 2, 0)],
        vec![JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::GcRef],
        }],
        vec![SlotType::GcRef, SlotType::Interface1, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SelectSend, 2, 0, 1, 0)],
        vec![JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![SlotType::GcRef, SlotType::Interface0, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::IfaceAssert,
            0b0010_1001,
            0,
            1,
            0,
        )],
        vec![JitInstructionMetadata::IfaceAssertLayout {
            result_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::IfaceEq, 0, 1, 3)],
        vec![JitInstructionMetadata::None],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Interface1,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::Recover, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::GcRef],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::QueueSend,
            access: "QueueSend value",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::QueueRecv,
            access: "QueueRecv ok",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::SelectSend,
            access: "SelectSend value",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[3], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::IfaceAssert,
            access: "IfaceAssert source",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[4], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::IfaceEq,
            access: "IfaceEq lhs",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[5], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::Recover,
            access: "Recover destination",
            ..
        })
    ));
}

#[test]
fn rejects_iface_assert_missing_or_mismatched_result_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::IfaceAssert,
            1 << 3,
            0,
            1,
            0,
        )],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::IfaceAssert,
            1 << 3,
            0,
            1,
            0,
        )],
        vec![JitInstructionMetadata::IfaceAssertLayout {
            result_layout: vec![SlotType::Value, SlotType::Value],
        }],
        vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::IfaceAssert,
            1 << 3,
            0,
            1,
            0,
        )],
        vec![JitInstructionMetadata::IfaceAssertLayout {
            result_layout: vec![SlotType::GcRef],
        }],
        vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingLayout {
            opcode: Opcode::IfaceAssert,
            layout: "IfaceAssertLayout",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::CallShapeMismatch {
            opcode: Opcode::IfaceAssert,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::IfaceAssert,
            access: "IfaceAssert destination",
            ..
        })
    ));
}

#[test]
fn rejects_wrong_slice_addr_layout_metadata_kind() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::SliceAddr, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value],
        }],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::WrongMetadataKind {
            opcode: Opcode::SliceAddr,
            ..
        })
    ));
}

#[test]
fn rejects_inconsistent_array_addr_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::ArrayAddr, 0x82, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 4,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        }],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InconsistentElemLayout { flags: 0x82, .. })
    ));
}

#[test]
fn accepts_packed_addr_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![
            Instruction::with_flags(Opcode::ArrayAddr, 0x82, 0, 1, 2),
            Instruction::with_flags(Opcode::SliceAddr, 0x44, 3, 4, 5),
        ],
        vec![
            JitInstructionMetadata::ElemLayout {
                elem_bytes: 2,
                needs_sign_extend: true,
                slot_layout: vec![SlotType::Value],
            },
            JitInstructionMetadata::ElemLayout {
                elem_bytes: 4,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Float],
            },
        ],
        vec![
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::Value,
        ],
        0,
    ));

    verify_jit_metadata(&module.functions[0], &module).unwrap();
}

#[test]
fn rejects_map_get_output_range_overflow() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::MapGet, 0, 1, 2)],
        vec![JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value; u16::MAX as usize],
            has_ok: true,
        }],
        4,
    ));
    module.functions[0].slot_types[1] = SlotType::GcRef;
    refresh_function_metadata(&mut module.functions[0]);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotRangeOverflow {
            access: "MapGet destination",
            ..
        })
    ));
}

#[test]
fn accepts_dynamic_map_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::MapGet, 4, 1, 2)],
        vec![JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value, SlotType::Value],
            val_layout: vec![SlotType::Value, SlotType::Value],
            has_ok: true,
        }],
        8,
    ));
    module.functions[0].slot_types[1] = SlotType::GcRef;
    refresh_function_metadata(&mut module.functions[0]);

    verify_jit_metadata(&module.functions[0], &module).expect("valid metadata");
}

#[test]
fn rejects_legacy_map_metadata_without_precise_slot_layouts() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::MapSet, 1, 2, 4)],
        vec![JitInstructionMetadata::LegacyMapSet {
            key_slots: 1,
            val_slots: 1,
        }],
        6,
    ));
    module.functions[0].slot_types[1] = SlotType::GcRef;
    refresh_function_metadata(&mut module.functions[0]);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::UnsupportedLegacyMetadata {
            opcode: Opcode::MapSet,
            metadata: "LegacyMapSet",
            ..
        })
    ));
}

#[test]
fn accepts_loop_end_metadata_with_real_backedge() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![
            hint_loop(0),
            Instruction::new(Opcode::LoadInt, 0, 1, 0),
            jump(-1),
        ],
        vec![
            JitInstructionMetadata::LoopEnd { end_pc: 2 },
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
        ],
        1,
    ));

    verify_jit_metadata(&module.functions[0], &module).expect("valid loop end");
}

#[test]
fn rejects_loop_end_metadata_without_real_backedge() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![
            hint_loop(0),
            Instruction::new(Opcode::LoadInt, 0, 1, 0),
            Instruction::new(Opcode::LoadInt, 0, 2, 0),
        ],
        vec![
            JitInstructionMetadata::LoopEnd { end_pc: 2 },
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
        ],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidLoopEndBackEdge {
            begin_pc: 1,
            end_pc: 2,
            ..
        })
    ));
}

#[test]
fn rejects_compact_loop_end_without_real_backedge() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![
            hint_loop(2),
            Instruction::new(Opcode::LoadInt, 0, 1, 0),
            Instruction::new(Opcode::LoadInt, 0, 2, 0),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
        ],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidLoopEndBackEdge {
            begin_pc: 1,
            end_pc: 2,
            ..
        })
    ));
}
