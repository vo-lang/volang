use super::*;

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
fn verifier_slot_contract_dispatches_from_supplied_semantics_row() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::AddI, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Value, SlotType::Value],
        0,
    ));
    let ctx =
        crate::verifier::instruction_contracts::VerifierCtx::new(&module.functions[0], &module, 0);
    let mut row = crate::semantics::opcode_semantics(Opcode::AddI);
    row.verifier_domain = crate::semantics::VerifierDomain::Invalid;

    assert!(matches!(
        crate::verifier::instruction_contracts::verify_slot_contract_with_row(ctx, &row),
        Err(JitMetadataError::InvalidOpcode { raw, .. }) if raw == Opcode::AddI as u8
    ));
}
