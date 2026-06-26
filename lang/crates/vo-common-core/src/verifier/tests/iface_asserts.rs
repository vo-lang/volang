use super::*;

#[test]
fn module_verifier_rejects_iface_assign_raw_pair_destination_root_layout_061() {
    let mut module = Module::new("iface-assign-raw-pair-dst".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module
        .constants
        .push(Constant::Int(i64::from(IFACE_ASSIGN_NO_ITAB)));
    let mut func =
        function_with_slot_types(vec![SlotType::Value, SlotType::Value, SlotType::GcRef]);
    func.code = vec![Instruction::with_flags(
        Opcode::IfaceAssign,
        ValueKind::String as u8,
        0,
        2,
        0,
    )];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("IfaceAssign destination must be precise interface root layout");

    let msg = err.to_string();
    assert!(
        msg.contains("IfaceAssign destination")
            && msg.contains("expected [Interface0, Interface1]")
            && msg.contains("[Value, Value]"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_iface_assert_value_result_root_layout_drift_061() {
    let module = iface_assert_layout_module(
        "iface-assert-value-root-layout-drift",
        0,
        0,
        1,
        vec![
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Interface1,
        ],
        vec![SlotType::Value],
        RuntimeType::Basic(ValueKind::String),
    );

    let err = verify_module(&module)
        .expect_err("IfaceAssert result layout must match target runtime type layout");

    let msg = err.to_string();
    assert!(
        msg.contains("IfaceAssert metadata layout [Value] does not match target layout [GcRef]"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_iface_assert_interface_result_root_layout_drift_061() {
    let module = iface_assert_layout_module(
        "iface-assert-interface-root-layout-drift",
        1,
        0,
        2,
        vec![
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Interface1,
        ],
        vec![SlotType::Value, SlotType::Value],
        RuntimeType::Interface {
            methods: Vec::new(),
            meta_id: 0,
        },
    );

    let err = verify_module(&module)
        .expect_err("IfaceAssert interface result layout must be an interface pair");

    let msg = err.to_string();
    assert!(
            msg.contains(
                "IfaceAssert metadata layout [Value, Value] does not match target layout [Interface0, Interface1]"
            ),
            "{msg}"
        );
}

#[test]
fn module_verifier_rejects_iface_assert_missing_target_interface_meta_061() {
    let module = iface_assert_layout_module(
        "iface-assert-missing-target-interface-meta",
        1,
        1,
        2,
        vec![
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::Interface0,
            SlotType::Interface1,
        ],
        vec![SlotType::Interface0, SlotType::Interface1],
        RuntimeType::Interface {
            methods: Vec::new(),
            meta_id: 0,
        },
    );

    let err = verify_module(&module).expect_err("IfaceAssert interface target metadata must exist");

    let msg = err.to_string();
    assert!(
        msg.contains("IfaceAssert target interface meta 1 is missing"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_iface_assert_concrete_kind_interface_target_061() {
    let mut module = iface_assert_layout_module(
        "iface-assert-concrete-kind-interface-target",
        0,
        0,
        2,
        vec![
            SlotType::Interface0,
            SlotType::Interface1,
            SlotType::Interface0,
            SlotType::Interface1,
        ],
        vec![SlotType::Interface0, SlotType::Interface1],
        RuntimeType::Interface {
            methods: Vec::new(),
            meta_id: 0,
        },
    );
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });

    let err = verify_module(&module)
        .expect_err("IfaceAssert interface targets must use interface assert kind");

    let msg = err.to_string();
    assert!(
            msg.contains("IfaceAssert target runtime type 0 has Interface kind; interface targets must use assert_kind=1"),
            "{msg}"
        );
}
