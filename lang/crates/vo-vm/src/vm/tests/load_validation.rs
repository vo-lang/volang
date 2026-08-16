use super::*;
use crate::bytecode::RegisteredExternSource;

#[cfg(feature = "std")]
static INHERITED_PROVIDER_SAW_CHILD_CONTEXT: AtomicBool = AtomicBool::new(false);

#[cfg(feature = "std")]
fn inherited_context_probe(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    INHERITED_PROVIDER_SAW_CHILD_CONTEXT.store(
        ctx.program_args() == [b"child".to_vec()],
        std::sync::atomic::Ordering::SeqCst,
    );
    ExternResult::Ok
}

fn valid_module_with_math_sqrt_extern(name: &str) -> Module {
    let mut module = malformed_single_instruction_module(
        name,
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    module.externs.push(extern_def_for_test(
        vo_runtime::vo_extern_name!("math", "Sqrt"),
        ParamShape::Exact { slots: 1 },
        ReturnShape::with_slot_types(vec![SlotType::Float]),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    module
}

fn verified_test_image(name: &str) -> std::sync::Arc<LoadedModule> {
    let mut module = malformed_single_instruction_module(
        name,
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    std::sync::Arc::new(
        vo_common_core::verifier::verify_loaded_module(module)
            .expect("test module must pass common verification"),
    )
}

fn assert_loaded_image_reused(vm: &Vm, expected: &std::sync::Arc<LoadedModule>) {
    let actual = vm.module.as_ref().expect("VM must retain loaded image");
    assert!(std::sync::Arc::ptr_eq(expected, actual));
    assert!(core::ptr::eq(
        expected.runtime_type_facts(),
        actual.runtime_type_facts()
    ));
}

#[test]
fn verified_load_reuses_exact_module_image_and_runtime_facts() {
    let image = verified_test_image("vm-load-preverified");
    let expected = image.clone();
    let mut vm = Vm::new();

    vm.load_verified(image)
        .expect("common-verified module must load");

    assert_loaded_image_reused(&vm, &expected);
    assert_eq!(expected.runtime_type_facts().len(), 1);
}

#[cfg(feature = "std")]
#[test]
fn verified_extension_load_reuses_exact_module_image_and_runtime_facts() {
    let image = verified_test_image("vm-load-preverified-extensions");
    let expected = image.clone();
    let mut vm = Vm::new();

    vm.load_verified_with_extensions(image, None)
        .expect("common-verified module with no native extensions must load");

    assert_loaded_image_reused(&vm, &expected);
}

#[test]
fn verified_embedder_load_reuses_exact_module_image_and_runtime_facts() {
    let module = valid_module_with_math_sqrt_extern("vm-load-preverified-embedder");
    let image = std::sync::Arc::new(
        vo_common_core::verifier::verify_loaded_module(module)
            .expect("embedder test module must pass common verification"),
    );
    let expected = image.clone();
    let mut vm = Vm::new();
    vo_stdlib::register_portable_externs(
        vm.extern_registry_mut().expect("configuration phase"),
        &image.externs,
    )
    .expect("preconfigure portable stdlib");

    vm.load_verified_with_embedder_externs(image)
        .expect("common-verified embedder module must load");

    assert_loaded_image_reused(&vm, &expected);
    assert!(vm
        .state
        .extern_registry
        .resolved(0)
        .is_some_and(|provider| provider.source == RegisteredExternSource::Builtin));
}

#[test]
fn default_load_still_installs_target_stdlib_providers() {
    let module = valid_module_with_math_sqrt_extern("vm-load-default-stdlib");
    let mut vm = Vm::new();

    vm.load(module).expect("default load installs stdlib");

    assert!(vm
        .state
        .extern_registry
        .resolved(0)
        .is_some_and(|provider| provider.source == RegisteredExternSource::Builtin));
}

#[test]
fn default_load_accepts_matching_duplicate_stdlib_and_vm_externs() {
    let mut module = malformed_single_instruction_module(
        "vm-load-matching-duplicate-externs",
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    let macro_stdlib = extern_def_for_test(
        vo_runtime::vo_extern_name!("errors", "identity"),
        ParamShape::Exact { slots: 2 },
        ReturnShape::with_slot_types(vec![SlotType::Value]),
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    let manual_stdlib = extern_def_for_test(
        vo_runtime::vo_extern_name!("io", "getIoErrors"),
        ParamShape::Exact { slots: 0 },
        ReturnShape::slots(24),
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    let vm_owned = extern_def_for_test(
        vo_runtime::builtins::RUNTIME_MEM_GC_COLLECT_EXTERN_NAME,
        ParamShape::Exact { slots: 0 },
        ReturnShape::with_slot_types(vec![SlotType::Value]),
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    module.externs = vec![
        macro_stdlib.clone(),
        macro_stdlib,
        manual_stdlib.clone(),
        manual_stdlib,
        vm_owned.clone(),
        vm_owned,
    ];

    let mut vm = Vm::new();
    vm.load(module)
        .expect("matching duplicate extern contracts must share one provider");

    for (first_id, duplicate_id, source) in [
        (0, 1, RegisteredExternSource::Stdlib),
        (2, 3, RegisteredExternSource::Stdlib),
        (4, 5, RegisteredExternSource::Builtin),
    ] {
        let first = vm
            .state
            .extern_registry
            .resolved(first_id)
            .expect("first extern must resolve");
        let duplicate = vm
            .state
            .extern_registry
            .resolved(duplicate_id)
            .expect("duplicate extern must resolve");
        assert_eq!(first.name, duplicate.name);
        assert_eq!(first.provider_identity, duplicate.provider_identity);
        assert_eq!(first.source, source);
        assert_eq!(duplicate.source, source);
    }
    assert_eq!(vm.state.extern_registry.len(), 3);
}

#[test]
fn embedder_load_uses_preconfigured_stdlib_exactly_once() {
    let module = valid_module_with_math_sqrt_extern("vm-load-embedder-stdlib");
    let mut vm = Vm::new();
    vo_stdlib::register_portable_externs(
        vm.extern_registry_mut().expect("configuration phase"),
        &module.externs,
    )
    .expect("preconfigure portable stdlib");

    vm.load_with_embedder_externs(module)
        .expect("embedder registration must not be repeated during load");

    assert!(vm
        .state
        .extern_registry
        .resolved(0)
        .is_some_and(|provider| provider.source == RegisteredExternSource::Builtin));
}

#[test]
fn island_load_reuses_verified_module_and_runtime_facts() {
    let module = malformed_single_instruction_module(
        "vm-family-shared-loaded-module",
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    let mut parent = Vm::new();
    parent.load(module).expect("parent module load");
    let shared = parent.module.as_ref().expect("loaded module").clone();
    let shared_registry = parent.state.extern_registry.clone();

    let mut child = Vm::new();
    child
        .load_inherited_module(parent.inherited_program_image().expect("parent image"))
        .expect("child shared module load");
    let child_module = child.module.as_ref().expect("child loaded module");

    assert!(std::sync::Arc::ptr_eq(&shared, child_module));
    assert!(core::ptr::eq(
        shared.runtime_type_facts(),
        child_module.runtime_type_facts()
    ));
    assert!(std::sync::Arc::ptr_eq(
        &shared_registry,
        &child.state.extern_registry
    ));
    assert_eq!(
        child.state.extern_registry.resolved_externs(),
        parent.state.extern_registry.resolved_externs()
    );
}

#[test]
fn island_load_inherits_embedder_configured_providers() {
    let module = valid_module_with_math_sqrt_extern("vm-family-embedder-externs");
    let mut parent = Vm::new();
    vo_stdlib::register_portable_externs(
        parent
            .extern_registry_mut()
            .expect("parent configuration phase"),
        &module.externs,
    )
    .expect("preconfigure portable stdlib");
    parent
        .load_with_embedder_externs(module)
        .expect("parent embedder load");

    let mut child = Vm::new();
    child
        .load_inherited_module(parent.inherited_program_image().expect("parent image"))
        .expect("child inherited load");

    assert_eq!(
        child.state.extern_registry.resolved_externs(),
        parent.state.extern_registry.resolved_externs()
    );
    assert_eq!(
        child
            .state
            .extern_registry
            .registered_by_name(vo_runtime::vo_extern_name!("math", "Sqrt"))
            .expect("child inherited provider")
            .provider_name(),
        vo_runtime::vo_extern_name!("math", "Sqrt")
    );
}

#[cfg(feature = "std")]
#[test]
fn inherited_provider_executes_with_child_vm_context() {
    INHERITED_PROVIDER_SAW_CHILD_CONTEXT.store(false, std::sync::atomic::Ordering::SeqCst);
    let name = vo_common_core::extern_key::ExternKeyRef::new(
        "github.com/volang/vm-tests",
        "InheritedContextProbe",
    )
    .encode()
    .expect("canonical test extern name");
    let mut module = malformed_single_instruction_module(
        "vm-family-child-extern-context",
        vec![
            Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        Vec::new(),
    );
    module.externs.push(extern_def_for_test(
        &name,
        ParamShape::exact(0),
        ReturnShape::slots(0),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    module.functions[0].instruction_metadata = vec![
        InstructionMetadata::CallExternLayout {
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        },
        InstructionMetadata::None,
    ];
    refresh_vm_test_function_metadata(&mut module.functions[0]);

    let mut parent = Vm::new();
    parent
        .extern_registry_mut()
        .expect("parent configuration phase")
        .try_register_wasm_host_with_effects(
            0,
            &name,
            inherited_context_probe,
            vo_runtime::bytecode::ExternEffects::NONE,
        )
        .expect("register inherited provider");
    parent
        .load_with_embedder_externs(module)
        .expect("parent embedder load");
    parent.set_program_args(vec!["parent".to_string()]);

    let mut child = Vm::new();
    child.set_program_args(vec!["child".to_string()]);
    child
        .load_inherited_module(parent.inherited_program_image().expect("parent image"))
        .expect("child inherited load");

    assert_eq!(
        child.run().expect("child provider execution"),
        SchedulingOutcome::Completed
    );
    assert!(INHERITED_PROVIDER_SAW_CHILD_CONTEXT.load(std::sync::atomic::Ordering::SeqCst));
}

#[cfg(feature = "std")]
#[test]
fn inherited_program_authorities_are_send_sync() {
    fn assert_send_sync<T: Send + Sync>() {}

    assert_send_sync::<vo_runtime::ExternRegistry>();
    assert_send_sync::<vo_runtime::ext_loader::ExtensionLoader>();
    assert_send_sync::<InheritedProgramImage>();
}

#[test]
fn vm_load_rejects_invalid_opcode_before_execution() {
    let module = malformed_single_instruction_module(
        "vm-load-invalid-opcode",
        vec![Instruction {
            op: 254,
            flags: 0,
            a: 0,
            b: 0,
            c: 0,
        }],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["invalid opcode 254"]);
}

#[test]
fn vm_load_rejects_metadata_length_mismatch_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-metadata-length",
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    module.functions[0].instruction_metadata.clear();

    assert_vm_load_rejects(module, &["instruction metadata length mismatch"]);
}

#[test]
fn vm_load_rejects_invalid_module_runtime_type_refs_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-runtime-type-ref",
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 99,
    });

    assert_vm_load_rejects(
        module,
        &["runtime_types[0] Struct references missing struct metadata 99"],
    );
}

#[test]
fn vm_load_rejects_slot_out_of_range_before_execution() {
    let module = malformed_single_instruction_module(
        "vm-load-slot-out-of-range",
        vec![Instruction::new(Opcode::Copy, 0, 7, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["Copy", "slot 7 out of range"]);
}

#[test]
fn vm_load_rejects_map_new_missing_key_rttid_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-map-new-missing-key-rttid",
        vec![
            Instruction::new(Opcode::LoadConst, 3, 0, 0),
            Instruction::new(Opcode::MapNew, 0, 3, 0),
        ],
        Vec::new(),
    );
    let int_meta = ValueMeta::new(0, ValueKind::Int64).to_raw() as i64;
    module
        .constants
        .push(Constant::Int((int_meta << 32) | int_meta));
    let func = &mut module.functions[0];
    func.slot_types[0] = SlotType::GcRef;
    func.slot_types.push(SlotType::Value);
    func.local_slots = func.slot_types.len() as u16;
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
    ];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(
        module,
        &["MapNew key RTTID register r4 is not a constant on every path"],
    );
}

#[test]
fn vm_load_rejects_map_new_metadata_drift_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-map-new-metadata-drift",
        vec![
            Instruction::new(Opcode::LoadConst, 1, 0, 0),
            Instruction::new(Opcode::LoadConst, 2, 1, 0),
            Instruction::new(Opcode::MapNew, 0, 1, 0),
        ],
        Vec::new(),
    );
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let int_rttid = vo_runtime::ValueRttid::new(0, ValueKind::Int64);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.constants = vec![
        Constant::Int(((i64::from(int_meta.to_raw())) << 32) | i64::from(int_meta.to_raw())),
        Constant::Int(i64::from(int_rttid.to_raw())),
    ];
    let func = &mut module.functions[0];
    func.local_slots = 3;
    func.slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::GcRef],
            val_layout: vec![SlotType::Value],
        },
    ];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(
        module,
        &[
            "MapNew",
            "key metadata layout [Value]",
            "instruction metadata [GcRef]",
        ],
    );
}

#[test]
fn vm_load_rejects_queue_new_metadata_drift_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-queue-new-metadata-drift",
        vec![
            Instruction::new(Opcode::LoadConst, 1, 0, 0),
            Instruction::with_flags(Opcode::QueueNew, 0, 0, 1, 2),
        ],
        Vec::new(),
    );
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let int_rttid = vo_runtime::ValueRttid::new(0, ValueKind::Int64);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.constants.push(Constant::Int(
        (i64::from(int_rttid.to_raw()) << 32) | i64::from(int_meta.to_raw()),
    ));
    let func = &mut module.functions[0];
    func.local_slots = 3;
    func.slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::GcRef],
        },
    ];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(
        module,
        &[
            "QueueNew",
            "element metadata layout [Value]",
            "instruction metadata [GcRef]",
        ],
    );
}

#[test]
fn vm_load_rejects_invalid_branch_target_before_execution() {
    let module = malformed_single_instruction_module(
        "vm-load-invalid-branch-target",
        vec![Instruction::new(Opcode::Jump, 0, 4, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["branch target", "Jump", "outside code length"]);
}

#[test]
fn vm_load_rejects_call_extern_layout_mismatch_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-call-extern-layout",
        vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 1)],
        Vec::new(),
    );
    module.externs.push(extern_def_for_test(
        "VmLoadTestExtern",
        ParamShape::Exact { slots: 1 },
        ReturnShape::with_slot_types(vec![SlotType::Value]),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let func = &mut module.functions[0];
    func.instruction_metadata = vec![InstructionMetadata::CallExternLayout {
        arg_layout: vec![SlotType::GcRef],
        ret_layout: vec![SlotType::Value],
    }];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(module, &["CallExtern", "CallExtern args"]);
}

#[test]
fn vm_load_rejects_raw_value_collection_into_gcref_slot_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-raw-value-array-get-into-gcref",
        vec![Instruction::with_flags(Opcode::ArrayGet, 0, 1, 0, 2)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.slot_types = vec![
        SlotType::GcRef,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
    ];
    func.instruction_metadata = vec![InstructionMetadata::ElemLayout {
        elem_bytes: 8,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::Value],
    }];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(
        module,
        &["ArrayGet destination", "expected [Value]", "actual [GcRef]"],
    );
}

#[test]
fn malformed_load_const_index_is_vm_error_instead_of_index_panic() {
    let module = malformed_single_instruction_module(
        "malformed-load-const",
        vec![Instruction::new(Opcode::LoadConst, 0, 0, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["missing constant 0"]);
}

#[test]
fn malformed_str_new_missing_constant_is_vm_error_instead_of_index_panic() {
    let module = malformed_single_instruction_module(
        "malformed-str-new-missing",
        vec![Instruction::new(Opcode::StrNew, 0, 0, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["missing constant 0"]);
}

#[test]
fn malformed_str_new_non_string_constant_is_vm_error_instead_of_nil_fill() {
    let module = malformed_single_instruction_module(
        "malformed-str-new-non-string",
        vec![Instruction::new(Opcode::StrNew, 0, 0, 0)],
        vec![Constant::Int(7)],
    );

    assert_vm_load_rejects(
        module,
        &[
            "constant kind mismatch",
            "StrNew",
            "expected String",
            "actual Int",
        ],
    );
}

#[test]
fn malformed_pc_fallthrough_is_rejected_before_execution() {
    let module = malformed_single_instruction_module(
        "malformed-pc-fallthrough",
        vec![Instruction::new(Opcode::Hint, 0, 0, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["final Hint instruction", "falls through"]);
}

#[test]
fn malformed_global_index_is_vm_error_instead_of_index_panic() {
    let module = malformed_single_instruction_module(
        "malformed-global-get",
        vec![Instruction::new(Opcode::GlobalGet, 0, 0, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["global slot 0 out of range"]);
}

#[test]
fn malformed_go_start_function_id_is_vm_error_instead_of_index_panic() {
    let module = malformed_single_instruction_module(
        "malformed-go-start",
        vec![Instruction::with_flags(Opcode::GoStart, 0, 7, 0, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["missing function 7"]);
}

#[test]
fn malformed_go_start_closure_target_is_vm_error_instead_of_nil_call_trap() {
    let mut module = malformed_single_instruction_module(
        "malformed-go-start-closure",
        vec![
            Instruction::with_flags(Opcode::GoStart, 1, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.slot_types[0] = SlotType::GcRef;
    func.instruction_metadata = vec![
        InstructionMetadata::CallLayout {
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        },
        InstructionMetadata::None,
    ];
    refresh_vm_test_function_metadata(func);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let closure_ref = vo_runtime::objects::closure::create(&mut vm.state.gc, 7, 0);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 4, 0, 0);
        fiber.stack[0] = closure_ref as u64;
    }

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run_scheduled()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(
                msg.contains("Go closure spawn missing function id 7"),
                "{msg}"
            );
        }
        Ok(other) => panic!("malformed closure GoStart should be a VM error, got {other:?}"),
        Err(_) => panic!("malformed closure GoStart must not panic"),
    }
}
