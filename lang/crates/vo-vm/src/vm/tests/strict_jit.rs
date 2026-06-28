use super::*;

#[cfg(feature = "jit")]
fn strict_only_wrong_metadata_kind_module() -> Module {
    let mut module = Module::new("strict-jit-metadata-policy-test".to_string());
    module.functions.push(FunctionDef {
        name: "main".to_string(),
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
        code: vec![Instruction::new(Opcode::Hint, 0, 0, 0)],
        jit_metadata: vec![JitInstructionMetadata::LoopEnd { end_pc: 0 }],
        slot_types: Vec::new(),
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&[]),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    module
}

#[cfg(feature = "jit")]
fn invalid_jit_return_flags_module() -> Module {
    let slot_types = vec![SlotType::Value];
    let mut module = Module::new("strict-jit-return-flags-test".to_string());
    module.functions.push(FunctionDef {
        name: "main".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 1,
        gc_scan_slots: 0,
        ret_slots: 1,
        ret_slot_types: vec![SlotType::Value],
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        code: vec![Instruction::with_flags(Opcode::Return, 0x04, 0, 1, 0)],
        jit_metadata: vec![vo_runtime::bytecode::JitInstructionMetadata::None],
        slot_types: slot_types.clone(),
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    module
}

#[cfg(feature = "jit")]
fn valid_empty_return_module() -> Module {
    let slot_types = Vec::new();
    let mut module = Module::new("strict-jit-valid-load-test".to_string());
    module.functions.push(FunctionDef {
        name: "main".to_string(),
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
        code: vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        jit_metadata: vec![vo_runtime::bytecode::JitInstructionMetadata::None],
        slot_types: slot_types.clone(),
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    module
}

#[cfg(feature = "jit")]
#[test]
fn strict_jit_load_rejects_invalid_metadata_before_interpreter_dispatch() {
    let module = strict_only_wrong_metadata_kind_module();

    let mut vm = Vm::try_with_jit_config(JitConfig {
        call_threshold: 1_000_000,
        loop_threshold: 1_000_000,
        debug_ir: false,
    })
    .expect("strict JIT VM");

    match vm.load(module) {
        Err(VmError::Jit(msg)) => {
            assert!(msg.contains("invalid JIT metadata"), "{msg}");
            assert!(msg.contains("wrong JIT metadata kind LoopEnd"), "{msg}");
        }
        other => panic!("strict JIT load should fail fast, got {other:?}"),
    }
}

#[cfg(feature = "jit")]
#[test]
fn strict_jit_load_rejects_return_unknown_flags_before_interpreter_dispatch() {
    let module = invalid_jit_return_flags_module();

    let mut vm = Vm::try_with_jit_config(JitConfig {
        call_threshold: 1_000_000,
        loop_threshold: 1_000_000,
        debug_ir: false,
    })
    .expect("strict JIT VM");

    match vm.load(module) {
        Err(VmError::Jit(msg)) => {
            assert!(msg.contains("invalid module metadata"), "{msg}");
            assert!(msg.contains("invalid flags 0x04 for Return"), "{msg}");
        }
        other => panic!("VM load should reject Return unknown flags, got {other:?}"),
    }
}

#[cfg(feature = "jit")]
#[test]
fn best_effort_jit_config_skips_strict_jit_only_load_policy() {
    let module = strict_only_wrong_metadata_kind_module();
    let mut vm = Vm::with_best_effort_jit_config(JitConfig {
        call_threshold: 1_000_000,
        loop_threshold: 1_000_000,
        debug_ir: false,
    });

    assert!(
        vm.load(module).is_ok(),
        "best-effort JIT load must skip strict JIT-only metadata policy"
    );
}

#[cfg(feature = "jit")]
#[test]
fn best_effort_init_jit_after_load_initializes_without_strict_metadata_validation() {
    let module = strict_only_wrong_metadata_kind_module();
    let mut vm = Vm::new();
    vm.load(module)
        .expect("VM load accepts bytecode that only violates strict JIT metadata policy");

    vm.init_jit_best_effort();

    assert_eq!(
        vm.jit.manager().map(|mgr| mgr.func_table_len()),
        Some(1),
        "explicit best-effort JIT init skips strict JIT-only metadata policy"
    );
}

#[cfg(feature = "jit")]
#[test]
fn strict_try_init_jit_after_load_rejects_invalid_metadata() {
    let module = strict_only_wrong_metadata_kind_module();
    let mut vm = Vm::new();
    vm.load(module)
        .expect("VM load accepts bytecode that only violates strict JIT metadata policy");

    let err = vm
        .try_init_jit()
        .expect_err("strict post-load JIT init must validate loaded module");
    assert!(
        err.to_string().contains("invalid JIT metadata"),
        "unexpected strict post-load init error: {err}"
    );
    assert!(
        err.to_string().contains("wrong JIT metadata kind LoopEnd"),
        "unexpected strict post-load init error: {err}"
    );
}

#[cfg(feature = "jit")]
#[test]
fn strict_try_init_jit_after_load_initializes_loaded_module_tables() {
    let module = valid_empty_return_module();
    let mut vm = Vm::new();
    vm.load(module).expect("valid module load");

    vm.try_init_jit().expect("strict post-load JIT init");

    assert_eq!(
        vm.jit.manager().map(|mgr| mgr.func_table_len()),
        Some(1),
        "post-load strict JIT init must size dispatch tables for the loaded module"
    );
}

#[cfg(feature = "jit")]
#[test]
fn strict_jit_load_rejects_missing_static_call_target_before_dispatch() {
    let mut module = Module::new("missing-call-target-test".to_string());
    module.functions.push(FunctionDef {
        name: "main".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 1,
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
        has_calls: true,
        has_call_extern: false,
        code: vec![Instruction::with_flags(Opcode::Call, 0, 7, 0, 0)],
        jit_metadata: vec![vo_runtime::bytecode::JitInstructionMetadata::None],
        slot_types: vec![SlotType::Value],
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&[
            SlotType::Value,
        ]),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });

    let mut vm = Vm::try_with_jit_config(JitConfig {
        call_threshold: 1,
        loop_threshold: 1,
        debug_ir: false,
    })
    .expect("strict JIT VM");
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.load(module)));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(msg.contains("invalid module metadata"), "{msg}");
            assert!(msg.contains("missing function 7"), "{msg}");
        }
        Ok(other) => panic!("missing call target should be rejected at load, got {other:?}"),
        Err(_) => panic!("missing call target must not panic during strict JIT load"),
    }
}
