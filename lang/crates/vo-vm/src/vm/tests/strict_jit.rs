use super::*;

#[cfg(feature = "jit")]
fn wrong_metadata_kind_module() -> Module {
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
        code: vec![
            Instruction::new(Opcode::Hint, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        instruction_metadata: vec![
            InstructionMetadata::LoopEnd { end_pc: 0 },
            InstructionMetadata::None,
        ],
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
        instruction_metadata: vec![vo_runtime::bytecode::InstructionMetadata::None],
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
        instruction_metadata: vec![vo_runtime::bytecode::InstructionMetadata::None],
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
    let module = wrong_metadata_kind_module();

    let mut vm = Vm::try_with_jit_config(JitConfig {
        call_threshold: 1_000_000,
        loop_threshold: 1_000_000,
        debug_ir: false,
        ..JitConfig::default()
    })
    .expect("strict JIT VM");

    match vm.load(module) {
        Err(VmError::Jit(msg)) => {
            assert!(msg.contains("invalid module metadata"), "{msg}");
            assert!(
                msg.contains("wrong instruction metadata kind LoopEnd"),
                "{msg}"
            );
        }
        other => panic!("strict JIT load should fail fast, got {other:?}"),
    }
}

#[cfg(feature = "jit")]
#[test]
fn strict_jit_failed_load_can_retry_with_a_valid_module() {
    let mut vm = Vm::try_with_jit_config(JitConfig {
        call_threshold: 1_000_000,
        loop_threshold: 1_000_000,
        debug_ir: false,
        ..JitConfig::default()
    })
    .expect("strict JIT VM");

    vm.load(wrong_metadata_kind_module())
        .expect_err("common metadata validation must fail");
    vm.load(valid_empty_return_module())
        .expect("failed validation must leave strict JIT module binding reusable");
}

#[cfg(feature = "jit")]
#[test]
fn strict_jit_load_rejects_return_unknown_flags_before_interpreter_dispatch() {
    let module = invalid_jit_return_flags_module();

    let mut vm = Vm::try_with_jit_config(JitConfig {
        call_threshold: 1_000_000,
        loop_threshold: 1_000_000,
        debug_ir: false,
        ..JitConfig::default()
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
fn best_effort_jit_also_obeys_common_metadata_authority() {
    let module = wrong_metadata_kind_module();
    let mut vm = Vm::with_best_effort_jit_config(JitConfig {
        call_threshold: 1_000_000,
        loop_threshold: 1_000_000,
        debug_ir: false,
        ..JitConfig::default()
    });

    let error = vm
        .load(module)
        .expect_err("best-effort mode cannot bypass common bytecode validity");
    assert!(matches!(
        error,
        VmError::Jit(message) if message.contains("wrong instruction metadata kind LoopEnd")
    ));
}

#[cfg(feature = "jit")]
#[test]
fn best_effort_code_memory_limit_falls_back_once_and_remains_observable() {
    let mut vm = Vm::with_best_effort_jit_config(JitConfig {
        call_threshold: 1,
        loop_threshold: 1_000_000,
        code_memory_limit_bytes: 0,
        ..JitConfig::default()
    });

    vm.load(valid_empty_return_module())
        .expect("best-effort module load");
    assert_eq!(
        vm.run().expect("code-budget interpreter fallback"),
        SchedulingOutcome::Completed
    );
    assert!(vm
        .jit_function_compile_error(0)
        .is_some_and(|error| error.contains("JIT code memory limit exceeded")));
    assert_eq!(vm.jit_unsupported_function_count(), 0);
    assert_eq!(vm.jit_resource_rejected_function_count(), 1);
    assert_eq!(
        vm.jit_function_failure_kind(0),
        Some(vo_jit::JitFailureKind::ResourceRejected)
    );
    let stats = vm.jit_code_memory_stats();
    assert_eq!(stats.limit_bytes, 0);
    assert_eq!(stats.total_bytes(), 0);
    assert_eq!(stats.function_count, 0);
    assert_eq!(stats.rejected_artifact_count, 1);
}

#[cfg(feature = "jit")]
#[test]
fn best_effort_analysis_memory_limit_falls_back_once_and_remains_observable() {
    let mut vm = Vm::with_best_effort_jit_config(JitConfig {
        call_threshold: 1,
        loop_threshold: 1_000_000,
        analysis_memory_limit_bytes: 0,
        ..JitConfig::default()
    });

    vm.load(valid_empty_return_module())
        .expect("best-effort module load");
    assert_eq!(
        vm.run().expect("analysis-budget interpreter fallback"),
        SchedulingOutcome::Completed
    );
    assert!(vm
        .jit_function_compile_error(0)
        .is_some_and(|error| error.contains("JIT analysis resource limit exceeded")));
    assert_eq!(vm.jit_unsupported_function_count(), 0);
    assert_eq!(vm.jit_resource_rejected_function_count(), 1);
    assert_eq!(
        vm.jit_function_failure_kind(0),
        Some(vo_jit::JitFailureKind::ResourceRejected)
    );
    assert_eq!(
        vm.jit_analysis_memory_stats(),
        vo_jit::JitAnalysisMemoryStats {
            analysis_count: 0,
            retained_bytes: 0,
            limit_bytes: 0,
            rejected_analysis_count: 1,
            eviction_count: 0,
        }
    );
}

#[cfg(feature = "jit")]
#[test]
fn strict_code_memory_limit_reports_a_jit_error_without_publishing_code() {
    let mut vm = Vm::try_with_jit_config(JitConfig {
        call_threshold: 1,
        loop_threshold: 1_000_000,
        code_memory_limit_bytes: 0,
        ..JitConfig::default()
    })
    .expect("strict JIT VM");

    vm.load(valid_empty_return_module())
        .expect("strict valid module load");
    let error = vm.run().expect_err("strict code budget must fail closed");
    assert!(matches!(
        error,
        VmError::Jit(message) if message.contains("JIT code memory limit exceeded")
    ));
    let stats = vm.jit_code_memory_stats();
    assert_eq!(stats.total_bytes(), 0);
    assert_eq!(stats.function_count, 0);
    assert_eq!(stats.rejected_artifact_count, 1);
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
        instruction_metadata: vec![vo_runtime::bytecode::InstructionMetadata::None],
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
        ..JitConfig::default()
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
