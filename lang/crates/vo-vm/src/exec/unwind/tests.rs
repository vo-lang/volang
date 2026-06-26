use super::*;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

fn func_with_slot_types(slot_types: Vec<SlotType>) -> FunctionDef {
    FunctionDef {
        name: "ret_meta".to_string(),
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
        slot_types,
        borrowed_scan_slots_prefix: Vec::new(),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}

#[test]
fn vm_defer_static_frame_shape_062_rejects_scan_slots_beyond_locals_before_stack_mutation() {
    let mut gc = Gc::new();
    let mut module = Module::new("defer-frame-shape-test".to_string());
    module
        .functions
        .push(func_with_slot_types(vec![SlotType::Value]));
    let mut malformed = func_with_slot_types(vec![SlotType::Value]);
    malformed.gc_scan_slots = 2;
    module.functions.push(malformed);

    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let entry = DeferEntry {
        frame_depth: fiber.frames.len(),
        func_id: 1,
        closure: core::ptr::null_mut(),
        args: core::ptr::null_mut(),
        arg_layout: crate::fiber::DeferArgLayout {
            slot_types: Vec::new(),
        },
        is_closure: false,
        is_errdefer: false,
        registered_at_generation: 0,
    };
    let before_frame_count = fiber.frames.len();
    let before_sp = fiber.sp;

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        call_defer_entry(&mut gc, &mut fiber, &entry, &module)
    }));

    match result {
        Ok(ExecResult::JitError(msg)) => {
            assert!(
                msg.contains("static defer invalid target frame shape"),
                "{msg}"
            );
        }
        Ok(other) => panic!("malformed defer frame shape should be JitError, got {other:?}"),
        Err(_) => panic!("malformed defer frame shape must not panic during stack zeroing"),
    }
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn stack_return_slot_metadata_missing_fails_fast() {
    let func = func_with_slot_types(vec![SlotType::Value]);
    match try_require_slot_types(&func, 3, 9, 1, 2, "test return") {
        Err(ExecResult::JitError(msg)) => {
            assert!(msg.contains("func_id=3"));
            assert!(msg.contains("pc=9"));
            assert!(msg.contains("slot range 1..3"));
            assert!(msg.contains("actual slot_types=1"));
        }
        other => panic!("missing return metadata should return JitError, got {other:?}"),
    }
}

#[test]
fn heap_return_slot_count_mismatch_fails_fast() {
    let mut func = func_with_slot_types(Vec::new());
    func.heap_ret_slots = vec![1];
    match try_require_heap_ret_slots(&func, 4, 10, 2, "test heap return") {
        Err(ExecResult::JitError(msg)) => {
            assert!(msg.contains("func_id=4"));
            assert!(msg.contains("pc=10"));
            assert!(msg.contains("expected heap_ret_slots=2 actual=1"));
        }
        other => panic!("missing heap return metadata should return JitError, got {other:?}"),
    }
}

#[test]
fn vm_closure_replay_heap_return_metadata_059_uses_return_layout_not_partition_offsets() {
    let mut func = func_with_slot_types(vec![SlotType::GcRef]);
    func.ret_slots = 3;
    func.ret_slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    func.heap_ret_gcref_count = 1;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![3];

    let slot_types = try_heap_return_slot_types(&func, 9, 17, &[3], "closure replay heap return")
        .expect("heap return slot metadata");

    assert_eq!(slot_types, func.ret_slot_types);
}

#[test]
fn heap_return_reader_rejects_missing_slot_counts() {
    let gc = Gc::new();
    match try_read_heap_gcrefs(&gc, &[0x1234], &[], 4, 10, "test heap return") {
        Err(ExecResult::JitError(msg)) => {
            assert!(msg.contains("heap return metadata count mismatch"));
            assert!(msg.contains("gcrefs=1"));
            assert!(msg.contains("slots_per_ref=0"));
        }
        other => panic!("heap return reader should return JitError, got {other:?}"),
    }
}

#[test]
fn vm_ver_002_heap_return_reader_rejects_invalid_gcref_before_deref() {
    let gc = Gc::new();
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        try_read_heap_gcrefs(&gc, &[0x1234], &[1], 4, 10, "VM-VER-002 test")
    }));

    match result {
        Ok(Err(ExecResult::JitError(msg))) => {
            assert!(msg.contains("heap return GcRef is invalid"), "{msg}");
            assert!(msg.contains("func_id=4"), "{msg}");
            assert!(msg.contains("pc=10"), "{msg}");
            assert!(msg.contains("raw=0x0000000000001234"), "{msg}");
        }
        Ok(other) => panic!("invalid heap return GcRef should be JitError, got {other:?}"),
        Err(_) => panic!("invalid heap return GcRef must not be dereferenced"),
    }
}

#[test]
fn vm_heap_return_reader_rejects_short_allocation_before_deref_059() {
    let mut gc = Gc::new();
    let short_ref = gc.alloc(
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Struct),
        1,
    );

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        try_read_heap_gcrefs(
            &gc,
            &[short_ref as u64],
            &[2],
            4,
            10,
            "heap return short allocation test",
        )
    }));

    match result {
        Ok(Err(ExecResult::JitError(msg))) => {
            assert!(msg.contains("heap return allocation too small"), "{msg}");
            assert!(msg.contains("required_slots=2"), "{msg}");
            assert!(msg.contains("actual_bytes=8"), "{msg}");
        }
        Ok(other) => panic!("short heap return allocation should be JitError, got {other:?}"),
        Err(_) => panic!("short heap return allocation must not be dereferenced past bounds"),
    }
}

#[test]
fn vm_errdefer_heap_return_check_rejects_short_error_allocation_before_deref_059() {
    let mut gc = Gc::new();
    let module = Module::new("errdefer-heap-return-short-error".to_string());
    let mut func = func_with_slot_types(vec![SlotType::GcRef]);
    func.ret_slots = 2;
    func.ret_slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    func.heap_ret_gcref_count = 1;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![2];
    func.error_ret_slot = 0;
    func.has_defer = true;

    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    let short_ref = gc.alloc(
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Struct),
        1,
    );
    fiber.stack[0] = short_ref as u64;
    fiber.defer_stack.push(DeferEntry {
        frame_depth: fiber.frames.len(),
        func_id: 0,
        closure: core::ptr::null_mut(),
        args: core::ptr::null_mut(),
        arg_layout: crate::fiber::DeferArgLayout {
            slot_types: Vec::new(),
        },
        is_closure: false,
        is_errdefer: true,
        registered_at_generation: 0,
    });

    let inst = Instruction::with_flags(Opcode::Return, ReturnFlags::HEAP_RETURNS.bits(), 0, 1, 0);
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        handle_return(&mut gc, &mut fiber, &inst, &func, &module, false)
    }));

    match result {
        Ok(ExecResult::JitError(msg)) => {
            assert!(msg.contains("errdefer heap return check"), "{msg}");
            assert!(msg.contains("heap return allocation too small"), "{msg}");
            assert!(msg.contains("required_slots=2"), "{msg}");
        }
        Ok(other) => {
            panic!("short errdefer heap error allocation should be JitError, got {other:?}")
        }
        Err(_) => panic!("short errdefer heap error allocation must not be dereferenced"),
    }
}

#[test]
fn vm_ver_002_fast_heap_return_rejects_nil_gcref_before_deref() {
    let mut func = func_with_slot_types(vec![SlotType::GcRef]);
    func.heap_ret_gcref_count = 1;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![1];

    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 1, 0, 0);
    fiber.stack[0] = 0;
    let inst = Instruction::new(Opcode::Return, 0, 1, 0);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let gc = Gc::new();
        fast_complete_heap_return(&gc, &mut fiber, &func, &inst)
    }));

    match result {
        Ok(ExecResult::JitError(msg)) => {
            assert!(msg.contains("fast heap return"), "{msg}");
            assert!(
                msg.contains("heap return GcRef is nil or uninitialized"),
                "{msg}"
            );
        }
        Ok(other) => panic!("nil heap return GcRef should be JitError, got {other:?}"),
        Err(_) => panic!("nil heap return GcRef must not be dereferenced"),
    }
}

#[test]
fn jit_ok_return_missing_stack_metadata_is_jit_error_instead_of_panic() {
    let mut gc = Gc::new();
    let module = Module::new("jit-return-metadata-test".to_string());
    let mut func = func_with_slot_types(vec![SlotType::Value]);
    func.ret_slots = 2;

    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 0, 0, 2, 1);
    fiber.defer_stack.push(DeferEntry {
        frame_depth: fiber.frames.len(),
        func_id: 0,
        closure: core::ptr::null_mut(),
        args: core::ptr::null_mut(),
        arg_layout: crate::fiber::DeferArgLayout {
            slot_types: Vec::new(),
        },
        is_closure: false,
        is_errdefer: false,
        registered_at_generation: 0,
    });

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        handle_jit_ok_return(
            &mut gc,
            &mut fiber,
            &func,
            &module,
            &[10, 20],
            false,
            0,
            0,
            false,
        )
    }));

    match result {
        Ok(ExecResult::JitError(msg)) => {
            assert!(msg.contains("JIT stack return"));
            assert!(msg.contains("slot range 0..2"));
        }
        Ok(other) => panic!("missing return metadata should be JitError, got {other:?}"),
        Err(_) => panic!("missing return metadata must not panic across the JIT bridge"),
    }
}

#[test]
fn fast_heap_return_missing_slot_counts_is_jit_error_instead_of_panic() {
    let mut func = func_with_slot_types(vec![SlotType::GcRef]);
    func.heap_ret_gcref_count = 1;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = Vec::new();

    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 1, 0, 0);
    fiber.stack[0] = 0;
    let inst = Instruction::new(Opcode::Return, 0, 1, 0);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let gc = Gc::new();
        fast_complete_heap_return(&gc, &mut fiber, &func, &inst)
    }));

    match result {
        Ok(ExecResult::JitError(msg)) => {
            assert!(msg.contains("fast heap return"), "{msg}");
            assert!(msg.contains("expected heap_ret_slots=1 actual=0"), "{msg}");
        }
        Ok(other) => {
            panic!("missing fast heap return metadata should be JitError, got {other:?}")
        }
        Err(_) => panic!("missing fast heap return metadata must not panic"),
    }
}

#[test]
fn jit_ok_return_at_closure_replay_boundary_caches_results() {
    let mut gc = Gc::new();
    let module = Module::new("jit-closure-replay-return-test".to_string());
    let mut func = func_with_slot_types(vec![
        SlotType::Value,
        SlotType::Interface0,
        SlotType::Interface1,
    ]);
    func.ret_slots = 3;
    let mut fiber = Fiber::new(0);
    fiber.push_frame(7, 3, 0, 0, 3);
    fiber.closure_replay.push_depth(fiber.frames.len());

    let result = handle_jit_ok_return(
        &mut gc,
        &mut fiber,
        &func,
        &module,
        &[42, 0, 0],
        false,
        0,
        0,
        false,
    );

    assert!(matches!(result, ExecResult::FrameChanged));
    assert!(fiber.frames.is_empty());
    assert_eq!(fiber.closure_replay.depth, 0);
    assert_eq!(fiber.closure_replay.results.len(), 1);
    assert_eq!(fiber.closure_replay.results[0].0, vec![42, 0, 0]);
    assert_eq!(
        fiber.closure_replay.results[0].1,
        vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1]
    );
}

#[test]
fn jit_closure_replay_return_skips_errdefer_without_panicking() {
    let mut gc = Gc::new();
    let module = Module::new("jit-closure-replay-errdefer-test".to_string());
    let mut func = func_with_slot_types(vec![SlotType::Value]);
    func.ret_slots = 1;
    let mut fiber = Fiber::new(0);
    fiber.push_frame(7, 1, 0, 0, 1);
    fiber.closure_replay.push_depth(fiber.frames.len());
    fiber.defer_stack.push(DeferEntry {
        frame_depth: fiber.frames.len(),
        func_id: 0,
        closure: core::ptr::null_mut(),
        args: core::ptr::null_mut(),
        arg_layout: crate::fiber::DeferArgLayout {
            slot_types: Vec::new(),
        },
        is_closure: false,
        is_errdefer: true,
        registered_at_generation: 0,
    });

    let result = handle_jit_ok_return(
        &mut gc,
        &mut fiber,
        &func,
        &module,
        &[99],
        false,
        0,
        0,
        false,
    );

    assert!(matches!(result, ExecResult::FrameChanged));
    assert!(fiber.frames.is_empty());
    assert!(fiber.defer_stack.is_empty());
    assert!(fiber.unwinding.is_none());
    assert_eq!(fiber.closure_replay.results[0].0, vec![99]);
}

#[test]
fn interpreter_closure_replay_return_skips_errdefer_without_panicking() {
    let mut gc = Gc::new();
    let module = Module::new("interp-closure-replay-errdefer-test".to_string());
    let mut func = func_with_slot_types(vec![SlotType::Value]);
    func.ret_slots = 1;
    let inst = Instruction::new(Opcode::Return, 0, 1, 0);
    let mut fiber = Fiber::new(0);
    fiber.push_frame(7, 1, 0, 0, 1);
    fiber.stack[0] = 123;
    fiber.closure_replay.push_depth(fiber.frames.len());
    fiber.defer_stack.push(DeferEntry {
        frame_depth: fiber.frames.len(),
        func_id: 0,
        closure: core::ptr::null_mut(),
        args: core::ptr::null_mut(),
        arg_layout: crate::fiber::DeferArgLayout {
            slot_types: Vec::new(),
        },
        is_closure: false,
        is_errdefer: true,
        registered_at_generation: 0,
    });

    let result = handle_return(&mut gc, &mut fiber, &inst, &func, &module, false);

    assert!(matches!(result, ExecResult::FrameChanged));
    assert!(fiber.frames.is_empty());
    assert!(fiber.defer_stack.is_empty());
    assert!(fiber.unwinding.is_none());
    assert_eq!(fiber.closure_replay.results[0].0, vec![123]);
}

#[test]
fn closure_replay_defer_completion_appends_final_return_values() {
    let mut gc = Gc::new();
    let mut module = Module::new("closure-replay-defer-final-test".to_string());
    let mut func = func_with_slot_types(vec![SlotType::Value]);
    func.ret_slots = 1;
    module.functions.push(func);

    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    fiber.closure_replay.push_depth(fiber.frames.len());
    fiber.unwinding = Some(UnwindingState {
        pending: Vec::new(),
        target_depth: 0,
        mode: UnwindingMode::Return,
        current_defer_generation: 0,
        return_values: Some(ReturnValues::Stack {
            vals: vec![2],
            slot_types: vec![SlotType::Value],
        }),
        return_func_id: 0,
        return_pc: 0,
        caller_ret_reg: 0,
        caller_ret_count: 1,
        is_closure_replay: true,
    });

    let result = handle_return_defer_returned(&mut gc, &mut fiber, &module, false);

    assert!(matches!(result, ExecResult::FrameChanged));
    assert!(fiber.unwinding.is_none());
    assert_eq!(fiber.closure_replay.depth, 0);
    assert_eq!(fiber.closure_replay.results.len(), 1);
    assert_eq!(fiber.closure_replay.results[0].0, vec![2]);
}

#[test]
fn recovered_closure_replay_panic_finalizes_through_replay_return() {
    let mut gc = Gc::new();
    let module = Module::new("closure-replay-recover-final-test".to_string());
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    fiber.closure_replay.push_depth(fiber.frames.len());
    fiber.unwinding = Some(UnwindingState {
        pending: Vec::new(),
        target_depth: 0,
        mode: UnwindingMode::Panic,
        current_defer_generation: 0,
        return_values: Some(ReturnValues::Stack {
            vals: vec![55],
            slot_types: vec![SlotType::Value],
        }),
        return_func_id: 0,
        return_pc: 0,
        caller_ret_reg: 0,
        caller_ret_count: 1,
        is_closure_replay: true,
    });
    fiber.panic_state = None;

    let result = handle_panic_defer_returned(&mut gc, &mut fiber, &module);

    assert!(matches!(result, ExecResult::FrameChanged));
    assert!(fiber.frames.is_empty());
    assert!(fiber.unwinding.is_none());
    assert_eq!(fiber.closure_replay.depth, 0);
    assert_eq!(fiber.closure_replay.results.len(), 1);
    assert_eq!(fiber.closure_replay.results[0].0, vec![55]);
}

#[test]
fn unrecovered_closure_replay_panic_intercepts_after_defers_finish() {
    let mut gc = Gc::new();
    let module = Module::new("closure-replay-unrecovered-final-test".to_string());
    let mut fiber = Fiber::new(0);
    fiber.push_frame(99, 1, 0, 0, 0);
    fiber.closure_replay.push_depth(2);
    fiber.push_frame(0, 1, 0, 0, 0);
    fiber.unwinding = Some(UnwindingState {
        pending: Vec::new(),
        target_depth: 1,
        mode: UnwindingMode::Panic,
        current_defer_generation: 0,
        return_values: None,
        return_func_id: 0,
        return_pc: 0,
        caller_ret_reg: 0,
        caller_ret_count: 0,
        is_closure_replay: true,
    });
    fiber.panic_state = Some(PanicState::Recoverable(vo_runtime::InterfaceSlot::nil()));

    let result = handle_panic_defer_returned(&mut gc, &mut fiber, &module);

    assert!(matches!(result, ExecResult::FrameChanged));
    assert_eq!(fiber.frames.len(), 1);
    assert!(fiber.unwinding.is_none());
    assert_eq!(fiber.closure_replay.depth, 0);
    assert_eq!(fiber.closure_replay.panic_message.as_deref(), Some("panic"));
    assert!(fiber.panic_state.is_none());
}

#[test]
fn defer_call_missing_function_is_jit_error_instead_of_index_panic() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let module = Module::new("missing-defer-target-test".to_string());
    let entry = DeferEntry {
        frame_depth: 1,
        func_id: 9,
        closure: core::ptr::null_mut(),
        args: core::ptr::null_mut(),
        arg_layout: crate::fiber::DeferArgLayout {
            slot_types: Vec::new(),
        },
        is_closure: false,
        is_errdefer: false,
        registered_at_generation: 0,
    };

    let result = call_defer_entry(&mut gc, &mut fiber, &entry, &module);

    assert!(matches!(
        result,
        ExecResult::JitError(msg)
            if msg.contains("defer target function 9 out of bounds")
    ));
}

#[test]
fn vm_defer_closure_kind_002_rejects_non_closure_gcref_before_header_read() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let module = Module::new("non-closure-defer-target-test".to_string());
    let string_ref =
        vo_runtime::objects::string::new_from_string(&mut gc, "not a closure".to_string());
    let entry = DeferEntry {
        frame_depth: 1,
        func_id: 0,
        closure: string_ref,
        args: core::ptr::null_mut(),
        arg_layout: crate::fiber::DeferArgLayout {
            slot_types: Vec::new(),
        },
        is_closure: true,
        is_errdefer: false,
        registered_at_generation: 0,
    };

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        call_defer_entry(&mut gc, &mut fiber, &entry, &module)
    }));

    match result {
        Ok(ExecResult::JitError(msg)) => {
            assert!(
                msg.contains("defer closure requested non-closure object kind"),
                "{msg}"
            );
        }
        Ok(other) => panic!("non-closure defer target should be JitError, got {other:?}"),
        Err(_) => panic!("non-closure defer target must not decode a closure header"),
    }
}

#[test]
fn vm_defer_closure_shape_002_rejects_arg_slot_shape_drift_before_frame_push() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let closure_ref = vo_runtime::objects::closure::create(&mut gc, 0, 0);
    let mut func = func_with_slot_types(vec![SlotType::Value]);
    func.name = "deferred_target".to_string();
    func.param_count = 1;
    func.param_slots = 1;
    let mut module = Module::new("defer-shape-test".to_string());
    module.functions.push(func);
    let entry = DeferEntry {
        frame_depth: 1,
        func_id: 0,
        closure: closure_ref,
        args: core::ptr::null_mut(),
        arg_layout: crate::fiber::DeferArgLayout {
            slot_types: Vec::new(),
        },
        is_closure: true,
        is_errdefer: false,
        registered_at_generation: 0,
    };

    let result = call_defer_entry(&mut gc, &mut fiber, &entry, &module);

    match result {
        ExecResult::JitError(msg) => {
            assert!(
                msg.contains("defer closure arg slot count 0 does not match target 1"),
                "{msg}"
            );
        }
        other => panic!("defer closure shape drift should be JitError, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 0);
}

#[test]
fn vm_defer_static_shape_002_rejects_arg_slot_shape_drift_before_frame_push() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let mut func = func_with_slot_types(vec![SlotType::Value]);
    func.name = "static_deferred_target".to_string();
    func.param_count = 1;
    func.param_slots = 1;
    let mut module = Module::new("static-defer-shape-test".to_string());
    module.functions.push(func);
    let entry = DeferEntry {
        frame_depth: 1,
        func_id: 0,
        closure: core::ptr::null_mut(),
        args: core::ptr::null_mut(),
        arg_layout: crate::fiber::DeferArgLayout {
            slot_types: Vec::new(),
        },
        is_closure: false,
        is_errdefer: false,
        registered_at_generation: 0,
    };

    let result = call_defer_entry(&mut gc, &mut fiber, &entry, &module);

    match result {
        ExecResult::JitError(msg) => {
            assert!(
                msg.contains("static defer arg slot count 0 does not match target 1"),
                "{msg}"
            );
        }
        other => panic!("static defer shape drift should be JitError, got {other:?}"),
    }
    assert_eq!(fiber.frames.len(), 0);
}

#[test]
fn execute_next_defer_empty_pending_is_jit_error_instead_of_remove_panic() {
    let mut gc = Gc::new();
    let mut fiber = Fiber::new(0);
    let module = Module::new("empty-pending-defer-test".to_string());
    fiber.unwinding = Some(UnwindingState {
        pending: Vec::new(),
        target_depth: 0,
        mode: UnwindingMode::Return,
        current_defer_generation: 0,
        return_values: None,
        return_func_id: 0,
        return_pc: 0,
        caller_ret_reg: 0,
        caller_ret_count: 0,
        is_closure_replay: false,
    });

    let result = execute_next_defer(&mut gc, &mut fiber, &module);

    assert!(matches!(
        result,
        ExecResult::JitError(msg)
            if msg.contains("unwind state has no pending defer to execute")
    ));
}
