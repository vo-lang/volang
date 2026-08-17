use super::*;
use crate::semantics::opcode_semantic_matrix;

#[test]
fn direct_jit_uses_frame_elision_contract() {
    let leaf = make_func(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 1);
    assert!(can_elide_frame_for_direct_jit(&leaf));

    let mut defer_leaf = make_func(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 1);
    defer_leaf.has_defer = true;
    assert!(
        !can_elide_frame_for_direct_jit(&defer_leaf),
        "defer functions need VM frames and must not use frame-elided dispatch"
    );

    let nested_call = make_func(vec![Instruction::new(Opcode::Call, 0, 0, 0)], 1);
    assert!(!can_elide_frame_for_direct_jit(&nested_call));
    assert!(!can_enter_prepared_shadow_frame_for_jit(&nested_call));
    assert!(
        can_enter_materialized_frame_for_jit(&nested_call),
        "materialized VM frames can safely re-enter ordinary nested-call JIT"
    );

    let alloc = make_func(vec![Instruction::new(Opcode::PtrNew, 0, 1, 1)], 2);
    assert!(
        can_elide_frame_for_direct_jit(&alloc),
        "allocation-only JIT callees poll and materialize before collection"
    );
    assert!(can_enter_prepared_shadow_frame_for_jit(&alloc));
    assert!(
        can_enter_materialized_frame_for_jit(&alloc),
        "allocation is safe with a materialized VM frame and precise roots"
    );

    let iface = make_func(vec![Instruction::new(Opcode::CallIface, 0, 2, 0)], 4);
    assert!(
        !can_elide_frame_for_direct_jit(&iface),
        "interface dispatch can panic/unwind and must not elide frames"
    );
    assert!(!can_enter_prepared_shadow_frame_for_jit(&iface));
    assert!(can_enter_materialized_frame_for_jit(&iface));
    assert!(!can_enter_prepared_shadow_frame_for_jit(&defer_leaf));
    assert!(!can_enter_materialized_frame_for_jit(&defer_leaf));

    let trapping_leaf = make_func(
        vec![
            Instruction::new(Opcode::PtrGet, 0, 1, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        2,
    );
    assert!(!can_elide_frame_for_direct_jit(&trapping_leaf));
    assert!(can_enter_prepared_shadow_frame_for_jit(&trapping_leaf));

    let mut heap_return = leaf;
    heap_return.heap_ret_gcref_count = 1;
    heap_return.heap_ret_slots = vec![1];
    assert!(!can_elide_frame_for_direct_jit(&heap_return));
    assert!(!can_enter_prepared_shadow_frame_for_jit(&heap_return));
    assert!(can_enter_materialized_frame_for_jit(&heap_return));
}

#[test]
fn gc_effect_contract_protects_key_runtime_boundaries() {
    let alloc = crate::contract::opcode_contract(Opcode::PtrNew);
    assert!(alloc.may_alloc && alloc.may_gc);

    let iface_call = crate::contract::opcode_contract(Opcode::CallIface);
    assert!(iface_call.may_call);
    assert!(iface_call.may_panic);
    assert!(iface_call.needs_frame);
    assert!(iface_call.touches_interface);

    let ptr_set = crate::contract::opcode_contract(Opcode::PtrSet);
    assert!(ptr_set.needs_write_barrier);

    let defer_push = crate::contract::opcode_contract(Opcode::DeferPush);
    assert!(defer_push.may_unwind);
    assert!(defer_push.may_observe_frame);
    assert!(defer_push.needs_frame);
}

#[test]
fn gc_write_barrier_contract_matches_vm_and_lowering_matrix() {
    let expected_barrier_ops = [
        Opcode::PtrSet,
        Opcode::ArraySet,
        Opcode::SliceSet,
        Opcode::MapSet,
    ];
    for opcode in opcode_semantic_matrix()
        .iter()
        .map(|row| row.opcode)
        .filter(|opcode| *opcode != Opcode::Invalid)
    {
        let expected = expected_barrier_ops.contains(&opcode);
        assert_eq!(
            crate::contract::opcode_contract(opcode).needs_write_barrier,
            expected,
            "{opcode:?} write-barrier contract must match VM/JIT heap-store semantics"
        );
    }
}

#[test]
fn compile_supports_port_select_recv_opcode() {
    let mut func = make_func_with_slot_types(
        vec![
            Instruction::with_flags(Opcode::SelectBegin, 0, 1, 0, 0),
            Instruction::with_flags(Opcode::SelectRecv, 0, 2, 0, 0),
            Instruction::new(Opcode::SelectExec, 1, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        vec![SlotType::GcBase, SlotType::Value, SlotType::Value],
    );
    func.instruction_metadata[1] = InstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    };
    func.instruction_metadata[2] = InstructionMetadata::SelectExecLayout {
        cases: vec![vo_runtime::bytecode::SelectCaseLayout::Recv {
            destination: 2,
            queue: 0,
            elem_slots: 1,
            has_ok: false,
        }],
    };
    let mut module = VoModule::new("test".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    let result = jit.compile(0, &module.functions[0], &module, env);

    assert!(
        result.is_ok(),
        "SelectRecv should compile in JIT: {:?}",
        result
    );
}

#[test]
fn compile_supports_port_queue_opcodes() {
    let elem_meta = ValueMeta::new(0, ValueKind::Int64);
    let elem_rttid = ValueRttid::new(0, ValueKind::Int64);
    let mut func = make_func_with_slot_types(
        vec![
            Instruction::new(Opcode::LoadConst, 1, 0, 0),
            Instruction::new(Opcode::LoadInt, 2, 0, 0),
            Instruction::with_flags(
                Opcode::QueueNew,
                vo_runtime::instruction::QUEUE_KIND_PORT_FLAG,
                0,
                1,
                2,
            ),
            Instruction::new(Opcode::QueueLen, 3, 0, 0),
            Instruction::new(Opcode::QueueCap, 4, 0, 0),
            Instruction::with_flags(Opcode::QueueSend, 0, 0, 1, 0),
            Instruction::with_flags(Opcode::QueueRecv, 1, 1, 0, 0),
            Instruction::new(Opcode::QueueClose, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        vec![
            SlotType::GcBase,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
        ],
    );
    func.instruction_metadata[2] = InstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    };
    func.instruction_metadata[5] = InstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    };
    func.instruction_metadata[6] = InstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    };
    let mut module = VoModule::new("test".into());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let packed = ((elem_rttid.to_raw() as i64) << 32) | elem_meta.to_raw() as i64;
    module.constants.push(Constant::Int(packed));
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    let result = jit.compile(0, &module.functions[0], &module, env);

    assert!(
        result.is_ok(),
        "Queue opcodes should compile in JIT: {:?}",
        result
    );
}

#[derive(Default)]
struct QueueTransitionCalls {
    sends: u32,
    recvs: u32,
}

extern "C" fn queue_send_runtime_transition(
    ctx: *mut vo_runtime::jit_api::JitContext,
    _chan: u64,
    _value: *const u64,
    _slots: u32,
) -> JitResult {
    let Some(ctx) = (unsafe { ctx.as_mut() }) else {
        return JitResult::JitError;
    };
    let Some(calls) = (unsafe { (ctx.callback_state as *mut QueueTransitionCalls).as_mut() })
    else {
        return JitResult::JitError;
    };
    calls.sends += 1;
    JitResult::RuntimeTransition
}

extern "C" fn queue_recv_after_transition(
    ctx: *mut vo_runtime::jit_api::JitContext,
    _chan: u64,
    _dst: *mut u64,
    _slots: u32,
    _has_ok: u32,
) -> JitResult {
    let Some(ctx) = (unsafe { ctx.as_mut() }) else {
        return JitResult::JitError;
    };
    let Some(calls) = (unsafe { (ctx.callback_state as *mut QueueTransitionCalls).as_mut() })
    else {
        return JitResult::JitError;
    };
    calls.recvs += 1;
    JitResult::Ok
}

extern "C" fn queue_recv_write_then_transition(
    ctx: *mut vo_runtime::jit_api::JitContext,
    _chan: u64,
    dst: *mut u64,
    _slots: u32,
    _has_ok: u32,
) -> JitResult {
    let Some(ctx) = (unsafe { ctx.as_mut() }) else {
        return JitResult::JitError;
    };
    unsafe {
        *dst = 0x2a;
    }
    let Some(resume_pc) = ctx.runtime_trap_pc.checked_add(1) else {
        return JitResult::JitError;
    };
    ctx.call_resume_pc = resume_pc;
    JitResult::RuntimeTransition
}

#[test]
fn jit_queue_runtime_transition_stops_before_following_recv() {
    let mut func = make_func_with_slot_types(
        vec![
            Instruction::with_flags(Opcode::QueueSend, 0, 0, 1, 0),
            Instruction::with_flags(Opcode::QueueRecv, 0, 2, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        vec![SlotType::GcBase, SlotType::Value, SlotType::Value],
    );
    for pc in 0..2 {
        func.instruction_metadata[pc] = InstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value],
        };
    }
    let mut module = VoModule::new("jit-queue-runtime-transition".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile queue transition repro");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64; 3];
    let mut ret = [0_u64; 1];
    let mut calls = QueueTransitionCalls::default();
    let mut parts = JitContextParts::new();
    parts.callbacks.queue_send_fn = Some(queue_send_runtime_transition);
    parts.callbacks.queue_recv_fn = Some(queue_recv_after_transition);
    let mut ctx = parts.context(&module, &mut args);
    ctx.callback_state = (&mut calls as *mut QueueTransitionCalls).cast();

    let result = unsafe { crate::invoke_test_jit(jit_func, &mut ctx, &mut args, &mut ret) };

    assert_eq!(result, JitResult::RuntimeTransition);
    assert_eq!(calls.sends, 1);
    assert_eq!(calls.recvs, 0, "native code must stop before the recv");
    assert_eq!(ctx.runtime_trap_pc, 0);
    assert_eq!(ctx.call_resume_pc, 1);
}

#[test]
fn jit_queue_recv_transition_preserves_callback_output() {
    let mut func = make_func_with_slot_types(
        vec![
            Instruction::with_flags(Opcode::QueueRecv, 0, 1, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        vec![SlotType::GcBase, SlotType::Value],
    );
    func.instruction_metadata[0] = InstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    };
    let mut module = VoModule::new("jit-queue-recv-transition-output".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
    )
    .expect("compile queue recv transition repro");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64, 0xdead_beef];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    parts.callbacks.queue_recv_fn = Some(queue_recv_write_then_transition);
    let mut ctx = parts.context(&module, &mut args);

    let result = unsafe { crate::invoke_test_jit(jit_func, &mut ctx, &mut args, &mut ret) };

    assert_eq!(result, JitResult::RuntimeTransition);
    assert_eq!(ctx.runtime_trap_pc, 0);
    assert_eq!(ctx.call_resume_pc, 1);
    assert_eq!(
        args[1], 0x2a,
        "callback output must survive side-exit spill"
    );
}

#[test]
fn compile_rejects_missing_dynamic_elem_layout_instead_of_panicking() {
    let func = make_func(
        vec![
            Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2),
            Instruction::new(Opcode::Return, 0, 1, 0),
        ],
        4,
    );
    let mut module = VoModule::new("test".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    let result = jit.compile(0, &module.functions[0], &module, env);

    assert!(matches!(
        result,
        Err(JitError::InvalidMetadata(JitMetadataError::MissingLayout {
            layout: "ElemLayout",
            ..
        }))
    ));
}

#[test]
fn compile_rejects_module_scope_change_instead_of_reusing_cached_function_042() {
    let mut first = VoModule::new("jit-cache-a".into());
    first.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let mut second = VoModule::new("jit-cache-b".into());
    second.functions.push(make_func(
        vec![
            Instruction::new(Opcode::LoadInt, 0, 7, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        1,
    ));

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &first.functions[0], &first, env)
        .expect("compile first module");

    assert!(
        jit.compile(0, &second.functions[0], &second, env).is_err(),
        "JitCompiler must not reuse func-id cache entries across different verified modules"
    );
}

#[test]
fn loaded_scope_distinguishes_distinct_module_images() {
    let mut first = VoModule::new("identity-a".into());
    first.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let mut second = VoModule::new("identity-b".into());
    second.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let mut jit = JitCompiler::new().expect("create jit compiler");
    let first = Arc::new(
        vo_common_core::verifier::verify_loaded_module(first).expect("verified loaded module"),
    );
    let second = Arc::new(
        vo_common_core::verifier::verify_loaded_module(second).expect("verified loaded module"),
    );

    jit.bind_loaded_module_scope(first)
        .expect("bind first loaded module");

    assert!(matches!(
        jit.bind_loaded_module_scope(second),
        Err(JitError::ModuleScopeChanged)
    ));
}

#[test]
fn production_compile_entry_requires_and_uses_retained_loaded_module() {
    let mut module = VoModule::new("loaded-production-entry".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let loaded = Arc::new(
        vo_common_core::verifier::verify_loaded_module(module).expect("verified loaded module"),
    );
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    let mut jit = JitCompiler::new().expect("create jit compiler");

    assert!(matches!(
        jit.compile_loaded(0, env),
        Err(JitError::Internal(message)) if message.contains("no loaded module")
    ));
    jit.bind_loaded_module_scope(loaded)
        .expect("bind retained module owner");
    jit.compile_loaded(0, env)
        .expect("compile through retained module authority");
    assert!(unsafe { jit.get_func_ptr(0) }.is_some());
}

#[test]
fn verified_immutable_bind_skips_reverification() {
    let mut module = VoModule::new("verified-immutable-bind".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let loaded = Arc::new(
        vo_common_core::verifier::verify_loaded_module(module).expect("common verifier image"),
    );
    let mut jit = JitCompiler::new().expect("create jit compiler");
    let before = crate::verifier::verification_work_counts_for_test();

    jit.bind_loaded_module_scope(Arc::clone(&loaded))
        .expect("bind common-verified image");
    jit.verify_module_once(loaded.module())
        .expect("bound module should use exact identity");
    jit.bind_loaded_module_scope(Arc::clone(&loaded))
        .expect("rebind same loaded image");

    assert_eq!(
        crate::verifier::verification_work_counts_for_test(),
        before,
        "common verification must stay outside the verified immutable path"
    );

    crate::verifier::verify_module(loaded.module()).expect("raw verifier control path");
    assert_eq!(
        crate::verifier::verification_work_counts_for_test(),
        (before.0 + 1, before.1),
        "the control path must observe one common verification"
    );
}

#[test]
fn common_verifier_owns_strict_instruction_metadata() {
    let mut module = VoModule::new("verified-strict-jit-metadata".into());
    module.functions.push(make_func(
        vec![
            Instruction::with_flags(Opcode::Hint, vo_runtime::instruction::HINT_LOOP, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        1,
    ));
    let err = vo_common_core::verifier::verify_module(&module)
        .expect_err("common verifier requires loop-end metadata");

    assert!(matches!(
        err,
        vo_common_core::verifier::ModuleVerificationError::MissingLayout {
            opcode: Opcode::Hint,
            layout: "LoopEnd",
            ..
        }
    ));
}

#[test]
fn verified_immutable_bind_rejects_a_distinct_module_image() {
    let mut first = VoModule::new("verified-identity-a".into());
    first.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let mut second = VoModule::new("verified-identity-b".into());
    second.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let first = Arc::new(
        vo_common_core::verifier::verify_loaded_module(first).expect("first common image"),
    );
    let second = Arc::new(
        vo_common_core::verifier::verify_loaded_module(second).expect("second common image"),
    );
    let mut jit = JitCompiler::new().expect("create jit compiler");

    jit.bind_loaded_module_scope(first)
        .expect("bind first loaded module");
    let err = jit
        .bind_loaded_module_scope(second)
        .expect_err("a compiler cannot cross immutable module images");

    assert!(matches!(err, JitError::ModuleScopeChanged));
}

#[test]
fn compile_rejects_env_scope_change_instead_of_reusing_cached_function_043() {
    let mut module = VoModule::new("jit-env-cache".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));

    let first_externs = resolved_extern_table_for_scope(1);
    let second_externs = resolved_extern_table_for_scope(2);
    let mut jit = JitCompiler::new().expect("create jit compiler");
    jit.compile(
        0,
        &module.functions[0],
        &module,
        JitCompileEnv {
            externs: &first_externs,
            backend_caps: JitBackendCaps {
                extern_suspend: true,
            },
        },
    )
    .expect("compile first env");

    assert!(
        jit.compile(
            0,
            &module.functions[0],
            &module,
            JitCompileEnv {
                externs: &second_externs,
                backend_caps: JitBackendCaps {
                    extern_suspend: false,
                },
            }
        )
        .is_err(),
        "JitCompiler must not reuse cache entries across resolved extern/backend-cap scopes"
    );
}

#[test]
fn compile_rejects_function_scope_change_instead_of_caching_foreign_function_044() {
    let mut module = VoModule::new("jit-function-scope-cache".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        1,
    ));
    let foreign_func = make_func(
        vec![
            Instruction::new(Opcode::LoadInt, 0, 44, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        1,
    );

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);

    assert!(
            jit.compile(0, &foreign_func, &module, env).is_err(),
            "JitCompiler must reject caller-supplied FunctionDef values that are not the module func_id owner"
        );
    assert!(
        !jit.cache.contains_for_tier(0, JitTier::Baseline),
        "rejected foreign FunctionDef must not poison the func_id cache"
    );
}

#[test]
fn jit_shift_precheck_ignores_stale_branch_constant_fact() {
    let func = make_func_with_sig(
        vec![
            jump_if_not(0, 2),
            Instruction::new(Opcode::LoadConst, 1, 0, 0),
            Instruction::new(Opcode::LoadInt, 2, 1, 0),
            Instruction::new(Opcode::Shl, 3, 2, 1),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ],
        2,
        2,
        4,
        1,
    );
    let mut module = VoModule::new("test".into());
    module.constants.push(Constant::Int(64));
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &module.functions[0], &module, env)
        .expect("compile repro function");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64, (-1_i64) as u64, 0, 0];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);

    let result = unsafe { crate::invoke_test_jit(jit_func, &mut ctx, &mut args, &mut ret) };

    assert_eq!(
        result,
        JitResult::Panic,
        "false branch keeps dynamic shift amount; -1 must not be optimized as const 64"
    );
    assert!(
        parts.panic_flag,
        "negative shift should set the runtime panic flag"
    );
}

fn run_jit_shift(opcode: Opcode, flags: u8, lhs: u64, rhs: u64) -> (JitResult, u64, bool) {
    let func = make_func_with_sig(
        vec![
            Instruction::with_flags(opcode, flags, 2, 0, 1),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ],
        2,
        2,
        3,
        1,
    );
    let mut module = VoModule::new("shift".into());
    module.functions.push(func);
    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &module.functions[0], &module, env)
        .expect("compile shift");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };
    let mut args = [lhs, rhs, 0];
    let mut ret = [0];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);
    let result = unsafe { crate::invoke_test_jit(jit_func, &mut ctx, &mut args, &mut ret) };
    (result, ret[0], parts.panic_flag)
}

#[test]
fn jit_shift_count_signedness_handles_unsigned_high_bit_without_negative_panic() {
    use vo_runtime::instruction::SHIFT_FLAG_RHS_UNSIGNED;

    let high = 1_u64 << 63;
    for (opcode, lhs, expected) in [
        (Opcode::Shl, 1, 0),
        (Opcode::ShrU, u64::MAX, 0),
        (Opcode::ShrS, (-5_i64) as u64, u64::MAX),
    ] {
        let (result, value, panicked) = run_jit_shift(opcode, SHIFT_FLAG_RHS_UNSIGNED, lhs, high);
        assert_eq!(result, JitResult::Ok, "{opcode:?}");
        assert_eq!(value, expected, "{opcode:?}");
        assert!(!panicked, "unsigned high-bit count must not be negative");
    }

    let (result, _, panicked) = run_jit_shift(Opcode::Shl, 0, 1, high);
    assert_eq!(result, JitResult::Panic);
    assert!(
        panicked,
        "the same raw bits remain negative for a signed count"
    );
}

fn run_const_float_to_int(value: f64, flags: u8) -> u64 {
    let func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            Instruction::with_flags(Opcode::ConvF2I, flags, 1, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ],
        vec![SlotType::Float, SlotType::Value],
        0,
        0,
        1,
    );
    let mut module = VoModule::new("test".into());
    module.constants.push(Constant::Float(value));
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &module.functions[0], &module, env)
        .expect("compile float-to-int repro");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64; 2];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);

    let result = unsafe { crate::invoke_test_jit(jit_func, &mut ctx, &mut args, &mut ret) };
    assert_eq!(result, JitResult::Ok);
    ret[0]
}

fn run_const_int_to_float(value: u64, flags: u8) -> u64 {
    let result_slot = SlotType::Float;
    let mut func = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            Instruction::with_flags(Opcode::ConvI2F, flags, 1, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ],
        vec![SlotType::Value, result_slot],
        0,
        0,
        1,
    );
    func.ret_slot_types = vec![result_slot];
    let mut module = VoModule::new("test".into());
    module.constants.push(Constant::Int(value as i64));
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &module.functions[0], &module, env)
        .expect("compile int-to-float repro");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64; 2];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);
    assert_eq!(
        unsafe { crate::invoke_test_jit(jit_func, &mut ctx, &mut args, &mut ret) },
        JitResult::Ok
    );
    ret[0]
}

#[test]
fn jit_float_to_int_matches_vm_saturating_cast_edges() {
    for value in [
        f64::NAN,
        f64::INFINITY,
        f64::NEG_INFINITY,
        (i64::MAX as f64) * 2.0,
        (i64::MIN as f64) * 2.0,
        3.9,
        -3.9,
    ] {
        assert_eq!(
            run_const_float_to_int(value, 0),
            value as i64 as u64,
            "ConvF2I must match VM/Rust cast semantics for {value:?}"
        );
    }
}

#[test]
fn jit_unsigned_and_narrow_float_to_int_saturate_to_final_target_width() {
    use vo_runtime::instruction::{CONV_FLAG_UNSIGNED, CONV_WIDTH_16, CONV_WIDTH_32, CONV_WIDTH_8};

    for value in [
        f64::NAN,
        f64::INFINITY,
        f64::NEG_INFINITY,
        -1.0,
        3.9,
        18_446_744_073_709_549_568.0,
    ] {
        assert_eq!(
            run_const_float_to_int(value, CONV_FLAG_UNSIGNED),
            value as u64,
            "unsigned ConvF2I edge {value:?}"
        );
    }

    for (value, flags, expected) in [
        (300.0, CONV_FLAG_UNSIGNED | CONV_WIDTH_8, u8::MAX as u64),
        (-300.0, CONV_WIDTH_8, i8::MIN as i64 as u64),
        (
            70_000.0,
            CONV_FLAG_UNSIGNED | CONV_WIDTH_16,
            u16::MAX as u64,
        ),
        (-70_000.0, CONV_WIDTH_16, i16::MIN as i64 as u64),
        (
            u32::MAX as f64 * 2.0,
            CONV_FLAG_UNSIGNED | CONV_WIDTH_32,
            u32::MAX as u64,
        ),
        (i32::MAX as f64 * 2.0, CONV_WIDTH_32, i32::MAX as i64 as u64),
    ] {
        assert_eq!(run_const_float_to_int(value, flags), expected);
    }
}

#[test]
fn jit_integer_to_float_honors_unsigned_and_direct_f32_rounding() {
    use vo_runtime::instruction::{CONV_FLAG_FLOAT32, CONV_FLAG_UNSIGNED};

    assert_eq!(
        f64::from_bits(run_const_int_to_float(u64::MAX, CONV_FLAG_UNSIGNED)),
        u64::MAX as f64
    );
    assert_eq!(f64::from_bits(run_const_int_to_float(u64::MAX, 0)), -1.0);

    let source = 4_611_686_293_305_294_849_i64;
    let result = f32::from_bits(run_const_int_to_float(source as u64, CONV_FLAG_FLOAT32) as u32);
    assert_eq!(result, source as f32);
    assert_ne!(result, (source as f64) as f32, "must avoid double rounding");
    assert_eq!(
        f32::from_bits(
            run_const_int_to_float(u64::MAX, CONV_FLAG_UNSIGNED | CONV_FLAG_FLOAT32,) as u32,
        ),
        u64::MAX as f32
    );
}

#[test]
fn typed_gc_barrier_helper_uses_checked_result_abi() {
    let typed_barrier_abi = vo_runtime::jit_api::runtime_helper_abi_fields()
        .iter()
        .find(|field| field.name == "vo_gc_typed_write_barrier_by_meta")
        .expect("typed metadata barrier ABI manifest row");
    assert_eq!(
        typed_barrier_abi.ret,
        vo_runtime::jit_api::JitAbiType::JitResult,
        "typed metadata barrier helper import must be generated with a JitResult return"
    );
}
