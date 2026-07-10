use super::*;

#[test]
fn lowering_paths_do_not_raw_expect_registered_helpers() {
    for (path, src) in [
        ("contract.rs", include_str!("../contract.rs")),
        ("call_helpers.rs", include_str!("../call_helpers.rs")),
        (
            "translate/memory.rs",
            include_str!("../translate/memory.rs"),
        ),
        (
            "translate/collections/mod.rs",
            include_str!("../translate/collections/mod.rs"),
        ),
        (
            "translate/collections/array.rs",
            include_str!("../translate/collections/array.rs"),
        ),
        (
            "translate/collections/element.rs",
            include_str!("../translate/collections/element.rs"),
        ),
        (
            "translate/collections/slice.rs",
            include_str!("../translate/collections/slice.rs"),
        ),
        (
            "translate/collections/map.rs",
            include_str!("../translate/collections/map.rs"),
        ),
        (
            "translate/collections/string.rs",
            include_str!("../translate/collections/string.rs"),
        ),
        (
            "translate/runtime_ops/mod.rs",
            include_str!("../translate/runtime_ops/mod.rs"),
        ),
        (
            "translate/runtime_ops/allocation.rs",
            include_str!("../translate/runtime_ops/allocation.rs"),
        ),
        (
            "translate/runtime_ops/closure.rs",
            include_str!("../translate/runtime_ops/closure.rs"),
        ),
        (
            "translate/runtime_ops/goroutine.rs",
            include_str!("../translate/runtime_ops/goroutine.rs"),
        ),
        (
            "translate/runtime_ops/interface.rs",
            include_str!("../translate/runtime_ops/interface.rs"),
        ),
        (
            "translate/runtime_ops/queue_select.rs",
            include_str!("../translate/runtime_ops/queue_select.rs"),
        ),
    ] {
        let production = vo_source_contract::production_source_without_test_modules(src);
        for needle in [
            ".expect(\"",
            "helper not registered",
            "helper must be registered",
            "must be available",
        ] {
            assert!(
                    !production.contains(needle),
                    "{path} must return JitError/JitResult::JitError for missing helpers; found {needle:?}"
                );
        }
    }
}

#[test]
fn cranelift_ir_verifier_is_fail_fast_in_all_builds() {
    let src = include_str!("../lib.rs");
    let finalize = src
        .split("fn finalize_function")
        .nth(1)
        .expect("finalize_function body");
    let verifier_prefix = finalize
        .split("verify_function")
        .next()
        .expect("verifier prefix");

    assert!(
        !verifier_prefix.contains("debug_assertions"),
        "Cranelift IR verification must not be debug-only"
    );
    assert!(
        finalize.contains("JitError::Internal(format!"),
        "Cranelift IR verifier errors must fail the JIT compile boundary"
    );
}

#[test]
fn jit_copy_n_overlap_matches_memmove_semantics() {
    let func = make_func_with_sig(
        vec![
            Instruction::new(Opcode::CopyN, 1, 0, 3),
            Instruction::new(Opcode::Return, 1, 3, 0),
        ],
        3,
        3,
        4,
        3,
    );
    let mut module = VoModule::new("test".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile(0, &module.functions[0], &module, env, &[])
        .expect("compile CopyN overlap repro");
    let code = jit.code_memory_stats();
    assert_eq!(code.function_count, 1);
    assert!(
        code.function_bytes > 0,
        "compiled code bytes must be observable"
    );
    assert_eq!(code.total_bytes(), code.function_bytes);
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [1_u64, 2, 3, 0];
    let mut ret = [0_u64; 3];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);

    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

    assert_eq!(result, JitResult::Ok);
    assert_eq!(
        ret,
        [1, 2, 3],
        "overlapping CopyN must read the whole source range before writing"
    );
}

#[test]
fn native_backedge_exhausts_budget_through_scheduler_yield_contract() {
    let func = make_func_with_sig(vec![Instruction::new(Opcode::Jump, 0, 0, 0)], 0, 0, 0, 0);
    let mut module = VoModule::new("native-timeslice".into());
    module.functions.push(func);

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    jit.compile(
        0,
        &module.functions[0],
        &module,
        default_compile_env(&externs),
        &[],
    )
    .expect("compile native loop");
    let jit_func = unsafe { jit.cache.get_func_ptr(0).expect("compiled entry") };

    let mut args = [0_u64; 1];
    let mut ret = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut args);
    let result = jit_func(&mut ctx, args.as_mut_ptr(), ret.as_mut_ptr());

    assert_eq!(result, JitResult::Call);
    assert_eq!(ctx.call_kind, JitContext::CALL_KIND_YIELD);
    assert_eq!(ctx.call_resume_pc, 0);
}

#[test]
fn loop_fallthrough_exit_uses_jit_result_ok_abi() {
    let func = make_func(vec![Instruction::new(Opcode::LoadInt, 0, 123, 0)], 1);
    let mut module = VoModule::new("test".into());
    module.functions.push(func);
    let loop_info = LoopInfo {
        depth: 0,
        begin_pc: 0,
        end_pc: 0,
        exit_pc: JitResult::JitError as usize,
        has_defer: false,
        has_labeled_break: false,
        has_labeled_continue: false,
        live_in: Vec::new(),
        live_out: vec![0],
        has_calls: false,
    };

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = JitCompileEnv {
        externs: &externs,
        backend_caps: Default::default(),
    };
    jit.compile_loop(0, &module.functions[0], &module, env, &loop_info, &[])
        .expect("compile minimal fallthrough loop");
    let loop_func = unsafe { jit.cache.get_loop_func_ptr(0, 0).expect("compiled loop") };

    let mut locals = [0_u64; 1];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut locals);

    let result = loop_func(&mut ctx, locals.as_mut_ptr());

    assert_eq!(
        result,
        JitResult::Ok,
        "normal OSR exits must return JitResult::Ok, not a raw exit pc"
    );
    assert_eq!(
        ctx.loop_exit_pc,
        JitResult::JitError as u32,
        "normal OSR exits must publish the resume pc through ctx.loop_exit_pc"
    );
    assert_eq!(locals[0], 123);
}

#[test]
fn compile_loop_rejects_module_scope_change_instead_of_reusing_cached_loop_042() {
    let mut first = VoModule::new("jit-loop-cache-a".into());
    first.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadInt, 0, 1, 0)],
        1,
    ));
    let mut second = VoModule::new("jit-loop-cache-b".into());
    second.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadInt, 0, 2, 0)],
        1,
    ));
    let loop_info = LoopInfo {
        depth: 0,
        begin_pc: 0,
        end_pc: 0,
        exit_pc: 1,
        has_defer: false,
        has_labeled_break: false,
        has_labeled_continue: false,
        live_in: Vec::new(),
        live_out: vec![0],
        has_calls: false,
    };

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = JitCompileEnv {
        externs: &externs,
        backend_caps: Default::default(),
    };
    jit.compile_loop(0, &first.functions[0], &first, env, &loop_info, &[])
        .expect("compile first module loop");

    assert!(
        jit.compile_loop(0, &second.functions[0], &second, env, &loop_info, &[])
            .is_err(),
        "JitCompiler must not reuse OSR loop cache entries across different verified modules"
    );
}

#[test]
fn compile_loop_rejects_env_scope_change_instead_of_reusing_cached_loop_043() {
    let mut module = VoModule::new("jit-loop-env-cache".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadInt, 0, 1, 0)],
        1,
    ));
    let loop_info = LoopInfo {
        depth: 0,
        begin_pc: 0,
        end_pc: 0,
        exit_pc: 1,
        has_defer: false,
        has_labeled_break: false,
        has_labeled_continue: false,
        live_in: Vec::new(),
        live_out: vec![0],
        has_calls: false,
    };

    let first_externs = resolved_extern_table_for_scope(1);
    let second_externs = resolved_extern_table_for_scope(2);
    let mut jit = JitCompiler::new().expect("create jit compiler");
    jit.compile_loop(
        0,
        &module.functions[0],
        &module,
        JitCompileEnv {
            externs: &first_externs,
            backend_caps: JitBackendCaps {
                extern_suspend: true,
            },
        },
        &loop_info,
        &[],
    )
    .expect("compile first env loop");

    assert!(
            jit.compile_loop(
                0,
                &module.functions[0],
                &module,
                JitCompileEnv {
                    externs: &second_externs,
                    backend_caps: JitBackendCaps {
                        extern_suspend: false,
                    },
                },
                &loop_info,
                &[],
            )
            .is_err(),
            "JitCompiler must not reuse OSR loop cache entries across resolved extern/backend-cap scopes"
        );
}

#[test]
fn compile_loop_rejects_loop_scope_change_instead_of_reusing_cached_loop_044() {
    let mut module = VoModule::new("jit-loop-scope-cache".into());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadInt, 0, 1, 0)],
        1,
    ));
    let first_loop_info = LoopInfo {
        depth: 0,
        begin_pc: 0,
        end_pc: 0,
        exit_pc: 1,
        has_defer: false,
        has_labeled_break: false,
        has_labeled_continue: false,
        live_in: Vec::new(),
        live_out: vec![0],
        has_calls: false,
    };
    let mut second_loop_info = first_loop_info.clone();
    second_loop_info.exit_pc = 2;

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    jit.compile_loop(0, &module.functions[0], &module, env, &first_loop_info, &[])
        .expect("compile first loop scope");

    assert!(
        jit.compile_loop(
            0,
            &module.functions[0],
            &module,
            env,
            &second_loop_info,
            &[],
        )
        .is_err(),
        "JitCompiler must not reuse OSR loop cache entries across different LoopInfo scopes"
    );
}

#[test]
fn vm_osr_borrow_boundary_001_loop_compiler_has_no_runtime_transition_path() {
    let loop_compiler = include_str!("../loop_compiler.rs");
    assert!(
            !loop_compiler.contains("apply_runtime_transition")
                && !loop_compiler.contains("push_pending_runtime_transition"),
            "vo-jit loop compiler must return JitResult boundaries instead of mutating VM runtime state"
        );
    assert!(
        loop_compiler.contains("JitResult::Ok"),
        "OSR loop exits must publish control back to the VM through JitResult"
    );
}

#[test]
fn compile_loop_rejects_out_of_range_loop_info_instead_of_panicking() {
    let func = make_func(vec![Instruction::new(Opcode::LoadInt, 0, 123, 0)], 1);
    let mut module = VoModule::new("test".into());
    module.functions.push(func);
    let loop_info = LoopInfo {
        depth: 0,
        begin_pc: 0,
        end_pc: 7,
        exit_pc: 1,
        has_defer: false,
        has_labeled_break: false,
        has_labeled_continue: false,
        live_in: Vec::new(),
        live_out: vec![0],
        has_calls: false,
    };

    let mut jit = JitCompiler::new().expect("create jit compiler");
    let externs = ResolvedExternTable::empty();
    let env = JitCompileEnv {
        externs: &externs,
        backend_caps: Default::default(),
    };
    let err = jit
        .compile_loop(0, &module.functions[0], &module, env, &loop_info, &[])
        .expect_err("malformed LoopInfo must fail fast");

    assert!(matches!(err, JitError::InvalidOsrTarget(0)));
}

#[test]
fn source_contract_tests_do_not_use_textual_cfg_test_truncation_062() {
    vo_source_contract::assert_no_textual_cfg_test_splits(env!("CARGO_MANIFEST_DIR"));
}
