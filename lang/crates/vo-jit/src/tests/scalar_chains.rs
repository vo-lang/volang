use super::*;
use vo_runtime::jit_api::JitTier;

fn dormant_call_loop() -> VoModule {
    let mut module = VoModule::new("inline-call-alternate-entry".into());
    module.functions = vec![
        make_func_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 0, 0),
                Instruction::new(Opcode::JumpIfNot, 0, 4, 0),
                Instruction::new(Opcode::Call, 1, 1, 0),
                Instruction::new(Opcode::LoadInt, 0, 0, 0),
                Instruction::new(Opcode::Jump, 0, (-3_i32) as u16, u16::MAX),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
            2,
            2,
            3,
            1,
        ),
        make_func_with_sig(vec![Instruction::new(Opcode::Return, 0, 1, 0)], 1, 1, 1, 1),
    ];
    module
}

#[test]
fn dormant_static_call_executes_at_osr_entry_without_a_child_entry() {
    let module = dormant_call_loop();
    let loaded = Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().unwrap();
    jit.bind_loaded_module_scope(loaded).unwrap();
    for tier in [JitTier::Baseline, JitTier::Optimizing] {
        jit.compile_loaded_tier(0, default_compile_env(&externs), tier)
            .unwrap();
        let eligibility = jit.function_entry_eligibility(0).unwrap();
        assert!(eligibility.frame_elided && eligibility.prepared_shadow && !eligibility.may_gc);
        let entry = unsafe { jit.get_func_ptr_for_tier(0, tier).unwrap() };
        let mut stack = [1, 41, 0xfeed, 0, 0, 0, 0, 0];
        let mut parts = JitContextParts::new();
        let mut ctx = parts.context(&module, &mut stack);
        ctx.current_func_id = 0;
        ctx.fiber_sp = 3;
        let mut ret = [0xfeed];
        assert_eq!(
            unsafe { crate::invoke_test_jit(entry, &mut ctx, &mut stack, &mut ret) },
            JitResult::Ok
        );
        // The ordinary entry overwrites the condition and initializes locals.
        assert_eq!(ret, [0]);
    }
    jit.compile_loaded_loop(
        0,
        default_compile_env(&externs),
        &LoopInfo {
            begin_pc: 1,
            end_pc: 4,
            exit_pc: 5,
        },
    )
    .unwrap();
    assert!(unsafe { jit.get_func_ptr_for_tier(1, JitTier::Baseline) }.is_none());
    assert!(unsafe { jit.get_func_ptr_for_tier(1, JitTier::Optimizing) }.is_none());
    let entry = unsafe { jit.cache.get_loop_func_ptr(0, 1).unwrap() };
    for condition in [0, 1] {
        let mut stack = [condition, 41, 0xfeed, 0, 0, 0, 0, 0];
        let mut parts = JitContextParts::new();
        let mut ctx = parts.context(&module, &mut stack);
        ctx.current_func_id = 0;
        ctx.fiber_sp = 3;
        assert_eq!(entry(&mut ctx, stack.as_mut_ptr()), JitResult::Ok);
        assert_eq!(stack[2], if condition == 0 { 0xfeed } else { 41 });
    }
}

#[test]
fn dormant_static_call_executes_in_optimized_and_fallback_continuations() {
    let module = dormant_call_loop();
    let pcs = [1, 5];
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    for optimize in [false, true] {
        let mut jit = JitCompiler::new().unwrap();
        jit.verify_module_once(&module).unwrap();
        jit.verify_env_once(env).unwrap();
        let module_analysis = jit.module_analysis(&module, env).unwrap();
        assert!(module_analysis.entry_eligibility[0].prepared_shadow);
        let ordinary = crate::analysis::FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            crate::MAX_JIT_ANALYSIS_BYTES,
        )
        .unwrap();
        let resumed = crate::analysis::FunctionAnalysis::try_for_continuations(
            &module.functions[0],
            &module,
            &[],
            &pcs,
            if optimize {
                crate::MAX_JIT_ANALYSIS_BYTES
            } else {
                0
            },
        )
        .unwrap();
        assert_eq!(resumed.is_some(), optimize);
        let analysis = resumed.as_ref().unwrap_or(&ordinary);
        let instructions = if optimize {
            crate::optimizer::OptimizedFunction::analyze_continuations(
                analysis.ir(),
                &module_analysis.inline_plan,
                0,
            )
        } else {
            crate::optimizer::OptimizedFunction::baseline_with_module(
                analysis.ir(),
                &module_analysis.inline_plan,
                0,
            )
        };
        let config = jit.module.target_config();
        jit.ctx.func.signature =
            crate::abi::native_signature(config.default_call_conv, config.pointer_type());
        let helpers = HelperRefs::new(&mut *jit.module, jit.helper_funcs);
        FunctionCompiler::new(
            &mut jit.ctx.func,
            &mut jit.func_ctx,
            0,
            &module.functions[0],
            &module,
            env,
            &module_analysis.entry_eligibility,
            helpers,
            analysis,
            crate::func_compiler::FunctionCompilePlan::Continuation {
                inlines: &module_analysis.inline_plan,
                instructions: &instructions,
                pcs: &pcs,
            },
        )
        .compile(config)
        .unwrap();
        let id = jit
            .module
            .declare_function(
                "dormant_inline_continuation",
                cranelift_module::Linkage::Local,
                &jit.ctx.func.signature,
            )
            .unwrap();
        let staged = jit
            .stage_function(id, "dormant inline continuation")
            .unwrap();
        let (code, _) = jit.publish_function_artifact(staged).unwrap();
        let entry: NativeJitFunc = unsafe { std::mem::transmute(code) };
        assert!(unsafe { jit.get_func_ptr_for_tier(1, JitTier::Optimizing) }.is_none());
        for (resume_pc, condition, expected) in [(1, 0, 0xfeed), (1, 1, 41), (5, 1, 0xfeed)] {
            let mut stack = [condition, 41, 0xfeed, 0, 0, 0, 0, 0];
            let mut parts = JitContextParts::new();
            let mut ctx = parts.context(&module, &mut stack);
            ctx.current_func_id = 0;
            ctx.fiber_sp = 3;
            let mut ret = [0];
            assert_eq!(
                entry(&mut ctx, 0, ret.as_mut_ptr(), resume_pc, 0, 0, 0, 0),
                JitResult::Ok
            );
            assert_eq!(
                ret,
                [expected],
                "optimized={optimize}, resume={resume_pc}, condition={condition}"
            );
        }
    }
}

fn chain(layers: u16) -> VoModule {
    let mut module = VoModule::new("scalar-chain-machine-code".into());
    for callee in 1..=layers {
        module.functions.push(make_func_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::Call, callee, 0, 0),
                Instruction::new(Opcode::AddI, 2, 1, 0),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
            vec![SlotType::Value; 3],
            1,
            1,
            1,
        ));
    }
    module.functions.push(make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::LoadInt, 1, 1, 0),
            Instruction::new(Opcode::AddI, 0, 0, 1),
            Instruction::new(Opcode::Return, 0, 1, 0),
        ],
        vec![SlotType::Value; 2],
        1,
        1,
        1,
    ));
    module
}

#[test]
fn scalar_chains_execute_function_and_osr_without_child_entries_or_argument_aliases() {
    for layers in [1, 4, 8] {
        let module = chain(layers);
        let loaded =
            Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
        let externs = ResolvedExternTable::empty();
        let mut jit = JitCompiler::new().unwrap();
        jit.bind_loaded_module_scope(loaded).unwrap();
        jit.compile_loaded_tier(0, default_compile_env(&externs), JitTier::Optimizing)
            .unwrap();
        jit.compile_loaded_loop(
            0,
            default_compile_env(&externs),
            &LoopInfo {
                begin_pc: 0,
                end_pc: 1,
                exit_pc: 2,
            },
        )
        .unwrap();
        for id in 1..=layers {
            assert!(
                unsafe { jit.get_func_ptr_for_tier(u32::from(id), JitTier::Optimizing) }.is_none()
            );
        }
        let function = unsafe { jit.get_func_ptr_for_tier(0, JitTier::Optimizing).unwrap() };
        let osr = unsafe { jit.cache.get_loop_func_ptr(0, 0).unwrap() };
        for use_osr in [false, true] {
            for input in [0_u64, 9, u64::MAX] {
                let mut stack = [0; 32];
                stack[0] = input;
                let mut parts = JitContextParts::new();
                let mut ctx = parts.context(&module, &mut stack);
                ctx.current_func_id = 0;
                ctx.fiber_sp = 3;
                let mut ret = [0];
                let outcome = if use_osr {
                    osr(&mut ctx, stack.as_mut_ptr())
                } else {
                    unsafe { crate::invoke_test_jit(function, &mut ctx, &mut stack, &mut ret) }
                };
                assert_eq!(outcome, JitResult::Ok, "layers={layers}, OSR={use_osr}");
                let expected = input.wrapping_mul(u64::from(layers) + 1).wrapping_add(1);
                assert_eq!(if use_osr { stack[2] } else { ret[0] }, expected);
            }
        }
    }
}

#[test]
fn scalar_chain_budget_exit_precedes_the_complete_expansion() {
    let module = chain(4);
    let loaded = Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().unwrap();
    jit.bind_loaded_module_scope(loaded).unwrap();
    jit.compile_loaded_tier(0, default_compile_env(&externs), JitTier::Optimizing)
        .unwrap();
    let entry = unsafe { jit.get_func_ptr_for_tier(0, JitTier::Optimizing).unwrap() };
    let mut stack = [0; 32];
    stack[0] = 9;
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut stack);
    ctx.current_func_id = 0;
    ctx.fiber_sp = 3;
    ctx.execution_budget = 1;
    let mut ret = [123];
    assert_eq!(
        unsafe { crate::invoke_test_jit(entry, &mut ctx, &mut stack, &mut ret) },
        JitResult::Call
    );
    assert_eq!(ctx.call_kind, JitContext::CALL_KIND_YIELD);
    assert_eq!(ctx.call_resume_pc, 0);
    assert_eq!(ctx.execution_budget, 1);
    assert_eq!(ret, [123]);
    assert_eq!(stack[0], 9);
    ctx.execution_budget = 1000;
    ctx.call_kind = 0;
    assert_eq!(
        unsafe { crate::invoke_test_jit(entry, &mut ctx, &mut stack, &mut ret) },
        JitResult::Ok
    );
    assert_eq!(ret, [46]);
}

#[test]
fn scalar_chains_initialize_private_return_slots_and_preserve_binary32_rounding() {
    for slot_type in [SlotType::Value, SlotType::Float] {
        for zero_return in [false, true] {
            let mut module = VoModule::new("scalar-chain-floating-locals".into());
            for callee in [1, 2] {
                let mut function = make_func_with_slot_types_and_sig(
                    vec![
                        Instruction::new(Opcode::Call, callee, 0, 0),
                        Instruction::new(Opcode::SubF32, 3, 2, 0),
                        Instruction::new(Opcode::Return, 3, 1, 0),
                    ],
                    vec![slot_type; 4],
                    2,
                    2,
                    1,
                );
                function.ret_slot_types = vec![slot_type];
                module.functions.push(function);
            }
            let leaf_code = if zero_return {
                vec![Instruction::new(Opcode::Return, 2, 1, 0)]
            } else {
                vec![
                    Instruction::new(Opcode::AddF32, 0, 0, 1),
                    Instruction::new(Opcode::Return, 0, 1, 0),
                ]
            };
            let mut leaf =
                make_func_with_slot_types_and_sig(leaf_code, vec![slot_type; 3], 2, 2, 1);
            leaf.ret_slot_types = vec![slot_type];
            module.functions.push(leaf);
            let loaded =
                Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
            let externs = ResolvedExternTable::empty();
            let mut jit = JitCompiler::new().unwrap();
            jit.bind_loaded_module_scope(loaded).unwrap();
            jit.compile_loaded_tier(0, default_compile_env(&externs), JitTier::Optimizing)
                .unwrap();
            let entry = unsafe { jit.get_func_ptr_for_tier(0, JitTier::Optimizing).unwrap() };
            assert!(unsafe { jit.get_func_ptr_for_tier(1, JitTier::Optimizing) }.is_none());
            assert!(unsafe { jit.get_func_ptr_for_tier(2, JitTier::Optimizing) }.is_none());
            for (left, right) in [
                (0_u32, 0x80000000_u32),
                (0x3f800001, 0x33800000),
                (0x7f800000, 0xff800000),
                (0x7fc00123, 0x3f800000),
                (1, 2),
            ] {
                let a = f32::from_bits(left);
                let b = f32::from_bits(right);
                let leaf = if zero_return { 0.0_f32 } else { a + b };
                let expected = (leaf - a) - a;
                let mut stack = [0; 32];
                stack[0] = u64::from(left) | 0xdeadbeef00000000;
                stack[1] = u64::from(right) | 0xabcdef1200000000;
                let mut parts = JitContextParts::new();
                let mut ctx = parts.context(&module, &mut stack);
                ctx.current_func_id = 0;
                ctx.fiber_sp = 4;
                let mut ret = [0];
                assert_eq!(
                    unsafe { crate::invoke_test_jit(entry, &mut ctx, &mut stack, &mut ret) },
                    JitResult::Ok
                );
                assert_eq!(ret[0] >> 32, 0);
                if expected.is_nan() {
                    assert!(f32::from_bits(ret[0] as u32).is_nan());
                } else {
                    assert_eq!(ret[0], u64::from(expected.to_bits()));
                }
            }
        }
    }
}

#[test]
fn scalar_chain_static_continuation_uses_resume_inputs_and_keeps_late_entry_independent() {
    let mut module = chain(4);
    module.functions[0]
        .code
        .insert(0, Instruction::new(Opcode::LoadInt, 0, 7, 0));
    module.functions[0]
        .instruction_metadata
        .insert(0, InstructionMetadata::None);
    let pcs = [1, 3];
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    let mut jit = JitCompiler::new().unwrap();
    jit.verify_module_once(&module).unwrap();
    jit.verify_env_once(env).unwrap();
    let module_analysis = jit.module_analysis(&module, env).unwrap();
    let analysis = crate::analysis::FunctionAnalysis::try_for_continuations(
        &module.functions[0],
        &module,
        &[],
        &pcs,
        crate::MAX_JIT_ANALYSIS_BYTES,
    )
    .unwrap()
    .unwrap();
    let instructions = crate::optimizer::OptimizedFunction::analyze_continuations(
        analysis.ir(),
        &module_analysis.inline_plan,
        0,
    );
    let config = jit.module.target_config();
    jit.ctx.func.signature =
        crate::abi::native_signature(config.default_call_conv, config.pointer_type());
    let helpers = HelperRefs::new(&mut *jit.module, jit.helper_funcs);
    FunctionCompiler::new(
        &mut jit.ctx.func,
        &mut jit.func_ctx,
        0,
        &module.functions[0],
        &module,
        env,
        &module_analysis.entry_eligibility,
        helpers,
        &analysis,
        crate::func_compiler::FunctionCompilePlan::Continuation {
            inlines: &module_analysis.inline_plan,
            instructions: &instructions,
            pcs: &pcs,
        },
    )
    .compile(config)
    .unwrap();
    let id = jit
        .module
        .declare_function(
            "scalar_chain_continuation",
            cranelift_module::Linkage::Local,
            &jit.ctx.func.signature,
        )
        .unwrap();
    let staged = jit.stage_function(id, "scalar chain continuation").unwrap();
    let (code, _) = jit.publish_function_artifact(staged).unwrap();
    let entry: NativeJitFunc = unsafe { std::mem::transmute(code) };
    for id in 1..=4 {
        assert!(unsafe { jit.get_func_ptr_for_tier(id, JitTier::Optimizing) }.is_none());
    }
    let mut stack = [0; 32];
    stack[0] = 40;
    let mut ret = [0xfeed];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(&module, &mut stack);
    ctx.current_func_id = 0;
    ctx.fiber_sp = 3;
    ctx.execution_budget = 1;
    assert_eq!(
        entry(&mut ctx, 0, ret.as_mut_ptr(), 1, 0, 0, 0, 0),
        JitResult::Call
    );
    assert_eq!(ctx.call_kind, JitContext::CALL_KIND_YIELD);
    assert_eq!(ctx.call_resume_pc, 1);
    assert_eq!(ret, [0xfeed]);
    ctx.execution_budget = 1000;
    ctx.call_kind = 0;
    assert_eq!(
        entry(&mut ctx, 0, ret.as_mut_ptr(), 1, 0, 0, 0, 0),
        JitResult::Ok
    );
    assert_eq!(ret, [201]);
    stack[2] = 101;
    assert_eq!(
        entry(&mut ctx, 0, ret.as_mut_ptr(), 3, 0, 0, 0, 0),
        JitResult::Ok
    );
    assert_eq!(ret, [101]);
    assert_eq!(stack[2], 101);
}
