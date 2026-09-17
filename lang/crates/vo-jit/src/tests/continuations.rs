use super::*;

#[test]
fn static_continuations_use_each_entry_frame_and_preserve_budget_recovery() {
    for optimize in [false, true] {
        let function = make_func_with_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 0, 7, 0),
                Instruction::new(Opcode::Jump, 0, 1, 0),
                Instruction::new(Opcode::AddI, 2, 0, 1),
                Instruction::new(Opcode::AddI, 3, 0, 1),
                Instruction::new(Opcode::MulI, 4, 2, 3),
                Instruction::new(Opcode::Return, 4, 1, 0),
            ],
            2,
            2,
            5,
            1,
        );
        let mut module = VoModule::new("static-continuation-entries".into());
        module.functions.push(function);
        let pcs = [2, 5];
        let externs = ResolvedExternTable::empty();
        let env = default_compile_env(&externs);
        let mut jit = JitCompiler::new().unwrap();
        jit.verify_module_once(&module).unwrap();
        jit.verify_env_once(env).unwrap();
        let module_analysis = jit.module_analysis(&module, env).unwrap();
        let ordinary_analysis = crate::analysis::FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            crate::MAX_JIT_ANALYSIS_BYTES,
        )
        .unwrap();
        let resume_analysis = crate::analysis::FunctionAnalysis::try_for_continuations(
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
        assert_eq!(resume_analysis.is_some(), optimize);
        let analysis = resume_analysis.as_ref().unwrap_or(&ordinary_analysis);
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
                "continuation_probe",
                cranelift_module::Linkage::Local,
                &jit.ctx.func.signature,
            )
            .unwrap();
        let staged = jit.stage_function(id, "continuation probe").unwrap();
        let (code, _) = jit.publish_function_artifact(staged).unwrap();
        let entry: NativeJitFunc = unsafe { std::mem::transmute(code) };

        let mut frame = [40, 2, 19, 23, 101];
        let mut ret = [0xfeed];
        let mut parts = JitContextParts::new();
        let mut ctx = parts.context(&module, &mut frame);
        // A continuation's lane zero is the resume PC, even if a baseline
        // entry-profile callback is requested before frame values are loaded.
        ctx.optimizing_threshold = 1;
        ctx.execution_budget = 0;
        assert_eq!(
            entry(&mut ctx, 0, ret.as_mut_ptr(), 2, 0, 0, 0, 0),
            JitResult::Call
        );
        assert_eq!(ctx.call_kind, JitContext::CALL_KIND_YIELD);
        assert_eq!(ctx.call_resume_pc, 2);
        assert_eq!(ret, [0xfeed]);
        ctx.execution_budget = 100;
        assert_eq!(
            entry(&mut ctx, 0, ret.as_mut_ptr(), 2, 0, 0, 0, 0),
            JitResult::Ok
        );
        assert_eq!(ret, [1764], "optimize={optimize}");

        frame[4] = 101;
        ctx.stack_ptr = frame.as_mut_ptr();
        assert_eq!(
            entry(&mut ctx, 0, ret.as_mut_ptr(), 5, 0, 0, 0, 0),
            JitResult::Ok
        );
        assert_eq!(ret, [101], "independent late entry, optimize={optimize}");
        ret[0] = 0xfeed;
        assert_eq!(
            entry(&mut ctx, 0, ret.as_mut_ptr(), 0, 0, 0, 0, 0),
            JitResult::JitError
        );
        assert_eq!(ret, [0xfeed]);
    }
}
