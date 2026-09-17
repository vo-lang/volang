use super::*;
use vo_runtime::jit_api::JitTier;

fn with_tiers(module: &VoModule, mut check: impl FnMut(&mut JitCompiler, JitTier)) {
    let loaded = Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().unwrap();
    jit.bind_loaded_module_scope(loaded).unwrap();
    for tier in [JitTier::Baseline, JitTier::Optimizing] {
        for id in 0..module.functions.len() {
            jit.compile_loaded_tier(id as u32, default_compile_env(&externs), tier)
                .unwrap();
        }
        check(&mut jit, tier);
    }
}

fn indexed_parameter() -> FunctionDef {
    let mut function = make_func_with_sig(
        vec![
            Instruction::new(Opcode::LoadInt, 8, 6, 0),
            Instruction::new(Opcode::IndexCheck, 6, 8, 0),
            Instruction::new(Opcode::SlotGet, 7, 0, 6),
            Instruction::new(Opcode::Return, 7, 1, 0),
        ],
        2,
        7,
        9,
        1,
    );
    function.instruction_metadata[2] = InstructionMetadata::SlotLayout {
        array_len: 6,
        elem_layout: vec![SlotType::Value],
    };
    function
}

#[test]
fn alias_backed_parameters_import_native_lanes_before_indexing() {
    let mut module = VoModule::new("native-aliased-parameters".into());
    module.functions.push(indexed_parameter());
    with_tiers(&module, |jit, tier| {
        let entry = unsafe { jit.get_func_ptr_for_tier(0, tier).unwrap() };
        const BP: usize = 3;
        for index in 0..6 {
            let mut stack = [999; BP + 9];
            stack[BP + 5] = 66;
            stack[BP + 6] = index;
            let mut ret = [0];
            let mut parts = JitContextParts::new();
            let mut ctx = parts.context(&module, &mut stack);
            ctx.jit_bp = BP as u32;
            assert_eq!(
                entry(&mut ctx, BP as u64, ret.as_mut_ptr(), 11, 22, 33, 44, 55),
                JitResult::Ok,
                "{tier:?} index {index}",
            );
            assert_eq!(ret[0], (index + 1) * 11);
            assert_eq!(&stack[..BP], &[999; BP], "preserve the caller window");
        }
    });
}

#[test]
fn static_calls_handoff_aliased_leading_and_wide_tail_parameters() {
    let mut module = VoModule::new("native-static-wide-parameters".into());
    module.functions.push(make_func_with_sig(
        vec![
            Instruction::new(Opcode::Call, 1, 0, 0),
            Instruction::new(Opcode::Return, 7, 1, 0),
        ],
        2,
        7,
        8,
        1,
    ));
    module.functions.push(indexed_parameter());
    with_tiers(&module, |jit, tier| {
        let entry = unsafe { jit.get_func_ptr_for_tier(0, tier).unwrap() };
        let mut dispatch = [vo_runtime::jit_api::JitDispatchEntry::unavailable(); 2];
        for (id, target) in dispatch.iter_mut().enumerate() {
            target.native =
                unsafe { jit.get_func_ptr_for_tier(id as u32, tier).unwrap() } as *const u8;
        }
        for index in 0..6 {
            let mut stack = [999; 32];
            stack[..7].copy_from_slice(&[11, 22, 33, 44, 55, 66, index]);
            let mut ret = [0];
            let mut parts = JitContextParts::new();
            let mut ctx = parts.context(&module, &mut stack);
            ctx.fiber_sp = 8;
            ctx.jit_func_table = dispatch.as_ptr();
            ctx.jit_func_count = 2;
            assert_eq!(
                unsafe { crate::invoke_test_jit(entry, &mut ctx, &mut stack, &mut ret) },
                JitResult::Ok,
                "{tier:?} index {index}",
            );
            assert_eq!(ret[0], (index + 1) * 11);
            assert_eq!(ctx.call_depth, 0);
        }
    });
}

#[test]
fn osr_calls_preserve_nonzero_activation_and_wide_arguments() {
    let mut module = VoModule::new("osr-activation-wide-parameters".into());
    module.functions.push(make_func_with_sig(
        vec![
            Instruction::new(Opcode::Call, 1, 0, 0),
            Instruction::new(Opcode::AddI, 8, 7, 7),
            Instruction::new(Opcode::Call, 1, 0, 0),
            Instruction::new(Opcode::AddI, 8, 8, 7),
            Instruction::new(Opcode::Return, 8, 1, 0),
        ],
        2,
        7,
        9,
        1,
    ));
    module.functions.push(indexed_parameter());
    with_tiers(&module, |jit, tier| {
        let externs = ResolvedExternTable::empty();
        jit.compile_loaded_loop(
            0,
            default_compile_env(&externs),
            &LoopInfo {
                begin_pc: 0,
                end_pc: 3,
                exit_pc: 4,
            },
        )
        .unwrap();
        let entry = unsafe { jit.cache.get_loop_func_ptr(0, 0).unwrap() };
        let mut dispatch = [vo_runtime::jit_api::JitDispatchEntry::unavailable(); 2];
        dispatch[1].native = unsafe { jit.get_func_ptr_for_tier(1, tier).unwrap() } as *const u8;
        const BP: usize = 3;
        const SP: usize = BP + 9;
        for index in 0..6 {
            for budget in [100, 0] {
                let mut stack = [999; SP + 9];
                let words = [11, 22, 33, 44, 55, 66, index];
                stack[BP..BP + 7].copy_from_slice(&words);
                let mut parts = JitContextParts::new();
                let mut ctx = parts.context(&module, &mut stack);
                ctx.jit_bp = BP as u32;
                ctx.fiber_sp = SP as u32;
                ctx.jit_func_table = dispatch.as_ptr();
                ctx.jit_func_count = 2;
                ctx.execution_budget = budget;
                let result = entry(&mut ctx, unsafe { stack.as_mut_ptr().add(BP) });
                if budget == 0 {
                    assert_eq!(result, JitResult::Call);
                    assert_eq!(ctx.call_kind, JitContext::CALL_KIND_YIELD);
                    assert_eq!(ctx.call_resume_pc, 0);
                    assert_eq!(stack[BP + 8], 999, "yield precedes both calls");
                } else {
                    assert_eq!(result, JitResult::Ok, "{tier:?} index {index}");
                    assert_eq!(ctx.loop_exit_pc, 4);
                    assert_eq!(stack[BP + 8], (index + 1) * 33);
                }
                assert_eq!(&stack[..BP], &[999; BP]);
                assert_eq!(&stack[BP..BP + 7], &words);
                assert_eq!(ctx.jit_bp, BP as u32);
                assert_eq!(ctx.fiber_sp, SP as u32);
                assert_eq!(ctx.call_depth, 0);
            }
        }
    });
}

fn mixed_parameters() -> (VoModule, [u64; 7]) {
    let types = vec![
        SlotType::Float,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
    ];
    let mut function = make_func_with_slot_types_and_sig(
        vec![Instruction::new(Opcode::Return, 0, 7, 0)],
        types.clone(),
        6,
        7,
        7,
    );
    function.ret_slot_types = types;
    let mut module = VoModule::new("native-mixed-parameters".into());
    module.functions.push(function);
    let interface = InterfaceSlot::from_i64(-91);
    // The interface header crosses in the final native lane; its payload is
    // the first wide-tail word. Preserve float raw bits including signed zero.
    let words = [
        (-0.0_f64).to_bits(),
        22,
        33,
        44,
        interface.slot0,
        interface.slot1,
        77,
    ];
    (module, words)
}

#[test]
fn mixed_lane_tail_boundary_preserves_returns_and_budget_recovery() {
    let (module, words) = mixed_parameters();
    with_tiers(&module, |jit, tier| {
        let entry = unsafe { jit.get_func_ptr_for_tier(0, tier).unwrap() };
        for budget in [100, 0] {
            let mut stack = [999; 7];
            stack[5..].copy_from_slice(&words[5..]);
            let mut ret = [123; 7];
            let mut parts = JitContextParts::new();
            let mut ctx = parts.context(&module, &mut stack);
            ctx.execution_budget = budget;
            let result = entry(
                &mut ctx,
                0,
                ret.as_mut_ptr(),
                words[0],
                words[1],
                words[2],
                words[3],
                words[4],
            );
            if budget != 0 {
                assert_eq!(result, JitResult::Ok);
                assert_eq!(ret, words, "{tier:?}");
            } else {
                assert_eq!(result, JitResult::Call);
                assert_eq!(ctx.call_kind, JitContext::CALL_KIND_YIELD);
                assert_eq!(ctx.call_resume_pc, 0);
                assert_eq!(
                    stack, words,
                    "{tier:?} must publish complete recovery words"
                );
                assert_eq!(ret, [123; 7], "yield precedes the guest return");
            }
        }
    });
}

#[test]
fn rejected_tier_up_preserves_register_only_entry_parameters() {
    extern "C" fn reject(_ctx: *mut JitContext, _func_id: u32) -> JitResult {
        JitResult::JitError
    }
    let (module, words) = mixed_parameters();
    with_tiers(&module, |jit, tier| {
        if tier != JitTier::Baseline {
            return;
        }
        let entry = unsafe { jit.get_func_ptr_for_tier(0, tier).unwrap() };
        let mut stack = [999; 7];
        stack[5..].copy_from_slice(&words[5..]);
        let mut ret = [123; 7];
        let mut parts = JitContextParts::new();
        parts.callbacks.tier_up_fn = Some(reject);
        let mut ctx = parts.context(&module, &mut stack);
        ctx.optimizing_threshold = 1;
        assert_eq!(
            entry(
                &mut ctx,
                0,
                ret.as_mut_ptr(),
                words[0],
                words[1],
                words[2],
                words[3],
                words[4]
            ),
            JitResult::JitError,
        );
        assert_eq!(stack, words);
        assert_eq!(ret, [123; 7]);
    });
}
