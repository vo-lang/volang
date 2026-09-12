use super::*;
use vo_runtime::jit_api::{JitDispatchEntry, JitResult, JitTier, PreparedCall};
use vo_runtime::objects::closure;

extern "C" fn reject_stale(
    ctx: *mut JitContext,
    _closure: u64,
    _ret: u32,
    _slots: u32,
    _pc: u32,
    _args: *const u64,
    _count: u32,
    _out: *mut PreparedCall,
) -> JitResult {
    unsafe {
        *(*ctx).callback_state.cast::<usize>() += 1;
    }
    JitResult::JitError
}

fn arithmetic_module(width: u16, float: bool) -> VoModule {
    let mut module = VoModule::new("feedback-target-change".into());
    let ty = if float {
        SlotType::Float
    } else {
        SlotType::Value
    };
    let mut slots = vec![SlotType::GcBase];
    slots.extend(vec![ty; width as usize + 1]);
    let mut caller = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::CallClosure, 0, 1, 0),
            Instruction::new(Opcode::Return, 1 + width, 1, 0),
        ],
        slots,
        1 + width,
        1 + width,
        1,
    );
    caller.ret_slot_types = vec![ty];
    caller.instruction_metadata[0] = InstructionMetadata::CallLayout {
        arg_layout: vec![ty; width as usize],
        ret_layout: vec![ty],
    };
    module.functions.push(caller);
    for plus in [true, false] {
        let op = match (float, plus) {
            (false, true) => Opcode::AddI,
            (false, false) => Opcode::SubI,
            (true, true) => Opcode::AddF,
            (true, false) => Opcode::SubF,
        };
        let mut callee = make_func_with_slot_types_and_sig(
            vec![
                Instruction::new(op, 0, 0, width - 1),
                Instruction::new(Opcode::Return, 0, 1, 0),
            ],
            vec![ty; width as usize],
            width,
            width,
            1,
        );
        callee.ret_slot_types = vec![ty];
        module.functions.push(callee);
    }
    module
}

#[test]
fn feedback_inline_executes_real_code_and_falls_back_when_targets_or_generations_change() {
    for width in [2_u16, 6] {
        for float in [false, true] {
            for observed in [vec![], vec![1_u32], vec![1, 2], vec![999]] {
                let module = arithmetic_module(width, float);
                let loaded = Arc::new(
                    vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap(),
                );
                let mut jit = JitCompiler::new().unwrap();
                jit.bind_loaded_module_scope(loaded).unwrap();
                let externs = ResolvedExternTable::empty();
                for id in 1..3 {
                    jit.compile_loaded_tier(id, default_compile_env(&externs), JitTier::Baseline)
                        .unwrap();
                }
                let mut feedback = alloc_ic_table(1);
                for id in &observed {
                    feedback[0].publish_interpreter_target(
                        u64::from(*id),
                        vo_runtime::DynamicCallTarget {
                            func_id: *id,
                            local_slots: width,
                        },
                    );
                }
                jit.compile_loaded_tier_with_feedback(
                    0,
                    default_compile_env(&externs),
                    JitTier::Optimizing,
                    &feedback,
                )
                .unwrap();
                let entry = unsafe { jit.get_func_ptr_for_tier(0, JitTier::Optimizing).unwrap() };
                let mut gc = vo_runtime::gc::Gc::new();
                let closures = [
                    closure::create(&mut gc, 1, 0),
                    closure::create(&mut gc, 2, 0),
                ];
                let mut dispatch = [JitDispatchEntry::unavailable(); 3];
                for id in 1..3 {
                    dispatch[id].native = unsafe {
                        jit.get_func_ptr_for_tier(id as u32, JitTier::Baseline)
                            .unwrap()
                    } as *const u8;
                    dispatch[id].generation = 7;
                }
                for target in [1_usize, 2, 1] {
                    for stale in [false, true] {
                        const BP: usize = 3;
                        const SENTINEL: u64 = 0xfeed_beef_dead_abcd;
                        let mut stack = [SENTINEL; 96];
                        let sp = BP + module.functions[0].local_slots as usize;
                        stack[BP] = closures[target - 1] as u64;
                        for index in 0..width as usize {
                            stack[BP + 1 + index] = if float {
                                (index as f64 + 1.25).to_bits()
                            } else {
                                index as u64 + 11
                            };
                        }
                        let a = stack[BP + 1];
                        let b = stack[BP + width as usize];
                        let expected = if float {
                            let a = f64::from_bits(a);
                            let b = f64::from_bits(b);
                            (if target == 1 { a + b } else { a - b }).to_bits()
                        } else if target == 1 {
                            a.wrapping_add(b)
                        } else {
                            a.wrapping_sub(b)
                        };
                        let mut parts = JitContextParts::new();
                        let mut ctx = parts.context(&module, &mut stack);
                        let mut misses = 0_usize;
                        ctx.callback_state = core::ptr::from_mut(&mut misses).cast();
                        ctx.prepare_closure_call_fn = Some(reject_stale);
                        ctx.gc = &mut gc;
                        ctx.jit_bp = BP as u32;
                        ctx.fiber_sp = sp as u32;
                        ctx.jit_func_table = dispatch.as_ptr();
                        ctx.jit_func_count = 3;
                        for id in 1..3 {
                            assert!(parts.ic_table[0].publish_native_target(
                                id as u64,
                                &PreparedCall {
                                    ic_jit_func_ptr: dispatch[id].native,
                                    callee_local_slots: u32::from(width),
                                    func_id: id as u32,
                                    dispatch_generation: if stale && id == target { 6 } else { 7 },
                                    jit_frame_elided: 1,
                                    ic_arg_offset: 0,
                                    ..Default::default()
                                }
                            ));
                        }
                        let lanes = std::array::from_fn::<_, 5, _>(|i| stack[BP + i]);
                        let mut ret = [SENTINEL];
                        let before = ctx.execution_budget;
                        let result = entry(
                            &mut ctx,
                            BP as u64,
                            ret.as_mut_ptr(),
                            lanes[0],
                            lanes[1],
                            lanes[2],
                            lanes[3],
                            lanes[4],
                        );
                        let label=format!("width={width} float={float} observed={observed:?} target={target} stale={stale}");
                        assert_eq!(parts.profiles[0].optimizing_entered, 1, "{label}");
                        if stale {
                            assert_eq!(result, JitResult::JitError, "{label}");
                            assert_eq!(misses, 1, "{label}");
                            assert_eq!(ret[0], SENTINEL);
                        } else {
                            assert_eq!(result, JitResult::Ok, "{label}");
                            assert_eq!(ret[0], expected, "{label}");
                            assert_eq!(misses, 0);
                            let inlined = observed == [1] && target == 1;
                            assert_eq!(
                                parts.profiles[target].entries,
                                u64::from(!inlined),
                                "real callee entry: {label}"
                            );
                            assert_eq!(
                                before - ctx.execution_budget,
                                4,
                                "same caller plus expanded callee work: {label}"
                            );
                        }
                        assert_eq!(ctx.call_depth, 0);
                        assert_eq!(ctx.jit_bp, BP as u32);
                        assert_eq!(ctx.fiber_sp, sp as u32);
                        assert_eq!(&stack[..BP], &[SENTINEL; BP]);
                        assert_eq!(stack[sp + width as usize], SENTINEL);
                    }
                }
            }
        }
    }
}

#[derive(Default)]
struct FeedbackBoundaryTrace {
    misses: usize,
    calls: Vec<[u32; 7]>,
}

extern "C" fn record_feedback_miss(
    ctx: *mut JitContext,
    _closure: u64,
    _ret: u32,
    _slots: u32,
    _pc: u32,
    _args: *const u64,
    _count: u32,
    _out: *mut PreparedCall,
) -> JitResult {
    unsafe {
        (*(*ctx).callback_state.cast::<FeedbackBoundaryTrace>()).misses += 1;
    }
    JitResult::JitError
}

extern "C" fn record_feedback_frame(
    ctx: *mut JitContext,
    function: u32,
    slots: u32,
    ret: u32,
    returns: u32,
    resume: u32,
) -> *mut u64 {
    unsafe {
        let context = &mut *ctx;
        let trace = &mut *context.callback_state.cast::<FeedbackBoundaryTrace>();
        trace
            .calls
            .push([0, function, slots, ret, returns, resume, context.fiber_sp]);
        let bp = context.fiber_sp;
        let Some(end) = bp.checked_add(slots) else {
            return core::ptr::null_mut();
        };
        if end > context.stack_cap {
            return core::ptr::null_mut();
        }
        context.jit_bp = bp;
        context.fiber_sp = end;
        context.stack_ptr.add(bp as usize)
    }
}

extern "C" fn record_feedback_resume(
    ctx: *mut JitContext,
    function: u32,
    resume: u32,
    bp: u32,
    caller: u32,
    ret: u32,
    returns: u32,
) -> JitResult {
    unsafe {
        (*(*ctx).callback_state.cast::<FeedbackBoundaryTrace>())
            .calls
            .push([1, function, resume, bp, caller, ret, returns]);
    }
    JitResult::Ok
}

#[test]
fn feedback_scalar_inline_retains_low_fuel_fallback_and_complete_return_windows() {
    const SENTINEL: u64 = 0xfeed_beef_dead_abcd;
    for returns in [0_u16, 1, 2] {
        let mut module = arithmetic_module(2, false);
        let caller = &mut module.functions[0];
        caller.slot_types = vec![SlotType::GcBase, SlotType::Value, SlotType::Value];
        caller
            .slot_types
            .extend(vec![SlotType::Value; usize::from(returns)]);
        caller.local_slots = 3 + returns;
        caller.ret_slots = returns;
        caller.ret_slot_types = vec![SlotType::Value; usize::from(returns)];
        caller.code[1] = Instruction::new(Opcode::Return, 3, returns, 0);
        caller.instruction_metadata[0] = InstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value; 2],
            ret_layout: caller.ret_slot_types.clone(),
        };
        for callee in &mut module.functions[1..] {
            callee.ret_slots = returns;
            callee.ret_slot_types = vec![SlotType::Value; usize::from(returns)];
            callee.code[1] = Instruction::new(Opcode::Return, 0, returns, 0);
        }
        let loaded =
            Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
        let externs = ResolvedExternTable::empty();
        let mut gc = vo_runtime::gc::Gc::new();
        let function = closure::create(&mut gc, 1, 0);
        let mut observations = Vec::new();
        for feedback_enabled in [false, true] {
            let mut jit = JitCompiler::new().unwrap();
            jit.bind_loaded_module_scope(Arc::clone(&loaded)).unwrap();
            jit.compile_loaded_tier(1, default_compile_env(&externs), JitTier::Baseline)
                .unwrap();
            let mut feedback = alloc_ic_table(1);
            if feedback_enabled {
                feedback[0].publish_interpreter_target(
                    1,
                    vo_runtime::DynamicCallTarget {
                        func_id: 1,
                        local_slots: 2,
                    },
                );
            }
            jit.compile_loaded_tier_with_feedback(
                0,
                default_compile_env(&externs),
                JitTier::Optimizing,
                &feedback,
            )
            .unwrap();
            let entry = unsafe { jit.get_func_ptr_for_tier(0, JitTier::Optimizing).unwrap() };
            let mut dispatch = [JitDispatchEntry::unavailable(); 3];
            dispatch[1].native =
                unsafe { jit.get_func_ptr_for_tier(1, JitTier::Baseline).unwrap() } as *const u8;
            dispatch[1].generation = 7;
            let mut rows = Vec::new();
            for budget in 0..=6 {
                const BP: usize = 3;
                let mut stack = [SENTINEL; 64];
                let words = [function as u64, 11, 12];
                stack[BP..BP + 3].copy_from_slice(&words);
                let mut parts = JitContextParts::new();
                let mut ctx = parts.context(&module, &mut stack);
                let mut trace = FeedbackBoundaryTrace::default();
                ctx.callback_state = core::ptr::from_mut(&mut trace).cast();
                ctx.prepare_closure_call_fn = Some(record_feedback_miss);
                ctx.push_frame_fn = Some(record_feedback_frame);
                ctx.push_resume_point_fn = Some(record_feedback_resume);
                ctx.gc = &mut gc;
                ctx.jit_bp = BP as u32;
                ctx.fiber_sp = (BP + usize::from(module.functions[0].local_slots)) as u32;
                ctx.jit_func_table = dispatch.as_ptr();
                ctx.jit_func_count = 3;
                ctx.execution_budget = budget;
                assert!(parts.ic_table[0].publish_native_target(
                    1,
                    &PreparedCall {
                        ic_jit_func_ptr: dispatch[1].native,
                        callee_local_slots: 2,
                        func_id: 1,
                        dispatch_generation: 7,
                        jit_frame_elided: 1,
                        ic_arg_offset: 0,
                        ..Default::default()
                    }
                ));
                let mut result = [SENTINEL; 3];
                let status = entry(
                    &mut ctx,
                    BP as u64,
                    result.as_mut_ptr(),
                    words[0],
                    words[1],
                    words[2],
                    SENTINEL,
                    SENTINEL,
                );
                let label =
                    format!("returns={returns} feedback={feedback_enabled} budget={budget}");
                assert_eq!(trace.misses, 0, "{label}");
                assert_eq!(
                    result[usize::from(returns)],
                    SENTINEL,
                    "return boundary: {label}"
                );
                assert_eq!(&stack[..BP], &[SENTINEL; BP], "{label}");
                if budget >= 4 {
                    assert_eq!(status, JitResult::Ok, "{label}");
                    assert_eq!(
                        &result[..usize::from(returns)],
                        &[23, 12][..usize::from(returns)],
                        "{label}"
                    );
                    assert_eq!(
                        parts.profiles[1].entries,
                        u64::from(!feedback_enabled),
                        "actual callee: {label}"
                    );
                }
                rows.push((
                    (
                        status,
                        result,
                        ctx.execution_budget,
                        ctx.call_kind,
                        ctx.call_func_id,
                        ctx.call_resume_pc,
                        ctx.call_callee_bp,
                        ctx.call_ret_reg,
                        ctx.call_ret_slots,
                        ctx.call_depth,
                        ctx.jit_bp,
                        ctx.fiber_sp,
                    ),
                    trace.calls,
                ));
            }
            observations.push(rows);
        }
        assert_eq!(
            observations[0], observations[1],
            "same budget exit and recovery contract; returns={returns}"
        );
    }
}

#[test]
fn scalar_copy_windows_preserve_overlapping_sources_in_native_inline_recipes() {
    use crate::call_helpers::SmallFunctionInline;
    for float in [false, true] {
        let ty = if float {
            SlotType::Float
        } else {
            SlotType::Value
        };
        for (destination, source) in [(1_u16, 0_u16), (0, 1), (0, 0)] {
            let mut module = VoModule::new("overlapping-inline-copy".into());
            let mut caller = make_func_with_slot_types_and_sig(
                vec![
                    Instruction::new(Opcode::Call, 1, 0, 0),
                    Instruction::new(Opcode::Return, 3, 2, 0),
                ],
                vec![ty; 5],
                3,
                3,
                2,
            );
            caller.ret_slot_types = vec![ty; 2];
            let mut leaf = make_func_with_slot_types_and_sig(
                vec![
                    Instruction::new(Opcode::CopyN, destination, source, 2),
                    Instruction::new(Opcode::Return, destination, 2, 0),
                ],
                vec![ty; 3],
                3,
                3,
                2,
            );
            leaf.ret_slot_types = vec![ty; 2];
            module.functions = vec![caller, leaf];
            let recipe =
                SmallFunctionInline::analyze_leaf(1, &module.functions[1], &module).unwrap();
            assert_eq!(recipe.duplication_work(), 4);
            let mut malformed = module.functions[1].clone();
            malformed.code[0].c = u16::MAX;
            assert!(SmallFunctionInline::analyze_leaf(1, &malformed, &module).is_none());
            if destination != source {
                malformed = module.functions[1].clone();
                malformed.slot_types[0] = if float {
                    SlotType::Value
                } else {
                    SlotType::Float
                };
                assert!(SmallFunctionInline::analyze_leaf(1, &malformed, &module).is_none());
            }
            let loaded =
                Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
            let mut compiler = JitCompiler::new().unwrap();
            compiler.bind_loaded_module_scope(loaded).unwrap();
            let externs = ResolvedExternTable::empty();
            for tier in [JitTier::Baseline, JitTier::Optimizing] {
                compiler
                    .compile_loaded_tier(0, default_compile_env(&externs), tier)
                    .unwrap();
                let entry = unsafe { compiler.get_func_ptr_for_tier(0, tier).unwrap() };
                let inputs = if float {
                    [1.25_f64.to_bits(), 2.5_f64.to_bits(), 3.75_f64.to_bits()]
                } else {
                    [11, 22, 33]
                };
                let mut stack = [0_u64; 32];
                stack[..3].copy_from_slice(&inputs);
                let mut parts = JitContextParts::new();
                let mut ctx = parts.context(&module, &mut stack);
                let mut ret = [0_u64; 2];
                // No callee dispatch entry is installed: this executes the
                // complete inline recipe and cannot fall back to a nested call.
                let status =
                    unsafe { crate::invoke_test_jit(entry, &mut ctx, &mut stack, &mut ret) };
                assert_eq!(status, JitResult::Ok);
                assert_eq!(ret, inputs[usize::from(source)..usize::from(source) + 2]);
            }
        }
    }
}
