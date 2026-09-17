use super::*;
use vo_runtime::jit_api::{JitDispatchEntry, JitTier, PreparedCall};
use vo_runtime::objects::closure;

extern "C" fn record_miss(
    ctx: *mut JitContext,
    _closure: u64,
    _ret_reg: u32,
    _ret_slots: u32,
    _resume_pc: u32,
    _args: *const u64,
    _arg_count: u32,
    _out: *mut PreparedCall,
) -> JitResult {
    // Each invocation owns this counter for the complete synchronous call.
    unsafe {
        *(*ctx).callback_state.cast::<usize>() += 1;
    }
    JitResult::JitError
}

fn module_for_width(width: u16, alias_parameters: bool) -> VoModule {
    let mut module = VoModule::new("dynamic-hidden-arguments".into());
    let mut slots = vec![SlotType::GcBase];
    slots.extend(vec![SlotType::Value; width as usize * 2]);
    let mut caller = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::CallClosure, 0, 1, 0),
            Instruction::new(Opcode::Return, 1 + width, width, 0),
        ],
        slots,
        1 + width,
        1 + width,
        width,
    );
    caller.instruction_metadata[0] = InstructionMetadata::CallLayout {
        arg_layout: vec![SlotType::Value; width as usize],
        ret_layout: vec![SlotType::Value; width as usize],
    };
    module.functions.push(caller);
    for hidden in [0, 1] {
        let mut slots = vec![SlotType::GcBase; hidden as usize];
        slots.extend(vec![SlotType::Value; width as usize]);
        let mut code = Vec::new();
        let mut projection = None;
        let result = if alias_parameters && width != 0 {
            let index = width + hidden;
            let limit = index + 1;
            let destination = limit + 1;
            slots.extend(vec![SlotType::Value; 2 + usize::from(width)]);
            code.push(Instruction::new(Opcode::LoadInt, index, 0, 0));
            code.push(Instruction::new(Opcode::LoadInt, limit, 1, 0));
            code.push(Instruction::new(Opcode::IndexCheck, index, limit, 0));
            projection = Some(code.len());
            code.push(Instruction::new(
                if width == 1 {
                    Opcode::SlotGet
                } else {
                    Opcode::SlotGetN
                },
                destination,
                hidden,
                index,
            ));
            destination
        } else {
            hidden
        };
        code.push(Instruction::new(Opcode::Return, result, width, 0));
        let mut callee =
            make_func_with_slot_types_and_sig(code, slots, width, width + hidden, width);
        if let Some(pc) = projection {
            callee.instruction_metadata[pc] = InstructionMetadata::SlotLayout {
                array_len: 1,
                elem_layout: vec![SlotType::Value; usize::from(width)],
            };
        }
        callee.is_closure = hidden != 0;
        assert!(crate::can_enter_prepared_shadow_frame_for_jit(&callee));
        module.functions.push(callee);
    }
    module
}

#[test]
fn dynamic_ic_executes_zero_and_one_hidden_slots_with_wide_tails_and_generation_guards() {
    for alias_parameters in [false, true] {
        for width in [0, 1, 4, 5, 6] {
            let module = module_for_width(width, alias_parameters);
            let loaded =
                Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
            let mut jit = JitCompiler::new().unwrap();
            jit.bind_loaded_module_scope(loaded).unwrap();
            let externs = ResolvedExternTable::empty();
            let mut gc = vo_runtime::gc::Gc::new();
            let closures = [
                closure::create(&mut gc, 1, 0),
                closure::create(&mut gc, 2, 0),
            ];
            for tier in [JitTier::Baseline, JitTier::Optimizing] {
                for id in 0..3 {
                    jit.compile_loaded_tier(id, default_compile_env(&externs), tier)
                        .unwrap();
                }
                jit.compile_loaded_loop(
                    0,
                    default_compile_env(&externs),
                    &LoopInfo {
                        begin_pc: 0,
                        end_pc: 0,
                        exit_pc: 1,
                    },
                )
                .unwrap();
                let entry = unsafe { jit.get_func_ptr_for_tier(0, tier).unwrap() };
                let osr = unsafe { jit.cache.get_loop_func_ptr(0, 0).unwrap() };
                let mut dispatch = [JitDispatchEntry::unavailable(); 3];
                for (id, target) in dispatch.iter_mut().enumerate() {
                    target.native =
                        unsafe { jit.get_func_ptr_for_tier(id as u32, tier).unwrap() } as *const u8;
                    target.generation = 7;
                }
                for use_osr in [false, true] {
                    for hidden in [0_usize, 1, 0, 1] {
                        for stale in [false, true] {
                            const BP: usize = 3;
                            const SENTINEL: u64 = 0xfeed_beef_dead_abcd;
                            let sp = BP + module.functions[0].local_slots as usize;
                            let mut stack = [SENTINEL; 64];
                            stack[BP] = closures[hidden] as u64;
                            let words: Vec<_> =
                                (0..width).map(|i| (u64::from(i) + 1) * 11).collect();
                            stack[BP + 1..BP + 1 + width as usize].copy_from_slice(&words);
                            let mut parts = JitContextParts::new();
                            let mut ctx = parts.context(&module, &mut stack);
                            let mut misses = 0_usize;
                            ctx.callback_state = core::ptr::from_mut(&mut misses).cast();
                            ctx.prepare_closure_call_fn = Some(record_miss);
                            ctx.gc = &mut gc;
                            ctx.jit_bp = BP as u32;
                            ctx.fiber_sp = sp as u32;
                            ctx.jit_func_table = dispatch.as_ptr();
                            ctx.jit_func_count = 3;
                            let callee_id = 1 + hidden;
                            // Publish both layouts in the same callsite cache. No prepare
                            // callback is permitted on a matching-generation machine-code hit.
                            for offset in 0..2 {
                                let id = 1 + offset;
                                assert!(parts.ic_table[0].publish_native_target(
                                    id as u64,
                                    &PreparedCall {
                                        ic_jit_func_ptr: dispatch[id].native,
                                        callee_local_slots: u32::from(
                                            module.functions[id].local_slots
                                        ),
                                        func_id: id as u32,
                                        dispatch_generation: if stale && id == callee_id {
                                            6
                                        } else {
                                            7
                                        },
                                        jit_frame_elided: u16::from(
                                            crate::can_elide_frame_for_direct_jit(
                                                &module.functions[id]
                                            )
                                        ),
                                        ic_arg_offset: offset as u16,
                                        ..Default::default()
                                    }
                                ));
                            }
                            let mut ret = [SENTINEL; 6];
                            let result = if use_osr {
                                osr(&mut ctx, unsafe { stack.as_mut_ptr().add(BP) })
                            } else {
                                let arg = std::array::from_fn::<_, 5, _>(|i| stack[BP + i]);
                                entry(
                                    &mut ctx,
                                    BP as u64,
                                    ret.as_mut_ptr(),
                                    arg[0],
                                    arg[1],
                                    arg[2],
                                    arg[3],
                                    arg[4],
                                )
                            };
                            let context = format!(
                            "width={width} hidden={hidden} alias={alias_parameters} {tier:?} osr={use_osr} stale={stale}"
                        );
                            if stale {
                                assert_eq!(result, JitResult::JitError, "{context}");
                                assert_eq!(misses, 1, "{context}");
                                assert_eq!(
                                    &stack[sp..],
                                    &[SENTINEL; 64][sp..],
                                    "stale entry must not write the callee frame"
                                );
                            } else {
                                assert_eq!(result, JitResult::Ok, "{context}");
                                assert_eq!(misses, 0, "{context}");
                                let actual = if use_osr {
                                    &stack[BP + 1 + width as usize..BP + 1 + width as usize * 2]
                                } else {
                                    &ret[..width as usize]
                                };
                                assert_eq!(actual, words, "{context}");
                                let first_tail = crate::NATIVE_ARG_LANES
                                    .saturating_sub(hidden)
                                    .min(width as usize);
                                assert_eq!(
                                    &stack[sp + hidden + first_tail..sp + hidden + width as usize],
                                    &words[first_tail..],
                                    "wide tail retains canonical placement"
                                );
                                if alias_parameters {
                                    assert_eq!(
                                        &stack[sp + hidden..sp + hidden + width as usize],
                                        words,
                                        "callee initializes aliased parameters from lanes"
                                    );
                                } else {
                                    assert_eq!(&stack[sp + hidden..sp + hidden + first_tail],
                                    &vec![SENTINEL; first_tail], "ordinary scalar leading lanes avoid a redundant frame copy");
                                }
                                assert_eq!(
                                    stack
                                        [sp + usize::from(module.functions[callee_id].local_slots)],
                                    SENTINEL,
                                    "no write beyond the callee frame"
                                );
                            }
                            assert_eq!(&stack[..BP], &[SENTINEL; BP]);
                            assert_eq!(ctx.jit_bp, BP as u32, "{context}");
                            assert_eq!(ctx.fiber_sp, sp as u32, "{context}");
                            assert_eq!(ctx.call_depth, 0, "{context}");
                        }
                    }
                }
            }
        }
    }
}

#[test]
fn proven_captureless_leaf_inlines_with_zero_hidden_slots_in_functions_and_osr() {
    for width in [0_u16, 1, 6] {
        let mut module = VoModule::new("known-captureless-leaf".into());
        let mut code = vec![Instruction::new(Opcode::ClosureNew, 0, 1, 0)];
        for arg in 0..width {
            code.push(Instruction::new(Opcode::LoadInt, 1 + arg, 77 + arg, 0));
        }
        let call_pc = code.len();
        code.push(Instruction::new(Opcode::CallClosure, 0, 1, 0));
        code.push(Instruction::new(Opcode::Return, 1 + width, 1, 0));
        let mut types = vec![SlotType::GcBase];
        types.extend(vec![SlotType::Value; width as usize + 1]);
        let mut caller = make_func_with_slot_types_and_sig(code, types, 0, 0, 1);
        caller.ret_slot_types = vec![SlotType::Value];
        caller.instruction_metadata[call_pc] = InstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value; width as usize],
            ret_layout: vec![SlotType::Value],
        };
        let mut body = Vec::new();
        if width == 0 {
            body.push(Instruction::new(Opcode::LoadInt, 0, 77, 0));
        }
        body.push(Instruction::new(Opcode::Return, 0, 1, 0));
        let callee = make_func_with_slot_types_and_sig(
            body,
            vec![SlotType::Value; usize::from(width.max(1))],
            width,
            width,
            1,
        );
        module.functions = vec![caller, callee];
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
                end_pc: call_pc,
                exit_pc: call_pc + 1,
            },
        )
        .unwrap();
        for use_osr in [false, true] {
            let mut gc = vo_runtime::gc::Gc::new();
            let mut stack = [0_u64; 64];
            let mut parts = JitContextParts::new();
            let mut ctx = parts.context(&module, &mut stack);
            let mut misses = 0_usize;
            ctx.callback_state = core::ptr::from_mut(&mut misses).cast();
            ctx.prepare_closure_call_fn = Some(record_miss);
            ctx.gc = &mut gc;
            ctx.fiber_sp = u32::from(module.functions[0].local_slots);
            let mut result = [0_u64];
            let status = if use_osr {
                let entry = unsafe { jit.cache.get_loop_func_ptr(0, 0).unwrap() };
                entry(&mut ctx, stack.as_mut_ptr())
            } else {
                let entry = unsafe { jit.get_func_ptr_for_tier(0, JitTier::Optimizing).unwrap() };
                entry(&mut ctx, 0, result.as_mut_ptr(), 0, 0, 0, 0, 0)
            };
            assert_eq!(status, JitResult::Ok, "width={width} osr={use_osr}");
            assert_eq!(
                misses, 0,
                "proven target must use the complete inline recipe"
            );
            assert_eq!(
                if use_osr {
                    stack[1 + width as usize]
                } else {
                    result[0]
                },
                77
            );
            assert_eq!(
                gc.object_count(),
                1,
                "closure allocation remains observable"
            );
        }
    }
}
