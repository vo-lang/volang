use super::*;
use vo_runtime::jit_api::JitTier;

fn expected(opcode: Opcode, a: u32, b: u32) -> u64 {
    let (a, b) = (f32::from_bits(a), f32::from_bits(b));
    let value = match opcode {
        Opcode::AddF32 => a + b,
        Opcode::SubF32 => a - b,
        Opcode::MulF32 => a * b,
        Opcode::DivF32 => a / b,
        Opcode::NegF32 => -a,
        Opcode::EqF32 => return u64::from(a == b),
        Opcode::NeF32 => return u64::from(a != b),
        Opcode::LtF32 => return u64::from(a < b),
        Opcode::LeF32 => return u64::from(a <= b),
        Opcode::GtF32 => return u64::from(a > b),
        Opcode::GeF32 => return u64::from(a >= b),
        _ => unreachable!(),
    };
    u64::from(value.to_bits())
}
fn check(actual: u64, expected: u64) {
    assert_eq!(actual >> 32, 0, "F32 results clear the high slot bits");
    if f32::from_bits(expected as u32).is_nan() {
        assert!(f32::from_bits(actual as u32).is_nan());
    } else {
        assert_eq!(actual, expected);
    }
}

#[test]
fn float32_direct_calls_leaf_recipes_and_osr_preserve_low_slot_bits() {
    let pairs = [
        (0, 0x80000000),
        (1, 2),
        (0x007fffff, 1),
        (0x00800001, 0x40000000),
        (0x3f800000, 0x33800000),
        (0x3f800001, 0x33800000),
        (0x7f7fffff, 0x40000000),
        (0x7f800000, 0xff800000),
        (0x7fc00001, 0x3f800000),
        (0xff800001, 0x3f800000),
        (0x3f800001, 0x3f7ffffe),
    ];
    for opcode in [
        Opcode::AddF32,
        Opcode::SubF32,
        Opcode::MulF32,
        Opcode::DivF32,
        Opcode::NegF32,
        Opcode::EqF32,
        Opcode::NeF32,
        Opcode::LtF32,
        Opcode::LeF32,
        Opcode::GtF32,
        Opcode::GeF32,
    ] {
        for operand in [SlotType::Value, SlotType::Float] {
            let comparison = matches!(
                opcode,
                Opcode::EqF32
                    | Opcode::NeF32
                    | Opcode::LtF32
                    | Opcode::LeF32
                    | Opcode::GtF32
                    | Opcode::GeF32
            );
            let destination = if comparison { SlotType::Value } else { operand };
            let mut leaf = make_func_with_slot_types_and_sig(
                vec![
                    Instruction::new(opcode, 2, 0, 1),
                    Instruction::new(Opcode::Return, 2, 1, 0),
                ],
                vec![operand, operand, destination],
                2,
                2,
                1,
            );
            leaf.ret_slot_types = vec![destination];
            let mut caller = make_func_with_slot_types_and_sig(
                vec![
                    Instruction::new(Opcode::Call, 1, 0, 0),
                    Instruction::new(Opcode::Return, 2, 1, 0),
                ],
                vec![operand, operand, destination],
                2,
                2,
                1,
            );
            caller.ret_slot_types = vec![destination];
            let mut module = VoModule::new("f32-native-slots".into());
            module.functions = vec![caller, leaf];
            assert!(crate::call_helpers::SmallFunctionInline::analyze_leaf(
                1,
                &module.functions[1],
                &module
            )
            .is_some());
            let loaded =
                Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
            let externs = ResolvedExternTable::empty();
            let mut jit = JitCompiler::new().unwrap();
            jit.bind_loaded_module_scope(loaded).unwrap();
            for tier in [JitTier::Baseline, JitTier::Optimizing] {
                for id in [0, 1] {
                    jit.compile_loaded_tier(id, default_compile_env(&externs), tier)
                        .unwrap();
                }
                let mut dispatch = [vo_runtime::jit_api::JitDispatchEntry::unavailable(); 2];
                for id in [0, 1] {
                    dispatch[id].native =
                        unsafe { jit.get_func_ptr_for_tier(id as u32, tier).unwrap() } as *const u8;
                }
                for id in [0, 1] {
                    let entry = unsafe { jit.get_func_ptr_for_tier(id, tier).unwrap() };
                    for (a, b) in pairs {
                        let mut stack = [0; 16];
                        stack[0] = u64::from(a) | 0xdeadbeef00000000;
                        stack[1] = u64::from(b) | 0xabcdef1200000000;
                        let mut parts = JitContextParts::new();
                        let mut ctx = parts.context(&module, &mut stack);
                        ctx.current_func_id = id;
                        ctx.fiber_sp = 3;
                        ctx.jit_func_table = dispatch.as_ptr();
                        ctx.jit_func_count = 2;
                        let mut ret = [0];
                        assert_eq!(
                            unsafe {
                                crate::invoke_test_jit(entry, &mut ctx, &mut stack, &mut ret)
                            },
                            JitResult::Ok,
                            "{opcode:?} {tier:?} {operand:?} entry {id}"
                        );
                        check(ret[0], expected(opcode, a, b));
                    }
                }
            }
            jit.compile_loaded_loop(
                1,
                default_compile_env(&externs),
                &LoopInfo {
                    begin_pc: 0,
                    end_pc: 0,
                    exit_pc: 1,
                },
            )
            .unwrap();
            let entry = unsafe { jit.cache.get_loop_func_ptr(1, 0).unwrap() };
            for (a, b) in pairs {
                let mut stack = [
                    u64::from(a) | 0xdeadbeef00000000,
                    u64::from(b) | 0xabcdef1200000000,
                    0,
                ];
                let mut parts = JitContextParts::new();
                let mut ctx = parts.context(&module, &mut stack);
                ctx.current_func_id = 1;
                assert_eq!(entry(&mut ctx, stack.as_mut_ptr()), JitResult::Ok);
                assert_eq!(ctx.loop_exit_pc, 1);
                check(stack[2], expected(opcode, a, b));
            }
        }
    }
}
