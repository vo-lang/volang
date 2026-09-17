use super::*;
use vo_common_core::debug_info::InstructionSource;
use vo_runtime::jit_api::{JitRuntimeTrapKind, JitTier};

#[test]
fn inline_and_ordinary_nil_traps_keep_distinct_sources_in_function_and_osr_code() {
    let mut caller = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::Call, 1, 1, 0),
            Instruction::new(Opcode::PtrGet, 3, 0, 0),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ],
        vec![
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ],
        2,
        2,
        1,
    );
    let mut leaf = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::LoadInt, 1, 7, 0),
            Instruction::new(Opcode::PtrGet, 1, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ],
        vec![SlotType::GcRef, SlotType::Value],
        1,
        1,
        1,
    );
    for function in [&mut caller, &mut leaf] {
        function.instruction_metadata[1] = InstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Value],
        };
    }
    let mut module = VoModule::new("inline-trap-origin".into());
    module.functions = vec![caller, leaf];
    module.struct_metas.push(vo_runtime::bytecode::StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    let loaded = Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
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
    // The leaf has no published native entry: a successful inline trap proves
    // this actual machine-code invocation expanded the admitted recipe.
    assert!(unsafe { jit.get_func_ptr_for_tier(1, JitTier::Optimizing) }.is_none());
    let function = unsafe { jit.get_func_ptr_for_tier(0, JitTier::Optimizing).unwrap() };
    let osr = unsafe { jit.cache.get_loop_func_ptr(0, 0).unwrap() };
    let mut gc = vo_runtime::gc::Gc::new();
    let object = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    unsafe {
        object.write(42);
    }
    for use_osr in [false, true] {
        let mut stack = [0; 16];
        let mut parts = JitContextParts::new();
        let mut ctx = parts.context(&module, &mut stack);
        ctx.gc = &mut gc;
        ctx.current_func_id = 0;
        ctx.fiber_sp = 4;
        let mut ret = [0];
        for inline_nil in [true, false, true, false] {
            stack[0] = if inline_nil { object as u64 } else { 0 };
            stack[1] = if inline_nil { 0 } else { object as u64 };
            unsafe {
                *ctx.panic_flag = false;
                *ctx.is_user_panic = false;
            }
            // Deliberately keep the previous trap's origin in this context.
            let result = if use_osr {
                osr(&mut ctx, stack.as_mut_ptr())
            } else {
                unsafe { crate::invoke_test_jit(function, &mut ctx, &mut stack, &mut ret) }
            };
            assert_eq!(result, JitResult::Panic);
            assert_eq!(
                ctx.runtime_trap_kind,
                JitRuntimeTrapKind::NilPointerDereference as u8
            );
            assert_eq!(ctx.runtime_trap_pc, if inline_nil { 0 } else { 1 });
            assert_eq!(
                ctx.runtime_trap_origin,
                if inline_nil {
                    InstructionSource::from_parts(1, 1).unwrap().raw()
                } else {
                    0
                }
            );
        }
    }
}
