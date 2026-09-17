//! Actual native execution of frame aliases, scalar facts and traps.
use super::*;

fn check_array_alias_osr(module: &VoModule, expected: Result<u64, u32>) {
    let function = &module.functions[0];
    let return_pc = function.code.len() - 1;
    let result_slot = usize::from(function.code[return_pc].a);
    let loaded = Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().unwrap();
    jit.bind_loaded_module_scope(loaded).unwrap();
    jit.compile_loaded_loop(
        0,
        default_compile_env(&externs),
        &LoopInfo {
            begin_pc: 0,
            end_pc: return_pc - 1,
            exit_pc: return_pc,
        },
    )
    .unwrap();
    let entry = unsafe { jit.cache.get_loop_func_ptr(0, 0).unwrap() };
    let mut gc = vo_runtime::gc::Gc::new();
    let mut frame = vec![0; usize::from(function.local_slots)];
    let mut parts = JitContextParts::new();
    let mut ctx = parts.context(module, &mut frame);
    ctx.gc = &mut gc;
    let result = entry(&mut ctx, frame.as_mut_ptr());
    match expected {
        Ok(value) => {
            assert_eq!(result, JitResult::Ok);
            assert_eq!(ctx.loop_exit_pc, return_pc as u32);
            assert_eq!(frame[result_slot], value);
        }
        Err(pc) => {
            assert_eq!(result, JitResult::Panic);
            assert_eq!(ctx.runtime_trap_pc, pc);
            assert_eq!(
                ctx.runtime_trap_kind,
                vo_runtime::jit_api::JitRuntimeTrapKind::NilPointerDereference as u8
            );
        }
    }
}

#[test]
fn scalar_object_fields_remain_visible_through_local_array_memory() {
    use vo_runtime::jit_api::JitTier;

    for width in [1, 2] {
        let mut function = make_func_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::LoadConst, 0, 0, 0),
                Instruction::new(Opcode::PtrNew, 1, 0, 0),
                Instruction::new(Opcode::LoadInt, 2, 42, 0),
                Instruction::new(Opcode::PtrSet, 1, 0, 2),
                Instruction::new(Opcode::CopyN, 3, 1, width),
                Instruction::new(Opcode::LoadInt, 5, 0, 0),
                Instruction::new(Opcode::LoadInt, 6, 1, 0),
                Instruction::new(Opcode::IndexCheck, 5, 6, 0),
                Instruction::new(
                    if width == 1 {
                        Opcode::SlotGet
                    } else {
                        Opcode::SlotGetN
                    },
                    7,
                    3,
                    5,
                ),
                Instruction::new(Opcode::PtrGet, 9, 7, 0),
                Instruction::new(Opcode::Return, 9, 1, 0),
            ],
            vec![
                SlotType::Value,
                SlotType::GcBase,
                SlotType::Value,
                SlotType::GcBase,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::GcBase,
                SlotType::Value,
                SlotType::Value,
            ],
            0,
            0,
            1,
        );
        for pc in [1, 3, 9] {
            function.instruction_metadata[pc] = InstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::Value],
            };
        }
        function.instruction_metadata[8] = InstructionMetadata::SlotLayout {
            array_len: 1,
            elem_layout: vec![SlotType::GcBase, SlotType::Value][..width as usize].to_vec(),
        };
        let mut module = VoModule::new("scalar-object-array-alias".into());
        module.constants.push(Constant::Int(
            ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
        ));
        module.functions.push(function);
        let loaded =
            Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
        let externs = ResolvedExternTable::empty();
        let mut jit = JitCompiler::new().unwrap();
        jit.bind_loaded_module_scope(loaded).unwrap();
        for tier in [JitTier::Baseline, JitTier::Optimizing] {
            jit.compile_loaded_tier(0, default_compile_env(&externs), tier)
                .unwrap();
            let entry = unsafe { jit.get_func_ptr_for_tier(0, tier).unwrap() };
            let mut gc = vo_runtime::gc::Gc::new();
            let mut frame = [0; 10];
            let mut ret = [0];
            let mut parts = JitContextParts::new();
            let mut ctx = parts.context(&module, &mut frame);
            ctx.gc = &mut gc;
            assert_eq!(
                unsafe { crate::invoke_test_jit(entry, &mut ctx, &mut frame, &mut ret) },
                JitResult::Ok,
            );
            assert_eq!(ret, [42], "{tier:?}, element width {width}");
        }
        check_array_alias_osr(&module, Ok(42));
    }
}

#[test]
fn native_scalar_facts_reload_cells_after_indexed_frame_writes() {
    use vo_runtime::jit_api::JitTier;

    let mut function = make_func_with_sig(
        vec![
            Instruction::new(Opcode::LoadInt, 0, 1, 0),
            Instruction::new(Opcode::LoadInt, 1, 0, 0),
            Instruction::new(Opcode::LoadInt, 2, 41, 0),
            Instruction::new(Opcode::IndexCheck, 1, 0, 0),
            Instruction::new(Opcode::SlotSet, 0, 1, 2),
            Instruction::new(Opcode::AddI, 3, 0, 2),
            Instruction::new(Opcode::LoadInt, 4, 82, 0),
            Instruction::new(Opcode::EqI, 3, 3, 4),
            Instruction::new(Opcode::JumpIfNot, 3, 2, 0),
            Instruction::new(Opcode::LoadInt, 3, 82, 0),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ],
        0,
        0,
        5,
        1,
    );
    function.instruction_metadata[4] = InstructionMetadata::SlotLayout {
        array_len: 1,
        elem_layout: vec![SlotType::Value],
    };
    let mut module = VoModule::new("indexed-frame-scalar-facts".into());
    module.functions.push(function);
    let loaded = Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
    let externs = ResolvedExternTable::empty();
    let mut jit = JitCompiler::new().unwrap();
    jit.bind_loaded_module_scope(loaded).unwrap();
    for tier in [JitTier::Baseline, JitTier::Optimizing] {
        jit.compile_loaded_tier(0, default_compile_env(&externs), tier)
            .unwrap();
        let entry = unsafe { jit.get_func_ptr_for_tier(0, tier).unwrap() };
        let mut frame = [0; 5];
        let mut ret = [0];
        let mut parts = JitContextParts::new();
        let mut ctx = parts.context(&module, &mut frame);
        assert_eq!(
            unsafe { crate::invoke_test_jit(entry, &mut ctx, &mut frame, &mut ret) },
            JitResult::Ok,
        );
        assert_eq!(ret, [82], "{tier:?}");
    }
    check_array_alias_osr(&module, Ok(82));
}

#[test]
fn indexed_frame_writes_invalidate_native_non_nil_proofs() {
    use vo_runtime::jit_api::{JitRuntimeTrapKind, JitTier};

    for opcode in [Opcode::SlotSet, Opcode::SlotSetN] {
        let width = if opcode == Opcode::SlotSet { 1 } else { 2 };
        let mut function = make_func_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::LoadInt, 3, 0, 0),
                Instruction::new(Opcode::LoadInt, 4, 1, 0),
                Instruction::new(Opcode::IndexCheck, 3, 4, 0),
                Instruction::new(Opcode::LoadConst, 0, 0, 0),
                Instruction::new(Opcode::PtrNew, 1, 0, 0),
                Instruction::new(Opcode::LoadInt, 5, 0, 0),
                Instruction::new(Opcode::LoadInt, 6, 19, 0),
                Instruction::new(opcode, 1, 3, 5),
                Instruction::new(Opcode::PtrGet, 7, 1, 0),
                Instruction::new(Opcode::Return, 7, 1, 0),
            ],
            vec![
                SlotType::Value,
                SlotType::GcBase,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::GcBase,
                SlotType::Value,
                SlotType::Value,
            ],
            0,
            0,
            1,
        );
        for pc in [4, 8] {
            function.instruction_metadata[pc] = InstructionMetadata::PtrLayout {
                value_layout: vec![SlotType::Value],
            };
        }
        function.instruction_metadata[7] = InstructionMetadata::SlotLayout {
            array_len: 1,
            elem_layout: vec![SlotType::GcBase, SlotType::Value][..width].to_vec(),
        };
        let mut module = VoModule::new("array-nil-proof".into());
        module.constants.push(Constant::Int(
            ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
        ));
        module.functions.push(function);
        let loaded =
            Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
        let externs = ResolvedExternTable::empty();
        let mut jit = JitCompiler::new().unwrap();
        jit.bind_loaded_module_scope(loaded).unwrap();
        for tier in [JitTier::Baseline, JitTier::Optimizing] {
            jit.compile_loaded_tier(0, default_compile_env(&externs), tier)
                .unwrap();
            let entry = unsafe { jit.get_func_ptr_for_tier(0, tier).unwrap() };
            let mut gc = vo_runtime::gc::Gc::new();
            let mut frame = [0; 8];
            let mut ret = [0];
            let mut parts = JitContextParts::new();
            let mut ctx = parts.context(&module, &mut frame);
            ctx.gc = &mut gc;
            assert_eq!(
                unsafe { crate::invoke_test_jit(entry, &mut ctx, &mut frame, &mut ret) },
                JitResult::Panic,
                "{opcode:?}, {tier:?}",
            );
            assert_eq!(
                ctx.runtime_trap_kind,
                JitRuntimeTrapKind::NilPointerDereference as u8
            );
            assert_eq!(ctx.runtime_trap_pc, 8);
        }
        check_array_alias_osr(&module, Err(8));
    }
}
