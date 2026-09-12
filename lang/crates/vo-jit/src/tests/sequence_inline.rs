use super::*;
use crate::call_helpers::SmallFunctionInline;
use vo_common_core::debug_info::InstructionSource;
use vo_runtime::jit_api::{JitRuntimeTrapKind, JitTier};
use vo_runtime::objects::slice;

fn sequence_module(partial_store: bool) -> VoModule {
    let caller = make_func_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::Jump, 0, 1, 0),
            Instruction::new(Opcode::Call, 1, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        vec![SlotType::GcBase, SlotType::Value, SlotType::Value],
        3,
        3,
        0,
    );
    let code = if partial_store {
        vec![
            Instruction::new(Opcode::SliceGet, 3, 0, 1),
            Instruction::new(Opcode::LoadInt, 4, 1, 0),
            Instruction::new(Opcode::AddI, 3, 3, 4),
            Instruction::new(Opcode::SliceSet, 0, 1, 3),
            Instruction::new(Opcode::SliceSet, 0, 2, 3),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ]
    } else {
        vec![
            Instruction::new(Opcode::SliceGet, 3, 0, 1),
            Instruction::new(Opcode::SliceGet, 4, 0, 2),
            Instruction::new(Opcode::SliceSet, 0, 1, 4),
            Instruction::new(Opcode::SliceSet, 0, 2, 3),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ]
    };
    let mut leaf = make_func_with_slot_types_and_sig(
        code,
        vec![
            SlotType::GcBase,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
        ],
        3,
        3,
        0,
    );
    for (inst, metadata) in leaf.code.iter().zip(&mut leaf.instruction_metadata) {
        if matches!(inst.opcode(), Opcode::SliceGet | Opcode::SliceSet) {
            *metadata = InstructionMetadata::ElemLayout {
                elem_bytes: 8,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Value],
            };
        }
    }
    let mut module = VoModule::new("sequence-inline".into());
    module.functions = vec![caller, leaf];
    module
}

#[test]
fn sequence_inline_admission_requires_complete_scalar_payload_metadata() {
    let module = sequence_module(false);
    let leaf = &module.functions[1];
    let plan = SmallFunctionInline::analyze_leaf(1, leaf, &module).unwrap();
    assert!(
        !plan.is_total_scalar(),
        "descriptor access keeps the effect boundary"
    );
    assert_eq!(plan.cost(), 5);
    for (bytes, layout) in [
        (0, vec![]),
        (1, vec![SlotType::Value]),
        (4, vec![SlotType::Value]),
        (8, vec![SlotType::Float]),
        (8, vec![SlotType::GcRef]),
        (16, vec![SlotType::Value; 2]),
    ] {
        for pc in [0, 1, 2, 3] {
            let mut other = leaf.clone();
            other.instruction_metadata[pc] = InstructionMetadata::ElemLayout {
                elem_bytes: bytes,
                needs_sign_extend: false,
                slot_layout: layout.clone(),
            };
            assert!(SmallFunctionInline::analyze_leaf(1, &other, &module).is_none());
        }
    }
    let mut missing = leaf.clone();
    missing.instruction_metadata[0] = InstructionMetadata::None;
    assert!(SmallFunctionInline::analyze_leaf(1, &missing, &module).is_none());
    let mut receiver = leaf.clone();
    receiver.recv_slots = 1;
    let plan = SmallFunctionInline::analyze_leaf(1, &receiver, &module).unwrap();
    assert!(!plan.supports_dynamic_layout(Opcode::CallIface, 2, 0));
    assert!(!plan.supports_dynamic_layout(Opcode::CallClosure, 2, 0));
    let mut scalar_pointer = leaf.clone();
    scalar_pointer.slot_types[0] = SlotType::Value;
    assert!(SmallFunctionInline::analyze_leaf(1, &scalar_pointer, &module).is_none());
}

#[derive(Clone, Copy, Debug)]
enum Entry {
    Function(JitFunc),
    Loop(crate::abi::LoopFunc),
    Continuation(NativeJitFunc),
}

impl Entry {
    fn invoke(self, ctx: &mut JitContext, stack: &mut [u64]) -> JitResult {
        let mut ret = [];
        match self {
            Self::Function(entry) => unsafe { crate::invoke_test_jit(entry, ctx, stack, &mut ret) },
            Self::Loop(entry) => entry(ctx, stack.as_mut_ptr()),
            Self::Continuation(entry) => entry(ctx, 0, ret.as_mut_ptr(), 1, 0, 0, 0, 0),
        }
    }
}

fn compile_entries(module: &VoModule) -> (JitCompiler, Vec<Entry>) {
    let loaded = Arc::new(vo_common_core::verifier::verify_loaded_module(module.clone()).unwrap());
    let externs = ResolvedExternTable::empty();
    let env = default_compile_env(&externs);
    let mut jit = JitCompiler::new().unwrap();
    jit.bind_loaded_module_scope(loaded).unwrap();
    let mut entries = Vec::new();
    for tier in [JitTier::Baseline, JitTier::Optimizing] {
        jit.compile_loaded_tier(0, env, tier).unwrap();
        entries.push(Entry::Function(unsafe {
            jit.get_func_ptr_for_tier(0, tier).unwrap()
        }));
    }
    jit.compile_loaded_loop(
        0,
        env,
        &LoopInfo {
            begin_pc: 1,
            end_pc: 1,
            exit_pc: 2,
        },
    )
    .unwrap();
    entries.push(Entry::Loop(unsafe {
        jit.cache.get_loop_func_ptr(0, 1).unwrap()
    }));
    let module_analysis = jit.module_analysis(module, env).unwrap();
    assert!(!module_analysis.entry_eligibility[0].prepared_shadow);
    for optimize in [false, true] {
        let ordinary = crate::analysis::FunctionAnalysis::for_function(
            &module.functions[0],
            module,
            crate::MAX_JIT_ANALYSIS_BYTES,
        )
        .unwrap();
        let pcs = [1, 2];
        let resumed = crate::analysis::FunctionAnalysis::try_for_continuations(
            &module.functions[0],
            module,
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
            module,
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
                &format!("sequence_continuation_{optimize}"),
                cranelift_module::Linkage::Local,
                &jit.ctx.func.signature,
            )
            .unwrap();
        let staged = jit.stage_function(id, "sequence continuation").unwrap();
        let (code, _) = jit.publish_function_artifact(staged).unwrap();
        entries.push(Entry::Continuation(unsafe { std::mem::transmute(code) }));
    }
    for tier in [JitTier::Baseline, JitTier::Optimizing] {
        assert!(
            unsafe { jit.get_func_ptr_for_tier(1, tier) }.is_none(),
            "no child entry may hide failed inlining"
        );
    }
    (jit, entries)
}

#[test]
fn sequence_inline_preserves_swaps_and_aliases_in_all_native_entries() {
    let module = sequence_module(false);
    let (_jit, entries) = compile_entries(&module);
    let meta = ValueMeta::new(0, ValueKind::Int64);
    for entry in entries {
        let mut gc = vo_runtime::gc::Gc::new();
        let packed = slice::create(&mut gc, meta, 8, 4, 4);
        let owner = gc.alloc(meta, 8);
        let mut permanent = [0_u64; 8];
        let mut views = vec![(packed, 1)];
        // Extended views with a non-packed stride cover both a managed owner
        // and stable externally rooted storage, including untouched padding.
        for backing in [owner, permanent.as_mut_ptr()] {
            let view = unsafe {
                slice::from_inline_array_range_with_cap(
                    &mut gc,
                    if backing == owner {
                        owner
                    } else {
                        ptr::null_mut()
                    },
                    backing.cast(),
                    4,
                    0,
                    4,
                    4,
                    meta,
                    8,
                    16,
                )
            };
            assert!(!view.is_null());
            views.push((view, 2));
        }
        for (view, stride) in views {
            for (left, right) in [(0, 3), (3, 0), (2, 2)] {
                unsafe {
                    for i in 0..4 {
                        slice::set(view, i, 10 + i as u64, 8);
                    }
                }
                let alias = unsafe { slice::slice_of(&mut gc, view, 0, 4) }.unwrap();
                let mut stack = [view as u64, left as u64, right as u64, 0, 0, 0, 0, 0];
                let mut parts = JitContextParts::new();
                let mut ctx = parts.context(&module, &mut stack);
                ctx.gc = &mut gc;
                ctx.current_func_id = 0;
                ctx.fiber_sp = 3;
                assert_eq!(
                    entry.invoke(&mut ctx, &mut stack),
                    JitResult::Ok,
                    "{entry:?}"
                );
                let mut expected = [10, 11, 12, 13];
                expected.swap(left, right);
                for (i, value) in expected.into_iter().enumerate() {
                    assert_eq!(
                        unsafe { slice::get(alias, i, 8) },
                        value,
                        "{entry:?} stride={stride}"
                    );
                }
                if stride == 2 {
                    let data = unsafe { slice::data_ptr(view) }.cast::<u64>();
                    for i in 0..4 {
                        assert_eq!(unsafe { *data.add(i * 2 + 1) }, 0);
                    }
                }
            }
        }
    }
}

#[test]
fn sequence_inline_keeps_partial_stores_and_exact_bounds_origin() {
    let module = sequence_module(true);
    let (_jit, entries) = compile_entries(&module);
    for entry in entries {
        let mut gc = vo_runtime::gc::Gc::new();
        let view = slice::create(&mut gc, ValueMeta::new(0, ValueKind::Int64), 8, 2, 2);
        for (nil, first, second, origin_pc, expected) in [
            (true, 0, 0, 0, 10),
            (false, u64::MAX, 0, 0, 10),
            (false, 2, 0, 0, 10),
            (false, 0, 2, 4, 11),
            (false, 0, u64::MAX, 4, 11),
        ] {
            unsafe {
                slice::set(view, 0, 10, 8);
            }
            let mut stack = [
                if nil { 0 } else { view as u64 },
                first,
                second,
                0,
                0,
                0,
                0,
                0,
            ];
            let mut parts = JitContextParts::new();
            let mut ctx = parts.context(&module, &mut stack);
            ctx.gc = &mut gc;
            ctx.current_func_id = 0;
            ctx.fiber_sp = 3;
            assert_eq!(
                entry.invoke(&mut ctx, &mut stack),
                JitResult::Panic,
                "{entry:?}"
            );
            assert_eq!(
                ctx.runtime_trap_kind,
                JitRuntimeTrapKind::IndexOutOfBounds as u8
            );
            assert_eq!(ctx.runtime_trap_pc, 1);
            assert_eq!(
                ctx.runtime_trap_origin,
                InstructionSource::from_parts(1, origin_pc).unwrap().raw()
            );
            assert_eq!(unsafe { slice::get(view, 0, 8) }, expected, "{entry:?}");
        }
    }
}

#[test]
fn sequence_inline_exhausted_budget_yields_before_stores_then_runs_once() {
    let module = sequence_module(true);
    let (_jit, entries) = compile_entries(&module);
    for entry in entries {
        let mut gc = vo_runtime::gc::Gc::new();
        let view = slice::create(&mut gc, ValueMeta::new(0, ValueKind::Int64), 8, 2, 2);
        unsafe {
            slice::set(view, 0, 10, 8);
        }
        let mut stack = [view as u64, 0, 1, 0, 0, 0, 0, 0];
        let mut parts = JitContextParts::new();
        let mut ctx = parts.context(&module, &mut stack);
        ctx.gc = &mut gc;
        ctx.current_func_id = 0;
        ctx.fiber_sp = 3;
        ctx.execution_budget = 1;
        assert_eq!(
            entry.invoke(&mut ctx, &mut stack),
            JitResult::Call,
            "{entry:?}"
        );
        assert_eq!(ctx.call_kind, JitContext::CALL_KIND_YIELD);
        assert_eq!(
            ctx.call_resume_pc,
            if matches!(entry, Entry::Function(_)) {
                0
            } else {
                1
            },
            "ordinary entry can yield before the initial jump: {entry:?}"
        );
        assert_eq!(unsafe { slice::get(view, 0, 8) }, 10);
        assert_eq!(unsafe { slice::get(view, 1, 8) }, 0);
        ctx.call_kind = 0;
        ctx.execution_budget = 100;
        assert_eq!(
            entry.invoke(&mut ctx, &mut stack),
            JitResult::Ok,
            "{entry:?}"
        );
        assert_eq!(unsafe { slice::get(view, 0, 8) }, 11);
        assert_eq!(unsafe { slice::get(view, 1, 8) }, 11);
    }
}
