use super::*;
use crate::analysis::FunctionAnalysis;
use crate::call_graph::ModuleCallGraph;
use crate::optimizer::{ModuleInlinePlan, ModuleOptimizationPlan, OptimizedFunction};
use crate::test_fixtures::{function_with_sig, function_with_slot_types_and_sig};
use crate::{JitBackendCaps, JitFrameEntryEligibility};
use std::sync::Arc;
use vo_runtime::bytecode::{InstructionMetadata, ResolvedExternTable};
use vo_runtime::instruction::Instruction;
use vo_runtime::SlotType;

fn identity() -> FunctionDef {
    function_with_sig(vec![Instruction::new(Opcode::Return, 0, 1, 0)], 1, 1, 1, 1)
}

fn wrapper(target: u16, calls: usize) -> FunctionDef {
    let mut code = vec![Instruction::new(Opcode::Call, target, 0, 0); calls];
    code.push(Instruction::new(Opcode::Return, 1, 1, 0));
    function_with_sig(code, 1, 1, 2, 1)
}

fn plan(module: &Module, limit: usize) -> Arc<ModuleInlinePlan> {
    Arc::new(
        ModuleInlinePlan::build_with_graph(module, Arc::new(ModuleCallGraph::build(module)), limit)
            .unwrap(),
    )
}

fn entries(module: &Module, plan: &ModuleInlinePlan) -> Vec<JitFrameEntryEligibility> {
    let externs = ResolvedExternTable::empty();
    module_frame_entry_eligibility_with_graph(
        module,
        JitCompileEnv {
            externs: &externs,
            backend_caps: JitBackendCaps {
                extern_suspend: true,
            },
        },
        &ModuleCallGraph::build(module),
        Some(plan),
    )
}

#[test]
fn entry_inline_cover_matches_exact_and_over_budget_lowering() {
    for calls in [256, 257] {
        let mut module = Module::new("entry-inline-budget".into());
        module.functions = vec![wrapper(1, calls), identity()];
        let inlines = plan(&module, usize::MAX);
        assert_eq!(
            inlines.has_total_static_inline_cover(0, &module.functions[0]),
            calls == 256
        );
        let entry = entries(&module, &inlines)[0];
        assert_eq!(entry.frame_elided, calls == 256);
        assert_eq!(entry.prepared_shadow, calls == 256);
        assert!(entry.static_prepared_shadow);
        assert!(!entry.may_gc);

        let analysis = FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            crate::MAX_JIT_ANALYSIS_BYTES,
        )
        .unwrap();
        let baseline = OptimizedFunction::baseline_with_module(analysis.ir(), &inlines, 0);
        let module_plan = ModuleOptimizationPlan::build_with_inline_plan(
            &module,
            inlines,
            crate::MAX_JIT_ANALYSIS_BYTES,
        )
        .unwrap();
        let optimized = OptimizedFunction::analyze_with_module(
            analysis.ir(),
            &module.functions[0],
            &module_plan,
            0,
        );
        for pc in 0..calls {
            let expected = (pc < 256).then_some(1);
            assert_eq!(baseline.inline_target(pc), expected);
            assert_eq!(optimized.inline_target(pc), expected);
        }
    }
}

#[test]
fn entry_inline_cover_retains_unpublished_wrapper_and_opaque_receiver_contracts() {
    let mut module = Module::new("entry-inline-receiver".into());
    module.functions = vec![
        function_with_slot_types_and_sig(
            vec![
                Instruction::new(Opcode::Call, 1, 1, 0),
                Instruction::new(Opcode::Return, 2, 1, 0),
            ],
            vec![SlotType::GcRef, SlotType::Value, SlotType::Value],
            1,
            2,
            1,
        ),
        identity(),
    ];
    module.functions[0].recv_slots = 1;
    let inlines = plan(&module, usize::MAX);
    // No opaque-reference transitive recipe is introduced by entry refinement.
    assert!(inlines.small_inline(1, 0).is_none());
    assert!(inlines.has_total_static_inline_cover(0, &module.functions[0]));
    let entry = entries(&module, &inlines)[0];
    assert!(entry.frame_elided && entry.prepared_shadow && !entry.may_gc);
    assert!(crate::compiler::function_needs_native_root_frame(
        &module.functions[0]
    ));
}

#[test]
fn entry_inline_cover_requires_the_retained_child_recipe() {
    let mut module = Module::new("entry-inline-retention".into());
    module.functions = vec![wrapper(1, 1), wrapper(2, 1), identity()];
    let leaf =
        crate::call_helpers::SmallFunctionInline::analyze_leaf(2, &module.functions[2], &module)
            .unwrap();
    let limit = core::mem::size_of::<ModuleInlinePlan>()
        + 3 * core::mem::size_of::<Option<Arc<crate::call_helpers::SmallFunctionInline>>>()
        + leaf.retained_bytes();
    let inlines = plan(&module, limit);
    assert!(inlines.small_inline(0, 1).is_none());
    let entries = entries(&module, &inlines);
    assert!(!entries[0].frame_elided && !entries[0].prepared_shadow);
    assert!(entries[1].frame_elided && entries[1].prepared_shadow);
    assert!(entries[2].frame_elided && entries[2].prepared_shadow);
}

#[test]
fn entry_inline_cover_checks_unreachable_pcs_and_ignores_entry_constants() {
    let mut module = Module::new("entry-inline-all-pcs".into());
    module.functions = vec![wrapper(1, 1), identity()];
    module.functions[0]
        .code
        .push(Instruction::new(Opcode::DivI, 1, 0, 0));
    module.functions[0]
        .instruction_metadata
        .push(InstructionMetadata::None);
    let inlines = plan(&module, usize::MAX);
    assert!(!inlines.has_total_static_inline_cover(0, &module.functions[0]));
    assert!(!entries(&module, &inlines)[0].prepared_shadow);

    module.functions[0] = function_with_sig(
        vec![
            Instruction::new(Opcode::LoadInt, 2, 2, 0),
            Instruction::new(Opcode::Call, 1, 0, 0),
            Instruction::new(Opcode::DivI, 0, 0, 2),
            Instruction::new(Opcode::Return, 0, 1, 0),
        ],
        1,
        1,
        3,
        1,
    );
    assert!(known_nonzero_divisors(&module.functions[0], &module).contains(&2));
    let inlines = plan(&module, usize::MAX);
    assert!(!inlines.has_total_static_inline_cover(0, &module.functions[0]));
    assert!(!entries(&module, &inlines)[0].prepared_shadow);
}

#[test]
fn entry_inline_cover_rejects_heap_reading_children_and_dynamic_competitors() {
    let mut module = Module::new("entry-inline-unsafe-child".into());
    module.functions = vec![wrapper(1, 1), identity()];
    module.functions[0].slot_types[0] = SlotType::GcRef;
    module.functions[1] = function_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::PtrGet, 1, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ],
        vec![SlotType::GcRef, SlotType::Value],
        1,
        1,
        1,
    );
    module.functions[1].instruction_metadata[0] = InstructionMetadata::PtrLayout {
        value_layout: vec![SlotType::Value],
    };
    let inlines = plan(&module, usize::MAX);
    assert!(inlines.small_inline(0, 1).is_some());
    assert!(!inlines.has_total_static_inline_cover(0, &module.functions[0]));
    assert!(!entries(&module, &inlines)[0].prepared_shadow);

    module.functions = vec![wrapper(1, 1), identity()];
    module.functions[0]
        .code
        .insert(0, Instruction::new(Opcode::CallClosure, 0, 0, 0));
    module.functions[0]
        .instruction_metadata
        .insert(0, InstructionMetadata::None);
    let inlines = plan(&module, usize::MAX);
    assert!(!inlines.has_total_static_inline_cover(0, &module.functions[0]));
    assert!(!entries(&module, &inlines)[0].prepared_shadow);
}

#[test]
fn entry_inline_cover_includes_dormant_calls_without_displacing_executable_calls() {
    for hot_calls in [255, 256] {
        let mut code = vec![
            Instruction::new(Opcode::LoadInt, 0, 0, 0),
            Instruction::new(Opcode::JumpIfNot, 0, 2, 0),
            Instruction::new(Opcode::Call, 1, 1, 0),
        ];
        code.extend((0..hot_calls).map(|_| Instruction::new(Opcode::Call, 1, 1, 0)));
        code.push(Instruction::new(Opcode::Return, 2, 1, 0));
        let mut module = Module::new("entry-inline-dormant-budget".into());
        module.functions = vec![function_with_sig(code, 2, 2, 3, 1), identity()];
        let inlines = plan(&module, usize::MAX);
        assert_eq!(
            entries(&module, &inlines)[0].prepared_shadow,
            hot_calls == 255
        );
        let analysis = FunctionAnalysis::for_function(
            &module.functions[0],
            &module,
            crate::MAX_JIT_ANALYSIS_BYTES,
        )
        .unwrap();
        let baseline = OptimizedFunction::baseline_with_module(analysis.ir(), &inlines, 0);
        let module_plan = ModuleOptimizationPlan::build_with_inline_plan(
            &module,
            inlines,
            crate::MAX_JIT_ANALYSIS_BYTES,
        )
        .unwrap();
        let optimized = OptimizedFunction::analyze_with_module(
            analysis.ir(),
            &module.functions[0],
            &module_plan,
            0,
        );
        assert!(!optimized.is_executable(2));
        let osr = optimized.project_osr(analysis.ir(), 1..module.functions[0].code.len());
        assert!(osr.is_executable(2));
        for artifact in [&baseline, &optimized, &osr] {
            assert_eq!(artifact.inline_target(2), (hot_calls == 255).then_some(1));
            for pc in 3..3 + hot_calls {
                assert_eq!(artifact.inline_target(pc), Some(1));
            }
        }
    }
}
