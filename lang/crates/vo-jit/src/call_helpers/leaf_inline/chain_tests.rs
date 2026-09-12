use super::*;
use crate::call_graph::ModuleCallGraph;
use crate::optimizer::ModuleInlinePlan;
use crate::test_fixtures::function_with_slot_types_and_sig;

fn scalar(code: Vec<Instruction>, slots: usize) -> FunctionDef {
    function_with_slot_types_and_sig(code, vec![SlotType::Value; slots], 1, 1, 1)
}

fn identity() -> FunctionDef {
    scalar(vec![Instruction::new(Opcode::Return, 0, 1, 0)], 1)
}

fn wrapper(callee: u16, calls: usize) -> FunctionDef {
    let mut code = vec![Instruction::new(Opcode::Call, callee, 0, 0); calls];
    code.push(Instruction::new(Opcode::Return, 1, 1, 0));
    scalar(code, 2)
}

fn build_plan(module: &Module, limit: usize) -> ModuleInlinePlan {
    ModuleInlinePlan::build_with_graph(module, Arc::new(ModuleCallGraph::build(module)), limit)
        .unwrap()
}

#[test]
fn scalar_chain_depth_and_cycles_are_bounded_independently_of_source_order() {
    let mut module = Module::new("chain-depth".into());
    module.functions = (1..=8).map(|id| wrapper(id, 1)).collect();
    module.functions.push(identity());
    let plan = build_plan(&module, usize::MAX);
    assert!(plan.small_inline(0, 0).is_none());
    for id in 1..=8 {
        assert_eq!(plan.small_inline(0, id).unwrap().depth, 9 - id as usize);
    }

    module.functions = vec![wrapper(1, 1), wrapper(0, 1), identity()];
    let plan = build_plan(&module, usize::MAX);
    assert!(plan.small_inline(2, 0).is_none());
    assert!(plan.small_inline(2, 1).is_none());
    assert!(plan.small_inline(0, 2).is_some());
}

#[test]
fn scalar_chain_shared_dependencies_charge_every_emission_and_bound_growth() {
    let mut module = Module::new("chain-duplication".into());
    module.functions = (1..=7).map(|id| wrapper(id, 2)).collect();
    module.functions.push(identity());
    let plan = build_plan(&module, usize::MAX);
    let recipe = plan.small_inline(0, 6).unwrap();
    assert!(Arc::ptr_eq(
        recipe.children[0].as_ref().unwrap(),
        recipe.children[1].as_ref().unwrap()
    ));
    let child = recipe.children[0].as_ref().unwrap();
    assert_eq!(recipe.cost, 3 + child.cost * 2);
    assert!(recipe.duplication_work() > recipe.cost);
    assert!(recipe.expansion_work >= child.expansion_work * 2);
    assert!(
        plan.small_inline(0, 0).is_none(),
        "expanded copies exceed the artifact budget"
    );
}

#[test]
fn scalar_chain_retention_limit_keeps_existing_leaf_and_skips_optional_wrapper() {
    let mut module = Module::new("chain-retention".into());
    module.functions = vec![wrapper(1, 1), identity()];
    let leaf = SmallFunctionInline::analyze_leaf(1, &module.functions[1], &module).unwrap();
    let limit = core::mem::size_of::<ModuleInlinePlan>()
        + 2 * core::mem::size_of::<Option<Arc<SmallFunctionInline>>>()
        + leaf.retained_bytes();
    let plan = build_plan(&module, limit);
    assert!(plan.small_inline(0, 0).is_none());
    assert!(plan.small_inline(0, 1).is_some());
    assert_eq!(plan.retained_bytes(), limit);
}

#[test]
fn scalar_chain_invalidates_return_constants_before_division_admission() {
    let child = scalar(
        vec![
            Instruction::new(Opcode::LoadInt, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 1, 0),
        ],
        1,
    );
    let mut caller = scalar(
        vec![
            Instruction::new(Opcode::LoadInt, 2, 2, 0),
            Instruction::new(Opcode::Call, 1, 1, 0),
            Instruction::new(Opcode::DivI, 0, 0, 2),
            Instruction::new(Opcode::Return, 0, 1, 0),
        ],
        3,
    );
    caller.param_count = 2;
    caller.param_slots = 2;
    let mut module = Module::new("chain-divisor-kill".into());
    module.functions = vec![caller, child];
    let plan = build_plan(&module, usize::MAX);
    assert!(plan.small_inline(0, 0).is_none());
    assert!(plan.small_inline(0, 1).is_some());
}

#[test]
fn scalar_chain_rejects_signature_drift_and_observable_children() {
    let mut module = Module::new("chain-admission".into());
    module.functions = vec![wrapper(1, 1), identity()];
    module.functions[0].slot_types[1] = SlotType::Float;
    assert!(build_plan(&module, usize::MAX).small_inline(0, 0).is_none());
    module.functions[0] = wrapper(1, 1);
    module.functions[1] = scalar(
        vec![
            Instruction::new(Opcode::CallExtern, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 1, 0),
        ],
        1,
    );
    assert!(build_plan(&module, usize::MAX).small_inline(0, 0).is_none());

    let mut heap = function_with_slot_types_and_sig(
        vec![
            Instruction::new(Opcode::PtrGet, 1, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ],
        vec![SlotType::GcRef, SlotType::Value],
        1,
        1,
        1,
    );
    heap.instruction_metadata[0] = vo_runtime::bytecode::InstructionMetadata::PtrLayout {
        value_layout: vec![SlotType::Value],
    };
    module.functions[0].slot_types[0] = SlotType::GcRef;
    module.functions[1] = heap;
    let plan = build_plan(&module, usize::MAX);
    assert!(plan.small_inline(0, 0).is_none());
    assert!(
        plan.small_inline(0, 1).is_some(),
        "ordinary leaf trap route is retained"
    );
}
