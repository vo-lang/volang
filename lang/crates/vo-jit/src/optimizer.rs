//! Immutable module-wide facts consumed only by the optimizing tier.

use vo_runtime::bytecode::Module;

use crate::call_graph::ModuleCallGraph;
use crate::call_helpers::SmallPureLeafInline;

pub(crate) struct ModuleOptimizationPlan {
    graph: ModuleCallGraph,
    pure_leaf_inlines: Box<[Option<SmallPureLeafInline>]>,
}

impl ModuleOptimizationPlan {
    pub(crate) fn build(module: &Module) -> Self {
        let graph = ModuleCallGraph::build(module);
        let pure_leaf_inlines = module
            .functions
            .iter()
            .map(|function| SmallPureLeafInline::analyze(function, &module.constants))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Self {
            graph,
            pure_leaf_inlines,
        }
    }

    pub(crate) fn pure_leaf_inline(
        &self,
        caller_id: u32,
        callee_id: u32,
    ) -> Option<&SmallPureLeafInline> {
        let caller = caller_id as usize;
        let callee = callee_id as usize;
        if self.graph.is_recursive_edge(caller, callee) {
            return None;
        }
        self.pure_leaf_inlines.get(callee)?.as_ref()
    }

    pub(crate) fn direct_self_call(&self, caller_id: u32, callee_id: u32) -> bool {
        caller_id == callee_id
            && self
                .graph
                .is_recursive_edge(caller_id as usize, callee_id as usize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::function_with_sig;
    use vo_runtime::instruction::{Instruction, Opcode};

    #[test]
    fn plan_separates_pure_leaf_inlining_from_recursive_direct_calls() {
        let mut module = Module::new("optimizer-plan".into());
        module.functions = vec![
            function_with_sig(
                vec![
                    Instruction::new(Opcode::Call, 0, 0, 0),
                    Instruction::new(Opcode::Return, 0, 0, 0),
                ],
                0,
                0,
                1,
                0,
            ),
            function_with_sig(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 0, 0, 1, 0),
        ];

        let plan = ModuleOptimizationPlan::build(&module);
        assert!(plan.direct_self_call(0, 0));
        assert!(plan.pure_leaf_inline(0, 0).is_none());
        assert!(plan.pure_leaf_inline(0, 1).is_some());
    }
}
