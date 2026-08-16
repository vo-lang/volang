//! Lazy interprocedural proofs for managed values returned by static calls.
//!
//! A function return is an exact object base only when every executable return
//! supplies an exact base. Summaries are solved callee-first over the module
//! SCC graph, then to the greatest fixed point inside recursive components.
//! The greatest fixed point captures recursive forwarding: assuming a managed
//! return is exact remains valid until a concrete return path disproves it.
//! Generated code can therefore read GC headers directly without weakening
//! support for interior pointers crossing function boundaries.

use vo_runtime::bytecode::Module;

use crate::call_graph::ModuleCallGraph;
use crate::ir::FunctionIr;
use crate::{JitError, MAX_JIT_COMPILE_WORK_BYTES};

pub(crate) struct ModuleReturnProvenance {
    exact_base_returns: Box<[Box<[bool]>]>,
    completed_components: Box<[bool]>,
    retained_bytes: usize,
}

impl ModuleReturnProvenance {
    pub(crate) fn new(
        module: &Module,
        graph: &ModuleCallGraph,
        retained_limit_bytes: usize,
    ) -> Result<Self, JitError> {
        let function_count = module.functions.len();
        let return_slot_count = module
            .functions
            .iter()
            .map(|function| usize::from(function.ret_slots))
            .sum::<usize>();
        let retained_bytes = core::mem::size_of::<Self>()
            .saturating_add(function_count.saturating_mul(core::mem::size_of::<Box<[bool]>>()))
            .saturating_add(return_slot_count.saturating_mul(core::mem::size_of::<bool>()))
            .saturating_add(
                graph
                    .component_count()
                    .saturating_mul(core::mem::size_of::<bool>()),
            );
        if retained_bytes > retained_limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes: retained_bytes,
            });
        }

        let mut exact_base_returns = Vec::new();
        exact_base_returns
            .try_reserve_exact(function_count)
            .map_err(|_| JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes: retained_bytes,
            })?;
        for function in &module.functions {
            let mut summary = Vec::new();
            summary
                .try_reserve_exact(usize::from(function.ret_slots))
                .map_err(|_| JitError::AnalysisResourceLimitExceeded {
                    limit_bytes: retained_limit_bytes,
                    requested_bytes: retained_bytes,
                })?;
            summary.resize(usize::from(function.ret_slots), false);
            exact_base_returns.push(summary.into_boxed_slice());
        }
        let mut completed_components = Vec::new();
        completed_components
            .try_reserve_exact(graph.component_count())
            .map_err(|_| JitError::AnalysisResourceLimitExceeded {
                limit_bytes: retained_limit_bytes,
                requested_bytes: retained_bytes,
            })?;
        completed_components.resize(graph.component_count(), false);

        Ok(Self {
            exact_base_returns: exact_base_returns.into_boxed_slice(),
            completed_components: completed_components.into_boxed_slice(),
            retained_bytes,
        })
    }

    pub(crate) fn retained_bytes(&self) -> usize {
        self.retained_bytes
    }

    pub(crate) fn summaries(&self) -> &[Box<[bool]>] {
        &self.exact_base_returns
    }

    pub(crate) fn ensure_function(
        &mut self,
        func_id: usize,
        module: &Module,
        graph: &ModuleCallGraph,
        transient_limit_bytes: usize,
    ) -> Result<(), JitError> {
        let root_component = graph
            .component_id(func_id)
            .ok_or(JitError::FunctionNotFound(func_id as u32))?;
        if self.completed_components[root_component] {
            return Ok(());
        }

        let component_count = graph.component_count();
        let traversal_bytes = component_count
            .saturating_mul(core::mem::size_of::<u8>())
            .saturating_add(component_count.saturating_mul(core::mem::size_of::<(usize, bool)>()))
            .saturating_add(component_count.saturating_mul(core::mem::size_of::<usize>()));
        if traversal_bytes > MAX_JIT_COMPILE_WORK_BYTES {
            return Err(JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes: traversal_bytes,
            });
        }

        let mut state = vec![0_u8; component_count];
        let mut stack = vec![(root_component, false)];
        let mut order = Vec::new();
        while let Some((component, expanded)) = stack.pop() {
            if self.completed_components[component] {
                continue;
            }
            if expanded {
                state[component] = 2;
                order.push(component);
                continue;
            }
            if state[component] != 0 {
                continue;
            }
            state[component] = 1;
            stack.push((component, true));
            for &callee in graph.component_callees(component).iter().rev() {
                if !self.completed_components[callee] && state[callee] == 0 {
                    stack.push((callee, false));
                }
            }
        }

        for component in order {
            self.solve_component(component, module, graph, transient_limit_bytes)?;
            self.completed_components[component] = true;
        }
        Ok(())
    }

    fn solve_component(
        &mut self,
        component: usize,
        module: &Module,
        graph: &ModuleCallGraph,
        transient_limit_bytes: usize,
    ) -> Result<(), JitError> {
        let function_count = module.functions.len();
        let work_bytes = function_count
            .saturating_mul(core::mem::size_of::<bool>())
            .saturating_add(
                graph
                    .component_members(component)
                    .len()
                    .saturating_mul(core::mem::size_of::<usize>()),
            );
        if work_bytes > MAX_JIT_COMPILE_WORK_BYTES {
            return Err(JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes: work_bytes,
            });
        }

        let mut queued = vec![false; function_count];
        let mut worklist = Vec::from(graph.component_members(component));
        for &member in &worklist {
            queued[member] = true;
            let function = module
                .functions
                .get(member)
                .ok_or(JitError::FunctionNotFound(member as u32))?;
            for (known, slot_type) in self.exact_base_returns[member]
                .iter_mut()
                .zip(function.ret_slot_types.iter())
            {
                *known = *slot_type == vo_runtime::SlotType::GcRef;
            }
        }
        while let Some(member) = worklist.pop() {
            queued[member] = false;
            let function = module
                .functions
                .get(member)
                .ok_or(JitError::FunctionNotFound(member as u32))?;
            let ir = FunctionIr::build_with_limit_and_return_summaries(
                function,
                module,
                &self.exact_base_returns,
                transient_limit_bytes,
            )?;
            let derived = ir.exact_base_return_slots(function);
            let summary = &mut self.exact_base_returns[member];
            let mut changed = false;
            for (known, proven) in summary.iter_mut().zip(derived.iter().copied()) {
                if *known && !proven {
                    *known = false;
                    changed = true;
                }
            }
            if !changed {
                continue;
            }
            for &caller in graph.callers(member) {
                if graph.component_id(caller) == Some(component) && !queued[caller] {
                    queued[caller] = true;
                    worklist.push(caller);
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::JitFunctionBuilder;
    use vo_runtime::bytecode::Module;
    use vo_runtime::instruction::{Instruction, Opcode};
    use vo_runtime::SlotType;

    fn branch(opcode: Opcode, condition: u16, offset: i32) -> Instruction {
        Instruction::with_flags(
            opcode,
            0,
            condition,
            offset as u32 as u16,
            (offset as u32 >> 16) as u16,
        )
    }

    #[test]
    fn exact_base_returns_reach_a_fixed_point_across_recursion_and_callers() {
        let recursive_factory = JitFunctionBuilder::new(vec![
            branch(Opcode::JumpIf, 0, 3),
            Instruction::new(Opcode::Call, 0, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
            Instruction::new(Opcode::LoadConst, 1, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ])
        .slot_types(vec![SlotType::Value, SlotType::GcRef])
        .signature(1, 1, 1)
        .return_slot_types(vec![SlotType::GcRef])
        .build();
        let forwarding_factory = JitFunctionBuilder::new(vec![
            Instruction::new(Opcode::Call, 0, 0, 0),
            Instruction::new(Opcode::Return, 1, 1, 0),
        ])
        .slot_types(vec![SlotType::Value, SlotType::GcRef])
        .signature(1, 1, 1)
        .return_slot_types(vec![SlotType::GcRef])
        .build();
        let interior_return = JitFunctionBuilder::new(vec![
            Instruction::new(Opcode::PtrAdd, 2, 0, 1),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ])
        .slot_types(vec![SlotType::GcRef, SlotType::Value, SlotType::GcRef])
        .signature(2, 2, 1)
        .return_slot_types(vec![SlotType::GcRef])
        .build();
        let mut module = Module::new("return-provenance".into());
        module.functions = vec![recursive_factory, forwarding_factory, interior_return];

        let graph = ModuleCallGraph::build(&module);
        let mut summaries =
            ModuleReturnProvenance::new(&module, &graph, crate::MAX_JIT_ANALYSIS_BYTES)
                .expect("return summary storage");
        summaries
            .ensure_function(1, &module, &graph, crate::MAX_JIT_ANALYSIS_BYTES)
            .expect("recursive return fixed point");
        summaries
            .ensure_function(2, &module, &graph, crate::MAX_JIT_ANALYSIS_BYTES)
            .expect("interior return analysis");

        assert_eq!(summaries.summaries()[0].as_ref(), &[true]);
        assert_eq!(summaries.summaries()[1].as_ref(), &[true]);
        assert_eq!(summaries.summaries()[2].as_ref(), &[false]);
    }
}
