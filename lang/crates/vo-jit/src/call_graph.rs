//! Immutable module call graph shared by contracts and optimizing tiers.

use vo_runtime::bytecode::Module;
use vo_runtime::instruction::Opcode;

use crate::{JitError, MAX_JIT_COMPILE_WORK_BYTES};

#[derive(Debug)]
pub(crate) struct ModuleCallGraph {
    callers: Box<[Box<[usize]>]>,
    callees: Box<[Box<[usize]>]>,
    component_ids: Box<[usize]>,
    recursive_components: Box<[bool]>,
    component_members: Box<[Box<[usize]>]>,
    component_callees: Box<[Box<[usize]>]>,
}

impl ModuleCallGraph {
    #[cfg(test)]
    pub(crate) fn build(module: &Module) -> Self {
        Self::build_with_limit(module, crate::MAX_JIT_ANALYSIS_BYTES)
            .expect("test call graph must fit the standard analysis budget")
    }

    pub(crate) fn build_with_limit(module: &Module, limit_bytes: usize) -> Result<Self, JitError> {
        let function_count = module.functions.len();
        let fixed_bytes =
            core::mem::size_of::<Self>().saturating_add(function_count.saturating_mul(
                core::mem::size_of::<Box<[usize]>>() * 2
                    + core::mem::size_of::<usize>()
                    + core::mem::size_of::<bool>()
                    + core::mem::size_of::<Box<[usize]>>() * 2,
            ));
        if fixed_bytes > limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes,
                requested_bytes: fixed_bytes,
            });
        }
        let raw_edge_count = module
            .functions
            .iter()
            .flat_map(|function| &function.code)
            .filter(|instruction| instruction.opcode() == Opcode::Call)
            .count();
        let requested_work_bytes = fixed_bytes
            .saturating_add(raw_edge_count.saturating_mul(core::mem::size_of::<usize>() * 2));
        if requested_work_bytes > MAX_JIT_COMPILE_WORK_BYTES {
            return Err(JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes: requested_work_bytes,
            });
        }
        let mut callees = vec![Vec::<usize>::new(); function_count];
        for (caller_id, function) in module.functions.iter().enumerate() {
            for instruction in &function.code {
                if instruction.opcode() != Opcode::Call {
                    continue;
                }
                let callee_id = instruction.static_call_func_id() as usize;
                if callee_id < function_count {
                    callees[caller_id].push(callee_id);
                }
            }
        }
        for edges in &mut callees {
            edges.sort_unstable();
            edges.dedup();
        }
        let edge_count = callees.iter().map(Vec::len).sum::<usize>();
        let requested_bytes = fixed_bytes
            .saturating_add(edge_count.saturating_mul(core::mem::size_of::<usize>() * 2));
        if requested_bytes > limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes,
                requested_bytes,
            });
        }
        let mut callers = vec![Vec::<usize>::new(); function_count];
        for (caller_id, edges) in callees.iter().enumerate() {
            for &callee_id in edges {
                callers[callee_id].push(caller_id);
            }
        }

        let (component_ids, recursive_components) = components(&callees, &callers);
        let mut component_members = vec![Vec::new(); recursive_components.len()];
        for (func_id, &component) in component_ids.iter().enumerate() {
            component_members[component].push(func_id);
        }
        let mut component_callees = vec![Vec::new(); recursive_components.len()];
        for (caller, targets) in callees.iter().enumerate() {
            let caller_component = component_ids[caller];
            for &callee in targets {
                let callee_component = component_ids[callee];
                if caller_component != callee_component {
                    component_callees[caller_component].push(callee_component);
                }
            }
        }
        for targets in &mut component_callees {
            targets.sort_unstable();
            targets.dedup();
        }
        let graph = Self {
            callers: callers
                .into_iter()
                .map(Vec::into_boxed_slice)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            callees: callees
                .into_iter()
                .map(Vec::into_boxed_slice)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            component_ids: component_ids.into_boxed_slice(),
            recursive_components: recursive_components.into_boxed_slice(),
            component_members: component_members
                .into_iter()
                .map(Vec::into_boxed_slice)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            component_callees: component_callees
                .into_iter()
                .map(Vec::into_boxed_slice)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        };
        let requested_bytes = graph.retained_bytes();
        if requested_bytes > limit_bytes {
            return Err(JitError::AnalysisResourceLimitExceeded {
                limit_bytes,
                requested_bytes,
            });
        }
        Ok(graph)
    }

    pub(crate) fn retained_bytes(&self) -> usize {
        core::mem::size_of::<Self>()
            .saturating_add(
                self.callers
                    .len()
                    .saturating_mul(core::mem::size_of::<Box<[usize]>>()),
            )
            .saturating_add(
                self.callers
                    .iter()
                    .map(|edges| edges.len().saturating_mul(core::mem::size_of::<usize>()))
                    .sum::<usize>(),
            )
            .saturating_add(
                self.callees
                    .len()
                    .saturating_mul(core::mem::size_of::<Box<[usize]>>()),
            )
            .saturating_add(
                self.callees
                    .iter()
                    .map(|edges| edges.len().saturating_mul(core::mem::size_of::<usize>()))
                    .sum::<usize>(),
            )
            .saturating_add(
                self.component_ids
                    .len()
                    .saturating_mul(core::mem::size_of::<usize>()),
            )
            .saturating_add(
                self.recursive_components
                    .len()
                    .saturating_mul(core::mem::size_of::<bool>()),
            )
            .saturating_add(
                self.component_members
                    .len()
                    .saturating_mul(core::mem::size_of::<Box<[usize]>>()),
            )
            .saturating_add(
                self.component_members
                    .iter()
                    .map(|members| members.len().saturating_mul(core::mem::size_of::<usize>()))
                    .sum::<usize>(),
            )
            .saturating_add(
                self.component_callees
                    .len()
                    .saturating_mul(core::mem::size_of::<Box<[usize]>>()),
            )
            .saturating_add(
                self.component_callees
                    .iter()
                    .map(|callees| callees.len().saturating_mul(core::mem::size_of::<usize>()))
                    .sum::<usize>(),
            )
    }

    #[inline]
    pub(crate) fn callers(&self, func_id: usize) -> &[usize] {
        self.callers.get(func_id).map_or(&[], AsRef::as_ref)
    }

    #[inline]
    pub(crate) fn callees(&self, func_id: usize) -> &[usize] {
        self.callees.get(func_id).map_or(&[], AsRef::as_ref)
    }

    #[inline]
    pub(crate) fn component_id(&self, func_id: usize) -> Option<usize> {
        self.component_ids.get(func_id).copied()
    }

    #[inline]
    pub(crate) fn component_count(&self) -> usize {
        self.component_members.len()
    }

    #[inline]
    pub(crate) fn component_members(&self, component: usize) -> &[usize] {
        self.component_members
            .get(component)
            .map_or(&[], AsRef::as_ref)
    }

    #[inline]
    pub(crate) fn component_callees(&self, component: usize) -> &[usize] {
        self.component_callees
            .get(component)
            .map_or(&[], AsRef::as_ref)
    }

    #[inline]
    pub(crate) fn same_component(&self, lhs: usize, rhs: usize) -> bool {
        self.component_ids.get(lhs) == self.component_ids.get(rhs)
    }

    #[inline]
    pub(crate) fn is_recursive_function(&self, func_id: usize) -> bool {
        self.component_ids
            .get(func_id)
            .and_then(|component| self.recursive_components.get(*component))
            .copied()
            .unwrap_or(false)
    }

    #[inline]
    pub(crate) fn is_recursive_edge(&self, caller: usize, callee: usize) -> bool {
        self.same_component(caller, callee) && self.is_recursive_function(caller)
    }
}

fn components(callees: &[Vec<usize>], callers: &[Vec<usize>]) -> (Vec<usize>, Vec<bool>) {
    let function_count = callees.len();
    let mut visited = vec![false; function_count];
    let mut finish_order = Vec::with_capacity(function_count);

    for start in 0..function_count {
        if visited[start] {
            continue;
        }
        visited[start] = true;
        let mut stack = vec![(start, 0usize)];
        while let Some((node, next_edge)) = stack.last_mut() {
            if let Some(&callee) = callees[*node].get(*next_edge) {
                *next_edge += 1;
                if !visited[callee] {
                    visited[callee] = true;
                    stack.push((callee, 0));
                }
            } else {
                finish_order.push(*node);
                stack.pop();
            }
        }
    }

    let mut component_ids = vec![usize::MAX; function_count];
    let mut component_sizes = Vec::new();
    for &start in finish_order.iter().rev() {
        if component_ids[start] != usize::MAX {
            continue;
        }
        let component_id = component_sizes.len();
        let mut size = 0usize;
        let mut stack = vec![start];
        component_ids[start] = component_id;
        while let Some(node) = stack.pop() {
            size += 1;
            for &caller in &callers[node] {
                if component_ids[caller] == usize::MAX {
                    component_ids[caller] = component_id;
                    stack.push(caller);
                }
            }
        }
        component_sizes.push(size);
    }

    let mut recursive_components = component_sizes
        .iter()
        .map(|size| *size > 1)
        .collect::<Vec<_>>();
    for (caller, targets) in callees.iter().enumerate() {
        if targets.contains(&caller) {
            recursive_components[component_ids[caller]] = true;
        }
    }
    (component_ids, recursive_components)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::function_with_sig;
    use vo_runtime::bytecode::Module;
    use vo_runtime::instruction::Instruction;

    #[test]
    fn classifies_self_and_mutual_recursion_from_one_graph() {
        let mut module = Module::new("call-graph-scc".into());
        module.functions = vec![
            function_with_sig(vec![Instruction::new(Opcode::Call, 0, 0, 0)], 0, 0, 1, 0),
            function_with_sig(vec![Instruction::new(Opcode::Call, 2, 0, 0)], 0, 0, 1, 0),
            function_with_sig(vec![Instruction::new(Opcode::Call, 1, 0, 0)], 0, 0, 1, 0),
        ];

        let graph = ModuleCallGraph::build(&module);
        assert!(graph.is_recursive_edge(0, 0));
        assert!(graph.is_recursive_edge(1, 2));
        assert!(graph.is_recursive_edge(2, 1));
        assert!(!graph.same_component(0, 1));
        assert_eq!(graph.callers(1), &[2]);
        assert_eq!(graph.callees(1), &[2]);
    }

    #[test]
    fn graph_budget_rejects_retained_edges() {
        let mut module = Module::new("bounded-call-graph".into());
        module.functions = vec![
            function_with_sig(vec![Instruction::new(Opcode::Call, 1, 0, 0)], 0, 0, 1, 0),
            function_with_sig(vec![Instruction::new(Opcode::Return, 0, 0, 0)], 0, 0, 1, 0),
        ];
        let mut empty_module = module.clone();
        for function in &mut empty_module.functions {
            function.code.clear();
        }
        let empty_graph =
            ModuleCallGraph::build_with_limit(&empty_module, crate::MAX_JIT_ANALYSIS_BYTES)
                .expect("empty graph");
        let limit = empty_graph.retained_bytes();

        let error = ModuleCallGraph::build_with_limit(&module, limit)
            .expect_err("one retained edge must exceed an empty graph budget");
        assert!(matches!(
            error,
            JitError::AnalysisResourceLimitExceeded {
                limit_bytes,
                requested_bytes,
            } if limit_bytes == limit && requested_bytes > limit
        ));
    }
}
