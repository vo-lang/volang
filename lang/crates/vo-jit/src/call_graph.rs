//! Immutable module call graph shared by contracts and optimizing tiers.

use vo_runtime::bytecode::Module;
use vo_runtime::instruction::Opcode;

#[derive(Debug)]
pub(crate) struct ModuleCallGraph {
    callers: Box<[Box<[usize]>]>,
    callees: Box<[Box<[usize]>]>,
    component_ids: Box<[usize]>,
    recursive_components: Box<[bool]>,
}

impl ModuleCallGraph {
    pub(crate) fn build(module: &Module) -> Self {
        let function_count = module.functions.len();
        let mut callers = vec![Vec::<usize>::new(); function_count];
        let mut callees = vec![Vec::<usize>::new(); function_count];
        for (caller_id, function) in module.functions.iter().enumerate() {
            for instruction in &function.code {
                if instruction.opcode() != Opcode::Call {
                    continue;
                }
                let callee_id = instruction.static_call_func_id() as usize;
                if callee_id < function_count {
                    callers[callee_id].push(caller_id);
                    callees[caller_id].push(callee_id);
                }
            }
        }
        for edges in callers.iter_mut().chain(callees.iter_mut()) {
            edges.sort_unstable();
            edges.dedup();
        }

        let (component_ids, recursive_components) = components(&callees, &callers);
        Self {
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
        }
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
}
