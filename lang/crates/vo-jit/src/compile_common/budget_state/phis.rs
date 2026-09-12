//! Keep the promoted variable's SSA sparse. A block parameter that only feeds
//! another budget parameter need not remain live. Cranelift's constant-phi pass
//! does not merge arbitrary nonconstant phi copies, so remove those here too.

use cranelift_codegen::ir::{Function, Inst, Value};
use std::collections::BTreeMap;

#[derive(Clone, Copy)]
struct Edge {
    inst: Inst,
    branch: usize,
    value: Value,
}

pub(super) fn prune(func: &mut Function, incoming: &[Option<Value>]) {
    let entry = func.layout.entry_block().unwrap();
    let initial = incoming[entry.as_u32() as usize].unwrap();
    let mut parameters = incoming.to_vec();
    parameters[entry.as_u32() as usize] = None;
    let indices: BTreeMap<_, _> = parameters
        .iter()
        .enumerate()
        .filter_map(|(index, value)| value.map(|v| (v, index)))
        .collect();
    let mut edges = vec![Vec::new(); parameters.len()];
    let mut live = vec![false; parameters.len()];
    let mut pending = Vec::new();
    let seed = |v, live: &mut [bool], pending: &mut Vec<usize>| {
        if let Some(&index) = indices.get(&func.dfg.resolve_aliases(v)) {
            if !live[index] {
                live[index] = true;
                pending.push(index);
            }
        }
    };
    for block in func.layout.blocks() {
        for inst in func.layout.block_insts(block) {
            for &value in func.dfg.inst_args(inst) {
                seed(value, &mut live, &mut pending);
            }
        }
        let inst = func.layout.last_inst(block).unwrap();
        for (branch, edge) in func.dfg.insts[inst]
            .branch_destination(&func.dfg.jump_tables, &func.dfg.exception_tables)
            .iter()
            .enumerate()
        {
            let mut args = edge.args(&func.dfg.value_lists);
            let value = args.next_back().unwrap().as_value().unwrap();
            let index = edge.block(&func.dfg.value_lists).as_u32() as usize;
            edges[index].push(Edge {
                inst,
                branch,
                value,
            });
            // Existing non-budget parameters are outside this pass. Preserve
            // any fuel value passed to them, even if they are themselves dead.
            for arg in args {
                if let Some(value) = arg.as_value() {
                    seed(value, &mut live, &mut pending);
                }
            }
        }
    }
    while let Some(index) = pending.pop() {
        for edge in &edges[index] {
            seed(edge.value, &mut live, &mut pending);
        }
    }
    for (index, parameter) in parameters.iter_mut().enumerate() {
        if !live[index] {
            if let Some(phi) = parameter.take() {
                remove(func, phi, initial, &edges[index]);
            }
        }
    }
    // Each round scans at most the already-admitted edge count. Eight rounds
    // bound optional cleanup; any remaining nontrivial copies remain valid SSA.
    for _ in 0..8 {
        let mut changed = false;
        for (index, parameter) in parameters.iter_mut().enumerate() {
            let Some(phi) = *parameter else { continue };
            let mut values = edges[index]
                .iter()
                .map(|e| func.dfg.resolve_aliases(e.value))
                .filter(|&v| v != phi);
            let Some(value) = values.next() else { continue };
            if values.all(|other| other == value) {
                remove(func, phi, value, &edges[index]);
                *parameter = None;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}

fn remove(func: &mut Function, phi: Value, replacement: Value, incoming: &[Edge]) {
    let cranelift_codegen::ir::ValueDef::Param(block, index) = func.dfg.value_def(phi) else {
        unreachable!("only newly appended budget parameters can be removed");
    };
    debug_assert_eq!(func.dfg.block_params(block).last(), Some(&phi));
    func.dfg.swap_remove_block_param(phi);
    func.dfg.change_to_alias(phi, replacement);
    for edge in incoming {
        let dfg = &mut func.dfg;
        let targets = dfg.insts[edge.inst]
            .branch_destination_mut(&mut dfg.jump_tables, &mut dfg.exception_tables);
        targets[edge.branch].remove(index, &mut dfg.value_lists);
    }
}
