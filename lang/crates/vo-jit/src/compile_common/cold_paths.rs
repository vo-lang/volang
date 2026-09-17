use cranelift_codegen::ir::Function;

/// Keep the descendants of a slow path out of the ordinary code layout.
///
/// Lowering marks the entry of an error, materialization, or budget-refill
/// path cold. Helpers inside that path may introduce further blocks. Cranelift
/// treats those blocks independently, so propagate the hint until a block can
/// also be reached without crossing any cold entry. This includes loops within
/// a slow path while preserving joins and every ordinary/continuation entry.
/// Only layout hints change; instructions, edges, and safepoints stay intact.
pub(crate) fn propagate_cold_paths(func: &mut Function) {
    // This optional layout pass has a fixed workspace and traversal budget.
    // Oversized functions retain the explicit hints supplied by lowering.
    const MAX_BLOCKS: usize = 16_384;
    const MAX_EDGES: usize = 65_536;
    let count = func.dfg.num_blocks();
    if count > MAX_BLOCKS {
        return;
    }
    let Some(entry) = func.layout.entry_block() else {
        return;
    };
    if func.layout.is_cold(entry) {
        return;
    }
    let mut ordinary = vec![false; count];
    let mut pending = vec![entry];
    ordinary[entry.as_u32() as usize] = true;
    let mut edges = 0_usize;
    while let Some(block) = pending.pop() {
        let Some(last) = func.layout.last_inst(block) else {
            continue;
        };
        let successors = func.dfg.insts[last]
            .branch_destination(&func.dfg.jump_tables, &func.dfg.exception_tables);
        edges = edges.saturating_add(successors.len());
        if edges > MAX_EDGES {
            return;
        }
        for successor in successors {
            let successor = successor.block(&func.dfg.value_lists);
            let index = successor.as_u32() as usize;
            if !ordinary[index] && !func.layout.is_cold(successor) {
                ordinary[index] = true;
                pending.push(successor);
            }
        }
    }
    let mut block = func.layout.entry_block();
    while let Some(current) = block {
        block = func.layout.next_block(current);
        if !ordinary[current.as_u32() as usize] {
            func.layout.set_cold(current);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cranelift_codegen::ir::{types, InstBuilder, JumpTableData};
    use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

    #[test]
    fn cold_cycle_and_nested_helper_preserve_the_ordinary_join() {
        let mut func = Function::new();
        let mut context = FunctionBuilderContext::new();
        let mut b = FunctionBuilder::new(&mut func, &mut context);
        let entry = b.create_block();
        let hot = b.create_block();
        let cold = b.create_block();
        let nested = b.create_block();
        let retry = b.create_block();
        let join = b.create_block();
        b.set_cold_block(cold);
        b.switch_to_block(entry);
        let condition = b.ins().iconst(types::I32, 1);
        b.ins().brif(condition, cold, &[], hot, &[]);
        b.switch_to_block(hot);
        b.ins().jump(join, &[]);
        b.switch_to_block(cold);
        b.ins().jump(nested, &[]);
        b.switch_to_block(nested);
        b.ins().brif(condition, retry, &[], join, &[]);
        b.switch_to_block(retry);
        b.ins().jump(nested, &[]);
        b.switch_to_block(join);
        b.ins().return_(&[]);
        b.seal_all_blocks();
        b.finalize(crate::compiler::test_frontend_config());
        let instructions = func.dfg.num_insts();
        propagate_cold_paths(&mut func);
        for block in [cold, nested, retry] {
            assert!(func.layout.is_cold(block));
        }
        for block in [entry, hot, join] {
            assert!(!func.layout.is_cold(block));
        }
        assert_eq!(func.dfg.num_insts(), instructions);
    }

    #[test]
    fn dispatch_reaches_all_continuation_targets_and_their_cycles() {
        let mut func = Function::new();
        let mut context = FunctionBuilderContext::new();
        let mut b = FunctionBuilder::new(&mut func, &mut context);
        let entry = b.create_block();
        let cold = b.create_block();
        let first = b.create_block();
        let second = b.create_block();
        b.set_cold_block(cold);
        b.switch_to_block(entry);
        let selector = b.ins().iconst(types::I32, 0);
        let first_call = b.func.dfg.block_call(first, &[]);
        let second_call = b.func.dfg.block_call(second, &[]);
        let table = b.create_jump_table(JumpTableData::new(first_call, &[second_call]));
        b.ins().br_table(selector, table);
        b.switch_to_block(first);
        b.ins().brif(selector, cold, &[], second, &[]);
        b.switch_to_block(cold);
        b.ins().jump(second, &[]);
        b.switch_to_block(second);
        b.ins().jump(first, &[]);
        b.seal_all_blocks();
        b.finalize(crate::compiler::test_frontend_config());
        propagate_cold_paths(&mut func);
        assert!(func.layout.is_cold(cold));
        for block in [entry, first, second] {
            assert!(!func.layout.is_cold(block));
        }
    }

    #[test]
    fn workspace_limit_keeps_the_original_hints() {
        let mut func = Function::new();
        for _ in 0..16_385 {
            let block = func.dfg.make_block();
            func.layout.append_block(block);
        }
        let first = func.layout.entry_block().unwrap();
        let second = func.layout.next_block(first).unwrap();
        func.layout.set_cold(second);
        propagate_cold_paths(&mut func);
        assert_eq!(
            func.layout
                .blocks()
                .filter(|&b| func.layout.is_cold(b))
                .count(),
            1
        );
    }
}
