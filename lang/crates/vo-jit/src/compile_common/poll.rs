use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, InstBuilder, Value};
use cranelift_frontend::{FunctionBuilder, Variable};

/// The two successors created by a cooperative back-edge budget check.
pub(crate) struct BackedgePollBlocks {
    pub(crate) exhausted: Block,
    pub(crate) ready: Block,
    remaining: Value,
    cost: i64,
}

pub(crate) fn initialize_execution_budget(builder: &mut FunctionBuilder<'_>, budget: Variable) {
    builder.declare_var(budget, types::I32);
    let initial = builder.ins().iconst(
        types::I32,
        i64::from(vo_runtime::EXECUTION_TIMESLICE_INSTRUCTIONS),
    );
    builder.def_var(budget, initial);
}

/// Terminate the current block with a budget check for one native back edge.
///
/// The caller owns both successors: `exhausted` must spill state and return a
/// cooperative yield, while `ready` continues to the original branch target.
pub(crate) fn branch_on_execution_budget(
    builder: &mut FunctionBuilder<'_>,
    budget: Variable,
    bytecode_cost: u32,
) -> BackedgePollBlocks {
    let cost = bytecode_cost.max(1);
    let remaining = builder.use_var(budget);
    let exhausted =
        builder
            .ins()
            .icmp_imm(IntCC::UnsignedLessThanOrEqual, remaining, i64::from(cost));
    let exhausted_block = builder.create_block();
    let ready_block = builder.create_block();
    builder
        .ins()
        .brif(exhausted, exhausted_block, &[], ready_block, &[]);

    BackedgePollBlocks {
        exhausted: exhausted_block,
        ready: ready_block,
        remaining,
        cost: i64::from(cost),
    }
}

/// Enter the non-exhausted successor and charge the completed back edge.
pub(crate) fn continue_after_execution_budget_poll(
    builder: &mut FunctionBuilder<'_>,
    budget: Variable,
    poll: &BackedgePollBlocks,
) {
    builder.switch_to_block(poll.ready);
    builder.seal_block(poll.ready);
    let updated = builder.ins().iadd_imm(poll.remaining, -poll.cost);
    builder.def_var(budget, updated);
}

pub(crate) fn backedge_bytecode_cost(current_pc: usize, target_pc: usize) -> u32 {
    current_pc
        .saturating_sub(target_pc)
        .saturating_add(1)
        .try_into()
        .unwrap_or(u32::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn backedge_cost_counts_the_inclusive_bytecode_span() {
        assert_eq!(backedge_bytecode_cost(9, 4), 6);
        assert_eq!(backedge_bytecode_cost(4, 4), 1);
    }
}
