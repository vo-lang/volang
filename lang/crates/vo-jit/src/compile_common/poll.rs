use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, InstBuilder, MemFlags, Value};
use cranelift_frontend::FunctionBuilder;
use vo_runtime::jit_api::JitContext;

/// The two successors created by a cooperative execution-budget check.
pub(crate) struct ExecutionBudgetPollBlocks {
    pub(crate) exhausted: Block,
    pub(crate) ready: Block,
    remaining: Value,
    cost: i64,
}

/// Terminate the current block with a scheduler-turn budget check.
///
/// The budget lives in `JitContext`, making it visible to nested native calls
/// and to every VM/JIT bridge in the same scheduler turn.
pub(crate) fn branch_on_execution_budget(
    builder: &mut FunctionBuilder<'_>,
    ctx: Value,
    bytecode_cost: u32,
) -> ExecutionBudgetPollBlocks {
    let cost = bytecode_cost.max(1);
    let remaining = builder.ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_EXECUTION_BUDGET,
    );
    let exhausted = builder
        .ins()
        .icmp_imm(IntCC::UnsignedLessThan, remaining, i64::from(cost));
    let exhausted_block = builder.create_block();
    let ready_block = builder.create_block();
    builder
        .ins()
        .brif(exhausted, exhausted_block, &[], ready_block, &[]);

    ExecutionBudgetPollBlocks {
        exhausted: exhausted_block,
        ready: ready_block,
        remaining,
        cost: i64::from(cost),
    }
}

/// Enter the non-exhausted successor and charge the region about to execute.
pub(crate) fn continue_after_execution_budget_poll(
    builder: &mut FunctionBuilder<'_>,
    ctx: Value,
    poll: &ExecutionBudgetPollBlocks,
) {
    builder.switch_to_block(poll.ready);
    builder.seal_block(poll.ready);
    let updated = builder.ins().iadd_imm(poll.remaining, -poll.cost);
    builder.ins().store(
        MemFlags::trusted(),
        updated,
        ctx,
        JitContext::OFFSET_EXECUTION_BUDGET,
    );
}

/// Publish a cooperative scheduler yield after the caller has spilled all
/// VM-visible frame state.
pub(crate) fn emit_cooperative_yield_return(
    builder: &mut FunctionBuilder<'_>,
    ctx: Value,
    resume_pc: usize,
) {
    let resume_pc = builder.ins().iconst(
        types::I32,
        i64::from(u32::try_from(resume_pc).unwrap_or(u32::MAX)),
    );
    builder.ins().store(
        MemFlags::trusted(),
        resume_pc,
        ctx,
        JitContext::OFFSET_CALL_RESUME_PC,
    );
    let call_kind = builder
        .ins()
        .iconst(types::I8, i64::from(JitContext::CALL_KIND_YIELD));
    builder.ins().store(
        MemFlags::trusted(),
        call_kind,
        ctx,
        JitContext::OFFSET_CALL_KIND,
    );
    let result = builder
        .ins()
        .iconst(types::I32, i64::from(JitContext::JIT_RESULT_CALL));
    builder.ins().return_(&[result]);
}
