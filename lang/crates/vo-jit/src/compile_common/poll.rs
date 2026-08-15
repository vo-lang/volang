use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, FuncRef, InstBuilder, MemFlagsData as MemFlags, Value};
use cranelift_frontend::FunctionBuilder;
use vo_runtime::jit_api::{JitContext, JitContextField};

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
    refill_execution_budget: FuncRef,
) -> ExecutionBudgetPollBlocks {
    let cost = bytecode_cost.max(1);
    let remaining = builder.ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContextField::ExecutionBudget.offset(),
    );
    let exhausted = builder
        .ins()
        .icmp_imm_u(IntCC::UnsignedLessThan, remaining, i64::from(cost));
    let refill_block = super::cold_block(builder);
    let exhausted_block = super::cold_block(builder);
    let ready_block = builder.create_block();
    builder.append_block_param(ready_block, types::I32);
    builder.ins().brif(
        exhausted,
        refill_block,
        &[],
        ready_block,
        &[remaining.into()],
    );

    builder.switch_to_block(refill_block);
    builder.seal_block(refill_block);
    let required = builder.ins().iconst(types::I32, i64::from(cost));
    let call = builder
        .ins()
        .call(refill_execution_budget, &[ctx, required]);
    let refilled = builder.func.dfg.inst_results(call)[0];
    let must_yield = builder
        .ins()
        .icmp_imm_u(IntCC::UnsignedLessThan, refilled, i64::from(cost));
    builder.ins().brif(
        must_yield,
        exhausted_block,
        &[],
        ready_block,
        &[refilled.into()],
    );

    let remaining = builder.block_params(ready_block)[0];

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
    let updated = builder.ins().iadd_imm_s(poll.remaining, -poll.cost);
    builder.ins().store(
        MemFlags::trusted(),
        updated,
        ctx,
        JitContextField::ExecutionBudget.offset(),
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
        JitContextField::CallResumePc.offset(),
    );
    let call_kind = builder
        .ins()
        .iconst(types::I8, i64::from(JitContext::CALL_KIND_YIELD));
    builder.ins().store(
        MemFlags::trusted(),
        call_kind,
        ctx,
        JitContextField::CallKind.offset(),
    );
    let result = builder
        .ins()
        .iconst(types::I32, i64::from(JitContext::JIT_RESULT_CALL));
    builder.ins().return_(&[result]);
}
