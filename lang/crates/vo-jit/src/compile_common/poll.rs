use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, FuncRef, InstBuilder, MemFlagsData as MemFlags, Value};
use cranelift_frontend::FunctionBuilder;
use vo_runtime::gc::JitGcPollField;
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
    let exhausted_block = super::cold_block(builder);
    let ready_block = builder.create_block();
    builder.append_block_param(ready_block, types::I32);
    builder.ins().brif(
        exhausted,
        exhausted_block,
        &[],
        ready_block,
        &[remaining.into()],
    );

    let remaining = builder.block_params(ready_block)[0];

    ExecutionBudgetPollBlocks {
        exhausted: exhausted_block,
        ready: ready_block,
        remaining,
        cost: i64::from(cost),
    }
}

/// Ask the VM for another native execution lease after the caller has serviced
/// the GC safepoint for this checkpoint. A zero or undersized grant requires a
/// real scheduler boundary; a sufficient grant rejoins the ready path.
pub(crate) fn refill_execution_budget(
    builder: &mut FunctionBuilder<'_>,
    ctx: Value,
    refill_execution_budget: FuncRef,
    poll: &ExecutionBudgetPollBlocks,
    yield_block: Block,
) {
    let required = builder.ins().iconst(types::I32, poll.cost);
    let call = builder
        .ins()
        .call(refill_execution_budget, &[ctx, required]);
    let refilled = builder.func.dfg.inst_results(call)[0];
    let must_yield = builder
        .ins()
        .icmp_imm_u(IntCC::UnsignedLessThan, refilled, poll.cost);
    builder
        .ins()
        .brif(must_yield, yield_block, &[], poll.ready, &[refilled.into()]);
}

/// Keep pure native code from renewing across a pending GC request. Such code
/// has no transitive `may_gc` effect, so its callers need not carry active
/// native stack maps. The VM boundary preserves that proof while a clear poll
/// may continue to the ordinary lease callback.
pub(crate) fn continue_if_no_gc_requested(
    builder: &mut FunctionBuilder<'_>,
    ctx: Value,
    yield_block: Block,
) -> Block {
    let gc = builder.ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContextField::Gc.offset(),
    );
    let gc_is_present = builder.ins().icmp_imm_u(IntCC::NotEqual, gc, 0);
    let check_required = builder.create_block();
    let clear = builder.create_block();
    builder
        .ins()
        .brif(gc_is_present, check_required, &[], clear, &[]);

    builder.switch_to_block(check_required);
    builder.seal_block(check_required);
    let required = builder.ins().load(
        types::I8,
        MemFlags::trusted(),
        gc,
        JitGcPollField::Required.offset(),
    );
    let required = builder.ins().icmp_imm_u(IntCC::NotEqual, required, 0);
    builder.ins().brif(required, yield_block, &[], clear, &[]);
    clear
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

/// Publish a cooperative scheduler yield after the caller has materialized the
/// exact VM recovery state.
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
