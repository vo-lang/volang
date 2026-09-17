//! Forward cooperative fuel reads through SSA while preserving every original
//! write. This pass consumes our native lowering, not arbitrary Cranelift IR.
//!
//! A JitContext cannot alias guest storage or other incoming pointer arguments.
//! Lowering may save it in a native frame record; observers of that record run
//! only at calls. Context memory remains current at every instruction; every
//! call (including helpers without a context argument) invalidates cached fuel.
//! Unsupported uses of the context reject the whole transformation before edits.

use cranelift_codegen::cursor::{Cursor, FuncCursor};
use cranelift_codegen::ir::{
    types, Block, Function, Inst, InstBuilder, InstructionData, MemFlagsData, Opcode, Value,
    ValueDef,
};
use vo_runtime::jit_api::JitContextField;

mod branches;
mod phis;

const MAX_BLOCKS: usize = 4_096;
const MAX_INSTS: usize = 65_536;
const MAX_EDGES: usize = 8_192;
const MAX_OPERANDS: usize = 262_144;

#[derive(Clone, Copy)]
enum Action {
    Read(Value),
    Write(Value),
    Call,
    Keep,
}

struct Plan {
    ctx: Value,
    blocks: Vec<(Block, Vec<(Inst, Action)>)>,
}

/// All existing checkpoints, costs, refill outcomes and resumable PCs remain
/// intact. The fixed workspace/edge limits bound optional compile work; a
/// rejected or acyclic artifact retains its original IR byte for byte.
pub(crate) fn forward_execution_budget(func: &mut Function) -> bool {
    let Some(plan) = plan(func) else {
        return false;
    };
    let entry = func.layout.entry_block().unwrap();
    let mut incoming = vec![None; func.dfg.num_blocks()];
    let first = func.layout.first_inst(entry).unwrap();
    incoming[entry.as_u32() as usize] =
        Some(load(&mut FuncCursor::new(func).at_inst(first), plan.ctx));
    for &(block, _) in &plan.blocks {
        if block != entry {
            incoming[block.as_u32() as usize] =
                Some(func.dfg.append_block_param(block, types::I32));
        }
    }
    for (block, insts) in plan.blocks {
        let mut fuel = incoming[block.as_u32() as usize].unwrap();
        for (inst, action) in insts {
            match action {
                Action::Read(result) => {
                    func.dfg.clear_results(inst);
                    func.dfg.change_to_alias(result, fuel);
                    func.layout.remove_inst(inst);
                }
                Action::Write(value) => {
                    fuel = value;
                }
                Action::Call => {
                    fuel = load(&mut FuncCursor::new(func).after_inst(inst), plan.ctx);
                }
                Action::Keep => {}
            }
        }
        let last = func.layout.last_inst(block).unwrap();
        branches::append_fuel(func, last, fuel);
    }
    phis::prune(func, &incoming);
    true
}

fn load(cursor: &mut FuncCursor<'_>, ctx: Value) -> Value {
    cursor.ins().load(
        types::I32,
        MemFlagsData::trusted(),
        ctx,
        JitContextField::ExecutionBudget.offset(),
    )
}

fn plan(func: &Function) -> Option<Plan> {
    let count = func.dfg.num_blocks();
    if count > MAX_BLOCKS || func.dfg.num_insts() > MAX_INSTS {
        return None;
    }
    let entry = func.layout.entry_block()?;
    let ctx = *func.dfg.block_params(entry).first()?;
    let mut predecessors = vec![0_usize; count];
    let mut successors = vec![Vec::new(); count];
    let mut blocks = Vec::new();
    let mut edges = 0;
    let mut operands = 0_usize;
    let mut writes = 0;
    for block in func.layout.blocks() {
        // Cranelift encodes block parameter indices in u16. Keep an oversized
        // original artifact intact instead of overflowing optional SSA state.
        if block != entry && func.dfg.num_block_params(block) >= usize::from(u16::MAX) {
            return None;
        }
        let mut insts = Vec::new();
        for inst in func.layout.block_insts(block) {
            operands = operands.saturating_add(func.dfg.inst_args(inst).len());
            if operands > MAX_OPERANDS {
                return None;
            }
            let action = classify(func, inst, ctx)?;
            writes += usize::from(matches!(action, Action::Write(_)));
            insts.push((inst, action));
        }
        let last = func.layout.last_inst(block)?;
        for edge in func.dfg.insts[last]
            .branch_destination(&func.dfg.jump_tables, &func.dfg.exception_tables)
        {
            edges += 1;
            operands = operands.saturating_add(edge.len(&func.dfg.value_lists));
            if edges > MAX_EDGES || operands > MAX_OPERANDS {
                return None;
            }
            let target = edge.block(&func.dfg.value_lists);
            if target == entry {
                return None;
            }
            // A new alias of ctx through a block parameter would need its own
            // proof. Ordinary lowering keeps the ABI context value dominating.
            if edge.args(&func.dfg.value_lists).any(|arg| {
                arg.as_value()
                    .is_some_and(|v| func.dfg.resolve_aliases(v) == ctx)
            }) {
                return None;
            }
            predecessors[target.as_u32() as usize] += 1;
            successors[block.as_u32() as usize].push(target);
        }
        blocks.push((block, insts));
    }
    if writes == 0 {
        return None;
    }
    // Kahn traversal bounds work even for irreducible or unreachable cycles.
    // Straight-line artifacts would only gain extra call-boundary traffic.
    let mut pending: Vec<_> = blocks
        .iter()
        .map(|(b, _)| *b)
        .filter(|b| predecessors[b.as_u32() as usize] == 0)
        .collect();
    let mut visited = 0;
    while let Some(block) = pending.pop() {
        visited += 1;
        for target in &successors[block.as_u32() as usize] {
            let degree = &mut predecessors[target.as_u32() as usize];
            *degree -= 1;
            if *degree == 0 {
                pending.push(*target);
            }
        }
    }
    (visited != blocks.len()).then_some(Plan { ctx, blocks })
}

fn classify(func: &Function, inst: Inst, ctx: Value) -> Option<Action> {
    let data = &func.dfg.insts[inst];
    let opcode = data.opcode();
    if opcode.is_branch() && !matches!(opcode, Opcode::Jump | Opcode::Brif | Opcode::BrTable) {
        return None;
    }
    if opcode.is_call() {
        return matches!(opcode, Opcode::Call | Opcode::CallIndirect).then_some(Action::Call);
    }
    let uses_ctx = func
        .dfg
        .inst_args(inst)
        .iter()
        .any(|&v| func.dfg.resolve_aliases(v) == ctx);
    if uses_ctx {
        let (offset, ty, action) = match *data {
            InstructionData::Load { arg, offset, .. } if func.dfg.resolve_aliases(arg) == ctx => {
                let result = func.dfg.inst_results(inst)[0];
                (
                    i64::from(offset),
                    func.dfg.value_type(result),
                    Action::Read(result),
                )
            }
            InstructionData::Store { args, offset, .. }
                if func.dfg.resolve_aliases(args[1]) == ctx
                    && func.dfg.resolve_aliases(args[0]) != ctx =>
            {
                (
                    i64::from(offset),
                    func.dfg.value_type(args[0]),
                    Action::Write(args[0]),
                )
            }
            // Our native-frame record retains ctx only for call-boundary root
            // scanning. No guest pointer can read it while native code runs.
            InstructionData::Store { args, .. }
                if opcode == Opcode::Store && func.dfg.resolve_aliases(args[0]) == ctx =>
            {
                let address = func.dfg.resolve_aliases(args[1]);
                return matches!(func.dfg.value_def(address), ValueDef::Result(i, _)
                    if func.dfg.insts[i].opcode() == Opcode::StackAddr)
                .then_some(Action::Keep);
            }
            _ => return None,
        };
        let budget = i64::from(JitContextField::ExecutionBudget.offset());
        // Using the result width also conservatively rejects narrow loads near
        // fuel. Aliased, partial, atomic or wide budget accesses are never lifted.
        if offset < budget + 4 && offset + i64::from(ty.bytes()) > budget {
            return (offset == budget
                && ty == types::I32
                && matches!(opcode, Opcode::Load | Opcode::Store)
                && data
                    .memflags()
                    .is_some_and(|f| func.dfg.mem_flags[f] == MemFlagsData::trusted()))
            .then_some(action);
        }
    }
    if opcode.is_terminator()
        && !matches!(
            opcode,
            Opcode::Jump | Opcode::Brif | Opcode::BrTable | Opcode::Return | Opcode::Trap
        )
    {
        return None;
    }
    Some(Action::Keep)
}

#[cfg(test)]
mod poll_tests;
#[cfg(test)]
mod tests;
