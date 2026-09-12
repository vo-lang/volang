use cranelift_codegen::ir::{BlockCall, Function, Inst, InstructionData, JumpTableData, Value};

/// Each branch must own its fuel argument. Jump tables can share both the table
/// itself and individual BlockCall lists, including default/duplicate targets.
/// Clone them per instruction and per edge before appending any mutable state.
pub(super) fn append_fuel(func: &mut Function, inst: Inst, fuel: Value) {
    let dfg = &mut func.dfg;
    if let InstructionData::BranchTable { table, .. } = dfg.insts[inst] {
        // Admission counted every edge and operand use, including repetitions.
        let branches: Vec<_> = dfg.jump_tables[table]
            .all_branches()
            .iter()
            .map(|edge| {
                (
                    edge.block(&dfg.value_lists),
                    edge.args(&dfg.value_lists).collect::<Vec<_>>(),
                )
            })
            .collect();
        let copies: Vec<_> = branches
            .into_iter()
            .map(|(block, mut args)| {
                args.push(fuel.into());
                BlockCall::new(block, args, &mut dfg.value_lists)
            })
            .collect();
        let copy = dfg
            .jump_tables
            .push(JumpTableData::new(copies[0], &copies[1..]));
        if let InstructionData::BranchTable { table, .. } = &mut dfg.insts[inst] {
            *table = copy;
        }
    } else {
        // Jump/Brif lowering creates independent BlockCalls.
        for edge in
            dfg.insts[inst].branch_destination_mut(&mut dfg.jump_tables, &mut dfg.exception_tables)
        {
            edge.append_argument(fuel, &mut dfg.value_lists);
        }
    }
}
