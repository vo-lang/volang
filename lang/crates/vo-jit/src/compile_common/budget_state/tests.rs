use super::*;
use cranelift_codegen::ir::{condcodes::IntCC, AbiParam, JumpTableData};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, Linkage, Module};

const OFFSET: i32 = JitContextField::ExecutionBudget.offset();

// Both versions execute actual machine code, including an observing callback
// that consumes, denies or replenishes fuel. The context buffer is opaque to
// this test program, which accesses only its naturally aligned budget word.
extern "C" fn observe_and_change(ctx: *mut u8, output: *mut u32) -> u32 {
    unsafe {
        let budget = &mut *ctx.add(OFFSET as usize).cast::<u32>();
        *output.add(1) = (*output.add(1)).wrapping_add(*budget);
        *output.add(2) += 1;
        *budget = match *output {
            0 => budget.saturating_sub(1),
            1 => budget.wrapping_add(7),
            _ => 0,
        };
        *budget
    }
}

std::thread_local! {
    static OBSERVED_CONTEXT: std::cell::Cell<*mut u8> = const {
        std::cell::Cell::new(std::ptr::null_mut())
    };
}

extern "C" fn observe_without_context_argument(output: *mut u32) -> u32 {
    OBSERVED_CONTEXT.with(|ctx| observe_and_change(ctx.get(), output))
}

fn fixture(module: &mut JITModule, call_shape: usize) -> (Function, Block) {
    let shared_table = call_shape >= 3;
    let call_shape = call_shape % 3;
    let mut func = module.make_context().func;
    func.signature.params = vec![
        AbiParam::new(types::I64),
        AbiParam::new(types::I32),
        AbiParam::new(types::I64),
    ];
    func.signature.returns.push(AbiParam::new(types::I32));
    let mut callback_sig = module.make_signature();
    callback_sig.params = vec![AbiParam::new(types::I64); if call_shape == 2 { 1 } else { 2 }];
    callback_sig.returns.push(AbiParam::new(types::I32));
    let callback = module
        .declare_function(
            if call_shape == 2 {
                "observe_without_context_argument"
            } else {
                "observe_and_change"
            },
            Linkage::Import,
            &callback_sig,
        )
        .unwrap();
    let callback_ref = module.declare_func_in_func(callback, &mut func);
    let mut frontend = FunctionBuilderContext::new();
    let mut b = FunctionBuilder::new(&mut func, &mut frontend);
    let entry = b.create_block();
    let head = b.create_block();
    let charge = b.create_block();
    let call = b.create_block();
    let back = b.create_block();
    let exit = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.append_block_param(head, types::I32);
    b.append_block_param(back, types::I32);
    b.append_block_param(exit, types::I32);
    b.switch_to_block(entry);
    let ctx = b.block_params(entry)[0];
    let rounds = b.block_params(entry)[1];
    let out = b.block_params(entry)[2];
    let zero = b.ins().iconst(types::I32, 0);
    b.ins().jump(head, &[zero.into()]);
    b.switch_to_block(head);
    let count = b.block_params(head)[0];
    let fuel = b
        .ins()
        .load(types::I32, MemFlagsData::trusted(), ctx, OFFSET);
    let insufficient = b.ins().icmp_imm_u(IntCC::UnsignedLessThan, fuel, 3);
    b.ins()
        .brif(insufficient, exit, &[count.into()], charge, &[]);
    b.switch_to_block(charge);
    let fuel = b.ins().iadd_imm_s(fuel, -3);
    b.ins().store(MemFlagsData::trusted(), fuel, ctx, OFFSET);
    let next = b.ins().iadd_imm_s(count, 1);
    let odd = b.ins().band_imm_u(next, 1);
    b.ins().brif(odd, back, &[next.into()], call, &[]);
    b.switch_to_block(call);
    if call_shape == 1 {
        let signature = b.import_signature(callback_sig);
        let address = b.ins().func_addr(types::I64, callback_ref);
        b.ins().call_indirect(signature, address, &[ctx, out]);
    } else if call_shape == 0 {
        b.ins().call(callback_ref, &[ctx, out]);
    } else {
        b.ins().call(callback_ref, &[out]);
    }
    b.ins().jump(back, &[next.into()]);
    b.switch_to_block(back);
    let next = b.block_params(back)[0];
    let done = b
        .ins()
        .icmp(IntCC::UnsignedGreaterThanOrEqual, next, rounds);
    b.ins()
        .brif(done, exit, &[next.into()], head, &[next.into()]);
    b.switch_to_block(exit);
    let count = b.block_params(exit)[0];
    b.ins().return_(&[count]);
    b.seal_all_blocks();
    b.finalize(module.target_config());
    if shared_table {
        let last = func.layout.last_inst(charge).unwrap();
        let InstructionData::Brif { arg, blocks, .. } = func.dfg.insts[last] else {
            unreachable!()
        };
        // Default and target zero share one list; a second instruction shares
        // the whole table but must supply the post-callback budget value.
        let table = func
            .dfg
            .jump_tables
            .push(JumpTableData::new(blocks[1], &[blocks[1], blocks[0]]));
        func.replace(last).br_table(arg, table);
        let last = func.layout.last_inst(call).unwrap();
        let one = FuncCursor::new(&mut func)
            .at_inst(last)
            .ins()
            .iconst(types::I32, 1);
        func.replace(last).br_table(one, table);
    }
    (func, charge)
}

fn module() -> JITModule {
    let mut builder = JITBuilder::new(default_libcall_names()).unwrap();
    builder.symbol("observe_and_change", observe_and_change as *const u8);
    builder.symbol(
        "observe_without_context_argument",
        observe_without_context_argument as *const u8,
    );
    JITModule::new(builder)
}

#[test]
fn generated_loops_preserve_writes_and_reload_callback_changes() {
    for call_shape in 0..6 {
        let mut module = module();
        let (original, charge) = fixture(&mut module, call_shape);
        let mut candidate = original.clone();
        assert!(forward_execution_budget(&mut candidate));
        for block in candidate.layout.blocks() {
            let predecessors = candidate
                .layout
                .blocks()
                .filter(|&pred| {
                    let last = candidate.layout.last_inst(pred).unwrap();
                    candidate.dfg.insts[last]
                        .branch_destination(
                            &candidate.dfg.jump_tables,
                            &candidate.dfg.exception_tables,
                        )
                        .iter()
                        .any(|edge| edge.block(&candidate.dfg.value_lists) == block)
                })
                .count();
            if predecessors <= 1 {
                assert_eq!(
                    candidate.dfg.num_block_params(block),
                    original.dfg.num_block_params(block),
                    "a single-predecessor block must not add a fuel copy"
                );
            }
        }
        let ctx = candidate
            .dfg
            .block_params(candidate.layout.entry_block().unwrap())[0];
        assert!(candidate
            .layout
            .block_insts(charge)
            .all(|i| !matches!(classify(&candidate, i, ctx), Some(Action::Read(_)))));
        let writes = |func: &Function| {
            func.layout
                .blocks()
                .flat_map(|block| func.layout.block_insts(block))
                .filter(|&inst| matches!(classify(func, inst, ctx), Some(Action::Write(_))))
                .count()
        };
        assert_eq!(writes(&candidate), writes(&original));
        // Cranelift's later phi cleanup mutates jump tables in place and cannot
        // compile a table shared by instructions. Compare the promoted shared
        // table against the equivalent ordinary branches; the promotion must
        // give every edge its own arguments before handing it to Cranelift.
        let control = if call_shape >= 3 {
            fixture(&mut module, call_shape % 3).0
        } else {
            original
        };
        let mut entries = Vec::new();
        for (name, func) in [("control", control), ("candidate", candidate)] {
            let id = module
                .declare_function(name, Linkage::Local, &func.signature)
                .unwrap();
            let mut context = module.make_context();
            context.func = func;
            module.define_function(id, &mut context).unwrap();
            entries.push(id);
        }
        module.finalize_definitions().unwrap();
        type Entry = extern "C" fn(*mut u8, u32, *mut u32) -> u32;
        let entries: Vec<Entry> = entries
            .into_iter()
            .map(|id| unsafe { std::mem::transmute(module.get_finalized_function(id)) })
            .collect();
        for initial in [0, 1, 3, 7, 21, 500] {
            for rounds in [1, 4, 17, 100] {
                for behavior in 0..3 {
                    let results: Vec<_> = entries
                        .iter()
                        .map(|entry| {
                            let mut buffer = vec![0_u64; (OFFSET as usize + 8) / 8];
                            let ctx = buffer.as_mut_ptr().cast::<u8>();
                            let mut output = [behavior, 0, 0];
                            unsafe {
                                *ctx.add(OFFSET as usize).cast::<u32>() = initial;
                            }
                            OBSERVED_CONTEXT.with(|slot| slot.set(ctx));
                            let iterations = entry(ctx, rounds, output.as_mut_ptr());
                            OBSERVED_CONTEXT.with(|slot| slot.set(std::ptr::null_mut()));
                            let remaining = unsafe { *ctx.add(OFFSET as usize).cast::<u32>() };
                            (iterations, remaining, output)
                        })
                        .collect();
                    assert_eq!(
                        results[0], results[1],
                        "{initial}/{rounds}/{behavior}/{call_shape}"
                    );
                }
            }
        }
        unsafe {
            module.free_memory();
        }
    }
}

#[test]
fn uncertain_context_access_and_resource_limits_preserve_original_ir() {
    for shape in 0..6 {
        let mut module = module();
        let (mut func, charge) = fixture(&mut module, 0);
        let entry = func.layout.entry_block().unwrap();
        let ctx = func.dfg.block_params(entry)[0];
        let first = func.layout.first_inst(entry).unwrap();
        let mut cursor = FuncCursor::new(&mut func).at_inst(first);
        match shape {
            0 => {
                cursor
                    .ins()
                    .load(types::I64, MemFlagsData::trusted(), ctx, OFFSET);
            }
            1 => {
                cursor
                    .ins()
                    .load(types::I32, MemFlagsData::trusted(), ctx, OFFSET - 1);
            }
            2 => {
                cursor.ins().iadd_imm_s(ctx, i64::from(OFFSET));
            }
            3 => {
                let last = func.layout.last_inst(charge).unwrap();
                let dfg = &mut func.dfg;
                let target = dfg.insts[last]
                    .branch_destination(&dfg.jump_tables, &dfg.exception_tables)[1]
                    .block(&dfg.value_lists);
                dfg.append_block_param(target, types::I64);
                dfg.insts[last]
                    .branch_destination_mut(&mut dfg.jump_tables, &mut dfg.exception_tables)[1]
                    .append_argument(ctx, &mut dfg.value_lists);
            }
            4 => {
                for _ in func.dfg.num_blocks()..=MAX_BLOCKS {
                    func.dfg.make_block();
                }
            }
            _ => {
                let block = func.dfg.make_block();
                func.layout.append_block(block);
                for _ in 0..u16::MAX {
                    func.dfg.append_block_param(block, types::I32);
                }
                let mut cursor = FuncCursor::new(&mut func).at_first_insertion_point(block);
                let zero = cursor.ins().iconst(types::I32, 0);
                cursor.ins().return_(&[zero]);
            }
        }
        let before = func.display().to_string();
        assert!(!forward_execution_budget(&mut func), "shape {shape}");
        assert_eq!(before, func.display().to_string());
        unsafe {
            module.free_memory();
        }
    }
}

#[test]
fn acyclic_and_no_budget_artifacts_are_unchanged() {
    let mut module = module();
    for no_budget in [false, true] {
        let (mut func, _) = fixture(&mut module, 0);
        let blocks: Vec<_> = func.layout.blocks().collect();
        if no_budget {
            let stores: Vec<_> = func
                .layout
                .blocks()
                .flat_map(|b| func.layout.block_insts(b))
                .filter(|&i| func.dfg.insts[i].opcode() == Opcode::Store)
                .collect();
            for inst in stores {
                func.layout.remove_inst(inst);
            }
        } else {
            let back = blocks[4];
            let next = func.dfg.block_params(back)[0];
            let last = func.layout.last_inst(back).unwrap();
            func.replace(last).jump(blocks[5], &[next.into()]);
        }
        let before = func.display().to_string();
        assert!(!forward_execution_budget(&mut func));
        assert_eq!(before, func.display().to_string());
    }
    unsafe {
        module.free_memory();
    }
}
