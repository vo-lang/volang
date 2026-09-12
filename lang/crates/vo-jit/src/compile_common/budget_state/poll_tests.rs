use super::*;
use crate::compile_common::{
    branch_on_execution_budget, continue_after_execution_budget_poll, refill_execution_budget,
};
use cranelift_codegen::ir::{condcodes::IntCC, AbiParam};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, Linkage, Module};

const OFFSET: usize = JitContextField::ExecutionBudget.offset() as usize;

extern "C" fn grant_budget(context: *mut u8, required: u32) -> u32 {
    unsafe {
        let words = context.add(OFFSET).cast::<u32>();
        *words.add(2) += 1;
        match *words.add(1) {
            0 => 0,
            1 => required - 1,
            2 => required,
            3 => required + 17,
            _ => {
                // A callback may publish state even when its grant is denied.
                *words = 23;
                required - 1
            }
        }
    }
}

#[test]
fn budget_polls_preserve_zero_partial_exact_and_larger_refill_grants() {
    let mut builder = JITBuilder::new(default_libcall_names()).unwrap();
    builder.symbol("grant_budget", grant_budget as *const u8);
    let mut module = JITModule::new(builder);
    let mut signature = module.make_signature();
    signature.params = vec![AbiParam::new(types::I64), AbiParam::new(types::I32)];
    signature.returns.push(AbiParam::new(types::I32));
    let callback = module
        .declare_function("grant_budget", Linkage::Import, &signature)
        .unwrap();
    let mut func = module.make_context().func;
    func.signature = signature;
    let callback = module.declare_func_in_func(callback, &mut func);
    let mut frontend = FunctionBuilderContext::new();
    let mut b = FunctionBuilder::new(&mut func, &mut frontend);
    let entry = b.create_block();
    let head = b.create_block();
    let yielded = b.create_block();
    let done = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.append_block_param(head, types::I32);
    b.switch_to_block(entry);
    let context = b.block_params(entry)[0];
    let rounds = b.block_params(entry)[1];
    let zero = b.ins().iconst(types::I32, 0);
    b.ins().jump(head, &[zero.into()]);
    b.switch_to_block(head);
    let count = b.block_params(head)[0];
    let poll = branch_on_execution_budget(&mut b, context, 3);
    b.switch_to_block(poll.exhausted);
    b.seal_block(poll.exhausted);
    refill_execution_budget(&mut b, context, callback, &poll, yielded);
    continue_after_execution_budget_poll(&mut b, context, &poll);
    let next = b.ins().iadd_imm_s(count, 1);
    let complete = b
        .ins()
        .icmp(IntCC::UnsignedGreaterThanOrEqual, next, rounds);
    b.ins().brif(complete, done, &[], head, &[next.into()]);
    b.switch_to_block(done);
    b.ins().return_(&[next]);
    b.switch_to_block(yielded);
    b.ins().return_(&[count]);
    b.seal_all_blocks();
    b.finalize(module.target_config());
    let mut forwarded = func.clone();
    assert!(forward_execution_budget(&mut forwarded));
    let mut entries = Vec::new();
    for (name, func) in [("poll_control", func), ("poll_forwarded", forwarded)] {
        let id = module
            .declare_function(name, Linkage::Local, &func.signature)
            .unwrap();
        let mut context = module.make_context();
        context.func = func;
        module.define_function(id, &mut context).unwrap();
        entries.push(id);
    }
    module.finalize_definitions().unwrap();
    type Entry = extern "C" fn(*mut u8, u32) -> u32;
    let entries: Vec<Entry> = entries
        .into_iter()
        .map(|id| unsafe { std::mem::transmute(module.get_finalized_function(id)) })
        .collect();
    for initial in [0, 1, 2, 3, 8, 19, u32::MAX] {
        for rounds in [1, 5] {
            for mode in 0..5 {
                let (mut remaining, mut count, mut callbacks) = (initial, 0, 0);
                while count < rounds {
                    if remaining < 3 {
                        callbacks += 1;
                        let grant = match mode {
                            0 => 0,
                            1 => 2,
                            2 => 3,
                            3 => 20,
                            _ => {
                                remaining = 23;
                                2
                            }
                        };
                        if grant < 3 {
                            break;
                        }
                        remaining = grant;
                    }
                    remaining -= 3;
                    count += 1;
                }
                for entry in &entries {
                    let mut buffer = vec![0_u64; (OFFSET + 24) / 8];
                    let context = buffer.as_mut_ptr().cast::<u8>();
                    let words = unsafe { context.add(OFFSET).cast::<u32>() };
                    unsafe {
                        *words = initial;
                        *words.add(1) = mode;
                    }
                    assert_eq!(entry(context, rounds), count);
                    assert_eq!(unsafe { *words }, remaining, "{initial}/{rounds}/{mode}");
                    assert_eq!(unsafe { *words.add(2) }, callbacks);
                }
            }
        }
    }
    unsafe {
        module.free_memory();
    }
}
