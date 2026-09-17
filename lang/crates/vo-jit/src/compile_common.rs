mod branches;
mod budget_state;
mod cold_paths;
mod driver;
mod poll;
mod slots;
mod state;

use cranelift_codegen::ir::Block;
use cranelift_frontend::FunctionBuilder;

pub(crate) use branches::*;
pub(crate) use budget_state::forward_execution_budget;
pub(crate) use cold_paths::propagate_cold_paths;
pub(crate) use driver::*;
pub(crate) use poll::*;
pub(crate) use slots::*;
pub(crate) use state::*;

pub(crate) fn cold_block(builder: &mut FunctionBuilder<'_>) -> Block {
    let block = builder.create_block();
    builder.set_cold_block(block);
    block
}
