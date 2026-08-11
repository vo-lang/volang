mod branches;
mod driver;
mod facts;
mod poll;
mod slots;
mod state;

use cranelift_codegen::ir::Block;
use cranelift_frontend::FunctionBuilder;

pub(crate) use branches::*;
pub(crate) use driver::*;
pub(crate) use facts::*;
pub(crate) use poll::*;
pub(crate) use slots::*;
pub(crate) use state::*;

pub(crate) fn cold_block(builder: &mut FunctionBuilder<'_>) -> Block {
    let block = builder.create_block();
    builder.set_cold_block(block);
    block
}
