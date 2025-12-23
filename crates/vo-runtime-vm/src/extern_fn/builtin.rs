//! Built-in functions (println, etc.)

use vo_vm::{ExternRegistry, ExternCtx, ExternResult};

pub fn register(registry: &mut ExternRegistry) {
    registry.register("println", extern_println);
}

fn extern_println(ctx: &mut ExternCtx) -> ExternResult {
    let output = ctx.format_all();
    println!("{}", output);
    ctx.ret_i64(0, (output.len() + 1) as i64);
    ExternResult::Ok(1)
}
