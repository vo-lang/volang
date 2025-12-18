//! sort package extern functions.

use gox_vm::{ExternRegistry, ExternCtx, ExternResult};

pub fn register(registry: &mut ExternRegistry) {
    registry.register("sort.Ints", extern_sort_ints);
    registry.register("sort.Float64s", extern_sort_floats);
    registry.register("sort.Strings", extern_sort_strings);
}

fn extern_sort_ints(ctx: &mut ExternCtx) -> ExternResult {
    // TODO: Need mutable slice access
    let _ = ctx;
    ExternResult::Ok(0)
}

fn extern_sort_floats(ctx: &mut ExternCtx) -> ExternResult {
    let _ = ctx;
    ExternResult::Ok(0)
}

fn extern_sort_strings(ctx: &mut ExternCtx) -> ExternResult {
    let _ = ctx;
    ExternResult::Ok(0)
}
