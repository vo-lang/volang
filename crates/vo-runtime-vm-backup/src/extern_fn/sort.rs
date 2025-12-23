//! sort package extern functions.

use vo_vm::{ExternRegistry, ExternCtx, ExternResult};

pub fn register(registry: &mut ExternRegistry) {
    registry.register("sort.Ints", extern_sort_ints);
    registry.register("sort.Float64s", extern_sort_floats);
    registry.register("sort.Strings", extern_sort_strings);
}

fn extern_sort_ints(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    if slice_ref.is_null() {
        return ExternResult::Ok(0);
    }
    
    let len = ctx.slice_len(slice_ref);
    if len <= 1 {
        return ExternResult::Ok(0);
    }
    
    // Simple insertion sort (good enough for small slices)
    for i in 1..len {
        let key = ctx.slice_get_i64(slice_ref, i);
        let mut j = i;
        while j > 0 && ctx.slice_get_i64(slice_ref, j - 1) > key {
            let prev = ctx.slice_get_i64(slice_ref, j - 1);
            ctx.slice_set_i64(slice_ref, j, prev);
            j -= 1;
        }
        ctx.slice_set_i64(slice_ref, j, key);
    }
    
    ExternResult::Ok(0)
}

fn extern_sort_floats(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    if slice_ref.is_null() {
        return ExternResult::Ok(0);
    }
    
    let len = ctx.slice_len(slice_ref);
    if len <= 1 {
        return ExternResult::Ok(0);
    }
    
    for i in 1..len {
        let key = ctx.slice_get_f64(slice_ref, i);
        let mut j = i;
        while j > 0 && ctx.slice_get_f64(slice_ref, j - 1) > key {
            let prev = ctx.slice_get_f64(slice_ref, j - 1);
            ctx.slice_set_f64(slice_ref, j, prev);
            j -= 1;
        }
        ctx.slice_set_f64(slice_ref, j, key);
    }
    
    ExternResult::Ok(0)
}

fn extern_sort_strings(ctx: &mut ExternCtx) -> ExternResult {
    // TODO: String sorting requires string comparison
    let _ = ctx;
    ExternResult::Ok(0)
}
