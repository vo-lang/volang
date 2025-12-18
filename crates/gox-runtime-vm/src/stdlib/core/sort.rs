//! Native implementations for the sort package.

use gox_vm::gc::GcRef;
use gox_vm::extern_fn::{ExternCtx, ExternResult, ExternRegistry};
use gox_vm::objects::{slice, string};

pub fn register(registry: &mut ExternRegistry) {
    // Sort functions
    registry.register("sort.Ints", native_sort_ints);
    registry.register("sort.Float64s", native_sort_float64s);
    registry.register("sort.Strings", native_sort_strings);
    
    // IsSorted functions
    registry.register("sort.IntsAreSorted", native_ints_are_sorted);
    registry.register("sort.Float64sAreSorted", native_float64s_are_sorted);
    registry.register("sort.StringsAreSorted", native_strings_are_sorted);
    
    // Search functions
    registry.register("sort.SearchInts", native_search_ints);
    registry.register("sort.SearchFloat64s", native_search_float64s);
    registry.register("sort.SearchStrings", native_search_strings);
    
    // Reverse functions
    registry.register("sort.Reverse", native_reverse_ints);
    registry.register("sort.ReverseFloat64s", native_reverse_float64s);
    registry.register("sort.ReverseStrings", native_reverse_strings);
}

// ============ Sort functions ============

fn native_sort_ints(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    if slice_ref.is_null() {
        return ExternResult::Ok(0);
    }
    
    let len = slice::len(slice_ref);
    if len <= 1 {
        return ExternResult::Ok(0);
    }
    
    // Read all values
    let mut values: Vec<i64> = (0..len)
        .map(|i| slice::get(slice_ref, i) as i64)
        .collect();
    
    // Sort
    values.sort();
    
    // Write back
    for (i, &v) in values.iter().enumerate() {
        slice::set(slice_ref, i, v as u64);
    }
    
    ExternResult::Ok(0)
}

fn native_sort_float64s(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    if slice_ref.is_null() {
        return ExternResult::Ok(0);
    }
    
    let len = slice::len(slice_ref);
    if len <= 1 {
        return ExternResult::Ok(0);
    }
    
    // Read all values
    let mut values: Vec<f64> = (0..len)
        .map(|i| f64::from_bits(slice::get(slice_ref, i)))
        .collect();
    
    // Sort (NaN-safe)
    values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    
    // Write back
    for (i, &v) in values.iter().enumerate() {
        slice::set(slice_ref, i, v.to_bits());
    }
    
    ExternResult::Ok(0)
}

fn native_sort_strings(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    if slice_ref.is_null() {
        return ExternResult::Ok(0);
    }
    
    let len = slice::len(slice_ref);
    if len <= 1 {
        return ExternResult::Ok(0);
    }
    
    // Read all string refs and their values
    let mut str_refs: Vec<GcRef> = (0..len)
        .map(|i| slice::get(slice_ref, i) as GcRef)
        .collect();
    
    // Sort by string content
    str_refs.sort_by(|&a, &b| {
        let sa = if a.is_null() { "" } else { string::as_str(a) };
        let sb = if b.is_null() { "" } else { string::as_str(b) };
        sa.cmp(sb)
    });
    
    // Write back
    for (i, &ref_) in str_refs.iter().enumerate() {
        slice::set(slice_ref, i, ref_ as u64);
    }
    
    ExternResult::Ok(0)
}

// ============ IsSorted functions ============

fn native_ints_are_sorted(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    if slice_ref.is_null() {
        ctx.ret_bool(0, true);
        return ExternResult::Ok(1);
    }
    
    let len = slice::len(slice_ref);
    if len <= 1 {
        ctx.ret_bool(0, true);
        return ExternResult::Ok(1);
    }
    
    let mut prev = slice::get(slice_ref, 0) as i64;
    for i in 1..len {
        let curr = slice::get(slice_ref, i) as i64;
        if curr < prev {
            ctx.ret_bool(0, false);
            return ExternResult::Ok(1);
        }
        prev = curr;
    }
    
    ctx.ret_bool(0, true);
    ExternResult::Ok(1)
}

fn native_float64s_are_sorted(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    if slice_ref.is_null() {
        ctx.ret_bool(0, true);
        return ExternResult::Ok(1);
    }
    
    let len = slice::len(slice_ref);
    if len <= 1 {
        ctx.ret_bool(0, true);
        return ExternResult::Ok(1);
    }
    
    let mut prev = f64::from_bits(slice::get(slice_ref, 0));
    for i in 1..len {
        let curr = f64::from_bits(slice::get(slice_ref, i));
        if curr < prev {
            ctx.ret_bool(0, false);
            return ExternResult::Ok(1);
        }
        prev = curr;
    }
    
    ctx.ret_bool(0, true);
    ExternResult::Ok(1)
}

fn native_strings_are_sorted(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    if slice_ref.is_null() {
        ctx.ret_bool(0, true);
        return ExternResult::Ok(1);
    }
    
    let len = slice::len(slice_ref);
    if len <= 1 {
        ctx.ret_bool(0, true);
        return ExternResult::Ok(1);
    }
    
    let mut prev_ref = slice::get(slice_ref, 0) as GcRef;
    for i in 1..len {
        let curr_ref = slice::get(slice_ref, i) as GcRef;
        let prev_str = if prev_ref.is_null() { "" } else { string::as_str(prev_ref) };
        let curr_str = if curr_ref.is_null() { "" } else { string::as_str(curr_ref) };
        if curr_str < prev_str {
            ctx.ret_bool(0, false);
            return ExternResult::Ok(1);
        }
        prev_ref = curr_ref;
    }
    
    ctx.ret_bool(0, true);
    ExternResult::Ok(1)
}

// ============ Search functions ============

fn native_search_ints(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    let x = ctx.arg_i64(1);
    
    if slice_ref.is_null() {
        ctx.ret_i64(0, 0);
        return ExternResult::Ok(1);
    }
    
    let len = slice::len(slice_ref);
    if len == 0 {
        ctx.ret_i64(0, 0);
        return ExternResult::Ok(1);
    }
    
    // Binary search
    let mut lo = 0i64;
    let mut hi = len as i64;
    while lo < hi {
        let mid = (lo + hi) / 2;
        let v = slice::get(slice_ref, mid as usize) as i64;
        if v < x {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    
    ctx.ret_i64(0, lo);
    ExternResult::Ok(1)
}

fn native_search_float64s(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    let x = ctx.arg_f64(1);
    
    if slice_ref.is_null() {
        ctx.ret_i64(0, 0);
        return ExternResult::Ok(1);
    }
    
    let len = slice::len(slice_ref);
    if len == 0 {
        ctx.ret_i64(0, 0);
        return ExternResult::Ok(1);
    }
    
    // Binary search
    let mut lo = 0i64;
    let mut hi = len as i64;
    while lo < hi {
        let mid = (lo + hi) / 2;
        let v = f64::from_bits(slice::get(slice_ref, mid as usize));
        if v < x {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    
    ctx.ret_i64(0, lo);
    ExternResult::Ok(1)
}

fn native_search_strings(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    let x = ctx.arg_str(1);
    
    if slice_ref.is_null() {
        ctx.ret_i64(0, 0);
        return ExternResult::Ok(1);
    }
    
    let len = slice::len(slice_ref);
    if len == 0 {
        ctx.ret_i64(0, 0);
        return ExternResult::Ok(1);
    }
    
    // Binary search
    let mut lo = 0i64;
    let mut hi = len as i64;
    while lo < hi {
        let mid = (lo + hi) / 2;
        let v_ref = slice::get(slice_ref, mid as usize) as GcRef;
        let v = if v_ref.is_null() { "" } else { string::as_str(v_ref) };
        if v < x {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    
    ctx.ret_i64(0, lo);
    ExternResult::Ok(1)
}

// ============ Reverse functions ============

fn native_reverse_ints(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    if slice_ref.is_null() {
        return ExternResult::Ok(0);
    }
    
    let len = slice::len(slice_ref);
    if len <= 1 {
        return ExternResult::Ok(0);
    }
    
    let mut i = 0;
    let mut j = len - 1;
    while i < j {
        let a = slice::get(slice_ref, i);
        let b = slice::get(slice_ref, j);
        slice::set(slice_ref, i, b);
        slice::set(slice_ref, j, a);
        i += 1;
        j -= 1;
    }
    
    ExternResult::Ok(0)
}

fn native_reverse_float64s(ctx: &mut ExternCtx) -> ExternResult {
    native_reverse_ints(ctx) // Same logic, just different type interpretation
}

fn native_reverse_strings(ctx: &mut ExternCtx) -> ExternResult {
    native_reverse_ints(ctx) // Same logic, refs are just u64
}

