//! Math intrinsics — emit Cranelift instructions directly instead of FFI calls.
//!
//! Only covers functions that have exact Cranelift instruction equivalents.
//! Note: math.Round uses "round half away from zero" (Go semantics), which differs
//! from Cranelift's `nearest` ("round half to even"), so it is NOT intrinsified.
//! Note: math.Abs, Min, Max, Copysign are implemented in Vo (not externs),
//! so they are handled by normal Call, not CallExtern.

use cranelift_codegen::ir::{types, InstBuilder, MemFlags, Value};

use vo_runtime::instruction::Instruction;

use crate::translator::ScalarEmitter;

/// Emit an intrinsic selected by the load-time resolved extern route.
pub fn emit_resolved_intrinsic<'a>(
    e: &mut impl ScalarEmitter<'a>,
    inst: &Instruction,
    name: &str,
) -> Result<(), crate::JitError> {
    match name {
        "math_Sqrt" => emit_unary(e, inst, |b, v| b.ins().sqrt(v)),
        "math_Floor" => emit_unary(e, inst, |b, v| b.ins().floor(v)),
        "math_Ceil" => emit_unary(e, inst, |b, v| b.ins().ceil(v)),
        "math_Trunc" => emit_unary(e, inst, |b, v| b.ins().trunc(v)),
        "math_FMA" => emit_fma(e, inst),
        _ => {
            return Err(crate::JitError::Internal(format!(
                "resolved intrinsic route for unsupported extern '{name}'"
            )))
        }
    }
    Ok(())
}

/// Emit a unary f64 → f64 intrinsic.
fn emit_unary<'a>(
    e: &mut impl ScalarEmitter<'a>,
    inst: &Instruction,
    op: impl FnOnce(&mut cranelift_frontend::FunctionBuilder, Value) -> Value,
) {
    let v = read_f64_arg(e, inst.c);
    let r = op(e.builder(), v);
    e.write_var(inst.a, r);
}

/// Emit fused multiply-add: FMA(x, y, z) = x*y + z
fn emit_fma<'a>(e: &mut impl ScalarEmitter<'a>, inst: &Instruction) {
    let arg_start = inst.c;
    let a = read_f64_arg(e, arg_start);
    let b = read_f64_arg(e, arg_start + 1);
    let c = read_f64_arg(e, arg_start + 2);
    let r = e.builder().ins().fma(a, b, c);
    e.write_var(inst.a, r);
}

/// Read an argument slot as F64. If the SSA variable is I64 (non-Float slot),
/// bitcast to F64.
#[inline]
fn read_f64_arg<'a>(e: &mut impl ScalarEmitter<'a>, slot: u16) -> Value {
    let val = e.read_var(slot);
    let ty = e.builder().func.dfg.value_type(val);
    if ty == types::F64 {
        val
    } else {
        e.builder().ins().bitcast(types::F64, MemFlags::new(), val)
    }
}

#[cfg(test)]
mod tests {
    const SUPPORTED_EXTERN_NAMES: &[&str] = &[
        "math_Sqrt",
        "math_Floor",
        "math_Ceil",
        "math_Trunc",
        "math_FMA",
    ];

    #[test]
    fn runtime_intrinsic_route_list_matches_jit_support() {
        let mut runtime = vo_runtime::ffi::jit_intrinsic_extern_names().to_vec();
        let mut jit = SUPPORTED_EXTERN_NAMES.to_vec();
        runtime.sort_unstable();
        jit.sort_unstable();
        assert_eq!(
            runtime, jit,
            "runtime ExternJitRoute::Intrinsic list must match JIT intrinsic support"
        );
    }
}
