//! External function definitions for the VM.
//!
//! Registers stdlib extern functions by name.

use vo_vm::exec::{ExternRegistry, ExternCallResult, ExternFn};
use vo_vm::bytecode::Module;

/// Standard library mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum StdMode {
    /// Core mode: no OS dependencies (for WASM, embedded)
    Core,
    /// Full mode: complete standard library
    #[default]
    Full,
}

/// Register stdlib extern functions based on module's extern definitions.
pub fn register_stdlib(registry: &mut ExternRegistry, module: &Module) {
    for (id, def) in module.externs.iter().enumerate() {
        if let Some(func) = get_extern_fn(&def.name) {
            registry.register(id as u32, func);
        }
    }
}

/// Get extern function by name.
fn get_extern_fn(name: &str) -> Option<ExternFn> {
    match name {
        "vo_print" => Some(vo_print),
        "vo_println" => Some(vo_println),
        "vo_copy" => Some(vo_copy),
        // fmt package functions (variadic)
        "fmt_Print" => Some(vo_print),
        "fmt_Println" => Some(vo_println),
        _ => None,
    }
}

/// Print without newline.
/// Args are passed as (value, value_kind) pairs.
fn vo_print(ret: &mut [u64], args: &[u64]) -> ExternCallResult {
    use vo_runtime_core::builtins::format_value;
    use vo_common_core::types::ValueKind;
    
    let mut total_len = 0usize;
    // args are (value, kind) pairs
    let mut i = 0;
    while i + 1 < args.len() {
        let val = args[i];
        let kind = ValueKind::from_u8(args[i + 1] as u8);
        let s = format_value(val, kind);
        if i > 0 {
            print!(" ");
            total_len += 1;
        }
        print!("{}", s);
        total_len += s.len();
        i += 2;
    }
    
    if !ret.is_empty() {
        ret[0] = total_len as u64;
    }
    ExternCallResult::Ok
}

/// Print with newline.
/// Args are passed as (value, value_kind) pairs.
fn vo_println(ret: &mut [u64], args: &[u64]) -> ExternCallResult {
    use vo_runtime_core::builtins::format_value;
    use vo_common_core::types::ValueKind;
    
    let mut total_len = 0usize;
    // args are (value, kind) pairs
    let mut i = 0;
    while i + 1 < args.len() {
        let val = args[i];
        let kind = ValueKind::from_u8(args[i + 1] as u8);
        let s = format_value(val, kind);
        if i > 0 {
            print!(" ");
            total_len += 1;
        }
        print!("{}", s);
        total_len += s.len();
        i += 2;
    }
    println!();
    total_len += 1;
    
    if !ret.is_empty() {
        ret[0] = total_len as u64;
    }
    ExternCallResult::Ok
}

/// Copy slice.
fn vo_copy(ret: &mut [u64], _args: &[u64]) -> ExternCallResult {
    // TODO: Implement slice copy
    if !ret.is_empty() {
        ret[0] = 0;
    }
    ExternCallResult::Ok
}
