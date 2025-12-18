//! Extern function dispatch for JIT/AOT.
//!
//! Provides a uniform C ABI interface for calling extern (Rust-implemented) functions
//! from JIT-compiled code. Instead of requiring individual C ABI wrappers for each
//! function, this module provides a single dispatch function.

use std::collections::HashMap;
use std::sync::OnceLock;

/// Extern function type for JIT dispatch.
/// Takes args array and writes to rets array.
pub type ExternDispatchFn = fn(args: &[u64], rets: &mut [u64]) -> Result<(), String>;

/// Registry of extern functions for JIT dispatch.
struct ExternDispatchRegistry {
    funcs: HashMap<String, ExternDispatchFn>,
}

impl ExternDispatchRegistry {
    fn new() -> Self {
        Self {
            funcs: HashMap::new(),
        }
    }
    
    fn register(&mut self, name: &str, func: ExternDispatchFn) {
        self.funcs.insert(name.to_string(), func);
    }
    
    fn get(&self, name: &str) -> Option<ExternDispatchFn> {
        self.funcs.get(name).copied()
    }
}

static REGISTRY: OnceLock<ExternDispatchRegistry> = OnceLock::new();

fn get_registry() -> &'static ExternDispatchRegistry {
    REGISTRY.get_or_init(|| {
        let mut registry = ExternDispatchRegistry::new();
        crate::extern_fns::register_all(&mut |name, func| {
            registry.register(name, func);
        });
        registry
    })
}

/// Initialize extern function registry.
/// Verifies that all required functions are registered.
pub fn init_extern_fns(names: &[String]) {
    let registry = get_registry();
    for name in names {
        if registry.get(name).is_none() {
            eprintln!("WARNING: extern function not registered for JIT: {}", name);
        }
    }
}

/// Dispatch an extern function call.
/// 
/// # Safety
/// Caller must ensure args and rets point to valid memory with correct sizes.
#[no_mangle]
pub unsafe extern "C" fn gox_extern_call(
    native_name_ptr: *const u8,
    native_name_len: usize,
    args: *const u64,
    arg_count: usize,
    rets: *mut u64,
    ret_count: usize,
) -> i32 {
    // Convert name from C string
    let name_slice = std::slice::from_raw_parts(native_name_ptr, native_name_len);
    let name = match std::str::from_utf8(name_slice) {
        Ok(s) => s,
        Err(_) => return -1,
    };
    
    let registry = get_registry();
    let func = match registry.get(name) {
        Some(f) => f,
        None => {
            eprintln!("Extern function not found: {}", name);
            return -2;
        }
    };
    
    let args_slice = std::slice::from_raw_parts(args, arg_count);
    let rets_slice = std::slice::from_raw_parts_mut(rets, ret_count);
    
    match func(args_slice, rets_slice) {
        Ok(()) => 0,
        Err(msg) => {
            eprintln!("Extern function error: {}", msg);
            -3
        }
    }
}
