//! Native implementations for the os package.

use gox_vm::gc::GcRef;
use gox_vm::native::{NativeCtx, NativeResult, NativeRegistry};
use gox_vm::objects::{array, slice, string};
use gox_vm::types::builtin;

pub fn register(registry: &mut NativeRegistry) {
    // Environment
    registry.register("os.Getenv", native_getenv);
    registry.register("os.Setenv", native_setenv);
    registry.register("os.Unsetenv", native_unsetenv);
    registry.register("os.Environ", native_environ);
    registry.register("os.LookupEnv", native_lookup_env);
    
    // Process
    registry.register("os.Exit", native_exit);
    registry.register("os.Getpid", native_getpid);
    registry.register("os.Getuid", native_getuid);
    registry.register("os.Getgid", native_getgid);
    
    // Working directory
    registry.register("os.Getwd", native_getwd);
    registry.register("os.Chdir", native_chdir);
    
    // Hostname
    registry.register("os.Hostname", native_hostname);
}

fn native_getenv(ctx: &mut NativeCtx) -> NativeResult {
    let key = ctx.arg_str(0).to_string();
    let value = std::env::var(&key).unwrap_or_default();
    ctx.ret_string(0, &value);
    NativeResult::Ok(1)
}

fn native_setenv(ctx: &mut NativeCtx) -> NativeResult {
    let key = ctx.arg_str(0).to_string();
    let value = ctx.arg_str(1).to_string();
    match std::env::set_var(&key, &value) {
        () => {
            ctx.ret_nil(0); // nil error
            NativeResult::Ok(1)
        }
    }
}

fn native_unsetenv(ctx: &mut NativeCtx) -> NativeResult {
    let key = ctx.arg_str(0).to_string();
    std::env::remove_var(&key);
    ctx.ret_nil(0); // nil error
    NativeResult::Ok(1)
}

fn native_environ(ctx: &mut NativeCtx) -> NativeResult {
    let env_vars: Vec<String> = std::env::vars()
        .map(|(k, v)| format!("{}={}", k, v))
        .collect();
    
    let gc = ctx.gc();
    let arr = array::create(gc, builtin::ARRAY, builtin::STRING, 1, env_vars.len());
    for (i, s) in env_vars.iter().enumerate() {
        let str_ref = string::from_rust_str(gc, builtin::STRING, s);
        array::set(arr, i, str_ref as u64);
    }
    let result = slice::from_array(gc, builtin::SLICE, arr);
    ctx.ret_ref(0, result);
    NativeResult::Ok(1)
}

fn native_lookup_env(ctx: &mut NativeCtx) -> NativeResult {
    let key = ctx.arg_str(0).to_string();
    match std::env::var(&key) {
        Ok(value) => {
            ctx.ret_string(0, &value);
            ctx.ret_bool(1, true);
        }
        Err(_) => {
            ctx.ret_string(0, "");
            ctx.ret_bool(1, false);
        }
    }
    NativeResult::Ok(2)
}

fn native_exit(ctx: &mut NativeCtx) -> NativeResult {
    let code = ctx.arg_i64(0) as i32;
    std::process::exit(code);
}

fn native_getpid(ctx: &mut NativeCtx) -> NativeResult {
    ctx.ret_i64(0, std::process::id() as i64);
    NativeResult::Ok(1)
}

fn native_getuid(ctx: &mut NativeCtx) -> NativeResult {
    // Return 0 on non-Unix platforms
    // On Unix, we'd need libc but we keep it simple for now
    ctx.ret_i64(0, 0);
    NativeResult::Ok(1)
}

fn native_getgid(ctx: &mut NativeCtx) -> NativeResult {
    // Return 0 on non-Unix platforms
    ctx.ret_i64(0, 0);
    NativeResult::Ok(1)
}

fn native_getwd(ctx: &mut NativeCtx) -> NativeResult {
    match std::env::current_dir() {
        Ok(path) => {
            let path_str = path.to_string_lossy().to_string();
            ctx.ret_string(0, &path_str);
            ctx.ret_nil(1); // nil error
        }
        Err(e) => {
            ctx.ret_string(0, "");
            ctx.ret_string(1, &e.to_string());
        }
    }
    NativeResult::Ok(2)
}

fn native_chdir(ctx: &mut NativeCtx) -> NativeResult {
    let dir = ctx.arg_str(0).to_string();
    match std::env::set_current_dir(&dir) {
        Ok(()) => {
            ctx.ret_nil(0); // nil error
        }
        Err(e) => {
            ctx.ret_string(0, &e.to_string());
        }
    }
    NativeResult::Ok(1)
}

fn native_hostname(ctx: &mut NativeCtx) -> NativeResult {
    // Simple implementation using environment or default
    let hostname = std::env::var("HOSTNAME")
        .or_else(|_| std::env::var("COMPUTERNAME"))
        .unwrap_or_else(|_| "localhost".to_string());
    ctx.ret_string(0, &hostname);
    ctx.ret_nil(1); // nil error
    NativeResult::Ok(2)
}

