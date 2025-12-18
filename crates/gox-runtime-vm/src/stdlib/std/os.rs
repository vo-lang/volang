//! Native implementations for the os package.

use gox_vm::extern_fn::{ExternCtx, ExternRegistry, ExternResult};
use gox_vm::objects::{array, slice, string};
use gox_vm::types::builtin;

pub fn register(registry: &mut ExternRegistry) {
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
    
    // File operations
    registry.register("os.ReadFile", native_read_file);
    registry.register("os.WriteFile", native_write_file);
    registry.register("os.Remove", native_remove);
    registry.register("os.RemoveAll", native_remove_all);
    registry.register("os.Mkdir", native_mkdir);
    registry.register("os.MkdirAll", native_mkdir_all);
    registry.register("os.Rename", native_rename);
}

fn native_getenv(ctx: &mut ExternCtx) -> ExternResult {
    let key = ctx.arg_str(0).to_string();
    let value = std::env::var(&key).unwrap_or_default();
    ctx.ret_string(0, &value);
    ExternResult::Ok(1)
}

fn native_setenv(ctx: &mut ExternCtx) -> ExternResult {
    let key = ctx.arg_str(0).to_string();
    let value = ctx.arg_str(1).to_string();
    match std::env::set_var(&key, &value) {
        () => {
            ctx.ret_nil(0); // nil error
            ExternResult::Ok(1)
        }
    }
}

fn native_unsetenv(ctx: &mut ExternCtx) -> ExternResult {
    let key = ctx.arg_str(0).to_string();
    std::env::remove_var(&key);
    ctx.ret_nil(0); // nil error
    ExternResult::Ok(1)
}

fn native_environ(ctx: &mut ExternCtx) -> ExternResult {
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
    ExternResult::Ok(1)
}

fn native_lookup_env(ctx: &mut ExternCtx) -> ExternResult {
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
    ExternResult::Ok(2)
}

fn native_exit(ctx: &mut ExternCtx) -> ExternResult {
    let code = ctx.arg_i64(0) as i32;
    std::process::exit(code);
}

fn native_getpid(ctx: &mut ExternCtx) -> ExternResult {
    ctx.ret_i64(0, std::process::id() as i64);
    ExternResult::Ok(1)
}

fn native_getuid(ctx: &mut ExternCtx) -> ExternResult {
    // Return 0 on non-Unix platforms
    // On Unix, we'd need libc but we keep it simple for now
    ctx.ret_i64(0, 0);
    ExternResult::Ok(1)
}

fn native_getgid(ctx: &mut ExternCtx) -> ExternResult {
    // Return 0 on non-Unix platforms
    ctx.ret_i64(0, 0);
    ExternResult::Ok(1)
}

fn native_getwd(ctx: &mut ExternCtx) -> ExternResult {
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
    ExternResult::Ok(2)
}

fn native_chdir(ctx: &mut ExternCtx) -> ExternResult {
    let dir = ctx.arg_str(0).to_string();
    match std::env::set_current_dir(&dir) {
        Ok(()) => {
            ctx.ret_nil(0); // nil error
        }
        Err(e) => {
            ctx.ret_string(0, &e.to_string());
        }
    }
    ExternResult::Ok(1)
}

fn native_hostname(ctx: &mut ExternCtx) -> ExternResult {
    // Simple implementation using environment or default
    let hostname = std::env::var("HOSTNAME")
        .or_else(|_| std::env::var("COMPUTERNAME"))
        .unwrap_or_else(|_| "localhost".to_string());
    ctx.ret_string(0, &hostname);
    ctx.ret_nil(1); // nil error
    ExternResult::Ok(2)
}

// ==================== File Operations ====================

fn native_read_file(ctx: &mut ExternCtx) -> ExternResult {
    let name = ctx.arg_str(0).to_string();
    match std::fs::read(&name) {
        Ok(data) => {
            let gc = ctx.gc();
            let arr = array::create(gc, builtin::ARRAY, builtin::UINT8, 1, data.len());
            for (i, &b) in data.iter().enumerate() {
                array::set(arr, i, b as u64);
            }
            let result = slice::from_array(gc, builtin::SLICE, arr);
            ctx.ret_ref(0, result);
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_nil(0);
            ctx.ret_string(1, &e.to_string());
        }
    }
    ExternResult::Ok(2)
}

fn native_write_file(ctx: &mut ExternCtx) -> ExternResult {
    let name = ctx.arg_str(0).to_string();
    let data_ref = ctx.arg_ref(1);
    let _perm = ctx.arg_i64(2);
    
    // Extract bytes from slice
    let len = if data_ref.is_null() { 0 } else { slice::len(data_ref) };
    let mut data = Vec::with_capacity(len);
    for i in 0..len {
        data.push(slice::get(data_ref, i) as u8);
    }
    
    match std::fs::write(&name, &data) {
        Ok(()) => ctx.ret_nil(0),
        Err(e) => ctx.ret_string(0, &e.to_string()),
    }
    ExternResult::Ok(1)
}

fn native_remove(ctx: &mut ExternCtx) -> ExternResult {
    let name = ctx.arg_str(0).to_string();
    match std::fs::remove_file(&name) {
        Ok(()) => ctx.ret_nil(0),
        Err(e) => ctx.ret_string(0, &e.to_string()),
    }
    ExternResult::Ok(1)
}

fn native_remove_all(ctx: &mut ExternCtx) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    let result = if std::path::Path::new(&path).is_dir() {
        std::fs::remove_dir_all(&path)
    } else {
        std::fs::remove_file(&path)
    };
    match result {
        Ok(()) => ctx.ret_nil(0),
        Err(e) => ctx.ret_string(0, &e.to_string()),
    }
    ExternResult::Ok(1)
}

fn native_mkdir(ctx: &mut ExternCtx) -> ExternResult {
    let name = ctx.arg_str(0).to_string();
    let _perm = ctx.arg_i64(1);
    match std::fs::create_dir(&name) {
        Ok(()) => ctx.ret_nil(0),
        Err(e) => ctx.ret_string(0, &e.to_string()),
    }
    ExternResult::Ok(1)
}

fn native_mkdir_all(ctx: &mut ExternCtx) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    let _perm = ctx.arg_i64(1);
    match std::fs::create_dir_all(&path) {
        Ok(()) => ctx.ret_nil(0),
        Err(e) => ctx.ret_string(0, &e.to_string()),
    }
    ExternResult::Ok(1)
}

fn native_rename(ctx: &mut ExternCtx) -> ExternResult {
    let oldpath = ctx.arg_str(0).to_string();
    let newpath = ctx.arg_str(1).to_string();
    match std::fs::rename(&oldpath, &newpath) {
        Ok(()) => ctx.ret_nil(0),
        Err(e) => ctx.ret_string(0, &e.to_string()),
    }
    ExternResult::Ok(1)
}
