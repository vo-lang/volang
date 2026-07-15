use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, MutexGuard, OnceLock};

use vo_module::schema::lockfile::LockedModule;
use vo_runtime::bytecode::ExternDef;
use vo_runtime::ext_loader::NativeExtensionSpec;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult, StdlibEntry};
use vo_runtime::io::{IoResourceToken, IoRuntime};
use vo_runtime::objects::interface::InterfaceSlot;
use vo_runtime::Module;

#[derive(Debug, Clone)]
pub struct ToolchainModule {
    pub module: Module,
    pub source_root: PathBuf,
    pub extensions: Vec<NativeExtensionSpec>,
    pub locked_modules: Vec<LockedModule>,
}

struct VmOwned<T> {
    value: T,
    cleanup_token: IoResourceToken,
}

struct VmHandleTable<T> {
    next_handle: Option<i64>,
    values: HashMap<i64, T>,
}

impl<T> Default for VmHandleTable<T> {
    fn default() -> Self {
        Self {
            next_handle: Some(1),
            values: HashMap::new(),
        }
    }
}

impl<T> VmHandleTable<T> {
    fn allocate_handle(&mut self) -> Result<i64, String> {
        let handle = self
            .next_handle
            .take()
            .ok_or_else(|| "toolchain handle space exhausted".to_string())?;
        self.next_handle = handle.checked_add(1);
        Ok(handle)
    }
}

fn lock_recover<T>(mutex: &'static Mutex<T>) -> MutexGuard<'static, T> {
    mutex
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

#[derive(Clone, Copy)]
pub enum ToolchainRunMode {
    Vm,
    Jit,
}

pub trait ToolchainHost: Send + Sync {
    fn compile_file(&self, path: &Path) -> Result<ToolchainModule, String>;
    fn compile_dir(&self, path: &Path) -> Result<ToolchainModule, String>;
    fn compile_string(&self, code: &str) -> Result<ToolchainModule, String>;
    fn run(&self, module: &ToolchainModule, mode: ToolchainRunMode) -> Result<(), String>;
    fn run_capture(
        &self,
        module: &ToolchainModule,
        mode: ToolchainRunMode,
    ) -> Result<Vec<u8>, String>;
    fn parse_file(&self, path: &Path) -> Result<String, String>;
    fn parse_string(&self, code: &str) -> Result<String, String>;
    fn format_source(&self, code: &str) -> Result<String, String>;
    fn format_bytecode(&self, module: &ToolchainModule) -> String;
    fn save_bytecode_text(&self, module: &ToolchainModule, path: &Path) -> Result<(), String>;
    fn save_bytecode_binary(&self, module: &ToolchainModule, path: &Path) -> Result<(), String>;
    fn load_bytecode_binary(&self, path: &Path) -> Result<ToolchainModule, String>;
    fn init_project(&self, dir: &Path, mod_name: &str) -> Result<String, String>;
    fn init_file(&self, path: &Path) -> Result<(), String>;
    fn get(&self, spec: &str) -> Result<Vec<u8>, String>;
}

fn host_cell() -> &'static Mutex<Option<Arc<dyn ToolchainHost>>> {
    static HOST: OnceLock<Mutex<Option<Arc<dyn ToolchainHost>>>> = OnceLock::new();
    HOST.get_or_init(|| Mutex::new(None))
}

fn module_cell() -> &'static Mutex<VmHandleTable<VmOwned<ToolchainModule>>> {
    static MODULES: OnceLock<Mutex<VmHandleTable<VmOwned<ToolchainModule>>>> = OnceLock::new();
    MODULES.get_or_init(|| Mutex::new(VmHandleTable::default()))
}

fn ast_cell() -> &'static Mutex<VmHandleTable<VmOwned<String>>> {
    static ASTS: OnceLock<Mutex<VmHandleTable<VmOwned<String>>>> = OnceLock::new();
    ASTS.get_or_init(|| Mutex::new(VmHandleTable::default()))
}

pub fn install_toolchain_host(host: Arc<dyn ToolchainHost>) {
    *lock_recover(host_cell()) = Some(host);
}

pub fn is_toolchain_host_installed() -> bool {
    lock_recover(host_cell()).is_some()
}

fn require_host() -> Result<Arc<dyn ToolchainHost>, String> {
    lock_recover(host_cell())
        .as_ref()
        .cloned()
        .ok_or_else(|| "toolchain host is not installed".to_string())
}

fn with_host<T, F>(f: F) -> Result<T, String>
where
    F: FnOnce(&dyn ToolchainHost) -> Result<T, String>,
{
    let host = require_host()?;
    f(host.as_ref())
}

fn store_module(io: &mut IoRuntime, module: ToolchainModule) -> Result<i64, String> {
    let id = lock_recover(module_cell()).allocate_handle()?;
    let cleanup_token = io
        .register_resource_cleanup(move |token| move || cleanup_module_handle(id, token))
        .map_err(|error| error.to_string())?;
    match lock_recover(module_cell()).values.entry(id) {
        std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(VmOwned {
                value: module,
                cleanup_token,
            });
        }
        std::collections::hash_map::Entry::Occupied(_) => {
            io.disarm_resource_cleanup(cleanup_token);
            return Err(format!(
                "toolchain module handle allocator produced duplicate handle {id}"
            ));
        }
    }
    Ok(id)
}

fn cleanup_module_handle(id: i64, cleanup_token: IoResourceToken) {
    let mut slots = lock_recover(module_cell());
    if slots
        .values
        .get(&id)
        .is_some_and(|slot| slot.cleanup_token == cleanup_token)
    {
        slots.values.remove(&id);
    }
}

fn valid_handle(id: i64) -> Option<i64> {
    (id > 0).then_some(id)
}

fn get_module(id: i64) -> Option<ToolchainModule> {
    let id = valid_handle(id)?;
    lock_recover(module_cell())
        .values
        .get(&id)
        .map(|slot| slot.value.clone())
}

fn require_module(id: i64) -> Result<ToolchainModule, String> {
    get_module(id).ok_or_else(|| "invalid module handle".to_string())
}

fn free_module(id: i64) -> Option<IoResourceToken> {
    let id = valid_handle(id)?;
    lock_recover(module_cell())
        .values
        .remove(&id)
        .map(|slot| slot.cleanup_token)
}

fn store_ast(io: &mut IoRuntime, ast: String) -> Result<i64, String> {
    let id = lock_recover(ast_cell()).allocate_handle()?;
    let cleanup_token = io
        .register_resource_cleanup(move |token| move || cleanup_ast_handle(id, token))
        .map_err(|error| error.to_string())?;
    match lock_recover(ast_cell()).values.entry(id) {
        std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(VmOwned {
                value: ast,
                cleanup_token,
            });
        }
        std::collections::hash_map::Entry::Occupied(_) => {
            io.disarm_resource_cleanup(cleanup_token);
            return Err(format!(
                "toolchain AST handle allocator produced duplicate handle {id}"
            ));
        }
    }
    Ok(id)
}

fn cleanup_ast_handle(id: i64, cleanup_token: IoResourceToken) {
    let mut slots = lock_recover(ast_cell());
    if slots
        .values
        .get(&id)
        .is_some_and(|slot| slot.cleanup_token == cleanup_token)
    {
        slots.values.remove(&id);
    }
}

fn get_ast(id: i64) -> Option<String> {
    let id = valid_handle(id)?;
    lock_recover(ast_cell())
        .values
        .get(&id)
        .map(|slot| slot.value.clone())
}

fn require_ast(id: i64) -> Result<String, String> {
    get_ast(id).ok_or_else(|| "invalid ast handle".to_string())
}

fn free_ast(id: i64) -> Option<IoResourceToken> {
    let id = valid_handle(id)?;
    lock_recover(ast_cell())
        .values
        .remove(&id)
        .map(|slot| slot.cleanup_token)
}

fn write_module_result(call: &mut ExternCallContext, result: Result<ToolchainModule, String>) {
    match result {
        Ok(module) => match store_module(call.io_mut(), module) {
            Ok(handle) => {
                call.ret_any(0, InterfaceSlot::from_i64(handle));
                call.ret_nil_error(2);
            }
            Err(err) => {
                call.ret_any(0, InterfaceSlot::nil());
                call.ret_error_msg(2, &err);
            }
        },
        Err(err) => {
            call.ret_any(0, InterfaceSlot::nil());
            call.ret_error_msg(2, &err);
        }
    }
}

fn write_ast_result(call: &mut ExternCallContext, result: Result<String, String>) {
    match result {
        Ok(ast) => match store_ast(call.io_mut(), ast) {
            Ok(handle) => {
                call.ret_any(0, InterfaceSlot::from_i64(handle));
                call.ret_nil_error(2);
            }
            Err(err) => {
                call.ret_any(0, InterfaceSlot::nil());
                call.ret_error_msg(2, &err);
            }
        },
        Err(err) => {
            call.ret_any(0, InterfaceSlot::nil());
            call.ret_error_msg(2, &err);
        }
    }
}

fn write_string_result(call: &mut ExternCallContext, result: Result<String, String>) {
    match result {
        Ok(value) => {
            call.ret_str(0, &value);
            call.ret_nil_error(1);
        }
        Err(err) => {
            call.ret_str(0, "");
            call.ret_error_msg(1, &err);
        }
    }
}

fn write_bytes_result(call: &mut ExternCallContext, result: Result<Vec<u8>, String>) {
    match result {
        Ok(value) => {
            call.ret_string_bytes(0, &value);
            call.ret_nil_error(1);
        }
        Err(err) => {
            call.ret_string_bytes(0, b"");
            call.ret_error_msg(1, &err);
        }
    }
}

fn text_arg(call: &ExternCallContext, slot: u16, description: &str) -> Result<String, String> {
    crate::host_bytes::utf8_arg(call, slot, description)
}

fn path_arg(call: &ExternCallContext, slot: u16, description: &str) -> Result<PathBuf, String> {
    crate::host_bytes::path_buf_from_bytes(call.arg_string_bytes(slot), description)
        .map_err(|error| error.to_string())
}

fn write_error_result(call: &mut ExternCallContext, result: Result<(), String>) {
    match result {
        Ok(()) => call.ret_nil_error(0),
        Err(err) => call.ret_error_msg(0, &err),
    }
}

fn unwrap_toolchain_result<T>(result: Result<T, String>) -> T {
    result.unwrap_or_else(|err| panic!("{}", err))
}

fn compile_check(call: &mut ExternCallContext) -> ExternResult {
    let code = match text_arg(call, 0, "source code") {
        Ok(code) => code,
        Err(error) => {
            call.ret_str(0, "");
            call.ret_error_msg(1, &error);
            return ExternResult::Ok;
        }
    };
    let host = match require_host() {
        Ok(host) => host,
        Err(err) => {
            call.ret_str(0, "");
            call.ret_error_msg(1, &err);
            return ExternResult::Ok;
        }
    };
    match host.compile_string(&code) {
        Ok(_) => {
            call.ret_str(0, "");
            call.ret_nil_error(1);
        }
        Err(err) => {
            call.ret_str(0, &err);
            call.ret_nil_error(1);
        }
    }
    ExternResult::Ok
}

fn toolchain_compile_file(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, 0, "source path")
        .and_then(|path| with_host(|host| host.compile_file(&path)));
    write_module_result(call, result);
    ExternResult::Ok
}

fn toolchain_compile_dir(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, 0, "source directory")
        .and_then(|path| with_host(|host| host.compile_dir(&path)));
    write_module_result(call, result);
    ExternResult::Ok
}

fn toolchain_compile_string(call: &mut ExternCallContext) -> ExternResult {
    let result = text_arg(call, 0, "source code")
        .and_then(|code| with_host(|host| host.compile_string(&code)));
    write_module_result(call, result);
    ExternResult::Ok
}

fn toolchain_run(call: &mut ExternCallContext) -> ExternResult {
    let module_id = call.arg_any_as_i64(0);
    let result = require_module(module_id)
        .and_then(|module| with_host(|host| host.run(&module, ToolchainRunMode::Vm)));
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_run_jit(call: &mut ExternCallContext) -> ExternResult {
    let module_id = call.arg_any_as_i64(0);
    let result = require_module(module_id)
        .and_then(|module| with_host(|host| host.run(&module, ToolchainRunMode::Jit)));
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_run_capture(call: &mut ExternCallContext) -> ExternResult {
    let module_id = call.arg_any_as_i64(0);
    let result = require_module(module_id)
        .and_then(|module| with_host(|host| host.run_capture(&module, ToolchainRunMode::Vm)));
    write_bytes_result(call, result);
    ExternResult::Ok
}

fn toolchain_run_jit_capture(call: &mut ExternCallContext) -> ExternResult {
    let module_id = call.arg_any_as_i64(0);
    let result = require_module(module_id)
        .and_then(|module| with_host(|host| host.run_capture(&module, ToolchainRunMode::Jit)));
    write_bytes_result(call, result);
    ExternResult::Ok
}

fn toolchain_run_file(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, 0, "source path").and_then(|path| {
        with_host(|host| {
            let module = host.compile_file(&path)?;
            host.run(&module, ToolchainRunMode::Vm)
        })
    });
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_run_file_jit(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, 0, "source path").and_then(|path| {
        with_host(|host| {
            let module = host.compile_file(&path)?;
            host.run(&module, ToolchainRunMode::Jit)
        })
    });
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_free(call: &mut ExternCallContext) -> ExternResult {
    if let Some(cleanup_token) = free_module(call.arg_any_as_i64(0)) {
        call.io_mut().disarm_resource_cleanup(cleanup_token);
    }
    ExternResult::Ok
}

fn toolchain_free_ast(call: &mut ExternCallContext) -> ExternResult {
    if let Some(cleanup_token) = free_ast(call.arg_any_as_i64(0)) {
        call.io_mut().disarm_resource_cleanup(cleanup_token);
    }
    ExternResult::Ok
}

fn toolchain_name(call: &mut ExternCallContext) -> ExternResult {
    let name = unwrap_toolchain_result(require_module(call.arg_any_as_i64(0)))
        .module
        .name;
    call.ret_str(0, &name);
    ExternResult::Ok
}

fn toolchain_format_source(call: &mut ExternCallContext) -> ExternResult {
    let result = text_arg(call, 0, "source code")
        .and_then(|code| with_host(|host| host.format_source(&code)));
    write_string_result(call, result);
    ExternResult::Ok
}

fn toolchain_format_bytecode(call: &mut ExternCallContext) -> ExternResult {
    let module = unwrap_toolchain_result(require_module(call.arg_any_as_i64(0)));
    let text = unwrap_toolchain_result(with_host(|host| Ok(host.format_bytecode(&module))));
    call.ret_str(0, &text);
    ExternResult::Ok
}

fn toolchain_parse_file(call: &mut ExternCallContext) -> ExternResult {
    let result =
        path_arg(call, 0, "source path").and_then(|path| with_host(|host| host.parse_file(&path)));
    write_ast_result(call, result);
    ExternResult::Ok
}

fn toolchain_parse_string(call: &mut ExternCallContext) -> ExternResult {
    let result = text_arg(call, 0, "source code")
        .and_then(|code| with_host(|host| host.parse_string(&code)));
    write_ast_result(call, result);
    ExternResult::Ok
}

fn toolchain_print_ast(call: &mut ExternCallContext) -> ExternResult {
    let text = unwrap_toolchain_result(require_ast(call.arg_any_as_i64(0)));
    call.ret_str(0, &text);
    ExternResult::Ok
}

fn toolchain_save_bytecode_text(call: &mut ExternCallContext) -> ExternResult {
    let module_id = call.arg_any_as_i64(0);
    let result = path_arg(call, 2, "bytecode path").and_then(|path| {
        require_module(module_id)
            .and_then(|module| with_host(|host| host.save_bytecode_text(&module, &path)))
    });
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_save_bytecode_binary(call: &mut ExternCallContext) -> ExternResult {
    let module_id = call.arg_any_as_i64(0);
    let result = path_arg(call, 2, "bytecode path").and_then(|path| {
        require_module(module_id)
            .and_then(|module| with_host(|host| host.save_bytecode_binary(&module, &path)))
    });
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_load_bytecode_binary(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, 0, "bytecode path")
        .and_then(|path| with_host(|host| host.load_bytecode_binary(&path)));
    write_module_result(call, result);
    ExternResult::Ok
}

fn toolchain_init_project(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, 0, "project directory").and_then(|dir| {
        text_arg(call, 1, "module name")
            .and_then(|mod_name| with_host(|host| host.init_project(&dir, &mod_name)))
    });
    write_string_result(call, result);
    ExternResult::Ok
}

fn toolchain_init_file(call: &mut ExternCallContext) -> ExternResult {
    let result =
        path_arg(call, 0, "source path").and_then(|path| with_host(|host| host.init_file(&path)));
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_get(call: &mut ExternCallContext) -> ExternResult {
    let result = text_arg(call, 0, "module specification")
        .and_then(|spec| with_host(|host| host.get(&spec)));
    write_bytes_result(call, result);
    ExternResult::Ok
}

#[doc(hidden)]
pub const REGISTERED_EXTERNS: &[StdlibEntry] = &[
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "CompileFile"),
        func: toolchain_compile_file,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "CompileDir"),
        func: toolchain_compile_dir,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "CompileString"),
        func: toolchain_compile_string,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "Run"),
        func: toolchain_run,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "RunJit"),
        func: toolchain_run_jit,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "RunCapture"),
        func: toolchain_run_capture,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "RunJitCapture"),
        func: toolchain_run_jit_capture,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "RunFile"),
        func: toolchain_run_file,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "RunFileJit"),
        func: toolchain_run_file_jit,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "Free"),
        func: toolchain_free,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "FreeAst"),
        func: toolchain_free_ast,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "Name"),
        func: toolchain_name,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "FormatSource"),
        func: toolchain_format_source,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "FormatBytecode"),
        func: toolchain_format_bytecode,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "ParseFile"),
        func: toolchain_parse_file,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "ParseString"),
        func: toolchain_parse_string,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "PrintAst"),
        func: toolchain_print_ast,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "SaveBytecodeText"),
        func: toolchain_save_bytecode_text,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "SaveBytecodeBinary"),
        func: toolchain_save_bytecode_binary,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "LoadBytecodeBinary"),
        func: toolchain_load_bytecode_binary,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "CompileCheck"),
        func: compile_check,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "InitProject"),
        func: toolchain_init_project,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "InitFile"),
        func: toolchain_init_file,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("toolchain", "Get"),
        func: toolchain_get,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
];

pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    for (id, def) in externs.iter().enumerate() {
        for entry in REGISTERED_EXTERNS {
            if def.name == entry.name() {
                entry.try_register(registry, id as u32)?;
                break;
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        ast_cell, cleanup_ast_handle, free_ast, get_ast, lock_recover, store_ast, valid_handle,
        VmHandleTable, VmOwned,
    };
    use vo_runtime::io::IoRuntime;

    #[test]
    fn toolchain_handles_reject_non_positive_values_without_host_width_aliasing() {
        assert_eq!(valid_handle(1), Some(1));
        assert_eq!(valid_handle(i64::MAX), Some(i64::MAX));
        assert_eq!(valid_handle(0), None);
        assert_eq!(valid_handle(-1), None);
        assert_eq!(valid_handle(i64::MIN), None);
    }

    #[test]
    fn freed_toolchain_handles_leave_no_tombstone_and_are_never_reissued() {
        let mut table = VmHandleTable::<&str>::default();
        let first = table.allocate_handle().expect("first handle");
        table.values.insert(first, "first");
        assert_eq!(table.values.remove(&first), Some("first"));
        assert!(table.values.is_empty());
        let second = table.allocate_handle().expect("second handle");
        table.values.insert(second, "second");

        assert_ne!(first, second);
        assert!(!table.values.contains_key(&first));
        assert_eq!(table.values.get(&second), Some(&"second"));
    }

    #[test]
    fn toolchain_handle_allocator_exhausts_stably_without_wrapping() {
        let mut table = VmHandleTable::<()> {
            next_handle: Some(i64::MAX),
            values: std::collections::HashMap::new(),
        };
        assert_eq!(table.allocate_handle().unwrap(), i64::MAX);
        for _ in 0..2 {
            let error = table.allocate_handle().unwrap_err();
            assert!(error.contains("handle space exhausted"));
        }
    }

    #[test]
    fn toolchain_ast_handles_follow_vm_lifetime_and_explicit_free_is_idempotent() {
        let mut owner = IoRuntime::new().expect("toolchain owner runtime");
        let handle =
            store_ast(&mut owner, String::from("owned AST")).expect("store VM-owned toolchain AST");
        assert_eq!(get_ast(handle).as_deref(), Some("owned AST"));
        drop(owner);
        assert_eq!(get_ast(handle), None);

        let mut explicit = IoRuntime::new().expect("toolchain explicit-free runtime");
        let handle = store_ast(&mut explicit, String::from("freed AST"))
            .expect("store explicitly freed toolchain AST");
        let cleanup_token = free_ast(handle).expect("free live AST handle");
        assert!(explicit.disarm_resource_cleanup(cleanup_token));
        assert!(!explicit.disarm_resource_cleanup(cleanup_token));
        assert_eq!(free_ast(handle), None);
        drop(explicit);
        assert_eq!(get_ast(handle), None);
    }

    #[test]
    fn stale_toolchain_cleanup_token_cannot_remove_a_reused_handle_generation() {
        let mut old_owner = IoRuntime::new().expect("old toolchain owner runtime");
        let handle =
            store_ast(&mut old_owner, String::from("old AST")).expect("store old VM-owned AST");
        let _old_cleanup_token = free_ast(handle).expect("detach old AST without disarming guard");

        let mut new_owner = IoRuntime::new().expect("new toolchain owner runtime");
        let new_cleanup_token = new_owner
            .register_resource_cleanup(move |token| move || cleanup_ast_handle(handle, token))
            .expect("new AST cleanup token");
        lock_recover(ast_cell()).values.insert(
            handle,
            VmOwned {
                value: String::from("new AST"),
                cleanup_token: new_cleanup_token,
            },
        );

        drop(old_owner);
        assert_eq!(get_ast(handle).as_deref(), Some("new AST"));
        drop(new_owner);
        assert_eq!(get_ast(handle), None);
    }
}
