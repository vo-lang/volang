use std::path::PathBuf;
use std::sync::{Arc, Mutex, OnceLock};

use vo_module::schema::lockfile::LockedModule;
use vo_runtime::ext_loader::NativeExtensionSpec;
use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
use vo_runtime::objects::interface::InterfaceSlot;
use vo_runtime::Module;

#[derive(Clone)]
pub struct ToolchainModule {
    pub module: Module,
    pub source_root: PathBuf,
    pub extensions: Vec<NativeExtensionSpec>,
    pub locked_modules: Vec<LockedModule>,
}

#[derive(Clone, Copy)]
pub enum ToolchainRunMode {
    Vm,
    Jit,
}

pub trait ToolchainHost: Send + Sync {
    fn compile_file(&self, path: &str) -> Result<ToolchainModule, String>;
    fn compile_dir(&self, path: &str) -> Result<ToolchainModule, String>;
    fn compile_string(&self, code: &str) -> Result<ToolchainModule, String>;
    fn run(&self, module: &ToolchainModule, mode: ToolchainRunMode) -> Result<(), String>;
    fn run_capture(
        &self,
        module: &ToolchainModule,
        mode: ToolchainRunMode,
    ) -> Result<String, String>;
    fn parse_file(&self, path: &str) -> Result<String, String>;
    fn parse_string(&self, code: &str) -> Result<String, String>;
    fn format_source(&self, code: &str) -> Result<String, String>;
    fn format_bytecode(&self, module: &ToolchainModule) -> String;
    fn save_bytecode_text(&self, module: &ToolchainModule, path: &str) -> Result<(), String>;
    fn load_bytecode_text(&self, path: &str) -> Result<ToolchainModule, String>;
    fn save_bytecode_binary(&self, module: &ToolchainModule, path: &str) -> Result<(), String>;
    fn load_bytecode_binary(&self, path: &str) -> Result<ToolchainModule, String>;
    fn init_project(&self, dir: &str, mod_name: &str) -> Result<String, String>;
    fn init_file(&self, path: &str) -> Result<(), String>;
    fn get(&self, spec: &str) -> Result<String, String>;
}

fn host_cell() -> &'static Mutex<Option<Arc<dyn ToolchainHost>>> {
    static HOST: OnceLock<Mutex<Option<Arc<dyn ToolchainHost>>>> = OnceLock::new();
    HOST.get_or_init(|| Mutex::new(None))
}

fn module_cell() -> &'static Mutex<Vec<Option<ToolchainModule>>> {
    static MODULES: OnceLock<Mutex<Vec<Option<ToolchainModule>>>> = OnceLock::new();
    MODULES.get_or_init(|| Mutex::new(Vec::new()))
}

fn ast_cell() -> &'static Mutex<Vec<Option<String>>> {
    static ASTS: OnceLock<Mutex<Vec<Option<String>>>> = OnceLock::new();
    ASTS.get_or_init(|| Mutex::new(Vec::new()))
}

pub fn install_toolchain_host(host: Arc<dyn ToolchainHost>) {
    *host_cell().lock().unwrap() = Some(host);
}

pub fn is_toolchain_host_installed() -> bool {
    host_cell().lock().unwrap().is_some()
}

fn require_host() -> Result<Arc<dyn ToolchainHost>, String> {
    host_cell()
        .lock()
        .unwrap()
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

fn store_module(module: ToolchainModule) -> i64 {
    let mut modules = module_cell().lock().unwrap();
    for (i, slot) in modules.iter_mut().enumerate() {
        if slot.is_none() {
            *slot = Some(module);
            return i as i64;
        }
    }
    let id = modules.len();
    modules.push(Some(module));
    id as i64
}

fn get_module(id: i64) -> Option<ToolchainModule> {
    module_cell()
        .lock()
        .unwrap()
        .get(id as usize)
        .and_then(|slot| slot.clone())
}

fn require_module(id: i64) -> Result<ToolchainModule, String> {
    get_module(id).ok_or_else(|| "invalid module handle".to_string())
}

fn free_module(id: i64) {
    if let Some(slot) = module_cell().lock().unwrap().get_mut(id as usize) {
        *slot = None;
    }
}

fn store_ast(ast: String) -> i64 {
    let mut asts = ast_cell().lock().unwrap();
    for (i, slot) in asts.iter_mut().enumerate() {
        if slot.is_none() {
            *slot = Some(ast);
            return i as i64;
        }
    }
    let id = asts.len();
    asts.push(Some(ast));
    id as i64
}

fn get_ast(id: i64) -> Option<String> {
    ast_cell()
        .lock()
        .unwrap()
        .get(id as usize)
        .and_then(|slot| slot.clone())
}

fn require_ast(id: i64) -> Result<String, String> {
    get_ast(id).ok_or_else(|| "invalid ast handle".to_string())
}

fn free_ast(id: i64) {
    if let Some(slot) = ast_cell().lock().unwrap().get_mut(id as usize) {
        *slot = None;
    }
}

fn write_module_result(call: &mut ExternCallContext, result: Result<ToolchainModule, String>) {
    match result {
        Ok(module) => {
            call.ret_any(0, InterfaceSlot::from_i64(store_module(module)));
            call.ret_nil_error(2);
        }
        Err(err) => {
            call.ret_any(0, InterfaceSlot::nil());
            call.ret_error_msg(2, &err);
        }
    }
}

fn write_ast_result(call: &mut ExternCallContext, result: Result<String, String>) {
    match result {
        Ok(ast) => {
            call.ret_any(0, InterfaceSlot::from_i64(store_ast(ast)));
            call.ret_nil_error(2);
        }
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
    let code = call.arg_str(0).to_string();
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
    let path = call.arg_str(0).to_string();
    let result = with_host(|host| host.compile_file(&path));
    write_module_result(call, result);
    ExternResult::Ok
}

fn toolchain_compile_dir(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0).to_string();
    let result = with_host(|host| host.compile_dir(&path));
    write_module_result(call, result);
    ExternResult::Ok
}

fn toolchain_compile_string(call: &mut ExternCallContext) -> ExternResult {
    let code = call.arg_str(0).to_string();
    let result = with_host(|host| host.compile_string(&code));
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
    write_string_result(call, result);
    ExternResult::Ok
}

fn toolchain_run_jit_capture(call: &mut ExternCallContext) -> ExternResult {
    let module_id = call.arg_any_as_i64(0);
    let result = require_module(module_id)
        .and_then(|module| with_host(|host| host.run_capture(&module, ToolchainRunMode::Jit)));
    write_string_result(call, result);
    ExternResult::Ok
}

fn toolchain_run_file(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0).to_string();
    let result = with_host(|host| {
        let module = host.compile_file(&path)?;
        host.run(&module, ToolchainRunMode::Vm)
    });
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_run_file_jit(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0).to_string();
    let result = with_host(|host| {
        let module = host.compile_file(&path)?;
        host.run(&module, ToolchainRunMode::Jit)
    });
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_free(call: &mut ExternCallContext) -> ExternResult {
    free_module(call.arg_any_as_i64(0));
    ExternResult::Ok
}

fn toolchain_free_ast(call: &mut ExternCallContext) -> ExternResult {
    free_ast(call.arg_any_as_i64(0));
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
    let code = call.arg_str(0).to_string();
    let result = with_host(|host| host.format_source(&code));
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
    let path = call.arg_str(0).to_string();
    let result = with_host(|host| host.parse_file(&path));
    write_ast_result(call, result);
    ExternResult::Ok
}

fn toolchain_parse_string(call: &mut ExternCallContext) -> ExternResult {
    let code = call.arg_str(0).to_string();
    let result = with_host(|host| host.parse_string(&code));
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
    let path = call.arg_str(2).to_string();
    let result = require_module(module_id)
        .and_then(|module| with_host(|host| host.save_bytecode_text(&module, &path)));
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_load_bytecode_text(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0).to_string();
    let result = with_host(|host| host.load_bytecode_text(&path));
    write_module_result(call, result);
    ExternResult::Ok
}

fn toolchain_save_bytecode_binary(call: &mut ExternCallContext) -> ExternResult {
    let module_id = call.arg_any_as_i64(0);
    let path = call.arg_str(2).to_string();
    let result = require_module(module_id)
        .and_then(|module| with_host(|host| host.save_bytecode_binary(&module, &path)));
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_load_bytecode_binary(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0).to_string();
    let result = with_host(|host| host.load_bytecode_binary(&path));
    write_module_result(call, result);
    ExternResult::Ok
}

fn toolchain_init_project(call: &mut ExternCallContext) -> ExternResult {
    let dir = call.arg_str(0).to_string();
    let mod_name = call.arg_str(1).to_string();
    let result = with_host(|host| host.init_project(&dir, &mod_name));
    write_string_result(call, result);
    ExternResult::Ok
}

fn toolchain_init_file(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0).to_string();
    let result = with_host(|host| host.init_file(&path));
    write_error_result(call, result);
    ExternResult::Ok
}

fn toolchain_get(call: &mut ExternCallContext) -> ExternResult {
    let spec = call.arg_str(0).to_string();
    let result = with_host(|host| host.get(&spec));
    write_string_result(call, result);
    ExternResult::Ok
}

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        let func = match def.name.as_str() {
            "toolchain_CompileFile" => Some(toolchain_compile_file as _),
            "toolchain_CompileDir" => Some(toolchain_compile_dir as _),
            "toolchain_CompileString" => Some(toolchain_compile_string as _),
            "toolchain_Run" => Some(toolchain_run as _),
            "toolchain_RunJit" => Some(toolchain_run_jit as _),
            "toolchain_RunCapture" => Some(toolchain_run_capture as _),
            "toolchain_RunJitCapture" => Some(toolchain_run_jit_capture as _),
            "toolchain_RunFile" => Some(toolchain_run_file as _),
            "toolchain_RunFileJit" => Some(toolchain_run_file_jit as _),
            "toolchain_Free" => Some(toolchain_free as _),
            "toolchain_FreeAst" => Some(toolchain_free_ast as _),
            "toolchain_Name" => Some(toolchain_name as _),
            "toolchain_FormatSource" => Some(toolchain_format_source as _),
            "toolchain_FormatBytecode" => Some(toolchain_format_bytecode as _),
            "toolchain_ParseFile" => Some(toolchain_parse_file as _),
            "toolchain_ParseString" => Some(toolchain_parse_string as _),
            "toolchain_PrintAst" => Some(toolchain_print_ast as _),
            "toolchain_SaveBytecodeText" => Some(toolchain_save_bytecode_text as _),
            "toolchain_LoadBytecodeText" => Some(toolchain_load_bytecode_text as _),
            "toolchain_SaveBytecodeBinary" => Some(toolchain_save_bytecode_binary as _),
            "toolchain_LoadBytecodeBinary" => Some(toolchain_load_bytecode_binary as _),
            "toolchain_CompileCheck" => Some(compile_check as _),
            "toolchain_InitProject" => Some(toolchain_init_project as _),
            "toolchain_InitFile" => Some(toolchain_init_file as _),
            "toolchain_Get" => Some(toolchain_get as _),
            _ => None,
        };
        if let Some(func) = func {
            registry.register(id as u32, func);
        }
    }
}
