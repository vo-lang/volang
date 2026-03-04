//! WASM-specific implementations of libs/vox FFI functions.
//!
//! These replace the native libs/vox FFI (which reads from std::fs) with
//! implementations that read from the JS VirtualFS via vo_web_runtime_wasm::vfs.
//!
//! Slot conventions (Vo calling convention):
//!   - string / GcRef  : 1 slot
//!   - any / interface : 2 slots (slot0 = type info, slot1 = value)
//!   - error           : 2 slots (same layout as interface)
//!
//! Extern name format: `#[vo_fn("libs/vox", "Foo")]` → "libs_vox_Foo"

use std::cell::RefCell;
use std::path::PathBuf;

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult, InterfaceSlot};
use vo_runtime::builtins::error_helper::write_error_to;
use vo_common::vfs::MemoryFs;

const PFX: &str = "libs_vox_";

// =============================================================================
// Module storage — bytecode + name, indexed by handle id (i64)
// =============================================================================

thread_local! {
    static WASM_VOX_MODULES: RefCell<Vec<Option<(Vec<u8>, String)>>> =
        RefCell::new(Vec::new());
}

fn store_module(bytecode: Vec<u8>, name: String) -> i64 {
    WASM_VOX_MODULES.with(|m| {
        let mut modules = m.borrow_mut();
        for (i, slot) in modules.iter_mut().enumerate() {
            if slot.is_none() {
                *slot = Some((bytecode, name));
                return i as i64;
            }
        }
        let id = modules.len();
        modules.push(Some((bytecode, name)));
        id as i64
    })
}

fn get_module_bytecode(id: i64) -> Option<(Vec<u8>, String)> {
    WASM_VOX_MODULES.with(|m| m.borrow().get(id as usize).and_then(|s| s.clone()))
}

fn free_module(id: i64) {
    WASM_VOX_MODULES.with(|m| {
        let mut modules = m.borrow_mut();
        if let Some(slot) = modules.get_mut(id as usize) {
            *slot = None;
        }
    });
}

// =============================================================================
// VFS compile helpers
// =============================================================================

fn read_vfs_dir_recursive(dir: &str, fs: &mut MemoryFs) -> Result<(), String> {
    let (entries, err) = vo_web_runtime_wasm::vfs::read_dir(dir);
    if let Some(e) = err {
        return Err(format!("read dir '{}': {}", dir, e));
    }
    for (name, is_dir, _mode) in entries {
        let full = if dir == "/" {
            format!("/{}", name)
        } else {
            format!("{}/{}", dir, name)
        };
        if is_dir {
            read_vfs_dir_recursive(&full, fs)?;
        } else if name.ends_with(".vo") {
            let (data, err) = vo_web_runtime_wasm::vfs::read_file(&full);
            if let Some(e) = err {
                return Err(format!("read file '{}': {}", full, e));
            }
            let content = String::from_utf8(data)
                .map_err(|e| format!("utf8 '{}': {}", full, e))?;
            fs.add_file(PathBuf::from(full.trim_start_matches('/')), content);
        }
    }
    Ok(())
}

fn compile_file_from_vfs(path: &str) -> Result<(Vec<u8>, String), String> {
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(path);
    if let Some(e) = err {
        return Err(format!("read file '{}': {}", path, e));
    }
    let content = String::from_utf8(data)
        .map_err(|e| format!("utf8 '{}': {}", path, e))?;

    let entry_clean = path.trim_start_matches('/');
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(PathBuf::from(entry_clean), content);

    let std_fs = crate::build_user_std_fs();
    let bytecode = vo_web::compile_entry_with_std_fs(entry_clean, local_fs, std_fs)
        .map_err(|e| format!("compile error: {}", e))?;

    let name = PathBuf::from(path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();
    Ok((bytecode, name))
}

fn compile_dir_from_vfs(dir_path: &str) -> Result<(Vec<u8>, String), String> {
    let mut local_fs = MemoryFs::new();
    read_vfs_dir_recursive(dir_path, &mut local_fs)?;

    let entry = format!("{}/main.vo", dir_path.trim_start_matches('/'));
    let std_fs = crate::build_user_std_fs();
    let bytecode = vo_web::compile_entry_with_std_fs(&entry, local_fs, std_fs)
        .map_err(|e| format!("compile error: {}", e))?;

    let name = PathBuf::from(dir_path)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();
    Ok((bytecode, name))
}

fn compile_string_from_code(code: &str) -> Result<(Vec<u8>, String), String> {
    let mut local_fs = MemoryFs::new();
    local_fs.add_file(PathBuf::from("main.vo"), code.to_string());

    let std_fs = crate::build_user_std_fs();
    let bytecode = vo_web::compile_entry_with_std_fs("main.vo", local_fs, std_fs)
        .map_err(|e| format!("compile error: {}", e))?;
    Ok((bytecode, "main".to_string()))
}

/// Run bytecode and capture stdout output.
fn run_bytecode_capture(bytecode: &[u8]) -> Result<String, String> {
    vo_runtime::output::clear_output();
    let result = vo_web::create_vm(bytecode, |_reg, _exts| {});
    let captured = vo_web::take_output();
    match result {
        Ok(_) => Ok(captured),
        Err(e) => {
            if captured.trim().is_empty() {
                Err(e)
            } else {
                Err(format!("{}\nRuntime error: {}", captured.trim_end(), e))
            }
        }
    }
}

/// Run bytecode without capturing output.
fn run_bytecode(bytecode: &[u8]) -> Result<(), String> {
    vo_runtime::output::clear_output();
    vo_web::create_vm(bytecode, |_reg, _exts| {}).map(|_| ())
}

// =============================================================================
// FFI functions
// =============================================================================

// CompileFile(path string) (Module, error)
// args: path@0[1]  rets: Module@0[2], error@2[2]
fn ffi_compile_file(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    match compile_file_from_vfs(&path) {
        Ok((bytecode, name)) => {
            let id = store_module(bytecode, name);
            ctx.ret_any(0, InterfaceSlot::from_i64(id));
            ctx.ret_nil_error(2);
        }
        Err(e) => {
            ctx.ret_any(0, InterfaceSlot::nil());
            write_error_to(ctx, 2, &e);
        }
    }
    ExternResult::Ok
}

// CompileDir(path string) (Module, error) — same layout as CompileFile
fn ffi_compile_dir(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    match compile_dir_from_vfs(&path) {
        Ok((bytecode, name)) => {
            let id = store_module(bytecode, name);
            ctx.ret_any(0, InterfaceSlot::from_i64(id));
            ctx.ret_nil_error(2);
        }
        Err(e) => {
            ctx.ret_any(0, InterfaceSlot::nil());
            write_error_to(ctx, 2, &e);
        }
    }
    ExternResult::Ok
}

// CompileString(code string) (Module, error) — same layout as CompileFile
fn ffi_compile_string(ctx: &mut ExternCallContext) -> ExternResult {
    let code = ctx.arg_str(0).to_string();
    match compile_string_from_code(&code) {
        Ok((bytecode, name)) => {
            let id = store_module(bytecode, name);
            ctx.ret_any(0, InterfaceSlot::from_i64(id));
            ctx.ret_nil_error(2);
        }
        Err(e) => {
            ctx.ret_any(0, InterfaceSlot::nil());
            write_error_to(ctx, 2, &e);
        }
    }
    ExternResult::Ok
}

// Run(m Module) error
// args: m@0[2]  rets: error@0[2]
fn ffi_run(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(0);
    match get_module_bytecode(module_id) {
        None => write_error_to(ctx, 0, "invalid module handle"),
        Some((bytecode, _)) => match run_bytecode(&bytecode) {
            Ok(()) => ctx.ret_nil_error(0),
            Err(e) => write_error_to(ctx, 0, &e),
        },
    }
    ExternResult::Ok
}

// RunCapture(m Module) (string, error)
// args: m@0[2]  rets: string@0[1], error@1[2]
fn ffi_run_capture(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(0);
    match get_module_bytecode(module_id) {
        None => {
            ctx.ret_str(0, "");
            write_error_to(ctx, 1, "invalid module handle");
        }
        Some((bytecode, _)) => match run_bytecode_capture(&bytecode) {
            Ok(captured) => {
                ctx.ret_str(0, &captured);
                ctx.ret_nil_error(1);
            }
            Err(e) => {
                ctx.ret_str(0, "");
                write_error_to(ctx, 1, &e);
            }
        },
    }
    ExternResult::Ok
}

// RunFile(path string) error / RunFileJit — compile+run in one step
// args: path@0[1]  rets: error@0[2]
fn ffi_run_file(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    match compile_file_from_vfs(&path) {
        Err(e) => write_error_to(ctx, 0, &e),
        Ok((bytecode, _)) => match run_bytecode(&bytecode) {
            Ok(()) => ctx.ret_nil_error(0),
            Err(e) => write_error_to(ctx, 0, &e),
        },
    }
    ExternResult::Ok
}

// Free(m Module) — no return value
// args: m@0[2]
fn ffi_free(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(0);
    free_module(module_id);
    ExternResult::Ok
}

// Name(m Module) string
// args: m@0[2]  rets: string@0[1]
fn ffi_name(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(0);
    let name = get_module_bytecode(module_id)
        .map(|(_, n)| n)
        .unwrap_or_default();
    ctx.ret_str(0, &name);
    ExternResult::Ok
}

// FormatBytecode(m Module) string — not available in WASM (returns empty)
fn ffi_format_bytecode(ctx: &mut ExternCallContext) -> ExternResult {
    ctx.ret_str(0, "");
    ExternResult::Ok
}

// SaveBytecodeBinary(m Module, path string) error
// args: m@0[2], path@2[1]  rets: error@0[2]
fn ffi_save_bytecode_binary(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(0);
    let path = ctx.arg_str(2).to_string();
    match get_module_bytecode(module_id) {
        None => write_error_to(ctx, 0, "invalid module handle"),
        Some((bytecode, _)) => {
            match vo_web_runtime_wasm::vfs::write_file(&path, &bytecode, 0o644) {
                None => ctx.ret_nil_error(0),
                Some(e) => write_error_to(ctx, 0, &e),
            }
        }
    }
    ExternResult::Ok
}

// SaveBytecodeText(m Module, path string) error — same arg layout as SaveBytecodeBinary
fn ffi_save_bytecode_text(ctx: &mut ExternCallContext) -> ExternResult {
    write_error_to(ctx, 0, "SaveBytecodeText is not available in WASM");
    ExternResult::Ok
}

// LoadBytecodeBinary(path string) (Module, error)
// args: path@0[1]  rets: Module@0[2], error@2[2]
fn ffi_load_bytecode_binary(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    let (data, err) = vo_web_runtime_wasm::vfs::read_file(&path);
    if let Some(e) = err {
        ctx.ret_any(0, InterfaceSlot::nil());
        write_error_to(ctx, 2, &e);
        return ExternResult::Ok;
    }
    let name = PathBuf::from(&path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module")
        .to_string();
    let id = store_module(data, name);
    ctx.ret_any(0, InterfaceSlot::from_i64(id));
    ctx.ret_nil_error(2);
    ExternResult::Ok
}

// CompileCheck(code string) (string, error)
// args: code@0[1]  rets: string@0[1], error@1[2]
fn ffi_compile_check(ctx: &mut ExternCallContext) -> ExternResult {
    let code = ctx.arg_str(0).to_string();
    match compile_string_from_code(&code) {
        Ok(_) => {
            ctx.ret_str(0, "");
            ctx.ret_nil_error(1);
        }
        Err(e) => {
            ctx.ret_str(0, &e);
            ctx.ret_nil_error(1);
        }
    }
    ExternResult::Ok
}

// Returns true when `path` exists in the JS VFS (stat returns no error).
#[inline]
fn vfs_exists(path: &str) -> bool {
    let (.., err) = vo_web_runtime_wasm::vfs::stat(path);
    err.is_none()
}

// InitProject(dir, modName string) (string, error)
// args: dir@0[1], modName@1[1]  rets: string@0[1], error@1[2]
fn ffi_init_project(ctx: &mut ExternCallContext) -> ExternResult {
    let dir      = ctx.arg_str(0).to_string();
    let mod_name = ctx.arg_str(1).to_string();

    if let Some(e) = vo_web_runtime_wasm::vfs::mkdir_all(&dir, 0o755) {
        ctx.ret_str(0, "");
        write_error_to(ctx, 1, &e);
        return ExternResult::Ok;
    }

    let mut created: Vec<&'static str> = Vec::new();

    let main_path = format!("{}/main.vo", dir);
    if !vfs_exists(&main_path) {
        let src = "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, Vo!\")\n}\n";
        if let Some(e) = vo_web_runtime_wasm::vfs::write_file(&main_path, src.as_bytes(), 0o644) {
            ctx.ret_str(0, "");
            write_error_to(ctx, 1, &e);
            return ExternResult::Ok;
        }
        created.push("main.vo");
    }

    let mod_path = format!("{}/vo.mod", dir);
    if !vfs_exists(&mod_path) {
        let mod_src = format!("module {}\n\nvo 0.1\n", mod_name);
        if let Some(e) = vo_web_runtime_wasm::vfs::write_file(&mod_path, mod_src.as_bytes(), 0o644) {
            ctx.ret_str(0, "");
            write_error_to(ctx, 1, &e);
            return ExternResult::Ok;
        }
        created.push("vo.mod");
    }

    ctx.ret_str(0, &created.join("\n"));
    ctx.ret_nil_error(1);
    ExternResult::Ok
}

// InitFile(path string) error
// args: path@0[1]  rets: error@0[2]
fn ffi_init_file(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(0).to_string();

    if vfs_exists(&path) {
        write_error_to(ctx, 0, &format!("file already exists: {}", path));
        return ExternResult::Ok;
    }

    let pkg = std::path::Path::new(&path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");

    let src = format!(
        "package {}\n\nimport \"fmt\"\n\nfunc main() {{\n\tfmt.Println(\"Hello, Vo!\")\n}}\n",
        pkg
    );

    match vo_web_runtime_wasm::vfs::write_file(&path, src.as_bytes(), 0o644) {
        None    => ctx.ret_nil_error(0),
        Some(e) => write_error_to(ctx, 0, &e),
    }
    ExternResult::Ok
}

// Get(spec string) (string, error)
// args: spec@0[1]  rets: string@0[1], error@1[2]
//
// In the Studio shell flow TypeScript intercepts `vo.get` in WasmVoRunner and
// calls preloadModule() directly — runShellHandler is never reached, so this
// function is only called when user Vo code invokes vox.Get() as a library
// function.  By the time that happens the caller must have arranged for the
// files to already be in VFS; we just echo back the canonical VFS root path.
fn ffi_get(ctx: &mut ExternCallContext) -> ExternResult {
    let spec = ctx.arg_str(0).to_string();
    let module = match spec.rsplit_once('@') {
        Some((m, _)) if !m.is_empty() => m.to_string(),
        _ => {
            ctx.ret_str(0, "");
            write_error_to(ctx, 1, &format!(
                "invalid spec {:?}: expected <module>@<version>", spec
            ));
            return ExternResult::Ok;
        }
    };
    let path = format!("/{}", module);
    ctx.ret_str(0, &path);
    ctx.ret_nil_error(1);
    ExternResult::Ok
}

// FreeAst(node AstNode) — no-op (AST not supported in WASM shell handler)
fn ffi_noop(_ctx: &mut ExternCallContext) -> ExternResult {
    ExternResult::Ok
}

// ParseFile/ParseString(... string) (AstNode, error) — not supported
// rets: AstNode@0[2], error@2[2]
fn ffi_not_supported_two_ret(ctx: &mut ExternCallContext) -> ExternResult {
    ctx.ret_any(0, InterfaceSlot::nil());
    write_error_to(ctx, 2, "not supported in WASM");
    ExternResult::Ok
}

// PrintAst(node AstNode) string — not supported
fn ffi_empty_str(ctx: &mut ExternCallContext) -> ExternResult {
    ctx.ret_str(0, "");
    ExternResult::Ok
}

// RunGui(m Module) ([]byte, error)
// args: m@0[2]  rets: []byte@0[1], error@1[2]
fn ffi_run_gui_stub(ctx: &mut ExternCallContext) -> ExternResult {
    ctx.ret_nil(0);
    write_error_to(ctx, 1, "RunGui is not supported in the browser sandbox");
    ExternResult::Ok
}

// SendGuiEvent(m Module, handlerId int, payload string) ([]byte, error)
// rets: []byte@0[1], error@1[2]
fn ffi_send_gui_event_stub(ctx: &mut ExternCallContext) -> ExternResult {
    ctx.ret_nil(0);
    write_error_to(ctx, 1, "SendGuiEvent is not supported in the browser sandbox");
    ExternResult::Ok
}

// =============================================================================
// Registration
// =============================================================================

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        let Some(func) = def.name.strip_prefix(PFX) else { continue };
        match func {
            "CompileFile"        => registry.register(id as u32, ffi_compile_file),
            "CompileDir"         => registry.register(id as u32, ffi_compile_dir),
            "CompileString"      => registry.register(id as u32, ffi_compile_string),
            "Run"                => registry.register(id as u32, ffi_run),
            "RunCapture"         => registry.register(id as u32, ffi_run_capture),
            "RunJit"             => registry.register(id as u32, ffi_run),
            "RunJitCapture"      => registry.register(id as u32, ffi_run_capture),
            "RunFile"            => registry.register(id as u32, ffi_run_file),
            "RunFileJit"         => registry.register(id as u32, ffi_run_file),
            "Free"               => registry.register(id as u32, ffi_free),
            "Name"               => registry.register(id as u32, ffi_name),
            "FormatBytecode"     => registry.register(id as u32, ffi_format_bytecode),
            "SaveBytecodeBinary" => registry.register(id as u32, ffi_save_bytecode_binary),
            "SaveBytecodeText"   => registry.register(id as u32, ffi_save_bytecode_text),
            "LoadBytecodeBinary" => registry.register(id as u32, ffi_load_bytecode_binary),
            "LoadBytecodeText"   => registry.register(id as u32, ffi_not_supported_two_ret),
            "CompileCheck"       => registry.register(id as u32, ffi_compile_check),
            "ParseFile"          => registry.register(id as u32, ffi_not_supported_two_ret),
            "ParseString"        => registry.register(id as u32, ffi_not_supported_two_ret),
            "PrintAst"           => registry.register(id as u32, ffi_empty_str),
            "FreeAst"            => registry.register(id as u32, ffi_noop),
            "RunGui"             => registry.register(id as u32, ffi_run_gui_stub),
            "SendGuiEvent"       => registry.register(id as u32, ffi_send_gui_event_stub),
            "StopGui"            => registry.register(id as u32, ffi_noop),
            "InitProject"        => registry.register(id as u32, ffi_init_project),
            "InitFile"           => registry.register(id as u32, ffi_init_file),
            "Get"                => registry.register(id as u32, ffi_get),
            _ => {}
        }
    }
}
