//! FFI bindings for the runner package.
//!
//! Exposes compile, run, AST, and bytecode functions to Vo code.

use std::sync::Mutex;
use vo_ext::prelude::*;
use vo_vm::bytecode::Module;
use crate::{compile, compile_string, CompileOutput, run, RunMode};
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::output;
use vo_common::symbol::SymbolInterner;
use vo_syntax::parser;
use vo_syntax::ast::File;

use crate::printer::AstPrinter;
use crate::format::format_text;


// ============ Module Storage ============

struct StoredModule {
    module: Module,
    source_root: std::path::PathBuf,
    extensions: Vec<vo_runtime::ext_loader::ExtensionManifest>,
}

#[vo_fn("libs/vox", "RunJitCapture")]
fn runner_run_jit_capture(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(slots::ARG_M);

    let stored = match get_module(module_id) {
        Some(m) => m,
        None => {
            ctx.ret_str(slots::RET_0, "");
            write_error_to(ctx, slots::RET_1, "invalid module handle");
            return ExternResult::Ok;
        }
    };

    let output_obj = CompileOutput {
        module: stored.module,
        source_root: stored.source_root,
        extensions: stored.extensions,
    };

    output::start_capture();
    let run_result = run(output_obj, RunMode::Jit, Vec::new());
    let captured = output::stop_capture();

    ctx.ret_str(slots::RET_0, &captured);
    match run_result {
        Ok(()) => ctx.ret_nil_error(slots::RET_1),
        Err(e) => write_error_to(ctx, slots::RET_1, &e.to_string()),
    }
    ExternResult::Ok
}

static MODULES: Mutex<Vec<Option<StoredModule>>> = Mutex::new(Vec::new());

fn store_module(output: CompileOutput) -> i64 {
    let mut modules = MODULES.lock().unwrap();
    for (i, slot) in modules.iter_mut().enumerate() {
        if slot.is_none() {
            *slot = Some(StoredModule {
                module: output.module,
                source_root: output.source_root,
                extensions: output.extensions,
            });
            return i as i64;
        }
    }
    let id = modules.len();
    modules.push(Some(StoredModule {
        module: output.module,
        source_root: output.source_root,
        extensions: output.extensions,
    }));
    id as i64
}

#[allow(dead_code)]
fn take_module(id: i64) -> Option<StoredModule> {
    let mut modules = MODULES.lock().unwrap();
    let idx = id as usize;
    if idx < modules.len() {
        modules[idx].take()
    } else {
        None
    }
}

fn get_module(id: i64) -> Option<StoredModule> {
    let modules = MODULES.lock().unwrap();
    let idx = id as usize;
    if idx < modules.len() {
        modules[idx].as_ref().map(|m| StoredModule {
            module: m.module.clone(),
            source_root: m.source_root.clone(),
            extensions: m.extensions.clone(),
        })
    } else {
        None
    }
}

fn free_module(id: i64) {
    let mut modules = MODULES.lock().unwrap();
    let idx = id as usize;
    if idx < modules.len() {
        modules[idx] = None;
    }
}

// ============ AST Storage ============

struct ParsedAst {
    file: File,
    interner: SymbolInterner,
}

static AST_NODES: Mutex<Vec<Option<ParsedAst>>> = Mutex::new(Vec::new());

fn store_ast(ast: ParsedAst) -> i64 {
    let mut nodes = AST_NODES.lock().unwrap();
    for (i, slot) in nodes.iter_mut().enumerate() {
        if slot.is_none() {
            *slot = Some(ast);
            return i as i64;
        }
    }
    let id = nodes.len();
    nodes.push(Some(ast));
    id as i64
}

fn free_ast(id: i64) {
    let mut nodes = AST_NODES.lock().unwrap();
    let idx = id as usize;
    if idx < nodes.len() {
        nodes[idx] = None;
    }
}

// ============ Compile Functions ============

#[vo_fn("libs/vox", "CompileFile")]
fn runner_compile_file(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(slots::ARG_PATH).to_string();
    
    match compile(&path) {
        Ok(output) => {
            let id = store_module(output);
            ctx.ret_any(slots::RET_0, InterfaceSlot::from_i64(id));
            ctx.ret_nil_error(slots::RET_1);
        }
        Err(e) => {
            ctx.ret_any(slots::RET_0, InterfaceSlot::nil());
            write_error_to(ctx, slots::RET_1, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[vo_fn("libs/vox", "CompileDir")]
fn runner_compile_dir(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(slots::ARG_PATH).to_string();
    
    match compile(&path) {
        Ok(output) => {
            let id = store_module(output);
            ctx.ret_any(slots::RET_0, InterfaceSlot::from_i64(id));
            ctx.ret_nil_error(slots::RET_1);
        }
        Err(e) => {
            ctx.ret_any(slots::RET_0, InterfaceSlot::nil());
            write_error_to(ctx, slots::RET_1, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[vo_fn("libs/vox", "CompileString")]
fn runner_compile_string(ctx: &mut ExternCallContext) -> ExternResult {
    let code = ctx.arg_str(slots::ARG_CODE).to_string();
    
    match compile_string(&code) {
        Ok(output) => {
            let id = store_module(output);
            ctx.ret_any(slots::RET_0, InterfaceSlot::from_i64(id));
            ctx.ret_nil_error(slots::RET_1);
        }
        Err(e) => {
            ctx.ret_any(slots::RET_0, InterfaceSlot::nil());
            write_error_to(ctx, slots::RET_1, &e.to_string());
        }
    }
    ExternResult::Ok
}

// ============ Run Functions ============

#[vo_fn("libs/vox", "Run")]
fn runner_run(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(slots::ARG_M);
    
    let stored = match get_module(module_id) {
        Some(m) => m,
        None => {
            write_error_to(ctx, slots::RET_0, "invalid module handle");
            return ExternResult::Ok;
        }
    };

    let output = CompileOutput {
        module: stored.module,
        source_root: stored.source_root,
        extensions: stored.extensions,
    };

    match run(output, RunMode::Vm, Vec::new()) {
        Ok(()) => ctx.ret_nil_error(slots::RET_0),
        Err(e) => {
            write_error_to(ctx, slots::RET_0, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[vo_fn("libs/vox", "RunCapture")]
fn runner_run_capture(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(slots::ARG_M);

    let stored = match get_module(module_id) {
        Some(m) => m,
        None => {
            ctx.ret_str(slots::RET_0, "");
            write_error_to(ctx, slots::RET_1, "invalid module handle");
            return ExternResult::Ok;
        }
    };

    let output_obj = CompileOutput {
        module: stored.module,
        source_root: stored.source_root,
        extensions: stored.extensions,
    };

    output::start_capture();
    let run_result = run(output_obj, RunMode::Vm, Vec::new());
    let captured = output::stop_capture();

    ctx.ret_str(slots::RET_0, &captured);
    match run_result {
        Ok(()) => ctx.ret_nil_error(slots::RET_1),
        Err(e) => write_error_to(ctx, slots::RET_1, &e.to_string()),
    }
    ExternResult::Ok
}

#[vo_fn("libs/vox", "RunJit")]
fn runner_run_jit(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(slots::ARG_M);
    
    let stored = match get_module(module_id) {
        Some(m) => m,
        None => {
            write_error_to(ctx, slots::RET_0, "invalid module handle");
            return ExternResult::Ok;
        }
    };

    let output = CompileOutput {
        module: stored.module,
        source_root: stored.source_root,
        extensions: stored.extensions,
    };

    match run(output, RunMode::Jit, Vec::new()) {
        Ok(()) => ctx.ret_nil_error(slots::RET_0),
        Err(e) => {
            write_error_to(ctx, slots::RET_0, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[vo_fn("libs/vox", "RunFile")]
fn runner_run_file(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(slots::ARG_PATH).to_string();
    
    match compile(&path).map_err(crate::RunError::from).and_then(|o| run(o, RunMode::Vm, Vec::new())) {
        Ok(()) => ctx.ret_nil_error(slots::RET_0),
        Err(e) => {
            write_error_to(ctx, slots::RET_0, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[vo_fn("libs/vox", "RunFileJit")]
fn runner_run_file_jit(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(slots::ARG_PATH).to_string();
    
    match compile(&path).map_err(crate::RunError::from).and_then(|o| run(o, RunMode::Jit, Vec::new())) {
        Ok(()) => ctx.ret_nil_error(slots::RET_0),
        Err(e) => {
            write_error_to(ctx, slots::RET_0, &e.to_string());
        }
    }
    ExternResult::Ok
}

// ============ Resource Functions ============

#[vo_fn("libs/vox", "Free")]
fn runner_free(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(slots::ARG_M);
    free_module(module_id);
    ExternResult::Ok
}

// ============ Info Functions ============

#[vo_fn("libs/vox", "Name")]
fn runner_name(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(slots::ARG_M);
    
    let name = match get_module(module_id) {
        Some(m) => m.module.name.clone(),
        None => String::new(),
    };
    
    ctx.ret_str(slots::RET_0, &name);
    ExternResult::Ok
}

// ============ Module Info Functions ============

#[vo_fn("libs/vox", "FormatBytecode")]
fn runner_format_bytecode(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(slots::ARG_M);
    
    let text = match get_module(module_id) {
        Some(m) => format_text(&m.module),
        None => String::new(),
    };
    
    ctx.ret_str(slots::RET_0, &text);
    ExternResult::Ok
}

// ============ AST Functions ============

#[vo_fn("libs/vox", "ParseFile")]
fn runner_parse_file(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(slots::ARG_PATH).to_string();
    
    let content = match std::fs::read_to_string(&path) {
        Ok(c) => c,
        Err(e) => {
            ctx.ret_any(slots::RET_0, InterfaceSlot::nil());
            write_error_to(ctx, slots::RET_1, &e.to_string());
            return ExternResult::Ok;
        }
    };
    
    let (file, diag, interner) = parser::parse(&content, 0);
    
    if diag.has_errors() {
        let msg = diag.iter().map(|d| d.message.as_str()).collect::<Vec<_>>().join("; ");
        ctx.ret_any(slots::RET_0, InterfaceSlot::nil());
        write_error_to(ctx, slots::RET_1, &msg);
        return ExternResult::Ok;
    }
    
    let id = store_ast(ParsedAst { file, interner });
    ctx.ret_any(slots::RET_0, InterfaceSlot::from_i64(id));
    write_nil_error(ctx, slots::RET_1);
    ExternResult::Ok
}

#[vo_fn("libs/vox", "ParseString")]
fn runner_parse_string(ctx: &mut ExternCallContext) -> ExternResult {
    let code = ctx.arg_str(slots::ARG_CODE).to_string();
    
    let (file, diag, interner) = parser::parse(&code, 0);
    
    if diag.has_errors() {
        let msg = diag.iter().map(|d| d.message.as_str()).collect::<Vec<_>>().join("; ");
        ctx.ret_any(slots::RET_0, InterfaceSlot::nil());
        write_error_to(ctx, slots::RET_1, &msg);
        return ExternResult::Ok;
    }
    
    let id = store_ast(ParsedAst { file, interner });
    ctx.ret_any(slots::RET_0, InterfaceSlot::from_i64(id));
    write_nil_error(ctx, slots::RET_1);
    ExternResult::Ok
}

#[vo_fn("libs/vox", "PrintAst")]
fn runner_print_ast(ctx: &mut ExternCallContext) -> ExternResult {
    let node_id = ctx.arg_any_as_i64(slots::ARG_NODE);
    
    let result = {
        let nodes = AST_NODES.lock().unwrap();
        let idx = node_id as usize;
        if idx < nodes.len() {
            if let Some(ast) = &nodes[idx] {
                let mut printer = AstPrinter::new(&ast.interner);
                Some(printer.print_file(&ast.file))
            } else {
                None
            }
        } else {
            None
        }
    };
    
    match result {
        Some(text) => ctx.ret_str(slots::RET_0, &text),
        None => ctx.ret_str(slots::RET_0, ""),
    }
    ExternResult::Ok
}

#[vo_fn("libs/vox", "FreeAst")]
fn runner_free_ast(ctx: &mut ExternCallContext) -> ExternResult {
    let node_id = ctx.arg_any_as_i64(slots::ARG_NODE);
    free_ast(node_id);
    ExternResult::Ok
}

// ============ Bytecode I/O Functions ============

#[vo_fn("libs/vox", "SaveBytecodeText")]
fn runner_save_bytecode_text(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(slots::ARG_M);
    let path = ctx.arg_str(slots::ARG_PATH).to_string();
    
    let module = match get_module(module_id) {
        Some(m) => m,
        None => {
            write_error_to(ctx, slots::RET_0, "invalid module handle");
            return ExternResult::Ok;
        }
    };
    
    let text = format_text(&module.module);
    match std::fs::write(&path, text) {
        Ok(()) => write_nil_error(ctx, slots::RET_0),
        Err(e) => write_error_to(ctx, slots::RET_0, &e.to_string()),
    }
    ExternResult::Ok
}

#[vo_fn("libs/vox", "LoadBytecodeText")]
fn runner_load_bytecode_text(ctx: &mut ExternCallContext) -> ExternResult {
    let _path = ctx.arg_str(slots::ARG_PATH).to_string();
    
    // Text parsing not yet implemented
    ctx.ret_any(slots::RET_0, InterfaceSlot::nil());
    write_error_to(ctx, slots::RET_1, "bytecode text parsing not yet implemented");
    ExternResult::Ok
}

#[vo_fn("libs/vox", "SaveBytecodeBinary")]
fn runner_save_bytecode_binary(ctx: &mut ExternCallContext) -> ExternResult {
    let module_id = ctx.arg_any_as_i64(slots::ARG_M);
    let path = ctx.arg_str(slots::ARG_PATH).to_string();
    
    let module = match get_module(module_id) {
        Some(m) => m,
        None => {
            write_error_to(ctx, slots::RET_0, "invalid module handle");
            return ExternResult::Ok;
        }
    };
    
    let bytes = module.module.serialize();
    match std::fs::write(&path, bytes) {
        Ok(()) => write_nil_error(ctx, slots::RET_0),
        Err(e) => write_error_to(ctx, slots::RET_0, &e.to_string()),
    }
    ExternResult::Ok
}

#[vo_fn("libs/vox", "LoadBytecodeBinary")]
fn runner_load_bytecode_binary(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(slots::ARG_PATH).to_string();
    
    let bytes = match std::fs::read(&path) {
        Ok(b) => b,
        Err(e) => {
            ctx.ret_any(slots::RET_0, InterfaceSlot::nil());
            write_error_to(ctx, slots::RET_1, &e.to_string());
            return ExternResult::Ok;
        }
    };
    
    match Module::deserialize(&bytes) {
        Ok(module) => {
            let output = CompileOutput {
                module,
                source_root: std::path::PathBuf::from(&path).parent().unwrap_or(std::path::Path::new(".")).to_path_buf(),
                extensions: Vec::new(),
            };
            let id = store_module(output);
            ctx.ret_any(slots::RET_0, InterfaceSlot::from_i64(id));
            write_nil_error(ctx, slots::RET_1);
        }
        Err(e) => {
            ctx.ret_any(slots::RET_0, InterfaceSlot::nil());
            write_error_to(ctx, slots::RET_1, &format!("{:?}", e));
        }
    }
    ExternResult::Ok
}

vo_ext::export_extensions!();
