//! Custom VM bytecode generation for GoX.
//!
//! This crate compiles type-checked GoX AST to VM bytecode.
//!
//! # Architecture
//!
//! ```text
//! gox-syntax::ast::File + gox-analysis::TypeCheckResult
//!                          │
//!                          ▼
//!                    ┌───────────┐
//!                    │  Codegen  │
//!                    └───────────┘
//!                          │
//!                          ▼
//!                   gox-vm::Module
//! ```
//!
//! # Multi-package compilation
//!
//! For multi-package projects:
//! 1. Packages are compiled in dependency order (dependencies first)
//! 2. Each package's init() functions are called in that order
//! 3. Cross-package calls use qualified names (pkg.Func)

mod types;
mod context;
mod expr;
mod stmt;

use std::collections::HashMap;
use gox_analysis::{Project, TypeCheckResult};
use gox_common::{Symbol, SymbolInterner};
use gox_syntax::ast::{Decl, File};
use gox_vm::bytecode::{FunctionDef, Module};
use gox_vm::instruction::Opcode;

pub use context::CodegenContext;

/// Compile a type-checked file to bytecode.
pub fn compile(
    file: &File,
    result: &TypeCheckResult,
    interner: &SymbolInterner,
) -> Result<Module, CodegenError> {
    let mut ctx = CodegenContext::new(file, result, interner);
    ctx.compile()
}

/// Compile a multi-package project to bytecode.
/// 
/// Packages are compiled in init order (dependencies first).
/// A module-level $init function is generated that calls all package inits.
pub fn compile_project(project: &Project) -> Result<Module, CodegenError> {
    use gox_vm::instruction::Instruction;
    
    
    let mut module = Module::new(&project.main_package);
    
    // Build cross-package function index: "pkg.Func" -> func_idx
    let mut cross_pkg_funcs: HashMap<String, u32> = HashMap::new();
    
    // Global variable indices: (pkg_name, var_name) -> global_idx
    let mut global_indices: HashMap<(String, Symbol), u32> = HashMap::new();
    
    // First pass: collect all global variables from ALL packages
    for pkg in &project.packages {
        for file in &pkg.files {
            for decl in &file.decls {
                if let Decl::Var(var) = decl {
                    for spec in &var.specs {
                        for name in &spec.names {
                            let var_name = pkg.interner.resolve(name.symbol).unwrap_or("");
                            let idx = module.add_global(var_name, 1);
                            global_indices.insert((pkg.name.clone(), name.symbol), idx);
                        }
                    }
                }
            }
        }
    }
    
    // Second pass: collect all function declarations from ALL packages
    let mut pkg_func_indices: Vec<HashMap<Symbol, u32>> = Vec::new();
    
    // method_table: "TypeName.MethodName" -> func_idx
    let mut method_table: HashMap<String, u32> = HashMap::new();
    
    // func_interface_params: func_idx -> Vec<param_index> for params that are interfaces
    let mut func_interface_params: HashMap<u32, Vec<u16>> = HashMap::new();
    
    for pkg in &project.packages {
        let mut func_indices: HashMap<Symbol, u32> = HashMap::new();
        
        for file in &pkg.files {
            for decl in &file.decls {
                if let Decl::Func(func) = decl {
                    let func_name = pkg.interner.resolve(func.name.symbol).unwrap_or("");
                    let idx = module.functions.len() as u32;
                    
                    // Track interface parameters
                    let mut iface_params: Vec<u16> = Vec::new();
                    let mut param_idx = 0u16;
                    for param in &func.sig.params {
                        if is_interface_type_expr(&pkg.types, &param.ty) {
                            for _ in &param.names {
                                iface_params.push(param_idx);
                                param_idx += 1;
                            }
                        } else {
                            param_idx += param.names.len() as u16;
                        }
                    }
                    if !iface_params.is_empty() {
                        func_interface_params.insert(idx, iface_params);
                    }
                    
                    // Register method with receiver type
                    if let Some(ref receiver) = func.receiver {
                        let type_name = pkg.interner.resolve(receiver.ty.symbol).unwrap_or("");
                        let method_key = format!("{}.{}", type_name, func_name);
                        method_table.insert(method_key, idx);
                        // Don't add methods to func_indices - they're looked up via method_table
                    } else {
                        // Only add non-method functions to func_indices
                        func_indices.insert(func.name.symbol, idx);
                    }
                    
                    // Register cross-package name for exported functions
                    if func_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                        let qualified_name = format!("{}.{}", pkg.name, func_name);
                        cross_pkg_funcs.insert(qualified_name, idx);
                    }
                    
                    // Also register init functions specially
                    if func_name == "init" {
                        let init_name = format!("{}.$init", pkg.name);
                        cross_pkg_funcs.insert(init_name, idx);
                    }
                    
                    module.functions.push(FunctionDef::new(func_name));
                }
            }
        }
        
        pkg_func_indices.push(func_indices);
    }
    
    // Third pass: generate $var_init functions for each package with var decls
    for pkg in &project.packages {
        if !pkg.has_var_decls {
            continue;
        }
        
        let var_init_idx = module.functions.len() as u32;
        let var_init_name = format!("{}.$var_init", pkg.name);
        cross_pkg_funcs.insert(var_init_name.clone(), var_init_idx);
        
        let mut func = FunctionDef::new(&format!("$var_init_{}", pkg.name));
        func.local_slots = 8; // Temp registers for init expressions
        
        // Compile var initializers
        for file in &pkg.files {
            for decl in &file.decls {
                if let Decl::Var(var) = decl {
                    for spec in &var.specs {
                        // If there are initializers, compile them
                        if !spec.values.is_empty() {
                            for (name, value) in spec.names.iter().zip(spec.values.iter()) {
                                if let Some(&global_idx) = global_indices.get(&(pkg.name.clone(), name.symbol)) {
                                    // Compile init expression to register 0
                                    let val_reg = compile_simple_expr(&pkg.interner, value, &mut func, &mut module, 0);
                                    // SetGlobal global_idx, val_reg
                                    func.code.push(Instruction::new(Opcode::SetGlobal, global_idx as u16, val_reg, 0));
                                }
                            }
                        }
                    }
                }
            }
        }
        
        func.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
        module.functions.push(func);
    }
    
    // Shared indices across all packages
    let mut native_indices: HashMap<String, u32> = HashMap::new();
    let mut const_indices: HashMap<String, u16> = HashMap::new();
    
    // Fourth pass: compile all function bodies
    for (pkg_idx, pkg) in project.packages.iter().enumerate() {
        let func_indices = &pkg_func_indices[pkg_idx];
        
        // Build package-local global indices
        let pkg_globals: HashMap<Symbol, u32> = global_indices.iter()
            .filter(|((pn, _), _)| pn == &pkg.name)
            .map(|((_, sym), idx)| (*sym, *idx))
            .collect();
        
        // Collect constant values from scope for inlining
        // Float constants are added to the module's constant pool
        let pkg_consts: HashMap<Symbol, ConstValue> = collect_const_values(&pkg.types.scope, &pkg.interner, &mut module);
        
        for file in &pkg.files {
            for decl in &file.decls {
                if let Decl::Func(func) = decl {
                    let mut ctx = context::CodegenContext::new(file, &pkg.types, &pkg.interner);
                    ctx.func_indices = func_indices.clone();
                    ctx.cross_pkg_funcs = cross_pkg_funcs.clone();
                    ctx.global_indices = pkg_globals.clone();
                    ctx.const_values = pkg_consts.clone();
                    ctx.native_indices = native_indices.clone();
                    ctx.const_indices = const_indices.clone();
                    ctx.method_table = method_table.clone();
                    ctx.func_interface_params = func_interface_params.clone();
                    // Set closure_func_offset to current function count so closures get correct indices
                    ctx.closure_func_offset = module.functions.len() as u32;
                    // Share the module's constant pool
                    ctx.module.constants = module.constants.clone();
                    
                    let func_def = ctx.compile_func_body(func)?;
                    // Look up function index - methods use method_table, regular functions use func_indices
                    let idx = if let Some(ref receiver) = func.receiver {
                        let func_name = pkg.interner.resolve(func.name.symbol).unwrap_or("");
                        let type_name = pkg.interner.resolve(receiver.ty.symbol).unwrap_or("");
                        let method_key = format!("{}.{}", type_name, func_name);
                        *method_table.get(&method_key).unwrap() as usize
                    } else {
                        func_indices[&func.name.symbol] as usize
                    };
                    module.functions[idx] = func_def;
                    
                    // Merge back changes
                    native_indices = ctx.native_indices;
                    const_indices = ctx.const_indices;
                    module.constants = ctx.module.constants;
                    module.natives = ctx.module.natives;
                    // Merge back any closure functions
                    for closure_func in ctx.module.functions {
                        module.functions.push(closure_func);
                    }
                }
            }
        }
    }
    
    // Generate module $init function that calls var inits then package inits in order
    let module_init_idx = module.functions.len() as u32;
    let module_init = generate_module_init(project, &cross_pkg_funcs);
    module.functions.push(module_init);
    
    // Find main function
    let main_idx = module.functions.iter()
        .position(|f| f.name == "main")
        .ok_or_else(|| CodegenError::Internal("no main function found".to_string()))?;
    
    // Generate entry point that calls $init then main
    let entry_idx = module.functions.len() as u32;
    let entry = generate_entry_point(module_init_idx, main_idx as u32);
    module.functions.push(entry);
    
    module.entry_func = entry_idx;
    
    Ok(module)
}

/// Compile a simple expression for var initialization.
/// Returns the register containing the result.
fn compile_simple_expr(
    interner: &SymbolInterner,
    expr: &gox_syntax::ast::Expr,
    func: &mut FunctionDef,
    module: &mut Module,
    reg: u16,
) -> u16 {
    use gox_vm::instruction::Instruction;
    use gox_vm::bytecode::Constant;
    use gox_syntax::ast::ExprKind;
    
    match &expr.kind {
        ExprKind::IntLit(v) => {
            let val = interner.resolve(v.raw)
                .and_then(|s| s.parse::<i64>().ok())
                .unwrap_or(0);
            if val >= i32::MIN as i64 && val <= i32::MAX as i64 {
                func.code.push(Instruction::new(Opcode::LoadInt, reg, 
                    (val as u32) as u16, ((val as u32) >> 16) as u16));
            } else {
                // Large int - store in constant pool
                let const_idx = module.add_constant(Constant::Int(val));
                func.code.push(Instruction::new(Opcode::LoadConst, reg, const_idx, 0));
            }
        }
        ExprKind::Ident(ident) => {
            // Check for true/false
            let name = interner.resolve(ident.symbol).unwrap_or("");
            if name == "true" {
                func.code.push(Instruction::new(Opcode::LoadTrue, reg, 0, 0));
            } else if name == "false" {
                func.code.push(Instruction::new(Opcode::LoadFalse, reg, 0, 0));
            } else {
                // Default to 0
                func.code.push(Instruction::new(Opcode::LoadInt, reg, 0, 0));
            }
        }
        ExprKind::Call(call) => {
            // Handle make() calls for global var initialization
            if let ExprKind::Ident(func_ident) = &call.func.kind {
                let func_name = interner.resolve(func_ident.symbol).unwrap_or("");
                if func_name == "make" && !call.args.is_empty() {
                    // Check for TypeAsExpr wrapper
                    if let ExprKind::TypeAsExpr(ty) = &call.args[0].kind {
                        match &ty.kind {
                            gox_syntax::ast::TypeExprKind::Chan(_) => {
                                let capacity = if call.args.len() > 1 {
                                    if let ExprKind::IntLit(lit) = &call.args[1].kind {
                                        interner.resolve(lit.raw)
                                            .and_then(|s| s.parse::<u16>().ok())
                                            .unwrap_or(0)
                                    } else {
                                        0
                                    }
                                } else {
                                    0
                                };
                                func.code.push(Instruction::new(Opcode::ChanNew, reg, 0, capacity));
                                return reg;
                            }
                            gox_syntax::ast::TypeExprKind::Map(_) => {
                                func.code.push(Instruction::new(Opcode::MapNew, reg, 0, 0));
                                return reg;
                            }
                            _ => {}
                        }
                    }
                }
            }
            // Default for other calls
            func.code.push(Instruction::new(Opcode::LoadInt, reg, 0, 0));
        }
        _ => {
            // Complex expressions: default to 0 for now
            func.code.push(Instruction::new(Opcode::LoadInt, reg, 0, 0));
        }
    }
    reg
}

/// Check if a type expression refers to an interface type.
fn is_interface_type_expr(types: &gox_analysis::TypeCheckResult, ty: &gox_syntax::ast::TypeExpr) -> bool {
    context::infer_type_from_type_expr(types, ty).0 == context::VarKind::Interface
}

/// Constant value for inlining.
#[derive(Clone, Copy)]
pub enum ConstValue {
    /// Integer constant (inlined as immediate)
    Int(i64),
    /// Float constant (stored in constant pool, value is index)
    FloatIdx(u16),
}

/// Collect constant values from scope for inlining.
/// Float constants are added to the module's constant pool.
fn collect_const_values(
    scope: &gox_analysis::scope::Scope,
    _interner: &SymbolInterner,
    module: &mut Module,
) -> HashMap<Symbol, ConstValue> {
    use gox_analysis::scope::Entity;
    use gox_analysis::types::{Type, BasicType};
    use gox_vm::bytecode::Constant as VmConstant;
    
    let mut consts = HashMap::new();
    
    for (sym, entity) in scope.local_symbols() {
        if let Entity::Var(var) = entity {
            if let Some(ref constant) = var.constant {
                // Check if this is a float type constant (typed or untyped)
                let is_float = matches!(&var.ty, 
                    Type::Basic(BasicType::Float32) | Type::Basic(BasicType::Float64) |
                    Type::Untyped(gox_analysis::types::UntypedKind::Float));
                
                if is_float {
                    // Add float to constant pool
                    if let Some(val) = constant.to_f64() {
                        let idx = module.add_constant(VmConstant::Float(val));
                        consts.insert(*sym, ConstValue::FloatIdx(idx));
                    }
                } else {
                    // For int constants, try to fit in i64
                    if let Some(val) = constant.to_i64() {
                        consts.insert(*sym, ConstValue::Int(val));
                    }
                }
            }
        }
    }
    
    consts
}

/// Generate the module $init function that calls var inits then package inits in order.
fn generate_module_init(
    project: &Project,
    cross_pkg_funcs: &HashMap<String, u32>,
) -> FunctionDef {
    use gox_vm::instruction::Instruction;
    
    let mut func = FunctionDef::new("$init");
    func.local_slots = 0;
    
    // Call each package's var init first, then init() in dependency order
    for pkg in &project.packages {
        // Call $var_init if package has var declarations
        let var_init_name = format!("{}.$var_init", pkg.name);
        if let Some(&var_init_idx) = cross_pkg_funcs.get(&var_init_name) {
            func.code.push(Instruction::with_flags(Opcode::Call, 0, var_init_idx as u16, 0, 0));
        }
        
        // Call init() if package has init function
        let init_name = format!("{}.$init", pkg.name);
        if let Some(&init_idx) = cross_pkg_funcs.get(&init_name) {
            func.code.push(Instruction::with_flags(Opcode::Call, 0, init_idx as u16, 0, 0));
        }
    }
    
    // Return
    func.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    
    func
}

/// Generate entry point that calls $init then main.
fn generate_entry_point(init_idx: u32, main_idx: u32) -> FunctionDef {
    use gox_vm::instruction::Instruction;
    
    let mut func = FunctionDef::new("$entry");
    func.local_slots = 0;
    
    // Call $init
    func.code.push(Instruction::with_flags(Opcode::Call, 0, init_idx as u16, 0, 0));
    
    // Call main
    func.code.push(Instruction::with_flags(Opcode::Call, 0, main_idx as u16, 0, 0));
    
    // Return
    func.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    
    func
}

/// Codegen error.
#[derive(Debug)]
pub enum CodegenError {
    /// Unsupported feature.
    Unsupported(String),
    /// Internal error.
    Internal(String),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::Unsupported(msg) => write!(f, "unsupported: {}", msg),
            CodegenError::Internal(msg) => write!(f, "internal error: {}", msg),
        }
    }
}

impl std::error::Error for CodegenError {}

#[cfg(test)]
mod tests {
    use super::*;
    use gox_common::{DiagnosticSink, FileId};
    use gox_syntax::parse;
    use gox_analysis::typecheck_file;
    use gox_vm::VmResult;

    fn compile_and_run(source: &str) -> VmResult {
        let file_id = FileId::new(0);
        let (file, _parse_diag, interner) = parse(file_id, source);
        let mut diag = DiagnosticSink::new();
        let result = typecheck_file(&file, &interner, &mut diag);
        
        if diag.has_errors() {
            panic!("Type check errors");
        }
        
        let module = compile(&file, &result, &interner).expect("Compilation failed");
        
        let mut vm = gox_runtime_vm::create_vm();
        vm.load_module(module);
        vm.run()
    }

    #[test]
    fn test_empty_main() {
        let result = compile_and_run(r#"
package main

func main() {
}
"#);
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
    }

    #[test]
    fn test_simple_arithmetic() {
        let result = compile_and_run(r#"
package main

func main() {
    x := 1 + 2 * 3
    _ = x
}
"#);
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
    }

    #[test]
    fn test_variable_assignment() {
        let result = compile_and_run(r#"
package main

func main() {
    x := 10
    y := 20
    z := x + y
    _ = z
}
"#);
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
    }

    #[test]
    fn test_println() {
        let result = compile_and_run(r#"
package main

func main() {
    println(42)
}
"#);
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
    }

    #[test]
    fn test_if_simple() {
        let result = compile_and_run(r#"
package main

func main() {
    x := 10
    if x > 5 {
        println(1)
    }
    println(2)
}
"#);
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
    }

    #[test]
    fn test_if_else() {
        let result = compile_and_run(r#"
package main

func main() {
    x := 3
    if x > 5 {
        println(1)
    } else {
        println(2)
    }
    println(3)
}
"#);
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
    }

    #[test]
    fn test_for_loop() {
        let result = compile_and_run(r#"
package main

func main() {
    for i := 0; i < 3; i = i + 1 {
        println(i)
    }
}
"#);
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
    }

    #[test]
    fn test_function_call() {
        let result = compile_and_run(r#"
package main

func add(a int, b int) int {
    return a + b
}

func main() {
    x := add(3, 4)
    println(x)
}
"#);
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
    }
}
