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
use gox_vm::bytecode::{Constant, FunctionDef, Module};
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
                            let var_name = project.interner.resolve(name.symbol).unwrap_or("");
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
                    let func_name = project.interner.resolve(func.name.symbol).unwrap_or("");
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
                        let type_name = project.interner.resolve(receiver.ty.symbol).unwrap_or("");
                        let method_key = format!("{}.{}", type_name, func_name);
                        method_table.insert(method_key, idx);
                        // Don't add methods to func_indices - they're looked up via method_table
                    } else if func_name == "init" {
                        // Register init functions with unique names (pkg.$init_0, pkg.$init_1, etc.)
                        // Don't add to func_indices since they can't be called directly
                        let init_count = cross_pkg_funcs.keys()
                            .filter(|k| k.starts_with(&format!("{}.$init_", pkg.name)))
                            .count();
                        let init_name = format!("{}.$init_{}", pkg.name, init_count);
                        cross_pkg_funcs.insert(init_name, idx);
                    } else {
                        // Only add non-method, non-init functions to func_indices
                        func_indices.insert(func.name.symbol, idx);
                        
                        // Register cross-package name for exported functions
                        if func_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                            let qualified_name = format!("{}.{}", pkg.name, func_name);
                            cross_pkg_funcs.insert(qualified_name, idx);
                        }
                    }
                    
                    module.functions.push(FunctionDef::new(func_name));
                }
            }
        }
        
        pkg_func_indices.push(func_indices);
    }
    
    // Third pass: reserve slots for $var_init functions (compiled in fourth pass)
    for pkg in &project.packages {
        if !pkg.has_var_decls {
            continue;
        }
        
        let var_init_idx = module.functions.len() as u32;
        let var_init_name = format!("{}.$var_init", pkg.name);
        cross_pkg_funcs.insert(var_init_name.clone(), var_init_idx);
        
        // Reserve slot - will be compiled in fourth pass with full context
        module.functions.push(FunctionDef::new(&format!("$var_init_{}", pkg.name)));
    }
    
    // Shared indices across all packages
    let mut native_indices: HashMap<String, u32> = HashMap::new();
    let mut const_indices: HashMap<String, u16> = HashMap::new();
    
    // Collect cross-package constants: "pkg.ConstName" -> const_pool_idx
    for pkg in &project.packages {
        let pkg_consts = collect_const_values(&pkg.types.scope, &project.interner, &mut module);
        for (sym, const_val) in &pkg_consts {
            if let Some(name) = project.interner.resolve(*sym) {
                // Only export capitalized constants
                if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                    let cross_key = format!("{}.{}", pkg.name, name);
                    match const_val {
                        ConstValue::Int(v) => {
                            let idx = module.constants.len() as u16;
                            module.constants.push(Constant::Int(*v));
                            const_indices.insert(cross_key, idx);
                        }
                        ConstValue::FloatIdx(idx) => {
                            const_indices.insert(cross_key, *idx);
                        }
                    }
                }
            }
        }
    }
    
    // Fourth pass: compile all function bodies
    for (pkg_idx, pkg) in project.packages.iter().enumerate() {
        let func_indices = &pkg_func_indices[pkg_idx];
        let mut init_counter = 0usize; // Track init function index within package
        
        // Build package-local global indices
        let pkg_globals: HashMap<Symbol, u32> = global_indices.iter()
            .filter(|((pn, _), _)| pn == &pkg.name)
            .map(|((_, sym), idx)| (*sym, *idx))
            .collect();
        
        // Collect constant values from scope for inlining
        // Float constants are added to the module's constant pool
        let pkg_consts: HashMap<Symbol, ConstValue> = collect_const_values(&pkg.types.scope, &project.interner, &mut module);
        
        for file in &pkg.files {
            for decl in &file.decls {
                if let Decl::Func(func) = decl {
                    let mut ctx = context::CodegenContext::new(file, &pkg.types, &project.interner);
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
                    let func_name = project.interner.resolve(func.name.symbol).unwrap_or("");
                    
                    // Look up function index - methods use method_table, init uses cross_pkg_funcs
                    let idx = if let Some(ref receiver) = func.receiver {
                        let type_name = project.interner.resolve(receiver.ty.symbol).unwrap_or("");
                        let method_key = format!("{}.{}", type_name, func_name);
                        *method_table.get(&method_key).unwrap() as usize
                    } else if func_name == "init" {
                        // Use init_counter to find the correct init function index
                        let init_name = format!("{}.$init_{}", pkg.name, init_counter);
                        init_counter += 1;
                        *cross_pkg_funcs.get(&init_name).unwrap() as usize
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
        
        // Compile $var_init function for this package if needed
        if pkg.has_var_decls {
            let var_init_name = format!("{}.$var_init", pkg.name);
            if let Some(&var_init_idx) = cross_pkg_funcs.get(&var_init_name) {
                // Create a context for compiling var initializers
                let first_file = pkg.files.first().unwrap();
                let mut ctx = context::CodegenContext::new(first_file, &pkg.types, &project.interner);
                ctx.func_indices = func_indices.clone();
                ctx.cross_pkg_funcs = cross_pkg_funcs.clone();
                ctx.global_indices = pkg_globals.clone();
                ctx.const_values = pkg_consts.clone();
                ctx.native_indices = native_indices.clone();
                ctx.const_indices = const_indices.clone();
                ctx.method_table = method_table.clone();
                ctx.func_interface_params = func_interface_params.clone();
                ctx.module.constants = module.constants.clone();
                
                let mut fctx = context::FuncContext::new(&format!("$var_init_{}", pkg.name));
                
                // Collect all var initializers for dependency analysis
                let mut var_inits: Vec<VarInit> = Vec::new();
                let mut pkg_var_symbols: std::collections::HashSet<Symbol> = std::collections::HashSet::new();
                
                for file in &pkg.files {
                    for decl in &file.decls {
                        if let Decl::Var(var) = decl {
                            for spec in &var.specs {
                                for name in &spec.names {
                                    pkg_var_symbols.insert(name.symbol);
                                }
                                if !spec.values.is_empty() {
                                    for (name, value) in spec.names.iter().zip(spec.values.iter()) {
                                        if let Some(&global_idx) = global_indices.get(&(pkg.name.clone(), name.symbol)) {
                                            var_inits.push(VarInit {
                                                name: name.symbol,
                                                value: value.clone(),
                                                global_idx,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                
                // Sort variables by dependency order
                let sorted_indices = topo_sort_vars(&var_inits, &pkg_var_symbols)?;
                
                // Compile var initializers in sorted order
                for idx in sorted_indices {
                    let var_init = &var_inits[idx];
                    if let Ok(val_reg) = expr::compile_expr(&mut ctx, &mut fctx, &var_init.value) {
                        fctx.emit(Opcode::SetGlobal, var_init.global_idx as u16, val_reg, 0);
                    }
                }
                
                fctx.emit(Opcode::Return, 0, 0, 0);
                let func_def = fctx.build();
                module.functions[var_init_idx as usize] = func_def;
                
                // Merge back changes
                native_indices = ctx.native_indices;
                const_indices = ctx.const_indices;
                module.constants = ctx.module.constants;
                module.natives = ctx.module.natives;
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

/// Check if a type expression refers to an interface type.
fn is_interface_type_expr(types: &gox_analysis::TypeCheckResult, ty: &gox_syntax::ast::TypeExpr) -> bool {
    context::infer_type_from_type_expr(types, ty).kind == gox_common::ValueKind::Interface
}

/// Represents a package-level variable with its initializer for dependency analysis.
struct VarInit {
    name: Symbol,
    value: gox_syntax::ast::Expr,
    global_idx: u32,
}

/// Collect variable references from an expression.
fn collect_var_refs(
    expr: &gox_syntax::ast::Expr,
    pkg_vars: &std::collections::HashSet<Symbol>,
    refs: &mut std::collections::HashSet<Symbol>,
) {
    use gox_syntax::ast::ExprKind;
    
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if pkg_vars.contains(&ident.symbol) {
                refs.insert(ident.symbol);
            }
        }
        ExprKind::Binary(bin) => {
            collect_var_refs(&bin.left, pkg_vars, refs);
            collect_var_refs(&bin.right, pkg_vars, refs);
        }
        ExprKind::Unary(un) => {
            collect_var_refs(&un.operand, pkg_vars, refs);
        }
        ExprKind::Call(call) => {
            collect_var_refs(&call.func, pkg_vars, refs);
            for arg in &call.args {
                collect_var_refs(arg, pkg_vars, refs);
            }
        }
        ExprKind::Index(idx) => {
            collect_var_refs(&idx.expr, pkg_vars, refs);
            collect_var_refs(&idx.index, pkg_vars, refs);
        }
        ExprKind::Selector(sel) => {
            collect_var_refs(&sel.expr, pkg_vars, refs);
        }
        ExprKind::Paren(inner) => {
            collect_var_refs(inner, pkg_vars, refs);
        }
        ExprKind::CompositeLit(comp) => {
            for elem in &comp.elems {
                if let Some(ref key) = &elem.key {
                    if let gox_syntax::ast::CompositeLitKey::Expr(key_expr) = key {
                        collect_var_refs(key_expr, pkg_vars, refs);
                    }
                }
                collect_var_refs(&elem.value, pkg_vars, refs);
            }
        }
        _ => {}
    }
}

/// Topological sort of variables based on dependencies.
/// Returns sorted variable indices or error if cycle detected.
/// Variables with no dependencies are initialized first.
fn topo_sort_vars(
    vars: &[VarInit],
    pkg_vars: &std::collections::HashSet<Symbol>,
) -> Result<Vec<usize>, CodegenError> {
    use std::collections::{HashMap, HashSet};
    
    // Build dependency graph: deps[var] = set of variables that var depends on
    let mut deps: HashMap<Symbol, HashSet<Symbol>> = HashMap::new();
    let name_to_idx: HashMap<Symbol, usize> = vars.iter()
        .enumerate()
        .map(|(i, v)| (v.name, i))
        .collect();
    
    for var in vars {
        let mut refs = HashSet::new();
        collect_var_refs(&var.value, pkg_vars, &mut refs);
        refs.remove(&var.name); // Remove self-reference
        deps.insert(var.name, refs);
    }
    
    // Build reverse graph: dependents[var] = set of variables that depend on var
    let mut dependents: HashMap<Symbol, HashSet<Symbol>> = vars.iter()
        .map(|v| (v.name, HashSet::new()))
        .collect();
    
    for (var, var_deps) in &deps {
        for dep in var_deps {
            if let Some(dep_set) = dependents.get_mut(dep) {
                dep_set.insert(*var);
            }
        }
    }
    
    // in_degree[var] = number of variables that var depends on (not yet initialized)
    let mut in_degree: HashMap<Symbol, usize> = deps.iter()
        .map(|(var, var_deps)| (*var, var_deps.len()))
        .collect();
    
    // Start with variables that have no dependencies
    let mut queue: Vec<Symbol> = in_degree.iter()
        .filter(|(_, &d)| d == 0)
        .map(|(&s, _)| s)
        .collect();
    
    let mut sorted = Vec::new();
    
    while let Some(var) = queue.pop() {
        sorted.push(name_to_idx[&var]);
        
        // For each variable that depends on this one, decrement its in_degree
        if let Some(var_dependents) = dependents.get(&var) {
            for dependent in var_dependents {
                if let Some(degree) = in_degree.get_mut(dependent) {
                    *degree -= 1;
                    if *degree == 0 {
                        queue.push(*dependent);
                    }
                }
            }
        }
    }
    
    if sorted.len() != vars.len() {
        return Err(CodegenError::Internal("initialization cycle detected".to_string()));
    }
    
    Ok(sorted)
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
    
    // Call each package's var init first, then all init() functions in dependency order
    for pkg in &project.packages {
        // Call $var_init if package has var declarations
        let var_init_name = format!("{}.$var_init", pkg.name);
        if let Some(&var_init_idx) = cross_pkg_funcs.get(&var_init_name) {
            func.code.push(Instruction::with_flags(Opcode::Call, 0, var_init_idx as u16, 0, 0));
        }
        
        // Call all init() functions in source order
        for i in 0..pkg.init_funcs.len() {
            let init_name = format!("{}.$init_{}", pkg.name, i);
            if let Some(&init_idx) = cross_pkg_funcs.get(&init_name) {
                func.code.push(Instruction::with_flags(Opcode::Call, 0, init_idx as u16, 0, 0));
            }
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
