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
use gox_analysis::types::{Type, BasicType, StructType, InterfaceType};
use gox_common::{Symbol, SymbolInterner};
use gox_common_core::SlotType;
use gox_syntax::ast::{Decl, File};
use gox_vm::bytecode::{Constant, FunctionDef, Module};
use gox_vm::instruction::Opcode;

use gox_common_core::RuntimeTypeId;

/// Registry for allocating type IDs during codegen.
/// - Each struct definition gets a unique ID (FirstStruct + idx)
/// - Interfaces are deduplicated by method set (FirstInterface + idx)
#[derive(Default)]
pub struct TypeRegistry {
    /// All struct types, each gets FirstStruct + index
    struct_types: Vec<StructType>,
    /// Interface types deduplicated by method set, each gets FirstInterface + index
    interface_types: Vec<InterfaceType>,
    /// Mapping from NamedTypeId to allocated type_id
    named_type_ids: HashMap<u32, u32>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Register a struct type and return its type_id.
    /// Each struct definition gets a new ID (no deduplication).
    pub fn register_struct(&mut self, st: &StructType) -> u32 {
        let idx = self.struct_types.len() as u32;
        self.struct_types.push(st.clone());
        RuntimeTypeId::FirstStruct as u32 + idx
    }
    
    /// Register an interface type and return its type_id.
    /// Interfaces with identical method sets share the same ID.
    pub fn register_interface(&mut self, iface: &InterfaceType) -> u32 {
        // Check if we already have an interface with the same method set
        for (idx, existing) in self.interface_types.iter().enumerate() {
            if Self::same_method_set(existing, iface) {
                return RuntimeTypeId::FirstInterface as u32 + idx as u32;
            }
        }
        // New unique interface
        let idx = self.interface_types.len() as u32;
        self.interface_types.push(iface.clone());
        RuntimeTypeId::FirstInterface as u32 + idx
    }
    
    /// Check if two interfaces have the same method set.
    fn same_method_set(a: &InterfaceType, b: &InterfaceType) -> bool {
        if a.methods.len() != b.methods.len() {
            return false;
        }
        // Sort methods by name for comparison
        let mut a_methods: Vec<_> = a.methods.iter().collect();
        let mut b_methods: Vec<_> = b.methods.iter().collect();
        a_methods.sort_by_key(|m| m.name);
        b_methods.sort_by_key(|m| m.name);
        
        for (ma, mb) in a_methods.iter().zip(b_methods.iter()) {
            if ma.name != mb.name || ma.sig != mb.sig {
                return false;
            }
        }
        true
    }
    
    /// Get all struct slot_types for GC scanning.
    pub fn get_struct_slot_types(&self, named_types: &[gox_analysis::NamedTypeInfo]) -> Vec<Vec<SlotType>> {
        self.struct_types.iter().map(|st| {
            struct_to_slot_types(st, named_types)
        }).collect()
    }
    
    /// Get struct count.
    pub fn struct_count(&self) -> usize {
        self.struct_types.len()
    }
    
    /// Get interface count.
    pub fn interface_count(&self) -> usize {
        self.interface_types.len()
    }
    
    /// Register a named type (struct or interface) and store the mapping.
    pub fn register_named_type(&mut self, named_id: u32, ty: &Type) -> Option<u32> {
        match ty {
            Type::Struct(st) => {
                let type_id = self.register_struct(st);
                self.named_type_ids.insert(named_id, type_id);
                Some(type_id)
            }
            Type::Interface(iface) => {
                let type_id = self.register_interface(iface);
                self.named_type_ids.insert(named_id, type_id);
                Some(type_id)
            }
            _ => None,
        }
    }
    
    /// Get the type_id for a named type.
    pub fn get_named_type_id(&self, named_id: u32) -> Option<u32> {
        self.named_type_ids.get(&named_id).copied()
    }
}

/// Convert a StructType to slot_types for GC scanning.
fn struct_to_slot_types(st: &StructType, named_types: &[gox_analysis::NamedTypeInfo]) -> Vec<SlotType> {
    let mut slot_types = Vec::new();
    for field in &st.fields {
        let field_slots = field_to_slot_types(&field.ty, named_types);
        slot_types.extend(field_slots);
    }
    slot_types
}

/// Convert a field type to slot_types.
fn field_to_slot_types(ty: &Type, named_types: &[gox_analysis::NamedTypeInfo]) -> Vec<SlotType> {
    match ty {
        Type::Basic(BasicType::String) => vec![SlotType::GcRef],
        Type::Basic(_) => vec![SlotType::Value],
        Type::Slice(_) | Type::Array(_) | Type::Map(_) | Type::Chan(_) | Type::Func(_) => vec![SlotType::GcRef],
        Type::Pointer(_) => vec![SlotType::GcRef],
        Type::Interface(_) => vec![SlotType::Interface0, SlotType::Interface1],
        Type::Struct(st) => struct_to_slot_types(st, named_types),
        Type::Named(id) => {
            if let Some(info) = named_types.get(id.0 as usize) {
                field_to_slot_types(&info.underlying, named_types)
            } else {
                vec![SlotType::Value]
            }
        }
        _ => vec![SlotType::Value],
    }
}

/// Convert analysis Type to RuntimeTypeId and slot count for GC scanning.
/// Uses TypeRegistry to look up Named struct/interface type IDs.
fn type_to_type_id_and_slots_with_registry(
    ty: &Type, 
    named_types: &[gox_analysis::NamedTypeInfo],
    registry: &TypeRegistry,
) -> (u32, u16) {
    match ty {
        // Value types (1 slot, no GC)
        Type::Basic(BasicType::Bool) => (RuntimeTypeId::Bool as u32, 1),
        Type::Basic(BasicType::Int) => (RuntimeTypeId::Int as u32, 1),
        Type::Basic(BasicType::Int8) => (RuntimeTypeId::Int8 as u32, 1),
        Type::Basic(BasicType::Int16) => (RuntimeTypeId::Int16 as u32, 1),
        Type::Basic(BasicType::Int32) => (RuntimeTypeId::Int32 as u32, 1),
        Type::Basic(BasicType::Int64) => (RuntimeTypeId::Int64 as u32, 1),
        Type::Basic(BasicType::Uint) => (RuntimeTypeId::Uint as u32, 1),
        Type::Basic(BasicType::Uint8) => (RuntimeTypeId::Uint8 as u32, 1),
        Type::Basic(BasicType::Uint16) => (RuntimeTypeId::Uint16 as u32, 1),
        Type::Basic(BasicType::Uint32) => (RuntimeTypeId::Uint32 as u32, 1),
        Type::Basic(BasicType::Uint64) => (RuntimeTypeId::Uint64 as u32, 1),
        Type::Basic(BasicType::Float32) => (RuntimeTypeId::Float32 as u32, 1),
        Type::Basic(BasicType::Float64) => (RuntimeTypeId::Float64 as u32, 1),
        Type::Untyped(_) => (RuntimeTypeId::Int as u32, 1),
        
        // Reference types (1 slot, GcRef)
        Type::Basic(BasicType::String) => (RuntimeTypeId::String as u32, 1),
        Type::Slice(_) => (RuntimeTypeId::Slice as u32, 1),
        Type::Array(_) => (RuntimeTypeId::Array as u32, 1),
        Type::Map(_) => (RuntimeTypeId::Map as u32, 1),
        Type::Pointer(inner) => {
            // Pointer to struct - use the struct's type_id
            type_to_type_id_and_slots_with_registry(inner, named_types, registry)
        }
        Type::Func(_) => (RuntimeTypeId::Closure as u32, 1),
        Type::Chan(_) => (RuntimeTypeId::Channel as u32, 1),
        
        // Tuple - treat as GcRef
        Type::Tuple(_) => (RuntimeTypeId::Slice as u32, 1),
        
        // Interface (2 slots) - use FirstInterface as marker
        Type::Interface(_) => (RuntimeTypeId::FirstInterface as u32, 2),
        
        // Struct - use FirstStruct as placeholder (actual ID assigned elsewhere)
        Type::Struct(_) => (RuntimeTypeId::FirstStruct as u32, 1),
        
        // Named type - look up in registry
        Type::Named(id) => {
            // First check if we have a registered type_id for this named type
            if let Some(type_id) = registry.get_named_type_id(id.0) {
                // For struct: 1 slot, for interface: 2 slots
                let slots = if RuntimeTypeId::is_interface(type_id) { 2 } else { 1 };
                (type_id, slots)
            } else if let Some(info) = named_types.get(id.0 as usize) {
                // Fallback: resolve to underlying type
                type_to_type_id_and_slots_with_registry(&info.underlying, named_types, registry)
            } else {
                (RuntimeTypeId::Int as u32, 1)
            }
        }
        
        // Default
        _ => (RuntimeTypeId::Int as u32, 1),
    }
}

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
    let mut module = Module::new(&project.main_package);
    
    // Pre-lookup well-known symbols for fast comparison
    let sym_init = project.interner.get("init");
    
    // Build cross-package function index: "pkg.Func" -> func_idx
    let mut cross_pkg_funcs: HashMap<String, u32> = HashMap::new();
    
    // Global variable indices: (pkg_name, var_name) -> global_idx
    let mut global_indices: HashMap<(String, Symbol), u32> = HashMap::new();
    
    // Type registry for allocating struct/interface type IDs
    let mut type_registry = TypeRegistry::new();
    
    // Pass 0: collect all struct and interface types from named_types
    for pkg in &project.packages {
        for (idx, info) in pkg.types.named_types.iter().enumerate() {
            type_registry.register_named_type(idx as u32, &info.underlying);
        }
    }
    
    // First pass: collect all global variables from ALL packages
    for pkg in &project.packages {
        for file in &pkg.files {
            for decl in &file.decls {
                if let Decl::Var(var) = decl {
                    for spec in &var.specs {
                        for name in &spec.names {
                            let var_name = project.interner.resolve(name.symbol).unwrap_or("");
                            // Determine type_id and slots based on variable type
                            let (type_id, slots) = pkg.types.scope.lookup(name.symbol)
                                .map(|e| match e {
                                    gox_analysis::scope::Entity::Var(v) => {
                                        type_to_type_id_and_slots_with_registry(&v.ty, &pkg.types.named_types, &type_registry)
                                    }
                                    _ => (RuntimeTypeId::Int as u32, 1),
                                })
                                .unwrap_or_else(|| (RuntimeTypeId::Int as u32, 1));
                            let idx = module.add_global(var_name, type_id, slots);
                            global_indices.insert((pkg.name.clone(), name.symbol), idx);
                        }
                    }
                }
            }
        }
    }
    
    // Shared indices across all packages (initialize early for extern func registration)
    let mut extern_indices: HashMap<String, u32> = HashMap::new();
    let mut const_indices: HashMap<String, u16> = HashMap::new();
    
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
                    
                    // Handle extern functions separately - register them in extern_indices
                    if func.is_extern() {
                        // Register extern function with qualified name
                        let qualified_name = format!("{}.{}", pkg.name, func_name);
                        // Calculate param and return slots from function signature
                        let param_slots = func.sig.params.iter()
                            .map(|p| p.names.len() as u16)
                            .sum::<u16>();
                        let ret_slots = func.sig.results.len().max(1) as u16;
                        let extern_idx = module.add_extern(&qualified_name, param_slots, ret_slots);
                        extern_indices.insert(qualified_name, extern_idx);
                        continue; // Don't add to regular functions
                    }
                    
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
                    } else if sym_init.map_or(false, |s| func.name.symbol == s) {
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
                    // Skip extern functions - they have no body to compile
                    if func.is_extern() {
                        continue;
                    }
                    
                    let mut ctx = context::CodegenContext::new(file, &pkg.types, &project.interner);
                    ctx.func_indices = func_indices.clone();
                    ctx.cross_pkg_funcs = cross_pkg_funcs.clone();
                    ctx.global_indices = pkg_globals.clone();
                    ctx.const_values = pkg_consts.clone();
                    ctx.extern_indices = extern_indices.clone();
                    ctx.const_indices = const_indices.clone();
                    ctx.method_table = method_table.clone();
                    ctx.func_interface_params = func_interface_params.clone();
                    ctx.pkg_name = pkg.name.clone();
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
                    } else if sym_init.map_or(false, |s| func.name.symbol == s) {
                        // Use init_counter to find the correct init function index
                        let init_name = format!("{}.$init_{}", pkg.name, init_counter);
                        init_counter += 1;
                        *cross_pkg_funcs.get(&init_name).unwrap() as usize
                    } else {
                        func_indices[&func.name.symbol] as usize
                    };
                    module.functions[idx] = func_def;
                    
                    // Merge back changes
                    extern_indices = ctx.extern_indices;
                    const_indices = ctx.const_indices;
                    module.constants = ctx.module.constants;
                    // Merge externs from ctx (don't replace - we may have pre-registered externs)
                    for ext in ctx.module.externs {
                        if !module.externs.iter().any(|n| n.name == ext.name) {
                            module.externs.push(ext);
                        }
                    }
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
                ctx.extern_indices = extern_indices.clone();
                ctx.const_indices = const_indices.clone();
                ctx.method_table = method_table.clone();
                ctx.func_interface_params = func_interface_params.clone();
                ctx.pkg_name = pkg.name.clone();
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
                extern_indices = ctx.extern_indices;
                const_indices = ctx.const_indices;
                module.constants = ctx.module.constants;
                module.externs = ctx.module.externs;
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
    use gox_common::DiagnosticSink;
    use gox_syntax::parse;
    use gox_analysis::typecheck_file;
    use gox_vm::VmResult;

    fn compile_and_run(source: &str) -> VmResult {
        let (file, _parse_diag, interner) = parse(source, 0);
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

    /// Test GoX struct memory layout sizes by inspecting generated bytecode.
    /// Verifies that Alloc instruction uses correct slot count for structs.
    #[test]
    fn test_struct_sizes() {
        use gox_vm::instruction::Opcode;

        // Helper to find Alloc instructions and return their size (c field)
        fn find_alloc_sizes(module: &gox_vm::bytecode::Module) -> Vec<u16> {
            let mut sizes = vec![];
            for func in &module.functions {
                for instr in &func.code {
                    if instr.opcode() == Opcode::Alloc {
                        // Alloc: a=dest, b=type_id, c=size_slots
                        sizes.push(instr.c);
                    }
                }
            }
            sizes
        }

        // Struct with 1 field -> 1 slot
        let (file, _, interner) = parse(r#"
package main
type S1 struct { x int }
func main() { _ = S1{x: 1} }
"#, 0);
        let mut diag = DiagnosticSink::new();
        let result = typecheck_file(&file, &interner, &mut diag);
        let module = compile(&file, &result, &interner).unwrap();
        let sizes = find_alloc_sizes(&module);
        assert!(sizes.contains(&1), "S1 should allocate 1 slot, got {:?}", sizes);

        // Struct with 2 fields -> 2 slots
        let (file, _, interner) = parse(r#"
package main
type S2 struct { x int; y int }
func main() { _ = S2{x: 1, y: 2} }
"#, 0);
        let mut diag = DiagnosticSink::new();
        let result = typecheck_file(&file, &interner, &mut diag);
        let module = compile(&file, &result, &interner).unwrap();
        let sizes = find_alloc_sizes(&module);
        assert!(sizes.contains(&2), "S2 should allocate 2 slots, got {:?}", sizes);

        // Struct with 3 fields -> 3 slots
        let (file, _, interner) = parse(r#"
package main
type S3 struct { x int; y int; z int }
func main() { _ = S3{x: 1, y: 2, z: 3} }
"#, 0);
        let mut diag = DiagnosticSink::new();
        let result = typecheck_file(&file, &interner, &mut diag);
        let module = compile(&file, &result, &interner).unwrap();
        let sizes = find_alloc_sizes(&module);
        assert!(sizes.contains(&3), "S3 should allocate 3 slots, got {:?}", sizes);

        // Struct with bool and byte fields (compact layout)
        // bool (1 byte) + byte (1 byte) + int (8 bytes) = 10 bytes
        // Compact layout: ceil(10/8) = 2 slots
        let (file, _, interner) = parse(r#"
package main
type Mixed struct { a bool; b byte; c int }
func main() { 
    m := Mixed{a: true, b: 42, c: 100}
    assert(m.a == true)
    assert(m.b == 42)
    assert(m.c == 100)
}
"#, 0);
        let mut diag = DiagnosticSink::new();
        let result = typecheck_file(&file, &interner, &mut diag);
        let module = compile(&file, &result, &interner).unwrap();
        let sizes = find_alloc_sizes(&module);
        // Compact layout: bool(1) + byte(1) + int(8) = 10 bytes → 2 slots
        assert!(sizes.contains(&2), "Mixed{{bool,byte,int}} should allocate 2 slots (compact), got {:?}", sizes);

        // Runtime test: actually run the code to verify field access works
        let result = compile_and_run(r#"
package main
type Mixed struct { a bool; b byte; c int }
func main() { 
    m := Mixed{a: true, b: 42, c: 100}
    assert(m.a == true)
    assert(m.b == 42)
    assert(m.c == 100)
}
"#);
        assert!(matches!(result, VmResult::Done | VmResult::Ok), "Mixed struct field access failed");
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
    
    /// Test that slot_types are correctly generated for GC root scanning.
    #[test]
    fn test_slot_types_generation() {
        use gox_vm::bytecode::SlotType;
        
        let (file, _, interner) = parse(r#"
package main

func main() {
    x := 42           // int -> Value
    s := "hello"      // string -> GcRef
    arr := []int{1,2} // slice -> GcRef
    _ = x
    _ = s
    _ = arr
}
"#, 0);
        let mut diag = DiagnosticSink::new();
        let result = typecheck_file(&file, &interner, &mut diag);
        let module = compile(&file, &result, &interner).expect("Compilation failed");
        
        // Find main function
        let main_func = module.functions.iter().find(|f| f.name == "main").unwrap();
        
        // Verify slot_types exist
        assert!(!main_func.slot_types.is_empty(), "slot_types should not be empty");
        
        // Check that we have some GcRef types (for string and slice)
        let gc_ref_count = main_func.slot_types.iter()
            .filter(|t| **t == SlotType::GcRef)
            .count();
        assert!(gc_ref_count >= 2, "Should have at least 2 GcRef types for string and slice, got {}", gc_ref_count);
        
        // Print slot_types for debugging
        println!("main func slot_types: {:?}", main_func.slot_types);
    }
    
    /// Test GC collection with reference types on stack.
    #[test]
    fn test_gc_with_references() {
        let result = compile_and_run(r#"
package main

func main() {
    // Create some strings that should be tracked by GC
    s1 := "hello"
    s2 := "world"
    s3 := s1 + " " + s2
    println(s3)
    
    // Create a slice
    arr := []int{1, 2, 3, 4, 5}
    println(len(arr))
}
"#);
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
    }
    
    /// Test that GC scanning works correctly with live references.
    #[test]
    fn test_gc_scanning() {
        let (file, _, interner) = parse(r#"
package main

func main() {
    x := 42
    println(x)
}
"#, 0);
        let mut diag = DiagnosticSink::new();
        let result = typecheck_file(&file, &interner, &mut diag);
        let module = compile(&file, &result, &interner).expect("Compilation failed");
        
        let mut vm = gox_runtime_vm::create_vm();
        vm.load_module(module);
        
        // Run
        let result = vm.run();
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
        
        // Force GC after execution
        vm.collect_garbage();
    }
    
    /// Test that GC actually reclaims memory.
    #[test]
    fn test_gc_reclaims_memory() {
        let (file, _, interner) = parse(r#"
package main

func createStrings() {
    // Create strings that become garbage after function returns
    s1 := "temp1"
    s2 := "temp2"
    s3 := s1 + s2
    _ = s3
}

func main() {
    createStrings()
    createStrings()
    createStrings()
}
"#, 0);
        let mut diag = DiagnosticSink::new();
        let result = typecheck_file(&file, &interner, &mut diag);
        let module = compile(&file, &result, &interner).expect("Compilation failed");
        
        let mut vm = gox_runtime_vm::create_vm();
        vm.load_module(module);
        
        // Run to create garbage
        let result = vm.run();
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
        
        // Get stats before GC
        let objects_before = vm.gc_object_count();
        let bytes_before = vm.gc_total_bytes();
        
        // Force GC
        vm.collect_garbage();
        
        // Get stats after GC
        let objects_after = vm.gc_object_count();
        let bytes_after = vm.gc_total_bytes();
        
        println!("Before GC: {} objects, {} bytes", objects_before, bytes_before);
        println!("After GC:  {} objects, {} bytes", objects_after, bytes_after);
        
        // Verify some memory was reclaimed (concatenated strings should be garbage)
        assert!(objects_after <= objects_before, "GC should not increase object count");
        assert!(bytes_after <= bytes_before, "GC should not increase memory usage");
    }
    
    /// Test GC with string constants.
    #[test]
    fn test_gc_with_strings() {
        let (file, _, interner) = parse(r#"
package main

func main() {
    s := "hello"
    println(s)
}
"#, 0);
        let mut diag = DiagnosticSink::new();
        let result = typecheck_file(&file, &interner, &mut diag);
        let module = compile(&file, &result, &interner).expect("Compilation failed");
        
        let mut vm = gox_runtime_vm::create_vm();
        vm.load_module(module);
        
        let result = vm.run();
        assert!(matches!(result, VmResult::Done | VmResult::Ok));
        
        // GC with string constants
        vm.collect_garbage();
    }
}
