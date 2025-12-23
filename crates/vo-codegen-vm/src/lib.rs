//! Vo VM bytecode code generator.
//!
//! This crate compiles type-checked Vo AST to VM bytecode.

mod context;
mod error;
mod expr;
mod func;
mod stmt;
mod type_info;

pub use context::CodegenContext;
pub use error::{CodegenError, Result};
pub use func::FuncBuilder;
pub use type_info::TypeInfo;

use vo_analysis::Project;
use vo_common_core::SlotType;
use vo_syntax::ast::{Decl, File, FuncDecl};
use vo_vm::bytecode::Module;
use vo_vm::instruction::Opcode;

use crate::expr::compile_expr;
use crate::stmt::compile_stmt;


/// Compile a project to a Module.
pub fn compile_project(project: &Project) -> Result<Module> {
    let pkg_name = project.main_pkg().name().as_deref().unwrap_or("main");
    let mut ctx = CodegenContext::new(pkg_name);

    // Create a TypeQuery for the main package
    let query = project.query();
    
    // Use expr_types, type_expr_types, and selections from Project
    let info = TypeInfo::new(query, project.expr_types(), project.type_expr_types(), project.selections());

    // Register all struct and interface types and generate TypeMeta
    register_types(project, &mut ctx, &info);

    // Collect declarations from all files
    for file in &project.files {
        collect_declarations(file, &info, &mut ctx)?;
    }

    // Compile functions from all files
    for file in &project.files {
        compile_functions(file, &info, &mut ctx)?;
    }

    // Generate init and entry
    compile_init_and_entry(project, &info, &mut ctx)?;

    Ok(ctx.finish())
}

/// Register all struct and interface types to allocate unique type IDs,
/// and generate TypeMeta for each type.
fn register_types(project: &Project, ctx: &mut CodegenContext, info: &TypeInfo) {
    use vo_analysis::Type;
    use vo_vm::types::TypeMeta;
    use vo_common_core::ValueKind;
    
    let query = project.query();
    
    // First pass: register all types to allocate IDs
    let mut struct_types: Vec<(vo_analysis::TypeKey, &Type)> = Vec::new();
    let mut interface_types: Vec<(vo_analysis::TypeKey, &Type)> = Vec::new();
    
    for (type_key, ty) in query.iter_types() {
        match ty {
            Type::Struct(_) => {
                ctx.register_struct_type(type_key);
                struct_types.push((type_key, ty));
            }
            Type::Interface(_) => {
                ctx.register_interface_type(type_key);
                interface_types.push((type_key, ty));
            }
            Type::Named(n) => {
                if let Some(underlying_key) = n.try_underlying() {
                    let underlying = query.get_type(underlying_key);
                    match underlying {
                        Type::Struct(_) => {
                            ctx.register_struct_type(type_key);
                            struct_types.push((type_key, underlying));
                        }
                        Type::Interface(_) => {
                            ctx.register_interface_type(type_key);
                            interface_types.push((type_key, underlying));
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    
    // Second pass: generate TypeMeta for structs, sorted by type_id
    struct_types.sort_by_key(|(tk, _)| ctx.get_struct_type_id(*tk)
        .expect("struct type should be registered"));
    
    for (type_key, ty) in struct_types {
        let type_id = ctx.get_struct_type_id(type_key)
            .expect("struct type should be registered");
        let size_slots = info.struct_size_slots(ty) as usize;
        let slot_types = info.struct_field_slot_types(ty);
        
        let meta = TypeMeta {
            value_kind: ValueKind::Struct,
            type_id,
            size_slots,
            size_bytes: size_slots * 8,
            slot_types,
            name: String::new(),
            field_layouts: Vec::new(),
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        };
        ctx.add_struct_type(meta);
    }
    
    // Third pass: generate TypeMeta for interfaces, sorted by type_id
    interface_types.sort_by_key(|(tk, _)| ctx.get_interface_type_id(*tk)
        .expect("interface type should be registered"));
    
    for (type_key, _ty) in interface_types {
        let type_id = ctx.get_interface_type_id(type_key)
            .expect("interface type should be registered");
        // Interface is 2 slots: (metadata, data)
        let meta = TypeMeta {
            value_kind: ValueKind::Interface,
            type_id,
            size_slots: 2,
            size_bytes: 16,
            slot_types: vec![SlotType::Interface0, SlotType::Interface1],
            name: String::new(),
            field_layouts: Vec::new(),
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        };
        ctx.add_interface_type(meta);
    }
}

fn compile_init_and_entry(
    project: &Project,
    info: &TypeInfo,
    ctx: &mut CodegenContext,
) -> Result<()> {
    let mut init_builder = FuncBuilder::new("__init__");

    // Compile global var initializers in dependency order from init_order
    for initializer in &project.type_info.init_order {
        // Compile the RHS expression
        let src = compile_expr(&initializer.rhs, ctx, &mut init_builder, info)?;
        
        // Set each LHS variable
        for &okey in &initializer.lhs {
            // Get the variable name from ObjKey
            let var_name = project.tc_objs.lobjs[okey].name();
            let var_sym = project.interner.get(var_name).expect("var name should be interned");
            if let Some(idx) = ctx.get_global_index(var_sym) {
                init_builder.emit_op(Opcode::SetGlobal, idx as u16, src, 0);
            }
        }
    }

    init_builder.emit_op(Opcode::Return, 0, 0, 0);
    let init_def = init_builder.build();
    ctx.add_function(init_def);

    let mut entry_builder = FuncBuilder::new("__entry__");

    // 1. Call __init__ (variable initialization)
    let init_idx = ctx.func_count() as u16 - 1;
    entry_builder.emit_op(Opcode::Call, init_idx, 0, 0);

    // 2. Call all init() functions in order
    for &init_func_idx in ctx.get_init_functions() {
        entry_builder.emit_op(Opcode::Call, init_func_idx as u16, 0, 0);
    }

    // 3. Call main()
    if let Some(main_idx) = ctx.find_function("main") {
        entry_builder.emit_with_flags(Opcode::Call, 0, main_idx as u16, 0, 0);
    }

    entry_builder.emit_op(Opcode::Return, 0, 0, 0);
    let entry_def = entry_builder.build();
    let entry_idx = ctx.add_function(entry_def);
    ctx.set_entry_func(entry_idx);

    Ok(())
}

/// Compile a single file to a Module using a Project.
pub fn compile_single_file(project: &Project) -> Result<Module> {
    let file = project.files.first().expect("project must have at least one file");
    let query = project.query();
    let mut ctx = CodegenContext::new("main");
    let info = TypeInfo::new(query, project.expr_types(), project.type_expr_types(), project.selections());

    collect_declarations(file, &info, &mut ctx)?;
    compile_functions(file, &info, &mut ctx)?;
    compile_init_and_entry(project, &info, &mut ctx)?;

    Ok(ctx.finish())
}

fn collect_declarations(
    file: &File,
    info: &TypeInfo,
    ctx: &mut CodegenContext,
) -> Result<()> {
    for decl in &file.decls {
        match decl {
            Decl::Func(func) => {
                if func.body.is_some() {
                    // Get receiver type for methods
                    let recv_type_key = func.receiver.as_ref().and_then(|recv| {
                        info.lookup_type_key(recv.ty.symbol)
                    });
                    let func_idx = ctx.register_method(recv_type_key, func.name.symbol);
                    
                    // Track init() functions (no receiver, name is "init")
                    let func_name = info.symbol_str(func.name.symbol);
                    if func_name == "init" && recv_type_key.is_none() {
                        ctx.register_init_function(func_idx);
                    }
                    
                    // Register ObjKey -> func_idx mapping for interface dispatch
                    // For methods, use lookup_concrete_method; for functions, use lookup_symbol_objkey
                    let objkey = if let Some(recv_key) = recv_type_key {
                        info.query.lookup_concrete_method(recv_key, func_name)
                    } else {
                        info.lookup_symbol_objkey(func.name.symbol)
                    };
                    if let Some(okey) = objkey {
                        ctx.register_objkey_func(okey, func_idx);
                    }
                } else {
                    let name = info.symbol_str(func.name.symbol);
                    let param_slots: u16 = func.sig.params.iter()
                        .map(|p| p.names.len().max(1) as u16)
                        .sum();
                    let ret_slots = func.sig.results.len() as u16;
                    ctx.register_extern(func.name.symbol, name, param_slots, ret_slots);
                }
            }
            Decl::Var(var) => {
                for spec in &var.specs {
                    for name in &spec.names {
                        let name_str = info.symbol_str(name.symbol);
                        // Without type info, use default slot count
                        // TODO: Get actual type from expr_types when available
                        let value_kind = vo_common_core::ValueKind::Nil as u8;
                        let type_id = 0u16;
                        let slots = 1;
                        ctx.register_global(name.symbol, name_str, value_kind, type_id, slots);
                    }
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn compile_functions(
    file: &File,
    info: &TypeInfo,
    ctx: &mut CodegenContext,
) -> Result<()> {
    for decl in &file.decls {
        if let Decl::Func(func) = decl {
            if func.body.is_some() {
                compile_func_decl(func, info, ctx)?;
            }
        }
    }
    Ok(())
}

fn compile_func_decl(
    func: &FuncDecl,
    info: &TypeInfo,
    ctx: &mut CodegenContext,
) -> Result<()> {
    let name = info.symbol_str(func.name.symbol);
    let mut builder = FuncBuilder::new(name);

    // Define receiver parameter first (for methods)
    // In VM, all structs are heap-allocated, so receiver is always 1 slot (GcRef)
    // Value receiver vs pointer receiver difference is handled at call site (deep copy for value)
    if let Some(recv) = &func.receiver {
        builder.define_param(recv.name.symbol, 1, &[SlotType::GcRef]);
    }

    // Define parameters with proper type handling
    // In VM, structs are heap-allocated (1 slot GcRef), other types use natural slots
    for param in &func.sig.params {
        let param_type = info.resolve_type_expr(&param.ty)
            .expect("param type must be recorded by analysis");
        
        for pname in &param.names {
            if info.is_interface(param_type) {
                // Interface parameter: 2 slots + InitInterface
                let type_key = info.type_expr_type_key(&param.ty).expect("interface type must have TypeKey");
                let iface_type_id = ctx.type_id_for_interface(type_key);
                builder.define_param_interface(pname.symbol, iface_type_id as u32);
            } else if info.is_struct_type(param_type) {
                // Struct parameter: 1 slot (GcRef) - heap allocated
                builder.define_param(pname.symbol, 1, &[SlotType::GcRef]);
            } else {
                // Other types: use natural slot count
                let slot_types = info.type_slot_types(param_type);
                let slots = slot_types.len() as u16;
                builder.define_param(pname.symbol, slots, &slot_types);
            }
        }
    }

    builder.ret_slots = func.sig.results.len() as u16;

    if let Some(body) = &func.body {
        for stmt in &body.stmts {
            compile_stmt(stmt, ctx, &mut builder, info)?;
        }
    }

    if builder.code.is_empty() || builder.code.last().map(|i| i.opcode()) != Some(Opcode::Return) {
        builder.emit_op(Opcode::Return, 0, 0, 0);
    }

    let func_def = builder.build();
    ctx.add_function(func_def);

    Ok(())
}

