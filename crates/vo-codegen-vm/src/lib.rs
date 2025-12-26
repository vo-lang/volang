//! Vo bytecode code generation.
//!
//! This crate compiles type-checked AST to VM bytecode.

mod context;
mod error;
mod expr;
mod func;
mod stmt;
mod type_info;

pub use context::CodegenContext;
pub use error::CodegenError;
pub use func::FuncBuilder;
pub use type_info::TypeInfoWrapper;

use vo_analysis::Project;
use vo_syntax::ast::Decl;
use vo_vm::bytecode::Module;

/// Compile a type-checked project to VM bytecode.
pub fn compile_project(project: &Project) -> Result<Module, CodegenError> {
    let info = TypeInfoWrapper::new(project);
    let pkg_name = "main"; // TODO: get from project
    let mut ctx = CodegenContext::new(pkg_name);
    
    // 1. Register types (StructMeta, InterfaceMeta)
    register_types(project, &mut ctx, &info)?;
    
    // 2. Collect declarations (functions, globals, externs)
    collect_declarations(project, &mut ctx, &info)?;
    
    // 3. Compile functions
    compile_functions(project, &mut ctx, &info)?;
    
    // 4. Generate __init__ and __entry__
    compile_init_and_entry(project, &mut ctx, &info)?;
    
    Ok(ctx.finish())
}

fn register_types(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::{Decl, TypeExprKind};
    use vo_vm::bytecode::{StructMeta, InterfaceMeta, NamedTypeMeta};
    use vo_common_core::types::ValueMeta;
    use std::collections::HashMap;
    
    // Iterate all type declarations
    for file in &project.files {
        for decl in &file.decls {
            if let Decl::Type(type_decl) = decl {
                let type_name = project.interner.resolve(type_decl.name.symbol)
                    .unwrap_or("?");
                
                // Get underlying type key from type expression, and named type key from declaration name
                let underlying_type_key = info.type_expr_type(type_decl.ty.id);
                let named_type_key = info.get_def(&type_decl.name).and_then(|obj| info.obj_type(obj));
                
                if let (Some(underlying_key), Some(named_key)) = (underlying_type_key, named_type_key) {
                    match &type_decl.ty.kind {
                        TypeExprKind::Struct(struct_type) => {
                            // Build StructMeta - keyed by underlying type
                            let mut field_names = Vec::new();
                            let mut field_offsets = Vec::new();
                            let mut slot_types = Vec::new();
                            let mut offset = 0u16;
                            
                            for field in &struct_type.fields {
                                // Field may have multiple names sharing same type
                                for name in &field.names {
                                    let field_name = project.interner.resolve(name.symbol)
                                        .unwrap_or("?");
                                    field_names.push(field_name.to_string());
                                    field_offsets.push(offset);
                                    
                                    // Get field type and slot count
                                    if let Some(field_type) = info.type_expr_type(field.ty.id) {
                                        let slots = info.type_slot_count(field_type);
                                        let slot_type_list = info.type_slot_types(field_type);
                                        slot_types.extend(slot_type_list);
                                        offset += slots;
                                    }
                                }
                            }
                            
                            let meta = StructMeta {
                                field_names,
                                field_offsets,
                                slot_types: slot_types.clone(),
                            };
                            // struct_meta uses underlying type key
                            let struct_meta_id = ctx.register_struct_meta(underlying_key, meta);
                            
                            // NamedTypeMeta - keyed by named type
                            let underlying_meta = ValueMeta::new(struct_meta_id as u32, vo_common_core::types::ValueKind::Struct);
                            let named_type_meta = NamedTypeMeta {
                                name: type_name.to_string(),
                                underlying_meta,
                                methods: HashMap::new(),
                            };
                            // named_type_meta uses named type key
                            ctx.register_named_type_meta(named_key, named_type_meta);
                        }
                        TypeExprKind::Interface(iface_type) => {
                            // Build InterfaceMeta - keyed by underlying type
                            let mut method_names = Vec::new();
                            for elem in &iface_type.elems {
                                if let vo_syntax::ast::InterfaceElem::Method(method) = elem {
                                    let method_name = project.interner.resolve(method.name.symbol)
                                        .unwrap_or("?");
                                    method_names.push(method_name.to_string());
                                }
                            }
                            
                            let meta = InterfaceMeta {
                                name: type_name.to_string(),
                                method_names,
                            };
                            // interface_meta uses underlying type key
                            ctx.register_interface_meta(underlying_key, meta);
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    Ok(())
}

fn collect_declarations(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Register all function names first (so calls can find them)
    for file in &project.files {
        for decl in &file.decls {
            match decl {
                Decl::Func(func_decl) => {
                    // Check if this is a method (has receiver)
                    let (recv_type, is_pointer_recv) = if let Some(recv) = &func_decl.receiver {
                        let base_type = info.get_use(&recv.ty).or_else(|| info.get_def(&recv.ty)).and_then(|obj| info.obj_type(obj));
                        (base_type, recv.is_pointer)
                    } else {
                        (None, false)
                    };
                    
                    ctx.declare_func(recv_type, is_pointer_recv, func_decl.name.symbol);
                }
                Decl::Var(var_decl) => {
                    // Register global variables (so functions can reference them)
                    for spec in &var_decl.specs {
                        for (i, name) in spec.names.iter().enumerate() {
                            let type_key = if let Some(ty) = &spec.ty {
                                info.project.type_info.type_exprs.get(&ty.id).copied()
                            } else if i < spec.values.len() {
                                info.expr_type(spec.values[i].id)
                            } else {
                                None
                            };
                            
                            let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                            let value_kind = type_key.map(|t| info.type_value_kind(t) as u8).unwrap_or(0);
                            ctx.register_global(
                                name.symbol,
                                vo_vm::bytecode::GlobalDef {
                                    name: project.interner.resolve(name.symbol).unwrap_or("?").to_string(),
                                    slots,
                                    value_kind,
                                    meta_id: 0,
                                },
                            );
                        }
                    }
                }
                _ => {}
            }
        }
    }
    Ok(())
}

fn compile_functions(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Collect method info for NamedTypeMeta.methods update
    let mut method_mappings: Vec<(vo_analysis::objects::TypeKey, String, u32, bool)> = Vec::new();
    
    // Iterate all files and compile function declarations
    for file in &project.files {
        for decl in &file.decls {
            if let Decl::Func(func_decl) = decl {
                let func_name = project.interner.resolve(func_decl.name.symbol)
                    .unwrap_or("unknown");
                let func_id = compile_func_decl(func_decl, ctx, info)?;
                
                // Check if this is an init() function (no receiver, name is "init")
                if func_decl.receiver.is_none() && func_name == "init" {
                    ctx.register_init_function(func_id);
                }
                
                // If this is a method, record the mapping
                if let Some(recv) = &func_decl.receiver {
                    // recv.ty is the struct type (e.g., MyNum), recv.is_pointer indicates if it's *T
                    if let Some(recv_type) = info.get_use(&recv.ty).or_else(|| info.get_def(&recv.ty)).and_then(|obj| info.obj_type(obj)) {
                        let method_name = project.interner.resolve(func_decl.name.symbol)
                            .unwrap_or("?").to_string();
                        method_mappings.push((recv_type, method_name, func_id, recv.is_pointer));
                    }
                }
            }
        }
    }
    
    // Update NamedTypeMeta.methods with method func_ids
    update_named_type_methods(ctx, &method_mappings);
    
    // Build pending itabs now that methods are registered
    ctx.finalize_itabs();
    
    Ok(())
}

fn update_named_type_methods(
    ctx: &mut CodegenContext,
    method_mappings: &[(vo_analysis::objects::TypeKey, String, u32, bool)],
) {
    // Group methods by type_key
    for (type_key, method_name, func_id, is_pointer_receiver) in method_mappings {
        if let Some(named_type_id) = ctx.get_named_type_id(*type_key) {
            ctx.update_named_type_method(named_type_id, method_name.clone(), *func_id, *is_pointer_receiver);
        }
    }
}

fn compile_func_decl(
    func_decl: &vo_syntax::ast::FuncDecl,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<u32, CodegenError> {
    let name = info.project.interner.resolve(func_decl.name.symbol)
        .unwrap_or("unknown");
    
    let mut builder = FuncBuilder::new(name);
    
    // Define receiver as first parameter (if method)
    if let Some(recv) = &func_decl.receiver {
        // For pointer receiver (*T), it's 1 slot (GcRef)
        // For value receiver (T), it's the struct's slot count
        let (slots, slot_types) = if recv.is_pointer {
            (1, vec![vo_common_core::types::SlotType::GcRef])
        } else {
            let type_key = info.get_use(&recv.ty).or_else(|| info.get_def(&recv.ty)).and_then(|obj| info.obj_type(obj));
            let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
            let slot_types = type_key
                .map(|t| info.type_slot_types(t))
                .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);
            (slots, slot_types)
        };
        
        // Receiver name
        builder.define_param(recv.name.symbol, slots, &slot_types);
    }
    
    // Define parameters
    for param in &func_decl.sig.params {
        let (slots, slot_types) = info.type_expr_layout(param.ty.id);
        for name in &param.names {
            builder.define_param(name.symbol, slots, &slot_types);
        }
    }
    
    // Set return slots
    let ret_slots: u16 = func_decl.sig.results.iter()
        .map(|r| info.type_expr_layout(r.ty.id).0)
        .sum();
    builder.set_ret_slots(ret_slots);
    
    // Compile function body
    if let Some(body) = &func_decl.body {
        stmt::compile_block(body, ctx, &mut builder, info)?;
    }
    
    // Add return if not present at end
    builder.emit_op(vo_vm::instruction::Opcode::Return, 0, 0, 0);
    
    // Build and add to module with proper name registration
    let func_def = builder.build();
    // Get receiver base type and is_pointer flag
    let (recv_base_type, is_pointer_recv) = func_decl.receiver.as_ref()
        .map(|recv| {
            let base_type = info.get_use(&recv.ty).or_else(|| info.get_def(&recv.ty)).and_then(|obj| info.obj_type(obj));
            (base_type, recv.is_pointer)
        })
        .unwrap_or((None, false));
    let func_id = ctx.define_func(func_def, recv_base_type, is_pointer_recv, func_decl.name.symbol);
    
    Ok(func_id)
}

fn compile_init_and_entry(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // 1. Generate __init__ function for global variable initialization
    let mut init_builder = FuncBuilder::new("__init__");
    
    // Initialize global variables in dependency order (from type checker analysis)
    for initializer in &info.project.type_info.init_order {
        // Each initializer has lhs (variables) and rhs (expression)
        // For now, handle single variable assignment (most common case)
        if initializer.lhs.len() == 1 {
            let obj_key = initializer.lhs[0];
            let obj = &info.project.tc_objs.lobjs[obj_key];
            let var_name = obj.name();
            let var_symbol = project.interner.get(var_name);
            
            if let Some(symbol) = var_symbol {
                if let Some(global_idx) = ctx.get_global_index(symbol) {
                    let type_key = obj.typ();
                    let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                    let slot_types = type_key
                        .map(|t| info.type_slot_types(t))
                        .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);
                    
                    let tmp = init_builder.alloc_temp_typed(&slot_types);
                    crate::expr::compile_expr_to(&initializer.rhs, tmp, ctx, &mut init_builder, info)?;
                    if slots == 1 {
                        init_builder.emit_op(vo_vm::instruction::Opcode::GlobalSet, global_idx as u16, tmp, 0);
                    } else {
                        init_builder.emit_with_flags(
                            vo_vm::instruction::Opcode::GlobalSetN,
                            slots as u8,
                            global_idx as u16,
                            tmp,
                            0,
                        );
                    }
                }
            }
        } else {
            // Multi-variable assignment: var a, b = expr
            // TODO: handle tuple unpacking if needed
            for (i, &obj_key) in initializer.lhs.iter().enumerate() {
                let obj = &info.project.tc_objs.lobjs[obj_key];
                let var_name = obj.name();
                let var_symbol = project.interner.get(var_name);
                
                if let Some(symbol) = var_symbol {
                    if let Some(global_idx) = ctx.get_global_index(symbol) {
                        let type_key = obj.typ();
                        let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                        let slot_types = type_key
                            .map(|t| info.type_slot_types(t))
                            .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);
                        
                        // For multi-var, compile rhs once and extract values
                        // For now, just compile rhs for each (inefficient but correct)
                        let tmp = init_builder.alloc_temp_typed(&slot_types);
                        crate::expr::compile_expr_to(&initializer.rhs, tmp, ctx, &mut init_builder, info)?;
                        if slots == 1 {
                            init_builder.emit_op(vo_vm::instruction::Opcode::GlobalSet, global_idx as u16, tmp, 0);
                        } else {
                            init_builder.emit_with_flags(
                                vo_vm::instruction::Opcode::GlobalSetN,
                                slots as u8,
                                global_idx as u16,
                                tmp,
                                0,
                            );
                        }
                    }
                }
                let _ = i; // suppress unused warning
            }
        }
    }
    
    // Add return
    init_builder.emit_op(vo_vm::instruction::Opcode::Return, 0, 0, 0);
    let init_func = init_builder.build();
    let init_func_id = ctx.add_function(init_func);
    // Note: __init__ is NOT registered as a user init function - it's handled separately
    
    // 2. Find main function
    let main_func_id = project.interner.get("main")
        .and_then(|sym| ctx.get_function_index(sym));
    
    // 3. Generate __entry__ function
    let mut entry_builder = FuncBuilder::new("__entry__");
    
    // Call __init__ for global variable initialization
    entry_builder.emit_op(vo_vm::instruction::Opcode::Call, init_func_id as u16, 0, 0);
    
    // Call user-defined init() functions in declaration order
    for &user_init_id in ctx.init_functions() {
        entry_builder.emit_op(vo_vm::instruction::Opcode::Call, user_init_id as u16, 0, 0);
    }
    
    // Call main if exists
    if let Some(main_id) = main_func_id {
        entry_builder.emit_op(vo_vm::instruction::Opcode::Call, main_id as u16, 0, 0);
    }
    
    // Return
    entry_builder.emit_op(vo_vm::instruction::Opcode::Return, 0, 0, 0);
    
    let entry_func = entry_builder.build();
    let entry_func_id = ctx.add_function(entry_func);
    ctx.set_entry_func(entry_func_id);
    
    Ok(())
}
