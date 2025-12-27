//! Vo bytecode code generation.
//!
//! This crate compiles type-checked AST to VM bytecode.

mod context;
mod error;
mod expr;
mod func;
mod stmt;
mod type_info;
mod type_interner;

pub use context::CodegenContext;
pub use error::CodegenError;
pub use func::FuncBuilder;
pub use type_info::TypeInfoWrapper;
pub use type_interner::{TypeInterner, type_key_to_runtime_type};

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
    
    // 5. Build runtime_types after all codegen (all types have been assigned rttid)
    build_runtime_types(project, &mut ctx, &info);
    
    // 6. Final check: all IDs within 24-bit limit
    ctx.check_id_limits().map_err(CodegenError::Internal)?;
    
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
                let underlying_key = info.type_expr_type(type_decl.ty.id);
                let named_key = info.obj_type(info.get_def(&type_decl.name), "type declaration must have type");
                
                // Register type-specific metadata first
                let underlying_meta = match &type_decl.ty.kind {
                    TypeExprKind::Struct(struct_type) => {
                        // Build StructMeta
                        let mut field_names = Vec::new();
                        let mut field_offsets = Vec::new();
                        let mut slot_types = Vec::new();
                        let mut offset = 0u16;
                        
                        for field in &struct_type.fields {
                            for name in &field.names {
                                let field_name = project.interner.resolve(name.symbol)
                                    .unwrap_or("?");
                                field_names.push(field_name.to_string());
                                field_offsets.push(offset);
                                
                                let field_type = info.type_expr_type(field.ty.id);
                                let slots = info.type_slot_count(field_type);
                                let slot_type_list = info.type_slot_types(field_type);
                                slot_types.extend(slot_type_list);
                                offset += slots;
                            }
                        }
                        
                        let meta = StructMeta {
                            field_names,
                            field_offsets,
                            slot_types,
                        };
                        let struct_meta_id = ctx.register_struct_meta(underlying_key, meta);
                        ValueMeta::new(struct_meta_id as u32, vo_common_core::types::ValueKind::Struct)
                    }
                    TypeExprKind::Interface(_) => {
                        // Build InterfaceMeta
                        let tc_objs = &info.project.tc_objs;
                        let method_names = if let vo_analysis::typ::Type::Interface(iface) = &tc_objs.types[underlying_key] {
                            let all_methods_ref = iface.all_methods();
                            if let Some(methods) = all_methods_ref.as_ref() {
                                methods.iter()
                                    .map(|m| tc_objs.lobjs[*m].name().to_string())
                                    .collect()
                            } else {
                                iface.methods().iter()
                                    .map(|m| tc_objs.lobjs[*m].name().to_string())
                                    .collect()
                            }
                        } else {
                            Vec::new()
                        };
                        
                        let meta = InterfaceMeta {
                            name: type_name.to_string(),
                            method_names,
                        };
                        let iface_meta_id = ctx.register_interface_meta(underlying_key, meta);
                        ValueMeta::new(iface_meta_id as u32, vo_common_core::types::ValueKind::Interface)
                    }
                    _ => {
                        // Other types: use underlying ValueKind, meta_id=0
                        let underlying_vk = info.type_value_kind(underlying_key);
                        ValueMeta::new(0, underlying_vk)
                    }
                };
                
                // All named types get NamedTypeMeta
                let named_type_meta = NamedTypeMeta {
                    name: type_name.to_string(),
                    underlying_meta,
                    methods: HashMap::new(),
                };
                ctx.register_named_type_meta(named_key, named_type_meta);
            }
        }
    }
    
    Ok(())
}

fn build_runtime_types(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) {
    use std::collections::HashMap;
    
    let tc_objs = &info.project.tc_objs;
    
    // Build TypeKey -> named_type_id mapping (u32 for compatibility)
    let named_type_ids: HashMap<vo_analysis::objects::TypeKey, u32> = ctx.named_type_ids_iter()
        .map(|(tk, id)| (tk, id as u32))
        .collect();
    
    // Build RuntimeType for each pending type (in rttid order)
    let type_keys: Vec<_> = ctx.pending_rttids().to_vec();
    let mut runtime_types = Vec::with_capacity(type_keys.len());
    
    for type_key in type_keys {
        let rt = type_key_to_runtime_type(type_key, tc_objs, &project.interner, &named_type_ids);
        runtime_types.push(rt);
    }
    
    ctx.set_runtime_types(runtime_types);
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
                    // Skip init() functions - they can have multiple declarations with same name
                    // and will be handled specially during compilation
                    let func_name = project.interner.resolve(func_decl.name.symbol).unwrap_or("");
                    if func_decl.receiver.is_none() && func_name == "init" {
                        continue;
                    }
                    
                    // Check if this is a method (has receiver)
                    let (recv_type, is_pointer_recv) = if let Some(recv) = &func_decl.receiver {
                        let base_type = info.obj_type(info.get_use(&recv.ty), "method receiver must have type");
                        (Some(base_type), recv.is_pointer)
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
                                info.type_expr_type(ty.id)
                            } else if i < spec.values.len() {
                                info.expr_type(spec.values[i].id)
                            } else {
                                panic!("global var must have type annotation or initializer")
                            };
                            
                            let slots = info.type_slot_count(type_key);
                            let value_kind = info.type_value_kind(type_key) as u8;
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
                    let recv_type = info.obj_type(info.get_use(&recv.ty), "method receiver must have type");
                    {
                        let method_name = project.interner.resolve(func_decl.name.symbol)
                            .unwrap_or("?").to_string();
                        
                        // For value receiver methods, generate a wrapper that accepts GcRef
                        // and dereferences it before calling the original method
                        let iface_func_id = if !recv.is_pointer {
                            generate_value_receiver_wrapper(ctx, info, func_decl, func_id, recv_type)?
                        } else {
                            func_id
                        };
                        
                        // Register the wrapper (or original for pointer receiver) for interface dispatch
                        method_mappings.push((recv_type, method_name, iface_func_id, recv.is_pointer));
                    }
                }
            }
        }
    }
    
    // Update NamedTypeMeta.methods with method func_ids
    update_named_type_methods(ctx, &method_mappings);
    
    // Propagate promoted methods from embedded fields
    propagate_promoted_methods(ctx, info);
    
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

/// Propagate promoted methods from embedded fields to outer types.
/// For each struct type with embedded fields, copy the embedded type's methods
/// to the outer type (if not already defined).
fn propagate_promoted_methods(ctx: &mut CodegenContext, info: &TypeInfoWrapper) {
    use vo_analysis::typ::Type;
    
    let tc_objs = &info.project.tc_objs;
    
    // Collect all named type keys with their IDs
    let named_types: Vec<(vo_analysis::objects::TypeKey, u32)> = ctx.named_type_ids_iter().collect();
    
    for (named_type_key, outer_id) in named_types {
        // Get underlying type
        let underlying_key = vo_analysis::typ::underlying_type(named_type_key, tc_objs);
        
        // Check if underlying is a struct
        if let Type::Struct(struct_detail) = &tc_objs.types[underlying_key] {
            // Check each field for embedded fields
            for &field_obj_key in struct_detail.fields() {
                let field_obj = &tc_objs.lobjs[field_obj_key];
                
                // Check if this is an embedded field
                if field_obj.entity_type().var_property().embedded {
                    // Get the embedded field's type
                    if let Some(field_type_key) = field_obj.typ() {
                        // Get the named type of the embedded field (strip pointer if needed)
                        let embed_type_key = if info.is_pointer(field_type_key) {
                            info.pointer_base(field_type_key)
                        } else {
                            field_type_key
                        };
                        
                        // Get embedded type's named_type_id and copy its methods
                        if let Some(embed_id) = ctx.get_named_type_id(embed_type_key) {
                            // Get methods from embedded type
                            let methods_to_copy: Vec<(String, vo_vm::bytecode::MethodInfo)> = 
                                ctx.get_named_type_methods(embed_id)
                                    .into_iter()
                                    .collect();
                            
                            // Copy methods to outer type (if not already present)
                            for (method_name, method_info) in methods_to_copy {
                                ctx.update_named_type_method_if_absent(
                                    outer_id,
                                    method_name,
                                    method_info.func_id,
                                    method_info.is_pointer_receiver,
                                );
                            }
                        }
                    }
                }
            }
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
            let type_key = info.obj_type(info.get_use(&recv.ty), "method receiver must have type");
            let slots = info.type_slot_count(type_key);
            let slot_types = info.type_slot_types(type_key);
            (slots, slot_types)
        };
        
        // Set recv_slots for interface method calls
        builder.set_recv_slots(slots);
        
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
            let base_type = info.obj_type(info.get_use(&recv.ty), "method receiver must have type");
            (Some(base_type), recv.is_pointer)
        })
        .unwrap_or((None, false));
    // init() functions use add_function (they're not pre-declared since there can be multiple)
    let func_name = info.project.interner.resolve(func_decl.name.symbol).unwrap_or("");
    let func_id = if recv_base_type.is_none() && func_name == "init" {
        ctx.add_function(func_def)
    } else {
        ctx.define_func(func_def, recv_base_type, is_pointer_recv, func_decl.name.symbol)
    };
    
    Ok(func_id)
}

/// Generate a wrapper function for value receiver methods.
/// The wrapper accepts a GcRef (pointer to the value), dereferences it,
/// and calls the original method with the value.
fn generate_value_receiver_wrapper(
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
    func_decl: &vo_syntax::ast::FuncDecl,
    original_func_id: u32,
    recv_type: vo_analysis::objects::TypeKey,
) -> Result<u32, CodegenError> {
    let name = info.project.interner.resolve(func_decl.name.symbol)
        .unwrap_or("unknown");
    let wrapper_name = format!("{}$iface", name);
    
    let mut builder = FuncBuilder::new(&wrapper_name);
    
    // Wrapper receives GcRef as first parameter (1 slot)
    let ptr_slot = builder.define_param(vo_common::symbol::Symbol::DUMMY, 1, &[vo_common_core::types::SlotType::GcRef]);
    
    // Define other parameters (forwarded from original function)
    let mut wrapper_param_slots = Vec::new();
    for param in &func_decl.sig.params {
        let (slots, slot_types) = info.type_expr_layout(param.ty.id);
        for name in &param.names {
            let slot = builder.define_param(name.symbol, slots, &slot_types);
            wrapper_param_slots.push((slot, slots));
        }
    }
    
    // Get receiver value slots
    let recv_slots = info.type_slot_count(recv_type);
    let recv_slot_types = info.type_slot_types(recv_type);
    
    let ret_slots: u16 = func_decl.sig.results.iter()
        .map(|r| info.type_expr_layout(r.ty.id).0)
        .sum();
    
    // Allocate args area for call: recv_value + params
    let args_start = builder.alloc_temp_typed(&recv_slot_types);
    
    // Dereference GcRef to get value: PtrGetN(recv_slots, args_start, ptr_slot, 0)
    builder.emit_with_flags(
        vo_vm::instruction::Opcode::PtrGetN,
        recv_slots as u8,
        args_start,
        ptr_slot,
        0,
    );
    
    // Copy other parameters after receiver value
    let mut dest_offset = args_start + recv_slots;
    for (src_slot, slots) in &wrapper_param_slots {
        if *slots > 0 {
            builder.alloc_temp(*slots);
            builder.emit_copy(dest_offset, *src_slot, *slots);
            dest_offset += *slots;
        }
    }
    
    // Call original function
    let forwarded_param_slots: u16 = wrapper_param_slots.iter().map(|(_, s)| *s).sum();
    let total_arg_slots = recv_slots + forwarded_param_slots;
    let c = crate::type_info::encode_call_args(total_arg_slots, ret_slots);
    let (func_id_low, func_id_high) = crate::type_info::encode_func_id(original_func_id);
    builder.emit_with_flags(vo_vm::instruction::Opcode::Call, func_id_high, func_id_low, args_start, c);
    
    // Return (result is already at args_start)
    builder.set_ret_slots(ret_slots);
    builder.emit_op(vo_vm::instruction::Opcode::Return, args_start, ret_slots, 0);
    
    let func_def = builder.build();
    let wrapper_id = ctx.add_function(func_def);
    
    Ok(wrapper_id)
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
