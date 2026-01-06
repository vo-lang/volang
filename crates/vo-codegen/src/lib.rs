//! Vo bytecode code generation.
//!
//! This crate compiles type-checked AST to VM bytecode.

mod context;
mod embed;
mod error;
mod expr;
mod func;
mod lvalue;
mod stmt;
mod type_info;
mod type_interner;
mod wrapper;

pub use context::CodegenContext;
pub use error::CodegenError;
pub use func::FuncBuilder;
pub use type_info::TypeInfoWrapper;
pub use type_interner::{TypeInterner, intern_type_key};

use vo_analysis::Project;
use vo_syntax::ast::Decl;
use vo_vm::bytecode::Module;

/// Compile a type-checked project to VM bytecode.
pub fn compile_project(project: &Project) -> Result<Module, CodegenError> {
    let info = TypeInfoWrapper::for_main_package(project);
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
    
    // 6. Fill WellKnownTypes for fast error creation
    ctx.fill_well_known_types(project);
    
    // 7. Finalize debug info (sort entries by PC)
    ctx.finalize_debug_info();
    
    // 7. Final check: all IDs within 24-bit limit
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
    use vo_runtime::ValueMeta;
    use std::collections::HashMap;

    fn register_pkg_types(
        pkg_path: &str,
        files: &[vo_syntax::ast::File],
        project: &Project,
        ctx: &mut CodegenContext,
        info: &TypeInfoWrapper,
    ) -> Result<(), CodegenError> {
        use vo_syntax::ast::{Decl, TypeExprKind};
        use vo_vm::bytecode::{StructMeta, InterfaceMeta, NamedTypeMeta};
        use vo_runtime::ValueMeta;
        use std::collections::HashMap;

        // Collect all type declarations (including those inside functions)
        let mut type_decls = Vec::new();

        for file in files {
            for decl in &file.decls {
                match decl {
                    Decl::Type(type_decl) => {
                        type_decls.push(type_decl.clone());
                    }
                    Decl::Func(func_decl) => {
                        // Also collect type declarations from function bodies
                        if let Some(body) = &func_decl.body {
                            collect_type_decls_from_stmts(&body.stmts, &mut type_decls);
                        }
                    }
                    _ => {}
                }
            }
        }

        // Process all collected type declarations
        for type_decl in &type_decls {
            let type_name = project
                .interner
                .resolve(type_decl.name.symbol)
                .unwrap_or("?");
            let qualified_type_name = format!("{}.{}", pkg_path, type_name);

            // Get underlying type key from type expression, and obj_key from declaration name
            let underlying_key = info.type_expr_type(type_decl.ty.id);
            let obj_key = info.get_def(&type_decl.name);
            let named_key = info.obj_type(obj_key, "type declaration must have type");

            // Register type-specific metadata first
            let underlying_meta = match &type_decl.ty.kind {
                TypeExprKind::Struct(struct_type) => {
                    // Build StructMeta with FieldMeta
                    let mut fields = Vec::new();
                    let mut slot_types = Vec::new();
                    let mut offset = 0u16;

                    for field in &struct_type.fields {
                        for name in &field.names {
                            let field_name = project.interner.resolve(name.symbol).unwrap_or("?").to_string();

                            let field_type = info.type_expr_type(field.ty.id);
                            let slot_count = info.type_slot_count(field_type);
                            let slot_type_list = info.type_slot_types(field_type);
                            slot_types.extend(slot_type_list);

                            let field_vk = info.type_value_kind(field_type);
                            let field_rttid = ctx.intern_type_key(field_type, info);

                            fields.push(vo_vm::bytecode::FieldMeta {
                                name: field_name,
                                offset,
                                slot_count,
                                type_info: vo_runtime::ValueRttid::new(field_rttid, field_vk),
                            });
                            offset += slot_count;
                        }
                    }

                    // Empty struct still needs 1 slot for zero-size type workaround
                    if slot_types.is_empty() {
                        slot_types.push(vo_runtime::SlotType::Value);
                    }
                    let field_index: std::collections::HashMap<String, usize> = fields.iter()
                        .enumerate()
                        .map(|(i, f)| (f.name.clone(), i))
                        .collect();
                    let meta = StructMeta { slot_types, fields, field_index };
                    let struct_meta_id = ctx.register_struct_meta(underlying_key, meta);
                    ctx.alias_struct_meta_id(named_key, struct_meta_id);
                    ValueMeta::new(struct_meta_id as u32, vo_runtime::ValueKind::Struct)
                }
                TypeExprKind::Interface(_) => {
                    // Build InterfaceMeta
                    let tc_objs = &info.project.tc_objs;
                    let (method_names, methods) =
                        if let vo_analysis::typ::Type::Interface(iface) = &tc_objs.types[underlying_key] {
                            let all_methods_ref = iface.all_methods();
                            let method_objs: Vec<vo_analysis::objects::ObjKey> =
                                if let Some(methods) = all_methods_ref.as_ref() {
                                    methods.iter().cloned().collect()
                                } else {
                                    iface.methods().iter().cloned().collect()
                                };

                            let names: Vec<String> = method_objs
                                .iter()
                                .map(|m| tc_objs.lobjs[*m].name().to_string())
                                .collect();

                            let metas: Vec<vo_vm::bytecode::InterfaceMethodMeta> = method_objs
                                .iter()
                                .map(|&m| {
                                    let obj = &tc_objs.lobjs[m];
                                    let name = obj.name().to_string();
                                    let sig = if let Some(sig_type) = obj.typ() {
                                        signature_type_to_runtime_type(sig_type, tc_objs, info, ctx)
                                    } else {
                                        vo_runtime::RuntimeType::Func {
                                            params: Vec::new(),
                                            results: Vec::new(),
                                            variadic: false,
                                        }
                                    };
                                    let signature_rttid = ctx.intern_rttid(sig);
                                    vo_vm::bytecode::InterfaceMethodMeta { name, signature_rttid }
                                })
                                .collect();

                            (names, metas)
                        } else {
                            (Vec::new(), Vec::new())
                        };

                    let meta = InterfaceMeta {
                        name: qualified_type_name.to_string(),
                        method_names,
                        methods,
                    };
                    let iface_meta_id = ctx.register_interface_meta(underlying_key, meta);
                    ValueMeta::new(iface_meta_id as u32, vo_runtime::ValueKind::Interface)
                }
                _ => {
                    // Other types (Map, Slice, Chan, etc.): intern underlying type to get rttid
                    let underlying_vk = info.type_value_kind(underlying_key);
                    let underlying_rt = info.type_to_runtime_type(underlying_key, ctx);
                    let underlying_rttid = ctx.intern_rttid(underlying_rt);
                    ValueMeta::new(underlying_rttid, underlying_vk)
                }
            };

            // All named types get NamedTypeMeta (keyed by ObjKey, the true identity)
            let named_type_meta = NamedTypeMeta {
                name: qualified_type_name.to_string(),
                underlying_meta,
                methods: HashMap::new(),
            };
            ctx.register_named_type_meta(obj_key, named_type_meta);
        }

        // Finalize runtime types: fill meta_id fields after all types are registered
        ctx.finalize_runtime_types();

        Ok(())
    }

    // Ensure builtin error interface has a stable InterfaceMeta name for runtime lookup.
    {
        use vo_analysis::typ;
        let tc_objs = &project.tc_objs;
        let error_type = tc_objs.universe().error_type();
        let underlying = typ::underlying_type(error_type, tc_objs);
        if ctx.get_interface_meta_id(underlying).is_none() {
            let (method_names, methods) = if let vo_analysis::typ::Type::Interface(iface) = &tc_objs.types[underlying] {
                let all_methods_ref = iface.all_methods();
                let method_objs: Vec<vo_analysis::objects::ObjKey> = if let Some(methods) = all_methods_ref.as_ref() {
                    methods.iter().cloned().collect()
                } else {
                    iface.methods().iter().cloned().collect()
                };

                let names: Vec<String> = method_objs.iter()
                    .map(|m| tc_objs.lobjs[*m].name().to_string())
                    .collect();

                let metas: Vec<vo_vm::bytecode::InterfaceMethodMeta> = method_objs.iter()
                    .map(|&m| {
                        let obj = &tc_objs.lobjs[m];
                        let name = obj.name().to_string();
                        let sig = if let Some(sig_type) = obj.typ() {
                            signature_type_to_runtime_type(sig_type, tc_objs, info, ctx)
                        } else {
                            vo_runtime::RuntimeType::Func { params: Vec::new(), results: Vec::new(), variadic: false }
                        };
                        let signature_rttid = ctx.intern_rttid(sig);
                        vo_vm::bytecode::InterfaceMethodMeta { name, signature_rttid }
                    })
                    .collect();

                (names, metas)
            } else {
                (Vec::new(), Vec::new())
            };

            let meta = InterfaceMeta {
                name: "error".to_string(),
                method_names,
                methods,
            };
            ctx.register_interface_meta(underlying, meta);
        }
    }
    
    register_pkg_types("main", &project.files, project, ctx, info)?;

    for (pkg_path, files) in &project.imported_files {
        if let Some(pkg_type_info) = project.imported_type_infos.get(pkg_path) {
            let pkg_info = TypeInfoWrapper::for_package(project, pkg_type_info);
            register_pkg_types(pkg_path, files, project, ctx, &pkg_info)?;
        }
    }

    Ok(())
}

/// Recursively collect type declarations from statements
fn collect_type_decls_from_stmts(stmts: &[vo_syntax::ast::Stmt], out: &mut Vec<vo_syntax::ast::TypeDecl>) {
    use vo_syntax::ast::StmtKind;
    for stmt in stmts {
        match &stmt.kind {
            StmtKind::Type(type_decl) => {
                out.push(type_decl.clone());
            }
            StmtKind::Block(block) => {
                collect_type_decls_from_stmts(&block.stmts, out);
            }
            StmtKind::If(if_stmt) => {
                collect_type_decls_from_stmts(&if_stmt.then.stmts, out);
                if let Some(else_stmt) = &if_stmt.else_ {
                    collect_type_decls_from_stmts(&[*else_stmt.clone()], out);
                }
            }
            StmtKind::For(for_stmt) => {
                collect_type_decls_from_stmts(&for_stmt.body.stmts, out);
            }
            StmtKind::Switch(switch_stmt) => {
                for case in &switch_stmt.cases {
                    collect_type_decls_from_stmts(&case.body, out);
                }
            }
            StmtKind::TypeSwitch(ts) => {
                for case in &ts.cases {
                    collect_type_decls_from_stmts(&case.body, out);
                }
            }
            StmtKind::Select(select_stmt) => {
                for case in &select_stmt.cases {
                    collect_type_decls_from_stmts(&case.body, out);
                }
            }
            _ => {}
        }
    }
}

fn build_runtime_types(
    _project: &Project,
    ctx: &mut CodegenContext,
    _info: &TypeInfoWrapper,
) {
    // RuntimeTypes are now built during codegen via intern_rttid()
    // Just transfer them from TypeInterner to module
    let runtime_types = ctx.runtime_types();
    ctx.set_runtime_types(runtime_types);
}

fn collect_declarations(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // First collect main package declarations (using main type_info)
    for file in &project.files {
        collect_file_declarations(file, project, ctx, info)?;
    }
    
    // Then collect imported package declarations (using their respective type_info)
    for (pkg_path, files) in &project.imported_files {
        if let Some(pkg_type_info) = project.imported_type_infos.get(pkg_path) {
            let pkg_info = TypeInfoWrapper::for_package(project, pkg_type_info);
            for file in files {
                collect_file_declarations(file, project, ctx, &pkg_info)?;
            }
        }
    }
    Ok(())
}

fn collect_file_declarations(
    file: &vo_syntax::ast::File,
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    for decl in &file.decls {
        match decl {
            Decl::Func(func_decl) => {
                // Skip extern functions (no body) - they use CallExtern
                if func_decl.body.is_none() {
                    continue;
                }
                
                // Skip init() functions - they can have multiple declarations with same name
                // and will be handled specially during compilation
                let func_name = project.interner.resolve(func_decl.name.symbol).unwrap_or("");
                if func_decl.receiver.is_none() && func_name == "init" {
                    continue;
                }
                
                // Check if this is a method (has receiver)
                let (recv_type, is_pointer_recv) = if let Some(recv) = &func_decl.receiver {
                    let base_type = info.method_receiver_base_type(func_decl)
                        .expect("method receiver must have type");
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
                        
                        // Arrays are stored as GcRef (1 slot) in globals
                        let (slots, slot_types) = if info.is_array(type_key) {
                            (1, vec![vo_runtime::SlotType::GcRef])
                        } else {
                            (info.type_slot_count(type_key), info.type_slot_types(type_key))
                        };
                        let value_kind = info.type_value_kind(type_key) as u8;
                        ctx.register_global(
                            name.symbol,
                            vo_vm::bytecode::GlobalDef {
                                name: project.interner.resolve(name.symbol).unwrap_or("?").to_string(),
                                slots,
                                value_kind,
                                meta_id: 0,
                                slot_types,
                            },
                        );
                    }
                }
            }
            _ => {}
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
    // (recv_type, method_name, func_id, is_pointer_receiver, signature)
    let mut method_mappings: Vec<(vo_analysis::objects::TypeKey, String, u32, bool, u32)> = Vec::new();
    
    // First compile main package files (using main type_info)
    for file in &project.files {
        compile_file_functions(file, project, ctx, info, &mut method_mappings)?;
    }
    
    // Then compile imported package files (using their respective type_info)
    for (pkg_path, files) in &project.imported_files {
        if let Some(pkg_type_info) = project.imported_type_infos.get(pkg_path) {
            let pkg_info = TypeInfoWrapper::for_package(project, pkg_type_info);
            for file in files {
                compile_file_functions(file, project, ctx, &pkg_info, &mut method_mappings)?;
            }
        }
    }
    
    // Update NamedTypeMeta.methods with method func_ids (direct methods only)
    update_named_type_methods(ctx, &method_mappings, info);
    
    // Build pending itabs - uses lookup_field_or_method to find methods (including promoted)
    ctx.finalize_itabs(&info.project.tc_objs, &info.project.interner);
    
    Ok(())
}

fn compile_file_functions(
    file: &vo_syntax::ast::File,
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
    method_mappings: &mut Vec<(vo_analysis::objects::TypeKey, String, u32, bool, u32)>,
) -> Result<(), CodegenError> {
    for decl in &file.decls {
        if let Decl::Func(func_decl) = decl {
            // Skip extern functions (no body) - they use CallExtern
            if func_decl.body.is_none() {
                continue;
            }
            
            let func_name = project.interner.resolve(func_decl.name.symbol)
                .unwrap_or("unknown");
            let func_id = compile_func_decl(func_decl, ctx, info)?;
            
            // Register ObjKey -> func_id mapping for lookup_field_or_method in itab building
            let func_obj_key = info.get_def(&func_decl.name);
            ctx.register_objkey_func(func_obj_key, func_id);
            
            // Check if this is an init() function (no receiver, name is "init")
            if func_decl.receiver.is_none() && func_name == "init" {
                ctx.register_init_function(func_id);
            }
            
            // If this is a method, record the mapping
            if let Some(recv) = &func_decl.receiver {
                // Get receiver base type from function signature
                let recv_type = info.method_receiver_base_type(func_decl)
                    .expect("method receiver must have type");
                {
                    let method_name = project.interner.resolve(func_decl.name.symbol)
                        .unwrap_or("?").to_string();
                    
                    // Generate method signature rttid
                    let signature = generate_method_signature(func_decl, info, &project.interner, ctx);
                    let signature_rttid = ctx.intern_rttid(signature);
                    
                    // For value receiver methods, generate a wrapper that accepts GcRef
                    // and dereferences it before calling the original method
                    let iface_func_id = if !recv.is_pointer {
                        wrapper::generate_iface_wrapper(ctx, info, func_decl, func_id, recv_type)?
                    } else {
                        func_id
                    };
                    
                    // Register ObjKey -> iface_func_id mapping for itab building
                    ctx.register_objkey_iface_func(func_obj_key, iface_func_id);
                    
                    // Register the wrapper (or original for pointer receiver) for interface dispatch
                    method_mappings.push((recv_type, method_name, iface_func_id, recv.is_pointer, signature_rttid));
                }
            }
        }
    }
    Ok(())
}

fn update_named_type_methods(
    ctx: &mut CodegenContext,
    method_mappings: &[(vo_analysis::objects::TypeKey, String, u32, bool, u32)],
    info: &TypeInfoWrapper,
) {
    use vo_analysis::typ::Type;
    let tc_objs = &info.project.tc_objs;
    
    // Group methods by type_key, lookup ObjKey from Named type
    for (type_key, method_name, func_id, is_pointer_receiver, signature_rttid) in method_mappings {
        // Get ObjKey from the Named type (the true identity)
        if let Type::Named(named) = &tc_objs.types[*type_key] {
            if let Some(obj_key) = named.obj() {
                if let Some(named_type_id) = ctx.get_named_type_id(*obj_key) {
                    ctx.update_named_type_method(named_type_id, method_name.clone(), *func_id, *is_pointer_receiver, *signature_rttid);
                }
            }
        }
    }
}

/// Generate RuntimeType::Func signature for a method (excluding receiver)
fn generate_method_signature(
    func_decl: &vo_syntax::ast::FuncDecl,
    info: &TypeInfoWrapper,
    interner: &vo_common::SymbolInterner,
    ctx: &mut CodegenContext,
) -> vo_runtime::RuntimeType {
    use vo_runtime::RuntimeType;
    use vo_runtime::ValueRttid;
    
    // Collect param ValueRttids
    let mut params = Vec::new();
    for param in &func_decl.sig.params {
        let param_type_key = info.type_expr_type(param.ty.id);
        let param_rttid = ctx.intern_type_key(param_type_key, info);
        let param_vk = info.type_value_kind(param_type_key);
        // Each name in param.names represents one parameter of this type
        for _ in &param.names {
            params.push(ValueRttid::new(param_rttid, param_vk));
        }
    }
    
    // Collect result ValueRttids
    let mut results = Vec::new();
    for result in &func_decl.sig.results {
        let result_type_key = info.type_expr_type(result.ty.id);
        let result_rttid = ctx.intern_type_key(result_type_key, info);
        let result_vk = info.type_value_kind(result_type_key);
        results.push(ValueRttid::new(result_rttid, result_vk));
    }
    
    RuntimeType::Func {
        params,
        results,
        variadic: func_decl.sig.variadic,
    }
}

/// Extract ValueRttids from a tuple type (params or results).
fn tuple_to_value_rttids(
    tuple_key: vo_analysis::objects::TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    info: &TypeInfoWrapper,
    ctx: &mut CodegenContext,
) -> Vec<vo_runtime::ValueRttid> {
    use vo_analysis::typ::Type;
    use vo_runtime::ValueRttid;
    if let Type::Tuple(tuple) = &tc_objs.types[tuple_key] {
        tuple.vars().iter()
            .filter_map(|&v| {
                tc_objs.lobjs[v].typ().map(|t| {
                    let rttid = ctx.intern_type_key(t, info);
                    let vk = info.type_value_kind(t);
                    ValueRttid::new(rttid, vk)
                })
            })
            .collect()
    } else {
        Vec::new()
    }
}

/// Convert a Signature type to RuntimeType::Func with proper params/results
fn signature_type_to_runtime_type(
    sig_type: vo_analysis::objects::TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    info: &TypeInfoWrapper,
    ctx: &mut CodegenContext,
) -> vo_runtime::RuntimeType {
    use vo_analysis::typ::Type;
    use vo_runtime::RuntimeType;
    
    if let Type::Signature(sig) = &tc_objs.types[sig_type] {
        RuntimeType::Func {
            params: tuple_to_value_rttids(sig.params(), tc_objs, info, ctx),
            results: tuple_to_value_rttids(sig.results(), tc_objs, info, ctx),
            variadic: sig.variadic(),
        }
    } else {
        RuntimeType::Func { params: Vec::new(), results: Vec::new(), variadic: false }
    }
}

fn compile_func_decl(
    func_decl: &vo_syntax::ast::FuncDecl,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<u32, CodegenError> {
    let name = info.project.interner.resolve(func_decl.name.symbol)
        .unwrap_or("unknown");
    
    // Get the func_id for pre-declared functions (for debug info recording)
    let (recv_base_type, is_pointer_recv) = func_decl.receiver.as_ref()
        .map(|recv| {
            let base_type = info.method_receiver_base_type(func_decl)
                .expect("method receiver must have type");
            (Some(base_type), recv.is_pointer)
        })
        .unwrap_or((None, false));
    
    // Set current function ID for debug info (if pre-declared)
    if let Some(func_id) = ctx.get_func_index(recv_base_type, is_pointer_recv, func_decl.name.symbol) {
        ctx.set_current_func_id(func_id);
    }
    
    let mut builder = FuncBuilder::new(name);
    
    // Define receiver as first parameter (if method)
    if let Some(recv) = &func_decl.receiver {
        // For pointer receiver (*T), it's 1 slot (GcRef)
        // For value receiver (T), it's the struct's slot count
        let (slots, slot_types) = if recv.is_pointer {
            (1, vec![vo_runtime::SlotType::GcRef])
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
    
    // Define parameters and collect escaped ones for boxing
    // Reference types (closure, slice, map, channel, pointer) don't need boxing - they're already GcRefs
    let mut escaped_params = Vec::new();
    for param in &func_decl.sig.params {
        let (slots, slot_types) = info.type_expr_layout(param.ty.id);
        let type_key = info.type_expr_type(param.ty.id);
        for name in &param.names {
            builder.define_param(name.symbol, slots, &slot_types);
            let obj_key = info.get_def(name);
            if info.is_escaped(obj_key) && !info.is_reference_type(type_key) {
                escaped_params.push((name.symbol, type_key, slots, slot_types.clone()));
            }
        }
    }
    
    // Box escaped parameters: allocate heap storage and copy param values
    for (sym, type_key, slots, slot_types) in escaped_params {
        if let Some((gcref_slot, param_slot)) = builder.box_escaped_param(sym, slots) {
            let meta_idx = ctx.get_or_create_value_meta(Some(type_key), slots, &slot_types);
            let meta_reg = builder.alloc_temp(1);
            builder.emit_op(vo_vm::instruction::Opcode::LoadConst, meta_reg, meta_idx, 0);
            builder.emit_with_flags(vo_vm::instruction::Opcode::PtrNew, slots as u8, gcref_slot, meta_reg, 0);
            builder.emit_ptr_set(gcref_slot, 0, param_slot, slots);
        }
    }
    
    // Set return slots and types
    let ret_slots: u16 = func_decl.sig.results.iter()
        .map(|r| info.type_expr_layout(r.ty.id).0)
        .sum();
    builder.set_ret_slots(ret_slots);
    let return_types: Vec<_> = func_decl.sig.results.iter()
        .map(|r| info.type_expr_type(r.ty.id))
        .collect();
    builder.set_return_types(return_types.clone());
    
    // Define named return variables as locals (zero-initialized)
    for result in &func_decl.sig.results {
        if let Some(name) = &result.name {
            let (slots, slot_types) = info.type_expr_layout(result.ty.id);
            builder.define_local_stack(name.symbol, slots, &slot_types);
            builder.register_named_return(name.symbol);
        }
    }
    
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

/// Emit GlobalSet or GlobalSetN depending on slot count.
fn emit_global_set(builder: &mut FuncBuilder, global_idx: u32, src: u16, slots: u16) {
    if slots == 1 {
        builder.emit_op(vo_vm::instruction::Opcode::GlobalSet, global_idx as u16, src, 0);
    } else {
        builder.emit_with_flags(
            vo_vm::instruction::Opcode::GlobalSetN,
            slots as u8,
            global_idx as u16,
            src,
            0,
        );
    }
}

/// Compile global array initialization: allocate heap array and store GcRef in global.
fn compile_global_array_init(
    rhs: &vo_syntax::ast::Expr,
    array_type: vo_analysis::objects::TypeKey,
    global_idx: u32,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_vm::instruction::Opcode;
    
    let array_len = info.array_len(array_type);
    let elem_type = info.array_elem_type(array_type);
    let elem_slots = info.type_slot_count(elem_type);
    let elem_bytes = (elem_slots as u32 * 8) as u16;
    let elem_vk = info.type_value_kind(elem_type);
    
    // Allocate registers for: gcref, meta_reg, len_reg, temp for elements
    let gcref_slot = func.alloc_temp(1);
    let meta_reg = func.alloc_temp(1);
    let len_reg = func.alloc_temp(1);
    
    // Get elem_meta: (rttid << 8) | elem_vk
    let elem_rttid = ctx.intern_type_key(elem_type, info);
    let elem_meta = ((elem_rttid as u64) << 8) | (elem_vk as u64);
    let meta_idx = ctx.const_int(elem_meta as i64);
    func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
    
    // Load array length
    let len_idx = ctx.const_int(array_len as i64);
    func.emit_op(Opcode::LoadConst, len_reg, len_idx, 0);
    
    // Emit ArrayNew: gcref_slot = ArrayNew(meta_reg, len_reg)
    let flags = if elem_bytes <= 8 { elem_bytes as u8 } else { 0 };
    if flags == 0 {
        // Dynamic elem_bytes: need extra register
        let eb_idx = ctx.const_int(elem_bytes as i64);
        func.emit_op(Opcode::LoadConst, len_reg + 1, eb_idx, 0);
    }
    func.emit_with_flags(Opcode::ArrayNew, flags, gcref_slot, meta_reg, len_reg);
    
    // Compile array elements and set them
    if let vo_syntax::ast::ExprKind::CompositeLit(lit) = &rhs.kind {
        let elem_slot_types = info.type_slot_types(elem_type);
        let tmp_elem = func.alloc_temp_typed(&elem_slot_types);
        // For dynamic elem_bytes (flags=0), need idx_reg and idx_reg+1 for elem_bytes
        let idx_reg = func.alloc_temp(if flags == 0 { 2 } else { 1 });
        
        for (i, elem) in lit.elems.iter().enumerate() {
            // Compile element value to tmp_elem
            crate::expr::compile_expr_to(&elem.value, tmp_elem, ctx, func, info)?;
            
            // Load index
            func.emit_op(Opcode::LoadInt, idx_reg, i as u16, 0);
            
            // For dynamic elem_bytes, load elem_bytes into idx_reg + 1
            if flags == 0 {
                let eb_idx = ctx.const_int(elem_bytes as i64);
                func.emit_op(Opcode::LoadConst, idx_reg + 1, eb_idx, 0);
            }
            
            // ArraySet: arr[idx] = tmp_elem
            func.emit_with_flags(Opcode::ArraySet, flags, gcref_slot, idx_reg, tmp_elem);
        }
    }
    
    // Store GcRef in global (1 slot)
    func.emit_op(Opcode::GlobalSet, global_idx as u16, gcref_slot, 0);
    
    Ok(())
}

fn compile_init_and_entry(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // 1. Generate __init__ function for global variable initialization
    let mut init_builder = FuncBuilder::new("__init__");
    
    // Initialize global variables in dependency order (from type checker analysis)
    for initializer in info.init_order() {
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
                    
                    // Special handling for arrays: allocate on heap
                    if let Some(tk) = type_key {
                        if info.is_array(tk) {
                            compile_global_array_init(
                                &initializer.rhs, tk, global_idx,
                                ctx, &mut init_builder, info
                            )?;
                            continue;
                        }
                    }
                    
                    let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                    let slot_types = type_key
                        .map(|t| info.type_slot_types(t))
                        .unwrap_or_else(|| vec![vo_runtime::SlotType::Value]);
                    
                    let tmp = init_builder.alloc_temp_typed(&slot_types);
                    crate::expr::compile_expr_to(&initializer.rhs, tmp, ctx, &mut init_builder, info)?;
                    emit_global_set(&mut init_builder, global_idx, tmp, slots);
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
                            .unwrap_or_else(|| vec![vo_runtime::SlotType::Value]);
                        
                        // For multi-var, compile rhs once and extract values
                        // For now, just compile rhs for each (inefficient but correct)
                        let tmp = init_builder.alloc_temp_typed(&slot_types);
                        crate::expr::compile_expr_to(&initializer.rhs, tmp, ctx, &mut init_builder, info)?;
                        emit_global_set(&mut init_builder, global_idx, tmp, slots);
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
