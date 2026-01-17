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
    
    // 5. Collect promoted methods from embedded interfaces
    // This must happen after compile_functions (direct methods registered)
    // and before finalize_itabs (itabs need complete method set)
    collect_promoted_methods(project, &mut ctx, &info);
    
    // 6. Build all pending itabs (from functions + __init__)
    ctx.finalize_itabs(&info.project.tc_objs, &info.project.interner);
    
    // 7. Build runtime_types after all codegen (all types have been assigned rttid)
    build_runtime_types(project, &mut ctx, &info);
    
    // 8. Fill WellKnownTypes for fast error creation
    ctx.fill_well_known_types(project);
    
    // 9. Finalize debug info (sort entries by PC)
    ctx.finalize_debug_info();
    
    // 10. Final check: all IDs within 24-bit limit
    ctx.check_id_limits().map_err(CodegenError::Internal)?;
    
    Ok(ctx.finish())
}

fn register_types(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    
    use vo_vm::bytecode::InterfaceMeta;
    
    

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
        // Named types are dynamically registered during intern_type_key if not already present
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
                        let field_type = info.type_expr_type(field.ty.id);
                        let slot_count = info.type_slot_count(field_type);
                        let slot_type_list = info.type_slot_types(field_type);
                        let field_vk = info.type_value_kind(field_type);
                        let field_rttid = ctx.intern_type_key(field_type, info);
                        
                        if field.names.is_empty() {
                            // Embedded field: name comes from the type
                            let field_name = info.get_type_name(field_type);
                            slot_types.extend(slot_type_list);
                            fields.push(vo_vm::bytecode::FieldMeta {
                                name: field_name,
                                offset,
                                slot_count,
                                type_info: vo_runtime::ValueRttid::new(field_rttid, field_vk),
                                embedded: true,
                            });
                            offset += slot_count;
                        } else {
                            // Named field(s)
                            for name in &field.names {
                                let field_name = project.interner.resolve(name.symbol).unwrap_or("?").to_string();
                                slot_types.extend(slot_type_list.clone());
                                fields.push(vo_vm::bytecode::FieldMeta {
                                    name: field_name,
                                    offset,
                                    slot_count,
                                    type_info: vo_runtime::ValueRttid::new(field_rttid, field_vk),
                                    embedded: false,
                                });
                                offset += slot_count;
                            }
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
    
    // Register builtin protocol interfaces (DynAttr, DynSetAttr, etc.)
    // These don't depend on user imports - they're language-level semantics.
    register_builtin_protocols(project, ctx, info);
    
    register_pkg_types("main", &project.files, project, ctx, info)?;

    for (pkg_path, files) in &project.imported_files {
        if let Some(pkg_type_info) = project.imported_type_infos.get(pkg_path) {
            let pkg_info = TypeInfoWrapper::for_package(project, pkg_type_info);
            register_pkg_types(pkg_path, files, project, ctx, &pkg_info)?;
        }
    }

    Ok(())
}

/// Register builtin protocol interfaces (DynAttr, DynSetAttr, etc.)
/// These enable protocol dispatch for ~> operator without requiring import "dyn".
fn register_builtin_protocols(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) {
    use vo_runtime::{RuntimeType, ValueKind, ValueRttid};
    use vo_vm::bytecode::{InterfaceMeta, InterfaceMethodMeta};
    
    let tc_objs = &project.tc_objs;
    
    // Get ValueRttid for basic types
    let string_rttid = ValueRttid::new(ValueKind::String as u32, ValueKind::String);
    
    // Get any type (empty interface) from universe and intern it
    let any_type = tc_objs.universe().any_type();
    let any_value_rttid = ctx.intern_type_key(any_type, info);
    let any_rttid = ValueRttid::new(any_value_rttid, ValueKind::Interface);
    
    // Get error type from universe and intern it properly
    let error_type = tc_objs.universe().error_type();
    let error_value_rttid = ctx.intern_type_key(error_type, info);
    let error_rttid = ValueRttid::new(error_value_rttid, ValueKind::Interface);
    
    // Helper to create signature rttid
    let make_sig_rttid = |ctx: &mut CodegenContext, params: Vec<ValueRttid>, results: Vec<ValueRttid>, variadic: bool| -> u32 {
        let rt = RuntimeType::Func { params, results, variadic };
        ctx.intern_rttid(rt)
    };
    
    // 1. AttrObject: DynAttr(name string) (any, error)
    let attr_sig_rttid = make_sig_rttid(ctx, vec![string_rttid], vec![any_rttid, error_rttid], false);
    let attr_meta = InterfaceMeta {
        name: "AttrObject".to_string(),
        method_names: vec!["DynAttr".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "DynAttr".to_string(),
            signature_rttid: attr_sig_rttid,
        }],
    };
    ctx.register_builtin_protocol("AttrObject", attr_meta);
    
    // 2. SetAttrObject: DynSetAttr(name string, value any) error
    let set_attr_sig_rttid = make_sig_rttid(ctx, vec![string_rttid, any_rttid], vec![error_rttid], false);
    let set_attr_meta = InterfaceMeta {
        name: "SetAttrObject".to_string(),
        method_names: vec!["DynSetAttr".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "DynSetAttr".to_string(),
            signature_rttid: set_attr_sig_rttid,
        }],
    };
    ctx.register_builtin_protocol("SetAttrObject", set_attr_meta);
    
    // 3. IndexObject: DynIndex(key any) (any, error)
    let index_sig_rttid = make_sig_rttid(ctx, vec![any_rttid], vec![any_rttid, error_rttid], false);
    let index_meta = InterfaceMeta {
        name: "IndexObject".to_string(),
        method_names: vec!["DynIndex".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "DynIndex".to_string(),
            signature_rttid: index_sig_rttid,
        }],
    };
    ctx.register_builtin_protocol("IndexObject", index_meta);
    
    // 4. SetIndexObject: DynSetIndex(key any, value any) error
    let set_index_sig_rttid = make_sig_rttid(ctx, vec![any_rttid, any_rttid], vec![error_rttid], false);
    let set_index_meta = InterfaceMeta {
        name: "SetIndexObject".to_string(),
        method_names: vec!["DynSetIndex".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "DynSetIndex".to_string(),
            signature_rttid: set_index_sig_rttid,
        }],
    };
    ctx.register_builtin_protocol("SetIndexObject", set_index_meta);
    
    // 5. CallObject: DynCall(args ...any) (any, error)
    // Variadic signature: the param is []any (slice of any), not any itself
    let slice_any_rttid_value = ctx.intern_rttid(RuntimeType::Slice(any_rttid));
    let slice_any_rttid = ValueRttid::new(slice_any_rttid_value, ValueKind::Slice);
    let call_sig_rttid = make_sig_rttid(ctx, vec![slice_any_rttid], vec![any_rttid, error_rttid], true);
    let call_meta = InterfaceMeta {
        name: "CallObject".to_string(),
        method_names: vec!["DynCall".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "DynCall".to_string(),
            signature_rttid: call_sig_rttid,
        }],
    };
    ctx.register_builtin_protocol("CallObject", call_meta);
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
                
                let obj_key = info.get_def(&func_decl.name);
                ctx.declare_func(recv_type, is_pointer_recv, func_decl.name.symbol, obj_key, func_name);
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
                        let obj_key = info.get_def(name);
                        ctx.register_global(
                            obj_key,
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
    
    // Note: Functions are already pre-registered by collect_declarations via declare_func.
    // This handles forward references (function A calls function B defined later).
    
    // Compile function bodies - main package files first
    for file in &project.files {
        compile_file_functions(file, project, ctx, info, &mut method_mappings)?;
    }
    // Then imported package files
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
    
    // Note: finalize_itabs is called in compile_project after compile_init_and_entry
    // to handle itabs from both functions and global variable initialization
    
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
            
            // init() functions are not pre-declared (can have multiple with same name)
            let is_init = func_decl.receiver.is_none() && func_name == "init";
            let func_obj_key = info.get_def(&func_decl.name);
            
            let func_id = if is_init {
                // Compile init() directly (not pre-declared)
                let id = compile_func_decl_new(func_decl, ctx, info)?;
                ctx.register_init_function(id);
                id
            } else {
                // Get pre-declared func_id using ObjKey (not func_indices which can collide across packages)
                let id = ctx.get_func_by_objkey(func_obj_key)
                    .ok_or_else(|| CodegenError::Internal(format!("function not pre-declared: {} obj_key={:?}", func_name, func_obj_key)))?;
                
                // Compile function body and replace placeholder
                compile_func_decl_at(func_decl, id, ctx, info)?;
                id
            };
            
            // If this is a method, record the mapping
            if let Some(recv) = &func_decl.receiver {
                // Get receiver base type from function signature
                let recv_type = info.method_receiver_base_type(func_decl)
                    .expect("method receiver must have type");
                {
                    let method_name = project.interner.resolve(func_decl.name.symbol)
                        .unwrap_or("?").to_string();
                    
                    // Generate method signature rttid
                    let signature = generate_method_signature(func_decl, info, ctx);
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

/// Collect promoted methods from embedded fields (interfaces and structs) for all struct types.
/// This generates wrapper functions and registers them to NamedTypeMeta.methods,
/// so that runtime itab building can find them.
fn collect_promoted_methods(
    _project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) {
    use vo_analysis::typ::Type;
    use vo_analysis::lookup::{lookup_field_or_method, LookupResult};
    
    
    let tc_objs = &info.project.tc_objs;
    let interner = &info.project.interner;
    
    // Collect all (type_key, obj_key, named_type_id) for Named struct types
    let named_structs: Vec<_> = ctx.all_named_type_ids()
        .filter_map(|(obj_key, named_type_id)| {
            // Find the type_key for this obj_key
            let type_key = tc_objs.types.iter()
                .find(|(_, t)| {
                    if let Type::Named(n) = t {
                        n.obj().as_ref() == Some(&obj_key)
                    } else {
                        false
                    }
                })
                .map(|(k, _)| k)?;
            
            // Check if underlying is struct
            let underlying = vo_analysis::typ::underlying_type(type_key, tc_objs);
            if tc_objs.types[underlying].try_as_struct().is_some() {
                Some((type_key, obj_key, named_type_id))
            } else {
                None
            }
        })
        .collect();
    
    // For each struct, find all promoted methods by collecting from embedded fields recursively
    for (type_key, _obj_key, named_type_id) in named_structs {
        // Get package for unexported method lookup
        let pkg = tc_objs.types[type_key].try_as_named()
            .and_then(|n| n.obj().and_then(|obj_key| tc_objs.lobjs[obj_key].pkg()));
        
        // Collect all potential method names from embedded types (recursively)
        let mut method_names: std::collections::HashSet<String> = std::collections::HashSet::new();
        collect_embedded_method_names(type_key, tc_objs, &mut method_names);
        
        // For each method name, use lookup_field_or_method to find it and generate wrapper
        for method_name in method_names {
            // Skip if already registered (direct method)
            if ctx.get_method_from_named_type(named_type_id, &method_name).is_some() {
                continue;
            }
            
            // Look up method to get indices and determine wrapper type
            match lookup_field_or_method(type_key, true, pkg, &method_name, tc_objs) {
                LookupResult::Entry(obj_key, indices, _) => {
                    // Skip if not promoted (indices.len() == 1 means direct method)
                    if indices.len() <= 1 {
                        continue;
                    }
                    
                    let path_info = crate::embed::analyze_embed_path(type_key, &indices, tc_objs);
                    
                    let func_id = if let Some(embed_iface) = path_info.embedded_iface {
                        // Method from embedded interface
                        wrapper::generate_embedded_iface_wrapper(
                            ctx,
                            type_key,
                            embed_iface.offset,
                            embed_iface.iface_type,
                            &method_name,
                            obj_key,
                            tc_objs,
                            interner,
                        )
                    } else if let Some(base_iface_func) = ctx.get_iface_func_by_objkey(obj_key) {
                        // Method from embedded struct
                        let original_func_id = ctx.get_func_by_objkey(obj_key)
                            .unwrap_or(base_iface_func);
                        wrapper::generate_promoted_wrapper(
                            ctx,
                            type_key,
                            &indices[..indices.len()-1],
                            original_func_id,
                            base_iface_func,
                            &method_name,
                            tc_objs,
                        )
                    } else {
                        continue;
                    };
                    
                    let sig_rttid = tc_objs.lobjs[obj_key].typ()
                        .map(|sig_type| {
                            let sig = signature_type_to_runtime_type(sig_type, tc_objs, info, ctx);
                            ctx.intern_rttid(sig)
                        })
                        .unwrap_or(0);
                    
                    ctx.update_named_type_method_if_absent(
                        named_type_id,
                        method_name,
                        func_id,
                        false,
                        sig_rttid,
                    );
                }
                _ => {}
            }
        }
    }
}

/// Recursively collect method names from embedded fields.
fn collect_embedded_method_names(
    type_key: vo_analysis::objects::TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    method_names: &mut std::collections::HashSet<String>,
) {
    
    
    let underlying = vo_analysis::typ::underlying_type(type_key, tc_objs);
    let struct_detail = match tc_objs.types[underlying].try_as_struct() {
        Some(s) => s,
        None => return,
    };
    
    for &field_obj in struct_detail.fields() {
        let field = &tc_objs.lobjs[field_obj];
        if !field.var_embedded() {
            continue;
        }
        
        let field_type = match field.typ() {
            Some(t) => t,
            None => continue,
        };
        
        // Handle pointer embedding
        let base_type = if let Some(ptr) = tc_objs.types[field_type].try_as_pointer() {
            ptr.base()
        } else {
            field_type
        };
        
        let field_underlying = vo_analysis::typ::underlying_type(base_type, tc_objs);
        
        // Collect methods from interface
        if let Some(iface_detail) = tc_objs.types[field_underlying].try_as_interface() {
            let all_methods = iface_detail.all_methods();
            let methods: &[vo_analysis::objects::ObjKey] = match all_methods.as_ref() {
                Some(v) => v.as_slice(),
                None => iface_detail.methods(),
            };
            for &m in methods {
                method_names.insert(tc_objs.lobjs[m].name().to_string());
            }
        }
        
        // Collect methods from Named type and recurse into its embedded fields
        if let Some(named_detail) = tc_objs.types[base_type].try_as_named() {
            for &m in named_detail.methods() {
                method_names.insert(tc_objs.lobjs[m].name().to_string());
            }
            // Recurse into embedded fields of this type
            collect_embedded_method_names(base_type, tc_objs, method_names);
        }
    }
}

/// Generate RuntimeType::Func signature for a method (excluding receiver).
/// Uses Var types directly - type checker already converts variadic T to []T.
fn generate_method_signature(
    func_decl: &vo_syntax::ast::FuncDecl,
    info: &TypeInfoWrapper,
    ctx: &mut CodegenContext,
) -> vo_runtime::RuntimeType {
    use vo_runtime::{RuntimeType, ValueRttid};
    
    // Collect param ValueRttids from Var objects (not TypeExpr)
    // Type checker already changed variadic param type from T to []T
    let mut params = Vec::new();
    for param in &func_decl.sig.params {
        for name in &param.names {
            let obj_key = info.get_def(name);
            let type_key = info.obj_type(obj_key, "param must have type");
            let rttid = ctx.intern_type_key(type_key, info);
            let vk = info.type_value_kind(type_key);
            params.push(ValueRttid::new(rttid, vk));
        }
    }
    
    let mut results = Vec::new();
    for r in &func_decl.sig.results {
        let type_key = info.type_expr_type(r.ty.id);
        let rttid = ctx.intern_type_key(type_key, info);
        let vk = info.type_value_kind(type_key);
        results.push(ValueRttid::new(rttid, vk));
    }
    
    RuntimeType::Func { params, results, variadic: func_decl.sig.variadic }
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

/// Compile function body and replace placeholder at pre-allocated func_id.
fn compile_func_decl_at(
    func_decl: &vo_syntax::ast::FuncDecl,
    func_id: u32,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    ctx.set_current_func_id(func_id);
    let func_def = compile_func_body(func_decl, ctx, info)?;
    ctx.replace_function(func_id, func_def);
    Ok(())
}

/// Compile function body without pre-allocated func_id (for init() functions).
fn compile_func_decl_new(
    func_decl: &vo_syntax::ast::FuncDecl,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<u32, CodegenError> {
    let func_def = compile_func_body(func_decl, ctx, info)?;
    Ok(ctx.add_function(func_def))
}

fn compile_func_body(
    func_decl: &vo_syntax::ast::FuncDecl,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<vo_vm::bytecode::FunctionDef, CodegenError> {
    let name = info.project.interner.resolve(func_decl.name.symbol)
        .unwrap_or("unknown");
    
    let mut builder = FuncBuilder::new(name);
    
    // Define parameters and collect escaped ones for boxing
    let mut escaped_params = Vec::new();
    
    // Define receiver as first parameter (if method)
    if let Some(recv) = &func_decl.receiver {
        let (slots, slot_types) = if recv.is_pointer {
            (1, vec![vo_runtime::SlotType::GcRef])
        } else {
            let type_key = info.obj_type(info.get_use(&recv.ty), "method receiver must have type");
            (info.type_slot_count(type_key), info.type_slot_types(type_key))
        };
        
        builder.set_recv_slots(slots);
        builder.define_param(recv.name.as_ref().map(|n| n.symbol), slots, &slot_types);
        
        // Check if receiver escapes (e.g., captured by closure)
        if let Some(name) = &recv.name {
            let obj_key = info.get_def(name);
            let type_key = info.obj_type(obj_key, "receiver must have type");
            if info.needs_boxing(obj_key, type_key) {
                escaped_params.push((name.symbol, type_key, slots, slot_types.clone()));
            }
        }
    }
    
    let params = &func_decl.sig.params;
    for (i, param) in params.iter().enumerate() {
        let variadic_last = func_decl.sig.variadic && i == params.len() - 1;
        let (slots, slot_types) = if variadic_last { (1, vec![vo_runtime::SlotType::GcRef]) } else { info.type_expr_layout(param.ty.id) };
        for name in &param.names {
            let obj_key = info.get_def(name);
            let type_key = info.obj_type(obj_key, "param must have type");
            builder.define_param(Some(name.symbol), slots, &slot_types);
            if info.needs_boxing(obj_key, type_key) {
                escaped_params.push((name.symbol, type_key, slots, slot_types.clone()));
            }
        }
    }
    
    // Box escaped parameters: allocate heap storage and copy param values
    for (sym, type_key, slots, _slot_types) in escaped_params {
        if let Some((gcref_slot, param_slot)) = builder.box_escaped_param(sym, slots) {
            let meta_idx = ctx.get_or_create_value_meta(type_key, info);
            let meta_reg = builder.alloc_temp_typed(&[vo_runtime::SlotType::Value]);
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
    // Check if they escape (e.g. captured by defer closure)
    // Two-pass approach for escaped returns to ensure contiguous GcRef slots:
    // 1. First allocate all slots (so escaped GcRef slots are contiguous)
    // 2. Then emit PtrNew instructions
    struct EscapedReturn {
        gcref_slot: u16,
        slots: u16,
        result_type: vo_analysis::objects::TypeKey,
    }
    let mut escaped_returns: Vec<EscapedReturn> = Vec::new();
    
    for result in &func_decl.sig.results {
        if let Some(name) = &result.name {
            let result_type = info.type_expr_type(result.ty.id);
            let (slots, slot_types) = info.type_expr_layout(result.ty.id);
            let obj_key = info.get_def(name);
            let escapes = info.is_escaped(obj_key);
            
            let slot = if escapes {
                // Named return escapes - allocate GcRef slot only (PtrNew emitted later)
                let gcref_slot = builder.define_local_heap_boxed(name.symbol, slots);
                escaped_returns.push(EscapedReturn { gcref_slot, slots, result_type });
                gcref_slot
            } else {
                builder.define_local_stack(name.symbol, slots, &slot_types)
            };
            builder.register_named_return(slot, slots, escapes);
        }
    }
    
    // Now emit PtrNew for all escaped returns (after all GcRef slots are allocated contiguously)
    for er in escaped_returns {
        let meta_idx = ctx.get_or_create_value_meta(er.result_type, info);
        let meta_reg = builder.alloc_temp_typed(&[vo_runtime::SlotType::Value]);
        builder.emit_op(vo_vm::instruction::Opcode::LoadConst, meta_reg, meta_idx, 0);
        builder.emit_with_flags(vo_vm::instruction::Opcode::PtrNew, er.slots as u8, er.gcref_slot, meta_reg, 0);
    }
    
    // Compile function body
    if let Some(body) = &func_decl.body {
        stmt::compile_block(body, ctx, &mut builder, info)?;
    }
    
    // Add return if not present at end
    builder.emit_op(vo_vm::instruction::Opcode::Return, 0, 0, 0);
    
    // Build and return FunctionDef
    Ok(builder.build())
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
    let gcref_slot = func.alloc_temp_typed(&[vo_runtime::SlotType::GcRef]);
    let meta_reg = func.alloc_temp_typed(&[vo_runtime::SlotType::Value]);
    let len_reg = func.alloc_temp_typed(&[vo_runtime::SlotType::Value]);
    
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
        let idx_reg = func.alloc_temp_typed(&[vo_runtime::SlotType::Value]);
        
        for (i, elem) in lit.elems.iter().enumerate() {
            crate::expr::compile_expr_to(&elem.value, tmp_elem, ctx, func, info)?;
            func.emit_op(Opcode::LoadInt, idx_reg, i as u16, 0);
            func.emit_array_set(gcref_slot, idx_reg, tmp_elem, elem_bytes as usize, elem_vk, ctx);
        }
    }
    
    // Store GcRef in global (1 slot)
    func.emit_op(Opcode::GlobalSet, global_idx as u16, gcref_slot, 0);
    
    Ok(())
}

/// Compile global variable initialization for a single package.
fn compile_package_globals(
    ctx: &mut CodegenContext,
    init_builder: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Initialize global variables in dependency order (from type checker analysis)
    for initializer in info.init_order() {
        // Each initializer has lhs (variables) and rhs (expression)
        // For now, handle single variable assignment (most common case)
        if initializer.lhs.len() == 1 {
            let obj_key = initializer.lhs[0];
            let obj = &info.project.tc_objs.lobjs[obj_key];
            
            if let Some(global_idx) = ctx.get_global_index(obj_key) {
                let type_key = obj.typ();
                
                // Special handling for arrays: allocate on heap
                if let Some(tk) = type_key {
                    if info.is_array(tk) {
                        compile_global_array_init(
                            &initializer.rhs, tk, global_idx,
                            ctx, init_builder, info
                        )?;
                        continue;
                    }
                    
                    // Special handling for interface: use compile_iface_assign for proper itab setup
                    if info.is_interface(tk) {
                        let tmp = init_builder.alloc_temp_typed(&[vo_runtime::SlotType::Interface0, vo_runtime::SlotType::Interface1]);
                        crate::stmt::compile_iface_assign(tmp, &initializer.rhs, tk, ctx, init_builder, info)?;
                        emit_global_set(init_builder, global_idx, tmp, 2);
                        continue;
                    }
                }
                
                let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                let slot_types = type_key
                    .map(|t| info.type_slot_types(t))
                    .unwrap_or_else(|| vec![vo_runtime::SlotType::Value]);
                
                let tmp = init_builder.alloc_temp_typed(&slot_types);
                crate::expr::compile_expr_to(&initializer.rhs, tmp, ctx, init_builder, info)?;
                emit_global_set(init_builder, global_idx, tmp, slots);
            }
        } else {
            // Multi-variable assignment: var a, b = expr
            // TODO: handle tuple unpacking if needed
            for (i, &obj_key) in initializer.lhs.iter().enumerate() {
                let obj = &info.project.tc_objs.lobjs[obj_key];
                
                if let Some(global_idx) = ctx.get_global_index(obj_key) {
                    let type_key = obj.typ();
                    let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                    let slot_types = type_key
                        .map(|t| info.type_slot_types(t))
                        .unwrap_or_else(|| vec![vo_runtime::SlotType::Value]);
                    
                    // For multi-var, compile rhs once and extract values
                    // For now, just compile rhs for each (inefficient but correct)
                    let tmp = init_builder.alloc_temp_typed(&slot_types);
                    crate::expr::compile_expr_to(&initializer.rhs, tmp, ctx, init_builder, info)?;
                    emit_global_set(init_builder, global_idx, tmp, slots);
                }
                let _ = i; // suppress unused warning
            }
        }
    }
    Ok(())
}

fn compile_init_and_entry(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // 1. Generate __init__ function for global variable initialization
    let mut init_builder = FuncBuilder::new("__init__");
    
    // Initialize imported packages' global variables in dependency order
    // (dependencies are initialized before dependents)
    for (_, pkg_type_info) in project.imported_packages_in_order() {
        let pkg_info = TypeInfoWrapper::for_package(project, pkg_type_info);
        compile_package_globals(ctx, &mut init_builder, &pkg_info)?;
    }
    
    // Then, initialize main package's global variables
    compile_package_globals(ctx, &mut init_builder, info)?;
    
    // Add return
    init_builder.emit_op(vo_vm::instruction::Opcode::Return, 0, 0, 0);
    let init_func = init_builder.build();
    let init_func_id = ctx.add_function(init_func);
    // Note: __init__ is NOT registered as a user init function - it's handled separately
    
    // 2. Find main function
    let main_func_id = ctx.main_func_id();
    
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
