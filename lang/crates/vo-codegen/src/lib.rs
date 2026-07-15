//! Vo bytecode code generation.
//!
//! This crate compiles type-checked AST to VM bytecode.

mod array_value;
mod assign;
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
pub use type_interner::{intern_type_key, TypeInterner};

use vo_analysis::Project;
use vo_runtime::bytecode::Module;
use vo_syntax::ast::{Decl, Visitor};

/// Compile a type-checked project to VM bytecode.
pub fn compile_project(project: &Project) -> Result<Module, CodegenError> {
    if let Some(declared) = project
        .main_pkg()
        .name()
        .as_deref()
        .filter(|name| *name != "main")
    {
        return Err(CodegenError::InvalidEntry(format!(
            "package must be named `main`, found `{declared}`"
        )));
    }
    let layout_facts = validate_project_type_layouts(project)?;
    let info = TypeInfoWrapper::for_main_package(project, layout_facts);
    let pkg_name = project.main_pkg().name().as_deref().unwrap_or("main");
    let mut ctx = CodegenContext::new(pkg_name);

    // 1. Register types (StructMeta, InterfaceMeta)
    register_types(project, &mut ctx, &info)?;
    ctx.check_layout_errors().map_err(CodegenError::Internal)?;

    // 2. Collect declarations (functions, globals, externs)
    collect_declarations(project, &mut ctx, &info)?;
    ctx.check_layout_errors().map_err(CodegenError::Internal)?;

    // 3. Compile functions
    compile_functions(project, &mut ctx, &info)?;
    ctx.check_layout_errors().map_err(CodegenError::Internal)?;

    // 4. Generate __init__ and __entry__
    compile_init_and_entry(project, &mut ctx, &info)?;
    ctx.check_layout_errors().map_err(CodegenError::Internal)?;

    // 5. Collect promoted methods from embedded interfaces
    // This must happen after compile_functions (direct methods registered)
    // and before finalize_itabs (itabs need complete method set)
    collect_promoted_methods(project, &mut ctx, &info);

    // 6. Build all pending itabs (from functions + __init__)
    ctx.finalize_itabs(&info.project.tc_objs, &info.project.interner);

    // 7. Build runtime_types after all codegen (all types have been assigned rttid)
    build_runtime_types(project, &mut ctx, &info);
    ctx.check_layout_errors().map_err(CodegenError::Internal)?;

    // 8. Reconcile generated transfer metadata against the final runtime type table.
    ctx.finalize_transfer_metadata()
        .map_err(CodegenError::Internal)?;

    // 9. Fill WellKnownTypes for fast error creation
    ctx.fill_well_known_types()
        .map_err(CodegenError::Internal)?;

    // 10. Finalize debug info (sort entries by PC)
    ctx.finalize_debug_info().map_err(CodegenError::Internal)?;

    // 11. Final check: all IDs within 24-bit limit
    ctx.check_id_limits().map_err(CodegenError::Internal)?;

    Ok(ctx.finish())
}

/// Validate arena layout metadata and the child types of intrinsically flat
/// runtime ABIs before codegen allocates any slot vectors. Concrete locals,
/// globals, and captures validate after choosing stack or canonical heap
/// storage, so metadata-only synthetic tuples cannot reject a valid program.
fn validate_project_type_layouts(
    project: &Project,
) -> Result<vo_analysis::check::type_info::TypeLayoutFacts, CodegenError> {
    use std::collections::HashSet;
    use vo_analysis::check::type_info::try_all_type_slot_counts;
    use vo_analysis::typ::Type;

    let counts = try_all_type_slot_counts(&project.tc_objs)
        .map_err(|error| CodegenError::Internal(error.to_string()))?;
    // The arena contains synthetic tuples and metadata-only types that never
    // occupy a VM frame. Enforce u16 only for constructors whose runtime ABI
    // intrinsically flattens their child values; local/global/closure storage
    // makes its own representation decision at the concrete lowering site.
    let mut required_flat_types = HashSet::new();
    for (_, typ) in project.tc_objs.types.iter() {
        match typ {
            Type::Map(map) => {
                required_flat_types.insert(map.key());
                required_flat_types.insert(map.elem());
            }
            Type::Slice(slice) => {
                required_flat_types.insert(slice.elem());
            }
            Type::Chan(chan) => {
                required_flat_types.insert(chan.elem());
            }
            Type::Port(port) => {
                required_flat_types.insert(port.elem());
            }
            _ => {}
        }
    }

    let mut ordered_counts: Vec<_> = counts.iter().collect();
    ordered_counts.sort_by_key(|(type_key, _)| type_key.raw());
    for (type_key, slots) in ordered_counts {
        if slots <= usize::from(u16::MAX) || slots == 0 {
            continue;
        }
        if required_flat_types.contains(&type_key) {
            return Err(CodegenError::Internal(format!(
                "type slot count exceeds u16::MAX: {slots} slots for {type_key:?}"
            )));
        }
    }
    Ok(counts)
}

fn register_types(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use std::collections::BTreeMap;
    use vo_runtime::bytecode::{InterfaceMeta, NamedTypeMeta};
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    fn register_pkg_types(
        pkg_path: &str,
        files: &[vo_syntax::ast::File],
        project: &Project,
        ctx: &mut CodegenContext,
        info: &TypeInfoWrapper,
    ) -> Result<(), CodegenError> {
        use std::collections::BTreeMap;
        use vo_runtime::bytecode::{InterfaceMeta, NamedTypeMeta, StructMeta};
        use vo_runtime::ValueMeta;
        use vo_syntax::ast::TypeExprKind;

        fn checked_struct_offset(offset: u16, slot_count: u16) -> Result<u16, CodegenError> {
            offset.checked_add(slot_count).ok_or_else(|| {
                CodegenError::Internal(format!(
                    "type slot count exceeds u16::MAX: {} slots",
                    offset as usize + slot_count as usize
                ))
            })
        }

        fn named_type_identity(
            pkg_path: &str,
            type_name: &str,
            type_decl: &vo_syntax::ast::TypeDecl,
            obj_key: vo_analysis::objects::ObjKey,
            project: &Project,
        ) -> Result<String, CodegenError> {
            let object = &project.tc_objs.lobjs[obj_key];
            let package = object
                .pkg()
                .and_then(|package_key| project.tc_objs.pkgs.get(package_key))
                .ok_or_else(|| {
                    CodegenError::Internal(format!(
                        "source type declaration {type_name:?} has no owning package"
                    ))
                })?;
            if package.path() != pkg_path {
                return Err(CodegenError::Internal(format!(
                    "source type declaration {type_name:?} belongs to package {:?}, registered as {pkg_path:?}",
                    package.path()
                )));
            }
            if object.parent() == Some(*package.scope()) {
                let identity = format!("{pkg_path}.{type_name}");
                if identity.len() > vo_common_core::MAX_NAMED_TYPE_IDENTITY_BYTES {
                    return Err(CodegenError::TargetLimit(format!(
                        "named type identity is {} bytes, exceeding the {}-byte limit",
                        identity.len(),
                        vo_common_core::MAX_NAMED_TYPE_IDENTITY_BYTES
                    )));
                }
                return Ok(identity);
            }

            let declaration_span = type_decl.name.span;
            // Project analysis admits one package directory at a time and
            // rejects portable spelling collisions, so this canonical base
            // name is unique within the package. The file-local offset then
            // remains stable when unrelated source files are added or moved
            // in the deterministic parse order.
            let source_file = project
                .source_map
                .lookup_span(declaration_span)
                .ok_or_else(|| {
                    CodegenError::Internal(format!(
                        "local type declaration {type_name:?} has no canonical source file"
                    ))
                })?;
            let source_offset = source_file
                .try_local_offset(declaration_span.start)
                .ok_or_else(|| {
                    CodegenError::Internal(format!(
                        "local type declaration {type_name:?} is outside its source file"
                    ))
                })?;
            match vo_common_core::identifier::build_local_type_identity(
                pkg_path,
                source_file.name(),
                source_offset,
                type_name,
            ) {
                Ok(identity) => Ok(identity),
                Err(vo_common_core::LocalTypeIdentityError::TooLong { len, max }) => {
                    Err(CodegenError::TargetLimit(format!(
                        "local type identity is {len} bytes, exceeding the {max}-byte limit"
                    )))
                }
                Err(error) => Err(CodegenError::Internal(format!(
                    "cannot build local type identity for {type_name:?}: {error}"
                ))),
            }
        }

        // The syntax visitor follows every executable expression, including
        // function literals nested in initializers and communication clauses.
        let type_decls = collect_type_decls(files);

        // Process all collected type declarations
        // Named types are dynamically registered during intern_type_key if not already present
        for type_decl in &type_decls {
            if type_decl.is_alias {
                continue;
            }
            let type_name = project
                .interner
                .resolve(type_decl.name.symbol)
                .unwrap_or("?");

            // Get underlying type key from type expression, and obj_key from declaration name
            let underlying_key = info.type_expr_type(type_decl.ty.id);
            let obj_key = info.get_def(&type_decl.name);
            let named_key = info.obj_type(obj_key, "type declaration must have type");
            let qualified_type_name =
                named_type_identity(pkg_path, type_name, type_decl, obj_key, project)?;

            // Register type-specific metadata first
            let underlying_meta = match &type_decl.ty.kind {
                TypeExprKind::Struct(struct_type) => {
                    // Build StructMeta with FieldMeta
                    let mut fields = Vec::new();
                    let mut slot_types = Vec::new();
                    let mut offset = 0u16;

                    for field in &struct_type.fields {
                        let field_type = info.type_expr_type(field.ty.id);
                        let slot_count = info
                            .try_type_slot_count(field_type)
                            .map_err(CodegenError::Internal)?;
                        let slot_type_list = info.type_slot_types(field_type);
                        let field_vk = info.type_value_kind(field_type);
                        let field_rttid = ctx.intern_type_key(field_type, info);
                        let mut tag = field.tag.as_ref().map(|t| t.value.clone());

                        if field.names.is_empty() {
                            let next_offset = checked_struct_offset(offset, slot_count)?;
                            // Embedded field: name comes from the type
                            let field_name = info.get_type_name(field_type);
                            slot_types.extend(slot_type_list);
                            fields.push(vo_runtime::bytecode::FieldMeta {
                                name: field_name,
                                offset,
                                slot_count,
                                type_info: vo_runtime::ValueRttid::new(field_rttid, field_vk),
                                embedded: true,
                                tag,
                            });
                            offset = next_offset;
                        } else {
                            // Named field(s) - tag is shared among all names
                            let names_count = field.names.len();
                            for (i, name) in field.names.iter().enumerate() {
                                let next_offset = checked_struct_offset(offset, slot_count)?;
                                let field_name = project
                                    .interner
                                    .resolve(name.symbol)
                                    .unwrap_or("?")
                                    .to_string();
                                slot_types.extend(slot_type_list.clone());
                                fields.push(vo_runtime::bytecode::FieldMeta {
                                    name: field_name,
                                    offset,
                                    slot_count,
                                    type_info: vo_runtime::ValueRttid::new(field_rttid, field_vk),
                                    embedded: false,
                                    tag: if i == names_count - 1 {
                                        tag.take()
                                    } else {
                                        tag.clone()
                                    },
                                });
                                offset = next_offset;
                            }
                        }
                    }

                    // Empty struct still needs 1 slot for zero-size type workaround
                    if slot_types.is_empty() {
                        slot_types.push(vo_runtime::SlotType::Value);
                    }
                    let field_index: std::collections::HashMap<String, usize> = fields
                        .iter()
                        .enumerate()
                        .filter(|(_, field)| field.name != "_")
                        .map(|(i, f)| (f.name.clone(), i))
                        .collect();
                    let meta = StructMeta {
                        slot_types,
                        fields,
                        field_index,
                    };
                    let struct_meta_id = ctx
                        .register_struct_meta(underlying_key, meta)
                        .map_err(CodegenError::Internal)?;
                    ctx.alias_struct_meta_id(named_key, struct_meta_id)
                        .map_err(CodegenError::Internal)?;
                    ValueMeta::new(struct_meta_id, vo_runtime::ValueKind::Struct)
                }
                TypeExprKind::Interface(_) => {
                    // Build InterfaceMeta
                    let tc_objs = &info.project.tc_objs;
                    let (method_names, methods) = if let vo_analysis::typ::Type::Interface(iface) =
                        &tc_objs.types[underlying_key]
                    {
                        let all_methods_ref = iface.all_methods();
                        let method_objs: Vec<vo_analysis::objects::ObjKey> =
                            if let Some(methods) = all_methods_ref.as_ref() {
                                methods.to_vec()
                            } else {
                                iface.methods().to_vec()
                            };

                        let names: Vec<String> = method_objs
                            .iter()
                            .map(|m| tc_objs.lobjs[*m].id(tc_objs).into_owned())
                            .collect();

                        let metas: Vec<vo_runtime::bytecode::InterfaceMethodMeta> = method_objs
                            .iter()
                            .map(|&m| {
                                let obj = &tc_objs.lobjs[m];
                                let name = obj.id(tc_objs).into_owned();
                                let sig_type = obj.typ().unwrap_or_else(|| {
                                    panic!("interface method {name} is missing signature type")
                                });
                                let sig =
                                    signature_type_to_runtime_type(sig_type, tc_objs, info, ctx);
                                let signature_rttid = ctx.intern_rttid(sig);
                                vo_runtime::bytecode::InterfaceMethodMeta {
                                    name,
                                    signature_rttid,
                                }
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
                    let iface_meta_id = ctx
                        .register_interface_meta(underlying_key, meta)
                        .map_err(CodegenError::Internal)?;
                    ValueMeta::new(iface_meta_id, vo_runtime::ValueKind::Interface)
                }
                _ => {
                    // Other types (Map, Slice, Chan, etc.): intern underlying type to get rttid
                    let underlying_vk = info.type_value_kind(underlying_key);
                    let underlying_rt = info.type_to_runtime_type(underlying_key, ctx);
                    let underlying_rttid = ctx.intern_rttid(underlying_rt);
                    ValueMeta::new(underlying_rttid, underlying_vk)
                }
            };
            let underlying_rttid = vo_runtime::ValueRttid::new(
                ctx.intern_type_key(underlying_key, info),
                info.type_value_kind(underlying_key),
            );

            // All named types get NamedTypeMeta (keyed by ObjKey, the true identity)
            let named_type_meta = NamedTypeMeta {
                name: qualified_type_name.to_string(),
                underlying_meta,
                underlying_rttid,
                methods: BTreeMap::new(),
            };
            ctx.register_named_type_meta(obj_key, named_type_meta)
                .map_err(CodegenError::Internal)?;
        }

        // Finalize runtime types: fill meta_id fields after all types are registered
        ctx.finalize_runtime_types();
        ctx.finalize_named_type_underlying_meta();

        Ok(())
    }

    // Ensure builtin error interface has a stable InterfaceMeta name for runtime lookup.
    {
        use vo_analysis::typ;
        let tc_objs = &project.tc_objs;
        let error_type = tc_objs.universe().error_type();
        let underlying = typ::underlying_type(error_type, tc_objs);
        if ctx.get_interface_meta_id(underlying).is_none() {
            let (method_names, methods) =
                if let vo_analysis::typ::Type::Interface(iface) = &tc_objs.types[underlying] {
                    let all_methods_ref = iface.all_methods();
                    let method_objs: Vec<vo_analysis::objects::ObjKey> =
                        if let Some(methods) = all_methods_ref.as_ref() {
                            methods.to_vec()
                        } else {
                            iface.methods().to_vec()
                        };

                    let names: Vec<String> = method_objs
                        .iter()
                        .map(|m| tc_objs.lobjs[*m].id(tc_objs).into_owned())
                        .collect();

                    let metas: Vec<vo_runtime::bytecode::InterfaceMethodMeta> = method_objs
                        .iter()
                        .map(|&m| {
                            let obj = &tc_objs.lobjs[m];
                            let name = obj.id(tc_objs).into_owned();
                            let sig_type = obj.typ().unwrap_or_else(|| {
                                panic!("interface method {name} is missing signature type")
                            });
                            let sig = signature_type_to_runtime_type(sig_type, tc_objs, info, ctx);
                            let signature_rttid = ctx.intern_rttid(sig);
                            vo_runtime::bytecode::InterfaceMethodMeta {
                                name,
                                signature_rttid,
                            }
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
            ctx.register_interface_meta(underlying, meta)
                .map_err(CodegenError::Internal)?;
        }
        let iface_meta_id = ctx
            .get_interface_meta_id(underlying)
            .expect("builtin error interface metadata must be registered");
        let error_obj = match &tc_objs.types[error_type] {
            vo_analysis::typ::Type::Named(named) => named
                .obj()
                .expect("builtin error named type must have a type object"),
            _ => panic!("builtin error type must be named"),
        };
        let underlying_rttid =
            ValueRttid::new(ctx.intern_type_key(underlying, info), ValueKind::Interface);
        ctx.register_named_type_meta(
            error_obj,
            NamedTypeMeta {
                name: "error".to_string(),
                underlying_meta: ValueMeta::new(iface_meta_id, ValueKind::Interface),
                underlying_rttid,
                methods: BTreeMap::new(),
            },
        )
        .map_err(CodegenError::Internal)?;
    }

    // Register builtin protocol interfaces (DynAttr, DynSetAttr, etc.)
    // These don't depend on user imports - they're language-level semantics.
    register_builtin_protocols(project, ctx, info)?;

    register_pkg_types(
        project.main_pkg().path(),
        &project.files,
        project,
        ctx,
        info,
    )?;

    for (pkg_path, pkg, pkg_type_info, files) in project
        .imported_packages_in_order()
        .map_err(CodegenError::Internal)?
    {
        let pkg_info =
            TypeInfoWrapper::for_package(project, pkg, pkg_type_info, info.shared_layout_facts());
        register_pkg_types(pkg_path, files, project, ctx, &pkg_info)?;
    }
    ctx.finalize_runtime_types();
    ctx.finalize_named_type_underlying_meta();

    Ok(())
}

/// Register builtin protocol interfaces (DynAttr, DynSetAttr, etc.)
/// These enable protocol dispatch for ~> operator without requiring import "dyn".
fn register_builtin_protocols(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_runtime::bytecode::{InterfaceMeta, InterfaceMethodMeta};
    use vo_runtime::{RuntimeType, ValueKind, ValueRttid};

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
    let make_sig_rttid = |ctx: &mut CodegenContext,
                          params: Vec<ValueRttid>,
                          results: Vec<ValueRttid>,
                          variadic: bool|
     -> u32 {
        let rt = RuntimeType::Func {
            params,
            results,
            variadic,
        };
        ctx.intern_rttid(rt)
    };

    // 1. AttrObject: DynAttr(name string) (any, error)
    let attr_sig_rttid =
        make_sig_rttid(ctx, vec![string_rttid], vec![any_rttid, error_rttid], false);
    let attr_meta = InterfaceMeta {
        name: "AttrObject".to_string(),
        method_names: vec!["DynAttr".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "DynAttr".to_string(),
            signature_rttid: attr_sig_rttid,
        }],
    };
    ctx.register_builtin_protocol("AttrObject", attr_meta)
        .map_err(CodegenError::Internal)?;

    // 2. SetAttrObject: DynSetAttr(name string, value any) error
    let set_attr_sig_rttid =
        make_sig_rttid(ctx, vec![string_rttid, any_rttid], vec![error_rttid], false);
    let set_attr_meta = InterfaceMeta {
        name: "SetAttrObject".to_string(),
        method_names: vec!["DynSetAttr".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "DynSetAttr".to_string(),
            signature_rttid: set_attr_sig_rttid,
        }],
    };
    ctx.register_builtin_protocol("SetAttrObject", set_attr_meta)
        .map_err(CodegenError::Internal)?;

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
    ctx.register_builtin_protocol("IndexObject", index_meta)
        .map_err(CodegenError::Internal)?;

    // 4. SetIndexObject: DynSetIndex(key any, value any) error
    let set_index_sig_rttid =
        make_sig_rttid(ctx, vec![any_rttid, any_rttid], vec![error_rttid], false);
    let set_index_meta = InterfaceMeta {
        name: "SetIndexObject".to_string(),
        method_names: vec!["DynSetIndex".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "DynSetIndex".to_string(),
            signature_rttid: set_index_sig_rttid,
        }],
    };
    ctx.register_builtin_protocol("SetIndexObject", set_index_meta)
        .map_err(CodegenError::Internal)?;

    // 5. CallObject: DynCall(args ...any) (any, error)
    // Variadic signature: the param is []any (slice of any), not any itself
    let slice_any_rttid_value = ctx.intern_rttid(RuntimeType::Slice(any_rttid));
    let slice_any_rttid = ValueRttid::new(slice_any_rttid_value, ValueKind::Slice);
    let call_sig_rttid = make_sig_rttid(
        ctx,
        vec![slice_any_rttid],
        vec![any_rttid, error_rttid],
        true,
    );
    let call_meta = InterfaceMeta {
        name: "CallObject".to_string(),
        method_names: vec!["DynCall".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "DynCall".to_string(),
            signature_rttid: call_sig_rttid,
        }],
    };
    ctx.register_builtin_protocol("CallObject", call_meta)
        .map_err(CodegenError::Internal)?;
    Ok(())
}

fn collect_type_decls(files: &[vo_syntax::ast::File]) -> Vec<vo_syntax::ast::TypeDecl> {
    #[derive(Default)]
    struct Collector {
        declarations: Vec<vo_syntax::ast::TypeDecl>,
    }

    impl Visitor for Collector {
        fn visit_decl(&mut self, decl: &Decl) {
            if let Decl::Type(type_decl) = decl {
                self.declarations.push(type_decl.clone());
            }
            vo_syntax::ast::walk_decl(self, decl);
        }
    }

    let mut collector = Collector::default();
    for file in files {
        collector.visit_file(file);
    }
    collector.declarations
}

fn build_runtime_types(_project: &Project, ctx: &mut CodegenContext, _info: &TypeInfoWrapper) {
    // RuntimeTypes are built during codegen via intern_rttid(). Codegen can
    // intern additional named runtime types after register_types has already
    // canonicalized the initial set, so run the canonicalization pass here
    // once all late interning is complete.
    ctx.finalize_runtime_types();
    ctx.finalize_named_type_underlying_meta();
}

fn collect_declarations(
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // First collect main package declarations (using main type_info)
    for file in &project.files {
        collect_file_declarations(file, project, ctx, info, true)?;
    }

    // Then collect imported package declarations in dependency order.
    for (_, pkg, pkg_type_info, files) in project
        .imported_packages_in_order()
        .map_err(CodegenError::Internal)?
    {
        let pkg_info =
            TypeInfoWrapper::for_package(project, pkg, pkg_type_info, info.shared_layout_facts());
        for file in files {
            collect_file_declarations(file, project, ctx, &pkg_info, false)?;
        }
    }
    Ok(())
}

fn collect_file_declarations(
    file: &vo_syntax::ast::File,
    project: &Project,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
    is_entry_package: bool,
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
                let func_name = project
                    .interner
                    .resolve(func_decl.name.symbol)
                    .unwrap_or("");
                if func_decl.receiver.is_none() && func_name == "init" {
                    continue;
                }

                // Check if this is a method (has receiver)
                let (recv_type, is_pointer_recv) = if let Some(recv) = &func_decl.receiver {
                    let base_type = info
                        .method_receiver_base_type(func_decl)
                        .expect("method receiver must have type");
                    (Some(base_type), recv.is_pointer)
                } else {
                    (None, false)
                };

                let obj_key = info.get_def(&func_decl.name);
                ctx.declare_func(
                    recv_type,
                    is_pointer_recv,
                    func_decl.name.symbol,
                    obj_key,
                    func_name,
                    is_entry_package,
                );
            }
            Decl::Var(var_decl) => {
                // Register global variables (so functions can reference them)
                for spec in &var_decl.specs {
                    for name in &spec.names {
                        let global_name = project.interner.resolve(name.symbol).unwrap_or("?");
                        // The blank identifier still participates in RHS
                        // evaluation and conversion, but it never denotes
                        // storage and must not consume a module global slot.
                        if global_name == "_" {
                            continue;
                        }
                        // The checker object is authoritative for each LHS name.
                        // A declaration such as `var a, b = f()` has one
                        // tuple-valued RHS, so indexing the RHS by LHS position
                        // loses the element types and panics for `b`.
                        let obj_key = info.get_def(name);
                        let type_key =
                            info.obj_type(obj_key, "package variable must have a checked type");

                        // Addressable aggregate globals own one stable GC
                        // allocation. Arrays use their canonical ArrayRef;
                        // structs use an ordinary typed Ptr allocation.
                        let (slots, slot_types) =
                            if info.is_array(type_key) || info.is_struct(type_key) {
                                (1, vec![vo_runtime::SlotType::GcRef])
                            } else {
                                (
                                    info.type_slot_count(type_key),
                                    info.type_slot_types(type_key),
                                )
                            };
                        let value_kind = info.type_value_kind(type_key) as u8;
                        let meta_id = ctx.compute_value_meta_raw(type_key, info) >> 8;
                        ctx.register_global(
                            obj_key,
                            vo_runtime::bytecode::GlobalDef {
                                name: global_name.to_string(),
                                slots,
                                value_kind,
                                meta_id,
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
    // (recv_type, method_name, func_id, is_pointer_receiver, receiver_is_iface_boxed, signature)
    let mut method_mappings: Vec<(vo_analysis::objects::TypeKey, String, u32, bool, bool, u32)> =
        Vec::new();

    // Note: Functions are already pre-registered by collect_declarations via declare_func.
    // This handles forward references (function A calls function B defined later).

    // Compile function bodies - main package files first
    for file in &project.files {
        compile_file_functions(file, project, ctx, info, &mut method_mappings)?;
    }
    // Then imported package files in dependency order.
    for (_, pkg, pkg_type_info, files) in project
        .imported_packages_in_order()
        .map_err(CodegenError::Internal)?
    {
        let pkg_info =
            TypeInfoWrapper::for_package(project, pkg, pkg_type_info, info.shared_layout_facts());
        for file in files {
            compile_file_functions(file, project, ctx, &pkg_info, &mut method_mappings)?;
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
    method_mappings: &mut Vec<(vo_analysis::objects::TypeKey, String, u32, bool, bool, u32)>,
) -> Result<(), CodegenError> {
    for decl in &file.decls {
        if let Decl::Func(func_decl) = decl {
            // Skip extern functions (no body) - they use CallExtern
            if func_decl.body.is_none() {
                continue;
            }

            let func_name = project
                .interner
                .resolve(func_decl.name.symbol)
                .unwrap_or("unknown");

            // init() functions are not pre-declared (can have multiple with same name)
            let is_init = func_decl.receiver.is_none() && func_name == "init";
            let func_obj_key = info.get_def(&func_decl.name);

            let func_id = if is_init {
                // Compile init() directly (not pre-declared)
                let id = compile_func_decl_new(func_decl, ctx, info)?;
                ctx.register_init_function(info.package_key(), id);
                id
            } else {
                // Get pre-declared func_id using ObjKey (not func_indices which can collide across packages)
                let id = ctx.get_func_by_objkey(func_obj_key).ok_or_else(|| {
                    CodegenError::Internal(format!(
                        "function not pre-declared: {} obj_key={:?}",
                        func_name, func_obj_key
                    ))
                })?;

                // Compile function body and replace placeholder
                compile_func_decl_at(func_decl, id, ctx, info)?;
                id
            };

            // If this is a method, record the mapping
            if let Some(recv) = &func_decl.receiver {
                // Get receiver base type from function signature
                let recv_type = info
                    .method_receiver_base_type(func_decl)
                    .expect("method receiver must have type");
                {
                    let method_name = info.project.tc_objs.lobjs[func_obj_key]
                        .id(&info.project.tc_objs)
                        .into_owned();

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
                    method_mappings.push((
                        recv_type,
                        method_name,
                        iface_func_id,
                        recv.is_pointer,
                        !recv.is_pointer && info.type_value_kind(recv_type).needs_boxing(),
                        signature_rttid,
                    ));
                }
            }
        }
    }
    Ok(())
}

fn update_named_type_methods(
    ctx: &mut CodegenContext,
    method_mappings: &[(vo_analysis::objects::TypeKey, String, u32, bool, bool, u32)],
    info: &TypeInfoWrapper,
) {
    use vo_analysis::typ::Type;
    let tc_objs = &info.project.tc_objs;

    // Group methods by type_key, lookup ObjKey from Named type
    for (
        type_key,
        method_name,
        func_id,
        is_pointer_receiver,
        receiver_is_iface_boxed,
        signature_rttid,
    ) in method_mappings
    {
        // Get ObjKey from the Named type (the true identity)
        if let Type::Named(named) = &tc_objs.types[*type_key] {
            if let Some(obj_key) = named.obj() {
                if let Some(named_type_id) = ctx.get_named_type_id(*obj_key) {
                    ctx.update_named_type_method(
                        named_type_id,
                        method_name.clone(),
                        *func_id,
                        *is_pointer_receiver,
                        *receiver_is_iface_boxed,
                        *signature_rttid,
                    );
                }
            }
        }
    }
}

/// Collect promoted methods from embedded fields (interfaces and structs) for all struct types.
/// This generates wrapper functions and registers them to NamedTypeMeta.methods,
/// so that runtime itab building can find them.
fn collect_promoted_methods(_project: &Project, ctx: &mut CodegenContext, info: &TypeInfoWrapper) {
    use vo_analysis::lookup::{lookup_field_or_method, LookupResult};
    use vo_analysis::typ::Type;

    let tc_objs = &info.project.tc_objs;
    let interner = &info.project.interner;

    // Collect all (type_key, obj_key, named_type_id) for Named struct types
    let named_structs: Vec<_> = ctx
        .all_named_type_ids()
        .into_iter()
        .filter_map(|(obj_key, named_type_id)| {
            // Find the type_key for this obj_key
            let type_key = tc_objs
                .types
                .iter()
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
        // Collect all potential methods from embedded types (recursively).
        // Private methods use package-qualified object identity so methods with
        // the same spelling from different packages remain distinct.
        let mut method_objects: std::collections::BTreeMap<String, vo_analysis::objects::ObjKey> =
            std::collections::BTreeMap::new();
        collect_embedded_methods(type_key, tc_objs, &mut method_objects);

        // For each method identity, use ordinary source spelling for lookup and
        // retain the qualified identity in runtime metadata.
        for (method_identity, embedded_method) in method_objects {
            let method_obj = &tc_objs.lobjs[embedded_method];
            let method_name = method_obj.name().to_string();
            // Skip if already registered (direct method)
            if ctx
                .get_method_from_named_type(named_type_id, &method_identity)
                .is_some()
            {
                continue;
            }

            // Look up method to get indices and determine wrapper type
            if let LookupResult::Entry(obj_key, indices, _) =
                lookup_field_or_method(type_key, true, method_obj.pkg(), &method_name, tc_objs)
            {
                // Skip if not promoted (indices.len() == 1 means direct method)
                if indices.len() <= 1 {
                    continue;
                }

                let path_info = crate::embed::analyze_embed_path(type_key, &indices, tc_objs);

                let func_id = if let Some(iface_type) = path_info.embedded_iface_type {
                    // Method from embedded interface
                    wrapper::generate_embedded_iface_wrapper(
                        ctx,
                        &path_info,
                        iface_type,
                        &method_name,
                        obj_key,
                        info,
                        tc_objs,
                        interner,
                    )
                } else if let Some(base_iface_func) = ctx.get_iface_func_by_objkey(obj_key) {
                    // Method from embedded struct
                    let original_func_id =
                        ctx.get_func_by_objkey(obj_key).unwrap_or(base_iface_func);
                    wrapper::generate_promoted_wrapper(
                        ctx,
                        type_key,
                        &indices[..indices.len() - 1],
                        obj_key,
                        original_func_id,
                        base_iface_func,
                        &method_name,
                        info,
                        tc_objs,
                    )
                } else {
                    continue;
                };

                let sig_type = tc_objs.lobjs[obj_key].typ().unwrap_or_else(|| {
                    panic!("promoted method {method_name} is missing signature type")
                });
                let sig = signature_type_to_runtime_type(sig_type, tc_objs, info, ctx);
                let sig_rttid = ctx.intern_rttid(sig);

                // Determine is_pointer_receiver for the promoted method:
                // - If the embedding path contains a pointer step (e.g., struct embeds *T),
                //   then even pointer receiver methods are accessible for the outer value type.
                // - Otherwise, use the original method's receiver type.
                let original_is_ptr_recv = tc_objs.lobjs[obj_key].entity_type().func_has_ptr_recv();
                let is_pointer_receiver = if path_info.has_pointer_step {
                    false // Method is always accessible via embedded pointer
                } else {
                    original_is_ptr_recv
                };

                ctx.update_named_type_method_if_absent(
                    named_type_id,
                    method_identity,
                    func_id,
                    is_pointer_receiver,
                    !is_pointer_receiver,
                    sig_rttid,
                );
            }
        }
    }
}

/// Collect method objects from embedded fields, keyed by their package-aware
/// language identity.
///
/// The work stack preserves the old depth-first, source-field traversal order.
/// A visited set makes this defensive against malformed cyclic embedding graphs
/// and avoids consuming the host stack on very deep, valid graphs. A `BTreeMap`
/// also gives wrapper generation a stable order across compiler processes.
fn collect_embedded_methods(
    type_key: vo_analysis::objects::TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    methods_by_identity: &mut std::collections::BTreeMap<String, vo_analysis::objects::ObjKey>,
) {
    enum Task {
        VisitType(vo_analysis::objects::TypeKey),
        VisitEmbedded(vo_analysis::objects::TypeKey),
    }

    let mut visited = std::collections::HashSet::new();
    let mut tasks = vec![Task::VisitType(type_key)];

    while let Some(task) = tasks.pop() {
        match task {
            Task::VisitType(type_key) => {
                if !visited.insert(type_key) {
                    continue;
                }
                let underlying = vo_analysis::typ::underlying_type(type_key, tc_objs);
                let Some(struct_detail) = tc_objs.types[underlying].try_as_struct() else {
                    continue;
                };

                // Reverse-push so the LIFO work stack observes source field order.
                for &field_obj in struct_detail.fields().iter().rev() {
                    let field = &tc_objs.lobjs[field_obj];
                    if !field.var_embedded() {
                        continue;
                    }
                    let Some(field_type) = field.typ() else {
                        continue;
                    };
                    let base_type = tc_objs.types[field_type]
                        .try_as_pointer()
                        .map_or(field_type, |pointer| pointer.base());
                    tasks.push(Task::VisitEmbedded(base_type));
                }
            }
            Task::VisitEmbedded(base_type) => {
                let field_underlying = vo_analysis::typ::underlying_type(base_type, tc_objs);

                if let Some(iface_detail) = tc_objs.types[field_underlying].try_as_interface() {
                    let all_methods = iface_detail.all_methods();
                    let methods: &[vo_analysis::objects::ObjKey] = match all_methods.as_ref() {
                        Some(methods) => methods.as_slice(),
                        None => iface_detail.methods(),
                    };
                    for &method in methods {
                        methods_by_identity
                            .insert(tc_objs.lobjs[method].id(tc_objs).into_owned(), method);
                    }
                }

                if let Some(named_detail) = tc_objs.types[base_type].try_as_named() {
                    for &method in named_detail.methods() {
                        methods_by_identity
                            .insert(tc_objs.lobjs[method].id(tc_objs).into_owned(), method);
                    }
                    tasks.push(Task::VisitType(base_type));
                }
            }
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
    let Type::Tuple(tuple) = &tc_objs.types[tuple_key] else {
        panic!("runtime_value_rttids_from_tuple requires tuple type metadata");
    };
    tuple
        .vars()
        .iter()
        .map(|&v| {
            let t = tc_objs.lobjs[v]
                .typ()
                .expect("tuple object must have type metadata");
            let rttid = ctx.intern_type_key(t, info);
            let vk = info.type_value_kind(t);
            ValueRttid::new(rttid, vk)
        })
        .collect()
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
        panic!("signature_type_to_runtime_type requires signature type metadata")
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
    let (func_def, debug_locs) = compile_func_body(func_decl, ctx, info)?;
    ctx.replace_function(func_id, func_def);
    ctx.record_function_debug_locs(func_id, &debug_locs, &info.project.source_map);
    Ok(())
}

/// Compile function body without pre-allocated func_id (for init() functions).
fn compile_func_decl_new(
    func_decl: &vo_syntax::ast::FuncDecl,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<u32, CodegenError> {
    let (func_def, debug_locs) = compile_func_body(func_decl, ctx, info)?;
    let func_id = ctx.add_function(func_def);
    ctx.record_function_debug_locs(func_id, &debug_locs, &info.project.source_map);
    Ok(func_id)
}

fn compile_func_body(
    func_decl: &vo_syntax::ast::FuncDecl,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<
    (
        vo_runtime::bytecode::FunctionDef,
        Vec<(u32, vo_common::span::Span)>,
    ),
    CodegenError,
> {
    let name = info
        .project
        .interner
        .resolve(func_decl.name.symbol)
        .unwrap_or("unknown");
    let func_type = info.obj_type(info.get_def(&func_decl.name), "function must have type");
    let param_type_keys = info.func_param_types(func_type);

    let mut builder = FuncBuilder::new(name);

    // Define parameters and collect escaped ones for boxing
    let mut escaped_params = Vec::new();

    // Define receiver as first parameter (if method)
    if let Some(recv) = &func_decl.receiver {
        let (slots, slot_types) = if recv.is_pointer {
            (1, vec![vo_runtime::SlotType::GcRef])
        } else {
            let type_key = info.obj_type(info.get_use(&recv.ty), "method receiver must have type");
            (
                info.try_type_slot_count(type_key)
                    .map_err(CodegenError::Internal)?,
                info.type_slot_types(type_key),
            )
        };

        builder.set_recv_slots(slots);
        builder
            .try_define_param(recv.name.as_ref().map(|n| n.symbol), slots, &slot_types)
            .map_err(CodegenError::Internal)?;

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
    let mut param_type_iter = param_type_keys.into_iter();
    for param in params {
        if param.names.is_empty() {
            let param_type_key = param_type_iter
                .next()
                .expect("function signature param types missing anonymous parameter entry");
            let slots = info
                .try_type_slot_count(param_type_key)
                .map_err(CodegenError::Internal)?;
            let slot_types = info.type_slot_types(param_type_key);
            builder
                .try_define_param(None, slots, &slot_types)
                .map_err(CodegenError::Internal)?;
            builder.add_param_type_key(param_type_key, ctx, info);
            continue;
        }
        for name in &param.names {
            let param_type_key = param_type_iter
                .next()
                .expect("function signature param types missing named parameter entry");
            let slots = info
                .try_type_slot_count(param_type_key)
                .map_err(CodegenError::Internal)?;
            let slot_types = info.type_slot_types(param_type_key);
            let obj_key = info.get_def(name);
            let type_key = info.obj_type(obj_key, "param must have type");
            builder
                .try_define_param(Some(name.symbol), slots, &slot_types)
                .map_err(CodegenError::Internal)?;
            builder.add_param_type_key(param_type_key, ctx, info);
            if info.needs_boxing(obj_key, type_key) {
                escaped_params.push((name.symbol, type_key, slots, slot_types.clone()));
            }
        }
    }
    assert!(
        param_type_iter.next().is_none(),
        "function signature param types had extra entries after binding AST params"
    );

    // Box escaped parameters: allocate heap storage and copy param values
    for (sym, type_key, slots, slot_types) in escaped_params {
        if info.is_array(type_key) {
            crate::array_value::materialize_escaped_param(sym, type_key, ctx, &mut builder, info)?;
        } else {
            let meta_idx = ctx.get_boxing_meta(type_key, info);
            builder.emit_box_escaped_param(
                sym,
                slots,
                info.is_pointer(type_key),
                meta_idx,
                &slot_types,
            );
        }
    }

    // Set return slots and types
    let mut ret_slot_types = Vec::new();
    for result in &func_decl.sig.results {
        let type_key = info.type_expr_type(result.ty.id);
        info.try_type_slot_count(type_key)
            .map_err(CodegenError::Internal)?;
        ret_slot_types.extend(info.type_slot_types(type_key));
    }
    let ret_slots = info
        .checked_slot_count(ret_slot_types.len())
        .map_err(CodegenError::Internal)?;
    assert_eq!(
        ret_slot_types.len(),
        ret_slots as usize,
        "function return slot layout must exactly match return slot count"
    );
    builder
        .try_set_ret_slot_types(ret_slot_types)
        .map_err(CodegenError::Internal)?;
    let return_types: Vec<_> = func_decl
        .sig
        .results
        .iter()
        .map(|r| info.type_expr_type(r.ty.id))
        .collect();
    builder.set_return_types(return_types.clone());

    // Compute error_ret_slot: if last return type is error, calculate its slot offset
    if let Some(last_type) = return_types.last() {
        if info.is_error_type(*last_type) {
            // Sum up all slots before the error return
            let mut offset: u16 = 0;
            for (i, result) in func_decl.sig.results.iter().enumerate() {
                if i == return_types.len() - 1 {
                    break; // Don't include the error type itself
                }
                let type_key = info.type_expr_type(result.ty.id);
                let slots = info
                    .try_type_slot_count(type_key)
                    .map_err(CodegenError::Internal)?;
                offset = offset.checked_add(slots).ok_or_else(|| {
                    CodegenError::Internal(format!(
                        "type slot count exceeds u16::MAX: {} slots",
                        offset as usize + slots as usize
                    ))
                })?;
            }
            builder.set_error_ret_slot(i32::from(offset));
        }
    }

    // Define named return variables as locals (zero-initialized)
    // Check if they escape (e.g. captured by defer closure)
    // IMPORTANT: For panic/recover to work correctly, if ANY named return escapes,
    // ALL named returns must escape. This is because the VM's heap_ret recovery path
    // only works when all named returns are heap-allocated (mixed case not supported).
    // Two-pass approach for escaped returns to ensure contiguous GcRef slots:
    // 1. First allocate all slots (so escaped GcRef slots are contiguous)
    // 2. Then emit PtrNew instructions
    struct EscapedReturn {
        gcref_slot: u16,
        slots: u16,
        result_type: vo_analysis::objects::TypeKey,
        slot_types: Vec<vo_runtime::SlotType>,
        is_array: bool,
    }
    let mut escaped_returns: Vec<EscapedReturn> = Vec::new();

    // First pass: check if ANY named return escapes
    let any_escapes = func_decl
        .sig
        .results
        .iter()
        .filter_map(|r| r.name.as_ref())
        .any(|name| info.is_escaped(info.get_def(name)));

    for result in &func_decl.sig.results {
        if let Some(name) = &result.name {
            let result_type = info.type_expr_type(result.ty.id);
            let (slots, slot_types) = info.type_expr_layout(result.ty.id);
            let obj_key = info.get_def(name);
            // Force escape if ANY named return escapes (for correct panic/recover)
            let escapes = any_escapes || info.is_escaped(obj_key);

            let slot = if escapes {
                // Allocate all return GcRef slots before emitting allocation
                // instructions so the heap-return ABI remains contiguous.
                let is_array = info.is_array(result_type);
                let gcref_slot = if is_array {
                    let elem_type = info.array_elem_type(result_type);
                    let elem_slots = info
                        .try_type_slot_count(elem_type)
                        .map_err(CodegenError::Internal)?;
                    builder.define_local_heap_array(
                        name.symbol,
                        elem_slots,
                        info.array_elem_bytes(result_type),
                        info.type_value_kind(elem_type),
                    )
                } else {
                    builder.define_local_heap_boxed(
                        name.symbol,
                        slots,
                        info.is_pointer(result_type),
                    )
                };
                escaped_returns.push(EscapedReturn {
                    gcref_slot,
                    slots,
                    result_type,
                    slot_types,
                    is_array,
                });
                gcref_slot
            } else {
                let slot = builder.define_local_stack(name.symbol, slots, &slot_types);
                // Zero-initialize non-escaped named return (Go zero-value semantics)
                // VM no longer does write_bytes, so codegen must handle this
                for i in 0..slots {
                    builder.emit_op(vo_runtime::instruction::Opcode::LoadInt, slot + i, 0, 0);
                }
                slot
            };
            builder.register_named_return(slot, slots, escapes);
        }
    }

    // Now emit PtrNew for all escaped returns (after all GcRef slots are allocated contiguously)
    for er in escaped_returns {
        if er.is_array {
            crate::array_value::emit_new_ref_at(
                er.gcref_slot,
                er.result_type,
                ctx,
                &mut builder,
                info,
            )?;
            continue;
        }
        let meta_idx = ctx.get_boxing_meta(er.result_type, info);
        let meta_reg = builder.alloc_slots(&[vo_runtime::SlotType::Value]);
        builder.emit_op(
            vo_runtime::instruction::Opcode::LoadConst,
            meta_reg,
            meta_idx,
            0,
        );
        assert_eq!(er.slots as usize, er.slot_types.len());
        builder.emit_ptr_new(er.gcref_slot, meta_reg, &er.slot_types);
    }

    // Compile function body
    if let Some(body) = &func_decl.body {
        stmt::compile_block(body, ctx, &mut builder, info)?;
    }

    builder.emit_fallthrough_return();
    builder
        .check_layout_error()
        .map_err(CodegenError::Internal)?;

    // Build the function together with call-site locations. Function IDs for
    // init functions are assigned only after compilation, so location
    // attachment deliberately happens in the caller.
    Ok(builder.build_with_debug_locs())
}

/// Emit GlobalSet or GlobalSetN depending on slot count.
fn emit_global_set(builder: &mut FuncBuilder, global_idx: u16, src: u16, slots: u16) {
    builder.emit_global_set(global_idx, src, slots);
}

/// Compile an array expression into ordinary value slots.
///
/// Stack arrays and call results already use flattened value slots. Package,
/// heap, and captured arrays use a GcRef-backed representation and must be read
/// element by element to preserve array value semantics.
pub(crate) fn compile_array_expr_to_slots(
    expr: &vo_syntax::ast::Expr,
    dst: u16,
    array_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    crate::array_value::prepare_expr(expr, array_type, ctx, func, info)?
        .emit_to_flat(dst, array_type, ctx, func, info)
}

/// Allocate the stable typed object owned by a package-level struct variable.
/// The allocation is installed before any package initializer is evaluated.
pub(crate) fn allocate_global_struct(
    struct_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    if !info.is_struct(struct_type) {
        return Err(CodegenError::Internal(
            "global boxed storage requires a struct type".to_string(),
        ));
    }
    let slot_types = info.type_slot_types(struct_type);
    let object = func.alloc_slots(&[vo_runtime::SlotType::GcRef]);
    let meta = func.alloc_slots(&[vo_runtime::SlotType::Value]);
    let meta_idx = ctx.get_boxing_meta(struct_type, info);
    func.emit_op(
        vo_runtime::instruction::Opcode::LoadConst,
        meta,
        meta_idx,
        0,
    );
    func.emit_ptr_new(object, meta, &slot_types);
    Ok(object)
}

/// Commit flattened struct slots into a package variable's stable allocation.
pub(crate) fn compile_global_struct_from_slots(
    src: u16,
    struct_type: vo_analysis::objects::TypeKey,
    global_idx: u16,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if !info.is_struct(struct_type) {
        return Err(CodegenError::Internal(
            "global boxed commit requires a struct type".to_string(),
        ));
    }
    let object = func.alloc_slots(&[vo_runtime::SlotType::GcRef]);
    func.emit_global_get(object, global_idx, 1);
    func.emit_ptr_set_with_slot_types(object, 0, src, &info.type_slot_types(struct_type));
    Ok(())
}

/// Materialize an ordinary flattened array value into the canonical heap array
/// representation used by globals and interface boxes.
pub(crate) fn materialize_array_from_slots(
    src: u16,
    array_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    crate::array_value::materialize_flat(src, array_type, ctx, func, info)
}

/// Allocate the heap-backed array representation used by a package global.
/// The runtime allocation is zeroed, so this also implements the zero value for
/// an array declaration without an explicit initializer.
fn allocate_global_array(
    array_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    crate::array_value::allocate_ref(array_type, ctx, func, info)
}

#[cfg(test)]
fn validate_heap_array_len_for_pointer_width(
    array_len: u64,
    pointer_width: u32,
) -> Result<(), CodegenError> {
    crate::array_value::validate_len_for_pointer_width(array_len, pointer_width)
}

/// Compile global variable initialization for a single package.
fn compile_package_globals(
    ctx: &mut CodegenContext,
    init_builder: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    files: &[vo_syntax::ast::File],
) -> Result<(), CodegenError> {
    enum PreparedGlobalInitializer {
        Flat {
            obj_key: vo_analysis::objects::ObjKey,
            type_key: vo_analysis::objects::TypeKey,
            slot: u16,
        },
        Array {
            obj_key: vo_analysis::objects::ObjKey,
            type_key: vo_analysis::objects::TypeKey,
            value: crate::array_value::ArrayValue,
        },
    }

    // Arrays and structs use stable package-owned allocations. Install every
    // object, including explicitly initialized variables, before evaluating any
    // RHS so pointers obtained during initialization cannot be invalidated by a
    // later commit.
    for file in files {
        for decl in &file.decls {
            let Decl::Var(var_decl) = decl else {
                continue;
            };
            for spec in &var_decl.specs {
                for name in &spec.names {
                    let obj_key = info.get_def(name);
                    if info.obj_name(obj_key) == "_" {
                        continue;
                    }
                    let type_key =
                        info.obj_type(obj_key, "package variable must have a checked type");
                    let global_idx = ctx.get_global_index(obj_key).ok_or_else(|| {
                        CodegenError::Internal(format!(
                            "missing storage for package variable {}",
                            info.obj_name(obj_key)
                        ))
                    })?;
                    let object = if info.is_array(type_key) {
                        allocate_global_array(type_key, ctx, init_builder, info)?
                    } else if info.is_struct(type_key) {
                        allocate_global_struct(type_key, ctx, init_builder, info)?
                    } else {
                        continue;
                    };
                    init_builder.emit_op(
                        vo_runtime::instruction::Opcode::GlobalSet,
                        global_idx,
                        object,
                        0,
                    );
                }
            }
        }
    }

    // Initialize global variables in dependency order (from type checker analysis)
    for initializer in info.init_order() {
        if initializer.lhs.is_empty() {
            return Err(CodegenError::Internal(
                "package initializer has no targets".to_string(),
            ));
        }
        if initializer.rhs.is_empty() {
            return Err(CodegenError::Internal(
                "package initializer has no values".to_string(),
            ));
        }

        // A package VarSpec has parallel-assignment semantics: every RHS is
        // evaluated and converted to its checked destination type before any
        // package variable in the group is updated.
        let mut pending = Vec::with_capacity(initializer.lhs.len());
        let is_tuple_rhs =
            initializer.rhs.len() == 1 && info.is_tuple(info.expr_type(initializer.rhs[0].id));

        if is_tuple_rhs {
            let tuple =
                crate::expr::CompiledTuple::compile(&initializer.rhs[0], ctx, init_builder, info)?;
            let tuple_len = info.tuple_len(tuple.tuple_type);
            if tuple_len != initializer.lhs.len() {
                return Err(CodegenError::Internal(format!(
                    "package initializer tuple arity mismatch: {} values for {} variables",
                    tuple_len,
                    initializer.lhs.len()
                )));
            }

            let mut src_offset = 0u16;
            for (index, &obj_key) in initializer.lhs.iter().enumerate() {
                let src_type = info.tuple_elem_type(tuple.tuple_type, index);
                let dst_type = info.obj_type(
                    obj_key,
                    "package initializer target must have a checked type",
                );
                let tmp = init_builder.alloc_slots(&info.type_slot_types(dst_type));
                crate::assign::emit_assign(
                    tmp,
                    crate::assign::AssignSource::Slot {
                        slot: tuple.base + src_offset,
                        type_key: src_type,
                    },
                    dst_type,
                    ctx,
                    init_builder,
                    info,
                )?;
                if info.is_array(dst_type) {
                    pending.push(PreparedGlobalInitializer::Array {
                        obj_key,
                        type_key: dst_type,
                        value: crate::array_value::ArrayValue::FlatSlots(tmp),
                    });
                } else {
                    pending.push(PreparedGlobalInitializer::Flat {
                        obj_key,
                        type_key: dst_type,
                        slot: tmp,
                    });
                }
                let src_slots = info
                    .try_type_slot_count(src_type)
                    .map_err(CodegenError::Internal)?;
                src_offset = src_offset.checked_add(src_slots).ok_or_else(|| {
                    CodegenError::Internal(
                        "package initializer tuple slot offset exceeds u16".to_string(),
                    )
                })?;
            }
        } else {
            if initializer.rhs.len() != initializer.lhs.len() {
                return Err(CodegenError::Internal(format!(
                    "package initializer arity mismatch: {} values for {} variables",
                    initializer.rhs.len(),
                    initializer.lhs.len()
                )));
            }

            for (&obj_key, rhs) in initializer.lhs.iter().zip(&initializer.rhs) {
                let dst_type = info.obj_type(
                    obj_key,
                    "package initializer target must have a checked type",
                );
                if info.is_array(dst_type) {
                    // Preserve the canonical representation through the
                    // transactional RHS phase. Borrowed globals/captures must
                    // be snapshotted now because a later RHS may mutate their
                    // stable allocation before this VarSpec commits.
                    let value =
                        crate::array_value::prepare_expr(rhs, dst_type, ctx, init_builder, info)?;
                    let value = match value {
                        crate::array_value::ArrayValue::BorrowedRef(_) => {
                            crate::array_value::ArrayValue::OwnedRef(value.into_owned_ref(
                                dst_type,
                                ctx,
                                init_builder,
                                info,
                            )?)
                        }
                        value => value,
                    };
                    pending.push(PreparedGlobalInitializer::Array {
                        obj_key,
                        type_key: dst_type,
                        value,
                    });
                } else {
                    let tmp = init_builder.alloc_slots(&info.type_slot_types(dst_type));
                    crate::assign::emit_assign(
                        tmp,
                        crate::assign::AssignSource::Expr(rhs),
                        dst_type,
                        ctx,
                        init_builder,
                        info,
                    )?;
                    pending.push(PreparedGlobalInitializer::Flat {
                        obj_key,
                        type_key: dst_type,
                        slot: tmp,
                    });
                }
            }
        }

        // Commit only after the full RHS group has completed successfully.
        // This also preserves source-order writes when a blank target appears.
        for prepared in pending {
            let (obj_key, dst_type) = match &prepared {
                PreparedGlobalInitializer::Flat {
                    obj_key, type_key, ..
                }
                | PreparedGlobalInitializer::Array {
                    obj_key, type_key, ..
                } => (*obj_key, *type_key),
            };
            if info.obj_name(obj_key) == "_" {
                continue;
            }
            let global_idx = ctx.get_global_index(obj_key).ok_or_else(|| {
                CodegenError::Internal(format!(
                    "missing storage for package variable {}",
                    info.obj_name(obj_key)
                ))
            })?;
            match prepared {
                PreparedGlobalInitializer::Array { value, .. } => {
                    let dst_ref = init_builder.alloc_slots(&[vo_runtime::SlotType::GcRef]);
                    init_builder.emit_global_get(dst_ref, global_idx, 1);
                    value.copy_into_ref(dst_ref, dst_type, ctx, init_builder, info)?;
                }
                PreparedGlobalInitializer::Flat { slot, .. } if info.is_struct(dst_type) => {
                    compile_global_struct_from_slots(
                        slot,
                        dst_type,
                        global_idx,
                        init_builder,
                        info,
                    )?;
                }
                PreparedGlobalInitializer::Flat { slot, .. } => {
                    emit_global_set(
                        init_builder,
                        global_idx,
                        slot,
                        info.type_slot_count(dst_type),
                    );
                }
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
    // 1. Generate the complete package initialization sequence. Each package's
    // globals are initialized immediately before that package's user init
    // functions. Dependencies precede dependents and the main package is last.
    let mut init_builder = FuncBuilder::new("__init__");

    // Initialize imported packages' global variables in dependency order
    // (dependencies are initialized before dependents)
    for (_, pkg, pkg_type_info, files) in project
        .imported_packages_in_order()
        .map_err(CodegenError::Internal)?
    {
        let pkg_info =
            TypeInfoWrapper::for_package(project, pkg, pkg_type_info, info.shared_layout_facts());
        compile_package_globals(ctx, &mut init_builder, &pkg_info, files)?;
        for user_init_id in ctx.init_functions_for_package(pkg) {
            emit_entry_static_call(&mut init_builder, ctx, user_init_id)?;
        }
    }

    // Then initialize the main package and run its init functions.
    compile_package_globals(ctx, &mut init_builder, info, &project.files)?;
    for user_init_id in ctx.init_functions_for_package(project.main_package) {
        emit_entry_static_call(&mut init_builder, ctx, user_init_id)?;
    }

    // Add return
    init_builder.emit_op(vo_runtime::instruction::Opcode::Return, 0, 0, 0);
    let init_func_id =
        ctx.add_function_from_builder_with_debug_locs(init_builder, &project.source_map);
    // Note: __init__ is NOT registered as a user init function - it's handled separately

    // 2. Find main function
    let main_func_id = ctx.main_func_id();
    if main_func_id.is_none() {
        return Err(CodegenError::FunctionNotFound(
            "missing entry function `func main()`".to_string(),
        ));
    }

    // 3. Generate __island_init__ function (init only, no main - for island VMs)
    let mut island_init_builder = FuncBuilder::new("__island_init__");
    emit_entry_static_call(&mut island_init_builder, ctx, init_func_id)?;
    island_init_builder.emit_op(vo_runtime::instruction::Opcode::Return, 0, 0, 0);
    let island_init_func_id =
        ctx.add_function_from_builder_with_debug_locs(island_init_builder, &project.source_map);
    ctx.set_island_init_func(island_init_func_id);

    // 4. Generate __entry__ function (full: init + main - for main island)
    let mut entry_builder = FuncBuilder::new("__entry__");

    // Call __init__ for global variable initialization
    emit_entry_static_call(&mut entry_builder, ctx, init_func_id)?;

    // Call main
    if let Some(main_id) = main_func_id {
        emit_entry_static_call(&mut entry_builder, ctx, main_id)?;
    }

    // Return
    entry_builder.emit_op(vo_runtime::instruction::Opcode::Return, 0, 0, 0);

    let entry_func_id =
        ctx.add_function_from_builder_with_debug_locs(entry_builder, &project.source_map);
    ctx.set_entry_func(entry_func_id);

    Ok(())
}

fn emit_entry_static_call(
    builder: &mut FuncBuilder,
    ctx: &CodegenContext,
    func_id: u32,
) -> Result<(), CodegenError> {
    let callee = ctx
        .module()
        .functions
        .get(func_id as usize)
        .ok_or_else(|| CodegenError::FunctionNotFound(format!("function id {func_id}")))?;
    if callee.param_slots != 0 {
        return Err(CodegenError::Internal(format!(
            "entry function cannot call {} with {} parameter slots",
            callee.name, callee.param_slots
        )));
    }

    let args_start = builder.alloc_call_buffer(&[], &callee.ret_slot_types);
    builder.emit_static_call(func_id, args_start, 0, callee.ret_slots);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        collect_embedded_methods, validate_heap_array_len_for_pointer_width,
        validate_project_type_layouts,
    };
    use std::collections::{BTreeMap, BTreeSet};
    use vo_analysis::objects::TCObjects;
    use vo_analysis::typ::BasicType;
    use vo_analysis::Project;
    use vo_common::span::Span;

    fn named_struct(
        objs: &mut TCObjects,
        methods: Vec<vo_analysis::objects::ObjKey>,
        embedded: Option<vo_analysis::objects::TypeKey>,
    ) -> vo_analysis::objects::TypeKey {
        let fields = embedded
            .map(|embedded| {
                vec![objs.new_field(
                    Span::dummy(),
                    None,
                    "Embedded".to_string(),
                    Some(embedded),
                    true,
                )]
            })
            .unwrap_or_default();
        let underlying = objs.new_t_struct(fields, None);
        objs.new_t_named(None, Some(underlying), methods)
    }

    #[test]
    fn heap_array_length_reports_target_address_width_overflow() {
        validate_heap_array_len_for_pointer_width(u32::MAX as u64, 32)
            .expect("u32::MAX fits a 32-bit target length");
        let error = validate_heap_array_len_for_pointer_width(u32::MAX as u64 + 1, 32)
            .expect_err("2^32 elements cannot be represented by wasm32 runtime indexing");
        assert!(error
            .to_string()
            .contains("exceeds the 32-bit target address width"));
    }

    #[test]
    fn project_layout_limit_reports_the_lowest_type_key_deterministically() {
        let mut messages = BTreeSet::new();
        for _ in 0..32 {
            let mut objects = TCObjects::new();
            let int = objects
                .universe()
                .lookup_type(BasicType::Int)
                .expect("int type");
            let first = objects.new_t_array(int, Some(u64::from(u16::MAX) + 1));
            let second = objects.new_t_array(int, Some(u64::from(u16::MAX) + 2));
            objects.new_t_map(first, int);
            objects.new_t_map(second, int);
            let main_package = objects.new_package("main".to_string(), "main".to_string());
            let project = Project {
                tc_objs: objects,
                interner: vo_common::SymbolInterner::new(),
                packages: vec![main_package],
                main_package,
                type_info: Default::default(),
                files: Vec::new(),
                imported_files: BTreeMap::new(),
                imported_type_infos: BTreeMap::new(),
                source_map: vo_common::SourceMap::new(),
                extensions: Vec::new(),
            };

            let error = validate_project_type_layouts(&project)
                .expect_err("both map key layouts exceed the VM slot domain");
            let message = error.to_string();
            assert!(message.contains(&format!("{first:?}")), "{message}");
            messages.insert(message);
        }
        assert_eq!(messages.len(), 1);
    }

    #[test]
    fn vm_codegen_generated_entry_builder_layout_023_uses_context_registration_owner() {
        let source = include_str!("lib.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("lib source should contain tests section");

        assert!(
            !source.contains("let init_func = init_builder.build()"),
            "__init__ builder must not bypass context-owned layout error recording"
        );
        assert!(
            !source.contains("let island_init_func = island_init_builder.build()"),
            "__island_init__ builder must not bypass context-owned layout error recording"
        );
        assert!(
            !source.contains("let entry_func = entry_builder.build()"),
            "__entry__ builder must not bypass context-owned layout error recording"
        );
    }

    #[test]
    fn embedded_method_collection_terminates_on_self_pointer_embedding() {
        let mut objs = TCObjects::new();
        let method = objs.new_func(Span::dummy(), None, "M".to_string(), None, false);
        let self_type = objs.new_t_named(None, None, vec![method]);
        let self_pointer = objs.new_t_pointer(self_type);
        let field = objs.new_field(
            Span::dummy(),
            None,
            "Self".to_string(),
            Some(self_pointer),
            true,
        );
        let underlying = objs.new_t_struct(vec![field], None);
        objs.types[self_type]
            .try_as_named_mut()
            .expect("test type must remain named")
            .set_underlying(underlying);

        let mut methods = BTreeMap::new();
        collect_embedded_methods(self_type, &objs, &mut methods);

        assert_eq!(methods.len(), 1);
        assert_eq!(methods.values().copied().next(), Some(method));
    }

    #[test]
    fn embedded_method_collection_handles_thousands_of_levels_iteratively() {
        let mut objs = TCObjects::new();
        let method = objs.new_func(Span::dummy(), None, "Deep".to_string(), None, false);
        let mut current = named_struct(&mut objs, vec![method], None);

        for _ in 0..4_096 {
            let pointer = objs.new_t_pointer(current);
            current = named_struct(&mut objs, Vec::new(), Some(pointer));
        }

        let mut methods = BTreeMap::new();
        collect_embedded_methods(current, &objs, &mut methods);

        assert_eq!(methods.len(), 1);
        assert_eq!(methods.values().copied().next(), Some(method));
    }

    #[test]
    fn embedded_method_identity_overwrite_follows_source_field_order() {
        let mut objs = TCObjects::new();
        let first = objs.new_func(Span::dummy(), None, "M".to_string(), None, false);
        let second = objs.new_func(Span::dummy(), None, "M".to_string(), None, false);
        let first_type = named_struct(&mut objs, vec![first], None);
        let second_type = named_struct(&mut objs, vec![second], None);
        let first_field = objs.new_field(
            Span::dummy(),
            None,
            "First".to_string(),
            Some(first_type),
            true,
        );
        let second_field = objs.new_field(
            Span::dummy(),
            None,
            "Second".to_string(),
            Some(second_type),
            true,
        );
        let root = objs.new_t_struct(vec![first_field, second_field], None);

        let mut methods = BTreeMap::new();
        collect_embedded_methods(root, &objs, &mut methods);

        assert_eq!(methods.len(), 1);
        assert_eq!(methods.values().copied().next(), Some(second));
    }
}
