//! GoX VM bytecode code generator.

mod context;
mod error;
mod expr;
mod func;
mod stmt;
mod type_info;
mod types;

pub use context::CodegenContext;
pub use error::{CodegenError, Result};
pub use func::FuncBuilder;
pub use type_info::TypeInfo;

use gox_analysis::project::Project;
use gox_analysis::scope::Entity;
use gox_common::SymbolInterner;
use gox_common_core::SlotType;
use gox_syntax::ast::{Decl, File, FuncDecl};
use gox_vm::bytecode::Module;
use gox_vm::instruction::Opcode;

use crate::expr::compile_expr;
use crate::stmt::compile_stmt;

/// Compile a project to a Module.
pub fn compile_project(project: &Project) -> Result<Module> {
    let mut ctx = CodegenContext::new(&project.main_package);

    // Process all packages in init order
    for pkg in &project.packages {
        let info = TypeInfo::new(
            &pkg.types.types,
            &pkg.types.scope,
            &pkg.types.named_types,
            &project.interner,
        );

        // Collect declarations from all files in the package
        for file in &pkg.files {
            collect_declarations(file, &info, &mut ctx)?;
        }

        // Compile functions from all files
        for file in &pkg.files {
            compile_functions(file, &info, &mut ctx)?;
        }
    }

    // Generate init and entry for the main package
    if let Some(main_pkg) = project.packages.iter().find(|p| p.name == project.main_package) {
        let info = TypeInfo::new(
            &main_pkg.types.types,
            &main_pkg.types.scope,
            &main_pkg.types.named_types,
            &project.interner,
        );

        compile_init_and_entry_project(project, &info, &mut ctx)?;
    }

    Ok(ctx.finish())
}

fn compile_init_and_entry_project(
    project: &Project,
    info: &TypeInfo,
    ctx: &mut CodegenContext,
) -> Result<()> {
    let mut init_builder = FuncBuilder::new("__init__");

    // Compile global var initializers from main package
    if let Some(main_pkg) = project.packages.iter().find(|p| p.name == project.main_package) {
        for file in &main_pkg.files {
            for decl in &file.decls {
                if let Decl::Var(var) = decl {
                    for spec in &var.specs {
                        for (i, name) in spec.names.iter().enumerate() {
                            if i < spec.values.len() {
                                let src = compile_expr(&spec.values[i], ctx, &mut init_builder, info)?;
                                if let Some(idx) = ctx.get_global_index(name.symbol) {
                                    init_builder.emit_op(Opcode::SetGlobal, idx as u16, src, 0);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    init_builder.emit_op(Opcode::Return, 0, 0, 0);
    let init_def = init_builder.build();
    ctx.module.add_function(init_def);

    let mut entry_builder = FuncBuilder::new("__entry__");

    let init_idx = ctx.module.functions.len() as u16 - 1;
    entry_builder.emit_op(Opcode::Call, init_idx, 0, 0);

    if let Some(main_idx) = ctx.module.find_function("main") {
        entry_builder.emit_with_flags(Opcode::Call, 0, main_idx as u16, 0, 0);
    }

    entry_builder.emit_op(Opcode::Return, 0, 0, 0);
    let entry_def = entry_builder.build();
    let entry_idx = ctx.module.add_function(entry_def);
    ctx.module.entry_func = entry_idx;

    Ok(())
}

/// Compile a single file to a Module (legacy API).
pub fn compile(
    file: &File,
    check_result: &gox_analysis::TypeCheckResult,
    interner: &SymbolInterner,
) -> Result<Module> {
    let mut ctx = CodegenContext::new("main");

    let info = TypeInfo::new(
        &check_result.types,
        &check_result.scope,
        &check_result.named_types,
        interner,
    );

    collect_declarations(file, &info, &mut ctx)?;

    compile_functions(file, &info, &mut ctx)?;

    compile_init_and_entry(file, &info, &mut ctx)?;

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
                    ctx.register_func(func.name.symbol);
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
                        let ty = if let Some(Entity::Var(v)) = info.lookup(name.symbol) {
                            &v.ty
                        } else {
                            continue;
                        };
                        let type_id = info.runtime_type_id(ty);
                        let slots = info.type_slots(ty);
                        ctx.register_global(name.symbol, name_str, type_id, slots);
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

    for param in &func.sig.params {
        let ty = info.types.get_type_expr_type(param.ty.id)
            .and_then(|id| info.types.try_resolve(id));
        let slot_types = ty.map(|t| info.type_slot_types(t)).unwrap_or_else(|| vec![SlotType::Value]);
        let slots = slot_types.len() as u16;
        for pname in &param.names {
            builder.define_param(pname.symbol, slots, &slot_types);
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
    ctx.module.add_function(func_def);

    Ok(())
}

fn compile_init_and_entry(
    file: &File,
    info: &TypeInfo,
    ctx: &mut CodegenContext,
) -> Result<()> {
    let mut init_builder = FuncBuilder::new("__init__");

    for decl in &file.decls {
        if let Decl::Var(var) = decl {
            for spec in &var.specs {
                for (i, name) in spec.names.iter().enumerate() {
                    if i < spec.values.len() {
                        let src = compile_expr(&spec.values[i], ctx, &mut init_builder, info)?;
                        if let Some(idx) = ctx.get_global_index(name.symbol) {
                            init_builder.emit_op(Opcode::SetGlobal, idx as u16, src, 0);
                        }
                    }
                }
            }
        }
    }

    init_builder.emit_op(Opcode::Return, 0, 0, 0);
    let init_def = init_builder.build();
    ctx.module.add_function(init_def);

    let mut entry_builder = FuncBuilder::new("__entry__");

    let init_idx = ctx.module.functions.len() as u16 - 1;
    entry_builder.emit_op(Opcode::Call, init_idx, 0, 0);

    if let Some(main_idx) = ctx.module.find_function("main") {
        entry_builder.emit_with_flags(Opcode::Call, 0, main_idx as u16, 0, 0);
    }

    entry_builder.emit_op(Opcode::Return, 0, 0, 0);
    let entry_def = entry_builder.build();
    let entry_idx = ctx.module.add_function(entry_def);
    ctx.module.entry_func = entry_idx;

    Ok(())
}
