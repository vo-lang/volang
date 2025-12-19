//! Statement compilation.

use gox_common_core::ExprId;
use gox_syntax::ast::{Stmt, StmtKind, Block, IfStmt, ForStmt, ForClause, ReturnStmt, AssignStmt, AssignOp, ShortVarDecl, ExprKind, SwitchStmt, DeferStmt, ErrDeferStmt, FailStmt, GoStmt, SendStmt, SelectStmt, CommClause};
use gox_vm::instruction::Opcode;
use gox_analysis::Type;

use crate::{CodegenContext, CodegenError};
use crate::context::{FuncContext, ValueKind, TypeInfo};
use crate::expr;

/// Compile a block of statements.
pub fn compile_block(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    block: &Block,
) -> Result<(), CodegenError> {
    let saved_reg = fctx.regs.current();
    
    for stmt in &block.stmts {
        compile_stmt(ctx, fctx, stmt)?;
    }
    
    fctx.regs.reset_to(saved_reg);
    Ok(())
}

/// Compile a single statement.
pub fn compile_stmt(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    stmt: &Stmt,
) -> Result<(), CodegenError> {
    match &stmt.kind {
        StmtKind::Empty => Ok(()),
        StmtKind::Block(block) => compile_block(ctx, fctx, block),
        StmtKind::Expr(e) => {
            let reg = fctx.regs.current();
            expr::compile_expr(ctx, fctx, e)?;
            fctx.regs.reset_to(reg);
            Ok(())
        }
        StmtKind::ShortVar(sv) => compile_short_var(ctx, fctx, sv),
        StmtKind::Var(var_decl) => {
            for spec in &var_decl.specs {
                // Check if this is an interface type declaration
                let is_interface = spec.ty.as_ref().map_or(false, |ty| {
                    match &ty.kind {
                        gox_syntax::ast::TypeExprKind::Interface(_) => true,
                        gox_syntax::ast::TypeExprKind::Ident(ident) => {
                            // Check if named type is an interface
                            is_named_interface_type(ctx, ident.symbol)
                        }
                        _ => false,
                    }
                });
                
                // Get iface_type for interface declarations
                let iface_type = if is_interface {
                    get_interface_type_id(ctx, spec.ty.as_ref())
                } else {
                    0
                };
                
                for (i, name) in spec.names.iter().enumerate() {
                    if is_interface {
                        // Interface needs 2 slots (type_id + data)
                        let dst = fctx.define_local_with_kind(*name, 2, ValueKind::Interface);
                        // Initialize interface with iface_type
                        fctx.emit(Opcode::InitInterface, dst, iface_type, 0);
                        if i < spec.values.len() {
                            // Box the value into interface
                            let src = expr::compile_expr(ctx, fctx, &spec.values[i])?;
                            let value_type = infer_runtime_type_id(ctx, fctx, &spec.values[i]);
                            fctx.emit(Opcode::BoxInterface, dst, value_type, src);
                        }
                        // else: nil interface already initialized by InitInterface
                    } else {
                        // Check if this is a struct type that needs allocation
                        let struct_slot_count = spec.ty.as_ref().and_then(|ty| {
                            if let gox_syntax::ast::TypeExprKind::Ident(type_ident) = &ty.kind {
                                // First check local types, then global types
                                fctx.get_local_type(type_ident.symbol)
                                    .map(|(count, _)| *count)
                                    .or_else(|| ctx.get_named_type_info(type_ident.symbol)
                                        .map(|ti| expr::get_type_slot_count(ctx, &ti.underlying)))
                            } else {
                                None
                            }
                        });
                        
                        let dst = fctx.define_local(*name, 1);
                        
                        // Track the type symbol for named types (local or global)
                        if let Some(ty) = &spec.ty {
                            if let gox_syntax::ast::TypeExprKind::Ident(type_ident) = &ty.kind {
                                // Set type_sym for both local and global named types
                                if fctx.get_local_type(type_ident.symbol).is_some() 
                                    || ctx.get_named_type_info(type_ident.symbol).is_some() {
                                    if let Some(local) = fctx.locals.get_mut(&name.symbol) {
                                        local.type_sym = Some(type_ident.symbol);
                                    }
                                }
                            }
                        }
                        
                        if i < spec.values.len() {
                            let src = expr::compile_expr(ctx, fctx, &spec.values[i])?;
                            if src != dst {
                                fctx.emit(Opcode::Mov, dst, src, 0);
                            }
                        } else if let Some(slot_count) = struct_slot_count {
                            // Allocate zero-initialized struct
                            fctx.emit(Opcode::Alloc, dst, 0, slot_count);
                        } else {
                            fctx.emit(Opcode::LoadNil, dst, 0, 0);
                        }
                    }
                }
            }
            Ok(())
        }
        StmtKind::Const(const_decl) => {
            for spec in &const_decl.specs {
                for (i, name) in spec.names.iter().enumerate() {
                    let dst = fctx.define_local(*name, 1);
                    if i < spec.values.len() {
                        let src = expr::compile_expr(ctx, fctx, &spec.values[i])?;
                        if src != dst {
                            fctx.emit(Opcode::Mov, dst, src, 0);
                        }
                    }
                }
            }
            Ok(())
        }
        StmtKind::Assign(assign) => compile_assign(ctx, fctx, assign),
        StmtKind::IncDec(incdec) => compile_inc_dec(ctx, fctx, &incdec.expr, incdec.is_inc),
        StmtKind::Return(ret) => compile_return(ctx, fctx, ret),
        StmtKind::If(if_stmt) => compile_if(ctx, fctx, if_stmt),
        StmtKind::For(for_stmt) => compile_for(ctx, fctx, for_stmt),
        StmtKind::Go(go_stmt) => compile_go(ctx, fctx, go_stmt),
        StmtKind::Defer(defer_stmt) => compile_defer(ctx, fctx, defer_stmt),
        StmtKind::ErrDefer(errdefer_stmt) => compile_errdefer(ctx, fctx, errdefer_stmt),
        StmtKind::Fail(fail_stmt) => compile_fail(ctx, fctx, fail_stmt),
        StmtKind::Send(send) => compile_send(ctx, fctx, send),
        StmtKind::Select(sel) => compile_select(ctx, fctx, sel),
        StmtKind::Switch(sw) => compile_switch(ctx, fctx, sw),
        StmtKind::TypeSwitch(_) => Err(CodegenError::Unsupported("type switch".to_string())),
        StmtKind::Break(_) => {
            // Emit jump placeholder, will be patched when loop ends
            let pc = fctx.pc();
            fctx.emit(Opcode::Jump, 0, 0, 0);
            fctx.add_break(pc);
            Ok(())
        }
        StmtKind::Continue(_) => {
            if let Some(continue_pc) = fctx.current_continue_pc() {
                let jump_pc = fctx.pc();
                fctx.emit(Opcode::Jump, 0, 0, 0);
                if continue_pc == 0 {
                    // Deferred patching for three-clause loops
                    fctx.add_continue(jump_pc);
                } else {
                    let offset = (continue_pc as i32) - (jump_pc as i32);
                    fctx.patch_jump(jump_pc, offset);
                }
                Ok(())
            } else {
                Err(CodegenError::Internal("continue outside loop".to_string()))
            }
        }
        StmtKind::Goto(_) | StmtKind::Fallthrough => {
            Err(CodegenError::Unsupported("goto/fallthrough".to_string()))
        }
        StmtKind::Labeled(_) => Err(CodegenError::Unsupported("labeled statement".to_string())),
        StmtKind::Type(type_decl) => {
            // Local type declarations - track struct types for var allocations and field access
            if let gox_syntax::ast::TypeExprKind::Struct(struct_type) = &type_decl.ty.kind {
                let slot_count = struct_type.fields.len() as u16;
                let field_names: Vec<gox_common::Symbol> = struct_type.fields.iter()
                    .filter_map(|f| f.names.first().map(|n| n.symbol))
                    .collect();
                fctx.local_types.insert(type_decl.name.symbol, (slot_count, field_names));
            }
            Ok(())
        }
    }
}

/// Compile short variable declaration (:=).
fn compile_short_var(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    sv: &ShortVarDecl,
) -> Result<(), CodegenError> {
    use gox_syntax::ast::ExprKind;
    
    for (i, name) in sv.names.iter().enumerate() {
        // Check if this is the blank identifier
        let is_blank = ctx.symbols.is(name.symbol, ctx.symbols.sym_blank);
        
        if is_blank {
            // Just compile the RHS and discard
            if i < sv.values.len() {
                expr::compile_expr(ctx, fctx, &sv.values[i])?;
            }
        } else {
            // Determine the type kind and type symbol from the RHS expression
            let type_info = if i < sv.values.len() {
                let info = infer_var_kind_and_type(ctx, Some(fctx), &sv.values[i]);
                // Also check if it's a float expression using fctx
                if info.kind == ValueKind::Int64 && expr::is_float_expr(ctx, fctx, &sv.values[i]) {
                    TypeInfo::new(ValueKind::Float64)
                } else {
                    info
                }
            } else {
                TypeInfo::new(ValueKind::Int64)
            };
            let (mut kind, mut type_sym) = (type_info.kind, type_info.type_sym);
            let field_count = type_info.field_count;
            
            // Check if RHS is an identifier referencing a struct/object variable
            // Or if it's an Index expression into a map (for nested maps)
            let src_struct_info = if i < sv.values.len() {
                match &sv.values[i].kind {
                    ExprKind::Ident(ident) => {
                        if let Some(src_local) = fctx.lookup_local(ident.symbol) {
                            if src_local.kind == ValueKind::Struct && src_local.field_count > 0 {
                                kind = ValueKind::Struct;
                                type_sym = src_local.type_sym;
                                Some(src_local.field_count)  // Need struct copy
                            } else if src_local.kind == ValueKind::Pointer {
                                kind = ValueKind::Pointer;
                                type_sym = src_local.type_sym;
                                None  // No copy needed - reference semantics
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    ExprKind::Index(idx) => {
                        // Check container type for proper handling
                        if let ExprKind::Ident(container_ident) = &idx.expr.kind {
                            if let Some(container_local) = fctx.lookup_local(container_ident.symbol) {
                                match container_local.kind {
                                    ValueKind::Map | ValueKind::Slice => {
                                        // Indexing into map/slice - check element/value type
                                        if let Some(elem_type_sym) = container_local.type_sym {
                                            if let Some(underlying) = expr::lookup_named_type(ctx, elem_type_sym) {
                                                match underlying {
                                                    Type::Struct(s) => {
                                                        let fc = s.fields.len() as u16;
                                                        return Ok(compile_index_struct_copy(ctx, fctx, name, idx, fc, elem_type_sym)?);
                                                    }
                                                    Type::Pointer(_) => {
                                                        kind = ValueKind::Pointer;
                                                        type_sym = Some(elem_type_sym);
                                                    }
                                                    Type::Map(_) => {
                                                        kind = ValueKind::Map;
                                                    }
                                                    _ => {}
                                                }
                                            } else if container_local.kind == ValueKind::Map {
                                                // Value type not found - assume nested map
                                                kind = ValueKind::Map;
                                            }
                                        } else if container_local.kind == ValueKind::Map {
                                            // No type_sym - assume nested map or primitive
                                            kind = ValueKind::Map;
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        None
                    }
                    _ => None
                }
            } else {
                None
            };
            
            // Check if this variable will be captured by a closure
            let is_captured = fctx.is_captured(name.symbol);
            
            // Handle interface type specially (needs 2 slots)
            // Note: := only infers interface type when RHS is interface, so just copy
            if kind == ValueKind::Interface {
                let dst = fctx.define_local_with_kind(*name, 2, ValueKind::Interface);
                if i < sv.values.len() {
                    // RHS is interface - copy both slots (including iface_type)
                    let src = expr::compile_expr(ctx, fctx, &sv.values[i])?;
                    if src != dst {
                        fctx.emit(Opcode::MovN, dst, src, 2);
                    }
                } else {
                    // No RHS but inferred as interface? This shouldn't happen with :=
                    return Err(CodegenError::Internal(
                        "short var decl inferred interface type without RHS".to_string()
                    ));
                }
                continue;
            }
            
            let dst = fctx.define_local_full(*name, 1, kind, field_count, type_sym, is_captured);
            
            if i < sv.values.len() {
                let src = expr::compile_expr(ctx, fctx, &sv.values[i])?;
                
                if is_captured {
                    // Create upval_box and store value in it
                    fctx.emit(Opcode::UpvalNew, dst, 0, 0);
                    if let Some(_field_count) = src_struct_info {
                        // Deep copy struct into temp, then store in upval
                        let tmp = fctx.regs.alloc(1);
                        crate::context::emit_deep_struct_copy(ctx.result, fctx, tmp, src, type_sym);
                        fctx.emit(Opcode::UpvalSet, dst, tmp, 0);
                    } else {
                        fctx.emit(Opcode::UpvalSet, dst, src, 0);
                    }
                } else if src != dst {
                    // Check if we need struct copy
                    if let Some(_field_count) = src_struct_info {
                        // Deep copy struct including nested structs
                        crate::context::emit_deep_struct_copy(ctx.result, fctx, dst, src, type_sym);
                    } else {
                        fctx.emit(Opcode::Mov, dst, src, 0);
                    }
                }
            } else if is_captured {
                // No initial value but captured - create empty upval_box
                fctx.emit(Opcode::UpvalNew, dst, 0, 0);
            }
        }
    }
    Ok(())
}

/// Compile assignment from container index (slice/map) to a struct variable
fn compile_index_struct_copy(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    name: &gox_common::symbol::Ident,
    idx: &gox_syntax::ast::IndexExpr,
    field_count: u16,
    type_sym: gox_common::Symbol,
) -> Result<(), CodegenError> {
    // Define the local variable with struct type
    let dst = fctx.define_local_struct(*name, 1, field_count, Some(type_sym));
    
    // Compile the index expression to get the struct pointer
    let ptr = expr::compile_expr(ctx, fctx, &gox_syntax::ast::Expr {
        id: ExprId::DUMMY,
        kind: ExprKind::Index(Box::new(idx.clone())),
        span: idx.expr.span,
    })?;
    
    // Store the pointer (reference semantics for container elements)
    fctx.emit(Opcode::Mov, dst, ptr, 0);
    
    Ok(())
}

/// Infer type info from an expression (for type tracking)
/// If fctx is provided, also checks local types for composite literals
fn infer_var_kind_and_type(ctx: &CodegenContext, fctx: Option<&FuncContext>, expr: &gox_syntax::ast::Expr) -> TypeInfo {
    use gox_syntax::ast::{ExprKind, TypeExprKind};
    
    let result = match &expr.kind {
        ExprKind::CompositeLit(lit) => {
            match &lit.ty.kind {
                TypeExprKind::Map(map_ty) => {
                    if let TypeExprKind::Ident(val_ident) = &map_ty.value.kind {
                        TypeInfo::with_sym(ValueKind::Map, val_ident.symbol)
                    } else {
                        TypeInfo::new(ValueKind::Map)
                    }
                }
                TypeExprKind::Slice(elem_ty) => {
                    if let TypeExprKind::Ident(elem_ident) = &elem_ty.kind {
                        TypeInfo::with_sym(ValueKind::Slice, elem_ident.symbol)
                    } else {
                        TypeInfo::new(ValueKind::Slice)
                    }
                }
                TypeExprKind::Struct(s) => TypeInfo::with_fields(ValueKind::Struct, s.fields.len() as u16),
                TypeExprKind::Pointer(_) => TypeInfo::new(ValueKind::Pointer),
                TypeExprKind::Ident(ident) => {
                    // Check local types first if fctx is provided
                    if let Some(fc) = fctx {
                        if let Some((slot_count, _)) = fc.get_local_type(ident.symbol) {
                            return TypeInfo::struct_type(*slot_count, Some(ident.symbol));
                        }
                    }
                    // Named type - look up in type check results
                    let field_count = lit.elems.len() as u16;
                    if is_named_type_object(ctx, ident.symbol) {
                        TypeInfo::with_sym(ValueKind::Pointer, ident.symbol)
                    } else {
                        TypeInfo::struct_type(field_count, Some(ident.symbol))
                    }
                }
                _ => TypeInfo::new(ValueKind::Int64),
            }
        }
        ExprKind::Call(_) => {
            // Use unified lookup for all function calls
            if let Some(kind) = ctx.lookup_call_return_type(expr) {
                return TypeInfo::new(kind);
            }
            TypeInfo::new(ValueKind::Int64)
        }
        ExprKind::Selector(sel) => {
            // Accessing a struct field - infer type from the field
            if let ExprKind::Ident(_) = &sel.expr.kind {
                // Look up the struct's type and find the field type
                for named in &ctx.result.named_types {
                    match &named.underlying {
                        Type::Struct(s) => {
                            for field in &s.fields {
                                if field.name == Some(sel.sel.symbol) {
                                    // Found the field - check its type
                                    match &field.ty {
                                        Type::Map(_) => return TypeInfo::new(ValueKind::Map),
                                        Type::Slice(_) => return TypeInfo::new(ValueKind::Slice),
                                        Type::Struct(fs) => return TypeInfo::with_fields(ValueKind::Struct, fs.fields.len() as u16),
                                        Type::Pointer(_) => return TypeInfo::new(ValueKind::Pointer),
                                        _ => {}
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            TypeInfo::new(ValueKind::Int64)
        }
        ExprKind::Index(_) => {
            // Index expressions need FuncContext to check container type
            // This is handled separately in compile_short_var
            TypeInfo::new(ValueKind::Int64)
        }
        ExprKind::FloatLit(_) => TypeInfo::new(ValueKind::Float64),
        ExprKind::StringLit(_) => TypeInfo::new(ValueKind::String),
        ExprKind::Binary(bin) => {
            // If either operand is float, result is float
            // If either operand is string (for +), result is string
            let left_info = infer_var_kind_and_type(ctx, fctx, &bin.left);
            let right_info = infer_var_kind_and_type(ctx, fctx, &bin.right);
            if left_info.kind == ValueKind::String || right_info.kind == ValueKind::String {
                TypeInfo::new(ValueKind::String)
            } else if left_info.kind == ValueKind::Float64 || right_info.kind == ValueKind::Float64 {
                TypeInfo::new(ValueKind::Float64)
            } else {
                TypeInfo::new(ValueKind::Int64)
            }
        }
        ExprKind::Paren(inner) => infer_var_kind_and_type(ctx, fctx, inner),
        _ => TypeInfo::new(ValueKind::Int64),
    };
    result
}
/// Check if a named type is an object type (reference semantics)
fn is_named_type_object(ctx: &CodegenContext, sym: gox_common::Symbol) -> bool {
    expr::lookup_named_type(ctx, sym).map_or(false, |ty| matches!(ty, Type::Pointer(_)))
}

/// Check if a named type is an interface type
fn is_named_interface_type(ctx: &CodegenContext, sym: gox_common::Symbol) -> bool {
    expr::lookup_named_type(ctx, sym).map_or(false, |ty| matches!(ty, Type::Interface(_)))
}

/// Get interface type ID from type expression.
/// Returns index into the interface type table (0 = interface{}, 1+ = named interfaces).
fn get_interface_type_id(ctx: &CodegenContext, ty: Option<&gox_syntax::ast::TypeExpr>) -> u16 {
    use gox_syntax::ast::TypeExprKind;
    
    let ty = match ty {
        Some(t) => t,
        None => return 0, // Default to interface{}
    };
    
    match &ty.kind {
        TypeExprKind::Interface(_) => 0, // Anonymous interface{} 
        TypeExprKind::Ident(ident) => {
            // Named interface - find its index
            get_named_interface_id(ctx, ident.symbol)
        }
        _ => 0,
    }
}

/// Get interface type ID for a named interface type.
fn get_named_interface_id(ctx: &CodegenContext, sym: gox_common::Symbol) -> u16 {
    let mut iface_idx = 1u16; // Start from 1 (0 = interface{})
    for info in ctx.result.named_types.iter() {
        if matches!(info.underlying, Type::Interface(_)) {
            if info.name == sym {
                return iface_idx;
            }
            iface_idx += 1;
        }
    }
    0 // Not found, fallback to interface{}
}

/// Check if an expression is an interface variable
fn is_interface_expr(ctx: &CodegenContext, fctx: &FuncContext, expr: &gox_syntax::ast::Expr) -> bool {
    use gox_syntax::ast::ExprKind;
    
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if let Some(local) = fctx.lookup_local(ident.symbol) {
                return local.kind == ValueKind::Interface;
            }
            false
        }
        ExprKind::Paren(inner) => is_interface_expr(ctx, fctx, inner),
        _ => false,
    }
}

/// Infer runtime type ID for boxing into interface.
/// Returns FIRST_USER_TYPE + idx for named types, or ValueKind as u16 for builtins.
pub fn infer_runtime_type_id(ctx: &CodegenContext, fctx: &FuncContext, expr: &gox_syntax::ast::Expr) -> u16 {
    use gox_syntax::ast::{ExprKind, TypeExprKind};
    
    match &expr.kind {
        ExprKind::IntLit(_) => ValueKind::Int64 as u16,
        ExprKind::FloatLit(_) => ValueKind::Float64 as u16,
        ExprKind::StringLit(_) => ValueKind::String as u16,
        ExprKind::RuneLit(_) => ValueKind::Int32 as u16,
        ExprKind::CompositeLit(lit) => {
            if let TypeExprKind::Ident(ident) = &lit.ty.kind {
                get_named_type_id(ctx, ident.symbol).unwrap_or(ValueKind::Struct as u16)
            } else {
                ValueKind::Struct as u16
            }
        }
        ExprKind::Ident(ident) => {
            if let Some(local) = fctx.lookup_local(ident.symbol) {
                local.type_sym
                    .and_then(|sym| get_named_type_id(ctx, sym))
                    .unwrap_or(local.kind as u16)
            } else {
                ValueKind::Int64 as u16
            }
        }
        ExprKind::Binary(_) => {
            if expr::is_float_expr(ctx, fctx, expr) {
                ValueKind::Float64 as u16
            } else if expr::is_string_expr(ctx, fctx, expr) {
                ValueKind::String as u16
            } else {
                ValueKind::Int64 as u16
            }
        }
        ExprKind::Call(_) => {
            ctx.lookup_call_return_type(expr)
                .map(crate::context::value_kind_to_builtin_type)
                .unwrap_or(ValueKind::Int64 as u16)
        }
        _ => ValueKind::Int64 as u16,
    }
}

/// Get runtime type_id for a named type by symbol.
/// Skips interface types in the count since they don't have runtime type_ids.
fn get_named_type_id(ctx: &CodegenContext, type_sym: gox_common::Symbol) -> Option<u16> {
    use gox_common_core::RuntimeTypeId;
    
    let type_name = ctx.interner.resolve(type_sym)?;
    let mut concrete_idx = 0u32;
    for info in ctx.result.named_types.iter() {
        // Skip interface types
        if matches!(info.underlying, Type::Interface(_)) {
            continue;
        }
        
        let name = ctx.interner.resolve(info.name).unwrap_or("");
        if name == type_name {
            return Some((RuntimeTypeId::FirstStruct as u32 + concrete_idx) as u16);
        }
        concrete_idx += 1;
    }
    None
}


/// Compile assignment statement.
fn compile_assign(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    assign: &AssignStmt,
) -> Result<(), CodegenError> {
    for (i, lhs) in assign.lhs.iter().enumerate() {
        if i >= assign.rhs.len() {
            break;
        }
        
        match &lhs.kind {
            ExprKind::Ident(ident) => {
                // Check for blank identifier
                if ctx.symbols.is(ident.symbol, ctx.symbols.sym_blank) {
                    // Just compile RHS and discard
                    expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                    continue;
                }
                
                if let Some(local) = fctx.lookup_local(ident.symbol) {
                    let dst = local.reg;
                    let local_kind = local.kind;
                    let local_field_count = local.field_count;
                    let is_captured = local.is_captured;
                    
                    // Handle compound assignment to local variable
                    match assign.op {
                        AssignOp::Assign => {
                            // Check if assigning to interface variable
                            if local_kind == ValueKind::Interface {
                                // Check if source is also an interface variable
                                if is_interface_expr(ctx, fctx, &assign.rhs[i]) {
                                    // Interface to interface: copy both slots
                                    let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                                    fctx.emit(Opcode::Mov, dst, src, 0);      // type_id
                                    fctx.emit(Opcode::Mov, dst + 1, src + 1, 0); // data
                                } else {
                                    // Box the value into interface
                                    let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                                    let type_id = infer_runtime_type_id(ctx, fctx, &assign.rhs[i]);
                                    fctx.emit(Opcode::BoxInterface, dst, type_id, src);
                                }
                            } else if is_captured {
                                // Variable is in upval_box - write through it
                                let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                                fctx.emit(Opcode::UpvalSet, dst, src, 0);
                            } else {
                                let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                                if src != dst {
                                    // Check if this is a struct (value type) - need to deep copy
                                    if local_kind == ValueKind::Struct && local_field_count > 0 {
                                        // Allocate new struct and copy fields
                                        fctx.emit(Opcode::Alloc, dst, 0, local_field_count);
                                        for f in 0..local_field_count {
                                            let tmp = fctx.regs.alloc(1);
                                            let byte_offset = (f * 8) as u16;
                                            fctx.emit_with_flags(Opcode::GetField, 0b11, tmp, src, byte_offset);
                                            fctx.emit_with_flags(Opcode::SetField, 0b11, dst, byte_offset, tmp);
                                        }
                                    } else {
                                        // Reference type or primitive - just copy reference
                                        fctx.emit(Opcode::Mov, dst, src, 0);
                                    }
                                }
                            }
                        }
                        AssignOp::Add => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::AddI64, dst, dst, src);
                        }
                        AssignOp::Sub => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::SubI64, dst, dst, src);
                        }
                        AssignOp::Mul => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::MulI64, dst, dst, src);
                        }
                        AssignOp::Div => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::DivI64, dst, dst, src);
                        }
                        AssignOp::Rem => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::ModI64, dst, dst, src);
                        }
                        AssignOp::And => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::Band, dst, dst, src);
                        }
                        AssignOp::Or => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::Bor, dst, dst, src);
                        }
                        AssignOp::Xor => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::Bxor, dst, dst, src);
                        }
                        AssignOp::AndNot => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            let tmp = fctx.regs.alloc(1);
                            fctx.emit(Opcode::Bnot, tmp, src, 0);
                            fctx.emit(Opcode::Band, dst, dst, tmp);
                        }
                        AssignOp::Shl => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::Shl, dst, dst, src);
                        }
                        AssignOp::Shr => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::Shr, dst, dst, src);
                        }
                    }
                } 
                // Check for global variable assignment
                else if let Some(&global_idx) = ctx.global_indices.get(&ident.symbol) {
                    match assign.op {
                        AssignOp::Assign => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::SetGlobal, global_idx as u16, src, 0);
                        }
                        AssignOp::Add | AssignOp::Sub | AssignOp::Mul | AssignOp::Div |
                        AssignOp::Rem | AssignOp::And | AssignOp::Or | AssignOp::Xor |
                        AssignOp::Shl | AssignOp::Shr => {
                            // For compound assignment, load global first
                            let tmp = fctx.regs.alloc(1);
                            fctx.emit(Opcode::GetGlobal, tmp, global_idx as u16, 0);
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            let op = match assign.op {
                                AssignOp::Add => Opcode::AddI64,
                                AssignOp::Sub => Opcode::SubI64,
                                AssignOp::Mul => Opcode::MulI64,
                                AssignOp::Div => Opcode::DivI64,
                                AssignOp::Rem => Opcode::ModI64,
                                AssignOp::And => Opcode::Band,
                                AssignOp::Or => Opcode::Bor,
                                AssignOp::Xor => Opcode::Bxor,
                                AssignOp::Shl => Opcode::Shl,
                                AssignOp::Shr => Opcode::Shr,
                                _ => unreachable!(),
                            };
                            fctx.emit(op, tmp, tmp, src);
                            fctx.emit(Opcode::SetGlobal, global_idx as u16, tmp, 0);
                        }
                        AssignOp::AndNot => {
                            let tmp = fctx.regs.alloc(1);
                            fctx.emit(Opcode::GetGlobal, tmp, global_idx as u16, 0);
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            let tmp2 = fctx.regs.alloc(1);
                            fctx.emit(Opcode::Bnot, tmp2, src, 0);
                            fctx.emit(Opcode::Band, tmp, tmp, tmp2);
                            fctx.emit(Opcode::SetGlobal, global_idx as u16, tmp, 0);
                        }
                    }
                }
                else {
                    return Err(CodegenError::Internal(format!("undefined variable")));
                }
            }
            ExprKind::Index(index_expr) => {
                // Handle slice/map index assignment: a[i] = v or m[k] = v
                let container = expr::compile_expr(ctx, fctx, &index_expr.expr)?;
                let index = expr::compile_expr(ctx, fctx, &index_expr.index)?;
                let value = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                
                // Check if this is a map type (using ctx for Selector support)
                if expr::is_map_expr_with_ctx(ctx, fctx, &index_expr.expr) {
                    // Check if key is a struct - need to compute hash
                    let key = expr::get_struct_key_hash(fctx, &index_expr.index, index);
                    fctx.emit(Opcode::MapSet, container, key, value);
                } else {
                    fctx.emit(Opcode::SliceSet, container, index, value);
                }
            }
            ExprKind::Selector(sel) => {
                // Handle struct/object field assignment: s.x = v
                let obj = expr::compile_expr(ctx, fctx, &sel.expr)?;
                let value = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                
                // First check local types
                if let ExprKind::Ident(ident) = &sel.expr.kind {
                    if let Some(local) = fctx.lookup_local(ident.symbol) {
                        if let Some(type_sym) = local.type_sym {
                            if let Some(field_idx) = fctx.get_local_type_field_index(type_sym, sel.sel.symbol) {
                                let byte_offset = (field_idx * 8) as u16;
                                fctx.emit_with_flags(Opcode::SetField, 0b11, obj, byte_offset, value);
                                continue;
                            }
                        }
                    }
                }
                
                // Get field access info with byte offset
                if let Some(ty) = expr::get_expr_type(ctx, fctx, &sel.expr) {
                    if let Some(access) = expr::get_field_access_by_name(ctx, &ty, sel.sel.symbol) {
                        fctx.emit_with_flags(Opcode::SetField, access.flags(), obj, access.byte_offset as u16, value);
                        continue;
                    }
                }
                let field_name = ctx.interner.resolve(sel.sel.symbol).unwrap_or("<unknown>");
                panic!("compile_assign_stmt: cannot resolve field '{}' - missing type info", field_name);
            }
            _ => {
                return Err(CodegenError::Unsupported("complex assignment target".to_string()));
            }
        }
    }
    Ok(())
}

/// Check if an expression is a map type (for range iteration)
fn is_map_expr(fctx: &FuncContext, expr: &gox_syntax::ast::Expr) -> bool {
    use gox_syntax::ast::ExprKind;
    if let ExprKind::Ident(ident) = &expr.kind {
        if let Some(local) = fctx.lookup_local(ident.symbol) {
            return local.kind == ValueKind::Map;
        }
    }
    false
}

/// Compile return statement.
fn compile_return(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    ret: &ReturnStmt,
) -> Result<(), CodegenError> {
    let ret_count = ret.values.len() as u16;
    
    if ret_count == 0 {
        fctx.emit(Opcode::Return, 0, 0, 0);
    } else {
        let start_reg = fctx.regs.current();
        for (i, e) in ret.values.iter().enumerate() {
            let expected_reg = start_reg + i as u16;
            let actual_reg = expr::compile_expr(ctx, fctx, e)?;
            if actual_reg != expected_reg {
                fctx.emit(Opcode::Mov, expected_reg, actual_reg, 0);
            }
        }
        fctx.emit(Opcode::Return, start_reg, ret_count, 0);
    }
    Ok(())
}

/// Compile if statement.
fn compile_if(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    if_stmt: &IfStmt,
) -> Result<(), CodegenError> {
    // Compile init if present
    if let Some(ref init) = if_stmt.init {
        compile_stmt(ctx, fctx, init)?;
    }
    
    // Compile condition
    let cond_reg = expr::compile_expr(ctx, fctx, &if_stmt.cond)?;
    
    // Jump over then-block if condition is false
    let jump_else_pc = fctx.pc();
    fctx.emit(Opcode::JumpIfNot, cond_reg, 0, 0);
    
    // Compile then-block
    compile_block(ctx, fctx, &if_stmt.then)?;
    
    if let Some(ref else_stmt) = if_stmt.else_ {
        // Jump over else-block
        let jump_end_pc = fctx.pc();
        fctx.emit(Opcode::Jump, 0, 0, 0);
        
        // Patch jump to else
        let else_offset = (fctx.pc() as i32) - (jump_else_pc as i32);
        fctx.patch_jump(jump_else_pc, else_offset);
        
        // Compile else
        compile_stmt(ctx, fctx, else_stmt)?;
        
        // Patch jump to end
        let end_offset = (fctx.pc() as i32) - (jump_end_pc as i32);
        fctx.patch_jump(jump_end_pc, end_offset);
    } else {
        let end_offset = (fctx.pc() as i32) - (jump_else_pc as i32);
        fctx.patch_jump(jump_else_pc, end_offset);
    }
    
    Ok(())
}

/// Compile for statement.
fn compile_for(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    for_stmt: &ForStmt,
) -> Result<(), CodegenError> {
    match &for_stmt.clause {
        ForClause::Cond(cond_opt) => {
            let loop_start = fctx.pc();
            fctx.push_loop(loop_start);  // continue jumps to loop_start
            
            let jump_end_pc = if let Some(cond) = cond_opt {
                let cond_reg = expr::compile_expr(ctx, fctx, cond)?;
                let pc = fctx.pc();
                fctx.emit(Opcode::JumpIfNot, cond_reg, 0, 0);
                Some(pc)
            } else {
                None
            };
            
            compile_block(ctx, fctx, &for_stmt.body)?;
            
            // VM does: pc = pc + offset - 1, so offset = target - current_instr_index
            let jump_pc = fctx.pc();
            fctx.emit(Opcode::Jump, 0, 0, 0);
            let loop_offset = (loop_start as i32) - (jump_pc as i32);
            fctx.patch_jump(jump_pc, loop_offset);
            
            // Patch break jumps
            let loop_ctx = fctx.pop_loop().unwrap();
            let end_pc = fctx.pc();
            for break_pc in loop_ctx.break_pcs {
                let offset = (end_pc as i32) - (break_pc as i32);
                fctx.patch_jump(break_pc, offset);
            }
            
            if let Some(pc) = jump_end_pc {
                let end_offset = (fctx.pc() as i32) - (pc as i32);
                fctx.patch_jump(pc, end_offset);
            }
        }
        ForClause::Three { init, cond, post } => {
            if let Some(init) = init {
                compile_stmt(ctx, fctx, init)?;
            }
            
            let loop_start = fctx.pc();
            
            let jump_end_pc = if let Some(c) = cond {
                let cond_reg = expr::compile_expr(ctx, fctx, c)?;
                let pc = fctx.pc();
                fctx.emit(Opcode::JumpIfNot, cond_reg, 0, 0);
                Some(pc)
            } else {
                None
            };
            
            // For three-clause, continue jumps to post (before loop back)
            // Push loop with placeholder, will update continue_pc after body
            fctx.push_loop(0);  // Placeholder
            
            compile_block(ctx, fctx, &for_stmt.body)?;
            
            // Continue target is here (before post)
            let post_pc = fctx.pc();
            
            // Patch deferred continue jumps
            if let Some(lctx) = fctx.loop_stack.last_mut() {
                for cont_pc in &lctx.continue_pcs {
                    let offset = (post_pc as i32) - (*cont_pc as i32);
                    fctx.code[*cont_pc].b = offset as u16;
                    fctx.code[*cont_pc].c = (offset >> 16) as u16;
                }
                lctx.continue_pcs.clear();
            }
            
            if let Some(post) = post {
                compile_stmt(ctx, fctx, post)?;
            }
            
            // VM does: pc = pc + offset - 1, so offset = target - current_instr_index
            let jump_pc = fctx.pc();
            fctx.emit(Opcode::Jump, 0, 0, 0);
            let loop_offset = (loop_start as i32) - (jump_pc as i32);
            fctx.patch_jump(jump_pc, loop_offset);
            
            // Patch break jumps
            let loop_ctx = fctx.pop_loop().unwrap();
            let end_pc = fctx.pc();
            for break_pc in loop_ctx.break_pcs {
                let offset = (end_pc as i32) - (break_pc as i32);
                fctx.patch_jump(break_pc, offset);
            }
            
            if let Some(pc) = jump_end_pc {
                let end_offset = (fctx.pc() as i32) - (pc as i32);
                fctx.patch_jump(pc, end_offset);
            }
        }
        ForClause::Range { key, value, define, expr: range_expr } => {
            // Compile the range expression (slice/map/string)
            let container = expr::compile_expr(ctx, fctx, range_expr)?;
            
            // Determine iter_type from expression type: 0=slice, 1=map, 2=string
            let iter_type = if is_map_expr(fctx, range_expr) {
                1u16 // map
            } else if expr::is_string_expr(ctx, fctx, range_expr) {
                2u16 // string
            } else {
                0u16 // slice (default)
            };
            
            // Allocate registers for key and value first
            let key_reg = if let Some(k) = key {
                if *define {
                    fctx.define_local(*k, 1);
                }
                fctx.lookup_local(k.symbol).map(|v| v.reg).unwrap_or_else(|| fctx.regs.alloc(1))
            } else {
                fctx.regs.alloc(1) // dummy register
            };
            
            let val_reg = if let Some(v) = value {
                if *define {
                    fctx.define_local(*v, 1);
                }
                fctx.lookup_local(v.symbol).map(|v| v.reg).unwrap_or_else(|| fctx.regs.alloc(1))
            } else {
                fctx.regs.alloc(1) // dummy register
            };
            
            // IterBegin: a=container, b=iter_type
            fctx.emit(Opcode::IterBegin, container, iter_type, 0);
            
            // Loop start - right before IterNext
            let iter_next_pc = fctx.pc();
            fctx.push_loop(iter_next_pc);  // continue jumps back to IterNext
            fctx.emit(Opcode::IterNext, key_reg, val_reg, 0);
            
            // Compile loop body
            compile_block(ctx, fctx, &for_stmt.body)?;
            
            // Jump back to IterNext (not IterBegin!)
            let jump_pc = fctx.pc();
            fctx.emit(Opcode::Jump, 0, 0, 0);
            // offset = target - current, VM does: pc = pc + offset - 1
            // So offset = target - jump_pc
            let back_offset = (iter_next_pc as i32) - (jump_pc as i32);
            fctx.patch_jump(jump_pc, back_offset);
            
            // IterEnd
            fctx.emit(Opcode::IterEnd, 0, 0, 0);
            
            // Patch break jumps - break should jump past IterEnd
            let loop_ctx = fctx.pop_loop().unwrap();
            let after_end = fctx.pc();
            for break_pc in loop_ctx.break_pcs {
                let offset = (after_end as i32) - (break_pc as i32);
                fctx.patch_jump(break_pc, offset);
            }
            
            // Patch IterNext to jump past IterEnd when done
            let end_offset = (fctx.pc() as i32) - (iter_next_pc as i32);
            fctx.code[iter_next_pc].c = end_offset as u16;
        }
    }
    Ok(())
}

/// Compile increment/decrement.
fn compile_inc_dec(
    _ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    expr: &gox_syntax::ast::Expr,
    is_inc: bool,
) -> Result<(), CodegenError> {
    if let ExprKind::Ident(ident) = &expr.kind {
        if let Some(local) = fctx.lookup_local(ident.symbol) {
            let reg = local.reg;
            let one = fctx.regs.alloc(1);
            fctx.emit(Opcode::LoadInt, one, 1, 0);
            
            if is_inc {
                fctx.emit(Opcode::AddI64, reg, reg, one);
            } else {
                fctx.emit(Opcode::SubI64, reg, reg, one);
            }
            
            fctx.regs.free(1);
            return Ok(());
        }
    }
    
    Err(CodegenError::Unsupported("complex inc/dec".to_string()))
}

/// Compile defer statement.
fn compile_defer(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    defer_stmt: &DeferStmt,
) -> Result<(), CodegenError> {
    // defer func() - push the call onto the defer stack
    if let ExprKind::Call(call) = &defer_stmt.call.kind {
        // Compile the function to call
        if let ExprKind::Ident(ident) = &call.func.kind {
            // Direct function call
            if let Some(&func_idx) = ctx.func_indices.get(&ident.symbol) {
                let arg_start = fctx.regs.current();
                let arg_count = call.args.len() as u16;
                
                // Compile arguments
                for arg in &call.args {
                    expr::compile_expr(ctx, fctx, arg)?;
                }
                
                // Push defer with function index and args
                fctx.emit(Opcode::DeferPush, func_idx as u16, arg_start, arg_count);
                return Ok(());
            }
        }
    }
    
    // For complex defer expressions, just skip for now
    Ok(())
}

/// Compile go statement.
fn compile_go(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    go_stmt: &GoStmt,
) -> Result<(), CodegenError> {
    // go func() - spawn a new goroutine
    if let ExprKind::Call(call) = &go_stmt.call.kind {
        if let ExprKind::Ident(ident) = &call.func.kind {
            if let Some(&func_idx) = ctx.func_indices.get(&ident.symbol) {
                let arg_start = fctx.regs.current();
                let arg_count = call.args.len() as u16;
                
                // Compile arguments and ensure they're in consecutive registers
                for (i, arg) in call.args.iter().enumerate() {
                    let expected_reg = arg_start + i as u16;
                    let actual_reg = expr::compile_expr(ctx, fctx, arg)?;
                    if actual_reg != expected_reg {
                        fctx.emit(Opcode::Mov, expected_reg, actual_reg, 0);
                    }
                }
                
                // Spawn goroutine
                fctx.emit(Opcode::Go, func_idx as u16, arg_start, arg_count);
                return Ok(());
            }
        }
    }
    
    Err(CodegenError::Unsupported("complex go expression".to_string()))
}

/// Compile send statement (ch <- value).
fn compile_send(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    send: &SendStmt,
) -> Result<(), CodegenError> {
    let ch_reg = expr::compile_expr(ctx, fctx, &send.chan)?;
    let val_reg = expr::compile_expr(ctx, fctx, &send.value)?;
    fctx.emit(Opcode::ChanSend, ch_reg, val_reg, 0);
    Ok(())
}

/// Compile switch statement.
fn compile_switch(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    sw: &SwitchStmt,
) -> Result<(), CodegenError> {
    // Compile init if present
    if let Some(ref init) = sw.init {
        compile_stmt(ctx, fctx, init)?;
    }
    
    // Compile tag expression (or use true for tagless switch)
    let tag_reg = if let Some(ref tag) = sw.tag {
        expr::compile_expr(ctx, fctx, tag)?
    } else {
        // Tagless switch: compare against true
        let reg = fctx.regs.alloc(1);
        fctx.emit(Opcode::LoadTrue, reg, 0, 0);
        reg
    };
    
    // Track jump addresses for patching
    let mut case_end_jumps: Vec<usize> = Vec::new();
    let mut default_body: Option<&Vec<Stmt>> = None;
    
    // Process each case
    for case in &sw.cases {
        if case.exprs.is_empty() {
            // Default case - save for last
            default_body = Some(&case.body);
            continue;
        }
        
        // Compare tag with each case expression
        let mut match_jumps: Vec<usize> = Vec::new();
        
        for case_expr in &case.exprs {
            let case_reg = expr::compile_expr(ctx, fctx, case_expr)?;
            let cmp_reg = fctx.regs.alloc(1);
            // Use EqI64 for now - TODO: handle different types
            fctx.emit(Opcode::EqI64, cmp_reg, tag_reg, case_reg);
            
            // Jump to case body if match
            let jump_match_pc = fctx.pc();
            fctx.emit(Opcode::JumpIf, cmp_reg, 0, 0);
            match_jumps.push(jump_match_pc);
            
            fctx.regs.free(1); // free cmp_reg
        }
        
        // Jump to next case if no match
        let jump_next_pc = fctx.pc();
        fctx.emit(Opcode::Jump, 0, 0, 0);
        
        // Patch match jumps to here (case body)
        let body_pc = fctx.pc();
        for jump_pc in match_jumps {
            let offset = (body_pc as i32) - (jump_pc as i32);
            fctx.patch_jump(jump_pc, offset);
        }
        
        // Compile case body
        for stmt in &case.body {
            compile_stmt(ctx, fctx, stmt)?;
        }
        
        // Jump to end of switch
        let jump_end_pc = fctx.pc();
        fctx.emit(Opcode::Jump, 0, 0, 0);
        case_end_jumps.push(jump_end_pc);
        
        // Patch jump to next case
        let next_offset = (fctx.pc() as i32) - (jump_next_pc as i32);
        fctx.patch_jump(jump_next_pc, next_offset);
    }
    
    // Compile default case if present
    if let Some(body) = default_body {
        for stmt in body {
            compile_stmt(ctx, fctx, stmt)?;
        }
    }
    
    // Patch all case end jumps to here
    let end_pc = fctx.pc();
    for jump_pc in case_end_jumps {
        let offset = (end_pc as i32) - (jump_pc as i32);
        fctx.patch_jump(jump_pc, offset);
    }
    
    Ok(())
}

/// Compile errdefer statement.
fn compile_errdefer(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    errdefer_stmt: &ErrDeferStmt,
) -> Result<(), CodegenError> {
    // errdefer func() - push the call onto the errdefer stack (only runs on error)
    if let ExprKind::Call(call) = &errdefer_stmt.call.kind {
        // Compile the function to call
        if let ExprKind::Ident(ident) = &call.func.kind {
            // Direct function call
            if let Some(&func_idx) = ctx.func_indices.get(&ident.symbol) {
                let arg_start = fctx.regs.current();
                let arg_count = call.args.len() as u16;
                
                // Compile arguments
                for arg in &call.args {
                    expr::compile_expr(ctx, fctx, arg)?;
                }
                
                // Push errdefer with function index and args
                fctx.emit(Opcode::ErrDeferPush, func_idx as u16, arg_start, arg_count);
                return Ok(());
            }
        }
    }
    
    // For complex errdefer expressions, just skip for now
    Ok(())
}

/// Compile fail statement.
fn compile_fail(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    fail_stmt: &FailStmt,
) -> Result<(), CodegenError> {
    // fail expr - return with error value
    // This is equivalent to: return <zero values>, err
    // The error is an interface type, which occupies 2 slots (type_id, data)
    
    // Compile the error expression (returns 2 consecutive registers for interface)
    let error_reg = expr::compile_expr(ctx, fctx, &fail_stmt.error)?;
    
    // Return with the error value (interface = 2 slots)
    // This allows the VM to detect it as an error return
    fctx.emit(Opcode::Return, error_reg, 2, 0);
    
    Ok(())
}

/// Compile select statement.
fn compile_select(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    sel: &SelectStmt,
) -> Result<(), CodegenError> {
    // Count cases (excluding default)
    let has_default = sel.cases.iter().any(|c| c.comm.is_none());
    let case_count = sel.cases.iter().filter(|c| c.comm.is_some()).count();
    
    // Emit SelectStart
    fctx.emit(Opcode::SelectStart, case_count as u16, has_default as u16, 0);
    
    // Emit each case's communication clause
    for case in &sel.cases {
        if let Some(ref comm) = case.comm {
            match comm {
                CommClause::Send(send) => {
                    // Compile channel and value expressions
                    let chan_reg = expr::compile_expr(ctx, fctx, &send.chan)?;
                    let val_reg = expr::compile_expr(ctx, fctx, &send.value)?;
                    fctx.emit(Opcode::SelectSend, chan_reg, val_reg, 0);
                }
                CommClause::Recv(recv) => {
                    // Compile channel expression
                    let chan_reg = expr::compile_expr(ctx, fctx, &recv.expr)?;
                    
                    // Allocate registers for result
                    let dest_reg = if !recv.lhs.is_empty() {
                        if recv.define {
                            fctx.define_local(recv.lhs[0], 1)
                        } else {
                            fctx.lookup_local(recv.lhs[0].symbol)
                                .map(|v| v.reg)
                                .unwrap_or_else(|| fctx.regs.alloc(1))
                        }
                    } else {
                        fctx.regs.alloc(1) // Discard result
                    };
                    
                    let ok_reg = if recv.lhs.len() > 1 {
                        if recv.define {
                            fctx.define_local(recv.lhs[1], 1)
                        } else {
                            fctx.lookup_local(recv.lhs[1].symbol)
                                .map(|v| v.reg)
                                .unwrap_or_else(|| fctx.regs.alloc(1))
                        }
                    } else {
                        0
                    };
                    
                    fctx.emit(Opcode::SelectRecv, dest_reg, chan_reg, ok_reg);
                }
            }
        }
    }
    
    // Emit SelectEnd - result goes to a temp register
    let result_reg = fctx.regs.alloc(1);
    fctx.emit(Opcode::SelectEnd, result_reg, 0, 0);
    
    // Generate code for case bodies using switch-like structure
    let mut case_jumps: Vec<usize> = Vec::new();
    let mut end_jumps: Vec<usize> = Vec::new();
    
    for (i, case) in sel.cases.iter().enumerate() {
        let case_idx = if case.comm.is_some() {
            sel.cases.iter().take(i).filter(|c| c.comm.is_some()).count()
        } else {
            case_count // default case index
        };
        
        // Jump if not this case
        let cmp_reg = fctx.regs.alloc(1);
        fctx.emit(Opcode::LoadInt, cmp_reg, case_idx as u16, 0);
        
        let ne_reg = fctx.regs.alloc(1);
        fctx.emit(Opcode::NeI64, ne_reg, result_reg, cmp_reg);
        
        let jump_pc = fctx.pc();
        fctx.emit(Opcode::JumpIf, ne_reg, 0, 0); // Will patch
        case_jumps.push(jump_pc);
        
        // Compile case body
        for stmt in &case.body {
            compile_stmt(ctx, fctx, stmt)?;
        }
        
        // Jump to end
        let end_pc = fctx.pc();
        fctx.emit(Opcode::Jump, 0, 0, 0);
        end_jumps.push(end_pc);
    }
    
    // Patch case jumps to skip to next case
    for (i, jump_pc) in case_jumps.iter().enumerate() {
        let next_case_pc = if i + 1 < case_jumps.len() {
            // Find start of next case comparison
            case_jumps[i + 1] - 2 // Back up before LoadInt
        } else {
            fctx.pc()
        };
        let offset = (next_case_pc as i32) - (*jump_pc as i32);
        fctx.patch_jump(*jump_pc, offset);
    }
    
    // Patch end jumps
    let end_pc = fctx.pc();
    for jump_pc in end_jumps {
        let offset = (end_pc as i32) - (jump_pc as i32);
        fctx.patch_jump(jump_pc, offset);
    }
    
    Ok(())
}
