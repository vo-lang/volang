//! Expression compilation.

pub mod binary;
pub mod builtin;
pub mod call;
pub mod comparison;
pub mod conversion;
pub mod dyn_access;
pub mod indexing;
pub mod literal;
pub mod method_value;
pub mod pointer;
pub mod selector;

use vo_runtime::SlotType;
use vo_syntax::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ExprSource, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

// Re-export commonly used items
pub use call::emit_receiver;
pub use literal::{compile_const_value, get_const_value};
pub use pointer::compile_expr_to_ptr;
pub use selector::{is_pkg_qualified_name, traverse_indirect_field};

// =============================================================================
// Tuple Expansion Helper
// =============================================================================

/// Represents a compiled tuple with its elements ready for iteration.
pub struct CompiledTuple {
    /// Base register where tuple is stored
    pub base: u16,
    /// Type of the tuple
    pub tuple_type: vo_analysis::objects::TypeKey,
}

impl CompiledTuple {
    /// Compile a tuple expression and return handle for iterating elements.
    pub fn compile(
        expr: &Expr,
        ctx: &mut CodegenContext,
        func: &mut FuncBuilder,
        info: &TypeInfoWrapper,
    ) -> Result<Self, CodegenError> {
        let tuple_type = info.expr_type(expr.id);
        let slot_types = info.type_slot_types(tuple_type);
        let base = func.alloc_slots(&slot_types);
        compile_expr_to(expr, base, ctx, func, info)?;
        Ok(Self { base, tuple_type })
    }

    /// Iterate over tuple elements, calling f for each (elem_slot, elem_type).
    pub fn for_each_element<F>(&self, info: &TypeInfoWrapper, mut f: F)
    where
        F: FnMut(u16, vo_analysis::objects::TypeKey),
    {
        let mut offset = 0u16;
        for i in 0..info.tuple_len(self.tuple_type) {
            let elem_type = info.tuple_elem_type(self.tuple_type, i);
            f(self.base + offset, elem_type);
            offset += info.type_slot_count(elem_type);
        }
    }
    
    /// Iterate over tuple elements with fallible callback.
    pub fn for_each_element_result<F, E>(&self, info: &TypeInfoWrapper, mut f: F) -> Result<(), E>
    where
        F: FnMut(u16, vo_analysis::objects::TypeKey) -> Result<(), E>,
    {
        let mut offset = 0u16;
        for i in 0..info.tuple_len(self.tuple_type) {
            let elem_type = info.tuple_elem_type(self.tuple_type, i);
            f(self.base + offset, elem_type)?;
            offset += info.type_slot_count(elem_type);
        }
        Ok(())
    }
}

// =============================================================================
// Helper Functions
// =============================================================================


/// Get ExprSource for an expression - determines where the value comes from.
pub fn get_expr_source(
    expr: &Expr,
    ctx: &CodegenContext,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> ExprSource {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if let Some(local) = func.lookup_local(ident.symbol) {
                return ExprSource::Location(local.storage);
            }
            let obj_key = info.get_use(ident);
            if let Some(global_idx) = ctx.get_global_index(obj_key) {
                if let Some(type_key) = info.try_obj_type(obj_key) {
                    // Global arrays are stored as GcRef (1 slot)
                    let slots = if info.is_array(type_key) { 1 } else { info.type_slot_count(type_key) };
                    return ExprSource::Location(StorageKind::Global { index: global_idx as u16, slots });
                }
            }
        }
        ExprKind::Selector(sel) => {
            if is_pkg_qualified_name(sel, info) {
                return ExprSource::NeedsCompile;
            }
            // Check if this is a method value (t.M) - needs special handling
            if let Some(selection) = info.get_selection(expr.id) {
                if *selection.kind() == vo_analysis::selection::SelectionKind::MethodVal {
                    return ExprSource::NeedsCompile;
                }
                // Indirect selection (embedded pointer fields) requires runtime deref
                if selection.indirect() {
                    return ExprSource::NeedsCompile;
                }
            }
            let expr_type = info.expr_type(expr.id);
            let field_slots = info.type_slot_count(expr_type);
            let recv_type = info.expr_type(sel.expr.id);
            if info.is_pointer(recv_type) {
                return ExprSource::NeedsCompile;
            }
            if let ExprSource::Location(StorageKind::StackValue { slot: base_slot, .. }) = 
                get_expr_source(&sel.expr, ctx, func, info) 
            {
                if let Some(field_name) = info.project.interner.resolve(sel.sel.symbol) {
                    let (offset, _) = info.selector_field_offset(expr.id, recv_type, field_name);
                    return ExprSource::Location(StorageKind::StackValue { 
                        slot: base_slot + offset, 
                        slots: field_slots 
                    });
                }
            }
        }
        ExprKind::Paren(inner) => {
            return get_expr_source(inner, ctx, func, info);
        }
        _ => {}
    }
    ExprSource::NeedsCompile
}

/// Get the GcRef slot from a StorageKind.
pub fn get_gcref_slot(storage: &StorageKind) -> Option<u16> {
    match storage {
        StorageKind::HeapBoxed { gcref_slot, .. } => Some(*gcref_slot),
        StorageKind::HeapArray { gcref_slot, .. } => Some(*gcref_slot),
        StorageKind::Reference { slot } => Some(*slot),
        StorageKind::StackValue { .. } | StorageKind::StackArray { .. } | StorageKind::Global { .. } => None,
    }
}

/// Compile a map key expression, boxing to interface if needed.
/// Used for static map index/set/delete and map literal Expr keys.
pub fn compile_map_key_expr(
    index_expr: &Expr,
    key_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let index_type = info.expr_type(index_expr.id);
    let needs_boxing = info.is_interface(key_type) && !info.is_interface(index_type);
    
    if needs_boxing {
        let src_reg = compile_expr(index_expr, ctx, func, info)?;
        let key_slot_types = info.type_slot_types(key_type);
        let iface_reg = func.alloc_slots(&key_slot_types);
        crate::assign::emit_iface_assign_from_concrete(
            iface_reg, src_reg, index_type, key_type, ctx, func, info
        )?;
        Ok(iface_reg)
    } else {
        compile_expr(index_expr, ctx, func, info)
    }
}

/// Compile an element expression to a destination slot, boxing to interface if the target type is interface.
/// Common pattern for array/slice/map element initialization.
pub fn compile_elem_to(
    elem_expr: &Expr,
    dst: u16,
    target_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    crate::assign::emit_assign(dst, crate::assign::AssignSource::Expr(elem_expr), target_type, ctx, func, info)
}

/// Get the GcRef slot of an escaped variable without copying.
fn get_escaped_var_gcref(
    expr: &Expr,
    ctx: &CodegenContext,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> Option<u16> {
    match get_expr_source(expr, ctx, func, info) {
        ExprSource::Location(storage) => get_gcref_slot(&storage),
        _ => None,
    }
}

/// Match nil comparison in if condition for optimization.
pub fn match_nil_comparison<'a>(
    bin: &'a vo_syntax::ast::BinaryExpr,
    info: &TypeInfoWrapper,
) -> Option<(&'a Expr, bool)> {
    let is_nil = |e: &Expr| -> bool {
        if let ExprKind::Ident(id) = &e.kind {
            info.project.interner.resolve(id.symbol) == Some("nil")
        } else {
            false
        }
    };
    match bin.op {
        BinaryOp::Eq | BinaryOp::NotEq => {
            let is_eq = bin.op == BinaryOp::Eq;
            if is_nil(&bin.right) {
                Some((&bin.left, is_eq))
            } else if is_nil(&bin.left) {
                Some((&bin.right, is_eq))
            } else {
                None
            }
        }
        _ => None,
    }
}

// =============================================================================
// Main Entry Points
// =============================================================================

/// Compile expression, return result slot.
pub fn compile_expr(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    if let ExprSource::Location(storage) = get_expr_source(expr, ctx, func, info) {
        match storage {
            StorageKind::StackValue { slot, slots: 1 } => return Ok(slot),
            StorageKind::Reference { slot } => return Ok(slot),
            _ => {}
        }
    }
    // Use expression type's slot types to ensure GcRefs (strings, pointers, etc.) are tracked by GC
    let expr_type = info.expr_type(expr.id);
    let slot_types = info.type_slot_types(expr_type);
    let dst = func.alloc_slots(&slot_types);
    compile_expr_to(expr, dst, ctx, func, info)?;
    // Truncate narrow integer types to ensure Go semantics (operations in 64-bit, result in type width)
    emit_int_trunc(dst, expr_type, func, info);
    Ok(dst)
}

/// Compile expression with implicit conversion to target type.
/// Handles cases like struct -> interface conversion automatically.
pub fn compile_expr_to_type(
    expr: &Expr,
    target_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let src_type = info.expr_type(expr.id);
    
    // Check if implicit interface conversion is needed
    if info.is_interface(target_type) && !info.is_interface(src_type) {
        let dst = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]); // interface is 2 slots
        crate::assign::emit_assign(dst, crate::assign::AssignSource::Expr(expr), target_type, ctx, func, info)?;
        Ok(dst)
    } else {
        compile_expr(expr, ctx, func, info)
    }
}

/// Compile expression to specified slot.
pub fn compile_expr_to(
    expr: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    match &expr.kind {
        // === Literals ===
        ExprKind::IntLit(_) | ExprKind::RuneLit(_) | ExprKind::FloatLit(_) | ExprKind::StringLit(_) => {
            let val = get_const_value(expr.id, info)
                .ok_or_else(|| CodegenError::Internal("literal has no const value".to_string()))?;
            let target_type = info.expr_type(expr.id);
            compile_const_value(val, dst, target_type, ctx, func, info)?;
        }

        // === Identifier ===
        ExprKind::Ident(ident) => {
            if info.project.interner.resolve(ident.symbol) == Some("nil") {
                func.emit_op(Opcode::LoadInt, dst, 0, 0);
                return Ok(());
            }
            if let Some(val) = get_const_value(expr.id, info) {
                let target_type = info.expr_type(expr.id);
                compile_const_value(val, dst, target_type, ctx, func, info)?;
                return Ok(());
            }
            match get_expr_source(expr, ctx, func, info) {
                ExprSource::Location(storage) => {
                    func.emit_storage_load(storage, dst);
                }
                ExprSource::NeedsCompile => {
                    let obj_key = info.get_use(ident);
                    // Closure capture: ClosureGet returns GcRef to the captured storage
                    if let Some(capture) = func.lookup_capture(ident.symbol) {
                        func.emit_op(Opcode::ClosureGet, dst, capture.index, 0);
                        
                        // Arrays: capture stores GcRef to [ArrayHeader][elems], use directly
                        // Others: capture stores GcRef to box [value], need PtrGet to read value
                        let type_key = info.obj_type(obj_key, "captured var must have type");
                        if !info.is_array(type_key) {
                            let value_slots = info.type_slot_count(type_key);
                            func.emit_ptr_get(dst, dst, 0, value_slots);
                        }
                    } else {
                        // Function reference: create closure with no captures
                        if let Some(func_idx) = ctx.get_func_by_objkey(obj_key) {
                            func.emit_closure_new(dst, func_idx, 0);
                        } else {
                            return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
                        }
                    }
                }
            }
        }

        // === Binary operations ===
        ExprKind::Binary(bin) => {
            binary::compile_binary(expr, bin, dst, ctx, func, info)?;
        }

        // === Unary operations ===
        ExprKind::Unary(unary) => {
            if let Some(val) = get_const_value(expr.id, info) {
                let target_type = info.expr_type(expr.id);
                compile_const_value(val, dst, target_type, ctx, func, info)?;
                return Ok(());
            }
            match unary.op {
                UnaryOp::Addr => {
                    pointer::compile_addr_of(&unary.operand, dst, ctx, func, info)?;
                }
                UnaryOp::Deref => {
                    pointer::compile_deref(&unary.operand, dst, ctx, func, info)?;
                }
                UnaryOp::Neg => {
                    let operand = compile_expr(&unary.operand, ctx, func, info)?;
                    let type_key = info.expr_type(expr.id);
                    let is_float = info.is_float(type_key);
                    let opcode = if is_float { Opcode::NegF } else { Opcode::NegI };
                    func.emit_op(opcode, dst, operand, 0);
                }
                UnaryOp::Not => {
                    let operand = compile_expr(&unary.operand, ctx, func, info)?;
                    func.emit_op(Opcode::BoolNot, dst, operand, 0);
                }
                UnaryOp::BitNot => {
                    let operand = compile_expr(&unary.operand, ctx, func, info)?;
                    func.emit_op(Opcode::Not, dst, operand, 0);
                }
                UnaryOp::Pos => {
                    compile_expr_to(&unary.operand, dst, ctx, func, info)?;
                }
            }
        }

        // === Parentheses ===
        ExprKind::Paren(inner) => {
            compile_expr_to(inner, dst, ctx, func, info)?;
        }

        // === Selector ===
        ExprKind::Selector(sel) => {
            selector::compile_selector(expr, sel, dst, ctx, func, info)?;
        }

        // === Index ===
        ExprKind::Index(idx) => {
            indexing::compile_index(expr, idx, dst, ctx, func, info)?;
        }

        // === Slice ===
        ExprKind::Slice(slice_expr) => {
            indexing::compile_slice_expr(expr, slice_expr, dst, ctx, func, info)?;
        }

        // === Type assertion ===
        ExprKind::TypeAssert(type_assert) => {
            compile_type_assert(expr, type_assert, dst, ctx, func, info)?;
        }

        // === Channel receive ===
        ExprKind::Receive(chan_expr) => {
            compile_receive(expr, chan_expr, dst, ctx, func, info)?;
        }

        // === Type conversion ===
        ExprKind::Conversion(conv) => {
            conversion::compile_conversion(expr, conv, dst, ctx, func, info)?;
        }

        // === Composite literal ===
        ExprKind::CompositeLit(lit) => {
            literal::compile_composite_lit(expr, lit, dst, ctx, func, info)?;
        }

        // === Type as expression ===
        ExprKind::TypeAsExpr(_) => {
            func.emit_op(Opcode::LoadInt, dst, 0, 0);
        }

        // === Try unwrap ===
        ExprKind::TryUnwrap(inner) => {
            compile_try_unwrap(inner, dst, ctx, func, info)?;
        }

        // === Dynamic access ===
        ExprKind::DynAccess(dyn_access_expr) => {
            dyn_access::compile_dyn_access(expr, dyn_access_expr, dst, ctx, func, info)?;
        }

        // === Call ===
        ExprKind::Call(call_expr) => {
            call::compile_call(expr, call_expr, dst, ctx, func, info)?;
        }

        // === Function literal ===
        ExprKind::FuncLit(func_lit) => {
            literal::compile_func_lit(expr, func_lit, dst, ctx, func, info)?;
        }

        // === Ellipsis (only valid in array type [...]T) ===
        ExprKind::Ellipsis => {
            return Err(CodegenError::Internal("Ellipsis expression should not be compiled directly".to_string()));
        }
    }

    Ok(())
}

// =============================================================================
// Expression Compilation Helpers
// =============================================================================

fn compile_type_assert(
    expr: &Expr,
    type_assert: &vo_syntax::ast::TypeAssertExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let iface_reg = compile_expr(&type_assert.expr, ctx, func, info)?;
    let target_type = type_assert.ty.as_ref()
        .map(|ty| info.type_expr_type(ty.id))
        .expect("type assertion must have target type");
    let target_slots = info.type_slot_count(target_type) as u8;
    let result_type = info.expr_type(expr.id);
    let has_ok = info.is_tuple(result_type);

    let (assert_kind, target_id): (u8, u32) = if info.is_interface(target_type) {
        let iface_meta_id = info.get_or_create_interface_meta_id(target_type, ctx);
        (1, iface_meta_id)
    } else {
        let rt = info.type_to_runtime_type(target_type, ctx);
        let rttid = ctx.intern_rttid(rt);
        (0, rttid)
    };

    let flags = assert_kind | (if has_ok { 1 << 2 } else { 0 }) | ((target_slots) << 3);
    func.emit_with_flags(Opcode::IfaceAssert, flags, dst, iface_reg, target_id as u16);
    Ok(())
}

fn compile_receive(
    expr: &Expr,
    target_expr: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let target_reg = compile_expr(target_expr, ctx, func, info)?;
    let target_type = info.expr_type(target_expr.id);
    
    if info.is_port(target_type) {
        // Port receive: <-p
        let elem_slots = info.port_elem_slots(target_type);
        let result_slots = info.expr_slots(expr.id);
        let has_ok = result_slots > elem_slots;
        let flags = ((elem_slots as u8) << 1) | (if has_ok { 1 } else { 0 });
        func.emit_with_flags(Opcode::PortRecv, flags, dst, target_reg, 0);
    } else {
        // Channel receive: <-ch
        let elem_slots = info.chan_elem_slots(target_type);
        let result_slots = info.expr_slots(expr.id);
        let has_ok = result_slots > elem_slots;
        let flags = ((elem_slots as u8) << 1) | (if has_ok { 1 } else { 0 });
        func.emit_with_flags(Opcode::ChanRecv, flags, dst, target_reg, 0);
    }
    Ok(())
}

fn compile_try_unwrap(
    inner: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let inner_type = info.expr_type(inner.id);
    let inner_slots = info.type_slot_count(inner_type);
    let inner_slot_types = info.type_slot_types(inner_type);
    let inner_start = func.alloc_slots(&inner_slot_types);
    compile_expr_to(inner, inner_start, ctx, func, info)?;
    
    let error_slots = 2u16;
    let error_start = inner_start + inner_slots - error_slots;
    let skip_fail_jump = func.emit_jump(Opcode::JumpIfNot, error_start);
    
    // Check if function returns error - determines propagate vs panic behavior
    if func.has_error_return(info) {
        // Use shared emit_error_return which handles escaped named returns correctly
        crate::stmt::emit_error_return(error_start, func, info);
    } else {
        // Panic mode: panic with error directly
        let panic_extern = ctx.get_or_register_extern("panic_with_error");
        func.emit_with_flags(Opcode::CallExtern, 2, error_start, panic_extern as u16, error_start);
    }
    
    func.patch_jump(skip_fail_jump, func.current_pc());
    
    let value_slots = inner_slots - error_slots;
    if value_slots > 0 {
        func.emit_copy(dst, inner_start, value_slots);
    }
    Ok(())
}

/// Emit truncation for narrow integer types (int8/16/32, uint8/16/32).
/// This ensures Go semantics where operations are done in 64-bit but
/// results are truncated to the target type width.
/// 
/// On 32-bit platforms, Int and Uint are also truncated to 32 bits.
/// 
/// Safe to call on any type - non-narrow-integer types are no-ops.
pub fn emit_int_trunc(
    reg: u16,
    type_key: vo_analysis::objects::TypeKey,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    if !info.is_int(type_key) {
        return;
    }
    use vo_runtime::ValueKind;
    let vk = info.type_value_kind(type_key);
    // flags: high bit (0x80) = signed, low bits = byte width
    match vk {
        ValueKind::Int8 => func.emit_with_flags(Opcode::Trunc, 0x81, reg, reg, 0),
        ValueKind::Int16 => func.emit_with_flags(Opcode::Trunc, 0x82, reg, reg, 0),
        ValueKind::Int32 => func.emit_with_flags(Opcode::Trunc, 0x84, reg, reg, 0),
        ValueKind::Uint8 => func.emit_with_flags(Opcode::Trunc, 0x01, reg, reg, 0),
        ValueKind::Uint16 => func.emit_with_flags(Opcode::Trunc, 0x02, reg, reg, 0),
        ValueKind::Uint32 => func.emit_with_flags(Opcode::Trunc, 0x04, reg, reg, 0),
        // On 32-bit platforms, Int and Uint are 32-bit and need truncation
        #[cfg(target_pointer_width = "32")]
        ValueKind::Int => func.emit_with_flags(Opcode::Trunc, 0x84, reg, reg, 0),
        #[cfg(target_pointer_width = "32")]
        ValueKind::Uint => func.emit_with_flags(Opcode::Trunc, 0x04, reg, reg, 0),
        _ => {} // No truncation needed for Int64, Uint64, and Int/Uint on 64-bit
    }
}
