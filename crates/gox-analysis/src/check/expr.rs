//! Expression type checking.
//!
//! This module type-checks expressions and records their types in TypeInfo.
//!
//! Basic algorithm:
//! Expressions are checked recursively, top down. Expression checker functions
//! are generally of the form:
//!   fn f(x: &mut Operand, e: &Expr, ...)
//! where e is the expression to be checked, and x is the result of the check.

#![allow(dead_code)]

use gox_common::vfs::FileSystem;
use gox_syntax::ast::{BinaryOp, Expr, ExprKind, UnaryOp};

use crate::obj::LangObj;
use crate::objects::TypeKey;
use crate::operand::{Operand, OperandMode};
use crate::scope;
use crate::typ::{self, BasicType, Type};

use super::checker::Checker;

impl<F: FileSystem> Checker<F> {
    /// Type-checks an expression and returns its type.
    pub fn check_expr(&mut self, expr: &Expr) -> TypeKey {
        let (mode, typ) = self.check_expr_impl(expr);
        self.result.record_type(expr.id, mode, typ);
        typ
    }

    /// Internal implementation of expression checking.
    fn check_expr_impl(&mut self, expr: &Expr) -> (OperandMode, TypeKey) {
        match &expr.kind {
            ExprKind::Ident(ident) => self.check_ident(ident),
            ExprKind::IntLit(_) => self.check_int_lit(),
            ExprKind::FloatLit(_) => self.check_float_lit(),
            ExprKind::RuneLit(_) => self.check_rune_lit(),
            ExprKind::StringLit(_) => self.check_string_lit(),
            ExprKind::Binary(bin) => self.check_binary(&bin.left, bin.op, &bin.right),
            ExprKind::Unary(unary) => self.check_unary(unary.op, &unary.operand),
            ExprKind::Call(call) => self.check_call(&call.func, &call.args),
            ExprKind::Index(idx) => self.check_index(&idx.expr, &idx.index),
            ExprKind::Slice(sl) => self.check_slice(&sl.expr, sl.low.as_ref(), sl.high.as_ref()),
            ExprKind::Selector(sel) => self.check_selector(&sel.expr, &sel.sel),
            ExprKind::TypeAssert(ta) => self.check_type_assert(&ta.expr, ta.ty.as_ref()),
            ExprKind::CompositeLit(lit) => self.check_composite_lit(&lit.ty, &lit.elems),
            ExprKind::FuncLit(func) => self.check_func_lit(&func.sig, &func.body),
            ExprKind::Conversion(conv) => self.check_conversion(&conv.ty, &conv.expr),
            ExprKind::Receive(recv) => self.check_receive(recv),
            ExprKind::Paren(inner) => self.check_expr_impl(inner),
            ExprKind::TypeAsExpr(ty) => {
                let typ = self.resolve_type(ty);
                (OperandMode::TypeExpr, typ)
            }
            ExprKind::TryUnwrap(inner) => self.check_try_unwrap(inner),
        }
    }

    /// Checks an identifier expression.
    fn check_ident(&mut self, _ident: &gox_common::symbol::Ident) -> (OperandMode, TypeKey) {
        let invalid = self.universe().lookup_type(BasicType::Invalid).unwrap();
        
        // Look up identifier in scope
        if let Some(scope_key) = self.octx.scope {
            if let Some((_skey, okey)) = scope::lookup_parent(scope_key, "", &self.tc_objs) {
                let obj = &self.tc_objs.lobjs[okey];
                if let Some(typ) = obj.typ() {
                    // Determine mode based on entity type
                    use crate::obj::EntityType;
                    let mode = match obj.entity_type() {
                        EntityType::Const { val } => OperandMode::Constant(val.clone()),
                        EntityType::Var(_) => OperandMode::Variable,
                        EntityType::Func { .. } => OperandMode::Value,
                        EntityType::TypeName => OperandMode::TypeExpr,
                        EntityType::Builtin(id) => OperandMode::Builtin(*id),
                        EntityType::Nil => OperandMode::Value,
                        EntityType::PkgName { .. } => OperandMode::Value,
                        EntityType::Label { .. } => OperandMode::Invalid,
                    };
                    return (mode, typ);
                }
            }
        }
        
        // Identifier not found
        (OperandMode::Invalid, invalid)
    }

    /// Checks an integer literal.
    fn check_int_lit(&self) -> (OperandMode, TypeKey) {
        let typ = self.universe().lookup_type(BasicType::UntypedInt).unwrap();
        (OperandMode::Constant(crate::obj::ConstValue::Int(0.into())), typ)
    }

    /// Checks a float literal.
    fn check_float_lit(&self) -> (OperandMode, TypeKey) {
        let typ = self.universe().lookup_type(BasicType::UntypedFloat).unwrap();
        (OperandMode::Constant(crate::obj::ConstValue::Float(0.0.into())), typ)
    }

    /// Checks a rune literal.
    fn check_rune_lit(&self) -> (OperandMode, TypeKey) {
        let typ = self.universe().lookup_type(BasicType::UntypedRune).unwrap();
        (OperandMode::Constant(crate::obj::ConstValue::Int(0.into())), typ)
    }

    /// Checks a string literal.
    fn check_string_lit(&self) -> (OperandMode, TypeKey) {
        let typ = self.universe().lookup_type(BasicType::UntypedString).unwrap();
        (OperandMode::Constant(crate::obj::ConstValue::with_str(String::new())), typ)
    }

    /// Checks a binary expression.
    fn check_binary(
        &mut self,
        left: &Expr,
        op: BinaryOp,
        right: &Expr,
    ) -> (OperandMode, TypeKey) {
        let left_typ = self.check_expr(left);
        let right_typ = self.check_expr(right);
        
        // For comparison operators, result is always bool
        if Self::is_comparison_op(op) {
            let bool_typ = self.universe().lookup_type(BasicType::Bool).unwrap();
            return (OperandMode::Value, bool_typ);
        }
        
        // For logical operators, result is bool
        if Self::is_logical_op(op) {
            let bool_typ = self.universe().lookup_type(BasicType::Bool).unwrap();
            return (OperandMode::Value, bool_typ);
        }
        
        // For arithmetic/bitwise operators, result type is the operand type
        // TODO: Handle type conversion between operands
        (OperandMode::Value, left_typ)
    }
    
    /// Returns true if op is a comparison operator.
    fn is_comparison_op(op: BinaryOp) -> bool {
        matches!(op, 
            BinaryOp::Eq | BinaryOp::NotEq | 
            BinaryOp::Lt | BinaryOp::LtEq | 
            BinaryOp::Gt | BinaryOp::GtEq
        )
    }
    
    /// Returns true if op is a logical operator.
    fn is_logical_op(op: BinaryOp) -> bool {
        matches!(op, BinaryOp::LogAnd | BinaryOp::LogOr)
    }
    
    /// Returns true if op is a shift operator.
    fn is_shift_op(op: BinaryOp) -> bool {
        matches!(op, BinaryOp::Shl | BinaryOp::Shr)
    }

    /// Checks a unary expression.
    fn check_unary(
        &mut self,
        op: UnaryOp,
        operand: &Expr,
    ) -> (OperandMode, TypeKey) {
        let typ = self.check_expr(operand);
        
        match op {
            UnaryOp::Not => {
                // Logical not - result is bool
                let bool_typ = self.universe().lookup_type(BasicType::Bool).unwrap();
                (OperandMode::Value, bool_typ)
            }
            UnaryOp::Neg | UnaryOp::Pos | UnaryOp::BitNot => {
                // Arithmetic operators preserve type
                (OperandMode::Value, typ)
            }
            UnaryOp::Addr => {
                // Address-of operator - create pointer type
                let ptr_detail = crate::typ::PointerDetail::new(typ);
                let ptr_typ = self.tc_objs.types.insert(Type::Pointer(ptr_detail));
                (OperandMode::Value, ptr_typ)
            }
            UnaryOp::Deref => {
                // Dereference - get base type from pointer
                if let Type::Pointer(ptr) = &self.tc_objs.types[typ] {
                    (OperandMode::Variable, ptr.base())
                } else {
                    let invalid = self.universe().lookup_type(BasicType::Invalid).unwrap();
                    (OperandMode::Invalid, invalid)
                }
            }
        }
    }

    /// Checks a call expression.
    fn check_call(&mut self, func: &Expr, args: &[Expr]) -> (OperandMode, TypeKey) {
        let _func_typ = self.check_expr(func);
        for arg in args {
            self.check_expr(arg);
        }
        // TODO: Determine return type from function signature
        let invalid = self.universe().lookup_type(BasicType::Invalid).unwrap();
        (OperandMode::Value, invalid)
    }

    /// Checks an index expression.
    fn check_index(&mut self, expr: &Expr, index: &Expr) -> (OperandMode, TypeKey) {
        let _base_typ = self.check_expr(expr);
        let _index_typ = self.check_expr(index);
        // TODO: Determine element type from base type
        let invalid = self.universe().lookup_type(BasicType::Invalid).unwrap();
        (OperandMode::Variable, invalid)
    }

    /// Checks a slice expression.
    fn check_slice(
        &mut self,
        expr: &Expr,
        low: Option<&Expr>,
        high: Option<&Expr>,
    ) -> (OperandMode, TypeKey) {
        let base_typ = self.check_expr(expr);
        if let Some(l) = low {
            self.check_expr(l);
        }
        if let Some(h) = high {
            self.check_expr(h);
        }
        // TODO: Determine slice type
        (OperandMode::Value, base_typ)
    }

    /// Checks a selector expression.
    fn check_selector(
        &mut self,
        expr: &Expr,
        _sel: &gox_common::symbol::Ident,
    ) -> (OperandMode, TypeKey) {
        let _base_typ = self.check_expr(expr);
        // TODO: Look up field/method in base type
        let invalid = self.universe().lookup_type(BasicType::Invalid).unwrap();
        (OperandMode::Variable, invalid)
    }

    /// Checks a type assertion expression.
    fn check_type_assert(
        &mut self,
        expr: &Expr,
        ty: Option<&gox_syntax::ast::TypeExpr>,
    ) -> (OperandMode, TypeKey) {
        let _base_typ = self.check_expr(expr);
        if let Some(t) = ty {
            let target = self.resolve_type(t);
            return (OperandMode::Value, target);
        }
        // Type switch case - return invalid
        let invalid = self.universe().lookup_type(BasicType::Invalid).unwrap();
        (OperandMode::Invalid, invalid)
    }

    /// Checks a composite literal.
    fn check_composite_lit(
        &mut self,
        ty: &gox_syntax::ast::TypeExpr,
        elems: &[gox_syntax::ast::CompositeLitElem],
    ) -> (OperandMode, TypeKey) {
        let typ = self.resolve_type(ty);
        for elem in elems {
            self.check_expr(&elem.value);
        }
        (OperandMode::Value, typ)
    }

    /// Checks a function literal.
    fn check_func_lit(
        &mut self,
        _sig: &gox_syntax::ast::FuncSig,
        _body: &gox_syntax::ast::Block,
    ) -> (OperandMode, TypeKey) {
        // TODO: Check function body
        let invalid = self.universe().lookup_type(BasicType::Invalid).unwrap();
        (OperandMode::Value, invalid)
    }

    /// Checks a type conversion.
    fn check_conversion(
        &mut self,
        ty: &gox_syntax::ast::TypeExpr,
        expr: &Expr,
    ) -> (OperandMode, TypeKey) {
        let target = self.resolve_type(ty);
        self.check_expr(expr);
        (OperandMode::Value, target)
    }

    /// Checks a receive expression.
    fn check_receive(&mut self, expr: &Expr) -> (OperandMode, TypeKey) {
        let _chan_typ = self.check_expr(expr);
        // TODO: Get element type from channel type
        let invalid = self.universe().lookup_type(BasicType::Invalid).unwrap();
        (OperandMode::CommaOk, invalid)
    }

    /// Checks a try-unwrap expression (?).
    fn check_try_unwrap(&mut self, expr: &Expr) -> (OperandMode, TypeKey) {
        let typ = self.check_expr(expr);
        // TODO: Handle error unwrapping
        (OperandMode::Value, typ)
    }
}
