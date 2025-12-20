//! Expression type checking.
//!
//! Basic algorithm:
//!
//! Expressions are checked recursively, top down. Expression checker functions
//! are generally of the form:
//!   fn f(x: &mut Operand, e: &Expr, ...)
//! where e is the expression to be checked, and x is the result of the check.
//! The check performed by f may fail in which case x.mode == OperandMode::Invalid,
//! and related error messages will have been issued by f.
//!
//! If a hint argument is present, it is the composite literal element type
//! of an outer composite literal; it is used to type-check composite literal
//! elements that have no explicit type specification in the source.
//!
//! All expressions are checked via raw_expr, which dispatches according
//! to expression kind. Upon returning, raw_expr is recording the types and
//! constant values for all expressions that have an untyped type.
//!
//! Untyped expressions may eventually become fully typed (i.e., not untyped),
//! typically when the value is assigned to a variable, or is used otherwise.
//! The update_expr_type method is used to record this final type and update
//! the recorded types.

#![allow(dead_code)]

use gox_common::span::Span;
use gox_common::vfs::FileSystem;
use gox_common_core::ExprId;
use gox_syntax::ast::{BinaryOp, Expr, ExprKind, UnaryOp};

use crate::constant::Value;
use crate::obj::LangObj;
use crate::objects::TypeKey;
use crate::operand::{Operand, OperandMode};
use crate::scope;
use crate::typ::{self, BasicType, Type};

use super::checker::{Checker, FilesContext};

impl<F: FileSystem> Checker<F> {
    // =========================================================================
    // Part 1: Operator checking and unary expressions
    // =========================================================================

    /// Checks if the operator is valid for the operand type.
    /// Returns true if valid, false otherwise.
    fn op_token(&self, x: &mut Operand, op: BinaryOp, binary: bool) -> bool {
        let pred = |o: BinaryOp, ty: TypeKey| -> Option<bool> {
            if binary {
                match o {
                    BinaryOp::Add => {
                        Some(typ::is_numeric(ty, &self.tc_objs) || typ::is_string(ty, &self.tc_objs))
                    }
                    BinaryOp::Sub => Some(typ::is_numeric(ty, &self.tc_objs)),
                    BinaryOp::Mul => Some(typ::is_numeric(ty, &self.tc_objs)),
                    BinaryOp::Div => Some(typ::is_numeric(ty, &self.tc_objs)),
                    BinaryOp::Rem => Some(typ::is_integer(ty, &self.tc_objs)),
                    BinaryOp::And => Some(typ::is_integer(ty, &self.tc_objs)),
                    BinaryOp::Or => Some(typ::is_integer(ty, &self.tc_objs)),
                    BinaryOp::Xor => Some(typ::is_integer(ty, &self.tc_objs)),
                    BinaryOp::AndNot => Some(typ::is_integer(ty, &self.tc_objs)),
                    BinaryOp::LogAnd => Some(typ::is_boolean(ty, &self.tc_objs)),
                    BinaryOp::LogOr => Some(typ::is_boolean(ty, &self.tc_objs)),
                    _ => None,
                }
            } else {
                // For unary, we map to BinaryOp equivalents
                match o {
                    BinaryOp::Add => Some(typ::is_numeric(ty, &self.tc_objs)),
                    BinaryOp::Sub => Some(typ::is_numeric(ty, &self.tc_objs)),
                    BinaryOp::Xor => Some(typ::is_integer(ty, &self.tc_objs)),
                    _ => None,
                }
            }
        };

        if let Some(ok) = pred(op, x.typ.unwrap()) {
            if !ok {
                self.invalid_op(
                    Span::default(),
                    &format!("operator {:?} not defined for operand", op),
                );
            }
            ok
        } else {
            self.invalid_ast(Span::default(), &format!("unknown operator {:?}", op));
            false
        }
    }

    /// Checks if a unary operator is valid for the operand type.
    fn op_unary(&self, x: &mut Operand, op: UnaryOp) -> bool {
        let pred = |o: UnaryOp, ty: TypeKey| -> Option<bool> {
            match o {
                UnaryOp::Pos => Some(typ::is_numeric(ty, &self.tc_objs)),
                UnaryOp::Neg => Some(typ::is_numeric(ty, &self.tc_objs)),
                UnaryOp::BitNot => Some(typ::is_integer(ty, &self.tc_objs)),
                UnaryOp::Not => Some(typ::is_boolean(ty, &self.tc_objs)),
                _ => None,
            }
        };

        if let Some(ok) = pred(op, x.typ.unwrap()) {
            if !ok {
                self.invalid_op(
                    Span::default(),
                    &format!("operator {:?} not defined for operand", op),
                );
            }
            ok
        } else {
            false
        }
    }

    /// Type-checks a unary expression.
    /// The expression e may be None. It's passed in for better error messages only.
    fn unary(
        &mut self,
        x: &mut Operand,
        e: Option<ExprId>,
        op: UnaryOp,
        fctx: &mut FilesContext<F>,
    ) {
        match op {
            UnaryOp::Addr => {
                // spec: "As an exception to the addressability requirement
                // x may also be a composite literal."
                if let Some(expr_id) = x.expr_id {
                    if x.mode != OperandMode::Variable {
                        // Check if it's a composite literal (addressable)
                        let is_composite_lit = fctx
                            .untyped
                            .get(&expr_id)
                            .map(|info| matches!(info.expr.kind, ExprKind::CompositeLit(_)))
                            .unwrap_or(false);
                        if !is_composite_lit {
                            self.invalid_op(Span::default(), "cannot take address of expression");
                            x.mode = OperandMode::Invalid;
                            return;
                        }
                    }
                }
                x.mode = OperandMode::Value;
                x.typ = Some(self.tc_objs.new_t_pointer(x.typ.unwrap()));
            }
            UnaryOp::Deref => {
                // Dereference operation
                if let Some(ptr) = self.otype(x.typ.unwrap()).try_as_pointer() {
                    x.mode = OperandMode::Variable;
                    x.typ = Some(ptr.base());
                } else {
                    self.invalid_op(Span::default(), "cannot dereference non-pointer");
                    x.mode = OperandMode::Invalid;
                }
            }
            _ => {
                // Arithmetic/logical unary operators
                if !self.op_unary(x, op) {
                    x.mode = OperandMode::Invalid;
                    return;
                }
                if let OperandMode::Constant(v) = &mut x.mode {
                    let ty = typ::underlying_type(x.typ.unwrap(), &self.tc_objs);
                    let tval = self.otype(ty);
                    let prec = if tval.is_unsigned(&self.tc_objs) {
                        tval.try_as_basic().map(|b| b.size_of()).unwrap_or(0)
                    } else {
                        0
                    };
                    *v = Value::unary_op(op, v, prec);
                    // Typed constants must be representable in
                    // their type after each constant operation.
                    if tval.is_typed(&self.tc_objs) {
                        if e.is_some() {
                            x.expr_id = e; // for better error message
                        }
                        self.representable(x, ty, fctx);
                    }
                    return;
                }
                x.mode = OperandMode::Value;
                // x.typ remains unchanged
            }
        }
    }

    // =========================================================================
    // Part 2: Helper functions
    // =========================================================================

    /// Returns true if op is a shift operator.
    fn is_shift(op: BinaryOp) -> bool {
        matches!(op, BinaryOp::Shl | BinaryOp::Shr)
    }

    /// Returns true if op is a comparison operator.
    fn is_comparison(op: BinaryOp) -> bool {
        matches!(
            op,
            BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq
        )
    }

    /// Checks that a constant operand is representable in the given basic type.
    pub fn representable(&mut self, x: &mut Operand, t: TypeKey, _fctx: &mut FilesContext<F>) {
        let tval = self.otype(t);
        let tbasic = match tval.try_as_basic() {
            Some(b) => b,
            None => return,
        };
        if let OperandMode::Constant(v) = &mut x.mode {
            let clone = v.clone();
            if !clone.representable(tbasic, Some(v)) {
                let xtval = self.otype(x.typ.unwrap());
                let tval = self.otype(t);
                // numeric conversion : error msg
                // integer -> integer : overflows
                // integer -> float   : overflows (actually not possible)
                // float   -> integer : truncated
                // float   -> float   : overflows
                let msg = if xtval.is_numeric(&self.tc_objs) && tval.is_numeric(&self.tc_objs) {
                    if !xtval.is_integer(&self.tc_objs) && tval.is_integer(&self.tc_objs) {
                        "truncated".to_string()
                    } else {
                        "overflows".to_string()
                    }
                } else {
                    "cannot convert".to_string()
                };
                self.error(Span::default(), msg);
                x.mode = OperandMode::Invalid;
            }
        }
    }

    // =========================================================================
    // Part 3: update_expr_type
    // =========================================================================

    /// Updates the type of x to typ and invokes itself recursively for the
    /// operands of x, depending on expression kind.
    /// If typ is still untyped and not the final type, update_expr_type only
    /// updates the recorded untyped type for x and possibly its operands.
    /// Otherwise (typ is not untyped anymore, or it is the final type for x),
    /// the type and value are recorded.
    pub fn update_expr_type(
        &mut self,
        expr_id: ExprId,
        t: TypeKey,
        final_: bool,
        fctx: &mut FilesContext<F>,
    ) {
        let info = match fctx.untyped.get(&expr_id) {
            Some(i) => i.clone(),
            None => return, // nothing to do
        };

        // Update operands of e if necessary
        match &info.expr.kind {
            ExprKind::FuncLit(_)
            | ExprKind::CompositeLit(_)
            | ExprKind::Index(_)
            | ExprKind::Slice(_)
            | ExprKind::TypeAssert(_) => {
                // These should not be untyped
                return;
            }
            ExprKind::Call(_) => {
                // Resulting in an untyped constant (e.g., built-in complex).
                // The respective calls take care of calling update_expr_type
                // for the arguments if necessary.
            }
            ExprKind::Ident(_)
            | ExprKind::IntLit(_)
            | ExprKind::FloatLit(_)
            | ExprKind::RuneLit(_)
            | ExprKind::StringLit(_)
            | ExprKind::Selector(_) => {
                // An identifier denoting a constant, a constant literal,
                // or a qualified identifier. No operands to take care of.
            }
            ExprKind::Paren(inner) => {
                self.update_expr_type(inner.id, t, final_, fctx);
            }
            ExprKind::Unary(u) => {
                // If x is a constant, the operands were constants.
                if info.mode.constant_val().is_none() {
                    self.update_expr_type(u.operand.id, t, final_, fctx);
                }
            }
            ExprKind::Binary(b) => {
                if info.mode.constant_val().is_none() {
                    if Self::is_comparison(b.op) {
                        // The result type is independent of operand types
                    } else if Self::is_shift(b.op) {
                        // The result type depends only on lhs operand.
                        self.update_expr_type(b.left.id, t, final_, fctx);
                    } else {
                        // The operand types match the result type.
                        self.update_expr_type(b.left.id, t, final_, fctx);
                        self.update_expr_type(b.right.id, t, final_, fctx);
                    }
                }
            }
            _ => {}
        }

        // If the new type is not final and still untyped, just update the recorded type.
        if !final_ && typ::is_untyped(t, &self.tc_objs) {
            if let Some(info) = fctx.untyped.get_mut(&expr_id) {
                info.typ = Some(typ::underlying_type(t, &self.tc_objs));
            }
            return;
        }

        // Otherwise we have the final (typed or untyped type).
        // Remove it from the map of yet untyped expressions.
        let removed = fctx.untyped.remove(&expr_id);
        let info = match removed {
            Some(o) => o,
            None => return,
        };

        if info.is_lhs {
            // If x is the lhs of a shift, its final type must be integer.
            if !typ::is_integer(t, &self.tc_objs) {
                self.invalid_op(Span::default(), "shifted operand must be integer");
                return;
            }
        }
        if info.mode.constant_val().is_some() {
            // If x is a constant, it must be representable as a value of typ.
            let mut c = Operand::with_expr(info.mode.clone(), expr_id, info.typ);
            self.convert_untyped(&mut c, t);
            if c.invalid() {
                return;
            }
        }

        // Everything's fine, record final type and value for x.
        self.result.record_type_and_value(expr_id, info.mode.clone(), t);
    }

    /// Updates the value of x to val in the untyped map.
    fn update_expr_val(expr_id: ExprId, val: Value, fctx: &mut FilesContext<F>) {
        if let Some(info) = fctx.untyped.get_mut(&expr_id) {
            if let OperandMode::Constant(v) = &mut info.mode {
                *v = val;
            }
        }
    }

    /// Attempts to set the type of an untyped value to the target type.
    /// This is the version that takes fctx for updating expression types.
    pub fn convert_untyped_fctx(
        &mut self,
        x: &mut Operand,
        target: TypeKey,
        fctx: &mut FilesContext<F>,
    ) {
        if x.invalid() || typ::is_typed(x.typ.unwrap(), &self.tc_objs) || target == self.invalid_type() {
            return;
        }

        if typ::is_untyped(target, &self.tc_objs) {
            // Both x and target are untyped
            let order = |bt: BasicType| -> usize {
                match bt {
                    BasicType::UntypedInt => 1,
                    BasicType::UntypedRune => 2,
                    BasicType::UntypedFloat => 3,
                    _ => 0,
                }
            };
            let xtval = self.otype(x.typ.unwrap());
            let ttval = self.otype(target);
            if let (Some(xbasic), Some(tbasic)) = (xtval.try_as_basic(), ttval.try_as_basic()) {
                let xbt = xbasic.typ();
                let tbt = tbasic.typ();
                if xbt != tbt {
                    if xtval.is_numeric(&self.tc_objs) && ttval.is_numeric(&self.tc_objs) {
                        if order(xbt) < order(tbt) {
                            x.typ = Some(target);
                            if let Some(expr_id) = x.expr_id {
                                self.update_expr_type(expr_id, target, false, fctx);
                            }
                        }
                    } else {
                        self.error(Span::default(), "cannot convert untyped value".to_string());
                        x.mode = OperandMode::Invalid;
                    }
                }
            }
            return;
        }

        let t = typ::underlying_type(target, &self.tc_objs);
        let xtype = x.typ.unwrap();
        let tval = self.otype(t);
        let final_target = match tval {
            Type::Basic(_) => {
                if let OperandMode::Constant(v) = &x.mode {
                    let v_clone = v.clone();
                    self.representable(x, t, fctx);
                    if x.invalid() {
                        return;
                    }
                    // Expression value may have been rounded - update if needed
                    if let Some(expr_id) = x.expr_id {
                        Self::update_expr_val(expr_id, v_clone, fctx);
                    }
                    Some(target)
                } else {
                    // Non-constant untyped values
                    let ok = if let Some(xbasic) = self.otype(x.typ.unwrap()).try_as_basic() {
                        match xbasic.typ() {
                            BasicType::UntypedBool => tval.is_boolean(&self.tc_objs),
                            BasicType::UntypedInt
                            | BasicType::UntypedRune
                            | BasicType::UntypedFloat => tval.is_numeric(&self.tc_objs),
                            BasicType::UntypedNil => typ::has_nil(t, &self.tc_objs),
                            _ => false,
                        }
                    } else {
                        false
                    };
                    if ok { Some(target) } else { None }
                }
            }
            Type::Interface(detail) => {
                if x.is_nil(&self.tc_objs) {
                    Some(self.basic_type(BasicType::UntypedNil))
                } else if detail.is_empty() {
                    Some(typ::untyped_default_type(xtype, &self.tc_objs))
                } else {
                    None
                }
            }
            Type::Pointer(_) | Type::Signature(_) | Type::Slice(_) | Type::Map(_) | Type::Chan(_) => {
                if x.is_nil(&self.tc_objs) {
                    Some(self.basic_type(BasicType::UntypedNil))
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(t) = final_target {
            x.typ = final_target;
            if let Some(expr_id) = x.expr_id {
                self.update_expr_type(expr_id, t, true, fctx);
            }
        } else {
            self.error(Span::default(), "cannot convert untyped value".to_string());
            x.mode = OperandMode::Invalid;
        }
    }

    // =========================================================================
    // Part 5: comparison
    // =========================================================================

    /// Type-checks a comparison operation.
    pub fn comparison(
        &mut self,
        x: &mut Operand,
        y: &Operand,
        op: BinaryOp,
        fctx: &mut FilesContext<F>,
    ) {
        // spec: "In any comparison, the first operand must be assignable
        // to the type of the second operand, or vice versa."
        let (xtype, ytype) = (x.typ.unwrap(), y.typ.unwrap());
        let mut reason = String::new();
        let assignable = self.assignable_to(x, ytype, &mut reason)
            || self.assignable_to(y, xtype, &mut reason);

        let emsg = if assignable {
            let (xtval, ytval) = (self.otype(xtype), self.otype(ytype));
            let defined = match op {
                BinaryOp::Eq | BinaryOp::NotEq => {
                    (xtval.comparable(&self.tc_objs) && ytval.comparable(&self.tc_objs))
                        || (x.is_nil(&self.tc_objs) && typ::has_nil(ytype, &self.tc_objs))
                        || (y.is_nil(&self.tc_objs) && typ::has_nil(xtype, &self.tc_objs))
                }
                BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => {
                    xtval.is_ordered(&self.tc_objs) && ytval.is_ordered(&self.tc_objs)
                }
                _ => false,
            };
            if !defined {
                Some(format!("operator {:?} not defined", op))
            } else {
                None
            }
        } else {
            Some("mismatched types".to_string())
        };

        if let Some(m) = emsg {
            self.error(Span::default(), format!("cannot compare: {}", m));
            x.mode = OperandMode::Invalid;
            return;
        }

        match (&mut x.mode, &y.mode) {
            (OperandMode::Constant(vx), OperandMode::Constant(vy)) => {
                *vx = Value::with_bool(Value::compare(vx, op, vy));
            }
            _ => {
                x.mode = OperandMode::Value;
                // Update operand types to their default types
                if let Some(expr_id) = x.expr_id {
                    let default_t = typ::untyped_default_type(xtype, &self.tc_objs);
                    self.update_expr_type(expr_id, default_t, true, fctx);
                }
                if let Some(expr_id) = y.expr_id {
                    let default_t = typ::untyped_default_type(ytype, &self.tc_objs);
                    self.update_expr_type(expr_id, default_t, true, fctx);
                }
            }
        }
        // spec: "Comparison operators compare two operands and yield
        //        an untyped boolean value."
        x.typ = Some(self.basic_type(BasicType::UntypedBool));
    }

    // =========================================================================
    // Part 6: shift
    // =========================================================================

    /// Type-checks a shift operation.
    fn shift(
        &mut self,
        x: &mut Operand,
        y: &mut Operand,
        op: BinaryOp,
        e: Option<ExprId>,
        fctx: &mut FilesContext<F>,
    ) {
        let xtval = self.otype(x.typ.unwrap());
        let xt_untyped = xtval.is_untyped(&self.tc_objs);
        let xt_integer = xtval.is_integer(&self.tc_objs);
        let x_const = x.mode.constant_val().map(|v| v.to_int().into_owned());

        // The lhs is of integer type or an untyped constant representable as an integer
        let lhs_ok = xt_integer || (xt_untyped && x_const.is_some() && x_const.as_ref().unwrap().is_int());
        if !lhs_ok {
            self.invalid_op(Span::default(), "shifted operand must be integer");
            x.mode = OperandMode::Invalid;
            return;
        }

        // spec: "The right operand in a shift expression must have unsigned
        // integer type or be an untyped constant representable by a value of type uint."
        let ytval = self.otype(y.typ.unwrap());
        if ytval.is_unsigned(&self.tc_objs) {
            // ok
        } else if ytval.is_untyped(&self.tc_objs) {
            self.convert_untyped(y, self.basic_type(BasicType::Uint));
            if y.invalid() {
                x.mode = OperandMode::Invalid;
                return;
            }
        } else {
            self.error(Span::default(), "shift count must be unsigned integer".to_string());
            x.mode = OperandMode::Invalid;
            return;
        }

        if let OperandMode::Constant(xv) = &mut x.mode {
            if let OperandMode::Constant(yv) = &y.mode {
                // rhs must be an integer value
                let yval = yv.to_int();
                if !yval.is_int() {
                    self.invalid_op(Span::default(), "shift count must be unsigned integer");
                    x.mode = OperandMode::Invalid;
                    return;
                }
                // rhs must be within reasonable bounds
                let shift_bound: u64 = 1023 - 1 + 52; // so we can express smallestFloat64
                let (s, ok) = yval.int_as_u64();
                if !ok || s > shift_bound {
                    self.invalid_op(Span::default(), "invalid shift count");
                    x.mode = OperandMode::Invalid;
                    return;
                }
                // The lhs is representable as an integer but may not be an integer
                if !xt_integer {
                    x.typ = Some(self.basic_type(BasicType::UntypedInt));
                }
                // x is a constant so xval != nil and it must be of Int kind.
                *xv = Value::shift(&xv.to_int(), op, s);
                // Typed constants must be representable in their type
                if typ::is_typed(x.typ.unwrap(), &self.tc_objs) {
                    if e.is_some() {
                        x.expr_id = e;
                    }
                    self.representable(x, x.typ.unwrap(), fctx);
                }
                return;
            }

            if xt_untyped {
                // Delay operand checking until we know the final type
                if let Some(expr_id) = x.expr_id {
                    if let Some(info) = fctx.untyped.get_mut(&expr_id) {
                        info.is_lhs = true;
                    }
                }
                x.mode = OperandMode::Value;
                return;
            }
        }

        // Constant rhs must be >= 0
        if let OperandMode::Constant(v) = &y.mode {
            if v.sign() < 0 {
                self.invalid_op(Span::default(), "shift count must not be negative");
            }
        }

        if !typ::is_integer(x.typ.unwrap(), &self.tc_objs) {
            self.invalid_op(Span::default(), "shifted operand must be integer");
            x.mode = OperandMode::Invalid;
            return;
        }
        x.mode = OperandMode::Value;
    }

    // =========================================================================
    // Part 7: binary
    // =========================================================================

    /// Type-checks a binary expression.
    /// The expression e may be None. It's passed in for better error messages only.
    pub fn binary(
        &mut self,
        x: &mut Operand,
        e: Option<ExprId>,
        lhs: &Expr,
        rhs: &Expr,
        op: BinaryOp,
        fctx: &mut FilesContext<F>,
    ) {
        let mut y = Operand::new();
        self.expr(x, lhs, fctx);
        self.expr(&mut y, rhs, fctx);

        if x.invalid() {
            return;
        }
        if y.invalid() {
            x.mode = OperandMode::Invalid;
            x.expr_id = y.expr_id;
            return;
        }

        if Self::is_shift(op) {
            self.shift(x, &mut y, op, e, fctx);
            return;
        }

        self.convert_untyped(x, y.typ.unwrap());
        if x.invalid() {
            return;
        }

        self.convert_untyped(&mut y, x.typ.unwrap());
        if y.invalid() {
            x.mode = OperandMode::Invalid;
            return;
        }

        if Self::is_comparison(op) {
            self.comparison(x, &y, op, fctx);
            return;
        }

        if !typ::identical_o(x.typ, y.typ, &self.tc_objs) {
            let invalid = Some(self.invalid_type());
            if x.typ != invalid && y.typ != invalid {
                self.invalid_op(Span::default(), "mismatched types");
            }
            x.mode = OperandMode::Invalid;
            return;
        }

        if !self.op_token(x, op, true) {
            x.mode = OperandMode::Invalid;
            return;
        }

        // Check for zero divisor
        if op == BinaryOp::Div || op == BinaryOp::Rem {
            if x.mode.constant_val().is_some() || typ::is_integer(x.typ.unwrap(), &self.tc_objs) {
                if let Some(v) = y.mode.constant_val() {
                    if v.sign() == 0 {
                        self.invalid_op(Span::default(), "division by zero");
                        x.mode = OperandMode::Invalid;
                        return;
                    }
                }
            }
        }

        match (&mut x.mode, &y.mode) {
            (OperandMode::Constant(vx), OperandMode::Constant(vy)) => {
                let ty = typ::underlying_type(x.typ.unwrap(), &self.tc_objs);
                *vx = Value::binary_op(vx, op, vy);
                // Typed constants must be representable in their type
                if typ::is_typed(ty, &self.tc_objs) {
                    x.expr_id = e;
                    self.representable(x, ty, fctx);
                }
            }
            _ => {
                x.mode = OperandMode::Value;
            }
        }
    }

    // =========================================================================
    // Part 8: index, indexed_elems
    // =========================================================================

    /// Checks an index expression for validity.
    /// max is the upper bound for index.
    /// Returns the value of the index when it's a constant, returns None if it's not.
    pub fn index(
        &mut self,
        index: &Expr,
        max: Option<u64>,
        fctx: &mut FilesContext<F>,
    ) -> Result<Option<u64>, ()> {
        let x = &mut Operand::new();
        self.expr(x, index, fctx);
        if x.invalid() {
            return Err(());
        }

        // An untyped constant must be representable as Int
        self.convert_untyped(x, self.basic_type(BasicType::Int));
        if x.invalid() {
            return Err(());
        }

        // The index must be of integer type
        if !typ::is_integer(x.typ.unwrap(), &self.tc_objs) {
            self.invalid_arg(Span::default(), "index must be integer");
            return Err(());
        }

        // A constant index i must be in bounds
        if let OperandMode::Constant(v) = &x.mode {
            if v.sign() < 0 {
                self.invalid_arg(Span::default(), "index must not be negative");
                return Err(());
            }
            let (i, valid) = v.to_int().int_as_u64();
            if !valid || max.map_or(false, |m| i >= m) {
                self.invalid_arg(Span::default(), "index out of bounds");
                return Err(());
            }
            return Ok(Some(i));
        }

        Ok(None)
    }

    /// Checks the elements of an array or slice composite literal against the
    /// literal's element type, and the element indices against the literal length
    /// if known. It returns the length of the literal (maximum index value + 1).
    fn indexed_elems(
        &mut self,
        elems: &[gox_syntax::ast::CompositeLitElem],
        t: TypeKey,
        length: Option<u64>,
        fctx: &mut FilesContext<F>,
    ) -> u64 {
        use std::collections::HashSet;
        use gox_syntax::ast::CompositeLitKey;
        let mut visited: HashSet<u64> = HashSet::new();
        let mut index: u64 = 0;
        let mut max: u64 = 0;

        for elem in elems {
            let (valid_index, eval) = if let Some(ref key) = elem.key {
                let kv_index = match key {
                    CompositeLitKey::Expr(key_expr) => {
                        let i = self.index(key_expr, length, fctx);
                        if let Ok(Some(idx)) = i {
                            Some(idx)
                        } else if i.is_ok() {
                            self.error(Span::default(), "index must be integer constant".to_string());
                            None
                        } else {
                            None
                        }
                    }
                    CompositeLitKey::Ident(_) => {
                        // Field name - not an index
                        None
                    }
                };
                (kv_index, &elem.value)
            } else if length.is_some() && index >= length.unwrap() {
                self.error(
                    Span::default(),
                    format!("index {} is out of bounds (>= {})", index, length.unwrap()),
                );
                (None, &elem.value)
            } else {
                (Some(index), &elem.value)
            };

            if let Some(i) = valid_index {
                if visited.contains(&i) {
                    self.error(
                        Span::default(),
                        format!("duplicate index {} in array or slice literal", i),
                    );
                }
                visited.insert(i);

                index = i + 1;
                if index > max {
                    max = index;
                }
            }

            // Check element against composite literal element type
            let x = &mut Operand::new();
            self.raw_expr(x, eval, Some(t), fctx);
            self.assignment(x, Some(t), "array or slice literal", fctx);
        }
        max
    }

    /// Type-checks an expression with a type hint (for composite literal elements).
    pub fn expr_with_hint(
        &mut self,
        x: &mut Operand,
        e: &Expr,
        hint: Option<TypeKey>,
        fctx: &mut FilesContext<F>,
    ) {
        self.raw_expr(x, e, hint, fctx);
    }

    // =========================================================================
    // Part 9: raw_expr, raw_internal
    // =========================================================================

    /// Raw expression type-checking entry point.
    /// Typechecks expression e and initializes x with the expression value or type.
    /// If an error occurred, x.mode is set to invalid.
    /// If hint is Some, it is the type of a composite literal element.
    pub fn raw_expr_impl(
        &mut self,
        x: &mut Operand,
        e: &Expr,
        hint: Option<TypeKey>,
        fctx: &mut FilesContext<F>,
    ) {
        self.raw_internal(x, e, hint, fctx);

        let ty = match &x.mode {
            OperandMode::Invalid => self.invalid_type(),
            OperandMode::NoValue => self.universe().no_value_tuple(),
            _ => x.typ.unwrap_or(self.invalid_type()),
        };

        if typ::is_untyped(ty, &self.tc_objs) {
            // Delay type and value recording until we know the type
            fctx.remember_untyped(
                e.id,
                super::checker::ExprInfo {
                    is_lhs: false,
                    mode: x.mode.clone(),
                    typ: Some(ty),
                    expr: e.clone(),
                },
            );
        } else {
            self.result.record_type_and_value(e.id, x.mode.clone(), ty);
        }
    }

    /// Core expression type-checking. Must only be called by raw_expr_impl.
    fn raw_internal(
        &mut self,
        x: &mut Operand,
        e: &Expr,
        hint: Option<TypeKey>,
        fctx: &mut FilesContext<F>,
    ) {
        // Make sure x has a valid state in case of bailout
        x.mode = OperandMode::Invalid;
        x.typ = Some(self.invalid_type());
        x.expr_id = Some(e.id);

        match &e.kind {
            ExprKind::Ident(ident) => {
                self.ident(x, ident, None, false, fctx);
            }
            ExprKind::IntLit(_lit) => {
                // TODO: Parse actual value from Symbol using interner
                x.mode = OperandMode::Constant(Value::with_i64(0));
                x.typ = Some(self.basic_type(BasicType::UntypedInt));
            }
            ExprKind::FloatLit(_lit) => {
                // TODO: Parse actual value from Symbol using interner
                x.mode = OperandMode::Constant(Value::with_f64(0.0));
                x.typ = Some(self.basic_type(BasicType::UntypedFloat));
            }
            ExprKind::RuneLit(lit) => {
                // Rune literals have a parsed value
                let val = lit.value as i64;
                x.mode = OperandMode::Constant(Value::with_i64(val));
                x.typ = Some(self.basic_type(BasicType::UntypedRune));
            }
            ExprKind::StringLit(lit) => {
                x.mode = OperandMode::Constant(Value::with_str(lit.value.clone()));
                x.typ = Some(self.basic_type(BasicType::UntypedString));
            }
            ExprKind::Paren(inner) => {
                self.raw_internal(x, inner, hint, fctx);
            }
            ExprKind::Unary(u) => {
                self.expr(x, &u.operand, fctx);
                if x.invalid() {
                    return;
                }
                self.unary(x, Some(e.id), u.op, fctx);
            }
            ExprKind::Binary(b) => {
                self.binary(x, Some(e.id), &b.left, &b.right, b.op, fctx);
            }
            ExprKind::Call(call) => {
                self.call(x, call, e.span, fctx);
            }
            ExprKind::Index(idx) => {
                self.expr(x, &idx.expr, fctx);
                if x.invalid() {
                    return;
                }
                // TODO: Full index expression handling
                let base_type = x.typ.unwrap();
                let utype = typ::underlying_type(base_type, &self.tc_objs);
                let elem_type = match self.otype(utype) {
                    Type::Array(arr) => Some(arr.elem()),
                    Type::Slice(sl) => Some(sl.elem()),
                    Type::Map(m) => {
                        x.mode = OperandMode::MapIndex;
                        Some(m.elem())
                    }
                    Type::Basic(b) if typ::is_string(utype, &self.tc_objs) => {
                        x.mode = OperandMode::Value;
                        Some(self.basic_type(BasicType::Byte))
                    }
                    _ => {
                        self.invalid_op(Span::default(), "cannot index expression");
                        None
                    }
                };
                if let Some(et) = elem_type {
                    x.typ = Some(et);
                    if x.mode != OperandMode::MapIndex {
                        x.mode = OperandMode::Variable;
                    }
                }
            }
            ExprKind::Slice(sl) => {
                self.expr(x, &sl.expr, fctx);
                if x.invalid() {
                    return;
                }
                // TODO: Full slice expression handling
                x.mode = OperandMode::Value;
            }
            ExprKind::Selector(sel) => {
                self.selector(x, sel, fctx);
            }
            ExprKind::TypeAssert(ta) => {
                self.expr(x, &ta.expr, fctx);
                if x.invalid() {
                    return;
                }
                // TODO: Full type assertion handling
                if let Some(ref ty) = ta.ty {
                    let target = self.type_expr(ty, fctx);
                    x.typ = Some(target);
                    x.mode = OperandMode::CommaOk;
                }
            }
            ExprKind::CompositeLit(lit) => {
                // TODO: Full composite literal handling
                let ty = self.type_expr(&lit.ty, fctx);
                x.mode = OperandMode::Value;
                x.typ = Some(ty);
            }
            ExprKind::FuncLit(func) => {
                // TODO: Full function literal handling
                x.mode = OperandMode::Value;
                x.typ = Some(self.invalid_type());
            }
            ExprKind::Conversion(conv) => {
                let ty = self.type_expr(&conv.ty, fctx);
                self.expr(x, &conv.expr, fctx);
                if x.invalid() {
                    return;
                }
                // TODO: conversion needs fctx but current signature doesn't take it
                self.convert_untyped(x, ty);
            }
            ExprKind::Receive(recv) => {
                self.expr(x, recv, fctx);
                if x.invalid() {
                    return;
                }
                if let Some(chan) = self.otype(x.typ.unwrap()).try_as_chan() {
                    x.mode = OperandMode::CommaOk;
                    x.typ = Some(chan.elem());
                    self.octx.has_call_or_recv = true;
                } else {
                    self.invalid_op(Span::default(), "cannot receive from non-channel");
                    x.mode = OperandMode::Invalid;
                }
            }
            ExprKind::TypeAsExpr(ty) => {
                let t = self.type_expr(ty, fctx);
                x.mode = OperandMode::TypeExpr;
                x.typ = Some(t);
            }
            ExprKind::TryUnwrap(inner) => {
                // GoX extension: ? operator for error propagation
                // The inner expression must return a value where the last element is of type error.
                // If error != nil, the function returns early with that error.
                // If error == nil, the result is the value(s) without the error part.
                self.expr(x, inner, fctx);
                if x.invalid() {
                    return;
                }

                let error_type = self.universe().error_type();
                let inner_type = x.typ.unwrap_or(self.invalid_type());

                // Check if the type is a tuple with error as the last element
                if let Some(tuple) = self.otype(inner_type).try_as_tuple() {
                    let vars = tuple.vars();
                    if vars.is_empty() {
                        self.error(e.span, "? operator requires expression returning error".to_string());
                        x.mode = OperandMode::Invalid;
                        return;
                    }

                    // Check that the last element is error type
                    let last_var = vars.last().unwrap();
                    let last_type = self.lobj(*last_var).typ().unwrap_or(self.invalid_type());
                    if !typ::identical(last_type, error_type, &self.tc_objs) {
                        self.error(e.span, "? operator requires expression with error as last return value".to_string());
                        x.mode = OperandMode::Invalid;
                        return;
                    }

                    // Result type is the tuple without the last error element
                    match vars.len() {
                        1 => {
                            // Just error -> NoValue after unwrap
                            x.mode = OperandMode::NoValue;
                            x.typ = None;
                        }
                        2 => {
                            // (T, error) -> T
                            x.mode = OperandMode::Value;
                            x.typ = self.lobj(vars[0]).typ();
                        }
                        _ => {
                            // (T1, T2, ..., error) -> (T1, T2, ...)
                            let new_vars: Vec<_> = vars[..vars.len() - 1].to_vec();
                            let new_tuple = self.tc_objs.new_t_tuple(new_vars);
                            x.mode = OperandMode::Value;
                            x.typ = Some(new_tuple);
                        }
                    }
                } else if typ::identical(inner_type, error_type, &self.tc_objs) {
                    // Single error type -> NoValue after unwrap
                    x.mode = OperandMode::NoValue;
                    x.typ = None;
                } else {
                    self.error(e.span, "? operator requires expression returning error type".to_string());
                    x.mode = OperandMode::Invalid;
                }
            }
        }
    }

    // =========================================================================
    // Part 10: Entry functions
    // =========================================================================

    /// Checks that x.(T) is legal; xtype must be the type of x.
    pub fn type_assertion(
        &mut self,
        x: &mut Operand,
        xtype: TypeKey,
        t: TypeKey,
        _fctx: &mut FilesContext<F>,
    ) {
        // Check that xtype is an interface type
        if self.otype(xtype).try_as_interface().is_none() {
            self.error(Span::default(), "type assertion requires interface type".to_string());
            x.mode = OperandMode::Invalid;
            return;
        }
        // TODO: Full type assertion check using lookup::assertable_to
        x.typ = Some(t);
        x.mode = OperandMode::CommaOk;
    }

    /// Reports an error if x is not a proper expression value.
    fn expr_value_err(&self, x: &mut Operand) {
        let msg = match &x.mode {
            OperandMode::NoValue => Some("used as value"),
            OperandMode::Builtin(_) => Some("must be called"),
            OperandMode::TypeExpr => Some("is not an expression"),
            _ => None,
        };
        if let Some(m) = msg {
            self.error(Span::default(), format!("expression {}", m));
            // Don't set x.mode to Invalid here - caller handles it
        }
    }

    /// Checks that x is a single value (not a tuple).
    pub fn single_value(&self, x: &mut Operand) {
        if x.mode == OperandMode::Value {
            if let Some(tuple) = self.otype(x.typ.unwrap()).try_as_tuple() {
                let len = tuple.vars().len();
                if len != 1 {
                    self.error(
                        Span::default(),
                        format!("{}-valued expression where single value is expected", len),
                    );
                    // Don't set x.mode to Invalid here
                }
            }
        }
    }

    /// Typechecks expression e and initializes x with the expression value.
    /// The result must be a single value.
    pub fn expr(&mut self, x: &mut Operand, e: &Expr, fctx: &mut FilesContext<F>) {
        self.multi_expr(x, e, fctx);
        self.single_value(x);
    }

    /// Like expr but the result may be a multi-value.
    pub fn multi_expr(&mut self, x: &mut Operand, e: &Expr, fctx: &mut FilesContext<F>) {
        self.raw_expr(x, e, None, fctx);
        self.expr_value_err(x);
    }

    /// Simple expression check - for use in stmt.rs where fctx is not available.
    /// Returns the type of the expression.
    pub fn check_expr(&mut self, e: &Expr) -> TypeKey {
        // Simplified check - just record the expression type
        // Full checking is done when fctx is available
        self.invalid_type()
    }

    /// Typechecks expression or type e and initializes x with the expression
    /// value or type. If an error occurred, x.mode is set to invalid.
    pub fn expr_or_type(&mut self, x: &mut Operand, e: &Expr, fctx: &mut FilesContext<F>) {
        self.raw_expr(x, e, None, fctx);
        self.single_value(x);
        if x.mode == OperandMode::NoValue {
            self.error(Span::default(), "expression used as value or type".to_string());
            x.mode = OperandMode::Invalid;
        }
    }

    /// Type-checks a selector expression (e.g., x.f).
    pub fn selector(
        &mut self,
        x: &mut Operand,
        sel: &gox_syntax::ast::SelectorExpr,
        fctx: &mut FilesContext<F>,
    ) {
        // If the identifier refers to a package, handle everything here
        // so we don't need a "package" mode for operands: package names
        // can only appear in qualified identifiers which are mapped to
        // selector expressions.
        if let ExprKind::Ident(ident) = &sel.expr.kind {
            let name = self.resolve_ident(ident);
            if let Some(okey) = self.lookup(name) {
                let lobj = self.lobj(okey);
                let lobj_pkg = lobj.pkg();
                if let crate::obj::EntityType::PkgName { imported, .. } = lobj.entity_type() {
                    let imported = *imported;
                    debug_assert_eq!(self.pkg, lobj_pkg.unwrap());
                    self.result.record_use(ident.clone(), okey);
                    // Mark package as used
                    if let crate::obj::EntityType::PkgName { used, .. } = self.lobj_mut(okey).entity_type_mut() {
                        *used = true;
                    }

                    let pkg = self.package(imported);
                    let pkg_scope = *pkg.scope();
                    let pkg_name = pkg.name().clone();
                    let pkg_fake = pkg.fake();

                    let sel_name = self.resolve_ident(&sel.sel);
                    let exp_op = self.scope(pkg_scope).lookup(sel_name);

                    if exp_op.is_none() {
                        if !pkg_fake {
                            let msg = format!(
                                "{} not declared by package {}",
                                sel_name,
                                pkg_name.as_ref().unwrap_or(&"<unknown>".to_string())
                            );
                            self.error(sel.sel.span, msg);
                        }
                        x.mode = OperandMode::Invalid;
                        x.expr_id = Some(sel.expr.id);
                        return;
                    }

                    let exp_key = exp_op.unwrap();
                    let exp = self.lobj(exp_key);
                    if !exp.exported() {
                        let msg = format!(
                            "{} not exported by package {}",
                            sel_name,
                            pkg_name.as_ref().unwrap_or(&"<unknown>".to_string())
                        );
                        self.error(sel.sel.span, msg);
                    }
                    self.result.record_use(sel.sel.clone(), exp_key);

                    // Simplified version of the code for Idents:
                    // - imported objects are always fully initialized
                    let exp = self.lobj(exp_key);
                    x.mode = match exp.entity_type() {
                        crate::obj::EntityType::Const { val } => OperandMode::Constant(val.clone()),
                        crate::obj::EntityType::TypeName => OperandMode::TypeExpr,
                        crate::obj::EntityType::Var(_) => OperandMode::Variable,
                        crate::obj::EntityType::Func { .. } => OperandMode::Value,
                        crate::obj::EntityType::Builtin(id) => OperandMode::Builtin(*id),
                        _ => unreachable!(),
                    };
                    x.typ = exp.typ();
                    x.expr_id = Some(sel.expr.id);
                    return;
                }
            }
        }

        self.expr_or_type(x, &sel.expr, fctx);
        if x.invalid() {
            x.expr_id = Some(sel.expr.id);
            return;
        }

        let sel_name = self.resolve_ident(&sel.sel).to_string();
        let result = crate::lookup::lookup_field_or_method(
            x.typ.unwrap(),
            x.mode == OperandMode::Variable,
            Some(self.pkg),
            &sel_name,
            &self.tc_objs,
        );

        let (okey, indices, indirect) = match result {
            crate::lookup::LookupResult::Entry(okey, indices, indirect) => (okey, indices, indirect),
            _ => {
                let msg = match &result {
                    crate::lookup::LookupResult::Ambiguous(_) => format!("ambiguous selector {}", sel_name),
                    crate::lookup::LookupResult::NotFound => {
                        format!(
                            "{} undefined (type has no field or method {})",
                            sel_name, sel_name
                        )
                    }
                    crate::lookup::LookupResult::BadMethodReceiver => {
                        format!("{} is not in method set of type", sel_name)
                    }
                    crate::lookup::LookupResult::Entry(_, _, _) => unreachable!(),
                };
                self.error(sel.sel.span, msg);
                x.mode = OperandMode::Invalid;
                x.expr_id = Some(sel.expr.id);
                return;
            }
        };

        // methods may not have a fully set up signature yet
        if self.lobj(okey).entity_type().is_func() {
            self.obj_decl(okey, None, fctx);
        }

        if x.mode == OperandMode::TypeExpr {
            // method expression
            if self.lobj(okey).entity_type().is_func() {
                let selection = crate::selection::Selection::new(
                    crate::selection::SelectionKind::MethodExpr,
                    x.typ,
                    okey,
                    indices,
                    indirect,
                    &self.tc_objs,
                );
                self.result.record_selection(sel.expr.id, selection);

                // the receiver type becomes the type of the first function
                // argument of the method expression's function type
                let var = self
                    .tc_objs
                    .new_var(0, Some(self.pkg), "".to_string(), x.typ);
                let lobj = self.lobj(okey);
                let sig = self.otype(lobj.typ().unwrap()).try_as_signature().unwrap();
                let (p, r, v) = (sig.params(), sig.results(), sig.variadic());
                let params_val = self.otype(p).try_as_tuple().unwrap();
                let mut vars = vec![var];
                vars.append(&mut params_val.vars().clone());
                let params = self.tc_objs.new_t_tuple(vars);
                let new_sig = self.tc_objs.new_t_signature(None, None, params, r, v);
                x.mode = OperandMode::Value;
                x.typ = Some(new_sig);

                self.add_decl_dep(okey);
            } else {
                let msg = format!(
                    "{} undefined (type has no method {})",
                    sel_name, sel_name
                );
                self.error(sel.sel.span, msg);
                x.mode = OperandMode::Invalid;
                x.expr_id = Some(sel.expr.id);
                return;
            }
        } else {
            // regular selector
            let is_var = self.lobj(okey).entity_type().is_var();
            let is_func = self.lobj(okey).entity_type().is_func();
            let field_typ = self.lobj(okey).typ();
            if is_var {
                let selection = crate::selection::Selection::new(
                    crate::selection::SelectionKind::FieldVal,
                    x.typ,
                    okey,
                    indices.clone(),
                    indirect,
                    &self.tc_objs,
                );
                self.result.record_selection(sel.expr.id, selection);
                x.mode = if x.mode == OperandMode::Variable || indirect {
                    OperandMode::Variable
                } else {
                    OperandMode::Value
                };
                x.typ = field_typ;
            } else if is_func {
                let selection = crate::selection::Selection::new(
                    crate::selection::SelectionKind::MethodVal,
                    x.typ,
                    okey,
                    indices.clone(),
                    indirect,
                    &self.tc_objs,
                );
                self.result.record_selection(sel.expr.id, selection);

                x.mode = OperandMode::Value;

                // remove receiver
                let lobj = self.lobj(okey);
                let sig = self.otype(lobj.typ().unwrap()).try_as_signature().unwrap();
                let (p, r, v) = (sig.params(), sig.results(), sig.variadic());
                let new_sig = self.tc_objs.new_t_signature(None, None, p, r, v);
                x.typ = Some(new_sig);

                self.add_decl_dep(okey);
            } else {
                unreachable!();
            }
        }
        x.expr_id = Some(sel.expr.id);
    }
}
