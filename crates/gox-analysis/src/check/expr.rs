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


use gox_common::span::Span;
use gox_common_core::ExprId;
use gox_syntax::ast::{BinaryOp, CompositeLitKey, Expr, ExprKind, UnaryOp};

use crate::constant::Value;
use crate::objects::TypeKey;
use crate::operand::{Operand, OperandMode};
use crate::typ::{self, BasicType, Type};

use super::checker::Checker;
use super::errors::TypeError;

impl Checker {
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
                        Some(typ::is_numeric(ty, self.objs()) || typ::is_string(ty, self.objs()))
                    }
                    BinaryOp::Sub => Some(typ::is_numeric(ty, self.objs())),
                    BinaryOp::Mul => Some(typ::is_numeric(ty, self.objs())),
                    BinaryOp::Div => Some(typ::is_numeric(ty, self.objs())),
                    BinaryOp::Rem => Some(typ::is_integer(ty, self.objs())),
                    BinaryOp::And => Some(typ::is_integer(ty, self.objs())),
                    BinaryOp::Or => Some(typ::is_integer(ty, self.objs())),
                    BinaryOp::Xor => Some(typ::is_integer(ty, self.objs())),
                    BinaryOp::AndNot => Some(typ::is_integer(ty, self.objs())),
                    BinaryOp::LogAnd => Some(typ::is_boolean(ty, self.objs())),
                    BinaryOp::LogOr => Some(typ::is_boolean(ty, self.objs())),
                    _ => None,
                }
            } else {
                // For unary, we map to BinaryOp equivalents
                match o {
                    BinaryOp::Add => Some(typ::is_numeric(ty, self.objs())),
                    BinaryOp::Sub => Some(typ::is_numeric(ty, self.objs())),
                    BinaryOp::Xor => Some(typ::is_integer(ty, self.objs())),
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
                UnaryOp::Pos => Some(typ::is_numeric(ty, self.objs())),
                UnaryOp::Neg => Some(typ::is_numeric(ty, self.objs())),
                UnaryOp::BitNot => Some(typ::is_integer(ty, self.objs())),
                UnaryOp::Not => Some(typ::is_boolean(ty, self.objs())),
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
        e: Option<&Expr>,
        op: UnaryOp,
        operand_expr: &Expr,
    ) {
        match op {
            UnaryOp::Addr => {
                // spec: "As an exception to the addressability requirement
                // x may also be a composite literal."
                match &Self::unparen(operand_expr).kind {
                    ExprKind::CompositeLit(_) => {}
                    _ => {
                        if x.mode != OperandMode::Variable {
                            self.invalid_op(Span::default(), "cannot take address of expression");
                            x.mode = OperandMode::Invalid;
                            return;
                        }
                    }
                }
                x.mode = OperandMode::Value;
                x.typ = Some(self.new_t_pointer(x.typ.unwrap()));
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
                    let ty = typ::underlying_type(x.typ.unwrap(), self.objs());
                    let tval = self.otype(ty);
                    let prec = if tval.is_unsigned(self.objs()) {
                        tval.try_as_basic().map(|b| b.size_of()).unwrap_or(0)
                    } else {
                        0
                    };
                    *v = Value::unary_op(op, v, prec);
                    // Typed constants must be representable in
                    // their type after each constant operation.
                    if tval.is_typed(self.objs()) {
                        if let Some(expr) = e {
                            x.set_expr(expr); // for better error message
                        }
                        self.representable(x, ty);
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
    pub fn representable(&mut self, x: &mut Operand, t: TypeKey) {
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
                let msg = if xtval.is_numeric(self.objs()) && tval.is_numeric(self.objs()) {
                    if !xtval.is_integer(self.objs()) && tval.is_integer(self.objs()) {
                        "truncated".to_string()
                    } else {
                        "overflows".to_string()
                    }
                } else {
                    "cannot convert".to_string()
                };
                self.error_code_msg(TypeError::TypeMismatch, Span::default(), msg);
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
    ) {
        let info = match self.untyped.get(&expr_id) {
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
                self.update_expr_type(inner.id, t, final_);
            }
            ExprKind::Unary(u) => {
                // If x is a constant, the operands were constants.
                if info.mode.constant_val().is_none() {
                    self.update_expr_type(u.operand.id, t, final_);
                }
            }
            ExprKind::Binary(b) => {
                if info.mode.constant_val().is_none() {
                    if Self::is_comparison(b.op) {
                        // The result type is independent of operand types
                    } else if Self::is_shift(b.op) {
                        // The result type depends only on lhs operand.
                        self.update_expr_type(b.left.id, t, final_);
                    } else {
                        // The operand types match the result type.
                        self.update_expr_type(b.left.id, t, final_);
                        self.update_expr_type(b.right.id, t, final_);
                    }
                }
            }
            _ => {}
        }

        // If the new type is not final and still untyped, just update the recorded type.
        if !final_ && typ::is_untyped(t, self.objs()) {
            let underlying = typ::underlying_type(t, self.objs());
            if let Some(info) = self.untyped.get_mut(&expr_id) {
                info.typ = Some(underlying);
            }
            return;
        }

        // Otherwise we have the final (typed or untyped type).
        // Remove it from the map of yet untyped expressions.
        let removed = self.untyped.remove(&expr_id);
        let info = match removed {
            Some(o) => o,
            None => return,
        };

        if info.is_lhs {
            // If x is the lhs of a shift, its final type must be integer.
            if !typ::is_integer(t, self.objs()) {
                self.invalid_op(Span::default(), "shifted operand must be integer");
                return;
            }
        }
        if info.mode.constant_val().is_some() {
            // If x is a constant, it must be representable as a value of typ.
            let mut c = Operand::with_mode(info.mode.clone(), info.typ);
            self.convert_untyped(&mut c, t);
            if c.invalid() {
                return;
            }
        }

        // Everything's fine, record final type and value for x.
        self.result.record_type_and_value(expr_id, info.mode.clone(), t);
    }

    /// Updates the value of x to val in the untyped map.
    fn update_expr_val(&mut self, expr_id: ExprId, val: Value) {
        if let Some(info) = self.untyped.get_mut(&expr_id) {
            if let OperandMode::Constant(_) = &info.mode {
                info.mode = OperandMode::Constant(val);
            }
        }
    }

    /// Attempts to set the type of an untyped value to the target type.
    pub fn convert_untyped(
        &mut self,
        x: &mut Operand,
        target: TypeKey,
    ) {
        if x.invalid() || typ::is_typed(x.typ.unwrap(), self.objs()) || target == self.invalid_type() {
            return;
        }

        if typ::is_untyped(target, self.objs()) {
            // both x and target are untyped
            let order = |bt: BasicType| -> usize {
                match bt {
                    BasicType::UntypedInt => 1,
                    BasicType::UntypedRune => 2,
                    BasicType::UntypedFloat => 3,
                    _ => unreachable!(),
                }
            };
            let xtval = self.otype(x.typ.unwrap());
            let ttval = self.otype(target);
            let xbasic = xtval.try_as_basic().unwrap().typ();
            let tbasic = ttval.try_as_basic().unwrap().typ();
            if xbasic != tbasic {
                if xtval.is_numeric(self.objs()) && ttval.is_numeric(self.objs()) {
                    if order(xbasic) < order(tbasic) {
                        x.typ = Some(target);
                        if let Some(expr_id) = x.expr_id() {
                            self.update_expr_type(expr_id, target, false);
                        }
                    }
                } else {
                    self.error_code_msg(TypeError::TypeMismatch, Span::default(), "cannot convert untyped value".to_string());
                    x.mode = OperandMode::Invalid;
                    return;
                }
            }
            return;
        }

        let t = typ::underlying_type(target, self.objs());
        let xtype = x.typ.unwrap();
        let tval = self.otype(t);
        let final_target = match tval {
            Type::Basic(_) => {
                if let OperandMode::Constant(v) = &x.mode {
                    let v_clone = v.clone();
                    self.representable(x, t);
                    if x.invalid() {
                        return;
                    }
                    // Expression value may have been rounded - update if needed
                    if let Some(expr_id) = x.expr_id() {
                        self.update_expr_val(expr_id, v_clone);
                    }
                    Some(target)
                } else {
                    // Non-constant untyped values may appear as the
                    // result of comparisons (untyped bool), intermediate
                    // (delayed-checked) rhs operands of shifts, and as
                    // the value nil.
                    let ok = match self.otype(x.typ.unwrap()).try_as_basic().unwrap().typ() {
                        BasicType::UntypedBool => tval.is_boolean(self.objs()),
                        BasicType::UntypedInt
                        | BasicType::UntypedRune
                        | BasicType::UntypedFloat => tval.is_numeric(self.objs()),
                        BasicType::UntypedString => unreachable!(),
                        BasicType::UntypedNil => typ::has_nil(t, self.objs()),
                        _ => false,
                    };
                    if ok { Some(target) } else { None }
                }
            }
            Type::Interface(detail) => {
                if x.is_nil(self.objs()) {
                    Some(self.basic_type(BasicType::UntypedNil))
                } else if detail.is_empty() {
                    Some(typ::untyped_default_type(xtype, self.objs()))
                } else {
                    None
                }
            }
            Type::Pointer(_) | Type::Signature(_) | Type::Slice(_) | Type::Map(_) | Type::Chan(_) => {
                if x.is_nil(self.objs()) {
                    Some(self.basic_type(BasicType::UntypedNil))
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(t) = final_target {
            x.typ = final_target;
            if let Some(expr_id) = x.expr_id() {
                self.update_expr_type(expr_id, t, true);
            }
        } else {
            self.error_code(TypeError::TypeMismatch, Span::default());
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
                    (xtval.comparable(self.objs()) && ytval.comparable(self.objs()))
                        || (x.is_nil(self.objs()) && typ::has_nil(ytype, self.objs()))
                        || (y.is_nil(self.objs()) && typ::has_nil(xtype, self.objs()))
                }
                BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => {
                    xtval.is_ordered(self.objs()) && ytval.is_ordered(self.objs())
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
            self.error_code_msg(TypeError::InvalidOp, Span::default(), format!("cannot compare: {}", m));
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
                if let Some(expr_id) = x.expr_id() {
                    let default_t = typ::untyped_default_type(xtype, self.objs());
                    self.update_expr_type(expr_id, default_t, true);
                }
                if let Some(expr_id) = y.expr_id() {
                    let default_t = typ::untyped_default_type(ytype, self.objs());
                    self.update_expr_type(expr_id, default_t, true);
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
        e: Option<&Expr>,
    ) {
        let xtval = self.otype(x.typ.unwrap());
        let xt_untyped = xtval.is_untyped(self.objs());
        let xt_integer = xtval.is_integer(self.objs());
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
        if ytval.is_unsigned(self.objs()) {
            // ok
        } else if ytval.is_untyped(self.objs()) {
            self.convert_untyped(y, self.basic_type(BasicType::Uint));
            if y.invalid() {
                x.mode = OperandMode::Invalid;
                return;
            }
        } else {
            self.error_code(TypeError::ShiftCountNotUnsigned, Span::default());
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
                if typ::is_typed(x.typ.unwrap(), self.objs()) {
                    if let Some(expr) = e {
                        x.set_expr(expr);
                    }
                    self.representable(x, x.typ.unwrap());
                }
                return;
            }

            if xt_untyped {
                // Delay operand checking until we know the final type
                if let Some(expr_id) = x.expr_id() {
                    if let Some(info) = self.untyped.get_mut(&expr_id) {
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

        if !typ::is_integer(x.typ.unwrap(), self.objs()) {
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
        e: Option<&Expr>,
        lhs: &Expr,
        rhs: &Expr,
        op: BinaryOp,
    ) {
        let mut y = Operand::new();
        self.expr(x, lhs);
        self.expr(&mut y, rhs);

        if x.invalid() {
            return;
        }
        if y.invalid() {
            x.mode = OperandMode::Invalid;
            x.expr = y.expr;
            return;
        }

        if Self::is_shift(op) {
            self.shift(x, &mut y, op, e);
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
            self.comparison(x, &y, op);
            return;
        }

        if !typ::identical_o(x.typ, y.typ, self.objs()) {
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
            if x.mode.constant_val().is_some() || typ::is_integer(x.typ.unwrap(), self.objs()) {
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
                let ty = typ::underlying_type(x.typ.unwrap(), self.objs());
                *vx = Value::binary_op(vx, op, vy);
                // Typed constants must be representable in their type
                if typ::is_typed(ty, self.objs()) {
                    if let Some(expr) = e {
                        x.set_expr(expr);
                    }
                    self.representable(x, ty);
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
    ) -> Result<Option<u64>, ()> {
        let x = &mut Operand::new();
        self.expr(x, index);
        if x.invalid() {
            return Err(());
        }

        // An untyped constant must be representable as Int
        self.convert_untyped(x, self.basic_type(BasicType::Int));
        if x.invalid() {
            return Err(());
        }

        // The index must be of integer type
        if !typ::is_integer(x.typ.unwrap(), self.objs()) {
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
                        let i = self.index(key_expr, length);
                        if let Ok(Some(idx)) = i {
                            Some(idx)
                        } else if i.is_ok() {
                            self.error_code_msg(TypeError::InvalidOp, Span::default(), "index must be integer constant");
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
                self.error_code_msg(
                    TypeError::InvalidOp,
                    Span::default(),
                    format!("index {} is out of bounds (>= {})", index, length.unwrap()),
                );
                (None, &elem.value)
            } else {
                (Some(index), &elem.value)
            };

            if let Some(i) = valid_index {
                if visited.contains(&i) {
                    self.error_code_msg(
                        TypeError::DuplicateCase,
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
            self.raw_expr(x, eval, Some(t));
            self.assignment(x, Some(t), "array or slice literal");
        }
        max
    }

    /// Type-checks an expression with a type hint (for composite literal elements).
    pub fn expr_with_hint(
        &mut self,
        x: &mut Operand,
        e: &Expr,
        hint: Option<TypeKey>,
    ) {
        self.raw_expr(x, e, hint);
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
    ) {
        self.raw_internal(x, e, hint);

        let ty = match &x.mode {
            OperandMode::Invalid => self.invalid_type(),
            OperandMode::NoValue => self.universe().no_value_tuple(),
            _ => x.typ.unwrap_or(self.invalid_type()),
        };

        if typ::is_untyped(ty, self.objs()) {
            // Delay type and value recording until we know the type
            self.remember_untyped(
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
    ) {
        // Make sure x has a valid state in case of bailout
        x.mode = OperandMode::Invalid;
        x.typ = Some(self.invalid_type());
        x.set_expr(e);

        match &e.kind {
            ExprKind::Ident(ident) => {
                self.ident(x, ident, None, false);
            }
            ExprKind::IntLit(lit) => {
                let raw = self.resolve_symbol(lit.raw);
                let val = raw.parse::<i64>().unwrap_or(0);
                x.mode = OperandMode::Constant(Value::with_i64(val));
                x.typ = Some(self.basic_type(BasicType::UntypedInt));
            }
            ExprKind::FloatLit(lit) => {
                let raw = self.resolve_symbol(lit.raw);
                let val = raw.parse::<f64>().unwrap_or(0.0);
                x.mode = OperandMode::Constant(Value::with_f64(val));
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
                self.raw_internal(x, inner, hint);
            }
            ExprKind::Unary(u) => {
                self.expr(x, &u.operand);
                if x.invalid() {
                    return;
                }
                self.unary(x, Some(e), u.op, &u.operand);
            }
            ExprKind::Binary(b) => {
                self.binary(x, Some(e), &b.left, &b.right, b.op);
            }
            ExprKind::Call(call) => {
                self.call(x, call, e.span);
            }
            ExprKind::Index(idx) => {
                self.expr(x, &idx.expr);
                if x.invalid() {
                    return;
                }

                let utype = typ::underlying_type(x.typ.unwrap(), self.objs());
                let utype_val = self.otype(utype);
                let (valid, length) = match &utype_val {
                    Type::Basic(_) if typ::is_string(utype, self.objs()) => {
                        // String indexing yields byte
                        let len = if let OperandMode::Constant(v) = &x.mode {
                            Some(v.str_as_string().len() as u64)
                        } else {
                            None
                        };
                        x.mode = OperandMode::Value;
                        x.typ = Some(self.universe().byte());
                        (true, len)
                    }
                    Type::Array(arr) => {
                        if x.mode != OperandMode::Variable {
                            x.mode = OperandMode::Value;
                        }
                        x.typ = Some(arr.elem());
                        (true, arr.len())
                    }
                    Type::Pointer(ptr) => {
                        // Pointer to array
                        if let Some(arr) = self.otype(ptr.base()).underlying_val(self.objs()).try_as_array() {
                            x.mode = OperandMode::Variable;
                            x.typ = Some(arr.elem());
                            (true, arr.len())
                        } else {
                            (false, None)
                        }
                    }
                    Type::Slice(sl) => {
                        x.mode = OperandMode::Variable;
                        x.typ = Some(sl.elem());
                        (true, None)
                    }
                    Type::Map(m) => {
                        let key = m.key();
                        let elem = m.elem();
                        // Check index type
                        let mut xkey = Operand::new();
                        self.expr(&mut xkey, &idx.index);
                        self.assignment(&mut xkey, Some(key), "map index");
                        if xkey.invalid() {
                            x.mode = OperandMode::Invalid;
                            return;
                        }
                        x.mode = OperandMode::MapIndex;
                        x.typ = Some(elem);
                        return;
                    }
                    _ => (false, None),
                };

                if !valid {
                    self.invalid_op(Span::default(), "cannot index expression");
                    x.mode = OperandMode::Invalid;
                    return;
                }
                // Check index
                let _ = self.index(&idx.index, length);
            }
            ExprKind::Slice(sl) => {
                self.expr(x, &sl.expr);
                if x.invalid() {
                    return;
                }

                let utype = typ::underlying_type(x.typ.unwrap(), self.objs());
                
                // Extract type info first to avoid borrow conflicts
                enum SliceInfo {
                    String { len: Option<u64>, is_untyped: bool },
                    Array { elem: TypeKey, len: Option<u64>, addressable: bool },
                    PointerArray { elem: TypeKey, len: Option<u64> },
                    Slice,
                    Invalid,
                }
                
                let info = {
                    let utype_val = self.otype(utype);
                    match utype_val {
                        Type::Basic(_) if typ::is_string(utype, self.objs()) => {
                            let len = if let OperandMode::Constant(v) = &x.mode {
                                Some(v.str_as_string().len() as u64)
                            } else {
                                None
                            };
                            SliceInfo::String { len, is_untyped: typ::is_untyped(utype, self.objs()) }
                        }
                        Type::Array(arr) => {
                            SliceInfo::Array { 
                                elem: arr.elem(), 
                                len: arr.len(),
                                addressable: x.mode == OperandMode::Variable 
                            }
                        }
                        Type::Pointer(ptr) => {
                            if let Some(arr) = self.otype(ptr.base()).underlying_val(self.objs()).try_as_array() {
                                SliceInfo::PointerArray { elem: arr.elem(), len: arr.len() }
                            } else {
                                SliceInfo::Invalid
                            }
                        }
                        Type::Slice(_) => SliceInfo::Slice,
                        _ => SliceInfo::Invalid,
                    }
                };
                
                let (valid, length) = match info {
                    SliceInfo::String { len, is_untyped } => {
                        if is_untyped {
                            x.typ = Some(self.basic_type(BasicType::Str));
                        }
                        (true, len)
                    }
                    SliceInfo::Array { elem, len, addressable } => {
                        if !addressable {
                            self.invalid_op(Span::default(), "cannot slice array (value not addressable)");
                            x.mode = OperandMode::Invalid;
                            return;
                        }
                        x.typ = Some(self.new_t_slice(elem));
                        (true, len)
                    }
                    SliceInfo::PointerArray { elem, len } => {
                        x.mode = OperandMode::Variable;
                        x.typ = Some(self.new_t_slice(elem));
                        (true, len)
                    }
                    SliceInfo::Slice => (true, None),
                    SliceInfo::Invalid => (false, None),
                };

                if !valid {
                    self.invalid_op(Span::default(), "cannot slice expression");
                    x.mode = OperandMode::Invalid;
                    return;
                }
                x.mode = OperandMode::Value;

                // Check slice indices
                if let Some(ref low) = sl.low {
                    let _ = self.index(low, length);
                }
                if let Some(ref high) = sl.high {
                    let _ = self.index(high, length);
                }
            }
            ExprKind::Selector(sel) => {
                self.selector(x, sel);
            }
            ExprKind::TypeAssert(ta) => {
                self.expr(x, &ta.expr);
                if x.invalid() {
                    return;
                }
                
                // Check that x is an interface type
                let xtype = typ::underlying_type(x.typ.unwrap(), self.objs());
                if self.otype(xtype).try_as_interface().is_none() {
                    self.invalid_op(Span::default(), "type assertion requires interface type");
                    x.mode = OperandMode::Invalid;
                    return;
                }
                
                // x.(type) expressions are handled in type switches
                if ta.ty.is_none() {
                    self.invalid_op(Span::default(), "use of .(type) outside type switch");
                    x.mode = OperandMode::Invalid;
                    return;
                }
                
                let target = self.type_expr(ta.ty.as_ref().unwrap());
                if target == self.invalid_type() {
                    x.mode = OperandMode::Invalid;
                    return;
                }
                
                // Check type assertion validity
                self.type_assertion(x, xtype, target, e.span);
                x.mode = OperandMode::CommaOk;
                x.typ = Some(target);
            }
            ExprKind::CompositeLit(lit) => {
                let ty = self.type_expr(&lit.ty);
                if ty == self.invalid_type() {
                    x.mode = OperandMode::Invalid;
                    return;
                }
                
                let utype = typ::underlying_type(ty, self.objs());
                let utype_val = self.otype(utype);
                
                match &utype_val {
                    Type::Struct(detail) => {
                        let fields = detail.fields().clone();
                        // Check elements
                        for (i, elem) in lit.elems.iter().enumerate() {
                            let mut val = Operand::new();
                            self.expr(&mut val, &elem.value);
                            if val.invalid() {
                                continue;
                            }
                            
                            // Get field type
                            let field_type = if let Some(ref key) = elem.key {
                                // Keyed element: field:value
                                match key {
                                    CompositeLitKey::Ident(ident) => {
                                        let name = self.resolve_ident(ident);
                                        fields.iter().find_map(|&f| {
                                            let fld = self.lobj(f);
                                            if fld.name() == name {
                                                fld.typ()
                                            } else {
                                                None
                                            }
                                        })
                                    }
                                    _ => None,
                                }
                            } else {
                                // Positional element
                                fields.get(i).and_then(|&f| self.lobj(f).typ())
                            };
                            
                            if let Some(ft) = field_type {
                                self.assignment(&mut val, Some(ft), "struct literal");
                            }
                        }
                    }
                    Type::Array(arr) => {
                        let elem_type = arr.elem();
                        let arr_len = arr.len();
                        let n = self.indexed_elems(&lit.elems, elem_type, arr_len);
                        // If array has unknown length (e.g. [...]T), set it now
                        if arr_len.is_none() {
                            if let Some(arr_mut) = self.otype_mut(utype).try_as_array_mut() {
                                arr_mut.set_len(n);
                            }
                        }
                    }
                    Type::Slice(sl) => {
                        let elem_type = sl.elem();
                        self.indexed_elems(&lit.elems, elem_type, None);
                    }
                    Type::Map(m) => {
                        let key_type = m.key();
                        let elem_type = m.elem();
                        for elem in &lit.elems {
                            // Check key
                            if let Some(ref key) = elem.key {
                                if let CompositeLitKey::Expr(key_expr) = key {
                                    let mut key_op = Operand::new();
                                    self.expr(&mut key_op, key_expr);
                                    if !key_op.invalid() {
                                        self.assignment(&mut key_op, Some(key_type), "map literal key");
                                    }
                                }
                            }
                            // Check value
                            let mut val = Operand::new();
                            self.expr(&mut val, &elem.value);
                            if !val.invalid() {
                                self.assignment(&mut val, Some(elem_type), "map literal value");
                            }
                        }
                    }
                    _ => {
                        self.invalid_op(Span::default(), "invalid composite literal type");
                        x.mode = OperandMode::Invalid;
                        return;
                    }
                }
                
                x.mode = OperandMode::Value;
                x.typ = Some(ty);
            }
            ExprKind::FuncLit(func) => {
                // Get function type from signature
                let t = self.func_type_from_sig(None, &func.sig);
                if self.otype(t).try_as_signature().is_some() {
                    x.mode = OperandMode::Value;
                    x.typ = Some(t);
                    // Note: func_body checking is deferred via delayed action
                } else {
                    x.mode = OperandMode::Invalid;
                    x.typ = Some(self.invalid_type());
                }
            }
            ExprKind::Conversion(conv) => {
                let ty = self.type_expr(&conv.ty);
                self.expr(x, &conv.expr);
                if x.invalid() {
                    return;
                }
                self.convert_untyped(x, ty);
            }
            ExprKind::Receive(recv) => {
                self.expr(x, recv);
                if x.invalid() {
                    return;
                }
                if let Some(chan) = self.otype(x.typ.unwrap()).underlying_val(self.objs()).try_as_chan() {
                    if chan.dir() == typ::ChanDir::SendOnly {
                        self.invalid_op(recv.span, "cannot receive from send-only channel");
                        x.mode = OperandMode::Invalid;
                        return;
                    }
                    x.mode = OperandMode::CommaOk;
                    x.typ = Some(chan.elem());
                    self.octx.has_call_or_recv = true;
                } else {
                    self.invalid_op(recv.span, "cannot receive from non-channel");
                    x.mode = OperandMode::Invalid;
                }
            }
            ExprKind::TypeAsExpr(ty) => {
                let t = self.type_expr(ty);
                x.mode = OperandMode::TypeExpr;
                x.typ = Some(t);
            }
            ExprKind::TryUnwrap(inner) => {
                // GoX extension: ? operator for error propagation
                // The inner expression must return a value where the last element is of type error.
                // If error != nil, the function returns early with that error.
                // If error == nil, the result is the value(s) without the error part.
                self.expr(x, inner);
                if x.invalid() {
                    return;
                }

                let error_type = self.universe().error_type();
                let inner_type = x.typ.unwrap_or(self.invalid_type());

                // Check if the type is a tuple with error as the last element
                if let Some(tuple) = self.otype(inner_type).try_as_tuple() {
                    let vars = tuple.vars();
                    if vars.is_empty() {
                        self.error_code_msg(TypeError::InvalidOp, e.span, "? operator requires expression returning error");
                        x.mode = OperandMode::Invalid;
                        return;
                    }

                    // Check that the last element is error type
                    let last_var = vars.last().unwrap();
                    let last_type = self.lobj(*last_var).typ().unwrap_or(self.invalid_type());
                    if !typ::identical(last_type, error_type, self.objs()) {
                        self.error_code_msg(TypeError::InvalidOp, e.span, "? operator requires expression with error as last return value");
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
                            let new_tuple = self.new_t_tuple(new_vars);
                            x.mode = OperandMode::Value;
                            x.typ = Some(new_tuple);
                        }
                    }
                } else if typ::identical(inner_type, error_type, self.objs()) {
                    // Single error type -> NoValue after unwrap
                    x.mode = OperandMode::NoValue;
                    x.typ = None;
                } else {
                    self.error_code_msg(TypeError::InvalidOp, e.span, "? operator requires expression returning error type");
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
        span: Span,
    ) {
        // Check that xtype is an interface type
        if self.otype(xtype).try_as_interface().is_none() {
            self.error_code(TypeError::TypeAssertNotInterface, span);
            x.mode = OperandMode::Invalid;
            return;
        }
        
        // Check that t can satisfy the interface
        if let Some((missing, wrong_type)) = crate::lookup::assertable_to(xtype, t, self) {
            let method_name = self.lobj(missing).name();
            if wrong_type {
                self.error_code_msg(
                    TypeError::TypeMismatch,
                    span,
                    format!("impossible type assertion: method {} has wrong type", method_name),
                );
            } else {
                self.error_code_msg(
                    TypeError::TypeMismatch,
                    span,
                    format!("impossible type assertion: missing method {}", method_name),
                );
            }
        }
        
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
            self.error_code_msg(TypeError::InvalidOp, Span::default(), format!("expression {}", m));
            // Don't set x.mode to Invalid here - caller handles it
        }
    }

    /// Checks that x is a single value (not a tuple).
    pub fn single_value(&self, x: &mut Operand) {
        if x.mode == OperandMode::Value {
            if let Some(tuple) = self.otype(x.typ.unwrap()).try_as_tuple() {
                let len = tuple.vars().len();
                if len != 1 {
                    self.error_code_msg(
                        TypeError::InvalidOp,
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
    pub fn expr(&mut self, x: &mut Operand, e: &Expr) {
        self.multi_expr(x, e);
        self.single_value(x);
    }

    /// Like expr but the result may be a multi-value.
    pub fn multi_expr(&mut self, x: &mut Operand, e: &Expr) {
        self.raw_expr(x, e, None);
        self.expr_value_err(x);
    }

    /// Simple expression check - for use in stmt.rs where fctx is not available.
    /// Returns the type of the expression.
    pub fn check_expr(&mut self, _e: &Expr) -> TypeKey {
        // Simplified check - just record the expression type
        // Full checking is done when fctx is available
        self.invalid_type()
    }

    /// Typechecks expression or type e and initializes x with the expression
    /// value or type. If an error occurred, x.mode is set to invalid.
    pub fn expr_or_type(&mut self, x: &mut Operand, e: &Expr) {
        self.raw_expr(x, e, None);
        self.single_value(x);
        if x.mode == OperandMode::NoValue {
            self.error_code_msg(TypeError::InvalidOp, Span::default(), "expression used as value or type");
            x.mode = OperandMode::Invalid;
        }
    }

    /// Type-checks a selector expression (e.g., x.f).
    pub fn selector(
        &mut self,
        x: &mut Operand,
        sel: &gox_syntax::ast::SelectorExpr,
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
                            self.error_code_msg(TypeError::Undeclared, sel.sel.span, msg);
                        }
                        x.mode = OperandMode::Invalid;
                        x.set_expr(&sel.expr);
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
                        self.error_code_msg(TypeError::Undeclared, sel.sel.span, msg);
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
                    x.set_expr(&sel.expr);
                    return;
                }
            }
        }

        self.expr_or_type(x, &sel.expr);
        if x.invalid() {
            x.set_expr(&sel.expr);
            return;
        }

        let sel_name = self.resolve_ident(&sel.sel).to_string();
        let result = crate::lookup::lookup_field_or_method(
            x.typ.unwrap(),
            x.mode == OperandMode::Variable,
            Some(self.pkg),
            &sel_name,
            self.objs(),
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
                self.error_code_msg(TypeError::Undeclared, sel.sel.span, msg);
                x.mode = OperandMode::Invalid;
                x.set_expr(&sel.expr);
                return;
            }
        };

        // methods may not have a fully set up signature yet
        if self.lobj(okey).entity_type().is_func() {
            self.obj_decl(okey, None);
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
                    self.objs(),
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
                let params = self.new_t_tuple(vars);
                let new_sig = self.new_t_signature(None, None, params, r, v);
                x.mode = OperandMode::Value;
                x.typ = Some(new_sig);

                self.add_decl_dep(okey);
            } else {
                let msg = format!(
                    "{} undefined (type has no method {})",
                    sel_name, sel_name
                );
                self.error_code_msg(TypeError::Undeclared, sel.sel.span, msg);
                x.mode = OperandMode::Invalid;
                x.set_expr(&sel.expr);
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
                    self.objs(),
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
                    self.objs(),
                );
                self.result.record_selection(sel.expr.id, selection);

                x.mode = OperandMode::Value;

                // remove receiver
                let lobj = self.lobj(okey);
                let sig = self.otype(lobj.typ().unwrap()).try_as_signature().unwrap();
                let (p, r, v) = (sig.params(), sig.results(), sig.variadic());
                let new_sig = self.new_t_signature(None, None, p, r, v);
                x.typ = Some(new_sig);

                self.add_decl_dep(okey);
            } else {
                unreachable!();
            }
        }
        x.set_expr(&sel.expr);
    }
}
