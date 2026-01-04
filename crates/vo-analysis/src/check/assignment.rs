//! Assignment checking.
//!
//! This module implements assignment compatibility checking, variable
//! initialization, and short variable declarations.


use vo_common::span::{BytePos, Span};
use vo_syntax::ast::Ident;
use vo_syntax::ast::Expr;

use crate::objects::{ObjKey, TypeKey};
use crate::operand::{Operand, OperandMode};
use crate::typ::{self, BasicType, Type};

/// Default span for error reporting when no span is available.
const DEFAULT_SPAN: Span = Span { start: BytePos(0), end: BytePos(0) };

use super::checker::Checker;
use super::errors::TypeError;

impl Checker {
    /// Reports whether x can be assigned to a variable of type t.
    /// If necessary, converts untyped values to the appropriate type.
    /// Use t == None to indicate assignment to an untyped blank identifier.
    /// x.mode is set to invalid if the assignment failed.
    pub(crate) fn assignment(&mut self, x: &mut Operand, t: Option<TypeKey>, note: &str) {
        self.single_value(x);
        if x.invalid() {
            return;
        }

        match x.mode {
            OperandMode::Constant(_)
            | OperandMode::Variable
            | OperandMode::MapIndex
            | OperandMode::Value
            | OperandMode::CommaOk => {}
            _ => return,
        }

        let xt = match x.typ {
            Some(t) => t,
            None => return,
        };

        if typ::is_untyped(xt, self.objs()) {
            if t.is_none() && xt == self.basic_type(BasicType::UntypedNil) {
                self.error_code_msg(TypeError::UseOfUntypedNil, DEFAULT_SPAN, format!("use of untyped nil in {}", note));
                x.mode = OperandMode::Invalid;
                return;
            }
            // spec: "If an untyped constant is assigned to a variable of interface
            // type or the blank identifier, the constant is first converted to type
            // bool, rune, int, float64, or string respectively."
            let target = if t.is_none() || typ::is_interface(t.unwrap(), self.objs()) {
                typ::untyped_default_type(xt, self.objs())
            } else {
                t.unwrap()
            };
            self.convert_untyped(x, target);
            if x.invalid() {
                return;
            }
        }
        // x.typ is typed

        // spec: "If a left-hand side is the blank identifier, any typed or
        // non-constant value except for the predeclared identifier nil may
        // be assigned to it."
        if t.is_none() {
            return;
        }

        let mut reason = String::new();
        if !self.assignable_to(x, t.unwrap(), &mut reason) {
            if reason.is_empty() {
                self.error_code_msg(TypeError::CannotAssign, DEFAULT_SPAN, format!("cannot use value as type in {}", note));
            } else {
                self.error_code_msg(TypeError::CannotAssign, DEFAULT_SPAN, format!("cannot use value as type in {}: {}", note, reason));
            }
            x.mode = OperandMode::Invalid;
        }
    }

    /// Initializes a constant with value x.
    pub(crate) fn init_const(&mut self, lhs: ObjKey, x: &mut Operand) {
        let invalid_type = self.invalid_type();
        
        if x.invalid() || x.typ == Some(invalid_type) {
            self.lobj_mut(lhs).set_type(Some(invalid_type));
            return;
        }
        
        if self.lobj(lhs).typ() == Some(invalid_type) {
            return;
        }

        // If the lhs doesn't have a type yet, use the type of x.
        if self.lobj(lhs).typ().is_none() {
            self.lobj_mut(lhs).set_type(x.typ);
        }

        // rhs must be a constant
        if let OperandMode::Constant(ref _val) = x.mode {
            let t = self.lobj(lhs).typ();
            self.assignment(x, t, "constant declaration");
            if x.mode != OperandMode::Invalid {
                if let OperandMode::Constant(ref v) = x.mode {
                    self.lobj_mut(lhs).set_const_val(v.clone());
                }
            }
        } else {
            self.error_code(TypeError::NotConstant, DEFAULT_SPAN);
        }
    }

    /// Initializes a variable with value x.
    pub(crate) fn init_var(&mut self, lhs: ObjKey, x: &mut Operand, msg: &str) -> Option<TypeKey> {
        let invalid_type = self.invalid_type();
        
        if x.invalid() || x.typ == Some(invalid_type) {
            if self.lobj(lhs).typ().is_none() {
                self.lobj_mut(lhs).set_type(Some(invalid_type));
            }
            return None;
        }
        
        if self.lobj(lhs).typ() == Some(invalid_type) {
            return None;
        }

        // If the lhs doesn't have a type yet, use the type of x.
        if self.lobj(lhs).typ().is_none() {
            let xt = x.typ.unwrap();
            let lhs_type = if typ::is_untyped(xt, self.objs()) {
                // convert untyped types to default types
                if xt == self.basic_type(BasicType::UntypedNil) {
                    self.error_code_msg(TypeError::UseOfUntypedNil, DEFAULT_SPAN, format!("use of untyped nil in {}", msg));
                    invalid_type
                } else {
                    typ::untyped_default_type(xt, self.objs())
                }
            } else {
                xt
            };

            self.lobj_mut(lhs).set_type(Some(lhs_type));
            if lhs_type == invalid_type {
                return None;
            }
        }

        let t = self.lobj(lhs).typ();
        self.assignment(x, t, msg);
        if x.mode != OperandMode::Invalid {
            x.typ
        } else {
            None
        }
    }

    /// Assigns x to the variable denoted by lhs expression.
    pub(crate) fn assign_var(&mut self, lhs: &Expr, x: &mut Operand) -> Option<TypeKey> {
        let invalid_type = self.invalid_type();
        if x.invalid() || x.typ == Some(invalid_type) {
            return None;
        }

        let mut v: Option<ObjKey> = None;
        let mut v_used = false;
        
        // determine if the lhs is a (possibly parenthesized) identifier.
        if let Some(ident) = self.expr_as_ident(lhs) {
            let name = self.resolve_ident(&ident);
            if name == "_" {
                self.result.record_def(ident, None);
                self.assignment(x, None, "assignment to _ identifier");
                return if x.mode != OperandMode::Invalid {
                    x.typ
                } else {
                    None
                };
            } else {
                // If the lhs is an identifier denoting a variable v, this assignment
                // is not a 'use' of v. Remember current value of v.used and restore
                // after evaluating the lhs via check.expr.
                if let Some(okey) = self.lookup(name) {
                    // It's ok to mark non-local variables, but ignore variables
                    // from other packages to avoid potential race conditions with
                    // dot-imported variables.
                    if self.lobj(okey).entity_type().is_var() {
                        v = Some(okey);
                        if let crate::obj::EntityType::Var(prop) = self.lobj(okey).entity_type() {
                            v_used = prop.used;
                        }
                    }
                }
            }
        }

        // Evaluate lhs
        let mut z = Operand::new();
        self.expr(&mut z, lhs);
        
        // restore v.used
        if let Some(okey) = v {
            if let crate::obj::EntityType::Var(prop) = self.lobj_mut(okey).entity_type_mut() {
                prop.used = v_used;
            }
        }

        if z.mode == OperandMode::Invalid || z.typ == Some(invalid_type) {
            return None;
        }

        // spec: "Each left-hand side operand must be addressable, a map index
        // expression, or the blank identifier. Operands may be parenthesized."
        match z.mode {
            OperandMode::Variable | OperandMode::MapIndex => {}
            _ => {
                // Check if this is a selector on a map index (cannot assign to struct field in map)
                if let vo_syntax::ast::ExprKind::Selector(sel) = &lhs.kind {
                    let mut op = Operand::new();
                    self.expr(&mut op, &sel.expr);
                    if op.mode == OperandMode::MapIndex {
                        self.error_code(TypeError::CannotAssignMapField, lhs.span);
                        return None;
                    }
                }
                self.error_code(TypeError::CannotAssign, lhs.span);
                return None;
            }
        }

        self.assignment(x, z.typ, "assignment");
        if x.mode != OperandMode::Invalid {
            x.typ
        } else {
            None
        }
    }

    /// Initializes multiple variables from multiple values.
    /// Aligned with goscript/types/src/check/assignment.rs::init_vars
    /// If return_pos is Some, init_vars is called to type-check return expressions.
    pub(crate) fn init_vars(&mut self, lhs: &[ObjKey], rhs: &[Expr], return_pos: Option<Span>) {
        use std::cmp::Ordering;
        use super::util::UnpackResult;
        use vo_syntax::ast::ExprKind;
        
        let invalid_type = self.invalid_type();
        let ll = lhs.len();
        
        // Special handling for DynAccess: generate return type based on lhs types
        if rhs.len() == 1 && return_pos.is_none() {
            if let ExprKind::DynAccess(dyn_access) = &rhs[0].kind {
                self.init_vars_dyn_access(lhs, &rhs[0], dyn_access);
                return;
            }
        }
        
        // Use unpack to handle all cases uniformly
        // requires return_pos.is_none for comma-ok handling
        let result = self.unpack(rhs, ll, ll == 2 && return_pos.is_none(), false);
        
        let mut invalidate_lhs = || {
            for &okey in lhs.iter() {
                if self.lobj(okey).typ().is_none() {
                    self.lobj_mut(okey).set_type(Some(invalid_type));
                }
            }
        };
        
        match &result {
            UnpackResult::Error => invalidate_lhs(),
            UnpackResult::Tuple(_, _, _)
            | UnpackResult::CommaOk(_, _)
            | UnpackResult::Multiple(_, _)
            | UnpackResult::Single(_, _)
            | UnpackResult::Nothing(_) => {
                let (count, ord) = result.rhs_count();
                match ord {
                    Ordering::Greater | Ordering::Less => {
                        invalidate_lhs();
                        result.use_(self, 0);
                        if !rhs.is_empty() {
                            if let Some(pos) = return_pos {
                                self.error_code_msg(
                                    TypeError::AssignmentMismatch,
                                    pos,
                                    format!("wrong number of return values (want {}, got {})", ll, count),
                                );
                            } else {
                                self.error_code_msg(
                                    TypeError::AssignmentMismatch,
                                    rhs[0].span,
                                    format!("cannot initialize {} variables with {} values", ll, count),
                                );
                            }
                        }
                        return;
                    }
                    Ordering::Equal => {
                        let context = if return_pos.is_some() { "return statement" } else { "assignment" };
                        for (i, &l) in lhs.iter().enumerate() {
                            let mut x = Operand::new();
                            result.get(self, &mut x, i);
                            self.init_var(l, &mut x, context);
                        }
                    }
                }
            }
        }
        if let UnpackResult::CommaOk(e, types) = result {
            if let Some(expr) = e {
                self.result.record_comma_ok_types(expr, &types, &mut self.tc_objs, self.pkg);
            }
        }
    }

    /// Assigns multiple values to multiple variables.
    /// Aligned with goscript/types/src/check/assignment.rs::assign_vars
    pub(crate) fn assign_vars(&mut self, lhs: &[Expr], rhs: &[Expr]) {
        use std::cmp::Ordering;
        use super::util::UnpackResult;
        use vo_syntax::ast::ExprKind;
        
        let ll = lhs.len();
        
        // Special handling for DynAccess: generate return type based on lhs types
        if rhs.len() == 1 {
            if let ExprKind::DynAccess(dyn_access) = &rhs[0].kind {
                self.assign_vars_dyn_access(lhs, &rhs[0], dyn_access);
                return;
            }
        }
        
        let result = self.unpack(rhs, ll, ll == 2, false);
        
        match &result {
            UnpackResult::Error => self.use_lhs(lhs),
            UnpackResult::Tuple(_, _, _)
            | UnpackResult::CommaOk(_, _)
            | UnpackResult::Multiple(_, _)
            | UnpackResult::Single(_, _)
            | UnpackResult::Nothing(_) => {
                let (count, ord) = result.rhs_count();
                match ord {
                    Ordering::Greater | Ordering::Less => {
                        result.use_(self, 0);
                        if !rhs.is_empty() {
                            self.error_code_msg(
                                TypeError::AssignmentMismatch,
                                rhs[0].span,
                                format!("cannot assign {} values to {} variables", count, ll),
                            );
                        }
                        return;
                    }
                    Ordering::Equal => {
                        for (i, l) in lhs.iter().enumerate() {
                            let mut x = Operand::new();
                            result.get(self, &mut x, i);
                            self.assign_var(l, &mut x);
                        }
                    }
                }
            }
        }
        if let UnpackResult::CommaOk(e, types) = result {
            if let Some(expr) = e {
                self.result.record_comma_ok_types(expr, &types, &mut self.tc_objs, self.pkg);
            }
        }
    }

    /// Handle DynAccess assignment: v1, v2, err := obj~>Method()
    /// Return type is derived from lhs types, with last being error.
    fn assign_vars_dyn_access(
        &mut self,
        lhs: &[Expr],
        rhs_expr: &Expr,
        dyn_access: &vo_syntax::ast::DynAccessExpr,
    ) {
        use vo_syntax::ast::ExprKind;
        
        let ll = lhs.len();
        if ll < 2 {
            self.error_code_msg(
                TypeError::AssignmentMismatch,
                rhs_expr.span,
                "dynamic access requires at least 2 lhs values (value, error)".to_string(),
            );
            return;
        }
        
        // Type check the base expression
        let mut base_x = Operand::new();
        self.multi_expr(&mut base_x, &dyn_access.base);
        if base_x.invalid() {
            return;
        }
        
        // Check base type is valid for dynamic access
        let base_type = base_x.typ.unwrap_or(self.invalid_type());
        if !self.is_dyn_access_base_type(base_type) {
            self.error_code_msg(
                TypeError::InvalidOp,
                dyn_access.base.span,
                "~> operator requires any/interface or (any, error) type".to_string(),
            );
            return;
        }
        
        // Type check operation arguments - convert to any (interface{})
        let any_type = self.new_t_empty_interface();
        match &dyn_access.op {
            vo_syntax::ast::DynAccessOp::Field(_) => {}
            vo_syntax::ast::DynAccessOp::Index(idx) => {
                let mut x = Operand::default();
                self.expr(&mut x, idx);
                self.convert_untyped(&mut x, any_type);
            }
            vo_syntax::ast::DynAccessOp::Call { args, .. }
            | vo_syntax::ast::DynAccessOp::MethodCall { args, .. } => {
                for arg in args {
                    let mut x = Operand::default();
                    self.expr(&mut x, arg);
                    self.convert_untyped(&mut x, any_type);
                }
            }
        }
        
        // Build return type tuple from lhs types
        // Last element must be error, others are value types
        let error_type = self.universe().error_type();
        let any_type = self.new_t_empty_interface();
        
        let mut tuple_vars = Vec::with_capacity(ll);
        for (i, lhs_expr) in lhs.iter().enumerate() {
            let elem_type = if i == ll - 1 {
                // Last element is error
                error_type
            } else {
                // Get lhs type, default to any for blank or untyped
                self.get_lhs_type_for_dyn(lhs_expr).unwrap_or(any_type)
            };
            tuple_vars.push(self.new_var(0, None, String::new(), Some(elem_type)));
        }
        
        let tuple_type = self.new_t_tuple(tuple_vars);
        
        // Record expression type
        self.result.record_type_and_value(rhs_expr.id, OperandMode::Value, tuple_type);
        
        // Assign each element to lhs
        let tuple_detail = self.otype(tuple_type).try_as_tuple().unwrap();
        let vars = tuple_detail.vars().to_vec();
        for (i, l) in lhs.iter().enumerate() {
            let var_type = self.lobj(vars[i]).typ();
            let mut x = Operand::new();
            x.mode = OperandMode::Value;
            x.typ = var_type;
            x.set_expr(rhs_expr);
            self.assign_var(l, &mut x);
        }
    }
    
    /// Get lhs type for dynamic access. Returns None for blank identifier.
    fn get_lhs_type_for_dyn(&mut self, lhs: &Expr) -> Option<TypeKey> {
        use vo_syntax::ast::ExprKind;
        
        // Check for blank identifier
        if let ExprKind::Ident(ident) = &lhs.kind {
            let name = self.resolve_ident(ident);
            if name == "_" {
                return None; // Blank - caller should use any
            }
            // Look up variable type
            if let Some(obj) = self.lookup(name) {
                return self.lobj(obj).typ();
            }
        }
        
        // For other expressions, type check and get type
        let mut x = Operand::new();
        self.expr(&mut x, lhs);
        x.typ
    }
    
    /// Handle DynAccess initialization for short var decl: v1, v2, err := obj~>Method()
    /// Return type is derived from lhs count, with all values as any and last being error.
    fn init_vars_dyn_access(
        &mut self,
        lhs: &[ObjKey],
        rhs_expr: &Expr,
        dyn_access: &vo_syntax::ast::DynAccessExpr,
    ) {
        let ll = lhs.len();
        if ll < 2 {
            self.error_code_msg(
                TypeError::AssignmentMismatch,
                rhs_expr.span,
                "dynamic access requires at least 2 lhs values (value, error)".to_string(),
            );
            return;
        }
        
        // Type check the base expression
        let mut base_x = Operand::new();
        self.multi_expr(&mut base_x, &dyn_access.base);
        if base_x.invalid() {
            return;
        }
        
        // Check base type is valid for dynamic access
        let base_type = base_x.typ.unwrap_or(self.invalid_type());
        if !self.is_dyn_access_base_type(base_type) {
            self.error_code_msg(
                TypeError::InvalidOp,
                dyn_access.base.span,
                "~> operator requires any/interface or (any, error) type".to_string(),
            );
            return;
        }
        
        // Type check operation arguments - convert to any (interface{})
        let any_type = self.new_t_empty_interface();
        match &dyn_access.op {
            vo_syntax::ast::DynAccessOp::Field(_) => {}
            vo_syntax::ast::DynAccessOp::Index(idx) => {
                let mut x = Operand::default();
                self.expr(&mut x, idx);
                self.convert_untyped(&mut x, any_type);
            }
            vo_syntax::ast::DynAccessOp::Call { args, .. }
            | vo_syntax::ast::DynAccessOp::MethodCall { args, .. } => {
                for arg in args {
                    let mut x = Operand::default();
                    self.expr(&mut x, arg);
                    self.convert_untyped(&mut x, any_type);
                }
            }
        }
        
        // Build return type tuple: (any, any, ..., error)
        let error_type = self.universe().error_type();
        let any_type = self.new_t_empty_interface();
        
        let mut tuple_vars = Vec::with_capacity(ll);
        for i in 0..ll {
            let elem_type = if i == ll - 1 {
                error_type
            } else {
                any_type
            };
            tuple_vars.push(self.new_var(0, None, String::new(), Some(elem_type)));
        }
        
        let tuple_type = self.new_t_tuple(tuple_vars.clone());
        
        // Record expression type
        self.result.record_type_and_value(rhs_expr.id, OperandMode::Value, tuple_type);
        
        // Initialize each lhs variable with corresponding type
        for (i, &l) in lhs.iter().enumerate() {
            let elem_type = if i == ll - 1 { error_type } else { any_type };
            self.lobj_mut(l).set_type(Some(elem_type));
        }
    }

    /// Checks if x is assignable to type t.
    /// Aligned with goscript/types/src/operand.rs::assignable_to
    pub(crate) fn assignable_to(&mut self, x: &Operand, t: TypeKey, reason: &mut String) -> bool {
        let xt = match x.typ {
            Some(typ) => typ,
            None => return false,
        };

        // Identical types are always assignable
        if typ::identical(xt, t, self.objs()) {
            return true;
        }

        // Check underlying types
        let xu = typ::underlying_type(xt, self.objs());
        let tu = typ::underlying_type(t, self.objs());
        let ut_x = self.otype(xu).underlying_val(self.objs());
        let ut_t = self.otype(tu).underlying_val(self.objs());

        // x is untyped
        if typ::is_untyped(xt, self.objs()) {
            match ut_t {
                Type::Basic(detail) => {
                    // untyped constant representable as t
                    if let Some(val) = x.mode.constant_val() {
                        return val.representable(detail, None);
                    }
                    // The result of a comparison is an untyped boolean, but may not be a constant
                    if detail.typ() == BasicType::Bool {
                        if let Type::Basic(xb) = ut_x {
                            return xb.typ() == BasicType::UntypedBool;
                        }
                    }
                }
                Type::Interface(detail) => {
                    // untyped nil or empty interface
                    return x.is_nil(self.objs()) || detail.is_empty();
                }
                Type::Pointer(_) | Type::Signature(_) | Type::Slice(_) | Type::Map(_) | Type::Chan(_) => {
                    return x.is_nil(self.objs());
                }
                _ => {}
            }
        }

        // x is typed
        // x's type V and T have identical underlying types and at least one is not a named type
        if typ::identical(xu, tu, self.objs()) {
            let x_is_named = !typ::identical(xt, xu, self.objs());
            let t_is_named = !typ::identical(t, tu, self.objs());
            if !x_is_named || !t_is_named {
                return true;
            }
        }

        // T is an interface type and x implements T
        if typ::is_interface(t, self.objs()) {
            if crate::lookup::missing_method(xt, t, true, self).is_none() {
                return true;
            }
            *reason = "does not implement interface".to_string();
            return false;
        }

        // x is a bidirectional channel value, T is a channel type,
        // they have identical element types, and at least one is not a named type
        if let (Type::Chan(xc), Type::Chan(tc)) = (ut_x, ut_t) {
            use crate::typ::ChanDir;
            if xc.dir() == ChanDir::SendRecv {
                if typ::identical(xc.elem(), tc.elem(), self.objs()) {
                    let x_is_named = !typ::identical(xt, xu, self.objs());
                    let t_is_named = !typ::identical(t, tu, self.objs());
                    if !x_is_named || !t_is_named {
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Extracts an identifier from an expression if possible.
    pub(crate) fn expr_as_ident(&self, e: &Expr) -> Option<Ident> {
        match &e.kind {
            vo_syntax::ast::ExprKind::Ident(ident) => Some(ident.clone()),
            vo_syntax::ast::ExprKind::Paren(inner) => self.expr_as_ident(inner),
            _ => None,
        }
    }
}
