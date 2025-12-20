//! Assignment checking.
//!
//! This module implements assignment compatibility checking, variable
//! initialization, and short variable declarations.

#![allow(dead_code)]

use gox_common::span::{BytePos, Span};
use gox_common::symbol::Ident;
use gox_syntax::ast::Expr;

use crate::objects::{ObjKey, TypeKey};
use crate::operand::{Operand, OperandMode};
use crate::typ::{self, BasicType};

/// Default span for error reporting when no span is available.
const DEFAULT_SPAN: Span = Span { start: BytePos(0), end: BytePos(0) };

use super::checker::{Checker, FilesContext};

impl Checker {
    /// Reports whether x can be assigned to a variable of type t.
    /// If necessary, converts untyped values to the appropriate type.
    /// Use t == None to indicate assignment to an untyped blank identifier.
    /// x.mode is set to invalid if the assignment failed.
    pub fn assignment(&mut self, x: &mut Operand, t: Option<TypeKey>, note: &str, _fctx: &mut FilesContext) {
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

        if typ::is_untyped(xt, &self.tc_objs) {
            if t.is_none() && xt == self.basic_type(BasicType::UntypedNil) {
                self.error(DEFAULT_SPAN, format!("use of untyped nil in {}", note));
                x.mode = OperandMode::Invalid;
                return;
            }
            // spec: "If an untyped constant is assigned to a variable of interface
            // type or the blank identifier, the constant is first converted to type
            // bool, rune, int, float64, or string respectively."
            let target = if t.is_none() || typ::is_interface(t.unwrap(), &self.tc_objs) {
                typ::untyped_default_type(xt, &self.tc_objs)
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
                self.error(DEFAULT_SPAN, format!("cannot use value as type in {}", note));
            } else {
                self.error(DEFAULT_SPAN, format!("cannot use value as type in {}: {}", note, reason));
            }
            x.mode = OperandMode::Invalid;
        }
    }

    /// Initializes a constant with value x.
    pub fn init_const(&mut self, lhs: ObjKey, x: &mut Operand, fctx: &mut FilesContext) {
        let invalid_type = self.invalid_type();
        
        if x.invalid() || x.typ == Some(invalid_type) {
            self.tc_objs.lobjs[lhs].set_type(Some(invalid_type));
            return;
        }
        
        if self.tc_objs.lobjs[lhs].typ() == Some(invalid_type) {
            return;
        }

        // If the lhs doesn't have a type yet, use the type of x.
        if self.tc_objs.lobjs[lhs].typ().is_none() {
            self.tc_objs.lobjs[lhs].set_type(x.typ);
        }

        // rhs must be a constant
        if let OperandMode::Constant(ref _val) = x.mode {
            let t = self.tc_objs.lobjs[lhs].typ();
            self.assignment(x, t, "constant declaration", fctx);
            if x.mode != OperandMode::Invalid {
                if let OperandMode::Constant(ref v) = x.mode {
                    self.tc_objs.lobjs[lhs].set_const_val(v.clone());
                }
            }
        } else {
            self.error(DEFAULT_SPAN, "value is not constant".to_string());
        }
    }

    /// Initializes a variable with value x.
    pub fn init_var(&mut self, lhs: ObjKey, x: &mut Operand, msg: &str, fctx: &mut FilesContext) -> Option<TypeKey> {
        let invalid_type = self.invalid_type();
        
        if x.invalid() || x.typ == Some(invalid_type) {
            if self.tc_objs.lobjs[lhs].typ().is_none() {
                self.tc_objs.lobjs[lhs].set_type(Some(invalid_type));
            }
            return None;
        }
        
        if self.tc_objs.lobjs[lhs].typ() == Some(invalid_type) {
            return None;
        }

        // If the lhs doesn't have a type yet, use the type of x.
        if self.tc_objs.lobjs[lhs].typ().is_none() {
            let xt = x.typ.unwrap();
            let lhs_type = if typ::is_untyped(xt, &self.tc_objs) {
                // convert untyped types to default types
                if xt == self.basic_type(BasicType::UntypedNil) {
                    self.error(DEFAULT_SPAN, format!("use of untyped nil in {}", msg));
                    invalid_type
                } else {
                    typ::untyped_default_type(xt, &self.tc_objs)
                }
            } else {
                xt
            };

            self.tc_objs.lobjs[lhs].set_type(Some(lhs_type));
            if lhs_type == invalid_type {
                return None;
            }
        }

        let t = self.tc_objs.lobjs[lhs].typ();
        self.assignment(x, t, msg, fctx);
        if x.mode != OperandMode::Invalid {
            x.typ
        } else {
            None
        }
    }

    /// Assigns x to the variable denoted by lhs expression.
    pub fn assign_var(&mut self, lhs: &Expr, x: &mut Operand, fctx: &mut FilesContext) -> Option<TypeKey> {
        let invalid_type = self.invalid_type();
        if x.invalid() || x.typ == Some(invalid_type) {
            return None;
        }

        // Check if lhs is a blank identifier
        if let Some(ident) = self.expr_as_ident(lhs) {
            let name = self.resolve_ident(&ident);
            if name == "_" {
                self.result.record_def(ident, None);
                self.assignment(x, None, "assignment to _ identifier", fctx);
                return if x.mode != OperandMode::Invalid {
                    x.typ
                } else {
                    None
                };
            }
        }

        // Evaluate lhs
        let mut z = Operand::new();
        self.expr(&mut z, lhs, fctx);

        if z.mode == OperandMode::Invalid || z.typ == Some(invalid_type) {
            return None;
        }

        // spec: "Each left-hand side operand must be addressable, a map index
        // expression, or the blank identifier."
        match z.mode {
            OperandMode::Variable | OperandMode::MapIndex => {}
            _ => {
                self.error(lhs.span, "cannot assign to expression".to_string());
                return None;
            }
        }

        self.assignment(x, z.typ, "assignment", fctx);
        if x.mode != OperandMode::Invalid {
            x.typ
        } else {
            None
        }
    }

    /// Initializes multiple variables from multiple values.
    pub fn init_vars(&mut self, lhs: &[ObjKey], rhs: &[Expr], fctx: &mut FilesContext) {
        let invalid_type = self.invalid_type();
        
        // Simple case: same number of lhs and rhs
        if lhs.len() == rhs.len() {
            for (i, &l) in lhs.iter().enumerate() {
                let mut x = Operand::new();
                self.expr(&mut x, &rhs[i], fctx);
                self.init_var(l, &mut x, "assignment", fctx);
            }
            return;
        }

        // Handle tuple assignment (single rhs with multiple values)
        if rhs.len() == 1 {
            let mut x = Operand::new();
            self.expr(&mut x, &rhs[0], fctx);
            
            // Check if x is a tuple
            if let Some(typ) = x.typ {
                if let crate::typ::Type::Tuple(tuple) = &self.tc_objs.types[typ] {
                    let vars = tuple.vars().clone();
                    if vars.len() == lhs.len() {
                        for (i, &l) in lhs.iter().enumerate() {
                            let var_type = self.tc_objs.lobjs[vars[i]].typ();
                            let mut elem = Operand::new();
                            elem.mode = OperandMode::Value;
                            elem.typ = var_type;
                            elem.expr_id = x.expr_id;
                            self.init_var(l, &mut elem, "assignment", fctx);
                        }
                        return;
                    }
                }
            }
        }

        // Error: mismatched counts
        self.error(
            rhs[0].span,
            format!("cannot initialize {} variables with {} values", lhs.len(), rhs.len()),
        );
        
        // Set all lhs to invalid
        for &l in lhs {
            if self.tc_objs.lobjs[l].typ().is_none() {
                self.tc_objs.lobjs[l].set_type(Some(invalid_type));
            }
        }
    }

    /// Assigns multiple values to multiple variables.
    pub fn assign_vars(&mut self, lhs: &[Expr], rhs: &[Expr], fctx: &mut FilesContext) {
        // Simple case: same number of lhs and rhs
        if lhs.len() == rhs.len() {
            for (i, l) in lhs.iter().enumerate() {
                let mut x = Operand::new();
                self.expr(&mut x, &rhs[i], fctx);
                self.assign_var(l, &mut x, fctx);
            }
            return;
        }

        // Handle tuple assignment
        if rhs.len() == 1 {
            let mut x = Operand::new();
            self.expr(&mut x, &rhs[0], fctx);
            
            if let Some(typ) = x.typ {
                if let crate::typ::Type::Tuple(tuple) = &self.tc_objs.types[typ] {
                    let vars = tuple.vars().clone();
                    if vars.len() == lhs.len() {
                        for (i, l) in lhs.iter().enumerate() {
                            let var_type = self.tc_objs.lobjs[vars[i]].typ();
                            let mut elem = Operand::new();
                            elem.mode = OperandMode::Value;
                            elem.typ = var_type;
                            elem.expr_id = x.expr_id;
                            self.assign_var(l, &mut elem, fctx);
                        }
                        return;
                    }
                }
            }
        }

        // Error: mismatched counts
        self.error(
            rhs[0].span,
            format!("cannot assign {} values to {} variables", rhs.len(), lhs.len()),
        );
    }

    /// Handles short variable declarations (:=).
    pub fn short_var_decl(&mut self, lhs: &[Expr], rhs: &[Expr], pos: Span, fctx: &mut FilesContext) {
        let scope_key = match self.octx.scope {
            Some(s) => s,
            None => return,
        };

        let mut new_vars = Vec::new();
        let mut lhs_vars = Vec::new();

        for l in lhs {
            if let Some(ident) = self.expr_as_ident(l) {
                let name = self.resolve_ident(&ident).to_string();
                
                // Check if variable already exists in current scope
                if let Some(okey) = self.scope(scope_key).lookup(&name) {
                    self.result.record_use(ident.clone(), okey);
                    if self.tc_objs.lobjs[okey].entity_type().is_var() {
                        lhs_vars.push(okey);
                    } else {
                        self.error(l.span, format!("cannot assign to {}", name));
                        let dummy = self.tc_objs.new_var(0, Some(self.pkg), "_".to_string(), None);
                        lhs_vars.push(dummy);
                    }
                } else {
                    // Declare new variable
                    let okey = self.tc_objs.new_var(0, Some(self.pkg), name.clone(), None);
                    if name != "_" {
                        new_vars.push(okey);
                    }
                    self.result.record_def(ident, Some(okey));
                    lhs_vars.push(okey);
                }
            } else {
                self.error(l.span, "non-name on left side of :=".to_string());
                let dummy = self.tc_objs.new_var(0, Some(self.pkg), "_".to_string(), None);
                lhs_vars.push(dummy);
            }
        }

        self.init_vars(&lhs_vars, rhs, fctx);

        // Declare new variables in scope
        if !new_vars.is_empty() {
            for okey in new_vars {
                self.declare(scope_key, okey);
            }
        } else {
            self.soft_error(pos, "no new variables on left side of :=".to_string());
        }
    }

    /// Converts comma-ok mode to single value mode.
    fn use_single_value(&mut self, x: &mut Operand) {
        if let OperandMode::CommaOk = x.mode {
            x.mode = OperandMode::Value;
        }
    }

    /// Converts an untyped operand to a typed value.
    pub fn convert_untyped(&mut self, x: &mut Operand, target: TypeKey) {
        if x.invalid() {
            return;
        }
        
        let xt = match x.typ {
            Some(t) => t,
            None => return,
        };
        
        if !typ::is_untyped(xt, &self.tc_objs) {
            return;
        }

        // Check if conversion is valid
        if let OperandMode::Constant(ref val) = x.mode {
            // For constants, check representability
            if !self.is_representable(val, target) {
                self.error(DEFAULT_SPAN, "constant not representable".to_string());
                x.mode = OperandMode::Invalid;
                return;
            }
        }

        x.typ = Some(target);
    }

    /// Checks if x is assignable to type t.
    pub fn assignable_to(&self, x: &Operand, t: TypeKey, reason: &mut String) -> bool {
        let xt = match x.typ {
            Some(typ) => typ,
            None => return false,
        };

        // Identical types are always assignable
        if typ::identical(xt, t, &self.tc_objs) {
            return true;
        }

        // Check underlying types
        let xu = typ::underlying_type(xt, &self.tc_objs);
        let tu = typ::underlying_type(t, &self.tc_objs);

        if typ::identical(xu, tu, &self.tc_objs) {
            // Check if either is a defined type
            let x_is_named = !typ::identical(xt, xu, &self.tc_objs);
            let t_is_named = !typ::identical(t, tu, &self.tc_objs);
            if !x_is_named || !t_is_named {
                return true;
            }
        }

        // T is an interface type and x implements T
        if typ::is_interface(t, &self.tc_objs) {
            if self.implements(xt, t) {
                return true;
            }
            *reason = "does not implement interface".to_string();
            return false;
        }

        // x is nil and T is a pointer, function, slice, map, channel, or interface type
        if x.is_nil(&self.tc_objs) {
            if typ::has_nil(t, &self.tc_objs) {
                return true;
            }
        }

        false
    }

    /// Checks if a constant value is representable as the target type.
    fn is_representable(&self, val: &crate::constant::Value, target: TypeKey) -> bool {
        if let crate::typ::Type::Basic(basic) = &self.tc_objs.types[target] {
            return val.representable(basic, None);
        }
        false
    }

    /// Extracts an identifier from an expression if possible.
    fn expr_as_ident(&self, e: &Expr) -> Option<Ident> {
        match &e.kind {
            gox_syntax::ast::ExprKind::Ident(ident) => Some(ident.clone()),
            gox_syntax::ast::ExprKind::Paren(inner) => self.expr_as_ident(inner),
            _ => None,
        }
    }
}
