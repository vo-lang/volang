//! Composite literal typing and canonical constant-key validation.

use super::checker::Checker;
use super::errors::TypeError;
use crate::objects::TypeKey;
use crate::operand::{Operand, OperandMode};
use crate::typ::{self, Type};
use vo_syntax::ast::{CompositeLit, Expr, ExprKind};

impl Checker {
    pub(super) fn composite_literal(
        &mut self,
        x: &mut Operand,
        e: &Expr,
        lit: &CompositeLit,
        hint: Option<TypeKey>,
    ) {
        // Determine composite literal type (aligns with goscript logic)
        let ty = if let Some(ref type_expr) = lit.ty {
            // Composite literal type present - use it
            let t = self.type_expr(type_expr);
            if t == self.invalid_type() {
                x.mode = OperandMode::Invalid;
                return;
            }
            t
        } else {
            // No composite literal type present - use hint (element type of enclosing type)
            if let Some(h) = hint {
                // For nested literals, dereference pointer types like goscript's try_deref
                let base = typ::underlying_type(h, self.objs());
                if let Some(ptr) = self.otype(base).try_as_pointer() {
                    ptr.base()
                } else {
                    h
                }
            } else {
                self.error_code_msg(
                    TypeError::InvalidOp,
                    e.span,
                    "missing type in composite literal",
                );
                x.mode = OperandMode::Invalid;
                return;
            }
        };

        let utype = typ::underlying_type(ty, self.objs());
        let utype_val = self.otype(utype);

        match &utype_val {
            Type::Struct(detail) => {
                let fields = detail.fields().clone();
                let mut keyed_form = None;
                let mut visited_fields = std::collections::HashSet::new();
                // Check elements
                for (i, elem) in lit.elems.iter().enumerate() {
                    let is_keyed = elem.key.is_some();
                    if let Some(expected_keyed) = keyed_form {
                        if expected_keyed != is_keyed {
                            self.error_code_msg(
                                TypeError::InvalidOp,
                                elem.span,
                                "mixture of keyed and unkeyed elements in struct literal",
                            );
                        }
                    } else {
                        keyed_form = Some(is_keyed);
                    }

                    let field = match &elem.key {
                        Some(Expr {
                            kind: ExprKind::Ident(ident),
                            ..
                        }) => {
                            let name = self.resolve_ident(ident).to_string();
                            let field = fields
                                .iter()
                                .copied()
                                .find(|&field| self.lobj(field).name() == name);
                            if let Some(field) = field {
                                if !visited_fields.insert(field) {
                                    self.error_code_msg(
                                        TypeError::InvalidOp,
                                        ident.span,
                                        format!("duplicate field {} in struct literal", name),
                                    );
                                }
                            } else {
                                self.error_code_msg(
                                    TypeError::InvalidOp,
                                    ident.span,
                                    format!("unknown field {} in struct literal", name),
                                );
                            }
                            field
                        }
                        Some(key) => {
                            self.error_code_msg(
                                TypeError::InvalidOp,
                                key.span,
                                "struct literal field name must be an identifier",
                            );
                            None
                        }
                        None => {
                            let field = fields.get(i).copied();
                            if field.is_none() {
                                self.error_code_msg(
                                    TypeError::InvalidOp,
                                    elem.span,
                                    "too many values in struct literal",
                                );
                            }
                            field
                        }
                    };

                    let field_type = field.and_then(|field| {
                        let field_obj = self.lobj(field);
                        if field_obj.pkg().is_some_and(|pkg| pkg != self.pkg)
                            && !field_obj.exported()
                        {
                            self.error_code_msg(
                                TypeError::InvalidOp,
                                elem.span,
                                format!(
                                    "cannot refer to unexported field {} in struct literal",
                                    field_obj.name()
                                ),
                            );
                        }
                        field_obj.typ()
                    });

                    let mut val = Operand::new();
                    self.expr_with_hint(&mut val, &elem.value, field_type);
                    if let Some(ft) = field_type.filter(|_| !val.invalid()) {
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
                let mut constant_keys = std::collections::HashSet::new();
                for elem in &lit.elems {
                    let mut key_op = Operand::new();
                    let key_span = match &elem.key {
                        Some(key_expr) => {
                            self.expr(&mut key_op, key_expr);
                            Some(key_expr.span)
                        }
                        None => {
                            self.error_code_msg(
                                TypeError::InvalidOp,
                                elem.span,
                                "missing key in map literal",
                            );
                            None
                        }
                    };
                    if !key_op.invalid() {
                        self.assignment(&mut key_op, Some(key_type), "map literal key");
                    }
                    if let (Some(span), OperandMode::Constant(value)) = (key_span, &key_op.mode) {
                        // Interface equality includes the dynamic type. For a
                        // concrete key, assignment has already normalized its value.
                        let dynamic_type =
                            typ::is_interface(key_type, self.objs()).then_some(key_op.typ.unwrap());
                        if let Some(key) = value.equality_key() {
                            if !constant_keys.insert((dynamic_type, key)) {
                                self.error_code_msg(
                                    TypeError::InvalidOp,
                                    span,
                                    format!("duplicate key {} in map literal", value),
                                );
                            }
                        }
                    }
                    // Check value (with hint for nested composite literals)
                    let mut val = Operand::new();
                    self.expr_with_hint(&mut val, &elem.value, Some(elem_type));
                    if !val.invalid() {
                        self.assignment(&mut val, Some(elem_type), "map literal value");
                    }
                }
            }
            _ => {
                self.invalid_op(e.span, "invalid composite literal type");
                x.mode = OperandMode::Invalid;
                return;
            }
        }

        x.mode = OperandMode::Value;
        x.typ = Some(ty);
    }
}
