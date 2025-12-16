//! Expression type checking.
//!
//! This module handles type checking for all expression forms:
//! - Literals (int, float, string, rune)
//! - Identifiers
//! - Binary and unary operations
//! - Function calls and type conversions
//! - Index and selector expressions
//! - Composite literals
//! - Type assertions and slice expressions

use gox_common::{Ident, Span, Symbol};
use gox_syntax::ast::{self, BinaryOp, Expr, ExprKind, UnaryOp};

use crate::errors::TypeError;
use crate::lookup::{Lookup, LookupResult};
use crate::scope::{Entity, ScopeKind};
use crate::types::{BasicType, FuncType, NamedTypeId, Type, UntypedKind};

use super::{TypeChecker, LOCAL_TYPE_ID_OFFSET};

impl<'a> TypeChecker<'a> {
    /// Type-checks an expression and returns its type.
    /// Also records the type for later use by codegen.
    pub fn check_expr(&mut self, expr: &Expr) -> Type {
        let ty = match &expr.kind {
            ExprKind::IntLit(lit) => self.check_int_lit(lit),
            ExprKind::FloatLit(lit) => self.check_float_lit(lit),
            ExprKind::StringLit(lit) => self.check_string_lit(lit),
            ExprKind::RuneLit(lit) => self.check_rune_lit(lit),
            ExprKind::Ident(ident) => self.check_ident(ident),
            ExprKind::Binary(bin) => self.check_binary(bin, expr.span),
            ExprKind::Unary(un) => self.check_unary(un, expr.span),
            ExprKind::Call(call) => self.check_call(call, expr.span),
            ExprKind::Index(idx) => self.check_index(idx, expr.span),
            ExprKind::Selector(sel) => self.check_selector(sel, expr.span),
            ExprKind::Paren(inner) => self.check_expr(inner),
            // Note: nil is handled as an identifier in GoX
            ExprKind::FuncLit(func) => self.check_func_lit(func),
            ExprKind::CompositeLit(lit) => self.check_composite_lit(lit, expr.span),
            ExprKind::TypeAssert(ta) => self.check_type_assert(ta, expr.span),
            ExprKind::Slice(sl) => self.check_slice_expr(sl, expr.span),
            ExprKind::Receive(recv) => self.check_receive(recv.as_ref(), expr.span),
            // Type used as expression (for make/new first argument)
            ExprKind::TypeAsExpr(ty) => self.resolve_type_expr(ty),
            _ => Type::Invalid,
        };
        // Record the type for codegen to look up later
        self.record_expr_type(expr, ty.clone());
        ty
    }

    // ========== Literal checking ==========

    /// Checks an integer literal.
    fn check_int_lit(&self, _lit: &ast::IntLit) -> Type {
        Type::Untyped(UntypedKind::Int)
    }

    /// Checks a float literal.
    fn check_float_lit(&self, _lit: &ast::FloatLit) -> Type {
        Type::Untyped(UntypedKind::Float)
    }

    /// Checks a string literal.
    fn check_string_lit(&self, _lit: &ast::StringLit) -> Type {
        Type::Untyped(UntypedKind::String)
    }

    /// Checks a rune literal.
    fn check_rune_lit(&self, _lit: &ast::RuneLit) -> Type {
        Type::Untyped(UntypedKind::Rune)
    }

    // ========== Identifier checking ==========

    /// Checks an identifier expression.
    fn check_ident(&mut self, ident: &Ident) -> Type {
        // Blank identifier is always valid and has no type
        let name = self.interner.resolve(ident.symbol).unwrap_or("");
        if name == "_" {
            return Type::Invalid; // Blank identifier - always valid
        }

        match self.lookup(ident.symbol) {
            Some(Entity::Var(v)) => v.ty.clone(),
            Some(Entity::Func(f)) => Type::Func(f.sig.clone()),
            Some(Entity::Type(_)) => {
                let name = self.interner.resolve(ident.symbol).unwrap_or("<unknown>");
                self.error_msg(
                    TypeError::TypeNotValue,
                    ident.span,
                    TypeError::TypeNotValue.with_name(name),
                );
                Type::Invalid
            }
            Some(Entity::Builtin(_b)) => {
                // Built-in functions have special handling
                Type::Invalid // Will be handled in call context
            }
            None => {
                let name = self.interner.resolve(ident.symbol).unwrap_or("<unknown>");
                self.error_msg(
                    TypeError::Undefined,
                    ident.span,
                    TypeError::Undefined.with_name(name),
                );
                Type::Invalid
            }
            _ => Type::Invalid,
        }
    }

    // ========== Binary operations ==========

    /// Checks a binary expression.
    fn check_binary(&mut self, bin: &ast::BinaryExpr, span: Span) -> Type {
        let left_ty = self.check_expr(&bin.left);
        let right_ty = self.check_expr(&bin.right);

        match bin.op {
            // Arithmetic operators
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                self.check_arithmetic_op(&left_ty, &right_ty, bin.op, span)
            }
            // Comparison operators
            BinaryOp::Eq | BinaryOp::NotEq => self.check_equality_op(&left_ty, &right_ty, span),
            BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => {
                self.check_ordering_op(&left_ty, &right_ty, span)
            }
            // Logical operators
            BinaryOp::LogAnd | BinaryOp::LogOr => self.check_logical_op(&left_ty, &right_ty, span),
            // Bitwise operators
            BinaryOp::And | BinaryOp::Or | BinaryOp::Xor | BinaryOp::AndNot => {
                self.check_bitwise_op(&left_ty, &right_ty, span)
            }
            // Shift operators
            BinaryOp::Shl | BinaryOp::Shr => self.check_shift_op(&left_ty, &right_ty, span),
        }
    }

    /// Checks arithmetic operators (+, -, *, /, %).
    fn check_arithmetic_op(&mut self, left: &Type, right: &Type, op: BinaryOp, span: Span) -> Type {
        // String concatenation
        if op == BinaryOp::Add && self.is_string_type(left) && self.is_string_type(right) {
            return self.common_type(left, right);
        }

        // Numeric operations
        if self.is_numeric_type(left) && self.is_numeric_type(right) {
            return self.common_type(left, right);
        }

        self.error(TypeError::NumericOperandRequired, span);
        Type::Invalid
    }

    /// Checks equality operators (==, !=).
    fn check_equality_op(&mut self, left: &Type, right: &Type, span: Span) -> Type {
        if self.are_comparable(left, right) {
            return Type::Untyped(UntypedKind::Bool);
        }

        self.error(TypeError::NotComparable, span);
        Type::Untyped(UntypedKind::Bool)
    }

    /// Checks ordering operators (<, <=, >, >=).
    fn check_ordering_op(&mut self, left: &Type, right: &Type, span: Span) -> Type {
        if self.is_ordered_type(left) && self.is_ordered_type(right) {
            return Type::Untyped(UntypedKind::Bool);
        }

        self.error(TypeError::OrderedOperandRequired, span);
        Type::Untyped(UntypedKind::Bool)
    }

    /// Checks logical operators (&&, ||).
    fn check_logical_op(&mut self, left: &Type, right: &Type, span: Span) -> Type {
        if self.is_bool_type(left) && self.is_bool_type(right) {
            return Type::Untyped(UntypedKind::Bool);
        }

        self.error(TypeError::BooleanOperandRequired, span);
        Type::Untyped(UntypedKind::Bool)
    }

    /// Checks bitwise operators (&, |, ^, &^).
    fn check_bitwise_op(&mut self, left: &Type, right: &Type, span: Span) -> Type {
        if self.is_integer_type(left) && self.is_integer_type(right) {
            return self.common_type(left, right);
        }

        self.error(TypeError::IntegerOperandRequired, span);
        Type::Invalid
    }

    /// Checks shift operators (<<, >>).
    fn check_shift_op(&mut self, left: &Type, right: &Type, span: Span) -> Type {
        // Left must be integer
        if !self.is_integer_type(left) {
            self.error(TypeError::ShiftOperandInteger, span);
            return Type::Invalid;
        }

        // Right must be unsigned or untyped int
        if !self.is_unsigned_or_untyped_int(right) {
            self.error(TypeError::ShiftCountUnsigned, span);
        }

        left.clone()
    }

    // ========== Unary operations ==========

    /// Checks a unary expression.
    fn check_unary(&mut self, un: &ast::UnaryExpr, span: Span) -> Type {
        let operand_ty = self.check_expr(&un.operand);

        match un.op {
            UnaryOp::Pos => {
                if self.is_numeric_type(&operand_ty) {
                    operand_ty
                } else {
                    self.error(TypeError::UnaryPosNumeric, span);
                    Type::Invalid
                }
            }
            UnaryOp::Neg => {
                if self.is_numeric_type(&operand_ty) {
                    operand_ty
                } else {
                    self.error(TypeError::UnaryNegNumeric, span);
                    Type::Invalid
                }
            }
            UnaryOp::Not => {
                if self.is_bool_type(&operand_ty) {
                    Type::Untyped(UntypedKind::Bool)
                } else {
                    self.error(TypeError::UnaryNotBoolean, span);
                    Type::Untyped(UntypedKind::Bool)
                }
            }
            UnaryOp::BitNot => {
                if self.is_integer_type(&operand_ty) {
                    operand_ty
                } else {
                    self.error(TypeError::UnaryBitNotInteger, span);
                    Type::Invalid
                }
            }
        }
    }

    // ========== Call expressions ==========

    /// Checks a function call expression.
    fn check_call(&mut self, call: &ast::CallExpr, span: Span) -> Type {
        // Check for built-in function calls
        if let ExprKind::Ident(ident) = &call.func.kind {
            // Check for built-in
            if let Some(Entity::Builtin(kind)) = self.lookup(ident.symbol) {
                let kind = *kind;
                return self.check_builtin_call(kind, &call.args, span);
            }

            // Check for type conversion (basic type)
            if let Some(basic) = self.resolve_basic_type_name(ident.symbol) {
                return self.check_basic_type_conversion(basic, &call.args, span);
            }

            // Check for type conversion (named type)
            if let Some(Entity::Type(t)) = self.lookup(ident.symbol) {
                let type_id = t.id;
                return self.check_type_conversion(type_id, &call.args, span);
            }
        }

        // Regular function call
        let func_ty = self.check_expr(&call.func);
        match self.underlying_type(&func_ty) {
            Type::Func(sig) => self.check_func_call(&sig, &call.args, span),
            _ if func_ty != Type::Invalid => {
                self.error(TypeError::NotCallable, span);
                Type::Invalid
            }
            _ => Type::Invalid,
        }
    }

    /// Checks a type conversion call on a named type.
    fn check_type_conversion(&mut self, target_id: NamedTypeId, args: &[Expr], span: Span) -> Type {
        if args.len() != 1 {
            self.error(TypeError::WrongArgCount, span);
            return Type::Named(target_id);
        }

        let arg_ty = self.check_expr(&args[0]);
        let target_ty = Type::Named(target_id);

        // Check if the conversion is valid
        if !self.is_convertible(&arg_ty, &target_ty) {
            self.error(TypeError::CannotConvert, span);
        }

        target_ty
    }

    /// Resolves a basic type name to its BasicType.
    fn resolve_basic_type_name(&self, symbol: Symbol) -> Option<BasicType> {
        let name = self.interner.resolve(symbol)?;
        match name {
            "bool" => Some(BasicType::Bool),
            "int" => Some(BasicType::Int),
            "int8" => Some(BasicType::Int8),
            "int16" => Some(BasicType::Int16),
            "int32" | "rune" => Some(BasicType::Int32),
            "int64" => Some(BasicType::Int64),
            "uint" => Some(BasicType::Uint),
            "uint8" | "byte" => Some(BasicType::Uint8),
            "uint16" => Some(BasicType::Uint16),
            "uint32" => Some(BasicType::Uint32),
            "uint64" => Some(BasicType::Uint64),
            "float32" => Some(BasicType::Float32),
            "float64" => Some(BasicType::Float64),
            "string" => Some(BasicType::String),
            _ => None,
        }
    }

    /// Checks a type conversion call to a basic type.
    fn check_basic_type_conversion(
        &mut self,
        target: BasicType,
        args: &[Expr],
        span: Span,
    ) -> Type {
        if args.len() != 1 {
            self.error(TypeError::WrongArgCount, span);
            return Type::Basic(target);
        }

        let arg_ty = self.check_expr(&args[0]);
        let target_ty = Type::Basic(target);

        // Check if conversion is valid
        if !self.is_basic_convertible(&arg_ty, target) {
            self.error(TypeError::CannotConvert, span);
        }

        target_ty
    }

    /// Checks if a type can be converted to a basic type.
    fn is_basic_convertible(&self, from: &Type, to: BasicType) -> bool {
        let from_u = self.underlying_type(from);
        match from_u {
            Type::Basic(b) => {
                // Numeric conversions
                if b.is_numeric() && to.is_numeric() {
                    return true;
                }
                // String conversions
                if b == BasicType::String && to == BasicType::String {
                    return true;
                }
                // int <-> rune
                if b.is_integer() && to.is_integer() {
                    return true;
                }
                // int/rune -> string (converts to UTF-8 character)
                if b.is_integer() && to == BasicType::String {
                    return true;
                }
                b == to
            }
            Type::Untyped(k) => match k {
                UntypedKind::Bool => to == BasicType::Bool,
                UntypedKind::Int | UntypedKind::Rune => {
                    to.is_integer() || to.is_float() || to == BasicType::String
                }
                UntypedKind::Float => to.is_float(),
                UntypedKind::String => to == BasicType::String,
            },
            // String to []byte / []rune
            Type::Slice(s) => {
                if to == BasicType::String {
                    matches!(
                        *s.elem,
                        Type::Basic(BasicType::Uint8) | Type::Basic(BasicType::Int32)
                    )
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Checks if a type can be converted to another type.
    pub(crate) fn is_convertible(&self, from: &Type, to: &Type) -> bool {
        // Same type
        if from == to {
            return true;
        }

        // Assignable
        if self.is_assignable(from, to) {
            return true;
        }

        let from_u = self.underlying_type(from);
        let to_u = self.underlying_type(to);

        // Same underlying type (ignoring named type wrapper)
        if from_u == to_u {
            return true;
        }

        // Numeric conversions
        match (&from_u, &to_u) {
            (Type::Basic(bf), Type::Basic(bt)) => {
                if bf.is_numeric() && bt.is_numeric() {
                    return true;
                }
            }
            (Type::Untyped(k), Type::Basic(bt)) => {
                return match k {
                    UntypedKind::Bool => *bt == BasicType::Bool,
                    UntypedKind::Int | UntypedKind::Rune => bt.is_integer() || bt.is_float(),
                    UntypedKind::Float => bt.is_float(),
                    UntypedKind::String => *bt == BasicType::String,
                };
            }
            // String <-> []byte
            (Type::Basic(BasicType::String), Type::Slice(s)) => {
                if let Type::Basic(BasicType::Uint8) = *s.elem {
                    return true;
                }
            }
            (Type::Slice(s), Type::Basic(BasicType::String)) => {
                if let Type::Basic(BasicType::Uint8) = *s.elem {
                    return true;
                }
            }
            _ => {}
        }

        false
    }

    /// Checks function call arguments against signature.
    fn check_func_call(&mut self, sig: &FuncType, args: &[Expr], span: Span) -> Type {
        // Check argument count
        let expected = sig.params.len();
        let got = args.len();

        if sig.variadic {
            if got < expected - 1 {
                self.error(TypeError::WrongArgCount, span);
            }
        } else if got != expected {
            self.error(TypeError::WrongArgCount, span);
        }

        // Check argument types
        for (i, arg) in args.iter().enumerate() {
            let arg_ty = self.check_expr(arg);

            // Get expected type (for variadic, use last param type for extra args)
            let param_idx = if sig.variadic && i >= expected - 1 {
                expected - 1
            } else {
                i
            };

            if param_idx < sig.params.len() {
                let param_ty = &sig.params[param_idx];
                // For variadic param, unwrap the slice type
                let expected_ty = if sig.variadic && param_idx == expected - 1 {
                    if let Type::Slice(s) = param_ty {
                        &s.elem
                    } else {
                        param_ty
                    }
                } else {
                    param_ty
                };

                if !self.is_assignable(&arg_ty, expected_ty) {
                    self.error(TypeError::ArgTypeMismatch, arg.span);
                }
            }
        }

        // Return type
        match sig.results.len() {
            0 => Type::Tuple(vec![]),
            1 => sig.results[0].clone(),
            _ => Type::Tuple(sig.results.clone()),
        }
    }

    // ========== Index and selector expressions ==========

    /// Checks an index expression.
    fn check_index(&mut self, idx: &ast::IndexExpr, span: Span) -> Type {
        let base_ty = self.check_expr(&idx.expr);
        let index_ty = self.check_expr(&idx.index);

        match self.underlying_type(&base_ty) {
            Type::Array(arr) => {
                if !self.is_integer_type(&index_ty) {
                    self.error(TypeError::IndexNotInteger, idx.index.span);
                }
                (*arr.elem).clone()
            }
            Type::Slice(sl) => {
                if !self.is_integer_type(&index_ty) {
                    self.error(TypeError::IndexNotInteger, idx.index.span);
                }
                (*sl.elem).clone()
            }
            Type::Map(m) => {
                if !self.is_assignable(&index_ty, &m.key) {
                    self.error(TypeError::KeyTypeMismatch, idx.index.span);
                }
                (*m.value).clone()
            }
            Type::Basic(BasicType::String) => {
                if !self.is_integer_type(&index_ty) {
                    self.error(TypeError::IndexNotInteger, idx.index.span);
                }
                Type::Basic(BasicType::Uint8)
            }
            _ => {
                self.error(TypeError::NotIndexable, span);
                Type::Invalid
            }
        }
    }

    /// Checks a selector expression (field or method access).
    fn check_selector(&mut self, sel: &ast::SelectorExpr, _span: Span) -> Type {
        // Check for cross-package call (pkg.Func)
        if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
            let pkg_name = self.interner.resolve(pkg_ident.symbol).unwrap_or("");
            if self.imported_packages.contains(pkg_name) {
                // This is a cross-package call
                let field_name = self.interner.resolve(sel.sel.symbol).unwrap_or("");
                if let Some(pkg_exports) = self.package_exports.get(pkg_name) {
                    if let Some(ty) = pkg_exports.get(field_name) {
                        return ty.clone();
                    }
                }
                // Exported symbol not found in package
                self.error_msg(
                    TypeError::Undefined,
                    sel.sel.span,
                    format!("undefined: {}.{}", pkg_name, field_name),
                );
                return Type::Invalid;
            }
        }

        let base_ty = self.check_expr(&sel.expr);
        let field_name = sel.sel.symbol;
        let field_name_str = self.interner.resolve(field_name).unwrap_or("<unknown>");

        // Check if base_ty is a cross-package type and look up method from exported types
        if let Some(method_ty) = self.lookup_cross_package_method(&base_ty, field_name_str) {
            return method_ty;
        }

        // Resolve local types to their underlying type for lookup
        let lookup_ty = self.resolve_for_lookup(&base_ty);

        // Use Lookup for field/method access
        let lookup = Lookup::new(self.named_types);
        match lookup.lookup_field_or_method(&lookup_ty, field_name) {
            LookupResult::Field(ty, _indices, _indirect) => ty,
            LookupResult::Method(sig, _indices, _indirect) => Type::Func(sig),
            LookupResult::Ambiguous(_indices) => {
                self.error_msg(
                    TypeError::NoFieldOrMethod,
                    sel.sel.span,
                    format!("ambiguous selector {}", field_name_str),
                );
                Type::Invalid
            }
            LookupResult::NotFound => {
                self.error_msg(
                    TypeError::NoFieldOrMethod,
                    sel.sel.span,
                    TypeError::NoFieldOrMethod.with_name(field_name_str),
                );
                Type::Invalid
            }
        }
    }

    /// Look up a method on a cross-package type.
    ///
    /// This handles the case where a variable's type is from another package.
    /// Since named_types is per-package, we can't rely on the NamedTypeId to look up
    /// the type name. Instead, we search all exported types for the method.
    fn lookup_cross_package_method(&self, _base_ty: &Type, method_name: &str) -> Option<Type> {
        // Search all imported packages' exported types for this method
        // This is a fallback when the normal Lookup doesn't find the method
        for (_pkg_name, pkg_types) in &self.package_exported_types {
            for (_type_name, type_methods) in pkg_types {
                if let Some(method_ty) = type_methods.get(method_name) {
                    return Some(method_ty.clone());
                }
            }
        }

        None
    }

    // ========== Composite literals ==========

    /// Checks a composite literal.
    fn check_composite_lit(&mut self, lit: &ast::CompositeLit, _span: Span) -> Type {
        // Resolve the literal's type
        let lit_ty = self.resolve_type_expr(&lit.ty);

        // Check elements based on the type
        match self.underlying_type(&lit_ty) {
            Type::Struct(s) | Type::Obx(s) => {
                self.check_struct_lit_elems(&lit.elems, &s);
            }
            Type::Array(arr) => {
                self.check_array_lit_elems(&lit.elems, &arr.elem);
            }
            Type::Slice(sl) => {
                self.check_slice_lit_elems(&lit.elems, &sl.elem);
            }
            Type::Map(m) => {
                self.check_map_lit_elems(&lit.elems, &m.key, &m.value);
            }
            Type::Invalid => {
                // Type resolution failed, already reported
            }
            _ => {
                self.error(TypeError::InvalidCompositeLitType, _span);
            }
        }

        lit_ty
    }

    /// Checks struct literal elements.
    fn check_struct_lit_elems(
        &mut self,
        elems: &[ast::CompositeLitElem],
        s: &crate::types::StructType,
    ) {
        for elem in elems {
            // Check value expression
            let value_ty = self.check_expr(&elem.value);

            // If key is specified, find the field
            if let Some(ref key) = elem.key {
                if let ast::CompositeLitKey::Ident(field_name) = key {
                    // Find the field in the struct
                    let field = s.fields.iter().find(|f| f.name == Some(field_name.symbol));
                    if let Some(f) = field {
                        if !self.is_assignable(&value_ty, &f.ty) {
                            self.error(TypeError::FieldTypeMismatch, elem.value.span);
                        }
                    } else {
                        let name = self
                            .interner
                            .resolve(field_name.symbol)
                            .unwrap_or("<unknown>");
                        self.error_msg(
                            TypeError::UnknownField,
                            field_name.span,
                            TypeError::UnknownField.with_name(name),
                        );
                    }
                }
            }
        }
    }

    /// Checks array literal elements.
    fn check_array_lit_elems(&mut self, elems: &[ast::CompositeLitElem], elem_ty: &Type) {
        for elem in elems {
            let value_ty = self.check_expr(&elem.value);
            if !self.is_assignable(&value_ty, elem_ty) {
                self.error(TypeError::ElementTypeMismatch, elem.value.span);
            }
        }
    }

    /// Checks slice literal elements.
    fn check_slice_lit_elems(&mut self, elems: &[ast::CompositeLitElem], elem_ty: &Type) {
        for elem in elems {
            let value_ty = self.check_expr(&elem.value);
            if !self.is_assignable(&value_ty, elem_ty) {
                self.error(TypeError::ElementTypeMismatch, elem.value.span);
            }
        }
    }

    /// Checks map literal elements.
    fn check_map_lit_elems(
        &mut self,
        elems: &[ast::CompositeLitElem],
        key_ty: &Type,
        value_ty: &Type,
    ) {
        for elem in elems {
            // Check key
            if let Some(ref key) = elem.key {
                if let ast::CompositeLitKey::Expr(key_expr) = key {
                    let k_ty = self.check_expr(key_expr);
                    if !self.is_assignable(&k_ty, key_ty) {
                        self.error(TypeError::KeyTypeMismatch, key_expr.span);
                    }
                }
            } else {
                self.error(TypeError::KeyTypeMismatch, elem.value.span);
            }

            // Check value
            let v_ty = self.check_expr(&elem.value);
            if !self.is_assignable(&v_ty, value_ty) {
                self.error(TypeError::ElementTypeMismatch, elem.value.span);
            }
        }
    }

    // ========== Type assertion and slice ==========

    /// Checks a type assertion.
    fn check_type_assert(&mut self, ta: &ast::TypeAssertExpr, span: Span) -> Type {
        let expr_ty = self.check_expr(&ta.expr);

        // Expression must be interface type
        if !self.is_interface_type(&expr_ty) {
            self.error(TypeError::TypeAssertNonInterface, span);
            return Type::Invalid;
        }

        // Resolve the asserted type
        match &ta.ty {
            Some(ty_expr) => {
                let asserted_ty = self.resolve_type_expr(ty_expr);

                // Check if the asserted type could possibly implement the interface
                // (This is a compile-time check - runtime check happens at execution)
                if let Type::Interface(iface) = self.underlying_type(&expr_ty) {
                    if self
                        .registry
                        .implements_interface(&asserted_ty, &iface)
                        .is_err()
                    {
                        // This is just a warning - the assertion might still be valid
                        // if the concrete type at runtime implements the interface
                    }
                }

                asserted_ty
            }
            None => {
                // x.(type) - used in type switch, returns the dynamic type
                // This is handled specially in type switch context
                Type::Invalid
            }
        }
    }

    /// Checks a slice expression.
    fn check_slice_expr(&mut self, sl: &ast::SliceExpr, span: Span) -> Type {
        let base_ty = self.check_expr(&sl.expr);

        // Check indices are integers
        if let Some(ref low) = sl.low {
            let low_ty = self.check_expr(low);
            if !self.is_integer_type(&low_ty) {
                self.error(TypeError::SliceIndexNotInteger, low.span);
            }
        }
        if let Some(ref high) = sl.high {
            let high_ty = self.check_expr(high);
            if !self.is_integer_type(&high_ty) {
                self.error(TypeError::SliceIndexNotInteger, high.span);
            }
        }

        match self.underlying_type(&base_ty) {
            Type::Array(arr) => Type::Slice(crate::types::SliceType {
                elem: arr.elem.clone(),
            }),
            Type::Slice(sl) => Type::Slice(sl.clone()),
            Type::Basic(BasicType::String) => Type::Basic(BasicType::String),
            _ => {
                self.error(TypeError::NotSliceable, span);
                Type::Invalid
            }
        }
    }

    /// Checks a receive expression.
    fn check_receive(&mut self, recv: &Expr, span: Span) -> Type {
        let chan_ty = self.check_expr(recv);

        match self.underlying_type(&chan_ty) {
            Type::Chan(c) => {
                if c.dir == crate::types::ChanDir::SendOnly {
                    self.error(TypeError::ReceiveFromSendOnly, span);
                }
                (*c.elem).clone()
            }
            _ => {
                self.error(TypeError::ReceiveNonChannel, span);
                Type::Invalid
            }
        }
    }

    // ========== Function literal ==========

    /// Checks a function literal.
    fn check_func_lit(&mut self, func: &ast::FuncLit) -> Type {
        // Push function scope
        self.push_scope(ScopeKind::Function);

        // Add parameters to scope
        for param in &func.sig.params {
            let param_ty = self.resolve_type_expr(&param.ty);
            for name in &param.names {
                self.define_var(name.symbol, param_ty.clone(), name.span);
            }
        }

        // Set expected return types
        let old_return_types = std::mem::take(&mut self.return_types);
        self.return_types = func
            .sig
            .results
            .iter()
            .map(|r| self.resolve_type_expr(&r.ty))
            .collect();

        // Check function body
        self.check_block(&func.body);

        // Restore return types
        self.return_types = old_return_types;

        // Pop function scope
        self.pop_scope();

        // Build the function type
        let params: Vec<Type> = func
            .sig
            .params
            .iter()
            .flat_map(|p| {
                let ty = self.resolve_type_expr(&p.ty);
                std::iter::repeat_n(ty, p.names.len().max(1))
            })
            .collect();

        let results: Vec<Type> = func
            .sig
            .results
            .iter()
            .map(|r| self.resolve_type_expr(&r.ty))
            .collect();

        Type::Func(FuncType {
            params,
            results,
            variadic: func.sig.variadic,
        })
    }

    // ========== Type predicates ==========

    /// Returns the underlying type, resolving named types.
    pub(crate) fn underlying_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Named(id) => {
                // Check if this is a local type (high offset)
                if id.0 >= LOCAL_TYPE_ID_OFFSET {
                    let local_idx = (id.0 - LOCAL_TYPE_ID_OFFSET) as usize;
                    if let Some(info) = self.local_types.get(local_idx) {
                        return self.underlying_type(&info.underlying);
                    }
                    return Type::Invalid;
                }
                // Package-level type
                if let Some(info) = self.named_types.get(id.0 as usize) {
                    self.underlying_type(&info.underlying)
                } else {
                    Type::Invalid
                }
            }
            _ => ty.clone(),
        }
    }

    /// Resolves a type for lookup purposes, converting local types to their underlying type.
    pub(crate) fn resolve_for_lookup(&self, ty: &Type) -> Type {
        match ty {
            Type::Named(id) if id.0 >= LOCAL_TYPE_ID_OFFSET => {
                // Local type - return underlying type for lookup
                let local_idx = (id.0 - LOCAL_TYPE_ID_OFFSET) as usize;
                if let Some(info) = self.local_types.get(local_idx) {
                    info.underlying.clone()
                } else {
                    Type::Invalid
                }
            }
            _ => ty.clone(),
        }
    }

    /// Checks if a type is numeric.
    pub(crate) fn is_numeric_type(&self, ty: &Type) -> bool {
        match self.underlying_type(ty) {
            Type::Basic(b) => b.is_numeric(),
            Type::Untyped(k) => {
                matches!(k, UntypedKind::Int | UntypedKind::Float | UntypedKind::Rune)
            }
            _ => false,
        }
    }

    /// Checks if a type is an integer type.
    pub(crate) fn is_integer_type(&self, ty: &Type) -> bool {
        match self.underlying_type(ty) {
            Type::Basic(b) => b.is_integer(),
            Type::Untyped(k) => matches!(k, UntypedKind::Int | UntypedKind::Rune),
            _ => false,
        }
    }

    /// Checks if a type is a string type.
    pub(crate) fn is_string_type(&self, ty: &Type) -> bool {
        match self.underlying_type(ty) {
            Type::Basic(BasicType::String) => true,
            Type::Untyped(UntypedKind::String) => true,
            _ => false,
        }
    }

    /// Checks if a type is a boolean type.
    pub(crate) fn is_bool_type(&self, ty: &Type) -> bool {
        match self.underlying_type(ty) {
            Type::Basic(BasicType::Bool) => true,
            Type::Untyped(UntypedKind::Bool) => true,
            _ => false,
        }
    }

    /// Checks if a type is ordered (supports <, <=, >, >=).
    pub(crate) fn is_ordered_type(&self, ty: &Type) -> bool {
        match self.underlying_type(ty) {
            Type::Basic(b) => b.is_numeric() || b == BasicType::String,
            Type::Untyped(k) => matches!(
                k,
                UntypedKind::Int | UntypedKind::Float | UntypedKind::Rune | UntypedKind::String
            ),
            _ => false,
        }
    }

    /// Checks if a type is an interface type.
    pub(crate) fn is_interface_type(&self, ty: &Type) -> bool {
        match self.underlying_type(ty) {
            Type::Interface(_) => true,
            _ => false,
        }
    }

    /// Checks if a type is unsigned integer or untyped int.
    pub(crate) fn is_unsigned_or_untyped_int(&self, ty: &Type) -> bool {
        match self.underlying_type(ty) {
            Type::Basic(b) => b.is_unsigned(),
            Type::Untyped(UntypedKind::Int) => true,
            _ => false,
        }
    }

    /// Checks if two types are comparable.
    pub(crate) fn are_comparable(&self, left: &Type, right: &Type) -> bool {
        // Nil is comparable to object types
        if left.is_nil() || right.is_nil() {
            return self.is_object_type(left)
                || self.is_object_type(right)
                || left.is_nil()
                || right.is_nil();
        }

        let _left_u = self.underlying_type(left);
        let _right_u = self.underlying_type(right);

        // Same type or assignable
        self.is_assignable(left, right) || self.is_assignable(right, left)
    }

    /// Checks if a type is an object type (reference semantics).
    pub(crate) fn is_object_type(&self, ty: &Type) -> bool {
        match self.underlying_type(ty) {
            Type::Slice(_)
            | Type::Map(_)
            | Type::Chan(_)
            | Type::Func(_)
            | Type::Obx(_)
            | Type::Interface(_) => true,
            _ => false,
        }
    }

    /// Checks if a value of type `from` can be assigned to type `to`.
    pub(crate) fn is_assignable(&self, from: &Type, to: &Type) -> bool {
        // Identical types
        if from == to {
            return true;
        }

        // Nil can be assigned to object types
        if from.is_nil() && self.is_object_type(to) {
            return true;
        }

        // Untyped constants can be assigned if representable
        if let Type::Untyped(kind) = from {
            return self.untyped_assignable_to(*kind, to);
        }

        // Interface assignment
        if let Type::Interface(iface) = self.underlying_type(to) {
            return self.registry.implements_interface(from, &iface).is_ok();
        }

        // Same underlying type for non-named types
        let from_u = self.underlying_type(from);
        let to_u = self.underlying_type(to);

        // If neither is named, compare underlying
        if !matches!(from, Type::Named(_)) && !matches!(to, Type::Named(_)) {
            return from_u == to_u;
        }

        false
    }

    /// Checks if an untyped constant kind can be assigned to a type.
    pub(crate) fn untyped_assignable_to(&self, kind: UntypedKind, to: &Type) -> bool {
        match self.underlying_type(to) {
            Type::Basic(b) => match kind {
                UntypedKind::Bool => b == BasicType::Bool,
                UntypedKind::Int | UntypedKind::Rune => b.is_integer(),
                UntypedKind::Float => b.is_numeric(),
                UntypedKind::String => b == BasicType::String,
            },
            Type::Untyped(k) => kind.precedence() <= k.precedence(),
            // Any untyped value can be assigned to an empty interface
            Type::Interface(iface) => iface.methods.is_empty() && iface.embeds.is_empty(),
            _ => false,
        }
    }

    /// Returns the default type for an untyped constant.
    pub(crate) fn default_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Untyped(kind) => match kind {
                UntypedKind::Bool => Type::Basic(BasicType::Bool),
                UntypedKind::Int => Type::Basic(BasicType::Int),
                UntypedKind::Rune => Type::Basic(BasicType::Int32),
                UntypedKind::Float => Type::Basic(BasicType::Float64),
                UntypedKind::String => Type::Basic(BasicType::String),
            },
            _ => ty.clone(),
        }
    }

    /// Returns the common type of two types (for binary operations).
    pub(crate) fn common_type(&self, left: &Type, right: &Type) -> Type {
        // If one is untyped, prefer the typed one
        match (left, right) {
            (Type::Untyped(lk), Type::Untyped(rk)) => {
                // Higher precedence wins
                if lk.precedence() >= rk.precedence() {
                    left.clone()
                } else {
                    right.clone()
                }
            }
            (Type::Untyped(_), _) => right.clone(),
            (_, Type::Untyped(_)) => left.clone(),
            _ => left.clone(), // Assume compatible
        }
    }
}
