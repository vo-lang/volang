//! Type expression resolution.
//!
//! This module converts AST type expressions (TypeExpr) to internal types (TypeKey).
//! It handles type-checking of type expressions and resolves them to TypeKey values.

#![allow(dead_code)]

use gox_common::span::Span;
use gox_common::symbol::{Ident, Symbol};
use gox_common::vfs::FileSystem;

use crate::obj::LangObj;
use crate::objects::{ObjKey, ScopeKey, TypeKey};
use crate::operand::{Operand, OperandMode};
use crate::scope;
use crate::typ::{
    ArrayDetail, BasicType, ChanDetail, ChanDir, MapDetail, PointerDetail, SignatureDetail,
    SliceDetail, StructDetail, TupleDetail, Type,
};
use gox_syntax::ast::{self, FuncSig, TypeExpr, TypeExprKind};

use super::checker::{Checker, FilesContext};

impl<F: FileSystem> Checker<F> {
    /// Resolves a type expression to a TypeKey.
    pub fn resolve_type(&mut self, ty: &TypeExpr) -> TypeKey {
        self.resolve_type_impl(ty, None)
    }

    /// Resolves a type expression with an optional definition context.
    fn resolve_type_impl(&mut self, ty: &TypeExpr, _def: Option<TypeKey>) -> TypeKey {
        match &ty.kind {
            TypeExprKind::Ident(ident) => self.resolve_type_name(ident),
            TypeExprKind::Selector(_) => self.get_invalid_type(),
            TypeExprKind::Array(arr) => self.resolve_array_type(arr),
            TypeExprKind::Slice(elem) => self.resolve_slice_type(elem),
            TypeExprKind::Map(map) => self.resolve_map_type(map),
            TypeExprKind::Chan(chan) => self.resolve_chan_type(chan),
            TypeExprKind::Func(func) => self.resolve_func_type(func),
            TypeExprKind::Struct(s) => self.resolve_struct_type(s),
            TypeExprKind::Pointer(base) => self.resolve_pointer_type(base),
            TypeExprKind::Interface(_) => self.resolve_empty_interface(),
        }
    }

    /// Returns the invalid type key (for typexpr).
    fn get_invalid_type(&self) -> TypeKey {
        self.universe().lookup_type(BasicType::Invalid).unwrap()
    }

    /// Resolves a simple type name (e.g., `int`, `MyType`).
    fn resolve_type_name(&mut self, ident: &gox_common::symbol::Ident) -> TypeKey {
        let name = ident.symbol;

        // Check for predeclared types first
        if let Some(type_key) = self.lookup_predeclared_type(name) {
            return type_key;
        }

        // Look up in current scope
        if let Some((_scope, obj_key)) = self.lookup_name(name) {
            let obj = &self.tc_objs.lobjs[obj_key];
            if let Some(typ) = obj.typ() {
                return typ;
            }
        }

        // Type not found - return invalid
        self.get_invalid_type()
    }

    /// Resolves an array type.
    fn resolve_array_type(&mut self, arr: &ast::ArrayType) -> TypeKey {
        let elem = self.resolve_type(&arr.elem);
        // TODO: Evaluate length expression
        let len = Some(0u64); // Placeholder
        let detail = ArrayDetail::new(elem, len);
        self.tc_objs.types.insert(Type::Array(detail))
    }

    /// Resolves a slice type.
    fn resolve_slice_type(&mut self, elem: &TypeExpr) -> TypeKey {
        let elem_key = self.resolve_type(elem);
        let detail = SliceDetail::new(elem_key);
        self.tc_objs.types.insert(Type::Slice(detail))
    }

    /// Resolves a map type.
    fn resolve_map_type(&mut self, map: &ast::MapType) -> TypeKey {
        let key = self.resolve_type(&map.key);
        let value = self.resolve_type(&map.value);
        let detail = MapDetail::new(key, value);
        self.tc_objs.types.insert(Type::Map(detail))
    }

    /// Resolves a channel type.
    fn resolve_chan_type(&mut self, chan: &ast::ChanType) -> TypeKey {
        let elem = self.resolve_type(&chan.elem);
        let dir = match chan.dir {
            ast::ChanDir::Both => ChanDir::SendRecv,
            ast::ChanDir::Send => ChanDir::SendOnly,
            ast::ChanDir::Recv => ChanDir::RecvOnly,
        };
        let detail = ChanDetail::new(dir, elem);
        self.tc_objs.types.insert(Type::Chan(detail))
    }

    /// Resolves a function type.
    fn resolve_func_type(&mut self, func: &ast::FuncType) -> TypeKey {
        // Resolve parameter types - create empty tuple for now
        // TODO: Create proper parameter objects
        let params = self.new_tuple(Vec::new());

        // Resolve result types - create empty tuple for now
        // TODO: Create proper result objects
        let results = self.new_tuple(Vec::new());

        let detail = SignatureDetail::new(None, None, params, results, false);
        self.tc_objs.types.insert(Type::Signature(detail))
    }

    /// Creates a new tuple type.
    fn new_tuple(&mut self, vars: Vec<ObjKey>) -> TypeKey {
        let detail = TupleDetail::new(vars);
        self.tc_objs.types.insert(Type::Tuple(detail))
    }

    /// Resolves a struct type.
    fn resolve_struct_type(&mut self, s: &ast::StructType) -> TypeKey {
        // For now, create struct with empty fields
        // TODO: Create proper field objects with names
        let fields = Vec::new();
        let detail = StructDetail::new(fields, None);
        self.tc_objs.types.insert(Type::Struct(detail))
    }

    /// Resolves a pointer type.
    fn resolve_pointer_type(&mut self, base: &TypeExpr) -> TypeKey {
        let base_key = self.resolve_type(base);
        let detail = PointerDetail::new(base_key);
        self.tc_objs.types.insert(Type::Pointer(detail))
    }

    /// Creates an empty interface type.
    fn resolve_empty_interface(&mut self) -> TypeKey {
        let detail = crate::typ::InterfaceDetail::new(Vec::new(), Vec::new());
        self.tc_objs.types.insert(Type::Interface(detail))
    }

    /// Looks up a predeclared type by name.
    fn lookup_predeclared_type(&self, name: gox_common::symbol::Symbol) -> Option<TypeKey> {
        // Use symbol's u32 value for comparison since we can't resolve without interner
        // TODO: Use proper interner-based lookup
        None // For now, always defer to scope lookup
    }

    /// Looks up a name in the current scope chain.
    fn lookup_name(&self, _name: Symbol) -> Option<(ScopeKey, ObjKey)> {
        // TODO: Implement scope lookup using octx.scope
        if let Some(scope_key) = self.octx.scope {
            // Use scope lookup
            if let Some((skey, okey)) = scope::lookup_parent(scope_key, "", &self.tc_objs) {
                return Some((skey, okey));
            }
        }
        None
    }

    // =========================================================================
    // Function type checking
    // =========================================================================

    /// Type-checks a function signature and returns its type.
    pub fn func_type_from_sig(&mut self, sig: &FuncSig) -> TypeKey {
        // Collect parameter types
        let mut param_vars = Vec::new();
        let variadic = sig.variadic;

        for (i, param) in sig.params.iter().enumerate() {
            let param_type = self.resolve_type(&param.ty);
            
            // Check for variadic (last parameter)
            if variadic && i == sig.params.len() - 1 {
                // For variadic, the type is already a slice from the parser
                // or we wrap element type in slice
                let slice_detail = SliceDetail::new(param_type);
                let slice_type = self.tc_objs.types.insert(Type::Slice(slice_detail));
                let var = self.tc_objs.lobjs.insert(LangObj::new_var(
                    0,
                    Some(self.pkg),
                    String::new(), // Anonymous for now
                    Some(slice_type),
                ));
                param_vars.push(var);
            } else {
                let var = self.tc_objs.lobjs.insert(LangObj::new_var(
                    0,
                    Some(self.pkg),
                    String::new(),
                    Some(param_type),
                ));
                param_vars.push(var);
            }
        }

        // Collect result types
        let mut result_vars = Vec::new();
        for result in sig.results.iter() {
            let result_type = self.resolve_type(&result.ty);
            let var = self.tc_objs.lobjs.insert(LangObj::new_var(
                0,
                Some(self.pkg),
                String::new(),
                Some(result_type),
            ));
            result_vars.push(var);
        }

        // Create tuple types for params and results
        let params_tuple = self.new_tuple(param_vars);
        let results_tuple = self.new_tuple(result_vars);

        // Create signature type
        let detail = SignatureDetail::new(None, None, params_tuple, results_tuple, variadic);
        self.tc_objs.types.insert(Type::Signature(detail))
    }

    /// Resolves a struct type with fields.
    fn resolve_struct_type_full(&mut self, s: &ast::StructType) -> TypeKey {
        let mut field_vars = Vec::new();

        for field in &s.fields {
            let field_type = self.resolve_type(&field.ty);
            
            // Create a var for each field name, or anonymous if no names
            if field.names.is_empty() {
                // Anonymous/embedded field
                let var = self.tc_objs.lobjs.insert(LangObj::new_var(
                    0,
                    Some(self.pkg),
                    String::new(),
                    Some(field_type),
                ));
                field_vars.push(var);
            } else {
                for _name in &field.names {
                    let var = self.tc_objs.lobjs.insert(LangObj::new_var(
                        0,
                        Some(self.pkg),
                        String::new(), // TODO: Use actual name from interner
                        Some(field_type),
                    ));
                    field_vars.push(var);
                }
            }
        }

        let detail = StructDetail::new(field_vars, None);
        self.tc_objs.types.insert(Type::Struct(detail))
    }

    /// Resolves an interface type.
    fn resolve_interface_type(&mut self, iface: &ast::InterfaceType) -> TypeKey {
        // For now, create empty interface
        // TODO: Collect methods and embedded interfaces
        let methods = Vec::new();
        let embeds = Vec::new();
        let detail = crate::typ::InterfaceDetail::new(methods, embeds);
        self.tc_objs.types.insert(Type::Interface(detail))
    }
}
