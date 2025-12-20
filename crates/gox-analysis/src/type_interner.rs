//! Type Interner - type deduplication and fast lookup.
//!
//! This module provides `TypeInterner`, which manages type identity:
//! - Structurally equal types share the same `TypeId`
//! - Expression → Type mapping via `ExprId`
//! - Symbol → Type mapping for variables and declarations

use std::collections::HashMap;

use gox_common::Symbol;
use gox_common_core::{ExprId, TypeExprId, TypeId};

use crate::types::Type;

/// Type interner - type deduplication + fast lookup.
///
/// Named types are always distinct (each declaration gets a unique TypeId).
/// Other types share TypeId if structurally equal.
#[derive(Debug, Default)]
pub struct TypeInterner {
    /// TypeId → Type (all unique types)
    types: Vec<Type>,

    /// Type → TypeId (for deduplication of non-Named types)
    type_to_id: HashMap<Type, TypeId>,

    /// ExprId → TypeId (expression → type)
    expr_types: HashMap<ExprId, TypeId>,

    /// Symbol → TypeId (variable/parameter → type)
    symbol_types: HashMap<Symbol, TypeId>,

    /// TypeExprId → TypeId (type expression → resolved type)
    type_expr_types: HashMap<TypeExprId, TypeId>,
}


impl TypeInterner {
    /// Creates a new empty type interner.
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            type_to_id: HashMap::new(),
            expr_types: HashMap::new(),
            symbol_types: HashMap::new(),
            type_expr_types: HashMap::new(),
        }
    }

    /// Interns a type, returning its TypeId.
    ///
    /// Named types are keyed by their NamedTypeId (each declaration is independent).
    /// Other types share TypeId if structurally equal.
    pub fn intern(&mut self, ty: Type) -> TypeId {
        // All types: check if already interned
        if let Some(&id) = self.type_to_id.get(&ty) {
            return id;
        }

        // Allocate new TypeId
        let id = TypeId(self.types.len() as u32);
        self.types.push(ty.clone());
        self.type_to_id.insert(ty, id);
        id
    }

    /// Resolves a TypeId to its Type.
    ///
    /// # Panics
    /// Panics if the TypeId is invalid.
    pub fn resolve(&self, id: TypeId) -> &Type {
        &self.types[id.0 as usize]
    }

    /// Tries to resolve a TypeId to its Type.
    pub fn try_resolve(&self, id: TypeId) -> Option<&Type> {
        self.types.get(id.0 as usize)
    }

    /// Gets the TypeId for an expression.
    pub fn get_expr_type(&self, expr_id: ExprId) -> Option<TypeId> {
        self.expr_types.get(&expr_id).copied()
    }

    /// Gets the TypeId for a symbol.
    pub fn get_symbol_type(&self, sym: Symbol) -> Option<TypeId> {
        self.symbol_types.get(&sym).copied()
    }

    /// Records the type of an expression.
    pub fn record_expr_type(&mut self, expr_id: ExprId, type_id: TypeId) {
        self.expr_types.insert(expr_id, type_id);
    }

    /// Binds a symbol to a type.
    pub fn bind_symbol(&mut self, sym: Symbol, type_id: TypeId) {
        self.symbol_types.insert(sym, type_id);
    }

    /// Gets the TypeId for a type expression.
    pub fn get_type_expr_type(&self, id: TypeExprId) -> Option<TypeId> {
        self.type_expr_types.get(&id).copied()
    }

    /// Binds a type expression to a type.
    pub fn bind_type_expr(&mut self, id: TypeExprId, type_id: TypeId) {
        self.type_expr_types.insert(id, type_id);
    }

    /// Returns the number of interned types.
    pub fn len(&self) -> usize {
        self.types.len()
    }

    /// Returns true if no types have been interned.
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    /// Returns an iterator over all (TypeId, Type) pairs.
    pub fn iter(&self) -> impl Iterator<Item = (TypeId, &Type)> {
        self.types
            .iter()
            .enumerate()
            .map(|(i, ty)| (TypeId(i as u32), ty))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BasicType, SliceType};

    #[test]
    fn test_basic_type_interning() {
        let mut interner = TypeInterner::new();

        let int1 = interner.intern(Type::Basic(BasicType::Int));
        let int2 = interner.intern(Type::Basic(BasicType::Int));
        let float = interner.intern(Type::Basic(BasicType::Float64));

        // Same type should get same TypeId
        assert_eq!(int1, int2);
        // Different types should get different TypeIds
        assert_ne!(int1, float);
    }

    #[test]
    fn test_slice_type_interning() {
        let mut interner = TypeInterner::new();

        let slice1 = interner.intern(Type::Slice(SliceType {
            elem: Box::new(Type::Basic(BasicType::Int)),
        }));
        let slice2 = interner.intern(Type::Slice(SliceType {
            elem: Box::new(Type::Basic(BasicType::Int)),
        }));
        let slice3 = interner.intern(Type::Slice(SliceType {
            elem: Box::new(Type::Basic(BasicType::String)),
        }));

        // Same structure should get same TypeId
        assert_eq!(slice1, slice2);
        // Different element type should get different TypeId
        assert_ne!(slice1, slice3);
    }

    #[test]
    fn test_expr_binding() {
        let mut interner = TypeInterner::new();

        let type_id = interner.intern(Type::Basic(BasicType::Int));
        let expr_id = ExprId(42);

        interner.record_expr_type(expr_id, type_id);

        assert_eq!(interner.get_expr_type(expr_id), Some(type_id));
        assert_eq!(interner.get_expr_type(ExprId(999)), None);
    }
}
