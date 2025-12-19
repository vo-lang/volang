//! Unified type information query interface.

use gox_analysis::scope::{BuiltinKind, Entity, FuncEntity, Scope, TypeEntity, VarEntity};
use gox_analysis::type_interner::TypeInterner;
use gox_analysis::types::{BasicType, NamedTypeId, NamedTypeInfo, Type};
use gox_common::{Symbol, SymbolInterner};
use gox_common_core::{ExprId, SlotType, TypeId};
use gox_syntax::ast::Expr;

use crate::types;

/// Unified type information query interface.
pub struct TypeInfo<'a> {
    pub types: &'a TypeInterner,
    pub scope: &'a Scope,
    pub named_types: &'a [NamedTypeInfo],
    pub interner: &'a SymbolInterner,
}

impl<'a> TypeInfo<'a> {
    pub fn new(
        types: &'a TypeInterner,
        scope: &'a Scope,
        named_types: &'a [NamedTypeInfo],
        interner: &'a SymbolInterner,
    ) -> Self {
        Self { types, scope, named_types, interner }
    }

    // === Expression type queries ===

    pub fn expr_type(&self, expr: &Expr) -> Option<&Type> {
        self.types.get_expr_type(expr.id).and_then(|id| self.types.try_resolve(id))
    }

    pub fn expr_type_id(&self, expr_id: ExprId) -> Option<TypeId> {
        self.types.get_expr_type(expr_id)
    }

    // === Symbol queries ===

    pub fn lookup(&self, sym: Symbol) -> Option<&Entity> {
        self.scope.lookup(sym)
    }

    pub fn lookup_var(&self, sym: Symbol) -> Option<&VarEntity> {
        match self.scope.lookup(sym) {
            Some(Entity::Var(v)) => Some(v),
            _ => None,
        }
    }

    pub fn lookup_func(&self, sym: Symbol) -> Option<&FuncEntity> {
        match self.scope.lookup(sym) {
            Some(Entity::Func(f)) => Some(f),
            _ => None,
        }
    }

    pub fn lookup_type(&self, sym: Symbol) -> Option<&TypeEntity> {
        match self.scope.lookup(sym) {
            Some(Entity::Type(t)) => Some(t),
            _ => None,
        }
    }

    pub fn is_builtin(&self, sym: Symbol) -> Option<BuiltinKind> {
        match self.scope.lookup(sym) {
            Some(Entity::Builtin(k)) => Some(*k),
            _ => None,
        }
    }

    // === Named type queries ===

    pub fn named_type_info(&self, id: NamedTypeId) -> Option<&NamedTypeInfo> {
        self.named_types.get(id.0 as usize)
    }

    pub fn underlying(&self, id: NamedTypeId) -> Option<&Type> {
        self.named_types.get(id.0 as usize).map(|info| &info.underlying)
    }

    // === Type property queries ===

    pub fn runtime_type_id(&self, ty: &Type) -> u32 {
        types::type_to_runtime_id(ty, self.named_types)
    }

    pub fn type_slots(&self, ty: &Type) -> u16 {
        types::type_slots(ty, self.named_types)
    }

    pub fn type_slot_types(&self, ty: &Type) -> Vec<SlotType> {
        types::type_slot_types(ty, self.named_types)
    }

    pub fn is_ref_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Basic(BasicType::String) => true,
            Type::Basic(_) => false,
            Type::Slice(_) | Type::Map(_) | Type::Chan(_) | Type::Func(_) | Type::Pointer(_) => true,
            Type::Array(_) | Type::Struct(_) => false,
            Type::Interface(_) => true,
            Type::Named(id) => {
                if let Some(info) = self.named_types.get(id.0 as usize) {
                    self.is_ref_type(&info.underlying)
                } else {
                    false
                }
            }
            Type::Nil | Type::Invalid | Type::Tuple(_) | Type::Untyped(_) => false,
        }
    }

    pub fn is_interface(&self, ty: &Type) -> bool {
        match ty {
            Type::Interface(_) => true,
            Type::Named(id) => {
                if let Some(info) = self.named_types.get(id.0 as usize) {
                    matches!(&info.underlying, Type::Interface(_))
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    // === Helper methods ===

    pub fn symbol_str(&self, sym: Symbol) -> &str {
        self.interner.resolve(sym).unwrap_or("")
    }

    pub fn resolve_type(&self, type_id: TypeId) -> Option<&Type> {
        self.types.try_resolve(type_id)
    }
}
