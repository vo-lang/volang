//! Symbol table and scope management for the GoX type checker.
//!
//! This module provides the scope hierarchy and entity definitions
//! for name resolution during type checking.
//!
//! # Scope Hierarchy
//!
//! ```text
//! Universe Scope (built-in types: int, bool, string, etc.)
//!     └── Package Scope (top-level declarations)
//!             └── Function Scope (parameters, named returns)
//!                     └── Block Scope (if/for/switch bodies)
//!                             └── Block Scope (nested blocks)
//! ```
//!
//! # Entities
//!
//! The symbol table stores different kinds of entities:
//! - [`Entity::Var`]: Variables and constants with their types
//! - [`Entity::Type`]: Type declarations (named types)
//! - [`Entity::Func`]: Function declarations with signatures
//! - [`Entity::Builtin`]: Built-in functions like `len`, `make`, `append`
//!
//! # Name Resolution
//!
//! Names are resolved by searching from the innermost scope outward:
//! 1. Current block scope (if any)
//! 2. Enclosing block scopes
//! 3. Function scope (parameters, named returns)
//! 4. Package scope (top-level declarations)
//! 5. Universe scope (built-in types and functions)

use std::collections::HashMap;

use gox_common::{Symbol, Span};

use crate::types::{Type, FuncType, NamedTypeId};
use crate::constant::Constant;

/// An entity in the symbol table.
#[derive(Debug, Clone)]
pub enum Entity {
    /// A variable or constant.
    Var(VarEntity),

    /// A type declaration.
    Type(TypeEntity),

    /// A function declaration.
    Func(FuncEntity),

    /// A package import.
    Package(PackageEntity),

    /// A label (for goto).
    Label(LabelEntity),

    /// A built-in function.
    Builtin(BuiltinKind),
}

/// A variable or constant entity.
#[derive(Debug, Clone)]
pub struct VarEntity {
    /// The variable's type.
    pub ty: Type,
    /// The constant value, if this is a constant.
    pub constant: Option<Constant>,
    /// The declaration span.
    pub span: Span,
}

/// A type declaration entity.
#[derive(Debug, Clone)]
pub struct TypeEntity {
    /// The named type ID in the type registry.
    pub id: NamedTypeId,
    /// The declaration span.
    pub span: Span,
}

/// A function declaration entity.
#[derive(Debug, Clone)]
pub struct FuncEntity {
    /// The function's signature.
    pub sig: FuncType,
    /// The declaration span.
    pub span: Span,
}

/// A package import entity.
#[derive(Debug, Clone)]
pub struct PackageEntity {
    /// The import path.
    pub path: String,
    /// The declaration span.
    pub span: Span,
}

/// A label entity.
#[derive(Debug, Clone)]
pub struct LabelEntity {
    /// The declaration span.
    pub span: Span,
}

/// Built-in function kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinKind {
    Len,
    Cap,
    Append,
    Copy,
    Delete,
    Make,
    Close,
    Panic,
    Recover,
    Print,
    Println,
}

impl BuiltinKind {
    /// Returns the name of this built-in function.
    pub fn name(&self) -> &'static str {
        match self {
            BuiltinKind::Len => "len",
            BuiltinKind::Cap => "cap",
            BuiltinKind::Append => "append",
            BuiltinKind::Copy => "copy",
            BuiltinKind::Delete => "delete",
            BuiltinKind::Make => "make",
            BuiltinKind::Close => "close",
            BuiltinKind::Panic => "panic",
            BuiltinKind::Recover => "recover",
            BuiltinKind::Print => "print",
            BuiltinKind::Println => "println",
        }
    }

    /// Returns all built-in function kinds.
    pub fn all() -> &'static [BuiltinKind] {
        &[
            BuiltinKind::Len,
            BuiltinKind::Cap,
            BuiltinKind::Append,
            BuiltinKind::Copy,
            BuiltinKind::Delete,
            BuiltinKind::Make,
            BuiltinKind::Close,
            BuiltinKind::Panic,
            BuiltinKind::Recover,
            BuiltinKind::Print,
            BuiltinKind::Println,
        ]
    }
}

/// A scope in the symbol table hierarchy.
#[derive(Debug, Default)]
pub struct Scope {
    /// Parent scope (None for universe scope).
    parent: Option<Box<Scope>>,
    /// Symbols defined in this scope.
    symbols: HashMap<Symbol, Entity>,
    /// The kind of this scope.
    kind: ScopeKind,
}

/// The kind of a scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ScopeKind {
    /// Universe scope (predefined types, constants, built-ins).
    #[default]
    Universe,
    /// Package scope (top-level declarations).
    Package,
    /// File scope (imports).
    File,
    /// Function scope (parameters).
    Function,
    /// Block scope (local variables).
    Block,
}

impl Scope {
    /// Creates a new scope with the given parent and kind.
    pub fn new(parent: Option<Scope>, kind: ScopeKind) -> Self {
        Self {
            parent: parent.map(Box::new),
            symbols: HashMap::new(),
            kind,
        }
    }

    /// Creates the universe scope with predefined entities.
    pub fn universe() -> Self {
        let mut scope = Self::new(None, ScopeKind::Universe);
        scope.populate_universe();
        scope
    }

    /// Populates the universe scope with predefined entities.
    fn populate_universe(&mut self) {
        // Built-in functions are added by the checker when it has access to the interner
    }

    /// Returns the kind of this scope.
    pub fn kind(&self) -> ScopeKind {
        self.kind
    }

    /// Inserts an entity into this scope.
    /// Returns the previous entity if the symbol was already defined.
    pub fn insert(&mut self, symbol: Symbol, entity: Entity) -> Option<Entity> {
        self.symbols.insert(symbol, entity)
    }

    /// Looks up a symbol in this scope only (not parent scopes).
    pub fn lookup_local(&self, symbol: Symbol) -> Option<&Entity> {
        self.symbols.get(&symbol)
    }

    /// Looks up a symbol in this scope and all parent scopes.
    pub fn lookup(&self, symbol: Symbol) -> Option<&Entity> {
        self.symbols
            .get(&symbol)
            .or_else(|| self.parent.as_ref().and_then(|p| p.lookup(symbol)))
    }

    /// Returns a mutable reference to an entity in this scope only.
    pub fn lookup_local_mut(&mut self, symbol: Symbol) -> Option<&mut Entity> {
        self.symbols.get_mut(&symbol)
    }

    /// Returns true if the symbol is defined in this scope (not parent scopes).
    pub fn contains_local(&self, symbol: Symbol) -> bool {
        self.symbols.contains_key(&symbol)
    }

    /// Returns true if the symbol is defined in this scope or any parent scope.
    pub fn contains(&self, symbol: Symbol) -> bool {
        self.symbols.contains_key(&symbol)
            || self.parent.as_ref().map_or(false, |p| p.contains(symbol))
    }

    /// Returns an iterator over all symbols in this scope (not parent scopes).
    pub fn local_symbols(&self) -> impl Iterator<Item = (&Symbol, &Entity)> {
        self.symbols.iter()
    }

    /// Takes ownership of the parent scope, leaving None in its place.
    pub fn take_parent(&mut self) -> Option<Scope> {
        self.parent.take().map(|b| *b)
    }

    /// Sets the parent scope.
    pub fn set_parent(&mut self, parent: Scope) {
        self.parent = Some(Box::new(parent));
    }

    /// Returns a reference to the parent scope.
    pub fn parent(&self) -> Option<&Scope> {
        self.parent.as_deref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::BasicType;

    fn dummy_symbol(n: u32) -> Symbol {
        // Create a symbol for testing
        unsafe { std::mem::transmute(n) }
    }

    #[test]
    fn test_scope_insert_lookup() {
        let mut scope = Scope::new(None, ScopeKind::Block);
        let sym = dummy_symbol(1);

        let entity = Entity::Var(VarEntity {
            ty: Type::Basic(BasicType::Int),
            constant: None,
            span: Span::dummy(),
        });

        assert!(scope.lookup_local(sym).is_none());
        scope.insert(sym, entity);
        assert!(scope.lookup_local(sym).is_some());
    }

    #[test]
    fn test_scope_parent_lookup() {
        let mut parent = Scope::new(None, ScopeKind::Function);
        let parent_sym = dummy_symbol(1);
        parent.insert(
            parent_sym,
            Entity::Var(VarEntity {
                ty: Type::Basic(BasicType::Int),
                constant: None,
                span: Span::dummy(),
            }),
        );

        let child = Scope::new(Some(parent), ScopeKind::Block);

        // Should find in parent
        assert!(child.lookup(parent_sym).is_some());
        // But not in local
        assert!(child.lookup_local(parent_sym).is_none());
    }

    #[test]
    fn test_scope_shadowing() {
        let mut parent = Scope::new(None, ScopeKind::Function);
        let sym = dummy_symbol(1);
        parent.insert(
            sym,
            Entity::Var(VarEntity {
                ty: Type::Basic(BasicType::Int),
                constant: None,
                span: Span::dummy(),
            }),
        );

        let mut child = Scope::new(Some(parent), ScopeKind::Block);
        child.insert(
            sym,
            Entity::Var(VarEntity {
                ty: Type::Basic(BasicType::String),
                constant: None,
                span: Span::dummy(),
            }),
        );

        // Child's definition shadows parent's
        if let Some(Entity::Var(v)) = child.lookup(sym) {
            assert!(matches!(v.ty, Type::Basic(BasicType::String)));
        } else {
            panic!("expected Var entity");
        }
    }

    #[test]
    fn test_builtin_kind() {
        assert_eq!(BuiltinKind::Len.name(), "len");
        assert_eq!(BuiltinKind::Make.name(), "make");
        assert!(BuiltinKind::all().contains(&BuiltinKind::Append));
    }
}
