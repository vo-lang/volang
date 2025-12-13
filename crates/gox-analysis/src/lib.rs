//! Semantic analysis and type checking for GoX.
//!
//! This crate implements the GoX type checker in three phases:
//! - Phase 1 (collect): Collect all top-level declarations, build symbol table
//! - Phase 2 (resolve): Resolve type references, compute method sets, detect cycles
//! - Phase 3 (check): Type-check function bodies, expressions, statements

pub mod types;
pub mod scope;
pub mod constant;
pub mod collect;

pub use types::{Type, BasicType, UntypedKind, NamedTypeId};
pub use scope::{Scope, ScopeKind, Entity, BuiltinKind};
pub use constant::Constant;
pub use collect::{collect_types, CollectResult};
