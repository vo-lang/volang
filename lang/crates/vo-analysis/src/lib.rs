//! Vo type checking and semantic analysis.
//!
//! This crate provides type checking for Vo source code, producing type information
//! needed for code generation.


#[macro_use]
pub mod arena;
pub mod check;
pub mod constant;
pub mod display;
pub mod importer;
pub mod lookup;
pub mod obj;
pub mod objects;
pub mod operand;
pub mod package;
pub mod project;
pub mod scope;
pub mod selection;
pub mod typ;
pub mod universe;

pub use check::sendable::{check_sendable, Sendability};
pub use check::{Checker, Initializer, TypeAndValue, TypeInfo};
pub use obj::{Builtin, ConstValue, EntityType, LangObj};
pub use objects::*;
pub use operand::*;
pub use package::Package;
pub use project::{analyze_project, analyze_project_with_options, AnalysisError, AnalysisOptions, Project};
pub use scope::Scope;
pub use selection::*;
pub use typ::*;
pub use universe::*;
