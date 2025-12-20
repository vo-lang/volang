//! Type checking implementation.
//!
//! This module contains the type checker that validates GoX source code
//! and produces type information for code generation.

mod assignment;
mod builtin;
mod call;
mod checker;
mod conversion;
mod decl;
mod expr;
mod initorder;
mod interface;
mod label;
mod resolver;
mod returns;
mod stmt;
mod type_info;
mod typexpr;
mod util;

pub use checker::Checker;
pub use resolver::DeclInfo;
pub use type_info::{Initializer, TypeAndValue, TypeInfo};
