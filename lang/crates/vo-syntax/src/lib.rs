//! # vo-syntax
//!
//! Lexer, AST definitions, parser, and source formatter for the Vo programming language.
//!
//! This crate provides the frontend components of the Vo compiler:
//! - Tokenization of source code
//! - Abstract Syntax Tree definitions
//! - Recursive descent parser with Pratt expression parsing
//! - Parse-preserving source formatting

pub mod ast;
pub mod display;
pub mod errors;
mod formatter;
pub mod identifier;
pub mod inline_mod;
pub mod lexer;
pub mod parser;
pub mod token;

pub use ast::*;
pub use formatter::format_source;
pub use identifier::{is_exported_name, is_identifier_continue, is_identifier_start};
pub use lexer::Lexer;
pub use parser::{parse, parse_with_interner, Parser};
pub use token::{Token, TokenKind};
