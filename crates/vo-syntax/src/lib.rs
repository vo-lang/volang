//! # vo-syntax
//!
//! Lexer, AST definitions, and parser for the Vo programming language.
//!
//! This crate provides the frontend components of the Vo compiler:
//! - Tokenization of source code
//! - Abstract Syntax Tree definitions
//! - Recursive descent parser with Pratt expression parsing

pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod errors;
pub mod display;

pub use token::{Token, TokenKind};
pub use lexer::Lexer;
pub use ast::*;
pub use parser::{Parser, parse, parse_with_interner};
