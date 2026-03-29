//! # vo-syntax
//!
//! Lexer, AST definitions, and parser for the Vo programming language.
//!
//! This crate provides the frontend components of the Vo compiler:
//! - Tokenization of source code
//! - Abstract Syntax Tree definitions
//! - Recursive descent parser with Pratt expression parsing

pub mod ast;
pub mod display;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod token;

pub use ast::*;
pub use lexer::Lexer;
pub use parser::{parse, parse_with_interner, Parser};
pub use token::{Token, TokenKind};
