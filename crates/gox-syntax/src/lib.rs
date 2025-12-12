//! GoX Syntax - Lexer, Parser, and AST for the GoX language.
//!
//! This crate provides the frontend components of the GoX compiler:
//! - **Lexer**: Tokenizes GoX source code
//! - **Parser**: Produces an AST from tokens
//! - **AST**: Abstract Syntax Tree types

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

#[cfg(test)]
mod tests;
