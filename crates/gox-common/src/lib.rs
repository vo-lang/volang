//! # gox-common
//!
//! Shared infrastructure for the GoX compiler.
//!
//! This crate provides foundational types and utilities used throughout the GoX compiler:
//! - Source file management and location tracking
//! - Diagnostic reporting with rich formatting
//! - Symbol interning for efficient identifier handling
//! - Common type definitions (ValueKind)

pub mod source;
pub mod span;
pub mod diagnostics;
pub mod symbol;
pub mod vfs;
pub mod types;

pub use source::{SourceMap, SourceFile, FileId};
pub use span::{Span, Spanned, BytePos};
pub use diagnostics::{Diagnostic, DiagnosticSink, Severity, Label};
pub use symbol::{Symbol, SymbolInterner, Ident};
pub use vfs::{FileSystem, RealFs, MemoryFs, FileSet};
pub use types::ValueKind;
