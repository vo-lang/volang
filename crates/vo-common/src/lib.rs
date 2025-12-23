//! # vo-common
//!
//! Shared infrastructure for the Vo compiler.
//!
//! This crate provides foundational types and utilities used throughout the Vo compiler:
//! - Source file management and location tracking
//! - Diagnostic reporting with rich formatting
//! - Symbol interning for efficient identifier handling
//! - Common type definitions (ValueKind) - re-exported from vo-common-core

pub mod source;
pub mod span;
pub mod diagnostics;
pub mod symbol;
pub mod vfs;

pub use source::{SourceMap, SourceFile, FileId};
pub use span::{Span, Spanned, BytePos};
pub use diagnostics::{Diagnostic, DiagnosticSink, Severity, Label};
pub use symbol::{Symbol, SymbolInterner, Ident};
pub use vfs::{FileSystem, RealFs, MemoryFs, FileSet};

// Re-export from vo-common-core for backwards compatibility
pub use vo_common_core::ValueKind;
