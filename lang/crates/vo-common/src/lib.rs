//! # vo-common
//!
//! Shared infrastructure for the Vo compiler.
//!
//! This crate provides foundational types and utilities used throughout the Vo compiler:
//! - Source file management and location tracking
//! - Diagnostic reporting with rich formatting
//! - Symbol interning for efficient identifier handling
//! - Common type definitions (ValueKind) - re-exported from vo-common-core

pub mod abi;
pub mod diagnostics;
pub mod source;
pub mod span;
pub mod stable_hash;
pub mod vfs;

pub use diagnostics::{Diagnostic, DiagnosticSink, Label, Severity};
pub use source::{FileId, SourceFile, SourceLoc, SourceMap};
pub use span::{BytePos, Span, Spanned};
#[cfg(feature = "zip")]
pub use vfs::ZipFs;
pub use vfs::{FileSet, FileSystem, MemoryFs, OverlayFs, RealFs};

// Re-export from vo-common-core
pub use vo_common_core::symbol::{
    self, builtin_consts, builtin_funcs, builtin_types, kw, Symbol, SymbolInterner, BLANK,
};
pub use vo_common_core::ValueKind;
