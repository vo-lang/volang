# vo-common

Shared infrastructure for the Vo compiler.

## Overview

This crate provides foundational types and utilities used throughout the Vo compiler pipeline:

- **Source Management** - File loading, source text storage, and location tracking
- **Diagnostics** - Error/warning reporting with source spans and rich formatting
- **Symbol Interning** - Efficient string interning for identifiers and symbols
- **Span Types** - Source location tracking with file, line, and column information

## Modules

### `source`
Manages source files and provides efficient access to source text:
- `SourceMap` - Central registry of all source files
- `SourceFile` - Individual source file with content and line information
- `FileId` - Unique identifier for source files

### `span`
Location tracking in source code:
- `Span` - A range in source code (start..end byte offsets)
- `Spanned<T>` - A value with associated source location
- `BytePos` - Absolute byte position in a file

### `diagnostics`
Compiler error and warning reporting:
- `Diagnostic` - A single diagnostic message with severity, message, and spans
- `DiagnosticSink` - Collector for diagnostics during compilation
- `Severity` - Error, Warning, Note, Help levels
- `Label` - Annotated span within a diagnostic

### `symbol`
String interning for efficient symbol handling:
- `Symbol` - Interned string identifier
- `SymbolInterner` - Thread-safe string interner
- `Ident` - Identifier with symbol and span

## Usage

```rust
use vo_common::{
    source::{SourceMap, FileId},
    span::{Span, Spanned},
    diagnostics::{Diagnostic, Severity, DiagnosticSink},
    symbol::{Symbol, SymbolInterner},
};

// Create a source map and add a file
let mut source_map = SourceMap::new();
let file_id = source_map.add_file("main.vo", "func main() {}");

// Create a diagnostic
let diagnostic = Diagnostic::error("undefined variable")
    .with_label(Label::primary(file_id, 5..9).with_message("not found in scope"));

// Render the diagnostic
diagnostic.emit(&source_map);
```

## Design Principles

1. **Zero-copy where possible** - Source text is stored once and referenced via spans
2. **Efficient interning** - All identifiers are interned to reduce memory and enable fast comparison
3. **Rich diagnostics** - Support for multi-span errors, suggestions, and fix hints
4. **Thread-safe** - Core types support concurrent access for parallel compilation
