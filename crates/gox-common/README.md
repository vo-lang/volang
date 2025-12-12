# gox-common

Shared utilities for the GoX compiler.

## Components

- **Span** (`span.rs`): Source code position tracking
- **Source** (`source.rs`): Source file management with `codespan` integration
- **Diagnostic** (`diagnostic.rs`): Rich error/warning reporting
- **Error** (`error.rs`): Common error types (`GoxError`, `GoxResult`)

## Usage

```rust
use gox_common::{Span, SourceManager, Diagnostic, GoxError, GoxResult};

// Create a source manager
let mut sm = SourceManager::new();
let file_id = sm.add_file("test.gox", "var x int = 1;");

// Create a diagnostic
let diag = Diagnostic::error("undefined variable")
    .with_code("E0001")
    .with_label(Span::new(4, 5), file_id, "not found in scope");

// Emit to terminal
diag.emit(&sm);
```

## Testing

```bash
cargo test -p gox-common
```
