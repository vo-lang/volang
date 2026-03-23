# Module System

Vo uses a module system similar to Go modules, backed by GitHub as a registry.

## Project Structure

A Vo module project contains:

```
myapp/
  main.vo       # entry point
  vo.mod        # module declaration + dependencies
  vo.lock       # deterministic dependency lock file
```

## `vo.mod`

Declares the module path and dependencies:

```
module myapp

require (
    github.com/vo-lang/vopack v0.1.0
)
```

## `vo.lock`

Auto-generated lock file for reproducible builds. Do not edit manually. Regenerate with:

```bash
vo mod sync
```

## Adding Dependencies

```bash
vo mod add github.com/vo-lang/vopack@v0.1.0
```

Or add manually to `vo.mod` and run:

```bash
vo mod download
```

## Importing Packages

```vo
import (
    "github.com/vo-lang/vopack"
)
```

## Native Extensions (FFI)

Modules can include Rust native extensions via `vo.ext.toml`:

```toml
[extension]
name = "myext"
```

The native code lives in a `rust/` subdirectory with a standard Cargo project. The extension is compiled to a `.wasm` file for portability.

## Workspaces

For multi-module development, use `vo.work`:

```
use (
    ./mylib
    ./myapp
)
```

This allows local modules to reference each other without publishing.
