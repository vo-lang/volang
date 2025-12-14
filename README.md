# GoX Programming Language

GoX is a statically typed, Go-like programming language with multiple backend targets.

## Overview

GoX aims to provide familiar Go syntax while introducing explicit reference semantics through the `object` type and supporting multiple compilation backends.

### Key Features

- **Go-like syntax** - Familiar to Go programmers
- **Static typing** with local type inference
- **Explicit memory model** - Clear distinction between value types (`struct`) and object types (`object`)
- **Multiple backends** - LLVM, WebAssembly, and a custom VM
- **No generics** - Simplified type system
- **No pointers** - Reference semantics through `object` types

## Project Structure

```
gox/
├── crates/
│   │
│   │  # ─────────── Frontend ───────────
│   ├── gox-common/           # Shared types, errors, spans
│   ├── gox-syntax/           # Lexer, parser, AST
│   ├── gox-analysis/         # Type checking, semantic analysis
│   ├── gox-module/           # Module/package management
│   │
│   │  # ─────────── Code Generation ───────────
│   ├── gox-codegen-vm/       # VM bytecode generation
│   ├── gox-codegen-llvm/     # LLVM IR generation (core)
│   │
│   │  # ─────────── Compilation Targets ───────────
│   ├── gox-target-native/    # Target: native executable
│   ├── gox-target-wasm/      # Target: WebAssembly
│   ├── gox-target-sbf/       # Target: Solana eBPF
│   │
│   │  # ─────────── VM ───────────
│   ├── gox-vm/               # VM core (interpreter, GC)
│   │
│   │  # ─────────── Runtime ───────────
│   ├── gox-runtime-core/     # Runtime API definitions (shared)
│   ├── gox-runtime-vm/       # VM runtime + native functions
│   ├── gox-runtime-native/   # Native runtime
│   ├── gox-runtime-wasm/     # WASM runtime
│   ├── gox-runtime-sbf/      # Solana runtime
│   │
│   │  # ─────────── Tools ───────────
│   ├── gox-cli/              # Command-line interface
│   └── gox-tests/            # Integration tests
│
├── stdlib/                   # GoX standard library (GoX code)
│   ├── fmt/
│   ├── strings/
│   ├── io/
│   ├── net/
│   ├── encoding/json/
│   └── ...
│
├── docs/                     # Documentation
│   ├── design/               # Design docs (vm.md, gc.md, ffi.md)
│   └── spec/                 # Language specification
│
└── examples/                 # Example programs
```

## Crate Dependencies

```
                              gox-cli
                                 │
            ┌────────────────────┼────────────────────┐
            │                    │                    │
            ▼                    ▼                    ▼
     gox-codegen-vm      gox-target-native    gox-target-wasm    gox-target-sbf
            │                    │                    │                │
            │                    └────────┬───────────┘                │
            │                             │                            │
            │                             ▼                            │
            │                    gox-codegen-llvm ◄─────────────────────┘
            │                             │
            ▼                             │
         gox-vm                           │
            │                             │
            ▼                             ▼
    gox-runtime-vm          ┌─────────────┴─────────────┐
            │               │             │             │
            │               ▼             ▼             ▼
            │      gox-runtime-    gox-runtime-   gox-runtime-
            │         native          wasm           sbf
            │               │             │             │
            └───────────────┴─────────────┴─────────────┘
                                    │
                                    ▼
                            gox-runtime-core
                                    │
                                    ▼
                        ┌───────────┴───────────┐
                        │                       │
                        ▼                       ▼
                  gox-analysis             gox-module
                        │
                        ▼
                   gox-syntax
                        │
                        ▼
                   gox-common
```

## Building

```bash
cargo build --release
```

## Usage

```bash
# Run a GoX program (using VM backend)
gox run program.gox

# Compile to native executable
gox build program.gox
gox build --target=native program.gox

# Compile to WebAssembly
gox build --target=wasm program.gox

# Compile to Solana eBPF
gox build --target=sbf program.gox

# Run with VM (interpreter mode)
gox run --vm program.gox
```

## Language Example

```gox
package main

type User struct {
    name string
    age  int
}

type UserRef object {
    name string
    age  int
}

interface Greeter {
    Greet() string
}

func (u User) Greet() string {
    return "Hello, " + u.name
}

func main() int {
    user := User{name: "Alice", age: 30}
    println(user.Greet())
    
    var ref UserRef = UserRef{name: "Bob", age: 25}
    ref.name = "Charlie"  // modifies the object
    
    numbers := []int{1, 2, 3}
    for i, v := range numbers {
        println(i, v)
    }
    
    return 0
}
```

## License

MIT License - see [LICENSE](LICENSE) for details.
