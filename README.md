# GoX

**GoX** is a compiled programming language inspired by Go, designed for simplicity and performance. This repository contains the GoX compiler toolchain.

## Features

- **Go-like syntax** — Clean, readable syntax familiar to Go developers
- **Explicit interface implementation** — Uses `implements` declarations instead of implicit interface satisfaction
- **Simple type system** — Basic types, structs, arrays, slices, maps, functions
- **Automatic semicolon insertion** — Write clean code without explicit semicolons at line ends

## Project Structure

```
gox/
├── crates/
│   ├── gox-common/     # Shared utilities (spans, errors, diagnostics)
│   ├── gox-syntax/     # Lexer, parser, AST definitions
│   ├── gox-analysis/   # Semantic analysis (WIP)
│   ├── gox-codegen-llvm/  # LLVM backend (WIP)
│   ├── gox-codegen-wasm/  # WebAssembly backend (WIP)
│   ├── gox-codegen-vm/    # VM bytecode backend (WIP)
│   ├── gox-vm/         # Virtual machine runtime (WIP)
│   └── gox-cli/        # Command-line interface
├── english/            # Language specification
└── instructions/       # Development guides
```

## Getting Started

### Prerequisites

- Rust 1.70+
- Cargo

### Build

```bash
cargo build --workspace
```

### Run the CLI

```bash
# Parse a GoX source file and display AST
cargo run -p gox-cli -- parse crates/gox-syntax/tests/test_data/hello.gox

# Display token stream
cargo run -p gox-cli -- parse --tokens crates/gox-syntax/tests/test_data/hello.gox

# Check syntax
cargo run -p gox-cli -- check crates/gox-syntax/tests/test_data/hello.gox
```

### Run Tests

```bash
cargo test --workspace
```

## Example

```go
// hello.gox
package main;

func main() {
    println("Hello, World!");
}
```

```go
// structs.gox
package main;

type Person struct {
    name string;
    age int;
};

func (p Person) Greet() string {
    return "Hello, " + p.name;
}

interface Greeter {
    Greet() string;
};

implements Person : Greeter;

func main() {
    p := Person{name: "Alice", age: 30};
    println(p.Greet());
}
```

## Language Specification

See [english/language_spec.md](english/language_spec.md) for the complete language specification.

## Status

| Component | Status |
|-----------|--------|
| Lexer | ✅ Complete |
| Parser | ✅ Complete |
| AST | ✅ Complete |
| CLI | ✅ Basic functionality |
| Semantic Analysis | 🚧 In Progress |
| Code Generation | 📋 Planned |

## License

MIT License - see [LICENSE](LICENSE) for details.
