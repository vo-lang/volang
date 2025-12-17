# GoX Programming Language

GoX is a statically typed, Go-like programming language with multiple compilation backends.

## Overview

GoX provides familiar Go syntax with simplified error handling and flexible compilation targets.

### Key Features

- **Go-like syntax** - Familiar to Go programmers
- **Static typing** with local type inference
- **Simplified error handling** - `fail`, `errdefer`, and `?` operator
- **Multiple backends** - VM interpreter, JIT, native executables, WebAssembly
- **No generics** - Simplified type system
- **Goroutines & channels** - Concurrent programming support

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
│   ├── gox-codegen-cranelift/# Shared Cranelift IR translation
│   │
│   │  # ─────────── VM ───────────
│   ├── gox-vm/               # VM core (interpreter, bytecode)
│   │
│   │  # ─────────── Native Backends ───────────
│   ├── gox-jit/              # JIT compilation (Cranelift)
│   ├── gox-aot/              # AOT → native object files
│   │
│   │  # ─────────── Runtime ───────────
│   ├── gox-runtime-core/     # Core runtime (GC, objects, FFI)
│   ├── gox-runtime-native/   # Native runtime symbols (AOT/JIT)
│   ├── gox-runtime-vm/       # VM runtime + native functions
│   │
│   │  # ─────────── Web ───────────
│   ├── gox-web/              # WASM bindings (run GoX in browsers)
│   │
│   │  # ─────────── Tools ───────────
│   ├── gox-cli/              # Command-line interface
│   └── gox-tests/            # Integration tests
│
├── stdlib/                   # GoX standard library
│   ├── fmt/
│   ├── strings/
│   ├── bytes/
│   ├── errors/
│   └── encoding/
│
├── docs/                     # Documentation
│   ├── design/               # Design docs (vm.md, gc.md, backends.md)
│   ├── impl/                 # Implementation docs
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
     gox-codegen-vm         gox-jit              gox-aot
            │                    │                    │
            │                    └────────┬───────────┘
            ▼                             ▼
         gox-vm              gox-codegen-cranelift (shared)
            │                             │
            ▼                             ▼
    gox-runtime-vm              gox-runtime-native
            │                             │
            └──────────┬──────────────────┘
                       ▼
               gox-runtime-core
                       │
                       ▼
                 gox-analysis ◄──────── gox-module
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
# Run a GoX program (VM interpreter)
gox run program.gox

# Run with JIT compilation (planned)
gox run --jit program.gox

# Compile to native executable (planned)
gox build program.gox
```

### Web (WASM)

GoX can run in browsers via WebAssembly using `gox-web`:

```javascript
import init, { GoxVM, compile_and_run } from 'gox-web';

await init();
const output = compile_and_run(`
    package main
    func main() { println("Hello from GoX!") }
`);
```

## Language Example

```gox
package main

type User struct {
    name string
    age  int
}

func (u User) Greet() string {
    return "Hello, " + u.name
}

func main() {
    user := User{name: "Alice", age: 30}
    println(user.Greet())
    
    numbers := []int{1, 2, 3}
    for i, v := range numbers {
        println(i, v)
    }
}
```

### Error Handling

GoX provides simplified error handling with `fail`, `errdefer`, and `?`:

```gox
func readConfig(path string) (Config, error) {
    file := open(path)?           // propagate error with ?
    errdefer file.Close()          // cleanup on error only
    
    data := readAll(file)?
    config := parse(data)?
    
    if config.Version < 1 {
        fail errors.New("invalid version")  // early error return
    }
    
    return config, nil
}
```

## License

MIT License - see [LICENSE](LICENSE) for details.
