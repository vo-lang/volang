# Vo Programming Language

Vo is a statically typed, Go-like programming language with multiple compilation backends.

## Overview

Vo provides familiar Go syntax with simplified error handling and flexible compilation targets.

### Key Features

- **Go-like syntax** - Familiar to Go programmers
- **Static typing** with local type inference
- **Simplified error handling** - `fail`, `errdefer`, and `?` operator
- **Multiple backends** - VM interpreter, JIT, native executables, WebAssembly
- **No generics** - Simplified type system
- **Goroutines & channels** - Concurrent programming support

## Project Structure

```
vo/
├── crates/
│   │
│   │  # ─────────── Frontend ───────────
│   ├── vo-common/           # Shared types, errors, spans
│   ├── vo-syntax/           # Lexer, parser, AST
│   ├── vo-analysis/         # Type checking, semantic analysis
│   ├── vo-module/           # Module/package management
│   │
│   │  # ─────────── Code Generation ───────────
│   ├── vo-codegen-vm/       # VM bytecode generation
│   ├── vo-codegen-cranelift/# Shared Cranelift IR translation
│   │
│   │  # ─────────── VM ───────────
│   ├── vo-vm/               # VM core (interpreter, bytecode)
│   │
│   │  # ─────────── Native Backends ───────────
│   ├── vo-jit/              # JIT compilation (Cranelift)
│   ├── vo-aot/              # AOT → native object files
│   │
│   │  # ─────────── Runtime ───────────
│   ├── vo-runtime-core/     # Core runtime (GC, objects, FFI)
│   ├── vo-runtime-native/   # Native runtime symbols (AOT/JIT)
│   ├── vo-runtime-vm/       # VM runtime + native functions
│   │
│   │  # ─────────── Web ───────────
│   ├── vo-web/              # WASM bindings (run Vo in browsers)
│   │
│   │  # ─────────── Tools ───────────
│   ├── vo-cli/              # Command-line interface
│   └── vo-tests/            # Integration tests
│
├── stdlib/                   # Vo standard library
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
                              vo-cli
                                 │
            ┌────────────────────┼────────────────────┐
            │                    │                    │
            ▼                    ▼                    ▼
     vo-codegen-vm         vo-jit              vo-aot
            │                    │                    │
            │                    └────────┬───────────┘
            ▼                             ▼
         vo-vm              vo-codegen-cranelift (shared)
            │                             │
            ▼                             ▼
    vo-runtime-vm              vo-runtime-native
            │                             │
            └──────────┬──────────────────┘
                       ▼
               vo-runtime-core
                       │
                       ▼
                 vo-analysis ◄──────── vo-module
                       │
                       ▼
                  vo-syntax
                       │
                       ▼
                  vo-common
```

## Building

```bash
cargo build --release
```

## Usage

```bash
# Run a Vo program (VM interpreter)
vo run program.vo

# Run with JIT compilation (planned)
vo run --jit program.vo

# Compile to native executable (planned)
vo build program.vo
```

### Web (WASM)

Vo can run in browsers via WebAssembly using `vo-web`:

```javascript
import init, { VoVM, compile_and_run } from 'vo-web';

await init();
const output = compile_and_run(`
    package main
    func main() { println("Hello from Vo!") }
`);
```

## Language Example

```vo
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

Vo provides simplified error handling with `fail`, `errdefer`, and `?`:

```vo
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
