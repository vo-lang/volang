# Vo Programming Language

> **Alpha (2026.1.1)** — 100% pure vibe coding in ~20 days.

Vo is a statically typed, Go-like programming language implemented in Rust (by AI, under my supervision).

## Overview

Vo aims to run Go-like code with a compact toolchain and multiple execution backends.

### Key Features

- **Go-like syntax** with a familiar developer experience
- **Static typing** with local type inference
- **Error handling sugar** - `fail`, `errdefer`, and the `?` operator
- **Concurrency** - goroutines and channels
- **No generics** (keep the type system simple)
- **Restricted pointers** - pointers are only allowed for struct types (no `*int`); no pointer arithmetic

## Execution Backends / Status

| Backend | Status | Notes |
|--------|--------|------|
| VM | 🚧 Functionality is mostly complete | Bytecode interpreter; still under active development/optimization |
| JIT | 🚧 Functionality is mostly complete | Cranelift-based JIT; still under active development/optimization |
| WASM | 📋 Planned | Not implemented yet |
| AOT | 📋 Planned | Not implemented yet |

## Performance (Table 1, reference only)

*Note: results are from an informal / non-strict benchmarking environment. Numbers are not authoritative.*

Relative time ranking (lower is faster, `1.0x` = fastest):

| Rank | Language | Relative |
|------|----------|----------|
| 1 | C | 1.80x |
| 2 | Go | 2.01x |
| 3 | LuaJIT | 2.95x |
| 4 | Java | 5.39x |
| 5 | Vo-JIT | 5.69x |
| 6 | Lua | 39.52x |
| 7 | Vo-VM | 40.48x |
| 8 | Ruby | 119.05x |
| 9 | Python | 132.59x |

## Project Structure

```
vo/
├── crates/
│   ├── vo-syntax/        # lexer/parser/AST
│   ├── vo-analysis/      # type checking, semantic analysis
│   ├── vo-codegen/       # bytecode generation
│   ├── vo-vm/            # bytecode VM
│   ├── vo-jit/           # JIT (Cranelift)
│   ├── vo-runtime/       # runtime (GC, builtins)
│   ├── vo-runtime-native/# runtime symbols (native)
│   ├── vo-module/        # module/package system
│   └── vo-cli/           # CLI (vo)
│
├── stdlib/               # standard library (selected packages)
├── test_data/            # integration tests
├── benchmark/            # benchmarks
└── docs/                 # specs and design notes
```

## Building

```bash
cargo build --release
```

## Usage

```bash
# VM mode
cargo run --bin vo -- run program.vo

# JIT mode
cargo run --bin vo -- run --mode=jit program.vo

# Print bytecode (debugging VM)
cargo run --bin vo -- run program.vo --codegen
```

## Development Scripts (`d.py`)

```bash
# All tests (VM + JIT)
./d.py test

# VM only
./d.py test vm

# JIT only
./d.py test jit

# GC verification tests only (enables VO_GC_DEBUG=1)
./d.py test gc

# Benchmarks
./d.py bench
./d.py bench vo
./d.py bench score

# Code statistics
./d.py loc
./d.py loc --with-tests
```

## Language Example

```vo
package main

type User struct {
    name string
    age  int
}

func (u *User) Greet() string {
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
