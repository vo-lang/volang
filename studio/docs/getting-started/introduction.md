# Introduction

> **The scripting language for the Rust ecosystem.**

Vo is the Python of the Rust world — a statically typed, low-ceremony language designed to be embedded in Rust applications. The compiler and VM are pure Rust libraries with no GIL. Programs run on a bytecode VM, a Cranelift JIT, or compile to WASM for the browser.

## What Vo Is For

- **Embed in Rust apps** — Vo's VM is a Rust library. Add a scripting layer to your Rust project without shipping a separate runtime.
- **Run in the browser** — First-class WASM target. Vo programs run in the browser with the same semantics as on native. This page is proof: the Studio editor and all examples execute Vo via WASM.
- **Rapid prototyping** — Go-like syntax means near-zero learning curve for anyone who has written Go, TypeScript, or Python. Most Go programs run with minimal changes.
- **AI-friendly** — Simple grammar, explicit types, no metaprogramming magic. LLMs can read and write Vo reliably.

## Execution Backends

Vo separates compilation from execution. The same source compiles to bytecode, then runs on the backend that fits your use case:

| Backend | Status | Use Case |
|---------|--------|----------|
| VM | Stable | Development, scripting, embedding |
| JIT | Stable | Performance-sensitive native execution (Cranelift) |
| WASM | Stable | Browser, sandboxed environments |
| AOT | Planned | Ahead-of-time native binaries |

## Quick Example

```vo
func readConfig(path string) (Config, error) {
    file := open(path)?           // propagate error with ?
    errdefer file.Close()         // cleanup only if later steps fail

    data := readAll(file)?
    config := parse(data)?

    if config.Version < 1 {
        fail errors.New("invalid version")
    }
    return config, nil
}
```

## Key Language Features

1. **Go-like syntax** — Familiar to Go/TypeScript developers. Structs, interfaces, goroutines, channels, defer.
2. **`?` error propagation** — Replace `if err != nil` boilerplate with a single `?`. Use `errdefer` for error-only cleanup.
3. **No generics** — Use `any` (interface{}) and type assertions. Keeps the language simple and the compiler fast.
4. **Restricted pointers** — Only structs can be pointers (`*User`). No `*int` or `*string`.
5. **Dynamic access** — `~>` operator for duck-typing over JSON, maps, and untyped data.
