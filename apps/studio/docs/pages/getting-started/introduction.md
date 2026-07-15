# Introduction

> **An experimental scripting language for the Rust ecosystem.**

Vo is a statically typed, low-ceremony language designed to be embedded in Rust applications. Its Go-shaped syntax is extended with explicit error propagation, dynamic access, and isolated concurrency. The compiler and VM are pure Rust libraries; programs run on a bytecode VM, a Cranelift JIT, or a WASM browser runtime.

## What Vo Is For

- **Embed in Rust apps** — Vo's VM is a Rust library. Add a scripting layer to your Rust project without shipping a separate runtime.
- **Run in the browser** — First-class WASM target. Studio web mode compiles
  and runs Vo through the Studio WASM bridge. Native Studio sessions use the
  Tauri backend and native VM/JIT paths.
- **Go-shaped syntax** — declarations and control flow are familiar to Go users. Existing Go programs require a deliberate port for Vo's type, module, error, pointer, and concurrency rules.
- **AI-friendly** — the familiar surface grammar gives tools a useful starting point, while Vo's specification defines the semantic differences they must honor.

## Execution Backends

Vo separates compilation from execution. The same source compiles to bytecode, then runs on the backend that fits your use case:

| Backend | Status | Use Case |
|---------|--------|----------|
| VM | Alpha | Development, scripting, embedding |
| JIT | Alpha | Performance-sensitive native execution (Cranelift) |
| WASM | Alpha | Browser, sandboxed environments |
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

1. **Go-shaped syntax** — Familiar declarations and control flow, with Vo-specific semantics for types, modules, errors, pointers, and islands.
2. **`?` error propagation** — Replace `if err != nil` boilerplate with a single `?`. Use `errdefer` for error-only cleanup.
3. **No generics** — Use `any` (interface{}) and type assertions. Keeps the language simple and the compiler fast.
4. **Restricted pointers** — Only structs can be pointers (`*User`). No `*int` or `*string`.
5. **Dynamic access** — `~>` operator for duck-typing over JSON, maps, and untyped data.
