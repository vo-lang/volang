# Vo Programming Language

> **The scripting language for the Rust ecosystem.**

Vo is the Python of the Rust world — a statically typed, low-ceremony language designed to be embedded in Rust applications. The compiler and VM are pure Rust libraries with built-in island concurrency. Programs run on a bytecode VM, a Cranelift JIT, or compile to WASM for the browser.

💻 **[Open Studio](https://volang.dev)** — docs are there too.

## What Vo Is For

- **Embed in Rust apps** — Vo's VM is a Rust library. Add a scripting layer to your Rust project without shipping a separate runtime.
- **Run in the browser** — First-class WASM target. Vo programs run in the browser with the same semantics as on native.
- **Almost Go** — Vo stays very close to Go. Most Go programs run with minimal changes.
- **AI-friendly** — AI already knows Go well, and because Vo stays close to Go and can be run directly in normal use, it is easy for AI to read, write, and use.

## If you know Go, you already know 95% of Vo

Just remember these 4 differences:

1. **Error Handling** — Use `?` instead of `if err != nil`. Use `errdefer` for error-only cleanup.
2. **No Generics** — Use `any` (interface{}) and type assertions.
3. **Restricted Pointers** — Only structs can be pointers (`*User`). No `*int` or `*string`.
4. **Dynamic Access** — Use `~>` operator for duck-typing (JSON, maps, untyped data).

## Studio

**Vo Studio** is the official IDE for Vo. It is currently a work in progress, available as both a desktop app (via Tauri) and a web app at [volang.dev](https://volang.dev).

## Quick Examples

### Error Handling

Use `?` to propagate errors, `errdefer` for error-only cleanup:

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

### Dynamic Access (`~>`)

Duck-typing for `any`/interface values, perfect for JSON:

```vo
func getName(data any) (string, error) {
    var name string
    name = data~>users~>[0]~>name?  // access path, auto-cast to string
    return name, nil
}
```

### Familiar Go Syntax

```vo
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

    for i, v := range []int{1, 2, 3} {
        println(i, v)
    }
}
```

## Execution Backends

Vo compiles to a single bytecode format; backends differ only in how that bytecode is executed:

| Backend | Status | Use Case |
|---------|--------|----------|
| VM | Stable | Development, scripting, embedding, `no_std` |
| JIT | Stable | Performance-sensitive native execution (Cranelift) |
| WASM | Stable | Browser, sandboxed environments |
| AOT | Planned | Ahead-of-time native binaries |

**VM** — register-based bytecode interpreter with fiber-based goroutines, island concurrency, and an incremental tri-color GC.

**JIT** — mixed-mode: starts in the VM, selectively compiles hot functions and loops to native code via [Cranelift](https://cranelift.dev). Supports loop OSR and direct JIT-to-JIT calls.

**WASM** — `vo-runtime` and `vo-vm` compiled to `wasm32-unknown-unknown` in `no_std` mode. Runs in-browser in Studio and Playground; no JIT in this path.

## Performance

Geometric-mean relative time across 12 benchmarks (lower is faster). Measured on Apple M1, single-core, `--release`:

| Rank | Language | Geometric Mean |
|------|----------|---------------|
| 1 | C | 1.11× |
| 2 | Go | 1.80× |
| 3 | LuaJIT | 2.37× |
| 4 | Java | 3.59× |
| 5 | **Vo-JIT** | **4.25×** |
| 6 | Node | 5.46× |
| 7 | Lua | 24.48× |
| 8 | **Vo-VM** | **34.96×** |

Vo-JIT is competitive with Go and Java on tight integer/array loops (e.g. sieve 2.46×, task-queue 1.67×, sum-array 2.56×). GC-heavy benchmarks show a wider gap due to allocation pressure on the incremental collector.

*Results are informal and hardware-dependent; not authoritative. Run `./d.py bench all` to reproduce.*

## License

MIT License - see [LICENSE](LICENSE) for details.
