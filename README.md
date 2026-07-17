# Vo Programming Language

> **An experimental scripting language for the Rust ecosystem.**

Vo is a statically typed, low-ceremony language designed to be embedded in Rust applications. Its Go-shaped syntax is extended with explicit error propagation, dynamic access, and isolated concurrency. The compiler and VM are pure Rust libraries; programs run on a bytecode VM, a Cranelift JIT, or a WASM browser runtime.

💻 **[Open Studio](https://volang.dev)** — docs are there too.

## What Vo Is For

- **Embed in Rust apps** — Vo's VM is a Rust library. Add a scripting layer to your Rust project without shipping a separate runtime.
- **Run in the browser** — First-class WASM target. Browser execution uses the
  `vo-web` WASM VM path; native CLI and Studio native sessions use native
  VM/JIT paths.
- **Go-shaped syntax** — declarations and control flow are familiar to Go users, with a deliberately different type, module, error, pointer, and concurrency model.
- **AI-friendly** — the familiar surface grammar gives tools a useful starting point, while Vo's specification defines the semantic differences they must honor.

## If you know Go, the surface will feel familiar

Just remember these 4 differences:

1. **Error Handling** — Use `?` instead of `if err != nil`. Use `errdefer` for error-only cleanup.
2. **No Generics** — Use `any` (interface{}) and type assertions.
3. **Restricted Pointers** — Only structs can be pointers (`*User`). No `*int` or `*string`.
4. **Dynamic Access** — Use `~>` operator for duck-typing (JSON, maps, untyped data).

## Studio

**Vo Studio** is the official IDE for Vo. It is currently a work in progress, available as both a desktop app (via Tauri) and a web app at [volang.dev](https://volang.dev).

## Getting Started

- If you want a normal module with committed `vo.mod` / `vo.lock`, start with a project directory.
- Single-file scripts use only the standard library. For third-party
  dependencies, create a project with `vo.mod` and commit its generated
  `vo.lock`; see [`lang/docs/spec/module-inline-mod-tutorial.md`](lang/docs/spec/module-inline-mod-tutorial.md).
- If you know Go already, see [`lang/docs/vo-for-gophers.md`](lang/docs/vo-for-gophers.md).

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
| VM | Alpha | Development, scripting, embedding, `no_std` |
| JIT | Alpha | Performance-sensitive native execution (Cranelift) |
| WASM | Alpha | Browser, sandboxed environments |
| AOT | Planned | Ahead-of-time native binaries |

**VM** — register-based bytecode interpreter with fiber-based goroutines, island concurrency, and an incremental tri-color GC.

**JIT** — mixed-mode: starts in the VM, selectively compiles hot functions and loops to native code via [Cranelift](https://cranelift.dev). Supports loop OSR and direct JIT-to-JIT calls.

**WASM** — `vo-runtime` and `vo-vm` compiled to `wasm32-unknown-unknown` in `no_std` mode. Runs in-browser in Studio and Playground; no JIT in this path.

## Performance

The benchmark suite currently has 17 manifest entries under `benchmarks/`.
`./d.py bench all` runs the suite through the `vo-dev` benchmark runner, which
uses `hyperfine` with a default of one warmup and three measured runs
(`--warmup N` / `--runs N` override those values). It writes transient JSON,
Markdown, and `summary.json` output under `target/bench/results/`, keeps native
build artifacts under `target/bench/artifacts/`, and uses `target/bench/go-cache/`
as the repo-local Go cache. Use `./d.py bench score` to compute the local
relative-time summary from those transient results; `vo-dev clean bench` removes
the generated benchmark tree.

Benchmark results are hardware-dependent local measurements, not checked-in
release claims.

## License

MIT License - see [LICENSE](LICENSE) for details.
