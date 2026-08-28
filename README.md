# Vo Programming Language

> **An experimental scripting language for the Rust ecosystem.**

Vo is a statically typed, low-ceremony language designed to be embedded in Rust applications. Its Go-shaped syntax is extended with explicit error propagation, dynamic access, and isolated concurrency. The compiler and VM are pure Rust libraries; programs run on a bytecode VM, a Cranelift JIT, Native AOT executables, or WebAssembly deployment images.

## What Vo Is For

- **Embed in Rust apps** — Vo's VM is a Rust library. Add a scripting layer to your Rust project without shipping a separate runtime.
- **Run in the browser** — First-class WASM target. Browser execution uses the
  `vo-web` WASM VM or AOT path; native CLI sessions use native VM/JIT/AOT paths.
- **Go-shaped syntax** — declarations and control flow are familiar to Go users, with a deliberately different type, module, error, pointer, and concurrency model.
- **AI-friendly** — the familiar surface grammar gives tools a useful starting point, while Vo's specification defines the semantic differences they must honor.

## If you know Go, the surface will feel familiar

Just remember these 4 differences:

1. **Error Handling** — Use `?` instead of `if err != nil`. Use `errdefer` for error-only cleanup.
2. **No Generics** — Use `any` (interface{}) and type assertions.
3. **Restricted Pointers** — Only structs can be pointers (`*User`). No `*int` or `*string`.
4. **Dynamic Access** — Use `~>` operator for duck-typing (JSON, maps, untyped data).

## Getting Started

- Begin with the maintained [Volang documentation index](lang/docs/README.md),
  then follow the [installation](lang/docs/guides/installation.md),
  [hello-world](lang/docs/guides/hello-world.md), and
  [language-tour](lang/docs/guides/language-tour.md) guides.
- A normal project commits `vo.mod` and, when it has dependencies, one generated `vo.lock`.
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
| Native AOT | Alpha | Ahead-of-time native executables and relocatable objects |
| Wasm AOT | Alpha | Browser, Node, and embedded Core Wasm deployment images |

**VM** — register-based bytecode interpreter with fiber-based goroutines, island concurrency, and an incremental tri-color GC.

**JIT** — mixed-mode: starts in the VM, selectively compiles hot functions and loops to native code via [Cranelift](https://cranelift.dev). Supports loop OSR and direct JIT-to-JIT calls.

**WASM** — `vo-runtime` and `vo-vm` compiled to `wasm32-unknown-unknown` in `no_std` mode. This path supports browser VM execution without a JIT.

**Native AOT** — compiles every verified function through the shared Cranelift lowering, embeds versioned GC/deoptimization metadata, and links the packaged static runtime. `vo build` produces a host executable by default; `--kind=object` emits a cross-target object.

**Wasm AOT** — `vo build --kind=wasm` lowers verified Volang bytecode into executable Core Wasm functions consumed by `vo-web` and other ABI v5 hosts. The image contains no bytecode interpreter. See the [AOT guide](docs/aot.md) for the target, runtime ABI, caching, extensions, and release guarantees.

## Performance

The benchmark suite currently has 21 manifest entries under `benchmarks/`.
`./d.py bench all` runs the suite through the `vo-dev` benchmark runner, which
uses `hyperfine` with a default of one warmup and three measured runs
(`--warmup N` / `--runs N` override those values). Every run includes VM and a
cache-disabled Core Wasm AOT image executed end to end through the
current `vo-web` runtime under Node. Every 64-bit run also includes JIT and
cache-disabled native AOT. The runner checks that every included Vo backend
produces identical output and records native executable and Wasm image compile
time plus size separately. It writes transient JSON, Markdown, and
`summary.json` output under
`target/bench/runs/`, keeps native build artifacts under
`target/bench/artifacts/`, and uses
`target/bench/go-cache/`
as the repo-local Go cache. Use `./d.py bench score` to compute the local
relative-time summary from those transient results; `vo-dev clean bench` removes
the generated benchmark tree.

Benchmark results are hardware-dependent local measurements, not checked-in
release claims.

## Volang UI

The repository contains the complete independent renderer-neutral Volang UI
framework: an official pure-Volang UIKit, VM/JIT development execution,
state-preserving reload, Core Wasm AOT Web deployment, Native AOT desktop
deployment, WGPU presentation, accessibility, system services, and governed
release certification. Applications stay in `.vo` files and do not import
JavaScript packages. Start a project with:

```sh
vo ui new hello-ui
cd hello-ui
vo mod sync
vo ui dev --open
```

Create a deployable Web release with compiled application code:

```sh
vo ui build -o dist
```

See the [UI overview](ui/README.md), [architecture](ui/docs/architecture.md),
[platform capabilities](ui/docs/platform-capabilities.md), and [release
policy](ui/docs/release-policy.md). Maintainers can validate the frozen
foundation, active VUI 1.0 capability graph, E0-E8 delivery plan, contract
probes, target matrix, and evidence paths with `vo-dev ui-certify --check`.

## Development

Maintainers can find the fixed CI lanes, local equivalents, deployment policy,
and release requirements in [the CI guide](docs/ci.md).

## License

MIT License - see [LICENSE](LICENSE) for details.
