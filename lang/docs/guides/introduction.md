# Introduction

Volang is a statically typed, low-ceremony language with Go-shaped syntax and
its own module, error, pointer, dynamic-access, memory, and concurrency
semantics. One source tree can run quickly during development and ship as an
ahead-of-time compiled native or WebAssembly artifact.

## The working model

A Volang program passes through four stable layers:

1. The frontend parses and type-checks packages.
2. Code generation produces a verified register-based bytecode module.
3. Development executes that module with the VM or Cranelift JIT.
4. Release builds lower the verified module to a Native AOT executable/object
   or a Core Wasm AOT image.

The VM, JIT, and AOT paths share language semantics, standard-library
contracts, stack maps, and runtime metadata. A backend choice changes startup,
throughput, packaging, and host integration; it does not select a language
dialect.

## Where Volang fits

- Embed application logic in a Rust host through `vo-engine` and narrow extern
  capabilities.
- Build command-line programs that develop with VM/JIT and release as native
  executables.
- Build browser and desktop products with the official pure-Volang UI
  framework.
- Run sandboxed workloads with a bounded VM or Core Wasm AOT host.
- Express concurrent services with goroutines, channels, contexts, and
  isolated cross-thread or cross-instance islands.

## Language characteristics

- Static types with local inference and predictable zero values.
- Struct values, restricted struct pointers, interfaces, slices, maps,
  functions, channels, ports, and islands.
- Explicit error values plus `?`, `fail`, and `errdefer`.
- Dynamic access through `~>` when typed structures are unavailable.
- Cooperative goroutines inside an island and explicit messages across
  islands.
- A source-distributed standard library with native and Wasm host providers
  where platform effects are required.

## Projects and scripts

A single `.vo` file is the shortest path for standard-library-only programs:

```vo
func main() {
    println("hello from Volang")
}
```

A maintained application uses a directory with `vo.mod`. When external
dependencies exist, `vo.lock` records one exact authenticated graph. Builds
consume that frozen graph and never solve versions implicitly.

```toml
format = 1
module = "example.com/acme/tool"
version = "0.1.0"
vo = "0.1.4"
```

## Development and release

Use `vo check`, `vo fmt`, and `vo test` for the normal feedback loop. Use
`vo run --mode=vm` for short startup and `--mode=jit` for longer native
sessions. `vo build` creates a native AOT executable by default;
`vo build --kind=wasm` creates a Core Wasm AOT image.

UI applications add `vo ui dev`, `vo ui run`, `vo ui test`, `vo ui build`, and
`vo ui package`. Their product code stays in `.vo` files. Browser and desktop
hosts supply rendering and platform capabilities through versioned contracts.

## Documentation authority

Learning guides live under `lang/docs/guides`. Normative language and runtime
documents live under `lang/docs/spec`. The official UI has its own maintained
source under `ui/docs`. Studio embeds generated copies of those sources and
shows the originating repository path for every page.
