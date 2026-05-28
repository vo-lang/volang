# Verification

Use the narrowest command that exercises the touched behavior. Prefer
repo-root commands routed through `./d.py` or `vo-dev`.

## Contents

- [Before Running](#before-running)
- [Common Commands](#common-commands)
- [Syntax And Parser](#syntax-and-parser)
- [Type Checker, Codegen, VM, Runtime](#type-checker-codegen-vm-runtime)
- [No_std, WASM, And Web Compiler](#no_std-wasm-and-web-compiler)
- [Module System And Release Logic](#module-system-and-release-logic)
- [Stdlib And Native FFI](#stdlib-and-native-ffi)
- [Studio, WASM Bridge, Quickplay](#studio-wasm-bridge-quickplay)
- [Engineering System](#engineering-system)
- [Docs](#docs)
- [Examples, Benchmarks, And Artifacts](#examples-benchmarks-and-artifacts)

## Before Running

- Run commands from the repo root unless a command explicitly requires another
  directory.
- Use `cargo run -q -p vo-dev -- tool check --task <task>` when a task depends
  on Node, wasm-pack, or first-party sibling repos.
- Use `VOWORK=off` when reproducing language tests outside the manifest runner
  and workspace overrides are not part of the task.
- Avoid relying on network behavior during frozen build tests. Use explicit
  `vo mod` lifecycle commands for dependency download/resolution behavior.

## Common Commands

Task selectors can be groups such as `pr`, `quality`, `test`, or `site`, or a
single task as `task:<task-name>`.

```sh
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- tool check --task <task>
cargo run -q -p vo-dev -- task plan pr --changed
cargo run -q -p vo-dev -- task run task:<task-name>
cargo run -q -p vo-dev -- ci matrix pr --base <sha> --head <sha>
cargo run -q -p vo-dev -- verify plan pr
cargo run -q -p vo-dev -- verify run quality
./d.py ci smart
./d.py ci quality
./d.py ci test
./d.py ci pr
```

## Syntax And Parser

Use when touching `vo-syntax`, parsing, AST IDs, lexing, inline module syntax,
or formatting.

```sh
cargo test -p vo-syntax
./d.py test both tests/lang/cases/<case>.vo
cargo run -q -p vo-dev -- test run --suite lang --targets compile
cargo fmt --all -- --check
```

Add a manifest case if executable behavior, diagnostics, or formatting-relevant
syntax changes.

## Type Checker, Codegen, VM, Runtime

Use when touching `vo-analysis`, `vo-codegen`, `vo-common-core`, `vo-runtime`,
or `vo-vm`.

```sh
cargo test -p vo-analysis
cargo test -p vo-codegen
./d.py test both tests/lang/cases/<case>.vo
./d.py test both
cargo run -q -p vo-dev -- test lint --suite lang
```

The language suite is manifest-driven through `tests/lang/manifest.toml`; add or
update case metadata when adding files under `tests/lang/cases`.
`./d.py test` uses `eng/tests.toml` default targets and currently covers only
`vm,jit`; run `./d.py test osr` and `./d.py test gc` explicitly for OSR and GC
coverage.

Use JIT-focused checks for call, loop, opcode, stack, defer/panic, or interface
dispatch changes. Add OSR when loops/backedges, HINT metadata, or `ForLoop`
lowering are touched:

```sh
./d.py test jit tests/lang/cases/<case>.vo
./d.py test osr tests/lang/cases/<case>.vo
VO_JIT_CALL_THRESHOLD=1 ./d.py run tests/lang/cases/<case>.vo --mode=jit
```

Use GC-focused checks for slot metadata, root scanning, write barriers,
allocation, scheduler boundaries, defer/panic, or JIT materialization:

```sh
./d.py test gc
cargo test -p vo-runtime gc
cargo test -p vo-vm gc
cargo test -p vo-jit
cargo run -q -p vo-dev -- gc-perf --release --json dead-sweep
```

## No_std, WASM, And Web Compiler

Use when behavior should work in browser/no_std or when touching `vo-web`,
`vo-web/runtime-wasm`, no_std runtime paths, stdlib feature gates, or embed.

```sh
./d.py test nostd
./d.py test wasm
cargo check -p vo-web --target wasm32-unknown-unknown
```

## Module System And Release Logic

Use when touching `vo-module`, `vo-release`, `vo-engine` compile context,
`cmd/vo mod`, `vo.work`, lock/cache/readiness, registry, extension artifacts,
or module specs.

```sh
cargo test -p vo-module
cargo test -p vo-release
cargo run -q -p vo-dev -- lint all
./d.py vo mod sync path/to/module
./d.py vo mod verify path/to/module
./d.py vo mod download path/to/module
./d.py vo release verify path/to/module
./d.py ci task release-verify-vogui
```

For temp-project lifecycle tests, keep generated project files under a temp
directory and be explicit about whether network access is expected.

For inline `/*vo:mod*/` dependency behavior, test native CLI and web/memory
compile paths separately because their external-require support differs.

## Stdlib And Native FFI

Use when touching `lang/stdlib`, `vo-stdlib`, `vo-ext`, `vo-ffi-macro`, native
extension loading, or extern ABI names.

```sh
./d.py test both tests/lang/cases/<stdlib-case>.vo
./d.py test wasm
cargo check --workspace --all-targets --exclude vo-playground
```

For ABI or macro changes, add Rust tests near the macro/runtime code and a Vo
integration case that exercises the public surface.

## Studio, WASM Bridge, Quickplay

Use when touching `apps/studio`, `apps/studio/wasm`, `vo-web`,
`vo-app-runtime`, renderer bridge, quickplay, or Studio docs.

```sh
./d.py ci task wasm-check
./d.py ci task vo-test-wasm
./d.py ci task studio-wasm-build
./d.py ci task studio-build
./d.py ci task quickplay-validate
./d.py ci task blockkart-smoke-static
./d.py ci task docs-lint
./d.py studio --build-only
./d.py ci site
```

Use direct `cd apps/studio && npm run build` for local Svelte/Vite debugging,
but final repo validation should prefer the task graph.

After significant frontend behavior changes, run `./d.py studio` and verify the
local app in a browser when feasible.

Until a dedicated Tauri task exists, use direct Rust checks for native Studio
command/runtime changes.

## Engineering System

Use when touching `eng/*.toml`, `cmd/vo-dev`, `d.py`, GitHub workflows, task
planning, tool checks, artifact policy, or release policy.

Use group selectors such as `quality`, `test`, `site`, `pr`, and `full`, or a
single task selector such as `task:docs-lint`.

```sh
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- task plan pr
cargo run -q -p vo-dev -- task plan pr --changed
cargo run -q -p vo-dev -- ci matrix pr --base <sha> --head <sha>
cargo run -q -p vo-dev -- verify plan pr
./d.py help
```

If a task declares outputs, run the task and confirm `vo-dev` output validation
passes.

## Docs

Use when touching `lang/docs/spec`, `lang/docs/vo-for-gophers.md`,
`apps/studio/docs`, generated Playground docs, or docs sync scripts.

```sh
./d.py ci task docs-lint
node scripts/ci/docs_sync.mjs --check
node scripts/ci/docs_lint.mjs
```

If source docs feed generated docs, run the declared generator instead of
editing generated mirrors by hand.

## Examples, Benchmarks, And Artifacts

Use when touching `examples`, `benchmarks`, checked-in quickplay packages, or
artifact policy.

```sh
cargo run -q -p vo-dev -- lint examples
cargo run -q -p vo-dev -- lint benchmarks
cargo run -q -p vo-dev -- lint artifacts
./d.py ci task examples-smoke
./d.py ci task quickplay-validate
./d.py ci task blockkart-smoke-static
```

Benchmark runs can create generated result/build files. Keep those separate
from source changes unless the task is explicitly about benchmark artifacts.
