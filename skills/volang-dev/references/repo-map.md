# Repo Map

Use this as the compact routing reference for Volang maintenance.

## Contents

- [Mental Model](#mental-model)
- [Top-Level Ownership](#top-level-ownership)
- [Crate Routing](#crate-routing)
- [Core Pipelines](#core-pipelines)
- [Source Truth Caveats](#source-truth-caveats)

## Mental Model

Volang is a Rust workspace containing a language implementation, module and
release tooling, a standard library, browser/WASM support, a Svelte Studio app,
legacy Playground code, examples, benchmarks, and a manifest-driven test
system.

## Top-Level Ownership

| Path | Owns |
| --- | --- |
| `d.py` | Thin compatibility wrapper over `cmd/vo-dev`; must run from repo root. |
| `cmd/vo` | Public `vo` CLI: run, build, check, test, fmt, mod, release, dump. |
| `cmd/vo-dev` | Engineering control plane: task graph, CI, tests, tools, lint, release, Studio helpers. |
| `cmd/vo-test` | Native language-test plan executor. Not the public selector surface. |
| `cmd/vo-embed` | no_std/embed-oriented bytecode runner. |
| `eng` | Task, CI, toolchain, test target, artifact, project, and release policy data. |
| `lang/crates` | Compiler, runtime, VM, JIT, module, web, stdlib, FFI, app runtime crates. |
| `lang/stdlib` | Vo source facade for stdlib packages. |
| `lang/docs/spec` | Intended language/module/runtime specs; verify source before exact claims. |
| `lang/docs/dev-notes` | Historical design notes and handoffs. |
| `lang/docs/outdated` | Explicitly stale design/archive material. Do not use as current truth. |
| `tests/lang` | Manifest-driven language cases, fixture projects, archives, bytecode fixtures. |
| `apps/studio` | Current Studio web app, Studio WASM, docs, quickplay assets, Tauri shell. |
| `apps/studio/public/quickplay` | Checked-in static quickplay packages governed by artifact policy and provenance. |
| `apps/playground-legacy` | Older Playground app and independent WASM bridge. |
| `scripts/ci` | Narrow docs and quickplay helper scripts; broad CI control lives in `vo-dev`. |
| `.github/workflows` | GitHub execution wrappers that should consume `vo-dev` matrices/metadata. |
| `examples` | Sample Vo programs and example manifest. |
| `benchmarks` | Benchmark sources plus generated benchmark artifacts. |
| `skills/volang-dev` | Checked-in Volang skill source. Install separately for automatic discovery. |

## Crate Routing

| Crate | Main responsibility |
| --- | --- |
| `vo-common` | Diagnostics, source map, symbol interning, VFS, ABI helpers. |
| `vo-common-core` | Bytecode model, instruction format, runtime type metadata, serialization, debug info. |
| `vo-syntax` | Lexer, parser, AST, inline module syntax, formatter. |
| `vo-analysis` | Project analysis, import loading, type checking, object/type arenas, scopes, selections, escape/sendability post-passes. |
| `vo-codegen` | Type-checked project to bytecode module, runtime metadata, wrappers, slot metadata, debug info. |
| `vo-engine` | Native compile/check/run API, module context, compile cache, native extension prep. |
| `vo-runtime` | GC, object layouts, builtins, extern effect contracts, FFI registry/loader, stdlib runtime support, islands. |
| `vo-vm` | Interpreter, runtime-boundary transitions, scheduler, fibers, root scanning, VM-side JIT integration. |
| `vo-jit` | Cranelift full-function and loop OSR compilation, opcode semantic rows, contract graph, verifier, lowering, call helpers, and runtime path policy. |
| `vo-module` | Module identity, manifests, lock files, workspaces, solver, registry, cache, lifecycle ops. |
| `vo-release` | Module release verification and staging. |
| `vo-stdlib` | Embedded stdlib source plus Rust extern implementations. |
| `vo-ext` | Native extension SDK and C ABI table export. |
| `vo-ffi-macro` | `#[vo_fn]` and `#[vostd_fn]` proc macros. |
| `vo-web` | Browser/WASM compile/run APIs, browser VFS/registry/runtime planning. |
| `vo-app-runtime` | Shared app/session runtime for GUI hosts and render island execution. |

## Core Pipelines

Native compile/run:

1. CLI or library calls `vo_engine::compile*`, `check*`, or `run*`.
2. `vo-engine` classifies project, single-file, inline module, or bytecode input.
3. `vo-module` loads pure-TOML `vo.mod`, minimal `vo.lock` v3, and the active
   members-only `vo.work` v1 context.
4. `vo-engine` authenticates locked release/package/source/artifact state,
   checks frozen dependency readiness, and collects a `FileSet`.
5. `vo-analysis` parses, imports, and type-checks packages.
6. `vo-codegen` builds a `vo-common-core::Module`.
7. `vo-engine::run` creates a `vo-vm::Vm`, registers stdlib/extensions, and runs the scheduler.

JIT execution:

1. `vo-engine::run` enables VM-side JIT policy through `vo-vm`.
2. `vo-vm/src/vm/jit_mgr.rs` tracks hot functions/loops and asks `vo-jit` to
   compile full functions or OSR loops.
3. `vo-jit/src/semantics` owns opcode facts; `metadata_contract`, `effects`,
   `capability`, `verifier`, `contract_graph`, and lowering derive from those
   facts.
4. `vo-jit/src/translate/*` lowers instructions to Cranelift; call behavior
   is split through `call_helpers/*`.
5. Runtime transitions return through `vo-vm/src/vm/jit/*`, including bridge
   results, materialization, callbacks, side exits, panic setup, and transition
   handling.

VM/runtime boundary:

1. Runtime side effects should flow through `RuntimeTransition`,
   `RuntimeCommand`, `FiberWakeKey`, and scheduler generation checks instead of
   raw fiber IDs or ad hoc scheduler mutation.
2. JIT callbacks may publish pending runtime transitions while generated code
   continues; pending effects commit with the next side-effect-carrying VM
   boundary and are discarded as a unit on terminal discard.
3. Boundary architecture notes live in
   `lang/docs/dev/vm-runtime-boundary-architecture.md`; repair/status notes
   live in `lang/docs/dev/vm-runtime-boundary-repair-plan.md`.

Web/Studio compile/run:

1. Studio backend prepares a workspace/session and dependency/runtime assets.
2. Studio WASM or `vo-web` prepares a memory/VFS-backed compile input.
3. `vo-web` runs analysis/codegen and returns bytecode/diagnostics.
4. GUI paths use `vo-app-runtime` plus renderer bridge and host event plumbing.
5. Native Studio routes filesystem/process/Git through Tauri commands.

Bare `vo-web` single-file compilation is dependency-free. Studio browser
projects prepare dependencies from their committed project graph. Browser
preparation consumes `vo.release.json` v2 and `vo.package.json` v1 directly,
sharing the native integrity chain.

Engineering workflow:

1. `./d.py` delegates to `cmd/vo-dev`.
2. `vo-dev` reads `eng/*.toml` and `tests/lang/manifest.toml`.
3. Tasks, tools, CI matrices, test targets, artifacts, release policy, and
   first-party repo paths are data-driven.

Docs and artifact flow:

1. Source docs live in `lang/docs/spec`, `lang/docs/vo-for-gophers.md`, and
   `apps/studio/docs/pages`.
2. `scripts/ci/docs_sync.mjs` generates Playground-visible docs into
   `apps/playground-legacy/src/assets/docs/generated`.
3. `scripts/ci/docs_lint.mjs` checks Studio docs manifests and generated docs.
4. Checked-in generated artifacts are declared in `eng/artifacts.toml` and must
   have matching generator/validator coverage and provenance when required.
5. Module release staging is source-owned by `vo-release`, while engineering
   release automation is data-owned by `eng/release.toml` and interpreted by
   `cmd/vo-dev`.

## Source Truth Caveats

- Specs describe intended behavior; source decides exact current behavior.
- `lang/docs/outdated` is intentionally not current.
- Generated docs in Playground are mirrors.
- `vo.work` can change local dependency resolution unless disabled.
- Build-like commands and lifecycle commands have different authority over
  dependency resolution and lock mutation.
- Native extension examples in older specs may use obsolete macro names.
- JIT docs and source comments are not perfectly aligned; inspect
  `lang/docs/dev/jit-fact-source.md`, current dispatch, semantic rows, VM bridge
  paths, and tests before making status claims.
