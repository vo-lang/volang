---
name: volang-dev
description: Detailed developer guide for the Volang repository. Use when Codex needs to modify, debug, review, or explain Volang compiler, parser, type checker, bytecode, VM, JIT, GC, module system, standard library, native FFI, CLI, tests, WASM, Studio, Playground, or related docs in this repo.
---

# Volang Dev

Use this skill as the repo-specific onboarding and navigation layer for Volang development. Start from the affected path, read the matching reference, then verify with the narrowest command that exercises the changed subsystem.

This checked-in copy lives at `skills/volang-dev`. For automatic discovery outside this repo, copy or symlink the `volang-dev` folder into `$CODEX_HOME/skills` or `~/.codex/skills`.

## Quick Triage

Read [references/repo-map.md](references/repo-map.md) first unless the task is already scoped to one crate or command.

Then load only the relevant detailed reference:

- Parser, AST, lexer, formatting, syntax docs: [references/frontend-analysis.md](references/frontend-analysis.md)
- Type checker, scopes, objects, imports, analysis results: [references/frontend-analysis.md](references/frontend-analysis.md)
- Compile pipeline, codegen, bytecode, VM, runtime, GC, JIT, islands: [references/compile-runtime.md](references/compile-runtime.md)
- `vo.mod`, `vo.lock`, `vo.work`, dependency cache, stdlib, native FFI: [references/modules-stdlib-ffi.md](references/modules-stdlib-ffi.md)
- `vo-web`, `vo-app-runtime`, Studio, Playground, browser VFS, Tauri: [references/web-studio.md](references/web-studio.md)
- CLI commands, `d.py`, `vo-test`, CI, benchmarks, validation choice: [references/cli-testing.md](references/cli-testing.md)

## Source Of Truth Rules

- Prefer current Rust/Vo/TypeScript source over older design notes and mirrored Playground docs.
- Treat `lang/docs/spec/*.md` as useful background, not guaranteed implementation truth.
- Treat `apps/playground-legacy/src/assets/docs/generated/module.md` as a redirect/mirror; the authoritative module spec is `lang/docs/spec/module.md`.
- Do not trust old FFI examples that use `#[vo_extern]`; current extension/stdlib macros are `#[vo_fn]` and `#[vostd_fn]`.
- Do not copy API examples from `lang/crates/vo-syntax/README.md` without checking source. Current parser constructors take source text plus a base offset.
- Treat `eng/README.md`, `eng/tasks.toml`, `eng/toolchains.toml`, `eng/tests.toml`, `eng/artifacts.toml`, `eng/project.toml`, `eng/release.toml`, and `tests/lang/manifest.toml` as the current engineering-system sources of truth.
- Use `rg` / `rg --files` first. Many generated or vendor directories exist (`target`, `dist`, `node_modules`, wasm-pack `pkg` output).

## Development Workflow

1. Identify the affected subsystem from paths and symbols.
2. Read the relevant reference file and then inspect the current source around the named entry points.
3. Check whether the change crosses subsystem boundaries:
   - Syntax changes usually require parser, AST, type checker, codegen, formatter, and tests.
   - Runtime instruction changes usually require `vo-common-core`, codegen, VM exec, serializer/text dump, JIT support or fallback, and tests.
   - Module changes usually require `vo-module`, `vo-engine` compile context, CLI `vo mod`, Studio/Web resolution if browser-visible, and lock/cache tests.
   - Stdlib/FFI changes usually require both `lang/stdlib/*.vo` facade and `lang/crates/vo-stdlib/src/*.rs` or extension ABI code.
   - Studio GUI changes often touch `apps/studio/src`, `apps/studio/wasm`, `vo-web`, `vo-app-runtime`, and renderer bridge ordering.
4. Make the smallest source-backed change.
5. Run focused validation from [references/cli-testing.md](references/cli-testing.md). Use `VOWORK=off` for repo language tests unless you intentionally need sibling workspace overrides.
6. Re-read the changed source and any reference caveats before finalizing. In this repo, stale docs are common enough that a second pass matters.

## High-Risk Areas

- `vo.work` is auto-discovered unless disabled. It can redirect dependencies to sibling repos; CI language tests use `VOWORK=off`.
- Build-like commands (`vo run`, `vo build`, `vo check`, `vo test`) should not re-resolve dependencies or mutate `vo.mod` / `vo.lock`, but current CLI auto-install paths may download artifacts already pinned by `vo.lock`. Explicit `vo mod ...` lifecycle commands own dependency resolution and lock mutation.
- The bytecode and JIT specs lag source in several places. Use `lang/crates/vo-common-core/src/instruction.rs`, `lang/crates/vo-common-core/src/bytecode.rs`, `lang/crates/vo-vm/src/exec/*`, and `lang/crates/vo-vm/src/vm/jit*` as truth.
- GC is non-moving incremental tri-color mark/sweep. Do not describe it as moving or conservative.
- `RenderBuffer` keeps only the latest frame. Do not promise frame replay.
- Playground and Studio do not share every runtime path. `apps/playground-legacy/rust` still has its own GUI state path; Studio uses `vo-app-runtime`.

## Common Verification Shortlist

- Engineering-system sanity: `cargo run -q -p vo-dev -- lint all`
- Toolchain readiness for a task: `cargo run -q -p vo-dev -- tool check --task <task>` or `cargo run -q -p vo-dev -- tool bootstrap --task <task>`
- PR readiness plan: `cargo run -q -p vo-dev -- verify plan pr`
- Manifest sanity: `cargo run -q -p vo-dev -- test lint --suite lang`
- Broad parser/type/codegen/runtime behavior: `cargo run -q -p vo-dev -- test run --suite lang --targets vm,jit`
- Focused parser/type/codegen/runtime behavior: `cargo run -q -p vo-dev -- test run --suite lang --targets vm,jit --path <file.vo>` or `./d.py test both <file.vo>`
- JIT-specific behavior: `cargo run -q -p vo-dev -- test run --suite lang --targets jit --path <file.vo>` or `VO_JIT_CALL_THRESHOLD=1 ./d.py run <file.vo> --mode=jit`
- Web/WASM compiler path: `cargo run -q -p vo-dev -- test run --suite lang --targets wasm` or `cargo check -p vo-web --target wasm32-unknown-unknown`
- Rust workspace sanity: `cargo fmt --all -- --check`, `cargo check --workspace --all-targets --exclude vo-playground`
- Focused Studio frontend build: `cd apps/studio && npm run build`
- Full Studio/site task graph: `./d.py ci site` or `./d.py ci task studio-build`
- Studio WASM helper: `./d.py studio --build-only`
- Studio local app: `./d.py studio`
- Local CI planner/runner: `./d.py ci smart`, `./d.py ci quality`, `./d.py ci test`, or `./d.py ci pr`
