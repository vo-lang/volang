# Modules, Standard Library, And Native FFI

## Contents

- [Module Model](#module-model)
- [Frozen Builds And Lifecycle Commands](#frozen-builds-and-lifecycle-commands)
- [Registry, Solver, Cache](#registry-solver-cache)
- [Inline Modules](#inline-modules)
- [Workspace Overrides](#workspace-overrides)
- [Standard Library Layout](#standard-library-layout)
- [Native FFI And Extensions](#native-ffi-and-extensions)
- [Change Recipes](#change-recipes)
- [Caveats](#caveats)

## Module Model

`lang/crates/vo-module` owns module identity, manifests, lock files, workspace overrides, solver, cache, registry, lifecycle operations, and release artifact modeling.

Important files:

Paths in this list are relative to `lang/crates/vo-module`.

- `src/schema/modfile.rs`: `ModFile`, `ModIdentity`, `Require`, toolchain constraint, web and extension metadata.
- `src/schema/lockfile.rs`: `LockFile`, `LockRoot`, `LockedModule`, `LockedArtifact`.
- `src/schema/workfile.rs`: `WorkFile`, `UseEntry`.
- `src/identity.rs`: canonical module/import identity and visibility rules.
- `src/project.rs`: project context, single-file classification, lock building.
- `src/inline_mod.rs`: inline module block parser.
- `src/ephemeral.rs`: ephemeral single-file module synthesis/cache.
- `src/workspace.rs`: `vo.work` discovery and replacement loading.
- `src/ops.rs`: CLI-level `vo mod` operations.

`vo.mod` is human-authored intent:

- module identity
- Vo toolchain constraint
- direct requirements
- optional extension metadata
- optional browser/web runtime metadata
- optional source/artifact publication intent

`vo.lock` is resolved, generated authority:

- root identity
- exact module versions and commits
- manifest digests
- source artifacts
- native/WASM artifacts

`vo.work` is local-only workspace replacement. It does not rewrite canonical module identity.

## Frozen Builds And Lifecycle Commands

Normal build-like commands are frozen with respect to dependency resolution and module file mutation:

- `vo run`
- `vo build`
- `vo check`
- `vo test`

They should not rewrite `vo.mod`, rewrite `vo.lock`, or re-solve dependencies. Current CLI auto-install paths can download artifacts already pinned by `vo.lock` when the local cache is missing them. Single-file inline `require` also has an explicit auto-install path.

Lifecycle commands live in `vo_module::ops` and are exposed by `cmd/vo`:

- `vo mod add <module[@constraint]>`
- `vo mod update [module]`
- `vo mod sync [path]`
- `vo mod download [path]`
- `vo mod verify [path]`
- `vo mod remove <module>`
- `vo mod tidy [path]`
- `vo mod why <module>`
- `vo mod clean [--all]`

Dependency resolution and lock mutation belong here. Artifact download for an already locked graph can also occur through current auto-install compile paths.

## Registry, Solver, Cache

Registry abstraction:

- `src/registry.rs`: `Registry` trait with version listing, module path probing, manifest/source/artifact fetch.
- `src/github_registry.rs`: GitHub Releases implementation.
- Tokens: `VO_GITHUB_TOKEN`, then `GITHUB_TOKEN`, then anonymous access.

Solver/lifecycle:

- `src/solver.rs`: deterministic version graph resolution with semver constraints, major path rules, toolchain compatibility, and existing lock preference.
- `src/lifecycle.rs`: `prepare_lock_file`, `download_locked_dependencies`, `verify_locked_dependencies`, `clean_cache`.
- `src/cache/*`: cache layout, install, validation.
- `src/readiness.rs`: ready module/artifact checks for frozen builds.

Default native module cache is `$HOME/.vo/mod` via `vo-engine::default_mod_cache_root`.

## Inline Modules

Single-file inline module blocks start with `/*vo:mod ... */`. They are used only when a file is not already inside a project with `vo.mod`.

Key types:

- `InlineMod`
- `InlineRequire`
- `SingleFileContext::{Project, EphemeralInlineMod, AdHoc}`
- `EphemeralProject`

Rules to keep in mind:

- If a file is inside a project, inline module metadata is a hard error.
- `local/*` identities are for ephemeral roots only; do not allow them in imports, requires, published manifests, or lock entries.
- Inline dependencies can synthesize temporary project files and cache by canonical inline block digest.
- Web support for inline `require` is more limited than native support; check `lang/crates/vo-web/src/compile.rs` before documenting browser behavior.

## Workspace Overrides

`vo.work` supports local sibling development. The root repo has a `vo.work` pointing at several sibling repos.

Important behavior:

- Workspace discovery can walk ancestors unless disabled.
- `VOWORK=off` disables workspace discovery.
- CI language tests use `vo-dev test run`; `VOWORK=off` is declared in `eng/tests.toml` for native targets to avoid local sibling pollution.
- `vo.mod` does not support a `replace` directive; local replacement belongs in `vo.work`.

Use workspace overrides intentionally. For module protocol tests and release verification, disable them unless the test is about workspace behavior.

## Standard Library Layout

The standard library is split between Vo facade source and Rust native implementations.

Vo source:

- `lang/stdlib/**/*.vo` (`lang/stdlib/<import-path>/*.vo`, including nested import paths)
- `lang/stdlib/stdlib.toml`

Rust shim:

- `lang/crates/vo-stdlib/src/lib.rs`: `register_externs`
- `lang/crates/vo-stdlib/src/source.rs`: `EmbeddedStdlib`, `StdlibFs`
- package files such as `math.rs`, `fmt.rs`, `strings.rs`, `os.rs`, `time.rs`

Registration:

- Runtime builtins are registered first.
- Cross-platform stdlib modules are registered next.
- `feature = "std"` gates std-only modules such as OS/time/net/filepath/exec/toolchain.

Facade pattern:

- `.vo` files declare body-less extern functions for native shims.
- Public APIs can wrap those externs in Vo.
- Rust functions use `#[vostd_fn("pkg", "Func")]` or manual `ExternCallContext`.
- `vo_runtime::stdlib_register!(pkg: ...)` registers generated wrappers.

Example patterns:

- `lang/stdlib/math/math.vo` plus `lang/crates/vo-stdlib/src/math.rs`.
- `lang/stdlib/fmt/fmt.vo` plus `lang/crates/vo-stdlib/src/fmt.rs`; `fmt` uses manual context for variadic/format behavior.

## Native FFI And Extensions

Extension SDK:

- `lang/crates/vo-ext/src/lib.rs`
- Use `vo_ext::prelude::*`.
- Annotate functions with `#[vo_fn("pkg", "Func")]`.
- Export the table with `vo_ext::export_extensions!()`.

Stdlib macro:

- `#[vostd_fn("pkg", "Func")]`
- `#[vostd_fn("pkg", "Func", std)]` for std-only functions with no_std stubs.

Macro implementation:

- `lang/crates/vo-ffi-macro/src/lib.rs`
- `lang/crates/vo-ffi-macro/src/codegen.rs`
- `lang/crates/vo-ffi-macro/src/registration.rs`
- `lang/crates/vo-ffi-macro/src/resolve.rs`

Supported function modes:

- simple: Rust function returns a mapped value
- result: Rust function returns `Result<T, String>`
- manual: `fn(ctx: &mut ExternCallContext) -> ExternResult`

ABI name matching:

- `.vo` package path
- extension manifest name/path
- Cargo metadata such as `[package.metadata.vo] vomod`
- macro package path
- `lang/crates/vo-common/src/abi.rs::abi_lookup_name`

These must line up. Codegen emits `Opcode::CallExtern` for body-less Vo functions, and runtime lookup uses ABI names.

Actual extern lookup uses the package `abi_path`, not just the surface import string. For nested packages or extension subpackages, check `package_abi_path`, `resolve_full_pkg_path`, and the resolved `VfsPackage.abi_path`.

Native dynamic loading:

- `lang/crates/vo-runtime/src/ext_loader.rs`: loads shared library, checks ABI version and fingerprint, reads entry table.
- `lang/crates/vo-runtime/src/ffi/mod.rs`: extern call context and registry.
- `lang/crates/vo-engine/src/compile/native.rs`: builds local/workspace native extension crates when appropriate.
- Published dependency extensions must be present as locked `extension-native` artifacts.

## Change Recipes

Changing module rules:

1. Update `vo-module` schema/identity/project/lifecycle as needed.
2. Update `vo-engine` compile context if frozen build behavior changes.
3. Update `cmd/vo` CLI help/command behavior.
4. Check `vo-web` and Studio if browser resolution is affected.
5. Add module tests, including failure cases.

Adding stdlib API:

1. Add or update `.vo` facade under `lang/stdlib`.
2. Add Rust shim in `vo-stdlib` if native behavior is required.
3. Register externs in the package module and `lang/crates/vo-stdlib/src/lib.rs` if a new Rust module is added.
4. Confirm `stdlib.toml` tier and source embedding.
5. Add `tests/lang` tests.
6. Run native and WASM validation if the API should work in browser/no_std.

Adding extension capability:

1. Check `vo-ext` public API.
2. Update `vo-ffi-macro` if signature mapping changes.
3. Update runtime `ExternCallContext` / loader if ABI changes.
4. Update module manifest/artifact validation if publication shape changes.
5. Test local workspace extension and locked published-artifact behavior separately.

## Caveats

- Current source uses `#[vo_fn]` and `#[vostd_fn]`, not old `#[vo_extern]` examples.
- `apps/playground-legacy/src/assets/docs/generated/native-ffi.md` can be stale.
- `vo.mod` has no `replace`.
- `local/*` is reserved for inline roots only.
- Build-like commands should not mutate dependency state, but can auto-download artifacts already pinned by `vo.lock`.
- Native extension artifact behavior differs for local/workspace modules versus published dependencies.
