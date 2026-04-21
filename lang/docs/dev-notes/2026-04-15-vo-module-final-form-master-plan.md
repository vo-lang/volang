# vo-module Final Form Master Plan

**Date**: 2026-04-15
**Status**: Planning
**Scope**: Transform `vo-module` into the authoritative, spec-complete implementation of `lang/docs/spec/module.md`
**Related documents**:
- `lang/docs/spec/module.md` — normative module specification
- `lang/docs/spec/native-ffi.md` — canonical `vo.ext.toml` schema and artifact contract
- `lang/docs/spec/repository-layout.md` — repo layout and publishing checklist
- `lang/docs/dev-notes/2026-03-31-vo-module-vo-ext-vo-engine-vo-web-architecture-cleanup-plan.md` — cross-crate boundary cleanup (parallel work)

---

## 1. Purpose

`lang/docs/spec/module.md` defines the complete module system contract.
The current `vo-module` crate implements large portions of it, but several areas remain incomplete, legacy-shaped, or scattered across consumer crates.

This document defines the steps required to bring `vo-module` to its **final authoritative state** — the single crate that fully owns every rule described in the spec.

This is not a bug-fix pass or a minimal cleanup.
It is the convergence plan for making `vo-module` the one source of truth for the Vo module protocol.

---

## 2. Gap Summary

| Spec area | Current state | Gap |
|---|---|---|
| `vo.ext.toml` schema (§5.5, native-ffi.md) | `ext_manifest.rs` parses the legacy flat schema (`[extension].path`) | Must be rewritten to canonical schema; legacy shapes must be hard-rejected |
| Target-support contract (§6.4) | Not enforced at publication time | `vo-release` must validate declared targets against published artifacts |
| Dependency readiness (§8.1–8.4) | Scattered across `vo-engine` compile pipeline and `vo-web` install pipeline | `vo-module` must own a unified readiness API |
| Native artifact resolution (§8.4) | Lives in `vo-engine/src/compile/native.rs` | Must be governed by `vo-module`'s target-support contract |
| WASM artifact contract (§6.4) | `vo-web` has its own parallel `WasmExtensionManifest` pipeline | Must consume `vo-module`'s canonical artifact model |
| Integrity cross-validation (§12.1) | `vo.ext.toml` ↔ `vo.release.json` consistency not verified | Must be enforced as part of lock-graph verification |
| Transitional APIs | `compat.rs` exists; `vo-web` fronts `ProjectContext` | Must be removed or narrowed |

---

## 3. Steps

### Step 1 — Canonical `vo.ext.toml` Schema and Legacy Rejection

**What**: Rewrite `vo-module/src/ext_manifest.rs` to parse only the canonical schema defined in `native-ffi.md`. Hard-reject every legacy shape. Update all test fixtures and consumer call sites.

**Why first**: Every downstream step depends on `vo-module` producing typed, validated extension metadata in the canonical shape. Nothing else can land cleanly until this foundation is correct.

**Key files**:
- `lang/crates/vo-module/src/ext_manifest.rs`
- all `vo.ext.toml` fixtures in test directories
- consumer imports in `vo-engine`, `vo-web`, `vo-release`

**Acceptance**:
- Parsing a legacy-shaped `vo.ext.toml` produces a hard error, not a compatibility fallback
- The parsed type exposes `native.targets[]`, `native.path`, `wasm.target`, `wasm.file`, `wasm.js_glue`
- All existing tests pass or are rewritten to use canonical fixtures

**Detailed plan**: see `2026-04-15-vo-module-step1-ext-schema-rewrite.md`

---

### Step 2 — Release Artifact Contract Enforcement

**What**: Update the release-staging pipeline (`vo-release`) to validate that every target declared in `vo.ext.toml` has its corresponding artifact set in `vo.release.json`. Reject publication if any declared target is missing artifacts.

**Why**: Spec §6.2 and §6.4 require that `artifacts` in `vo.release.json` satisfy the declared target-support contract. Currently, `vo-release/src/repo.rs::stage_release` does not cross-validate against `vo.ext.toml` target declarations.

**Key files**:
- `lang/crates/vo-release/src/repo.rs` — `stage_release`
- `lang/crates/vo-release/src/tests.rs`
- `lang/crates/vo-module/src/ext_manifest.rs` (consumer of Step 1 output)
- `lang/crates/vo-module/src/schema/manifest.rs` — `ManifestArtifact`

**Acceptance**:
- `stage_release` fails if `vo.ext.toml` declares a target but the artifact set for that target is absent
- `stage_release` fails if `vo.ext.toml` uses a legacy schema shape
- New test cases cover: all targets present, one target missing, legacy schema rejection, pure-source module (no `vo.ext.toml`)

---

### Step 3 — Target-Artifact Resolution API in `vo-module`

**What**: Add a `vo-module` API that answers: "given a locked module, a host target triple, and the module's `vo.ext.toml`, which locked artifacts are required and where should they be?" Move the artifact-lookup logic currently in `vo-engine/src/compile/native.rs` into `vo-module`.

**Why**: Spec §8.4 defines the frozen-build artifact rules. Currently, `vo-engine` implements its own artifact-name derivation and cache lookup. That logic is module-protocol knowledge that belongs in `vo-module`.

**Key files**:
- `lang/crates/vo-module/src/ext_manifest.rs` (canonical types from Step 1)
- new module: `lang/crates/vo-module/src/artifact.rs` or extend `cache/validate.rs`
- `lang/crates/vo-engine/src/compile/native.rs` — delete duplicated logic, call `vo-module` API
- `lang/crates/vo-module/src/schema/lockfile.rs` — `LockedArtifact`

**Acceptance**:
- `vo-module` exposes a function like `required_artifacts_for_target(locked_module, ext_manifest, host_target) -> Vec<RequiredArtifact>`
- `vo-engine` calls this instead of computing artifact names itself
- `vo-web` can also call this for `wasm32-unknown-unknown`

---

### Step 4 — Unified Dependency Readiness API

**What**: Create a single `vo-module` entry point that answers: "are all dependencies for this project ready for a frozen build on this target?" This replaces the scattered readiness checks in `vo-engine` compile pipeline and `vo-web` install pipeline.

**Why**: Spec §8.1–8.2 define frozen-build semantics. Currently, each consumer assembles its own readiness pipeline from low-level `vo-module` primitives. The readiness question is module-protocol knowledge, not engine or web knowledge.

**Key files**:
- new: `lang/crates/vo-module/src/readiness.rs` (or extend `lifecycle.rs`)
- `lang/crates/vo-module/src/cache/validate.rs`
- `lang/crates/vo-module/src/lifecycle.rs`
- `lang/crates/vo-engine/src/compile/pipeline.rs` — simplify to call readiness API
- `lang/crates/vo-web/src/module_install/mod.rs` — simplify to call readiness API

**Acceptance**:
- `vo-module` exposes a `DependencyReadiness` check that reports: source ready, artifacts ready, integrity verified, or specific failure
- `vo-engine` and `vo-web` both call this single API instead of assembling their own validation chains
- `vo-app-runtime` and Studio do not need to understand dependency preparation details

---

### Step 5 — `vo-web` Alignment to Canonical Artifact Contract

**What**: Update `vo-web` to consume `vo-module`'s canonical extension manifest types and artifact resolution API (from Steps 1 and 3). Remove duplicated `WasmExtensionManifest` handling that predates the canonical schema.

**Why**: `vo-web/src/module_install/extension.rs` currently parses `vo.ext.toml` through `vo-module` but then applies its own WASM-specific artifact logic. After Steps 1 and 3, the canonical model already covers WASM targets.

**Key files**:
- `lang/crates/vo-web/src/module_install/extension.rs`
- `lang/crates/vo-web/src/module_install/fetch.rs`
- `lang/crates/vo-web/src/module_install/vfs_io.rs`

**Acceptance**:
- `vo-web` extension loading uses the same `required_artifacts_for_target` API as native
- No WASM-specific manifest interpretation logic remains in `vo-web` that duplicates `vo-module`
- Browser fetch transport code remains in `vo-web` (transport is correctly platform-specific)

---

### Step 6 — Integrity Cross-Validation Hardening

**What**: Implement the full verification chain from spec §12.1, specifically the currently missing checks:
- packaged `vo.ext.toml` consistency with `vo.release.json.artifacts`
- locked artifact set equality with published artifact set
- `vo.ext.toml` target-support contract consistency at lock-graph verification time

**Why**: The spec requires these as hard errors. The current implementation verifies source and manifest digests but does not cross-validate extension metadata against the published artifact contract.

**Key files**:
- `lang/crates/vo-module/src/lock.rs` — lock construction and verification
- `lang/crates/vo-module/src/cache/validate.rs`
- `lang/crates/vo-module/src/lifecycle.rs` — `verify_dependencies`

**Acceptance**:
- Installing or verifying a module whose `vo.ext.toml` declares targets not covered by `vo.release.json` artifacts is a hard error
- Locked artifact mismatch produces the error from spec §15.4
- No weaker validation mode exists

---

### Step 7 — `compat.rs` Removal and Public Surface Cleanup

**What**: Delete `vo-module/src/compat.rs`. Migrate all internal callers to typed APIs. Narrow or remove `vo-web`'s public re-exports of `vo-module` project types.

**Why**: `compat.rs` is a transitional convenience layer that teaches new callers to depend on stringly-typed wrappers instead of the real typed model. The architecture cleanup plan (2026-03-31, Phase 5) already identifies this as a target.

**Key files**:
- `lang/crates/vo-module/src/compat.rs` — delete
- `lang/crates/vo-module/src/lib.rs` — remove `pub mod compat`
- `lang/crates/vo-analysis/src/` — migrate callers
- `lang/crates/vo-web/src/lib.rs` — remove redundant re-exports
- `lang/crates/vo-web/src/compile.rs`

**Acceptance**:
- `compat.rs` no longer exists
- All callers use typed identity, version, and project APIs directly
- `vo-web` does not publicly front `ProjectContext` as if it owns the concept

---

## 4. Dependency Order

```
Step 1 ──→ Step 2 ──→ Step 6
  │                      ↑
  └──→ Step 3 ──→ Step 4─┘
         │
         └──→ Step 5

Step 7 is independent and can land at any point after Step 1.
```

- **Step 1** is the foundation. Everything else depends on canonical extension types.
- **Steps 2 and 3** can proceed in parallel after Step 1.
- **Step 4** depends on Step 3 (needs the artifact resolution API).
- **Step 5** depends on Steps 1 and 3.
- **Step 6** depends on Steps 2 and 4 (needs both publication enforcement and readiness API).
- **Step 7** is independent and low-risk; it can land whenever convenient.

---

## 5. Relationship to Architecture Cleanup Plan (2026-03-31)

The architecture cleanup plan defines cross-crate boundary corrections (P1–P8) and 6 phases.
This master plan focuses specifically on making `vo-module` spec-complete.

Overlap:
- **Step 1** here aligns with architecture cleanup Phase 1 prerequisite (extension manifest ownership in `vo-module`)
- **Step 3** here aligns with architecture cleanup Phase 2 (registry/release rules in `vo-module`)
- **Step 4** here aligns with architecture cleanup Phase 3 (explicit extension state)
- **Step 5** here is a subset of architecture cleanup Phase 2
- **Step 7** here aligns with architecture cleanup Phase 5

The two plans reinforce each other. This plan can be read as the `vo-module`-internal view of the broader cross-crate cleanup.

---

## 6. Non-Goals

- Redesigning `vo.mod` / `vo.lock` / `vo.work` syntax or semantics (already stable)
- Changing the solver algorithm
- Redesigning browser fetch transport
- Introducing fallback semantics or weaker validation modes
- Supporting legacy `vo.ext.toml` schema in any form

---

## 7. Validation

After each step, the following must pass:

```sh
cargo test -p vo-module --release
cargo test -p vo-engine --release
cargo test -p vo-web --release
cargo test -p vo-release --release
cargo check -p vo --release
cargo check --release --target wasm32-unknown-unknown -p vo-web
```

The full module-system test suite should be run after Steps 1, 4, and 6:

```sh
./d.py test both
```
