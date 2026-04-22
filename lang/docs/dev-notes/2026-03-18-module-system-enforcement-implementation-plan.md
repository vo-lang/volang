# Module System Enforcement Implementation Plan

**Date**: 2026-03-18
**Status**: Completed
**Scope**: `vo-module`, `vo-analysis`, `vo-engine`, `vo-web`, `cmd/vo`, release verification, module-system enforcement CI
**Authoritative Spec**: `lang/docs/spec/module.md`
**Related Design Context**: `lang/docs/dev-notes/2026-03-17-module-system-1.0-landing-plan.md`

---

## 1. Why This Document Exists

The 2026-03-17 landing plan already defines the target architecture.
This document is the concrete implementation plan for the current enforcement pass.

The review of the current tree found that the main remaining problems are no longer broad design uncertainty.
They are now **authoritative enforcement gaps**:

- module identity is still validated differently in different layers
- web/playground paths still preserve old versioned-import and ad hoc fetch semantics
- some import-policy rules exist in helper code but are not wired into the real import pipeline
- CLI/module lifecycle surfaces are still partially split between old and new command models
- frozen-build boundaries are still not completely clean around native extension preparation

This pass does not introduce a weaker transitional model.
It hardens the current codebase around one authoritative module system.

---

## 2. Non-Negotiable Rules For This Pass

The implementation must preserve these rules throughout the pass:

- there is exactly one canonical identity model for modules and packages
- `vo.mod` is direct intent, `vo.lock` is the exact resolved graph
- build/check/run remain frozen with respect to dependency resolution and network access
- `vo.work` remains local-only and never changes published identity
- old `@version` import semantics are removed from authoritative compiler/runtime paths
- native and web must converge on the same module contract instead of keeping parallel semantics
- ambiguous removed state must fail fast instead of silently falling back

---

## 3. Main Gaps To Close

### 3.1 Split validation rules

The tree currently has multiple partially-overlapping validators for module paths and import paths:

- `vo-module/src/modfile.rs`
- `vo-module/src/lockfile.rs`
- `vo-analysis/src/importer.rs`
- release-manifest validation code

These do not currently encode one authoritative rule set.

### 3.2 Web/playground bypasses

The web/playground path still supports source-driven dependency acquisition by scanning imports and accepting versioned import syntax.
That bypasses the spec model centered on explicit lifecycle commands and `vo.mod` / `vo.lock`.

### 3.3 Import policy not fully enforced

The codebase has support code for rules such as internal-package visibility, but the real import pipeline does not consistently enforce them.
The implementation also needs an explicit rule for rejecting imported `main` packages.

### 3.4 CLI surface still not fully aligned

The dependency lifecycle already moved significantly under `vo mod ...`, but command shape and validation are not fully centralized yet.

---

## 4. Execution Plan

## Phase A — Create One Authoritative Identity / Validation Layer

### Goal

All module-path and import-path validation must come from one shared implementation.
No duplicate local rule sets remain authoritative.

### Required changes

- introduce a dedicated validation module under `vo-module`
- define separate but related validators for:
  - canonical module paths
  - canonical package/import paths
  - standard-library import paths
  - versioned import rejection in compiler paths
  - module path / semantic-version major compatibility
- make existing parsers and consumers call the shared validator instead of re-implementing rules
- update release-manifest validation to use the same canonical identity rules

### Expected file groups

- `lang/crates/vo-module/src/lib.rs`
- new shared validation module in `lang/crates/vo-module/src/`
- `lang/crates/vo-module/src/modfile.rs`
- `lang/crates/vo-module/src/lockfile.rs`
- `lang/crates/vo-module/src/release_manifest.rs`
- `lang/crates/vo-analysis/src/importer.rs`

### Acceptance criteria

- one invalid path yields the same decision regardless of parser/consumer
- non-canonical module roots stop passing one layer and failing later in another
- tests cover both valid canonical forms and fast failure for removed forms

---

## Phase B — Remove Removed Versioned-Import / Ad Hoc Web Resolution Paths

### Goal

The authoritative module system must stop treating source imports as a dependency-management protocol.

### Required changes

- remove or hard-fail the removed `@version` import path in web/playground-facing compiler flows
- stop using source scanning as the authoritative dependency acquisition model for real module builds
- require `vo.mod` / `vo.lock` for external-module builds in the same way native frozen paths already do
- keep any purely local/preloaded VFS helpers only if they follow the same lock-based contract

### Expected file groups

- `lang/crates/vo-module/src/fetch.rs`
- `lang/crates/vo-web/src/module_install.rs`
- `lang/crates/vo-web/src/lib.rs`
- callers in playground/studio WASM integration

### Acceptance criteria

- no authoritative compile path depends on `import "github.com/...@vX.Y.Z"`
- web/native external-module semantics converge on `vo.mod` + `vo.lock`
- removed versioned import syntax fails fast with a precise migration message

---

## Phase C — Enforce Import Policy In The Real Analysis Pipeline

### Goal

Import-policy helpers must become real semantic enforcement.

### Required changes

- enforce internal-package visibility during actual import resolution / project analysis
- explicitly reject importing packages whose declared package name is `main`
- keep relative-import rejection and compiler-path `@version` rejection in the same policy layer
- ensure diagnostics mention canonical paths and the exact violated rule

### Expected file groups

- `lang/crates/vo-analysis/src/project.rs`
- `lang/crates/vo-analysis/src/importer.rs`
- `lang/crates/vo-module/src/vfs.rs`
- `lang/crates/vo-module/src/resolver.rs` if shared logic needs to move

### Acceptance criteria

- importing `.../internal/...` from outside the allowed prefix fails
- importing a package whose package clause is `main` fails
- rule enforcement happens in the real project analysis path, not only unit helpers

---

## Phase D — Finish CLI / Lifecycle Alignment

### Goal

The exposed module-management surface must match the spec and the implementation architecture.

### Required changes

- centralize module lifecycle validation and command semantics
- align `init` with the canonical module-path rules
- review `vo mod update [module]` semantics and either implement targeted behavior or remove the false specificity
- preserve the hard removal of removed `vo get`

### Expected file groups

- `cmd/vo/src/main.rs`
- `lang/crates/vo-module/src/lifecycle.rs`

### Acceptance criteria

- CLI argument validation follows the shared identity rules
- lifecycle commands expose one coherent model with no fake per-module behavior

---

## Phase E — Revisit Frozen Extension Boundary

### Goal

Extension preparation must be architecturally consistent with frozen build rules.

### Required changes

- audit when native extension build is allowed versus when locked artifacts must already exist
- either tighten the runtime/build boundary or document and explicitly encode the allowed preparation step
- ensure local workspace extension behavior does not silently weaken published-module guarantees

### Expected file groups

- `lang/crates/vo-engine/src/run.rs`
- `lang/crates/vo-module/src/fetch.rs`
- extension-loading consumers

### Acceptance criteria

- native extension preparation no longer feels like an accidental backdoor
- published-module behavior remains deterministic and verifiable

---

## 5. Implementation Order For This Pass

The implementation order is intentionally narrower than the architectural landing plan.
This pass should land in the following order:

1. write this implementation plan and keep it updated
2. land the shared validation layer
3. switch parsers/analysis/release verification to the shared validator
4. enforce missing import-policy rules in the real analysis pipeline
5. remove removed versioned-import / ad hoc web dependency paths
6. align CLI semantics around the hardened core
7. finish with targeted tests and CI validation updates

This order matters:

- without a shared validator, later cleanup just duplicates drift
- without import-policy enforcement, canonical identity is still only partially real
- without removing the web bypasses, the tree still has two module systems

---

## 6. Validation Strategy

Every phase in this pass must be validated with assert-based tests.
Do not rely on manual inspection.

### Main validation targets

- `cargo test -p vo-module --release`
- `cargo test -p vo-analysis --release`
- `cargo test -p vo-engine --release`
- `cargo test -p vo-web --release`
- `cargo check -p vo-web --target wasm32-unknown-unknown --release`
- existing module-system enforcement workflow in `.github/workflows/module-system-enforcement.yml`

### Test categories to add or strengthen

- canonical module path acceptance / rejection
- import path acceptance / rejection
- internal-package visibility enforcement in real analysis
- imported `main` package rejection
- CLI validation for invalid module identities
- rejection of versioned import syntax in authoritative compile paths
- web/native parity tests around external module requirements

---

## 7. Immediate Work Item

The first implementation task is:

> Extract and adopt one authoritative identity / validation layer, then wire it through `vo-module`, `vo-analysis`, and release verification.

Nothing later in this pass should be implemented on top of duplicated path-validation logic.

---

## 8. Completion Summary

This enforcement pass is complete.

### Implemented outcomes

- shared canonical identity / validation is authoritative across `vo-module`, `vo-analysis`, release-manifest verification, registry resolution, and CLI-facing lifecycle parsing
- real analysis now enforces canonical import policy, including internal-package visibility and rejection of imported `package main`
- authoritative web/playground/studio compile flows no longer use source-scanned `@version` import semantics or ad hoc dependency installation; single-file external imports fail fast and project mode requires `vo.mod` + `vo.lock`
- CLI/module lifecycle surfaces are aligned around the hardened core:
  - `vo init` validates canonical module paths
  - `vo mod ...` and `vo release ...` require a real `vo.mod` ancestor
  - `vo mod update [module]` now performs a real targeted refresh by preferring the current lock for unrelated modules while still allowing shared transitive upgrades when required by the selected module
- the frozen native-extension boundary is now explicit:
  - cached published modules do not compile native extensions from source during frozen compile/run paths
  - frozen builds require locked `extension-native` artifacts for cached published modules
  - only local workspace override modules may build native extensions from source during extension preparation

### Validation rerun for completion

- `cargo test -p vo-module --release`
- `cargo test -p vo-analysis --release`
- `cargo test -p vo-engine --release`
- `cargo test -p vo-web --release`
- `cargo test -p vo --release`
- `cargo check -p vo --release`
- `cargo check -p vo-module --release --target wasm32-unknown-unknown`
- `cargo check -p vo-web --release --target wasm32-unknown-unknown`
- `cargo check --release --target wasm32-unknown-unknown` in `playground/rust`
- `cargo check --release --target wasm32-unknown-unknown` in `studio/wasm`
