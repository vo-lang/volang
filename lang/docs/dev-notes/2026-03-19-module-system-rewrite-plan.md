# Module System Full Rewrite Plan

**Date**: 2026-03-19
**Status**: Design — Pending approval
**Scope**: `vo-module` complete rewrite, consumer cutover for `vo-analysis`, `vo-engine`, `vo-web`, `cmd/vo`
**Authoritative Spec**: `lang/docs/spec/module.md`
**Supersedes**: `lang/docs/dev-notes/2026-03-17-module-system-1.0-landing-plan.md` (incremental cutover approach)
**Builds On**: `lang/docs/dev-notes/2026-03-18-module-system-enforcement-implementation-plan.md` (completed enforcement pass)

---

## 1. Why Rewrite Instead of Continuing to Harden

The 2026-03-17 landing plan defined an incremental cutover strategy.
The 2026-03-18 enforcement pass executed one round of hardening.

After the enforcement pass completed, the following structural problems remain:

- **Split authority surfaces persist**. Even with shared validation wired through, the code still has multiple partially-overlapping codepaths for resolution, lock verification, and materialization. Enforcement unified the rules but not the architecture.
- **The data flow is not layered**. Solver, lock authority, materialization, and workspace logic are interleaved rather than composed in a strict dependency order. This makes each fix fragile and each new feature a cross-cutting concern.
- **The schema layer is implicit**. `vo.mod`, `vo.lock`, `vo.work`, and `vo.release.json` are parsed and validated by ad hoc code scattered across multiple files. There is no single typed schema layer that all consumers share.
- **The enforcement pass revealed the ceiling of incremental patching**. Each enforcement fix required touching 4–6 files across 3–4 crates. The fan-out is a symptom of missing architectural boundaries, not missing rules.

The updated `spec/module.md` now defines a strict, unambiguous protocol.
The correct next step is to implement that protocol from scratch as a brand-new crate. The old `vo-module` will be renamed (archived) before development begins, so the new crate owns the `vo-module` name from day one. Consumer crates (`vo-analysis`, `vo-engine`, `vo-web`, `cmd/vo`) will be adapted to the new API only after the new `vo-module` is complete and self-tested.

---

## 2. Pre-Conditions (Already Completed)

The following work from the enforcement pass is assumed done and will not be repeated:

- `vo.sum` is no longer the build contract
- alias-based `@"name"` import syntax is rejected in authoritative compile paths
- `files(...)` WASM-only fetch protocol is removed from `vo.mod`
- published `replace` directives are rejected in `vo.mod`
- web/playground/studio single-file mode rejects external imports
- `vo get` fails fast with guidance to use `vo mod ...`
- shared canonical identity validation exists in `vo-module::identity`
- internal-package visibility and `package main` import rejection are enforced in `vo-analysis`

These are pre-conditions, not tasks in this plan.

---

## 3. Non-Negotiable Rules

These rules from the spec are invariant throughout the rewrite:

- there is exactly one canonical identity model for modules and packages
- `vo.mod` is human-authored direct intent; `vo.lock` is the exact resolved graph
- only the root project's `vo.lock` is authoritative for a build
- `vo build` / `vo check` / `vo test` / `vo run` are frozen: no network, no solving, no lock mutation
- `vo.work` is local-only source replacement; it does not participate in graph solving
- all integrity mismatches are hard errors with no fallback
- native and WASM share one module graph and one registry protocol
- published dependencies do not build native extensions from source during frozen builds

---

## 4. Target Architecture

The new `vo-module` is organized into 8 layers with strict unidirectional dependencies.

```text
Layer 1: Core Domain
    |
Layer 2: Schema
    |
Layer 3: Registry Protocol
    |
Layer 4: Solver
    |
Layer 5: Lock Authority
    |
Layer 6: Materialization
    |
Layer 7: Workspace
    |
Layer 8: Operations / CLI
```

Each layer may depend only on layers above it.
No lateral dependencies.
No reaching down.

### 4.1 Layer 1 — Core Domain

Pure value types and pure validation rules.

Contents:

- `ModulePath` — canonical `github.com/<owner>/<repo>[/<subdir>][/vN]`
- `PackagePath` — canonical package import path
- `ImportPath` — classified import (stdlib vs external)
- `ExactVersion` — `vMAJOR.MINOR.PATCH[-PRERELEASE]`
- `ToolchainVersion` — `MAJOR.MINOR.PATCH[-PRERELEASE]`
- `DependencyConstraint` — `^`, `~`, exact
- `ToolchainConstraint` — `^`, `~`, exact (no `v` prefix)
- `Digest` — `sha256:<64 lowercase hex>`
- `ArtifactId` — `(kind, target, name)`
- import classification: first segment contains `.` → external, else stdlib
- major-version / module-path compatibility check

Constraints:

- no filesystem access
- no network access
- no CLI awareness
- no `serde` dependency in core types (serialization belongs to schema layer)

### 4.2 Layer 2 — Schema

Typed parse / normalize / validate / serialize for all module-system file formats.

Contents:

- `vo.mod` parser and renderer
- `vo.lock` parser and renderer (stable TOML serialization)
- `vo.work` parser and renderer (TOML schema, `version = 1`)
- `vo.release.json` parser and renderer

Constraints:

- schema validation is self-contained: a schema object is valid or invalid without cross-file checks
- cross-file consistency (e.g. root `vo.mod` vs root `vo.lock`) belongs to the lock authority layer
- deterministic serialization: parse → render → parse round-trip is identity

### 4.3 Layer 3 — Registry Protocol

Defines what a "published module version" is and how to discover candidates.

Contents:

- canonical module path → GitHub repository mapping
- module-root-relative Git tag computation
- release asset discovery (manifest, source package, target artifacts)
- published metadata validation (schema_version, module, version, commit, require, source, artifacts)

Constraints:

- this layer returns validated candidate metadata
- it does not solve or select versions
- it does not know about lock files

### 4.4 Layer 4 — Solver

Deterministic version selection from root intent and registry candidates.

Inputs:

- root `vo.mod` (direct dependency constraints)
- registry candidate metadata per module
- optional lock preferences for targeted update

Output:

- `ResolvedGraph`: set of `(ModulePath, ExactVersion)` with dependency edges

Constraints:

- highest compatible version wins
- single version per canonical module path
- no network-order or timestamp-order dependence
- no awareness of cache, workspace, or filesystem
- pre-release versions require explicit constraint

### 4.5 Layer 5 — Lock Authority

Bidirectional bridge between `ResolvedGraph` and `vo.lock`.

Contents:

- lock generation: `ResolvedGraph` + registry metadata → `vo.lock`
- lock loading: `vo.lock` → typed lock structure
- root consistency: root `vo.mod` module/vo fields must equal root `vo.lock`
- graph consistency: locked `deps` must equal published `vo.release.json` direct deps
- artifact consistency: locked `artifact` set must equal published artifact set including size and digest
- commit/digest format validation

Constraints:

- this is the **single** lock authority — no other layer writes or interprets lock semantics
- workspace is not visible to this layer

### 4.6 Layer 6 — Materialization

Download, verify, cache, and frozen-build validation.

Contents:

- source package download and digest verification
- target artifact download and digest verification
- cache population and eviction
- frozen-build materializer: verify that all required locked artifacts exist in cache with correct digests

Constraints:

- published dependency native extensions are never built from source in this layer
- if cache entry fails validation against `vo.lock`, treat it as missing
- cache layout is an implementation detail, not a protocol contract

### 4.7 Layer 7 — Workspace

Local source replacement via `vo.work`.

Contents:

- nearest-ancestor `vo.work` discovery
- `VOWORK=off` support
- canonical module identity verification against override directory's `vo.mod`
- source tree replacement in import resolution

Constraints:

- does not participate in graph solving
- does not rewrite lock semantics
- consuming build does not read override module's nested `vo.work` or `vo.lock`
- override importing an external package not in root lock → hard error
- root module must not override itself

### 4.8 Layer 8 — Operations / CLI

Command semantics that compose the lower layers.

Contents:

- `vo mod init <module-path>`
- `vo mod add <module-path>[@constraint]`
- `vo mod update [module-path]`
- `vo mod sync`
- `vo mod download`
- `vo mod verify`
- `vo mod remove <module-path>`
- frozen build entry point (used by `vo build` / `vo check` / `vo test` / `vo run`)

Constraints:

- only `mod add`, `mod update`, `mod sync`, and `mod remove` are resolution-changing
- `mod download` materializes without re-solving
- frozen build entry point never accesses the network or mutates lock/mod files

---

## 5. Implementation Phases

### Phase 1 — Core Domain

**Goal**: Pure, tested value types and validation functions that all later layers import.

Deliverables:

- `ModulePath::parse` with full canonical validation (lowercase, no empty segments, major suffix rules)
- `PackagePath::parse` / `ImportPath::classify` (single classifier, no competing implementations)
- `ExactVersion::parse`, `DependencyConstraint::parse`, `ToolchainConstraint::parse`
- `Digest::parse`, `ArtifactId`
- major-version / module-path compatibility check
- internal-package visibility check

Acceptance criteria:

- every path/version rule from spec §3–§4 has a corresponding unit test
- no downstream code re-implements these rules

### Phase 2 — Schema Rewrite

**Goal**: Typed, round-trippable schema layer for all four file formats.

Deliverables:

- `ModFile` parse/render with strict `module` + `vo` + sorted `require` format
- `LockFile` parse/render with stable TOML, including root, resolved modules, deps, artifacts with size/digest
- `WorkFile` parse/render as TOML (`version = 1`, `[[use]]` entries)
- `ReleaseManifest` parse/render for `vo.release.json` (`schema_version = 1`)
- golden fixture tests for each format

Acceptance criteria:

- parse → render → parse produces identical output for all golden fixtures
- schema layer rejects unknown directives, duplicate entries, unsorted fields
- no cross-file validation in this layer

### Phase 3 — Registry Protocol

**Goal**: Clean, testable GitHub release protocol client.

Deliverables:

- module path → `(owner, repo, module_root)` mapping
- module-root-relative tag computation
- release asset discovery and manifest validation
- source package validation (archive contains `vo.mod` at root, correct content)

Acceptance criteria:

- "tag exists but release metadata is incomplete" does not produce a valid candidate
- native and web consume the same protocol implementation
- mockable for solver and integration tests

### Phase 4 — Solver

**Goal**: Deterministic constraint solver.

Deliverables:

- highest-satisfying deterministic solve
- single-version-per-module enforcement
- optional lock preferences for targeted `vo mod update [module]`
- pre-release gating (explicit constraint required)
- unsatisfiable constraint diagnostics

Acceptance criteria:

- solver is deterministic: same inputs always produce same output
- solver does not depend on cache, filesystem, workspace, or network order
- conflict diagnostics show which constraints are incompatible

### Phase 5 — Lock Authority

**Goal**: Single authoritative lock contract.

Deliverables:

- `ResolvedGraph` + registry metadata → `vo.lock` generation
- root `vo.mod` vs root `vo.lock` consistency check (module path, vo constraint)
- locked module `deps` vs published manifest `require` equality
- locked module `artifact` vs published manifest `artifacts` equality (including size, digest)
- commit format and digest format validation

Acceptance criteria:

- no other layer reads or writes lock semantics
- any consistency mismatch is a hard error
- dependency-local lockfiles are explicitly ignored

### Phase 6 — Materialization + Frozen Verification

**Goal**: Download, cache, and frozen-build boundary.

Deliverables:

- source package download with digest verification
- target artifact download with digest verification
- cache validator: entry valid only if matches exact locked version + digest
- frozen-build verifier: all required locked artifacts must exist in cache

Acceptance criteria:

- frozen builds never access network
- published dependency native extension source fallback is not possible
- cache mismatch → treat as missing, not silently use
- missing lock → hard error with actionable message
- missing artifact → hard error with `vo mod download` guidance

### Phase 7 — Workspace Layer

**Goal**: Local source replacement only.

Deliverables:

- nearest-ancestor `vo.work` discovery
- `VOWORK=off` environment variable support
- override identity verification (override `vo.mod` module path must match)
- import ownership integration (override module path participates in longest-prefix resolution)
- hard error if override imports unlocked external dependency

Acceptance criteria:

- workspace does not alter lock semantics
- root module cannot override itself
- nested override `vo.work` and `vo.lock` are ignored
- CI/release mode can disable workspace

### Phase 8 — Operations Layer

**Goal**: Implement all CLI command semantics on top of the lower layers, completing the new `vo-module` crate.

Deliverables:

- all `vo mod ...` commands implemented on top of new layers
- frozen build entry point API for `vo build` / `vo check` / `vo test` / `vo run`
- public API surface finalized and documented

Acceptance criteria:

- the new `vo-module` crate is self-contained and fully tested
- all spec rules from `module.md` are covered by the crate's own test suite
- the crate compiles independently without depending on any consumer crate

### Phase 9 — Consumer Adaptation

**Goal**: Adapt all consumer crates to use the new `vo-module` API.

This phase happens **after** the new `vo-module` is complete and self-tested.

Deliverables:

- `vo-analysis` import resolution uses new ownership model
- `vo-engine` compile planning uses new lock authority and materialization
- `vo-web` uses new registry protocol and lock authority
- `cmd/vo` routes through new operations layer

Acceptance criteria:

- all consumers compile against the new `vo-module`
- all consumers agree on the same module identity, lock contract, and build boundary
- the old renamed crate is no longer referenced by any consumer

---

## 6. Directory Structure

The old `vo-module` crate is renamed before development begins (e.g. `vo-module-old`).
The new `vo-module` is a fresh crate at `lang/crates/vo-module/`.

Layout inside `lang/crates/vo-module/src/`:

```text
lib.rs             # crate root, public API re-exports
core.rs            # Layer 1: ModulePath, ExactVersion, Digest, etc.
schema/
  mod.rs
  modfile.rs       # Layer 2: vo.mod
  lockfile.rs      # Layer 2: vo.lock
  workfile.rs      # Layer 2: vo.work
  manifest.rs      # Layer 2: vo.release.json
registry.rs        # Layer 3: GitHub release protocol
solver.rs          # Layer 4: deterministic solver
lock.rs            # Layer 5: lock authority
materialize.rs     # Layer 6: download + cache + frozen verification
workspace.rs       # Layer 7: vo.work resolution
ops.rs             # Layer 8: CLI command semantics
```

During Phases 1–8, consumer crates still depend on the old renamed crate.
At Phase 9, consumers are switched to the new `vo-module` and the old renamed crate is dropped from the workspace.

---

## 7. Development Strategy

The old `vo-module` is renamed before the rewrite begins.
The new `vo-module` crate is written from scratch and owns the canonical crate name.

Rules:

- the old crate is renamed (e.g. `vo-module-old`) and remains functional for existing consumers during Phases 1–8
- the new `vo-module` is developed and tested independently; it does not import or wrap the old crate
- consumers (`vo-analysis`, `vo-engine`, `vo-web`, `cmd/vo`) continue depending on the old renamed crate until Phase 9
- Phase 9 switches all consumers to the new `vo-module` and removes the old renamed crate from the workspace
- no code from the old crate is copied into the new crate; the new crate implements the spec from scratch

This avoids the failure modes of both incremental migration (indefinite coexistence) and in-place rewrite (broken consumers during development).

---

## 8. Testing Strategy

### Unit tests (Phases 1–7)

- identity: path/import/internal visibility rules from spec §3–§4
- version: canonical semver, prerelease, major-path boundary from spec §5.2
- schema: parse/render/golden for all four formats
- solver: highest-version, unsatisfiable, targeted update, prerelease gating
- lock: generation, consistency checks, round-trip
- materialization: digest verification, frozen-build missing-artifact errors
- workspace: identity mismatch, unlocked-dep detection

### Invariant tests (Phases 5–7)

- lock `deps` must equal published direct dependency set
- lock `artifact` must equal published artifact set (including size and digest)
- frozen build: no lock → hard error; missing artifact → hard error; digest mismatch → hard error
- workspace override does not introduce external deps outside root lock

### Integration tests (Phase 8)

- `vo mod add/update/sync/remove/download/verify` end-to-end
- `vo build/check/test/run` frozen behavior
- consumer parity: engine, web, analysis, CLI all agree

### Execution rules

- batch tests use `--release`
- all assertions use `assert!` / `assert_eq!`, not print-based failure
- golden fixtures are committed and version-controlled

---

## 9. Validation Commands

```text
cargo test -p vo-module --release
cargo test -p vo-analysis --release
cargo test -p vo-engine --release
cargo test -p vo-web --release
cargo test -p vo --release
cargo check -p vo --release
cargo check -p vo-module --release --target wasm32-unknown-unknown
cargo check -p vo-web --release --target wasm32-unknown-unknown
cargo check --release --target wasm32-unknown-unknown    # playground/rust
cargo check --release --target wasm32-unknown-unknown    # studio/wasm
```

---

## 10. Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| New crate drifts from spec during long development | High | Each phase is validated against spec sections before merge |
| Atomic cutover in Phase 8 is too large to land safely | High | Phase 8 is split into per-consumer sub-tasks; each sub-task is independently testable |
| Old renamed crate needs bug fixes during Phases 1–8 | Medium | Old crate fixes are minimal and do not refactor; new crate is a separate directory with no shared files |
| Registry protocol requires real GitHub API integration testing | Medium | Mock-based unit tests plus optional live integration test behind feature flag |
| `vo.work` TOML schema change breaks existing workspace files | Low | Workspace files are local-only and not committed to published modules; migration is one manual edit |
| Performance regression in solver or materialization | Low | Benchmark against current system before cutover; solver is pure computation and easy to profile |

---

## 11. Relationship to Previous Plans

### 2026-03-17 Landing Plan

That plan defined 6 phases for incremental cutover on the existing codebase.
This plan **supersedes** that approach.

The architectural goals are the same. The execution strategy is different:

- The landing plan assumed the existing code would be incrementally refactored into the target architecture.
- This plan assumes the existing code has reached its refactoring ceiling and a clean rewrite with atomic cutover is more efficient and less risky than continued incremental migration.

### 2026-03-18 Enforcement Plan

That plan is **completed** and serves as a pre-condition for this plan.

The enforcement pass unified validation rules and closed the most dangerous semantic gaps.
That work ensures the old system is stable enough to remain in service during Phases 1–7 while the new system is built in parallel.

---

## 12. Summary

- The old `vo-module` is renamed; a brand-new `vo-module` crate is written from scratch against the authoritative `spec/module.md`.
- 8 architectural layers with strict unidirectional dependencies.
- 9 implementation phases: core → schema → registry → solver → lock → materialization → workspace → ops → consumer adaptation.
- The new crate is developed and fully self-tested before any consumer is changed.
- Consumer adaptation is a separate final phase, not interleaved with core development.
- No dual-path coexistence. No incremental migration. No dual-path fallbacks.
