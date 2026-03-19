# Vo Module System 1.0 Landing Plan

**Date**: 2026-03-17
**Status**: Design — Approved direction
**Scope**: `vo-syntax`, `vo-analysis`, `vo-module`, `vo-engine`, `vo-web`, `cmd/vo`, module publishing/release tooling, first-party module cutover

---

## 1. Decision Summary

The new `spec/module.md` is not a documentation-only cleanup. It defines a **different and stricter module architecture** than the one the current implementation grew into.

This landing plan adopts that architecture directly.

The implementation is organized into **six architectural phases**:

1. **Canonical import and module identity cutover**
2. **`vo.mod` / `vo.lock` model split**
3. **GitHub release protocol and registry client**
4. **Deterministic resolution and explicit module lifecycle commands**
5. **Frozen build pipeline, workspace overrides, and artifact verification**
6. **First-party repository cutover and CI enforcement**

The key policy decisions are:

- **No dual semantic model** for alias imports vs canonical imports.
- **No long-term support** for `vo.sum` as the build contract.
- **No long-term support** for `files(...)` as a separate WASM-only module distribution protocol.
- **No implicit network access** during `build`, `check`, `test`, or `run`.
- **No published `replace` semantics** inside `vo.mod`.
- **No hidden compatibility fallback** that keeps the old architecture alive under the new names.

No compatibility fallback is part of the plan. Legacy state is removed directly by authoritative parser/CI cutover, not kept alive behind helper commands.

Temporary breakage policy:

- The `volang` tree must stay internally coherent after each phase.
- It is acceptable for old external module workflows to fail fast once the new model becomes authoritative.
- It is acceptable for publishing/downloading workflows to be temporarily incomplete between Phase 2 and Phase 4, as long as the direction of the tree is architecturally correct and clearly progressing toward the final protocol.

---

## 2. Why This Refactor Is Necessary

The previous review established that the current module system is not merely unfinished; it is **split across conflicting designs**.

The most important current contradictions are:

- The old spec describes alias-based external imports such as `@"gin"`, while the actual implementation is centered on full module-path imports.
- `vo.mod` currently mixes direct dependency intent with local override concepts (`replace`) and legacy WASM fetch details (`files(...)`).
- `vo.sum` is written during install but is not the true build contract and is not consistently verified during build/check.
- Native and WASM dependency acquisition do not follow one registry protocol.
- `vo get` / prepare / auto-install behavior does not define a clean module lifecycle.
- Build flows still contain install-oriented logic in places that should be frozen, deterministic consumers of already-resolved state.

That drift is already expensive:

- tooling semantics are unclear
- the CLI contract is not trustworthy
- web/native behavior diverges
- registry/publishing cannot be made robust on top of ad hoc fetch paths
- future module features would compound the confusion instead of fixing it

The correct response is not to patch the old design. The correct response is to **cut over to one clean model** and force the implementation to match it.

---

## 3. Hard Constraints

These constraints are non-negotiable.

### 3.1 GitHub is the registry

The module registry is GitHub Releases.

This means the implementation must standardize around:

- GitHub tags
- GitHub Releases
- a release manifest asset
- a canonical source-package asset
- optional target-specific artifact assets

### 3.2 Canonical full-path imports are the only import identity

The canonical package path is the identity used by:

- source imports
- resolver ownership
- lock file entries
- diagnostics
- cache keys
- extension artifact ownership

Alias imports are not part of the target architecture.

### 3.3 `vo.mod` is intent, `vo.lock` is execution contract

The implementation must enforce a strict split:

- `vo.mod`: human-authored direct dependency intent
- `vo.lock`: machine-generated exact resolved graph and verified artifact digests for the published registry graph

`vo.sum` is not the long-term build contract.

### 3.4 Build commands are frozen

`vo build`, `vo check`, `vo test`, and `vo run` must:

- not resolve dependencies
- not mutate lock state
- not access the network
- not silently install missing modules

### 3.5 One module graph across native and WASM

The dependency graph must be target-neutral.

Native and WASM may consume different locked artifacts, but they must come from the **same module version**, the **same release manifest**, and the **same lock file**.

### 3.6 `vo.work` is local-only

Local workspace overrides are valid only as local development state.
They must not leak into published module semantics.

`vo.work` may provide local shorthand such as `use ../vogui`, but that shorthand must desugar to a canonical module-path mapping after reading the target directory's `vo.mod`.

### 3.7 Preserve the correct recent extension-loading direction

Recent work already moved native extension preparation toward **manifest-driven loading of actually referenced extensions**, instead of eager workspace-wide builds.
That is the correct direction and must be preserved.

The new module system must integrate extension artifacts with this model instead of regressing to broad eager builds.

---

## 4. Target Architecture Snapshot

### 4.1 Final ownership model

```text
Source code
  |
  +-- imports canonical package paths
  |
  +-- vo.mod
  |     direct dependency intent + toolchain constraint
  |
  +-- vo.lock
  |     exact registry graph + verified digests
  |
  +-- vo.work
  |     local workspace overrides only; never the published graph
  |
  +-- vo.release.json (published release asset)
  |     machine-readable registry metadata
  |
  +-- source package / target-specific release artifacts
        downloaded and verified against vo.lock
```

### 4.2 Main subsystem responsibilities

| Area | Required end state | Primary code areas |
|------|--------------------|--------------------|
| Syntax / analysis | Canonical import paths only; no alias semantics in resolution | `lang/crates/vo-syntax`, `lang/crates/vo-analysis` |
| Manifest parsing | `vo.mod`, `vo.lock`, `vo.work` as distinct data models | `lang/crates/vo-module/src/modfile.rs`, `workfile.rs`, new lockfile module |
| Resolver / solver | Deterministic single-version graph from `vo.mod` + registry metadata | `lang/crates/vo-module/src/resolver.rs` |
| Fetch / cache | Release-manifest fetch, source package fetch, artifact verification, cache population | `lang/crates/vo-module/src/fetch.rs` |
| Engine | Build/check/run consume `vo.lock`; no auto-install in frozen path | `lang/crates/vo-engine/src/compile.rs`, `run.rs` |
| Web | Same registry protocol and artifact verification as native | `lang/crates/vo-web/src/module_install.rs` |
| CLI | Explicit lifecycle commands for add/update/sync/download | `cmd/vo/src/main.rs` |
| Publishing | Standardized release manifest + source package + artifact layout | release scripts / CI / docs |

### 4.3 What must disappear

The following patterns must disappear from the architectural definition of the module system:

- alias-based external import identity
- `vo.sum` as the build contract
- published `replace` directives in `vo.mod`
- WASM-only `files(...)` fetch protocol
- implicit install/resolve behavior inside build commands
- cache layout as identity contract

---

## 5. Phase 1 — Canonical Import and Module Identity Cutover

This is the first phase because every later layer depends on package identity being unambiguous.

### 5.1 Goals

After Phase 1:

- all non-stdlib imports are canonical full package paths
- the checker, resolver, and VFS use the same identity model
- alias imports are rejected or removed from the semantic pipeline
- project-relative intra-module imports no longer define module semantics

### 5.2 Required changes

1. **Syntax and AST cleanup**
   - Remove alias-based external import semantics from the language-level module contract.
   - `ImportKind::External` must stop being a second identity axis.
   - Keep only lexical aliasing such as `import foo "github.com/acme/lib/foo"`.

2. **Analysis / importer cleanup**
   - `vo-analysis` must resolve packages only by canonical package path.
   - Diagnostics must print canonical paths.
   - No hidden interpretation of import strings as “module alias first, path second”.

3. **Resolver / VFS cleanup**
   - `vo-module` and VFS resolution must use longest-prefix module ownership for canonical package paths.
   - Root-package and intra-module package resolution must work by module path prefix, not project-relative import spelling.

4. **Error contract**
   - old `@"name"` syntax must fail fast with an explicit canonical-import diagnostic
   - ambiguous old import patterns must not be guessed silently

### 5.3 Primary files to touch

- `lang/crates/vo-syntax/src/ast.rs`
- `lang/crates/vo-analysis/src/check/resolver.rs`
- `lang/crates/vo-analysis/src/project.rs`
- `lang/crates/vo-module/src/resolver.rs`
- `lang/crates/vo-module/src/vfs.rs`
- `lang/crates/vo-module/src/error.rs`

### 5.4 Acceptance criteria

- `std/...` imports resolve only to stdlib packages
- canonical full-path imports resolve consistently in syntax, analysis, and runtime preparation
- alias external syntax is no longer a supported module-system feature
- module ownership is defined by longest canonical module-path prefix

### 5.5 Validation

- add parser/resolver tests for canonical import forms
- add failure tests for `@"alias"` imports
- `cargo test -p vo-syntax --release`
- `cargo test -p vo-analysis --release`
- `cargo test -p vo-module --release`

---

## 6. Phase 2 — Split `vo.mod` and `vo.lock`

This phase creates the real data model that the spec describes.

### 6.1 Goals

After Phase 2:

- `vo.mod` contains only direct dependency intent and toolchain constraint
- `vo.lock` exists as the authoritative resolved graph
- `vo.sum` is removed from the mainline design
- `vo.work` is the only place for local path overrides

### 6.2 Required changes

1. **Redesign `vo.mod` parser and schema**
   - `require <module-path> <version-constraint>`
   - remove alias as dependency identity
   - remove published `replace` semantics from `vo.mod`
   - remove `files(...)` from `vo.mod`

2. **Introduce `vo.lock` data model**
   - exact module versions
   - source commit / release revision identity
   - release manifest digest
   - source package digest
   - locked target-artifact digests as needed
   - direct dependency edges for the resolved graph

3. **Introduce deterministic serialization**
   - stable sort order
   - stable formatting
   - exact round-trip behavior

4. **Define `vo.work` as the only override file**
   - canonical module path -> local directory
   - support current local shorthand `use ../module` by desugaring it through the target `vo.mod`
   - local directory must declare the same module identity

5. **Stop writing and reading `vo.sum`**
   - do not keep `vo.sum` alive under a different role
   - remove code paths that imply it is the source of truth

### 6.3 Legacy State Policy

Phase 2 does not depend on a dedicated migration command.

Instead:

- alias-based `require` lines are rejected by the authoritative parser
- local override semantics live only in `vo.work`
- `vo.sum` is deleted instead of preserved under another role
- repositories carrying legacy imports or manifest syntax must be fixed explicitly before they pass CI or release verification

### 6.4 Primary files to touch

- `lang/crates/vo-module/src/modfile.rs`
- `lang/crates/vo-module/src/workfile.rs`
- `lang/crates/vo-module/src/lib.rs`
- new `lang/crates/vo-module/src/lockfile.rs`
- callers in `vo-engine`, `cmd/vo`, and `vo-web`

### 6.5 Acceptance criteria

- `vo.mod` no longer carries alias identity or published override semantics
- `vo.lock` round-trips deterministically
- projects with external dependencies require `vo.lock` for frozen builds
- `vo.sum` is not used by the authoritative build path

### 6.6 Validation

- `cargo test -p vo-module --release`
- lockfile golden tests
- assert-based tests for rejecting legacy alias-require, `replace`, and `files(...)` state

### 6.7 Bootstrapping Note

Phase 2 must introduce the **final `vo.lock` schema**, not a weaker transitional lock format.

That means:

- `vo.lock` already requires exact module versions, immutable revision identity, release-manifest digest, source-package digest, and dependency edges
- local `vo.work` overrides do not change what `vo.lock` means; they only change where local development sources are read from
- if a temporary in-tree bootstrap path is needed before the full GitHub client from Phase 3 lands, it must still populate the final lock schema from explicit fixture metadata or equivalent verified inputs

The tree may temporarily remain incomplete for external publishing/downloading workflows until Phase 3 lands, but the meaning of `vo.lock` must not change between phases.

---

## 7. Phase 3 — GitHub Release Protocol and Registry Client

This phase standardizes the publish/download protocol.

### 7.1 Goals

After Phase 3:

- published modules are resolved from GitHub Releases by canonical module path and version
- each published version exposes `vo.release.json`
- each published version exposes a canonical source package
- optional target-specific artifacts are declared in release metadata and verified by digest

### 7.2 Required changes

1. **Release manifest schema**
   - define `vo.release.json`
   - include module path, version, commit, module-root path, direct requirements, source package metadata, artifact metadata

2. **Fetcher rewrite**
   - replace tarball-first ad hoc behavior with release-manifest-first fetch logic
   - fetch metadata first, then exact assets
   - verify digests before cache promotion

3. **Source package definition**
   - standardize that the package contains the module root contents, not an unspecified repository snapshot
   - enforce inclusion of `vo.mod` at archive root
   - optionally include `vo.lock` and `vo.ext.toml`

4. **Repository/subdir publishing rules**
   - define repository mapping from canonical module path
   - define module-root computation for root modules, nested modules, and `/vN` modules
   - define Git tag shapes using module-root-relative tags such as `graphics/v0.8.0` or `graphics/v2/v2.1.0`

5. **Artifact model unification**
   - native extension binaries and WASM artifacts become ordinary target-specific release assets
   - remove the special `files(...)` path and separate WASM fetch protocol from the architectural definition

6. **Publishing workflow**
   - release CI must produce `vo.release.json`
   - release CI must produce source package assets
   - release CI may additionally produce locked target artifacts

### 7.3 Primary files to touch

- `lang/crates/vo-module/src/fetch.rs`
- `lang/crates/vo-module/src/ext_manifest.rs`
- `lang/crates/vo-web/src/module_install.rs`
- release automation / docs / GitHub workflow templates

### 7.4 Acceptance criteria

- native and web loaders consume the same release metadata model
- source package verification is mandatory before use
- target-specific artifact selection is driven by release metadata and lock data
- `files(...)` is no longer needed to define published WASM module contents

### 7.5 Validation

- release-manifest parser tests
- end-to-end fetch tests against fixture releases or mocked GitHub responses
- `cargo test -p vo-module --release`
- `cargo test -p vo-web --release`

---

## 8. Phase 4 — Deterministic Resolution and Explicit Module Lifecycle

This phase establishes the real dependency workflow.

### 8.1 Goals

After Phase 4:

- `vo.mod` constraints are resolved deterministically into `vo.lock`
- exactly one version exists per canonical module path in a build graph
- dependency resolution happens only in explicit module-management commands
- build commands become pure consumers of `vo.lock`

### 8.2 Required changes

1. **Constraint model**
   - exact versions
   - `^` compatible ranges
   - `~` patch-compatible ranges
   - correct pre-release handling

2. **Deterministic solver**
   - highest compatible version wins
   - no network-order dependence
   - no timestamp-order dependence
   - no multiple-version support for the same canonical module path

3. **CLI lifecycle rewrite**
   - explicit commands equivalent to `mod add`, `mod update`, `mod sync`, `mod download`
   - existing top-level commands may call the new engine internally, but the lifecycle semantics must be unified in one implementation
   - include explicit `mod remove`, because pruning stale direct and transitive dependencies is part of the lifecycle contract

4. **Engine boundary cleanup**
   - move auto-install / auto-fetch behavior out of frozen build paths
   - make preparation consume already-resolved lock state
   - ensure missing lock or missing artifacts fail clearly instead of triggering implicit install

### 8.3 Primary files to touch

- `lang/crates/vo-module/src/resolver.rs`
- `lang/crates/vo-engine/src/compile.rs`
- `lang/crates/vo-engine/src/run.rs`
- `cmd/vo/src/main.rs`

### 8.4 Acceptance criteria

- dependency solving is deterministic from the same registry view
- build/check/test/run do not mutate `vo.mod` or `vo.lock`
- missing dependency state results in actionable hard errors
- the CLI exposes one coherent dependency lifecycle

### 8.5 Validation

- solver unit tests for version range interactions and conflicts
- CLI integration tests for add/update/sync/download/build flows
- `cargo test -p vo-module --release`
- `cargo test -p vo-engine --release`
- `cargo test -p vo --release`

---

## 9. Phase 5 — Frozen Build Pipeline, Workspace Overrides, and Artifact Verification

This phase hardens the execution path around the lock file.

### 9.1 Goals

After Phase 5:

- build commands consume only `vo.lock`
- cache entries are validated against locked digests
- `vo.work` overrides are explicit local state
- extension artifacts participate cleanly in the lock-and-verify flow

### 9.2 Required changes

1. **Frozen build enforcement**
   - hard error on missing lock when external modules are needed
   - hard error on missing locked artifacts instead of auto-install

2. **Cache verification**
   - content-address or equivalent verified-digest cache entries
   - no trust in directory names alone
   - cache layout remains implementation detail, not protocol identity

3. **Workspace semantics**
   - `vo.work` keys by canonical module path
   - shorthand `use ../module` is desugared locally to the same canonical mapping after reading target `vo.mod`
   - override target must declare the same module identity
   - release/CI mode must ignore `vo.work`

4. **Extension artifact integration**
   - extension source metadata and binary artifacts tie back to locked module identity
   - preserve manifest-driven build of only referenced extensions
   - no return to workspace-wide eager extension build behavior

### 9.3 Primary files to touch

- `lang/crates/vo-module/src/fetch.rs`
- `lang/crates/vo-engine/src/compile.rs`
- `lang/crates/vo-engine/src/run.rs`
- `lang/crates/vo-runtime/src/ext_loader.rs`
- `lang/crates/vo-web/src/module_install.rs`

### 9.4 Acceptance criteria

- frozen builds never reach the network
- workspace overrides cannot silently change published module identity
- locked artifacts are verified before load or execution
- extension loading remains manifest-driven and import-driven

### 9.5 Validation

- regression tests for missing-lock and missing-artifact failures
- workspace identity-mismatch tests
- extension-loading tests that prove only referenced manifests are built or loaded
- `cargo test -p vo-engine --release`
- `cargo test -p vo-module --release`
- `cargo test -p vo-runtime --release`
- `cargo test -p vo-web --release`

---

## 10. Phase 6 — First-Party Repository Cutover and CI Enforcement

This phase makes the new system real across the Vo ecosystem.

### 10.1 Goals

After Phase 6:

- first-party repositories follow canonical import rules
- first-party manifests use `vo.mod` + `vo.lock` + optional `vo.work`
- release pipelines emit the required metadata/assets
- CI rejects old semantics

### 10.2 Required changes

1. **Repository cutover**
   - update `volang`, `vogui`, `voplay`, `resvg`, `notify`, `image`, `zip`, and other first-party modules
   - rewrite imports to canonical paths where needed
   - generate `vo.lock`
   - remove `vo.sum`

2. **Release process cutover**
   - generate `vo.release.json`
   - publish source package assets
   - publish target artifacts where applicable

3. **CI enforcement**
   - fail if `vo.lock` is stale or missing for published modules
   - fail if old alias import syntax remains
   - fail if old `vo.sum`-based assumptions remain in module repos
   - fail if release metadata/assets are missing for publish jobs

4. **Documentation cleanup**
   - align CLI help, repository layout docs, and publishing docs with the landed implementation
   - remove outdated module-system docs once cutover is complete

### 10.3 Acceptance criteria

- first-party modules build under the new rules without compatibility fallbacks
- release CI emits the standardized assets
- docs, CLI, and implementation describe the same lifecycle

### 10.4 Validation

- `./d.py check`
- `./d.py test both --release`
- focused release-mode end-to-end tests for at least one pure-Vo module and one extension-bearing module

---

## 11. Cross-Cutting Engineering Rules

These rules apply throughout all phases.

### 11.1 Clean cutovers beat long-lived compatibility layers

If a temporary layer keeps the old architecture semantically alive, delete it.

Short-lived in-tree cleanup is acceptable.
Long-lived runtime fallback is not.

### 11.2 One source of truth per concept

- module identity: canonical module path
- package identity: canonical package path
- dependency intent: `vo.mod`
- resolved graph: `vo.lock`
- local overrides: `vo.work`
- registry metadata: `vo.release.json`

No concept should be split across two equally authoritative files.

### 11.3 Keep build and module-management codepaths separate

Build/check/run should not contain dependency-management policy.
Module-management commands should not be hidden inside compile preparation.

### 11.4 Fail fast on ambiguous legacy state

When legacy state is ambiguous:

- do not guess
- do not silently preserve old semantics
- emit a precise report and stop

### 11.5 Native and WASM are consumers of one protocol

Do not allow native and WASM to keep separate ideas of what a published module is.

They may use different locked artifacts, but they must share:

- module identity
- version identity
- release metadata
- source package definition
- lock file contract

---

## 12. Recommended Implementation Order Inside the Tree

The phases above (§5–§10) are **architectural phases**, not a claim that every commit lands in numeric order without interleaving.
They group the target architecture by concern.
The actual execution order interleaves them, because some later hardening work must land before earlier protocol work can be trusted end-to-end.

The practical execution order should be:

1. Cut syntax/analysis/resolution over to canonical import identity.
2. Introduce `vo.lock` and simplify `vo.mod` / `vo.work` responsibilities.
3. Rewrite engine boundaries so frozen build paths consume lock state instead of auto-installing.
4. Implement the GitHub release-manifest protocol in fetch/install layers.
5. Implement deterministic solver + explicit lifecycle commands on top of the new registry protocol.
6. Integrate artifact verification and workspace rules.
7. Cut over first-party repositories and CI.
8. Remove remaining legacy codepaths and docs.

This order is intentional:

- identity must be fixed before lock files make sense
- lock files must exist in final-schema form before frozen builds are enforceable
- frozen builds must be correct before registry/publishing semantics are trusted end-to-end
- first-party repos should cut over only after the architecture is real, not while the core remains half-old and half-new

---

## 13. Main Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Import cutover touches syntax, checker, resolver, and diagnostics at once | High | Execute Phase 1 as a deliberate semantic cutover before touching lock/registry work |
| Replacing `vo.sum` with `vo.lock` leaves partial old assumptions behind | High | Treat `vo.sum` removal as an explicit acceptance criterion, not a cleanup detail |
| Current build flows still auto-install in preparation paths | High | Move resolution/fetch policy into explicit lifecycle commands before finishing Phase 4 |
| Native and WASM fetching diverge again during implementation | High | Make Phase 3 release-manifest-first and require both native and web to consume it |
| Workspace overrides leak into published semantics | Medium | Restrict overrides to `vo.work`, add CI/release mode that ignores it |
| Extension support regresses into eager builds | Medium | Keep manifest-driven extension preparation as a required invariant in Phase 5 |

---

## 14. Short Version

- The new `module.md` must be landed as a **hard architectural cutover**, not as a cosmetic doc refresh.
- The implementation sequence is:
  1. canonical imports
  2. `vo.mod` / `vo.lock`
  3. GitHub release protocol
  4. deterministic solver + explicit module lifecycle
  5. frozen builds + verified artifacts + local-only workspace overrides
  6. first-party cutover + CI enforcement
- `vo.sum`, alias import identity, published `replace`, and WASM-only `files(...)` protocol are **not** part of the final system.
- Legacy state is cleaned directly; the landed design does not depend on a dedicated migration command.
- Native and WASM must converge on **one registry protocol and one lock contract**.
- Extension loading must stay **manifest-driven and reference-driven**, not eager.
