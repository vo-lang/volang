# Step 2: Release Artifact Contract Enforcement

**Date**: 2026-04-16
**Status**: Planning
**Parent**: `2026-04-15-vo-module-final-form-master-plan.md` — Step 2
**Depends on**: Step 1 (canonical `vo.ext.toml` schema — landed)
**Normative reference**: `lang/docs/spec/module.md` §6.2, §6.4

---

## 1. Objective

Update the release-staging pipeline (`vo-release`) so that `stage_release` cross-validates the caller-provided artifact set against the target-support contract declared in `vo.ext.toml`:

1. Every target declared in `vo.ext.toml` must have its complete artifact set present in the staged artifacts.
2. No artifact may claim a target that is not declared in `vo.ext.toml`.
3. If `vo.ext.toml` uses a legacy schema shape, `stage_release` fails via the Step 1 parser (already enforced).
4. Pure-source modules (no `vo.ext.toml`) continue to stage without artifacts as before.

---

## 2. Current State

### 2.1 `stage_release` today

```
stage_release(repo_root, options) -> ReleaseResult<StagedRelease>
```

Flow:
1. `verify_repo` — checks vo.sum, alias imports, vo.mod/vo.lock consistency
2. Read `vo.mod`, resolve version, commit, output dir
3. `build_source_package` — tar.gz from source files + `included_source_files` (reads `vo.ext.toml` include paths)
4. `prepare_artifacts` — hash and validate caller-provided `ArtifactInput` list
5. Build `ReleaseManifest`, render `vo.release.json`, write all outputs

**Gap**: Between steps 4 and 5, there is no validation that the prepared artifact set matches the declared target-support contract from `vo.ext.toml`. The caller can pass any artifacts (or none), and `stage_release` will blindly accept them.

### 2.2 `ArtifactInput` structure

```rust
pub struct ArtifactInput {
    pub kind: String,
    pub target: String,
    pub name: String,
    pub path: PathBuf,
}
```

Callers manually construct these. There is no enforcement that `kind`, `target`, or `name` match what `vo.ext.toml` declares.

### 2.3 `ExtensionManifest::declared_artifact_ids()` (from Step 1)

Step 1 already provides:

```rust
pub fn declared_artifact_ids(&self) -> Vec<DeclaredArtifactId>
```

This returns the sorted canonical set of `(kind, target, name)` tuples implied by the manifest's `[extension.native]` targets and `[extension.wasm]`. This is the authoritative declared contract.

### 2.4 Relevant spec rules

From `module.md`:

- §6.2: "If the packaged module contains `vo.ext.toml`, the published `artifacts` set MUST satisfy the declared target-support contract for that module version."
- §6.4: "If `vo.ext.toml` declares support for a target that requires a published binary artifact, `vo.release.json` MUST include that artifact set for the same target."
- §6.3: "If the packaged source contains `vo.ext.toml`, its declared published target-support set MUST be consistent with the artifacts recorded in `vo.release.json`."

---

## 3. Design

### 3.1 New validation step in `stage_release`

After `prepare_artifacts` and before building `ReleaseManifest`, insert a new function:

```rust
fn validate_artifact_contract(
    repo_root: &Path,
    prepared: &[PreparedArtifact],
) -> ReleaseResult<()>
```

This function:

1. Reads `vo.ext.toml` from `repo_root`. If absent, return `Ok(())` — pure-source module.
2. Parses via `parse_ext_manifest_content` (Step 1 canonical parser). Legacy schema triggers hard error here.
3. Calls `manifest.declared_artifact_ids()` to get the declared set.
4. Builds the staged set from `prepared` as `BTreeSet<(kind, target, name)>`.
5. Computes:
   - **missing**: declared but not staged
   - **undeclared**: staged but not declared
6. If either set is non-empty, return a structured error.

### 3.2 Error variant

Add a new `ReleaseError` variant:

```rust
ReleaseError::ArtifactContractViolation {
    missing: Vec<DeclaredArtifactId>,
    undeclared: Vec<DeclaredArtifactId>,
}
```

The `Display` implementation should produce a clear, actionable message showing which artifacts are expected and which are unexpected.

### 3.3 What about partial target sets?

The spec allows a module version to support only a subset of targets. The set of supported targets is determined by what `vo.ext.toml` declares — not by what the host can build. If the author declares `aarch64-apple-darwin` and `x86_64-unknown-linux-gnu`, both must have artifacts at release time.

This validation does **not** require the author to support all possible targets. It only requires that every declared target has its artifact.

### 3.4 Integration point

In `stage_release`, after line ~131 (`let prepared_artifacts = prepare_artifacts(...)?;`):

```rust
validate_artifact_contract(&repo_root, &prepared_artifacts)?;
```

---

## 4. Consumer Call Sites

### 4.1 `vo-release/src/repo.rs`

Only `stage_release` is affected. The new validation is entirely internal to this function.

### 4.2 CLI (`vo release stage`)

No change needed. The CLI already constructs `ArtifactInput` from user arguments and passes them to `stage_release`. If the user provides wrong/missing artifacts, the new validation reports the error.

### 4.3 CI workflows

Existing CI scripts that invoke `vo release stage` must pass the correct artifact set. The new validation will catch CI misconfigurations that previously produced silently invalid releases.

---

## 5. Test Cases

### 5.1 All declared artifacts present — pass

```rust
#[test]
fn stage_release_succeeds_when_all_declared_artifacts_are_present() {
    // vo.ext.toml declares native aarch64-apple-darwin + wasm
    // ArtifactInput includes both native lib + wasm + js_glue
    // stage_release succeeds
}
```

### 5.2 Declared native target missing artifact — fail

```rust
#[test]
fn stage_release_rejects_missing_declared_native_artifact() {
    // vo.ext.toml declares native for aarch64-apple-darwin + x86_64-unknown-linux-gnu
    // ArtifactInput only includes aarch64-apple-darwin
    // stage_release fails with ArtifactContractViolation mentioning linux target
}
```

### 5.3 Declared wasm missing artifact — fail

```rust
#[test]
fn stage_release_rejects_missing_declared_wasm_artifact() {
    // vo.ext.toml declares wasm standalone
    // ArtifactInput is empty
    // stage_release fails with ArtifactContractViolation mentioning wasm
}
```

### 5.4 Undeclared artifact present — fail

```rust
#[test]
fn stage_release_rejects_undeclared_artifact() {
    // vo.ext.toml declares wasm only
    // ArtifactInput includes wasm + an undeclared native artifact
    // stage_release fails with ArtifactContractViolation mentioning undeclared
}
```

### 5.5 Pure-source module with no `vo.ext.toml` — pass

```rust
#[test]
fn stage_release_succeeds_for_pure_source_module_without_ext_manifest() {
    // No vo.ext.toml
    // ArtifactInput is empty
    // stage_release succeeds
}
```

### 5.6 Pure-source module but artifacts provided — fail

```rust
#[test]
fn stage_release_rejects_artifacts_without_ext_manifest() {
    // No vo.ext.toml
    // ArtifactInput includes a wasm artifact
    // stage_release fails — artifacts require a vo.ext.toml declaration
}
```

### 5.7 Legacy `vo.ext.toml` schema — fail

```rust
#[test]
fn stage_release_rejects_legacy_ext_manifest_schema() {
    // vo.ext.toml uses [extension].path (legacy)
    // stage_release fails via Step 1 parser error propagation
}
```

### 5.8 Bindgen wasm missing js_glue artifact — fail

```rust
#[test]
fn stage_release_rejects_bindgen_wasm_missing_js_glue_artifact() {
    // vo.ext.toml declares bindgen wasm with js_glue
    // ArtifactInput includes .wasm but not .js
    // stage_release fails with ArtifactContractViolation
}
```

---

## 6. Implementation Order

### Phase A — Error type and validation function

1. Add `ArtifactContractViolation` to `ReleaseError` enum with `Display` implementation
2. Implement `validate_artifact_contract` in `repo.rs`
3. Wire into `stage_release` after `prepare_artifacts`

### Phase B — Test cases

4. Add all test cases from §5
5. Ensure existing tests still pass (some existing tests pass no artifacts for modules with `vo.ext.toml` — these may need artifact inputs added, or the `vo.ext.toml` fixtures need to be adjusted)

### Phase C — Existing test fixture audit

6. Review existing `stage_release` tests that write `vo.ext.toml` with native declarations but pass zero artifacts. These tests currently pass because there is no contract validation. After this step, they must either:
   - Provide matching artifact inputs, or
   - Remove/adjust the `vo.ext.toml` to not declare the targets they don't provide artifacts for

Specific tests to audit:
- `stage_release_includes_declared_include_files_from_dist_dirs` — has `vo.ext.toml` with native target but passes no artifacts
- `stage_release_includes_declared_include_directories_recursively` — same
- `stage_release_fails_when_declared_include_file_is_missing` — same (fails before artifact check, but still should be clean)

---

## 7. Risk Assessment

| Risk | Mitigation |
|---|---|
| Existing CI workflows fail after this change | This is intentional — catching previously silent misconfigurations. Workflows must provide the correct artifact set. |
| Existing tests break because they write `vo.ext.toml` without matching artifacts | Phase C explicitly audits and fixes these. Tests that only exercise include-path logic can use wasm-only manifests to avoid needing real native artifact files. |
| `validate_artifact_contract` reads `vo.ext.toml` a second time (already read by `included_source_files`) | Acceptable for correctness. If profiling shows overhead, a future pass can cache the parsed manifest. Premature optimization is not a goal. |
| Partial-target confusion: author declares 2 targets but only wants to publish 1 this release | The spec is clear: declared = required. If the author wants to drop a target, they must update `vo.ext.toml` first. |

---

## 8. Relationship to Other Steps

- **Step 1** (landed): Provides `ExtensionManifest`, `declared_artifact_ids()`, `DeclaredArtifactId`, and the canonical parser. Step 2 is a direct consumer.
- **Step 3** (target-artifact resolution API): Step 3 builds the install/verify-time resolution. Step 2 builds the publication-time enforcement. They address opposite ends of the artifact lifecycle but share the same `DeclaredArtifactId` type.
- **Step 6** (integrity cross-validation): Step 6 enforces `vo.ext.toml` ↔ `vo.release.json` consistency at lock-graph verification time (consumer side). Step 2 enforces it at publication time (producer side). Together they close the loop.

---

## 9. Validation

After completing all phases:

```sh
cargo test -p vo-release --release
cargo test -p vo-module --release
cargo check -p vo --release
```

Quick smoke for non-regression:

```sh
cargo test -p vo-engine --release
cargo test -p vo-web --release
cargo test -p vo-analysis --release
```

---

## 10. Deliverables

- `ArtifactContractViolation` error variant in `vo-release`
- `validate_artifact_contract` function in `repo.rs`
- Cross-validation wired into `stage_release` between artifact preparation and manifest construction
- Comprehensive test suite covering all declared/undeclared/missing/legacy/pure-source scenarios
- Existing test fixtures updated to satisfy the new contract
