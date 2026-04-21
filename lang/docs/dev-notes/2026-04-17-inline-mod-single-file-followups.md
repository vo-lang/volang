# Inline-Mod & Single-File Context Follow-Up Plan

**Date:** 2026-04-17
**Scope:** `vo-module` single-file classification, `vo-engine`
single-file compile pipeline, `vo-web` single-file compile pipeline,
CLI and studio surface, spec §5.6 / §10 conformance.
**Related spec:** `lang/docs/spec/module.md` §5.6 (inline `vo:mod`),
§10.1 (ad hoc programs), §10.2 (single-file ephemeral modules).

---

## 1. Purpose

Spec §5.6 introduced three single-file classifications:

1. `Project` — file has an ancestor `vo.mod`.
2. `EphemeralInlineMod` — file begins with `/*vo:mod ... */` and has no
   ancestor `vo.mod`.
3. `AdHoc` — no ancestor `vo.mod` and no inline mod.

The spec also defines strict precedence (§5.6.4), a reserved-prefix
namespace (§5.6.1), and ephemeral build isolation rules (§5.6.5–§5.6.6,
§10.2). The reference implementation partially covers this contract:

- ✅ `vo_module::inline_mod::parse_inline_mod_from_source` parses and
  validates inline mod bodies (reserved-prefix guard, single-block,
  duplicates, `replace` ban, `local/*` in `require` ban).
- ✅ `vo_module::project::load_single_file_context` classifies a source
  file into the three variants and enforces §5.6.4 precedence.
- ✅ `vo-engine` real-path and `compile_prepared_project` single-file
  paths route through `load_single_file_context`; ephemeral `require`
  entries raise `ModuleSystem{Missing/ModFile}` instead of silently
  falling back.
- ✅ `vo-syntax` lexer skips `/*vo:mod ... */` as a block comment
  (regression locked by `test_inline_mod_block_is_skipped_as_block_comment`).

What remains is wiring the classifier into the browser/studio compile
path, turning the ephemeral-require error into a real resolution
pipeline, tightening ad hoc semantics to spec §10.1, and adding
IDE-facing surfaces.

---

## 2. Gap Matrix

| # | Area | Current state | Gap | Severity |
|---|---|---|---|---|
| G1 | `vo-web` single-file compile (`prepare_single_file_input`) | Rejects external imports by syntactic inspection of parsed `import` statements; calls `load_project_context`, not `load_single_file_context`. | No §5.6 classification; inline `/*vo:mod*/` is silently tolerated as a block comment but its `module` / `require` lines are never consulted; error taxonomy is `Policy` instead of `ModFile`. | **High** — user-visible divergence between native and web toolchains. |
| G2 | Spec §10.1 compliance for `AdHoc` | `vo-engine` keeps the legacy behavior of applying ancestor `vo.work` overrides to ad hoc files (two existing tests rely on this). | Spec §10.1 forbids `vo.work` usage from ad hoc programs; this path silently violates the spec. | **High** — spec conformance + test hygiene. |
| G3 | Ephemeral dependency resolution (spec §5.6.5) | Non-empty `require` entries in `/*vo:mod*/` raise `Missing`. | No resolver, no cache-local ephemeral lock, no fetch path. | **Medium** — blocks the canonical use case of single-file scripts with third-party deps. |
| G4 | AST / LSP surface for inline mod | Lexer treats the block as a comment; parser/AST have no record of it. | No spans, no diagnostics with source positions, no `vo fmt` round-trip, no `vo.ext.toml`-style discoverability in IDE. | **Medium** — affects tooling quality. |
| G5 | Cache fingerprint coverage | Compile cache fingerprints source bytes (so inline mod content changes invalidate implicitly). | No explicit test that edits to the inline `require` list invalidate the cache; no guarantee the fingerprint hashes the classified context. | **Low** — defensive, but worth a targeted regression test. |
| G6 | `vo build <file.vo>` for inline mod | Real-path path compiles fine; `.vob` output is emitted. | Not explicitly tested; behavior on `require` non-empty (hard error) may surprise users who expect a packaged artifact. | **Low** — document behavior, add one integration test. |
| G7 | `vo init --script` / templates | No template. | Minor UX: users who want to start a script file have to hand-type `/*vo:mod*/`. | **Low** — nice-to-have. |
| G8 | Studio / Tauri routes | `studio/src-tauri` calls `vo_engine::compile_with_auto_install` which already benefits from A. | None known — but needs smoke test for studio "Run script" with inline mod file. | **Low** — verification only. |
| G9 | Spec §5.6.6 negative checks | Inline mod parser rejects `replace`, duplicate `module`/`vo`, `local/*` in require, reserved-prefix mismatch. | Missing: explicit rejection of `vo.ext.toml` or extension metadata alongside inline mod; not asserted anywhere. | **Low** — add assertion and test. |
| G10 | Documentation | Spec text exists; implementation exists. | No tutorial-style dev note explaining how to write / run / cache an inline-mod script; no migration note about AdHoc vs EphemeralInlineMod. | **Low** — documentation hygiene. |

---

## 3. Prioritized Steps

The steps are ordered by dependency and severity. Each step is
self-contained and test-gated.

### Step P1 — `vo-web` single-file path aligned to `SingleFileContext`

**What.** Replace `prepare_single_file_input` (in
`lang/crates/vo-web/src/compile.rs`) so that it calls
`vo_module::project::load_single_file_context` on the in-memory `MemoryFs`
holding the single file, then branches on the returned variant:

- `Project(_)`: currently impossible for in-memory single-file inputs
  (no ancestor `vo.mod` is synthesized by the web harness); surface a
  clear error if it ever occurs.
- `EphemeralInlineMod { inline_mod, .. }`: reuse the engine helper
  `ensure_inline_mod_ephemeral_build_is_supported`. If `require` is
  non-empty the web frontend must produce the same
  `Missing/ModFile` diagnostic until P3 lands.
- `AdHoc { .. }`: current behavior; no module deps.

Delete the bespoke external-import policy check in favor of the
classifier's diagnostics. External import rejection for ad hoc files
now flows naturally through the existing `read_project_deps` validation
(spec §10.1).

**Why.** Today a pasted `/*vo:mod*/ require github.com/.../x ^0.1.0 */`
in the playground compiles silently as if the inline block were a
comment, then errors later at import resolution with a policy message —
hiding the real cause. Native and web toolchains must agree on
classification semantics.

**Risk.** The web path currently accepts single files that import
stdlib packages without any module declaration; the classifier keeps
that behavior under `AdHoc`. External imports will still be rejected,
but with a `ModFile` / `LockFile` diagnostic instead of the current
`Policy` one — callers that pattern-match on stage will need updating.

**Validation.**
- Add `vo-web` unit tests mirroring the engine's inline-mod suite:
  happy-path ephemeral compile, `require` → `Missing` error, reserved
  sentinel → `ParseFailed`, external import without mod → existing
  rejection shape.
- Ensure playground + studio WASM still compile (`studio/wasm`).

---

### Step P2 — Spec §10.1 compliance for `AdHoc`

**What.**
1. Remove the `AdHoc` fallback that re-invokes `load_project_context`
   in `real_path_compile_context_for_single_file` (see
   `lang/crates/vo-engine/src/compile/mod.rs`). Ad hoc programs must
   see no ancestor `vo.work` and no ancestor `vo.mod` overrides.
2. Migrate the two legacy tests that depend on the old behavior:
   - `test_compile_prefers_local_replace_extension_manifest_paths`
   - `test_compile_single_file_without_vo_mod_uses_ancestor_workfile_extension_manifest`

   Both intentionally compile a file under `volang/examples/` that
   resolves `github.com/vo-lang/vogui` via an ancestor `vo.work`.
   Per spec §10.1 this is illegal. Convert the fixture to a proper
   project (add a minimal `vo.mod` at the appropriate ancestor) so
   the same assertion validates the `Project` code path instead.
3. Add a regression test asserting that an `AdHoc` file whose nearest
   ancestor has only a `vo.work` (no `vo.mod`) builds with an **empty**
   `workspace_replaces` map.

**Why.** Keeping the fallback is an explicit deviation from spec that
will silently allow misconfigured projects to appear to work. We locked
the deviation in the initial SingleFileContext wiring with a
`// Ad hoc single-file entries preserve legacy behavior` comment; this
step retires that deviation.

**Risk.** External users in the wild may have scripts under
`volang/examples` or similar that rely on the ancestor-work fallback.
Mitigation: land P2 as a minor-version change; include a migration note
in the release changelog pointing users to promote scripts to
single-file ephemeral modules (with inline mod) or small projects.

**Validation.**
- Engine tests from step 2.2 pass.
- New regression test from step 2.3 passes.
- Workspace-wide `cargo test` green.

---

### Step P3 — Ephemeral dependency resolution (spec §5.6.5)

**What.** Turn `/*vo:mod*/` `require` entries into real builds.

1. **Cache layout.** Define a cache namespace for ephemeral modules at
   `$VO_CACHE/ephemeral/<content-hash>/` where `<content-hash>` is
   derived from the canonicalized inline mod body. Inside that dir:
   `vo.mod` (synthesized from the inline block, canonical form),
   `vo.lock` (generated by the solver), and the usual `.vo-cache/`.
2. **Synthesis.** `vo-module` grows
   `InlineMod::to_synthetic_mod_file()` → canonical `ModFile` and a
   corresponding root lock-root descriptor.
3. **Solver integration.** Run the existing
   `vo_module::lifecycle::solve_and_lock` against the synthetic root
   to produce `vo.lock`. Write the lock into the cache namespace and
   reuse the standard download / verify pipeline.
4. **Engine.** Replace `ensure_inline_mod_ephemeral_build_is_supported`
   in `compile/pipeline.rs` with a real `ephemeral_project_context`
   builder that populates `ProjectCompileContext.project_deps` with the
   resolved locked modules and `project_root` pointing at the cache dir
   (but `source_root` still pointing at the user's file dir, so file
   collection keeps working).
5. **Cache invalidation.** Content-hash includes every inline mod field
   (module, vo constraint, full sorted require list with constraints);
   any edit to the `/*vo:mod*/` block produces a new cache bucket.
   Garbage-collect unreachable buckets behind `vo mod tidy`.

**Why.** Inline mod with external deps is the reason §5.6.5 exists —
spec §10.2 explicitly requires a fully resolved graph. Without P3,
useful inline-mod programs (scripts that import `github.com/...`
packages) cannot build.

**Risk.** Cache correctness and concurrent invocation safety are the
hard parts. Re-use the locking primitives already used by the
project-level cache (see `vo_module::cache::install`).

**Validation.**
- Integration test: a `main.vo` with an inline mod declaring
  `require github.com/vo-lang/resvg ^0.1.0` compiles and runs after
  `vo run main.vo` downloads the dep into the ephemeral bucket.
- Edit-inline-require test: changing the constraint produces a new
  bucket and re-solves.
- Spec §10.2 frozen-build invariant: the resolved graph is the only
  source of truth; the solver never consults the user's working
  directory.

---

### Step P4 — AST / LSP surface for inline mod

**What.** Model inline mod metadata in the AST without disturbing the
current "block-comment is invisible to parser" stance.

1. **Syntax.** Add an optional `InlineModMetadata` field to the
   top-level `File` AST node. The parser's comment handling
   pre-scans the leading trivia; when it encounters `/*vo:mod ... */`
   it hands the block to `vo_module::inline_mod::parse_inline_mod_from_source`
   and attaches the result to `File`.
2. **Diagnostics.** Inline mod parse errors produce `ParseError`s with
   real spans (offsets within the leading trivia), not today's string
   `ProjectDepsError`. The engine maps both into a unified
   diagnostic stream.
3. **`vo fmt`.** Preserve and reformat inline mod blocks: canonical
   ordering (`module`, `vo`, blank, `require` entries alphabetized),
   stable indentation.
4. **LSP.** Expose hover / go-to-definition for module paths in inline
   mod (`module local/demo`, `require github.com/...`).

**Why.** Improves discoverability and tooling parity with `vo.mod`.

**Risk.** Parser changes are load-bearing; gate behind
thorough syntax test coverage (already abundant in `vo-syntax`).

**Validation.**
- `vo-syntax` tests: `File.inline_mod` populated when present; `None`
  otherwise; parse errors have accurate spans.
- `vo fmt` round-trip test.

---

### Step P5 — `vo build <file.vo>` coverage

**What.** Add an integration test that `vo build main.vo` (ephemeral,
no `require`) produces a `.vob`, and that `vo build main.vo` with
non-empty `require` fails loudly (pre-P3) or succeeds (post-P3).
Document the bytecode artifact's module name — for ephemeral modules,
the synthesized `local/...` module name must appear in the artifact
metadata.

**Why.** Prevent regressions in the less-exercised `vo build` CLI
path.

---

### Step P6 — Cache fingerprint regression for inline mod

**What.** In `lang/crates/vo-engine/src/compile/tests/cases.rs`, add a
dedicated test paralleling the existing
`test_compile_with_cache_fingerprint_tracks_modfile_replace_sources`:

1. Compile a file with `/*vo:mod\nmodule local/demo\nvo ^0.1.0\n*/`.
2. Record cache fingerprint.
3. Edit inline body to `/*vo:mod\nmodule local/demo\nvo ^0.2.0\n*/`.
4. Compile again; assert fingerprints differ.

Rationale: source-bytes-based fingerprinting already covers this
transitively; lock it in as an explicit invariant.

---

### Step P7 — Spec §5.6.6 negative-check closure

**What.** Assert that an inline mod block cannot co-exist with a
`vo.ext.toml`-style extension manifest or any `replace` directive.
The parser already rejects `replace`; add:

- An engine-level test confirming that a single-file ephemeral with an
  inline mod **plus** an auxiliary `vo.ext.toml` in the same directory
  is rejected at the manifest-discovery stage.
- A parser-level test confirming duplicate `vo` / `module` directives
  are diagnosed with line numbers.

---

### Step P8 — Documentation & examples

**What.** Add `lang/docs/spec/module-inline-mod-tutorial.md` (or
similar) walking through:

- Writing a single-file ephemeral script.
- The difference vs ad hoc (when to promote).
- The reserved-prefix contract (`/*vo:` is off-limits to user
  comments that aren't `/*vo:mod*/`).
- Ephemeral cache layout (post-P3).

Cross-link from `module.md` §5.6 and the root README's "Getting
started" section.

---

## 4. Dependency Order

```
P1 ──┐
     ├──→ P3 ──→ P5
P2 ──┘                ↘
                       P6
P4 (independent of P1–P3; touches parser)
P7 (independent)
P8 (after P1+P2+P3 stabilize)
```

- P1 and P2 are independent and can land in either order.
- P3 builds on the single-file classifier and benefits from P2's
  tightened ad hoc semantics (no accidental `vo.work` leakage into
  the ephemeral project context).
- P4 is orthogonal and can land any time; it changes the parser
  surface but not the engine contract.
- P5 / P6 are cheap test additions once the underlying paths exist.
- P7 is parser-level hygiene; can ride alongside P4.
- P8 documents the final state; land after P1–P3.

---

## 5. Validation Gates

After each step, all must pass:

- `cargo test --workspace --exclude vo-studio-wasm`
- `cargo clippy --workspace --all-targets -- -D warnings`
- `cargo fmt --check`
- Relevant `./d.py test` scenarios:
  - `./d.py test vm` (language semantics)
  - `./d.py test jit` (JIT compile path for ephemeral entries)
  - For P1: playground / studio WASM smoke build

Spec conformance gate (post-P2, post-P3): a dedicated spec-conformance
test suite under `lang/crates/vo-module/tests/spec_compliance.rs`
asserts each numbered rule in spec §5.6 and §10 individually.

---

## 6. Non-Goals

- Changing inline mod syntax (grammar is fixed by spec §5.6).
- Extending `/*vo:*/` to additional directive namespaces beyond `mod`
  (explicitly reserved by spec §5.6.1 for future expansion).
- Redesigning `vo.mod`, `vo.lock`, or `vo.work` semantics.
- Changing the solver algorithm.

---

## 7. Tracking

Progress on each step should be recorded in this file's header via
commit amendment ("P1 done", etc.) rather than by spawning additional
dev-note files. When all P1–P8 are green, archive this document by
adding a "Closed" banner and linking to the final test suite.
