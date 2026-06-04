# Risk Ledger

Load this reference when a task touches docs, implementation status, generated
artifacts, module behavior, FFI, JIT, GC, Studio, or CI policy.

## Contents

- [Source Versus Docs](#source-versus-docs)
- [Known Stale Or Nuanced Areas](#known-stale-or-nuanced-areas)
- [Module And Dependency Risks](#module-and-dependency-risks)
- [Runtime And GC Risks](#runtime-and-gc-risks)
- [Generated Artifact Risks](#generated-artifact-risks)
- [Engineering-System Risks](#engineering-system-risks)

## Source Versus Docs

- Source wins over docs when exact current behavior matters.
- The checked-in `volang-dev` skill is the compact current maintainer guide,
  but repo maps can still lag after moves. Verify named paths.
- `lang/docs/spec/*` describes intended behavior and user-facing contracts; use
  it as background, then inspect source.
- Some specs are intentionally canonical for contracts but stale for
  implementation status. Exact current behavior still needs source and test
  verification.
- `lang/docs/dev-notes/*` is historical context. It often explains intent but
  may describe plans, not shipped behavior.
- `lang/docs/outdated/*` is explicitly stale.
- Playground generated docs mirror `lang/docs/spec/*` and
  `lang/docs/vo-for-gophers.md`; do not treat generated mirrors as source.

## Known Stale Or Nuanced Areas

- Native FFI docs may contain old `#[vo_extern]` examples. Current extension
  macros are `#[vo_fn]` and `#[vostd_fn]`.
- `lang/crates/vo-syntax/README.md` has old API examples and still lists
  `object` as a keyword. Check `token.rs` before documenting keywords.
- `lang/docs/spec/language.md` describes `goto` syntax and semantics, but the
  current checker rejects `goto` and manifest cases expect that diagnostic.
- Repo maps can lag path moves such as the migration from old language-test
  locations to `tests/lang`. Verify named paths.
- Studio installation docs can lag `rust-toolchain.toml`. Verify the pinned Rust
  toolchain before repeating version claims.
- Studio backend docs can lag JIT target env in `eng/tests.toml`. Verify the
  target definitions before citing thresholds for `jit`, `osr`, or GC targets.
- Studio module docs mention manual `vo.mod` edits followed by `vo mod download`.
  Prefer `vo mod sync` when resolving and writing `vo.lock`, then `vo mod
  download` when fetching pinned dependencies.
- `lang/docs/spec/module.md` has described frozen build commands as
  network-free. Source has allowed downloads of already-locked cache artifacts
  and native inline deps while still not re-solving or mutating `vo.mod` /
  `vo.lock`.
- `lang/docs/spec/module.md` has described `vo.web.json` as committed at module
  root. Verify current `vo-release` staging before changing release metadata or
  docs.
- `lang/docs/spec/native-ffi.md` has stale ABI snippets: macro name, result
  variants, and entry-count details need source verification before copying.
- `lang/docs/spec/native-ffi.md` suggests other top-level app tables may exist,
  but current extension metadata rejects unknown root keys and top-level
  `[studio]` in favor of `[extension.web]`.
- JIT docs, README, Studio docs, and source comments do not all phrase current
  status the same way. Verify `vo-vm` dispatch, `jit_mgr`, `vo-jit`, and tests
  before claiming JIT support level.
- Bytecode/JIT specs and comments can lag current opcodes and callback support.
  Verify `vo-common-core`, `vo-engine::run`, `vo-vm/src/vm/jit/*`, and
  `vo-jit` before editing or explaining backend behavior.
- `lang/docs/dev/jit-fact-source.md` documents the current maintainer
  contract. In code, `vo-jit/src/semantics` is the opcode fact source for
  capability, metadata requirement, register effects, runtime dependencies,
  verifier requirements/domain, lowering owner, frame/trap policy, fail-fast
  policy, and effect contracts.
- `metadata_contract.rs`, `effects`, `capability`, `contract_graph`,
  `verifier`, and lowering should derive opcode facts from semantic rows. Do
  not reintroduce scattered opcode-family match tables unless a new explicit
  row spec cannot express the fact.
- Strict JIT verification and compile paths should fail fast for invalid
  opcodes, missing/wrong JIT metadata, invalid references, slot/layout drift,
  helper or callback ABI drift, and call-shape mismatches. Legal runtime paths
  are explicit side exits, VM call materialization, runtime helpers, or runtime
  panic recording.
- Avoid using "fallback" for intentional JIT runtime paths. Prefer
  `RuntimePathPolicy`, `JitSideExitReason`, "side exit", and "VM call
  materialization"; legacy fallback wording is only compatibility terminology
  at the language-test manifest boundary.
- Dynamic access and error-sugar specs are intended behavior; current support
  crosses parser, checker, codegen, runtime builtins, and manifest expectations.
  Do not infer implementation status from spec text alone.
- `vo-web` comments can overstate module fetching. Bare single-file web compile
  rejects external imports and inline `require`; Studio `prepareEntry` handles
  browser dependency preparation.
- Studio docs say all examples execute via WASM. That is true for web mode, not
  native Tauri sessions.
- Older Studio/legacy Playground paths may leak previous product names such as
  `Vibe`. Grep current source before broad rename or branding claims.
- Native real-path inline modules with `require` can auto-install into the
  ephemeral cache through `vo-engine::compile_with_auto_install`; web and memory
  compile paths still reject external inline requirements.
- `vo test` is a user CLI command for compiling/running a project or tests
  directory. Repo regression testing is `vo-dev test run` or `./d.py test`.
- Studio and Playground do not share every runtime path. Studio uses
  `vo-app-runtime`; Playground legacy still has independent WASM/Rust glue.
- `RenderBuffer` is keep-latest. It is not a replay queue.

## Module And Dependency Risks

- `vo.work` can redirect local first-party dependencies. Use `VOWORK=off` for
  hermetic language-test expectations unless testing workspace behavior.
- Frozen build commands should not mutate `vo.mod` or `vo.lock` or re-solve the
  graph. Lifecycle commands under `vo mod` own graph mutation.
- `cmd/vo` build-like commands may call auto-install paths. Describe them as
  frozen with respect to graph solving and manifest/lock mutation, not as
  guaranteed network-free.
- Published module paths and imports use canonical GitHub identities. Relative
  or version-suffixed imports are invalid.
- `local/*` identities are reserved for ephemeral roots and must not appear in
  imports, requirements, published manifests, or lock entries.
- Native extension artifacts differ for local/workspace modules and published
  dependencies. Check readiness and artifact resolution source before changing
  errors.

## Runtime And GC Risks

- GC is non-moving, incremental, tri-color mark/sweep, and precise by
  `SlotType`. Do not describe it as conservative or moving.
- Interface values have special scanning rules: metadata slot first, data slot
  scanned only when metadata says it is GC-backed.
- Codegen slot metadata is safety-critical. `slot_types`, `capture_slot_types`,
  function call buffers, wrappers, and runtime extern return slot counts must
  remain consistent with GC and JIT scanning.
- Root scanning includes more than stacks and globals. Defer args, unwinding
  returns, panic state, closure replay, select queues, wait registrations,
  endpoint registries, JIT panic messages, sentinel errors, and materialized JIT
  frames can matter.
- Scheduler behavior is cooperative. Runtime outcomes can be completed,
  blocked, suspended, or suspended-for-host-events.
- Changes to calls, returns, defer, panic/recover, borrowed frames, JIT spills,
  wait registrations, queue/select state, or host events can be root-sensitive.

## Generated Artifact Risks

- Generated Playground docs must be produced by `scripts/ci/docs_sync.mjs` and
  checked by `scripts/ci/docs_lint.mjs`.
- Generated docs can mirror stale source specs until `docs-sync` and
  `docs-lint` run.
- Quickplay BlockKart assets are checked-in generated artifacts. Validate them
  with `scripts/ci/quickplay_validate.mjs` or `./d.py ci task quickplay-validate`.
- Artifact policy lives in `eng/artifacts.toml`; task generator/validator
  coverage is checked by `vo-dev lint artifacts`.
- Benchmarks can generate result files and compiled products. Keep generated
  benchmark outputs out of source changes unless explicitly requested.

## Engineering-System Risks

- `eng/*.toml` is the policy/data layer. `cmd/vo-dev` is the interpreter.
- `d.py` is only a compatibility wrapper and should not grow duplicated
  selectors or task behavior.
- GitHub workflow YAML should consume `vo-dev` matrix/metadata outputs instead
  of hard-coding tool/cache/task policy.
- Node workspace, Rust cache workspace, artifact, first-party, and release
  policies are linted; update data and code together.
- Release policy is part of the same data-driven system. Update
  `eng/release.toml` and `cmd/vo-dev/src/release_*` together, then validate with
  `vo-dev lint release` and release matrix/metadata commands.
