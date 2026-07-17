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
- Development plans can be mixed records: a single file may contain current
  implementation status, completed migration phases, original risk checkpoints,
  and future maintenance checklists. Read the status/document-role section
  first; do not treat historical problem summaries or required changes as
  current defects without checking source and tests against completion status.
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
- Module lifecycle uses `vo mod sync` to solve/write and `vo mod fetch` to
  materialize an existing lock. Keep graph mutation commands free of package
  downloads, and keep `verify`, `why`, `graph`, and `snapshot` read-only.
- `lang/docs/spec/module.md` has described frozen build commands as
  network-free. Source has allowed downloads of already-locked cache artifacts
  and native inline deps while still not re-solving or mutating `vo.mod` /
  `vo.lock`.
- The current release chain is `vo.lock` v3 -> raw `vo.release.json` v2 -> raw
  `vo.package.json` v1 -> exact file bytes, with source/archive/artifact digests
  bound by the release. Browser consumers use this chain directly; do not
  recreate a browser-only manifest or duplicate release fields in the lock.
- `lang/docs/spec/native-ffi.md` has stale ABI snippets: macro name, result
  variants, and entry-count details need source verification before copying.
- Extension protocol separates public `[extension.*]` runtime metadata from
  local `[build.*]` adapters. Unknown root keys and top-level `[studio]` are
  rejected. Local Cargo/prebuilt/WASM paths must stay out of publication
  identity. The optional `[extension.native].library` public stem can differ
  from Cargo package and library target names.
- JIT docs, README, Studio docs, and source comments do not all phrase current
  status the same way. Verify `vo-vm` dispatch, `jit_mgr`, `vo-jit`, and tests
  before claiming JIT support level.
- Bytecode/JIT specs and comments can lag current opcodes and callback support.
  Verify `vo-common-core`, `vo-engine::run`, `vo-vm/src/vm/jit/*`, and
  `vo-jit` before editing or explaining backend behavior.
- VM/runtime-boundary docs are active design contracts with historical context.
  Treat `vm-runtime-boundary-architecture.md` as the current invariant map and
  `vm-runtime-boundary-repair-plan.md` as a completed repair record when its
  status says phases landed. For boundary work, inspect
  `vo-vm/src/runtime_boundary.rs`, scheduler wake-key APIs, `vm/jit/*`
  transition/callback code, and the current source-audit sections before
  reporting a defect. Endpoint stale-response non-decrement and terminal
  discard of uncommitted pending JIT transitions are intentional contracts
  unless current source or tests violate the acceptance matrix.
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
- JIT callbacks may publish pending runtime transitions while generated code
  continues to the next VM boundary. Pending side effects commit with the next
  side-effect-carrying boundary and are discarded as a unit on terminal discard;
  do not assume every interpreter opcode boundary is observable by generated
  code.
- Avoid using "fallback" for intentional JIT runtime paths. Prefer
  `RuntimePathPolicy`, `JitSideExitReason`, "side exit", and "VM call
  materialization"; manifest side-exit wording is parsed only at the
  language-test boundary.
- Dynamic access and error-sugar specs are intended behavior; current support
  crosses parser, checker, codegen, runtime builtins, and manifest expectations.
  Do not infer implementation status from spec text alone.
- `vo-web` comments can overstate module fetching. Single-file compilation has
  no external dependency mode. Third-party imports require a project with
  `vo.mod` and committed `vo.lock`.
- Studio docs say all examples execute via WASM. That is true for web mode, not
  native Tauri sessions.
- Older Studio/Playground paths may leak previous product names such as
  `Vibe`. Grep current source before broad rename or branding claims.
- Inline modules are dependency-free on native, web, and memory paths.
- `vo test` is a user CLI command for compiling/running a project or tests
  directory. Repo regression testing is `vo-dev test run` or `./d.py test`.
- Studio and Playground do not share every runtime path. Studio uses
  `vo-app-runtime`; the older Playground still has independent WASM/Rust glue.
- `RenderBuffer` is keep-latest. It is not a replay queue.

## Module And Dependency Risks

- `vo.work` can redirect local first-party dependencies. Use `VOWORK=off` for
  hermetic language-test expectations unless testing workspace behavior.
- `vo.work` v1 contains only `version` and `members`; member identity comes
  from each member's pure-TOML `vo.mod`. Never add path-to-module identity maps
  or let workspaces rewrite the registry graph.
- Frozen build commands should not mutate `vo.mod` or `vo.lock` or re-solve the
  graph. Lifecycle commands under `vo mod` own graph mutation.
- `cmd/vo` build-like commands may call auto-install paths. Describe them as
  frozen with respect to graph solving and manifest/lock mutation, not as
  guaranteed network-free.
- Published module paths and imports use canonical GitHub identities. Relative
  or version-suffixed imports are invalid.
- `local/*` identities are reserved for ephemeral roots and must not appear in
  imports, dependencies, published manifests, or lock entries.
- `vo.lock` v3 stores release digests and dependency edges, with no source,
  commit, package, or artifact duplication. Authenticate the raw release bytes
  before using those derived fields.
- Native extension artifacts differ for local/workspace modules and published
  dependencies. Check readiness and artifact resolution source before changing
  errors.
- FFI provider identity is assigned by the registry. Function pointer addresses
  are not stable identities in optimized release builds because LLVM may merge
  identical functions.

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
- Host-event, island, queue, and I/O wakes must use validated wake keys and
  source checks. Raw fiber slot IDs are vulnerable to reuse and generation
  drift.
- Endpoint response accounting follows accepted state changes:
  `pending_island_responses` tracks live obligations, so stale generation,
  wrong source, wrong endpoint, or wrong response-kind commands must not
  decrement it while the real obligation remains pending.
- Boundary effects are also GC effects. When adding a new suspend/block/yield
  path, audit `GcRootEffect`, dirty fiber epochs, pending JIT extern payloads,
  and whether terminal discard drops every pending side effect.
- Pending transition effects are scheduler-visible boundary work. Queue/select
  helpers may already have committed queue-local object state; report a
  pending-discard issue only with a reachable path where that committed state
  leaves a waiter dependent on a discarded pending wake.

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
