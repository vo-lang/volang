# Subsystem Playbooks

These recipes are starting points. Always inspect the current source around the
entry points before editing.

## Contents

- [Add Or Change Syntax](#add-or-change-syntax)
- [Change Type Rules Or Diagnostics](#change-type-rules-or-diagnostics)
- [Change Codegen Lowering Without Adding Opcodes](#change-codegen-lowering-without-adding-opcodes)
- [Add Or Change A Bytecode Instruction](#add-or-change-a-bytecode-instruction)
- [Change VM Runtime Boundary Or Scheduler Wakes](#change-vm-runtime-boundary-or-scheduler-wakes)
- [Change JIT Semantics, Contracts, Or Lowering](#change-jit-semantics-contracts-or-lowering)
- [Change GC Or Root Scanning](#change-gc-or-root-scanning)
- [Change Module Resolution Or Lock Semantics](#change-module-resolution-or-lock-semantics)
- [Change Module Release Or Staging Behavior](#change-module-release-or-staging-behavior)
- [Add Or Change Stdlib API](#add-or-change-stdlib-api)
- [Change Native Extension Or FFI Behavior](#change-native-extension-or-ffi-behavior)
- [Change Studio Or GUI Runtime](#change-studio-or-gui-runtime)
- [Change Playground Or Docs Mirrors](#change-playground-or-docs-mirrors)
- [Change Quickplay Or Checked-In Artifacts](#change-quickplay-or-checked-in-artifacts)
- [Change Examples Or Benchmarks](#change-examples-or-benchmarks)
- [Change Engineering Tasks Or CI](#change-engineering-tasks-or-ci)
- [Change Release Automation](#change-release-automation)

## Add Or Change Syntax

Touch likely areas:

- `lang/crates/vo-syntax/src/token.rs`
- `lang/crates/vo-syntax/src/lexer.rs`
- `lang/crates/vo-syntax/src/ast.rs`
- `lang/crates/vo-syntax/src/parser/*.rs`
- `lang/crates/vo-syntax/src/display.rs`
- `lang/crates/vo-analysis/src/check/*`
- `lang/crates/vo-codegen/src/*` if executable
- `tests/lang/cases` and `tests/lang/manifest.toml`
- `lang/docs/spec/language.md` if user-facing behavior changes

Checks:

```sh
cargo test -p vo-syntax
./d.py test both tests/lang/cases/<case>.vo
```

Watch for shared `IdState`, global span bases, automatic semicolon insertion,
formatter normalization, inline module comments, and AST IDs consumed by
`TypeInfo`. Inline module metadata is attached to `File.inline_mod`; dependency
semantics belong to `vo-module`, not the parser.

## Change Type Rules Or Diagnostics

Touch likely areas:

- `lang/crates/vo-analysis/src/check/checker.rs`
- `lang/crates/vo-analysis/src/check/expr.rs`
- `lang/crates/vo-analysis/src/check/stmt.rs`
- `lang/crates/vo-analysis/src/check/typexpr.rs`
- `lang/crates/vo-analysis/src/check/assignment.rs`
- `lang/crates/vo-analysis/src/check/errors.rs`
- `lang/crates/vo-codegen/src/type_info.rs`
- `lang/crates/vo-codegen/src/context.rs`
- `lang/crates/vo-codegen/src/func.rs`
- `tests/lang/manifest.toml`

Checks:

```sh
cargo test -p vo-analysis
./d.py test both tests/lang/cases/<case>.vo
cargo run -q -p vo-dev -- test run --suite lang --targets compile
```

When diagnostics are the behavior, add expected-failure manifest patterns.
When runtime output changes, add pass cases too.

Type checker changes can affect later codegen through `TypeInfo`, selections,
escape results, closure captures, sendability results, init order, and untyped
constant recording. Re-check codegen consumers before treating a checker change
as diagnostic-only.

## Change Codegen Lowering Without Adding Opcodes

Touch likely areas:

- `lang/crates/vo-codegen/src/lib.rs`
- `lang/crates/vo-codegen/src/context.rs`
- `lang/crates/vo-codegen/src/func.rs`
- `lang/crates/vo-codegen/src/type_info.rs`
- `lang/crates/vo-codegen/src/assign.rs`
- `lang/crates/vo-codegen/src/expr/*`
- `lang/crates/vo-codegen/src/stmt/*`
- `lang/crates/vo-codegen/src/wrapper.rs`
- `lang/crates/vo-runtime/src/*` if builtins or ABI helpers are involved
- `tests/lang/cases` and `tests/lang/manifest.toml`

Checks:

```sh
cargo test -p vo-codegen
./d.py test both tests/lang/cases/<case>.vo
./d.py test jit tests/lang/cases/<case>.vo
./d.py test wasm tests/lang/cases/<case>.vo
```

`compile_project` registers types, collects declarations, compiles functions,
emits init/entry wrappers, collects promoted methods, finalizes itabs, builds
runtime type metadata, fills well-known types, finalizes debug info, and checks
24-bit ID limits. Preserve that ordering unless the change deliberately reworks
module metadata.

Audit `builtin_extern_ret_slots`, wrapper caches, method-value wrappers,
closure capture slot metadata, loop hints, named return/error slots, and
storage strategy decisions when touching dynamic access, defer/errdefer/fail,
go/island, interfaces, embedded methods, or pointer receiver behavior.

## Add Or Change A Bytecode Instruction

Touch likely areas:

- `lang/crates/vo-common-core/src/instruction.rs`
- `lang/crates/vo-common-core/src/bytecode.rs`
- `lang/crates/vo-common-core/src/serialize.rs`
- `lang/crates/vo-engine/src/format.rs`
- `lang/crates/vo-codegen/src/*`
- `lang/crates/vo-vm/src/exec/*`
- `lang/crates/vo-vm/src/vm/mod.rs`
- `lang/crates/vo-jit/src/semantics/*`
- `lang/crates/vo-jit/src/metadata_contract.rs`
- `lang/crates/vo-jit/src/verifier/*`
- `lang/crates/vo-jit/src/translate/*`
- `lang/crates/vo-jit/src/contract_graph/*`
- `lang/crates/vo-vm/src/vm/jit/*` for JIT bridge/materialization behavior
- `tests/lang/cases` and `tests/lang/manifest.toml`

Checks:

```sh
./d.py test both tests/lang/cases/<case>.vo
./d.py test jit tests/lang/cases/<case>.vo
./d.py test osr tests/lang/cases/<case>.vo
./d.py test wasm tests/lang/cases/<case>.vo
```

Audit serialization, text dump, debug info, GC slot metadata, call/return frame
shape, `FunctionDef` derived metadata, `slot_types`, `capture_slot_types`,
runtime/JIT ABI helper needs, JIT side exits, and VM call materialization.

## Change VM Runtime Boundary Or Scheduler Wakes

Touch likely areas:

- `lang/crates/vo-vm/src/runtime_boundary.rs`
- `lang/crates/vo-vm/src/scheduler.rs`
- `lang/crates/vo-vm/src/fiber.rs`
- `lang/crates/vo-vm/src/exec/*`
- `lang/crates/vo-vm/src/vm/mod.rs`
- `lang/crates/vo-vm/src/vm/jit/*`
- `lang/crates/vo-runtime/src/jit_api.rs`
- `lang/docs/dev/vm-runtime-boundary-architecture.md`
- `lang/docs/dev/vm-runtime-boundary-repair-plan.md`

Checks:

```sh
cargo test -p vo-vm
cargo test -p vo-vm --features jit
./d.py test both tests/lang/cases/<case>.vo
./d.py test osr tests/lang/cases/<case>.vo
./d.py test wasm tests/lang/cases/<case>.vo
cargo run -q -p vo-dev -- task run gc-contract
```

Use `RuntimeTransition`/`RuntimeCommand` for runtime side effects. JIT
callbacks that cannot commit immediately should publish pending transitions and
let the next VM boundary commit them; terminal discard must drop the pending
transition as a unit. Do not mutate scheduler state directly from JIT callback
helpers unless the helper is explicitly the boundary applier.

Use `FiberWakeKey`/generation checks for host event, island, queue, and I/O
wakes. Raw `FiberId`/slot identity is insufficient across reuse. Treat
`GcRootEffect` as part of the boundary contract: new block/yield/suspend paths
must either mark the current fiber, all roots, or intentionally preserve the
existing dirty-root protocol.

## Change JIT Semantics, Contracts, Or Lowering

Touch likely areas:

- `lang/docs/dev/jit-fact-source.md`
- `lang/crates/vo-jit/src/semantics/*`
- `lang/crates/vo-jit/src/metadata_contract.rs`
- `lang/crates/vo-jit/src/effects/*`
- `lang/crates/vo-jit/src/capability.rs`
- `lang/crates/vo-jit/src/contract_graph/*`
- `lang/crates/vo-jit/src/verifier/*`
- `lang/crates/vo-jit/src/translate/*`
- `lang/crates/vo-jit/src/call_helpers/*`
- `lang/crates/vo-vm/src/vm/jit/*`
- `lang/crates/vo-vm/src/runtime_boundary.rs`
- `lang/crates/vo-vm/src/vm/jit_mgr.rs`
- `tests/lang/cases` and `tests/lang/manifest.toml`

Checks:

```sh
cargo test -p vo-jit
cargo test -p vo-vm --features jit
./d.py test jit tests/lang/cases/<case>.vo
./d.py test osr tests/lang/cases/<case>.vo
VO_JIT_CALL_THRESHOLD=1 ./d.py run tests/lang/cases/<case>.vo --mode=jit
```

`vo-jit/src/semantics` is the opcode fact source. When an opcode changes, add
or update one semantic row and keep capability, metadata requirement, register
effects, runtime dependencies, helper return policy, verifier requirements,
verifier domain, lowering owner, frame/trap policy, fail-fast policy, and
effect contract derived from it.

`metadata_contract.rs` delegates opcode metadata requirements back to the
semantic row. Do not create a second opcode-to-metadata match. Missing metadata,
wrong metadata kind, invalid references, call-shape drift, helper/callback ABI
drift, and slot-layout mismatches are strict JIT errors, not implicit
interpreter side paths.

Keep VM-shared bytecode/module validation in
`vo-common-core/src/verifier.rs`. Strict JIT verifier code should only add
JIT-specific metadata policy and lowering/ABI capability checks after the shared
`ModuleVerifier` has accepted the module.

Use `RuntimePathPolicy`, `JitSideExitReason`, "VM call materialization", and
"side exit" precisely. Avoid broad interpreter-path language unless referring to
language-test manifest side-exit observation fields at the test boundary.

For call work, follow `call_helpers/*`: plan route selection in `plan.rs`,
dynamic call state in `dynamic/*`, extern behavior in `externs.rs`, prepared
calls in `prepared.rs`, result handling in `result_flow.rs`, VM materialization
in `vm_materialization.rs`, and callback ABI wrappers in `callback_abi.rs`.
VM-side bridge code belongs under `vo-vm/src/vm/jit/*`, with grouped transition
state preferred over long argument lists.

When a JIT helper performs local runtime side effects such as spawning,
waking, host-event resume, queue/select interaction, or island response
handling, audit whether it returns a `RuntimeTransition`, a pending transition,
or a terminal discard. Tests should cover commit-at-boundary and discard paths.

## Change GC Or Root Scanning

Touch likely areas:

- `lang/crates/vo-runtime/src/gc.rs`
- `lang/crates/vo-runtime/src/gc_types.rs`
- `lang/crates/vo-vm/src/gc_roots.rs`
- `lang/crates/vo-vm/src/fiber.rs`
- `lang/crates/vo-vm/src/vm/mod.rs`
- `lang/crates/vo-vm/src/vm/jit/*`
- `lang/crates/vo-jit/src/*`

Checks:

```sh
./d.py test gc
./d.py test jit tests/lang/cases/<case>.vo
cargo test -p vo-runtime gc
cargo test -p vo-vm gc
cargo test -p vo-jit
cargo run -q -p vo-dev -- task run gc-contract
cargo run -q -p vo-dev -- gc-perf --release --json dead-sweep
```

Remember that GC is non-moving and precise by `SlotType`. Interface slots,
typed write barriers, dirty-root epochs, defer arguments, unwinding returns,
panic state, closure replay, borrowed frames, select queues, wait
registrations, endpoint registries, JIT panic messages, sentinel errors, and
JIT materialized frames are root-sensitive.

## Change Module Resolution Or Lock Semantics

Touch likely areas:

- `lang/crates/vo-module/src/identity.rs`
- `lang/crates/vo-module/src/project.rs`
- `lang/crates/vo-module/src/workspace.rs`
- `lang/crates/vo-module/src/solver.rs`
- `lang/crates/vo-module/src/lifecycle.rs`
- `lang/crates/vo-module/src/readiness.rs`
- `lang/crates/vo-module/src/ops.rs`
- `lang/crates/vo-engine/src/compile/*`
- `lang/crates/vo-web/src/compile.rs`
- `cmd/vo/src/main.rs`
- `lang/docs/spec/module.md`
- `lang/docs/spec/module-inline-mod-tutorial.md`

Checks:

```sh
cargo test -p vo-module
./d.py vo mod verify path/to/module
./d.py test both tests/lang/cases/<case>.vo
./d.py test wasm tests/lang/cases/<case>.vo
```

Keep frozen build commands separate from lifecycle commands. `vo.work` affects
local replacement only and must not rewrite canonical identity.

Build-like commands should not re-solve dependencies or mutate `vo.mod` /
`vo.lock`, but current native paths can download already-locked cache artifacts
and materialize native inline dependencies.

For inline modules, distinguish native real-path auto-install from web/memory
compilation. Native `compile_with_auto_install` can resolve inline `require`
entries into the ephemeral cache; web and memory paths still reject unsupported
external inline requires.

Native inline dependency compilation can use `.volang/cache/vo/compile/native`.
If cache behavior changes, audit fingerprints, generated ephemeral project
inputs, and frozen-readiness behavior together.

## Change Module Release Or Staging Behavior

Touch likely areas:

- `lang/crates/vo-release/src/*`
- `lang/crates/vo-module/src/cache/validate.rs`
- `lang/crates/vo-module/src/ext_manifest.rs`
- `cmd/vo/src/main.rs`
- `lang/docs/spec/module.md`
- `lang/docs/spec/repository-layout.md`

Checks:

```sh
cargo test -p vo-release
cargo test -p vo-module
./d.py vo release verify path/to/module
./d.py ci task release-verify-vogui
```

Release staging owns `vo.release.json`, virtual/staged `vo.web.json`, source
packages, artifact declarations, and tracked include paths. Audit `vo.sum`,
old alias imports, dependency freshness, declared-vs-staged artifact equality,
digest/size checks, and web/local artifact byte matching. `vo.release.json`
must contain required `web_manifest.size` and `web_manifest.digest` fields;
every consumer verifies the raw `vo.web.json` bytes against them before JSON
parsing or cache insertion.

Run staging only in a temp or explicitly approved workspace, because it writes
release outputs.

## Add Or Change Stdlib API

Touch likely areas:

- `lang/stdlib/<pkg>/*.vo`
- `lang/stdlib/stdlib.toml`
- `lang/crates/vo-stdlib/src/<pkg>.rs`
- `lang/crates/vo-stdlib/src/lib.rs`
- `lang/crates/vo-runtime/src/builtins/*` for runtime builtins
- `tests/lang/cases/stdlib/*`

Checks:

```sh
./d.py test both tests/lang/cases/stdlib/<case>.vo
./d.py test wasm tests/lang/cases/stdlib/<case>.vo
```

Vo facade declarations and Rust extern registration must agree on ABI package
and function names. Use `#[vostd_fn]` for stdlib shims.

## Change Native Extension Or FFI Behavior

Touch likely areas:

- `lang/crates/vo-ext/src/lib.rs`
- `lang/crates/vo-ffi-macro/src/*`
- `lang/crates/vo-runtime/src/ffi/*`
- `lang/crates/vo-runtime/src/ext_loader.rs`
- `lang/crates/vo-stdlib/src/extern_manifest.rs`
- `lang/crates/vo-engine/src/compile/native.rs`
- `lang/crates/vo-module/src/ext_manifest.rs`
- `lang/docs/spec/native-ffi.md`

Checks:

```sh
cargo test -p vo-ffi-macro
cargo test -p vo-ext
cargo test -p vo-runtime --release resolved_call_rejects_provider_identity_drift_after_load
./d.py test both tests/lang/cases/<extension-case>.vo
```

Current macros are `#[vo_fn]` and `#[vostd_fn]`. ABI name matching spans
Vo package path, extension manifest metadata, Cargo metadata, macro package
path, and runtime lookup names.

`#[vo_fn]` supports simple, result, and manual wrapper modes. Result wrappers
expect `String` errors. Native exports must provide `vo_ext_get_entries` and
the ABI fingerprint expected by the runtime loader.

Resolved extern calls compare provider identity allocated by the registry, not
function pointer addresses. Release builds may merge identical functions, so
never use `ExternFn` pointer equality as a provider drift check. Keep declared
extern effects, provider manifest effects, resolved extern tables, and JIT
routing policy in sync.

Do not copy `#[vo_extern]` examples from older docs without fixing them.
Also verify stale ABI snippets before copying fields or result variants.

## Change Studio Or GUI Runtime

Touch likely areas:

- `apps/studio/src/lib/backend/backend.ts`
- `apps/studio/src/lib/backend/web_backend.ts`
- `apps/studio/src/lib/backend/native_backend.ts`
- `apps/studio/src/lib/services/runtime_service.ts`
- `apps/studio/src/lib/gui/gui_pipeline.ts`
- `apps/studio/src/lib/gui/renderer_bridge.ts`
- `apps/studio/wasm/src/lib.rs`
- `apps/studio/src-tauri/src/commands/*`
- `lang/crates/vo-web/src/*`
- `lang/crates/vo-app-runtime/src/*`

Checks:

```sh
./d.py ci task studio-build
./d.py studio --build-only
./d.py ci site
```

Host bridge readiness before initial render is important. `RenderBuffer` keeps
only the latest frame. Native and web backends should stay aligned through the
shared backend interface where possible.

For Web GUI execution, keep the sequence explicit: `WebBackend.runGui`,
`prepareEntry`, `compileGui`, `executeGuiFromCompileOutput`, preload WASM
extensions, load host bridge, then start GUI bytecode. For native Studio, Tauri
commands compile through `vo-engine`, derive browser renderer artifacts from
native extensions and locked modules, then start the native GUI runtime.

Bare `vo-web` compile comments are not enough evidence for module fetching.
Studio `prepareEntry` is the browser dependency-preparation path; test
web/memory/native dependency paths separately.

## Change Playground Or Docs Mirrors

Touch likely areas:

- `lang/docs/spec/*`
- `lang/docs/vo-for-gophers.md`
- `apps/playground-legacy/src/assets/docs/generated/*`
- `apps/studio/docs/manifest.toml`
- `apps/studio/docs/pages/*`
- `scripts/ci/docs_sync.mjs`
- `scripts/ci/docs_lint.mjs`

Checks:

```sh
./d.py ci task docs-lint
node scripts/ci/docs_sync.mjs --check
node scripts/ci/docs_sync.mjs
```

Generated Playground docs include a provenance header and digest. Regenerate
them through `docs-sync`; do not hand-edit generated mirrors.

## Change Quickplay Or Checked-In Artifacts

Touch likely areas:

- `apps/studio/public/quickplay/blockkart/*`
- `apps/studio/scripts/package_blockkart_quickplay.mjs`
- `apps/studio/src/lib/quickplay.ts`
- `scripts/ci/quickplay_validate.mjs`
- `eng/artifacts.toml`
- `eng/tasks.toml`

Checks:

```sh
./d.py ci task quickplay-validate
./d.py ci task blockkart-smoke-static
cargo run -q -p vo-dev -- lint artifacts
```

Checked-in generated artifacts must match declared generator inputs, validator
inputs, provenance fields, digests, and size policy.

`quickplay.ts` should derive artifact URLs from `deps.json`. Hard-coded
quickplay artifact URLs are validator failures.

## Change Examples Or Benchmarks

Touch likely areas:

- `examples/**`
- `examples/manifest.toml`
- `benchmarks/**`
- `benchmarks/manifest.toml`
- `cmd/vo-dev/src/dev_bench.rs`
- `cmd/vo-dev/src/lint_system.rs`

Checks:

```sh
cargo run -q -p vo-dev -- lint examples
cargo run -q -p vo-dev -- lint benchmarks
./d.py ci task examples-smoke
./d.py bench vo
```

Benchmarks may produce local generated outputs and language-specific build
products. Do not commit those unless artifact policy explicitly calls for them.

## Change Engineering Tasks Or CI

Touch likely areas:

- `eng/tasks.toml`
- `eng/toolchains.toml`
- `eng/tests.toml`
- `eng/ci.toml`
- `eng/artifacts.toml`
- `eng/project.toml`
- `eng/release.toml`
- `cmd/vo-dev/src/*`
- `.github/workflows/*`

Checks:

```sh
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- task plan pr
cargo run -q -p vo-dev -- task run task:<task-name>
cargo run -q -p vo-dev -- verify plan pr
cargo run -q -p vo-dev -- task run contract
```

Do not duplicate task policy in GitHub YAML, ad hoc shell snippets, or `d.py`.
Add data to `eng/` and interpretation to `cmd/vo-dev`.
For test-system completeness work, use
`lang/docs/dev/test-system-completion-plan.md` as the acceptance contract and
close with `contract`, `vm-production`, `site`, and `release-verify` from one
frozen source state, then synchronize readiness and run the aggregate lint.

## Change Release Automation

Touch likely areas:

- `eng/release.toml`
- `cmd/vo-dev/src/release_config.rs`
- `cmd/vo-dev/src/release_homebrew.rs`
- `cmd/vo-dev/src/release_system.rs`
- `.github/workflows/release.yml`
- `.github/actions/setup-rust`
- Cargo package metadata for releasable crates

Checks:

```sh
cargo run -q -p vo-dev -- lint release
cargo run -q -p vo-dev -- release matrix --github-output
cargo run -q -p vo-dev -- release cross-version
cargo run -q -p vo-dev -- release homebrew-metadata --github-output
```

Release package, publish, and Homebrew update commands are real release
operations. Do not run them unless the user explicitly asks, or unless the
command has a safe dry-run/temp-workspace mode that you have verified in source.
