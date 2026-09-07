# Language implementation boundaries

The language pipeline is `vo-syntax` → `vo-analysis` → `vo-codegen` → verified
bytecode. `vo-engine` freezes inputs, attaches package provenance, verifies output,
and selects execution. Runtime providers and platform adapters depend on shared
contracts; portable bytecode generation does not depend on their implementations.

## One owner per semantic rule

| Contract | Owner | Consumers |
| --- | --- | --- |
| Physical slot widths, empty aggregates, overflow and by-value cycles | `vo-common::slot_layout` | Analysis type arena and FFI declaration parser |
| Runtime dynamic assignability | `vo-common-core::dynamic_layout` | Native runtime and Core Wasm AOT |
| Declaration extern identities and allowed effects | `vo-common-core::extern_contracts` | Codegen, runtime registration and stdlib effect manifest |
| Execution and register/frame effects | `vo-common-core::{execution_effects,instruction_effects}` | Verifier, VM and generated backends |
| Dynamic-call leaf inlining | `vo-jit::call_helpers::leaf_inline` | Function compilation and loop OSR |

Layout consumers describe their own type graphs. The shared iterative traversal
owns arithmetic and cycle detection; it avoids recursive stack growth. Empty
structs occupy one slot. Zero-length arrays occupy zero slots, including arrays
whose element width exceeds the materializable limit. Analysis and FFI use a
saturated over-width sentinel for intermediate types, then apply their own public
layout limits and diagnostics.

Extern tables expand into lightweight declaration metadata or actual provider
bindings. The compiler does not load native providers to discover declared effects.
Provider resolution still authenticates identity, ABI and effective effects before
freezing the complete registry. Runtime registration and the stdlib conformance
tests validate implementations against the same declarations.

## UI composition above the engine

Dependency direction is `CLI / Studio / native shell` → `vo-ui-integration` →
`vo-engine`. The language engine has no UI dependency, feature, native session,
SSR entry point, or UI test dependency, even with all backend features enabled.
Renderer-neutral framework crates remain independent of both adapters and engine.

`Engine` owns the common compilation and execution pipeline. `EngineExtension`
supplies an immutable compilation policy, provider admission, target restrictions,
and a versioned cache identity. Every compiler input path passes through the
selected instance; common verification, frozen snapshots, native extension
admission, and generation validation remain mandatory engine boundaries.
Free `vo_engine` functions preserve the plain language API. UI hosts explicitly use
`vo_ui_integration::engine()` for compilation, execution, AOT, and AOT cache keys.
They use `vo_ui_integration::build_native_gui_vm_for_mode` and its related entry
points for sessions, transactional reload, and SSR. This replaces the former
engine `ui` feature and UI convenience exports.

Compilation and AOT caches include the selected extension identity. Source
snapshot generation checks remain independent of the cache partition. The engine
and UI adapter derive build identities from their own local production/build
dependency closures through `eng/build-identity.rs`; UI source changes invalidate
UI artifacts without becoming language-engine build inputs.

Engine features remain explicit:

| Feature | Capability |
| --- | --- |
| No default features | Language compilation and interpreter execution |
| `jit` | VM execution with JIT and OSR |
| `aot-native` | Native AOT generation and its cache |
| `aot-wasm` | Core Wasm AOT generation and its cache |

Both engine and integration default to JIT and both AOT backends; integration
forwards these features to its engine dependency. Core VM/JIT regressions live in
the engine, and UI compilation, VM/JIT, reload, SSR, and AOT regressions live in
`vo-ui-integration`. CI rejects UI dependencies in the engine's complete graph.

The stdlib toolchain host is a process-wide application service. An engine can
construct `toolchain_host()` retaining its selected compiler/execution policy.
Applications can install that host explicitly at startup; automatic installation
preserves an already installed application host. Ordinary compile/run instances
do not change one another's extension configuration.

## Core Wasm AOT organization

`vo-wasm-aot/src/codegen/mod.rs` assembles modules and defines private shared ABI
layouts. The implementation is organized by responsibility:

- `analysis`: reachability, propagated effects and execution-tier planning.
- `metadata`, `heap`, `objects`, `collections`, `scheduler`: image metadata and
  generated runtime operations.
- `functions`, `direct`, `calls`: function bodies, direct calls and materialization.
- `scalar`, `conversions`: shared arithmetic and conversion semantics for typed
  locals, cached locals and linear-memory frames.
- `frame`: dispatch and focused memory, collection, control, interface and scheduler
  instruction handlers.
- `dynamic_calls`, `dynamic_values`, `dynamic_support`, `externs`: dynamic language
  operations and authenticated extern selection.
- `frame_helpers`: frame access, scalar synchronization and unwind publication.

Scalar lowering admits all operands before emitting instructions, so a failed
cached-local attempt can fall back without leaving partial Wasm. Storage adapters
own loads and stores. Each execution tier supplies its panic publication path,
including the required scalar spills before unwind.

Local allocation and unwind analysis starts from the exhaustive common opcode
contract. A small documented refinement accounts for image-owned strings,
backend-allocated scheduler/defer records and closure-environment checks. Resolved
callee effects propagate through the existing fixed point. The emitted image ABI,
verification, precise root ownership and scheduler contracts remain unchanged.

## Focused validation

Run owning-crate regressions and compile engine configurations independently:

```sh
VOWORK=off cargo test -p vo-common -p vo-analysis -p vo-ffi-macro --locked
VOWORK=off cargo test -p vo-common-core -p vo-runtime -p vo-codegen -p vo-jit --locked
VOWORK=off cargo test -p vo-wasm-aot --locked
VOWORK=off cargo test -p vo-engine --all-features --locked
VOWORK=off cargo check -p vo-engine --no-default-features --locked
VOWORK=off cargo check -p vo-ui-integration --no-default-features --locked
VOWORK=off cargo test -p vo-ui-integration --all-features --locked
```

Use `--target-dir` for isolated Rust tests that spawn Cargo fixtures; a globally
inherited `CARGO_TARGET_DIR` changes those fixtures' own target-directory contracts.
Language execution regressions use manifest-selected VM/JIT/OSR and explicit
`wasm-aot` targets. Native AOT requires separate generated-object and linked-image
coverage. Wasm scalar tests validate all three storage adapters, operand aliases,
conversion flags and panic-return stack shapes; execution regressions establish
results, defer/recover behavior and GC visibility.
