# Extern Effect Contract And JIT Routing Design

Status: target design. This document describes the complete correctness
contract for extern calls across bytecode, runtime registration, VM execution,
JIT lowering, native extension ABI, cache invalidation, and test coverage.

The goal is not to make `is_blocking` smarter. The goal is to replace name
heuristics and payload-less JIT replay with an explicit extern control-flow
contract that every execution path can verify and consume.

Related context:

- [`vm-runtime-boundary-architecture.md`](vm-runtime-boundary-architecture.md)
- [`jit-fact-source.md`](jit-fact-source.md)

## Problem

Extern calls currently have two different kinds of information:

- `ExternResult` describes what happened after one extern invocation.
- `ExternDef` describes the bytecode-level extern shape: name, param slots,
  return slots, `is_blocking`, and WASM extension parameter kinds.

The missing piece is the static contract that says which control-flow effects a
specific extern is allowed to produce. Today `ExternDef::is_blocking` is only a
name heuristic (`name.contains("_blocking_")`) and is not a reliable runtime or
JIT contract. It also cannot express host events, host-event replay, closure
replay, queue blocking, or unknown native extension behavior.

This matters because `CallExtern` is not just an FFI call. It can:

- return normally and write return slots;
- yield or block the current fiber;
- submit I/O and replay the extern after the I/O token wakes;
- wait for a host event and continue at the next instruction;
- wait for a host event and replay the extern with resume data;
- request a closure call, then replay with cached closure results;
- panic with a user-visible message;
- fail because the extern was not registered or violated an ABI contract.

The interpreter path currently owns the full semantics. It fetches an
instruction, increments `frame.pc`, executes `CallExtern`, and then rewinds the
PC only for replay-style results. It also handles closure replay by pushing a
real closure frame and recording the replay boundary.

The JIT path currently calls `jit_call_extern`. `WaitIo` gets a token, but
`HostEventWait`, `HostEventWaitAndReplay`, and `CallClosure` are collapsed into
`JitResult::Replay`, losing the payload and relying on re-executing the extern
in the interpreter. That is not a correct long-term ABI.

## Target Invariants

The target design has these invariants:

1. Extern effects are explicit metadata, not naming conventions.
2. Bytecode declares an upper bound for each extern's permitted effects.
3. Runtime providers declare the effects their implementation may return.
4. VM load resolves bytecode declarations against provider declarations into a
   frozen `ResolvedExternTable`.
5. JIT compilation receives the resolved table, not a raw `Module` alone.
6. JIT lowering chooses its route from resolved extern effects and backend
   capabilities.
7. `JitResult::Replay` is not a generic extern-suspend transport.
8. Suspend payloads are VM-owned before control returns from JIT to the VM.
9. PC replay/resume policy is explicit and is not hidden inside scheduler
   block handling.
10. Contract violations are infrastructure errors, not user panics and not
    implicit interpreter paths.

## Extern Effect Model

`vo-common-core` should define a stable effect bitset that is serialized as
part of bytecode:

```rust
#[repr(transparent)]
pub struct ExternEffects(u64);

impl ExternEffects {
    pub const NONE: Self = Self(0);
    pub const MAY_YIELD: Self = Self(1 << 0);
    pub const MAY_QUEUE_BLOCK: Self = Self(1 << 1);
    pub const MAY_WAIT_IO_REPLAY: Self = Self(1 << 2);
    pub const MAY_HOST_WAIT: Self = Self(1 << 3);
    pub const MAY_HOST_REPLAY: Self = Self(1 << 4);
    pub const MAY_CALL_CLOSURE_REPLAY: Self = Self(1 << 5);
    pub const UNKNOWN_CONTROL: Self = Self(1 << 6);
}
```

The bitset is intentionally about control-flow protocol, not about target,
implementation source, or ordinary side effects. It should not contain
`native`, `wasm`, `stdlib`, `extension`, `before_suspend`, or `after_resume`.
Those belong in provider/source metadata or diagnostics, not in the JIT routing
contract.

`Panic` and `NotRegistered` are not effect bits. Any extern can panic, and
registration failure is a runtime binding error. Both must be handled by every
extern call path.

`UNKNOWN_CONTROL` is conservative. It means the provider contract cannot give a
precise upper bound. JIT must not assume no-suspend behavior for such externs.

## Bytecode Model

`ExternDef` should become:

```rust
pub struct ExternDef {
    pub name: String,
    pub param_slots: u16,
    pub ret_slots: u16,
    pub allowed_effects: ExternEffects,
    pub param_kinds: Vec<ExtSlotKind>,
}
```

`is_blocking` should not remain a semantic field. The bytecode format already
uses a strict current-version policy, so the target design should bump the
bytecode version and remove the heuristic from serialized bytecode rather than
preserve a second source of truth.

Common bytecode verification must check:

- reserved effect bits are rejected;
- `UNKNOWN_CONTROL` is rejected when combined with precise bits;
- `CallExtern` references a valid extern id;
- arg and return slot layouts still match `CallExternLayout`;
- WASM extension `param_kinds` still match the encoded argument slot count;
- no `CallExtern` metadata can contradict `ExternDef` shape.

The effect bits are per extern definition, not per call site. They should not
be placed in `CallExternLayout`, which remains a per-PC slot layout fact.

## Runtime Provider Metadata

Every runtime provider must declare effects with the function pointer.

Static stdlib registration should use:

```rust
pub struct StdlibEntry {
    pub name: &'static str,
    pub func: ExternFn,
    pub effects: ExternEffects,
}
```

Native extension ABI should bump `EXTENSION_ABI_VERSION` and extend the stable
entry metadata:

```rust
#[repr(C)]
pub struct ExternEntry {
    pub name_ptr: *const u8,
    pub name_len: u32,
    pub module_owner_ptr: *const u8,
    pub module_owner_len: u32,
    pub func: Option<ExternFnPtr>,
    pub effects_bits: u64,
}
```

Each extension entry carries the exact canonical module owner read from its
authoritative `vo.mod`; the decoded extern package must remain inside that
owner. The effect layout, result-code mapping, and the size, alignment, and
every field offset of both `ExternEntry` and `ExtensionTable` are included in
the extension ABI fingerprint. The `vo-ffi-macro` and `vo-ext` export macros
generate the same metadata for Rust extension functions. Old extension
libraries are rejected by the existing version/fingerprint mismatch path.

WASM host runtime providers must also register effects. Examples:

- native `time_blocking_sleepNano`: `MAY_WAIT_IO_REPLAY`;
- WASM `time_blocking_sleepNano`: `MAY_HOST_WAIT`;
- native Unix `net_http_nativeHttpsRequest`: `MAY_WAIT_IO_REPLAY`;
- WASM `net_http_nativeHttpsRequest`: `MAY_HOST_REPLAY`;
- WASM generic extension bridge: `MAY_HOST_WAIT | MAY_HOST_REPLAY` or
  `UNKNOWN_CONTROL` if the JS-side module metadata is not precise;
- dynamic protocol/call externs: `MAY_CALL_CLOSURE_REPLAY`.

Unknown native extensions default to `UNKNOWN_CONTROL` until their ABI metadata
is available and verified.

## Resolved Extern Table

`ExternRegistry` should no longer store only function pointers. It should store
registered entries:

```rust
pub enum RegisteredFn {
    Internal(ExternFn),
    Extension(ExternFnPtr),
}

pub enum RegisteredExternSource {
    Stdlib,
    LinkmeExtension,
    NativeExtension,
    WasmHost,
    WasmExtensionBridge,
    Test,
}

pub struct RegisteredExtern {
    pub func: RegisteredFn,
    pub provider_effects: ExternEffects,
    pub source: RegisteredExternSource,
}
```

After registering stdlib, linkme extensions, native extension loader entries,
and host/WASM providers, `Vm::load` must resolve each `Module.externs[id]`
against the matching `RegisteredExtern`:

```rust
pub struct ResolvedExtern {
    pub id: u32,
    pub name: String,
    pub allowed_effects: ExternEffects,
    pub provider_effects: ExternEffects,
    pub effective_effects: ExternEffects,
    pub source: RegisteredExternSource,
    pub jit_route: ExternJitRoute,
}

pub struct ResolvedExternTable {
    entries: Vec<ResolvedExtern>,
}
```

Resolution rules:

- missing provider is a load error;
- invalid provider effect bits are a load error;
- provider effects must be a subset of `allowed_effects`, unless the module
  allowed `UNKNOWN_CONTROL`;
- provider/effective `UNKNOWN_CONTROL` forces a conservative JIT route;
- module-side `allowed_effects = UNKNOWN_CONTROL` is a conservative upper
  bound, not by itself a reason to materialize once the provider is precise;
- resolved entries are frozen for the lifetime of the loaded module;
- strict JIT verification and compilation consume the resolved table.

Provider effects are an optimization and runtime-contract boundary, not a
sandbox. A native extension can still lie or perform arbitrary host side
effects before returning. Runtime validation can fail fast after a bad result,
but it cannot undo side effects already performed by native code.

## Extern Call Outcomes

`ExternRegistry::call` should return an infrastructure-aware result:

```rust
pub type ExternCallOutcome = Result<ExternResult, ExternContractError>;
```

`ExternContractError` is used for:

- provider returned an effect outside its declared `provider_effects`;
- extension result code requires a missing payload;
- terminal result did not consume required replay/resume input;
- declared `NoSuspend` extern returned a suspend result;
- the registered provider and module declaration drifted after load.

VM and JIT callers must map `ExternContractError` to `ExecResult::JitError` or
`JitResult::JitError`. It must not become a user panic.

## Explicit PC Policy

The scheduler should not be responsible for hidden PC rewinds. The producer of
a block/replay request must set the final PC before returning the block result.

Define:

```rust
pub enum ResumePcPolicy {
    ResumeNext,
    ReplayCurrent,
}
```

Interpreter `CallExtern` already knows both values:

- `HostEventWait` uses `ResumeNext`;
- `WaitIo`, `HostEventWaitAndReplay`, and `CallClosure` use `ReplayCurrent`;
- `Yield` and queue `Block` use the current instruction's existing scheduler
  semantics and must be audited case by case.

The VM should move replay rewinds out of generic `handle_exec_result` block
handling and into the instruction-specific producers. `BlockReason` should
describe why the fiber is blocked, not mutate the current frame as a side
effect.

JIT transitions must materialize frames at the final PC selected by the
payload. They should not materialize at one PC and rely on scheduler rewind to
repair it later.

## JIT Extern Suspend Transport

`JitResult::Replay` should remain a narrow side-exit result for payload-less
VM re-entry. It must not be the transport for extern suspend payloads.

Add a first-class JIT extern-suspend path:

```rust
pub enum JitResult {
    Ok = 0,
    Panic = 1,
    Call = 2,
    WaitIo = 3,
    WaitQueue = 4,
    Replay = 5,
    JitError = 6,
    ExternSuspend = 7,
}

pub enum JitExternSuspend {
    Yield { resume_pc: u32 },
    QueueBlock { resume_pc: u32 },
    WaitIo { token: u64, replay_pc: u32 },
    HostWait { token: u64, delay_ms: u32, resume_pc: u32 },
    HostReplay { token: u64, replay_pc: u32 },
    CallClosure {
        closure_ref: GcRef,
        args: Vec<u64>,
        replay_pc: u32,
    },
}
```

The payload must be VM-owned before generated JIT code returns. Do not store
pointers into Cranelift stack slots or temporary extern buffers across
suspension. `jit_call_extern` can publish the payload into a field on `Fiber`
or another VM-owned state object, then return `JitResult::ExternSuspend`.

The bridge then:

1. takes the payload from the fiber or VM-owned state;
2. materializes JIT frames at the payload PC;
3. converts the payload to a `JitBridgeTransition`;
4. records an accurate `JitSideExitReason`;
5. blocks, yields, or pushes the closure frame without re-executing the extern.

For `CallClosure`, extract the interpreter's closure-frame setup into a shared
helper so interpreter and JIT-origin extern suspend use the same layout,
replay-depth, return-cache, and panic-boundary semantics.

## JIT Compile Environment

The resolved extern table must enter JIT compilation explicitly. Storing it in
`ExternRegistry` is not enough because current JIT compile APIs take only the
module and function definitions.

Introduce:

```rust
pub struct JitCompileEnv<'a> {
    pub externs: &'a ResolvedExternTable,
    pub backend_caps: JitBackendCaps,
}
```

`JitManager::compile_function`, `JitManager::compile_loop`,
`JitCompiler::compile_function`, `JitCompiler::compile_loop`,
`FunctionCompiler`, and `LoopCompiler` should receive this environment.

`IrEmitter` should expose a route query:

```rust
fn resolved_extern(&self, extern_id: u32) -> Result<&ResolvedExtern, JitError>;
```

`CallExtern` lowering then uses:

```rust
pub enum ExternJitRoute {
    Intrinsic,
    DirectHelper,
    VmMaterializeBeforeCall,
}
```

Route selection:

- known precise effects with full JIT extern-suspend support can use
  `DirectHelper`;
- provider/effective `UNKNOWN_CONTROL` uses `VmMaterializeBeforeCall`;
- providers marked as requiring a materialized VM frame use
  `VmMaterializeBeforeCall`;
- intrinsic externs still bypass FFI and do not need provider metadata.

The route is computed once during VM load or JIT manager initialization, then
compiled code consumes the frozen route. It must not inspect mutable registry
state at compile time.

## VM Materialize Before Call

`VmMaterializeBeforeCall` means:

- spill all JIT locals to `fiber.stack`;
- materialize the active JIT frame chain;
- set the current frame PC to the current `CallExtern` instruction;
- return a payload-less VM re-entry result;
- let the interpreter execute the extern exactly once.

This is not a fallback for invalid metadata. It is a legal runtime path for
externs whose provider contract is unknown or requires VM-owned frame state.

## Verifier Boundaries

`ModuleVerifier` owns shared bytecode validity:

- extern id validity;
- extern shape and callsite layout;
- valid effect bits;
- serialized effect metadata presence;
- no stale `is_blocking` semantics;
- all layout metadata needed by any backend.

`vo-jit` strict verification owns JIT policy:

- every `CallExtern` can find a resolved route;
- `DirectHelper` is only used when backend caps cover every resolved effect;
- `ExternSuspend` ABI fields are present in the helper/callback ABI manifest;
- lowering capability and semantic rows agree;
- loop OSR and full function paths produce identical extern routes;
- unknown or VM-frame-required externs do not compile to direct helper calls.

Do not duplicate opcode-family match tables. `CallExtern` remains one semantic
row; resolved extern effects refine that row at compile time.

## Cache And Versioning

Required versioned surfaces:

- bytecode `VERSION` changes because `ExternDef` layout changes;
- native extension `EXTENSION_ABI_VERSION` and fingerprint change because
  `ExternEntry` layout changes;
- compile cache schema changes because extern effects affect generated bytecode
  and JIT routing;
- JIT module digest changes naturally once serialized module bytes include
  extern effects.

The compile cache fingerprint must include:

- bytecode schema;
- compiler build id;
- target triple;
- extension ABI version and fingerprint;
- extension manifest bytes;
- native extension exported metadata digest when available;
- source tree and dependency lock data as today.

Cache load must deserialize and run the common verifier. If a cached module has
missing or invalid extern effects, the cache entry is ignored.

## Source And Manifest Declarations

Effect declarations should have one canonical data source per provider kind:

- stdlib/internal: generated or hand-written `StdlibEntry.effects`;
- native extensions: `#[vo_fn(..., effects(...))]` macro metadata plus ABI
  export;
- WASM extension bridge: module or per-export metadata loaded by the web
  runtime;
- test-only externs: explicit registration helper requiring effects;
- source extern declarations without provider metadata: `UNKNOWN_CONTROL`.

The language does not need to infer effects from names. If source-level
annotations are added later, codegen should treat them as declarations for the
module's allowed effects and still resolve them against provider metadata at
load time.

## Documentation Updates

The implementation must update:

- `lang/docs/spec/vm-bytecode.md` for `ExternDef.allowed_effects`;
- `lang/docs/spec/vm-jit-design.md` for `JitResult::ExternSuspend` and PC
  policy;
- `lang/docs/spec/native-ffi.md` for extension effect metadata and ABI version;
- `lang/docs/dev/jit-fact-source.md` for resolved extern effects as a
  module-context refinement of the `CallExtern` semantic row;
- `lang/docs/dev/native-extensions.md` for authoring effect declarations.

## Implementation Checklist

- Define `ExternEffects` in `vo-common-core`.
- Replace serialized `ExternDef::is_blocking` with
  `ExternDef::allowed_effects`.
- Bump bytecode version.
- Update serialization round trips and bytecode text formatting.
- Update `ModuleVerifier` effect validation.
- Add `StdlibEntry.effects`.
- Add effect arguments to `#[vostd_fn]` and `#[vo_fn]` registration output.
- Update all hand-written stdlib and WASM host registrations with effects.
- Extend native `ExternEntry` and ABI fingerprint.
- Update `vo-ext` export macros.
- Update `ExtensionLoader` validation and cache metadata.
- Change `ExternRegistry` to store `RegisteredExtern`.
- Change `ExternRegistry::call` to return `ExternCallOutcome`.
- Resolve `ResolvedExternTable` during VM load.
- Store the resolved table in VM/JIT manager state.
- Pass `JitCompileEnv` through JIT manager and compiler APIs.
- Add `JitResult::ExternSuspend`.
- Add VM-owned `JitExternSuspend` payload storage.
- Update `jit_call_extern` to validate result effects and publish payloads.
- Refactor interpreter `CallExtern` closure setup into a shared VM helper.
- Move replay PC mutation out of generic block handling.
- Update `JitBridgeTransition`, full-function transition, and OSR transition.
- Update side-exit reason stats for wait, host wait, host replay, closure
  replay, and VM materialize-before-call.
- Update semantics/contract graph tests for `CallExtern`.
- Update docs listed above.

## Required Validation

The change is not complete until these pass from the repository root:

```sh
cargo test -p vo-common-core
cargo test -p vo-runtime
cargo test -p vo-ffi-macro
cargo test -p vo-ext
cargo test -p vo-vm --features jit
cargo test -p vo-jit
cargo test -p vo-engine
cargo check -p vo-web --target wasm32-unknown-unknown
cargo test --workspace --all-targets --locked
cargo run -q -p vo-dev --locked -- test run --suite lang --targets vm,jit,osr,compile
```

Focused language tests must cover at least:

- native `time_blocking_sleepNano` through VM, JIT, and OSR;
- native HTTP `net_http_nativeHttpsRequest` effect metadata;
- WASM `time_blocking_sleepNano` host wait;
- WASM HTTP host replay;
- dynamic `CallClosure` replay through JIT-origin extern suspend;
- unknown extension defaulting to VM materialize-before-call;
- declared `NoSuspend` provider returning a suspend result and failing fast;
- compile cache invalidation when extern effects change;
- extension ABI mismatch for old effect-less extensions.

## Acceptance Criteria

- No code path uses extern name text to infer blocking or suspend behavior.
- No JIT extern suspend result is transported as payload-less `Replay`.
- A JIT-origin `HostEventWaitAndReplay` starts host work exactly once.
- A JIT-origin `CallClosure` calls the closure exactly once per replay round and
  preserves panic/replay cache behavior.
- Full-function JIT and loop OSR use the same resolved extern routes.
- VM, JIT, WASM, native stdlib, native extension, and compile-cache tests cover
  the same effect contract.
