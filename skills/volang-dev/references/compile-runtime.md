# Compile, Bytecode, VM, Runtime, GC, And JIT

## Contents

- [Engine Compile Pipeline](#engine-compile-pipeline)
- [Codegen Pipeline](#codegen-pipeline)
- [Bytecode Model](#bytecode-model)
- [VM Load And Run](#vm-load-and-run)
- [Scheduler And Fiber Model](#scheduler-and-fiber-model)
- [GC And Root Scanning](#gc-and-root-scanning)
- [Native Externs During Execution](#native-externs-during-execution)
- [Islands And Cross-Island Values](#islands-and-cross-island-values)
- [JIT](#jit)
- [Change Recipes](#change-recipes)
- [Caveats](#caveats)

## Engine Compile Pipeline

`lang/crates/vo-engine` is the public native compile/run API used by `cmd/vo` and host integrations.

Important files:

Paths in this list are relative to `lang/crates/vo-engine`.

- `src/lib.rs`: public exports.
- `src/compile/mod.rs`: compile entry points, cache wrappers, module-system errors, compile log sink.
- `src/compile/pipeline.rs`: project/file preparation, analysis, extension preparation, codegen.
- `src/compile/cache.rs`: compile cache.
- `src/compile/native.rs`: local/workspace native extension build and readiness.
- `src/run.rs`: VM/JIT creation, extension loading, output sink, runtime error mapping.
- `src/format.rs`: bytecode text dump.
- `src/toolchain.rs`: toolchain host install/run integration.

Key entry points:

- `compile(path)`
- `compile_with_options(path, options)`
- `compile_with_cache(path)`
- `compile_from_memory(fs, root)`
- `compile_source_at(source, root)`
- `compile_string(code)`
- `compile_with_auto_install(path)`
- `check*` variants

Native compile flow:

1. Determine source root, project root, package dir, and optional single-file entry.
2. Load project context from `vo-module` or classify single-file context.
3. Use frozen lock/cache readiness for normal project builds.
4. Collect source files with `FileSet`.
5. Resolve stdlib, module cache, current-module, and workspace replacements.
6. Analyze project with `vo-analysis`.
7. Prepare native extension specs.
8. Compile with `vo-codegen`.
9. Return `vo_stdlib::toolchain::ToolchainModule` as `CompileOutput`.

## Codegen Pipeline

`vo-codegen::compile_project(project)` compiles a type-checked `vo_analysis::Project` into `vo_common_core::bytecode::Module`.

Main stages in `lang/crates/vo-codegen/src/lib.rs`:

1. `TypeInfoWrapper::for_main_package`
2. create `CodegenContext`
3. `register_types`
4. `collect_declarations`
5. `compile_functions`
6. `compile_init_and_entry`
7. `collect_promoted_methods`
8. `finalize_itabs`
9. `build_runtime_types`
10. `fill_well_known_types`
11. `finalize_debug_info`
12. ID limit check
13. `ctx.finish()`

Important files:

- `context.rs`: module construction state.
- `func.rs`: `FuncBuilder`, storage allocation, instruction emission helpers.
- `assign.rs`, `lvalue.rs`: assignment and lvalue lowering.
- `type_info.rs`, `type_interner.rs`: mapping checked types to runtime metadata.
- `wrapper.rs`: wrapper functions and method/interface adaptation.
- `embed.rs`: embedded or generated wrapper support.

`FuncBuilder` storage can be stack values, stack arrays, heap boxes, heap arrays, references, or globals. Escape analysis and named return behavior can force heap allocation.

## Bytecode Model

`vo-common-core` owns the bytecode data model shared by VM, runtime, codegen, JIT, and web.

Important files:

- `src/instruction.rs`: fixed 8-byte `Instruction` and `Opcode`.
- `src/bytecode.rs`: `Module`, `FunctionDef`, metadata definitions.
- `src/types.rs`: `ValueKind`, `ValueMeta`, `ValueRttid`, `SlotType`.
- `src/runtime_type.rs`: `RuntimeType`.
- `src/serialize.rs`: bytecode serialization/deserialization.
- `src/debug_info.rs`: runtime error source lookup.

`Instruction` is:

```rust
pub struct Instruction {
    pub op: u8,
    pub flags: u8,
    pub a: u16,
    pub b: u16,
    pub c: u16,
}
```

`Module` includes functions, constants, externs, globals, struct/interface/named-type metadata, runtime types, itabs, well-known types, and debug info.

`FunctionDef` fields are runtime-sensitive. Check them before changing calling convention or stack layout:

- `param_count`, `param_slots`, `local_slots`, `ret_slots`
- `recv_slots`
- heap named-return metadata
- `is_closure`
- `error_ret_slot`
- `has_defer`, `has_calls`, `has_call_extern`
- `slot_types`
- `gc_scan_slots`
- `borrowed_scan_slots_prefix`
- cross-island `capture_types`, `capture_slot_types`, `param_types`

## VM Load And Run

`lang/crates/vo-vm` is no_std-compatible by defaulting through runtime abstractions when built for WASM.

Important files:

Paths in this list are relative to `lang/crates/vo-vm`.

- `src/vm/mod.rs`: `Vm`, load/run loops, interpreter dispatch, JIT integration hooks.
- `src/exec/*`: opcode-specific execution helpers.
- `src/scheduler.rs`: cooperative fiber scheduler.
- `src/fiber.rs`: stacks, call frames, defer/panic state.
- `src/gc_roots.rs`: VM root scanning.
- `src/vm/types.rs`: `VmState`, `VmError`, traps, scheduling outcomes.
- `src/vm/jit*`: VM-side JIT manager and bridge.

Native run flow:

1. `vo_engine::run_with_output_interruptible` creates `Vm`.
2. If JIT mode and feature enabled, create `Vm::with_jit_config`, call `init_jit`.
3. Set output sink, program args, interrupt flag.
4. `vm.load_with_extensions(module, ext_loader)`.
5. `vm.run()`.
6. If `SchedulingOutcome::Blocked`, convert to deadlock error.

VM loading initializes globals, extern registry, itab cache, entry/init metadata, and optional JIT manager.

## Scheduler And Fiber Model

Volang uses cooperative scheduling, not OS-thread preemption.

`Scheduler` manages:

- runnable queue
- current fiber
- blocked fibers
- host event waits
- I/O waits
- queue/port waits
- deadlock detection

`Fiber` contains:

- stack `Vec<u64>`
- `CallFrame` stack
- defer stack
- panic/unwind state
- closure replay state
- blocked/runnable/dead state

`FiberState` is `Runnable`, `Running`, `Blocked(BlockReason)`, or `Dead`. Avoid older docs that imply a simpler suspended model.

Borrowed call frames can reduce argument copying. Changes to calls, returns, defer, panic, JIT spill, or GC scan must account for borrowed frames.

## GC And Root Scanning

Runtime GC lives in `lang/crates/vo-runtime/src/gc.rs`. It is non-moving incremental tri-color mark/sweep with:

- white bit flipping
- gray and grayagain queues
- bounded root scanning
- allocation debt
- write barrier
- stress/debug support

Object scanning lives in `lang/crates/vo-runtime/src/gc_types.rs`.

VM root scanning lives in `lang/crates/vo-vm/src/gc_roots.rs`. Roots include:

- globals
- all non-dead fiber frames up to `gc_scan_slots`
- defer arguments
- unwinding return values
- panic interface values
- closure replay results
- queue/select wait registrations
- endpoint registry handles
- sentinel errors
- JIT panic messages and relevant side state

Root scanning is precise by `SlotType`. Do not describe it as conservative.

Interface scanning is special: interface slot 0 is type/itab metadata. The data slot is scanned only when metadata says it is GC-backed.

## Native Externs During Execution

Externs are registered through `vo-runtime::ffi::ExternRegistry`.

Runtime extension loading:

- `lang/crates/vo-runtime/src/ext_loader.rs`: dynamic library loading, ABI version/fingerprint checks, entry table loading.
- `lang/crates/vo-runtime/src/ffi/mod.rs`: `ExternCallContext`, `ExternResult`, registry dispatch.
- `lang/crates/vo-vm/src/vm/mod.rs`: `Vm::load_with_extensions`.

`ExternResult` can represent success, yield/blocking behavior, host event waits, replay, panic, not-registered, and closure calls. If an extern can block or require host replay, verify VM/JIT bridge paths as well as normal interpreter behavior.

## Islands And Cross-Island Values

Core runtime files:

- `lang/crates/vo-runtime/src/island.rs`: `IslandData`, island metadata, commands.
- `lang/crates/vo-runtime/src/island_msg.rs`, `lang/crates/vo-runtime/src/island_transport.rs`: command/frame transport.
- `lang/crates/vo-runtime/src/pack.rs`: `pack_slots` / `unpack_slots`.
- `lang/crates/vo-vm/src/vm/island_thread.rs`: native island thread loop.

Islands are separate VM/heap/scheduler domains. They communicate through commands, endpoint messages, and packed values. Do not imply ordinary heap sharing.

Cross-island value transfer deep-copies sendable values. Queue/port values can be represented through handles/proxies. Closure/interface/island transfer requires careful source review.

## JIT

JIT is feature-gated through `vo-vm` feature `jit`, enabled by default by `cmd/vo`.

Important files:

- `lang/crates/vo-vm/src/vm/jit_mgr.rs`: `JitConfig`, hotness counting, compile triggers.
- `lang/crates/vo-vm/src/vm/jit/mod.rs`: VM/JIT dispatch and side-exit handling.
- `lang/crates/vo-jit/src/lib.rs`: Cranelift compiler, cache, top-level compile APIs.
- `lang/crates/vo-jit/src/func_compiler.rs`: full function compilation.
- `lang/crates/vo-jit/src/loop_analysis.rs`, `lang/crates/vo-jit/src/loop_compiler.rs`: loop OSR.
- `lang/crates/vo-jit/src/translate.rs`: opcode lowering.
- `lang/crates/vo-runtime/src/jit_api.rs`: `JitContext`, helper function ABI, `JitResult`.

Environment variables:

- `VO_JIT_CALL_THRESHOLD`
- `VO_JIT_LOOP_THRESHOLD`
- `VO_JIT_DEBUG`

JIT calls use an extern C ABI with `JitContext`, args pointer, return pointer, and `JitResult`. Unsupported instructions, blocking externs, defer/panic complexity, or safepoints can side-exit or fall back to the VM. Describe JIT support per path, not as all-or-nothing.

## Change Recipes

Adding or changing an opcode:

1. Update `lang/crates/vo-common-core/src/instruction.rs`.
2. Update bytecode text dump and serialization if needed.
3. Update codegen emission.
4. Add VM execution in `lang/crates/vo-vm/src/exec/*` or interpreter dispatch.
5. Update GC root metadata if the instruction manipulates references.
6. Add JIT lowering or explicitly preserve VM fallback.
7. Add focused `tests/lang` tests and run VM/JIT.

Changing call/return convention:

1. Audit `FunctionDef`, codegen call sites, VM `exec/call.rs`, returns, defer/panic/recover.
2. Audit borrowed frames.
3. Audit JIT materialization/spill paths.
4. Audit bytecode serialization compatibility if persisted artifacts matter.

Changing GC behavior:

1. Update runtime object scanning.
2. Update VM root scanning snapshots.
3. Run GC stress/regression tests.
4. Check JIT side-exit and non-moving assumptions.

## Caveats

- Bytecode specs lag source. Use `instruction.rs`, `bytecode.rs`, codegen emit sites, and VM exec sites as truth.
- Some old docs refer to `Chan*`; current source includes queue/port/island opcodes.
- Some old docs mention `PtrClone`; it is not a current top-level opcode.
- `PtrNew` and call instruction encoding must be checked against current source, not old tables.
- JIT has real call and loop OSR paths, but unsupported opcodes and complex runtime effects can fall back. Avoid "fully complete" claims.
- GC is non-moving. JIT safepoints rely on that and on VM root/spill discipline, not on moving-GC stack maps.
