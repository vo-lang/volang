# Volang AOT compilation

Volang provides native and Core WebAssembly AOT deployment from the same
verified bytecode module:

| Target | `vo build --kind` | Output | Runtime contract |
| --- | --- | --- | --- |
| 64-bit little-endian native host | `bin` | linked executable | packaged `libvo_aot_runtime.a` or UI-aware `libvo_ui_aot_runtime_native.a` |
| 64-bit little-endian native target | `object` | ELF, Mach-O, or COFF object | matching static runtime at link time |
| `wasm32-unknown-unknown` | `wasm` | executable Core Wasm module | `volang:runtime/v3` ABI |

All paths consume the serialized, verified Volang module and run target
verification before lowering. Target spellings must be canonical. Native
32-bit, native big-endian, WASI Preview 1, and WASI Component AOT requests are
rejected at the target boundary.

The public WebAssembly artifact is intentionally Core Wasm. A Component Model
wrapper needs a WIT-level capability ABI that can move strings, slices,
interfaces, errors, and extension values across the canonical ABI. Publishing
a component whose extern function receives only offsets into private core
memory would make that memory inaccessible to the component host. Volang does
not expose that incomplete contract as a build option.

## Commands

Build a native executable for the current host:

```sh
vo build ./cmd/server -o server
```

The official release archive places `vo`, `libvo_aot_runtime.a`, and
`libvo_ui_aot_runtime_native.a` together.
A custom runtime can be selected explicitly:

```sh
vo build ./cmd/server --runtime=/opt/volang/libvo_aot_runtime.a -o server
```

On Windows/MSVC the matching archives are `vo_aot_runtime.lib` and
`vo_ui_aot_runtime_native.lib`; `vo build` drives `link.exe` and emits an
`.exe`. `VO_AOT_LINKER` may select another link-compatible MSVC driver. As on
Unix, executable linking requires the platform linker and SDK to be installed;
`--kind=object` remains available when a downstream build owns final linking.

The linker selects the platform's UI runtime archive automatically when the
verified module mounts `github.com/vo-lang/ui`; ordinary command-line programs
keep the smaller core runtime. Packaged toolchains install both archives. A
custom UI runtime can be selected with `VO_UI_AOT_RUNTIME_LIB` or
`--runtime=PATH`; `VO_AOT_RUNTIME_LIB` continues to select the core runtime and
acts as the explicit fallback for UI builds.

Programs importing the `toolchain` package require a compiler host. Their
generated native entry calls the versioned runtime symbol
`vo_aot_initialize_toolchain_host_v1` before starting the program. The default
core runtime omits that capability, so linking such a program fails with the
missing symbol instead of producing an executable whose compiler calls fail
at runtime. `--kind=object` preserves this dependency for downstream linking.

An embedding that needs dynamic compilation can build `vo-aot-runtime` with
`--features toolchain-host` and select that archive through `--runtime=PATH`.
The CLI links the system libraries needed by this host on macOS and Windows.
`vo-dev test run --targets vm,native-aot-host --tags compiler-host` checks the
three dynamic compilation cases against the opt-in runtime and compares them
with VM execution. The core and compiler-host runtime variants run separately;
combining them in one test invocation is rejected to preserve their contracts.
Nightly runs both contracts on Linux, macOS, and Windows.
This opt-in archive includes the compiler and installs its host before entry.
Custom runtimes can implement the same C ABI initializer `int(void)`: install
a `ToolchainHost`, return zero on success, and return a nonzero process exit
code on failure. Initialization failure prevents the Vo program from running.
The ordinary release archive retains the smaller runtime without this feature.

Emit a relocatable cross-target object:

```sh
vo build ./cmd/server \
  --target=aarch64-unknown-linux-gnu \
  --kind=object \
  -o server.o
```

The CLI links executables only when the requested target equals the host.
Cross-target builds stop at an object so the target toolchain can supply its
linker, system libraries, and matching Volang runtime.

Build Core Wasm:

```sh
vo build ./webapp --kind=wasm -o webapp.wasm
```

Run it through the maintained JavaScript host:

```ts
import { runAot } from "vo-web";

const image = await fetch("/webapp.wasm").then((response) => response.arrayBuffer());
const execution = await runAot(image, {
  args: ["one", "two"],
  stdin: "one line\nsecond line\n",
  memoryLimitPages: 4096,
  fuel: 100_000_000n,
});
console.log(execution.result.stdout, execution.exitCode);
```

The host admits at most 128 MiB of image bytes, 16 MiB of encoded process
arguments, and 64 MiB of standard input. Standard input is consumed as raw,
line-oriented bytes with stable EOF behavior. The default guest-memory ceiling
is 4096 pages (256 MiB); embeddings may choose a smaller or larger explicit
ceiling, but can never admit fewer pages than the image manifest requires.
Fuel is optional and counts guest basic-block entries.

The published `vo-web` package also contains the lazily loaded
`vo-aot-support-wasm` semantic module. A browser loads it only when the image
declares regexp externs. Non-browser hosts should compile or read that module
once and pass it explicitly:

```ts
import { readFile } from "node:fs/promises";

const supportModule = await WebAssembly.compile(
  await readFile("node_modules/vo-web/aot-support/vo_aot_support_wasm_bg.wasm"),
);
const execution = await runAot(image, { args: ["one", "two"], supportModule });
```

This module contains the same Go-compatible regexp implementation used by the
VM, including arbitrary-byte strings, Unicode 16 tables, named captures,
replacement expansion, and syntax/resource validation. It is a versioned
runtime library with 64 MiB request/response admission limits, one
million-result bounds, and a 256 MiB Wasm memory ceiling. It contains no
Volang bytecode or interpreter.

Use `--debug-ir` for Native AOT lowering diagnostics and `--no-cache` for one
uncached build. Bytecode output remains available through `vo emit bytecode`
or `vo build --kind=bytecode`.

## Core Wasm execution model

The backend lowers every reachable, verified Volang function to a Wasm
function. Arithmetic, conversions, branches, loops, static and dynamic calls,
closures, strings, arrays, slices, maps, interfaces, defer, panic/recover,
goroutines, channels, `select`, ports, and islands execute in the generated
module. The artifact carries no serialized Volang bytecode and imports no VM
dispatch loop. A future bytecode opcode without a sound lowering fails the AOT
build with the function and bytecode PC.

The backend computes a closed-world capability summary for every reachable
function: suspension, allocation, unwind, host effects, managed roots, and
direct-local support. The fixed-point result selects one of three calling
conventions from semantics and verified layouts:

1. Safe-point-free functions use a typed multi-value Wasm ABI. Arguments and
   results travel as `i64` values alongside an owner frame and native-call
   budget; calls use Wasm locals directly and allocate no scratch frame.
2. Non-suspending allocation functions use precise shadow-root frames. Each
   fiber owns a bump-allocated root chain. Spawned fibers acquire a 4 KiB base
   chunk lazily; fiber teardown releases it. Deep calls grow through 64 KiB
   overflow chunks that are reclaimed on return. A frame records its
   predecessor, function identity, durable unwind owner, and compiler-described
   root slots. The collector walks this chain exactly.
3. Suspending functions, defer targets, unwind-capable fiber entries,
   recursive functions that require observable or effectful semantics, and
   callers with resumable children use durable linear-memory frames and resume
   states. These frames share the owning fiber's checked chunk stack, remain
   valid while the fiber is parked, and are reclaimed in constant time when a
   child returns.

Acyclic and recursive non-suspending allocation functions enter the shadow-root
tier directly. Every direct function also has a resumable durable lowering.
Rooted recursion runs in bounded native-Wasm segments; when a segment reaches
its engine-stack budget, execution continues synchronously from that exact
call boundary on the per-fiber explicit stack. Calls made after the transition
stay durable, including static, closure, and interface targets. Earlier
allocations and host-visible effects are never replayed. GC roots, panic state,
fuel, return values, and the logical stack limit remain continuous across the
transition.

Pure scalar recursive strongly connected components use a related retry path.
Their bounded native-Wasm attempt may restart through the durable body after
restoring consumed fuel because the closed-world proof excludes writes,
allocation, suspension, host effects, and other operations whose repetition
could be observed. Read-only pointer, array, slice, and closure access is
eligible because no fiber or host callback can interleave with the attempt.
This recovers native recursion speed on numeric and read-only traversal kernels
while retaining deterministic, recoverable deep-stack behavior.

Every tier carries a logical call budget and converts excessive recursion to a
regular Volang stack-overflow panic before a host engine can trap. That panic
uses the same defer/recover state machine as an explicit language panic,
including when it originates after a rooted-to-durable transition. The
scheduler may transfer up to 256 consecutive synchronous child calls or
returns within one fiber before rotating the run queue, preserving both call
locality and cooperative fairness.

The 16 MiB call budget is logical accounting, not an eager linear-memory
reservation. The image reserves only the entry frame plus one 4 KiB root
window, page-aligned; deeper root storage grows through the recyclable chunks
above. Small instances therefore avoid a fixed 16 MiB startup cost.
Reused durable frames are zero-initialized by the shared allocator. Shadow-root
chunks skip that whole-chunk clear because the compiler clears the exact live
root record before linking it into the collector-visible chain. This keeps the
language's zero-value and precise-GC guarantees while avoiding repeated 64 KiB
clears when deep root chunks cycle through the free list.

The backend promotes verified `Value` and `Float` slots to Wasm locals across
general control flow. The shared bytecode read/write-effect tables drive exact
materialization around calls, dynamic frame indexing, and runtime operations;
managed and interface slots remain authoritative in the precise frame. Slice
addressing and scalar pointer loads/stores consume promoted indices and values
directly while preserving nil, bounds, arithmetic-trap, and unwind behavior.
Static, closure, and interface call sites spill the complete promoted state
only when one of their verified targets can suspend; closed dynamic targets
retain their unrelated scalar locals across the call.

Straight-line memory kernels and scalar control-flow or recursive functions
execute with Wasm locals and native Wasm calls. This path includes scalar
array/slice access, checked pointer access, conversions, shifts, division,
authenticated numeric intrinsics, and pure closure bodies with capture reads.
Scheduler, defer, closure, and interface dispatch can all enter an eligible
callee directly. Small pure leaf functions are inlined through a semantic cost
model. Closed closure and interface targets share a typed Wasm function table;
dynamic dispatch resolves the verified target and uses `call_indirect`, with
the same capability-based ABI selection as static calls. No benchmark name or
source spelling participates in these decisions.

The language `copy` builtin lowers layout-identical slice and string copies to
Core Wasm `memory.copy`, including overlap-safe memmove behavior. Compact
primitive views with different physical strides keep the authenticated staged
runtime path, which preserves the same overlap contract without assuming a
byte layout the type system did not prove.

Managed heap objects carry compiler-produced tracing descriptors. The module
runs precise tracing collection over globals, live fibers, frames, closures,
slice backing storage, maps, interfaces, panic state, and scheduler objects,
then reuses reclaimed blocks. An ordered allocation-owner index resolves
interior references in logarithmic time; a capacity-safe chain lookup remains
available when the fixed index is full. GC debt is saturating, and collection
polls are emitted only on control-flow blocks that can increase managed debt.
Island transfer performs descriptor-driven deep cloning and preserves graph
identity without sharing mutable guest objects. Every child island owns a
fresh global state. Its generated package initializer runs to completion and
publishes a one-way initialization state before routed work becomes runnable,
so worker code cannot observe zeroed or partially initialized globals.

Maps grow at a bounded load factor, reuse deleted buckets, and implement the
language's scalar, string, floating-point, array, struct, and interface
hash/equality rules. Dynamic interface equality and type assertions use the
closed-world type and method tables produced from the verified module.

Authenticated `math.Sqrt`, `math.Floor`, `math.Ceil`, and `math.Trunc`
providers lower directly to equivalent Wasm instructions only when their
provider trust, effect declaration, and ABI match exactly. Same-name host or
extension providers receive no intrinsic authority. `math.FMA` stays on the
runtime path because Core Wasm lacks equivalent fused single-rounding
semantics.

## ABI v5

Each image contains exactly one `volang.aot.v5` manifest, one
`volang.externs.v3` extern table, one `volang.runtime.v1` type/layout table,
and one `volang.debug.v2` source-location and versioned frame-walk table. The
debug table carries the private frame-header width and field offsets needed by
`runtime.Caller`, preventing host code from embedding compiler-layout
constants. The manifest magic is `VOAOTW05`; it records
ABI version 5, Core-module kind 1, canonical target, initial memory pages,
verified semantic-module length, and the input module's SHA-256. The digest
identifies compiler input while the input bytecode itself stays outside the
image.

The artifact also emits the standard WebAssembly `name` custom section.
Imported runtime helpers, compiler helpers, source functions, typed fast
bodies, rooted bodies, retry bodies, durable bodies, indirect thunks, and
scheduler entry points receive deterministic names. Engines and profilers can
therefore attribute samples without depending on Volang's private debug
metadata; stripping this optional section does not change execution semantics.

The module imports from `volang:runtime/v3`:

```text
memory: WebAssembly.Memory
call-extern(extern_id: i32, frame: i32, destination: i32,
            arguments: i32, argument_slots: i32) -> i32
```

It exports:

```text
memory
vo_start() -> i32
vo_alloc(bytes: i32) -> i32
vo_alloc_sequence(bytes: i32, element_meta: i32) -> i32
vo_alloc_typed(bytes: i32, type_id: i32) -> i32
vo_map_lookup(map: i32, key_slots: i32) -> i32
vo_panic_message() -> i32
vo_panic_type() -> i64
vo_panic_data() -> i64
vo_raise_host_panic(frame: i32, message: i32) -> i32
vo_fuel: mutable i64
```

The imported and exported memory are the same object. `vo_start` returns zero
on success and a stable non-zero runtime status for bounds, arithmetic,
allocation, scheduler, type, stack, or panic failures. The panic exports expose
the unhandled panic value after status 12. Host sequence allocation requires
compiler-authenticated element metadata, which lets the collector trace
host-created reference sequences precisely and reject unknown layouts.
`vo_alloc_typed` and `vo_map_lookup` let authenticated host providers build
and inspect values using compiler-emitted runtime metadata. `vo_fuel` provides
a deterministic optional basic-block budget without changing language
semantics when left unlimited.

`vo-web` validates the complete import/export shape, all required custom sections,
target, reserved fields, size limits, extern-table bounds, and UTF-8 before
instantiation. Only externs declared in `volang.externs.v3` can cross into the
host. Every missing handler fails closed. A non-JavaScript embedding can
implement the same ABI directly.

## Benchmark contract

`./d.py bench` reports this backend as `Vo-WASM-AOT(Node)`. Correctness
preflight requires identical stdout from VM, JIT, Native AOT, and Wasm AOT.
Each measured Wasm sample starts Node, validates and instantiates the image,
builds the ABI host, and calls `vo_start`; timings therefore include process
startup, Wasm compilation, instantiation, and guest execution. Wasm build time
and image size are recorded separately.

The current 21-case measurements and cross-language comparisons are in
[`aot-benchmark-report.md`](aot-benchmark-report.md).

## AOT cache

AOT objects and Wasm images use a content-addressed cache at
`$HOME/.vo/aot/v1`. Set `VO_AOT_CACHE` to a non-empty absolute path to relocate
it. The key covers:

- compiler version and source-derived compiler build identity;
- canonical target and WebAssembly feature baseline;
- artifact kind and Native debug-IR option;
- Native AOT, Wasm AOT, and extension ABI versions;
- the complete serialized verified module.

Entries carry their key, exact length, and payload SHA-256. Reads are bounded,
symbolic-link entries are rejected, and writes use a private temporary file,
`fsync`, and atomic replacement. Missing or corrupt entries are rebuilt. Cache
I/O failures produce a warning and leave the build usable.

## Extensions and host capabilities

Native AOT authenticates extension contracts during compilation and requires
matching providers in the final executable. Build each provider as a Rust
`staticlib`, register its linkme owner once, and pass every archive to the
linker:

```rust
#[cfg(feature = "dynamic-extension")]
vo_ext::export_extensions!();

#[cfg(feature = "aot-static-extension")]
vo_ext::register_static_extension!();
```

```sh
vo build ./app \
  --link-extension=/absolute/path/libimage_extension.a \
  --link-extension=/absolute/path/libdatabase_extension.a \
  -o app
```

The CLI canonicalizes and deduplicates archives, then force-loads them so their
provider tables survive archive dead-code elimination.

Core Wasm capabilities are named ABI v5 extern handlers. `vo-web` provides
portable numeric, text, Unicode, formatting/scanning, JSON, TOML, regexp,
clock, virtual-filesystem, environment, and Fetch-backed HTTP providers. Raw
sockets, child processes, signals, links, ownership changes, and other browser-
forbidden operations return stable unsupported errors. Additional providers
must be supplied explicitly through `runAot`, and their ABI fingerprint and
control effects are checked before execution.

The governed `wasm-aot` test target shares the canonical wasm32 compile
surface while owning its stronger execution capability set. It therefore runs
goroutine, channel, select, island, dynamic-access, VFS, network-client, and
stdlib cases that the bytecode-in-browser target skips. Cases requiring native
process creation, listening sockets, pipes, symbolic links, host ownership or
permission changes, access to the repository filesystem, or the in-process
compiler are skipped explicitly for `wasm-aot`; those capabilities remain
available to Native AOT and can be added to custom Wasm embeddings as
authenticated providers where the host permits them.

## Compatibility and release contract

Native metadata and Wasm manifests have independent ABI versions. Decoders
reject unknown versions, duplicate or missing sections, non-zero reserved
fields, truncated inputs, trailing manifest bytes, invalid tables, target
mismatches, and values above declared limits.

Official release provenance records the CLI and static Native AOT runtime
path, size, and SHA-256. Release CI links and runs a Native AOT program before
packaging. Main CI builds a CLI-produced Core Wasm image, runs semantic cases
covering unwind, interfaces, compound map keys, slice allocation, scheduler,
channel wake/replay, child-island initialization, island cloning/isolation,
deep tracing, GC layout, formatting/scanning, VFS, Fetch networking, regexp,
and binary trees, then executes the image through the maintained JavaScript
host. The complete language job also runs the independent `wasm-aot` matrix.
