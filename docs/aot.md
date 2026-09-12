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

Runtime type metadata records an array's logical slot count independently of
the bytecode frame's 16-bit slot limit. Arrays beyond that limit use canonical
heap storage and allocation descriptors for their elements; their type records
omit descriptors for flattening the whole array into a frame or sequence element.
The host validates the logical count against the array length and element type.
Ordinary value types still require complete flat allocation descriptors, and
runtime storage widths remain bounded by wasm32.

The backend computes a closed-world capability summary for every reachable
function: suspension, allocation, unwind, host effects, managed roots, and
direct-local support. The fixed-point result selects one of three calling
conventions from semantics and verified layouts:

1. Safe-point-free functions use a typed multi-value Wasm ABI. Arguments and
   results travel as `i64` values alongside an owner frame and native-call
   budget; calls use Wasm locals directly and allocate no scratch frame.
2. Allocation-free direct functions with managed slots use a temporary rooted
   ABI and an explicit owner frame. They cannot cross a collector or suspension
   boundary while that temporary state is authoritative.
3. Allocating and suspending functions, defer targets, unwind-capable entries,
   and callers with resumable children use durable frames in Island-owned span
   cells. Each frame is independently registered with its function layout and
   remains valid while its fiber is parked. Return unregisters it before reuse.

Every direct function also has a durable lowering. A direct segment that needs
an explicit stack continues from its exact call boundary; already performed
allocations and host-visible effects remain committed. GC roots, panic state,
fuel, return values, and logical stack accounting survive the transition.

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

The 16 MiB call budget is logical accounting. The image reserves its entry
frame and a small root window; deeper durable frames acquire span cells as
needed. Reuse clears the requested record, avoiding a fixed 16 MiB startup
reservation and preserving the language's zero-value guarantee.

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
Image-owned string literals can also return through this typed ABI without
allocating a child frame. Dynamic string construction retains its allocation
effects and durable-frame requirements.
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

Managed objects use stable-address 64 KiB spans owned by individual Islands.
The JavaScript memory provider maintains a shared page directory; each Island
owns its allocation bitmap, exact requested extents, size-class reuse, roots,
collector cursors, policy, counters, and sticky errors. Empty blocks can change
size class or serve contiguous large runs. Terminal Island cleanup returns
unused pages to the provider for reuse by other Islands. Guest headers never
establish allocation membership.

Generated functions and container helpers call the memory owner through
reserved negative runtime operations. Compiler-emitted descriptors cover
frames, inline values, sequences, map backing, and queue payloads. Root, object,
remembered-set, remark, lease, sweep, epoch-reset, and page-reclaim work retains
cursors across bounded steps. Active materialized frames are registered per
Island, avoiding repeated traversal of a changing recursive frame chain.
Typed new-value barriers preserve incremental and old-to-young reachability.
Compiler-proven scalar frame writes and zero fills need no new-value barrier.
Other stores consult conservative slot hints through a four-byte-per-page
Wasm table before crossing the host boundary. The table occupies 256 KiB per
instance; object headers cache a first-32-slot mask and a remaining-root byte
cutoff. These hints never establish allocation membership or replace precise
host validation. Root barriers run only while a collector is active; heap
barriers also retain old-to-young edges between collections.
Allocation polls consume the common execution-effect contract, including
implicit interface payload copies. Allocating functions use durable frames;
verified allocation-free numeric functions retain the scalar fast ABI.

Island transfer performs descriptor-driven deep cloning and preserves graph
identity without sharing mutable guest objects. Every child island owns a
fresh global state. Its generated package initializer runs to completion and
publishes a one-way initialization state before routed work becomes runnable,
so worker code cannot observe zeroed or partially initialized globals.

Maps grow at a bounded load factor and compact tombstones into a new backing
generation. Iterators retain their starting backing and follow entry forwarding
across subsequent generations, preserving current values and deletion without
repeating an original entry. Maps implement the
language's scalar, string, floating-point, array, struct, and interface
hash/equality rules. Dynamic interface equality and type assertions use the
closed-world type and method tables produced from the verified module.

Core Wasm declares the `island-span-heap` contract. Child Islands inherit memory
policy while retaining independent heaps, collectors, and terminal errors.
Failed allocation exits before a null dereference or subsequent guest effect;
healthy Islands keep executing. `runtime/mem` exposes the same statistics and
accepted-work semantics as the native runtime. `GCStep(0)` changes no collector
state, and `GCCollect` schedules a major collection at a scheduler boundary.

`memoryLimitPages` bounds the entire instance, including static data and all
Islands. `memory` configures reserve, managed hard limit, growth/allocation
permissions, object/lease limits, collection mode, automatic GC, and OOM policy.
Hard limits cover committed managed pages. JavaScript/provider allocations are
an external domain and retain an unknown-provider counter; a process-wide
zero-allocation or memory limit needs host-side accounting as well.
No-growth admission reserves span records and gray capacity before execution;
lease admission enforces the configured or derived capacity.

Hosts can require `requireMemoryContract: 'island-span-heap'`. The `onMemory`
callback receives controls after admission and before guest execution. Controls
can reserve capacity, change permissions/limits, select an idle collector's
mode, toggle automatic GC, report external bytes, read stats, request a major,
or perform a bounded step. Mutating controls require a host scheduling boundary.

Programs using concurrency or async externs have cooperative block quanta in
suspending functions and their callers. Durable frames retain the next block
before yielding; the scheduler rotates runnable fibers and the JavaScript
driver periodically yields to host timers. Synchronous recursion fallbacks
retain internal resume state without emitting host-yield statuses. Total fuel
remains a separate execution limit. GC slices consume a bounded work budget. Long individual mutator helpers
remain outside the block quantum, so the quantum is not a wall-clock pause
guarantee.

Authenticated `math.Sqrt`, `math.Floor`, `math.Ceil`, and `math.Trunc`
providers lower directly to equivalent Wasm instructions only when their
provider trust, effect declaration, and ABI match exactly. Same-name host or
extension providers receive no intrinsic authority. `math.FMA` stays on the
runtime path because Core Wasm lacks equivalent fused single-rounding
semantics.

## ABI v9

Each image contains exactly one `volang.aot.v8` manifest, one
`volang.externs.v3` extern table, one `volang.runtime.v1` type/layout table,
one `volang.debug.v3` source-location and versioned frame-walk table, and one
`volang.memory.v1` precise allocation/root-layout table. The
debug table carries the private frame-header width and field offsets needed by
`runtime.Caller`, preventing host code from embedding compiler-layout
constants. The manifest magic is `VOAOTW09`; it records
ABI version 9, Core-module kind 1, memory-contract tag 1 (`island-span-heap`),
canonical target, initial memory pages,
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
vo_current_fiber: mutable i32
vo_fiber_head: mutable i32
vo_gc_debt: mutable i32
vo_gc_barrier: mutable i32
vo_memory_failed: mutable i32
vo_execution_quantum: mutable i32
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

Host extern APIs are scoped to the invoking Island and expire after synchronous
return or asynchronous replay. Allocations remain rooted through an outstanding
Promise. Asynchronous return tuples are staged and published together, including
conditional interface pairs. `call.lease(reference)` creates an explicit root
that can outlive a call; resolve it before use and release it when finished.
Released leases fail closed. Accessing `call.memory` borrows a raw view for that
call and conservatively retains its owner until a subsequent bounded major
rebuilds precise edges. Raw views must not be used after their call expires.
Pending host calls are cancelled when guest execution ends.

ABI v9 uses an explicit FIFO ready queue with reusable host-owned waiters.
Channel/select state changes wake the affected contenders; a select wake removes
its other registrations before publishing readiness. Closing a queue advances
at most 64 wakeups per scheduler crossing. Promise completion retains the fiber
identity, so a late completion cannot wake a reused address. The optional
`memory.maxSchedulerWaiters` bounds instance-wide waiter capacity separately from
managed bytes. Managed frames and queued payloads remain precise GC roots.
The returned `schedulerStats` reports dispatches, waits, wakeups, live fibers,
active registrations, admitted waiter capacity, and outstanding host waits.
These counters describe scheduler work independently of `memoryStats`.

The memory owner publishes `vo_memory_failed` only when a failure occurs, letting
healthy scheduler dispatch stay in Wasm. Frame construction supplies its function
identity explicitly, and the common materialized-frame allocator registers it in
the same host call. Explicit frame destruction repays debt from the current
allocation period; a frame surviving an earlier collection cannot cancel newer
guest allocation debt. Object tracing reuses a bounded cursor and checks both object and backing
identity across steps; minor sweep visits young bitmaps and skips entirely old
spans. Remembered cards use bounded bitmap scans and empty spans leave the card
set immediately. Retired span metadata is reused with a bounded cache per size
class. Shared execution budgets are interrupted on every request independently
of the cached host-owned GC flags.

`vo-web` validates the complete import/export shape, all required custom sections,
target, reserved fields, size limits, extern-table bounds, and UTF-8 before
instantiation. Only externs declared in `volang.externs.v3` can cross into the
host. Every missing handler fails closed. A non-JavaScript embedding can
implement the same ABI directly.

## Benchmark contract

`./d.py bench` reports this backend as `Vo-WASM-AOT(Node)`. Correctness
preflight requires matching stdout from VM, JIT, OSR, Native AOT, Core Wasm AOT,
no_std VM, and Wasm VM on the native 64-bit benchmark matrix.
Each measured Wasm sample starts Node, validates and instantiates the image,
builds the ABI host, and calls `vo_start`; timings therefore include process
startup, Wasm compilation, instantiation, and guest execution. Wasm build time
and image size are recorded separately.

Each run retains its generated bytecode and native/Wasm executables in its own
artifact directory. `results/sources.json` binds the tracked and untracked source
bytes to the commit; `results/build.json` records tool versions and executable,
archive, and Web host hashes. Preflight output and failure status are retained
under `results/preflight`, including native JIT/AOT and Core Wasm work counters.
Instrumentation is confined to preflight. Timed samples run without those extra
diagnostics. The no_std runner consumes precompiled bytecode; VM/JIT/OSR source
runs may use the compiler cache, while Wasm VM compiles the source in each new
Node process. These series have distinct startup boundaries.

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

Core Wasm capabilities are named ABI v9 extern handlers. `vo-web` provides
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

Native AOT ABI version 3 adds a separately relocated static continuation table,
ordered resume PCs, and precise stack maps for each recovery body. The recovery
ABI carries its PC in argument lane zero and loads canonical frame slots; it
never participates in ordinary call dispatch or deopt fallback. Execution-budget
charges use the same region analysis as normal entry. This restores native
execution after scheduler and GC transitions without a runtime compiler.

Recovery optimization models every advertised PC as an independent CFG entry.
Its live-in values are unknown canonical-frame values, including unknown root
provenance unless the static slot type itself guarantees an exact base.
Constant propagation, value numbering, and range checks honor all external
edges. Entry trampolines load the live values and complete root projection for
their selected PC; object virtualization remains disabled for recovery bodies.
Additional analysis shares the per-function analysis budget, and exhaustion
retains the baseline recovery body. These decisions are made while building the
native image and do not require runtime compilation.

ABI version 4 changes the generated small-object allocation contract: a region
is guarded by exact physical size and every consumed object publishes its own
ValueMeta before its allocation bitmap bit. This permits equal-size objects of
different types to share admission. Code and runtime using the previous guard
and header contract must be rebuilt together; their unchanged field offsets do
not establish ABI compatibility.

ABI version 5 adopts the compact three-slot Rust string descriptor and its
exact-base array owner edge. Native images and their runtime must be rebuilt
together. The bytecode wire representation of string constants is unchanged;
the extension ABI fingerprint separately identifies the descriptor layout.

ABI version 6 makes the first five native argument words authoritative in
machine lanes. Callers populate the wide tail of the callee's shadow window;
Selected recursive call paths retain their existing leading-word mirror.
Ordinary callees initialize alias-backed leading parameters
before guest safepoints and publish the leading words on the cold tier-up path
that can reject entry before SSA initialization. VM and prepared-call entries
derive their lanes from their initialized frame; continuation entries retain
their independent frame-import contract. The signature width is unchanged,
but older callees rely on caller-populated leading slots, so images and runtime
must be rebuilt together.

ABI version 7 adopts the seven-slot canonical array slice and nine-slot
extended slice descriptors. All hot access fields remain in the common prefix;
element metadata and layout occupy separate 32-bit halves of one slot. Generated
native code reads the new layout and stride offsets. Images, runtimes and native
extensions must be rebuilt together; the extension fingerprint includes both
shapes and their tags. Bytecode and Core Wasm retain their existing formats.

ABI version 8 adopts one control word per Rust runtime Map bucket. Two high
bits distinguish empty, tombstone, occupied and forwarded buckets; the lower
62 bits retain the hash or forwarding index. Checked backing extents preserve
all usable probe/index bits. Key equality and generation-based iterator
identity are unchanged. The native extension fingerprint includes the Map
header, iterator, backing geometry, control encoding and key hash scheme; rebuild images,
runtimes and extensions together. Core Wasm retains its independent Map layout.

ABI version 9 embeds VOB22 modules with direct binary32 arithmetic and
comparison opcodes. Existing opcode numbers are preserved; new binary32
opcodes follow `ForLoop`. Each operation rounds at its declared width and
stores the low 32 result bits with a cleared high half. Rebuild native images
and runtimes together. Core Wasm emits standard binary32 Wasm instructions
and retains its existing host ABI and physical layouts.

The metadata also includes the four-entry dynamic-call cache, the
optimizing-entry profile marker, the native stack-byte guard, and distinct
prepared-call continuation/frame fields. Producers, runtime metadata decoding, and the
native artifact cache key share `vo_jit::NATIVE_AOT_ABI_VERSION`; older native
images must be rebuilt. Literal bytes are read from the immutable module constant table through
`vo_str_new_const`; non-empty runtime strings still allocate Island-local
backing and descriptors under the managed admission contract.


Native ABI version 10 adds a verified zero/one hidden-argument offset to prepared
calls and dynamic-call cache entries. Captureless function values place user
arguments at slot zero; closure/receiver calls retain their hidden slot. The
register lanes and shadow frame use the same verified layout. Old native images
must be rebuilt with the matching runtime even though both C structs retain
their previous sizes. VOB22 and Core Wasm host ABI8 remain unchanged.

Native ABI version 11 carries an optional module-local instruction source for
runtime traps separately from the physical recovery PC. Shared cold trap blocks
forward that source; generated frame recovery continues to use the calling
instruction. The VM validates the source against the loaded module, and every
new trap or explicit panic clears the previous source. Images and runtime
archives must be rebuilt together. This does not change VOB22, Core host ABI8,
or the separate extension-provider ABI.


Native ABI version 12 accounts for the bounded detached small-block metadata
cache in SpanHeap. Its fixed directory changes generated-code-visible collector
field offsets. Rebuild native images and static runtime archives together;
images from the earlier layout must be rejected. The cache retains host metadata
only, with at most one instance per size class, and immediately returns empty
managed blocks for reuse. VOB22 and Core host ABI8 remain unchanged. The extension
provider ABI remains independent: extensions use owner dispatch and their own
private Gc proxy, without accessing the host collector's Rust layout.

Native ABI version 14 embeds VOB24 compact source coordinates and logical source metadata. Images and static
runtime archives must be rebuilt together so the compiler-free decoder consumes
the same format as the producer. Compile cache schema 19 prevents reuse of the
earlier debug representation. This step leaves generated-code context offsets
and extension-provider ABI10 unchanged.

Core Wasm optionally includes `volang.inline-sources.v1` when logical inline
attachments exist. Core host ABI9 requires the compact `volang.debug.v3`
physical Caller table. Its `VODBG003` header, file paths and counts retain fixed
little-endian u32 fields; each physical record encodes PC, file ID, line, column
and length as canonical unsigned LEB128, matching the VOB24 coordinate domain.
Hosts reject overlong, overflowing and truncated words before use. The host
retains sorted packed numeric records and materializes locations on query.
The optional inline section keeps its independent wire contract. Its `VOINS001` payload starts
with three little-endian u32 counts: module functions, source frames, and sparse
function tables. Each frame stores six u32 words: parent index (`u32::MAX` for
the physical root), function ID, file ID, line, column and span length. An absent
span stores file ID `u32::MAX` and three zero coordinates. Each function table
stores function ID, final function bytecode length, entry count, then `(pc, frame)`
pairs. File IDs refer to the required debug section. The common depth/count
limits apply, and hosts validate ancestry, exact PCs and physical ownership
before instantiation. The function count must match the physical debug section.

`parseAotDebugMetadata` and `lookupAotLogicalSources` expose these immutable
coordinates to host diagnostics. Ordinary Caller queries continue to use their
exact physical table and execution-frame walk. Missing optional ancestry yields
the physical source entry; an inline chain never supplies a suspension or GC frame.

The host retains the validated inline section as one shared numeric buffer.
Sparse PC indexes use the same sorted read-only lookup abstraction as physical
source locations. Frame and location objects are created on query; callers do
not receive a mutation API for the retained numeric records.

Native runtime failures retain an instruction source plus an optional physical
inline-callsite source. Both anchors resolve through the common bounded source
DAG. Panic save/recover/reuse moves the two anchors as one state. Engine errors
own their resolved names and locations independently of the executable module;
the compiler-free static runtime formats the chain while its module is alive.
This changes Rust-owned diagnostic state without adding a generated-code context
field or changing Native ABI14. Suspension PCs and precise root maps retain their
separate contracts.


Native ABI version 15 expands the shared dynamic-call cache to four stable
entries and one replaceable victim entry. Generated VM/JIT/OSR/Native code and
the static runtime agree on the 200-byte site stride; each entry keeps the
existing 40-byte identity, prepared-call shape, and dispatch generation guard.
Replacing the victim invalidates its previous native pointer and shape before
publishing a new target. Extension ABI10, VOB24, the bytecode compilation cache
schema, and Core host ABI9 are unchanged. Older Native images fail version
validation before execution.
