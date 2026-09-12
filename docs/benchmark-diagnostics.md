# Native benchmark diagnostics

`vo-dev bench diagnostics` owns the maintained native compiler, allocation,
collector-recycling, Island-transfer, layout-resource, GC-root, parent-Island and execution-work probes. The workload registry and output
contracts live in `cmd/vo-dev/src/dev_bench/diagnostics/probes.rs`. The examples
remain in the crates whose behavior they exercise.

Run from the repository root. Build `vo-dev` with the locked workspace first:

```sh
VOWORK=off cargo build --locked -p vo-dev
target/debug/vo-dev bench diagnostics list
target/debug/vo-dev bench diagnostics prepare all
```

`prepare` serially builds optimized `release-native` executables and freezes
them under a new `target/bench/diagnostics/<timestamp>` directory. It records the
source commit, complete file hashes, dirty diff and new source files, toolchain,
build environment, target and executable hashes. Sources must remain unchanged
until preparation completes. Allocation counters have separate builds; ordinary
timing executables compile out the allocator instrumentation. Partial or failed
preparations retain their logs and do not produce a completed snapshot.

Finish other builds, tests and profiles before measuring:

```sh
target/debug/vo-dev bench diagnostics run target/bench/diagnostics/<snapshot>
target/debug/vo-dev bench diagnostics compare target/bench/diagnostics/<before> target/bench/diagnostics/<after>
```

The default is two warmup processes and twelve formal processes per case and
version. `--runs N` and `--warmup N` override these counts, up to 100 each; at
least one formal process is required. `prepare compile`, `prepare regions`,
`prepare pipeline`, `prepare transfer`, `prepare recycling`, `prepare layouts`,
`prepare roots`, `prepare island` and `prepare execution` select individual suites. Comparison
snapshots must have matching probe sources, toolchain, build environment and
probe/counter variants. Copies in equal-length paths run in alternating AB/BA
order, with probe order rotated between rounds.

Every attempt retains its command, stdout, stderr, process outcome and semantic
validation. Timeouts, cancellation, malformed output and incomplete workload
matrices fail the run. The owned process group or Windows job also cleans up
descendants. Counter executables run in a separate phase; their duration fields
are excluded from timing summaries.

Completed results are under `target/bench/diagnostic-runs/<timestamp>`:

| File | Meaning |
| --- | --- |
| `identity.json` | Driver and frozen snapshot hashes, repetitions and measurement scope |
| `results.json` | Every successful warmup, formal and counter result, with its attempt directory |
| `summary.json` | Per-metric process sample counts, minimum, median and maximum; paired changes when comparing |
| `completed.json` | Completion count and result/summary hashes |
| `<attempt>/process.json` | Exit or failure, plus orchestration duration |

Inner repeats reduce to one median per process before cross-process statistics.
The region probe's first two inner iterations and all outer warmup processes
are excluded. Comparisons use the median paired log ratio. With at least six
independent process pairs, a fixed-seed, 20,000-draw paired bootstrap supplies a
95% percentile interval. Negative percentage changes mean shorter duration.
Raw observations remain available, including all slow samples. These summaries
do not estimate tail latency.

Timing scopes are explicit:

| Suite | Reported unit and included work |
| --- | --- |
| `compile` | Nanoseconds per in-memory compiler stage: combined project analysis, codegen, verification, serialization, decoding and loaded-module verification |
| `pipeline` | Exclusive nanoseconds per real Engine phase, plus total and unattributed time; filesystem input capture and compile caches included |
| `regions` | Nanoseconds per object; the probe's allocation loop and its checksum work |
| `transfer` | Nanoseconds per packet encoding; decoding and value validation happen outside the measured loop |
| `recycling` | Nanoseconds per allocation/collection cycle; five shapes in both collector modes |
| `layouts` | Verified module loading and layout-view cloning; numeric, pointer and aggregate inputs, with each view held after module destruction |
| `roots` | Explicit collection-step, bounded host-return and guest-resumption latency; global, blocked-Fiber and deferred roots |
| `island` | Parent bounded scheduler time per turn; idle polling or a fixed batch of runnable fibers |

Compiler input capture and filesystem caches are excluded from `compile`;
parsing/import/type analysis are currently combined in its analysis stage.
Requested allocation bytes, managed capacity and process RSS are different
resource domains. Counter records preserve each probe's original fields and
must be interpreted in that scope.

The `pipeline` suite compiles projects with 2, 9 and 33 owned packages. Each
process checks five repetitions of cache misses, cache hits and a changed leaf
file, giving 45 captures. Setup, guest execution and an independent reference
compilation happen outside the intervals. Cache hits must preserve cache bytes
and modification times and perform no analyzer source loading, type checking or
codegen. Workspace and captured-snapshot context loading still inspect import
headers; those operations have their own phases.
Every compiled guest checks its output, and capture-disabled recompilation must
produce identical bytecode. The `parsed_files` and `parsed_bytes` counters describe
analyzer source loading, excluding context import discovery. Parsed and module
totals also include imported standard-library work; owned project counts are
reported separately.

The non-default `compiler-profile` Cargo feature enables 15 nested, exclusive
phases: source-map insertion, lexing/parsing, import resolution, type checking,
escape/capture analysis, sendability, input capture, input fingerprinting, cache
lookup, cache publication, codegen, verification, bytecode decoding, workspace
context loading and captured-snapshot context loading. Profile schema
`volang.compiler-profile.v2` includes the two context phases; the reader preserves
replay of earlier snapshots with the original 13 phases and no profile schema. Fixed
thread-local storage bounds nesting to 64 scopes; overflow or a missed allocator
event invalidates attribution. This feature is compiler-only. The ordinary
runtime and AOT images acquire no profiling clock, allocator hook or dependency.

Pipeline timing and allocation executables are separate builds. Only the latter
uses `VO_PIPELINE_ALLOCATION_DIAGNOSTICS` to report successful allocation and
reallocation requests, assigned to their innermost active phase. A reallocation
charges its full new request size. Phase totals plus unattributed work must equal
the complete captured interval and allocation counts. Instrumented phase clocks
are diagnostic measurements, and ordinary compiler throughput needs a separate
feature-disabled product comparison.

Snapshot schema v3 adds a frozen input compiler and authenticated source/VOB
pairs for the three runtime suites. Preparation builds the CLI once, compiles
the registered fixtures with caching disabled, and retains every source and
bytecode file. Content-addressed staging paths keep source debug names stable
between snapshots. Runtime comparisons require identical source and VOB bytes;
changes to their producer cannot silently become execution-performance changes.
The compiler is never invoked during `run` or `compare`.

Snapshot schema v4 adds the execution-work suite and its separately declared
`execution-profile` feature. The reader also accepts existing v3 runtime-input
snapshots, v2 snapshots with explicit Cargo features and
featureless v1 snapshots; unsupported or incomplete
feature/timing/counter combinations fail validation. The optional standalone
`compiler_pipeline --reference-compiler PATH` mode checks all 45 VOBs against a
frozen feature-disabled CLI with `--no-cache`. That mode verifies the reference
binary hash and is excluded from ordinary diagnostic timing collection.

Process orchestration duration includes polling and is unsuitable for cold-start
benchmark claims. These native probes supplement the seven-backend benchmark
catalog; they do not establish Native AOT, Wasm VM, Core Wasm, browser or another
native architecture's coverage.

## Runtime input and result contracts

The layout suite uses the maintained `sum-array`, `binary-trees` and
`codegen-storage` sources. Each input runs with pointer views, element views and
both views. Processes discard two internal warmups and retain 25 load/clone
samples. The separate allocation build records requested bytes, peak extra
bytes and bytes retained solely by the views; the last view must release all
owned storage. Its clocks never enter timing summaries.

The root suite generates seven bounded fixtures: 0/256/8,192 separate globals,
16/1,024 blocked fibers and defer chains of depth 16/512. Every retained object
is checked after release. Each input runs VM and function JIT in both GC modes,
with a forced one-unit collection step and an ordinary control arm. Processes
discard 128 internal warmups and retain 512 observations. Automatic collection
stays enabled. The forced step's actual work and all subsequent host turns are
accounted separately; guest observation must occur exactly once. Function JIT
must enter compiled code during measurement with no new compilation.

Raw root records preserve each host turn, progress, root work and active/wall
latency. Summaries report process medians, retaining one independent sample per
process and metric. They provide no p95/p99 estimate; tail analysis must use the
raw event distribution with its actual event and independent-process counts.
Island processes retain 256 samples after their original 128 warmups. Requested
mode, child count, workload, checksum and native entry counts are validated.
All three suites use frozen executables and input copies in equal-length paths.

## Dynamic call distributions

The regular catalog includes 20 dynamic-call workloads, named
`{closure,interface}-{leaf,wrapped}-{mono,poly2,poly4,poly8,phase8}`.
Each performs 200,000 calls through one source call site and checks its exact
checksum. `mono` holds one target; `poly2`, `poly4` and `poly8` rotate through
two, four and eight targets. `phase8` holds one target for the first 100,000
calls, then rotates through eight targets. Leaf targets perform scalar work;
wrapped targets call the same small scalar `mix` function before returning.

For example, run one workload through the regular backend matrix with:

```sh
VOWORK=off target/debug/vo-dev bench closure-wrapped-poly4 --warmup 2 --runs 12
```

Keep these 20 workloads separate from the earlier 38-case common catalog when
comparing aggregate results. In particular, rapid monomorphic warmup does not
establish a benefit for target rotation or phase changes. Generated-code entry
counters and cache preparation/publication counters belong in a separate
diagnostic run; successful output alone does not prove a JIT call-site hit.

## Parent Island event polling

The standalone `vo-engine` example `island_poll` isolates parent scheduler work
with 0, 1, 8, 32 or 128 independent idle child Islands. Its checked source
fixture is next to the example. Build and compile it before timing:

```sh
VOWORK=off cargo build --locked --profile release-native -p vo
VOWORK=off cargo build --locked --profile release-native -p vo-engine --example island_poll
VOWORK=off target/release-native/vo build lang/crates/vo-engine/examples/island_poll.vo --kind=bytecode --no-cache -o target/island-poll.vob
VOWORK=off target/release-native/examples/island_poll target/island-poll.vob optimizing 128 busy 256 target/island-poll.json
```

Execution modes are `vm`, `baseline` and `optimizing`; workloads are `idle`
and `busy`. Each process warms 128 batches. A busy batch queues 32 fibers
outside the interval and executes 64 integer updates per fiber; an idle batch
calls the bounded scheduler 4,096 times. Creation, loading, output validation,
compilation, explicit collection and teardown are outside the intervals.
The probe completes collection before every interval to remove debt from
validation strings, then requires zero measured GC work and compilation.
It preserves the ordinary GC policy and scheduling budget. Busy JIT samples
require 32 actual parent function entries; the optimizing mode also proves
both hot functions executed at that tier. Child JIT entries are outside this
proof. Every checksum, output marker and final shutdown is checked.

The example also runs through `bench diagnostics prepare island` and the common
`run`/`compare` commands. The versioned JSON contains all
inner samples and the exact measurement scope. Treat independent processes
as statistical samples, keep build/profile work separate, and bind both
executables and the shared VOB when comparing versions. These measurements
do not describe Island thread count, process RSS or complete application
throughput. The controlled comparison is in
[the event polling report](island-event-gate-report-20260911.md).


## Interpreter and scheduler work

`prepare execution` freezes seven workloads (arithmetic, dynamic calls, maps,
buffered channels, rendezvous, ready select and short tasks) in four configured
modes: VM, baseline, optimizing and OSR. The fixture checks its output and values
on every invocation. Each process initializes once, warms 64 invocations, then
records 32 execution intervals. Decoding, construction, loading and initialization
have separate clocks. Spawning the measured entry and checking its output are
outside the execution interval. Compilation must remain unchanged after warmup.

The timing executable excludes `execution-profile`. The counter executable
includes this non-default feature and emits null clocks. Its per-VM fixed opcode
histogram, allocation polling/retry counts, frame refetches, immediate/blocked
queue outcomes and scheduler slice results have no process-global or implicit
child-Island aggregation. Saturating counts cannot wrap. The reader checks the
canonical opcode effect table, histogram totals and slice accounting. A dispatch
counts once after the allocation poll; a poll yield is counted separately.

Configured modes describe thresholds. The `warmup_jit` record reports actual
function/OSR entries, published tiers, distinct optimized functions and
low-progress disables. Per-interval records report native entries, feedback
fallback exits, dynamic cache callbacks and memory/GC deltas. A rendezvous
workload may disable a low-progress entry before reaching the optimizing tier;
zero optimized coverage remains explicit. Native entry counts alone do not prove
that every operation in a workload ran in native code. Inner repeats form one
median sample per process and never increase the independent sample count.
