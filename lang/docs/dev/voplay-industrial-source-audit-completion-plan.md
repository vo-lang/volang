# voplay Industrial Source-Audit Completion Plan

Status: superseded
Superseded-By: lang/docs/dev/voplay-code-engineering-quality-plan.md
Superseded-Date: 2026-07-05
Date: 2026-07-04
Scope: final completion plan for making voplay's WebGPU render runtime and
kart physics foundation industrial-grade for the scoped BlockKart validation
product.

## Role

This historical document superseded:

- `lang/docs/dev/voplay-industrial-completion-plan.md`
- `lang/docs/dev/voplay-industrial-first-principles-completion-plan.md`

The active execution plan is now
`lang/docs/dev/voplay-code-engineering-quality-plan.md`.

The previous readiness report can currently write `industrialReady: true`, but
source inspection shows that the result is a false positive. This plan turns
that false positive into concrete engineering work.

The final policy is simple:

- Source audit has priority over report flags.
- A report flag is valid only when the source path proves the same result.
- A gate that misses a source-audit failure must be repaired before more
  implementation work is credited.
- Phase passes cannot be described as final engine readiness.

## Current Source-Audit Baseline

Current inspected roots:

- Volang: `/Users/macm1/code/github/volang`
- voplay: `/Users/macm1/code/github/voplay`
- BlockKart: `/Users/macm1/code/github/BlockKart`

Current source facts:

- `rust/src/renderer.rs` is about 1230 lines.
- `rust/src/renderer/frame_submit.rs` is about 9 lines and delegates to
  `FrameSubmitOrchestrator::run`.
- `rust/src/renderer/frame_orchestrator.rs` is about 1628 lines and still owns
  frame decode, scene collection, batch planning, graph building, pass wiring,
  post setup, backend submit invocation, and telemetry encoding.
- `rust/src/pipeline3d.rs` is about 373 lines and has split submodules.
- Render pass bodies now live in pass executor modules.
- Runtime pass execution uses `FrameGraphExecutor.execute_node`.
- `RenderResourceRegistry` owns several concrete render targets.
- `RenderBatchPlan` affects main and water submission through planned draw
  lists.
- `voplay-render-stress-budgeted` can report `status: pass` while its summary
  contains frame p90 around `959ms`, frame p99 around `5814ms`, and 11 slow
  frames.
- `scripts/ci/voplay_render_stress.mjs` still has a `hostPacingOnly` path that
  can suppress p99 and slow-frame budget failures.
- `RenderFrameDecode`, `RenderSceneSnapshot`, `FrameGraphBuild`,
  `FrameGraphExecute`, `PerfPacketEncode`, and `RenderFramePipeline` are
  declared but not constructed by the runtime path.
- `RenderWorldChunk` currently records zero bounds for planned mesh, primitive,
  and water chunks.
- `RenderBatchPlanner::select_lod` uses seed and workload heuristics rather
  than distance, projected size, quality profile, and chunk metadata.
- `frustum_culled_chunks` and `distance_culled_chunks` exist as report fields,
  but current source does not increment them from the batch planner.
- The physics control chain exists, but surface material selection still uses
  track-position inference in vehicle, contact, and telemetry paths.
- `Vehicle.SetPose` writes body pose and velocity directly, then applies a
  backend wheel command.
- BlockKart still owns a large primitive authoring layer and a wide HUD fact
  assembly path.

Current false-positive evidence:

- `target/voplay-industrial-readiness-report/report.md` says
  `industrialReady: true`.
- `target/voplay-industrial-readiness-report/report.json` also records
  `sourceFacts.batchPlanSceneWired: false`.
- The same report records render stress summary values that do not prove the
  scoped 60 FPS target.

## Industrial Completion Contract

Final completion requires all of these facts to be true at the same time:

- The source audit has no unresolved failures.
- `./d.py ci task voplay-industrial-readiness` passes.
- The readiness report contains `industrialReady: true` and `failures: []`.
- The readiness report contains no internal contradiction such as a failing
  source fact with an overall pass.
- Render stress and soak gates enforce budgets without `hostPacingOnly`
  suppression.
- The browser quickplay artifact and source commits match through provenance
  and source audit.
- Every required final report is regenerated after the final source changes.

Before all of those facts are true, the only allowed final status is:

```text
industrialReady: false
```

## First-Principles Audit Method

Every capability must answer these questions with file and line evidence:

| Question | Required answer |
| --- | --- |
| Who owns the data? | One subsystem owns the durable state. |
| Who mutates the data? | Mutations occur through a narrow owner API. |
| Who executes the runtime path? | The runtime path is traceable from entrypoint to backend. |
| What bypasses remain? | Bypasses are either removed or fail the gate. |
| What proves performance? | Fresh stress or soak evidence with hard budgets. |

The readiness script must emit these audit facts in machine-readable form. Any
failed audit fact is a P0 final-readiness failure.

## Workstream 0: Repair The Final Gate

### Required Result

`voplay-industrial-readiness` must become a source-audit gate, not a flag
aggregator. It must fail on the current source state.

### Required Checks

- Fail when any source fact in `sourceFacts` is false and the fact is listed as
  industrial-required.
- Fail when `RenderFrameDecode`, `RenderSceneSnapshot`, `FrameGraphBuild`,
  `FrameGraphExecute`, `PerfPacketEncode`, or `RenderFramePipeline` is declared
  but not constructed by runtime code.
- Fail when `run_frame_orchestrator` still directly performs decode, snapshot,
  batch, graph, execute, submit, and telemetry work in one large function.
- Fail when render stress summary p90, p99, or slow-frame totals exceed the
  final budget, even if every scene status is `pass`.
- Fail when `hostPacingOnly` suppresses p99 or slow-frame failures.
- Fail when batch planner bounds are zero placeholders.
- Fail when LOD selection is based on seed or workload heuristics.
- Fail when frustum or distance culling counters are present but never
  incremented by real batch planning.
- Fail when surface material is inferred from track position in industrial
  vehicle, contact, or telemetry paths.
- Fail when `Vehicle.SetPose`, reset, sleep, or recovery bypass backend helper
  contracts.
- Fail when BlockKart primitive authoring and low-level HUD fact assembly remain
  outside read-only product consumption.

### Exit Criteria

- The current source produces `industrialReady: false`.
- Each false-positive class above appears as a named P0 failure.
- Deleting or weakening a required check fails `eng-lint-tasks`.
- The report includes an evidence table with file paths, line numbers, owner,
  mutation path, execution path, bypass status, and gate result.

## Workstream 1: Real Frame Pipeline

### Required Result

The render frame path must use real pipeline stages:

```text
Renderer.submit_frame
  -> FrameSubmitOrchestrator::run
      -> decode_frame
      -> build_scene_snapshot
      -> build_batch_plan
      -> build_frame_graph
      -> execute_frame_graph
      -> submit_backend
      -> encode_telemetry
```

The stage structs in `renderer_frame.rs` must be constructed from runtime data
and carried into telemetry.

### Required Code Results

- `run_frame_orchestrator` becomes a short orchestrator.
- `RenderFrameDecode` is returned from `decode_frame`.
- `RenderSceneSnapshot` is returned from `build_scene_snapshot`.
- `FrameGraphBuild` is returned from `build_frame_graph`.
- `FrameGraphExecute` is returned from `execute_frame_graph`.
- `PerfPacketEncode` is returned from `encode_telemetry`.
- `RenderFramePipeline` is assembled once per submitted frame.
- Scene collection mutates retained world state only during decode or explicit
  scene update.
- Pass execution receives immutable snapshot and plan views.
- Telemetry reports decode, snapshot, batch, graph build, pass execution,
  backend submit, present, resource churn, and perf packet encode timings.

### Exit Criteria

- `rg "never constructed" target output` no longer reports the stage structs
  after `voplay-render-core-unit`.
- `rg "let mut reader = StreamReader" rust/src/renderer/frame_orchestrator.rs`
  has no result.
- `rg "PostUniform::from_settings|encode_renderer_perf_packet" \
  rust/src/renderer/frame_orchestrator.rs` has no result.
- `frame_orchestrator.rs` is small enough to review as orchestration glue.
- The readiness evidence table points to each stage owner and caller.

## Workstream 2: FrameGraph Execution Ownership

### Required Result

FrameGraph must own pass dispatch. The orchestrator may build a graph and hand
it a frame context, but it may not pass ad hoc closures that contain pass
execution policy.

### Required Code Results

- `RenderPassNode` stores or resolves its executor through a pass registry.
- `FrameGraphExecutor.execute_node` dispatches through node metadata and a
  typed frame execution context.
- The `execute_render_node!` macro is removed from runtime code.
- `FrameGraphExecutor.execute_pass` is removed or restricted to tests.
- Pass enabled predicates, reads, writes, transient writes, workload, and
  diagnostics are declared with the pass node.
- `BackendSubmitExecutor` remains the only runtime owner of `queue.submit`.

### Exit Criteria

- `rg "execute_render_node!" rust/src/renderer` has no result.
- `rg "\.execute_pass\(" rust/src/renderer rust/src/renderer_frame.rs` shows
  test-only use or no result.
- `FrameGraphExecutor.execute_node` can execute all runtime pass kinds through
  registered pass nodes.
- A newly added runtime pass without a node fails structure lint.

## Workstream 3: Concrete Resource Registry

### Required Result

`RenderResourceRegistry` must own all render target lifecycle and expose pass
safe views through context APIs.

### Required Code Results

- Registry owns surface frame metadata, depth, MSAA main, post color, receiver
  mask, surface props, shadow, water, overlay, capture, and readback targets.
- Resize and recreate paths are registry methods.
- Pass executors request targets by resource ID and pass name.
- Missing required reads or writes produce a hard failure with resource and pass
  names.
- Shadow target ownership moves out of pipeline internals into registry-owned
  lifecycle, while pipeline code may still own shader and draw state.
- Resource telemetry distinguishes create, reuse, alias, recreate, missing
  read, missing write, and capture/readback allocation.

### Exit Criteria

- No render target creation helpers are called from renderer runtime glue.
- `Renderer` does not hold parallel target-owner fields.
- Resize/recreate stress passes with target readiness equal to target count.
- Resource churn stays within final budgets with no waiver path.

## Workstream 4: Real BatchPlanner And World Visibility

### Required Result

`RenderBatchPlan` must be the source of truth for visibility, LOD, upload,
resident state, and submission batches.

### Required Code Results

- Mesh objects carry world bounds derived from model bounds and transform.
- Primitive chunks carry real merged bounds from `PrimitiveRenderWorld`.
- Water, terrain, and decal batches carry bounds and material groups.
- Frustum culling removes batches from submitted work.
- Distance culling removes batches from submitted work.
- LOD selection is based on distance, projected size, chunk metadata, and
  `RenderQualityProfile`.
- `frustum_culled_chunks` and `distance_culled_chunks` increment from real
  decisions.
- `dirty_uploads` and `resident_rebuilds` come from real dirty/resident state.
- Main, depth, shadow, water, terrain, decal, and transparent pass inputs are
  plan-derived pass views.
- Pass executors do not receive raw `model_draws`, `primitive_draws`, or
  `primitive_chunks` as primary submission state.

### Exit Criteria

- Zero-bounds placeholder chunks fail unit tests.
- Seed/workload LOD selection fails unit tests.
- A chunk outside the frustum changes submitted draw workload.
- A chunk beyond distance cutoff changes submitted draw workload.
- LOD policy changes submitted triangle or instance workload.
- Stress telemetry explains visible, culled, LOD0, LOD1, dirty upload, and
  resident rebuild counts.

## Workstream 5: Render Performance Gate Without Waivers

### Required Result

Render gates must enforce the scoped product budgets directly.

### Required Code Results

- Remove p99 and slow-frame suppression based on `hostPacingOnly`.
- Keep host pacing as diagnostic classification only.
- Budgeted stress status fails when summary p90, p99, slow frames, P0, P1,
  resource failures, missing samples, missing pass timing, or target readiness
  exceed final limits.
- Scene-level and summary-level budgets are both enforced.
- Soak report must be fresh relative to source changes or commit provenance.
- Stress reports include pass timings, workload, resource churn, canvas
  evidence, and source/artifact commit IDs.

### Exit Criteria

- `rg "hostPacingOnly" scripts/ci/voplay_render_stress.mjs` appears only in
  diagnostics output or has no result.
- `voplay-render-stress-budgeted` fails on p90, p99, or slow-frame summary
  overruns.
- `voplay-render-soak-10m` fails without real 10-minute evidence.
- The final report cannot pass with p90 around `959ms` or p99 around `5814ms`.

## Workstream 6: Physics Backend Contract Closure

### Required Result

Vehicle control, force application, contact, surface, replay, reset, sleep, and
recovery must all route through explicit voplay contracts.

### Required Code Results

- The only control chain is:

```text
VehicleIntent
  -> KartDynamics.Step
  -> VehicleForceCommand
  -> PhysicsBackendApplyCommand
  -> PhysicsBackend.Apply
  -> PhysicsStep
  -> VehicleTelemetry
```

- `SetPose`, respawn, reset, recovery, and idle sleep use backend helper
  contracts.
- Direct body pose and velocity writes are limited to backend helper
  implementation.
- Surface material source order is collider/material/contact/raycast.
- Track-position material inference is a named compat path and fails
  industrial gates.
- `WheelContactSample` is produced from backend wheel, raycast, or contact
  data.
- `ContactEvent` industrial mode requires point, normal, relative velocity,
  normal impulse, tangent impulse, surface ID, and `fallback=false`.
- `PhysicsReplayTrace` records fixed dt, intent stream, surface/contact
  summary, pose hash, telemetry hash, and backend apply hash.

### Exit Criteria

- `rg "SurfaceMaterialAtTrackPosition" scene3d` shows compat-only paths that
  cannot run in industrial tests.
- `Vehicle.SetPose` no longer mutates body physics state directly.
- Industrial physics stress has fallback contacts `0`.
- Invalid samples, excessive velocity, excessive angular velocity, and long
  stuck counts are `0`.
- Replay drift remains at or below `0.01m`.

## Workstream 7: BlockKart Product Boundary

### Required Result

BlockKart must consume voplay engine APIs as a product layer.

### Required Code Results

- Primitive authoring APIs move to voplay generic authoring modules.
- BlockKart content files define track, props, visual recipes, race rules,
  tuning profile, quality profile, HUD layout, and diagnostics consumption.
- BlockKart stops owning reusable chunk authoring, collider authoring, surface
  material authoring, primitive batch authoring, and low-level water/boost/rail
  material tagging APIs.
- BlockKart HUD reads structured `RendererTelemetry`, `VehicleTelemetry`, and
  `PhysicsReplayTrace` summaries.
- BlockKart does not build renderer/fixed/worker JSON by hand.
- Kart visual state uses voplay visual telemetry rather than reading public
  steering and wheel-spin mutable fields directly.

### Exit Criteria

- `blockkart-product-boundary-strict` rejects primitive authoring owners in
  BlockKart.
- `performance_budget.vo` is a thin structured telemetry consumer.
- `world.vo` does not assemble bottom-layer engine facts.
- voplay API contains no BlockKart-specific track names, rules, or product
  identifiers.

## Workstream 8: Evidence Regeneration

### Required Result

All final evidence must be regenerated after the last source change.

### Required Commands

```sh
./d.py ci task voplay-render-core-unit
./d.py ci task voplay-batch-planner-unit
./d.py ci task voplay-render-structure-lint
./d.py ci task voplay-framegraph-unit
./d.py ci task voplay-render-stress-budgeted
./d.py ci task voplay-render-soak-10m
./d.py ci task voplay-vehicle-model-unit
./d.py ci task voplay-physics-backend-contract
./d.py ci task voplay-physics-industrial-stress
./d.py ci task blockkart-product-boundary-strict
./d.py ci task quickplay-source-audit
./d.py ci task quickplay-regenerate-check
./d.py ci task quickplay-validate
./d.py ci task blockkart-baseline
./d.py ci task blockkart-baseline-restart-50
./d.py ci task docs-lint
./d.py ci task eng-lint-tasks
git diff --check
git diff --cached --check
./d.py ci task voplay-industrial-readiness
```

### Exit Criteria

- Reports are generated after the final voplay, BlockKart, and Volang changes.
- Quickplay source audit proves artifact and source commit agreement.
- Provenance records voplay and BlockKart commits.
- Readiness report contains no stale report warning.

## Final Acceptance

The engine can be called industrial-grade for the scoped graphics and physics
foundation only when all items below pass:

- Source audit finds no unresolved bypass.
- Final readiness gate passes.
- `industrialReady: true`.
- `failures: []`.
- `sourceFacts` contains no required false fact.
- Frame pipeline stages are constructed in runtime.
- FrameGraph dispatch owns pass execution.
- Resource registry owns concrete target lifecycle.
- BatchPlanner owns visibility, LOD, upload, resident state, and submission.
- Render stress and soak enforce budgets with no host pacing waiver.
- Physics backend contract owns control, force, contact, surface, replay,
  reset, sleep, and recovery.
- BlockKart is limited to product content, rules, tuning, HUD, and read-only
  diagnostics consumption.
- Quickplay artifact and source commits match.

## Forbidden Completion Claims

These statements are invalid before final acceptance:

- `industrialReady: true` when source audit has failed facts.
- Final readiness based only on line counts.
- Final readiness based only on pass module existence.
- Final readiness based only on scene status fields.
- Final readiness while host pacing suppresses budget failures.
- Final readiness while stage structs are declared but unused.
- Final readiness while BlockKart still owns reusable engine authoring.

## Allowed Interim Statement

Use this wording before final acceptance:

```text
phase pass. industrialReady remains false. Source-audit failures are listed in
the readiness report and the source-audit completion plan.
```

## Allowed Final Statement

Use this wording only after final acceptance:

```text
voplay industrial render and physics foundation is complete for the scoped
WebGPU kart-racing baseline. The final source audit and readiness report agree:
industrialReady=true, failures=[], render budgets pass without waiver, physics
industrial stress passes without fallback contacts, and BlockKart consumes the
engine through product-level APIs.
```
