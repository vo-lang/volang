# voplay Industrial First-Principles Completion Plan

Status: superseded
Superseded-By: lang/docs/dev/voplay-code-engineering-quality-plan.md
Superseded-Date: 2026-07-05
Date: 2026-07-04
Scope: corrected completion plan for making voplay's WebGPU render runtime and
kart physics foundation industrial-grade for the scoped BlockKart validation
product.

## Role

This historical document superseded the optimistic interpretation of
`lang/docs/dev/voplay-industrial-completion-plan.md`. The active execution plan
is now `lang/docs/dev/voplay-code-engineering-quality-plan.md`.

The earlier readiness gate passed after `submit_frame` became a three-line
proxy, but first-principles inspection found that the real render runtime body
had moved to `rust/src/renderer/frame_submit.rs`. That file still owns pass
bodies, direct draw calls, queue submit, and target access. The gate has been
tightened to scan the real runtime path.

Completion remains strict:

```sh
./d.py ci task voplay-industrial-readiness
```

The only acceptable final result is:

```json
{
  "industrialReady": true,
  "failures": []
}
```

Before that exact result, every report must say `industrialReady: false`.

## Current First-Principles Baseline

Current source-of-truth report:

- `/Users/macm1/code/github/volang/target/voplay-industrial-readiness/report.json`
- `/Users/macm1/code/github/volang/target/voplay-industrial-readiness/report.md`

Current readiness:

- `industrialReady: false`
- failure count: 7

Current source facts:

- `renderer.rs`: 1284 lines
- `renderer/frame_submit.rs`: 2090 lines
- `submit_frame`: 3 lines
- `pipeline3d.rs`: 373 lines
- `runtimeHasRenderPassBody: true`
- `runtimeHasDirectDraw: true`
- `runtimeHasQueueSubmit: true`
- `rendererStillOwnsTargets: true`
- render soak report exists and passes
- render stress report exists and passes
- physics industrial stress report exists and passes
- quickplay provenance currently records matching voplay and BlockKart commits

Current hard failures:

- `renderer.frame_submit_size`
- `renderer.no_begin_render_pass_in_runtime_glue`
- `renderer.no_direct_draw_in_runtime_glue`
- `renderer.no_queue_submit_in_runtime_glue`
- `framegraph.pass_modules_exist`
- `renderer.targets_owned_by_registry`
- `batch_plan.drives_submission`

## First-Principles Definition Of Done

The engine is ready only when the runtime architecture matches these facts:

- FrameGraph owns pass scheduling.
- Pass executor modules own pass implementation.
- Resource registry owns render target lifecycle.
- BatchPlan owns render submission selection.
- `frame_submit.rs` performs frame orchestration and data plumbing only.
- No runtime glue file contains pass bodies.
- No runtime glue file directly starts WGPU render passes.
- No runtime glue file directly submits WGPU queues.
- No runtime glue file calls low-level model or primitive draw entrypoints.
- Every stress and soak report is regenerated after the architectural move.

The point is not reducing line counts for appearance. The point is ownership:
each subsystem must own the behavior it is responsible for, and the final gate
must prove that ownership in code.

## Target Runtime Shape

Final render call chain:

```text
Renderer.submit_frame
  -> FrameSubmitOrchestrator::run
      -> decode_frame
      -> build_scene_snapshot
      -> build_batch_plan
      -> build_frame_graph
      -> execute_frame_graph
          -> DepthPassExecutor
          -> ShadowPassExecutor
          -> MainOpaquePassExecutor
          -> MainTransparentPassExecutor
          -> WaterPassExecutor
          -> PostPassExecutor
          -> OverlayPassExecutor
          -> BackendSubmitExecutor
      -> encode_telemetry
```

Final data ownership:

```text
RenderSceneSnapshot
  immutable per-frame scene facts

RenderBatchPlan
  visibility, LOD, upload, resident rebuild, and submission batches

RenderResourceRegistry
  concrete target allocation, resize, recreate, alias, reuse, and readiness

RenderPassNode
  pass identity, reads, writes, enabled state, executor, workload, diagnostics
```

## Workstream A: Gate Integrity

### Required Result

The readiness gate must catch architectural shortcuts before they become
claims. The current tightening is the baseline; further implementation must not
weaken it.

### Required Checks

- Scan `renderer.rs` and `renderer/frame_submit.rs` as runtime glue.
- Fail if runtime glue contains `begin_render_pass`.
- Fail if runtime glue contains `draw_models`.
- Fail if runtime glue contains `draw_main_and_water`.
- Fail if runtime glue contains `queue.submit`.
- Fail if pass executor modules are missing.
- Fail if `RendererTargetRegistry` or `self.targets` remains the concrete
  render target owner.
- Fail if BatchPlan is only telemetry.

### Exit Criteria

- `./d.py ci task voplay-industrial-readiness-report` reports the current
  source facts accurately.
- Adding a pass body back to runtime glue fails the readiness task.
- Adding a direct draw call back to runtime glue fails the readiness task.
- Adding queue submit back to runtime glue fails the readiness task.

## Workstream B: Pass Executor Modules

### Required Result

Pass bodies move from `renderer/frame_submit.rs` into dedicated executor
modules. Runtime glue registers nodes and invokes executors; it does not
construct WGPU pass descriptors or issue draw commands.

### Required Modules

- `rust/src/renderer/depth_pass.rs`
- `rust/src/renderer/shadow_pass.rs`
- `rust/src/renderer/main_opaque_pass.rs`
- `rust/src/renderer/main_transparent_pass.rs`
- `rust/src/renderer/water_pass.rs`
- `rust/src/renderer/post_pass.rs`
- `rust/src/renderer/overlay_pass.rs`
- `rust/src/renderer/backend_submit_pass.rs`

### Required Executor API

Each module exposes an executor with this shape:

```rust
pub(crate) struct PassExecutor;

impl PassExecutor {
    pub(crate) fn node(...) -> RenderPassNode;
    pub(crate) fn execute(ctx: &mut RenderPassContext<'_>) -> Result<RenderPassResult, String>;
    pub(crate) fn workload(ctx: &RenderPassContext<'_>) -> RenderPassWorkload;
}
```

The exact Rust types may differ if local ergonomics require it, but the
responsibility boundary must remain intact:

- pass modules may call `begin_render_pass`
- pass modules may call low-level draw entrypoints
- backend submit module may call `queue.submit`
- runtime glue may not do those things

### Migration Order

1. Extract BackendSubmit first. It has a small behavioral surface and proves
   queue submit ownership.
2. Extract Overlay. It has simple color target use and 2D draw loops.
3. Extract Post. It proves post target and bind group handoff.
4. Extract MainOpaque. It is the largest pass and should move only after
   context types are stable.
5. Extract Water as a true pass rather than timing around work performed in
   MainOpaque.
6. Extract MainTransparent as a real placeholder with explicit empty workload
   until transparent content exists.
7. Extract Depth and Shadow, preserving existing diagnostics.

### Exit Criteria

- `frame_submit.rs` contains no `begin_render_pass`.
- `frame_submit.rs` contains no `.draw_models(`.
- `frame_submit.rs` contains no `draw_main_and_water(`.
- `frame_submit.rs` contains no `queue.submit`.
- pass modules exist.
- pass modules own their pass bodies.
- pass timing and workload telemetry remain present.
- `renderer.frame_submit_size` clears.
- `renderer.no_begin_render_pass_in_runtime_glue` clears.
- `renderer.no_direct_draw_in_runtime_glue` clears.
- `renderer.no_queue_submit_in_runtime_glue` clears.
- `framegraph.pass_modules_exist` clears.

## Workstream C: Render Resource Registry As Concrete Owner

### Required Result

`RenderResourceRegistry` becomes the concrete owner of render target lifecycle,
not only a diagnostics registry.

### Required Design

Introduce a concrete target store behind the registry:

```text
RenderResourceRegistry
  -> RenderTargetStore
      -> surface frame view
      -> depth view
      -> MSAA color view
      -> post color view
      -> receiver mask view
      -> surface props view
      -> shadow views
      -> overlay/capture/readback views
```

The registry must own:

- target creation
- target lookup
- target readiness
- resize generation
- recreate paths
- transient reuse
- alias reuse
- missing read failures
- resource churn counters

### Required Renderer Changes

- Remove `targets: RendererTargetRegistry` from `Renderer`.
- Remove direct writes to `self.targets.depth_view`.
- Remove direct writes to `self.targets.post_color_view`.
- Remove direct writes to `self.targets.receiver_mask_view`.
- Remove direct writes to `self.targets.surface_props_view`.
- Move resize target recreation into registry methods.
- Pass executors request target views through pass context.
- Missing required target returns a pass failure with resource name and pass
  name.

### Required Tests

Add or update tests for:

- target create
- target reuse
- target recreate after resize
- transient reuse
- alias reuse
- missing read failure
- resize generation propagation
- resource churn attribution

### Exit Criteria

- `renderer.targets_owned_by_registry` clears.
- resize/recreate reports still pass.
- post and main pass target access comes through pass context.
- `voplay-framegraph-unit` passes.
- `voplay-render-core-unit` passes.

## Workstream D: BatchPlan As Submission Owner

### Required Result

BatchPlan drives what gets submitted. It must not be only a telemetry object
computed beside independent draw lists.

### Required Design

Extend `RenderBatchPlan` to contain submission-ready groups:

```text
RenderBatchPlan
  model_batches
  primitive_batches
  terrain_batches
  water_batches
  decal_batches
  transparent_batches
  visible_chunks
  selected_lods
  dirty_uploads
  resident_rebuilds
  workload
```

Batches must carry stable handles or indices into immutable frame data. Pass
executors consume the plan.

### Required Renderer Changes

- MainOpaque pass reads model and primitive opaque batches from BatchPlan.
- Water pass reads water batches from BatchPlan.
- Shadow and depth passes read shadow/depth eligible batches from BatchPlan or
  a plan-derived pass view.
- Post pass reads decal batches or decal bindings derived from BatchPlan.
- Runtime glue stops passing independent `model_draws`, `primitive_draws`, and
  `primitive_chunks` into pass bodies as primary submission state.
- Pipeline submitters receive plan batches.

### Required Tests

Add or update tests for:

- mesh batch selection affects draw workload
- primitive batch selection affects draw workload
- water batch selection affects water pass workload
- LOD selection changes selected batch complexity
- dirty upload ranges are represented in plan
- resident rebuild count is represented in plan
- pass executor receives only plan-derived batches

### Exit Criteria

- `batch_plan.drives_submission` clears.
- render stress scenes still pass.
- workload telemetry still matches submitted work.
- no direct draw call remains in runtime glue.

## Workstream E: FrameSubmit Orchestrator Reduction

### Required Result

`renderer/frame_submit.rs` becomes a readable orchestration module at or below
700 lines.

### Allowed Responsibilities

- decode draw command stream
- create immutable frame snapshot
- call BatchPlanner
- build FrameGraph
- build pass contexts
- invoke FrameGraph executor
- collect pass results
- encode telemetry
- update last report fields

### Forbidden Responsibilities

- WGPU render pass descriptor construction
- WGPU draw calls
- WGPU queue submit
- render target creation or resize
- independent visibility policy
- independent LOD policy
- pass-local bind group construction except through context builders

### Exit Criteria

- `renderer.frame_submit_size` clears.
- code review can trace a frame without reading pass internals.
- adding a new pass requires registering a pass node and module, not adding a
  hand-written branch in runtime glue.

## Workstream F: Revalidate Existing Physics And Product Boundaries

### Required Result

The current physics/product gates may remain green, but they must be rerun
after render architecture moves because quickplay artifacts and reports become
stale after source changes.

### Required Checks

- `voplay-vehicle-model-unit`
- `voplay-physics-backend-contract`
- `voplay-physics-industrial-stress`
- `blockkart-product-boundary-strict`
- `quickplay-source-audit`
- `quickplay-regenerate-check`
- `quickplay-validate`
- `blockkart-baseline`
- `blockkart-baseline-restart-50`

### Exit Criteria

- physics industrial stress still has fallback contacts equal `0`
- replay drift remains at or below `0.01m`
- BlockKart remains a product layer
- quickplay provenance records matching voplay and BlockKart source commits

## Workstream G: Full Evidence Regeneration

### Required Result

All performance and provenance evidence must be regenerated after the render
runtime ownership changes. Reports generated before the architecture move are
not enough for final completion.

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

- `voplay-industrial-readiness` passes.
- readiness report has `industrialReady: true`.
- readiness report has `failures: []`.
- report source facts show:
  - `runtimeHasRenderPassBody: false`
  - `runtimeHasDirectDraw: false`
  - `runtimeHasQueueSubmit: false`
  - `rendererStillOwnsTargets: false`
  - `frameSubmitLines <= 700`

## Non-Negotiable Engineering Rules

- Do not delete readiness checks to make the gate pass.
- Do not narrow scans back to `renderer.rs` only.
- Do not move pass bodies into a differently named runtime glue file.
- Do not hide queue submit behind a wrapper still called from runtime glue
  unless the wrapper is the backend submit pass executor.
- Do not keep BatchPlan as telemetry while independent draw lists drive passes.
- Do not rely on old stress reports after source changes.
- Do not claim completion while `voplay-industrial-readiness` fails.

## Final Completion Statement

Use this statement only after the final gate passes:

```text
voplay industrial render and physics foundation is complete for the scoped
WebGPU kart-racing baseline. The readiness report shows industrialReady=true,
failures=[], and the runtime source facts prove pass bodies, direct draw,
queue submit, target lifecycle, and batch submission ownership are in their
proper engine modules.
```

Until then, use this statement:

```text
phase pass. industrialReady remains false. Remaining failures are listed in
target/voplay-industrial-readiness/report.md.
```
