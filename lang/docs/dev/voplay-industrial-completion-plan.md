# voplay Industrial Completion Plan

Status: proposed
Date: 2026-07-04
Scope: completion plan for the voplay graphics, physics, resource,
diagnostics, performance, and BlockKart product-boundary work required before
the engine can be called industrial-grade for a kart-racing product.

## Document Role

This document is the completion contract for the voplay industrial effort. It
turns the current `industrialReady: false` report into concrete work packages,
owned outcomes, gates, and exit criteria.

The only final completion signal is:

```sh
./d.py ci task voplay-industrial-readiness
```

That task must pass and write a readiness report whose JSON contains:

```json
{
  "industrialReady": true
}
```

Every other milestone may report only phase completion. A phase pass means the
work package is ready for the next package. It does not authorize calling the
engine industrial-grade.

## Current Baseline

Source of truth:

- `/Users/macm1/code/github/volang/target/voplay-industrial-readiness/report.json`
- `/Users/macm1/code/github/volang/target/voplay-industrial-readiness/report.md`
- `/Users/macm1/code/github/volang/scripts/ci/voplay_industrial_readiness.mjs`

Current readiness:

- `industrialReady: false`
- failure count: 16
- voplay source commit: `d2db1bc12ec580625dac937626c9f608292ae32d`
- BlockKart artifact commit: `2610086b952078d3d6b32d9fb410e3c5815e721a`
- voplay artifact: `v0.1.34`

Current structural facts:

- `renderer.rs`: 3367 lines
- `submit_frame`: 2085 lines
- `pipeline3d.rs`: 1926 lines
- `submit_frame` still contains render pass bodies
- `submit_frame` still contains direct draw submission
- `submit_frame` still contains queue submission
- 10 minute render soak report has not been produced

Current hard failures:

- `renderer.submit_frame_size`
- `renderer.file_size`
- `renderer.no_begin_render_pass_in_submit_frame`
- `renderer.no_direct_draw_in_submit_frame`
- `renderer.no_queue_submit_body`
- `renderer.targets_owned_by_registry`
- `batch_plan.drives_submission`
- `pipeline3d.file_size`
- `pipeline3d.split_modules_exist`
- `physics.backend_adapter_only_wheel_control`
- `physics.replay_trace_contract`
- `blockkart.perf_json_encoder_migrated`
- `gate.render_stress_budgeted`
- `gate.render_soak_10m`
- `gate.physics_industrial_stress`
- `artifact.voplay_commit_recorded`

## Completion Policy

The completion policy is part of the engineering work.

Required behavior:

- `voplay-industrial-readiness-report` may pass with `industrialReady: false`.
- `voplay-industrial-readiness` must fail while any hard failure remains.
- All phase-level reports must use phase language only.
- A missing soak report keeps final readiness false.
- A missing provenance commit keeps final readiness false.
- Any render host pacing waiver keeps final readiness false.
- Any fake telemetry keeps final readiness false.
- Any industrial physics fallback contact keeps final readiness false.
- Any pass timing gap keeps final readiness false.
- Any product-layer render or physics workaround keeps final readiness false.

The readiness report is the scoreboard. The plan is complete when the scoreboard
has no failures.

## Target End State

voplay must own reusable engine capability. BlockKart must consume that
capability as a product.

Final engine state:

- Render runtime is controlled by FrameGraph, RenderBatchPlan, and
  RenderResourceRegistry.
- `Renderer.submit_frame` is a small orchestration entry.
- Each render pass is registered as a pass node and executed through
  `FrameGraphExecutor.execute_node`.
- Render targets are created, reused, recreated, aliased, and diagnosed through
  one resource registry.
- Batch planner decisions drive real draw submission.
- `pipeline3d` is split into independently testable responsibilities.
- Vehicle physics flows through `VehicleIntent`, `KartDynamics`,
  `PhysicsBackendApplyCommand`, backend apply, physics step, and telemetry.
- Reset, sleep, recovery, and pose helpers route through backend contract
  helpers.
- Contact and wheel samples come from backend data.
- Replay traces contain deterministic pose and telemetry hashes.
- BlockKart owns content, rules, tuning, HUD, and product diagnostics
  consumption.
- BlockKart does not build bottom-layer render facts or physics facts.
- quickplay provenance proves the browser artifact and source commits agree.

## Work Package R1: FrameGraph Runtime Ownership

### Outcome

The renderer main loop becomes an orchestration shell:

```text
decode_frame
build_scene_snapshot
build_batch_plan
build_frame_graph
execute_frame_graph
submit_backend
encode_telemetry
```

### Required Code Results

- `submit_frame` line count is at or below 220.
- `renderer.rs` line count is at or below 1600.
- `submit_frame` contains no `begin_render_pass`.
- `submit_frame` contains no direct draw calls.
- `submit_frame` contains no `queue.submit`.
- `submit_frame` contains no shadow, water, post, overlay, or backend-submit
  pass body.
- All runtime pass execution goes through `FrameGraphExecutor.execute_node`.
- Pass implementations live in dedicated modules:
  - `depth_pass`
  - `shadow_pass`
  - `main_opaque_pass`
  - `main_transparent_pass`
  - `water_pass`
  - `post_pass`
  - `overlay_pass`
  - `backend_submit_pass`
- Each pass exposes:
  - name
  - kind
  - reads
  - writes
  - transient writes
  - enabled predicate
  - execute function
  - workload diagnostics
  - failure diagnostics

### Required Resource Results

- `RenderResourceRegistry` owns:
  - surface target
  - depth target
  - shadow targets
  - main color target
  - post color target
  - receiver mask target
  - surface props target
  - water target
  - overlay target
  - capture/readback target
- Resize and recreate go through the registry.
- Missing reads are hard failures.
- Transient reuse is measured.
- Resource churn is attributed to create, reuse, recreate, alias, or failure.

### Gates

- `./d.py ci task voplay-render-core-unit`
- `./d.py ci task voplay-framegraph-unit`
- `./d.py ci task voplay-render-structure-lint`
- `./d.py ci task voplay-industrial-readiness-report`

### Exit Criteria

The readiness report clears:

- `renderer.submit_frame_size`
- `renderer.file_size`
- `renderer.no_begin_render_pass_in_submit_frame`
- `renderer.no_direct_draw_in_submit_frame`
- `renderer.no_queue_submit_body`
- `renderer.targets_owned_by_registry`

## Work Package R2: BatchPlanner And Pipeline Split

### Outcome

RenderBatchPlan becomes the submission model. Visibility, LOD, upload,
resident rebuild, and batch grouping are computed once and consumed by
submitters.

### Required Code Results

- `RenderBatchPlan` drives mesh, primitive, terrain, water, and decal
  submission.
- `submit_frame` and pass bodies do not independently decide visibility.
- `submit_frame` and pass bodies do not independently decide LOD.
- `submit_frame` and pass bodies do not independently decide dirty upload.
- `RenderWorldChunk` contains:
  - bounds
  - material group
  - LOD level
  - instance ranges
  - dirty ranges
  - resident state
  - last upload frame
- Culling has three mandatory layers:
  - frustum culling
  - distance culling
  - LOD selection
- Culling changes real draw workload and telemetry.

### Required Split Results

`pipeline3d.rs` is reduced to glue. Dedicated modules own stable
responsibilities:

- `shader_library`
- `pipeline_cache`
- `material_binder`
- `mesh_submitter`
- `terrain_submitter`
- `skinned_submitter`
- `primitive_submitter`
- `water_submitter`
- `decal_submitter`

Ownership rules:

- material binder does not own pipeline cache state
- pipeline cache does not own material authoring state
- submitters read batch plan inputs
- submitters do not perform independent visibility policy
- submitters can be tested in isolation

### Required Telemetry Results

Renderer telemetry explains:

- visible chunks
- culled chunks
- selected LODs
- dirty upload ranges
- resident rebuilds
- mesh batches
- primitive batches
- terrain batches
- water batches
- decal batches
- instance count
- triangle count
- upload bytes
- resource churn

### Gates

- `./d.py ci task voplay-batch-planner-unit`
- `./d.py ci task voplay-render-core-unit`
- `./d.py ci task voplay-render-stress-budgeted`
- `./d.py ci task voplay-industrial-readiness-report`

### Exit Criteria

The readiness report clears:

- `batch_plan.drives_submission`
- `pipeline3d.file_size`
- `pipeline3d.split_modules_exist`

## Work Package P1: Physics Backend Contract Closure

### Outcome

All vehicle control, reset, recovery, wheel command, contact, and replay facts
flow through the backend contract.

### Required Control Results

The only vehicle control chain is:

```text
VehicleIntent
KartDynamics.Step
VehicleForceCommand
PhysicsBackendApplyCommand
PhysicsBackend.Apply
PhysicsStep
VehicleTelemetry
```

Required source constraints:

- Product code cannot write raycast wheel control.
- Controller code cannot write raycast wheel control.
- Scene code cannot write raycast wheel control.
- `Vehicle.SetPose` cannot write raycast wheel control directly.
- reset, sleep, recovery, and pose changes use backend helpers.
- backend helpers record debug hashes.

### Required Contact Results

- `WheelContactSample` comes from backend wheel, raycast, or contact data.
- Wheel samples include hit point, normal, compression, load, forward slip,
  side slip, surface id, and grip.
- `ContactEvent` industrial mode requires point, normal, relative velocity,
  normal impulse, tangent impulse, surface id, and `fallback=false`.
- Compatibility fallback may exist only outside industrial gates.

### Required Replay Results

`PhysicsReplayTrace` records:

- fixed dt
- intent stream
- surface summary
- contact summary
- pose hash
- telemetry hash

Replay acceptance:

- replay drift at or below `0.01m`
- invalid samples equal `0`
- excessive velocity equal `0`
- excessive angular velocity equal `0`
- long stuck equal `0`
- industrial fallback contacts equal `0`

### Gates

- `./d.py ci task voplay-vehicle-model-unit`
- `./d.py ci task voplay-physics-backend-contract`
- `./d.py ci task voplay-physics-industrial-stress`
- `./d.py ci task voplay-industrial-readiness-report`

### Exit Criteria

The readiness report clears:

- `physics.backend_adapter_only_wheel_control`
- `physics.replay_trace_contract`
- `gate.physics_industrial_stress`

## Work Package B1: BlockKart Product Boundary Slimming

### Outcome

BlockKart becomes a product layer that consumes voplay engine facts. It does
not construct bottom-layer engine facts.

### Required Product Results

BlockKart may own:

- content
- race rules
- kart tuning profile
- quality profile selection
- HUD
- input mapping
- product diagnostics consumption
- product copy
- product flow

BlockKart must stop owning:

- raw shadow knobs
- raw post knobs
- resource pacing waivers
- raycast wheel access
- contact fallback logic
- product-side surface fallback inference
- bottom-layer performance fact encoding
- primitive authoring machinery that belongs in voplay

### Required voplay Authoring Results

voplay exposes generic authoring APIs for:

- chunk authoring
- collider authoring
- surface material authoring
- primitive batch authoring
- water material tags
- boost material tags
- rail material tags
- wall material tags
- content diagnostics

### Required Diagnostics Results

BlockKart diagnostics read only:

- `RendererTelemetry`
- `VehicleTelemetry`
- `PhysicsReplayTrace`
- product-level race state

Structured telemetry encoding belongs in voplay. BlockKart can add product
fields around that payload, but it cannot hand-build engine performance JSON.

### Gates

- `./d.py ci task blockkart-product-boundary-strict`
- `./d.py ci task blockkart-baseline`
- `./d.py ci task blockkart-baseline-restart-50`
- `./d.py ci task voplay-industrial-readiness-report`

### Exit Criteria

The readiness report clears:

- `blockkart.perf_json_encoder_migrated`

## Work Package E1: Stress Coverage And Provenance Closure

### Outcome

The evidence layer proves the current source, current generated quickplay
artifact, and current browser-running package agree.

### Required Render Stress Results

`voplay-render-stress-budgeted` covers and budgets:

- BlockKart baseline
- primitive 10k
- chunked world drive
- water/wake
- shadow/post matrix
- resize/recreate
- resource churn
- restart 50

Every scene must report:

- real canvas capture
- real perf samples
- pass timing
- workload
- resource churn
- ready target state
- missing read count
- p90 frame time
- p99 frame time
- slow frame count

### Required Render Soak Results

`voplay-render-soak-10m` must run and produce a report:

- 1280x720 medium
- 10 minutes
- p90 at or below `16.7ms`
- p99 at or below `22.5ms`
- slow frames at or below `8`
- resource failures equal `0`

### Required Physics Stress Results

`voplay-physics-industrial-stress` covers:

- skidpad
- slalom
- drift turbo
- boost pad
- offroad transition
- jump landing
- wall impact
- rail ride
- wall ride
- water skim
- 24 vehicle soak
- deterministic replay

### Required Provenance Results

quickplay provenance records:

- volang commit
- BlockKart source commit
- BlockKart artifact commit
- voplay source commit
- voplay artifact version
- voplay artifact source commit
- dependency source digest
- output digest
- generator command
- dirty state

The generated quickplay package must match the checked-in package.

### Gates

- `./d.py ci task voplay-render-stress-budgeted`
- `./d.py ci task voplay-render-soak-10m`
- `./d.py ci task voplay-physics-industrial-stress`
- `./d.py ci task quickplay-source-audit`
- `./d.py ci task quickplay-regenerate-check`
- `./d.py ci task quickplay-validate`
- `./d.py ci task voplay-industrial-readiness-report`

### Exit Criteria

The readiness report clears:

- `gate.render_stress_budgeted`
- `gate.render_soak_10m`
- `gate.physics_industrial_stress`
- `artifact.voplay_commit_recorded`

## Work Package H1: Performance And Code Quality Hardening

### Outcome

The final engine state remains maintainable after the architecture moves.
Performance is measured by code-owned telemetry, not by manual browser
inspection.

### Required Results

- Every pass records CPU timing and workload.
- Slow-frame attribution includes decode, snapshot, batch plan, graph build,
  each pass, queue submit, present, resource churn, and host pacing.
- Any missing timing field fails a core render gate.
- Any resource failure fails a render gate.
- Any NaN or invalid physics sample fails a physics gate.
- Any fallback contact in industrial stress fails a physics gate.
- Each extracted render module has focused unit coverage.
- Each physics backend helper has focused unit coverage.
- Docs describe shipped state and current gates.

### Gates

- `./d.py ci task voplay-render-core-unit`
- `./d.py ci task voplay-framegraph-unit`
- `./d.py ci task voplay-batch-planner-unit`
- `./d.py ci task voplay-vehicle-model-unit`
- `./d.py ci task voplay-physics-backend-contract`
- `./d.py ci task docs-lint`
- `./d.py ci task eng-lint-tasks`
- `git diff --check`
- `git diff --cached --check`

## Final Gate

Final completion requires all of these commands to pass on the same source
state:

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

The final command must produce:

```json
{
  "industrialReady": true,
  "failures": []
}
```

## Work Sequencing

The work can span many commits and many review cycles. The sequence below is
optimized to reduce false progress:

1. Finish R1, because pass ownership and resource ownership are blocking
   structural failures.
2. Finish R2, because batch planner and pipeline split depend on clean pass
   boundaries.
3. Finish P1, because physics contract closure is parallelizable after render
   pass extraction starts.
4. Finish B1, because BlockKart slimming should consume stable voplay APIs.
5. Finish E1, because soak, stress, and provenance should validate final
   source and artifacts.
6. Finish H1 continuously, because quality hardening must accompany every
   package.

No sequence step may delete a failing readiness check to make progress look
better. The readiness check may be updated only when the target contract is
more accurate or stricter.

## Commit Discipline

Recommended commit categories:

- readiness contract updates
- mechanical module extraction
- render pass behavior migration
- resource registry migration
- batch planner submission migration
- pipeline split
- physics backend helper migration
- replay/contact telemetry migration
- BlockKart product-boundary cleanup
- stress scene additions
- provenance generator updates
- docs and final evidence

Each commit should keep narrow gates passing. Large mechanical moves should
avoid behavior changes. Behavior changes should carry tests or stress evidence.

## Completion Checklist

Use this checklist before any final claim:

- `submit_frame` is an orchestration entry.
- Runtime pass execution uses `execute_node`.
- Render pass bodies live in pass modules.
- Resource registry owns render target lifecycle.
- Batch planner drives real draw submission.
- `pipeline3d` responsibilities are split and testable.
- Vehicle wheel writes are backend-owned.
- Reset, pose, sleep, and recovery use backend helpers.
- Replay trace includes pose and telemetry hashes.
- Industrial physics fallback contacts equal `0`.
- BlockKart performance JSON uses voplay structured telemetry encoder.
- Missing render stress scenes are implemented.
- 10 minute render soak report exists and passes.
- `boost-pad` physics scenario exists and passes.
- quickplay provenance records voplay artifact source commit.
- generated quickplay package matches checked-in artifact.
- `voplay-industrial-readiness` passes.
- readiness report says `industrialReady: true`.

## Completion Statement Template

Use this wording only after the Final Gate passes:

```text
voplay industrial foundation complete for the scoped WebGPU kart-racing
graphics and physics baseline. The readiness report shows
industrialReady=true on commit <commit>, with no failures.
```

Before that point, use this wording:

```text
phase pass: <phase name>. industrialReady remains false until
voplay-industrial-readiness passes.
```
