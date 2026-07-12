# BlockKart / voplay Render And Physics Refactor Plan

Status: proposed
Date: 2026-07-02
Scope: voplay render and physics architecture required before BlockKart can
move from a demo-grade kart sample toward a product-grade racing game.

## Document Role

This plan is intentionally narrower than the whole BlockKart product
foundation. It focuses only on graphics and physics. It is not a feature
wishlist. It defines the architecture, code quality, visual quality, and
runtime performance outcomes that must exist before larger racing content is
safe to build.

BlockKart is the acceptance application. voplay must gain reusable render and
physics engine capabilities, but voplay must not become a BlockKart-specific
engine.

## Current Assessment

voplay has real engine foundations:

- WGPU renderer with 2D, sprite, 3D mesh, primitive, depth, shadow, skybox, and
  post-process pipelines.
- Retained 3D scene updates through `Scene.Draw`, `flushRenderScene`, and
  `DrawScene3D`.
- Primitive layers with revisions, static chunks, cached visibility, and
  chunk-level uploads.
- Rapier3D-backed physics with rigid bodies, colliders, heightfields, trimesh
  collision, ray casts, AABB queries, contact pairs, and raycast vehicles.
- Kart controller support for throttle, steering, drift charge, boost,
  off-road response, and recovery.
- Performance diagnostics and baseline reports that can already attribute
  several startup and slow-frame causes.

Those foundations are enough for a lightweight single-player 3D kart demo.
They are not yet enough for a product-grade racing game. The main risk is not
one missing shader or one missing vehicle parameter. The risk is that render
and physics code still mixes orchestration, runtime state, backend calls,
product tuning, diagnostics, and content assumptions in ways that make future
features expensive and fragile.

Current structural pressure points:

- `voplay/rust/src/renderer.rs` is a large orchestration file that owns device,
  surface, render targets, pipeline setup, pass sequencing, resource managers,
  performance collection, and slow-submit logging.
- `voplay/rust/src/pipeline3d.rs` combines static meshes, terrain, skinned
  meshes, material binding, instancing, texture bind groups, and shader
  pipeline construction.
- `voplay/scene3d/scene.vo` owns scene entities, render dirty tracking, physics
  world ownership, vehicle synchronization, audio listener updates, primitive
  layers, and cleanup.
- `voplay/scene3d/vehicle.vo` exposes a thin raycast-vehicle wrapper whose
  control path writes steering, engine force, and brake directly to wheels.
- `voplay/scene3d/kart_controller.vo` mixes generic kart-control ideas with
  specific drift, boost, off-road, and recovery behavior.
- `BlockKart/world.vo` and `BlockKart/primitive_world.vo` still carry too much
  game-specific world construction, visual construction, physics construction,
  tuning, diagnostics, and product behavior in single files.

Current runtime evidence:

- The checked-in BlockKart quickplay baseline can pass a 60 FPS target in a
  light scene, but the product uses conservative render settings.
- BlockKart currently targets 1280x720 at 60 FPS and disables bloom, FXAA, and
  contact AO in its performance budget.
- The default baseline can pass with bounded resources, but long restart soak
  has exposed reliability and frame-pacing risk.
- Current scene complexity is still small: about 68 active entities and about
  60 physics bodies in the default quickplay capture. That does not prove a
  larger racing product will remain smooth.

## Product Target

The refactor succeeds when BlockKart can use voplay as a generic graphics and
physics engine layer with these properties:

1. Render code has a clear architecture:
   game scene data, render-scene data, frame snapshots, frame graph passes,
   resource caches, backend submission, and diagnostics are separate concepts.

2. Physics code has a clear architecture:
   player intent, kart dynamics, physics backend, contact events, surface
   materials, and vehicle telemetry are separate concepts.

3. Visual quality can improve without destabilizing performance:
   quality presets, post effects, lighting, shadows, materials, primitives,
   and terrain are budgeted and diagnosable.

4. Driving quality can improve without hiding game rules inside voplay:
   BlockKart owns final handling values and product rules, while voplay owns
   reusable kart dynamics, telemetry, contacts, and physics backend contracts.

5. Performance is a contract:
   every added render or physics capability must have a stress scene, a budget,
   and regression gates.

## Non-Goals

Do not use this refactor to build:

- online multiplayer
- matchmaking
- battle mode
- item economy
- large AI rosters
- open-world streaming
- full character or vehicle progression
- BlockKart-specific renderer branches inside voplay
- BlockKart-specific vehicle rules inside voplay

Those are future product features. This plan builds the render and physics
foundation needed to support them later.

## Target Architecture

### Render Architecture

Target layers:

1. Scene authoring layer.
   Game-side objects, transforms, materials, lights, cameras, decals, emitters,
   terrain, and primitive layers. This layer is ergonomic for games.

2. Render scene layer.
   Stable render handles, immutable resource identity, material revisions,
   mesh revisions, visibility state, primitive chunk state, and dirty ranges.
   This layer does not own game rules.

3. Render frame snapshot.
   A frozen frame description containing one or more views, visible object
   lists, pass inputs, quality decisions, debug flags, and frame diagnostics.
   This enables screenshot capture, replay of render submissions, and future
   multi-view rendering.

4. Frame graph.
   Explicit passes for shadow, depth, main color, receiver mask, surface
   properties, post, overlay, debug, capture, and future extra views. Passes
   declare resources and dependencies instead of living only as a fixed
   sequence in `renderer.rs`.

5. Backend submission.
   WGPU resource binding, buffer uploads, draw dispatch, queue submit, present,
   device-loss handling, and backend-specific performance metrics.

6. Quality policy.
   Data-driven presets and adaptive decisions for resolution scale, shadow
   quality, contact AO, bloom, FXAA, primitive density, terrain detail,
   particle density, and debug overhead.

Required render refactor outcomes:

- `renderer.rs` becomes an orchestrator, not the home of every render concern.
- Pass setup and pass execution move into explicit pass modules.
- `pipeline3d.rs` separates mesh, terrain, skinned, material binding, and
  texture-binding concerns.
- Scene-side render flushing outputs render-scene changes, not ad hoc backend
  commands mixed with culling and diagnostics.
- Render diagnostics survive the refactor and become more structured.
- No new voplay render API mentions BlockKart.

### Physics Architecture

Target layers:

1. Intent layer.
   `VehicleIntent` or equivalent data for throttle, brake, steering, drift,
   boost request, air control, reset request, and assist hints. It contains no
   direct wheel force values.

2. Kart dynamics layer.
   Generic racing vehicle behavior: steering response, grip curves, drift
   state, slip recovery, boost impulse, off-road response, air state, landing
   state, wall impact response, and recovery assist.

3. Physics backend layer.
   Rapier-backed rigid bodies, colliders, ray casts, vehicles, queries, and
   step execution. The backend remains replaceable through an interface.

4. Contact event layer.
   Structured collision events with body IDs, normal, contact point, relative
   velocity, impulse or estimated severity, surface material, and duration.

5. Surface material layer.
   Data-driven road, off-road, boost, ice, rough, wall, jump, rail, and other
   future surface responses. voplay owns the generic material model; BlockKart
   owns content values and game interpretation.

6. Vehicle telemetry layer.
   Stable outputs for speed, grounded state, wheel contacts, normal load,
   suspension compression, longitudinal and lateral slip, active surface,
   drift state, boost state, recovery state, collision state, and NaN/limit
   safety flags.

Required physics refactor outcomes:

- `Vehicle` stops being only a thin wheel-control wrapper.
- `KartController` stops mixing generic dynamics with product rules.
- Rapier remains an implementation detail behind the backend contract.
- Contact data is rich enough for handling, audio, visual effects, debugging,
  and future AI.
- Physics tests can run without WebGPU.
- No new voplay physics API mentions BlockKart.

## Development Route

### Phase 0: Measurement And Contract Baseline

Goal: lock down current behavior before moving code.

Deliverables:

- Render architecture map for current files and target module boundaries.
- Physics architecture map for current files and target module boundaries.
- Dedicated render stress scenes:
  - baseline scene
  - many static meshes
  - many primitive chunks
  - terrain plus decals
  - skinned model scene
  - post-process quality scene
- Dedicated physics stress scenes:
  - single kart skidpad
  - slalom
  - jump and landing
  - wall impact
  - surface transition
  - many static colliders
  - multi-vehicle soak, initially with simple scripted intents
- Golden screenshots for render scenes.
- Numeric telemetry baselines for physics scenes.
- Perf budget schema that records p50, p90, p99, max, slow frames, render
  phase cost, physics step cost, upload bytes, draw counts, body counts, and
  contact counts.

Acceptance:

- Existing BlockKart default, start-race, storage-reload, restart-2, and
  restart-10 baselines still pass.
- The new stress scenes run from official tasks, not manual browser steps.
- Each stress scene emits machine-readable JSON.
- Golden render scenes have nonblank canvas checks and screenshot diff
  thresholds.
- Physics scenes report deterministic input, final pose, final velocity,
  contact count, wheel contact count, slip range, and any NaN or limit failure.
- No production behavior changes are hidden inside this phase.

### Phase 1: Render Module Split Without Visual Change

Goal: improve code structure while keeping output equivalent.

Deliverables:

- Split `renderer.rs` into focused modules for:
  - render targets and resize
  - frame decoding
  - pass orchestration
  - shadow pass
  - main pass
  - post pass
  - overlay pass
  - perf collection
  - device and surface failure handling
- Split `pipeline3d.rs` responsibilities into mesh, terrain, skinned, material,
  and bind-group/cache modules.
- Keep public voplay APIs stable unless the old API directly blocks the
  architecture.
- Add source-level contracts or focused tests that keep pass ownership from
  collapsing back into one giant file.

Acceptance:

- Golden screenshots are unchanged within threshold.
- BlockKart baseline remains green.
- Render stress scenes show no more than 5 percent regression in p90 frame time
  on the reference runner.
- `renderer.rs` no longer owns pass-specific draw logic directly.
- `pipeline3d.rs` no longer owns terrain and skinned pipeline details directly.
- Render diagnostics keep the same fields or provide a documented migration.

### Phase 2: Render Scene Snapshot And Frame Graph

Goal: make rendering extensible before adding bigger effects.

Deliverables:

- `RenderScene` change set that records object, material, mesh, primitive, and
  visibility revisions.
- `RenderFrame` snapshot with explicit view list and pass inputs.
- First `FrameGraph` implementation for the existing shadow, main, post, and
  overlay sequence.
- Pass resource declarations for color, depth, receiver mask, surface props,
  shadow map, post targets, and capture targets.
- Frame capture mode that can dump a render-frame summary without replaying
  game simulation.
- Multi-view-ready data structures, even if only one view is enabled at first.

Acceptance:

- Existing single-view rendering is visually equivalent.
- Adding a second disabled view path does not change the single-view output.
- Frame graph reports pass count, resource count, and per-pass timings.
- Slow-frame attribution names the responsible pass when possible.
- Render-frame snapshot contains no BlockKart-specific fields.
- Screenshot capture uses the render-frame boundary rather than game-specific
  state scraping.

### Phase 3: Render Quality, Materials, And Effects Budget

Goal: improve visual quality without losing performance control.

Deliverables:

- Data-driven quality profiles for low, medium, high, and cinematic.
- Material profile cleanup:
  - normalized material inputs
  - texture fallback policy
  - sampler policy
  - terrain material policy
  - skinned material policy
- Contact AO, bloom, FXAA, shadow, terrain, decal, primitive, and particle
  budgets exposed through one quality policy.
- Particle and transient-effect path suitable for boosts, dust, sparks, hits,
  and landing effects.
- Visual debug modes for material, overdraw proxy, shadow cascades, primitive
  chunks, LOD, and surface materials.

Acceptance:

- BlockKart can enable a visible medium-quality profile without exceeding the
  60 FPS baseline budget in the default scene.
- Low quality passes on a weaker budget with shadows and expensive post effects
  reduced or disabled by policy, not product-specific conditionals.
- High quality shows a measurable visual difference in golden screenshots.
- Material fallback never produces black or missing-texture output unless the
  asset is intentionally invalid.
- Render stress scenes have budgeted p90 and p99 values per quality profile.
- No quality decision is hard-coded inside BlockKart.

### Phase 4: Physics Contract And Telemetry Split

Goal: make vehicle behavior tunable and testable before changing handling.

Deliverables:

- `VehicleIntent` data type.
- `KartDynamics` data type or service that converts intent and current state
  into backend controls.
- `VehicleTelemetry` data type with wheel, contact, surface, boost, drift,
  recovery, and safety information.
- Rich `ContactEvent` data type and backend support.
- Surface material response data model.
- Headless physics scenario runner.
- JSON output for all physics scenarios.

Acceptance:

- BlockKart uses intent and telemetry instead of reading or writing low-level
  wheel controls directly.
- Existing handling is preserved within agreed tolerances before new tuning is
  applied.
- Physics tests run without WebGPU.
- Contact events include enough information to classify wall hit, ground hit,
  vehicle hit, and trigger contact.
- Telemetry contains no NaN, infinity, or invalid quaternion values in stress
  scenes.
- voplay APIs remain generic and do not include BlockKart names.

### Phase 5: Kart Dynamics And Collision Quality

Goal: move from "the kart moves" to "the kart is tunable and repeatable".

Deliverables:

- Steering response curves.
- Grip and slip curves.
- Drift entry, sustain, and release states.
- Boost impulse and acceleration model.
- Off-road and surface transition model.
- Wall impact and recovery model.
- Jump, landing, and air-control hooks.
- Track collision validation for road, shoulder, wall, boost, and off-road
  surfaces.
- Handling preset format owned by BlockKart but interpreted through generic
  voplay dynamics.

Acceptance:

- Skidpad scenario reports stable turning radius within 5 percent between
  repeated same-runtime runs.
- Slalom scenario reports no control latency spikes above the configured
  threshold.
- Jump scenario lands without NaN, excessive bounce, or unrecoverable flip.
- Wall-impact scenario classifies the hit and produces bounded velocity loss.
- Surface-transition scenario reports expected speed and grip changes.
- Replay of the same scripted intent over 60 seconds stays within the agreed
  same-runtime position and velocity drift thresholds.
- BlockKart tuning changes no longer require editing voplay internals.

### Phase 6: BlockKart Integration And Demo Debt Removal

Goal: make BlockKart consume the new engine shape and remove local demo hacks.

Deliverables:

- BlockKart render settings moved to quality profile selection, not raw engine
  knobs.
- BlockKart vehicle handling moved to product tuning data using voplay
  dynamics.
- Primitive track collision replaced or wrapped by validated track collision
  generation where possible.
- World construction split so visual scene, physics scene, vehicle state, and
  gameplay state have separate ownership.
- BlockKart diagnostics updated to report render quality, physics telemetry,
  surface material, contact events, and stress-scene compatibility.

Acceptance:

- BlockKart `world.vo` and `primitive_world.vo` no longer need to own engine
  architecture responsibilities.
- Restart rebuilds product state while reusing immutable render data as
  intended.
- Default, start-race, storage-reload, restart-10, and restart-50 pass with
  P0/P1 equal to zero.
- Default scene remains within the 60 FPS budget with the selected production
  quality profile.
- Handling telemetry explains every major state shown in the HUD or baseline
  report.
- Any remaining BlockKart-specific workaround is documented with an owner,
  deadline, and deletion condition.

## Code Quality Acceptance

The refactor is not complete unless the code shape improves. A feature that
only adds capability while making the architecture harder to change does not
pass.

Required code quality outcomes:

- Render orchestration, pass execution, resource lifetime, pipeline creation,
  and diagnostics live in separate modules.
- Physics intent, dynamics, backend integration, contact events, and telemetry
  live in separate modules.
- BlockKart owns product tuning and presentation; voplay owns reusable engine
  mechanisms.
- New voplay APIs are demonstrated by at least one non-BlockKart sample or
  focused engine test.
- New renderer and physics APIs are documented at the module boundary.
- Large files shrink by responsibility, not by moving unrelated code into
  anonymous helper files.
- Error paths return structured results where product code can recover.
- Panics are limited to programmer-contract violations, not normal product
  failure paths.
- Diagnostics schemas are versioned when used by baseline scripts.
- Generated quickplay artifacts are updated only by declared generators.

Code review checklist:

- Does the change reduce responsibility mixing?
- Does it keep BlockKart-specific behavior out of voplay?
- Does it make future visual or physics features easier to add?
- Does it preserve diagnostics or improve them?
- Does it include a stress case or explain why no stress case is needed?
- Does it keep the default BlockKart baseline green?

## Performance Acceptance

Performance gates must be hardware-aware, but the product still needs concrete
budgets. The reference target for these gates is the current browser WebGPU
runner at 1280x720.

Initial render budgets:

- Target frame rate: 60 FPS for default BlockKart product profile.
- Default short baseline: P0/P1 equal to zero.
- Default short baseline: steady-state slow frames <= 2 after warmup.
- Default short baseline: renderer submit slow frames <= 1 after warmup.
- Default scene: p99 frame time <= 16.7 ms when full perf snapshot data is
  available from the runner.
- Renderer phase diagnostics must include shadow, main, primitive, post,
  overlay, upload, queue submit, and present timings.
- Render stress scenes must define their own p90 and p99 budgets before being
  used as regression gates.

Initial physics budgets:

- Single-kart physics step p95 <= 1.5 ms in the headless scenario runner.
- Single-kart physics step p99 <= 3.0 ms in the headless scenario runner.
- Multi-vehicle scripted soak defines body count, vehicle count, contact count,
  p95, and p99 budgets before it becomes a required gate.
- Physics scenarios must report NaN, infinity, excessive velocity, excessive
  angular velocity, invalid quaternion, lost contact, and stuck-state failures.
- Contact event generation must not allocate unbounded data per frame.

Startup and restart budgets:

- Cold BlockKart startup to running: warning above 3 seconds, fail above
  5 seconds on the reference baseline.
- Restart-10: P0/P1 equal to zero and bounded resources.
- Restart-50: P0/P1 equal to zero before this refactor can be called product
  ready.
- Immutable render data must not rebuild during restart unless the content
  digest, quality profile, or device identity changed.

Quality budgets:

- Low profile must prioritize frame stability over effects.
- Medium profile is the default product target for the first polished
  BlockKart build.
- High profile may trade cost for visual quality but must still report all
  overruns.
- Cinematic profile is allowed to miss 60 FPS only when explicitly selected
  and reported as non-default.

## Visual Quality Acceptance

The refactor must improve visible output, but visual polish is only accepted
when it is measurable and stable.

Required visual gates:

- Golden screenshot scenes for:
  - default BlockKart scene
  - material fallback scene
  - terrain and decal scene
  - shadow cascade scene
  - primitive chunk scene
  - post-process scene
  - particle/effect scene once particles are introduced
- Canvas nonblank and framing checks for every browser render gate.
- Screenshot diff thresholds for expected equivalence phases.
- Intentional visual improvements documented with before and after captures.
- Debug views for material, LOD, primitive chunks, contact surfaces, and shadow
  cascades.

Acceptance:

- Refactor-only phases do not change screenshots beyond threshold.
- Visual-improvement phases show intentional differences in the relevant
  golden scenes.
- Missing textures, invalid materials, and failed cubemaps produce structured
  diagnostics and visible safe fallbacks.
- Quality profiles produce predictable screenshot differences.

## Physics Quality Acceptance

The refactor must improve handling quality and make physics bugs reproducible.

Required physics gates:

- Skidpad.
- Slalom.
- Jump and landing.
- Wall impact.
- Surface transition.
- Recovery from stuck or upside-down state.
- Static collider density.
- Multi-vehicle scripted soak.

Acceptance:

- Each scenario has deterministic scripted intent.
- Each scenario emits JSON telemetry.
- Each scenario has numeric thresholds for pass/fail.
- Same-runtime replay of scripted intent stays within tolerance.
- Contact events provide enough data for gameplay, audio, effects, and debug.
- Surface material changes can be tested without changing engine code.

## Documentation And Validation

Every phase must update the source of truth it changes.

Required validation for documentation-only edits:

- `./d.py ci task docs-lint`

Required validation for render architecture changes:

- renderer unit tests or focused Rust tests where available
- browser render smoke
- relevant render stress scenes
- `./d.py ci task blockkart-smoke-static`
- `./d.py ci task blockkart-baseline`

Required validation for physics architecture changes:

- headless physics scenarios
- focused vehicle and contact tests
- BlockKart start-race baseline
- restart baseline if scene or physics lifetime changed

Required validation for quickplay integration:

- `./d.py ci task quickplay-validate`
- `./d.py ci task blockkart-smoke-static`
- `./d.py ci task blockkart-baseline`
- restart-10 and restart-50 explicit product soaks

## Completion Definition

This plan is complete only when all of the following are true:

- The renderer has clear module boundaries and an explicit frame graph.
- Render scene snapshots support future multi-view rendering.
- Quality policy is generic, data-driven, and used by BlockKart.
- Visual golden scenes and render stress scenes are official gates.
- The physics stack separates intent, dynamics, backend, contacts, surfaces,
  and telemetry.
- BlockKart uses voplay dynamics and telemetry instead of local wheel-control
  or render-budget workarounds.
- Physics scenario tests cover handling, collision, surface response, and
  repeatability.
- Default BlockKart runs at the 60 FPS product budget with P0/P1 equal to zero.
- Restart-10 and restart-50 pass with bounded resources and no lifecycle
  failure.
- No new voplay render or physics API is BlockKart-specific.
- Docs, baselines, and CI tasks agree on the exact evidence used to claim
  completion.
