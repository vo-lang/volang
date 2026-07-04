# BlockKart / voplay Foundational Design Change Plan

Status: proposed
Date: 2026-07-02
Scope: foundation-level design changes required before BlockKart can move from
demo-grade quickplay content toward a product-grade kart game.

## Document Role

This is not a gameplay feature roadmap, and it is not a sequence of small
tasks. It defines the completed foundation state that voplay, vopack, Studio
quickplay, and BlockKart source must reach before larger product work should be
treated as safe.

BlockKart is the acceptance application. voplay must gain reusable game-engine
capabilities, but it must not become a BlockKart-specific racing engine.

## Current Problem

The checked-in quickplay package can be made to run, but that is not enough.
The foundation is still weak when a fix can land only as edited packaged JSON,
when release dependency source bytes can drift from release manifests, and when
baseline results can claim success without proving the generator, dependency
release, and source repositories agree.

The product foundation must make the correct path the easy path:

- changes land in source repositories first
- module versions and release manifests describe the bytes that run
- checked-in quickplay artifacts are generated, not hand-repaired
- lifecycle, resources, rendering, input, settings, and diagnostics have generic
  engine contracts
- BlockKart owns kart-racing rules, content, tuning, UI copy, and product flow

## Design Principles

1. Source truth before artifact truth.
   Generated quickplay artifacts are delivery outputs, never the first-class
   place to fix engine or game behavior.

2. Generic engine, product-owned rules.
   voplay may own lifecycle, renderer, assets, scene primitives, input,
   settings, diagnostics, and fixed-step services. BlockKart owns race modes,
   item rules, kart tuning, progression, track semantics, product copy, and HUD
   meaning.

3. Explicit ownership beats implicit cleanup.
   Every long-lived object must have a scope, a close policy, diagnostics, and a
   restart/reload expectation.

4. Data pipelines beat code-only content.
   Tracks, materials, collision, spawn points, checkpoints, surface behavior,
   and content validation should move toward data contracts.

5. Diagnostics are part of the design.
   A product-grade game foundation must explain failures, slow frames, resource
   growth, input state, storage state, and render workload without manual
   browser archaeology.

6. Gates must test product invariants.
   CI should fail on broken source provenance, broken reload/restart behavior,
   stale release manifests, resource growth, and renderer regression. It should
   not merely prove that a packaged snapshot is internally self-consistent.

## Foundation Transformations

### F1. Source, Release, And Provenance Chain

Goal: make quickplay packages reproducible from real source and pinned module
releases.

Required changes:

- move voplay and vopack behavior changes into their source repositories
- publish or locally pin new dependency versions before updating BlockKart
- make BlockKart quickplay generation require a real BlockKart source checkout
  or an explicit source archive with commit/digest provenance
- make quickplay provenance record source repository, commit, dirty state,
  module versions, release manifest digests, source digests, and output digests
- make `quickplay_validate` verify release package source entries even when
  `vo.release.json` exists
- reject dependency packages whose embedded source files disagree with
  `vo.web.json`, `.vo-source-digest`, `vo.release.json`, or `vo.lock`
- add a CI/report mode that compares checked-in quickplay artifacts against a
  freshly generated package

Acceptance:

- `quickplay-blockkart-package` can regenerate the checked-in package from
  declared inputs
- changing one embedded dependency source byte without updating the release
  metadata fails validation
- checked-in quickplay diffs are expected only after a generator run
- no BlockKart or voplay/vopack source change exists only inside
  `apps/studio/public/quickplay/blockkart`

### F2. Generic App Lifecycle Contract

Goal: make game startup, failure, retry, reload, pause, resume, restart, and
close explicit voplay app contracts.

Required changes:

- define a generic lifecycle state machine in voplay
- define structured failure reports with owner, subsystem, cause, user message,
  retryability, and developer evidence
- provide app hooks for `Init`, `Enter`, `Pause`, `Resume`, `Restart`, `Close`,
  and failure recovery without assuming BlockKart semantics
- make lifecycle transitions observable through diagnostics and testable without
  screenshot-only validation
- ensure retry/restart/reload cannot leave stale input, audio, render, asset, or
  scene handles behind

Acceptance:

- a minimal non-BlockKart voplay sample uses the same lifecycle contract
- BlockKart failure/retry/restart uses the generic contract
- forced asset failure reaches `Failed` with structured evidence
- reload and restart both return to `Running` without duplicate persistent
  runtime state

### F3. Resource Ownership And Asset Runtime

Goal: make asset lifetime and cache behavior engine-owned, observable, and
reusable across games.

Required changes:

- introduce generic asset scopes for app lifetime, level lifetime, race/session
  lifetime, and frame/transient lifetime
- make mount idempotence, mount counts, group counts, and release state part of
  the public asset diagnostics
- provide a generic immutable-resource cache keyed by stable content identity
  instead of ad hoc product-level fields
- support prepared resource reuse for maps, meshes, textures, audio clips, and
  scene templates without tying the cache to BlockKart names
- define cache invalidation rules for source changes, quality preset changes,
  device loss, and content package changes
- make close/release behavior deterministic and safe to call repeatedly

Acceptance:

- restart-10 and restart-50 keep mount count and core asset counts bounded
- reload keeps settings but does not keep stale browser/session-only handles
- prepared map and static scene cache reuse is visible in generic diagnostics
- multiple mounted packages can coexist without overlay leaks or duplicate
  readers

### F4. Scene Template And Primitive Layer Model

Goal: separate immutable static scenery from dynamic gameplay state.

Required changes:

- define a generic immutable scene template abstraction
- define dynamic instance layers that can be rebuilt without rebuilding static
  templates
- make clone/reuse semantics explicit for primitive layers, material registries,
  chunk buffers, and upload state
- separate visual scenery from physics/collision and gameplay entity state
- expose scene complexity diagnostics: entities, visible entities, chunks,
  draw calls, upload bytes, physics bodies, static template reuse, and dynamic
  rebuild cost

Acceptance:

- restarting a level rebuilds dynamic gameplay and physics state but reuses
  immutable static scene templates
- adding a second content scene uses the same template/layer model
- renderer-facing scene data can explain whether cost is static rebuild,
  dynamic churn, upload churn, or draw workload

### F5. Render Pipeline Foundation

Goal: make rendering capable of product growth without hiding work behind
slow-frame log suppression.

Required changes:

- formalize render phases: game encode, scene prepare, upload, submit, GPU
  completion, present pacing, and worker reply wait
- expose a stable render performance schema with frame percentiles, submit cost,
  upload bytes, draw calls, primitive batches, texture uploads, buffer churn,
  GPU queue depth, and frame pacing reason
- introduce explicit pipeline/material/texture/mesh caches with lifecycle rules
- distinguish intentional cadence waits from real renderer stalls without
  deleting evidence
- support device-loss detection and recoverable renderer failure reports
- define render quality presets as data, not product-specific conditionals
- make headless and browser screenshots validate nonblank canvas, stable
  framing, and basic visual integrity

Acceptance:

- baseline reports can assign slow frames to app, scene, renderer, GPU, pacing,
  or browser/navigation causes
- restart/reload does not recreate renderer-global resources unnecessarily
- render metrics remain meaningful for a second scene or second game
- no slow-frame category is hidden without being counted in a separate bucket

### F6. Input, Settings, And Persistence Foundation

Goal: make product choices durable and cross-platform without hard-coding
BlockKart controls into voplay.

Required changes:

- define serializable action maps with keyboard, gamepad, touch, and future
  native-host bindings
- provide device capability diagnostics
- define a generic settings schema/version/migration API
- provide web and native storage adapters with identical game-facing behavior
- make reload/app restart validation first-class in baseline
- make aborted navigation resource requests distinguishable from real missing
  assets during reload tests

Acceptance:

- settings survive page reload under the correct baseline command
- reload validation does not fail on expected canceled navigation requests
- input preset, quality preset, storage status, and loaded-from-storage state
  appear in diagnostics
- BlockKart owns its action names and defaults; voplay owns the binding engine

### F7. Simulation Boundary And Deterministic Game Loop

Goal: stop product logic from depending on render timing.

Required changes:

- define a fixed-step simulation service with explicit accumulated time,
  interpolation, pause, catch-up, and maximum step policy
- keep render preparation and game simulation separated
- expose simulation step counts, dropped/capped steps, and frame-to-sim timing
  diagnostics
- provide headless tests for checkpoint/lap/respawn/vehicle state that do not
  require WebGPU
- prepare a snapshot/replay boundary for future ghost and regression testing

Acceptance:

- restart resets simulation state deterministically
- race-loop primitives can be tested outside the renderer
- a render stall does not silently advance product state incorrectly

### F8. Content Package And Track Pipeline

Goal: make new product content addable without engine surgery.

Required changes:

- define track package metadata: id, version, display name, assets, surfaces,
  spawn points, checkpoints, laps, collision, lighting, skybox, audio, and
  thumbnail
- validate content packages before runtime
- separate authoring data from runtime prepared assets
- provide content digest identity for caches and provenance
- make quality/LOD decisions data-driven
- keep voplay content APIs generic; BlockKart owns kart-racing interpretation

Acceptance:

- a second track can be packaged and selected without changing voplay internals
- invalid track data fails before entering gameplay
- content package digest appears in diagnostics and baseline reports

### F9. Diagnostics, Gates, And Soak Strategy

Goal: turn product risk into repeatable gates.

Required changes:

- define a baseline matrix: default, forced failure, retry, start race, storage
  reload, restart-2, restart-10, long-run soak, no-WebGPU fallback, and
  simulated device/resource failure
- split fast PR gates from slower product soaks
- make each gate report product state, lifecycle, asset counts, scene counts,
  input/settings state, render attribution, and source provenance
- fix resource failure classification so expected navigation cancellations do
  not mask or create false P0s
- add a provenance gate that fails before browser tests when source or release
  metadata is stale

Acceptance:

- quick PR validation catches source/provenance mistakes quickly
- slow product baseline can run intentionally without blocking every edit
- every P0/P1 finding has owner, subsystem, and evidence
- docs cannot claim a scenario passed unless the matching command and report
  prove it

## Completed Foundation Result

The foundation is complete only when the system has the following shape as a
whole. None of these outcomes is optional, and none is sufficient alone.

### R1. The Running Game Is Source-Backed

The BlockKart build running in Studio quickplay is generated from declared
source inputs, pinned module releases, and reproducible artifacts. The checked-
in package is a delivery artifact, not a hidden source fork.

Required final state:

- BlockKart source contains every BlockKart behavior present in the quickplay
  package.
- voplay source contains every engine behavior present in the quickplay package.
- vopack source contains every package/mount behavior present in the quickplay
  package.
- BlockKart `vo.mod` and `vo.lock` point at module versions or local pins that
  contain those behaviors.
- `project.json`, `deps.json`, and `provenance.json` are regenerated from those
  inputs.
- release manifests, `vo.web.json`, `.vo-source-digest`, lockfile digests, and
  embedded source bytes all agree.

Completion evidence:

- a clean quickplay package regeneration produces the checked-in artifact bytes
  or a documented equivalent digest set
- changing any embedded source byte without updating its release/source digest
  fails validation
- deleting local generated quickplay JSON and rebuilding from declared inputs
  restores the same runnable game

### R2. voplay Has A Generic Game App Contract

voplay exposes a reusable app contract for game startup, lifecycle, structured
failure, restart, reload, pause/resume, diagnostics, and shutdown. BlockKart
uses that contract, but the contract does not mention kart racing.

Required final state:

- lifecycle states and transitions are engine-level concepts
- failure reports are structured and retry-aware
- restart/reload/close are first-class lifecycle paths
- diagnostics are emitted through engine APIs
- stale audio, input, render, asset, scene, and simulation handles cannot survive
  restart or reload unnoticed
- at least one non-BlockKart sample uses the same lifecycle contract

Completion evidence:

- forced BlockKart asset failure reaches `Failed` with structured player and
  developer reports
- BlockKart retry and restart use the same generic lifecycle machinery as first
  startup
- a minimal sample app proves the API is not BlockKart-specific

### R3. Asset And Resource Ownership Is Bounded

Asset lifetime is controlled by explicit scopes and caches. Restart and reload
reuse immutable data where intended and release dynamic state where required.

Required final state:

- asset package mounts are idempotent and counted
- asset groups have clear ownership and release rules
- immutable resources are cached by content identity
- cache invalidation handles content changes, quality changes, reloads, and
  renderer/device loss
- repeated close/restart/reload is safe
- diagnostics expose mounts, groups, loaded asset counts, cache builds, cache
  reuses, and release state

Completion evidence:

- restart-10 and restart-50 keep mount count and core asset counts bounded
- prepared map and static scene resources build once and reuse across restarts
- reload keeps persisted settings but does not retain stale session-only handles

### R4. Static Scene Data And Dynamic Game State Are Separated

voplay supports immutable scene templates and dynamic gameplay layers as
separate concepts. BlockKart can rebuild race, physics, vehicle, audio, and
input state without rebuilding static scenery.

Required final state:

- immutable scene templates have explicit clone/reuse semantics
- dynamic layers are rebuilt independently from static templates
- primitive/material/chunk/upload ownership is visible and testable
- visual scenery, collision, physics bodies, and gameplay entities have clear
  boundaries
- scene diagnostics explain static rebuild, dynamic churn, upload churn, and
  draw workload separately

Completion evidence:

- restart rebuilds dynamic gameplay state while reusing static scene templates
- a second scene or track can use the same template/layer model
- scene reports identify whether cost comes from static template work, dynamic
  state, renderer upload, or draw volume

### R5. Rendering Is Product-Capable And Explainable

The renderer does not merely draw a canvas; it explains cost and failure in a
way that can support larger scenes, product polish, and future content growth.

Required final state:

- render phases are explicit: encode, prepare, upload, submit, GPU completion,
  present pacing, and worker reply
- renderer diagnostics expose frame percentiles, submit cost, upload bytes, draw
  calls, primitive batches, texture uploads, buffer churn, queue depth, and pace
  reason
- pipeline, material, texture, mesh, and prepared-scene caches have lifecycle
  rules
- intentional cadence waits are separated from real stalls without discarding
  evidence
- device loss and recoverable renderer failure have structured reports
- render quality presets are data-driven

Completion evidence:

- slow frames are attributed to app, scene, renderer, GPU, pacing, browser
  reload/navigation, or unknown
- no slow-frame category disappears from accounting
- restart and reload do not unnecessarily recreate renderer-global resources
- screenshots and canvas checks prove nonblank rendering and stable framing

### R6. Input, Settings, And Persistence Are Product-Safe

Player-facing choices survive reloads and future native host contexts through a
generic settings and input foundation.

Required final state:

- action maps are serializable and support keyboard, gamepad, touch, and future
  native bindings
- device capabilities are reported
- settings have schema versioning and migration
- web and native storage adapters expose the same game-facing behavior
- input preset, quality preset, storage status, and loaded-from-storage state
  are visible in diagnostics
- reload validation distinguishes expected canceled navigation requests from
  real missing resources

Completion evidence:

- settings survive page reload and app restart under the baseline gate
- reload passes without false P0 resource failures
- BlockKart owns action names and defaults; voplay owns binding/storage
  mechanics

### R7. Simulation Is Independent From Rendering

Game simulation has a deterministic fixed-step boundary and does not depend on
render timing.

Required final state:

- fixed-step simulation exposes accumulated time, interpolation, pause, catch-up,
  and maximum-step policy
- render preparation and simulation mutation are separated
- diagnostics expose step counts, dropped/capped steps, and frame-to-simulation
  timing
- checkpoint, lap, respawn, and vehicle-state logic can be tested without
  WebGPU
- a snapshot/replay boundary exists for future ghost and regression testing

Completion evidence:

- restart resets simulation deterministically
- render stalls do not silently corrupt game progression
- race-loop logic can be validated headlessly

### R8. Content Is Data-Driven Enough To Grow

Adding tracks, packages, and content variants does not require voplay engine
surgery.

Required final state:

- track/content packages define id, version, display name, assets, surfaces,
  spawn points, checkpoints, laps, collision, lighting, skybox, audio, and
  thumbnail
- package validation runs before gameplay
- authoring data is separated from runtime prepared assets
- content digest identity feeds caches and provenance
- quality and LOD choices are data-driven
- BlockKart interprets racing semantics; voplay supplies generic content
  loading, validation, and prepared-resource support

Completion evidence:

- a second track can be packaged and selected without modifying voplay internals
- invalid content fails before entering gameplay
- content package identity appears in diagnostics and baseline reports

### R9. Product Gates Prove The Whole Foundation

The foundation is not complete because one demo path works. It is complete when
the product baseline proves source integrity, lifecycle safety, bounded
resources, reload/restart behavior, rendering, persistence, and diagnostics
together.

Required final state:

- gates cover default launch, forced failure, retry, start race, storage reload,
  restart-2, restart-10, long-run soak, no-WebGPU fallback, and simulated
  device/resource failure
- fast PR gates and slower soak gates are separated but both are official
  completion evidence
- every report includes product state, lifecycle, asset counts, scene counts,
  input/settings state, render attribution, source provenance, and P0/P1 owner
- docs can claim completion only when the matching report exists and passes

Completion evidence:

- default, forced failure, start race, storage reload, restart-2, restart-10,
  and long-run soak pass with source-backed artifacts
- provenance validation passes before browser tests
- failures are actionable without reading raw browser logs

## Whole-System Completion Definition

The foundation is complete only when all of the following are true at the same
time:

- the runnable BlockKart package is generated from source that contains the
  claimed changes
- voplay exposes generic lifecycle, asset, scene, render, input, settings,
  simulation, diagnostics, and storage contracts
- BlockKart uses those generic contracts while keeping racing rules and product
  semantics in BlockKart
- resource counts stay bounded through repeated restart and reload
- settings persist through reload and app restart
- render diagnostics explain slow frames and resource churn instead of hiding
  them
- at least one non-BlockKart voplay sample proves the engine contracts are
  generic
- content package validation is strong enough to add a second track without
  engine edits
- all official baseline gates pass from regenerated artifacts

## Not Complete If

The work is not complete if any of these are true:

- a behavior exists only in checked-in quickplay JSON
- a dependency version stays the same while embedded source bytes change
- `quickplay_validate` passes despite release/source manifest drift
- package generation cannot reproduce the checked-in game
- restart works but reload fails or produces false P0s
- render slow frames are suppressed instead of classified
- BlockKart-specific racing concepts leak into voplay engine APIs
- resource reuse works only for the current track or current asset names
- the baseline proves a short happy path but not failure, reload, restart, and
  persistence
- docs claim completion without source-backed reports

## Product Work Unlocked By This Result

When the foundation above is complete, larger product systems can start without
building on sand:

- item system
- AI racers
- product menus
- progression
- ghost/replay UX
- multiple polished tracks
- richer visual and audio content
- online or local multiplayer experiments

Those systems are not the foundation result. The foundation result is the
engine, source, content, and gate structure that lets those systems be built
without repeatedly collapsing into one-off demo fixes.
