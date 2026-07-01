# BlockKart / voplay Product Foundation Plan

Status: active plan  
Date: 2026-07-01  
Scope: BlockKart first product phase, driven by voplay and Studio quickplay foundations.

## Purpose

BlockKart must stop being treated as a runnable demo and become a product-grade
game project. The first phase is not about adding a large feature list. It is
about building the foundation that lets the game grow without collapsing under
runtime failures, resource ambiguity, untracked performance regressions, or
engine-specific hacks.

The long-term ambition is a kart-racing product that can eventually move toward
Mario Kart-class expectations: polished controls, rich tracks, reliable
rendering, repeatable content production, strong diagnostics, and later support
for larger modes. voplay is not ready for that whole target today, but it has a
credible starting point and must be improved deliberately.

## Current Baseline

The current checked-in BlockKart quickplay package is a real downstream pressure
test for voplay and Studio:

- BlockKart is exposed on `volang.dev` as a featured quickplay project.
- The runner reaches a full-screen 3D canvas with road, terrain, kart, scatter,
  decals, sky, post effects, and audio startup.
- The new `blockkart-baseline` task captures a smoke, visual, startup, and perf
  baseline from the actual quickplay package.
- Recent local evidence:
  - canvas: `1280x720`, non-empty
  - startup phases: `42`
  - voplay perf reports: `11`
  - slow frames: `1`
  - warnings: `0`
  - errors: `0`
  - resource failures: `0`

That baseline is only a guardrail. It proves the demo still runs. It does not
prove the game is product-grade.

## Non-Goals For This Phase

Do not spend this phase on:

- online multiplayer
- matchmaking, accounts, rankings, or seasons
- open-world streaming
- battle mode, knockout mode, or large AI grids
- photo mode
- large character or vehicle rosters
- economy, unlock trees, or live operations
- rewriting voplay as a BlockKart-only engine
- adding renderer effects before the current scene proves a need

These remain future product directions. They must not block the foundation work.

## Product Bar

The first product phase succeeds when BlockKart can be used as a reliable
downstream product sample:

1. It can start, load, run, restart, and fail gracefully.
2. It exposes enough diagnostics to explain startup, render, asset, scene,
   vehicle, and runtime problems.
3. Its resources and scene objects have explicit lifetimes and can be checked
   for leaks or stale state.
4. Its first race loop can be built on stable lifecycle primitives instead of
   ad-hoc demo control flow.
5. voplay improvements remain generic and reusable by other games.

## Ownership Model

### voplay owns generic engine foundations

- game lifecycle state and host/runtime error surfaces
- renderer and resource proxy failure reporting
- asset groups, asset reports, and rollback primitives
- track/map validation primitives
- scene diagnostics and cleanup contracts
- vehicle/controller telemetry and reusable tuning hooks
- perf snapshot and renderer/runtime diagnostics
- input action serialization primitives

### BlockKart owns game product behavior

- game state flow and presentation
- race rules, lap rules, completion conditions, scoring, and HUD semantics
- kart handling values and product tuning
- track content, visual direction, and gameplay pacing
- local save shape and player-facing settings
- product-specific error copy and UX

### Studio / quickplay owns distribution and verification

- checked-in quickplay package validation
- browser runner smoke coverage
- visual and perf baseline capture
- artifact provenance and dependency integrity
- developer-facing reports from quickplay runs

## Requirements

### R1. Baseline Gate

The BlockKart quickplay package must remain a required downstream gate.

Required capabilities:

- launch the checked-in BlockKart quickplay package in runner mode
- verify first frame and non-empty canvas
- collect startup phase logs
- collect console errors, warnings, network failures, and slow-frame logs
- collect voplay perf reports when trace mode is enabled
- save viewport and canvas images
- emit JSON and Markdown reports
- fail CI on P0/P1 issues

Current status: mostly implemented by `scripts/ci/blockkart_baseline.mjs`.

Next hardening:

- add a restart scenario once BlockKart exposes a restart entry point
- add a longer capture mode for local product investigation
- split initial slow-frame tolerance from steady-state slow-frame tolerance
- include a small summary table of the slowest BlockKart startup phases

Acceptance:

- `./d.py ci task blockkart-baseline` passes on a machine with WebGPU support
- changed-mode PR planning selects `blockkart-baseline` for Studio, quickplay,
  and browser-smoke changes
- report artifacts are actionable without opening the browser manually

### R2. Game Lifecycle

BlockKart and voplay need a real lifecycle contract before more gameplay is
added.

Required states:

- `Boot`
- `Loading`
- `Ready`
- `Running`
- `Paused`
- `Restarting`
- `Failed`
- `Closed`

voplay requirements:

- expose lifecycle callbacks or events from `Game`
- expose structured engine errors
- expose loading progress hooks that can be driven by asset and scene phases
- ensure renderer/resource failures can reach the game instead of only console
  logs or panics

BlockKart requirements:

- own a single product-level lifecycle state machine
- map engine lifecycle to player-visible loading/failure states
- record structured startup phases with stable names
- make restart go through the same lifecycle path as first start

Acceptance:

- forced asset failure enters `Failed` with a clear report
- forced renderer/resource error reaches product-level error handling
- restart transitions through `Restarting -> Loading -> Running`
- baseline report records lifecycle state at capture time

### R3. Error And Diagnostic Reports

Errors must be classified by owner and subsystem.

Required error classes:

- `Runtime`
- `Renderer`
- `ResourceProxy`
- `Asset`
- `Map`
- `Scene`
- `Physics`
- `Vehicle`
- `Audio`
- `Input`
- `Gameplay`

voplay requirements:

- define reusable `EngineError` / `EngineIssue` structures
- make resource proxy failures non-panic in product paths
- expose scene and asset diagnostics as data, not only console output
- preserve original error messages and subsystem context

BlockKart requirements:

- convert engine issues into product issue reports
- attach current lifecycle state, selected track, quality profile, and input
  mode to reports
- keep player-facing messages separate from developer diagnostics

Acceptance:

- one command can produce a JSON issue report for the current quickplay run
- reports identify whether the owner is `voplay`, `BlockKart`, or `Studio`
- no P0 startup failure requires reading raw browser console output first

### R4. Resource And Asset Lifecycle

BlockKart needs product-grade content loading before it needs more content.

voplay requirements:

- add scoped asset groups or `WithGroup`-style helpers
- support rollback when map/track preparation fails mid-load
- provide an `AssetReport` with groups, handles, source paths, dependency
  status, approximate bytes, and leaked handles
- make map validation return structured issue lists, similar to track validation

BlockKart requirements:

- load its runtime asset pack through a declared product loading phase
- define the required contents of a valid track package
- fail before gameplay if required content is missing or inconsistent
- record asset and map issue reports in baseline output when available

Acceptance:

- invalid map package reports all critical issues, not just the first error
- failed map preparation releases any resources loaded by that attempt
- restart does not increase live asset/entity/audio counts

### R5. Scene And Restart Integrity

Restart must be a product operation, not a browser refresh.

voplay requirements:

- provide scene diagnostics for entities, renderables, physics bodies, audio
  sources, vehicles, primitive layers, and generated models
- provide idempotent cleanup contracts for scene helpers
- expose malformed physics/backend decode issues through diagnostics

BlockKart requirements:

- implement a restart path that destroys and rebuilds the playable world
- compare pre-restart and post-restart diagnostics
- keep player input and settings across restart

Acceptance:

- 10 restart cycles do not grow entity/resource/audio counts
- restart can be added to the baseline gate
- stale scene state cannot affect the next run

### R6. Render And Performance Budgets

Render capability is enough for the first phase. The missing piece is budget
discipline.

voplay requirements:

- make `PerfSnapshot` the primary public diagnostics surface
- connect render island, WebGPU, draw stream, scene diagnostics, and startup
  phases into a single report shape
- expose draw bytes, slow frames, GPU queue status, pulse metrics, scene object
  counts, primitive/scatter counts, and texture/resource counts where available
- keep quality profiles generic and device-aware

BlockKart requirements:

- define first-phase budgets for startup, frame stability, draw data, asset
  count, and scene complexity
- keep product quality presets separate from raw engine knobs
- use baseline reports to catch regressions before visual tuning

Initial budgets:

- startup to running: warning above `3s`, fail above `5s`
- steady-state render slow frames: warning above `2` in short baseline capture
- console errors: fail at `1`
- resource failures: fail at `1` unless explicitly ignored
- canvas visual uniformity: fail

Acceptance:

- baseline report lists slowest startup phases
- baseline separates first-frame warmup slow frames from steady-state slow frames
- every P1 performance issue names the likely subsystem

### R7. Vehicle And Control Foundation

The first product phase needs tunable driving, not final racing depth.

voplay requirements:

- separate vehicle physics from game-specific boost economy
- make vehicle/controller config support intentional zero values
- expose steering, throttle, brake, drift, surface contact, boost, wheel, and
  recovery telemetry
- keep surface response generic and data-driven

BlockKart requirements:

- own the actual kart handling values
- record tuning presets and the active preset in diagnostics
- define minimum acceptable controls for keyboard and gamepad

Acceptance:

- a diagnostic capture can explain why the kart is slow, airborne, off-road,
  boosting, or recovering
- tuning changes do not require editing voplay internals
- boost rules are owned by BlockKart, not hidden inside voplay controller code

### R8. Input And Settings Foundation

Product quality requires persistent player choices.

voplay requirements:

- provide serializable action bindings
- expose keyboard/gamepad/touch capability information
- provide a small settings storage abstraction usable in web and native contexts

BlockKart requirements:

- define product settings for input, volume, quality, and accessibility basics
- persist settings locally
- apply settings before gameplay starts

Acceptance:

- settings survive page reload or app restart
- baseline can report active quality and input mode
- default controls are documented in game-facing UI later

### R9. Minimal Race Loop Primitives

Race features should be built after lifecycle and diagnostics are stable.

voplay may provide generic primitives:

- race clock
- checkpoint graph
- lap tracker
- route segment helper
- respawn point helper
- ghost recording format foundation

BlockKart owns:

- race mode flow
- completion conditions
- result screen
- scoring
- HUD semantics

Acceptance:

- the first race loop uses generic primitives without making voplay a
  BlockKart-specific racing framework
- lap/checkpoint data is testable without a renderer
- restart returns the race loop to a clean state

## Development Plan

### Phase 0: Baseline Gate

Goal: make BlockKart a downstream guardrail.

Status: implemented enough to use.

Deliverables:

- `blockkart-baseline` task
- JSON and Markdown reports
- viewport and canvas screenshots
- CI task graph integration

Exit criteria:

- baseline task passes locally
- task lint passes
- changed-mode PR plan selects the baseline task
- quickplay static smoke still passes

### Phase 1: Product Lifecycle And Error Surface

Goal: make startup, failure, and restart explicit.

Deliverables:

- lifecycle state model in BlockKart
- voplay lifecycle/error hooks needed by that model
- product-level failure report
- baseline report includes lifecycle state

Work order:

1. audit BlockKart startup and restart code paths
2. define lifecycle state names and transition rules
3. add engine issue/error propagation for the first real failure path
4. add player-safe failure display and developer report
5. update baseline to assert lifecycle reaches `Running`

Exit criteria:

- forced asset failure is visible and classified
- forced engine/resource failure is visible and classified
- restart path exists and is observable

### Phase 2: Asset, Map, And Scene Integrity

Goal: make content loading safe and explainable.

Deliverables:

- voplay asset scope or equivalent lifecycle helper
- BlockKart asset/map issue report
- scene diagnostics before and after restart
- restart leak test or baseline restart mode

Work order:

1. inventory assets and generated scene objects used by BlockKart
2. add an asset report that can be produced without a successful full run
3. add rollback around map/track preparation failures
4. add scene count diagnostics
5. run repeated restart checks

Exit criteria:

- bad content produces a structured report
- restart does not grow resource or scene counts
- baseline can include asset and scene summaries

### Phase 3: Performance Attribution

Goal: make performance regressions actionable.

Deliverables:

- slowest startup phase table
- first-frame vs steady-state slow-frame classification
- unified perf report fields for render, pulse, WebGPU, draw bytes, and scene
  complexity
- first-phase budgets documented in the baseline report

Work order:

1. promote current startup phase strings into structured records
2. attach render/scene complexity counters to perf output
3. add budget classification to baseline JSON
4. use BlockKart baseline to identify the first real optimization target

Exit criteria:

- a baseline failure tells which subsystem probably owns the regression
- `preparePrimitiveMapAsset` and `buildPrimitiveTrackVisuals` are separately
  attributable
- perf changes can be compared between runs

### Phase 4: Control And Vehicle Foundation

Goal: make the kart tunable as a product.

Deliverables:

- vehicle telemetry report
- controller config that does not hide game rules
- surface response report
- default input/settings persistence foundation

Work order:

1. split product boost rules from generic controller behavior
2. fix config defaulting so zero can be intentional
3. expose telemetry needed to tune steering, drift, boost, off-road, and recovery
4. add active tuning preset to diagnostics
5. persist minimum input and quality settings

Exit criteria:

- handling changes can be made in BlockKart without voplay edits
- telemetry explains the current handling state
- settings survive reload

### Phase 5: First Product Race Loop

Goal: build the first playable product loop on the foundation.

Deliverables:

- start flow
- race clock
- checkpoint/lap validation
- completion state
- result state
- retry using restart lifecycle
- local best time storage

Work order:

1. add generic race primitives only where BlockKart proves the need
2. keep BlockKart mode flow in BlockKart
3. extend baseline with a scripted short run when browser input control is stable
4. add local save for best time and settings

Exit criteria:

- a player can complete one simple run and retry without reload
- baseline still passes
- restart leak checks still pass
- no new engine-specific hacks are introduced

## Later Expansion Gates

Do not start these until Phases 1-5 are stable:

- AI racers: requires race loop, vehicle telemetry, and checkpoint graph
- item system: requires deterministic hit/effect diagnostics and race state
- multiple tracks: requires asset/map validation and package migration
- online ghost leaderboard: requires replay/ghost schema and local race loop
- real-time multiplayer: requires deterministic input/snapshot strategy
- open-world/free-roam: requires world graph and streaming foundations

## Immediate Next Step

The next engineering task is Phase 1:

1. inspect BlockKart startup, world construction, failure, and restart paths
2. write the lifecycle state contract
3. implement the smallest lifecycle/error surface needed to make the current
   quickplay report product-grade
4. update `blockkart-baseline` to verify lifecycle reaches `Running`

Do not add new gameplay until this is complete.
