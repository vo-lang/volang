# voplay Code Review — Issues & Fix Plan

Date: 2026-03-08

## Overview

Full review of the voplay project (Vo game engine: Vo frontend + Rust backend).
All Vo source files (10) and Rust source files (18) were read and analyzed.

---

## Architecture-Level Issues

### A1. externs.rs: Massive cfg duplication for native vs wasm renderer access

**File:** `rust/src/externs.rs` (1068 lines)

Every resource extern (loadTexture, loadFont, loadModel, and their Bytes/Free variants)
repeats the same pattern:

```rust
#[cfg(feature = "native")]
{ crate::native::xxx() }
#[cfg(not(feature = "native"))]
{ RENDERER.get() ... renderer.xxx() }
```

This pattern appears **6+ times**, each 20-30 lines.

**Fix:** Extract a `with_renderer` helper that unifies both paths. All resource
externs call through it.

---

### A2. physics.rs (2D) missing persistent reverse_map and query_pipeline

**File:** `rust/src/physics.rs`

- `ray_cast()`, `query_rect()`, `get_contacts()` each rebuild a
  `collider_to_body: HashMap<ColliderHandle, u32>` from scratch every call.
- `ray_cast()` and `query_rect()` create a new `QueryPipeline` every call.

In contrast, `physics3d.rs` already maintains persistent `reverse_map` and
`query_pipeline` fields.

**Fix:** Add `reverse_map: HashMap<RigidBodyHandle, u32>` and
`query_pipeline: QueryPipeline` to `PhysicsWorld2D`, maintain them in
`spawn_body`/`destroy_body`/`step`, and use them in query methods.

---

### A3. Scene2D/Scene3D massive code duplication

**Files:** `scene2d.vo` (814 lines), `scene3d.vo` (464 lines)

Parallel structures: Spawn/Destroy/Clear/FindByTag/ForEach/Close, physics
integration (ensurePhysics, writePhysCmd, StepAndSyncPhysics, Contacts, RayCast),
Physics2D/Physics3D structs with identical fields.

**Status:** Noted. Structural refactor deferred — requires Vo generics or
interface-based abstraction. Will address shared types extraction (see A7).

---

### A4. physics.rs and physics3d.rs code duplication

**Files:** `rust/src/physics.rs` (415 lines), `rust/src/physics3d.rs` (394 lines)

Near-identical structure: registry, create/destroy world, spawn/destroy body,
apply_commands, step, serialize_state, ray_cast, get_contacts.

**Status:** Noted. Rapier2D and Rapier3D are separate crates with incompatible
types, making generic abstraction difficult. Could use macros but ROI is low.

---

## Abstraction-Level Issues

### A5. Collider type: 2D/3D mixed, magic numbers, no type safety

**File:** `scene2d.vo:131-154`

```go
type Collider struct {
    kind int
    args [3]float64
}
```

- `kind` uses magic ints (1=Box, 2=Circle, 3=Capsule, 4=Box3D, 5=Sphere)
- 2D and 3D colliders share the same type — user can pass Box() to 3D physics
- `args` meaning depends on `kind`, no compile-time safety

**Fix:** Split into `Collider2D` (kind 1-3) and `Collider3D` (kind 3-5) with
named constants. Move constructors to appropriate types.

---

### A6. DrawSprite / DrawSpriteRotated code duplication + zero-tint hack

**File:** `draw.vo:184-239`

Two functions differ only in rotation source (spr.Rotation vs parameter).
30+ lines of identical encoding logic.

Also, zero tint is silently replaced with White:
```go
if tint.A == 0 && tint.R == 0 && tint.G == 0 && tint.B == 0 {
    tint = White
}
```
This prevents using transparent tint.

**Fix:** Extract common sprite encoding into a private method. Remove
zero-tint hack (Sprite already initializes Tint=White in NewSprite).

---

### A7. Shared types scattered in wrong files

**File:** `scene2d.vo`

Types that are shared or independent of Scene2D are defined inside scene2d.vo:
- `TextureID`, `ModelID` — resource IDs used everywhere
- `BodyType` — used by both 2D and 3D physics
- `Collider` — used by both 2D and 3D
- `Sprite`, `SpriteSheet`, `Animation`, `TileSet`, `Tilemap` — rendering types

**Fix:** Move resource IDs to `game.vo`. Move BodyType + Collider types to
a new section or keep in scene2d.vo but with clear 2D/3D separation after
the Collider split.

---

### A8. Resource ID naming inconsistency

**Files:** `game.vo`, `scene2d.vo`, `audio.vo`

| Resource | Type Name   | Free style             |
|----------|-------------|------------------------|
| Texture  | TextureID   | method + standalone    |
| Font     | FontID      | method + standalone    |
| Model    | ModelID     | method + standalone    |
| Audio    | AudioClip   | method only            |

Naming: `TextureID`/`FontID`/`ModelID` vs `AudioClip` (not `AudioClipID`).
Free API: Texture/Font/Model have both `.Free()` and `FreeXxx()`, Audio only `.Free()`.

**Fix:** Remove standalone `FreeTexture`, `FreeFont`, `FreeModel` — keep only
the method `.Free()` for all. Keep type names as-is (AudioClip is semantically
different — it holds data, not just an ID).

---

### A9. MeasureText hardcodes 8x8 monospace

**File:** `draw.vo:162-168`

```go
func MeasureText(text string, size float64) (float64, float64) {
    scale := size / 8.0
    w := float64(len(text)) * 8.0 * scale
    return w, size
}
```

Only valid for built-in 8x8 font. Completely wrong for loaded TTF fonts.

**Status:** Noted. Requires new extern to Rust fontdue for real text metrics.
Deferred — needs API design for font-aware measurement.

---

## Interface-Level Issues

### A10. Web path: initSurface is async but caller doesn't wait

**File:** `rust/src/externs.rs:98-111`

On WASM, `Renderer::new()` is spawned via `spawn_local` and returns immediately.
The Vo side proceeds to call `game.Init()` and enter the game loop, but
`RENDERER` may not be set yet. First few frames silently fail.

**Status:** Noted. This is a WASM platform limitation. The current design
tolerates it (submitFrame checks RENDERER availability). Not a crash bug but
could cause visible flicker on slow GPU init.

---

### A11. Web path missing resize handling

**File:** `host_vogui.vo`

`runWeb` doesn't handle window resize — `GameCtx.Width`/`Height` and scene
screen dimensions are never updated after initial setup.

**Status:** Noted. Requires vogui resize event integration.

---

## Small Issues

### S1. Tilemap.id dead code
**File:** `scene2d.vo:647-648` — `id` field defined but never assigned or used.
**Fix:** Remove `id` field.

### S2. Predefined colors use `var` instead of immutable
**File:** `color.vo:55-66` — `White`, `Black`, etc. are `var`, user can mutate.
**Status:** Noted. Vo may not support top-level `const` for structs.

### S3. Hardcoded pi values
**Files:** `draw.vo:149`, `scene3d.vo:54` — use literal `6.283185307179586` / `3.14159...`
**Fix:** Use `math.Pi` if available, or define a package-level constant.

### S4. Deprecated frame() function still exists
**File:** `rust/src/native.rs:326-330`
**Fix:** Remove it.

### S5. Audio clip.bytes.clone() on every playback
**File:** `rust/src/audio.rs:92` — clones entire audio buffer per play_sound call.
**Status:** Noted. rodio Decoder requires owned data. Could use Arc<[u8]> to share.

### S6. Pan not actually implemented in audio
**File:** `rust/src/audio.rs:111-122` — comment says "approximate" but code ignores pan.
**Status:** Noted. rodio limitation.

### S7. float64Bits/float64FromBits should be in Vo stdlib
**File:** `game.vo:86-89` — these are general-purpose bit conversion functions.
**Status:** Noted. Requires volang stdlib change.

---

## Fix Status

- [x] **A1** — externs.rs: extracted `with_renderer` helper, eliminated all cfg duplication
- [x] **A2** — physics.rs: added persistent `reverse_map` + `query_pipeline` (aligned with physics3d.rs)
- [x] **A5** — Split `Collider` into `Collider2D` (scene2d.vo) / `Collider3D` (scene3d.vo)
- [x] **A6** — Merged DrawSprite/DrawSpriteRotated via `writeSprite`, removed zero-tint hack
- [x] **A8** — Removed redundant standalone `FreeTexture`/`FreeFont`/`FreeModel`
- [x] **A7** — Moved `TextureID`/`ModelID` from scene2d.vo to game.vo
- [x] **S1** — Removed dead `id` field from Tilemap
- [x] **S3** — Replaced hardcoded pi in draw.vo and scene3d.vo with `math.Pi`
- [x] **S4** — Removed deprecated `frame()` from native.rs

### Deferred (noted, not fixed)

- **A3** — Scene2D/Scene3D code duplication: structural refactor, needs Vo generics or interface abstraction
- **A4** — physics.rs / physics3d.rs duplication: rapier2d/3d are separate crates, macro approach low ROI
- **A9** — MeasureText hardcoded 8x8: needs new extern + API design for font-aware measurement
- **A10** — WASM initSurface async race: tolerated, not a crash
- **A11** — Web resize handling: needs vogui resize event integration
- **S2** — Predefined colors use `var`: Vo may not support top-level const for structs
- **S5** — Audio clip.bytes.clone(): rodio requires owned data, could use Arc
- **S6** — Pan not implemented: rodio limitation
- **S7** — float64Bits/float64FromBits: needs volang stdlib change
