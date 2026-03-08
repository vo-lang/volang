# VoPlay — Lightweight Game Engine for Vo

**Date**: 2026-03-08
**Status**: Design — Pre-implementation

---

## 1. What VoPlay Is

VoPlay is a 2D + 3D game engine for Vo. It supports full 2D (sprites, tilemaps, arcade) and simple 3D (scene rendering, avatars, fixed-camera perspectives). It does NOT target open-world or large-scale continuous terrain — it targets games like tower defense, fixed-perspective RTS, roguelikes, arcade games, avatar displays, and card games with 3D effects.

**One import, one module.** Game developers write `import "voplay"` and nothing else. They never touch `vogui`, `wgpu`, or any host integration code.

**GPU-only rendering.** VoPlay renders exclusively through wgpu (WebGPU on browser, Metal/Vulkan/DX12 on native). It does NOT use vogui's Canvas 2D API or `emitRenderBinary` for game frames.

**Built-in physics.** 2D and 3D physics are built-in via Rapier (`rapier2d` + `rapier3d`), the only pure-Rust physics engine with full WASM support. Physics runs on the Rust side; game developers declare physics properties on entities/nodes at spawn time. No physics world, body, or collider management is exposed — voplay handles it internally.

**VoGUI as internal host.** When running in Studio/browser, VoPlay internally uses vogui to create a `<canvas>` DOM element and receive game loop ticks + input events. This is an implementation detail invisible to game developers.

---

## 2. Architecture

```
┌─────────────────────────────────────────────────────┐
│  Game code (only imports voplay)                     │
│                                                      │
│  voplay.Run(voplay.Game{ Init, Update, Draw })       │
└──────────────────────┬──────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────┐
│  voplay (Vo side)                                    │
│                                                      │
│  2D: Scene2D, Entity2D, Sprite, Camera2D, Tilemap    │
│  3D: Scene3D, Node3D, Camera3D, Light                │
│  Physics is a declarative property of Entity/Node    │
│  InputState, Animation, DrawCtx                      │
│                                                      │
│  Draw + physics commands batched into []byte         │
│  ≤4 extern calls per frame (pollInput+step+submit)   │
└──────────────────────┬──────────────────────────────┘
                       │ extern bridge
┌──────────────────────▼──────────────────────────────┐
│  vo-voplay (Rust crate)                              │
│                                                      │
│  2D: Sprite batcher, tilemap renderer, shape/text    │
│  3D: Mesh renderer, model loader (glTF), lighting    │
│  Physics: rapier2d + rapier3d (built-in)             │
│  Directly depends on wgpu + rapier (NOT Vo modules)  │
└─────────────────────────────────────────────────────┘
```

### Key design axiom: Record locally, submit once

Following the same principle as the wgpu wrapper design (`2026-02-22_03_wgpu_wrapper.md`):

- Vo side records draw commands into a binary `[]byte` buffer.
- One extern call per frame (`SubmitFrame`) sends the entire command stream to Rust.
- Rust side decodes the stream and issues batched wgpu draw calls.
- This keeps the Vo↔Rust boundary crossing to **≤4 calls per frame** (pollInput + physics step + submit; resource ops are low-frequency).

---

## 3. Dependency Graph

```
wgpu crate (Rust)          rapier2d + rapier3d (Rust)
  ↑                          ↑
  └────────┬─────────────────┘
           ↓
vo-voplay (Rust crate — renderer + physics)
  ↑ extern bridge
voplay (Vo package — game API, scene graph, draw commands)
  ↑ internal dependency (game developer never sees this)
vogui (Vo package — canvas surface host + game loop tick, web only)
```

| Module | Depends on | Role |
|--------|------------|------|
| `voplay` (Vo) | `vogui` (internal), `vo-voplay` (externs) | Game API, scene, physics wrapper, draw command encoding |
| `vo-voplay` (Rust) | `wgpu`, `rapier2d`, `rapier3d`, `vo-ext`, `vo-runtime` | GPU renderer, physics engine, extern implementations |
| `vogui` | nothing new | Canvas surface + game loop tick only (unchanged, no modifications needed). Input is NOT routed through vogui — voplay registers its own listeners. |

---

## 4. Repo Layout

```
voplay/
├── game.vo                 Game, GameCtx, Run
├── scene2d.vo              Scene2D, Entity2D, Physics2D config, Spawn/Destroy
├── sprite.vo               Sprite, SpriteSheet, Animation
├── camera2d.vo             Camera2D (viewport, follow, zoom, screen↔world)
├── tilemap.vo              Tilemap, TileLayer, TileSet
├── collider.vo             Collider shapes: Box, Circle, Capsule, Sphere, MeshCollider
├── scene3d.vo              Scene3D, Node3D, Physics3D config, Spawn/Destroy
├── camera3d.vo             Camera3D (perspective, orbit, lookAt)
├── light.vo                Light (directional, point, ambient)
├── input.vo                InputState, key/pointer polling
├── draw.vo                 DrawCtx, draw command encoding (binary []byte)
├── physics_cmds.vo         Internal: batch physics command encoding
├── color.vo                Color, predefined colors, hex parsing
├── math.vo                 Vec2, Vec3, Rect, Quat, basic ops (pure Vo, ~200 lines)
├── host_vogui.vo           Internal: vogui integration (Canvas + RunGameLoop + events)
├── host_native.vo          Internal: native window (wgpu surface + own event loop)
├── vo.mod                  module github.com/vo-lang/voplay
├── vo.ext.toml             Extension manifest pointing to rust/
└── rust/
    ├── Cargo.toml           depends on: wgpu, glam, rapier2d, rapier3d, gltf, image
    └── src/
        ├── lib.rs           Extern registration, cfg dispatch
        ├── externs.rs       All extern functions
        ├── renderer.rs      Top-level frame orchestration
        ├── sprite_batch.rs  2D sprite batching (instanced quads)
        ├── tilemap_render.rs 2D tilemap rendering
        ├── shape_render.rs  2D shape rendering (rect, circle, line)
        ├── text_render.rs   Text rendering (bitmap font or glyph atlas)
        ├── mesh_render.rs   3D mesh rendering (forward renderer)
        ├── model_loader.rs  glTF model loading
        ├── lighting.rs      3D lighting (Blinn-Phong or PBR-lite)
        ├── physics.rs       Rapier2D + Rapier3D world management
        ├── pipeline.rs      Shader + pipeline cache
        ├── texture.rs       Texture/image loading, handle management
        ├── stream.rs        Binary draw command decoder (2D + 3D)
        ├── shaders/
        │   ├── sprite.wgsl
        │   ├── tilemap.wgsl
        │   ├── text.wgsl
        │   ├── mesh.wgsl     3D mesh (with lighting)
        │   └── shape.wgsl    2D shapes
        ├── web/             #[cfg(target_arch = "wasm32")]
        │   └── mod.rs       wasm-bindgen exports, WebGPU surface init
        └── native/          #[cfg(not(target_arch = "wasm32"))]
            └── mod.rs       vo-ext registration, native surface init
```

---

## 5. Vo API Design (What Game Developers See)

### 5.1 Entry Point

```go
package main

import "voplay"

func main() {
    voplay.Run(voplay.Game{
        Width:  800,
        Height: 600,
        Title:  "Tower Defense",
        Init:   initGame,
        Update: update,
        Draw:   draw,
    })
}
```

### 5.2 Game Lifecycle

```go
type Game struct {
    Width     int
    Height    int
    Title     string
    Init      func(g *GameCtx)
    Update    func(g *GameCtx, dt float64)      // dt in seconds
    Draw      func(g *GameCtx)
    OnResize  func(g *GameCtx, w, h int)          // optional
}
```

Input is handled exclusively via `InputState` polling (see 5.7). No per-event callbacks — game logic polls `Input.IsKeyPressed`, `Input.IsPointerDown`, etc. in `Update`.

### 5.3 GameCtx

```go
type GameCtx struct {
    Scene2D  *Scene2D       // 2D scene (includes Camera2D, physics, queries)
    Scene3D  *Scene3D       // 3D scene (includes Camera3D, physics, queries)
    Input    *InputState    // keyboard + pointer polling
    Draw     *DrawCtx       // draw command buffer for current frame
    Width    int             // current viewport width (updated on resize)
    Height   int             // current viewport height (updated on resize)
    Data     any             // user-attached game data
}
```

7 fields. Camera belongs to Scene (it’s "how you view the scene"). Physics is internal to Scene (entities declare physics properties, Scene manages the Rapier world). Unused scenes (e.g. `Scene3D` in a pure-2D game) are initialized but cost nothing.

### 5.4 Scene2D & Entity2D

```go
type Scene2D struct {
    Camera       Camera2D      // the camera for this scene
    Tilemap      *Tilemap      // tilemap for this scene (nil = none); drawn first by DrawScene2D
    Gravity      Vec2          // physics gravity (default: 0, 9.8)
    ViewAngle    float64       // 3D viewing angle for 2.5D entities (degrees from vertical)
                               // 0 = top-down, 45 = isometric, 60 = RTS-like
                               // only used when entities have Model set; default: 0 (top-down)
}

// Entity lifecycle
func (s *Scene2D) Spawn(e Entity2D) *Entity2D   // creates entity + Rapier body if Physics set
func (s *Scene2D) Destroy(e *Entity2D)           // removes entity + cleans up Rapier body

// Queries
func (s *Scene2D) FindByTag(tag string) []*Entity2D
func (s *Scene2D) ForEach(fn func(e *Entity2D))
func (s *Scene2D) RayCast(origin, dir Vec2, maxDist float64) *RayCastHit2D
func (s *Scene2D) QueryRect(rect Rect) []*Entity2D
func (s *Scene2D) QueryRadius(cx, cy, r float64) []*Entity2D
func (s *Scene2D) Contacts() []Contact2D         // collision pairs this frame

type Entity2D struct {
    X, Y       float64
    W, H       float64
    Rotation   float64
    ScaleX     float64
    ScaleY     float64
    Sprite     *Sprite         // 2D sprite render (used if Model == 0)
    Model      ModelID         // 3D model render (0 = none, uses Sprite instead)
    ModelYaw   float64         // Y-axis rotation for 3D model (only if Model != 0)
    Tag        string
    Active     bool
    Visible    bool
    ZOrder     int
    Data       any             // user-attached game data
    Physics    *Physics2D      // nil = no physics; set at Spawn time
    bodyID     int             // internal: Rapier body handle, set by Spawn
}

// Physics forces (only valid if entity has Physics)
func (e *Entity2D) ApplyForce(fx, fy float64)
func (e *Entity2D) ApplyImpulse(ix, iy float64)
func (e *Entity2D) Velocity() Vec2
func (e *Entity2D) SetVelocity(vx, vy float64)
```

`Spawn` returns a pointer to the engine-managed entity. If `Physics` is set, voplay internally creates a Rapier rigid body + collider. The entity’s `X, Y, Rotation` are automatically synced from the physics engine each frame (for dynamic bodies) or pushed to the physics engine (for kinematic bodies). Game code reads/writes `entity.X` directly — never touches Rapier.

**2.5D support**: If `Model` is set (non-zero), `DrawScene2D` emits a `DrawModel` command instead of `DrawSprite` for that entity, placing the 3D model at `(X, 0, Y)` with Y-axis rotation `ModelYaw`. This enables games like tower defense with 3D models on a 2D tilemap — game logic stays 2D (rapier2d), visuals are 3D. No Scene3D needed for this pattern.

### 5.4.1 Physics2D Config

```go
type Physics2D struct {
    Type        BodyType       // Dynamic, Static, Kinematic
    Collider    Collider       // Box(hw,hh), Circle(r), Capsule(hh,r)
    Restitution float64        // bounciness (0..1)
    Friction    float64        // surface friction
    Density     float64        // mass = density * area
    FixedRotation bool         // prevent rotation
}

type BodyType int
const (
    Dynamic   BodyType = 0
    Static    BodyType = 1
    Kinematic BodyType = 2
)

// Collider is a tagged value type — kind determines shape, args hold dimensions.
type Collider struct {
    kind    int          // internal: 1=Box, 2=Circle, 3=Capsule, 4=Box3D, 5=Sphere, 6=MeshCollider
    args    [3]float64   // shape dimensions (usage varies by kind)
    model   ModelID      // only for MeshCollider
}

// 2D collider constructors
func Box(halfW, halfH float64) Collider
func Circle(radius float64) Collider
func Capsule(halfHeight, radius float64) Collider
```

### 5.4.2 Collision Events

```go
type Contact2D struct {
    A       *Entity2D
    B       *Entity2D
    Started bool          // true on first contact frame
    Stopped bool          // true on separation frame
}

type RayCastHit2D struct {
    Entity   *Entity2D
    Point    Vec2
    Normal   Vec2
    Distance float64
}
```

### 5.5 Sprite & Animation

```go
type Sprite struct {
    Texture   TextureID
    Region    Rect        // sub-rect within texture (for atlas/spritesheet)
    FlipX     bool
    FlipY     bool
    Anim      *Animation  // nil if static
}

type SpriteSheet struct {
    Texture    TextureID
    FrameW     int
    FrameH     int
    Columns    int
}

func NewSpriteSheet(tex TextureID, frameW, frameH, columns int) *SpriteSheet

type Animation struct {
    Sheet     *SpriteSheet
    Frames    []int           // frame indices
    FPS       float64
    Loop      bool
}

func (a *Animation) Update(dt float64)
func (a *Animation) CurrentFrame() Rect
```

### 5.6 Camera2D

```go
type Camera2D struct {
    X, Y       float64     // center position in world
    Zoom       float64     // 1.0 = no zoom
    Rotation   float64
}

func (c *Camera2D) Follow(x, y float64, lerp float64)
func (c *Camera2D) ScreenToWorld(sx, sy float64) (float64, float64)
func (c *Camera2D) WorldToScreen(wx, wy float64) (float64, float64)
func (c *Camera2D) ViewRect() Rect
```

### 5.7 Tilemap

```go
type TileSet struct {
    Texture   TextureID
    TileW     int
    TileH     int
    Columns   int
}

type TileLayer struct {
    Tiles     []int          // tile indices (0 = empty, 1+ = tile ID)
    Cols      int
    Rows      int
}

type Tilemap struct {
    TileSet   *TileSet
    Layers    []*TileLayer
    id        int             // internal: Rust-side tilemapID, set by Upload
}

func (tm *Tilemap) Upload()                            // packs tile data + tileset info, calls uploadTilemap extern once
func (tm *Tilemap) GetTile(layer, col, row int) int    // Vo-side read (from Layers)
func (tm *Tilemap) SetTile(layer, col, row, tileID int) // updates Vo-side + calls updateTile extern
func (tm *Tilemap) WorldToTile(x, y float64) (int, int)  // pure math
func (tm *Tilemap) TileToWorld(col, row int) (float64, float64)
```

`Upload()` is called once after building the tilemap (typically in `Init`). `SetTile` updates both the Vo-side array and sends an incremental update to Rust. `DrawTilemap` only sends the tilemapID + camera params (~16 bytes per frame, not the full tile array).

### 5.8 Input

```go
type InputState struct { ... }

// Keyboard
func (i *InputState) IsKeyDown(key string) bool
func (i *InputState) IsKeyPressed(key string) bool      // true only on the frame it was pressed
func (i *InputState) IsKeyReleased(key string) bool

// Pointer (mouse / touch)
func (i *InputState) PointerPos() (float64, float64)    // screen coords
func (i *InputState) PointerDelta() (float64, float64)  // movement since last frame
func (i *InputState) WorldPointerPos(cam *Camera2D) (float64, float64) // screen→world via camera
func (i *InputState) IsPointerDown(button int) bool
func (i *InputState) IsPointerPressed(button int) bool  // true only on click frame
```

All input is polling-based. No per-event callbacks on `Game`. This is the standard game engine pattern — game logic checks state in `Update`, never reacts to individual events.

### 5.9 DrawCtx (Draw Command Buffer)

```go
type DrawCtx struct { ... }

// === Scene rendering (automatic) ===
func (d *DrawCtx) DrawScene2D(s *Scene2D)  // draws all visible entities, sorted by ZOrder, using s.Camera
func (d *DrawCtx) DrawScene3D(s *Scene3D)  // draws all visible nodes with models, using s.Camera

// === 2D Manual Primitives (for HUD, debug, effects) ===
func (d *DrawCtx) Clear(r, g, b, a float64)
func (d *DrawCtx) DrawSprite(spr *Sprite, x, y, w, h float64)
func (d *DrawCtx) DrawSpriteRotated(spr *Sprite, x, y, w, h, rotation float64)
func (d *DrawCtx) DrawTilemap(tm *Tilemap, cam *Camera2D)  // encodes tilemapID + camera (16 bytes)
func (d *DrawCtx) DrawRect(x, y, w, h float64, color Color)
func (d *DrawCtx) DrawRectOutline(x, y, w, h float64, color Color, lineWidth float64)
func (d *DrawCtx) DrawCircle(cx, cy, r float64, color Color)
func (d *DrawCtx) DrawLine(x1, y1, x2, y2 float64, color Color, lineWidth float64)
func (d *DrawCtx) DrawText(text string, x, y float64, size int, color Color)

// === 3D extras (usable after DrawScene3D while 3D camera is active) ===
func (d *DrawCtx) DrawBillboard(spr *Sprite, worldPos Vec3, w, h float64) // always faces camera

// === Camera control ===
func (d *DrawCtx) SetCamera2D(cam *Camera2D)  // for manual drawing in world space
func (d *DrawCtx) ResetCamera()               // switch to screen space (for HUD)
func (d *DrawCtx) SetLayer(z int)             // 2D draw order grouping (within 2D mode only)
```

`DrawScene2D` first draws `s.Tilemap` (if non-nil), then iterates all visible entities in z-order. For entities with `Sprite`, it encodes `DrawSprite` commands. For entities with `Model`, it encodes `DrawModel` commands (placing the 3D model at `(X, 0, Y)` in 3D space). Camera is set from `s.Camera`. Game code calls this once, then draws HUD after `ResetCamera()`.

`DrawScene3D` encodes `SetCamera3D` + `SetLights3D` + `DrawModel × N` for all visible nodes. Rust side already has the GPU meshes from `LoadModel` — it just needs the per-frame transform array.

`DrawBillboard` emits a screen-aligned quad at a 3D world position, using the current 3D camera. Useful for health bars, name tags, and damage numbers floating above 3D models.

**Draw ordering**: Commands are processed sequentially. 2D/3D interleaving is determined by call order in `Draw`. `SetLayer` controls ordering within 2D mode only. All methods buffer binary commands into `[]byte`. No extern calls until frame end.

### 5.10 Scene3D & Node3D

```go
type Scene3D struct {
    Camera   Camera3D          // the camera for this scene
    Ambient  Color              // ambient light color
    Gravity  Vec3               // physics gravity (default: 0, -9.8, 0)
}

// Node lifecycle
func (s *Scene3D) Spawn(n Node3D) *Node3D
func (s *Scene3D) Destroy(n *Node3D)

// Hierarchy
func (s *Scene3D) FindByTag(tag string) []*Node3D
func (s *Scene3D) ForEach(fn func(n *Node3D))

// Lighting
func (s *Scene3D) AddLight(l Light)
func (s *Scene3D) ClearLights()

// Physics queries
func (s *Scene3D) RayCast(origin, dir Vec3, maxDist float64) *RayCastHit3D
func (s *Scene3D) Contacts() []Contact3D

type Node3D struct {
    Position   Vec3
    Rotation   Quat            // quaternion
    Scale      Vec3            // default (1,1,1)
    Model      ModelID         // 0 = no model (group node)
    Children   []*Node3D
    Tag        string
    Visible    bool
    Data       any
    Physics    *Physics3D      // nil = no physics; set at Spawn time
    bodyID     int             // internal: Rapier body handle, set by Spawn
}

// Physics forces (only valid if node has Physics)
func (n *Node3D) ApplyForce(force Vec3)
func (n *Node3D) ApplyImpulse(impulse Vec3)
func (n *Node3D) Velocity() Vec3
func (n *Node3D) SetVelocity(vel Vec3)
```

Same pattern as Scene2D: `Spawn` creates the Rapier body if `Physics` is set. `Position` and `Rotation` are auto-synced from physics.

### 5.10.1 Physics3D Config

```go
type Physics3D struct {
    Type          BodyType
    Collider      Collider       // Box3D(hx,hy,hz), Sphere(r), Capsule(hh,r), MeshCollider(model)
    Restitution   float64
    Friction      float64
    Density       float64
    FixedRotation bool
}

// 3D Collider constructors
func Box3D(halfX, halfY, halfZ float64) Collider
func Sphere(radius float64) Collider
func MeshCollider(model ModelID) Collider     // convex hull from model
```

### 5.10.2 3D Types

```go
type Vec3 struct { X, Y, Z float64 }
type Quat struct { X, Y, Z, W float64 }

type Camera3D struct {
    Position   Vec3
    Target     Vec3            // look-at point
    Up         Vec3            // default (0,1,0)
    FOV        float64         // field of view in degrees
    Near       float64         // near clip plane
    Far        float64         // far clip plane
}

func (c *Camera3D) LookAt(pos, target Vec3)
func (c *Camera3D) Orbit(center Vec3, distance, yaw, pitch float64)
func (c *Camera3D) ScreenToRay(sx, sy float64, viewW, viewH int) Ray

type Ray struct { Origin, Direction Vec3 }

type Light struct {
    Kind       LightKind
    Color      Color
    Intensity  float64
    Position   Vec3            // for point light
    Direction  Vec3            // for directional light
}

type LightKind int
const (
    LightDirectional LightKind = 0
    LightPoint       LightKind = 1
)

type Contact3D struct {
    A, B    *Node3D
    Started bool
    Stopped bool
}

type RayCastHit3D struct {
    Node     *Node3D
    Point    Vec3
    Normal   Vec3
    Distance float64
}
```

### 5.11 Resource Loading

```go
type ModelID int
type TextureID int

func LoadModel(path string) ModelID
func LoadModelBytes(data []byte) ModelID
func FreeModel(id ModelID)

func LoadTexture(path string) TextureID
func LoadTextureBytes(data []byte) TextureID
func FreeTexture(id TextureID)
```

Returns opaque handles managed by the Rust side. GPU resources (meshes, textures) live in Rust; Vo only holds integer IDs.

---

## 6. Rust Renderer Design

### 6.1 Externs Registered by vo-voplay

```
// Surface
voplay.initSurface(canvasRef string) error             // create wgpu device + surface + register input listeners
voplay.submitFrame(cmds []byte) error                  // decode + execute draw commands

// Input (voplay manages its own keyboard/pointer listeners, independent of vogui)
voplay.pollInput() []byte                               // returns packed input events since last poll
    // output: [eventKind:u8, ...args]* where:
    //   0x01 KeyDown: keyCode:u16, len:u8, utf8:bytes
    //   0x02 KeyUp:   keyCode:u16, len:u8, utf8:bytes
    //   0x03 PointerDown: x:f32, y:f32, button:u8
    //   0x04 PointerUp:   x:f32, y:f32, button:u8
    //   0x05 PointerMove:  x:f32, y:f32, buttons:u8

// Resources (low-frequency, called during Init or on demand)
voplay.loadTexture(path string) (int, error)            // load image → GPU texture
voplay.loadTextureBytes(data []byte) (int, error)
voplay.freeTexture(id int)
voplay.loadModel(path string) (int, error)              // load glTF/GLB → GPU mesh + materials
voplay.loadModelBytes(data []byte) (int, error)
voplay.freeModel(id int)
voplay.uploadTilemap(cfg []byte) int                    // upload tile data to Rust, returns tilemapID
voplay.updateTile(tilemapID, layer, col, row, tileID int) // incremental tile update

// Physics (batch sync — follows "submit once" principle)
voplay.physicsSpawnBody(dim int, cfg []byte) int        // dim=2|3, cfg=packed Physics2D/3D, returns bodyID
voplay.physicsDestroyBody(bodyID int)
voplay.physicsStep(dim int, dt float64, cmds []byte) []byte
    // input cmds: packed [bodyID, opcode, args...]* for ApplyForce/Impulse/SetVelocity
    // output:     packed [bodyID, x, y, (z), rotation...]* for ALL active bodies
voplay.physicsRayCast(dim int, args []byte) []byte      // input: origin+dir+maxDist, output: hit result
voplay.physicsContacts(dim int) []byte                  // output: packed contact pairs
```

**Total: 16 externs.** Per-frame hot path uses only **3–4 calls**: `pollInput` (1) + `physicsStep` (1 per active dimension, max 2) + `submitFrame` (1). Resource and spawn externs are low-frequency.

**Why `pollInput` instead of vogui events?** vogui's `SetGlobalKeyHandler` only delivers keydown — no keyup. Games need both to implement `IsKeyDown`/`IsKeyReleased`. voplay's Rust/JS side registers `document.addEventListener("keydown"/"keyup")` and canvas pointer listeners during `initSurface`, buffers events, and returns them packed via `pollInput`. This keeps voplay's input system self-contained and independent of vogui's event model.

The key insight: physics state sync follows the same "record locally, submit once" pattern as rendering:
- Vo side buffers all force/impulse/velocity commands into `[]byte` during `Update`
- One `physicsStep` call sends all commands AND receives all body positions back
- Vo side unpacks positions into entity fields
- Zero per-body extern calls during the frame loop

### 6.2 Draw Command Stream Format

Binary, little-endian. Each command is `[u8 opcode][args...]`.

| Opcode | Name | Args |
|--------|------|------|
| 0x01 | Clear | r:f32, g:f32, b:f32, a:f32 |
| 0x02 | SetCamera | x:f32, y:f32, zoom:f32, rotation:f32 |
| 0x03 | ResetCamera | — |
| 0x04 | SetLayer | z:i32 |
| 0x10 | DrawSprite | texID:u32, srcX:f32, srcY:f32, srcW:f32, srcH:f32, dstX:f32, dstY:f32, dstW:f32, dstH:f32, flipX:u8, flipY:u8 |
| 0x11 | DrawSpriteRotated | same as 0x10 + rotation:f32 |
| 0x12 | DrawTilemap | tilemapID:u32, camX:f32, camY:f32, camZoom:f32 |
| 0x20 | DrawRect | x:f32, y:f32, w:f32, h:f32, r:u8, g:u8, b:u8, a:u8 |
| 0x21 | DrawRectOutline | x:f32, y:f32, w:f32, h:f32, r:u8, g:u8, b:u8, a:u8, lineWidth:f32 |
| 0x22 | DrawCircle | cx:f32, cy:f32, radius:f32, r:u8, g:u8, b:u8, a:u8 |
| 0x23 | DrawLine | x1:f32, y1:f32, x2:f32, y2:f32, r:u8, g:u8, b:u8, a:u8, lineWidth:f32 |
| 0x30 | DrawText | x:f32, y:f32, size:u16, r:u8, g:u8, b:u8, a:u8, len:u16, utf8:bytes |
| **3D** | | |
| 0x40 | SetCamera3D | posX:f32, posY:f32, posZ:f32, targetX:f32, targetY:f32, targetZ:f32, upX:f32, upY:f32, upZ:f32, fov:f32, near:f32, far:f32 |
| 0x41 | SetLights3D | lightCount:u8, lights:[kind:u8, r:u8, g:u8, b:u8, intensity:f32, posOrDir:f32×3]* |
| 0x42 | DrawModel | modelID:u32, posX:f32, posY:f32, posZ:f32, rotX:f32, rotY:f32, rotZ:f32, rotW:f32, scaleX:f32, scaleY:f32, scaleZ:f32 |
| 0x43 | DrawBillboard | texID:u32, srcX:f32, srcY:f32, srcW:f32, srcH:f32, worldX:f32, worldY:f32, worldZ:f32, w:f32, h:f32 |

`DrawScene3D` emits: `SetCamera3D` + `SetLights3D` + `DrawModel × N`.
`DrawScene2D` emits: `SetCamera2D` + (`DrawSprite` or `DrawModel` per entity) + `DrawTilemap`. Entities with `Model` set emit `DrawModel` (placed at `(X, 0, Y)`) instead of `DrawSprite`.

### 6.3 Sprite Batcher (2D)

The Rust renderer collects all DrawSprite commands per texture, sorts by layer, and emits instanced draw calls:

- One vertex buffer: unit quad (4 vertices)
- One instance buffer per batch: [dstRect, srcRect, flip, rotation] per sprite
- One draw call per texture per layer

Target: 10,000+ sprites at 60fps.

### 6.4 Tilemap Renderer (2D)

Optimized path for tilemap rendering:

- Tilemap data uploaded as a storage buffer (tile indices)
- TileSet texture bound as atlas
- Single draw call renders entire visible area
- Camera culling: only tiles within viewport are processed in shader

### 6.5 3D Mesh Renderer

Forward rendering pipeline for simple 3D scenes:

- **Model loading**: glTF 2.0 / GLB via the `gltf` Rust crate. Extract meshes, materials, textures.
- **Mesh rendering**: per-node transform matrix, vertex buffer (position + normal + UV), index buffer.
- **Lighting**: Blinn-Phong initially (directional + point + ambient). PBR-lite upgrade path.
- **Depth buffer**: standard depth testing, no transparency sorting (opaque-first).
- **Draw call structure**: one draw call per mesh per material. No instancing needed for simple scenes.
- **Skeletal animation**: out of initial scope. Static meshes and rigid transforms only.

Target: render a scene with 10–50 models (avatars, props, terrain chunks) at 60fps.

### 6.6 Rapier Physics Integration

- `physics.rs` manages Rapier worlds (2D and 3D separately).
- Each world owns: `RigidBodySet`, `ColliderSet`, `IntegrationParameters`, `IslandManager`, `BroadPhase`, `NarrowPhase`, `ImpulseJointSet`, `MultibodyJointSet`, `CCDSolver`, `PhysicsPipeline`.
- Bodies are tracked in `HandleRegistry<RigidBodyHandle>`, mapping voplay integer IDs to Rapier handles.
- `physicsStep` extern does three things in one call:
  1. Decode input command buffer: apply forces, impulses, velocity overrides per body
  2. Call `PhysicsPipeline::step()`
  3. Encode output buffer: all body positions + rotations as packed binary array
- Vo side unpacks the output buffer and writes to `Entity2D.X/Y/Rotation` or `Node3D.Position/Rotation`.
- Contact events collected from `NarrowPhase::contact_pairs()` and returned via `physicsContacts`.
- Rapier features enabled: `enhanced-determinism` for cross-platform reproducibility, `serde-serialize` for save/load.

**Physics command buffer format** (input to `physicsStep`):

| Opcode | Name | Args |
|--------|------|------|
| 0x01 | ApplyForce | bodyID:u32, fx:f32, fy:f32, (fz:f32 if 3D) |
| 0x02 | ApplyImpulse | bodyID:u32, ix:f32, iy:f32, (iz:f32 if 3D) |
| 0x03 | SetVelocity | bodyID:u32, vx:f32, vy:f32, (vz:f32 if 3D) |
| 0x04 | SetPosition | bodyID:u32, x:f32, y:f32, (z:f32 if 3D) |

**Physics state buffer format** (output from `physicsStep`):

2D: `[bodyID:u32, x:f32, y:f32, rotation:f32]*`
3D: `[bodyID:u32, x:f32, y:f32, z:f32, qx:f32, qy:f32, qz:f32, qw:f32]*`

### 6.7 Render Pass Architecture (2D/3D Compositing)

The Rust renderer processes the command stream in a **single render pass**, switching GPU pipeline state as it encounters different opcode types. No multiple render passes or FBOs needed.

**Mode switching rules**:

| Opcode encountered | Renderer action |
|-------------------|----------------|
| `SetCamera3D` | Switch to **3D mode**: perspective projection, depth test ON, depth write ON |
| `DrawModel` | Use 3D mesh pipeline, Blinn-Phong lighting from current lights |
| `DrawBillboard` | Use billboard pipeline (camera-facing quad in 3D space), depth test ON |
| `SetCamera2D` | Switch to **2D mode**: orthographic projection, depth test OFF |
| `DrawSprite` / shapes | Use 2D sprite/shape pipeline, alpha blend, no depth |
| `ResetCamera` | Switch to **screen mode**: identity projection, depth test OFF |
| `DrawText` | Use text pipeline in current mode (screen or 2D) |

**Depth buffer policy**:
- Depth buffer is **always attached** at `BeginRenderPass` time (wgpu requires depth-stencil attachment to be fixed for the entire pass). Created once on first frame, reused across frames.
- **3D pipelines** (`mesh`, `billboard`): `depth_write_enabled: true`, `depth_compare: Less` — standard depth testing.
- **2D pipelines** (`sprite`, `shape`, `text`, `tilemap`): `depth_write_enabled: false`, `depth_compare: Always` — effectively ignores depth, renders on top of preceding 3D content.
- If `DrawScene2D` emits `DrawModel` (2.5D entities), those models use the 3D mesh pipeline (depth ON).
- This is a single render pass with pipeline switching — no FBO, no multi-pass.

**Typical command stream for a 2.5D tower defense** (emitted by `DrawScene2D`):
```
[Clear]
[SetCamera2D]                 ← 2D mode: ortho projection
[DrawTilemap]                 ← tilemap ground layer (depth ignored)
[SetCamera3D (derived)]       ← 3D mode: perspective from ViewAngle
[DrawModel × N]               ← 3D towers/enemies (depth tested)
[DrawBillboard × N]           ← health bars above models (depth tested)
[SetCamera2D]                 ← back to 2D for remaining sprites
[DrawSprite × N]              ← 2D sprite entities (depth ignored)
```

For 2.5D, `DrawScene2D` internally reorders: tilemap first (2D), then model entities (3D). The exact emission order:
1. `SetCamera2D` + `DrawTilemap` (2D ground layer)
2. `SetCamera3D` (derived from `Camera2D` + `Scene2D.ViewAngle`) + `DrawModel × N` (3D entity models, z-sorted)
3. `SetCamera2D` + remaining 2D sprite entities (if any, overlaid)

**Camera2D → Camera3D derivation** for 2.5D:
- Camera2D `(X, Y, Zoom)` maps to a 3D camera looking at `(X, 0, Y)` from above
- `ViewAngle` controls the pitch: 0° = straight down (top-down), 45° = isometric, 60° = RTS
- Camera distance is derived from `Zoom` to keep the same visible area
- This is computed internally in `DrawScene2D`; game developers only set `ViewAngle` once

**Pipeline inventory**:
- **2D sprite pipeline**: instanced textured quads, alpha blend
- **2D shape pipeline**: vertex-colored triangles (rect, circle, line)
- **2D text pipeline**: bitmap font atlas + instanced quads
- **3D mesh pipeline**: vertex (pos + normal + UV) + uniform (MVP + lights), depth test
- **3D billboard pipeline**: camera-facing textured quad, depth test, alpha blend
- All pipelines created lazily on first use, cached for frame lifetime

---

## 7. VoGUI Integration (Internal)

### 7.1 Web/Studio Path (`host_vogui.vo`)

```go
package voplay

import "vogui"

var surfaceReady bool

func runWeb(game *Game) {
    vogui.Run(vogui.App{
        Init: func() any {
            startGameLoop(game)
            return nil
        },
        View: func(state any) vogui.Node {
            return vogui.Canvas(vogui.CanvasProps{
                Fullscreen: true,
            })
        },
    })
}
```

vogui provides only two things: the `<canvas>` DOM element and the game loop tick via `requestAnimationFrame`. All input is handled by voplay's own listeners registered during `initSurface` (see Section 6.1).

Game loop registration:

```go
func startGameLoop(game *Game) {
    vogui.RunGameLoop(func(state any, dtMs float64) {
        dt := dtMs / 1000.0  // vogui delivers ms, game API uses seconds

        if !surfaceReady {
            initSurface("canvas")  // find <canvas> in DOM, create wgpu surface + register input listeners
            game.Init(gameCtx)
            surfaceReady = true
        }

        // 1. Poll input (one extern: returns packed key/pointer events)
        inputBytes := pollInput()
        gameCtx.Input.unpackEvents(inputBytes)

        // 2. Physics step + sync (batch: one extern each)
        stepAndSyncPhysics(dt)

        // 3. Game logic (forces/impulses buffered during Update, sent next frame)
        game.Update(gameCtx, dt)

        // 4. Render
        gameCtx.Draw.reset()
        game.Draw(gameCtx)
        submitFrame(gameCtx.Draw.bytes())

        gameCtx.Input.endFrame()
    })
}

// stepAndSyncPhysics sends buffered commands from LAST frame,
// steps Rapier, receives all body positions, writes to entities.
func stepAndSyncPhysics(dt float64) {
    if gameCtx.Scene2D.hasPhysics() {
        stateBytes := physicsStep(2, dt, gameCtx.Scene2D.physicsCmdBuf.bytes())
        gameCtx.Scene2D.unpackPhysicsState(stateBytes)
        gameCtx.Scene2D.physicsCmdBuf.reset()
    }
    if gameCtx.Scene3D.hasPhysics() {
        stateBytes := physicsStep(3, dt, gameCtx.Scene3D.physicsCmdBuf.bytes())
        gameCtx.Scene3D.unpackPhysicsState(stateBytes)
        gameCtx.Scene3D.physicsCmdBuf.reset()
    }
}
```

### 7.2 Native Path (`host_native.vo`)

```go
func runNative(game *Game) {
    initNativeSurface(game.Width, game.Height, game.Title)
    game.Init(gameCtx)

    for {
        dt := pollEvents(gameCtx)       // returns dt in seconds
        inputBytes := pollInput()
        gameCtx.Input.unpackEvents(inputBytes)
        stepAndSyncPhysics(dt)
        game.Update(gameCtx, dt)
        gameCtx.Draw.reset()
        game.Draw(gameCtx)
        submitFrame(gameCtx.Draw.bytes())
        gameCtx.Input.endFrame()
    }
}
```

### 7.3 `Run` Dispatch

```go
func Run(game Game) {
    gameCtx = newGameCtx(game)
    if isWebEnvironment() {
        runWeb(&game)
    } else {
        runNative(&game)
    }
}
```

`isWebEnvironment()` checks a compile-time or runtime flag.

---

## 8. Per-Frame Data Flow

### Web/Studio

```
1. vogui event loop → waitForEvent() → eventIDGameLoop {Dt: 16.6}

2. vogui.RunGameLoop handler fires
     ↓
   pollInput() → inputBytes           ← ONE extern: packed key/pointer events
   Input.unpackEvents(inputBytes)     ← pure Vo: update key/pointer state tables
   physicsStep(cmds) → stateBytes     ← ONE extern: send forces, step Rapier, receive positions
   unpackPhysicsState(stateBytes)     ← pure Vo: write positions to Entity2D/Node3D fields
   game.Update(gameCtx, dt)           ← game logic (pure Vo, forces buffered into physicsCmdBuf)
   gameCtx.Draw.reset()
   game.Draw(gameCtx)                 ← buffers draw commands into []byte (pure Vo)
   submitFrame(cmds []byte)           ← ONE extern: Rust decodes → wgpu GPU render
   gameCtx.Input.endFrame()           ← clear pressed/released flags

3. vogui render() → emitRenderBinary
     ↓
   Produces: Canvas node (keeps <canvas> alive)
   Does NOT contain game pixels — those went through wgpu directly
```

Per-frame extern calls: **pollInput** (1) + **physicsStep** (1 per active dimension, max 2) + **submitFrame** (1) = **3–4 total**.

Two independent rendering paths:
- **GPU**: voplay → submitFrame → wgpu → canvas pixels
- **DOM**: vogui → emitRenderBinary → JS → DOM nodes (canvas container only)

### Native

```
1. pollEvents()                       ← extern: OS events, returns dt (seconds)
2. pollInput() → inputBytes           ← extern: packed key/pointer events
3. physicsStep(cmds) → stateBytes     ← batch physics sync
4. game.Update(gameCtx, dt)           ← game logic
5. game.Draw(gameCtx)                 ← buffer draw commands
6. submitFrame(cmds []byte)           ← wgpu render + present
```

---

## 9. What a Real Game Looks Like

### Tower Defense Example (2.5D: 2D Gameplay + 3D Models)

```go
package main

import (
    "fmt"
    "voplay"
)

type TD struct {
    Map      *voplay.Tilemap
    Enemies  []*voplay.Entity2D
    Towers   []*voplay.Entity2D
    Wave     int
    Gold     int
}

func main() {
    voplay.Run(voplay.Game{
        Width:  960,
        Height: 640,
        Title:  "Tower Defense",
        Init:   initTD,
        Update: updateTD,
        Draw:   drawTD,
    })
}

func initTD(g *voplay.GameCtx) {
    td := &TD{Gold: 100}
    g.Data = td

    // 2.5D setup: 2D tilemap + 3D models viewed from 60° angle
    g.Scene2D.ViewAngle = 60  // RTS-like perspective for 3D models

    tilesetTex := voplay.LoadTexture("assets/tileset.png")
    tileset := &voplay.TileSet{Texture: tilesetTex, TileW: 32, TileH: 32, Columns: 16}
    td.Map = &voplay.Tilemap{TileSet: tileset}
    // ... populate tile layers ...
    td.Map.Upload()  // upload tile data to Rust once
    g.Scene2D.Tilemap = td.Map  // DrawScene2D will auto-draw this tilemap

    // Spawn enemy with 3D model — game logic is 2D (rapier2d), visuals are 3D
    enemy := g.Scene2D.Spawn(voplay.Entity2D{
        X: 0, Y: 300, W: 32, H: 32,
        Model: voplay.LoadModel("assets/enemy.glb"),  // 3D model instead of sprite
        Tag:   "enemy",
        Physics: &voplay.Physics2D{
            Type:     voplay.Dynamic,
            Collider: voplay.Circle(16),
        },
    })
    td.Enemies = append(td.Enemies, enemy)

    // Spawn static tower with 3D model
    tower := g.Scene2D.Spawn(voplay.Entity2D{
        X: 400, Y: 300, W: 48, H: 48,
        Model: voplay.LoadModel("assets/tower.glb"),  // 3D tower model
        Tag:   "tower",
        Physics: &voplay.Physics2D{
            Type:     voplay.Static,
            Collider: voplay.Box(24, 24),
        },
    })
    td.Towers = append(td.Towers, tower)
}

func updateTD(g *voplay.GameCtx, dt float64) {
    td := g.Data.(*TD)

    // Move enemies toward target (physics velocity)
    for _, e := range td.Enemies {
        e.SetVelocity(50, 0)  // buffered, sent in batch
    }

    // Check collisions this frame
    for _, c := range g.Scene2D.Contacts() {
        if c.Started && (c.A.Tag == "enemy" || c.B.Tag == "enemy") {
            // enemy hit something
        }
    }

    // Place tower on click
    if g.Input.IsPointerPressed(0) {
        wx, wy := g.Input.WorldPointerPos(&g.Scene2D.Camera)
        col, row := td.Map.WorldToTile(wx, wy)
        placeTower(g, td, col, row)
    }
}

func drawTD(g *voplay.GameCtx) {
    td := g.Data.(*TD)
    g.Draw.Clear(0.1, 0.15, 0.1, 1.0)
    g.Draw.DrawScene2D(g.Scene2D)   // auto-draws: tilemap → 3D models → 2D sprites
    // HUD in screen space
    g.Draw.ResetCamera()
    g.Draw.DrawText(fmt.Sprintf("Gold: %d  Wave: %d", td.Gold, td.Wave), 10, 10, 16, voplay.White)
}
```

Notice:
- **2.5D with zero boilerplate** — `Entity2D.Model` renders 3D models; game logic stays 2D (rapier2d)
- **No Scene3D needed** — `ViewAngle` + `Camera2D` auto-derive the 3D camera
- **No `PhysicsWorld`** — physics is declared on the entity at spawn time
- **No `ForEach` + manual draw** — `DrawScene2D` handles tilemap + sprites + 3D models
- **`SetVelocity` / `Contacts()`** are the only physics interactions game code uses

### 3D Avatar Scene Example

```go
package main

import "voplay"

var avatar *voplay.Node3D

func main() {
    voplay.Run(voplay.Game{
        Width: 800, Height: 600, Title: "Avatar Viewer",
        Init: initScene, Update: updateScene, Draw: drawScene,
    })
}

func initScene(g *voplay.GameCtx) {
    // Camera
    g.Scene3D.Camera.LookAt(
        voplay.Vec3{0, 2, 5},   // position
        voplay.Vec3{0, 1, 0},   // target
    )

    // Lighting
    g.Scene3D.AddLight(voplay.Light{
        Kind: voplay.LightDirectional, Direction: voplay.Vec3{-1, -1, -1},
        Color: voplay.White, Intensity: 0.8,
    })
    g.Scene3D.Ambient = voplay.Color{0.2, 0.2, 0.3, 1}

    // Ground
    g.Scene3D.Spawn(voplay.Node3D{
        Model: voplay.LoadModel("assets/ground.glb"),
        Physics: &voplay.Physics3D{
            Type: voplay.Static, Collider: voplay.Box3D(10, 0.1, 10),
        },
    })

    // Avatar
    avatar = g.Scene3D.Spawn(voplay.Node3D{
        Position: voplay.Vec3{0, 1, 0},
        Model:    voplay.LoadModel("assets/avatar.glb"),
        Physics: &voplay.Physics3D{
            Type: voplay.Dynamic, Collider: voplay.Capsule(0.9, 0.3),
            FixedRotation: true,
        },
    })
}

func updateScene(g *voplay.GameCtx, dt float64) {
    // Orbit camera with mouse drag
    if g.Input.IsPointerDown(0) {
        dx, dy := g.Input.PointerDelta()
        g.Scene3D.Camera.Orbit(avatar.Position, 5, dx*0.01, dy*0.01)
    }

    // WASD movement
    var vx, vz float64
    if g.Input.IsKeyDown("w") { vz = -3 }
    if g.Input.IsKeyDown("s") { vz = 3 }
    if g.Input.IsKeyDown("a") { vx = -3 }
    if g.Input.IsKeyDown("d") { vx = 3 }
    avatar.SetVelocity(voplay.Vec3{vx, avatar.Velocity().Y, vz})
}

func drawScene(g *voplay.GameCtx) {
    g.Draw.Clear(0.4, 0.6, 0.9, 1.0)    // sky blue
    g.Draw.DrawScene3D(g.Scene3D)         // auto-draws all nodes + lighting
}
```

---

## 10. Implementation Phases

### Phase 0: Skeleton (1 week)

**Goal**: Empty voplay project that compiles and runs a colored screen in Studio.

- [ ] `voplay/` repo: `vo.mod`, `vo.ext.toml`, `game.vo`, `host_vogui.vo`
- [ ] `voplay/rust/`: Cargo.toml, lib.rs, externs.rs
- [ ] Externs: `initSurface`, `submitFrame` (clear color only)
- [ ] Rust renderer: wgpu device init, surface config, clear pass
- [ ] Test: `voplay.Run(Game{Draw: func(g) { g.Draw.Clear(0.2, 0.3, 0.8, 1.0) }})` shows blue screen in Studio

### Phase 1: 2D Sprite Rendering (1–2 weeks)

**Goal**: Load and draw textured sprites.

- [ ] `draw.vo`: DrawCtx with binary command encoding
- [ ] `sprite.vo`: Sprite, SpriteSheet
- [ ] `color.vo`, `math.vo`: Vec2, Vec3, Rect, Quat, Color
- [ ] Externs: `loadTexture`, `loadTextureBytes`, `freeTexture`
- [ ] Rust: `sprite_batch.rs` — instanced quad rendering
- [ ] Rust: `texture.rs` — image loading, atlas management
- [ ] Rust: `shaders/sprite.wgsl`
- [ ] Rust: `stream.rs` — binary command decoder
- [ ] Test: draw 100+ sprites with different textures

### Phase 2: 2D Camera & Input (1 week)

**Goal**: Camera transform and full input handling.

- [ ] `camera2d.vo`: Camera2D, Follow, ScreenToWorld, WorldToScreen
- [ ] `input.vo`: InputState, key/pointer state tracking
- [ ] Draw command: SetCamera2D, ResetCamera
- [ ] Rust: camera uniform buffer, viewport transform in sprite shader
- [ ] Test: drag to pan camera, scroll to zoom, sprites in world space

### Phase 3: Tilemap (1–2 weeks)

**Goal**: Efficient tile-based map rendering.

- [ ] `tilemap.vo`: Tilemap, TileSet, TileLayer
- [ ] Draw command: DrawTilemap
- [ ] Rust: `tilemap_render.rs` — instanced or storage-buffer based
- [ ] Rust: `shaders/tilemap.wgsl`
- [ ] Tilemap editor data format (simple JSON or binary)
- [ ] Test: 100×100 tilemap scrolling at 60fps

### Phase 4: 2D Scene & Entity (1–2 weeks)

**Goal**: Entity lifecycle, automatic scene rendering, Camera inside Scene.

- [ ] `scene2d.vo`: Scene2D with Spawn/Destroy, FindByTag, ForEach
- [ ] Entity2D struct with Sprite, Physics config, ZOrder
- [ ] `collider.vo`: Collider type, Box/Circle/Capsule constructors
- [ ] `DrawScene2D`: auto-draws all visible entities sorted by ZOrder using Scene2D.Camera
- [ ] Z-order sorting for draw order
- [ ] Test: Spawn 500 entities, Destroy some, DrawScene2D renders correct subset

### Phase 5: Sprite Animation (1 week)

**Goal**: Sprite sheet animation.

- [ ] `sprite.vo`: Animation, frame cycling, FPS control
- [ ] Spritesheet region calculation from frame index
- [ ] Test: animated character walking, attack animation

### Phase 6: Text & Shapes (1 week)

**Goal**: In-game text and primitive shape rendering.

- [ ] Rust: `text_render.rs` — bitmap font atlas
- [ ] Rust: `shaders/text.wgsl`
- [ ] Rust: `shape_render.rs` — DrawRect, DrawCircle, DrawLine, DrawRectOutline
- [ ] Rust: `shaders/shape.wgsl`
- [ ] Test: HUD text, health bars, debug collision rects

### Phase 7: Physics 2D — Rapier (2 weeks)

**Goal**: Built-in 2D rigid body physics, hidden behind Entity2D.

- [ ] Rust: `physics.rs` — Rapier2D world management, HandleRegistry<RigidBodyHandle>
- [ ] Rapier Cargo deps: `rapier2d` with `enhanced-determinism` + `serde-serialize` features
- [ ] Batch sync externs: `physicsSpawnBody`, `physicsDestroyBody`, `physicsStep` (cmds in, state out)
- [ ] `physics_cmds.vo`: internal command buffer for ApplyForce/Impulse/SetVelocity
- [ ] Physics2D config struct on Entity2D (Type, Collider, Restitution, Friction, Density)
- [ ] Scene2D.Spawn creates Rapier body when Physics is set; Destroy cleans up
- [ ] Auto-sync: physicsStep output → Entity2D.X/Y/Rotation
- [ ] Entity2D.ApplyForce/ApplyImpulse/SetVelocity buffer into physicsCmdBuf
- [ ] Scene2D.Contacts(), Scene2D.RayCast(), Scene2D.QueryRect()
- [ ] Test: boxes falling under gravity, bouncing balls, platform collisions

### Phase 8: 3D Rendering — Mesh & Model (2–3 weeks)

**Goal**: Load and render 3D models with lighting.

- [ ] `scene3d.vo`: Scene3D with Spawn/Destroy, Camera inside Scene, AddLight
- [ ] Node3D struct with Position/Rotation/Scale, Model, Physics config
- [ ] `camera3d.vo`: Camera3D, LookAt, Orbit, ScreenToRay
- [ ] `light.vo`: Light (directional, point)
- [ ] `math.vo`: Vec3, Quat additions
- [ ] Externs: `loadModel`, `loadModelBytes`, `freeModel`
- [ ] Rust: `model_loader.rs` — glTF/GLB loading via `gltf` crate
- [ ] Rust: `mesh_render.rs` — forward renderer, vertex (pos+normal+uv)
- [ ] Rust: `lighting.rs` — Blinn-Phong (directional + point + ambient)
- [ ] Rust: `shaders/mesh.wgsl` — 3D mesh with lighting
- [ ] `DrawScene3D`: encodes camera + transform snapshot, Rust renders with existing meshes
- [ ] Depth buffer management
- [ ] Test: render a glTF avatar model with orbit camera and directional light

### Phase 9: Physics 3D — Rapier (1–2 weeks)

**Goal**: Built-in 3D rigid body physics, hidden behind Node3D.

- [ ] Rust: extend `physics.rs` with Rapier3D world management
- [ ] Rapier Cargo deps: `rapier3d` with `enhanced-determinism` + `serde-serialize` features
- [ ] Physics3D config struct on Node3D (same pattern as Physics2D)
- [ ] 3D collider constructors: Box3D, Sphere, Capsule, MeshCollider
- [ ] Scene3D.Spawn creates Rapier3D body when Physics is set
- [ ] Batch sync: reuse physicsStep with dim=3
- [ ] Scene3D.Contacts(), Scene3D.RayCast()
- [ ] Test: cubes falling, sphere bouncing off ground plane, avatar with capsule collider

### Phase 10: Audio (1–2 weeks)

**Goal**: Sound effects and background music.

- [ ] Externs: `loadAudio`, `playSound`, `playMusic`, `stopMusic`, `setVolume`
- [ ] Rust: audio backend (rodio for native, Web Audio API for browser)
- [ ] Sound groups (SFX, Music) with independent volume
- [ ] Test: shoot SFX on click, background music loop

### Phase 11: Native Host (1–2 weeks)

**Goal**: voplay runs standalone without vogui/browser.

- [ ] `host_native.vo`: native event loop, window creation
- [ ] Rust: winit window creation, wgpu native surface
- [ ] Rust: event polling extern (keyboard, mouse, resize, close)
- [ ] Test: same games run as native window

### Phase 12: Demo Game — Tower Defense (2–3 weeks)

**Goal**: Complete playable tower defense game, validating 2D + physics subsystems.

- [ ] Map: tilemap with path, buildable areas
- [ ] Enemies: pathfinding along predefined waypoints, health bars
- [ ] Towers: placement on grid, range visualization, projectiles with physics
- [ ] Waves: spawning schedule, difficulty scaling
- [ ] Economy: gold, tower costs, upgrades
- [ ] UI: score, wave counter, tower selection (voplay HUD)
- [ ] Polish: animations, sound effects

### Phase 13: Demo — 3D Avatar Scene (1–2 weeks)

**Goal**: Validate 3D rendering + physics with a simple interactive scene.

- [ ] Avatar: load glTF character model, place in scene
- [ ] Environment: ground plane, a few props (trees, rocks)
- [ ] Lighting: directional sun + ambient
- [ ] Camera: orbit camera controlled by mouse drag
- [ ] Physics: avatar and props have colliders, can be pushed
- [ ] Test: smooth orbit, correct lighting, physics interactions

---

## 11. What VoPlay Does NOT Do

These are explicitly out of scope:

- **Open-world / large terrain** — voplay targets bounded scenes, not streaming open worlds.
- **Skeletal animation** — initial scope is static meshes with rigid transforms. Skeletal animation is a future extension.
- **Networking** — voplay provides no multiplayer primitives. Can be added as `voplay-net` if needed.
- **Editor** — no visual tilemap editor, scene editor, or level designer. These are future tools.
- **ECS** — voplay uses simple entity lists / node trees, not entity-component-system.
- **Particle system** — future extension, not in initial scope.
- **PBR / advanced materials** — start with Blinn-Phong. PBR is an upgrade path, not initial.

---

## 12. Physics Engine Selection Rationale

| Engine | Language | 2D | 3D | WASM | Standalone | Verdict |
|--------|----------|----|----|------|------------|----------|
| **Rapier** | Pure Rust | ✅ `rapier2d` | ✅ `rapier3d` | ✅ Official | ✅ No framework | **✅ Selected** |
| Avian | Rust | ✅ | ✅ | ✅ | ❌ Bevy-only | ❌ |
| PhysX (physx-rs) | C++ binding | ❌ | ✅ | ❌ C++ | ✅ | ❌ |
| Wrapped2D (Box2D) | C++ binding | ✅ | ❌ | ❌ C++ | ✅ | ❌ |

Rapier is the only pure-Rust physics engine with both 2D+3D and full WASM support. Key features used:
- `enhanced-determinism`: cross-platform reproducible simulations
- `serde-serialize`: save/load physics state
- Same Dimforge ecosystem as Parry (collision detection) and Nalgebra (math)

---

## 13. Math Library Strategy

Math types exist at two independent layers connected only by packed `[]byte`:

| Layer | Types | Source | Why |
|-------|-------|--------|-----|
| **Vo side** (`math.vo`) | Vec2, Vec3, Quat, Rect, Color | **Self-written in pure Vo** (~200 lines) | Vo cannot import Rust crates. These are trivial structs + basic ops. |
| **Rust side** (renderer) | Mat4, projection, transforms | **`glam`** crate | Lightweight, GPU-oriented, standard for Rust game dev. |
| **Rust side** (physics) | Rapier API types | **`nalgebra`** (already a Rapier dep) | Zero additional cost, convert at Rapier boundary. |

**Vo side math** includes: Add, Sub, Scale, Dot, Cross, Normalize, Length, Distance, Lerp, Slerp (for Quat). All pure Vo, no externs needed.

**Rust side** uses `glam` internally for camera matrices, projection, and transform math. At the Rapier API boundary, convert via `nalgebra::Vector3::new(v.x, v.y, v.z)` (trivial, zero-alloc).

The Vo↔Rust boundary transmits only packed f32/f64 bytes — neither side's math library leaks across.

---

## 14. Open Questions

1. **Tilemap data format**: JSON for editor interoperability, or binary for performance? Probably JSON for import, binary at runtime.
2. **Font rendering strategy**: Bitmap font atlas (simple, fast) vs SDF (scalable, more work). Start with bitmap, upgrade later if needed.
3. **Texture atlas packing**: Manual (developer provides atlas) or automatic (voplay packs at load time)? Start with manual.
4. **`isWebEnvironment` detection**: Compile-time `#[cfg(target_arch = "wasm32")]` equivalent in Vo, or runtime check? Needs Vo compiler support or runtime flag.
5. **glTF feature scope**: Which glTF extensions to support initially? Start with basic meshes + materials, no skinning/morph targets.

---

## 15. Success Criteria

VoPlay is done when:

1. A game developer can write a tower defense game with `import "voplay"` as the only dependency.
2. The same developer can render 3D avatars and props in a simple scene with physics.
3. 2D: runs in Studio at 60fps with 100×100 tilemap + 200 animated sprites + physics.
4. 3D: renders 10–50 glTF models with lighting at 60fps.
5. The same code runs as a native window with zero changes.
6. Total API surface is ≤ 50 public functions/types — small enough to learn in a day.
