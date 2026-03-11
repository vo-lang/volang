# Unified Island Transport for GPU Rendering

**Date**: 2026-03-12  
**Status**: Design  
**Scope**: vo-runtime, vo-vm, voplay, Studio, Playground

---

## 1. Problem Statement

VoPlay games need a GPU rendering surface. On web this is trivial — bind a `<canvas>` to
WebGPU. On native (Studio/Tauri), the situation is fundamentally different: the Tauri window
hosts a WebView, and **native wgpu cannot bind to a WebView canvas**. They are separate GPU
stacks.

The current approach creates a macOS `CAMetalLayer` overlay on top of the WebView and passes
the raw Metal surface pointer to voplay's Rust wgpu renderer. This requires per-platform OS
SDK code (macOS: `CAMetalLayer` + `NSView`; Windows: `HWND` child; Linux: X11 subwindow),
has thread-affinity constraints, and creates z-order/input synchronization edge cases.

### Core insight

Vo's **island** concurrency model already provides exactly the right abstraction:

- Islands have **independent GC heaps** — no shared mutable state
- Islands communicate via **ports** — deep-copy message passing
- Islands run on **independent execution contexts** — currently OS threads, but the runtime
  can map them to anything (threads, processes, Web Workers, WebView wasm instances)
- VoPlay's draw command stream is already a **serialized `[]byte`** — it crosses the
  extern boundary as bytes today

If we make the island transport layer pluggable, the renderer can run as an island inside
the WebView (using browser WebGPU on `<canvas>`) while game logic stays on native Rust
(with JIT, unlimited memory, multi-threaded physics). The same mechanism maps to Web Workers
on the web platform for multi-core parallelism.

---

## 2. First Principles

| # | Principle | Implication |
|---|-----------|-------------|
| 1 | **Island = execution context, not thread** | Runtime decides where an island runs: native thread, WebView wasm, Web Worker |
| 2 | **Port = abstract channel** | The transport layer serializes/deserializes; Vo code sees the same `port` type |
| 3 | **Draw commands are already bytes** | `submitFrame([]byte)` needs zero additional serialization for cross-island transport |
| 4 | **GPU work is not CPU work** | The render island's CPU load is minimal (decode commands + submit to GPU). wasm overhead is negligible for this workload |
| 5 | **Location transparency** | Identical Vo source code runs on all platforms; only the runtime's island scheduling differs |

---

## 3. Architecture

### 3.1 High-level view

```
┌─────────────────────────────────────────────────────────────────┐
│                        Vo Source Code                            │
│                                                                  │
│  renderIsland := island()                                        │
│  drawPort := port([]byte, 1)                                     │
│  inputPort := port([]byte, 1)                                    │
│  go @renderIsland renderLoop(drawPort, inputPort)                │
│  gameLoop(drawPort, inputPort)                                   │
│                                                                  │
└──────────────────────────────┬──────────────────────────────────┘
                               │
               ┌───────────────┼───────────────┐
               │ Island Transport Abstraction   │
               └───────┬───────────────┬───────┘
                       │               │
        ┌──────────────▼──┐    ┌───────▼──────────────┐
        │  Island 0       │    │  Island N (render)    │
        │  (game logic)   │    │                       │
        │                 │    │  Receives draw cmds   │
        │  VM + JIT       │    │  via port             │
        │  Physics        │    │  Submits to GPU       │
        │  Animation      │    │  Sends input back     │
        │  Audio          │    │  via port             │
        └─────────────────┘    └───────────────────────┘
```

### 3.2 Platform mapping

| Platform | Island 0 (logic) | Island N (render) | Transport | GPU API |
|----------|------------------|-------------------|-----------|---------|
| **Studio (Tauri)** | Native thread, JIT enabled | WebView wasm VM (interpreter) | TauriTransport | WebGPU via `<canvas>` |
| **Desktop standalone** | Native thread, JIT enabled | Native thread | InThreadTransport | Native wgpu (Metal/Vulkan/DX12) |
| **Web / Playground** | Main thread wasm VM | Web Worker wasm VM (interpreter) | WorkerTransport | WebGPU via `OffscreenCanvas` |
| **Mobile (Capacitor)** | Native thread, JIT enabled | WebView wasm VM (interpreter) | TauriTransport | WebGPU via `<canvas>` |

### 3.3 Data flow per frame

```
  Island 0 (logic)                          Island N (render)
  ─────────────────                         ──────────────────
  1. input := inputPort.TryRecv()           4. cmds := <-drawPort         (block)
     (non-blocking, may be empty)           5. submitFrame(cmds)
  2. game.Update(input, dt)                 6. inputBytes := pollInput()
  3. drawPort <- drawCmdBytes   ──────────► 7. if len(inputBytes) > 0:
                                ◄─ ─ ─ ─ ──     inputPort <- inputBytes
```

**Logic island drives frame timing.** The logic loop uses `TryRecv` (non-blocking)
for input — if no events arrived from the render island, the frame continues with no
new input (matching current `pollInput` semantics). Frame synchronization is achieved
through `drawPort` (capacity 1): logic blocks on send when render hasn't consumed,
render blocks on recv when logic hasn't produced. No first-frame deadlock; no stall
when the user provides no input.

Draw command bytes are the **exact same `[]byte`** that `DrawCtx` produces today.
No re-encoding, no protocol translation. The port just moves the bytes.

### 3.4 Resource loading

Resources (textures, models, fonts, cubemaps) are loaded by the render island,
not the logic island. The logic island only holds u32 handle IDs.

| Operation | Current flow | New flow |
|-----------|-------------|----------|
| `loadTexture(path)` | Vo extern → Rust renderer → GPU | Resource proxy → render island → WebGPU → return handle |
| `loadModel(path)` | Vo extern → Rust renderer → GPU | Same as above |
| `submitFrame(cmds)` | Vo extern → Rust renderer | Port send → render island recv → wasm renderer |
| `pollInput()` | Vo extern → Rust input buffer | Non-blocking port TryRecv from render island |
| `measureText(...)` | Vo extern → Rust font → return | Resource proxy → render island → return via reply port |
| `physicsStep()` | Vo extern → Rust Rapier | Unchanged (stays on logic island) |

Key: resource loading externs (`loadTexture`, `loadModel`, `loadFont`, etc.) are
intercepted on the logic island by a **resource proxy** that forwards commands to
the render island via a dedicated resource port. The calling fiber blocks until the
render island responds with the result (handle ID, measurement, etc.). This is
transparent to game code.

**Sync query APIs** (`MeasureText`, `ModelBounds`) also go through the resource proxy.
These are infrequent and small-payload; the IPC round-trip (~100-200 µs on Tauri)
is acceptable.

The render island runs voplay's renderer code. The logic island runs game code +
physics + animation.

### 3.5 What stays on Island 0 (native)

- vox VM with JIT (cranelift aarch64/x86_64)
- Game logic (Vo user code)
- Physics (Rapier 2D/3D) — CPU-intensive, benefits from native SIMD
- Animation (skeletal sampling) — CPU-intensive
- Audio (playback engine)
- File I/O, networking

### 3.6 What moves to Island N (render)

- voplay Renderer (wgpu)
- Texture/Model/Font managers (GPU resource ownership)
- Input event collection (DOM events on `<canvas>`)
- Draw command decoding + GPU submission

---

## 4. Island Transport Abstraction

### 4.1 Rust trait

```rust
/// Abstract transport for cross-island communication.
/// Implementations: InThreadTransport, TauriTransport, WorkerTransport.
pub trait IslandTransport: Send + 'static {
    /// Send a command to the remote island. Non-blocking.
    fn send(&self, cmd: IslandCommand) -> Result<(), TransportError>;

    /// Try to receive a command without blocking.
    fn try_recv(&self) -> Result<Option<IslandCommand>, TransportError>;

    /// Block until a command arrives or timeout expires.
    fn recv_timeout(&self, timeout: Duration) -> Result<IslandCommand, TransportError>;

    /// Block until a command arrives.
    fn recv(&self) -> Result<IslandCommand, TransportError>;
}
```

### 4.2 Port data transport

Ports currently use a shared `PortState` (Arc'd ring buffer) for in-process islands.
Cross-process ports need a different backing:

```rust
/// Port backing for cross-boundary islands.
/// Data is serialized through the IslandTransport.
pub enum PortBacking {
    /// In-process: shared memory ring buffer (current implementation)
    InProcess(Arc<PortState>),

    /// Cross-boundary: serialized through transport channel
    /// The port send/recv operations translate to IslandCommand messages
    Remote {
        transport: Arc<dyn IslandTransport>,
        port_id: u32,
    },
}
```

For the initial implementation, we can use a simpler approach: bundle port data
into `IslandCommand` variants:

```rust
pub enum IslandCommand {
    SpawnFiber { closure_data: PackedValue, capture_slots: u16 },
    WakeFiber { fiber_id: u32 },
    Shutdown,
    // New: port data transfer
    PortSend { port_id: u32, data: Vec<u8> },
    PortRecv { port_id: u32 },          // request
    PortData { port_id: u32, data: Vec<u8> }, // response
}
```

**Prerequisite — wasm VM port support.** The current wasm VM (`vo-web`) depends on
`vo-vm` with `default-features = false`, which disables the `std` feature. Without
`std`, `PortSend`/`PortRecv` opcodes trap with `PortNotSupported`, and `IslandNew`
returns a dummy handle. For the render island to participate in port communication,
the wasm VM must gain port support. Two paths:

1. **Transport-backed ports (recommended)**: Add a `PortBacking::Remote` variant
   that routes send/recv through the `IslandTransport` instead of
   `Arc<Mutex<PortState>>`. This works in both `std` and `no_std` environments
   because it doesn't require shared memory — the transport layer handles
   serialization and delivery.
2. **Enable `std` on wasm**: `wasm32-unknown-unknown` supports `Mutex`/`Arc`, so
   enabling `vo-vm/std` for wasm is technically possible. However, the current
   no_std choice was deliberate (binary size, no threading), and the shared-memory
   `PortState` doesn't work cross-context regardless.

Path 1 is required for cross-context ports (native ↔ WebView) and also solves the
wasm port gap. This is a hard prerequisite and is captured in Phase 1.

### 4.3 Transport implementations

#### InThreadTransport (existing, refactored)

Wraps `std::sync::mpsc::channel`. No behavioral change from current island threads.

```rust
pub struct InThreadTransport {
    tx: Sender<IslandCommand>,
    rx: Receiver<IslandCommand>,
}
```

#### TauriTransport (new — Studio/Desktop native ↔ WebView)

Bidirectional communication uses **asymmetric APIs** because Tauri commands are
always invoked from the WebView:

- **WebView → Native**: `invoke('__island_transport_push', { data })` — standard
  Tauri command. WebView pushes island messages (input bytes, resource responses)
  to native.
- **Native → WebView**: `app.emit("island_data", payload)` — Tauri event. Native
  pushes island messages (draw bytes, resource requests) to WebView via event
  listener. Push-based for minimal latency.

Native side (Rust):
```rust
pub struct TauriTransport {
    app_handle: tauri::AppHandle,
    /// Inbound queue: populated by __island_transport_push Tauri command handler
    rx: Receiver<IslandCommand>,
}

impl TauriTransport {
    /// Send command to the WebView render island via Tauri event (push)
    fn send(&self, cmd: IslandCommand) -> Result<(), TransportError> {
        let bytes = cmd.encode();
        self.app_handle.emit("island_data", bytes)?;
        Ok(())
    }
}
```

WebView side (JS/TS):
```typescript
class TauriIslandChannel {
    // Receives commands from native via Tauri event (push)
    private listener: UnlistenFn;
    constructor() {
        this.listener = listen("island_data", (event) => {
            this.onCommand(event.payload);
        });
    }
    // Sends commands to native via Tauri invoke
    async send(data: Uint8Array): Promise<void> {
        await invoke('__island_transport_push', { data: Array.from(data) });
    }
}
```

Binary protocol: length-prefixed frames, same `island_msg` encoding already used
for in-process island spawn. Phase 5 optimizes to a binary custom protocol to
avoid JSON serialization overhead on `emit` payloads.

#### WorkerTransport (new — Web Worker ↔ main thread)

Uses `postMessage` with `Transferable` (zero-copy `ArrayBuffer` transfer).

```typescript
// Main thread side
class WorkerIslandChannel {
    private worker: Worker;
    send(data: Uint8Array): void {
        const buffer = data.buffer.slice(0);
        this.worker.postMessage(buffer, [buffer]); // transfer ownership
    }
}

// Worker side
class HostIslandChannel {
    send(data: Uint8Array): void {
        const buffer = data.buffer.slice(0);
        self.postMessage(buffer, [buffer]);
    }
}
```

`Transferable` `ArrayBuffer` means zero-copy for draw command bytes (~10-50 KB/frame).

---

## 5. Render Island Implementation

### 5.1 Render island bootstrap

The render island runs a minimal Vo VM (interpreter, no JIT) with voplay's wasm
renderer. It executes a Vo render loop function that receives draw commands via
port, submits them to WebGPU, collects DOM input, and sends input back via port.
This achieves full location transparency — the render loop is Vo code identical
on all platforms.

```
Render Island VM
├── voplay externs (wasm feature):
│   ├── initSurface(canvasRef)    → WebGPU canvas binding
│   ├── syncSurface(canvasRef)    → resize tracking
│   ├── submitFrame(cmdBytes)     → decode + GPU submit
│   ├── loadTexture/Model/Font    → GPU resource management
│   ├── pollInput()               → DOM event drain
│   └── isRendererReady()
├── No physics externs (stays on logic island)
├── No animation externs (stays on logic island)
├── No audio externs (stays on logic island)
└── Module: same compiled .vob as logic island
```

**Prerequisite**: Phase 1 must deliver transport-backed ports on no_std/wasm so
the render island VM can execute `PortSend`/`PortRecv` opcodes.

### 5.2 JS bootstrap bridge

The JS side is responsible for **bootstrapping** the render island VM — loading the
wasm module, creating the Vo VM instance, wiring the transport channel, and
providing the canvas. The render loop itself runs inside the Vo VM, not in JS.

WebView and Web Worker have fundamentally different DOM access, so canvas
acquisition and input wiring require **platform-specific adapters**:

```
voplay/js/
├── render_bootstrap.ts    — Shared: load voplay.wasm, create Vo VM, start render fiber
├── bootstrap_webview.ts   — WebView adapter (Studio)
│   ├── canvas: HtmlCanvasElement from DOM
│   ├── input: DOM event listeners on canvas + window
│   └── channel: TauriChannel (invoke + event listener)
├── bootstrap_worker.ts    — Web Worker adapter (Playground)
│   ├── canvas: OffscreenCanvas received via postMessage
│   ├── input: received from main thread via postMessage
│   └── channel: WorkerChannel (self.postMessage)
├── island_channel.ts      — Abstract message channel
│   ├── TauriChannel       — invoke + listen
│   └── WorkerChannel      — postMessage + onmessage
└── types.ts               — Shared protocol types
```

### 5.3 Canvas and input — per-platform

**WebView (Studio)**: The render island has full DOM access. It obtains an
`HtmlCanvasElement` from the page, binds WebGPU to it, and installs
pointer/keyboard/scroll listeners directly on the canvas and `window`. This uses the
same code paths as the current voplay wasm input handler (`input.rs`
`install_wasm_input_handlers`).

**Web Worker (Playground)**: The Worker has **no DOM access**. The main thread
creates a `<canvas>`, calls `canvas.transferControlToOffscreen()`, and sends the
`OffscreenCanvas` to the Worker via `postMessage`. The Worker binds WebGPU to the
`OffscreenCanvas`. Input events are captured by the main thread's DOM listeners and
forwarded to the Worker via `postMessage`. The Worker's render bridge deserializes
them into the same binary input format.

Both paths produce the same serialized input bytes (`input.rs` / `input.vo` format)
and consume the same draw command bytes. The transport protocol is identical.

This eliminates the current `PreviewPanel.svelte` native canvas bridge, pointer event
forwarding, and all OS-level input interception code.

---

## 6. What This Eliminates

| Component | Lines | Status |
|-----------|-------|--------|
| `native_gui_host.rs` (macOS overlay, CAMetalLayer, NSView) | ~510 | **Deleted entirely** |
| `native_surface.rs` (surface provision channel) | ~150 | **Deleted entirely** |
| `host_api.rs` (C-ABI input push functions) | ~110 | **Deleted entirely** |
| `PreviewPanel.svelte` native canvas bridge (pointer/scroll forwarding, resize sync) | ~180 | **Deleted entirely** |
| Tauri commands: `cmd_update_native_canvas`, `cmd_hide_native_canvas`, `cmd_push_native_*` | ~80 | **Deleted entirely** |
| `NativeExtensionHost` trait + `configure_loaded_extensions` | ~100 | **Deleted entirely** |
| **Total removed** | **~1130 lines** | Platform-specific code → zero |

---

## 7. What This Adds

| Component | Estimated size | Description |
|-----------|---------------|-------------|
| `island_transport.rs` (vo-runtime) | ~100 lines | `IslandTransport` trait + `TransportError` |
| `island_thread.rs` refactor (vo-vm) | ~50 lines delta | Extract `InThreadTransport` from existing code |
| `IslandCommand` extensions (vo-runtime) | ~30 lines | `PortSend` / `PortData` variants |
| `render_bootstrap.ts` + adapters (voplay/js) | ~250 lines | VM bootstrap + WebView/Worker adapters |
| `island_channel.ts` (voplay/js) | ~100 lines | TauriChannel + WorkerChannel |
| Studio Tauri transport (Rust side) | ~150 lines | `TauriTransport` impl + 1 command + 1 event |
| Playground Worker transport (JS side) | ~80 lines | Worker setup + `postMessage` bridge |
| **Total added** | **~710 lines** | Cross-platform, shared code |

**Net: -420 lines, zero platform-specific code, unified web + native.**

---

## 8. Performance Analysis

### 8.1 Transport overhead per frame

| Transport | Draw cmds (~20 KB) | Input (~200 bytes) | Latency |
|-----------|-------------------|-------------------|---------|
| InThreadTransport (mpsc) | ~2 µs (memcpy) | ~0.1 µs | ~2 µs |
| TauriTransport (IPC event) | ~50-200 µs | ~10 µs | ~100 µs |
| WorkerTransport (postMessage + Transferable) | ~5-20 µs (zero-copy) | ~2 µs | ~10 µs |

At 60 fps (16.6 ms budget), even the slowest transport (Tauri IPC at ~200 µs) uses
only ~1.2% of the frame budget. Acceptable.

### 8.2 Render island CPU overhead

The render island's Vo VM runs a tight loop:

```vo
fn renderLoop(drawPort port([]byte), inputPort port([]byte)) {
    initSurface("canvas")
    for {
        cmds := <-drawPort
        submitFrame(cmds)
        inputBytes := pollInput()
        if len(inputBytes) > 0 {
            inputPort <- inputBytes
        }
    }
}
```

This is ~10 VM instructions per frame. The actual work (draw command decode + GPU
submission) happens inside the wasm voplay extern, which is compiled Rust→wasm.
Overhead: negligible.

### 8.3 GPU execution

**Zero difference.** The same GPU hardware runs the same shaders regardless of
whether the wgpu commands were issued from native Rust or wasm-in-WebView. GPU
execution time is identical.

### 8.4 Resource loading

Textures and models are loaded on the render island. File access:
- **Studio**: Tauri `convertFileSrc()` or asset protocol provides HTTP URLs for
  local files. The render island's wasm can fetch them.
- **Playground**: Already works via HTTP fetch.
- **Desktop standalone**: Native `InThreadTransport`, no change from current.

### 8.5 Physics / Animation / Audio

Unchanged. These stay on the native logic island with full JIT + SIMD + multi-thread
capability. Zero performance regression.

---

## 9. Migration Path for Existing Games

### 9.1 Current voplay game structure

```vo
import "voplay"

fn main() {
    voplay.Run(voplay.Game{
        Init:   initGame,
        Update: updateGame,
        Draw:   drawGame,
    })
}
```

`voplay.Run` internally calls `initSurface`, then loops: `pollInput → Update → Draw → submitFrame`.

### 9.2 New internal implementation of voplay.Run

The public API (`voplay.Run`) does NOT change. Internally, `voplay.Run` detects the
environment and either:

1. **Single-island mode** (desktop standalone, or fallback): runs everything on one
   island as today.
2. **Split-island mode** (Studio, Playground): creates a render island, spawns the
   render loop on it, and keeps game logic on the current island.

```vo
// Internal to voplay — game developer never sees this
fn Run(game Game) {
    if shouldSplitIslands() {
        runSplitMode(game)
    } else {
        runSingleMode(game)  // current behavior
    }
}

fn runSplitMode(game Game) {
    renderIsland := island()
    drawPort := port([]byte, 1)
    inputPort := port([]byte, 4)  // capacity > 1: input may accumulate
    resPort := port([]byte, 8)    // resource request/reply, serialized

    go @renderIsland renderWorker(drawPort, inputPort, resPort)

    // Wait for renderer ready (blocking recv on a ready signal)
    ctx := GameCtx{ ... }
    game.Init(&ctx)  // resource loads go through resPort proxy

    for {
        // Non-blocking: drain any input from render island
        for {
            inputBytes, ok := inputPort.TryRecv()
            if !ok { break }
            decodeInput(inputBytes, &ctx.input)
        }

        game.Update(&ctx)

        cmds := ctx.draw.Bytes()
        drawPort <- cmds  // blocks if render hasn't consumed previous frame
        ctx.draw.Reset()
    }
}
```

**Game developer code is 100% unchanged.** The split is an internal optimization.
The logic loop uses `TryRecv` for input (non-blocking), matching current `pollInput`
semantics where no input simply means an empty event list. Frame synchronization
happens through `drawPort` (capacity 1): the logic island cannot run more than one
frame ahead of the render island.

### 9.3 Resource loading in split mode

In split mode, `game.Init(&ctx)` runs on the logic island, but games typically call
`LoadTexture`/`LoadModel`/`LoadFont` during Init. These externs need the GPU, which
lives on the render island.

**Design: resource proxy on logic island.** Resource loading externs on the logic
island are replaced by proxy implementations that forward requests to the render
island via `resPort` and block the calling fiber until the render island responds:

```
Logic island                                Render island
─────────────                               ─────────────
game calls LoadTexture("player.png")
  → proxy encodes request            ────► render receives request
  → resPort <- [TAG_LOAD_TEX, path]         → calls real loadTexture
  fiber blocks...                            → gets TextureID = 7
                                             → resReply <- [TAG_OK, 7]
  resReply.Recv()                   ◄─────
  → proxy decodes: TextureID(7)
  → returns 7 to game code
```

**Resource command protocol** (serialized as `[]byte`):

| Command | Request payload | Response payload |
|---------|----------------|------------------|
| `LoadTexture` | tag + path string | tag + u32 handle (or error) |
| `FreeTexture` | tag + u32 handle | tag + ok |
| `LoadModel` | tag + path string | tag + u32 handle |
| `FreeModel` | tag + u32 handle | tag + ok |
| `LoadFont` | tag + path string | tag + u32 handle |
| `FreeFont` | tag + u32 handle | tag + ok |
| `LoadCubemap` | tag + 6× path string | tag + u32 handle |
| `FreeCubemap` | tag + u32 handle | tag + ok |
| `MeasureText` | tag + u32 font + string + f64 size | tag + f64 width + f64 height |
| `ModelBounds` | tag + u32 model | tag + 6× f32 (min/max) |

**Sync query APIs** (`MeasureText`, `ModelBounds`) use the same request/reply
mechanism. These are called infrequently and have small payloads; the IPC round-trip
(~100-200 µs on Tauri) is imperceptible.

Resource loading is infrequent (startup + level transitions). The proxy round-trip
latency is negligible compared to actual GPU upload time.

---

## 10. Detailed Module Changes

### 10.1 vo-runtime (lang/crates/vo-runtime)

**`src/island.rs`** — Add transport abstraction:
```rust
pub enum IslandCommand {
    SpawnFiber { closure_data: PackedValue, capture_slots: u16 },
    WakeFiber { fiber_id: u32 },
    Shutdown,
    PortSend { port_id: u32, data: Vec<u8> },
    PortData { port_id: u32, data: Vec<u8> },
}
```

**`src/island_transport.rs`** — New file:
```rust
pub trait IslandTransport: Send + 'static {
    fn send(&self, cmd: IslandCommand) -> Result<(), TransportError>;
    fn try_recv(&self) -> Result<Option<IslandCommand>, TransportError>;
    fn recv_timeout(&self, timeout: Duration) -> Result<IslandCommand, TransportError>;
    fn recv(&self) -> Result<IslandCommand, TransportError>;
}

pub struct InThreadTransport { /* wraps mpsc::channel */ }
impl IslandTransport for InThreadTransport { /* ... */ }
```

### 10.2 vo-vm (lang/crates/vo-vm)

**`src/vm/island_thread.rs`** — Refactor to use `IslandTransport` trait instead of
raw `mpsc::Receiver`:

```rust
fn run_island_loop(vm: &mut Vm, transport: &dyn IslandTransport) {
    // Same loop structure, but calls transport.try_recv() / transport.recv_timeout()
}
```

**`src/vm/mod.rs`** — Island creation accepts optional transport factory:

```rust
pub fn create_island_with_transport(
    &mut self,
    transport_factory: Option<Box<dyn FnOnce() -> Box<dyn IslandTransport>>>,
) -> GcRef { /* ... */ }
```

### 10.3 voplay (Rust side)

**Delete**: `native_surface.rs`, `host_api.rs`

**Modify**: `externs/render.rs` — `create_native_renderer` removed. Native path
uses `InThreadTransport` with standard wgpu surface creation. No special surface
provision channel needed.

**Keep unchanged**: All renderer, pipeline, physics, animation, audio code.
The renderer doesn't care how it got its surface.

### 10.4 voplay (Vo side)

**Modify**: `game.vo` — `Run()` gains internal split-island mode detection.
`TryRecv` for input, `drawPort` send for frame sync.

**New**: Resource proxy externs — logic-island stubs that forward
resource commands to the render island via `resPort`.

**New**: `render_worker.vo` — Vo render loop using ports directly
(receives draw commands, submits to GPU, sends input back).

### 10.5 voplay (JS side — new)

**New**: `voplay/js/render_bootstrap.ts` — Shared bootstrap: load voplay.wasm,
create Vo VM, wire transport, start render fiber.

**New**: `voplay/js/bootstrap_webview.ts` — WebView adapter: DOM canvas, input
listeners, TauriChannel.

**New**: `voplay/js/bootstrap_worker.ts` — Worker adapter: OffscreenCanvas via
postMessage, forwarded input events, WorkerChannel.

**New**: `voplay/js/island_channel.ts` — `TauriChannel` and `WorkerChannel`
implementations.

### 10.6 Studio (Tauri)

**Delete**: `native_gui_host.rs` (entire file)

**Simplify**: `lib.rs` — Remove all `cmd_update_native_canvas`, `cmd_hide_native_canvas`,
`cmd_push_native_*` commands. Remove `NativeExtensionHost` usage.

**Add**: Island transport plumbing:
- `cmd_island_transport_push` — Tauri command (WebView → Native): WebView pushes
  island messages (input bytes, resource responses) to native
- `"island_data"` Tauri event — (Native → WebView): native pushes island messages
  (draw bytes, resource requests) to WebView via `app.emit()`

**Modify**: `PreviewPanel.svelte` — Remove native canvas bridge entirely. The
render island owns the `<canvas>` in the WebView; it handles its own input.

### 10.7 Playground

**Modify**: Worker initialization to use `WorkerTransport` for render island.
Currently the playground runs everything in one wasm instance. With this change,
render island runs in a Web Worker for parallel execution.

---

## 11. Development Plan

### Phase 0: Island Transport Trait (est. 1 day)

**Goal**: Abstract the transport layer without breaking existing island functionality.

1. Create `vo-runtime/src/island_transport.rs` with `IslandTransport` trait
2. Implement `InThreadTransport` wrapping `mpsc::channel`
3. Refactor `vo-vm/src/vm/island_thread.rs` to use `IslandTransport` trait
4. Refactor `vo-vm/src/vm/mod.rs` island creation to use transport
5. All existing island tests must pass unchanged

**Validation**: `./d.py test both --release` — zero regressions.

### Phase 1: Port Data Commands + Transport-Backed Ports (est. 2-3 days)

**Goal**: Enable port send/recv to work over `IslandTransport` for `[]byte` data,
including on wasm/no_std targets.

1. Add `PortSend` / `PortData` variants to `IslandCommand`
2. Add `PortBacking::Remote { transport, port_id }` to `vo-runtime` port internals
3. Implement remote port send/recv: `PortSend` opcode on a `Remote`-backed port
   serializes data into an `IslandCommand::PortSend` and sends it through the
   transport; receiving side delivers via `IslandCommand::PortData`
4. Enable `PortSend`/`PortRecv`/`PortNew` opcodes on no_std VM path for
   `Remote`-backed ports (currently these trap with `PortNotSupported`)
5. Write unit test: two in-thread islands passing `[]byte` through port
6. Write unit test: port operations work in no_std VM configuration

**Validation**: New tests passing + existing tests green. Port round-trip works
across `InThreadTransport`.

### Phase 2: Render Island Vo Code + Resource Proxy (est. 2-3 days)

**Goal**: Create the Vo-side render worker and resource proxy, validated on native.

1. Write `voplay/render_worker.vo` — Vo render loop: receive draw commands via
   port, call `initSurface` + `submitFrame`, collect input via `pollInput`,
   send input back via port
2. Implement resource proxy externs on logic island: `loadTexture`, `loadModel`,
   `loadFont`, `loadCubemap`, `measureText`, `modelBounds` forward requests to
   render island via resource port and block until reply
3. Modify `voplay/game.vo` `Run()` to support split-island mode with `TryRecv`
   for input
4. Test with InThreadTransport (two native threads, same process)

**Validation**: MarbleRush runs correctly with split islands on native
(two threads, same process, InThreadTransport).

### Phase 3: WebView Render Island — Studio (est. 2-3 days)

**Goal**: Render island runs in Tauri WebView as wasm + WebGPU.

1. Create `voplay/js/render_bootstrap.ts` — shared bootstrap: load voplay.wasm,
   create Vo VM, wire transport, start render fiber
2. Create `voplay/js/bootstrap_webview.ts` — WebView adapter: obtains
   `HtmlCanvasElement` from DOM, installs input listeners
3. Create `voplay/js/island_channel.ts` — `TauriChannel` (invoke + event listener)
4. Add `TauriTransport` to Studio Rust side: `cmd_island_transport_push` command +
   `"island_data"` event emission
5. Modify Studio to boot render island in WebView instead of `NativeExtensionHost`
6. Delete `native_gui_host.rs`, `native_surface.rs`, `host_api.rs`
7. Delete PreviewPanel native canvas bridge code

**Validation**: MarbleRush renders correctly in Studio with render island in WebView.
Input works. Resize works. No OS SDK code.

### Phase 4: Web Worker Render Island — Playground (est. 2-3 days)

**Goal**: Playground uses Web Worker for render island (multi-core).

1. Create `voplay/js/bootstrap_worker.ts` — Worker adapter: receives
   `OffscreenCanvas` via `postMessage`, receives forwarded input events from main
   thread
2. Main thread: `canvas.transferControlToOffscreen()`, install DOM input listeners,
   forward events to Worker via `postMessage`
3. Create `WorkerTransport` (JS `postMessage` + `Transferable`)
4. Main thread runs logic island; Worker runs render island

**Note**: This phase requires browser support for `OffscreenCanvas` + WebGPU in
Workers (Chrome 113+, Firefox 105+, Safari 17+). Feature-detect and fall back to
main-thread canvas if unavailable.

**Validation**: Playground games render correctly with Worker-based render island.

### Phase 5: Cleanup and Optimization (est. 1 day)

1. Remove all dead code (overlay, surface provision, host_api)
2. Optimize Tauri transport: use binary custom protocol instead of JSON `emit`
3. Profile frame latency end-to-end
4. Update voplay documentation

---

## 12. Risk Assessment

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| WebGPU not available in WKWebView (older macOS) | Low — supported since macOS 14 | Fallback to WebGL2 via wgpu's GL backend |
| Tauri IPC latency too high for 60fps | Low — measured ~100-200µs | Use `emit` (push) for native→WebView; binary custom protocol in Phase 5 |
| voplay wasm binary too large for fast load | Medium — current wasm is ~2-5MB | Lazy load, cache in IndexedDB |
| Resource loading latency over IPC | Low — only at startup/level load | Batch resource commands, preload |
| `OffscreenCanvas` + WebGPU in Worker | Medium — WebGPU in Worker is newer | Feature detection, fallback to main-thread canvas |
| wasm VM port support (Phase 1) | Medium — requires `PortBacking::Remote` + no_std opcode enablement | Hard prerequisite; Phase 2+ blocked until this lands |
| Sync resource query APIs (`MeasureText`, `ModelBounds`) over IPC | Low — infrequent calls, small payload | Request/reply over resource port; ~100-200 µs round-trip acceptable |
| Tauri `emit` JSON overhead for binary draw data | Medium — ~20 KB/frame as JSON array is wasteful | Phase 5: binary custom protocol or base64+ArrayBuffer |

---

## 13. Future Extensions

### 13.1 Multiple render islands

For split-screen or multi-view rendering, spawn multiple render islands,
each bound to a different canvas. The transport abstraction supports this
naturally.

### 13.2 Compute islands

Heavy CPU work (pathfinding, procedural generation) can run on dedicated
islands. On web, these map to Web Workers for parallelism. On native,
they get JIT and full CPU access.

### 13.3 Vo → wasm AOT compiler

Long-term, a Vo-to-wasm AOT compiler backend would eliminate the VM
interpreter overhead on web entirely. The island transport design is
orthogonal to this — it works with both interpreted and AOT-compiled
islands.

### 13.4 wasm64

When browsers support wasm64, render islands gain 64-bit address space.
No architectural change needed.

---

## 14. Summary

The unified island transport design replaces ~1130 lines of platform-specific
overlay/surface/input code with ~710 lines of cross-platform transport
abstraction. It leverages Vo's island + port semantics for location-transparent
GPU rendering, works identically on native (Studio), web (Playground), and
mobile, and preserves full native performance for CPU-intensive game logic while
using the browser's GPU stack for rendering.

The render island runs a Vo VM (interpreter, no JIT) executing the same `.vob`
as the logic island. Transport-backed ports (Phase 1) enable `PortSend`/`PortRecv`
on wasm/no_std, achieving full location transparency — the render loop is ordinary
Vo code using ports directly.

Game developer code requires **zero changes**. The split is an internal
runtime optimization invisible to user code.
