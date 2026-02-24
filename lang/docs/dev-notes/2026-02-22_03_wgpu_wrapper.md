# Cross-Platform wgpu Wrapper — Detailed Design

> Date: 2026-02-22 (v2 — rewritten from first principles)  
> Parent: `2026-02-22_00_overview.md`  
> Performance target: **near-optimal, capable of driving a 3D engine at 60fps+**

---

## 1. First Principles

The single most expensive operation in a Vo extension is **crossing the language boundary**
(Vo → Rust). On native this costs ~50–100 ns per call; on web (WASM₁ → JS → WASM₂)
it costs ~1–5 µs. A 3D engine issuing 200+ GPU API calls per frame at 60 fps has a
budget of 16.6 ms/frame, of which bridge overhead must be negligible (< 1 ms).

Three design axioms follow:

| # | Axiom | Implication |
|---|-------|-------------|
| 1 | **Record locally, submit once** | GPU commands are recorded into a byte buffer on the Vo side. Only `Queue.Submit()` crosses the boundary — **one extern call per frame** for the entire command stream. |
| 2 | **Raw bytes for bulk data** | `WriteBuffer` / `WriteTexture` / `GetMappedRange` pass `[]byte` directly. No base64, no JSON wrapping. |
| 3 | **JSON for metadata only** | Resource creation, pipeline configuration, and other low-frequency operations use JSON for readability and debuggability. |

**Key insight**: `CommandEncoder`, `RenderPassEncoder`, and `ComputePassEncoder` are
**pure Vo structs** that write binary opcodes into a `[]byte`. They hold no Rust handle
and make zero extern calls. The Rust backend only sees the finished byte stream at
submit time.

---

## 2. Architecture: Three Planes

```
┌──────────────────────────────────────────────────────────┐
│  Vo user code                                            │
│  ┌────────────┐  ┌──────────────────┐  ┌──────────────┐ │
│  │ Resource    │  │ Command          │  │ Data         │ │
│  │ creation    │  │ recording        │  │ transfer     │ │
│  │ (JSON)      │  │ (binary []byte)  │  │ (raw []byte) │ │
│  └─────┬──────┘  └────────┬─────────┘  └──────┬───────┘ │
│        │ 1 call/resource   │ 0 calls            │ 1 call  │
│        │                   │ until Submit        │ /upload │
├────────▼───────────────────▼────────────────────▼────────┤
│  Extern bridge  (Vo ↔ Rust)                              │
│  ≤ 3 boundary crossings per frame (typ.)                 │
├──────────────────────────────────────────────────────────┤
│  Rust backend  (web-sys or wgpu crate)                   │
│  ┌──────────────┐  ┌────────────────┐  ┌──────────────┐ │
│  │ JSON handler │  │ Stream executor│  │ Raw handler  │ │
│  │ serde_json   │  │ opcode loop    │  │ memcpy       │ │
│  └──────────────┘  └────────────────┘  └──────────────┘ │
└──────────────────────────────────────────────────────────┘
```

### 2.1 Control Plane (JSON, low-frequency)

- Adapter / Device initialization
- Surface creation & configuration
- Pipeline / Layout / BindGroup creation
- Buffer / Texture / Sampler / ShaderModule creation & destruction
- Each operation = **one extern call** with JSON `[]byte` in → JSON `[]byte` out

### 2.2 Command Plane (binary, high-frequency)

- All per-frame render / compute commands
- `CommandEncoder` writes binary opcodes into `[]byte` on the Vo side
- `Queue.Submit()` sends the complete byte stream in **one extern call**
- Rust backend iterates opcodes, creates real wgpu objects, executes GPU calls

### 2.3 Data Plane (raw bytes, medium-frequency)

- `Queue.WriteBuffer(buf, offset, data []byte)` — raw bytes, no encoding
- `Queue.WriteTexture(...)` — raw bytes
- `Buffer.GetMappedRange(...)` — returns raw bytes
- Each = **one extern call** with raw byte payload

---

## 3. Package Layout

```
3rdparty/wgpu/                           # Repo: github.com/vo-lang/wgpu
├── wgpu.vo                              # User-facing API (methods, command recording)
├── types.vo                             # Handle types, enums, descriptors
├── cmd.vo                               # Binary encoding helpers (internal)
├── vo.mod                               # module github.com/vo-lang/wgpu
├── vo.ext.toml                          # Extension manifest (web + native)
├── BUILD.md
├── rust/
│   ├── Cargo.toml                       # Dual backend (feature-gated)
│   └── src/
│       ├── lib.rs                       # cfg dispatch + public exports
│       ├── registry.rs                  # HandleRegistry<T>
│       ├── protocol.rs                  # JSON serde types (control plane only)
│       ├── stream.rs                    # Command stream decoder + executor
│       ├── enums.rs                     # u32 ↔ backend enum mapping
│       ├── web/                         # #[cfg(target_arch = "wasm32")]
│       │   ├── mod.rs                   # WebState + wasm-bindgen exports
│       │   ├── init.rs                  # __voInit, adapter/device setup
│       │   ├── control.rs              # JSON-based resource creation
│       │   ├── executor.rs             # Stream executor (web-sys calls)
│       │   └── data.rs                 # WriteBuffer, WriteTexture, MapAsync
│       └── native/                      # #[cfg(not(target_arch = "wasm32"))]
│           ├── mod.rs                   # NativeState + vo-ext registration
│           ├── init.rs                  # Adapter/device (pollster)
│           ├── control.rs              # JSON-based resource creation
│           ├── executor.rs             # Stream executor (wgpu crate calls)
│           └── data.rs                 # WriteBuffer, WriteTexture, MapAsync
├── wgpu.wasm                            # Pre-built web artifact
└── wgpu.js                              # Pre-built web JS glue
```

Note: `stream.rs` defines the **shared opcode constants and stream header** used by
both backends. Each backend's `executor.rs` implements the actual GPU dispatch using
platform-specific types.

---

## 4. Cargo.toml

```toml
[package]
name = "vo-wgpu"
version = "0.2.0"
edition = "2021"

[lib]
crate-type = ["cdylib", "lib"]

[features]
default = []
native = ["dep:wgpu", "dep:vo-ext", "dep:vo-runtime", "dep:pollster"]

[dependencies]
serde = { version = "1", features = ["derive"] }
serde_json = "1"

# Web backend (auto-active on wasm32)
[target.'cfg(target_arch = "wasm32")'.dependencies]
wasm-bindgen = "0.2"
wasm-bindgen-futures = "0.4"
js-sys = "0.3"
web-sys = { version = "0.3", features = [
    "Window", "Navigator", "Document", "HtmlCanvasElement",
    "Gpu", "GpuAdapter", "GpuAdapterInfo", "GpuDevice", "GpuQueue",
    "GpuBuffer", "GpuBufferDescriptor",
    "GpuTexture", "GpuTextureDescriptor", "GpuTextureView", "GpuTextureViewDescriptor",
    "GpuTextureFormat", "GpuTextureUsage",
    "GpuSampler", "GpuSamplerDescriptor",
    "GpuShaderModule", "GpuShaderModuleDescriptor",
    "GpuBindGroupLayout", "GpuBindGroupLayoutDescriptor", "GpuBindGroupLayoutEntry",
    "GpuBindGroup", "GpuBindGroupDescriptor", "GpuBindGroupEntry",
    "GpuPipelineLayout", "GpuPipelineLayoutDescriptor",
    "GpuRenderPipeline", "GpuRenderPipelineDescriptor",
    "GpuComputePipeline", "GpuComputePipelineDescriptor",
    "GpuCommandEncoder", "GpuCommandEncoderDescriptor", "GpuCommandBuffer",
    "GpuRenderPassEncoder", "GpuRenderPassDescriptor",
    "GpuRenderPassColorAttachment", "GpuColorDict",
    "GpuComputePassEncoder", "GpuComputePassDescriptor",
    "GpuVertexState", "GpuFragmentState", "GpuPrimitiveState",
    "GpuColorTargetState", "GpuVertexBufferLayout", "GpuVertexAttribute",
    "GpuDepthStencilState",
    "GpuCanvasContext", "GpuCanvasConfiguration",
    "GpuLoadOp", "GpuStoreOp", "GpuPrimitiveTopology", "GpuIndexFormat",
    "GpuMapMode",
] }

# Native backend
[dependencies.wgpu]
version = "24"
optional = true

[dependencies.pollster]
version = "0.4"
optional = true

[dependencies.vo-ext]
path = "../../lang/crates/vo-ext"
optional = true

[dependencies.vo-runtime]
path = "../../lang/crates/vo-runtime"
optional = true
```

---

## 5. Handle Registry (`registry.rs`)

Platform-independent typed handle management. Unchanged from v1 — this is
orthogonal to the protocol design.

```rust
pub struct HandleRegistry<T> {
    slots: Vec<Option<T>>,
    free_list: Vec<u32>,
}

impl<T> HandleRegistry<T> {
    pub fn new() -> Self {
        Self { slots: Vec::new(), free_list: Vec::new() }
    }

    pub fn alloc(&mut self, value: T) -> u32 {
        if let Some(id) = self.free_list.pop() {
            self.slots[id as usize] = Some(value);
            id
        } else {
            let id = self.slots.len() as u32;
            self.slots.push(Some(value));
            id
        }
    }

    pub fn get(&self, id: u32) -> &T {
        self.slots.get(id as usize)
            .and_then(|s| s.as_ref())
            .unwrap_or_else(|| panic!("wgpu: invalid handle {}", id))
    }

    pub fn get_mut(&mut self, id: u32) -> &mut T {
        self.slots.get_mut(id as usize)
            .and_then(|s| s.as_mut())
            .unwrap_or_else(|| panic!("wgpu: invalid handle {}", id))
    }

    pub fn take(&mut self, id: u32) -> T {
        self.slots.get_mut(id as usize)
            .and_then(|s| s.take())
            .unwrap_or_else(|| panic!("wgpu: double-free handle {}", id))
    }

    pub fn put_back(&mut self, id: u32, value: T) {
        self.slots[id as usize] = Some(value);
    }

    pub fn free(&mut self, id: u32) -> T {
        let val = self.take(id);
        self.free_list.push(id);
        val
    }
}
```

### Global State

Each backend defines thread-local registries. Example (native):

```rust
thread_local! {
    static STATE: RefCell<NativeState> = RefCell::new(NativeState::new());
}

struct NativeState {
    instance: Option<wgpu::Instance>,
    adapters: HandleRegistry<wgpu::Adapter>,
    devices: HandleRegistry<wgpu::Device>,
    queues: HandleRegistry<wgpu::Queue>,
    buffers: HandleRegistry<wgpu::Buffer>,
    textures: HandleRegistry<wgpu::Texture>,
    texture_views: HandleRegistry<wgpu::TextureView>,
    samplers: HandleRegistry<wgpu::Sampler>,
    shader_modules: HandleRegistry<wgpu::ShaderModule>,
    bind_group_layouts: HandleRegistry<wgpu::BindGroupLayout>,
    bind_groups: HandleRegistry<wgpu::BindGroup>,
    pipeline_layouts: HandleRegistry<wgpu::PipelineLayout>,
    render_pipelines: HandleRegistry<wgpu::RenderPipeline>,
    compute_pipelines: HandleRegistry<wgpu::ComputePipeline>,
    surfaces: HandleRegistry<wgpu::Surface<'static>>,
    // Track which surfaces had getCurrentTexture called (for auto-present)
    acquired_surfaces: Vec<(u32, wgpu::SurfaceTexture)>,
}
```

Web backend mirrors this with `web_sys::Gpu*` types.

---

## 6. Binary Command Stream

### 6.1 Stream Layout

The submit payload sent to `extSubmitCommands`:

```
┌─────────────────────────────────────┐
│ queue_handle     : u32 LE           │  ← which queue to submit to
│ device_handle    : u32 LE           │  ← for creating CommandEncoders
├─────────────────────────────────────┤
│ magic            : u32 LE (0x564F4750 = "VOGP")
│ version          : u16 LE (1)       │
│ num_encoders     : u16 LE           │  ← how many CommandBuffers
├─────────────────────────────────────┤
│ Commands for encoder 0...           │
│ OP_FINISH (0xFF)                    │  ← encoder boundary
├─────────────────────────────────────┤
│ Commands for encoder 1...           │
│ OP_FINISH (0xFF)                    │
├─────────────────────────────────────┤
│ ...                                 │
└─────────────────────────────────────┘
```

**Header size**: 4 + 4 + 4 + 2 + 2 = **16 bytes**.

### 6.2 Command Format

Each command:
```
opcode : u8
payload: [varies by opcode, fixed-size per opcode except where noted]
```

No per-command length field. The decoder knows payload size from the opcode.
Variable-length commands (BeginRenderPass, SetBindGroup with dynamic offsets)
use an inline count prefix.

All multi-byte values are **little-endian**. No strings, no keys.

### 6.3 Opcode Table

#### Render Pass Commands

| Opcode | Name | Payload |
|--------|------|---------|
| `0x01` | BeginRenderPass | `num_colors:u8` `has_depth:u8` `_pad:u16` then per-color: `view:u32` `resolve:u32` `load_op:u8` `store_op:u8` `_pad:u16` `clear_r:f64` `clear_g:f64` `clear_b:f64` `clear_a:f64` (36 bytes each); if has_depth: `view:u32` `depth_load:u8` `depth_store:u8` `stencil_load:u8` `stencil_store:u8` `clear_depth:f32` `clear_stencil:u32` (12 bytes) |
| `0x02` | EndRenderPass | (none) |
| `0x03` | RpSetPipeline | `pipeline:u32` |
| `0x04` | RpSetBindGroup | `index:u32` `bg:u32` `num_dyn:u32` then `offsets:u32[]` |
| `0x05` | RpSetVertexBuffer | `slot:u32` `buffer:u32` `offset:u64` `size:u64` |
| `0x06` | RpSetIndexBuffer | `buffer:u32` `format:u8` `_pad:u8[3]` `offset:u64` `size:u64` |
| `0x07` | RpDraw | `vertex_count:u32` `instance_count:u32` `first_vertex:u32` `first_instance:u32` |
| `0x08` | RpDrawIndexed | `index_count:u32` `instance_count:u32` `first_index:u32` `base_vertex:i32` `first_instance:u32` |
| `0x09` | RpSetViewport | `x:f32` `y:f32` `w:f32` `h:f32` `min_depth:f32` `max_depth:f32` |
| `0x0A` | RpSetScissorRect | `x:u32` `y:u32` `w:u32` `h:u32` |
| `0x0B` | RpSetBlendConstant | `r:f64` `g:f64` `b:f64` `a:f64` |
| `0x0C` | RpSetStencilRef | `ref:u32` |
| `0x0D` | RpDrawIndirect | `buffer:u32` `offset:u64` |
| `0x0E` | RpDrawIndexedIndirect | `buffer:u32` `offset:u64` |

#### Compute Pass Commands

| Opcode | Name | Payload |
|--------|------|---------|
| `0x20` | BeginComputePass | (none) |
| `0x21` | EndComputePass | (none) |
| `0x22` | CpSetPipeline | `pipeline:u32` |
| `0x23` | CpSetBindGroup | `index:u32` `bg:u32` `num_dyn:u32` then `offsets:u32[]` |
| `0x24` | CpDispatch | `x:u32` `y:u32` `z:u32` |
| `0x25` | CpDispatchIndirect | `buffer:u32` `offset:u64` |

#### Encoder-Level Commands

| Opcode | Name | Payload |
|--------|------|---------|
| `0x30` | CopyBufferToBuffer | `src:u32` `src_offset:u64` `dst:u32` `dst_offset:u64` `size:u64` |
| `0x31` | CopyBufferToTexture | `src_buf:u32` `src_offset:u64` `bytes_per_row:u32` `rows_per_image:u32` `dst_tex:u32` `mip:u32` `origin_x:u32` `origin_y:u32` `origin_z:u32` `size_w:u32` `size_h:u32` `size_d:u32` |
| `0x32` | CopyTextureToBuffer | (inverse of 0x31) |
| `0x33` | CopyTextureToTexture | `src_tex:u32` `src_mip:u32` `src_origin:u32[3]` `dst_tex:u32` `dst_mip:u32` `dst_origin:u32[3]` `size:u32[3]` |

#### Control

| Opcode | Name | Payload |
|--------|------|---------|
| `0xFF` | FinishEncoder | (none) — marks encoder boundary |

### 6.4 Design Rationale

- **No per-command length**: Saves 2-4 bytes per command. With 200 commands/frame,
  that's 400-800 bytes/frame saved. More importantly, no branch to read/skip length.
- **Fixed-size payloads**: The decoder does a single `match opcode` then reads a
  known number of bytes. No parsing, no allocation.
- **Variable-length via count**: Only `BeginRenderPass` (N color attachments) and
  `SetBindGroup` (N dynamic offsets) are variable. Both use an inline count byte/u32.
- **FinishEncoder delimiter**: Allows multiple CommandBuffers in one stream without
  needing a separate "begin encoder" opcode.

### 6.5 Performance Budget

For a moderately complex 3D scene (100 draw calls, 5 render passes, 200 total commands):

| Metric | Budget | Expected |
|--------|--------|----------|
| Stream size | — | ~3 KB (200 cmds × ~15 bytes avg) |
| Vo-side recording (append bytes) | < 0.2 ms | ~0.05 ms |
| Boundary crossing (1 call) | < 0.1 ms | ~0.005 ms (native), ~0.05 ms (web) |
| Rust-side decode + execute | < 0.8 ms | ~0.3 ms |
| **Total bridge overhead** | **< 1.0 ms** | **~0.4 ms** |

Compare to old design (200 individual extern calls with JSON):
- Old: 200 × (JSON serialize + boundary + JSON deserialize) ≈ 4–16 ms
- New: 1 × (binary append + boundary + binary decode) ≈ 0.4 ms
- **Speedup: 10–40×**

---

## 7. Vo API — Types (`types.vo`)

### 7.1 Rust-Side Handles (opaque, obtained via extern calls)

```vo
package wgpu

type Adapter struct         { handle uint32 }
type Device struct           { handle uint32 }
type Queue struct            { handle uint32 }
type Buffer struct           { handle uint32 }
type Texture struct          { handle uint32 }
type TextureView struct      { handle uint32 }
type Sampler struct          { handle uint32 }
type ShaderModule struct     { handle uint32 }
type BindGroupLayout struct  { handle uint32 }
type BindGroup struct        { handle uint32 }
type PipelineLayout struct   { handle uint32 }
type RenderPipeline struct   { handle uint32 }
type ComputePipeline struct  { handle uint32 }
type Surface struct          { handle uint32 }
```

### 7.2 Vo-Side Command Types (NO Rust handle, NO boundary crossing)

```vo
// CommandEncoder records GPU commands into a local byte buffer.
// All methods append binary opcodes — zero extern calls until Submit.
type CommandEncoder struct {
    buf []byte
}

// RenderPassEncoder records render pass commands into the parent encoder's buffer.
type RenderPassEncoder struct {
    enc *CommandEncoder
}

// ComputePassEncoder records compute pass commands into the parent encoder's buffer.
type ComputePassEncoder struct {
    enc *CommandEncoder
}

// CommandBuffer is the finished output of a CommandEncoder.
// It is an opaque byte slice passed to Queue.Submit().
type CommandBuffer struct {
    data []byte
}

// Color represents an RGBA color with f64 components (matching wgpu::Color).
type Color struct {
    R float64
    G float64
    B float64
    A float64
}
```

### 7.3 Enums

```vo
type TextureFormat uint32
const (
    FormatBGRA8Unorm          TextureFormat = 1
    FormatRGBA8Unorm          TextureFormat = 2
    FormatBGRA8UnormSrgb      TextureFormat = 3
    FormatRGBA8UnormSrgb      TextureFormat = 4
    FormatRGBA16Float         TextureFormat = 5
    FormatDepth24Plus         TextureFormat = 20
    FormatDepth32Float        TextureFormat = 21
    FormatDepth24PlusStencil8 TextureFormat = 22
)

type BufferUsage uint32
const (
    BufferUsageMapRead      BufferUsage = 0x0001
    BufferUsageMapWrite     BufferUsage = 0x0002
    BufferUsageCopySrc      BufferUsage = 0x0004
    BufferUsageCopyDst      BufferUsage = 0x0008
    BufferUsageIndex        BufferUsage = 0x0010
    BufferUsageVertex       BufferUsage = 0x0020
    BufferUsageUniform      BufferUsage = 0x0040
    BufferUsageStorage      BufferUsage = 0x0080
    BufferUsageIndirect     BufferUsage = 0x0100
    BufferUsageQueryResolve BufferUsage = 0x0200
)

type TextureUsage uint32
const (
    TextureUsageCopySrc          TextureUsage = 0x01
    TextureUsageCopyDst          TextureUsage = 0x02
    TextureUsageTextureBinding   TextureUsage = 0x04
    TextureUsageStorageBinding   TextureUsage = 0x08
    TextureUsageRenderAttachment TextureUsage = 0x10
)

type ShaderStage uint32
const (
    ShaderStageVertex   ShaderStage = 0x1
    ShaderStageFragment ShaderStage = 0x2
    ShaderStageCompute  ShaderStage = 0x4
)

type PrimitiveTopology uint32
const (
    TopologyPointList     PrimitiveTopology = 0
    TopologyLineList      PrimitiveTopology = 1
    TopologyLineStrip     PrimitiveTopology = 2
    TopologyTriangleList  PrimitiveTopology = 3
    TopologyTriangleStrip PrimitiveTopology = 4
)

type IndexFormat uint32
const (
    IndexUint16 IndexFormat = 0
    IndexUint32 IndexFormat = 1
)

type MapMode uint32
const (
    MapRead  MapMode = 0x0001
    MapWrite MapMode = 0x0002
)

type LoadOp uint32
const (
    LoadOpLoad  LoadOp = 0
    LoadOpClear LoadOp = 1
)

type StoreOp uint32
const (
    StoreOpStore   StoreOp = 0
    StoreOpDiscard StoreOp = 1
)

type VertexFormat uint32
const (
    VertexFloat32   VertexFormat = 0
    VertexFloat32x2 VertexFormat = 1
    VertexFloat32x3 VertexFormat = 2
    VertexFloat32x4 VertexFormat = 3
    VertexSint32    VertexFormat = 10
    VertexUint32    VertexFormat = 11
)

type VertexStepMode uint32
const (
    StepModeVertex   VertexStepMode = 0
    StepModeInstance VertexStepMode = 1
)

type BindingType uint32
const (
    BindingUniformBuffer  BindingType = 0
    BindingStorageBuffer  BindingType = 1
    BindingTexture        BindingType = 2
    BindingSampler        BindingType = 3
    BindingStorageTexture BindingType = 4
)
```

### 7.4 Descriptor Structs

```vo
type BufferDescriptor struct {
    Size             uint64      `json:"size"`
    Usage            BufferUsage `json:"usage"`
    MappedAtCreation bool        `json:"mapped_at_creation,omitempty"`
}

type TextureDescriptor struct {
    Width     uint32        `json:"width"`
    Height    uint32        `json:"height"`
    Depth     uint32        `json:"depth,omitempty"`
    MipLevels uint32        `json:"mip_levels,omitempty"`
    Format    TextureFormat `json:"format"`
    Usage     TextureUsage  `json:"usage"`
}

type SamplerDescriptor struct {
    AddressModeU string `json:"address_mode_u,omitempty"`
    AddressModeV string `json:"address_mode_v,omitempty"`
    MagFilter    string `json:"mag_filter,omitempty"`
    MinFilter    string `json:"min_filter,omitempty"`
    MipmapFilter string `json:"mipmap_filter,omitempty"`
}

type VertexAttribute struct {
    Format         VertexFormat `json:"format"`
    Offset         uint64       `json:"offset"`
    ShaderLocation uint32       `json:"shader_location"`
}

type VertexBufferLayout struct {
    ArrayStride uint64            `json:"array_stride"`
    StepMode    VertexStepMode    `json:"step_mode"`
    Attributes  []VertexAttribute `json:"attributes"`
}

type BindGroupLayoutEntry struct {
    Binding    uint32      `json:"binding"`
    Visibility ShaderStage `json:"visibility"`
    Type       BindingType `json:"type"`
}

type BindGroupEntry struct {
    Binding uint32      `json:"binding"`
    Buffer  Buffer      `json:"buffer,omitempty"`
    Texture TextureView `json:"texture,omitempty"`
    Sampler Sampler     `json:"sampler,omitempty"`
    Offset  uint64      `json:"offset,omitempty"`
    Size    uint64      `json:"size,omitempty"`
}

type ColorTargetState struct {
    Format TextureFormat `json:"format"`
}

type PrimitiveState struct {
    Topology PrimitiveTopology `json:"topology,omitempty"`
}

type DepthStencilState struct {
    Format            TextureFormat `json:"format"`
    DepthWriteEnabled bool          `json:"depth_write_enabled"`
    DepthCompare      string        `json:"depth_compare"`
}

type ColorAttachment struct {
    View       TextureView `json:"view"`
    ResolveTarget TextureView `json:"resolve_target,omitempty"`
    LoadOp     LoadOp      `json:"load_op"`
    StoreOp    StoreOp     `json:"store_op"`
    ClearColor Color       `json:"clear_color,omitempty"`
}

type DepthStencilAttachment struct {
    View            TextureView `json:"view"`
    DepthLoadOp     LoadOp      `json:"depth_load_op"`
    DepthStoreOp    StoreOp     `json:"depth_store_op"`
    DepthClearValue float32     `json:"depth_clear_value,omitempty"`
    StencilClearValue uint32    `json:"stencil_clear_value,omitempty"`
}

type RenderPassDescriptor struct {
    ColorAttachments []ColorAttachment       `json:"color_attachments"`
    DepthAttachment  DepthStencilAttachment  `json:"depth_attachment,omitempty"`
}

type RenderPipelineDescriptor struct {
    Layout          PipelineLayout       `json:"layout"`
    VertexModule    ShaderModule         `json:"vertex_module"`
    VertexEntry     string               `json:"vertex_entry"`
    VertexBuffers   []VertexBufferLayout `json:"vertex_buffers,omitempty"`
    FragmentModule  ShaderModule         `json:"fragment_module,omitempty"`
    FragmentEntry   string               `json:"fragment_entry,omitempty"`
    FragmentTargets []ColorTargetState   `json:"fragment_targets,omitempty"`
    Primitive       PrimitiveState       `json:"primitive,omitempty"`
    DepthStencil    DepthStencilState    `json:"depth_stencil,omitempty"`
}

type ComputePipelineDescriptor struct {
    Layout     PipelineLayout `json:"layout"`
    Module     ShaderModule   `json:"module"`
    EntryPoint string         `json:"entry_point"`
}
```

---

## 8. Vo API — Methods (`wgpu.vo`)

### 8.1 Binary Encoding Helpers (`cmd.vo`, internal)

These are used by `CommandEncoder` and pass encoders to write binary commands.
Each is a trivial byte-append — no allocation beyond the slice growth.

```vo
package wgpu

import "math"

func (enc *CommandEncoder) writeU8(v uint8) {
    enc.buf = append(enc.buf, v)
}

func (enc *CommandEncoder) writeU16(v uint16) {
    enc.buf = append(enc.buf, uint8(v), uint8(v>>8))
}

func (enc *CommandEncoder) writeU32(v uint32) {
    enc.buf = append(enc.buf,
        uint8(v), uint8(v>>8), uint8(v>>16), uint8(v>>24))
}

func (enc *CommandEncoder) writeI32(v int32) {
    enc.writeU32(uint32(v))
}

func (enc *CommandEncoder) writeU64(v uint64) {
    enc.buf = append(enc.buf,
        uint8(v), uint8(v>>8), uint8(v>>16), uint8(v>>24),
        uint8(v>>32), uint8(v>>40), uint8(v>>48), uint8(v>>56))
}

func (enc *CommandEncoder) writeF32(v float32) {
    enc.writeU32(math.Float32bits(v))
}

func (enc *CommandEncoder) writeF64(v float64) {
    enc.writeU64(math.Float64bits(v))
}

func (enc *CommandEncoder) writePad16() {
    enc.buf = append(enc.buf, 0, 0)
}
```

### 8.2 Initialization & Surface (Control Plane — JSON)

```vo
package wgpu

import (
    "encoding/json"
    "errors"
)

func decodeHandle(resp []byte) (uint32, error) {
    type r struct {
        Handle uint32 `json:"handle"`
        Error  string `json:"error,omitempty"`
    }
    var result r
    err := json.Unmarshal(resp, &result)
    if err != nil { return 0, err }
    if result.Error != "" { return 0, errors.New(result.Error) }
    return result.Handle, nil
}

func checkError(resp []byte) error {
    type r struct {
        Error string `json:"error,omitempty"`
    }
    var result r
    err := json.Unmarshal(resp, &result)
    if err != nil { return err }
    if result.Error != "" { return errors.New(result.Error) }
    return nil
}

func RequestAdapter() (Adapter, error) {
    resp := extRequestAdapter([]byte("{}"))?
    h := decodeHandle(resp)?
    return Adapter{handle: h}, nil
}

func (a Adapter) RequestDevice() (Device, error) {
    req, _ := json.Marshal(struct{ Adapter uint32 `json:"adapter"` }{a.handle})
    resp := extRequestDevice(req)?
    h := decodeHandle(resp)?
    return Device{handle: h}, nil
}

func (d Device) GetQueue() (Queue, error) {
    req, _ := json.Marshal(struct{ Device uint32 `json:"device"` }{d.handle})
    resp := extGetQueue(req)?
    h := decodeHandle(resp)?
    return Queue{handle: h}, nil
}

func CreateSurface(canvasID string) (Surface, error) {
    req, _ := json.Marshal(struct{ CanvasID string `json:"canvas_id"` }{canvasID})
    resp := extCreateSurface(req)?
    h := decodeHandle(resp)?
    return Surface{handle: h}, nil
}

func (s Surface) Configure(d Device, format TextureFormat, width, height uint32) error {
    req, _ := json.Marshal(struct {
        Surface uint32 `json:"surface"`
        Device  uint32 `json:"device"`
        Format  uint32 `json:"format"`
        Width   uint32 `json:"width"`
        Height  uint32 `json:"height"`
    }{s.handle, d.handle, uint32(format), width, height})
    resp := extConfigureSurface(req)?
    return checkError(resp)
}

func (s Surface) GetCurrentTexture() (TextureView, error) {
    req, _ := json.Marshal(struct{ Surface uint32 `json:"surface"` }{s.handle})
    resp := extGetCurrentTexture(req)?
    h := decodeHandle(resp)?
    return TextureView{handle: h}, nil
}

func (s Surface) GetPreferredFormat(a Adapter) (TextureFormat, error) {
    req, _ := json.Marshal(struct {
        Surface uint32 `json:"surface"`
        Adapter uint32 `json:"adapter"`
    }{s.handle, a.handle})
    resp := extGetPreferredFormat(req)?
    type r struct{ Format uint32 `json:"format"` }
    var result r
    err := json.Unmarshal(resp, &result)
    if err != nil { return 0, err }
    return TextureFormat(result.Format), nil
}

func (s Surface) Present() error {
    req, _ := json.Marshal(struct{ Surface uint32 `json:"surface"` }{s.handle})
    resp := extPresent(req)?
    return checkError(resp)
}
```

### 8.3 Resource Creation (Control Plane — JSON)

```vo
func (d Device) CreateBuffer(desc BufferDescriptor) (Buffer, error) {
    req, _ := json.Marshal(struct {
        Device uint32 `json:"device"`
        BufferDescriptor
    }{d.handle, desc})
    resp := extCreateBuffer(req)?
    h := decodeHandle(resp)?
    return Buffer{handle: h}, nil
}

func (d Device) CreateTexture(desc TextureDescriptor) (Texture, error) {
    req, _ := json.Marshal(struct {
        Device uint32 `json:"device"`
        TextureDescriptor
    }{d.handle, desc})
    resp := extCreateTexture(req)?
    h := decodeHandle(resp)?
    return Texture{handle: h}, nil
}

func (d Device) CreateSampler(desc SamplerDescriptor) (Sampler, error) {
    req, _ := json.Marshal(struct {
        Device uint32 `json:"device"`
        SamplerDescriptor
    }{d.handle, desc})
    resp := extCreateSampler(req)?
    h := decodeHandle(resp)?
    return Sampler{handle: h}, nil
}

func (d Device) CreateShaderModule(code string) (ShaderModule, error) {
    req, _ := json.Marshal(struct {
        Device uint32 `json:"device"`
        Code   string `json:"code"`
    }{d.handle, code})
    resp := extCreateShaderModule(req)?
    h := decodeHandle(resp)?
    return ShaderModule{handle: h}, nil
}

func (d Device) CreateBindGroupLayout(entries []BindGroupLayoutEntry) (BindGroupLayout, error) {
    req, _ := json.Marshal(struct {
        Device  uint32                 `json:"device"`
        Entries []BindGroupLayoutEntry `json:"entries"`
    }{d.handle, entries})
    resp := extCreateBindGroupLayout(req)?
    h := decodeHandle(resp)?
    return BindGroupLayout{handle: h}, nil
}

func (d Device) CreatePipelineLayout(layouts []BindGroupLayout) (PipelineLayout, error) {
    handles := make([]uint32, len(layouts))
    for i, l := range layouts { handles[i] = l.handle }
    req, _ := json.Marshal(struct {
        Device  uint32   `json:"device"`
        Layouts []uint32 `json:"layouts"`
    }{d.handle, handles})
    resp := extCreatePipelineLayout(req)?
    h := decodeHandle(resp)?
    return PipelineLayout{handle: h}, nil
}

func (d Device) CreateBindGroup(layout BindGroupLayout, entries []BindGroupEntry) (BindGroup, error) {
    req, _ := json.Marshal(struct {
        Device  uint32           `json:"device"`
        Layout  uint32           `json:"layout"`
        Entries []BindGroupEntry `json:"entries"`
    }{d.handle, layout.handle, entries})
    resp := extCreateBindGroup(req)?
    h := decodeHandle(resp)?
    return BindGroup{handle: h}, nil
}

func (d Device) CreateRenderPipeline(desc RenderPipelineDescriptor) (RenderPipeline, error) {
    req, _ := json.Marshal(struct {
        Device uint32 `json:"device"`
        RenderPipelineDescriptor
    }{d.handle, desc})
    resp := extCreateRenderPipeline(req)?
    h := decodeHandle(resp)?
    return RenderPipeline{handle: h}, nil
}

func (d Device) CreateComputePipeline(desc ComputePipelineDescriptor) (ComputePipeline, error) {
    req, _ := json.Marshal(struct {
        Device uint32 `json:"device"`
        ComputePipelineDescriptor
    }{d.handle, desc})
    resp := extCreateComputePipeline(req)?
    h := decodeHandle(resp)?
    return ComputePipeline{handle: h}, nil
}

func (t Texture) CreateView() (TextureView, error) {
    req, _ := json.Marshal(struct{ Texture uint32 `json:"texture"` }{t.handle})
    resp := extCreateTextureView(req)?
    h := decodeHandle(resp)?
    return TextureView{handle: h}, nil
}
```

### 8.4 Command Recording (Command Plane — binary, ZERO extern calls)

This is the core of the performance design. Every method here only appends
bytes to `enc.buf`. No JSON, no extern call, no allocation beyond slice growth.

```vo
// NewCommandEncoder creates a Vo-side command encoder.
// No Rust object is created until Queue.Submit().
func NewCommandEncoder() *CommandEncoder {
    return &CommandEncoder{buf: make([]byte, 0, 1024)}
}

// Finish seals the encoder and returns a CommandBuffer (byte slice).
func (enc *CommandEncoder) Finish() CommandBuffer {
    enc.writeU8(0xFF) // OP_FINISH
    return CommandBuffer{data: enc.buf}
}
```

#### Render Pass

```vo
func (enc *CommandEncoder) BeginRenderPass(desc RenderPassDescriptor) *RenderPassEncoder {
    enc.writeU8(0x01) // OP_BEGIN_RENDER_PASS
    enc.writeU8(uint8(len(desc.ColorAttachments)))
    hasDepth := uint8(0)
    if desc.DepthAttachment.View.handle != 0 { hasDepth = 1 }
    enc.writeU8(hasDepth)
    enc.writePad16() // alignment padding (1 byte opcode + 1 + 1 + 2 pad = 5... let me fix)

    for _, ca := range desc.ColorAttachments {
        enc.writeU32(ca.View.handle)
        enc.writeU32(ca.ResolveTarget.handle)
        enc.writeU8(uint8(ca.LoadOp))
        enc.writeU8(uint8(ca.StoreOp))
        enc.writePad16()
        enc.writeF64(ca.ClearColor.R)
        enc.writeF64(ca.ClearColor.G)
        enc.writeF64(ca.ClearColor.B)
        enc.writeF64(ca.ClearColor.A)
    }

    if hasDepth == 1 {
        da := desc.DepthAttachment
        enc.writeU32(da.View.handle)
        enc.writeU8(uint8(da.DepthLoadOp))
        enc.writeU8(uint8(da.DepthStoreOp))
        enc.writeU8(0) // stencil_load (reserved)
        enc.writeU8(0) // stencil_store (reserved)
        enc.writeF32(da.DepthClearValue)
        enc.writeU32(da.StencilClearValue)
    }

    return &RenderPassEncoder{enc: enc}
}

func (rp *RenderPassEncoder) SetPipeline(p RenderPipeline) {
    rp.enc.writeU8(0x03)
    rp.enc.writeU32(p.handle)
}

func (rp *RenderPassEncoder) SetBindGroup(index uint32, bg BindGroup) {
    rp.enc.writeU8(0x04)
    rp.enc.writeU32(index)
    rp.enc.writeU32(bg.handle)
    rp.enc.writeU32(0) // num_dynamic_offsets = 0
}

func (rp *RenderPassEncoder) SetBindGroupDynamic(index uint32, bg BindGroup, offsets []uint32) {
    rp.enc.writeU8(0x04)
    rp.enc.writeU32(index)
    rp.enc.writeU32(bg.handle)
    rp.enc.writeU32(uint32(len(offsets)))
    for _, off := range offsets {
        rp.enc.writeU32(off)
    }
}

func (rp *RenderPassEncoder) SetVertexBuffer(slot uint32, buf Buffer, offset, size uint64) {
    rp.enc.writeU8(0x05)
    rp.enc.writeU32(slot)
    rp.enc.writeU32(buf.handle)
    rp.enc.writeU64(offset)
    rp.enc.writeU64(size)
}

func (rp *RenderPassEncoder) SetIndexBuffer(buf Buffer, format IndexFormat, offset, size uint64) {
    rp.enc.writeU8(0x06)
    rp.enc.writeU32(buf.handle)
    rp.enc.writeU8(uint8(format))
    rp.enc.writeU8(0); rp.enc.writeU8(0); rp.enc.writeU8(0) // pad to 4-byte align
    rp.enc.writeU64(offset)
    rp.enc.writeU64(size)
}

func (rp *RenderPassEncoder) Draw(vertexCount, instanceCount, firstVertex, firstInstance uint32) {
    rp.enc.writeU8(0x07)
    rp.enc.writeU32(vertexCount)
    rp.enc.writeU32(instanceCount)
    rp.enc.writeU32(firstVertex)
    rp.enc.writeU32(firstInstance)
}

func (rp *RenderPassEncoder) DrawIndexed(indexCount, instanceCount, firstIndex uint32, baseVertex int32, firstInstance uint32) {
    rp.enc.writeU8(0x08)
    rp.enc.writeU32(indexCount)
    rp.enc.writeU32(instanceCount)
    rp.enc.writeU32(firstIndex)
    rp.enc.writeI32(baseVertex)
    rp.enc.writeU32(firstInstance)
}

func (rp *RenderPassEncoder) SetViewport(x, y, w, h, minDepth, maxDepth float32) {
    rp.enc.writeU8(0x09)
    rp.enc.writeF32(x); rp.enc.writeF32(y)
    rp.enc.writeF32(w); rp.enc.writeF32(h)
    rp.enc.writeF32(minDepth); rp.enc.writeF32(maxDepth)
}

func (rp *RenderPassEncoder) SetScissorRect(x, y, w, h uint32) {
    rp.enc.writeU8(0x0A)
    rp.enc.writeU32(x); rp.enc.writeU32(y)
    rp.enc.writeU32(w); rp.enc.writeU32(h)
}

func (rp *RenderPassEncoder) SetBlendConstant(c Color) {
    rp.enc.writeU8(0x0B)
    rp.enc.writeF64(c.R); rp.enc.writeF64(c.G)
    rp.enc.writeF64(c.B); rp.enc.writeF64(c.A)
}

func (rp *RenderPassEncoder) SetStencilReference(ref uint32) {
    rp.enc.writeU8(0x0C)
    rp.enc.writeU32(ref)
}

func (rp *RenderPassEncoder) DrawIndirect(buf Buffer, offset uint64) {
    rp.enc.writeU8(0x0D)
    rp.enc.writeU32(buf.handle)
    rp.enc.writeU64(offset)
}

func (rp *RenderPassEncoder) DrawIndexedIndirect(buf Buffer, offset uint64) {
    rp.enc.writeU8(0x0E)
    rp.enc.writeU32(buf.handle)
    rp.enc.writeU64(offset)
}

func (rp *RenderPassEncoder) End() {
    rp.enc.writeU8(0x02) // OP_END_RENDER_PASS
}
```

#### Compute Pass

```vo
func (enc *CommandEncoder) BeginComputePass() *ComputePassEncoder {
    enc.writeU8(0x20) // OP_BEGIN_COMPUTE_PASS
    return &ComputePassEncoder{enc: enc}
}

func (cp *ComputePassEncoder) SetPipeline(p ComputePipeline) {
    cp.enc.writeU8(0x22)
    cp.enc.writeU32(p.handle)
}

func (cp *ComputePassEncoder) SetBindGroup(index uint32, bg BindGroup) {
    cp.enc.writeU8(0x23)
    cp.enc.writeU32(index)
    cp.enc.writeU32(bg.handle)
    cp.enc.writeU32(0) // num_dynamic_offsets = 0
}

func (cp *ComputePassEncoder) SetBindGroupDynamic(index uint32, bg BindGroup, offsets []uint32) {
    cp.enc.writeU8(0x23)
    cp.enc.writeU32(index)
    cp.enc.writeU32(bg.handle)
    cp.enc.writeU32(uint32(len(offsets)))
    for _, off := range offsets {
        cp.enc.writeU32(off)
    }
}

func (cp *ComputePassEncoder) Dispatch(x, y, z uint32) {
    cp.enc.writeU8(0x24)
    cp.enc.writeU32(x); cp.enc.writeU32(y); cp.enc.writeU32(z)
}

func (cp *ComputePassEncoder) DispatchIndirect(buf Buffer, offset uint64) {
    cp.enc.writeU8(0x25)
    cp.enc.writeU32(buf.handle)
    cp.enc.writeU64(offset)
}

func (cp *ComputePassEncoder) End() {
    cp.enc.writeU8(0x21) // OP_END_COMPUTE_PASS
}
```

#### Encoder-Level Copy Commands

```vo
func (enc *CommandEncoder) CopyBufferToBuffer(src Buffer, srcOff uint64, dst Buffer, dstOff uint64, size uint64) {
    enc.writeU8(0x30)
    enc.writeU32(src.handle); enc.writeU64(srcOff)
    enc.writeU32(dst.handle); enc.writeU64(dstOff)
    enc.writeU64(size)
}

func (enc *CommandEncoder) CopyBufferToTexture(
    srcBuf Buffer, srcOffset uint64, bytesPerRow, rowsPerImage uint32,
    dstTex Texture, mip, originX, originY, originZ, sizeW, sizeH, sizeD uint32,
) {
    enc.writeU8(0x31)
    enc.writeU32(srcBuf.handle); enc.writeU64(srcOffset)
    enc.writeU32(bytesPerRow); enc.writeU32(rowsPerImage)
    enc.writeU32(dstTex.handle); enc.writeU32(mip)
    enc.writeU32(originX); enc.writeU32(originY); enc.writeU32(originZ)
    enc.writeU32(sizeW); enc.writeU32(sizeH); enc.writeU32(sizeD)
}

func (enc *CommandEncoder) CopyTextureToBuffer(
    srcTex Texture, srcMip, srcX, srcY, srcZ uint32,
    dstBuf Buffer, dstOffset uint64, bytesPerRow, rowsPerImage uint32,
    sizeW, sizeH, sizeD uint32,
) {
    enc.writeU8(0x32)
    enc.writeU32(srcTex.handle); enc.writeU32(srcMip)
    enc.writeU32(srcX); enc.writeU32(srcY); enc.writeU32(srcZ)
    enc.writeU32(dstBuf.handle); enc.writeU64(dstOffset)
    enc.writeU32(bytesPerRow); enc.writeU32(rowsPerImage)
    enc.writeU32(sizeW); enc.writeU32(sizeH); enc.writeU32(sizeD)
}

func (enc *CommandEncoder) CopyTextureToTexture(
    src Texture, srcMip, srcX, srcY, srcZ uint32,
    dst Texture, dstMip, dstX, dstY, dstZ uint32,
    sizeW, sizeH, sizeD uint32,
) {
    enc.writeU8(0x33)
    enc.writeU32(src.handle); enc.writeU32(srcMip)
    enc.writeU32(srcX); enc.writeU32(srcY); enc.writeU32(srcZ)
    enc.writeU32(dst.handle); enc.writeU32(dstMip)
    enc.writeU32(dstX); enc.writeU32(dstY); enc.writeU32(dstZ)
    enc.writeU32(sizeW); enc.writeU32(sizeH); enc.writeU32(sizeD)
}
```

### 8.5 Queue — Submission & Data Transfer

```vo
// Submit sends all recorded commands to the GPU in ONE extern call.
// This is the only per-frame boundary crossing for commands.
func (q Queue) Submit(device Device, cmds []CommandBuffer) error {
    // Calculate total stream size
    total := 16 // header: queue(4) + device(4) + magic(4) + version(2) + num_enc(2)
    for _, c := range cmds {
        total += len(c.data)
    }

    stream := make([]byte, 0, total)

    // Preamble: queue + device handles
    stream = appendU32(stream, q.handle)
    stream = appendU32(stream, device.handle)

    // Stream header
    stream = appendU32(stream, 0x564F4750) // magic "VOGP"
    stream = appendU16(stream, 1)           // version
    stream = appendU16(stream, uint16(len(cmds))) // num_encoders

    // Concatenate all CommandBuffer data (each ends with OP_FINISH)
    for _, c := range cmds {
        stream = append(stream, c.data...)
    }

    resp := extSubmitCommands(stream)?
    return checkError(resp)
}

// WriteBuffer uploads raw bytes to a GPU buffer. ONE extern call, no encoding.
func (q Queue) WriteBuffer(buf Buffer, offset uint64, data []byte) error {
    // Pack header: queue(4) + buffer(4) + offset(8) = 16 bytes, then raw data
    header := make([]byte, 0, 16+len(data))
    header = appendU32(header, q.handle)
    header = appendU32(header, buf.handle)
    header = appendU64(header, offset)
    header = append(header, data...)

    resp := extWriteBuffer(header)?
    return checkError(resp)
}

// WriteTexture uploads raw pixel data to a GPU texture. ONE extern call.
func (q Queue) WriteTexture(
    tex Texture, mip, originX, originY, originZ uint32,
    bytesPerRow, rowsPerImage, width, height, depth uint32,
    data []byte,
) error {
    // Pack header (40 bytes) then raw pixel data
    header := make([]byte, 0, 44+len(data))
    header = appendU32(header, q.handle)
    header = appendU32(header, tex.handle)
    header = appendU32(header, mip)
    header = appendU32(header, originX)
    header = appendU32(header, originY)
    header = appendU32(header, originZ)
    header = appendU32(header, bytesPerRow)
    header = appendU32(header, rowsPerImage)
    header = appendU32(header, width)
    header = appendU32(header, height)
    header = appendU32(header, depth)
    header = append(header, data...)

    resp := extWriteTexture(header)?
    return checkError(resp)
}

// appendU32/appendU16/appendU64 are package-level helpers (same as enc.writeU32 but for []byte)
func appendU32(b []byte, v uint32) []byte {
    return append(b, uint8(v), uint8(v>>8), uint8(v>>16), uint8(v>>24))
}

func appendU16(b []byte, v uint16) []byte {
    return append(b, uint8(v), uint8(v>>8))
}

func appendU64(b []byte, v uint64) []byte {
    return append(b,
        uint8(v), uint8(v>>8), uint8(v>>16), uint8(v>>24),
        uint8(v>>32), uint8(v>>40), uint8(v>>48), uint8(v>>56))
}
```

### 8.6 Resource Management

```vo
func (b Buffer) Destroy() {
    extDestroyResource([]byte(`{"type":"buffer","handle":` + uitoa(b.handle) + `}`))
}

func (t Texture) Destroy() {
    extDestroyResource([]byte(`{"type":"texture","handle":` + uitoa(t.handle) + `}`))
}

func (b Buffer) MapAsync(mode MapMode, offset, size uint64) error {
    req, _ := json.Marshal(struct {
        Buffer uint32 `json:"buffer"`
        Mode   uint32 `json:"mode"`
        Offset uint64 `json:"offset"`
        Size   uint64 `json:"size"`
    }{b.handle, uint32(mode), offset, size})
    resp := extMapBufferAsync(req)?
    return checkError(resp)
}

func (b Buffer) GetMappedRange(offset, size uint64) ([]byte, error) {
    // Binary header: buffer(4) + offset(8) + size(8) = 20 bytes
    header := make([]byte, 0, 20)
    header = appendU32(header, b.handle)
    header = appendU64(header, offset)
    header = appendU64(header, size)
    return extReadBuffer(header)
}

func (b Buffer) Unmap() {
    req, _ := json.Marshal(struct{ Buffer uint32 `json:"buffer"` }{b.handle})
    extUnmapBuffer(req)
}
```

---

## 9. Extern Functions (Complete List)

All externs have uniform signature: `([]byte) -> ([]byte, error)`.
This maps cleanly to both web (`fn(input: &[u8]) -> Vec<u8>`) and native
(`#[vo_extern_ctx]` reading/writing byte slots).

### 9.1 Control Plane (JSON in/out) — 18 functions

```vo
// Initialization
func extRequestAdapter(input []byte) ([]byte, error)
func extRequestDevice(input []byte) ([]byte, error)
func extGetQueue(input []byte) ([]byte, error)

// Surface
func extCreateSurface(input []byte) ([]byte, error)
func extConfigureSurface(input []byte) ([]byte, error)
func extGetCurrentTexture(input []byte) ([]byte, error)
func extGetPreferredFormat(input []byte) ([]byte, error)
func extPresent(input []byte) ([]byte, error)

// Resources
func extCreateBuffer(input []byte) ([]byte, error)
func extCreateTexture(input []byte) ([]byte, error)
func extCreateTextureView(input []byte) ([]byte, error)
func extCreateSampler(input []byte) ([]byte, error)
func extCreateShaderModule(input []byte) ([]byte, error)
func extCreateBindGroupLayout(input []byte) ([]byte, error)
func extCreatePipelineLayout(input []byte) ([]byte, error)
func extCreateBindGroup(input []byte) ([]byte, error)
func extCreateRenderPipeline(input []byte) ([]byte, error)
func extCreateComputePipeline(input []byte) ([]byte, error)
```

### 9.2 Command Plane (binary in, JSON error out) — 1 function

```vo
func extSubmitCommands(stream []byte) ([]byte, error)
```

### 9.3 Data Plane (binary in/out) — 4 functions

```vo
func extWriteBuffer(input []byte) ([]byte, error)     // header + raw data
func extWriteTexture(input []byte) ([]byte, error)     // header + raw pixels
func extReadBuffer(input []byte) ([]byte, error)       // returns raw bytes
func extMapBufferAsync(input []byte) ([]byte, error)   // JSON (async)
```

### 9.4 Resource Management — 2 functions

```vo
func extDestroyResource(input []byte) ([]byte, error)  // JSON: {"type":"...", "handle":N}
func extUnmapBuffer(input []byte) ([]byte, error)      // JSON
```

**Total: 25 extern functions** (down from 38 in v1).

### 9.5 Per-Frame Extern Call Count

| Scenario | Extern calls/frame |
|----------|-------------------|
| Simple triangle (no data upload) | 2 (GetCurrentTexture + Submit) |
| Typical scene (uniform update) | 3 (GetCurrentTexture + WriteBuffer + Submit) |
| Complex scene (dynamic geometry + texture streaming) | 4-5 |
| Worst case (many buffer uploads) | N+2 where N = upload count |

Compare v1: **minimum 8 calls** for a trivial triangle, **200+** for a real scene.

---

## 10. Rust Backend — Shared Components

### 10.1 Opcode Constants (`stream.rs`)

```rust
// stream.rs — shared between web and native backends

pub const STREAM_MAGIC: u32 = 0x564F4750; // "VOGP"
pub const STREAM_VERSION: u16 = 1;
pub const STREAM_HEADER_SIZE: usize = 16; // queue(4) + device(4) + magic(4) + ver(2) + num_enc(2)

// Render pass
pub const OP_BEGIN_RENDER_PASS: u8 = 0x01;
pub const OP_END_RENDER_PASS: u8 = 0x02;
pub const OP_RP_SET_PIPELINE: u8 = 0x03;
pub const OP_RP_SET_BIND_GROUP: u8 = 0x04;
pub const OP_RP_SET_VERTEX_BUFFER: u8 = 0x05;
pub const OP_RP_SET_INDEX_BUFFER: u8 = 0x06;
pub const OP_RP_DRAW: u8 = 0x07;
pub const OP_RP_DRAW_INDEXED: u8 = 0x08;
pub const OP_RP_SET_VIEWPORT: u8 = 0x09;
pub const OP_RP_SET_SCISSOR_RECT: u8 = 0x0A;
pub const OP_RP_SET_BLEND_CONSTANT: u8 = 0x0B;
pub const OP_RP_SET_STENCIL_REF: u8 = 0x0C;
pub const OP_RP_DRAW_INDIRECT: u8 = 0x0D;
pub const OP_RP_DRAW_INDEXED_INDIRECT: u8 = 0x0E;

// Compute pass
pub const OP_BEGIN_COMPUTE_PASS: u8 = 0x20;
pub const OP_END_COMPUTE_PASS: u8 = 0x21;
pub const OP_CP_SET_PIPELINE: u8 = 0x22;
pub const OP_CP_SET_BIND_GROUP: u8 = 0x23;
pub const OP_CP_DISPATCH: u8 = 0x24;
pub const OP_CP_DISPATCH_INDIRECT: u8 = 0x25;

// Encoder-level
pub const OP_COPY_BUF_TO_BUF: u8 = 0x30;
pub const OP_COPY_BUF_TO_TEX: u8 = 0x31;
pub const OP_COPY_TEX_TO_BUF: u8 = 0x32;
pub const OP_COPY_TEX_TO_TEX: u8 = 0x33;

// Control
pub const OP_FINISH: u8 = 0xFF;
```

### 10.2 Stream Reader (`stream.rs`)

Zero-allocation cursor that reads from the byte slice:

```rust
pub struct StreamReader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> StreamReader<'a> {
    pub fn new(data: &'a [u8]) -> Self { Self { data, pos: 0 } }

    pub fn remaining(&self) -> usize { self.data.len() - self.pos }

    pub fn read_u8(&mut self) -> u8 {
        let v = self.data[self.pos];
        self.pos += 1;
        v
    }

    pub fn read_u16(&mut self) -> u16 {
        let v = u16::from_le_bytes(self.data[self.pos..self.pos+2].try_into().unwrap());
        self.pos += 2;
        v
    }

    pub fn read_u32(&mut self) -> u32 {
        let v = u32::from_le_bytes(self.data[self.pos..self.pos+4].try_into().unwrap());
        self.pos += 4;
        v
    }

    pub fn read_i32(&mut self) -> i32 {
        self.read_u32() as i32
    }

    pub fn read_u64(&mut self) -> u64 {
        let v = u64::from_le_bytes(self.data[self.pos..self.pos+8].try_into().unwrap());
        self.pos += 8;
        v
    }

    pub fn read_f32(&mut self) -> f32 {
        f32::from_bits(self.read_u32())
    }

    pub fn read_f64(&mut self) -> f64 {
        f64::from_bits(self.read_u64())
    }

    pub fn skip(&mut self, n: usize) {
        self.pos += n;
    }
}
```

### 10.3 Command Stream Executor (abstract pattern)

Both backends implement the same execution pattern. The executor creates real
GPU objects and dispatches commands in a single linear pass:

```rust
pub fn execute_stream(input: &[u8], state: &mut State) -> Result<(), String> {
    let mut r = StreamReader::new(input);

    // Read preamble
    let queue_handle = r.read_u32();
    let device_handle = r.read_u32();

    // Validate header
    let magic = r.read_u32();
    if magic != STREAM_MAGIC { return Err("invalid stream magic".into()); }
    let version = r.read_u16();
    if version != STREAM_VERSION { return Err("unsupported stream version".into()); }
    let num_encoders = r.read_u16();

    let device = state.devices.get(device_handle);
    let queue = state.queues.get(queue_handle);
    let mut command_buffers = Vec::with_capacity(num_encoders as usize);

    // Process encoders
    let mut encoder = device.create_command_encoder(&Default::default());
    let mut active_render_pass: Option<RenderPass> = None;
    let mut active_compute_pass: Option<ComputePass> = None;

    while r.remaining() > 0 {
        let op = r.read_u8();
        match op {
            OP_FINISH => {
                // Drop any active pass (shouldn't happen if well-formed)
                drop(active_render_pass.take());
                drop(active_compute_pass.take());
                command_buffers.push(encoder.finish());
                // Create next encoder if more data follows
                if r.remaining() > 0 {
                    encoder = device.create_command_encoder(&Default::default());
                }
            }

            OP_BEGIN_RENDER_PASS => {
                let num_colors = r.read_u8();
                let has_depth = r.read_u8() != 0;
                r.skip(2); // padding

                // Read color attachments
                let mut colors = Vec::with_capacity(num_colors as usize);
                for _ in 0..num_colors {
                    let view_h = r.read_u32();
                    let resolve_h = r.read_u32();
                    let load_op = r.read_u8();
                    let store_op = r.read_u8();
                    r.skip(2); // padding
                    let clear = [r.read_f64(), r.read_f64(), r.read_f64(), r.read_f64()];
                    colors.push(make_color_attachment(
                        state, view_h, resolve_h, load_op, store_op, clear,
                    ));
                }

                // Read depth attachment if present
                let depth = if has_depth {
                    let view_h = r.read_u32();
                    let d_load = r.read_u8();
                    let d_store = r.read_u8();
                    r.skip(2); // stencil load/store (reserved)
                    let clear_depth = r.read_f32();
                    let clear_stencil = r.read_u32();
                    Some(make_depth_attachment(
                        state, view_h, d_load, d_store, clear_depth, clear_stencil,
                    ))
                } else {
                    None
                };

                let desc = make_render_pass_desc(&colors, depth.as_ref());
                active_render_pass = Some(encoder.begin_render_pass(&desc));
            }

            OP_END_RENDER_PASS => {
                active_render_pass.take(); // drop ends the pass
            }

            OP_RP_SET_PIPELINE => {
                let p = r.read_u32();
                let pass = active_render_pass.as_mut().unwrap();
                pass.set_pipeline(state.render_pipelines.get(p));
            }

            OP_RP_DRAW => {
                let vc = r.read_u32();
                let ic = r.read_u32();
                let fv = r.read_u32();
                let fi = r.read_u32();
                let pass = active_render_pass.as_mut().unwrap();
                pass.draw(fv..fv+vc, fi..fi+ic);
            }

            // ... remaining opcodes follow same pattern ...

            OP_FINISH => { /* handled above */ }

            _ => return Err(format!("unknown opcode 0x{:02X}", op)),
        }
    }

    queue.submit(command_buffers);
    Ok(())
}
```

**Key properties of the executor**:
- **Single pass, no backtracking**: reads left to right, O(n) in stream size
- **Zero heap allocation** on the hot path (except wgpu's own internal allocs)
- **Fail-fast**: returns first error, discards rest of stream
- **No JSON parsing** — pure binary reads

### 10.4 Enum Mapping (`enums.rs`)

Shared between backends. Maps Vo `u32` constants to backend-specific enums:

```rust
pub fn texture_format_from_u32(v: u32) -> wgpu::TextureFormat {
    match v {
        1 => wgpu::TextureFormat::Bgra8Unorm,
        2 => wgpu::TextureFormat::Rgba8Unorm,
        3 => wgpu::TextureFormat::Bgra8UnormSrgb,
        4 => wgpu::TextureFormat::Rgba8UnormSrgb,
        5 => wgpu::TextureFormat::Rgba16Float,
        20 => wgpu::TextureFormat::Depth24Plus,
        21 => wgpu::TextureFormat::Depth32Float,
        22 => wgpu::TextureFormat::Depth24PlusStencil8,
        _ => panic!("wgpu: unknown TextureFormat {}", v),
    }
}

pub fn load_op_from_u8(v: u8) -> wgpu::LoadOp<wgpu::Color> {
    match v {
        0 => wgpu::LoadOp::Load,
        1 => wgpu::LoadOp::Clear(wgpu::Color::BLACK), // clear value set separately
        _ => panic!("wgpu: unknown LoadOp {}", v),
    }
}

pub fn store_op_from_u8(v: u8) -> wgpu::StoreOp {
    match v {
        0 => wgpu::StoreOp::Store,
        1 => wgpu::StoreOp::Discard,
        _ => panic!("wgpu: unknown StoreOp {}", v),
    }
}

pub fn primitive_topology_from_u32(v: u32) -> wgpu::PrimitiveTopology {
    match v {
        0 => wgpu::PrimitiveTopology::PointList,
        1 => wgpu::PrimitiveTopology::LineList,
        2 => wgpu::PrimitiveTopology::LineStrip,
        3 => wgpu::PrimitiveTopology::TriangleList,
        4 => wgpu::PrimitiveTopology::TriangleStrip,
        _ => panic!("wgpu: unknown PrimitiveTopology {}", v),
    }
}

pub fn index_format_from_u8(v: u8) -> wgpu::IndexFormat {
    match v {
        0 => wgpu::IndexFormat::Uint16,
        1 => wgpu::IndexFormat::Uint32,
        _ => panic!("wgpu: unknown IndexFormat {}", v),
    }
}

// Same pattern for: BufferUsages, TextureUsages, VertexFormat, VertexStepMode,
// ShaderStages, MapMode, CompareFunction, AddressMode, FilterMode
```

Web backend maps to `web_sys::Gpu*` enum types using the same u32/u8 constants.

---

## 11. Web Backend (`web/`)

### 11.1 Entry Point (`web/mod.rs`)

```rust
use wasm_bindgen::prelude::*;
use std::cell::RefCell;
use crate::registry::HandleRegistry;

mod init;
mod control;
mod executor;
mod data;

thread_local! {
    static STATE: RefCell<WebState> = RefCell::new(WebState::new());
}

pub(crate) struct WebState {
    pub adapters: HandleRegistry<web_sys::GpuAdapter>,
    pub devices: HandleRegistry<web_sys::GpuDevice>,
    pub queues: HandleRegistry<web_sys::GpuQueue>,
    pub buffers: HandleRegistry<web_sys::GpuBuffer>,
    pub textures: HandleRegistry<web_sys::GpuTexture>,
    pub texture_views: HandleRegistry<web_sys::GpuTextureView>,
    pub samplers: HandleRegistry<web_sys::GpuSampler>,
    pub shader_modules: HandleRegistry<web_sys::GpuShaderModule>,
    pub bind_group_layouts: HandleRegistry<web_sys::GpuBindGroupLayout>,
    pub bind_groups: HandleRegistry<web_sys::GpuBindGroup>,
    pub pipeline_layouts: HandleRegistry<web_sys::GpuPipelineLayout>,
    pub render_pipelines: HandleRegistry<web_sys::GpuRenderPipeline>,
    pub compute_pipelines: HandleRegistry<web_sys::GpuComputePipeline>,
    pub contexts: HandleRegistry<web_sys::GpuCanvasContext>,
    pub preferred_format: Option<web_sys::GpuTextureFormat>,
}
```

### 11.2 Async Init (`web/init.rs`)

```rust
#[wasm_bindgen(js_name = "__voInit")]
pub async fn vo_init() -> Result<(), JsValue> {
    // Pre-acquire adapter + device for fast first-frame startup
    let window = web_sys::window().ok_or("no window")?;
    let gpu = window.navigator().gpu();

    let adapter: web_sys::GpuAdapter = wasm_bindgen_futures::JsFuture::from(
        gpu.request_adapter()
    ).await?.dyn_into().map_err(|_| "adapter cast failed")?;

    let device: web_sys::GpuDevice = wasm_bindgen_futures::JsFuture::from(
        adapter.request_device()
    ).await?.dyn_into().map_err(|_| "device cast failed")?;

    let queue = device.queue();
    let format = gpu.get_preferred_canvas_format();

    STATE.with(|s| {
        let mut s = s.borrow_mut();
        s.adapters.alloc(adapter);
        s.devices.alloc(device);
        s.queues.alloc(queue);
        s.preferred_format = Some(format);
    });

    Ok(())
}
```

### 11.3 Command Stream Executor (`web/executor.rs`)

Same `execute_stream` pattern as §10.3, but using `web_sys::Gpu*` types:

```rust
#[wasm_bindgen(js_name = "extSubmitCommands")]
pub fn ext_submit_commands(input: &[u8]) -> Vec<u8> {
    STATE.with(|s| {
        let mut s = s.borrow_mut();
        match web_execute_stream(input, &mut s) {
            Ok(()) => serde_json::to_vec(&serde_json::json!({})).unwrap(),
            Err(e) => error_json(&e),
        }
    })
}

fn web_execute_stream(input: &[u8], state: &mut WebState) -> Result<(), String> {
    let mut r = StreamReader::new(input);
    let queue_h = r.read_u32();
    let device_h = r.read_u32();
    // ... validate header ...

    let device = state.devices.get(device_h);
    let queue = state.queues.get(queue_h);
    let mut command_buffers: Vec<web_sys::GpuCommandBuffer> = Vec::new();

    let enc_desc = web_sys::GpuCommandEncoderDescriptor::new();
    let mut encoder = device.create_command_encoder_with_descriptor(&enc_desc)
        .map_err(|e| format!("createCommandEncoder: {:?}", e))?;

    while r.remaining() > 0 {
        let op = r.read_u8();
        match op {
            OP_FINISH => {
                command_buffers.push(encoder.finish());
                if r.remaining() > 0 {
                    encoder = device.create_command_encoder_with_descriptor(&enc_desc)
                        .map_err(|e| format!("createCommandEncoder: {:?}", e))?;
                }
            }
            OP_BEGIN_RENDER_PASS => {
                // Build web_sys::GpuRenderPassDescriptor from stream bytes
                // Call encoder.begin_render_pass(&desc)
                // Store pass for subsequent commands
            }
            // ... same pattern for all opcodes, using web_sys API ...
            _ => return Err(format!("unknown opcode 0x{:02X}", op)),
        }
    }

    // Submit all command buffers
    let js_array = js_sys::Array::new();
    for cb in command_buffers {
        js_array.push(&cb);
    }
    queue.submit(&js_array);
    Ok(())
}
```

### 11.4 Control & Data Externs (`web/control.rs`, `web/data.rs`)

Control plane externs (JSON) follow the same pattern as v1 — parse JSON input,
call web_sys method, return JSON output:

```rust
#[wasm_bindgen(js_name = "extCreateBuffer")]
pub fn ext_create_buffer(input: &[u8]) -> Vec<u8> {
    let req: CreateBufferReq = serde_json::from_slice(input).unwrap();
    STATE.with(|s| {
        let mut s = s.borrow_mut();
        let device = s.devices.get(req.device);
        let desc = web_sys::GpuBufferDescriptor::new(req.size as f64, req.usage);
        if req.mapped_at_creation.unwrap_or(false) {
            desc.set_mapped_at_creation(true);
        }
        let buffer = device.create_buffer(&desc).unwrap();
        let handle = s.buffers.alloc(buffer);
        serde_json::to_vec(&HandleResponse { handle }).unwrap()
    })
}
```

Data plane externs (raw bytes) read binary headers:

```rust
#[wasm_bindgen(js_name = "extWriteBuffer")]
pub fn ext_write_buffer(input: &[u8]) -> Vec<u8> {
    let mut r = StreamReader::new(input);
    let queue_h = r.read_u32();
    let buffer_h = r.read_u32();
    let offset = r.read_u64();
    let data = &input[r.pos..]; // remaining bytes = raw data

    STATE.with(|s| {
        let s = s.borrow();
        let queue = s.queues.get(queue_h);
        let buffer = s.buffers.get(buffer_h);
        // web_sys queue.write_buffer takes a BufferSource
        queue.write_buffer_with_u8_array_and_f64(buffer, offset as f64, data)
            .unwrap();
        serde_json::to_vec(&serde_json::json!({})).unwrap()
    })
}
```

---

## 12. Native Backend (`native/`)

### 12.1 Entry Point (`native/mod.rs`)

```rust
use std::cell::RefCell;
use crate::registry::HandleRegistry;

mod init;
mod control;
mod executor;
mod data;

thread_local! {
    static STATE: RefCell<NativeState> = RefCell::new(NativeState::new());
}

pub(crate) struct NativeState {
    pub instance: Option<wgpu::Instance>,
    pub adapters: HandleRegistry<wgpu::Adapter>,
    pub devices: HandleRegistry<wgpu::Device>,
    pub queues: HandleRegistry<wgpu::Queue>,
    pub buffers: HandleRegistry<wgpu::Buffer>,
    pub textures: HandleRegistry<wgpu::Texture>,
    pub texture_views: HandleRegistry<wgpu::TextureView>,
    pub samplers: HandleRegistry<wgpu::Sampler>,
    pub shader_modules: HandleRegistry<wgpu::ShaderModule>,
    pub bind_group_layouts: HandleRegistry<wgpu::BindGroupLayout>,
    pub bind_groups: HandleRegistry<wgpu::BindGroup>,
    pub pipeline_layouts: HandleRegistry<wgpu::PipelineLayout>,
    pub render_pipelines: HandleRegistry<wgpu::RenderPipeline>,
    pub compute_pipelines: HandleRegistry<wgpu::ComputePipeline>,
    pub surfaces: HandleRegistry<wgpu::Surface<'static>>,
}

vo_ext::export_extensions!();

pub fn ensure_linked() {
    extern "C" { fn vo_ext_get_entries() -> vo_ext::ExtensionTable; }
    let _ = std::hint::black_box(unsafe { vo_ext_get_entries() });
}
```

### 12.2 Command Stream Executor (`native/executor.rs`)

```rust
use vo_ext::prelude::*;
use crate::stream::*;

#[vo_extern_ctx("github.com/vo-lang/wgpu", "extSubmitCommands")]
fn ext_submit_commands(ctx: &mut ExternCallContext) -> ExternResult {
    let input = ctx.arg_bytes(slots::ARG_INPUT);

    STATE.with(|s| {
        let mut s = s.borrow_mut();
        match native_execute_stream(input, &mut s) {
            Ok(()) => {
                ctx.ret_bytes(slots::RET_0, b"{}");
                write_nil_error(ctx, slots::RET_ERR);
            }
            Err(e) => {
                ctx.ret_bytes(slots::RET_0, &error_json(&e));
                write_nil_error(ctx, slots::RET_ERR);
            }
        }
        ExternResult::Ok
    })
}

fn native_execute_stream(input: &[u8], state: &mut NativeState) -> Result<(), String> {
    let mut r = StreamReader::new(input);
    let queue_h = r.read_u32();
    let device_h = r.read_u32();

    let magic = r.read_u32();
    if magic != STREAM_MAGIC { return Err("invalid stream magic".into()); }
    let version = r.read_u16();
    if version != STREAM_VERSION { return Err("unsupported stream version".into()); }
    let num_encoders = r.read_u16();

    let device = state.devices.get(device_h);
    let queue = state.queues.get(queue_h);
    let mut command_buffers = Vec::with_capacity(num_encoders as usize);
    let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());

    while r.remaining() > 0 {
        let op = r.read_u8();
        match op {
            OP_FINISH => {
                command_buffers.push(encoder.finish());
                if r.remaining() > 0 {
                    encoder = device.create_command_encoder(
                        &wgpu::CommandEncoderDescriptor::default()
                    );
                }
            }

            OP_BEGIN_RENDER_PASS => {
                let num_colors = r.read_u8() as usize;
                let has_depth = r.read_u8() != 0;
                r.skip(2);

                // Read color attachments
                let mut color_views = Vec::with_capacity(num_colors);
                let mut color_descs = Vec::with_capacity(num_colors);
                for _ in 0..num_colors {
                    let view_h = r.read_u32();
                    let _resolve_h = r.read_u32();
                    let load_op = r.read_u8();
                    let store_op = r.read_u8();
                    r.skip(2);
                    let cr = r.read_f64();
                    let cg = r.read_f64();
                    let cb = r.read_f64();
                    let ca = r.read_f64();

                    color_views.push(view_h);
                    color_descs.push((load_op, store_op, wgpu::Color { r: cr, g: cg, b: cb, a: ca }));
                }

                let depth_desc = if has_depth {
                    let view_h = r.read_u32();
                    let d_load = r.read_u8();
                    let d_store = r.read_u8();
                    r.skip(2);
                    let clear_depth = r.read_f32();
                    let _clear_stencil = r.read_u32();
                    Some((view_h, d_load, d_store, clear_depth))
                } else {
                    None
                };

                // Build wgpu::RenderPassDescriptor
                // (lifetime-safe: build attachments referencing state.texture_views)
                // Then: let pass = encoder.begin_render_pass(&desc);
                // Store pass in a local variable for subsequent commands
                // ...
            }

            OP_RP_SET_PIPELINE => {
                let p = r.read_u32();
                // render_pass.set_pipeline(state.render_pipelines.get(p));
            }

            OP_RP_SET_BIND_GROUP => {
                let index = r.read_u32();
                let bg = r.read_u32();
                let num_dyn = r.read_u32();
                let mut offsets = Vec::with_capacity(num_dyn as usize);
                for _ in 0..num_dyn { offsets.push(r.read_u32()); }
                // render_pass.set_bind_group(index, state.bind_groups.get(bg), &offsets);
            }

            OP_RP_SET_VERTEX_BUFFER => {
                let slot = r.read_u32();
                let buf = r.read_u32();
                let offset = r.read_u64();
                let size = r.read_u64();
                // render_pass.set_vertex_buffer(slot, state.buffers.get(buf).slice(offset..offset+size));
            }

            OP_RP_SET_INDEX_BUFFER => {
                let buf = r.read_u32();
                let fmt = index_format_from_u8(r.read_u8());
                r.skip(3);
                let offset = r.read_u64();
                let size = r.read_u64();
                // render_pass.set_index_buffer(state.buffers.get(buf).slice(offset..offset+size), fmt);
            }

            OP_RP_DRAW => {
                let vc = r.read_u32();
                let ic = r.read_u32();
                let fv = r.read_u32();
                let fi = r.read_u32();
                // render_pass.draw(fv..fv+vc, fi..fi+ic);
            }

            OP_RP_DRAW_INDEXED => {
                let ic = r.read_u32();
                let inc = r.read_u32();
                let fi = r.read_u32();
                let bv = r.read_i32();
                let f_inst = r.read_u32();
                // render_pass.draw_indexed(fi..fi+ic, bv, f_inst..f_inst+inc);
            }

            OP_RP_SET_VIEWPORT => {
                let x = r.read_f32(); let y = r.read_f32();
                let w = r.read_f32(); let h = r.read_f32();
                let min_d = r.read_f32(); let max_d = r.read_f32();
                // render_pass.set_viewport(x, y, w, h, min_d, max_d);
            }

            OP_RP_SET_SCISSOR_RECT => {
                let x = r.read_u32(); let y = r.read_u32();
                let w = r.read_u32(); let h = r.read_u32();
                // render_pass.set_scissor_rect(x, y, w, h);
            }

            OP_END_RENDER_PASS => {
                // drop(render_pass); // ends the pass
            }

            OP_BEGIN_COMPUTE_PASS => {
                // let pass = encoder.begin_compute_pass(&Default::default());
            }

            OP_END_COMPUTE_PASS => {
                // drop(compute_pass);
            }

            OP_CP_SET_PIPELINE => {
                let p = r.read_u32();
                // compute_pass.set_pipeline(state.compute_pipelines.get(p));
            }

            OP_CP_SET_BIND_GROUP => {
                let index = r.read_u32();
                let bg = r.read_u32();
                let num_dyn = r.read_u32();
                let mut offsets = Vec::with_capacity(num_dyn as usize);
                for _ in 0..num_dyn { offsets.push(r.read_u32()); }
                // compute_pass.set_bind_group(index, state.bind_groups.get(bg), &offsets);
            }

            OP_CP_DISPATCH => {
                let x = r.read_u32(); let y = r.read_u32(); let z = r.read_u32();
                // compute_pass.dispatch_workgroups(x, y, z);
            }

            OP_COPY_BUF_TO_BUF => {
                let src = r.read_u32(); let src_off = r.read_u64();
                let dst = r.read_u32(); let dst_off = r.read_u64();
                let size = r.read_u64();
                // encoder.copy_buffer_to_buffer(
                //     state.buffers.get(src), src_off,
                //     state.buffers.get(dst), dst_off, size);
            }

            _ => return Err(format!("unknown opcode 0x{:02X}", op)),
        }
    }

    queue.submit(command_buffers);
    Ok(())
}
```

### 12.3 Surface Creation (Native)

On native, `CreateSurface` needs a window handle. The Tauri integration provides
it via a platform callback:

```rust
thread_local! {
    static SURFACE_PROVIDER: RefCell<Option<Box<dyn Fn(&str) -> Option<wgpu::Surface<'static>>>>> =
        RefCell::new(None);
}

pub fn set_surface_provider(provider: Box<dyn Fn(&str) -> Option<wgpu::Surface<'static>>>) {
    SURFACE_PROVIDER.with(|p| *p.borrow_mut() = Some(provider));
}

#[vo_extern_ctx("github.com/vo-lang/wgpu", "extCreateSurface")]
fn ext_create_surface(ctx: &mut ExternCallContext) -> ExternResult {
    let input = ctx.arg_bytes(slots::ARG_INPUT);
    let req: CreateSurfaceReq = serde_json::from_slice(input).unwrap();

    SURFACE_PROVIDER.with(|p| {
        let provider = p.borrow();
        let provider = provider.as_ref().unwrap();
        match provider(&req.canvas_id) {
            Some(surface) => {
                STATE.with(|s| {
                    let handle = s.borrow_mut().surfaces.alloc(surface);
                    ctx.ret_bytes(slots::RET_0,
                        &serde_json::to_vec(&HandleResponse { handle }).unwrap());
                    write_nil_error(ctx, slots::RET_ERR);
                });
            }
            None => {
                ctx.ret_bytes(slots::RET_0,
                    &error_json(&format!("surface '{}' not available", req.canvas_id)));
                write_nil_error(ctx, slots::RET_ERR);
            }
        }
        ExternResult::Ok
    })
}
```

---

## 13. `vo.ext.toml`

```toml
[extension]
name = "github.com/vo-lang/wgpu"

[extension.wasm]
type = "bindgen"
js_glue = "wgpu.js"
wasm = "wgpu.wasm"

[extension.native]
type = "native"
path = "libvo_wgpu"
```

---

## 14. Build Instructions

### Web (wasm-pack)

```bash
cd 3rdparty/wgpu/rust
wasm-pack build --target web --out-dir ../pkg --no-opt
cp ../pkg/vo_wgpu.js ../wgpu.js
cp ../pkg/vo_wgpu_bg.wasm ../wgpu.wasm
```

`--no-opt` remains mandatory (wasm-opt table reordering bug).

### Native (cargo)

```bash
# Static linking (Tauri playground — just add as Cargo dependency)
# No separate build step needed.

# Dynamic library (standalone `vo` CLI)
cd 3rdparty/wgpu/rust
cargo build --release --features native --lib
# Produces libvo_wgpu.so / libvo_wgpu.dylib / vo_wgpu.dll
```

---

## 15. Testing & Performance Criteria

### 15.1 Performance Acceptance Criteria (Hard Requirements)

These must pass before the wrapper ships:

| Metric | Requirement | How to measure |
|--------|-------------|----------------|
| Bridge overhead at 60fps, 100 draw calls | < 1.0 ms/frame (P95) | Instrument `extSubmitCommands` latency |
| Vo-side command recording, 200 commands | < 0.2 ms | Benchmark `NewCommandEncoder` → `Finish` |
| Command stream decode throughput | ≥ 200k cmds/sec | Rust unit benchmark on synthetic stream |
| `WriteBuffer` 1 MB | < 0.5 ms (P95) | Instrument `extWriteBuffer` latency |
| Per-frame extern call count (typical) | ≤ 5 | Count in integration test |
| Zero heap allocs in stream executor hot path | 0 (excluding wgpu internals) | `#[global_allocator]` counting allocator in test |

### 15.2 Rust Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_handle_registry() {
        let mut reg = HandleRegistry::new();
        let h1 = reg.alloc("hello");
        let h2 = reg.alloc("world");
        assert_eq!(*reg.get(h1), "hello");
        assert_eq!(*reg.get(h2), "world");
        assert_eq!(reg.free(h1), "hello");
        let h3 = reg.alloc("reuse");
        assert_eq!(h3, h1); // slot reused
    }

    #[test]
    fn test_stream_reader() {
        let data = [0x50, 0x47, 0x4F, 0x56, 0x01, 0x00, 0x01, 0x00];
        let mut r = StreamReader::new(&data);
        assert_eq!(r.read_u32(), STREAM_MAGIC);
        assert_eq!(r.read_u16(), 1);
        assert_eq!(r.read_u16(), 1);
        assert_eq!(r.remaining(), 0);
    }

    #[test]
    fn test_stream_round_trip() {
        // Build a minimal stream: one encoder, one draw call
        let mut buf = Vec::new();
        buf.extend_from_slice(&0u32.to_le_bytes());    // queue
        buf.extend_from_slice(&0u32.to_le_bytes());    // device
        buf.extend_from_slice(&STREAM_MAGIC.to_le_bytes());
        buf.extend_from_slice(&STREAM_VERSION.to_le_bytes());
        buf.extend_from_slice(&1u16.to_le_bytes());    // 1 encoder

        // BeginRenderPass: 1 color, no depth
        buf.push(OP_BEGIN_RENDER_PASS);
        buf.push(1);  // num_colors
        buf.push(0);  // no depth
        buf.extend_from_slice(&[0, 0]); // padding
        // Color attachment: view=0, resolve=0, load=Clear, store=Store, clear=black
        buf.extend_from_slice(&0u32.to_le_bytes());    // view
        buf.extend_from_slice(&0u32.to_le_bytes());    // resolve
        buf.push(1);  // load_op = Clear
        buf.push(0);  // store_op = Store
        buf.extend_from_slice(&[0, 0]); // padding
        for _ in 0..4 { buf.extend_from_slice(&0.0f64.to_le_bytes()); } // clear RGBA

        // SetPipeline
        buf.push(OP_RP_SET_PIPELINE);
        buf.extend_from_slice(&0u32.to_le_bytes());

        // Draw(3, 1, 0, 0)
        buf.push(OP_RP_DRAW);
        buf.extend_from_slice(&3u32.to_le_bytes());
        buf.extend_from_slice(&1u32.to_le_bytes());
        buf.extend_from_slice(&0u32.to_le_bytes());
        buf.extend_from_slice(&0u32.to_le_bytes());

        // EndRenderPass
        buf.push(OP_END_RENDER_PASS);

        // Finish
        buf.push(OP_FINISH);

        // Verify stream parses without error (decode only, no GPU)
        let mut r = StreamReader::new(&buf[STREAM_HEADER_SIZE..]);
        // ... walk opcodes and assert expected values
    }
}
```

### 15.3 Vo Integration Tests

```
tests/wgpu/
├── 01_adapter.vo         # RequestAdapter → verify non-zero handle
├── 02_device.vo          # RequestDevice + GetQueue
├── 03_buffer.vo          # CreateBuffer + WriteBuffer + Destroy
├── 04_shader.vo          # CreateShaderModule from WGSL string
├── 05_pipeline.vo        # Full render pipeline creation
├── 06_clear.vo           # Clear color render pass (command stream path)
├── 07_triangle.vo        # Vertex buffer + draw triangle
├── 08_uniform.vo         # Uniform buffer + bind group + per-frame update
├── 09_texture.vo         # Texture creation + WriteTexture + sampling
├── 10_compute.vo         # Compute pipeline + dispatch + readback
├── 11_multi_pass.vo      # Multiple render passes per frame
├── 12_indexed_draw.vo    # Index buffer + DrawIndexed
└── 13_perf_stress.vo     # 500 draw calls, assert < 2ms bridge overhead
```

Each test:
1. Initialize wgpu (`RequestAdapter`, `RequestDevice`)
2. Create resources (JSON control plane)
3. Record commands (Vo-side binary — zero extern calls)
4. Submit (one extern call)
5. Assert no error (assert-based, not print)

### 15.4 Cross-Platform Parity

```bash
# Web: via playground
./d.py play
# Load test program, verify no errors in console

# Native: via CLI
./d.py run tests/wgpu/07_triangle.vo --mode=vm

# Native: via Tauri playground
cd apps/playground-desktop && cargo tauri dev
```

### 15.5 Visual Regression

For render output verification:
1. Render one frame to an offscreen texture (not surface)
2. CopyTextureToBuffer → MapAsync → GetMappedRange → raw pixels
3. Compare against expected checksum or reference image
4. Works identically on both web and native

---

## 16. Example: Full Triangle Demo

Shows how user code looks with the new API. **2 extern calls per frame**
(GetCurrentTexture + Submit), everything else is Vo-side binary recording:

```vo
package main

import "github.com/vo-lang/wgpu"

func main() {
    adapter := wgpu.RequestAdapter()?
    device := adapter.RequestDevice()?
    queue := device.GetQueue()?

    surface := wgpu.CreateSurface("gpu")?
    format := surface.GetPreferredFormat(adapter)?
    surface.Configure(device, format, 800, 600)?

    shader := device.CreateShaderModule(`
        @vertex fn vs(@builtin(vertex_index) i: u32) -> @builtin(position) vec4f {
            var p = array<vec2f,3>(vec2f(0,.5), vec2f(-.5,-.5), vec2f(.5,-.5));
            return vec4f(p[i], 0, 1);
        }
        @fragment fn fs() -> @location(0) vec4f {
            return vec4f(.3, .7, 1, 1);
        }
    `)?

    layout := device.CreatePipelineLayout([]wgpu.BindGroupLayout{})?
    pipeline := device.CreateRenderPipeline(wgpu.RenderPipelineDescriptor{
        Layout:       layout,
        VertexModule: shader, VertexEntry: "vs",
        FragmentModule: shader, FragmentEntry: "fs",
        FragmentTargets: []wgpu.ColorTargetState{{Format: format}},
        Primitive: wgpu.PrimitiveState{Topology: wgpu.TopologyTriangleList},
    })?

    // --- Render loop (via vogui timer) ---
    // Per-frame: only 2 extern calls (GetCurrentTexture + Submit)
    view := surface.GetCurrentTexture()?

    enc := wgpu.NewCommandEncoder()            // Vo-side, no extern
    pass := enc.BeginRenderPass(wgpu.RenderPassDescriptor{
        ColorAttachments: []wgpu.ColorAttachment{{
            View: view, LoadOp: wgpu.LoadOpClear, StoreOp: wgpu.StoreOpStore,
            ClearColor: wgpu.Color{0, 0, 0, 1},
        }},
    })                                          // Vo-side, no extern
    pass.SetPipeline(pipeline)                  // Vo-side, no extern
    pass.Draw(3, 1, 0, 0)                      // Vo-side, no extern
    pass.End()                                  // Vo-side, no extern
    cmd := enc.Finish()                         // Vo-side, no extern

    queue.Submit(device, []wgpu.CommandBuffer{cmd})?  // ONE extern call

    surface.Present()?
}
```

---

## 17. Risk & Mitigation

| Risk | Impact | Mitigation |
|------|--------|------------|
| Opcode versioning: future commands break old streams | Medium | `version` field in header; executor rejects unknown versions. New opcodes get new version bump. |
| wgpu `RenderPass` borrow lifetime vs stream execution | Low | Executor processes passes inline — begin/end is always within one encoder scope. Rust ownership is maintained by lexical scoping in the executor loop. |
| wgpu crate API drift vs WebGPU spec | Low | Pin wgpu version. Enum mapping layer (`enums.rs`) absorbs differences. |
| Large command streams (>64KB) could cause WASM memory pressure | Low | Pre-allocate with `make([]byte, 0, 1024)` default; reuse encoder across frames via `Reset()` method (future). |
| Web canvas context lost during tab switch | Medium | Detect via `device.lost` event; re-initialize on resume. |
| Command stream corruption (malformed bytes) | Low | Fail-fast with clear error message. Debug builds can add stream validation pass. |
