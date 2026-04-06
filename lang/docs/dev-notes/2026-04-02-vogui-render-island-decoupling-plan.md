# vogui → Render-Island Decoupling Plan

**Date**: 2026-04-02
**Status**: In Progress

## Problem Statement

Studio is currently hard-coupled to `@vogui/runtime` via three direct imports:

1. `GuiRuntimeSurface.svelte` — imports `render`, `decodeBinaryRender`, `injectStyles`, `setupKeyHandler`, `getRef`
2. `runtime_service.ts` — imports `findExternalWidgetHandlerIdInBytes` (protocol parsing)
3. `studio_wasm.ts` — imports `getRef`, `measureText`, `measureTextLines` (web WASM host bridge)

This creates two rendering paths (GuiRuntimeSurface vs render-island), prevents Studio from being
truly framework-agnostic, and causes `startRenderIsland` to unconditionally load ~5MB of Studio WASM
even when the framework doesn't need it.

## Design: 3 Orthogonal Artifacts

Each framework (e.g. vogui) declares up to 3 artifacts in `vo.ext.toml [studio]`. Studio loads them
dynamically via VFS blob URLs — zero static imports of any framework package.

```toml
[studio]
renderer = "js/dist/renderer.js"
protocol = "js/dist/protocol.js"
host_bridge = "js/dist/host_bridge.js"
```

### Artifact A: RendererModule

Self-contained ES module bundling all rendering logic (preact, decoder, events, refs, canvas, audio,
styles). Loaded via blob URL from VFS snapshot.

```typescript
export interface RendererModule {
  init(host: RendererHost): Promise<void>;
  render(container: HTMLElement, bytes: Uint8Array): void;
  stop(): void;
}
```

### Artifact B: ProtocolModule

Lightweight (~2KB) binary protocol parser. Replaces the `findExternalWidgetHandlerIdInBytes` import
in `runtime_service.ts`. Loaded early (before renderer) so Studio can extract metadata from initial
render bytes.

```typescript
export interface ProtocolModule {
  findExternalWidgetHandlerId(bytes: Uint8Array): number | null;
}
```

### Artifact C: HostBridgeModule

Provides WASM import entries for framework-specific host functions (getRef, measureText, focus, blur,
scrollTo, etc.). Replaces the hardcoded `host_*` functions in `studio_wasm.ts`.

```typescript
export interface HostBridgeModule {
  buildImports(ctx: HostBridgeContext): Record<string, WebAssembly.ImportValue>;
}

interface HostBridgeContext {
  readString(ptr: number, len: number): string;
  alloc(size: number): number;
  sendEvent(handlerId: number, payload: string): Promise<void>;
  sendEventSync(handlerId: number, payload: string): void;
}
```

### RendererHost (Studio → Renderer)

Lean interface with lazy capabilities. No unconditional WASM loading.

```typescript
export interface RendererHost {
  // Core — every renderer needs these
  sendEvent(handlerId: number, payload: string): Promise<void>;
  log(message: string): void;

  // On-demand capabilities — only materialized when accessed
  getCapability<K extends keyof CapabilityMap>(name: K): CapabilityMap[K] | null;
}

interface CapabilityMap {
  canvas:           { getCanvas(): HTMLCanvasElement | null };
  island_transport: { createChannel(): Promise<IslandChannel> };
  vo_web:           { getVoWeb(): Promise<VoWebModule> };  // lazy WASM load
  vfs:              { getBytes(path: string): Uint8Array | null };
  widget:           { register(name: string, factory: WidgetFactory): void };
}
```

Frameworks declare needed capabilities in `vo.ext.toml`:

```toml
[studio]
capabilities = ["canvas", "island_transport"]  # only these are available
```

vogui declares NO capabilities beyond the core — so `vo_web` is never loaded, no WASM overhead.

## Implementation Phases

### Phase 1: vogui Artifacts

Build 3 studio artifacts from vogui/js:

- `src/studio_renderer.ts` → `dist/renderer.js` (self-contained, all deps bundled)
- `src/studio_protocol.ts` → `dist/protocol.js` (tiny, protocol parsing only)
- `src/studio_host_bridge.ts` → `dist/host_bridge.js` (refs + text measurement)

Each gets its own vite config. The renderer bundles preact and all vogui internals.
The host_bridge shares ref registry logic with the renderer (same package, built separately).

Update `vogui/vo.ext.toml`:

```toml
[studio]
renderer = "js/dist/renderer.js"
protocol = "js/dist/protocol.js"
host_bridge = "js/dist/host_bridge.js"
```

**Validation**: `npm run build` in vogui/js produces all 3 artifacts.

### Phase 2: Rust — Extend StudioManifest

In `studio/src-tauri/src/commands/gui.rs`:

- `StudioManifest` gains `protocol_path: Option<PathBuf>` and `host_bridge_path: Option<PathBuf>`
- `parse_studio_manifest` reads `[studio].protocol` and `[studio].host_bridge`
- `FrameworkContract` gains `protocolPath` and `hostBridgePath` fields
- `collect_render_island_vfs_files` includes all 3 artifact directories
- `studio/wasm` VFS snapshot collection also includes protocol + host_bridge paths

**Validation**: `cargo check` in studio/src-tauri, `cargo check --target wasm32-unknown-unknown` in studio/wasm.

### Phase 3: Studio — RendererHost with Lazy Capabilities

In `studio/src/lib/gui/render_island.ts`:

- Define `RendererHost` and `CapabilityMap` interfaces
- Replace `StudioGuiHost` with `RendererHost`
- `makeRendererHost()` builds capabilities lazily — `vo_web` only triggers `loadStudioWasm()` on access
- `startRenderIsland()` no longer calls `loadStudioWasm()` unconditionally
- Framework capabilities from contract determine which capabilities are available

**Validation**: `npm run build` in studio. Existing render-island frameworks still work.

### Phase 4: Studio — ProtocolModule Loader

In `studio/src/lib/gui/render_island.ts` or a new `protocol_loader.ts`:

- `loadProtocolModule(protocolPath, backend, entryPath)` → loads protocol artifact from VFS
- Returns `ProtocolModule` interface

In `runtime_service.ts`:

- Remove `import { findExternalWidgetHandlerIdInBytes } from '@vogui/runtime'`
- Add `private protocolModule: ProtocolModule | null = null`
- `setProtocolModule(mod)` called by PreviewPanel after framework contract is available
- `applyGuiRender()` and `runGui()` use `this.protocolModule?.findExternalWidgetHandlerId(bytes)`
- Fallback: if no protocol module loaded, use `externalWidgetHandlerId` from `GuiRunOutput`

**Validation**: `npm run build`, gui_chat + external widget demos work on native + web.

### Phase 5: Studio — HostBridge Loader

In `studio/src/lib/gui/render_island.ts` or a new `host_bridge_loader.ts`:

- `loadHostBridgeModule(hostBridgePath, backend, entryPath)` → loads host bridge from VFS
- Returns `HostBridgeModule` interface

In `studio_wasm.ts`:

- Remove hardcoded `host_focus`, `host_blur`, `host_scroll_to`, `host_scroll_into_view`,
  `host_select_text`, `host_measure_text`, `host_measure_text_lines` from `buildStandaloneImports()`
- Instead, merge framework bridge imports: `Object.assign(imports.env, bridge.buildImports(ctx))`
- Remove `import { getRef, measureText, measureTextLines } from '@vogui/runtime'`

**Validation**: `npm run build`, web WASM demos with measureText/getRef/scrollTo still work.

### Phase 6: Unify Rendering Path

- Delete `GuiRuntimeSurface.svelte`
- In `PreviewPanel.svelte`:
  - Remove `import GuiRuntimeSurface`
  - Remove the `{:else}` branch that renders `<GuiRuntimeSurface />`
  - All GUI apps go through `startRenderIsland` → `deliverRenderBytes`
  - If no `rendererPath` in framework contract, that's an error (every GUI framework must declare one)
- Remove `@vogui/runtime` from `studio/package.json` dependencies
- Remove vogui symlink from `studio/node_modules/@vogui/runtime`

**Validation**: `npm run build` in studio with zero `@vogui/runtime` references. All demos work.

### Phase 7: Full Validation

| Feature | Demo | Platform |
|---|---|---|
| Click / onChange | gui_showcase | native + web |
| OnScroll / VirtualScroll | gui_chat | native + web |
| ResizeObserver | gui_chat resize | native + web |
| Canvas 2D | gui_tetris | native + web |
| MeasureText | gui_chat bubbles | native + web |
| Keyboard (global) | gui_tetris arrow keys | native + web |
| Timer / Interval | gui_chat streaming | native + web |
| Audio | gui_audio | native + web |
| External widget | widget demo | native + web |
| Island transport | island demo | native |

## Files Changed

### vogui (new files)
- `js/src/studio_renderer.ts`
- `js/src/studio_protocol.ts`
- `js/src/studio_host_bridge.ts`
- `js/vite.renderer.config.ts`
- `js/vite.protocol.config.ts`
- `js/vite.bridge.config.ts`

### vogui (modified)
- `vo.ext.toml` — add [studio] section
- `js/package.json` — add build scripts for 3 artifacts

### volang/studio (modified)
- `src-tauri/src/commands/gui.rs` — extend StudioManifest + FrameworkContract
- `src/lib/gui/render_island.ts` — RendererHost, lazy capabilities, protocol/bridge loaders
- `src/lib/services/runtime_service.ts` — remove @vogui/runtime import, use ProtocolModule
- `src/lib/studio_wasm.ts` — remove @vogui/runtime import, use HostBridgeModule
- `src/lib/types.ts` — extend FrameworkContract
- `src/components/PreviewPanel.svelte` — remove GuiRuntimeSurface, unify to one path

### volang/studio (deleted)
- `src/components/GuiRuntimeSurface.svelte`

### volang/studio/wasm (modified)
- VFS snapshot collection includes protocol + host_bridge paths

## Not In Scope

- vogui VirtualScroll scroll-to-0 bug (Vo-side, independent issue)
- `emit()` global `currentConfig` coupling (not a regression, pre-existing)
- Splitting `@vogui/runtime` npm package into multiple packages (the 3 artifacts achieve the goal without npm restructuring)
