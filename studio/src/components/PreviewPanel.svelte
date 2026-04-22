<script lang="ts">
  import { onDestroy, tick } from 'svelte';
  import { runtime } from '../stores/runtime';
  import {
    startRendererBridge, stopRendererBridge, deliverRenderBytes,
    isRendererBridgeActive,
    loadProtocolModule, unloadProtocolModule,
    loadHostBridgeModule, unloadHostBridgeModule,
    fetchVfsSnapshot,
    type HostBridgeModule,
    type ProtocolModule,
    type VfsFile,
  } from '../lib/gui/renderer_bridge';
  import { setActiveHostBridge, clearActiveHostBridge } from '../lib/studio_wasm';
  import type { ServiceRegistry } from '../lib/services/service_registry';
  import { frameworkContractKey, frameworkJsModulePath, type FrameworkContract } from '../lib/types';

  type PreviewPanelSharedState = {
    rendererSurfaceHost: HTMLDivElement | null;
  };

  const previewPanelGlobal = globalThis as Record<string, unknown>;
  const previewPanelSharedState = (() => {
    const existing = previewPanelGlobal.__studioPreviewPanelState as PreviewPanelSharedState | undefined;
    if (existing) {
      return existing;
    }
    const created: PreviewPanelSharedState = { rendererSurfaceHost: null };
    previewPanelGlobal.__studioPreviewPanelState = created;
    return created;
  })();

  export let registry: ServiceRegistry | null = null;
  export let chromeless = false;
  export let collapsed = false;
  export let fullscreen = false;
  export let fullscreenTitle = '';
  export let showFullscreenAction = false;
  export let onFullscreenAction: (() => void) | null = null;
  export let onExitFullscreenAction: (() => void) | null = null;
  export let onToggleCollapsed: (() => void) | null = null;

  // Must match the canvasRef passed to the external widget renderer
  const CANVAS_ID = 'canvas';

  let panelWidth = 400;
  let isResizing = false;
  let resizeStartX = 0;
  let resizeStartW = 0;
  let rendererBridgeActive = false;
  let rendererBridgeLaunching = false;
  let rendererBridgeLaunchGeneration = 0;
  let rendererBridgeSessionId: number | null = null;
  let rendererBridgeFailedSessionId: number | null = null;
  let rendererBridgeError: string | null = null;
  let rendererContainer: HTMLDivElement | undefined;
  let rendererSurface: HTMLDivElement | undefined;

  function ensureRendererSurfaceHost(): HTMLDivElement {
    let host = previewPanelSharedState.rendererSurfaceHost;
    if (!host) {
      host = document.createElement('div');
      previewPanelSharedState.rendererSurfaceHost = host;
    }
    host.style.width = '100%';
    host.style.height = '100%';
    host.style.flex = '1 1 auto';
    host.style.display = 'block';
    host.style.minHeight = '0';
    host.style.position = '';
    host.style.left = '';
    host.style.top = '';
    host.style.overflow = previewSurfaceScrollable ? 'auto' : 'hidden';
    return host;
  }

  function attachRendererSurfaceHost(): HTMLDivElement | null {
    if (!rendererSurface) {
      return null;
    }
    const host = ensureRendererSurfaceHost();
    if (host.parentElement !== rendererSurface) {
      rendererSurface.replaceChildren();
      rendererSurface.appendChild(host);
    }
    return host;
  }

  function parkRendererSurfaceHost(): void {
    const host = previewPanelSharedState.rendererSurfaceHost;
    if (!host) {
      return;
    }
    const rect = host.getBoundingClientRect();
    host.style.width = `${Math.max(1, Math.round(rect.width))}px`;
    host.style.height = `${Math.max(1, Math.round(rect.height))}px`;
    host.style.position = 'fixed';
    host.style.left = '-9999px';
    host.style.top = '0';
    host.style.overflow = 'hidden';
    document.body.appendChild(host);
  }

  function removeRendererSurfaceHost(): void {
    previewPanelSharedState.rendererSurfaceHost?.remove();
    previewPanelSharedState.rendererSurfaceHost = null;
  }

  function frameworkKey(framework: FrameworkContract): string {
    return frameworkContractKey(framework);
  }

  function collectBridgeFrameworks(primary: FrameworkContract | null, providers: FrameworkContract[]): FrameworkContract[] {
    const seen = new Set<string>();
    const frameworks: FrameworkContract[] = [];
    const ordered = primary ? [primary, ...providers] : [...providers];
    for (const framework of ordered) {
      const key = frameworkKey(framework);
      if (seen.has(key)) {
        continue;
      }
      seen.add(key);
      frameworks.push(framework);
    }
    return frameworks;
  }

  function combineProtocolModules(modules: ProtocolModule[]): ProtocolModule {
    return {
      findHostWidgetHandlerId(bytes: Uint8Array): number | null {
        for (const module of modules) {
          const handlerId = module.findHostWidgetHandlerId(bytes);
          if (handlerId != null) {
            return handlerId;
          }
        }
        return null;
      },
    };
  }

  function combineHostBridgeModules(modules: HostBridgeModule[]): HostBridgeModule {
    return {
      buildImports(ctx) {
        const imports: Record<string, (...args: number[]) => number | void> = {};
        for (const module of modules) {
          const next = module.buildImports(ctx);
          for (const [name, handler] of Object.entries(next)) {
            if (!(name in imports)) {
              imports[name] = handler;
            }
          }
        }
        return imports;
      },
    };
  }

  $: isGuiApp = $runtime.kind === 'gui' && $runtime.isRunning;
  $: guiFramework = $runtime.gui.framework;
  $: providerFrameworks = $runtime.gui.providerFrameworks;
  $: bridgeFrameworks = collectBridgeFrameworks(guiFramework, providerFrameworks);
  $: hasRendererBridge = bridgeFrameworks.some((framework) => frameworkJsModulePath(framework, 'renderer') != null);
  $: capabilities = Array.from(new Set(bridgeFrameworks.flatMap((framework) => framework.capabilities ?? [])));
  $: isRenderSurface = capabilities.includes('render_surface');
  $: isIslandTransport = capabilities.includes('island_transport');
  $: needsManagedCanvas = isRenderSurface;
  $: previewSurfaceScrollable = hasRendererBridge && !isIslandTransport;
  $: effectiveCollapsed = !fullscreen && !chromeless && collapsed;
  $: frameworkPending = isGuiApp && bridgeFrameworks.length === 0;
  $: showRendererBridgeLoading = isGuiApp && hasRendererBridge && !rendererBridgeError && (!rendererBridgeActive || rendererBridgeLaunching);

  $: if (!isGuiApp) {
    rendererBridgeError = null;
    rendererBridgeFailedSessionId = null;
  }

  // Launch renderer bridge when GUI app with renderer becomes active.
  // If the renderer bridge survived a layout transition (e.g. preview→fullscreen),
  // just re-adopt the canvas without re-launching.
  $: if (rendererBridgeActive && rendererBridgeSessionId != null && $runtime.gui.sessionId !== rendererBridgeSessionId) {
    teardownRendererBridge(rendererBridgeSessionId);
  }
  $: if (isGuiApp && hasRendererBridge && registry && $runtime.gui.moduleBytes && $runtime.gui.entryPath && $runtime.gui.sessionId != null && $runtime.gui.sessionId !== rendererBridgeFailedSessionId && !rendererBridgeActive && !rendererBridgeLaunching) {
    if (isRendererBridgeActive($runtime.gui.sessionId)) {
      console.log('[PreviewPanel] re-adopt: renderer bridge still active, re-attaching canvas');
      rendererBridgeActive = true;
      rendererBridgeSessionId = $runtime.gui.sessionId;
      rendererBridgeFailedSessionId = null;
      tick().then(() => {
        const surface = attachRendererSurfaceHost();
        if (needsManagedCanvas) {
          attachCanvas();
        }
        if (surface && $runtime.gui.renderBytes && $runtime.gui.renderBytes.length > 0) {
          deliverRenderBytes(surface, $runtime.gui.renderBytes);
        }
      });
    } else {
      console.log('[PreviewPanel] fresh launch: no active renderer bridge');
      void launchRendererBridge();
    }
  }
  $: if (!isGuiApp && rendererBridgeActive) {
    teardownRendererBridge();
  }

  $: if (rendererBridgeActive && $runtime.gui.renderBytes && previewPanelSharedState.rendererSurfaceHost) {
    deliverRenderBytes(previewPanelSharedState.rendererSurfaceHost, $runtime.gui.renderBytes);
  }

  // ---- Canvas management ----
  // The canvas is created/managed via JS (not Svelte template) so it can
  // survive component destroy/recreate during layout transitions.
  // The WebGPU surface is tied to the canvas DOM element, not its position.

  function ensureCanvas(): HTMLCanvasElement {
    let canvas = document.getElementById(CANVAS_ID) as HTMLCanvasElement | null;
    if (!canvas) {
      canvas = document.createElement('canvas');
      canvas.id = CANVAS_ID;
    }
    canvas.tabIndex = 0;
    return canvas;
  }

  function applyCanvasStyle(canvas: HTMLCanvasElement): void {
    // Inline styles because Svelte scoped CSS won't apply to JS-created elements.
    canvas.style.width = '100%';
    canvas.style.height = '100%';
    canvas.style.display = 'block';
    canvas.style.position = '';
    canvas.style.left = '';
    canvas.style.top = '';
  }

  function attachCanvas(): void {
    const surface = attachRendererSurfaceHost();
    if (!surface) {
      console.warn('[PreviewPanel] attachCanvas: rendererSurface not ready, retrying after tick');
      tick().then(() => attachCanvas());
      return;
    }
    const canvas = ensureCanvas();
    applyCanvasStyle(canvas);
    if (canvas.parentElement !== surface) {
      surface.appendChild(canvas);
    }
    console.warn(`[PreviewPanel] attachCanvas canvas=${canvas.id} active=${document.activeElement instanceof HTMLElement ? `${document.activeElement.tagName}#${document.activeElement.id}` : '<none>'}`);
    requestAnimationFrame(() => {
      if (canvas.parentElement === surface) {
        canvas.focus();
        const active = document.activeElement instanceof HTMLElement ? `${document.activeElement.tagName}#${document.activeElement.id}` : '<none>';
        console.warn(`[PreviewPanel] focusCanvas canvas=${canvas.id} active=${active}`);
      }
    });
  }

  function removeCanvas(): void {
    const canvas = document.getElementById(CANVAS_ID);
    canvas?.remove();
  }

  // ---- Lifecycle ----

  async function launchRendererBridge(): Promise<void> {
    if (!registry || rendererBridgeLaunching) return;
    const launchGeneration = ++rendererBridgeLaunchGeneration;
    const sessionId = $runtime.gui.sessionId;
    rendererBridgeLaunching = true;
    try {
      const moduleBytes = $runtime.gui.moduleBytes;
      const entryPath = $runtime.gui.entryPath;
      if (!moduleBytes || !entryPath || sessionId == null) {
        throw new Error('Renderer bridge host context missing gui runtime data');
      }
      rendererBridgeError = null;
      await tick();
      if ($runtime.gui.sessionId !== sessionId || !($runtime.kind === 'gui' && $runtime.isRunning)) {
        return;
      }
      // Fetch VFS snapshot once and share across all framework module loaders.
      const framework = $runtime.gui.framework;
      const providerFrameworks = $runtime.gui.providerFrameworks;
      const bridgeFrameworks = collectBridgeFrameworks(framework, providerFrameworks);
      let vfsFiles: VfsFile[] | undefined;
      const needsVfs = bridgeFrameworks.some((provider) =>
        frameworkJsModulePath(provider, 'protocol')
          || frameworkJsModulePath(provider, 'host_bridge')
          || frameworkJsModulePath(provider, 'renderer'),
      );
      if (needsVfs && registry) {
        vfsFiles = await fetchVfsSnapshot(registry.backend, entryPath);
      }
      const protocolModules: ProtocolModule[] = [];
      for (const provider of bridgeFrameworks) {
        const protocolPath = frameworkJsModulePath(provider, 'protocol');
        if (!protocolPath || !registry) {
          continue;
        }
        try {
          protocolModules.push(await loadProtocolModule(protocolPath, registry.backend, entryPath, vfsFiles));
        } catch (e) {
          console.warn('[PreviewPanel] protocol module load failed, external widget detection may be unavailable:', e);
        }
      }
      registry.runtime.setProtocolModule(protocolModules.length > 0 ? combineProtocolModules(protocolModules) : null);
      const hostBridgeModules: HostBridgeModule[] = [];
      for (const provider of bridgeFrameworks) {
        const hostBridgePath = frameworkJsModulePath(provider, 'host_bridge');
        if (!hostBridgePath || !registry) {
          continue;
        }
        try {
          hostBridgeModules.push(await loadHostBridgeModule(hostBridgePath, registry.backend, entryPath, vfsFiles));
        } catch (e) {
          console.warn('[PreviewPanel] host bridge module load failed, WASM host functions may be unavailable:', e);
        }
      }
      setActiveHostBridge(hostBridgeModules.length > 0 ? combineHostBridgeModules(hostBridgeModules) : null);
      const surface = attachRendererSurfaceHost();
      if (needsManagedCanvas) {
        attachCanvas();
      }
      await startRendererBridge(CANVAS_ID, registry.backend, registry.runtime, sessionId, {
        entryPath,
        moduleBytes,
        framework,
        providerFrameworks,
        onError: (message) => {
          rendererBridgeError = message;
        },
      }, vfsFiles);
      if (launchGeneration !== rendererBridgeLaunchGeneration || !($runtime.kind === 'gui' && $runtime.isRunning) || $runtime.gui.sessionId !== sessionId) {
        stopRendererBridge(sessionId);
        if (!isRendererBridgeActive(sessionId)) {
          removeCanvas();
          removeRendererSurfaceHost();
        }
        return;
      }
      if (surface && $runtime.gui.renderBytes && $runtime.gui.renderBytes.length > 0) {
        deliverRenderBytes(surface, $runtime.gui.renderBytes);
      }
      rendererBridgeActive = true;
      rendererBridgeSessionId = sessionId;
      rendererBridgeFailedSessionId = null;
    } catch (e) {
      rendererBridgeFailedSessionId = sessionId;
      rendererBridgeError = e instanceof Error ? e.message : String(e);
      stopRendererBridge(sessionId);
      removeCanvas();
      removeRendererSurfaceHost();
      unloadProtocolModule();
      registry?.runtime.setProtocolModule(null);
      unloadHostBridgeModule();
      clearActiveHostBridge();
      console.error('[PreviewPanel] renderer bridge start failed:', e);
    } finally {
      rendererBridgeLaunching = false;
    }
  }

  function teardownRendererBridge(sessionId = rendererBridgeSessionId): void {
    rendererBridgeLaunchGeneration++;
    rendererBridgeActive = false;
    rendererBridgeSessionId = null;
    stopRendererBridge(sessionId);
    removeCanvas();
    removeRendererSurfaceHost();
    // Clean up framework artifact modules.
    unloadProtocolModule();
    registry?.runtime.setProtocolModule(null);
    unloadHostBridgeModule();
    clearActiveHostBridge();
  }

  $: if (effectiveCollapsed && rendererBridgeActive) {
    parkRendererSurfaceHost();
  }
  $: if (!effectiveCollapsed && rendererBridgeActive && rendererSurface) {
    attachRendererSurfaceHost();
    if (needsManagedCanvas) {
      attachCanvas();
    }
  }

  onDestroy(() => {
    const sessionId = rendererBridgeSessionId;
    const running = $runtime.isRunning;
    const active = sessionId != null && isRendererBridgeActive(sessionId);
    console.log('[PreviewPanel] onDestroy', { running, active });
    if (running && active && $runtime.gui.sessionId === sessionId) {
      // Layout transition (e.g. preview → fullscreen): keep the renderer bridge
      // alive and park the canvas off-screen so the WebGPU surface stays valid.
      parkRendererSurfaceHost();
      console.log('[PreviewPanel] onDestroy: parked renderer surface for layout transition');
    } else {
      teardownRendererBridge(sessionId);
      console.log('[PreviewPanel] onDestroy: full teardown');
    }
  });

  function onResizeStart(e: MouseEvent) {
    isResizing = true;
    resizeStartX = e.clientX;
    resizeStartW = panelWidth;
    e.preventDefault();
    window.addEventListener('mousemove', onResizeMove);
    window.addEventListener('mouseup', onResizeEnd);
  }
  function onResizeMove(e: MouseEvent) {
    if (!isResizing) return;
    panelWidth = Math.max(220, Math.min(800, resizeStartW + (resizeStartX - e.clientX)));
  }
  function onResizeEnd() {
    isResizing = false;
    window.removeEventListener('mousemove', onResizeMove);
    window.removeEventListener('mouseup', onResizeEnd);
  }
</script>

<div
  class="preview-panel"
  class:chromeless
  class:collapsed={effectiveCollapsed}
  class:fullscreen
  class:resizing={isResizing}
  style={fullscreen ? '' : chromeless ? '' : effectiveCollapsed ? 'width: 56px; flex: 0 0 56px;' : `width: ${panelWidth}px; flex: 0 0 ${panelWidth}px;`}
>
  {#if fullscreen}
    <div class="fullscreen-bar">
      {#if onExitFullscreenAction}
        <button class="fs-back" on:click={onExitFullscreenAction}>← Back</button>
      {/if}
      {#if fullscreenTitle}
        <span class="fs-title">{fullscreenTitle}</span>
      {/if}
    </div>
  {:else if !chromeless}
    {#if !effectiveCollapsed}
      <!-- svelte-ignore a11y-no-static-element-interactions -->
      <div class="resize-handle" on:mousedown={onResizeStart}>
        <div class="resize-bar"></div>
      </div>
    {/if}
    <div class="panel-header" class:collapsed-header={effectiveCollapsed}>
      <div class="panel-leading" class:collapsed-leading={effectiveCollapsed}>
        {#if !effectiveCollapsed && showFullscreenAction && onFullscreenAction}
          <button class="panel-icon-btn" on:click={onFullscreenAction} aria-label="Open fullscreen preview">
            <svg viewBox="0 0 16 16" aria-hidden="true">
              <path d="M2.5 6V2.5H6" />
              <path d="M9.5 2.5H13.5V6" />
              <path d="M13.5 10V13.5H10" />
              <path d="M6 13.5H2.5V10" />
              <path d="M6 2.5L2.5 6" />
              <path d="M10 2.5L13.5 6" />
              <path d="M13.5 10L10 13.5" />
              <path d="M6 13.5L2.5 10" />
            </svg>
          </button>
        {/if}
      </div>
      <div class="panel-title" class:collapsed-title={effectiveCollapsed}>
        {#if effectiveCollapsed}
          <span class="label">GUI</span>
        {:else if hasRendererBridge}
          <span class="badge">Renderer</span>
        {/if}
      </div>
      <div class="panel-trailing" class:collapsed-trailing={effectiveCollapsed}>
        {#if onToggleCollapsed}
          <button class="panel-icon-btn" on:click={onToggleCollapsed} aria-label={effectiveCollapsed ? 'Expand preview' : 'Collapse preview'}>
            <svg viewBox="0 0 16 16" aria-hidden="true">
              {#if effectiveCollapsed}
                <path d="M10 3L5.5 8L10 13" />
              {:else}
                <path d="M6 3L10.5 8L6 13" />
              {/if}
            </svg>
          </button>
        {/if}
      </div>
    </div>
  {/if}

  <div class="preview-body" class:hidden={effectiveCollapsed}>
    {#if isGuiApp && registry}
      {#if frameworkPending}
        <div class="preview-loading">
          <div class="preview-loading-inner">
            <div class="preview-spinner"></div>
            <span>Preparing GUI framework…</span>
          </div>
        </div>
      {:else if hasRendererBridge}
        <div bind:this={rendererContainer} class="renderer-container">
          <div bind:this={rendererSurface} class="renderer-surface" class:renderer-surface-scrollable={previewSurfaceScrollable}></div>
          {#if showRendererBridgeLoading}
            <div class="preview-loading preview-loading-overlay">
              <div class="preview-loading-inner">
                <div class="preview-spinner"></div>
                <span>Launching GUI preview…</span>
              </div>
            </div>
          {/if}
          {#if rendererBridgeError}
            <div class="render-error">{rendererBridgeError}</div>
          {/if}
        </div>
      {:else if isRenderSurface}
        <canvas id={CANVAS_ID} class="render-canvas"></canvas>
      {:else}
        <div class="render-error">GUI framework does not declare a renderer path. Update vo.ext.toml [extension.web] section.</div>
      {/if}
    {:else}
      <div class="idle-hint">
        <span>Run to preview this GUI project</span>
      </div>
    {/if}
  </div>
</div>

<style>
  .preview-panel {
    flex-shrink: 0;
    background: #181825;
    border-left: 1px solid #1e1e2e;
    display: flex;
    flex-direction: column;
    min-width: 0;
    overflow: hidden;
  }
  .preview-panel.fullscreen {
    position: fixed;
    inset: 0;
    z-index: 1000;
    border-left: none;
    background: #0d0f16;
  }
  .preview-panel.chromeless {
    flex: 1;
    border-left: none;
    background: #0d0f16;
  }
  .preview-panel.collapsed {
    min-width: 56px;
  }
  .resizing { user-select: none; }
  .resize-handle {
    width: 6px;
    position: absolute;
    left: 0;
    top: 0;
    bottom: 0;
    cursor: col-resize;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  .resize-bar { width: 2px; height: 32px; background: #313244; border-radius: 2px; }
  .panel-header {
    display: grid;
    grid-template-columns: auto 1fr auto;
    align-items: center;
    height: var(--studio-topbar-height);
    min-height: var(--studio-topbar-height);
    padding: 6px 12px 6px 14px;
    border-bottom: 1px solid #1e1e2e;
    flex-shrink: 0;
    gap: 8px;
  }
  .collapsed-header {
    position: relative;
    flex: 1;
    height: auto;
    min-height: 0;
    padding: 10px 8px;
    display: block;
  }
  .fullscreen-bar {
    display: flex;
    align-items: center;
    gap: 12px;
    height: var(--studio-topbar-height);
    min-height: var(--studio-topbar-height);
    padding: 6px 12px;
    background: #11111b;
    border-bottom: 1px solid #1e1e2e;
    flex-shrink: 0;
  }
  .panel-leading,
  .panel-trailing {
    display: flex;
    align-items: center;
    min-height: 28px;
  }
  .panel-trailing {
    justify-content: flex-end;
  }
  .collapsed-leading {
    display: none;
  }
  .collapsed-trailing {
    position: absolute;
    top: 10px;
    right: 8px;
    min-height: 28px;
  }
  .panel-title { display: flex; align-items: center; justify-content: center; gap: 8px; min-height: 28px; }
  .collapsed-title {
    height: 100%;
    flex-direction: column;
    justify-content: center;
    align-items: center;
  }
  .label { font-size: 11px; font-weight: 700; letter-spacing: 0.06em; text-transform: uppercase; color: #585b70; }
  .collapsed-title .label {
    writing-mode: vertical-rl;
    transform: rotate(180deg);
  }
  .badge {
    font-size: 10px;
    padding: 2px 6px;
    border-radius: 999px;
    background: rgba(137, 180, 250, 0.15);
    color: #89b4fa;
    font-weight: 600;
  }
  .panel-icon-btn {
    border: none;
    background: none;
    color: #89b4fa;
    cursor: pointer;
    width: 28px;
    height: 28px;
    padding: 0;
    border-radius: 8px;
    font-family: inherit;
    display: inline-flex;
    align-items: center;
    justify-content: center;
  }
  .panel-icon-btn:hover {
    background: #1e1e2e;
  }
  .panel-icon-btn svg {
    width: 16px;
    height: 16px;
    stroke: currentColor;
    fill: none;
    stroke-width: 1.7;
    stroke-linecap: round;
    stroke-linejoin: round;
  }
  .fs-back {
    border: none;
    background: none;
    color: #a6adc8;
    cursor: pointer;
    font-size: 13px;
    padding: 4px 8px;
    border-radius: 6px;
    font-family: inherit;
  }
  .fs-back:hover { background: #1e1e2e; }
  .fs-title { color: #585b70; font-size: 12px; }
  .preview-body {
    flex: 1;
    display: flex;
    flex-direction: column;
    min-width: 0;
    min-height: 0;
    overflow: hidden;
    position: relative;
  }
  .preview-body.hidden {
    display: none;
  }
  .preview-loading {
    flex: 1;
    display: grid;
    place-items: center;
    min-width: 0;
    min-height: 0;
    background: linear-gradient(180deg, #181825 0%, #11111b 100%);
  }
  .preview-loading-overlay {
    position: absolute;
    inset: 0;
    z-index: 1;
    pointer-events: none;
  }
  .preview-loading-inner {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 14px;
    color: #7f849c;
    font-size: 13px;
  }
  .preview-spinner {
    width: 28px;
    height: 28px;
    border: 3px solid #313244;
    border-top-color: #89b4fa;
    border-radius: 50%;
    animation: preview-spin 0.8s linear infinite;
  }
  @keyframes preview-spin {
    to { transform: rotate(360deg); }
  }
  .renderer-container {
    width: 100%;
    height: 100%;
    display: flex;
    flex-direction: column;
    min-width: 0;
    min-height: 0;
    overflow: hidden;
    position: relative;
    background: #181825;
  }
  .renderer-surface {
    width: 100%;
    height: 100%;
    flex: 1;
    min-width: 0;
    min-height: 0;
    overflow: hidden;
  }
  .renderer-surface.renderer-surface-scrollable {
    overflow: auto;
  }
  .render-canvas {
    width: 100%;
    height: 100%;
    display: block;
  }
  .render-error {
    position: absolute;
    inset: 12px;
    padding: 12px;
    border-radius: 8px;
    background: rgba(24, 24, 37, 0.92);
    border: 1px solid rgba(243, 139, 168, 0.35);
    color: #f38ba8;
    font-size: 12px;
    line-height: 1.5;
    white-space: pre-wrap;
    overflow: auto;
  }
  .idle-hint {
    flex: 1;
    display: flex;
    align-items: center;
    justify-content: center;
    color: #313244;
    font-size: 13px;
  }
</style>
