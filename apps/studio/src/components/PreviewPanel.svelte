<script lang="ts">
  import { onDestroy, tick } from 'svelte';
  import { runtime } from '../stores/runtime';
  import {
    startRendererBridge, stopRendererBridge, deliverRenderBytes, deliverGameRenderBytes,
    restartRendererBridge,
    isRendererBridgeActive,
    loadHostBridgeModule, unloadHostBridgeModule,
    combineHostBridgeModules,
    type HostBridgeModule,
    type VfsSnapshot,
  } from '../lib/gui/renderer_bridge';
  import { clearHostBridgeForSession, setHostBridgeForSession } from '../lib/studio_wasm';
  import { planFrameworkArtifacts } from '../lib/gui/framework_artifact_plan';
  import type { ServiceRegistry } from '../lib/services/service_registry';
  import type { GuiPreview } from '../lib/services/runtime_service';
  import { frameworkJsModulePath, type FrameworkContract } from '../lib/types';

  type PreviewPanelSharedState = {
    rendererSurfaceHosts: Map<number, HTMLDivElement>;
  };

  const previewPanelGlobal = globalThis as Record<string, unknown>;
  const previewPanelSharedState = (() => {
    const existing = previewPanelGlobal.__studioPreviewPanelState as PreviewPanelSharedState | undefined;
    if (existing?.rendererSurfaceHosts instanceof Map) {
      return existing;
    }
    const created: PreviewPanelSharedState = { rendererSurfaceHosts: new Map() };
    previewPanelGlobal.__studioPreviewPanelState = created;
    return created;
  })();

  function previewFrameworks(
    primary: FrameworkContract | null,
    providers: FrameworkContract[],
  ): readonly FrameworkContract[] {
    try {
      return planFrameworkArtifacts(primary, providers).frameworks;
    } catch {
      return [];
    }
  }

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
  const canvasIdForSession = (sessionId: number): string => `canvas-${sessionId}`;

  let panelWidth = 400;
  let isResizing = false;
  let resizeStartX = 0;
  let resizeStartW = 0;
  let rendererBridgeActive = false;
  let rendererBridgeLaunching = false;
  let rendererBridgeLaunchGeneration = 0;
  let rendererBridgeLaunchAbort: AbortController | null = null;
  let rendererBridgeLaunchingSessionId: number | null = null;
  let rendererBridgeSessionId: number | null = null;
  let rendererBridgeFailedSessionId: number | null = null;
  let rendererBridgeError: string | null = null;
  let rendererBridgeRecovering = false;
  let rendererBridgeRecoveryAttemptedSessionId: number | null = null;
  let rendererContainer: HTMLDivElement | undefined;
  let rendererSurface: HTMLDivElement | undefined;
  let lastGameRenderBytes: Uint8Array | null = null;
  let previews: readonly GuiPreview[] = [];
  $: currentCanvasId = canvasIdForSession($runtime.gui.sessionId ?? 0);
  $: {
    $runtime;
    previews = registry?.runtime.listGuiPreviews() ?? [];
  }

  function previewLabel(preview: GuiPreview): string {
    const path = preview.output.entryPath;
    const file = path.slice(path.lastIndexOf('/') + 1) || path;
    return `${file} · ${preview.session.id}`;
  }

  async function selectPreview(preview: GuiPreview): Promise<void> {
    if (!registry || preview.session.id === $runtime.gui.sessionId) return;
    try {
      await registry.runtime.selectGuiPreview(preview.session);
    } catch (error) {
      rendererBridgeError = error instanceof Error ? error.message : String(error);
    }
  }

  async function closePreview(event: MouseEvent, preview: GuiPreview): Promise<void> {
    event.stopPropagation();
    if (!registry) return;
    try {
      await registry.runtime.stopGuiPreview(preview.session);
    } catch (error) {
      rendererBridgeError = error instanceof Error ? error.message : String(error);
    }
  }

  function ensureRendererSurfaceHost(sessionId: number): HTMLDivElement {
    let host = previewPanelSharedState.rendererSurfaceHosts.get(sessionId);
    if (!host) {
      host = document.createElement('div');
      host.dataset.studioPreviewSession = String(sessionId);
      previewPanelSharedState.rendererSurfaceHosts.set(sessionId, host);
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

  function attachRendererSurfaceHost(sessionId: number): HTMLDivElement | null {
    if (!rendererSurface) {
      return null;
    }
    const host = ensureRendererSurfaceHost(sessionId);
    if (host.parentElement !== rendererSurface) {
      rendererSurface.replaceChildren();
      rendererSurface.appendChild(host);
    }
    return host;
  }

  function parkRendererSurfaceHost(sessionId: number): void {
    const host = previewPanelSharedState.rendererSurfaceHosts.get(sessionId);
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

  function removeRendererSurfaceHost(sessionId: number): void {
    previewPanelSharedState.rendererSurfaceHosts.get(sessionId)?.remove();
    previewPanelSharedState.rendererSurfaceHosts.delete(sessionId);
  }

  $: isGuiApp = $runtime.kind === 'gui' && $runtime.isRunning;
  $: guiFramework = $runtime.gui.framework;
  $: providerFrameworks = $runtime.gui.providerFrameworks;
  $: bridgeFrameworks = previewFrameworks(guiFramework, providerFrameworks);
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

  $: if (
    rendererBridgeLaunchingSessionId != null
    && $runtime.gui.sessionId !== rendererBridgeLaunchingSessionId
  ) {
    rendererBridgeLaunchGeneration += 1;
    rendererBridgeLaunchAbort?.abort(new Error('Renderer bridge session changed during startup'));
    stopRendererBridge(rendererBridgeLaunchingSessionId);
  }

  // Launch renderer bridge when GUI app with renderer becomes active.
  // If the renderer bridge survived a layout transition (e.g. preview→fullscreen),
  // just re-adopt the canvas without re-launching.
  $: if (rendererBridgeActive && rendererBridgeSessionId != null && $runtime.gui.sessionId !== rendererBridgeSessionId) {
    parkRendererSurfaceHost(rendererBridgeSessionId);
    rendererBridgeActive = false;
    rendererBridgeSessionId = null;
    lastGameRenderBytes = null;
  }
  $: if (isGuiApp && hasRendererBridge && registry && $runtime.gui.moduleBytes && $runtime.gui.entryPath && $runtime.gui.sessionId != null && $runtime.gui.sessionId !== rendererBridgeFailedSessionId && !rendererBridgeActive && !rendererBridgeLaunching) {
    if (isRendererBridgeActive($runtime.gui.sessionId)) {
      rendererBridgeActive = true;
      rendererBridgeSessionId = $runtime.gui.sessionId;
      rendererBridgeFailedSessionId = null;
      tick().then(() => {
        const sessionId = $runtime.gui.sessionId!;
        const surface = attachRendererSurfaceHost(sessionId);
        if (needsManagedCanvas) {
          attachCanvas(sessionId);
        }
        if (surface && $runtime.gui.renderBytes && $runtime.gui.renderBytes.length > 0) {
          deliverRenderBytes($runtime.gui.sessionId!, surface, $runtime.gui.renderBytes);
        }
      });
    } else {
      void launchRendererBridge();
    }
  }
  $: if (!isGuiApp && rendererBridgeActive && rendererBridgeSessionId != null) {
    const live = registry?.runtime.listGuiPreviews()
      .some((preview) => preview.session.id === rendererBridgeSessionId) ?? false;
    if (live) {
      parkRendererSurfaceHost(rendererBridgeSessionId);
      rendererBridgeActive = false;
      rendererBridgeSessionId = null;
      lastGameRenderBytes = null;
    } else {
      teardownRendererBridge();
    }
  }

  $: if (rendererBridgeActive && rendererBridgeSessionId != null && $runtime.gui.renderBytes && previewPanelSharedState.rendererSurfaceHosts.has(rendererBridgeSessionId)) {
    deliverRenderBytes(rendererBridgeSessionId, previewPanelSharedState.rendererSurfaceHosts.get(rendererBridgeSessionId)!, $runtime.gui.renderBytes);
  }
  $: if (
    rendererBridgeActive
    && rendererBridgeSessionId != null
    && registry
    && $runtime.gui.gameRenderBytes
    && $runtime.gui.vfsSnapshot
    && $runtime.gui.gameRenderBytes !== lastGameRenderBytes
  ) {
    const bytes = $runtime.gui.gameRenderBytes;
    const sessionId = rendererBridgeSessionId;
    const vfsSnapshot = $runtime.gui.vfsSnapshot;
    lastGameRenderBytes = bytes;
    void deliverGameRenderBytes(sessionId, bytes)
      .then((result) => (
        result
          ? registry?.runtime.submitGameRenderResult(result, sessionId)
          : undefined
      ))
      .catch((error) => {
        const message = error instanceof Error ? error.message : String(error);
        rendererBridgeError = message;
        void recoverRendererBridge(sessionId, message, vfsSnapshot);
      });
  }

  // ---- Canvas management ----
  // The canvas is created/managed via JS (not Svelte template) so it can
  // survive component destroy/recreate during layout transitions.
  // The WebGPU surface is tied to the canvas DOM element, not its position.

  function ensureCanvas(sessionId: number): HTMLCanvasElement {
    const canvasId = canvasIdForSession(sessionId);
    let canvas = document.getElementById(canvasId) as HTMLCanvasElement | null;
    if (!canvas) {
      canvas = document.createElement('canvas');
      canvas.id = canvasId;
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

  function attachCanvas(sessionId: number): void {
    const surface = attachRendererSurfaceHost(sessionId);
    if (!surface) {
      tick().then(() => attachCanvas(sessionId));
      return;
    }
    const canvas = ensureCanvas(sessionId);
    applyCanvasStyle(canvas);
    if (canvas.parentElement !== surface) {
      surface.appendChild(canvas);
    }
    requestAnimationFrame(() => {
      if (canvas.parentElement === surface) {
        canvas.focus();
      }
    });
  }

  function removeCanvas(sessionId: number): void {
    const canvas = document.getElementById(canvasIdForSession(sessionId));
    canvas?.remove();
  }

  // ---- Lifecycle ----

  async function launchRendererBridge(): Promise<void> {
    if (!registry || rendererBridgeLaunching) return;
    const launchGeneration = ++rendererBridgeLaunchGeneration;
    const sessionId = $runtime.gui.sessionId;
    const launchAbort = new AbortController();
    rendererBridgeLaunchAbort = launchAbort;
    rendererBridgeLaunchingSessionId = sessionId;
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
      // Share the launch-bound VFS snapshot across all framework module loaders.
      const framework = $runtime.gui.framework;
      const providerFrameworks = $runtime.gui.providerFrameworks;
      const artifactPlan = planFrameworkArtifacts(framework, providerFrameworks);
      const bridgeFrameworks = artifactPlan.frameworks;
      const vfsSnapshot = $runtime.gui.vfsSnapshot;
      if (!vfsSnapshot) throw new Error('Renderer bridge VFS snapshot is unavailable');
      const vfsFiles = vfsSnapshot.files;
      const hostBridgeModules: HostBridgeModule[] = [];
      for (const provider of bridgeFrameworks) {
        const hostBridgePath = frameworkJsModulePath(provider, 'host_bridge');
        if (!hostBridgePath || !registry) {
          continue;
        }
        try {
          hostBridgeModules.push(await loadHostBridgeModule(
            sessionId,
            hostBridgePath,
            entryPath,
            vfsFiles,
            launchAbort.signal,
          ));
        } catch (e) {
          if (launchAbort.signal.aborted) throw e;
          console.warn('[PreviewPanel] host bridge module load failed, WASM host functions may be unavailable:', e);
        }
      }
      setHostBridgeForSession(
        sessionId,
        hostBridgeModules.length > 0 ? combineHostBridgeModules(hostBridgeModules) : null,
      );
      const surface = attachRendererSurfaceHost(sessionId);
      if (needsManagedCanvas) {
        attachCanvas(sessionId);
      }
      if (!surface) {
        throw new Error('Renderer bridge Surface host is unavailable');
      }
      await startRendererBridge(canvasIdForSession(sessionId), surface, registry.backend, registry.runtime, sessionId, {
        entryPath,
        moduleBytes,
        framework,
        providerFrameworks,
        onDetached: (detachedSessionId) => {
          removeCanvas(detachedSessionId);
          removeRendererSurfaceHost(detachedSessionId);
        },
        onError: (message) => {
          rendererBridgeError = message;
          void recoverRendererBridge(sessionId, message, vfsSnapshot);
        },
      }, vfsSnapshot, launchAbort.signal);
      if (launchGeneration !== rendererBridgeLaunchGeneration || !($runtime.kind === 'gui' && $runtime.isRunning) || $runtime.gui.sessionId !== sessionId) {
        stopRendererBridge(sessionId);
        if (!isRendererBridgeActive(sessionId)) {
          removeCanvas(sessionId);
          removeRendererSurfaceHost(sessionId);
        }
        return;
      }
      if (surface && $runtime.gui.renderBytes && $runtime.gui.renderBytes.length > 0) {
        deliverRenderBytes(sessionId, surface, $runtime.gui.renderBytes);
      }
      rendererBridgeActive = true;
      rendererBridgeSessionId = sessionId;
      rendererBridgeFailedSessionId = null;
      rendererBridgeRecoveryAttemptedSessionId = null;
    } catch (e) {
      if (sessionId != null) stopRendererBridge(sessionId);
      if (sessionId != null) {
        removeCanvas(sessionId);
        removeRendererSurfaceHost(sessionId);
      }
      if (sessionId != null) {
        unloadHostBridgeModule(sessionId);
        clearHostBridgeForSession(sessionId);
      }
      if (!launchAbort.signal.aborted && launchGeneration === rendererBridgeLaunchGeneration) {
        rendererBridgeFailedSessionId = sessionId;
        rendererBridgeError = e instanceof Error ? e.message : String(e);
        console.error('[PreviewPanel] renderer bridge start failed:', e);
      }
    } finally {
      if (rendererBridgeLaunchAbort === launchAbort) {
        rendererBridgeLaunchAbort = null;
        rendererBridgeLaunchingSessionId = null;
        rendererBridgeLaunching = false;
      }
    }
  }

  async function recoverRendererBridge(
    sessionId: number,
    cause: string,
    vfsSnapshot: VfsSnapshot,
  ): Promise<void> {
    if (
      rendererBridgeRecovering
      || rendererBridgeRecoveryAttemptedSessionId === sessionId
      || !registry
      || !isRendererBridgeActive(sessionId)
      || $runtime.gui.sessionId !== sessionId
    ) {
      return;
    }
    rendererBridgeRecovering = true;
    rendererBridgeRecoveryAttemptedSessionId = sessionId;
    try {
      const entryPath = $runtime.gui.entryPath;
      const moduleBytes = $runtime.gui.moduleBytes;
      if (!entryPath || !moduleBytes) {
        throw new Error('Renderer bridge recovery context is unavailable');
      }
      const surface = attachRendererSurfaceHost(sessionId);
      if (!surface) throw new Error('Renderer bridge recovery Surface host is unavailable');
      const framework = $runtime.gui.framework;
      const providerFrameworks = $runtime.gui.providerFrameworks;
      await restartRendererBridge(
        canvasIdForSession(sessionId),
        surface,
        registry.backend,
        registry.runtime,
        sessionId,
        {
          entryPath,
          moduleBytes,
          framework,
          providerFrameworks,
          onDetached: (detachedSessionId) => {
            removeCanvas(detachedSessionId);
            removeRendererSurfaceHost(detachedSessionId);
          },
          onError: (message) => {
            rendererBridgeError = message;
          },
        },
        vfsSnapshot,
      );
      if ($runtime.gui.sessionId !== sessionId) {
        stopRendererBridge(sessionId);
        return;
      }
      rendererBridgeActive = true;
      rendererBridgeSessionId = sessionId;
      rendererBridgeError = null;
      lastGameRenderBytes = null;
    } catch (error) {
      rendererBridgeActive = false;
      rendererBridgeFailedSessionId = sessionId;
      const recovery = error instanceof Error ? error.message : String(error);
      rendererBridgeError = `${cause}; WebView recovery failed: ${recovery}`;
    } finally {
      rendererBridgeRecovering = false;
    }
  }

  function teardownRendererBridge(
    sessionId = rendererBridgeSessionId ?? rendererBridgeLaunchingSessionId,
  ): void {
    rendererBridgeLaunchGeneration++;
    rendererBridgeLaunchAbort?.abort(new Error('Renderer bridge torn down during startup'));
    rendererBridgeActive = false;
    rendererBridgeSessionId = null;
    lastGameRenderBytes = null;
    stopRendererBridge(sessionId);
    if (sessionId != null) {
      removeCanvas(sessionId);
      removeRendererSurfaceHost(sessionId);
    }
    // Clean up framework artifact modules.
    if (sessionId != null) unloadHostBridgeModule(sessionId);
    if (sessionId != null) clearHostBridgeForSession(sessionId);
  }

  $: if (effectiveCollapsed && rendererBridgeActive) {
    if (rendererBridgeSessionId != null) parkRendererSurfaceHost(rendererBridgeSessionId);
  }
  $: if (!effectiveCollapsed && rendererBridgeActive && rendererSurface) {
    if (rendererBridgeSessionId != null) {
      attachRendererSurfaceHost(rendererBridgeSessionId);
      if (needsManagedCanvas) {
        attachCanvas(rendererBridgeSessionId);
      }
    }
  }

  onDestroy(() => {
    onResizeEnd();
    const sessionId = rendererBridgeSessionId;
    const running = $runtime.isRunning;
    const active = sessionId != null && isRendererBridgeActive(sessionId);
    if (running && active && $runtime.gui.sessionId === sessionId) {
      // Layout transition (e.g. preview → fullscreen): keep the renderer bridge
      // alive and park the canvas off-screen so the WebGPU surface stays valid.
      parkRendererSurfaceHost(sessionId);
    } else {
      teardownRendererBridge(sessionId);
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

  {#if !effectiveCollapsed && previews.length > 0}
    <div class="preview-tabs" role="tablist" aria-label="GUI previews">
      {#each previews as preview (preview.session.id)}
        <div
          class="preview-tab"
          class:active-preview-tab={preview.session.id === $runtime.gui.sessionId}
          role="presentation"
        >
          <button
            class="preview-tab-select"
            role="tab"
            aria-selected={preview.session.id === $runtime.gui.sessionId}
            title={preview.output.entryPath}
            on:click={() => selectPreview(preview)}
          >
            <span class="preview-tab-label">{previewLabel(preview)}</span>
          </button>
          <button
            class="preview-tab-close"
            aria-label={`Close preview ${previewLabel(preview)}`}
            on:click={(event) => closePreview(event, preview)}
          >×</button>
        </div>
      {/each}
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
        <canvas id={currentCanvasId} class="render-canvas"></canvas>
      {:else}
        <div class="render-error">GUI framework does not declare a renderer path. Update vo.mod [extension.web] section.</div>
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
    position: relative;
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
  .preview-tabs {
    display: flex;
    align-items: center;
    gap: 4px;
    min-height: 34px;
    padding: 4px 8px;
    overflow-x: auto;
    border-bottom: 1px solid #1e1e2e;
    background: #11111b;
    flex-shrink: 0;
  }
  .preview-tab {
    display: inline-flex;
    align-items: center;
    min-width: 0;
    max-width: 190px;
    border: 1px solid transparent;
    border-radius: 6px;
    background: transparent;
    color: #7f849c;
  }
  .preview-tab:hover {
    color: #cdd6f4;
    background: #181825;
  }
  .active-preview-tab {
    color: #cdd6f4;
    border-color: #313244;
    background: #1e1e2e;
  }
  .preview-tab-select {
    min-width: 0;
    padding: 4px 5px 4px 9px;
    border: 0;
    background: transparent;
    color: inherit;
    cursor: pointer;
    font: inherit;
    font-size: 11px;
  }
  .preview-tab-label {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .preview-tab-close {
    display: inline-grid;
    place-items: center;
    width: 16px;
    height: 16px;
    border-radius: 4px;
    color: #585b70;
    flex: 0 0 auto;
    padding: 0;
    border: 0;
    background: transparent;
    cursor: pointer;
    font: inherit;
  }
  .preview-tab-close:hover {
    color: #f38ba8;
    background: rgba(243, 139, 168, 0.12);
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

  @media (max-width: 720px) {
    .preview-panel:not(.fullscreen):not(.chromeless) {
      width: 100% !important;
      min-height: 140px;
      flex: 1 1 0 !important;
      border-top: 1px solid #1e1e2e;
      border-left: 0;
    }

    .preview-panel.collapsed:not(.fullscreen):not(.chromeless) {
      width: 100% !important;
      min-width: 0;
      min-height: 48px;
      height: 48px;
      flex: 0 0 48px !important;
    }

    .resize-handle {
      display: none;
    }

    .collapsed-header {
      height: 48px;
      min-height: 48px;
      padding: 6px 8px 6px 14px;
      display: grid;
      grid-template-columns: 1fr auto;
      align-items: center;
    }

    .collapsed-title {
      height: auto;
      min-height: 28px;
      flex-direction: row;
      justify-content: flex-start;
    }

    .collapsed-title .label {
      writing-mode: horizontal-tb;
      transform: none;
    }

    .collapsed-trailing {
      position: static;
      grid-column: 2;
      grid-row: 1;
    }

    .panel-trailing .panel-icon-btn svg {
      transform: rotate(90deg);
    }
  }
</style>
