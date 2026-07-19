<script lang="ts">
  import type { ServiceRegistry } from '../lib/services/service_registry';
  import type { FsEntry } from '../lib/types';
  import { ide } from '../stores/ide';
  import Console from './Console.svelte';
  import Editor from './Editor.svelte';
  import FileTree from './FileTree.svelte';
  import PreviewPanel from './PreviewPanel.svelte';
  import Toolbar from './Toolbar.svelte';

  export let registry: ServiceRegistry | null = null;
  export let explorerEntries: FsEntry[] = [];
  export let currentDir = '';
  export let sessionRoot = '';
  export let showExplorer = false;
  export let isSingleFileSession = false;
  export let isGuiProject = false;
  export let projectHasGui = false;
  export let previewCollapsed = false;
  export let outputExpanded = false;
  export let previewTitle = '';
  export let onSave: () => void = () => {};
  export let onShare: () => void = () => {};
  export let onRun: () => void = () => {};
  export let onRunFullscreen: () => void = () => {};
  export let onStop: () => void = () => {};
  export let onSetProjectHasGui: (hasGui: boolean) => void = () => {};
  export let onOpenEntry: (entry: FsEntry) => void = () => {};
  export let onGoParent: () => void = () => {};
  export let onExitFullscreen: () => void = () => {};
  export let onTogglePreviewCollapsed: () => void = () => {};

  interface SizeLimits {
    min: number;
    max: number;
  }

  const SPLITTER_SIZE = 7;
  const DEFAULT_EXPLORER_MIN = 160;
  const DEFAULT_EXPLORER_MAX = 420;
  const MIN_CODE_WIDTH = 280;
  const DEFAULT_EDITOR_MIN = 180;
  const DEFAULT_CONSOLE_MIN = 112;

  let workbenchGrid: HTMLDivElement | undefined;
  let codeColumn: HTMLDivElement | undefined;
  let workbenchWidth = 0;
  let codeColumnHeight = 0;
  let explorerWidthOverride: number | null = null;
  let editorHeightOverride: number | null = null;
  let explorerDragging = false;
  let editorDragging = false;

  function clamp(value: number, min: number, max: number): number {
    return Math.min(max, Math.max(min, value));
  }

  function explorerSizeLimits(width: number): SizeLimits {
    const availableForExplorer = Math.max(120, width - MIN_CODE_WIDTH - SPLITTER_SIZE);
    const min = Math.min(DEFAULT_EXPLORER_MIN, availableForExplorer);
    return {
      min,
      max: Math.max(min, Math.min(DEFAULT_EXPLORER_MAX, availableForExplorer)),
    };
  }

  function editorSizeLimits(height: number): SizeLimits {
    const available = Math.max(0, height - SPLITTER_SIZE);
    const preferredEditorMin = Math.min(DEFAULT_EDITOR_MIN, Math.max(80, available * 0.42));
    const consoleMin = Math.min(DEFAULT_CONSOLE_MIN, Math.max(64, available * 0.28));
    const max = Math.max(0, available - consoleMin);
    const min = Math.min(preferredEditorMin, max);
    return { min, max: Math.max(min, max) };
  }

  function resizeExplorerAt(clientX: number): void {
    if (!workbenchGrid) return;
    const rect = workbenchGrid.getBoundingClientRect();
    const limits = explorerSizeLimits(rect.width);
    explorerWidthOverride = clamp(clientX - rect.left, limits.min, limits.max);
  }

  function resizeEditorAt(clientY: number): void {
    if (!codeColumn) return;
    const rect = codeColumn.getBoundingClientRect();
    const limits = editorSizeLimits(rect.height);
    editorHeightOverride = clamp(clientY - rect.top, limits.min, limits.max);
  }

  function beginExplorerResize(event: PointerEvent): void {
    if (event.button !== 0 || !event.isPrimary) return;
    event.preventDefault();
    const target = event.currentTarget as HTMLElement;
    target.focus();
    target.setPointerCapture(event.pointerId);
    explorerDragging = true;
    resizeExplorerAt(event.clientX);
  }

  function moveExplorerResize(event: PointerEvent): void {
    if (explorerDragging) resizeExplorerAt(event.clientX);
  }

  function endExplorerResize(event: PointerEvent): void {
    const target = event.currentTarget as HTMLElement;
    if (target.hasPointerCapture(event.pointerId)) target.releasePointerCapture(event.pointerId);
    explorerDragging = false;
  }

  function beginEditorResize(event: PointerEvent): void {
    if (event.button !== 0 || !event.isPrimary) return;
    event.preventDefault();
    const target = event.currentTarget as HTMLElement;
    target.focus();
    target.setPointerCapture(event.pointerId);
    editorDragging = true;
    resizeEditorAt(event.clientY);
  }

  function moveEditorResize(event: PointerEvent): void {
    if (editorDragging) resizeEditorAt(event.clientY);
  }

  function endEditorResize(event: PointerEvent): void {
    const target = event.currentTarget as HTMLElement;
    if (target.hasPointerCapture(event.pointerId)) target.releasePointerCapture(event.pointerId);
    editorDragging = false;
  }

  function resizeStep(event: KeyboardEvent): number {
    return event.shiftKey ? 40 : 16;
  }

  function handleExplorerResizeKey(event: KeyboardEvent): void {
    let next: number;
    switch (event.key) {
      case 'ArrowLeft':
        next = explorerWidth - resizeStep(event);
        break;
      case 'ArrowRight':
        next = explorerWidth + resizeStep(event);
        break;
      case 'Home':
        next = explorerLimits.min;
        break;
      case 'End':
        next = explorerLimits.max;
        break;
      default:
        return;
    }
    event.preventDefault();
    explorerWidthOverride = clamp(next, explorerLimits.min, explorerLimits.max);
  }

  function handleEditorResizeKey(event: KeyboardEvent): void {
    let next: number;
    switch (event.key) {
      case 'ArrowUp':
        next = editorHeight - resizeStep(event);
        break;
      case 'ArrowDown':
        next = editorHeight + resizeStep(event);
        break;
      case 'Home':
        next = editorLimits.min;
        break;
      case 'End':
        next = editorLimits.max;
        break;
      default:
        return;
    }
    event.preventDefault();
    editorHeightOverride = clamp(next, editorLimits.min, editorLimits.max);
  }

  $: measuredWorkbenchWidth = workbenchWidth || 1000;
  $: measuredCodeColumnHeight = codeColumnHeight || 600;
  $: explorerLimits = explorerSizeLimits(measuredWorkbenchWidth);
  $: explorerWidth = clamp(
    explorerWidthOverride ?? clamp(measuredWorkbenchWidth * 0.22, 190, 300),
    explorerLimits.min,
    explorerLimits.max,
  );
  $: editorLimits = editorSizeLimits(measuredCodeColumnHeight);
  $: editorHeight = clamp(
    editorHeightOverride ?? (measuredCodeColumnHeight - SPLITTER_SIZE) * 0.7,
    editorLimits.min,
    editorLimits.max,
  );
</script>

<div class="dev-workbench" class:single-file={isSingleFileSession}>
  <Toolbar
    onSave={onSave}
    onShare={onShare}
    onRun={onRun}
    onRunFullscreen={onRunFullscreen}
    onStop={onStop}
    onSetProjectHasGui={onSetProjectHasGui}
    projectHasGui={projectHasGui}
  />

  <div class="dev-body">
    <div class="workbench-surface" class:with-preview={isGuiProject}>
      <div
        class="workbench-grid"
        class:with-explorer={showExplorer}
        bind:this={workbenchGrid}
        bind:clientWidth={workbenchWidth}
        style={`--explorer-width: ${explorerWidth}px`}
      >
        {#if showExplorer}
          <aside id="project-explorer-pane" class="explorer-pane" aria-label="Project explorer">
            <FileTree
              entries={explorerEntries}
              {currentDir}
              {sessionRoot}
              onOpenEntry={onOpenEntry}
              onGoParent={onGoParent}
            />
          </aside>
          <!-- svelte-ignore a11y_no_noninteractive_tabindex a11y_no_noninteractive_element_interactions (focusable ARIA separator) -->
          <div
            class="splitter explorer-splitter"
            class:dragging={explorerDragging}
            role="separator"
            tabindex="0"
            aria-label="Resize project explorer"
            aria-orientation="vertical"
            aria-controls="project-explorer-pane code-workspace"
            aria-valuemin={Math.round(explorerLimits.min)}
            aria-valuemax={Math.round(explorerLimits.max)}
            aria-valuenow={Math.round(explorerWidth)}
            aria-valuetext={`${Math.round(explorerWidth)} pixel explorer width`}
            title="Resize project explorer (Left and Right Arrow keys)"
            on:pointerdown={beginExplorerResize}
            on:pointermove={moveExplorerResize}
            on:pointerup={endExplorerResize}
            on:pointercancel={endExplorerResize}
            on:lostpointercapture={() => (explorerDragging = false)}
            on:keydown={handleExplorerResizeKey}
          ></div>
        {/if}

        <div
          id="code-workspace"
          class="code-column"
          bind:this={codeColumn}
          bind:clientHeight={codeColumnHeight}
          style={`--editor-height: ${editorHeight}px`}
        >
          <section id="code-editor-pane" class="editor-pane" aria-label="Code editor">
            <Editor />
          </section>
          <!-- svelte-ignore a11y_no_noninteractive_tabindex a11y_no_noninteractive_element_interactions (focusable ARIA separator) -->
          <div
            class="splitter editor-splitter"
            class:dragging={editorDragging}
            role="separator"
            tabindex="0"
            aria-label="Resize editor and program output"
            aria-orientation="horizontal"
            aria-controls="code-editor-pane program-output-pane"
            aria-valuemin={Math.round(editorLimits.min)}
            aria-valuemax={Math.round(editorLimits.max)}
            aria-valuenow={Math.round(editorHeight)}
            aria-valuetext={`${Math.round(editorHeight)} pixel editor height`}
            title="Resize editor and program output (Up and Down Arrow keys)"
            on:pointerdown={beginEditorResize}
            on:pointermove={moveEditorResize}
            on:pointerup={endEditorResize}
            on:pointercancel={endEditorResize}
            on:lostpointercapture={() => (editorDragging = false)}
            on:keydown={handleEditorResizeKey}
          ></div>
          <section id="program-output-pane" class="console-pane" aria-label="Program output">
            <Console mode="pane" />
          </section>
        </div>
      </div>
    </div>

    {#if isGuiProject && registry}
      <PreviewPanel
        {registry}
        collapsed={previewCollapsed}
        fullscreen={outputExpanded}
        fullscreenTitle={previewTitle}
        showFullscreenAction={true}
        onFullscreenAction={() => ide.update((state) => ({ ...state, outputExpanded: true, previewCollapsed: false }))}
        onExitFullscreenAction={onExitFullscreen}
        onToggleCollapsed={onTogglePreviewCollapsed}
      />
    {/if}
  </div>
</div>

<style>
  .dev-workbench {
    display: flex;
    flex: 1;
    flex-direction: column;
    min-width: 0;
    min-height: 0;
    overflow: hidden;
    background: var(--surface-canvas);
  }

  .dev-workbench.single-file {
    background: linear-gradient(180deg, rgba(11, 14, 24, 0.98), rgba(16, 21, 34, 0.96));
  }

  .dev-body {
    display: flex;
    flex: 1;
    min-width: 0;
    min-height: 0;
    overflow: hidden;
  }

  .workbench-surface {
    flex: 1;
    min-width: 0;
    min-height: 0;
    overflow: hidden;
  }

  .workbench-surface.with-preview {
    border-right: 1px solid var(--line-subtle);
  }

  .workbench-grid {
    width: 100%;
    height: 100%;
    display: grid;
    grid-template-columns: minmax(0, 1fr);
    min-width: 0;
    min-height: 0;
  }

  .workbench-grid.with-explorer {
    grid-template-columns: minmax(0, var(--explorer-width)) 7px minmax(0, 1fr);
  }

  .explorer-pane {
    min-width: 0;
    min-height: 0;
    display: flex;
    overflow: hidden;
    background: var(--surface-0);
  }

  .code-column {
    min-width: 0;
    min-height: 0;
    display: grid;
    grid-template-rows: minmax(0, var(--editor-height)) 7px minmax(0, 1fr);
    overflow: hidden;
  }

  .editor-pane,
  .console-pane {
    min-width: 0;
    min-height: 0;
    display: flex;
    overflow: hidden;
  }

  .console-pane {
    background: var(--surface-0);
  }

  .splitter {
    position: relative;
    z-index: 2;
    min-width: 0;
    min-height: 0;
    border: 0;
    outline: 0;
    background: transparent;
    touch-action: none;
    user-select: none;
  }

  .splitter::before {
    content: '';
    position: absolute;
    pointer-events: none;
    background: var(--line);
    transition: background 120ms ease, box-shadow 120ms ease;
  }

  .explorer-splitter {
    cursor: col-resize;
  }

  .explorer-splitter::before {
    top: 0;
    bottom: 0;
    left: 3px;
    width: 1px;
  }

  .editor-splitter {
    cursor: row-resize;
  }

  .editor-splitter::before {
    top: 3px;
    right: 0;
    left: 0;
    height: 1px;
  }

  .splitter:hover::before,
  .splitter:focus-visible::before,
  .splitter.dragging::before {
    background: var(--accent-soft);
    box-shadow: 0 0 0 1px color-mix(in srgb, var(--accent) 28%, transparent);
  }

  .splitter:focus-visible {
    outline: 2px solid color-mix(in srgb, var(--accent-soft) 72%, transparent);
    outline-offset: -2px;
  }

  .explorer-pane :global(> *),
  .editor-pane :global(> *),
  .console-pane :global(> *) {
    flex: 1;
    min-width: 0;
    min-height: 0;
  }

  @media (max-width: 620px) {
    .workbench-grid.with-explorer {
      grid-template-columns: 1fr;
      grid-template-rows: minmax(120px, 26vh) minmax(0, 1fr);
    }

    .explorer-splitter {
      display: none;
    }

    .explorer-pane {
      border-right: 0;
      border-bottom: 1px solid var(--line-subtle);
    }

    .workbench-surface.with-preview .workbench-grid.with-explorer {
      grid-template-rows: minmax(96px, 20vh) minmax(0, 1fr);
    }
  }

  @media (max-width: 720px) {
    .dev-body {
      flex-direction: column;
    }

    .workbench-surface {
      width: 100%;
    }

    .workbench-surface.with-preview {
      flex: 2 1 0;
      min-height: 200px;
      border-right: 0;
      border-bottom: 1px solid var(--line-subtle);
    }
  }
</style>
