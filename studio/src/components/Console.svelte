<script lang="ts">
  import { afterUpdate, onMount } from 'svelte';
  import { ide, consoleClear } from '../stores/ide';
  import type { ConsoleLine, ConsoleLineKind, RunStatus } from '../stores/ide';

  export let mode: 'panel' | 'fullscreen' = 'panel';

  let scrollContainer: HTMLDivElement;
  let userScrolledUp = false;
  let showScrollPill = false;

  $: lines = $ide.consoleLines;
  $: runStatus = $ide.runStatus;
  $: durationMs = $ide.runDurationMs;
  $: showTs = $ide.consoleShowTimestamps;
  $: wordWrap = $ide.consoleWordWrap;

  function toggleTimestamps() {
    ide.update(s => ({ ...s, consoleShowTimestamps: !s.consoleShowTimestamps }));
  }

  function toggleWordWrap() {
    ide.update(s => ({ ...s, consoleWordWrap: !s.consoleWordWrap }));
  }

  function copyAll() {
    const text = lines.map(l => l.text).join('\n');
    navigator.clipboard.writeText(text).catch(() => {});
  }

  function handleScroll() {
    if (!scrollContainer) return;
    const { scrollTop, scrollHeight, clientHeight } = scrollContainer;
    const atBottom = scrollHeight - scrollTop - clientHeight < 30;
    userScrolledUp = !atBottom;
    showScrollPill = userScrolledUp && lines.length > 0;
  }

  function scrollToBottom() {
    if (!scrollContainer) return;
    scrollContainer.scrollTop = scrollContainer.scrollHeight;
    userScrolledUp = false;
    showScrollPill = false;
  }

  afterUpdate(() => {
    if (!userScrolledUp && scrollContainer) {
      scrollContainer.scrollTop = scrollContainer.scrollHeight;
    }
  });

  function formatTs(ts: number): string {
    const d = new Date(ts);
    const h = String(d.getHours()).padStart(2, '0');
    const m = String(d.getMinutes()).padStart(2, '0');
    const s = String(d.getSeconds()).padStart(2, '0');
    return `${h}:${m}:${s}`;
  }

  function formatDuration(ms: number): string {
    if (ms < 1000) return `${ms}ms`;
    return `${(ms / 1000).toFixed(2)}s`;
  }

  function statusIcon(status: RunStatus): string {
    switch (status) {
      case 'preparing': return '◌';
      case 'compiling': return '◌';
      case 'running': return '●';
      case 'done': return '✓';
      case 'error': return '✗';
      default: return '';
    }
  }

  function lineClass(kind: ConsoleLineKind): string {
    switch (kind) {
      case 'stdout': return 'line-stdout';
      case 'stderr': return 'line-stderr';
      case 'system': return 'line-system';
      case 'success': return 'line-success';
    }
  }

  // Drag resize for panel mode
  let panelHeight = 180;
  let isDragging = false;
  let dragStartY = 0;
  let dragStartHeight = 0;

  function onDragStart(e: MouseEvent) {
    if (mode !== 'panel') return;
    isDragging = true;
    dragStartY = e.clientY;
    dragStartHeight = panelHeight;
    e.preventDefault();
    window.addEventListener('mousemove', onDragMove);
    window.addEventListener('mouseup', onDragEnd);
  }

  function onDragMove(e: MouseEvent) {
    if (!isDragging) return;
    const delta = dragStartY - e.clientY;
    panelHeight = Math.max(80, Math.min(600, dragStartHeight + delta));
  }

  function onDragEnd() {
    isDragging = false;
    window.removeEventListener('mousemove', onDragMove);
    window.removeEventListener('mouseup', onDragEnd);
  }
</script>

<div
  class="console-root {mode}"
  style={mode === 'panel' ? `height: ${panelHeight}px` : ''}
>
  {#if mode === 'panel'}
    <!-- svelte-ignore a11y-no-static-element-interactions -->
    <div class="drag-handle" on:mousedown={onDragStart}>
      <div class="drag-bar"></div>
    </div>
  {/if}

  <div class="console-header">
      <div class="header-left">
        <span class="console-title">Console</span>
        {#if runStatus !== 'idle'}
          <span class="status-badge status-{runStatus}">
            <span class="status-icon">{statusIcon(runStatus)}</span>
            <span class="status-text">
              {runStatus === 'preparing' ? 'Preparing' :
               runStatus === 'compiling' ? 'Compiling' :
               runStatus === 'running' ? 'Running' :
               runStatus === 'done' ? 'Done' : 'Error'}
            </span>
            {#if durationMs !== null && (runStatus === 'done' || runStatus === 'error')}
              <span class="status-duration">{formatDuration(durationMs)}</span>
            {/if}
          </span>
        {/if}
      </div>
      <div class="header-actions">
        <button
          class="action-btn"
          class:active={showTs}
          title="Toggle timestamps"
          on:click={toggleTimestamps}
        >
          <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
            <circle cx="12" cy="12" r="10"/><polyline points="12 6 12 12 16 14"/>
          </svg>
        </button>
        <button
          class="action-btn"
          class:active={wordWrap}
          title="Toggle word wrap"
          on:click={toggleWordWrap}
        >
          <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
            <path d="M3 6h18"/><path d="M3 12h15a3 3 0 1 1 0 6h-4"/><polyline points="16 16 14 18 16 20"/><path d="M3 18h7"/>
          </svg>
        </button>
        <button class="action-btn" title="Copy all" on:click={copyAll}>
          <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
            <rect x="9" y="9" width="13" height="13" rx="2" ry="2"/><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"/>
          </svg>
        </button>
        <button class="action-btn" title="Clear console" on:click={consoleClear}>
          <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
            <circle cx="12" cy="12" r="10"/><line x1="15" y1="9" x2="9" y2="15"/><line x1="9" y1="9" x2="15" y2="15"/>
          </svg>
        </button>
    </div>
  </div>

  <div
    class="console-body"
    class:word-wrap={wordWrap}
    bind:this={scrollContainer}
    on:scroll={handleScroll}
  >
    {#each lines as line, i (i)}
      <div class="console-line {lineClass(line.kind)}">
        {#if showTs}
          <span class="line-ts">{formatTs(line.ts)}</span>
        {/if}
        <span class="line-text">{line.text}</span>
      </div>
    {/each}
  </div>

  {#if showScrollPill}
    <button class="scroll-pill" on:click={scrollToBottom}>
      ↓ New output
    </button>
  {/if}
</div>

<style>
  .console-root {
    display: flex;
    flex-direction: column;
    background: #0a0a0f;
    overflow: hidden;
    position: relative;
  }

  .console-root.panel {
    border-top: 1px solid #1a1a2e;
    flex-shrink: 0;
  }

  .console-root.fullscreen {
    flex: 1;
  }

  /* Drag handle */
  .drag-handle {
    height: 6px;
    cursor: ns-resize;
    display: flex;
    align-items: center;
    justify-content: center;
    flex-shrink: 0;
    background: #0a0a0f;
  }

  .drag-handle:hover .drag-bar {
    background: #3b3b5c;
  }

  .drag-bar {
    width: 40px;
    height: 2px;
    border-radius: 1px;
    background: #1e1e35;
    transition: background 0.15s;
  }

  /* Header */
  .console-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 4px 12px;
    flex-shrink: 0;
    min-height: 32px;
    border-bottom: 1px solid #111122;
  }

  .header-left {
    display: flex;
    align-items: center;
    gap: 10px;
  }

  .console-title {
    font-size: 11px;
    font-weight: 700;
    color: #4a4a6a;
    text-transform: uppercase;
    letter-spacing: 0.08em;
  }

  .status-badge {
    display: inline-flex;
    align-items: center;
    gap: 5px;
    font-size: 11px;
    font-weight: 500;
    padding: 1px 8px;
    border-radius: 10px;
    line-height: 1.4;
  }

  .status-compiling {
    color: #94a3b8;
    background: #1e293b;
  }

  .status-preparing .status-icon,
  .status-compiling .status-icon {
    animation: spin 1.2s linear infinite;
  }

  .status-preparing {
    color: #cbd5e1;
    background: #0f172a;
  }

  .status-running {
    color: #4ade80;
    background: #052e16;
  }

  .status-running .status-icon {
    animation: pulse 1.5s ease-in-out infinite;
  }

  .status-done {
    color: #4ade80;
    background: #052e16;
  }

  .status-error {
    color: #f87171;
    background: #2d0f0f;
  }

  .status-duration {
    opacity: 0.7;
    font-size: 10px;
  }

  @keyframes spin {
    from { transform: rotate(0deg); }
    to   { transform: rotate(360deg); }
  }

  @keyframes pulse {
    0%, 100% { opacity: 1; }
    50%      { opacity: 0.4; }
  }

  /* Header actions */
  .header-actions {
    display: flex;
    align-items: center;
    gap: 2px;
  }

  .action-btn {
    width: 26px;
    height: 26px;
    display: flex;
    align-items: center;
    justify-content: center;
    background: none;
    border: none;
    border-radius: 4px;
    color: #3b3b5c;
    cursor: pointer;
    transition: color 0.12s, background 0.12s;
  }

  .action-btn:hover {
    color: #8888aa;
    background: #151528;
  }

  .action-btn.active {
    color: #89b4fa;
  }

  /* Console body */
  .console-body {
    flex: 1;
    overflow-x: auto;
    overflow-y: auto;
    padding: 8px 0;
    font-family: 'JetBrains Mono', 'Fira Code', 'Cascadia Code', 'SF Mono', 'Consolas', monospace;
    font-size: 12.5px;
    line-height: 1.65;
  }

  .console-body.word-wrap {
    overflow-x: hidden;
  }

  .console-body.word-wrap .line-text {
    white-space: pre-wrap;
    word-break: break-all;
  }

  /* Scrollbar */
  .console-body::-webkit-scrollbar {
    width: 6px;
    height: 6px;
  }

  .console-body::-webkit-scrollbar-track {
    background: transparent;
  }

  .console-body::-webkit-scrollbar-thumb {
    background: #1e1e35;
    border-radius: 3px;
  }

  .console-body::-webkit-scrollbar-thumb:hover {
    background: #2e2e4e;
  }

  /* Lines */
  .console-line {
    display: flex;
    align-items: baseline;
    padding: 0 14px;
    min-height: 20px;
  }

  .console-line:hover {
    background: #0d0d18;
  }

  .line-ts {
    flex-shrink: 0;
    width: 62px;
    color: #2a2a45;
    font-size: 10.5px;
    margin-right: 10px;
    user-select: none;
  }

  .line-text {
    white-space: pre;
    flex: 1;
    min-width: 0;
  }

  /* Line kinds */
  .line-stdout .line-text {
    color: #c8cee0;
  }

  .line-stderr .line-text {
    color: #f87171;
  }

  .line-system .line-text {
    color: #4a4a6a;
    font-style: italic;
  }

  .line-success .line-text {
    color: #4ade80;
  }

  /* Scroll pill */
  .scroll-pill {
    position: absolute;
    bottom: 12px;
    left: 50%;
    transform: translateX(-50%);
    background: #1e293b;
    color: #89b4fa;
    border: 1px solid #2d3f5f;
    border-radius: 16px;
    padding: 4px 14px;
    font-size: 11px;
    font-weight: 600;
    cursor: pointer;
    z-index: 10;
    transition: background 0.12s;
    font-family: inherit;
  }

  .scroll-pill:hover {
    background: #253349;
  }
</style>
