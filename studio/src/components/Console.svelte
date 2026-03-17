<script lang="ts">
  import { afterUpdate } from 'svelte';
  import { console_, consoleClear, consoleToggleTimestamps, consoleToggleWordWrap, type ConsoleLine, type ConsoleLineKind } from '../stores/console';
  import { runtime } from '../stores/runtime';
  import type { RunStatus } from '../stores/ide';

  export let mode: 'panel' | 'fullscreen' = 'panel';

  let scrollEl: HTMLDivElement;
  let userScrolledUp = false;

  $: lines = $console_.lines;
  $: showTs = $console_.showTimestamps;
  $: wordWrap = $console_.wordWrap;
  $: runStatus = ((): RunStatus => {
    const s = $runtime;
    if (s.status === 'idle') return 'idle';
    if (s.status === 'running') return 'running';
    if (s.lastError) return 'error';
    return 'done';
  })();
  $: durationMs = null as number | null;

  let panelHeight = 180;
  let isDragging = false;
  let dragStartY = 0;
  let dragStartH = 0;

  function onDragStart(e: MouseEvent) {
    if (mode !== 'panel') return;
    isDragging = true;
    dragStartY = e.clientY;
    dragStartH = panelHeight;
    e.preventDefault();
    window.addEventListener('mousemove', onDragMove);
    window.addEventListener('mouseup', onDragEnd);
  }
  function onDragMove(e: MouseEvent) {
    if (!isDragging) return;
    panelHeight = Math.max(80, Math.min(600, dragStartH + (dragStartY - e.clientY)));
  }
  function onDragEnd() {
    isDragging = false;
    window.removeEventListener('mousemove', onDragMove);
    window.removeEventListener('mouseup', onDragEnd);
  }

  function handleScroll() {
    if (!scrollEl) return;
    const { scrollTop, scrollHeight, clientHeight } = scrollEl;
    userScrolledUp = scrollHeight - scrollTop - clientHeight > 30;
  }

  afterUpdate(() => {
    if (!userScrolledUp && scrollEl) {
      scrollEl.scrollTop = scrollEl.scrollHeight;
    }
  });

  function lineClass(kind: ConsoleLineKind): string {
    return { stdout: 'stdout', stderr: 'stderr', system: 'system', success: 'success' }[kind] ?? 'stdout';
  }

  function statusIcon(s: RunStatus): string {
    return { preparing: '◌', compiling: '◌', running: '●', done: '✓', error: '✗', idle: '' }[s] ?? '';
  }

  function fmtTs(ts: number): string {
    const d = new Date(ts);
    return `${String(d.getHours()).padStart(2,'0')}:${String(d.getMinutes()).padStart(2,'0')}:${String(d.getSeconds()).padStart(2,'0')}`;
  }

  function fmtDuration(ms: number): string {
    return ms < 1000 ? `${ms}ms` : `${(ms / 1000).toFixed(2)}s`;
  }
</script>

<div
  class="console-root {mode}"
  style={mode === 'panel' ? `height: ${panelHeight}px` : ''}
>
  {#if mode === 'panel'}
    <!-- svelte-ignore a11y-no-static-element-interactions -->
    <div class="drag-handle" on:mousedown={onDragStart}><div class="drag-bar"></div></div>
  {/if}

  <div class="con-header">
    <div class="header-left">
      <span class="con-title">Console</span>
      {#if runStatus !== 'idle'}
        <span class="status-badge status-{runStatus}">
          <span class="status-icon">{statusIcon(runStatus)}</span>
          <span class="status-text">{runStatus}</span>
          {#if durationMs !== null && (runStatus === 'done' || runStatus === 'error')}
            <span class="status-dur">{fmtDuration(durationMs)}</span>
          {/if}
        </span>
      {/if}
    </div>
    <div class="header-actions">
      <button class="act-btn" class:active={showTs} title="Timestamps"
        on:click={consoleToggleTimestamps}>T</button>
      <button class="act-btn" class:active={wordWrap} title="Word wrap"
        on:click={consoleToggleWordWrap}>W</button>
      <button class="act-btn" title="Clear" on:click={consoleClear}>✕</button>
    </div>
  </div>

  <div class="lines" bind:this={scrollEl} on:scroll={handleScroll}>
    {#each lines as line}
      <div class="line {lineClass(line.kind)}" class:nowrap={!wordWrap}>
        {#if showTs}<span class="ts">{fmtTs(line.ts)}</span>{/if}
        <span class="text">{line.text}</span>
      </div>
    {/each}
    {#if lines.length === 0 && runStatus === 'idle'}
      <div class="empty-hint">Run a file to see output</div>
    {/if}
  </div>
</div>

<style>
  .console-root {
    display: flex;
    flex-direction: column;
    background: #11111b;
    border-top: 1px solid #1e1e2e;
    min-height: 0;
    overflow: hidden;
  }
  .console-root.fullscreen {
    flex: 1;
    border-top: none;
  }
  .drag-handle {
    height: 6px;
    cursor: row-resize;
    display: flex;
    align-items: center;
    justify-content: center;
    flex-shrink: 0;
  }
  .drag-bar { width: 32px; height: 2px; border-radius: 2px; background: #313244; }
  .con-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 4px 10px;
    border-bottom: 1px solid #1e1e2e;
    flex-shrink: 0;
  }
  .header-left { display: flex; align-items: center; gap: 8px; }
  .con-title { font-size: 11px; font-weight: 700; letter-spacing: 0.06em; text-transform: uppercase; color: #585b70; }
  .status-badge {
    display: flex;
    align-items: center;
    gap: 4px;
    font-size: 11px;
    padding: 2px 7px;
    border-radius: 999px;
    background: rgba(100,100,180,0.12);
    color: #89b4fa;
  }
  .status-badge.status-running { color: #a6e3a1; background: rgba(100,200,100,0.1); }
  .status-badge.status-done    { color: #a6e3a1; background: rgba(100,200,100,0.1); }
  .status-badge.status-error   { color: #f38ba8; background: rgba(200,100,100,0.1); }
  .status-icon { font-size: 10px; }
  .status-dur  { color: #585b70; font-size: 10px; }
  .header-actions { display: flex; gap: 4px; }
  .act-btn {
    border: none;
    background: none;
    color: #45475a;
    cursor: pointer;
    padding: 2px 6px;
    border-radius: 4px;
    font-size: 11px;
    font-weight: 700;
    font-family: inherit;
    transition: background 0.1s, color 0.1s;
  }
  .act-btn:hover { background: #1e1e2e; color: #a6adc8; }
  .act-btn.active { color: #89b4fa; }
  .lines {
    flex: 1;
    overflow-y: auto;
    padding: 6px 0;
    font-family: 'JetBrains Mono', 'Fira Mono', monospace;
    font-size: 12px;
    line-height: 1.55;
  }
  .line {
    display: flex;
    gap: 8px;
    padding: 1px 10px;
    word-break: break-all;
  }
  .line.nowrap { white-space: nowrap; word-break: normal; }
  .stdout { color: #cdd6f4; }
  .stderr { color: #f38ba8; }
  .system { color: #585b70; }
  .success { color: #a6e3a1; }
  .ts { color: #45475a; flex-shrink: 0; user-select: none; }
  .text { min-width: 0; }
  .empty-hint { color: #313244; font-size: 12px; padding: 12px 10px; }
</style>
