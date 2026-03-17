<script lang="ts">
  import { editor } from '../stores/editor';
  import { runtime } from '../stores/runtime';
  import { session } from '../stores/session';

  export let onSave: () => void = () => {};
  export let onRun: () => void = () => {};
  export let onRunFullscreen: () => void = () => {};
  export let onStop: () => void = () => {};
  export let onSetProjectHasGui: (hasGui: boolean) => void = () => {};
  export let projectHasGui = false;

  $: sessionEntryName = $session.entryPath ? ($session.entryPath.split('/').pop() ?? '') : '';
  $: fileName = $editor.activeFilePath
    ? ($editor.activeFilePath.split('/').pop() ?? '')
    : sessionEntryName;
  $: isRunning = $runtime.isRunning;
  $: dirty = $editor.dirty;
  $: projectMode = $session.projectMode;
  $: projectModeLabel = projectMode === 'single-file' ? 'Single File' : projectMode === 'module' ? 'Project' : 'Session';
  $: isGuiProject = projectHasGui;
</script>

<div class="toolbar">
  <div class="action-group">
    <button class="btn btn-save" on:click={onSave} disabled={!dirty || isRunning}>
      💾 Save
    </button>
    <button class="btn btn-run" on:click={onRun} disabled={isRunning}>
      {isRunning ? '▶ Running…' : '▶ Run'}
    </button>
    {#if isGuiProject}
      <button class="btn btn-launch" on:click={onRunFullscreen} disabled={isRunning}>
        ▶ Fullscreen
      </button>
    {/if}
    <button class="btn btn-stop" on:click={onStop} disabled={!isRunning}>
      ■ Stop
    </button>
  </div>

  <div class="meta-group">
    <span class="session-badge">{projectModeLabel}</span>
    <span class="filename">{fileName || 'Untitled'}{dirty ? ' ·' : ''}</span>
  </div>

  <div class="spacer"></div>

  <div class="project-kind">
    <span class="kind-label">GUI</span>
    <button
      class="gui-toggle"
      class:active={projectHasGui}
      on:click={() => onSetProjectHasGui(!projectHasGui)}
      disabled={isRunning}
      aria-pressed={projectHasGui}
    >
      <span class="gui-toggle-track">
        <span class="gui-toggle-thumb"></span>
      </span>
    </button>
  </div>
</div>

<style>
  .toolbar {
    display: flex;
    align-items: center;
    gap: 12px;
    height: var(--studio-topbar-height);
    min-height: var(--studio-topbar-height);
    padding: 0 10px;
    background: #181825;
    border-bottom: 1px solid #1e1e2e;
    flex-shrink: 0;
  }
  .action-group,
  .meta-group,
  .project-kind {
    display: flex;
    align-items: center;
    gap: 8px;
    min-width: 0;
  }
  .btn {
    border: none;
    border-radius: 4px;
    padding: 4px 10px;
    cursor: pointer;
    font-weight: 600;
    font-size: 12px;
    font-family: inherit;
    transition: opacity 0.12s;
  }
  .btn:disabled { opacity: 0.35; cursor: not-allowed; }
  .btn-save { color: #11111b; background: #f9e2af; }
  .btn-save:hover:not(:disabled) { background: #fab387; }
  .btn-run  { color: #fff; background: #22c55e; }
  .btn-run:hover:not(:disabled)    { background: #16a34a; }
  .btn-launch { color: #fff; background: #3b82f6; }
  .btn-launch:hover:not(:disabled) { background: #2563eb; }
  .btn-stop { color: #fff; background: #ef4444; }
  .btn-stop:hover:not(:disabled)   { background: #dc2626; }
  .spacer {
    flex: 1;
    min-width: 12px;
  }
  .session-badge {
    flex-shrink: 0;
    padding: 2px 8px;
    border-radius: 999px;
    background: rgba(137, 180, 250, 0.12);
    color: #89b4fa;
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.04em;
    text-transform: uppercase;
  }
  .filename {
    color: #6c7086;
    font-size: 12px;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    min-width: 0;
  }
  .kind-label {
    color: #6c7086;
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.04em;
    text-transform: uppercase;
  }
  .gui-toggle {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    border: none;
    background: transparent;
    color: #7f849c;
    font: inherit;
    font-size: 12px;
    font-weight: 700;
    padding: 2px 0;
    border-radius: 999px;
    cursor: pointer;
    transition: color 0.12s, opacity 0.12s;
  }
  .gui-toggle-track {
    position: relative;
    width: 38px;
    height: 22px;
    border-radius: 999px;
    background: #313244;
    transition: background 0.12s;
  }
  .gui-toggle-thumb {
    position: absolute;
    top: 3px;
    left: 3px;
    width: 16px;
    height: 16px;
    border-radius: 50%;
    background: #cdd6f4;
    transition: transform 0.12s;
  }
  .gui-toggle.active {
    color: #89b4fa;
  }
  .gui-toggle.active .gui-toggle-track {
    background: rgba(137, 180, 250, 0.8);
  }
  .gui-toggle.active .gui-toggle-thumb {
    transform: translateX(16px);
    background: #11111b;
  }
  .gui-toggle:disabled {
    opacity: 0.45;
    cursor: not-allowed;
  }
</style>
