<script lang="ts">
  import { ide } from '../stores/ide';
  import { actions } from '../lib/actions';

  $: fileName = $ide.activeFilePath
    ? $ide.activeFilePath.split('/').pop() ?? ''
    : '';
  $: isRunning = $ide.isRunning;
  $: dirty = $ide.dirty;
</script>

<div class="toolbar">
  <button
    class="btn btn-run"
    on:click={() => actions.runCode()}
    disabled={isRunning}
  >
    {isRunning ? '▶ Running…' : '▶ Run'}
  </button>
  <button
    class="btn btn-launch"
    on:click={() => actions.launchApp()}
    disabled={isRunning}
  >
    ▶ Run Fullscreen
  </button>
  <button
    class="btn btn-stop"
    on:click={() => actions.stopCode()}
    disabled={!isRunning}
  >
    ■ Stop
  </button>

  <span class="sep"></span>

  <span class="filename">{fileName || 'Untitled'}{dirty ? ' ·' : ''}</span>
</div>

<style>
  .toolbar {
    display: flex;
    align-items: center;
    gap: 6px;
    height: 36px;
    padding: 0 10px;
    background: #181825;
    border-bottom: 1px solid #1e1e2e;
    flex-shrink: 0;
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

  .btn:disabled {
    opacity: 0.35;
    cursor: not-allowed;
  }

  .btn-run {
    color: #fff;
    background: #22c55e;
  }
  .btn-run:hover:not(:disabled) { background: #16a34a; }

  .btn-launch {
    color: #fff;
    background: #3b82f6;
  }
  .btn-launch:hover:not(:disabled) { background: #2563eb; }

  .btn-stop {
    color: #fff;
    background: #ef4444;
  }
  .btn-stop:hover:not(:disabled) { background: #dc2626; }

  .sep {
    width: 1px;
    height: 16px;
    background: #313244;
    margin: 0 4px;
  }

  .filename {
    color: #6c7086;
    font-size: 12px;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    min-width: 0;
  }
</style>
