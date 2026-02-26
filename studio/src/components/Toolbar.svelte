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
    on:click={actions.runCode}
    disabled={isRunning}
  >
    {isRunning ? '⏳ Running…' : '▶ Run'}
  </button>
  <button
    class="btn btn-stop"
    on:click={actions.stopCode}
    disabled={!isRunning}
  >
    ■ Stop
  </button>
  <span class="filename">{fileName}{dirty ? ' *' : ''}</span>
  <span class="spacer"></span>
  <span class="title">Vibe Studio</span>
</div>

<style>
  .toolbar {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 6px 12px;
    background: #181825;
    border-bottom: 1px solid #313244;
    flex-shrink: 0;
    height: 40px;
  }

  .btn {
    border: none;
    border-radius: 4px;
    padding: 4px 14px;
    cursor: pointer;
    font-weight: 600;
    font-size: 13px;
    transition: opacity 0.15s;
  }

  .btn:disabled {
    opacity: 0.4;
    cursor: not-allowed;
  }

  .btn-run  { background: #22c55e; color: #fff; }
  .btn-stop { background: #ef4444; color: #fff; }

  .filename {
    color: #888;
    font-size: 13px;
  }

  .spacer { flex: 1; }

  .title {
    color: #cdd6f4;
    font-weight: 700;
    font-size: 14px;
    letter-spacing: 0.02em;
  }
</style>
