<script lang="ts">
  export let output: string = '';
  export let compileError: string = '';
</script>

<div class="output-panel">
  <div class="panel-header">
    <span class="label">Output</span>
    {#if compileError}
      <span class="badge error-badge">Error</span>
    {:else if output}
      <span class="badge ok-badge">OK</span>
    {/if}
  </div>
  <div class="panel-body">
    {#if compileError}
      <pre class="error-text">{compileError}</pre>
    {:else}
      <pre class="output-text">{output || '(no output — click Run to execute)'}</pre>
    {/if}
  </div>
</div>

<style>
  .output-panel {
    flex: 0 0 400px;
    background: #181825;
    border-left: 1px solid #313244;
    display: flex;
    flex-direction: column;
    overflow: hidden;
  }

  .panel-header {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 8px 12px;
    border-bottom: 1px solid #313244;
    flex-shrink: 0;
  }

  .label {
    color: #6c7086;
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.08em;
  }

  .badge {
    font-size: 10px;
    font-weight: 700;
    padding: 1px 6px;
    border-radius: 3px;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  .error-badge { background: #45122080; color: #f38ba8; }
  .ok-badge    { background: #17321480; color: #a6e3a1; }

  .panel-body {
    flex: 1;
    overflow: auto;
    padding: 12px;
  }

  .output-text,
  .error-text {
    font-size: 13px;
    font-family: 'JetBrains Mono', 'Fira Code', 'Cascadia Code', monospace;
    white-space: pre-wrap;
    word-break: break-word;
    margin: 0;
    line-height: 1.6;
  }

  .output-text { color: #cdd6f4; }
  .error-text  { color: #f38ba8; }
</style>
