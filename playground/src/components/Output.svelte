<script lang="ts">
  import type { RunStatus } from '../wasm/vo.ts';

  let { stdout, stderr, status, collapsible = false, collapsed = $bindable(false) }: { 
    stdout: string; 
    stderr: string; 
    status: RunStatus;
    collapsible?: boolean;
    collapsed?: boolean;
  } = $props();

  const statusLabels: Record<RunStatus, string> = {
    idle: 'Ready',
    running: 'Running...',
    success: 'Success',
    error: 'Error',
  };
</script>

<div class="output">
  <div class="output-header" class:clickable={collapsible} role={collapsible ? "button" : undefined} tabindex={collapsible ? 0 : undefined} onclick={() => collapsible && (collapsed = !collapsed)} onkeydown={(e) => collapsible && (e.key === 'Enter' || e.key === ' ') && (collapsed = !collapsed)}>
    <div class="header-left">
      <span class="output-title">Console</span>
      {#if collapsible}
        <button class="collapse-btn">{collapsed ? '▲' : '▼'}</button>
      {/if}
    </div>
    <div class="header-right">
      <span class="status" class:success={status === 'success'} class:error={status === 'error'} class:running={status === 'running'}>
        <span class="status-dot"></span>
        {statusLabels[status]}
      </span>
    </div>
  </div>
  <div class="output-content" class:hidden={collapsed}>
    {#if stdout}
      <div class="log-entry stdout">{stdout}</div>
    {/if}
    {#if stderr}
      <div class="log-entry stderr">{stderr}</div>
    {/if}
    {#if !stdout && !stderr && status === 'idle'}
      <div class="placeholder">
        <span class="prompt">$</span> ready to run...
      </div>
    {/if}
    {#if !stdout && !stderr && status === 'success'}
      <div class="placeholder success-msg">
        <span class="prompt">></span> Program completed with no output
      </div>
    {/if}
  </div>
</div>

<style>
  .output {
    display: flex;
    flex-direction: column;
    height: 100%;
    background: var(--bg-primary);
  }

  .output-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0 16px;
    height: 40px;
    background: var(--bg-secondary);
    border-bottom: 1px solid var(--border);
    flex-shrink: 0;
  }

  .output-header.clickable {
    cursor: pointer;
  }

  .output-header.clickable:hover {
    background: var(--bg-tertiary);
  }

  .header-left {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .output-title {
    font-weight: 600;
    font-size: 13px;
    color: var(--text-secondary);
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .collapse-btn {
    background: none;
    border: none;
    color: var(--text-secondary);
    cursor: pointer;
    padding: 2px 6px;
    font-size: 10px;
  }

  .output-content.hidden {
    display: none;
  }

  .status {
    font-size: 12px;
    display: flex;
    align-items: center;
    gap: 6px;
    color: var(--text-secondary);
    font-family: var(--font-mono);
  }

  .status-dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: var(--text-tertiary);
  }

  .status.success { color: var(--success); }
  .status.success .status-dot { background: var(--success); }

  .status.error { color: var(--error); }
  .status.error .status-dot { background: var(--error); }

  .status.running { color: var(--accent); }
  .status.running .status-dot { 
    background: var(--accent); 
    animation: pulse 1s infinite;
  }

  @keyframes pulse {
    0% { opacity: 1; }
    50% { opacity: 0.4; }
    100% { opacity: 1; }
  }

  .output-content {
    flex: 1;
    padding: 16px;
    overflow: auto;
    font-family: var(--font-mono);
    font-size: 13px;
    line-height: 1.6;
    background: var(--bg-primary);
  }

  .log-entry {
    white-space: pre-wrap;
    word-wrap: break-word;
    margin-bottom: 8px;
  }

  .stdout {
    color: var(--text-primary);
  }

  .stderr {
    color: var(--error);
    background: rgba(239, 68, 68, 0.05);
    padding: 8px;
    border-radius: 4px;
    border-left: 2px solid var(--error);
  }

  .placeholder {
    color: var(--text-tertiary);
    font-style: italic;
    display: flex;
    gap: 8px;
  }

  .prompt {
    color: var(--accent);
    font-style: normal;
    font-weight: bold;
  }

  .success-msg .prompt {
    color: var(--success);
  }
</style>
