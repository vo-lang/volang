<script lang="ts">
  import { explorer } from '../stores/explorer';
  import type { AppMode } from '../stores/explorer';

  const modes: { id: AppMode; icon: string; label: string; title: string }[] = [
    { id: 'manage',  icon: '⌂', label: 'Home',   title: 'Home'            },
    { id: 'develop', icon: '⌨', label: 'Code',   title: 'Development'     },
    { id: 'run',     icon: '▶', label: 'Run',    title: 'Run / Preview'   },
  ];

  function setMode(mode: AppMode) {
    explorer.update(e => ({ ...e, appMode: mode }));
  }
</script>

<div class="sidebar">
  {#each modes as m}
    <button
      class="mode-btn"
      class:active={$explorer.appMode === m.id}
      title={m.title}
      on:click={() => setMode(m.id)}
    >
      <span class="icon">{m.icon}</span>
      <span class="label">{m.label}</span>
    </button>
  {/each}
</div>

<style>
  .sidebar {
    width: 52px;
    background: #11111b;
    border-right: 1px solid #1e1e2e;
    display: flex;
    flex-direction: column;
    align-items: center;
    padding: 10px 0;
    gap: 2px;
    flex-shrink: 0;
  }

  .mode-btn {
    width: 44px;
    height: 52px;
    border: none;
    background: none;
    color: #45475a;
    cursor: pointer;
    border-radius: 8px;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 4px;
    transition: background 0.1s, color 0.1s;
    position: relative;
    font-family: inherit;
  }

  .mode-btn::before {
    content: '';
    position: absolute;
    left: 0;
    top: 12px;
    bottom: 12px;
    width: 2px;
    border-radius: 0 2px 2px 0;
    background: transparent;
    transition: background 0.15s;
  }

  .mode-btn:hover {
    background: #1e1e2e;
    color: #a6adc8;
  }

  .mode-btn.active {
    color: #cdd6f4;
    background: #1e1e2e;
  }

  .mode-btn.active::before {
    background: #89b4fa;
  }

  .icon {
    font-size: 18px;
    line-height: 1;
  }

  .label {
    font-size: 9px;
    font-weight: 700;
    letter-spacing: 0.05em;
    text-transform: uppercase;
  }
</style>
