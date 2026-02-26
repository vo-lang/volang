<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import { explorer } from '../stores/explorer';
  import { actions } from '../lib/actions';

  $: target = $explorer.contextMenu;

  function close() {
    explorer.update(e => ({ ...e, contextMenu: null }));
  }

  function onGlobalMousedown(evt: MouseEvent) {
    if ($explorer.contextMenu && !(evt.target as HTMLElement).closest('.ctx-menu')) {
      close();
    }
  }

  function onGlobalKeydown(evt: KeyboardEvent) {
    if (evt.key === 'Escape' && $explorer.contextMenu) {
      close();
    }
  }

  onMount(() => {
    window.addEventListener('mousedown', onGlobalMousedown, { capture: true });
    window.addEventListener('keydown', onGlobalKeydown);
  });

  onDestroy(() => {
    window.removeEventListener('mousedown', onGlobalMousedown, { capture: true });
    window.removeEventListener('keydown', onGlobalKeydown);
  });
</script>

{#if target}
  {@const t = target}
  <div
    class="ctx-menu"
    style="left:{t.x}px; top:{t.y}px"
    role="menu"
  >
    {#if t.isDir}
      <button role="menuitem" on:click={() => actions.startCreate(t.path, false)}>
        <span class="icon">+</span> New File
      </button>
      <button role="menuitem" on:click={() => actions.startCreate(t.path, true)}>
        <span class="icon">+</span> New Folder
      </button>
      <div class="separator"></div>
    {/if}
    <button role="menuitem" on:click={() => actions.startRename(t.path, t.name)}>
      <span class="icon">✎</span> Rename
    </button>
    <button
      role="menuitem"
      class="danger"
      on:click={() => actions.deleteEntry(t.path, t.isDir)}
    >
      <span class="icon">✕</span> Delete
    </button>
    <div class="separator"></div>
    <button role="menuitem" on:click={() => { navigator.clipboard?.writeText(t.path); close(); }}>
      <span class="icon">⎎</span> Copy Path
    </button>
  </div>
{/if}

<style>
  .ctx-menu {
    position: fixed;
    z-index: 1000;
    background: #1e1e2e;
    border: 1px solid #45475a;
    border-radius: 6px;
    padding: 4px 0;
    min-width: 172px;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.6);
    font-size: 13px;
  }

  button {
    display: flex;
    align-items: center;
    gap: 8px;
    width: 100%;
    text-align: left;
    padding: 6px 14px;
    border: none;
    background: none;
    color: #cdd6f4;
    cursor: pointer;
    font-size: 13px;
    white-space: nowrap;
  }

  button:hover {
    background: #313244;
  }

  button.danger {
    color: #f38ba8;
  }

  button.danger:hover {
    background: #3b1f2b;
  }

  .icon {
    width: 14px;
    text-align: center;
    opacity: 0.7;
    font-size: 12px;
    flex-shrink: 0;
  }

  .separator {
    border-top: 1px solid #313244;
    margin: 4px 0;
  }

</style>
