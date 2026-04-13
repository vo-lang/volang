<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import type { ManagedProject, ProjectSyncState } from '../../lib/project_catalog/types';

  export let x = 0;
  export let y = 0;
  export let project: ManagedProject;
  export let state: ProjectSyncState;
  export let hasGitHub = false;
  export let busy = false;
  export let checkingSync = false;

  const dispatch = createEventDispatcher<{
    close: void;
    open: void;
    share: void;
    rename: void;
    diff: void;
    push: void;
    pull: void;
    remote: void;
    delete: void;
    deleteRemote: void;
    deleteEverywhere: void;
    forget: void;
  }>();

  function doAction(type: 'open' | 'share' | 'rename' | 'diff' | 'push' | 'pull' | 'remote' | 'delete' | 'deleteRemote' | 'deleteEverywhere' | 'forget'): void {
    if (busy) return;
    dispatch('close');
    dispatch(type);
  }
</script>

<div
  class="backdrop"
  role="button"
  tabindex="0"
  aria-label="Close project actions"
  on:click|self={() => dispatch('close')}
  on:keydown={(event) => (event.key === 'Escape' || event.key === 'Enter' || event.key === ' ') && dispatch('close')}
>
  <div class="menu" style={`left:${x}px;top:${y}px`} role="menu" tabindex="-1" aria-label="Project actions">
    <button class="item" on:click={() => doAction('open')} disabled={busy}>Open</button>
    {#if project.remote?.kind === 'repo' && project.remote.owner && project.remote.repo}
      <button class="item" on:click={() => doAction('share')} disabled={busy}>Copy Share Link</button>
    {/if}
    <button class="item" on:click={() => doAction('rename')} disabled={busy}>Rename</button>
    {#if project.localPath && project.remote}
      <button class="item" on:click={() => doAction('diff')} disabled={busy}>Compare Local vs Cloud</button>
    {/if}
    {#if hasGitHub && project.localPath}
      <button class="item" on:click={() => doAction('push')} disabled={busy || checkingSync || state === 'remote-ahead'}>Upload Local → Cloud</button>
    {/if}
    {#if project.remote}
      <button class="item" on:click={() => doAction('pull')} disabled={busy || checkingSync || state === 'local-ahead'}>Download Cloud → Local</button>
      <button class="item" on:click={() => doAction('remote')} disabled={busy}>Open on GitHub</button>
    {/if}
    <div class="sep"></div>
    {#if project.remote && !project.localPath}
      <button class="item danger" on:click={() => doAction('deleteRemote')} disabled={busy}>Delete Cloud Copy</button>
      <button class="item danger" on:click={() => doAction('forget')} disabled={busy}>Remove from List</button>
    {:else if project.remote}
      <button class="item danger" on:click={() => doAction('delete')} disabled={busy}>Remove Local Copy</button>
      <button class="item danger" on:click={() => doAction('deleteRemote')} disabled={busy}>Delete Cloud Copy</button>
      <button class="item danger" on:click={() => doAction('deleteEverywhere')} disabled={busy}>Delete Everywhere</button>
    {:else}
      <button class="item danger" on:click={() => doAction('delete')} disabled={busy}>Delete Project</button>
    {/if}
  </div>
</div>

<style>
  .backdrop {
    position: fixed;
    inset: 0;
    z-index: 180;
  }
  .menu {
    position: fixed;
    min-width: 200px;
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 10px;
    box-shadow: 0 16px 40px rgba(0, 0, 0, 0.5);
    padding: 6px;
    display: flex;
    flex-direction: column;
    gap: 2px;
    z-index: 181;
  }
  .item {
    border: none;
    background: transparent;
    color: #cdd6f4;
    text-align: left;
    font: inherit;
    padding: 9px 10px;
    border-radius: 8px;
    cursor: pointer;
  }
  .item:hover:not(:disabled) {
    background: #313244;
  }
  .item:disabled {
    opacity: 0.45;
    cursor: not-allowed;
  }
  .danger {
    color: #f38ba8;
  }
  .danger:hover:not(:disabled) {
    background: rgba(243, 139, 168, 0.12);
  }
  .sep {
    height: 1px;
    background: #313244;
    margin: 4px 2px;
  }
</style>
