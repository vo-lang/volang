<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import type { ManagedProject } from '../../lib/project_catalog/types';

  export let project: ManagedProject;
  export let busy = false;

  const dispatch = createEventDispatcher<{
    close: void;
    confirm: { name: string };
  }>();

  let name = '';
  $: name = project?.name ?? '';

  function submit(): void {
    dispatch('confirm', { name });
  }
</script>

<svelte:window on:keydown={(event) => event.key === 'Escape' && dispatch('close')} />

<div
  class="backdrop"
  role="button"
  tabindex="0"
  aria-label="Close dialog overlay"
  on:click|self={() => dispatch('close')}
  on:keydown={(event) => event.key === 'Escape' && dispatch('close')}
>
  <div class="modal" role="dialog" aria-modal="true" aria-label="Rename project">
    <h3>Rename Project</h3>
    <p>Rename the local project and its GitHub remote identity together.</p>
    <input bind:value={name} class="name-input" on:keydown={(event) => event.key === 'Enter' && submit()} />
    <div class="actions">
      <button class="secondary" on:click={() => dispatch('close')} disabled={busy}>Cancel</button>
      <button class="primary" on:click={submit} disabled={busy || !name.trim()}>{busy ? 'Renaming…' : 'Rename'}</button>
    </div>
  </div>
</div>

<style>
  .backdrop {
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.72);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 200;
  }
  .modal {
    width: min(100%, 420px);
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 16px;
    padding: 20px;
    display: flex;
    flex-direction: column;
    gap: 14px;
  }
  h3 {
    margin: 0;
    color: #cdd6f4;
    font-size: 17px;
  }
  p {
    margin: 0;
    color: #7f849c;
    font-size: 13px;
    line-height: 1.6;
  }
  .name-input {
    width: 100%;
    box-sizing: border-box;
    padding: 11px 13px;
    border-radius: 10px;
    border: 1px solid #313244;
    background: #181825;
    color: #cdd6f4;
    font-size: 13px;
    outline: none;
  }
  .name-input:focus {
    border-color: #89b4fa;
    box-shadow: 0 0 0 2px rgba(137, 180, 250, 0.16);
  }
  .actions {
    display: flex;
    justify-content: flex-end;
    gap: 10px;
  }
  button {
    border: none;
    border-radius: 10px;
    padding: 10px 14px;
    font: inherit;
    cursor: pointer;
  }
  .secondary {
    background: #313244;
    color: #cdd6f4;
  }
  .primary {
    background: #89b4fa;
    color: #11111b;
    font-weight: 700;
  }
</style>
