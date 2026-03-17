<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import type { ManagedProject, ProjectSyncState } from '../../lib/project_catalog/types';
  import { projectStatusPresentation } from '../../lib/project_catalog/types';

  export let project: ManagedProject;
  export let state: ProjectSyncState;
  export let busy = false;
  export let checkingSync = false;

  const dispatch = createEventDispatcher<{
    open: void;
    menu: { x: number; y: number };
  }>();

  $: status = projectStatusPresentation(project, { state, checkingSync });

  function open(): void {
    if (busy) return;
    dispatch('open');
  }

  function openMenu(event: MouseEvent): void {
    if (busy) return;
    dispatch('menu', { x: event.clientX, y: event.clientY });
  }

  function handleKeydown(event: KeyboardEvent): void {
    if (busy) return;
    if (event.key === 'Enter' || event.key === ' ') {
      event.preventDefault();
      open();
    }
  }
</script>

<div
  class="card"
  class:busy
  role="button"
  tabindex={busy ? -1 : 0}
  aria-disabled={busy}
  on:click={open}
  on:keydown={handleKeydown}
  on:contextmenu|preventDefault={openMenu}
>
  <div class="row">
    <div class="icon" class:single={project.type === 'single'} class:module={project.type === 'module'}>
      {#if project.type === 'single'}VO{:else}MOD{/if}
    </div>
    <div class="info">
      <div class="name">{project.name}</div>
      <div class="meta">
        <span class={`tag ${status.tone}`}>
          {#if status.glyph}{status.glyph} {/if}{status.label}
        </span>
      </div>
    </div>
    <button class="more" type="button" aria-label="Project actions" disabled={busy} on:click|stopPropagation={openMenu}>⋯</button>
  </div>
</div>

<style>
  .card {
    border: 1px solid #252535;
    background: #181825;
    border-radius: 10px;
    padding: 14px;
    cursor: pointer;
    color: #cdd6f4;
    transition: border-color 0.12s, background 0.12s;
  }
  .card:hover:not(.busy) {
    border-color: #313244;
    background: #1e1e2e;
  }
  .card.busy {
    opacity: 0.5;
    cursor: wait;
  }
  .row {
    display: flex;
    align-items: center;
    gap: 12px;
  }
  .icon {
    width: 36px;
    height: 28px;
    border-radius: 6px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 10px;
    font-weight: 800;
    letter-spacing: 0.06em;
    flex-shrink: 0;
  }
  .icon.single {
    background: rgba(116, 199, 236, 0.12);
    color: #74c7ec;
  }
  .icon.module {
    background: rgba(137, 180, 250, 0.12);
    color: #89b4fa;
  }
  .info {
    flex: 1;
    min-width: 0;
    display: flex;
    flex-direction: column;
    gap: 3px;
  }
  .name {
    font-size: 14px;
    font-weight: 700;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .meta {
    display: flex;
    align-items: center;
    gap: 6px;
  }
  .tag {
    font-size: 11px;
    font-weight: 600;
  }
  .tag.local {
    color: #585b70;
  }
  .tag.cloud {
    color: #a6adc8;
  }
  .tag.synced {
    color: #a6e3a1;
  }
  .tag.pending {
    color: #f9e2af;
  }
  .tag.danger {
    color: #f38ba8;
  }
  .tag.checking {
    color: #89b4fa;
  }
  .more {
    border: none;
    background: transparent;
    color: #45475a;
    font-size: 18px;
    line-height: 1;
    cursor: pointer;
    padding: 4px;
    border-radius: 6px;
    flex-shrink: 0;
  }
  .more:hover:not(:disabled) {
    color: #a6adc8;
    background: rgba(205, 214, 244, 0.06);
  }
  .more:disabled {
    opacity: 0.45;
    cursor: default;
  }
</style>
