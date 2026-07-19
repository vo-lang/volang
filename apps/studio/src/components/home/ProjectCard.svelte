<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import type { ManagedProject, ProjectSyncState } from '../../lib/project_catalog/types';
  import { projectStatusPresentation } from '../../lib/project_catalog/types';

  export let project: ManagedProject;
  export let state: ProjectSyncState;
  export let busy = false;
  export let checkingSync = false;
  export let menuOpen = false;

  const dispatch = createEventDispatcher<{
    open: void;
    menu: { x: number; y: number; trigger: HTMLButtonElement };
  }>();

  $: status = projectStatusPresentation(project, { state, checkingSync });
  let menuButton: HTMLButtonElement;

  function open(): void {
    if (busy) return;
    dispatch('open');
  }

  function openMenu(event: MouseEvent): void {
    if (busy) return;
    const keyboardClick = event.type === 'click' && event.detail === 0;
    const rect = menuButton.getBoundingClientRect();
    dispatch('menu', {
      x: keyboardClick ? rect.right : event.clientX,
      y: keyboardClick ? rect.bottom : event.clientY,
      trigger: menuButton,
    });
  }

</script>

<article
  class="card"
  class:busy
  on:contextmenu|preventDefault={openMenu}
>
  <button class="project-main" type="button" disabled={busy} on:click={open}>
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
    </div>
  </button>
  <button
    class="more"
    type="button"
    bind:this={menuButton}
    aria-label={`Project actions for ${project.name}`}
    aria-haspopup="menu"
    aria-expanded={menuOpen}
    disabled={busy}
    on:click={openMenu}
  >⋯</button>
</article>

<style>
  .card {
    position: relative;
    border: 1px solid var(--line-subtle);
    background: var(--surface-1);
    border-radius: 14px;
    color: var(--text);
    transition: border-color 0.12s, background 0.12s;
  }
  .card:hover:not(.busy) {
    border-color: var(--line-strong);
    background: var(--surface-raised);
  }
  .card.busy {
    opacity: 0.5;
  }
  .project-main {
    width: 100%;
    display: block;
    border: 0;
    padding: 14px 48px 14px 14px;
    color: inherit;
    background: transparent;
    cursor: pointer;
    text-align: left;
  }
  .project-main:disabled {
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
    font-size: 11px;
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
    font-size: 12px;
    font-weight: 600;
  }
  .tag.local {
    color: var(--text-faint);
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
    position: absolute;
    top: 50%;
    right: 10px;
    width: 32px;
    height: 32px;
    transform: translateY(-50%);
    border: none;
    background: transparent;
    color: var(--text-faint);
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
  .more:focus-visible { transform: translateY(-50%); }
  .more:disabled {
    opacity: 0.45;
    cursor: default;
  }
</style>
