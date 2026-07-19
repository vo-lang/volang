<script lang="ts">
  import { createEventDispatcher, onMount, tick } from 'svelte';
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

  const VIEWPORT_MARGIN = 8;
  let menuElement: HTMLDivElement;
  let menuLeft = x;
  let menuTop = y;

  function clamp(value: number, min: number, max: number): number {
    return Math.min(max, Math.max(min, value));
  }

  function enabledItems(): HTMLButtonElement[] {
    if (!menuElement) return [];
    return Array.from(menuElement.querySelectorAll<HTMLButtonElement>('[role="menuitem"]:not(:disabled)'));
  }

  function focusItem(index: number): void {
    const items = enabledItems();
    if (items.length === 0) {
      menuElement?.focus();
      return;
    }
    items[(index + items.length) % items.length]?.focus();
  }

  function updatePosition(): void {
    if (!menuElement) return;
    const rect = menuElement.getBoundingClientRect();
    const viewportWidth = document.documentElement.clientWidth;
    const viewportHeight = document.documentElement.clientHeight;
    const maxLeft = Math.max(VIEWPORT_MARGIN, viewportWidth - rect.width - VIEWPORT_MARGIN);
    const maxTop = Math.max(VIEWPORT_MARGIN, viewportHeight - rect.height - VIEWPORT_MARGIN);
    menuLeft = clamp(x, VIEWPORT_MARGIN, maxLeft);
    menuTop = clamp(y, VIEWPORT_MARGIN, maxTop);
  }

  function closeMenu(): void {
    dispatch('close');
  }

  function handleMenuKeydown(event: KeyboardEvent): void {
    const items = enabledItems();
    const currentIndex = items.findIndex((item) => item === document.activeElement);
    let nextIndex: number | null = null;

    switch (event.key) {
      case 'ArrowDown':
        nextIndex = currentIndex + 1;
        break;
      case 'ArrowUp':
        nextIndex = currentIndex <= 0 ? items.length - 1 : currentIndex - 1;
        break;
      case 'Home':
        nextIndex = 0;
        break;
      case 'End':
        nextIndex = items.length - 1;
        break;
      case 'Escape':
      case 'Tab':
        event.preventDefault();
        event.stopPropagation();
        closeMenu();
        return;
      default:
        if (event.key.length === 1 && !event.metaKey && !event.ctrlKey && !event.altKey) {
          const query = event.key.toLocaleLowerCase();
          const ordered = items.slice(currentIndex + 1).concat(items.slice(0, currentIndex + 1));
          const match = ordered.find((item) => item.textContent?.trim().toLocaleLowerCase().startsWith(query));
          if (match) {
            event.preventDefault();
            match.focus();
          }
        }
        return;
    }

    if (nextIndex !== null && items.length > 0) {
      event.preventDefault();
      focusItem(nextIndex);
    }
  }

  function doAction(type: 'open' | 'share' | 'rename' | 'diff' | 'push' | 'pull' | 'remote' | 'delete' | 'deleteRemote' | 'deleteEverywhere' | 'forget'): void {
    if (busy) return;
    dispatch('close');
    dispatch(type);
  }

  onMount(() => {
    const reposition = (): void => updatePosition();
    window.addEventListener('resize', reposition);
    void tick().then(() => {
      updatePosition();
      focusItem(0);
    });
    return () => window.removeEventListener('resize', reposition);
  });
</script>

<!-- svelte-ignore a11y_no_static_element_interactions (pointer backdrop; keyboard dismissal is handled by the menu) -->
<div
  class="backdrop"
  role="presentation"
  on:pointerdown|self={closeMenu}
>
  <div
    bind:this={menuElement}
    class="menu"
    style={`left:${menuLeft}px;top:${menuTop}px`}
    role="menu"
    tabindex="-1"
    aria-label={`Actions for ${project.name}`}
    on:keydown={handleMenuKeydown}
  >
    <button class="item" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('open')} disabled={busy}>Open</button>
    {#if project.remote?.kind === 'repo' && project.remote.owner && project.remote.repo}
      <button class="item" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('share')} disabled={busy}>Copy Share Link</button>
    {/if}
    <button class="item" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('rename')} disabled={busy}>Rename</button>
    {#if project.localPath && project.remote}
      <button class="item" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('diff')} disabled={busy}>Compare Local vs Cloud</button>
    {/if}
    {#if hasGitHub && project.localPath}
      <button class="item" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('push')} disabled={busy || checkingSync || state === 'remote-ahead'}>Upload Local → Cloud</button>
    {/if}
    {#if project.remote}
      <button class="item" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('pull')} disabled={busy || checkingSync || state === 'local-ahead'}>Download Cloud → Local</button>
      <button class="item" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('remote')} disabled={busy}>Open on GitHub</button>
    {/if}
    <div class="sep" role="separator"></div>
    {#if project.remote && !project.localPath}
      <button class="item danger" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('deleteRemote')} disabled={busy}>Delete Cloud Copy</button>
      <button class="item danger" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('forget')} disabled={busy}>Remove from List</button>
    {:else if project.remote}
      <button class="item danger" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('delete')} disabled={busy}>Remove Local Copy</button>
      <button class="item danger" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('deleteRemote')} disabled={busy}>Delete Cloud Copy</button>
      <button class="item danger" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('deleteEverywhere')} disabled={busy}>Delete Everywhere</button>
    {:else}
      <button class="item danger" type="button" role="menuitem" tabindex="-1" on:click={() => doAction('delete')} disabled={busy}>Delete Project</button>
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
    max-width: calc(100vw - 16px);
    max-height: calc(100dvh - 16px);
    overflow-y: auto;
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
  .item:focus-visible {
    outline: 2px solid var(--accent-soft);
    outline-offset: -2px;
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
