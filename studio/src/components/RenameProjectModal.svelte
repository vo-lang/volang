<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import type { ProjectEntry } from '../stores/projects';

  export let project: ProjectEntry;

  const dispatch = createEventDispatcher<{
    close: void;
    confirm: { project: ProjectEntry; newName: string };
  }>();

  let value = project.name;
  let saving = false;
  let error = '';
  let inputEl: HTMLInputElement;

  function focus(el: HTMLInputElement) {
    el.focus();
    el.select();
  }

  async function confirm() {
    if (!value.trim() || saving) return;
    dispatch('confirm', { project, newName: value.trim() });
  }
</script>

<!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
<div class="backdrop" on:click={() => !saving && dispatch('close')}>
  <div class="modal" role="dialog" aria-modal="true" aria-labelledby="rename-title" on:click|stopPropagation={() => {}}>
    <h3 class="title" id="rename-title">Rename Project</h3>
    <p class="desc">Rename local project and GitHub reference together.</p>

    <input
      class="input"
      use:focus
      bind:this={inputEl}
      bind:value
      disabled={saving}
      on:keydown={(e) => e.key === 'Enter' && confirm()}
    />

    {#if error}
      <div class="error">{error}</div>
    {/if}

    <div class="actions">
      <button class="confirm-btn" on:click={confirm} disabled={!value.trim() || saving}>
        {saving ? 'Renaming…' : 'Rename'}
      </button>
      <button class="cancel-btn" on:click={() => dispatch('close')} disabled={saving}>Cancel</button>
    </div>
  </div>
</div>

<style>
  .backdrop {
    position: fixed; inset: 0; z-index: 100;
    background: rgba(0, 0, 0, 0.65);
    backdrop-filter: blur(4px);
    display: flex; align-items: center; justify-content: center;
    padding: 24px;
    animation: fade-in 0.15s ease;
  }
  @keyframes fade-in { from { opacity: 0; } to { opacity: 1; } }

  .modal {
    background: #1e1e2e; border: 1px solid #313244;
    border-radius: 12px; box-shadow: 0 16px 48px rgba(0,0,0,0.6);
    width: 100%; max-width: 360px; padding: 24px;
    display: flex; flex-direction: column; gap: 12px;
    animation: slide-up 0.18s ease;
  }
  @keyframes slide-up { from { transform: translateY(10px); opacity: 0; } to { transform: none; opacity: 1; } }

  .title { font-size: 15px; font-weight: 700; color: #cdd6f4; margin: 0; }
  .desc { font-size: 12px; color: #585b70; margin: 0; line-height: 1.5; }

  .input {
    background: #181825; border: 1px solid #313244;
    color: #cdd6f4; font-size: 13px; padding: 9px 11px;
    border-radius: 7px; outline: none; font-family: inherit; box-sizing: border-box; width: 100%;
  }
  .input:focus { border-color: #89b4fa; box-shadow: 0 0 0 2px rgba(137,180,250,0.15); }

  .error { font-size: 12px; color: #f38ba8; }

  .actions { display: flex; gap: 8px; }
  .confirm-btn {
    flex: 1; background: linear-gradient(135deg, #89b4fa, #74c7ec);
    color: #1e1e2e; font-size: 13px; font-weight: 700;
    padding: 9px 14px; border: none; border-radius: 7px;
    cursor: pointer; font-family: inherit; transition: opacity 0.15s;
  }
  .confirm-btn:disabled { opacity: 0.5; cursor: not-allowed; }
  .cancel-btn {
    background: #313244; color: #a6adc8; font-size: 13px;
    padding: 9px 14px; border: 1px solid #45475a; border-radius: 7px;
    cursor: pointer; font-family: inherit; transition: background 0.15s;
  }
  .cancel-btn:hover:not(:disabled) { background: #45475a; }
</style>
