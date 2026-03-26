<script lang="ts">
  import { createEventDispatcher } from 'svelte';

  export let busy = false;
  export let error = '';
  export let platform: 'native' | 'wasm' = 'wasm';
  export let defaultLocation = '';
  export let onPickDirectory: (() => Promise<string | null>) | null = null;

  const dispatch = createEventDispatcher<{
    close: void;
    create: { kind: 'single' | 'module'; name: string; location: string | undefined };
  }>();

  let kind: 'single' | 'module' = 'single';
  let name = '';
  let location = defaultLocation;

  $: isNative = platform === 'native';
  $: effectiveName = name.trim() || (kind === 'single' ? 'my_app' : 'my_project');
  $: normalizedLocation = location.trim();
  $: effectiveLocation = normalizedLocation || defaultLocation;
  $: previewPath = isNative && effectiveLocation
    ? `${effectiveLocation}/${kind === 'single' ? `${effectiveName}.vo` : `${effectiveName}/vo.mod`}`
    : kind === 'single' ? `${effectiveName}.vo` : `${effectiveName}/vo.mod`;

  async function browseLocation(): Promise<void> {
    if (!onPickDirectory) return;
    const picked = await onPickDirectory();
    if (picked) {
      location = picked;
    }
  }

  function submit(): void {
    dispatch('create', { kind, name, location: isNative && normalizedLocation ? normalizedLocation : undefined });
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
  <div class="modal" role="dialog" aria-modal="true" aria-label="Create project">
    <h3>Create New</h3>
    <p>Create a local starter, then optionally push it to GitHub later.</p>

    <div class="kind-row">
      <button class:active={kind === 'single'} on:click={() => (kind = 'single')}>Single File</button>
      <button class:active={kind === 'module'} on:click={() => (kind = 'module')}>Module Project</button>
    </div>

    <input
      bind:value={name}
      class="name-input"
      placeholder={kind === 'single' ? 'my_app' : 'my_project'}
      on:keydown={(event) => event.key === 'Enter' && submit()}
    />

    <div class="location-row">
      <label class="location-label">Location</label>
      {#if isNative}
        <div class="location-native">
          <input
            bind:value={location}
            class="location-input"
            placeholder="~/code"
          />
          <button class="browse-btn" on:click={browseLocation} disabled={busy}>Browse</button>
        </div>
      {:else}
        <span class="location-static">Browser Workspace</span>
      {/if}
    </div>

    <div class="preview">
      Creates <code>{previewPath}</code>
    </div>

    {#if error}
      <div class="error">{error}</div>
    {/if}

    <div class="actions">
      <button class="secondary" on:click={() => dispatch('close')} disabled={busy}>Cancel</button>
      <button class="primary" on:click={submit} disabled={busy || !name.trim()}>{busy ? 'Creating…' : 'Create'}</button>
    </div>
  </div>
</div>

<style>
  .backdrop {
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.72);
    backdrop-filter: blur(6px);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 200;
  }
  .modal {
    width: min(100%, 430px);
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 16px;
    padding: 22px;
    display: flex;
    flex-direction: column;
    gap: 16px;
  }
  h3 {
    margin: 0;
    color: #cdd6f4;
    font-size: 18px;
  }
  p {
    margin: 0;
    color: #7f849c;
    font-size: 13px;
    line-height: 1.6;
  }
  .kind-row {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 8px;
  }
  .kind-row button {
    border: 1px solid #313244;
    background: #181825;
    color: #cdd6f4;
    border-radius: 10px;
    padding: 10px 12px;
    cursor: pointer;
    font: inherit;
    font-weight: 700;
  }
  .kind-row button.active {
    border-color: #89b4fa;
    background: rgba(137, 180, 250, 0.12);
    color: #89b4fa;
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
  .location-row {
    display: flex;
    flex-direction: column;
    gap: 6px;
  }
  .location-label {
    color: #7f849c;
    font-size: 12px;
    font-weight: 600;
  }
  .location-native {
    display: flex;
    gap: 8px;
  }
  .location-input {
    flex: 1;
    padding: 9px 13px;
    border-radius: 10px;
    border: 1px solid #313244;
    background: #181825;
    color: #cdd6f4;
    font-size: 13px;
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
    outline: none;
  }
  .location-input:focus {
    border-color: #89b4fa;
    box-shadow: 0 0 0 2px rgba(137, 180, 250, 0.16);
  }
  .browse-btn {
    border: 1px solid #313244;
    background: #313244;
    color: #cdd6f4;
    border-radius: 10px;
    padding: 9px 14px;
    font: inherit;
    font-size: 13px;
    cursor: pointer;
    white-space: nowrap;
  }
  .browse-btn:hover {
    background: #45475a;
  }
  .browse-btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
  .location-static {
    color: #585b70;
    font-size: 13px;
    font-style: italic;
  }
  .preview {
    color: #7f849c;
    font-size: 12px;
  }
  code {
    color: #89b4fa;
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
  }
  .actions {
    display: flex;
    justify-content: flex-end;
    gap: 10px;
  }
  .actions button {
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
  .primary:disabled,
  .secondary:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
  .error {
    color: #f38ba8;
    font-size: 12px;
    line-height: 1.5;
  }
</style>
