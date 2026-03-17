<script lang="ts">
  import { createEventDispatcher } from 'svelte';

  export let connecting = false;
  export let error = '';

  const dispatch = createEventDispatcher<{
    close: void;
    submit: { token: string };
  }>();

  let token = '';

  function submit(): void {
    dispatch('submit', { token });
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
  <div class="modal" role="dialog" aria-modal="true" aria-label="Connect GitHub">
    <div class="header">
      <div>
        <h3>Connect GitHub</h3>
        <p>Use a Personal Access Token to enable remote project storage and sync.</p>
      </div>
      <button class="close" on:click={() => dispatch('close')} aria-label="Close">×</button>
    </div>

    <div class="steps">
      <div class="step">
        <span class="num">1</span>
        <div>
          <div class="label">Create a fine-grained token or classic PAT</div>
          <a href="https://github.com/settings/tokens" target="_blank" rel="noreferrer">Open GitHub token settings</a>
        </div>
      </div>
      <div class="step">
        <span class="num">2</span>
        <div>
          <div class="label">Grant repo + gist access</div>
          <div class="scopes">
            <span>repo</span>
            <span>gist</span>
          </div>
        </div>
      </div>
      <div class="step">
        <span class="num">3</span>
        <div>
          <div class="label">Paste the token below</div>
          <div class="hint">The token is stored locally in this browser/webview only.</div>
        </div>
      </div>
    </div>

    <input
      class="token-input"
      bind:value={token}
      type="password"
      autocomplete="off"
      autocorrect="off"
      autocapitalize="off"
      spellcheck="false"
      placeholder="ghp_..."
      on:keydown={(event) => event.key === 'Enter' && submit()}
    />

    {#if error}
      <div class="error">{error}</div>
    {/if}

    <div class="actions">
      <button class="secondary" on:click={() => dispatch('close')} disabled={connecting}>Cancel</button>
      <button class="primary" on:click={submit} disabled={connecting || !token.trim()}>
        {#if connecting}Connecting…{:else}Connect{/if}
      </button>
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
    padding: 24px;
    z-index: 200;
  }
  .modal {
    width: min(100%, 460px);
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 16px;
    box-shadow: 0 24px 64px rgba(0, 0, 0, 0.55);
    padding: 22px;
    display: flex;
    flex-direction: column;
    gap: 18px;
  }
  .header {
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
    gap: 16px;
  }
  h3 {
    margin: 0;
    color: #cdd6f4;
    font-size: 18px;
  }
  p {
    margin: 6px 0 0;
    color: #7f849c;
    font-size: 13px;
    line-height: 1.6;
  }
  .close {
    border: none;
    background: transparent;
    color: #7f849c;
    font-size: 22px;
    line-height: 1;
    cursor: pointer;
  }
  .steps {
    display: flex;
    flex-direction: column;
    gap: 14px;
  }
  .step {
    display: flex;
    gap: 12px;
  }
  .num {
    width: 24px;
    height: 24px;
    border-radius: 999px;
    background: rgba(137, 180, 250, 0.14);
    border: 1px solid rgba(137, 180, 250, 0.32);
    color: #89b4fa;
    font-size: 12px;
    font-weight: 700;
    display: flex;
    align-items: center;
    justify-content: center;
    flex-shrink: 0;
  }
  .label {
    color: #cdd6f4;
    font-size: 13px;
    font-weight: 600;
  }
  .hint,
  a {
    color: #89b4fa;
    font-size: 12px;
    text-decoration: none;
  }
  a:hover {
    text-decoration: underline;
  }
  .scopes {
    display: flex;
    gap: 6px;
    margin-top: 6px;
  }
  .scopes span {
    padding: 2px 8px;
    border-radius: 999px;
    background: rgba(137, 180, 250, 0.12);
    border: 1px solid rgba(137, 180, 250, 0.28);
    color: #89b4fa;
    font-size: 11px;
    font-weight: 700;
  }
  .token-input {
    width: 100%;
    box-sizing: border-box;
    padding: 12px 14px;
    border-radius: 10px;
    border: 1px solid #313244;
    background: #181825;
    color: #cdd6f4;
    font-size: 13px;
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
    outline: none;
  }
  .token-input:focus {
    border-color: #89b4fa;
    box-shadow: 0 0 0 2px rgba(137, 180, 250, 0.16);
  }
  .error {
    color: #f38ba8;
    font-size: 12px;
    line-height: 1.5;
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
    background: linear-gradient(135deg, #89b4fa, #74c7ec);
    color: #11111b;
    font-weight: 700;
  }
  button:disabled {
    opacity: 0.6;
    cursor: not-allowed;
  }
</style>
