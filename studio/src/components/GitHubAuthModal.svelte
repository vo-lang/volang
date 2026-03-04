<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import { github, loginWithToken, logout } from '../stores/github';

  const dispatch = createEventDispatcher<{ close: void }>();

  let tokenInput = '';
  let isConnecting = false;
  let connectError = '';

  async function connect() {
    const t = tokenInput.trim();
    if (!t) return;
    isConnecting = true; connectError = '';
    try {
      await loginWithToken(t);
      tokenInput = '';
      dispatch('close');
    } catch (e: any) {
      connectError = String(e.message ?? e);
    } finally { isConnecting = false; }
  }

  function handleLogout() {
    logout();
  }
</script>

<!-- Brand bar user section (slot-like, rendered inline by parent) -->
<slot name="trigger" {handleLogout} />

<!-- ── GitHub auth modal ── -->
<!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
<div class="auth-backdrop" on:click={() => dispatch('close')}>
  <div class="auth-modal" on:click|stopPropagation={() => {}} role="dialog" aria-modal="true" aria-labelledby="auth-title">
    <div class="auth-logo-wrap">
      <svg class="auth-gh-icon" viewBox="0 0 24 24" width="36" height="36" fill="currentColor"><path d="M12 0C5.37 0 0 5.37 0 12c0 5.31 3.435 9.795 8.205 11.385.6.105.825-.255.825-.57 0-.285-.015-1.23-.015-2.235-3.015.555-3.795-.735-4.035-1.41-.135-.345-.72-1.41-1.23-1.695-.42-.225-1.02-.78-.015-.795.945-.015 1.62.87 1.845 1.23 1.08 1.815 2.805 1.305 3.495.99.105-.78.42-1.305.765-1.605-2.67-.3-5.46-1.335-5.46-5.925 0-1.305.465-2.385 1.23-3.225-.12-.3-.54-1.53.12-3.18 0 0 1.005-.315 3.3 1.23.96-.27 1.98-.405 3-.405s2.04.135 3 .405c2.295-1.56 3.3-1.23 3.3-1.23.66 1.65.24 2.88.12 3.18.765.84 1.23 1.905 1.23 3.225 0 4.605-2.805 5.625-5.475 5.925.435.375.81 1.095.81 2.22 0 1.605-.015 2.895-.015 3.3 0 .315.225.69.825.57A12.02 12.02 0 0024 12c0-6.63-5.37-12-12-12z"/></svg>
    </div>
    <h2 class="auth-title" id="auth-title">Connect to GitHub</h2>
    <p class="auth-desc">Link your account to push and pull projects between local and GitHub.</p>

    <div class="auth-steps">
      <div class="auth-step">
        <span class="step-num">1</span>
        <div class="step-body">
          <span class="step-label">Create a Personal Access Token</span>
          <a class="step-link" href="https://github.com/settings/tokens/new?description=Vibe+Studio&scopes=gist,public_repo" target="_blank" rel="noopener noreferrer">Open GitHub Settings ↗</a>
        </div>
      </div>
      <div class="auth-step">
        <span class="step-num">2</span>
        <div class="step-body">
          <span class="step-label">Enable these scopes</span>
          <div class="scope-chips">
            <code class="scope">gist</code>
            <code class="scope">public_repo</code>
            <span class="scope-note">(or <code class="scope">repo</code> for private)</span>
          </div>
        </div>
      </div>
      <div class="auth-step">
        <span class="step-num">3</span>
        <div class="step-body"><span class="step-label">Paste it below</span></div>
      </div>
    </div>

    <div class="auth-input-wrap">
      <input
        class="auth-token-input"
        type="password"
        placeholder="ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
        bind:value={tokenInput}
        on:keydown={(e) => e.key === 'Enter' && connect()}
        autocomplete="off"
        spellcheck="false"
      />
    </div>

    {#if connectError}
      <div class="auth-error">{connectError}</div>
    {/if}

    <div class="auth-btns">
      <button class="auth-connect-btn" disabled={!tokenInput.trim() || isConnecting} on:click={connect}>
        {#if isConnecting}
          <svg class="auth-spin" viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2.5"><path d="M12 2v4M12 18v4M4.93 4.93l2.83 2.83M16.24 16.24l2.83 2.83M2 12h4M18 12h4M4.93 19.07l2.83-2.83M16.24 7.76l2.83-2.83"/></svg>
          Connecting…
        {:else}
          Connect
        {/if}
      </button>
      <button class="auth-cancel-btn" on:click={() => dispatch('close')}>Cancel</button>
    </div>
  </div>
</div>

<style>
  .auth-backdrop {
    position: fixed; inset: 0; z-index: 100;
    background: rgba(0, 0, 0, 0.65);
    backdrop-filter: blur(4px);
    display: flex; align-items: center; justify-content: center;
    padding: 24px;
    animation: fade-in 0.15s ease;
  }
  @keyframes fade-in { from { opacity: 0; } to { opacity: 1; } }

  .auth-modal {
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 14px;
    box-shadow: 0 24px 64px rgba(0, 0, 0, 0.6);
    width: 100%; max-width: 400px;
    padding: 32px 28px 24px;
    display: flex; flex-direction: column; align-items: stretch; gap: 0;
    animation: slide-up 0.18s ease;
  }
  @keyframes slide-up { from { transform: translateY(12px); opacity: 0; } to { transform: none; opacity: 1; } }

  .auth-logo-wrap { display: flex; justify-content: center; margin-bottom: 16px; }
  .auth-gh-icon { color: #585b70; filter: drop-shadow(0 0 16px rgba(137,180,250,0.2)); }
  .auth-title { font-size: 17px; font-weight: 700; color: #cdd6f4; text-align: center; margin: 0 0 8px; }
  .auth-desc { font-size: 13px; color: #585b70; text-align: center; line-height: 1.6; margin: 0 0 24px; }

  .auth-steps { display: flex; flex-direction: column; gap: 16px; margin-bottom: 24px; }
  .auth-step { display: flex; align-items: flex-start; gap: 12px; }
  .step-num {
    width: 22px; height: 22px; border-radius: 50%;
    background: rgba(137,180,250,0.12); border: 1px solid rgba(137,180,250,0.3);
    color: #89b4fa; font-size: 11px; font-weight: 700;
    display: flex; align-items: center; justify-content: center; flex-shrink: 0;
  }
  .step-body { display: flex; flex-direction: column; gap: 6px; }
  .step-label { font-size: 13px; color: #a6adc8; font-weight: 500; }
  .step-link { font-size: 12px; color: #89b4fa; text-decoration: none; }
  .step-link:hover { text-decoration: underline; }
  .scope-chips { display: flex; flex-wrap: wrap; align-items: center; gap: 6px; }
  .scope { background: rgba(137,180,250,0.1); border: 1px solid rgba(137,180,250,0.25); color: #89b4fa; padding: 2px 7px; border-radius: 4px; font-size: 11px; }
  .scope-note { font-size: 11px; color: #585b70; }

  .auth-input-wrap { margin-bottom: 12px; }
  .auth-token-input {
    width: 100%; background: #181825; border: 1px solid #313244;
    color: #cdd6f4; font-size: 13px; padding: 10px 12px;
    border-radius: 8px; outline: none; font-family: monospace; box-sizing: border-box;
  }
  .auth-token-input:focus { border-color: #89b4fa; box-shadow: 0 0 0 2px rgba(137,180,250,0.15); }

  .auth-error { font-size: 12px; color: #f38ba8; margin-bottom: 12px; }

  .auth-btns { display: flex; gap: 8px; }
  .auth-connect-btn {
    flex: 1; background: linear-gradient(135deg, #89b4fa, #74c7ec);
    color: #1e1e2e; font-size: 13px; font-weight: 700;
    padding: 10px 16px; border: none; border-radius: 8px; cursor: pointer;
    display: flex; align-items: center; justify-content: center; gap: 6px;
    transition: opacity 0.15s; font-family: inherit;
  }
  .auth-connect-btn:hover:not(:disabled) { opacity: 0.85; }
  .auth-connect-btn:disabled { opacity: 0.5; cursor: not-allowed; }
  .auth-cancel-btn {
    background: #313244; color: #a6adc8; font-size: 13px;
    padding: 10px 16px; border: 1px solid #45475a; border-radius: 8px;
    cursor: pointer; font-family: inherit; transition: background 0.15s;
  }
  .auth-cancel-btn:hover { background: #45475a; }
  .auth-spin { animation: spin 1s linear infinite; }
  @keyframes spin { to { transform: rotate(360deg); } }
</style>
