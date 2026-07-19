<script lang="ts">
  import { readable, type Readable } from 'svelte/store';
  import type { GitHubAccountState } from '../lib/project_catalog/types';
  import { ide, type AppMode } from '../stores/ide';

  export let githubStore: Readable<GitHubAccountState> = readable({
    token: null,
    user: null,
    connecting: false,
    error: '',
  });
  export let onConnectGitHub: () => void = () => {};
  export let onDisconnectGitHub: () => void = () => {};
  export let onNavigate: (mode: AppMode) => void = () => {};

  const modes: { id: AppMode; label: string; title: string }[] = [
    { id: 'manage', label: 'Home', title: 'Home and projects' },
    { id: 'develop', label: 'Code', title: 'Open the editor' },
    { id: 'docs', label: 'Docs', title: 'Read the documentation' },
  ];

  $: expanded = $ide.outputExpanded;
  let accountPopoverOpen = false;

  function setMode(mode: AppMode): void {
    accountPopoverOpen = false;
    onNavigate(mode);
    ide.update((state) => ({ ...state, appMode: mode }));
  }

  function toggleAccountPopover(): void {
    if (!$githubStore.connecting) accountPopoverOpen = !accountPopoverOpen;
  }

  function handleConnectGitHub(): void {
    accountPopoverOpen = false;
    onConnectGitHub();
  }

  function handleDisconnectGitHub(): void {
    accountPopoverOpen = false;
    onDisconnectGitHub();
  }
</script>

{#if !expanded}
  {#if accountPopoverOpen}
    <button
      class="account-backdrop"
      aria-label="Close GitHub account menu"
      on:click={() => (accountPopoverOpen = false)}
    ></button>
  {/if}

  <nav class="rail" aria-label="Primary navigation">
    <button class="brand" aria-label="Vo home" on:click={() => setMode('manage')}>
      <span>V</span><i>o</i>
    </button>

    <div class="nav-items">
      {#each modes as mode}
        <button
          class="nav-item"
          class:active={$ide.appMode === mode.id}
          aria-current={$ide.appMode === mode.id ? 'page' : undefined}
          title={mode.title}
          on:click={() => setMode(mode.id)}
        >
          <span class="nav-icon" aria-hidden="true">
            {#if mode.id === 'manage'}
              <svg viewBox="0 0 24 24"><path d="m4 10 8-6 8 6v9a1 1 0 0 1-1 1h-5v-6h-4v6H5a1 1 0 0 1-1-1v-9Z"/></svg>
            {:else if mode.id === 'develop'}
              <svg viewBox="0 0 24 24"><path d="m8.5 7-5 5 5 5M15.5 7l5 5-5 5M14 4l-4 16"/></svg>
            {:else}
              <svg viewBox="0 0 24 24"><path d="M5 4h11a3 3 0 0 1 3 3v13H8a3 3 0 0 1-3-3V4Zm0 13a3 3 0 0 1 3-3h11M9 8h6"/></svg>
            {/if}
          </span>
          <span class="nav-label">{mode.label}</span>
        </button>
      {/each}
    </div>

    <div class="account-slot">
      <button
        class="account-trigger"
        class:active={accountPopoverOpen}
        aria-expanded={accountPopoverOpen}
        aria-label={$githubStore.user ? `GitHub account @${$githubStore.user.login}` : 'Connect GitHub account'}
        title={$githubStore.user ? `GitHub @${$githubStore.user.login}` : 'Connect GitHub'}
        disabled={$githubStore.connecting}
        on:click={toggleAccountPopover}
      >
        {#if $githubStore.user}
          <img src={$githubStore.user.avatarUrl} alt="" />
        {:else if $githubStore.connecting}
          <span class="account-loader" aria-hidden="true"></span>
        {:else}
          <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M12 2a10 10 0 0 0-3.16 19.49c.5.09.68-.22.68-.48v-1.87c-2.78.6-3.37-1.18-3.37-1.18-.45-1.16-1.11-1.47-1.11-1.47-.91-.62.07-.61.07-.61 1 .07 1.53 1.03 1.53 1.03.9 1.53 2.35 1.09 2.92.83.09-.65.35-1.09.64-1.34-2.22-.25-4.56-1.11-4.56-4.94 0-1.09.39-1.98 1.03-2.68-.1-.25-.45-1.27.1-2.64 0 0 .84-.27 2.75 1.02A9.6 9.6 0 0 1 12 6.82c.85 0 1.7.11 2.5.34 1.91-1.29 2.75-1.02 2.75-1.02.55 1.37.2 2.39.1 2.64.64.7 1.03 1.59 1.03 2.68 0 3.84-2.34 4.68-4.57 4.93.36.31.68.92.68 1.86V21c0 .27.18.58.69.48A10 10 0 0 0 12 2Z"/></svg>
        {/if}
        <span class="nav-label">GitHub</span>
      </button>

      {#if accountPopoverOpen}
        <div class="account-popover" role="dialog" aria-label="GitHub account">
          <div class="popover-kicker">Project sync</div>
          {#if $githubStore.user}
            <div class="account-summary">
              <img src={$githubStore.user.avatarUrl} alt="" />
              <div><strong>@{$githubStore.user.login}</strong><span>Connected</span></div>
            </div>
            <button class="popover-action danger" on:click={handleDisconnectGitHub}>Disconnect</button>
          {:else}
            <p>Connect GitHub when you want to sync projects across devices.</p>
            <button class="popover-action" on:click={handleConnectGitHub}>Connect GitHub</button>
          {/if}
          {#if $githubStore.error}
            <p class="popover-error" role="alert">{$githubStore.error}</p>
          {/if}
        </div>
      {/if}
    </div>
  </nav>
{/if}

<style>
  .account-backdrop {
    position: fixed;
    inset: 0;
    z-index: 80;
    border: 0;
    background: transparent;
  }

  .rail {
    position: relative;
    z-index: 90;
    width: 76px;
    flex: 0 0 76px;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 24px;
    padding: 18px 10px 14px;
    border-right: 1px solid var(--line-subtle);
    background: color-mix(in srgb, var(--surface-0) 92%, transparent);
    backdrop-filter: blur(18px);
  }

  .brand {
    width: 48px;
    height: 42px;
    display: inline-flex;
    align-items: baseline;
    justify-content: center;
    border: 0;
    border-radius: 13px;
    color: var(--text-strong);
    background: linear-gradient(145deg, var(--accent), var(--accent-2));
    box-shadow: 0 10px 30px color-mix(in srgb, var(--accent) 24%, transparent);
    cursor: pointer;
    font-size: 20px;
    font-weight: 850;
    letter-spacing: -0.08em;
  }

  .brand i {
    margin-left: -4px;
    font-size: 15px;
    font-style: normal;
    font-weight: 650;
    opacity: 0.82;
  }

  .nav-items {
    width: 100%;
    display: flex;
    flex-direction: column;
    gap: 7px;
  }

  .nav-item,
  .account-trigger {
    width: 100%;
    min-height: 52px;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 4px;
    border: 1px solid transparent;
    border-radius: 13px;
    color: var(--text-muted);
    background: transparent;
    cursor: pointer;
    transition: color 160ms ease, border-color 160ms ease, background 160ms ease, transform 160ms ease;
  }

  .nav-item:hover,
  .account-trigger:hover:not(:disabled) {
    color: var(--text);
    background: var(--surface-hover);
    transform: translateY(-1px);
  }

  .nav-item.active {
    color: var(--accent-soft);
    border-color: color-mix(in srgb, var(--accent) 22%, transparent);
    background: color-mix(in srgb, var(--accent) 10%, transparent);
  }

  .nav-icon,
  .account-trigger > svg,
  .account-trigger > img,
  .account-loader {
    width: 22px;
    height: 22px;
    flex: 0 0 auto;
  }

  svg {
    width: 100%;
    height: 100%;
    fill: none;
    stroke: currentColor;
    stroke-width: 1.7;
    stroke-linecap: round;
    stroke-linejoin: round;
  }

  .account-trigger > svg { fill: currentColor; stroke: none; }
  .account-trigger > img { border-radius: 50%; object-fit: cover; }

  .nav-label {
    font-size: 10px;
    font-weight: 650;
    letter-spacing: 0.02em;
  }

  .account-slot {
    width: 100%;
    margin-top: auto;
    position: relative;
  }

  .account-trigger:disabled { cursor: progress; opacity: 0.65; }
  .account-trigger.active { color: var(--text); background: var(--surface-raised); }

  .account-loader {
    border: 2px solid var(--line);
    border-top-color: var(--accent);
    border-radius: 50%;
    animation: spin 700ms linear infinite;
  }

  .account-popover {
    position: absolute;
    left: calc(100% + 14px);
    bottom: 0;
    width: 248px;
    display: grid;
    gap: 12px;
    padding: 16px;
    border: 1px solid var(--line);
    border-radius: 16px;
    color: var(--text);
    background: var(--surface-raised);
    box-shadow: var(--shadow-xl);
  }

  .popover-kicker {
    color: var(--text-muted);
    font-size: 10px;
    font-weight: 800;
    letter-spacing: 0.13em;
    text-transform: uppercase;
  }

  .account-popover p {
    margin: 0;
    color: var(--text-muted);
    font-size: 12px;
    line-height: 1.55;
  }

  .account-summary { display: flex; align-items: center; gap: 10px; }
  .account-summary img { width: 34px; height: 34px; border-radius: 50%; }
  .account-summary div { display: grid; gap: 2px; }
  .account-summary strong { font-size: 12px; }
  .account-summary span { color: var(--success); font-size: 10px; }

  .popover-action {
    min-height: 36px;
    border: 1px solid color-mix(in srgb, var(--accent) 32%, transparent);
    border-radius: 10px;
    color: var(--accent-soft);
    background: color-mix(in srgb, var(--accent) 12%, transparent);
    cursor: pointer;
    font-weight: 700;
  }

  .popover-action.danger { color: var(--danger); border-color: color-mix(in srgb, var(--danger) 30%, transparent); background: transparent; }
  .account-popover .popover-error { color: var(--danger); }

  @keyframes spin { to { transform: rotate(360deg); } }

  @media (max-width: 720px) {
    .rail {
      position: fixed;
      inset: auto 0 0 0;
      width: 100%;
      height: calc(66px + env(safe-area-inset-bottom));
      padding: 6px 12px env(safe-area-inset-bottom);
      flex-direction: row;
      gap: 6px;
      border-top: 1px solid var(--line-subtle);
      border-right: 0;
    }

    .brand { display: none; }
    .nav-items { flex: 1; flex-direction: row; gap: 4px; }
    .nav-item, .account-trigger { min-height: 52px; }
    .account-slot { width: min(76px, 22vw); margin-top: 0; }
    .account-popover { left: auto; right: 0; bottom: calc(100% + 12px); width: min(280px, calc(100vw - 24px)); }
  }

  @media (prefers-reduced-motion: reduce) {
    .nav-item, .account-trigger { transition: none; }
    .account-loader { animation-duration: 1.4s; }
  }
</style>
