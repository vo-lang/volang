<script lang="ts">
  import { ide, type AppMode } from '../stores/ide';
  import { readable, type Readable } from 'svelte/store';
  import type { GitHubAccountState } from '../lib/project_catalog/types';

  export let githubStore: Readable<GitHubAccountState> = readable({
    token: null,
    user: null,
    connecting: false,
    error: '',
  });
  export let onConnectGitHub: () => void = () => {};
  export let onDisconnectGitHub: () => void = () => {};

  const modes: { id: AppMode; icon: string; label: string; title: string }[] = [
    { id: 'manage',   icon: '⌂',  label: 'Home',  title: 'Home'           },
    { id: 'develop',  icon: '⌨',  label: 'Dev',   title: 'Development'    },
    { id: 'docs',     icon: '☰',  label: 'Docs',  title: 'Documentation'  },
    { id: 'term',     icon: '$_', label: 'TERM',  title: 'TERM'           },
  ];

  $: expanded = $ide.outputExpanded;
  let accountPopoverOpen = false;

  function setMode(mode: AppMode) {
    ide.update(s => ({ ...s, appMode: mode }));
  }

  function toggleAccountPopover(): void {
    if ($githubStore.connecting) {
      return;
    }
    accountPopoverOpen = !accountPopoverOpen;
  }

  function closeAccountPopover(): void {
    accountPopoverOpen = false;
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
    on:click={closeAccountPopover}
  ></button>
{/if}
<div class="sidebar">
  {#each modes as m}
    <button
      class="mode-btn"
      class:active={$ide.appMode === m.id}
      title={m.title}
      on:click={() => setMode(m.id)}
    >
      <span class="icon">{m.icon}</span>
      <span class="label">{m.label}</span>
    </button>
  {/each}
  <div class="spacer"></div>
  <div class="account-slot">
    <button
      class="account-trigger"
      class:active={accountPopoverOpen}
      title={$githubStore.user ? `GitHub @${$githubStore.user.login}` : 'GitHub account'}
      aria-label={$githubStore.user ? `GitHub @${$githubStore.user.login}` : 'GitHub account'}
      on:click={toggleAccountPopover}
      disabled={$githubStore.connecting}
    >
      {#if $githubStore.user}
        <img
          class="account-avatar"
          src={$githubStore.user.avatarUrl}
          alt={$githubStore.user.login}
        />
      {:else if $githubStore.connecting}
        <div class="account-placeholder" aria-hidden="true"></div>
      {:else}
        <span class="account-glyph">GH</span>
      {/if}
    </button>
    {#if accountPopoverOpen}
      <div class="account-popover" role="dialog" aria-label="GitHub account">
        <div class="account-popover-title">GitHub</div>
        {#if $githubStore.user}
          <div class="account-summary">
            <img
              class="account-summary-avatar"
              src={$githubStore.user.avatarUrl}
              alt={$githubStore.user.login}
            />
            <div class="account-summary-meta">
              <div class="account-summary-login">@{$githubStore.user.login}</div>
              <div class="account-summary-state">Connected</div>
            </div>
          </div>
          <button class="account-popover-action account-popover-action-danger" on:click={handleDisconnectGitHub}>
            Sign out
          </button>
        {:else}
          <div class="account-summary-copy">
            Connect GitHub to sync and store remote projects.
          </div>
          <button class="account-popover-action account-popover-action-primary" on:click={handleConnectGitHub}>
            Connect GitHub
          </button>
        {/if}
        {#if $githubStore.error}
          <div class="account-popover-error">{$githubStore.error}</div>
        {/if}
      </div>
    {/if}
  </div>
</div>
{/if}

<style>
  .account-backdrop {
    position: fixed;
    inset: 0;
    border: none;
    background: transparent;
    padding: 0;
    margin: 0;
    z-index: 20;
    cursor: default;
  }
  .sidebar {
    width: 52px;
    background: #11111b;
    border-right: 1px solid #1e1e2e;
    display: flex;
    flex-direction: column;
    align-items: center;
    padding: 10px 0;
    gap: 2px;
    flex-shrink: 0;
    position: relative;
    z-index: 21;
  }
  .spacer { flex: 1; }
  .account-slot {
    width: 44px;
    margin-bottom: 6px;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 6px;
    flex-shrink: 0;
    position: relative;
    z-index: 22;
  }
  .account-trigger {
    width: 44px;
    height: 44px;
    border: none;
    background: transparent;
    color: #7f849c;
    cursor: pointer;
    border-radius: 10px;
    display: flex;
    align-items: center;
    justify-content: center;
    transition: background 0.1s, color 0.1s;
    font-family: inherit;
    padding: 0;
  }
  .account-trigger:hover:not(:disabled),
  .account-trigger.active {
    background: #1e1e2e;
    color: #cdd6f4;
  }
  .account-trigger:disabled {
    cursor: wait;
    opacity: 0.9;
  }
  .account-avatar,
  .account-placeholder {
    width: 28px;
    height: 28px;
    border-radius: 999px;
    flex-shrink: 0;
  }
  .account-avatar {
    border: 1px solid #313244;
    object-fit: cover;
  }
  .account-placeholder {
    border: 1px solid #313244;
    background: #181825;
  }
  .account-glyph {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 28px;
    height: 28px;
    border-radius: 999px;
    border: 1px solid currentColor;
    font-size: 9px;
    font-weight: 800;
    letter-spacing: 0.06em;
    line-height: 1;
  }
  .account-popover {
    position: absolute;
    left: calc(100% + 12px);
    bottom: 0;
    width: 220px;
    border: 1px solid #313244;
    border-radius: 14px;
    background: #181825;
    box-shadow: 0 24px 64px rgba(0, 0, 0, 0.45);
    padding: 14px;
    display: flex;
    flex-direction: column;
    gap: 12px;
  }
  .account-popover-title {
    color: #cdd6f4;
    font-size: 12px;
    font-weight: 700;
    letter-spacing: 0.04em;
    text-transform: uppercase;
  }
  .account-summary {
    display: flex;
    align-items: center;
    gap: 10px;
  }
  .account-summary-avatar {
    width: 32px;
    height: 32px;
    border-radius: 999px;
    border: 1px solid #313244;
    flex-shrink: 0;
  }
  .account-summary-meta {
    min-width: 0;
  }
  .account-summary-login {
    color: #cdd6f4;
    font-size: 13px;
    font-weight: 700;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .account-summary-state {
    color: #7f849c;
    font-size: 11px;
  }
  .account-summary-copy {
    color: #7f849c;
    font-size: 12px;
    line-height: 1.5;
  }
  .account-popover-action {
    width: 100%;
    border: 1px solid #313244;
    border-radius: 10px;
    background: #1e1e2e;
    color: #cdd6f4;
    font: inherit;
    font-size: 12px;
    font-weight: 700;
    padding: 8px 12px;
    cursor: pointer;
    transition: background 0.1s, border-color 0.1s, color 0.1s;
  }
  .account-popover-action:hover {
    background: #242438;
    border-color: #45475a;
  }
  .account-popover-action-primary {
    background: rgba(137, 180, 250, 0.12);
    color: #89b4fa;
  }
  .account-popover-action-primary:hover {
    background: rgba(137, 180, 250, 0.18);
    border-color: #89b4fa;
  }
  .account-popover-action-danger {
    color: #f38ba8;
  }
  .account-popover-action-danger:hover {
    border-color: rgba(243, 139, 168, 0.45);
    background: rgba(243, 139, 168, 0.08);
  }
  .account-popover-error {
    color: #f38ba8;
    font-size: 11px;
    line-height: 1.5;
  }
  .mode-btn {
    width: 44px;
    height: 52px;
    border: none;
    background: none;
    color: #45475a;
    cursor: pointer;
    border-radius: 8px;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 4px;
    transition: background 0.1s, color 0.1s;
    position: relative;
    font-family: inherit;
  }
  .mode-btn::before {
    content: '';
    position: absolute;
    left: 0;
    top: 12px;
    bottom: 12px;
    width: 2px;
    border-radius: 0 2px 2px 0;
    background: transparent;
    transition: background 0.15s;
  }
  .mode-btn:hover { background: #1e1e2e; color: #a6adc8; }
  .mode-btn.active { color: #cdd6f4; background: #1e1e2e; }
  .mode-btn.active::before { background: #89b4fa; }
  .icon { font-size: 18px; line-height: 1; }
  .label { font-size: 9px; font-weight: 700; letter-spacing: 0.05em; text-transform: uppercase; }
</style>
