<script lang="ts">
  import { onMount, tick } from 'svelte';
  import { get } from 'svelte/store';
  import { explorer } from '../stores/explorer';
  import { github, loginWithToken, logout, loadSavedToken } from '../stores/github';
  import { projects, loadProjects, syncState } from '../stores/projects';
  import type { ProjectEntry, SyncState } from '../stores/projects';
  import { actions } from '../lib/actions';
  import { bridge } from '../lib/bridge';

  // ── Auth ───────────────────────────────────────────────────────────────────
  let tokenInput = '';
  let isConnecting = false;
  let connectError = '';
  let showAuthModal = false;

  onMount(() => {
    loadSavedToken();
    // Load projects once bridge is ready
    const root = get(explorer).localRoot;
    if (root) loadProjects(root);
  });

  // Reload projects when GitHub auth state changes
  let prevToken: string | null = null;
  $: {
    const tok = $github.token;
    if (tok !== prevToken) {
      prevToken = tok;
      const root = $explorer.localRoot;
      if (root) loadProjects(root);
    }
  }

  async function connect() {
    const t = tokenInput.trim();
    if (!t) return;
    isConnecting = true; connectError = '';
    try {
      await loginWithToken(t);
      tokenInput = ''; showAuthModal = false;
    } catch (e: any) {
      connectError = String(e.message ?? e);
    } finally { isConnecting = false; }
  }

  function handleLogout() {
    logout();
  }

  // ── Project actions ────────────────────────────────────────────────────────
  let busyProject = '';
  let actionError = '';

  function projectKey(p: ProjectEntry): string {
    return p.name + ':' + p.type;
  }

  async function openProject(p: ProjectEntry) {
    busyProject = projectKey(p); actionError = '';
    try {
      await actions.openProjectEntry(p, $explorer.localRoot);
    } catch (e: any) {
      actionError = `Open failed: ${e.message ?? e}`;
    } finally { busyProject = ''; }
  }

  async function pushProject(p: ProjectEntry) {
    busyProject = projectKey(p); actionError = '';
    try {
      await actions.pushProject(p);
    } catch (e: any) {
      actionError = `Push failed: ${e.message ?? e}`;
    } finally { busyProject = ''; }
  }

  async function pullProject(p: ProjectEntry) {
    busyProject = projectKey(p); actionError = '';
    try {
      await actions.pullProject(p, $explorer.localRoot);
    } catch (e: any) {
      actionError = `Pull failed: ${e.message ?? e}`;
    } finally { busyProject = ''; }
  }

  async function deleteProject(p: ProjectEntry) {
    busyProject = projectKey(p); actionError = '';
    try {
      await actions.deleteProject(p, $explorer.localRoot);
    } catch (e: any) {
      actionError = `Delete failed: ${e.message ?? e}`;
    } finally { busyProject = ''; }
  }

  async function createNewFile() {
    const b = bridge();
    const root = $explorer.localRoot;
    // Find a unique name
    let idx = 1;
    let name = 'main.vo';
    const existing = $projects.projects.map(p => p.name);
    while (existing.includes(name.replace(/\.vo$/, ''))) {
      idx++;
      name = `main${idx}.vo`;
    }
    await b.fsWriteFile(root + '/' + name, `package main

import "fmt"

func main() {
\tfmt.Println("Hello, Vo!")
}
`);
    await loadProjects(root);
    // Open the new file
    const ps = get(projects);
    const created = ps.projects.find(p => p.name === name.replace(/\.vo$/, '') && p.type === 'single');
    if (created) await openProject(created);
  }

  async function createNewProject() {
    const b = bridge();
    const root = $explorer.localRoot;
    let idx = 1;
    let dirName = 'project';
    const existing = $projects.projects.map(p => p.name);
    while (existing.includes(dirName)) {
      idx++;
      dirName = `project${idx}`;
    }
    const dir = root + '/' + dirName;
    await b.fsMkdir(dir);
    await b.fsWriteFile(dir + '/vo.mod', `module main\n\nvo 0.1\n`);
    await b.fsWriteFile(dir + '/main.vo', `package main

import "fmt"

func main() {
\tfmt.Println("Hello, Vo!")
}
`);
    await loadProjects(root);
    const ps = get(projects);
    const created = ps.projects.find(p => p.name === dirName && p.type === 'multi');
    if (created) await openProject(created);
  }

  async function refresh() {
    const root = $explorer.localRoot;
    if (root) await loadProjects(root);
  }

  // ── Helpers ────────────────────────────────────────────────────────────────
  function stateLabel(state: SyncState): string {
    if (state === 'local-only') return 'Local';
    if (state === 'remote-only') return 'Cloud';
    return 'Synced';
  }

  function formatPushedAt(value: string | null): string {
    if (!value) return '';
    const d = new Date(value);
    if (Number.isNaN(d.getTime())) return '';
    return d.toLocaleString(undefined, {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      hour12: false,
      timeZoneName: 'short',
    });
  }

  // ── Filtered / sorted list ─────────────────────────────────────────────────
  let searchQuery = '';
  $: filteredProjects = $projects.projects.filter(p =>
    !searchQuery || p.name.toLowerCase().includes(searchQuery.toLowerCase())
  );

  // ── Context menu ──────────────────────────────────────────────────────────
  let ctxMenu: { x: number; y: number; project: ProjectEntry; state: SyncState } | null = null;
  let renameModal: { project: ProjectEntry; value: string; saving: boolean; error: string } | null = null;
  let renameInputEl: HTMLInputElement | null = null;

  function onCardContext(e: MouseEvent, p: ProjectEntry, state: SyncState) {
    const menuW = 220, menuH = 290;
    const x = e.clientX + menuW > window.innerWidth ? e.clientX - menuW : e.clientX;
    const y = e.clientY + menuH > window.innerHeight ? e.clientY - menuH : e.clientY;
    ctxMenu = { x: Math.max(0, x), y: Math.max(0, y), project: p, state };
  }

  async function openRenameModal(p: ProjectEntry) {
    ctxMenu = null;
    renameModal = {
      project: p,
      value: p.name,
      saving: false,
      error: '',
    };
    await tick();
    renameInputEl?.focus();
    renameInputEl?.select();
  }

  async function confirmRename() {
    const m = renameModal;
    if (!m) return;
    const root = $explorer.localRoot;
    if (!root) {
      renameModal = { ...m, error: 'Workspace is not ready.' };
      return;
    }
    renameModal = { ...m, saving: true, error: '' };
    try {
      await actions.renameProject(m.project, m.value, root);
      renameModal = null;
    } catch (e: any) {
      renameModal = {
        ...m,
        saving: false,
        error: `Rename failed: ${e.message ?? e}`,
      };
    }
  }

  function viewOnGitHub(p: ProjectEntry) {
    if (!p.remote) return;
    if (p.remote.kind === 'gist') {
      window.open(`https://gist.github.com/${p.remote.gistId}`, '_blank', 'noopener,noreferrer');
    } else {
      window.open(`https://github.com/${p.remote.owner}/${p.remote.repo}`, '_blank', 'noopener,noreferrer');
    }
  }

  function handleKeydown(e: KeyboardEvent) {
    if (e.key === 'Escape') {
      if (ctxMenu) { ctxMenu = null; }
      else if (renameModal) { renameModal = null; }
      else if (showAuthModal) { showAuthModal = false; connectError = ''; }
    }
  }
</script>

<svelte:window on:keydown={handleKeydown} />

<div class="home">
  <!-- ── Brand header ── -->
  <div class="brand-bar">
    <svg class="brand-logo" viewBox="0 0 24 24" width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polygon points="12 2 2 7 12 12 22 7 12 2"/><polyline points="2 17 12 22 22 17"/><polyline points="2 12 12 17 22 12"/></svg>
    <span class="brand-name">Vibe Studio</span>
    <span class="spacer"></span>
    {#if $github.user}
      <img class="avatar" src={$github.user.avatar_url} alt={$github.user.login} width="24" height="24" />
      <span class="gh-login">@{$github.user.login}</span>
      <button class="link-btn" on:click={handleLogout}>Sign out</button>
    {:else if $github.isLoading}
      <span class="gh-hint">Connecting…</span>
      {:else}
      <button class="connect-btn" on:click={() => (showAuthModal = true)}>
        <svg viewBox="0 0 16 16" width="14" height="14" fill="currentColor"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"/></svg>
        Connect GitHub
      </button>
    {/if}
  </div>
  <!-- ── GitHub auth modal ── -->
  {#if showAuthModal}
    <!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
    <div class="auth-backdrop" on:click={() => { showAuthModal = false; connectError = ''; }}>
      <div class="auth-modal" on:click|stopPropagation={() => {}} role="dialog" aria-modal="true" aria-labelledby="auth-title">
        <!-- GitHub logo -->
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
              <a
                class="step-link"
                href="https://github.com/settings/tokens/new?description=Vibe+Studio&scopes=gist,public_repo"
                target="_blank"
                rel="noopener noreferrer"
              >Open GitHub Settings ↗</a>
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
            <div class="step-body">
              <span class="step-label">Paste it below</span>
            </div>
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
          <button
            class="auth-connect-btn"
            disabled={!tokenInput.trim() || isConnecting}
            on:click={connect}
          >
            {#if isConnecting}
              <svg class="auth-spin" viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2.5"><path d="M12 2v4M12 18v4M4.93 4.93l2.83 2.83M16.24 16.24l2.83 2.83M2 12h4M18 12h4M4.93 19.07l2.83-2.83M16.24 7.76l2.83-2.83"/></svg>
              Connecting…
            {:else}
              Connect
            {/if}
          </button>
          <button class="auth-cancel-btn" on:click={() => { showAuthModal = false; connectError = ''; }}>
            Cancel
          </button>
        </div>
      </div>
    </div>
  {/if}

  <!-- ── Toolbar ── -->
  <div class="toolbar-bar">
    <div class="search-wrap">
      <svg class="search-icon" viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="11" cy="11" r="8"/><line x1="21" y1="21" x2="16.65" y2="16.65"/></svg>
      <input class="search-input" type="text" placeholder="Search…" bind:value={searchQuery} />
    </div>
    <button class="new-btn file" on:click={createNewFile} title="New single file">
      <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8z"/><polyline points="14 2 14 8 20 8"/><line x1="12" y1="18" x2="12" y2="12"/><line x1="9" y1="15" x2="15" y2="15"/></svg>
      File
    </button>
    <button class="new-btn proj" on:click={createNewProject} title="New multi-file project">
      <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M22 19a2 2 0 01-2 2H4a2 2 0 01-2-2V5a2 2 0 012-2h5l2 3h9a2 2 0 012 2z"/><line x1="12" y1="11" x2="12" y2="17"/><line x1="9" y1="14" x2="15" y2="14"/></svg>
      Project
    </button>
    <button class="icon-btn" title="Refresh" on:click={refresh}>
      <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="23 4 23 10 17 10"/><polyline points="1 20 1 14 7 14"/><path d="M3.51 9a9 9 0 0114.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0020.49 15"/></svg>
    </button>
  </div>

  {#if actionError}
    <div class="error-bar">{actionError}</div>
  {/if}

  <!-- ── Project list ── -->
  <div class="project-list">
    {#if $projects.isLoading}
      <div class="empty-state">
        <div class="spinner"></div>
        <span>Loading projects…</span>
      </div>
    {:else if $projects.error}
      <div class="empty-state"><span class="err-msg">{$projects.error}</span></div>
    {:else if filteredProjects.length === 0}
      <div class="empty-state">
        <svg class="empty-icon" viewBox="0 0 24 24" width="40" height="40" fill="none" stroke="currentColor" stroke-width="1.2" stroke-linecap="round" stroke-linejoin="round"><path d="M22 19a2 2 0 01-2 2H4a2 2 0 01-2-2V5a2 2 0 012-2h5l2 3h9a2 2 0 012 2z"/></svg>
        <span>{searchQuery ? 'No matching projects' : 'No projects yet'}</span>
        <span class="empty-sub">Create a file or project to get started</span>
      </div>
    {:else}
      <div class="card-grid">
        {#each filteredProjects as p (projectKey(p))}
          {@const state = syncState(p)}
          {@const busy = busyProject === projectKey(p)}
          <button class="card" class:busy on:click={() => openProject(p)} on:contextmenu|preventDefault={(e) => onCardContext(e, p, state)} disabled={busy}>
            <!-- Type icon -->
            <div class="card-icon" class:single={p.type === 'single'} class:multi={p.type === 'multi'}>
              {#if p.type === 'single'}
                <svg viewBox="0 0 24 24" width="22" height="22" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"><path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8z"/><polyline points="14 2 14 8 20 8"/></svg>
              {:else}
                <svg viewBox="0 0 24 24" width="22" height="22" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"><path d="M22 19a2 2 0 01-2 2H4a2 2 0 01-2-2V5a2 2 0 012-2h5l2 3h9a2 2 0 012 2z"/></svg>
              {/if}
            </div>
            <!-- Info + always-visible sync status -->
            <div class="card-body">
              <span class="card-name">{p.name}</span>
              <div class="sync-row">
                <span class="loc-badge" class:present={!!p.localPath}>
                  <svg viewBox="0 0 24 24" width="11" height="11" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="2" y="3" width="20" height="14" rx="2"/><line x1="8" y1="21" x2="16" y2="21"/><line x1="12" y1="17" x2="12" y2="21"/></svg>
                  Local
                </span>
                <span class="sync-conn" class:local-only={state === 'local-only'} class:remote-only={state === 'remote-only'} class:synced={state === 'synced'}>
                  {#if state === 'synced'}✓{:else if state === 'local-only'}→{:else}←{/if}
                </span>
                <span class="loc-badge github" class:present={!!p.remote}>
                  <svg viewBox="0 0 24 24" width="11" height="11" fill="currentColor"><path d="M12 0C5.37 0 0 5.37 0 12c0 5.31 3.435 9.795 8.205 11.385.6.105.825-.255.825-.57 0-.285-.015-1.23-.015-2.235-3.015.555-3.795-.735-4.035-1.41-.135-.345-.72-1.41-1.23-1.695-.42-.225-1.02-.78-.015-.795.945-.015 1.62.87 1.845 1.23 1.08 1.815 2.805 1.305 3.495.99.105-.78.42-1.305.765-1.605-2.67-.3-5.46-1.335-5.46-5.925 0-1.305.465-2.385 1.23-3.225-.12-.3-.54-1.53.12-3.18 0 0 1.005-.315 3.3 1.23.96-.27 1.98-.405 3-.405s2.04.135 3 .405c2.295-1.56 3.3-1.23 3.3-1.23.66 1.65.24 2.88.12 3.18.765.84 1.23 1.905 1.23 3.225 0 4.605-2.805 5.625-5.475 5.925.435.375.81 1.095.81 2.22 0 1.605-.015 2.895-.015 3.3 0 .315.225.69.825.57A12.02 12.02 0 0024 12c0-6.63-5.37-12-12-12z"/></svg>
                  GitHub
                </span>
                {#if state === 'local-only' && $github.token}
                  <button class="sync-act push" title="Push to GitHub" disabled={busy} on:click|stopPropagation={() => pushProject(p)}>
                    <svg viewBox="0 0 24 24" width="10" height="10" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="19" x2="12" y2="5"/><polyline points="5 12 12 5 19 12"/></svg>
                    Push
                  </button>
                {:else if state === 'remote-only'}
                  <button class="sync-act pull" title="Pull to local" disabled={busy} on:click|stopPropagation={() => pullProject(p)}>
                    <svg viewBox="0 0 24 24" width="10" height="10" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="5" x2="12" y2="19"/><polyline points="19 12 12 19 5 12"/></svg>
                    Pull
                  </button>
                {:else if state === 'synced'}
                  <span class="sync-ts">{formatPushedAt(p.pushedAt)}</span>
                {/if}
              </div>
            </div>
            <!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
            <div role="presentation" class="del-wrap" on:click|stopPropagation={() => {}}>
              <button class="del-btn" title="Delete" disabled={busy} on:click={() => deleteProject(p)}>
                <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="3 6 5 6 21 6"/><path d="M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6m3 0V4a2 2 0 012-2h4a2 2 0 012 2v2"/></svg>
              </button>
            </div>
          </button>
        {/each}
      </div>
    {/if}
  </div>

  <!-- ── Context menu ── -->
  {#if ctxMenu}
    {@const p = ctxMenu.project}
    {@const s = ctxMenu.state}
    <!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
    <div class="ctx-backdrop" on:click={() => (ctxMenu = null)}>
      <div class="ctx-menu" style="left:{ctxMenu.x}px;top:{ctxMenu.y}px" on:click|stopPropagation={() => {}}>
        <button class="ctx-item" on:click={() => { ctxMenu = null; openProject(p); }}>
          <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M5 3l14 9-14 9V3z"/></svg>
          Open
        </button>
        <button class="ctx-item" on:click={() => openRenameModal(p)}>
          <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 20h9"/><path d="M16.5 3.5a2.121 2.121 0 113 3L7 19l-4 1 1-4 12.5-12.5z"/></svg>
          Rename
        </button>
        {#if (s === 'local-only' && $github.token) || s === 'synced'}
          <button class="ctx-item" on:click={() => { ctxMenu = null; pushProject(p); }}>
            <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="19" x2="12" y2="5"/><polyline points="5 12 12 5 19 12"/></svg>
            Push to GitHub
          </button>
        {/if}
        {#if s === 'remote-only' || s === 'synced'}
          <button class="ctx-item" on:click={() => { ctxMenu = null; pullProject(p); }}>
            <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="5" x2="12" y2="19"/><polyline points="19 12 12 19 5 12"/></svg>
            Pull from GitHub
          </button>
        {/if}
        {#if p.remote}
          <button class="ctx-item" on:click={() => { ctxMenu = null; viewOnGitHub(p); }}>
            <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M18 13v6a2 2 0 01-2 2H5a2 2 0 01-2-2V8a2 2 0 012-2h6"/><polyline points="15 3 21 3 21 9"/><line x1="10" y1="14" x2="21" y2="3"/></svg>
            View on GitHub
          </button>
        {/if}
        <div class="ctx-sep"></div>
        <button class="ctx-item danger" on:click={() => { ctxMenu = null; deleteProject(p); }}>
          <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="3 6 5 6 21 6"/><path d="M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6m3 0V4a2 2 0 012-2h4a2 2 0 012 2v2"/></svg>
          Delete
        </button>
      </div>
    </div>
  {/if}

  {#if renameModal}
    <!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
    <div class="auth-backdrop" on:click={() => !renameModal?.saving && (renameModal = null)}>
      <div class="rename-modal" role="dialog" aria-modal="true" aria-labelledby="rename-title" on:click|stopPropagation={() => {}}>
        <h3 class="rename-title" id="rename-title">Rename Project</h3>
        <p class="rename-desc">Rename local project and GitHub reference together.</p>

        <input
          class="rename-input"
          bind:this={renameInputEl}
          bind:value={renameModal.value}
          disabled={renameModal.saving}
          on:keydown={(e) => e.key === 'Enter' && !renameModal?.saving && confirmRename()}
        />

        {#if renameModal.error}
          <div class="rename-error">{renameModal.error}</div>
        {/if}

        <div class="rename-actions">
          <button
            class="rename-confirm"
            on:click={confirmRename}
            disabled={!renameModal.value.trim() || renameModal.saving}
          >
            {renameModal.saving ? 'Renaming…' : 'Rename'}
          </button>
          <button class="rename-cancel" on:click={() => (renameModal = null)} disabled={renameModal.saving}>Cancel</button>
        </div>
      </div>
    </div>
  {/if}
</div>

<style>
  .home {
    flex: 1; display: flex; flex-direction: column;
    overflow: hidden; min-width: 0;
    background: #11111b;
  }

  /* ── Brand bar ── */
  .brand-bar {
    display: flex; align-items: center; gap: 8px;
    padding: 10px 16px;
    background: linear-gradient(135deg, #181825 0%, #1e1e2e 100%);
    border-bottom: 1px solid #252535;
    flex-shrink: 0;
  }
  .brand-logo { color: #89b4fa; flex-shrink: 0; }
  .brand-name { font-size: 13px; font-weight: 700; color: #cdd6f4; letter-spacing: 0.04em; }
  .spacer { flex: 1; }

  .avatar { width: 24px; height: 24px; border-radius: 50%; border: 1.5px solid #313244; }
  .gh-login { font-size: 12px; color: #7f849c; font-weight: 500; }
  .gh-hint { font-size: 12px; color: #45475a; }

  .connect-btn {
    display: flex; align-items: center; gap: 6px;
    border: 1px solid #313244; background: rgba(137,180,250,0.08);
    color: #89b4fa; font-size: 12px; font-weight: 600;
    padding: 5px 12px; border-radius: 6px; cursor: pointer; font-family: inherit;
    transition: all 0.15s;
  }
  .connect-btn:hover { background: rgba(137,180,250,0.16); border-color: #89b4fa; }
  .connect-btn svg { flex-shrink: 0; }

  .link-btn {
    border: none; background: none; color: #585b70; font-size: 11px;
    cursor: pointer; font-family: inherit; padding: 2px 4px;
    transition: color 0.1s;
  }
  .link-btn:hover { color: #a6adc8; }

  /* ── Auth modal ── */
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
    width: 100%;
    max-width: 400px;
    padding: 32px 28px 24px;
    display: flex; flex-direction: column; align-items: stretch; gap: 0;
    animation: slide-up 0.18s ease;
  }
  @keyframes slide-up { from { transform: translateY(12px); opacity: 0; } to { transform: none; opacity: 1; } }

  .auth-logo-wrap {
    display: flex; justify-content: center; margin-bottom: 16px;
  }
  .auth-gh-icon {
    color: #585b70;
    filter: drop-shadow(0 0 16px rgba(137,180,250,0.2));
  }

  .auth-title {
    font-size: 17px; font-weight: 700; color: #cdd6f4;
    text-align: center; margin: 0 0 8px;
  }
  .auth-desc {
    font-size: 13px; color: #585b70; text-align: center;
    line-height: 1.6; margin: 0 0 24px;
  }

  .auth-steps {
    display: flex; flex-direction: column; gap: 14px;
    margin-bottom: 20px;
    padding: 16px;
    background: #181825;
    border: 1px solid #252535;
    border-radius: 10px;
  }
  .auth-step {
    display: flex; align-items: flex-start; gap: 12px;
  }
  .step-num {
    width: 22px; height: 22px; border-radius: 50%;
    background: rgba(137,180,250,0.12); border: 1px solid rgba(137,180,250,0.25);
    color: #89b4fa; font-size: 11px; font-weight: 700;
    display: flex; align-items: center; justify-content: center;
    flex-shrink: 0; margin-top: 1px;
  }
  .step-body {
    display: flex; flex-direction: column; gap: 5px; min-width: 0;
  }
  .step-label {
    font-size: 13px; color: #cdd6f4; font-weight: 500; line-height: 1.3;
  }
  .step-link {
    font-size: 12px; color: #89b4fa; text-decoration: none; width: fit-content;
    transition: opacity 0.1s;
  }
  .step-link:hover { opacity: 0.8; text-decoration: underline; }
  .scope-chips {
    display: flex; align-items: center; gap: 6px; flex-wrap: wrap;
  }
  .scope {
    font-family: 'JetBrains Mono', monospace; font-size: 11px;
    background: #252535; color: #f9e2af;
    padding: 2px 6px; border-radius: 4px; border: 1px solid #313244;
  }
  .scope-note {
    font-size: 11px; color: #45475a;
  }
  .scope-note .scope { color: #a6adc8; }

  .auth-input-wrap {
    margin-bottom: 12px;
  }
  .auth-token-input {
    width: 100%; background: #11111b; border: 1px solid #313244;
    border-radius: 8px; color: #cdd6f4; font-size: 13px;
    padding: 10px 14px; outline: none;
    font-family: 'JetBrains Mono', monospace; letter-spacing: 0.04em;
    transition: border-color 0.15s;
  }
  .auth-token-input:focus { border-color: #89b4fa; box-shadow: 0 0 0 3px rgba(137,180,250,0.1); }
  .auth-token-input::placeholder { color: #313244; letter-spacing: normal; }

  .auth-error {
    font-size: 12px; color: #f38ba8; margin-bottom: 12px;
    padding: 8px 12px; background: rgba(243,139,168,0.08);
    border: 1px solid rgba(243,139,168,0.2); border-radius: 7px; line-height: 1.4;
  }

  .auth-btns {
    display: flex; gap: 8px;
  }
  .auth-connect-btn {
    flex: 1; display: flex; align-items: center; justify-content: center; gap: 7px;
    border: none; background: #89b4fa; color: #11111b;
    font-size: 13px; font-weight: 700; padding: 10px 20px;
    border-radius: 8px; cursor: pointer; font-family: inherit;
    transition: opacity 0.12s;
  }
  .auth-connect-btn:hover:not(:disabled) { opacity: 0.88; }
  .auth-connect-btn:disabled { opacity: 0.35; cursor: not-allowed; }
  .auth-cancel-btn {
    border: 1px solid #313244; background: none; color: #585b70;
    font-size: 13px; padding: 10px 16px; border-radius: 8px;
    cursor: pointer; font-family: inherit; transition: all 0.1s;
  }
  .auth-cancel-btn:hover { color: #a6adc8; background: #252535; border-color: #45475a; }

  .auth-spin {
    animation: spin 0.8s linear infinite;
  }

  .rename-modal {
    width: min(420px, 100%);
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 12px;
    box-shadow: 0 24px 64px rgba(0, 0, 0, 0.6);
    padding: 20px;
    display: flex;
    flex-direction: column;
    gap: 12px;
    animation: slide-up 0.18s ease;
  }
  .rename-title {
    margin: 0;
    font-size: 16px;
    font-weight: 700;
    color: #cdd6f4;
  }
  .rename-desc {
    margin: 0;
    font-size: 12px;
    color: #7f849c;
  }
  .rename-input {
    width: 100%;
    border: 1px solid #313244;
    background: #11111b;
    color: #cdd6f4;
    border-radius: 8px;
    font-size: 13px;
    padding: 9px 12px;
    outline: none;
    font-family: inherit;
  }
  .rename-input:focus {
    border-color: #89b4fa;
    box-shadow: 0 0 0 3px rgba(137,180,250,0.1);
  }
  .rename-input:disabled {
    opacity: 0.6;
  }
  .rename-error {
    font-size: 12px;
    color: #f38ba8;
    background: rgba(243,139,168,0.08);
    border: 1px solid rgba(243,139,168,0.2);
    border-radius: 7px;
    padding: 8px 10px;
  }
  .rename-actions {
    display: flex;
    gap: 8px;
    justify-content: flex-end;
  }
  .rename-confirm,
  .rename-cancel {
    border-radius: 8px;
    padding: 8px 12px;
    font-size: 12px;
    font-weight: 600;
    font-family: inherit;
    cursor: pointer;
    transition: all 0.12s;
  }
  .rename-confirm {
    border: none;
    background: #89b4fa;
    color: #11111b;
  }
  .rename-confirm:hover:not(:disabled) { opacity: 0.9; }
  .rename-confirm:disabled { opacity: 0.35; cursor: not-allowed; }
  .rename-cancel {
    border: 1px solid #313244;
    background: none;
    color: #a6adc8;
  }
  .rename-cancel:hover:not(:disabled) { background: #252535; }
  .rename-cancel:disabled { opacity: 0.35; cursor: not-allowed; }

  /* ── Toolbar bar ── */
  .toolbar-bar {
    display: flex; align-items: center; gap: 6px;
    padding: 8px 16px;
    background: #181825;
    border-bottom: 1px solid #1e1e2e;
    flex-shrink: 0;
  }

  .search-wrap {
    flex: 1; position: relative; display: flex; align-items: center;
  }
  .search-icon {
    position: absolute; left: 8px; color: #45475a; pointer-events: none;
  }
  .search-input {
    width: 100%; background: #11111b; border: 1px solid #252535;
    border-radius: 6px; color: #cdd6f4; font-size: 12px;
    padding: 6px 10px 6px 28px; outline: none; font-family: inherit;
    transition: border-color 0.15s;
  }
  .search-input:focus { border-color: #45475a; }
  .search-input::placeholder { color: #313244; }

  .new-btn {
    display: flex; align-items: center; gap: 5px;
    border: 1px solid #252535; background: #1e1e2e; color: #a6adc8;
    font-size: 12px; font-weight: 500; padding: 5px 10px;
    border-radius: 6px; cursor: pointer; font-family: inherit;
    white-space: nowrap; transition: all 0.12s;
  }
  .new-btn:hover { border-color: #45475a; color: #cdd6f4; background: #252535; }
  .new-btn svg { flex-shrink: 0; }

  .icon-btn {
    border: none; background: none; color: #45475a;
    padding: 5px; cursor: pointer; border-radius: 5px;
    display: flex; align-items: center; justify-content: center;
    transition: all 0.1s;
  }
  .icon-btn:hover { color: #a6adc8; background: #252535; }

  /* ── Error bar ── */
  .error-bar {
    padding: 7px 16px; font-size: 12px; color: #f38ba8;
    background: rgba(243,139,168,0.08);
    border-bottom: 1px solid rgba(243,139,168,0.15);
    flex-shrink: 0;
  }

  /* ── Project list ── */
  .project-list {
    flex: 1; overflow-y: auto; min-height: 0;
    padding: 12px 16px;
  }
  .project-list::-webkit-scrollbar { width: 4px; }
  .project-list::-webkit-scrollbar-track { background: transparent; }
  .project-list::-webkit-scrollbar-thumb { background: #252535; border-radius: 2px; }

  .empty-state {
    display: flex; flex-direction: column; align-items: center; justify-content: center;
    gap: 8px; padding: 48px 20px; color: #45475a; font-size: 13px;
  }
  .empty-icon { color: #313244; }
  .empty-sub { font-size: 11px; color: #313244; }
  .err-msg { color: #f38ba8; font-size: 13px; text-align: center; }

  .spinner {
    width: 20px; height: 20px; border: 2px solid #252535;
    border-top-color: #89b4fa; border-radius: 50%;
    animation: spin 0.7s linear infinite;
  }
  @keyframes spin { to { transform: rotate(360deg); } }

  /* ── Card grid ── */
  .card-grid {
    display: flex; flex-direction: column; gap: 4px;
  }

  .card {
    position: relative;
    display: flex; align-items: center; gap: 12px;
    padding: 10px 12px;
    background: #181825;
    border: 1px solid #1e1e2e;
    border-radius: 8px;
    cursor: pointer; text-align: left; color: inherit;
    font-family: inherit;
    transition: all 0.12s ease;
    min-width: 0;
  }
  .card:hover {
    background: #1e1e2e;
    border-color: #313244;
    box-shadow: 0 2px 12px rgba(0,0,0,0.2);
  }
  .card:hover .del-btn { opacity: 0.4; }
  .card.busy { opacity: 0.45; pointer-events: none; }
  .card:disabled { cursor: wait; }

  /* ── Card icon ── */
  .card-icon {
    width: 38px; height: 38px;
    display: flex; align-items: center; justify-content: center;
    border-radius: 8px; flex-shrink: 0;
    transition: background 0.12s;
  }
  .card-icon.single { background: rgba(137,180,250,0.1); color: #89b4fa; }
  .card-icon.multi  { background: rgba(166,227,161,0.1); color: #a6e3a1; }
  .card:hover .card-icon.single { background: rgba(137,180,250,0.18); }
  .card:hover .card-icon.multi  { background: rgba(166,227,161,0.18); }

  /* ── Card body ── */
  .card-body {
    flex: 1; display: flex; flex-direction: column; gap: 5px; min-width: 0;
  }
  .card-name {
    font-size: 13px; font-weight: 600; color: #cdd6f4;
    overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
  }

  /* ── Sync row (always visible, shows both locations) ── */
  .sync-row {
    display: flex; align-items: center; gap: 5px;
    flex-wrap: nowrap; min-width: 0;
  }
  .loc-badge {
    display: inline-flex; align-items: center; gap: 3px;
    padding: 2px 6px; border-radius: 4px;
    font-size: 10px; font-weight: 600; letter-spacing: 0.03em;
    border: 1px solid #252535; background: #181825; color: #313244;
    white-space: nowrap; flex-shrink: 0;
    transition: all 0.15s; user-select: none;
  }
  .loc-badge.present { border-color: rgba(249,226,175,0.3); background: rgba(249,226,175,0.07); color: #f9e2af; }
  .loc-badge.github.present { border-color: rgba(137,180,250,0.3); background: rgba(137,180,250,0.07); color: #89b4fa; }
  .sync-conn {
    font-size: 11px; font-weight: 800; flex-shrink: 0;
    color: #252535; line-height: 1; transition: color 0.15s;
  }
  .sync-conn.synced { color: #a6e3a1; }
  .sync-conn.local-only { color: #313244; }
  .sync-conn.remote-only { color: #313244; }
  .sync-act {
    display: inline-flex; align-items: center; gap: 4px;
    padding: 2px 7px; border-radius: 4px;
    font-size: 10px; font-weight: 700; letter-spacing: 0.03em;
    cursor: pointer; font-family: inherit; border: 1px solid;
    white-space: nowrap; flex-shrink: 0; transition: all 0.12s;
  }
  .sync-act:disabled { opacity: 0.25; cursor: not-allowed; }
  .sync-act.push { color: #89b4fa; background: rgba(137,180,250,0.08); border-color: rgba(137,180,250,0.3); }
  .sync-act.push:hover:not(:disabled) { background: rgba(137,180,250,0.2); border-color: #89b4fa; }
  .sync-act.pull { color: #a6e3a1; background: rgba(166,227,161,0.08); border-color: rgba(166,227,161,0.3); }
  .sync-act.pull:hover:not(:disabled) { background: rgba(166,227,161,0.2); border-color: #a6e3a1; }
  .sync-ts { font-size: 10px; color: #45475a; font-style: italic; }

  /* ── Delete button (revealed on card hover) ── */
  .del-wrap { flex-shrink: 0; }
  .del-btn {
    width: 28px; height: 28px;
    display: flex; align-items: center; justify-content: center;
    border: 1px solid transparent; background: none; color: #313244;
    cursor: pointer; border-radius: 6px;
    opacity: 0; transition: all 0.12s;
  }
  .del-btn:hover:not(:disabled) { color: #f38ba8; background: rgba(243,139,168,0.1); border-color: rgba(243,139,168,0.25); opacity: 1; }
  .del-btn:disabled { opacity: 0; cursor: not-allowed; }

  /* ── Context menu ── */
  .ctx-backdrop {
    position: fixed; inset: 0; z-index: 200;
  }
  .ctx-menu {
    position: fixed;
    min-width: 184px;
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 8px;
    box-shadow: 0 8px 32px rgba(0,0,0,0.55), 0 0 0 1px rgba(0,0,0,0.1);
    padding: 4px;
    animation: fade-in 0.08s ease;
  }
  .ctx-item {
    display: flex; align-items: center; gap: 10px;
    width: 100%; padding: 7px 12px;
    border: none; background: none;
    color: #cdd6f4; font-size: 12px; font-weight: 500;
    font-family: inherit; cursor: pointer;
    border-radius: 5px; text-align: left;
    transition: background 0.08s;
  }
  .ctx-item:hover { background: #252535; }
  .ctx-item svg { flex-shrink: 0; color: #585b70; }
  .ctx-item:hover svg { color: #a6adc8; }
  .ctx-item.danger { color: #585b70; }
  .ctx-item.danger:hover { color: #f38ba8; background: rgba(243,139,168,0.08); }
  .ctx-item.danger:hover svg { color: #f38ba8; }
  .ctx-sep {
    height: 1px; background: #252535; margin: 4px 8px;
  }
</style>
