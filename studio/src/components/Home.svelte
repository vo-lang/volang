<script lang="ts">
  import { onMount } from 'svelte';
  import { get } from 'svelte/store';
  import { explorer } from '../stores/explorer';
  import { github, logout, loadSavedToken } from '../stores/github';
  import { projects, syncState, syncStateLabel } from '../stores/projects';
  import type { ProjectEntry, SyncState } from '../stores/projects';
  import { actions } from '../lib/actions';
  import { bridge } from '../lib/bridge';
  import SyncDetailModal from './SyncDetailModal.svelte';
  import GitHubAuthModal from './GitHubAuthModal.svelte';
  import ProjectContextMenu from './ProjectContextMenu.svelte';
  import RenameProjectModal from './RenameProjectModal.svelte';

  // ── Auth ───────────────────────────────────────────────────────────────────
  let showAuthModal = false;

  onMount(() => {
    loadSavedToken();
    const root = get(explorer).localRoot;
    if (root) actions.loadProjects(root);
  });

  // Reload projects when GitHub auth state changes
  let prevToken: string | null = null;
  $: {
    const tok = $github.token;
    if (tok !== prevToken) {
      prevToken = tok;
      const root = $explorer.localRoot;
      if (root) actions.loadProjects(root);
    }
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
    await actions.loadProjects(root);
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
    await actions.loadProjects(root);
    const ps = get(projects);
    const created = ps.projects.find(p => p.name === dirName && p.type === 'multi');
    if (created) await openProject(created);
  }

  async function refresh() {
    const root = $explorer.localRoot;
    if (root) await actions.loadProjects(root);
  }

  // ── Helpers ────────────────────────────────────────────────────────────────
  function formatTimestamp(value: string | null): string {
    if (!value) return '';
    const d = new Date(value);
    if (Number.isNaN(d.getTime())) return '';
    return d.toLocaleString(undefined, {
      month: 'short', day: 'numeric',
      hour: '2-digit', minute: '2-digit', hour12: false,
    });
  }

  function syncIcon(state: SyncState): string {
    switch (state) {
      case 'in-sync': return '✓';
      case 'local-ahead': return '↑';
      case 'remote-ahead': return '↓';
      case 'diverged': return '⇅';
      case 'local-only': return '→';
      case 'remote-only': return '←';
    }
  }

  // ── Sync detail modal ──────────────────────────────────────────────────────
  let syncDetailProject: { project: ProjectEntry; state: SyncState } | null = null;

  function openSyncDetail(p: ProjectEntry, state: SyncState) {
    syncDetailProject = { project: p, state };
  }

  async function handleSyncPush() {
    if (!syncDetailProject) return;
    const p = syncDetailProject.project;
    syncDetailProject = null;
    await pushProject(p);
  }

  async function handleSyncPull() {
    if (!syncDetailProject) return;
    const p = syncDetailProject.project;
    syncDetailProject = null;
    await pullProject(p);
  }

  // ── Filtered / sorted list ─────────────────────────────────────────────────
  let searchQuery = '';
  $: filteredProjects = $projects.projects.filter(p =>
    !searchQuery || p.name.toLowerCase().includes(searchQuery.toLowerCase())
  );

  // ── Context menu ──────────────────────────────────────────────────────────
  let ctxMenu: { x: number; y: number; project: ProjectEntry; state: SyncState } | null = null;
  let renameTarget: ProjectEntry | null = null;

  function onCardContext(e: MouseEvent, p: ProjectEntry, state: SyncState) {
    const menuW = 220, menuH = 290;
    const x = e.clientX + menuW > window.innerWidth ? e.clientX - menuW : e.clientX;
    const y = e.clientY + menuH > window.innerHeight ? e.clientY - menuH : e.clientY;
    ctxMenu = { x: Math.max(0, x), y: Math.max(0, y), project: p, state };
  }

  async function handleRenameConfirm(e: CustomEvent<{ project: ProjectEntry; newName: string }>) {
    const { project: p, newName } = e.detail;
    const root = $explorer.localRoot;
    if (!root) return;
    try {
      await actions.renameProject(p, newName, root);
    } catch (e: any) {
      actionError = `Rename failed: ${e.message ?? e}`;
    } finally {
      renameTarget = null;
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
      if (syncDetailProject) { syncDetailProject = null; }
      else if (ctxMenu) { ctxMenu = null; }
      else if (renameTarget) { renameTarget = null; }
      else if (showAuthModal) { showAuthModal = false; }
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
      <button class="link-btn" on:click={logout}>Sign out</button>
    {:else if $github.isLoading}
      <span class="gh-hint">Connecting…</span>
      {:else}
      <button class="connect-btn" on:click={() => (showAuthModal = true)}>
        <svg viewBox="0 0 16 16" width="14" height="14" fill="currentColor"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"/></svg>
        Connect GitHub
      </button>
    {/if}
  </div>
  {#if showAuthModal}
    <GitHubAuthModal on:close={() => (showAuthModal = false)} />
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
            <!-- Info + sync status -->
            <div class="card-body">
              <span class="card-name">{p.name}</span>
              <div class="sync-row">
                <!-- Sync state badge (clickable to open diff when both local+remote exist) -->
                {#if p.localPath && p.remote}
                  <button class="sync-badge {state}" title="View sync details" on:click|stopPropagation={() => openSyncDetail(p, state)}>
                    <span class="sync-icon">{syncIcon(state)}</span>
                    {syncStateLabel(state)}
                  </button>
                {:else}
                  <span class="sync-badge {state}">
                    <span class="sync-icon">{syncIcon(state)}</span>
                    {syncStateLabel(state)}
                  </span>
                {/if}

                <!-- Timestamps -->
                {#if p.localPath && p.remote}
                  {#if state !== 'in-sync'}
                    <span class="sync-ts local" title="Last synced">
                      <svg viewBox="0 0 24 24" width="10" height="10" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="2" y="3" width="20" height="14" rx="2"/><line x1="8" y1="21" x2="16" y2="21"/><line x1="12" y1="17" x2="12" y2="21"/></svg>
                      {formatTimestamp(p.pushedAt) || 'never'}
                    </span>
                    {#if p.remoteUpdatedAt}
                      <span class="sync-ts remote" title="Remote updated">
                        <svg viewBox="0 0 24 24" width="10" height="10" fill="currentColor"><path d="M12 0C5.37 0 0 5.37 0 12c0 5.31 3.435 9.795 8.205 11.385.6.105.825-.255.825-.57 0-.285-.015-1.23-.015-2.235-3.015.555-3.795-.735-4.035-1.41-.135-.345-.72-1.41-1.23-1.695-.42-.225-1.02-.78-.015-.795.945-.015 1.62.87 1.845 1.23 1.08 1.815 2.805 1.305 3.495.99.105-.78.42-1.305.765-1.605-2.67-.3-5.46-1.335-5.46-5.925 0-1.305.465-2.385 1.23-3.225-.12-.3-.54-1.53.12-3.18 0 0 1.005-.315 3.3 1.23.96-.27 1.98-.405 3-.405s2.04.135 3 .405c2.295-1.56 3.3-1.23 3.3-1.23.66 1.65.24 2.88.12 3.18.765.84 1.23 1.905 1.23 3.225 0 4.605-2.805 5.625-5.475 5.925.435.375.81 1.095.81 2.22 0 1.605-.015 2.895-.015 3.3 0 .315.225.69.825.57A12.02 12.02 0 0024 12c0-6.63-5.37-12-12-12z"/></svg>
                        {formatTimestamp(p.remoteUpdatedAt)}
                      </span>
                    {/if}
                  {:else}
                    <span class="sync-ts synced" title="Last synced">{formatTimestamp(p.pushedAt)}</span>
                  {/if}
                {/if}

                <!-- Action buttons -->
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
                {:else if state === 'local-ahead' || state === 'diverged'}
                  <button class="sync-act push" title="Push local to remote (overwrite)" disabled={busy} on:click|stopPropagation={() => pushProject(p)}>
                    <svg viewBox="0 0 24 24" width="10" height="10" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="19" x2="12" y2="5"/><polyline points="5 12 12 5 19 12"/></svg>
                    Push
                  </button>
                  {#if state === 'diverged'}
                    <button class="sync-act pull" title="Pull remote to local (overwrite)" disabled={busy} on:click|stopPropagation={() => pullProject(p)}>
                      <svg viewBox="0 0 24 24" width="10" height="10" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="5" x2="12" y2="19"/><polyline points="19 12 12 19 5 12"/></svg>
                      Pull
                    </button>
                  {/if}
                {:else if state === 'remote-ahead'}
                  <button class="sync-act pull" title="Pull remote to local (overwrite)" disabled={busy} on:click|stopPropagation={() => pullProject(p)}>
                    <svg viewBox="0 0 24 24" width="10" height="10" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="5" x2="12" y2="19"/><polyline points="19 12 12 19 5 12"/></svg>
                    Pull
                  </button>
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

  {#if ctxMenu}
    <ProjectContextMenu
      x={ctxMenu.x}
      y={ctxMenu.y}
      project={ctxMenu.project}
      state={ctxMenu.state}
      on:close={() => (ctxMenu = null)}
      on:open={(e) => openProject(e.detail)}
      on:rename={(e) => { renameTarget = e.detail; }}
      on:viewDiff={(e) => openSyncDetail(e.detail.project, e.detail.state)}
      on:push={(e) => pushProject(e.detail)}
      on:pull={(e) => pullProject(e.detail)}
      on:viewOnGitHub={(e) => viewOnGitHub(e.detail)}
      on:delete={(e) => deleteProject(e.detail)}
    />
  {/if}

  {#if renameTarget}
    <RenameProjectModal
      project={renameTarget}
      on:close={() => (renameTarget = null)}
      on:confirm={handleRenameConfirm}
    />
  {/if}

  {#if syncDetailProject}
    <SyncDetailModal
      project={syncDetailProject.project}
      state={syncDetailProject.state}
      busy={busyProject === projectKey(syncDetailProject.project)}
      on:close={() => (syncDetailProject = null)}
      on:push={handleSyncPush}
      on:pull={handleSyncPull}
    />
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

  /* ── Sync row (always visible, shows status + timestamps + actions) ── */
  .sync-row {
    display: flex; align-items: center; gap: 5px;
    flex-wrap: nowrap; min-width: 0;
  }

  /* ── Sync badge ── */
  .sync-badge {
    display: inline-flex; align-items: center; gap: 3px;
    padding: 2px 7px; border-radius: 4px;
    font-size: 10px; font-weight: 700; letter-spacing: 0.03em;
    border: 1px solid; white-space: nowrap; flex-shrink: 0;
    user-select: none; transition: all 0.12s;
    background: none; font-family: inherit; color: inherit;
  }
  button.sync-badge { cursor: pointer; }
  button.sync-badge:hover { filter: brightness(1.15); }
  .sync-icon { font-weight: 800; font-size: 11px; line-height: 1; }

  .sync-badge.in-sync {
    color: #a6e3a1; border-color: rgba(166,227,161,0.3); background: rgba(166,227,161,0.08);
  }
  .sync-badge.local-ahead {
    color: #f9e2af; border-color: rgba(249,226,175,0.3); background: rgba(249,226,175,0.08);
  }
  .sync-badge.remote-ahead {
    color: #89b4fa; border-color: rgba(137,180,250,0.3); background: rgba(137,180,250,0.08);
  }
  .sync-badge.diverged {
    color: #f38ba8; border-color: rgba(243,139,168,0.3); background: rgba(243,139,168,0.08);
  }
  .sync-badge.local-only {
    color: #45475a; border-color: #252535; background: rgba(69,71,90,0.06);
  }
  .sync-badge.remote-only {
    color: #585b70; border-color: #252535; background: rgba(88,91,112,0.06);
  }

  /* ── Sync timestamps ── */
  .sync-ts {
    display: inline-flex; align-items: center; gap: 3px;
    font-size: 10px; color: #45475a; white-space: nowrap; flex-shrink: 0;
  }
  .sync-ts svg { flex-shrink: 0; opacity: 0.6; }
  .sync-ts.local { color: #585b70; }
  .sync-ts.remote { color: #585b70; }
  .sync-ts.synced { color: #45475a; font-style: italic; }

  /* ── Sync action buttons ── */
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

</style>
