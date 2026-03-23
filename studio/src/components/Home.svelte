<script lang="ts">
  import { tick } from 'svelte';
  import type { ProjectCatalogService } from '../lib/services/project_catalog_service';
  import type { ManagedProject, ProjectDiffResult } from '../lib/project_catalog/types';
  import { projectKey, syncState } from '../lib/project_catalog/types';
  import { formatError } from '../lib/format_error';
  import CreateProjectModal from './home/CreateProjectModal.svelte';
  import ProjectActionsMenu from './home/ProjectActionsMenu.svelte';
  import ProjectCard from './home/ProjectCard.svelte';
  import ProjectRenameModal from './home/ProjectRenameModal.svelte';
  import ProjectSyncModal from './home/ProjectSyncModal.svelte';

  // Example source imports (raw strings)
  import exChannels from '../assets/examples/channels.vo?raw';
  import exClosures from '../assets/examples/closures.vo?raw';
  import exDefer from '../assets/examples/defer.vo?raw';
  import exErrorHandling from '../assets/examples/error_handling.vo?raw';
  import exGoroutines from '../assets/examples/goroutines.vo?raw';
  import exInterfaces from '../assets/examples/interfaces.vo?raw';
  import exSelect from '../assets/examples/select.vo?raw';
  import exTime from '../assets/examples/time.vo?raw';
  import exRegexp from '../assets/examples/regexp.vo?raw';
  import exGuiCounter from '../assets/examples/gui_counter.vo?raw';
  import exGuiTodo from '../assets/examples/gui_todo.vo?raw';
  import exGuiTetris from '../assets/examples/gui_tetris.vo?raw';
  import exGuiShowcase from '../assets/examples/gui_showcase.vo?raw';
  import exDashboard from '../assets/examples/dashboard.vo?raw';

  interface Example {
    name: string;
    file: string;
    desc: string;
    source: string;
    tag: 'lang' | 'gui';
  }

  const examples: Example[] = [
    { name: 'Channels',        file: 'channels.vo',        desc: 'Goroutine communication',       source: exChannels,      tag: 'lang' },
    { name: 'Closures',        file: 'closures.vo',        desc: 'Captured variables',            source: exClosures,      tag: 'lang' },
    { name: 'Defer',           file: 'defer.vo',           desc: 'LIFO cleanup',                  source: exDefer,         tag: 'lang' },
    { name: 'Error Handling',  file: 'error_handling.vo',  desc: '? operator and errdefer',       source: exErrorHandling, tag: 'lang' },
    { name: 'Goroutines',      file: 'goroutines.vo',      desc: 'Concurrent computation',        source: exGoroutines,    tag: 'lang' },
    { name: 'Interfaces',      file: 'interfaces.vo',      desc: 'Duck typing',                   source: exInterfaces,    tag: 'lang' },
    { name: 'Select',          file: 'select.vo',          desc: 'Channel multiplexing',          source: exSelect,        tag: 'lang' },
    { name: 'Time',            file: 'time.vo',            desc: 'Duration and timers',           source: exTime,          tag: 'lang' },
    { name: 'Regexp',          file: 'regexp.vo',          desc: 'Pattern matching',              source: exRegexp,        tag: 'lang' },
    { name: 'Counter',         file: 'gui_counter.vo',     desc: 'Minimal GUI app',               source: exGuiCounter,    tag: 'gui' },
    { name: 'Todo App',        file: 'gui_todo.vo',        desc: 'List rendering & events',       source: exGuiTodo,       tag: 'gui' },
    { name: 'Tetris',          file: 'gui_tetris.vo',      desc: 'Canvas 2D game loop',           source: exGuiTetris,     tag: 'gui' },
    { name: 'Showcase',        file: 'gui_showcase.vo',    desc: 'All GUI components',            source: exGuiShowcase,   tag: 'gui' },
    { name: 'Dashboard',       file: 'dashboard.vo',       desc: 'Composition patterns',          source: exDashboard,     tag: 'gui' },
  ];

  $: langExamples = examples.filter(e => e.tag === 'lang');
  $: guiExamples = examples.filter(e => e.tag === 'gui');

  let examplesExpanded = false;

  export let projectCatalog: ProjectCatalogService;
  export let onOpenProject: (project: ManagedProject) => Promise<void> | void = () => {};
  export let onOpenExample: (source: string, filename: string) => Promise<void> | void = () => {};
  export let onDocs: () => void = () => {};
  export let onDevelop: () => void = () => {};
  export let onConnectGitHub: () => void = () => {};

  let searchQuery = '';
  let actionError = '';
  let showCreateModal = false;
  let menuState: { x: number; y: number; project: ManagedProject } | null = null;
  let renameTarget: ManagedProject | null = null;
  let syncDialog: ProjectDiffResult | null = null;
  let syncDialogProjectKey = '';
  let confirmDialog: { title: string; message: string; danger: boolean; action: () => void } | null = null;
  let creating = false;
  let createError = '';
  let localRefreshRequested = false;

  $: githubStore = projectCatalog.github;
  $: catalogStore = projectCatalog.catalog;
  $: filteredProjects = $catalogStore.projects.filter((project) =>
    !searchQuery || project.name.toLowerCase().includes(searchQuery.toLowerCase()),
  );
  $: isInitialLoading = $catalogStore.loading;
  $: isRefreshing = localRefreshRequested || $catalogStore.refreshing || $catalogStore.remoteLoading;
  $: hasProjects = !$catalogStore.loading && $catalogStore.projects.length > 0;
  $: isGitHubConnected = Boolean($githubStore.user);

  function busy(project: ManagedProject): boolean {
    return $catalogStore.busyKeys.includes(projectKey(project));
  }

  function checkingSync(project: ManagedProject): boolean {
    return $catalogStore.checkingSyncKeys.includes(projectKey(project));
  }

  function waitForPaint(): Promise<void> {
    return tick().then(() => new Promise((resolve) => requestAnimationFrame(() => setTimeout(resolve, 0))));
  }

  async function triggerRefresh(): Promise<void> {
    if (isInitialLoading || localRefreshRequested || $catalogStore.refreshing || $catalogStore.remoteLoading) return;
    localRefreshRequested = true;
    await waitForPaint();
    void projectCatalog.refresh().finally(() => {
      localRefreshRequested = false;
    });
  }

  async function createProject(kind: 'single' | 'module', name: string): Promise<void> {
    createError = '';
    creating = true;
    try {
      const project = kind === 'single'
        ? await projectCatalog.createSingleProject(name)
        : await projectCatalog.createModuleProject(name);
      showCreateModal = false;
      createError = '';
      await openProject(project);
    } catch (error) {
      createError = formatError(error);
    } finally {
      creating = false;
    }
  }

  async function openProject(project: ManagedProject): Promise<void> {
    actionError = '';
    try {
      const ready = await projectCatalog.ensureProjectReady(project);
      await onOpenProject(ready);
      onDevelop();
    } catch (error) {
      actionError = formatError(error);
    }
  }

  async function pushProject(project: ManagedProject): Promise<void> {
    actionError = '';
    try {
      const updated = await projectCatalog.pushProject(project);
      if (syncDialog && projectKey(syncDialog.project) === projectKey(project)) {
        syncDialog = await projectCatalog.loadDiff(updated);
      }
    } catch (error) {
      actionError = formatError(error);
    }
  }

  async function pullProject(project: ManagedProject): Promise<void> {
    actionError = '';
    try {
      const updated = await projectCatalog.pullProject(project);
      if (syncDialogProjectKey === projectKey(project)) {
        syncDialog = await projectCatalog.loadDiff(updated);
      }
    } catch (error) {
      actionError = formatError(error);
    }
  }

  async function openDiff(project: ManagedProject): Promise<void> {
    actionError = '';
    syncDialogProjectKey = projectKey(project);
    try {
      syncDialog = await projectCatalog.loadDiff(project);
    } catch (error) {
      actionError = formatError(error);
    }
  }

  async function renameProject(name: string): Promise<void> {
    if (!renameTarget) return;
    actionError = '';
    try {
      await projectCatalog.renameProject(renameTarget, name);
      renameTarget = null;
    } catch (error) {
      actionError = formatError(error);
    }
  }

  function confirmDeleteProject(project: ManagedProject): void {
    confirmDialog = {
      title: 'Delete Project',
      message: `Delete "${project.name}" from local storage? This cannot be undone.`,
      danger: true,
      action: () => doDeleteProject(project),
    };
  }

  function confirmDeleteRemoteProject(project: ManagedProject): void {
    confirmDialog = {
      title: 'Delete Cloud Copy',
      message: project.localPath
        ? `Delete the cloud copy of "${project.name}"? Your local copy will remain available.`
        : `Delete the cloud copy of "${project.name}"? This will permanently remove it from GitHub.`,
      danger: true,
      action: () => doDeleteRemoteProject(project),
    };
  }

  function confirmDeleteEverywhere(project: ManagedProject): void {
    confirmDialog = {
      title: 'Delete Everywhere',
      message: `Delete both the local and cloud copies of "${project.name}"? This cannot be undone.`,
      danger: true,
      action: () => doDeleteEverywhere(project),
    };
  }

  function confirmForgetProject(project: ManagedProject): void {
    confirmDialog = {
      title: 'Remove from List',
      message: `Remove "${project.name}" from the project list? The cloud copy on GitHub will remain available.`,
      danger: true,
      action: () => doForgetProject(project),
    };
  }

  async function doDeleteProject(project: ManagedProject): Promise<void> {
    actionError = '';
    confirmDialog = null;
    try {
      await projectCatalog.deleteProject(project);
    } catch (error) {
      actionError = formatError(error);
    }
  }

  async function doForgetProject(project: ManagedProject): Promise<void> {
    actionError = '';
    confirmDialog = null;
    try {
      await projectCatalog.forgetRemoteProject(project);
    } catch (error) {
      actionError = formatError(error);
    }
  }

  async function doDeleteRemoteProject(project: ManagedProject): Promise<void> {
    actionError = '';
    confirmDialog = null;
    try {
      await projectCatalog.deleteRemoteProject(project);
    } catch (error) {
      actionError = formatError(error);
    }
  }

  async function doDeleteEverywhere(project: ManagedProject): Promise<void> {
    actionError = '';
    confirmDialog = null;
    try {
      await projectCatalog.deleteEverywhere(project);
    } catch (error) {
      actionError = formatError(error);
    }
  }

  function viewRemote(project: ManagedProject): void {
    if (!project.remote) return;
    const url = project.remote.kind === 'gist' && project.remote.gistId
      ? `https://gist.github.com/${project.remote.gistId}`
      : project.remote.owner && project.remote.repo
        ? `https://github.com/${project.remote.owner}/${project.remote.repo}`
        : '';
    if (!url) return;
    window.open(url, '_blank', 'noopener,noreferrer');
  }

</script>

<svelte:window
  on:keydown={(event) => {
    if (event.key !== 'Escape') return;
    menuState = null;
    renameTarget = null;
    syncDialog = null;
    showCreateModal = false;
    confirmDialog = null;
  }}
/>

<div class="home">
  <div class="ambient"></div>

  <!-- ═══════ DASHBOARD ═══════ -->
  <div class="dashboard">
    <!-- ── Left column: brand + examples ── -->
    <aside class="sidebar">
      <div class="brand-block">
        <h1 class="brand">Vo</h1>
        <p class="tagline">The scripting language<br/>for the Rust ecosystem.</p>
        <p class="subtitle">Statically typed · Go-like syntax<br/>Embeds in Rust · Compiles to WASM</p>
        <div class="pills">
          <span class="pill">VM</span>
          <span class="pill pill-jit">JIT</span>
          <span class="pill pill-wasm">WASM</span>
        </div>
        <div class="actions">
          <button class="btn-primary" on:click={() => { createError = ''; showCreateModal = true; }}>
            <svg class="btn-icon" viewBox="0 0 16 16" aria-hidden="true"><path d="M8 3v10M3 8h10"/></svg>
            New Project
          </button>
          <button class="btn-ghost" on:click={onDocs}>
            Docs
            <svg class="btn-arrow" viewBox="0 0 16 16" aria-hidden="true"><path d="M6 3l5 5-5 5"/></svg>
          </button>
        </div>
      </div>

      <div class="quick-try">
        <span class="quick-try-label">Quick try</span>
        <div class="ex-list">
          {#each langExamples as ex}
            <button class="ex-link" on:click={() => onOpenExample(ex.source, ex.file)}>
              <span class="ex-link-name">{ex.name}</span>
              <span class="ex-link-dot">·</span>
              <span class="ex-link-desc">{ex.desc}</span>
            </button>
          {/each}
        </div>
        <span class="quick-try-label quick-try-label-gui">GUI</span>
        <div class="ex-list">
          {#each guiExamples as ex}
            <button class="ex-link ex-link-gui" on:click={() => onOpenExample(ex.source, ex.file)}>
              <span class="ex-link-name">{ex.name}</span>
              <span class="ex-link-dot">·</span>
              <span class="ex-link-desc">{ex.desc}</span>
            </button>
          {/each}
        </div>
      </div>
    </aside>

    <!-- ── Right column: code + workspace ── -->
    <main class="content">
      <div class="code-area">
        <div class="code-glow"></div>
        <div class="code-window">
          <div class="code-titlebar">
            <span class="dot dot-r"></span><span class="dot dot-y"></span><span class="dot dot-g"></span>
            <span class="code-filename">error_handling.vo</span>
          </div>
          <pre class="code-body"><code><span class="hl-kw">func</span> <span class="hl-fn">readConfig</span>(path <span class="hl-type">string</span>) (<span class="hl-type">Config</span>, <span class="hl-type">error</span>) &#123;
    file := open(path)<span class="hl-op">?</span>
    <span class="hl-kw">errdefer</span> file.Close()

    data := readAll(file)<span class="hl-op">?</span>
    config := parse(data)<span class="hl-op">?</span>

    <span class="hl-kw">if</span> config.Version &lt; <span class="hl-num">1</span> &#123;
        <span class="hl-kw">fail</span> errors.New(<span class="hl-str">"invalid version"</span>)
    &#125;
    <span class="hl-kw">return</span> config, <span class="hl-lit">nil</span>
&#125;</code></pre>
          <button class="code-try" on:click={() => onOpenExample(exErrorHandling, 'error_handling.vo')}>
            Open in editor
            <svg class="btn-arrow small" viewBox="0 0 16 16" aria-hidden="true"><path d="M6 3l5 5-5 5"/></svg>
          </button>
        </div>
      </div>

      <!-- ── Workspace area (always visible) ── -->
      <div class="workspace">
        {#if !isGitHubConnected}
          <div class="gh-block">
            <button class="gh-cta" on:click={onConnectGitHub}>
              <div class="gh-cta-left">
                <svg class="gh-icon" viewBox="0 0 16 16" aria-hidden="true"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"/></svg>
                <div class="gh-cta-text">
                  <span class="gh-cta-title">Connect GitHub</span>
                  <span class="gh-cta-desc">Sync projects across devices and back up to Gist</span>
                </div>
              </div>
              <svg class="btn-arrow" viewBox="0 0 16 16" aria-hidden="true"><path d="M6 3l5 5-5 5"/></svg>
            </button>
            <p class="gh-warn">Code is AI-assisted and not audited for security. Use a test account with a minimal-scope token.</p>
          </div>
        {/if}

        <div class="projects-area">
          <div class="projects-header">
            <span class="projects-title">{isGitHubConnected ? 'Your Projects' : 'Local Projects'}</span>
            {#if hasProjects}
              <div class="projects-search">
                <svg class="search-icon" viewBox="0 0 16 16" aria-hidden="true"><circle cx="6.5" cy="6.5" r="4.5"/><path d="M10 10l4 4"/></svg>
                <input bind:value={searchQuery} placeholder="Search…" />
              </div>
            {/if}
            <div class="projects-header-spacer"></div>
            <button
              class="icon-btn"
              title="Refresh projects"
              disabled={isInitialLoading}
              on:click={triggerRefresh}
            ><span class="refresh-icon" class:spinning={isInitialLoading || isRefreshing}>↻</span></button>
            {#if $catalogStore.remoteLoading}
              <span class="sync-hint">Syncing GitHub…</span>
            {/if}
          </div>

          {#if isInitialLoading || isRefreshing}
            <div class="progress-track"><div class="progress-bar"></div></div>
          {/if}

          {#if actionError || $catalogStore.error || $githubStore.error}
            <div class="error-bar">
              <span>{actionError || $catalogStore.error || $githubStore.error}</span>
              <button class="error-dismiss" on:click={() => (actionError = '')}>×</button>
            </div>
          {/if}

          {#if $catalogStore.loading}
            <div class="grid">
              {#each Array(3) as _}
                <div class="skeleton-card">
                  <div class="skeleton-row">
                    <div class="skeleton-icon"></div>
                    <div class="skeleton-info">
                      <div class="skeleton-line skeleton-name"></div>
                      <div class="skeleton-line skeleton-meta"></div>
                    </div>
                  </div>
                </div>
              {/each}
            </div>
          {:else if filteredProjects.length === 0 && searchQuery}
            <div class="empty">
              <div class="empty-title">No matching projects</div>
              <div class="empty-sub">Try a different search term.</div>
            </div>
          {:else if hasProjects}
            <div class="grid">
              {#each filteredProjects as project (projectKey(project))}
                {@const state = syncState(project)}
                <ProjectCard
                  {project}
                  {state}
                  busy={busy(project)}
                  checkingSync={checkingSync(project)}
                  on:open={() => openProject(project)}
                  on:menu={(event) => {
                    menuState = { x: event.detail.x, y: event.detail.y, project };
                  }}
                />
              {/each}
            </div>
          {:else}
            <div class="empty-workspace">
              <span class="empty-workspace-text">No projects yet. Click <strong>New Project</strong> to get started.</span>
            </div>
          {/if}
        </div>
      </div>
    </main>
  </div>

  {#if showCreateModal}
    <CreateProjectModal
      busy={creating}
      error={createError}
      on:close={() => { showCreateModal = false; createError = ''; }}
      on:create={(event) => createProject(event.detail.kind, event.detail.name)}
    />
  {/if}

  {#if menuState}
    {@const project = menuState.project}
    {@const state = syncState(project)}
    <ProjectActionsMenu
      x={menuState.x}
      y={menuState.y}
      {project}
      {state}
      hasGitHub={Boolean($githubStore.user)}
      busy={busy(project)}
      checkingSync={checkingSync(project)}
      on:close={() => (menuState = null)}
      on:open={() => openProject(project)}
      on:rename={() => {
        renameTarget = project;
        menuState = null;
      }}
      on:diff={() => {
        menuState = null;
        void openDiff(project);
      }}
      on:push={() => {
        menuState = null;
        void pushProject(project);
      }}
      on:pull={() => {
        menuState = null;
        void pullProject(project);
      }}
      on:remote={() => {
        menuState = null;
        viewRemote(project);
      }}
      on:delete={() => { menuState = null; confirmDeleteProject(project); }}
      on:deleteRemote={() => { menuState = null; confirmDeleteRemoteProject(project); }}
      on:deleteEverywhere={() => { menuState = null; confirmDeleteEverywhere(project); }}
      on:forget={() => { menuState = null; confirmForgetProject(project); }}
    />
  {/if}

  {#if renameTarget}
    <ProjectRenameModal
      project={renameTarget}
      busy={busy(renameTarget)}
      on:close={() => (renameTarget = null)}
      on:confirm={(event) => renameProject(event.detail.name)}
    />
  {/if}

  {#if confirmDialog}
    <div class="confirm-backdrop" role="button" tabindex="0" aria-label="Close" on:click|self={() => (confirmDialog = null)} on:keydown={(e) => e.key === 'Escape' && (confirmDialog = null)}>
      <div class="confirm-modal" role="dialog" aria-modal="true">
        <h3 class:danger={confirmDialog.danger}>{confirmDialog.title}</h3>
        <p class="confirm-msg">{confirmDialog.message}</p>
        <div class="confirm-actions">
          <button class="secondary" on:click={() => (confirmDialog = null)}>Cancel</button>
          <button class={confirmDialog.danger ? 'danger-btn' : 'primary'} on:click={confirmDialog.action}>{confirmDialog.title}</button>
        </div>
      </div>
    </div>
  {/if}

  {#if syncDialog}
    <ProjectSyncModal
      diff={syncDialog}
      busy={$catalogStore.busyKeys.includes(projectKey(syncDialog.project))}
      on:close={() => {
        syncDialog = null;
        syncDialogProjectKey = '';
      }}
      on:push={() => void pushProject(syncDialog.project)}
      on:pull={() => void pullProject(syncDialog.project)}
    />
  {/if}
</div>

<style>
  /* ══════════════════════════════════════════
     ROOT — full-height, no page scroll
     ══════════════════════════════════════════ */
  .home {
    flex: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
    position: relative;
    background: #06060c;
    color: #cdd6f4;
  }

  .ambient {
    position: absolute;
    inset: 0;
    pointer-events: none;
    z-index: 0;
    background:
      radial-gradient(ellipse 60% 50% at 20% 0%, rgba(137, 180, 250, 0.08) 0%, transparent 70%),
      radial-gradient(ellipse 40% 40% at 80% 10%, rgba(116, 199, 236, 0.05) 0%, transparent 60%);
  }

  /* ══════════════════════════════════════════
     DASHBOARD — two-column layout
     ══════════════════════════════════════════ */
  .dashboard {
    flex: 1;
    display: grid;
    grid-template-columns: 280px 1fr;
    min-height: 0;
    position: relative;
    z-index: 1;
  }

  /* ══════════════════════════════════════════
     LEFT SIDEBAR
     ══════════════════════════════════════════ */
  .sidebar {
    display: flex;
    flex-direction: column;
    min-height: 0;
    padding: 32px 24px 24px;
    border-right: 1px solid rgba(88, 91, 112, 0.08);
    overflow-y: auto;
    gap: 0;
  }

  .brand-block {
    display: flex;
    flex-direction: column;
    gap: 10px;
    padding-bottom: 20px;
  }

  .brand {
    margin: 0;
    font-size: 52px;
    font-weight: 900;
    letter-spacing: -0.04em;
    line-height: 1;
    background: linear-gradient(135deg, #89b4fa 0%, #74c7ec 50%, #94e2d5 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
    filter: drop-shadow(0 0 30px rgba(137, 180, 250, 0.2));
  }

  .tagline {
    margin: 0;
    font-size: 15px;
    font-weight: 600;
    color: #cdd6f4;
    line-height: 1.4;
    letter-spacing: -0.01em;
  }

  .subtitle {
    margin: 0;
    font-size: 12px;
    color: #6c7086;
    line-height: 1.5;
  }

  .pills {
    display: flex;
    gap: 6px;
    margin: 2px 0;
  }
  .pill {
    padding: 3px 10px;
    border-radius: 20px;
    font-size: 9px;
    font-weight: 800;
    letter-spacing: 0.1em;
    border: 1px solid rgba(137, 180, 250, 0.2);
    color: #89b4fa;
    background: rgba(137, 180, 250, 0.06);
  }
  .pill-jit {
    border-color: rgba(250, 179, 135, 0.2);
    color: #fab387;
    background: rgba(250, 179, 135, 0.06);
  }
  .pill-wasm {
    border-color: rgba(166, 227, 161, 0.2);
    color: #a6e3a1;
    background: rgba(166, 227, 161, 0.06);
  }

  .actions {
    display: flex;
    gap: 8px;
    margin-top: 4px;
  }

  button { font: inherit; }

  .btn-primary {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    border: none;
    border-radius: 10px;
    padding: 9px 16px;
    background: linear-gradient(135deg, #89b4fa, #74c7ec);
    color: #06060c;
    font-size: 12px;
    font-weight: 700;
    cursor: pointer;
    transition: filter 0.2s, transform 0.2s, box-shadow 0.2s;
    box-shadow: 0 0 0 rgba(137, 180, 250, 0);
  }
  .btn-primary:hover {
    filter: brightness(1.1);
    transform: translateY(-1px);
    box-shadow: 0 6px 20px rgba(137, 180, 250, 0.2);
  }
  .btn-icon {
    width: 13px; height: 13px;
    stroke: currentColor; fill: none;
    stroke-width: 2.2; stroke-linecap: round;
  }

  .btn-ghost {
    display: inline-flex;
    align-items: center;
    gap: 5px;
    border: 1px solid rgba(88, 91, 112, 0.3);
    border-radius: 10px;
    padding: 9px 14px;
    background: transparent;
    color: #7f849c;
    font-size: 12px;
    font-weight: 600;
    cursor: pointer;
    transition: border-color 0.2s, color 0.2s;
  }
  .btn-ghost:hover {
    border-color: rgba(137, 180, 250, 0.4);
    color: #89b4fa;
  }
  .btn-arrow {
    width: 12px; height: 12px;
    stroke: currentColor; fill: none;
    stroke-width: 2; stroke-linecap: round; stroke-linejoin: round;
  }
  .btn-arrow.small { width: 11px; height: 11px; }

  /* ── Quick try (example links) ── */
  .quick-try {
    display: flex;
    flex-direction: column;
    gap: 4px;
    border-top: 1px solid rgba(88, 91, 112, 0.08);
    padding-top: 16px;
  }

  .quick-try-label {
    font-size: 9px;
    font-weight: 800;
    letter-spacing: 0.12em;
    text-transform: uppercase;
    color: #89b4fa;
    padding: 0 0 4px;
  }
  .quick-try-label-gui {
    color: #a6e3a1;
    padding-top: 8px;
  }

  .ex-list {
    display: flex;
    flex-direction: column;
  }

  .ex-link {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 5px 8px;
    margin: 0 -8px;
    border: none;
    border-radius: 6px;
    background: none;
    cursor: pointer;
    font: inherit;
    color: inherit;
    text-align: left;
    transition: background 0.15s;
  }
  .ex-link:hover {
    background: rgba(137, 180, 250, 0.06);
  }
  .ex-link-gui:hover {
    background: rgba(166, 227, 161, 0.06);
  }
  .ex-link-name {
    font-size: 12px;
    font-weight: 500;
    color: #bac2de;
  }
  .ex-link:hover .ex-link-name {
    color: #cdd6f4;
  }
  .ex-link-dot {
    color: #313244;
    font-size: 10px;
  }
  .ex-link-desc {
    font-size: 11px;
    color: #45475a;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  /* ══════════════════════════════════════════
     RIGHT CONTENT
     ══════════════════════════════════════════ */
  .content {
    display: flex;
    flex-direction: column;
    min-height: 0;
    overflow-y: auto;
    padding: 28px 28px 24px;
    gap: 20px;
  }

  /* ── Code window ── */
  .code-area {
    position: relative;
    flex-shrink: 0;
  }
  .code-glow {
    position: absolute;
    inset: -1px;
    border-radius: 16px;
    background: linear-gradient(135deg, rgba(137, 180, 250, 0.12), rgba(116, 199, 236, 0.08), rgba(148, 226, 213, 0.06));
    filter: blur(18px);
    z-index: 0;
    animation: glow-pulse 4s ease-in-out infinite alternate;
  }
  @keyframes glow-pulse {
    0% { opacity: 0.5; }
    100% { opacity: 1; }
  }

  .code-window {
    position: relative;
    z-index: 1;
    display: flex;
    flex-direction: column;
    border-radius: 14px;
    border: 1px solid rgba(88, 91, 112, 0.2);
    background: rgba(11, 11, 18, 0.85);
    backdrop-filter: blur(16px);
    overflow: hidden;
  }
  .code-titlebar {
    display: flex;
    align-items: center;
    gap: 7px;
    padding: 10px 14px;
    border-bottom: 1px solid rgba(88, 91, 112, 0.1);
  }
  .dot { width: 8px; height: 8px; border-radius: 50%; }
  .dot-r { background: #f38ba8; opacity: 0.5; }
  .dot-y { background: #f9e2af; opacity: 0.5; }
  .dot-g { background: #a6e3a1; opacity: 0.5; }
  .code-filename {
    margin-left: 8px;
    font-size: 11px;
    color: #585b70;
    font-weight: 500;
    letter-spacing: 0.02em;
  }
  .code-body {
    margin: 0;
    padding: 16px 18px;
    font-family: 'SF Mono', 'Fira Code', 'Cascadia Code', 'JetBrains Mono', monospace;
    font-size: 12.5px;
    line-height: 1.7;
    color: #a6adc8;
    tab-size: 4;
  }
  .code-body code { font: inherit; color: inherit; }

  .hl-kw  { color: #cba6f7; }
  .hl-fn  { color: #89b4fa; }
  .hl-type { color: #89dceb; }
  .hl-op  { color: #f38ba8; font-weight: 700; }
  .hl-str { color: #a6e3a1; }
  .hl-num { color: #fab387; }
  .hl-lit { color: #f9e2af; }

  .code-try {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 5px;
    padding: 9px;
    border: none;
    border-top: 1px solid rgba(88, 91, 112, 0.1);
    background: rgba(137, 180, 250, 0.02);
    color: #585b70;
    font-size: 11px;
    font-weight: 600;
    cursor: pointer;
    transition: background 0.2s, color 0.2s;
  }
  .code-try:hover {
    background: rgba(137, 180, 250, 0.07);
    color: #89b4fa;
  }

  /* ══════════════════════════════════════════
     WORKSPACE (always visible)
     ══════════════════════════════════════════ */
  .workspace {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 16px;
    min-height: 0;
  }

  /* ── GitHub CTA ── */
  .gh-block {
    display: flex;
    flex-direction: column;
    gap: 6px;
    flex-shrink: 0;
  }
  .gh-warn {
    margin: 0;
    padding: 0 4px;
    font-size: 10px;
    color: #f9e2af;
    opacity: 0.6;
    line-height: 1.4;
  }
  .gh-cta {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    padding: 14px 16px;
    border: 1px solid rgba(88, 91, 112, 0.15);
    border-radius: 12px;
    background: linear-gradient(135deg, rgba(137, 180, 250, 0.04) 0%, rgba(116, 199, 236, 0.02) 100%);
    cursor: pointer;
    font: inherit;
    color: inherit;
    text-align: left;
    flex-shrink: 0;
    transition: border-color 0.2s, background 0.2s;
  }
  .gh-cta:hover {
    border-color: rgba(137, 180, 250, 0.25);
    background: linear-gradient(135deg, rgba(137, 180, 250, 0.07) 0%, rgba(116, 199, 236, 0.04) 100%);
  }
  .gh-cta-left {
    display: flex;
    align-items: center;
    gap: 12px;
    min-width: 0;
  }
  .gh-icon {
    width: 20px;
    height: 20px;
    fill: #7f849c;
    flex-shrink: 0;
    transition: fill 0.2s;
  }
  .gh-cta:hover .gh-icon { fill: #cdd6f4; }
  .gh-cta-text {
    display: flex;
    flex-direction: column;
    gap: 2px;
    min-width: 0;
  }
  .gh-cta-title {
    font-size: 13px;
    font-weight: 600;
    color: #cdd6f4;
  }
  .gh-cta-desc {
    font-size: 11px;
    color: #585b70;
    line-height: 1.35;
  }
  .gh-cta .btn-arrow {
    stroke: #45475a;
    flex-shrink: 0;
    transition: stroke 0.2s;
  }
  .gh-cta:hover .btn-arrow { stroke: #89b4fa; }

  /* ── Empty workspace ── */
  .empty-workspace {
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 24px 16px;
  }
  .empty-workspace-text {
    font-size: 12px;
    color: #45475a;
    text-align: center;
  }
  .empty-workspace-text strong {
    color: #585b70;
  }

  /* ── Projects ── */
  .projects-area {
    flex: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
  }
  .projects-header {
    display: flex;
    align-items: center;
    gap: 10px;
    padding: 0 0 10px;
    flex-shrink: 0;
  }
  .projects-title {
    font-size: 10px;
    font-weight: 800;
    letter-spacing: 0.12em;
    text-transform: uppercase;
    color: #585b70;
    flex-shrink: 0;
  }
  .projects-search {
    display: flex;
    align-items: center;
    gap: 6px;
    flex: 1;
    max-width: 200px;
    padding: 5px 10px;
    border-radius: 8px;
    border: 1px solid rgba(88, 91, 112, 0.15);
    background: rgba(11, 11, 18, 0.5);
    transition: border-color 0.2s;
  }
  .projects-search:focus-within {
    border-color: rgba(137, 180, 250, 0.3);
  }
  .search-icon {
    width: 12px; height: 12px;
    stroke: #45475a; fill: none;
    stroke-width: 1.6; stroke-linecap: round;
    flex-shrink: 0;
  }
  .projects-search input {
    flex: 1; min-width: 0;
    border: none; background: transparent;
    color: #cdd6f4; font-size: 12px;
    outline: none; padding: 0;
  }
  .projects-search input::placeholder { color: #45475a; }
  .projects-header-spacer { flex: 1; }
  .sync-hint { color: #585b70; font-size: 11px; white-space: nowrap; }

  .secondary {
    border: 1px solid rgba(88, 91, 112, 0.2);
    border-radius: 8px;
    padding: 6px 10px;
    background: rgba(11, 11, 18, 0.5);
    color: #a6adc8; font-size: 12px;
    cursor: pointer; white-space: nowrap;
  }
  .secondary:hover { border-color: rgba(88, 91, 112, 0.4); color: #cdd6f4; }

  .icon-btn {
    border: 1px solid rgba(88, 91, 112, 0.15);
    border-radius: 8px;
    width: 28px; height: 28px;
    display: flex; align-items: center; justify-content: center;
    background: rgba(11, 11, 18, 0.5);
    color: #6c7086; font-size: 14px;
    cursor: pointer; flex-shrink: 0;
    transition: border-color 0.2s, color 0.2s;
  }
  .icon-btn:hover:not(:disabled) { border-color: rgba(88, 91, 112, 0.4); color: #cdd6f4; }
  .icon-btn:disabled { opacity: 0.35; cursor: default; }
  .refresh-icon { display: inline-block; }
  .refresh-icon.spinning { animation: spin 0.8s linear infinite; }
  @keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }

  .error-bar {
    padding: 8px 12px;
    border-radius: 8px;
    border: 1px solid rgba(243, 139, 168, 0.12);
    background: rgba(243, 139, 168, 0.04);
    color: #f38ba8; font-size: 12px;
    flex-shrink: 0;
    display: flex; align-items: center; justify-content: space-between; gap: 10px;
  }
  .error-dismiss {
    border: none; background: none; color: #f38ba8;
    font-size: 16px; cursor: pointer; padding: 0 2px;
    opacity: 0.5; flex-shrink: 0;
  }
  .error-dismiss:hover { opacity: 1; }

  .grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
    gap: 10px;
  }

  /* ── Confirm dialog ── */
  .confirm-backdrop {
    position: fixed; inset: 0;
    background: rgba(0, 0, 0, 0.78);
    backdrop-filter: blur(8px);
    display: flex; align-items: center; justify-content: center;
    z-index: 210;
  }
  .confirm-modal {
    width: min(100%, 380px);
    background: #13131e;
    border: 1px solid rgba(88, 91, 112, 0.3);
    border-radius: 16px;
    padding: 22px;
    display: flex; flex-direction: column; gap: 12px;
  }
  .confirm-modal h3 { margin: 0; color: #cdd6f4; font-size: 16px; font-weight: 700; }
  .confirm-modal h3.danger { color: #f38ba8; }
  .confirm-msg { margin: 0; color: #7f849c; font-size: 13px; line-height: 1.6; }
  .confirm-actions { display: flex; justify-content: flex-end; gap: 8px; margin-top: 4px; }
  .danger-btn {
    border: none; border-radius: 8px;
    padding: 8px 14px;
    background: #f38ba8; color: #06060c;
    font-weight: 700; font-size: 13px;
    cursor: pointer; white-space: nowrap;
  }
  .danger-btn:hover { filter: brightness(1.1); }

  .progress-track { height: 2px; background: rgba(30, 30, 46, 0.3); overflow: hidden; flex-shrink: 0; }
  .progress-bar {
    height: 100%; width: 30%;
    background: linear-gradient(90deg, #89b4fa, #74c7ec);
    border-radius: 1px;
    animation: progress-slide 1.2s ease-in-out infinite;
  }
  @keyframes progress-slide { 0% { transform: translateX(-100%); } 100% { transform: translateX(430%); } }

  .skeleton-card {
    border: 1px solid rgba(88, 91, 112, 0.1);
    background: rgba(11, 11, 18, 0.5);
    border-radius: 12px; padding: 12px;
  }
  .skeleton-row { display: flex; align-items: center; gap: 10px; }
  .skeleton-icon {
    width: 32px; height: 24px; border-radius: 5px;
    background: rgba(88, 91, 112, 0.1);
    flex-shrink: 0; animation: pulse 1.4s ease-in-out infinite;
  }
  .skeleton-info { flex: 1; min-width: 0; display: flex; flex-direction: column; gap: 5px; }
  .skeleton-line { border-radius: 3px; background: rgba(88, 91, 112, 0.1); animation: pulse 1.4s ease-in-out infinite; }
  .skeleton-name { width: 60%; height: 13px; }
  .skeleton-meta { width: 40%; height: 10px; animation-delay: 0.15s; }
  @keyframes pulse { 0%, 100% { opacity: 0.3; } 50% { opacity: 0.8; } }

  .empty {
    display: flex; flex-direction: column; align-items: center; justify-content: center;
    gap: 8px; padding: 40px 20px; text-align: center;
  }
  .empty-title { color: #585b70; font-size: 13px; font-weight: 600; }
  .empty-sub { color: #45475a; font-size: 12px; max-width: 300px; line-height: 1.5; }

  /* ── Responsive ── */
  @media (max-width: 700px) {
    .dashboard { grid-template-columns: 1fr; }
    .sidebar {
      border-right: none;
      border-bottom: 1px solid rgba(88, 91, 112, 0.08);
      padding: 20px 16px 16px;
      overflow-y: visible;
    }
    .quick-try { display: none; }
    .content { padding: 16px; }
  }
</style>
