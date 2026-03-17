<script lang="ts">
  import { tick } from 'svelte';
  import type { BootstrapContext } from '../lib/types';
  import type { ProjectCatalogService } from '../lib/services/project_catalog_service';
  import type { ManagedProject, ProjectDiffResult } from '../lib/project_catalog/types';
  import { projectKey, syncState } from '../lib/project_catalog/types';
  import { formatError } from '../lib/format_error';
  import CreateProjectModal from './home/CreateProjectModal.svelte';
  import ProjectActionsMenu from './home/ProjectActionsMenu.svelte';
  import ProjectCard from './home/ProjectCard.svelte';
  import ProjectRenameModal from './home/ProjectRenameModal.svelte';
  import ProjectSyncModal from './home/ProjectSyncModal.svelte';

  export let bootstrap: BootstrapContext | null = null;
  export let projectCatalog: ProjectCatalogService;
  export let localPathInput = '';
  export let urlInput = '';
  export let onOpenWorkspace: () => void = () => {};
  export let onOpenLocalPath: () => void = () => {};
  export let onOpenUrl: () => void = () => {};
  export let onOpenProject: (project: ManagedProject) => Promise<void> | void = () => {};
  export let onDevelop: () => void = () => {};

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
  let showAdvanced = false;
  let localRefreshRequested = false;

  $: githubStore = projectCatalog.github;
  $: catalogStore = projectCatalog.catalog;
  $: filteredProjects = $catalogStore.projects.filter((project) =>
    !searchQuery || project.name.toLowerCase().includes(searchQuery.toLowerCase()),
  );
  $: isInitialLoading = $catalogStore.loading;
  $: isRefreshing = localRefreshRequested || $catalogStore.refreshing || $catalogStore.remoteLoading;

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
  <!-- ── Header bar ── -->
  <div class="header">
    <div class="brand">
      <span class="logo-vo">Vo</span><span class="logo-studio">Studio</span>
    </div>
    <div class="search">
      <input bind:value={searchQuery} placeholder="Search projects…" />
    </div>
  </div>

  <!-- ── Action row ── -->
  <div class="actions">
    <button class="primary" on:click={() => { createError = ''; showCreateModal = true; }}>+ New Project</button>
    <button
      class="icon-btn"
      title="Refresh projects"
      disabled={isInitialLoading}
      on:click={triggerRefresh}
    ><span class="refresh-icon" class:spinning={isInitialLoading || isRefreshing}>↻</span></button>
    {#if $catalogStore.remoteLoading}
      <span class="sync-hint">Syncing GitHub…</span>
    {/if}
    <div class="actions-spacer"></div>
    <button class="text-btn" on:click={() => (showAdvanced = !showAdvanced)}>{showAdvanced ? 'Less ▴' : 'More ▾'}</button>
  </div>

  {#if showAdvanced}
    <div class="advanced">
      {#if bootstrap?.platform === 'native'}
        <div class="adv-row">
          <button class="secondary" on:click={onOpenWorkspace}>Browse Folder…</button>
          <span class="adv-hint">Pick a folder from your filesystem to open as a project</span>
        </div>
        <div class="adv-row">
          <div class="inline-open">
            <input bind:value={localPathInput} placeholder="/absolute/path/to/project" on:keydown={(event) => event.key === 'Enter' && onOpenLocalPath()} />
            <button class="secondary" on:click={onOpenLocalPath}>Open Path</button>
          </div>
          <span class="adv-hint">Open a project by its absolute filesystem path</span>
        </div>
      {/if}
      <div class="adv-row">
        <div class="inline-open">
          <input bind:value={urlInput} placeholder="https://github.com/user/repo" on:keydown={(event) => event.key === 'Enter' && onOpenUrl()} />
          <button class="secondary" on:click={onOpenUrl}>Clone URL</button>
        </div>
        <span class="adv-hint">Download a project from a GitHub URL or tarball link</span>
      </div>
    </div>
  {/if}

  <!-- ── Progress bar ── -->
  {#if isInitialLoading || isRefreshing}
    <div class="progress-track"><div class="progress-bar"></div></div>
  {/if}

  <!-- ── Error / status ── -->
  {#if actionError || $catalogStore.error || $githubStore.error}
    <div class="error-bar">
      <span>{actionError || $catalogStore.error || $githubStore.error}</span>
      <button class="error-dismiss" on:click={() => (actionError = '')}>×</button>
    </div>
  {/if}

  <!-- ── Project grid ── -->
  {#if $catalogStore.loading}
    <div class="grid">
      {#each Array(6) as _}
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
  {:else if filteredProjects.length === 0}
    <div class="empty">
      <div class="empty-title">{searchQuery ? 'No matching projects' : 'No projects yet'}</div>
      <div class="empty-sub">Create a new project or connect GitHub to see remote projects.</div>
      <div class="empty-actions">
        <button class="primary" on:click={() => (showCreateModal = true)}>+ New Project</button>
      </div>
    </div>
  {:else}
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
  {/if}

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
  .home {
    flex: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
    overflow: auto;
    background: #11111b;
  }

  /* ── Header ── */
  .header {
    display: flex;
    align-items: center;
    gap: 16px;
    padding: 12px 20px;
    border-bottom: 1px solid #1e1e2e;
    flex-shrink: 0;
  }
  .brand {
    display: flex;
    align-items: baseline;
    gap: 4px;
    flex-shrink: 0;
  }
  .logo-vo {
    font-size: 20px;
    font-weight: 900;
    color: #89b4fa;
  }
  .logo-studio {
    font-size: 14px;
    font-weight: 400;
    color: #585b70;
  }
  .search {
    flex: 1;
    min-width: 0;
  }
  .search input {
    width: 100%;
    box-sizing: border-box;
    padding: 8px 12px;
    border-radius: 8px;
    border: 1px solid #252535;
    background: #181825;
    color: #cdd6f4;
    font-size: 13px;
    outline: none;
  }
  .search input:focus {
    border-color: #45475a;
  }
  .search input::placeholder {
    color: #45475a;
  }
  .text-btn {
    border: none;
    background: none;
    color: #585b70;
    font: inherit;
    font-size: 11px;
    cursor: pointer;
    padding: 2px 4px;
  }
  .text-btn:hover {
    color: #a6adc8;
  }

  /* ── Action row ── */
  .actions {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 10px 20px;
    border-bottom: 1px solid #1e1e2e;
    flex-shrink: 0;
  }
  .actions-spacer {
    flex: 1;
  }
  .sync-hint {
    color: #585b70;
    font-size: 11px;
    white-space: nowrap;
  }

  /* ── Advanced section ── */
  .advanced {
    display: flex;
    flex-direction: column;
    gap: 10px;
    padding: 12px 20px;
    border-bottom: 1px solid #1e1e2e;
    background: rgba(24, 24, 37, 0.5);
    flex-shrink: 0;
  }
  .adv-row {
    display: flex;
    align-items: center;
    gap: 12px;
  }
  .adv-hint {
    color: #45475a;
    font-size: 11px;
  }
  .inline-open {
    display: flex;
    gap: 4px;
  }
  .inline-open input {
    width: 220px;
    box-sizing: border-box;
    padding: 7px 10px;
    border-radius: 8px;
    border: 1px solid #252535;
    background: #181825;
    color: #cdd6f4;
    font-size: 12px;
    outline: none;
  }
  .inline-open input:focus {
    border-color: #45475a;
  }
  .inline-open input::placeholder {
    color: #45475a;
  }

  /* ── Buttons ── */
  button {
    font: inherit;
  }
  .primary {
    border: none;
    border-radius: 8px;
    padding: 7px 14px;
    background: linear-gradient(135deg, #89b4fa, #74c7ec);
    color: #11111b;
    font-weight: 700;
    font-size: 13px;
    cursor: pointer;
    white-space: nowrap;
  }
  .primary:hover {
    filter: brightness(1.1);
  }
  .secondary {
    border: 1px solid #252535;
    border-radius: 8px;
    padding: 7px 12px;
    background: #1e1e2e;
    color: #a6adc8;
    font-size: 12px;
    cursor: pointer;
    white-space: nowrap;
  }
  .secondary:hover {
    border-color: #45475a;
    color: #cdd6f4;
  }
  .icon-btn {
    border: 1px solid #252535;
    border-radius: 8px;
    width: 32px;
    height: 32px;
    display: flex;
    align-items: center;
    justify-content: center;
    background: #1e1e2e;
    color: #a6adc8;
    font-size: 16px;
    cursor: pointer;
    flex-shrink: 0;
  }
  .icon-btn:hover:not(:disabled) {
    border-color: #45475a;
    color: #cdd6f4;
  }
  .icon-btn:disabled {
    opacity: 0.5;
    cursor: default;
  }
  .refresh-icon {
    display: inline-block;
  }
  .refresh-icon.spinning {
    animation: spin 0.8s linear infinite;
  }
  @keyframes spin {
    from { transform: rotate(0deg); }
    to { transform: rotate(360deg); }
  }

  /* ── Error bar ── */
  .error-bar {
    margin: 0 20px;
    padding: 10px 14px;
    border-radius: 8px;
    border: 1px solid rgba(243, 139, 168, 0.2);
    background: rgba(243, 139, 168, 0.06);
    color: #f38ba8;
    font-size: 12px;
    flex-shrink: 0;
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 10px;
  }
  .error-dismiss {
    border: none;
    background: none;
    color: #f38ba8;
    font-size: 16px;
    cursor: pointer;
    padding: 0 2px;
    opacity: 0.6;
    flex-shrink: 0;
  }
  .error-dismiss:hover {
    opacity: 1;
  }

  /* ── Confirm dialog ── */
  .confirm-backdrop {
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.72);
    backdrop-filter: blur(4px);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 210;
  }
  .confirm-modal {
    width: min(100%, 380px);
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 14px;
    padding: 20px;
    display: flex;
    flex-direction: column;
    gap: 12px;
  }
  .confirm-modal h3 {
    margin: 0;
    color: #cdd6f4;
    font-size: 16px;
    font-weight: 700;
  }
  .confirm-modal h3.danger {
    color: #f38ba8;
  }
  .confirm-msg {
    margin: 0;
    color: #7f849c;
    font-size: 13px;
    line-height: 1.6;
  }
  .confirm-actions {
    display: flex;
    justify-content: flex-end;
    gap: 8px;
    margin-top: 4px;
  }
  .danger-btn {
    border: none;
    border-radius: 8px;
    padding: 7px 14px;
    background: #f38ba8;
    color: #11111b;
    font-weight: 700;
    font-size: 13px;
    cursor: pointer;
    white-space: nowrap;
  }
  .danger-btn:hover {
    filter: brightness(1.1);
  }

  /* ── Progress bar ── */
  .progress-track {
    height: 2px;
    background: #1e1e2e;
    overflow: hidden;
    flex-shrink: 0;
  }
  .progress-bar {
    height: 100%;
    width: 30%;
    background: linear-gradient(90deg, #89b4fa, #74c7ec);
    border-radius: 1px;
    animation: progress-slide 1.2s ease-in-out infinite;
  }
  @keyframes progress-slide {
    0% { transform: translateX(-100%); }
    100% { transform: translateX(430%); }
  }

  /* ── Project grid ── */
  .grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
    gap: 12px;
    padding: 16px 20px;
  }

  /* ── Skeleton cards ── */
  .skeleton-card {
    border: 1px solid #252535;
    background: #181825;
    border-radius: 10px;
    padding: 14px;
  }
  .skeleton-row {
    display: flex;
    align-items: center;
    gap: 12px;
  }
  .skeleton-icon {
    width: 36px;
    height: 28px;
    border-radius: 6px;
    background: #252535;
    flex-shrink: 0;
    animation: skeleton-pulse 1.4s ease-in-out infinite;
  }
  .skeleton-info {
    flex: 1;
    min-width: 0;
    display: flex;
    flex-direction: column;
    gap: 6px;
  }
  .skeleton-line {
    border-radius: 4px;
    background: #252535;
    animation: skeleton-pulse 1.4s ease-in-out infinite;
  }
  .skeleton-name {
    width: 60%;
    height: 14px;
  }
  .skeleton-meta {
    width: 40%;
    height: 11px;
    animation-delay: 0.15s;
  }
  @keyframes skeleton-pulse {
    0%, 100% { opacity: 0.4; }
    50% { opacity: 1; }
  }

  /* ── Empty state ── */
  .empty {
    flex: 1;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 10px;
    padding: 60px 20px;
    text-align: center;
  }
  .empty-title {
    color: #585b70;
    font-size: 16px;
    font-weight: 600;
  }
  .empty-sub {
    color: #45475a;
    font-size: 13px;
    max-width: 400px;
    line-height: 1.6;
  }
  .empty-actions {
    display: flex;
    gap: 10px;
    margin-top: 10px;
  }
</style>
