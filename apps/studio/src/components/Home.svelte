<script lang="ts">
  import { tick } from 'svelte';
  import type { Backend, FileDialogFilter } from '../lib/backend/backend';
  import { formatError } from '../lib/format_error';
  import type { ManagedProject, ProjectDiffResult } from '../lib/project_catalog/types';
  import { projectKey, syncState } from '../lib/project_catalog/types';
  import type { ProjectCatalogService } from '../lib/services/project_catalog_service';
  import BlockKartShowcase from './home/BlockKartShowcase.svelte';
  import CreateProjectModal from './home/CreateProjectModal.svelte';
  import OpenProjectModal from './home/OpenProjectModal.svelte';
  import ProjectActionsMenu from './home/ProjectActionsMenu.svelte';
  import ProjectCard from './home/ProjectCard.svelte';
  import ProjectRenameModal from './home/ProjectRenameModal.svelte';
  import ProjectSyncModal from './home/ProjectSyncModal.svelte';
  import StarterGallery from './home/StarterGallery.svelte';
  import WelcomeHero from './home/WelcomeHero.svelte';
  import {
    BLOCKKART_GITHUB_URL,
    BLOCKKART_PLAY_URL,
    STARTER_EXAMPLES,
    type StarterExample,
  } from './home/content';

  export let projectCatalog: ProjectCatalogService;
  export let backend: Backend;
  export let platform: 'native' | 'wasm' = 'wasm';
  export let onOpenProject: (project: ManagedProject) => Promise<void> | void = () => {};
  export let onOpenLocalPath: (path: string) => Promise<void> | void = () => {};
  export let onOpenFeaturedProject: (url: string, hasGui: boolean) => Promise<void> | void = () => {};
  export let onPlayFeaturedProject: (url: string, hasGui: boolean) => Promise<void> | void = () => {};
  export let onOpenExample: (example: StarterExample) => Promise<void> | void = () => {};
  export let onDocs: () => void = () => {};
  export let onConnectGitHub: () => void = () => {};

  const LAST_CREATE_LOCATION_KEY = 'vo_studio_last_create_location';
  const VO_FILE_FILTERS: FileDialogFilter[] = [{ name: 'Vo source', extensions: ['vo'] }];
  const firstStarter = STARTER_EXAMPLES.find((example) => example.id === 'channels') ?? STARTER_EXAMPLES[0];

  interface ProjectMenuState {
    x: number;
    y: number;
    project: ManagedProject;
    trigger: HTMLButtonElement;
  }

  interface ConfirmDialogState {
    title: string;
    message: string;
    danger: boolean;
    action: () => void;
  }

  let searchQuery = '';
  let actionError = '';
  let starterError = '';
  let openingStarterId: string | null = null;
  let blockKartBusyAction: 'play' | 'source' | null = null;
  let blockKartError = '';
  let showCreateModal = false;
  let showOpenProjectModal = false;
  let menuState: ProjectMenuState | null = null;
  let renameTarget: ManagedProject | null = null;
  let syncDialog: ProjectDiffResult | null = null;
  let syncDialogProjectKey = '';
  let confirmDialog: ConfirmDialogState | null = null;
  let lastProjectMenuTrigger: HTMLButtonElement | null = null;
  let confirmReturnFocus: HTMLElement | null = null;
  let confirmModalElement: HTMLDivElement | null = null;
  let confirmCancelButton: HTMLButtonElement | null = null;
  let projectsSection: HTMLElement | null = null;
  let creating = false;
  let openingProject = false;
  let createError = '';
  let openProjectError = '';
  let localRefreshRequested = false;
  let lastCreateLocation = '';

  $: isNative = platform === 'native';
  $: githubStore = projectCatalog.github;
  $: catalogStore = projectCatalog.catalog;
  $: filteredProjects = $catalogStore.projects.filter((project) =>
    !searchQuery || project.name.toLowerCase().includes(searchQuery.trim().toLowerCase()),
  );
  $: isInitialLoading = $catalogStore.loading;
  $: isRefreshing = localRefreshRequested || $catalogStore.refreshing || $catalogStore.remoteLoading;
  $: hasProjects = !$catalogStore.loading && $catalogStore.projects.length > 0;
  $: isGitHubConnected = Boolean($githubStore.user);
  $: createLocationDefault = lastCreateLocation || $catalogStore.root;
  $: openLocationDefault = createLocationDefault;

  lastCreateLocation = localStorage.getItem(LAST_CREATE_LOCATION_KEY) ?? '';

  function busy(project: ManagedProject): boolean {
    return $catalogStore.busyKeys.includes(projectKey(project));
  }

  function checkingSync(project: ManagedProject): boolean {
    return $catalogStore.checkingSyncKeys.includes(projectKey(project));
  }

  async function restoreFocus(target: HTMLElement | null): Promise<void> {
    await tick();
    if (target?.isConnected) {
      target.focus();
      return;
    }
    projectsSection?.focus();
  }

  function openProjectMenu(project: ManagedProject, detail: { x: number; y: number; trigger: HTMLButtonElement }): void {
    lastProjectMenuTrigger = detail.trigger;
    menuState = { ...detail, project };
  }

  function closeProjectMenu(): void {
    if (!menuState) return;
    const target = menuState.trigger;
    menuState = null;
    void restoreFocus(target);
  }

  function openConfirmDialog(dialog: ConfirmDialogState): void {
    const activeElement = document.activeElement instanceof HTMLElement ? document.activeElement : null;
    confirmReturnFocus = lastProjectMenuTrigger?.isConnected ? lastProjectMenuTrigger : activeElement;
    confirmDialog = dialog;
    void tick().then(() => confirmCancelButton?.focus());
  }

  function closeConfirmDialog(): void {
    if (!confirmDialog) return;
    const target = confirmReturnFocus;
    confirmDialog = null;
    confirmReturnFocus = null;
    void restoreFocus(target);
  }

  function trapConfirmFocus(event: KeyboardEvent): void {
    if (event.key !== 'Tab' || !confirmModalElement) return;
    const focusable = Array.from(
      confirmModalElement.querySelectorAll<HTMLElement>(
        'button:not(:disabled), a[href], input:not(:disabled), select:not(:disabled), textarea:not(:disabled), [tabindex]:not([tabindex="-1"])',
      ),
    );
    if (focusable.length === 0) {
      event.preventDefault();
      confirmModalElement.focus();
      return;
    }
    const first = focusable[0];
    const last = focusable[focusable.length - 1];
    const active = document.activeElement;
    if (event.shiftKey && (active === first || !confirmModalElement.contains(active))) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && (active === last || !confirmModalElement.contains(active))) {
      event.preventDefault();
      first.focus();
    }
  }

  function waitForPaint(): Promise<void> {
    return tick().then(() => new Promise((resolve) => requestAnimationFrame(() => setTimeout(resolve, 0))));
  }

  async function openStarter(example: StarterExample | undefined): Promise<void> {
    if (!example || openingStarterId !== null) return;
    starterError = '';
    openingStarterId = example.id;
    try {
      await onOpenExample(example);
    } catch (error) {
      starterError = formatError(error);
    } finally {
      openingStarterId = null;
    }
  }

  async function quickPlayBlockKart(): Promise<void> {
    if (blockKartBusyAction) return;
    blockKartError = '';
    blockKartBusyAction = 'play';
    try {
      await onPlayFeaturedProject(BLOCKKART_PLAY_URL, true);
    } catch (error) {
      blockKartError = formatError(error);
    } finally {
      blockKartBusyAction = null;
    }
  }

  async function openBlockKartSource(): Promise<void> {
    if (blockKartBusyAction) return;
    blockKartError = '';
    blockKartBusyAction = 'source';
    try {
      await onOpenFeaturedProject(BLOCKKART_GITHUB_URL, true);
    } catch (error) {
      blockKartError = formatError(error);
    } finally {
      blockKartBusyAction = null;
    }
  }

  async function triggerRefresh(): Promise<void> {
    if (isInitialLoading || isRefreshing) return;
    localRefreshRequested = true;
    await waitForPaint();
    void projectCatalog.refresh().finally(() => {
      localRefreshRequested = false;
    });
  }

  async function createProject(
    kind: 'single' | 'module',
    name: string,
    modulePath: string | undefined,
    location: string | undefined,
  ): Promise<void> {
    createError = '';
    creating = true;
    try {
      if (location) {
        lastCreateLocation = location;
        localStorage.setItem(LAST_CREATE_LOCATION_KEY, location);
      }
      const project = kind === 'single'
        ? await projectCatalog.createSingleProject(name, location)
        : await projectCatalog.createModuleProject(name, modulePath ?? '', location);
      showCreateModal = false;
      await openProject(project);
    } catch (error) {
      createError = formatError(error);
    } finally {
      creating = false;
    }
  }

  async function openProjectFromDirectory(): Promise<void> {
    await openPickedProject(() => backend.pickDirectory(openLocationDefault || undefined));
  }

  async function openProjectFromFile(): Promise<void> {
    await openPickedProject(() => backend.pickFile(openLocationDefault || undefined, VO_FILE_FILTERS));
  }

  async function openPickedProject(pick: () => Promise<string | null>): Promise<void> {
    openProjectError = '';
    openingProject = true;
    try {
      const picked = await pick();
      if (!picked) return;
      await onOpenLocalPath(picked);
      showOpenProjectModal = false;
    } catch (error) {
      openProjectError = formatError(error);
    } finally {
      openingProject = false;
    }
  }

  function pickDirectoryForCreate(): Promise<string | null> {
    return backend.pickDirectory(createLocationDefault || undefined);
  }

  async function openProject(project: ManagedProject): Promise<void> {
    actionError = '';
    try {
      const ready = await projectCatalog.ensureProjectReady(project);
      await onOpenProject(ready);
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
    actionError = '';
    openConfirmDialog({
      title: 'Delete project',
      message: `Delete “${project.name}” from local storage? This cannot be undone.`,
      danger: true,
      action: () => void doDeleteProject(project),
    });
  }

  function confirmDeleteRemoteProject(project: ManagedProject): void {
    actionError = '';
    openConfirmDialog({
      title: 'Delete cloud copy',
      message: project.localPath
        ? `Delete the cloud copy of “${project.name}”? Your local copy will remain available.`
        : `Delete the cloud copy of “${project.name}”? This permanently removes it from GitHub.`,
      danger: true,
      action: () => void doDeleteRemoteProject(project),
    });
  }

  function confirmDeleteEverywhere(project: ManagedProject): void {
    actionError = '';
    openConfirmDialog({
      title: 'Delete everywhere',
      message: `Delete both local and cloud copies of “${project.name}”? This cannot be undone.`,
      danger: true,
      action: () => void doDeleteEverywhere(project),
    });
  }

  function confirmForgetProject(project: ManagedProject): void {
    actionError = '';
    openConfirmDialog({
      title: 'Remove from list',
      message: `Remove “${project.name}” from this list? The GitHub copy will remain available.`,
      danger: true,
      action: () => void doForgetProject(project),
    });
  }

  async function performConfirmedAction(action: () => Promise<void>): Promise<void> {
    actionError = '';
    const target = confirmReturnFocus;
    confirmDialog = null;
    confirmReturnFocus = null;
    try {
      await action();
    } catch (error) {
      actionError = formatError(error);
    } finally {
      await restoreFocus(target);
    }
  }

  async function doDeleteProject(project: ManagedProject): Promise<void> {
    await performConfirmedAction(() => projectCatalog.deleteProject(project));
  }

  async function doForgetProject(project: ManagedProject): Promise<void> {
    await performConfirmedAction(() => projectCatalog.forgetRemoteProject(project));
  }

  async function doDeleteRemoteProject(project: ManagedProject): Promise<void> {
    await performConfirmedAction(() => projectCatalog.deleteRemoteProject(project));
  }

  async function doDeleteEverywhere(project: ManagedProject): Promise<void> {
    await performConfirmedAction(() => projectCatalog.deleteEverywhere(project));
  }

  function viewRemote(project: ManagedProject): void {
    actionError = '';
    if (!project.remote) return;
    const url = project.remote.kind === 'gist' && project.remote.gistId
      ? `https://gist.github.com/${project.remote.gistId}`
      : project.remote.owner && project.remote.repo
        ? `https://github.com/${project.remote.owner}/${project.remote.repo}`
        : '';
    if (url) window.open(url, '_blank', 'noopener,noreferrer');
  }

  async function shareProject(project: ManagedProject): Promise<void> {
    actionError = '';
    try {
      const share = await projectCatalog.getProjectShareInfo(project, { mode: 'dev' });
      if (!share.shareable || !share.canonicalUrl) throw new Error(share.reason ?? 'Project is not shareable');
      if (!navigator.clipboard?.writeText) throw new Error('Clipboard API is unavailable');
      await navigator.clipboard.writeText(share.canonicalUrl);
    } catch (error) {
      actionError = formatError(error);
    }
  }
</script>

<svelte:window
  on:keydown={(event) => {
    if (confirmDialog) {
      if (event.key === 'Escape') {
        event.preventDefault();
        closeConfirmDialog();
      } else {
        trapConfirmFocus(event);
      }
      return;
    }
    if (event.key !== 'Escape') return;
    if (menuState) {
      closeProjectMenu();
      return;
    }
    renameTarget = null;
    syncDialog = null;
    showCreateModal = false;
    showOpenProjectModal = false;
  }}
/>

<main class="home">
  <WelcomeHero
    busy={openingStarterId !== null}
    onTry={() => void openStarter(firstStarter)}
    onDocs={onDocs}
  />

  {#if starterError}
    <div class="page-error" role="alert">{starterError}</div>
  {/if}

  <BlockKartShowcase
    busy={blockKartBusyAction}
    error={blockKartError || null}
    onPlay={() => void quickPlayBlockKart()}
    onOpen={() => void openBlockKartSource()}
  />
  <StarterGallery
    examples={STARTER_EXAMPLES}
    openingId={openingStarterId}
    onOpen={(example: StarterExample) => void openStarter(example)}
  />

  <section
    bind:this={projectsSection}
    class="projects"
    tabindex="-1"
    aria-labelledby="projects-title"
    aria-busy={isInitialLoading || isRefreshing}
  >
    <div class="projects-inner">
      <div class="projects-heading">
        <div>
          <span class="section-kicker">Your workspace</span>
          <h2 id="projects-title">Projects</h2>
          <p>Create locally, bring in a folder, or connect GitHub when you want cloud sync.</p>
        </div>
        <div class="project-actions">
          {#if isNative}
            <button class="button secondary" on:click={() => { openProjectError = ''; showOpenProjectModal = true; }}>
              <svg viewBox="0 0 18 18" aria-hidden="true"><path d="M3 14h12M4 10l5-5 5 5" /></svg>
              Open
            </button>
          {/if}
          <button class="button primary" on:click={() => { createError = ''; showCreateModal = true; }}>
            <svg viewBox="0 0 18 18" aria-hidden="true"><path d="M9 4v10M4 9h10" /></svg>
            New project
          </button>
        </div>
      </div>

      <div class="project-toolbar">
        {#if hasProjects}
          <label class="project-search">
            <span class="sr-only">Search projects</span>
            <svg viewBox="0 0 18 18" aria-hidden="true"><circle cx="8" cy="8" r="4.5"/><path d="m11.5 11.5 3.5 3.5"/></svg>
            <input bind:value={searchQuery} placeholder="Search projects" />
          </label>
        {:else}
          <span class="project-count">Start with a clean project or one of the examples above.</span>
        {/if}
        <span class="toolbar-spacer"></span>
        {#if $catalogStore.remoteLoading}<span class="sync-hint">Syncing…</span>{/if}
        <button class="icon-button" aria-label="Refresh projects" title="Refresh projects" disabled={isInitialLoading || isRefreshing} on:click={triggerRefresh}>
          <svg class:spinning={isInitialLoading || isRefreshing} viewBox="0 0 18 18" aria-hidden="true"><path d="M14.5 6A6 6 0 1 0 15 10M14.5 3v3h-3"/></svg>
        </button>
        {#if !isGitHubConnected}
          <button class="button ghost" on:click={onConnectGitHub}>
            <svg class="github-icon" viewBox="0 0 24 24" aria-hidden="true"><path d="M12 2a10 10 0 0 0-3.16 19.49c.5.09.68-.22.68-.48v-1.87c-2.78.6-3.37-1.18-3.37-1.18-.45-1.16-1.11-1.47-1.11-1.47-.91-.62.07-.61.07-.61 1 .07 1.53 1.03 1.53 1.03.9 1.53 2.35 1.09 2.92.83.09-.65.35-1.09.64-1.34-2.22-.25-4.56-1.11-4.56-4.94 0-1.09.39-1.98 1.03-2.68-.1-.25-.45-1.27.1-2.64 0 0 .84-.27 2.75 1.02A9.6 9.6 0 0 1 12 6.82c.85 0 1.7.11 2.5.34 1.91-1.29 2.75-1.02 2.75-1.02.55 1.37.2 2.39.1 2.64.64.7 1.03 1.59 1.03 2.68 0 3.84-2.34 4.68-4.57 4.93.36.31.68.92.68 1.86V21c0 .27.18.58.69.48A10 10 0 0 0 12 2Z"/></svg>
            Connect GitHub
          </button>
        {/if}
      </div>

      {#if isInitialLoading || isRefreshing}
        <div class="progress" role="progressbar" aria-label="Loading projects"><span></span></div>
      {/if}

      {#if actionError || $catalogStore.error || $githubStore.error}
        <div class="inline-error" role="alert">
          <span>{actionError || $catalogStore.error || $githubStore.error}</span>
          {#if actionError}
            <button aria-label="Dismiss error" on:click={() => (actionError = '')}>×</button>
          {/if}
        </div>
      {/if}

      {#if $catalogStore.loading}
        <div class="project-grid" aria-label="Loading projects">
          {#each Array(3) as _}<div class="skeleton"></div>{/each}
        </div>
      {:else if filteredProjects.length === 0 && searchQuery}
        <div class="empty"><strong>No matching projects</strong><span>Try a shorter search term.</span></div>
      {:else if hasProjects}
        <div class="project-grid">
          {#each filteredProjects as project (projectKey(project))}
            {@const state = syncState(project)}
            <ProjectCard
              {project}
              {state}
              busy={busy(project)}
              checkingSync={checkingSync(project)}
              menuOpen={menuState !== null && projectKey(menuState.project) === projectKey(project)}
              on:open={() => openProject(project)}
              on:menu={(event) => openProjectMenu(project, event.detail)}
            />
          {/each}
        </div>
      {:else}
        <div class="empty">
          <span class="empty-mark" aria-hidden="true">V</span>
          <strong>Your next idea starts here</strong>
          <span>Create a project, or open any starter and save it when it grows.</span>
          <button class="button primary" on:click={() => (showCreateModal = true)}>Create your first project</button>
        </div>
      {/if}
    </div>
  </section>

  <footer>
    <strong>Vo</strong>
    <span>Experimental language tooling for the Rust ecosystem.</span>
    <a href="https://github.com/vo-lang/volang" target="_blank" rel="noreferrer">Source</a>
    <button on:click={onDocs}>Documentation</button>
  </footer>
</main>

{#if showCreateModal}
  <CreateProjectModal
    busy={creating}
    error={createError}
    {platform}
    defaultLocation={createLocationDefault}
    onPickDirectory={isNative ? pickDirectoryForCreate : null}
    on:close={() => { showCreateModal = false; createError = ''; }}
    on:create={(event) => createProject(event.detail.kind, event.detail.name, event.detail.modulePath, event.detail.location)}
  />
{/if}

{#if showOpenProjectModal}
  <OpenProjectModal
    busy={openingProject}
    error={openProjectError}
    on:close={() => { showOpenProjectModal = false; openProjectError = ''; }}
    on:openFolder={() => void openProjectFromDirectory()}
    on:openFile={() => void openProjectFromFile()}
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
    on:close={closeProjectMenu}
    on:open={() => openProject(project)}
    on:share={() => void shareProject(project)}
    on:rename={() => (renameTarget = project)}
    on:diff={() => void openDiff(project)}
    on:push={() => void pushProject(project)}
    on:pull={() => void pullProject(project)}
    on:remote={() => viewRemote(project)}
    on:delete={() => confirmDeleteProject(project)}
    on:deleteRemote={() => confirmDeleteRemoteProject(project)}
    on:deleteEverywhere={() => confirmDeleteEverywhere(project)}
    on:forget={() => confirmForgetProject(project)}
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
  <div class="confirm-backdrop" role="presentation" on:click|self={closeConfirmDialog}>
    <div
      bind:this={confirmModalElement}
      class="confirm-modal"
      role="dialog"
      tabindex="-1"
      aria-modal="true"
      aria-labelledby="confirm-title"
      aria-describedby="confirm-description"
    >
      <h3 id="confirm-title" class:danger={confirmDialog.danger}>{confirmDialog.title}</h3>
      <p id="confirm-description">{confirmDialog.message}</p>
      <div class="confirm-actions">
        <button bind:this={confirmCancelButton} class="button secondary" on:click={closeConfirmDialog}>Cancel</button>
        <button class="button" class:danger-button={confirmDialog.danger} class:primary={!confirmDialog.danger} on:click={confirmDialog.action}>{confirmDialog.title}</button>
      </div>
    </div>
  </div>
{/if}

{#if syncDialog}
  {@const dialog = syncDialog}
  <ProjectSyncModal
    diff={dialog}
    busy={$catalogStore.busyKeys.includes(projectKey(dialog.project))}
    on:close={() => { syncDialog = null; syncDialogProjectKey = ''; }}
    on:push={() => void pushProject(dialog.project)}
    on:pull={() => void pullProject(dialog.project)}
  />
{/if}

<style>
  .home {
    flex: 1;
    min-height: 0;
    overflow-y: auto;
    overflow-x: hidden;
    color: var(--text);
    background: var(--surface-canvas);
  }

  .page-error {
    margin: 0;
    padding: 12px clamp(20px, 7vw, 104px);
    border-block: 1px solid color-mix(in srgb, var(--danger) 22%, transparent);
    color: var(--danger);
    background: color-mix(in srgb, var(--danger) 7%, var(--surface-0));
    font-size: 13px;
  }

  .projects {
    padding: clamp(64px, 8vw, 112px) clamp(20px, 7vw, 104px);
    background:
      radial-gradient(circle at 12% 10%, rgba(124, 140, 255, 0.08), transparent 28rem),
      var(--surface-0);
  }

  .projects-inner { max-width: 1440px; margin: 0 auto; }
  .projects-heading { display: flex; align-items: flex-end; justify-content: space-between; gap: 32px; }
  .projects-heading > div:first-child { max-width: 650px; }
  .section-kicker { color: var(--accent-2); font-size: 12px; font-weight: 820; letter-spacing: 0.13em; text-transform: uppercase; }
  h2 { margin: 10px 0 0; color: var(--text-strong); font-size: clamp(32px, 4vw, 52px); line-height: 1.05; letter-spacing: -0.04em; }
  .projects-heading p { max-width: 580px; margin: 14px 0 0; color: var(--text-muted); font-size: 15px; line-height: 1.65; }
  .project-actions { display: flex; gap: 9px; flex-wrap: wrap; justify-content: flex-end; }

  .button,
  .icon-button {
    min-height: 42px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    gap: 8px;
    border: 1px solid var(--line);
    border-radius: 11px;
    color: var(--text);
    background: var(--surface-hover);
    cursor: pointer;
    font-size: 13px;
    font-weight: 740;
    transition: transform 150ms ease, border-color 150ms ease, background 150ms ease;
  }

  .button { padding: 0 15px; }
  .button:hover, .icon-button:hover:not(:disabled) { transform: translateY(-1px); border-color: var(--line-strong); }
  .button svg, .icon-button svg { width: 17px; height: 17px; fill: none; stroke: currentColor; stroke-width: 1.7; stroke-linecap: round; stroke-linejoin: round; }
  .button.primary { color: #07101a; border-color: color-mix(in srgb, var(--accent) 48%, transparent); background: linear-gradient(135deg, color-mix(in srgb, var(--accent) 86%, #fff), color-mix(in srgb, var(--accent-2) 68%, var(--accent))); }
  .button.secondary { background: var(--surface-raised); }
  .button.ghost { color: var(--text-muted); background: transparent; }
  .button .github-icon { fill: currentColor; stroke: none; }
  .danger-button { color: var(--surface-canvas); border-color: var(--danger); background: var(--danger); }

  .project-toolbar {
    display: flex;
    align-items: center;
    gap: 9px;
    margin-top: 34px;
    padding: 12px;
    border: 1px solid var(--line-subtle);
    border-radius: 15px;
    background: color-mix(in srgb, var(--surface-raised) 64%, transparent);
  }

  .project-search { width: min(320px, 42vw); min-height: 40px; display: flex; align-items: center; gap: 8px; padding: 0 12px; border: 1px solid var(--line); border-radius: 10px; background: var(--surface-0); }
  .project-search:focus-within { border-color: color-mix(in srgb, var(--accent) 60%, transparent); }
  .project-search svg { width: 16px; height: 16px; fill: none; stroke: var(--text-faint); stroke-width: 1.7; }
  .project-search input { flex: 1; min-width: 0; border: 0; outline: 0; color: var(--text); background: transparent; font-size: 13px; }
  .project-search input::placeholder { color: var(--text-faint); }
  .project-count, .sync-hint { color: var(--text-faint); font-size: 12px; }
  .toolbar-spacer { flex: 1; }
  .icon-button { width: 42px; padding: 0; }
  .icon-button:disabled { cursor: progress; opacity: 0.55; }
  .icon-button svg.spinning { animation: spin 800ms linear infinite; }
  @keyframes spin { to { transform: rotate(360deg); } }

  .progress { height: 2px; margin-top: -2px; overflow: hidden; background: var(--line-subtle); }
  .progress span { display: block; width: 30%; height: 100%; background: linear-gradient(90deg, var(--accent), var(--accent-2)); animation: progress 1.1s ease-in-out infinite; }
  @keyframes progress { from { transform: translateX(-110%); } to { transform: translateX(440%); } }

  .inline-error { display: flex; align-items: center; justify-content: space-between; gap: 12px; margin-top: 12px; padding: 11px 13px; border: 1px solid color-mix(in srgb, var(--danger) 22%, transparent); border-radius: 11px; color: var(--danger); background: color-mix(in srgb, var(--danger) 7%, transparent); font-size: 13px; }
  .inline-error button { border: 0; color: inherit; background: transparent; cursor: pointer; font-size: 18px; }

  .project-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(230px, 1fr)); gap: 12px; margin-top: 16px; }
  .skeleton { height: 94px; border: 1px solid var(--line-subtle); border-radius: 14px; background: linear-gradient(105deg, var(--surface-1) 35%, var(--surface-raised) 48%, var(--surface-1) 62%); background-size: 220% 100%; animation: shimmer 1.5s infinite; }
  @keyframes shimmer { to { background-position-x: -220%; } }

  .empty { min-height: 220px; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 9px; margin-top: 16px; padding: 28px; border: 1px dashed var(--line); border-radius: 18px; color: var(--text-muted); text-align: center; }
  .empty strong { color: var(--text); font-size: 15px; }
  .empty > span:not(.empty-mark) { max-width: 430px; font-size: 13px; line-height: 1.5; }
  .empty-mark { width: 45px; height: 45px; display: grid; place-items: center; margin-bottom: 5px; border: 1px solid color-mix(in srgb, var(--accent) 28%, transparent); border-radius: 13px; color: var(--accent-soft); background: color-mix(in srgb, var(--accent) 10%, transparent); font-size: 18px; font-weight: 850; }

  footer { display: flex; align-items: center; gap: 18px; padding: 24px clamp(20px, 7vw, 104px); border-top: 1px solid var(--line-subtle); color: var(--text-faint); background: var(--surface-canvas); font-size: 12px; }
  footer strong { color: var(--text); font-size: 16px; }
  footer span { flex: 1; }
  footer a, footer button { border: 0; color: var(--text-muted); background: none; cursor: pointer; text-decoration: none; }
  footer a:hover, footer button:hover { color: var(--text); }

  .confirm-backdrop { position: fixed; inset: 0; z-index: 210; display: grid; place-items: center; padding: 20px; background: rgba(2, 4, 10, 0.76); backdrop-filter: blur(8px); }
  .confirm-modal { width: min(100%, 410px); display: grid; gap: 14px; padding: 22px; border: 1px solid var(--line); border-radius: 18px; background: var(--surface-raised); box-shadow: var(--shadow-xl); }
  .confirm-modal h3 { margin: 0; color: var(--text-strong); font-size: 17px; }
  .confirm-modal h3.danger { color: var(--danger); }
  .confirm-modal p { margin: 0; color: var(--text-muted); font-size: 13px; line-height: 1.6; }
  .confirm-actions { display: flex; justify-content: flex-end; gap: 8px; margin-top: 4px; }
  .sr-only { position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0, 0, 0, 0); white-space: nowrap; border: 0; }

  @media (max-width: 780px) {
    .projects-heading { align-items: flex-start; flex-direction: column; }
    .project-actions { justify-content: flex-start; }
    .project-toolbar { flex-wrap: wrap; }
    .project-search { order: 3; width: 100%; }
    .project-count { order: 3; width: 100%; }
    footer { flex-wrap: wrap; }
    footer span { order: 4; width: 100%; flex-basis: 100%; }
  }

  @media (max-width: 520px) {
    .projects { padding-inline: 16px; }
    .button.ghost { width: 42px; overflow: hidden; padding: 0; font-size: 0; }
    .button.ghost svg { width: 18px; height: 18px; }
    .project-grid { grid-template-columns: 1fr; }
    footer { padding-inline: 16px; }
  }
</style>
