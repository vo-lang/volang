<script lang="ts">
  import { onDestroy, onMount } from 'svelte';
  import { readable, type Readable } from 'svelte/store';
  import { createServiceRegistry, type ServiceRegistry } from './lib/services/service_registry';
  import type { BootstrapContext, FsEntry, SessionInfo, ShareInfo, StudioMode } from './lib/types';
  import type { GitHubAccountState, ManagedProject } from './lib/project_catalog/types';
  import { ide } from './stores/ide';
  import { route, setModeHash } from './lib/router';
  import { consolePush } from './stores/console';
  import { editor, editorMarkSaved, editorOpen } from './stores/editor';
  import { session, sessionOpen } from './stores/session';
  import { runtime } from './stores/runtime';
  import { buildShareInfo } from './lib/session_share';
  import { BLOCKKART_GITHUB_URL, BLOCKKART_QUICKPLAY_SPEC } from './lib/quickplay';
  import Sidebar from './components/Sidebar.svelte';
  import DevWorkbench from './components/DevWorkbench.svelte';
  import Home from './components/Home.svelte';
  import GitHubConnectModal from './components/home/GitHubConnectModal.svelte';
  import RunnerModeLayout from './components/RunnerModeLayout.svelte';
  import DocsPanel from './components/DocsPanel.svelte';

  let registry: ServiceRegistry | null = null;
  let bootstrap: BootstrapContext | null = null;
  let sessionInfo: SessionInfo | null = null;
  let explorerEntries: FsEntry[] = [];
  let currentDir = '';
  let loading = 'Bootstrapping Studio…';
  let error = '';
  let runtimePollTimer: number | null = null;
  let runtimePollGeneration = 0;
  let sessionProjectHasGui = false;
  let showGitHubConnectModal = false;
  let confirmDialog: { title: string; message: string; danger: boolean; action: () => void } | null = null;
  let githubStore: Readable<GitHubAccountState> = readable({
    token: null,
    user: null,
    connecting: false,
    error: '',
  });

  function launchLoading(proj: string | null): string {
    if (!proj) return 'Opening workspace session…';
    if (proj.includes('github.com/')) {
      const match = proj.match(/github\.com\/([^/]+\/[^/]+)/);
      if (match) return `Opening ${match[1]}…`;
    }
    const name = proj.split('/').pop() ?? proj;
    return `Opening ${name}…`;
  }

  onMount(async () => {
    try {
      registry = await createServiceRegistry();
      githubStore = registry.projectCatalog.github;
      bootstrap = registry.project.bootstrapContext;
      const launch = bootstrap.launch;
      const spec = launch ?? { proj: null, mode: bootstrap.mode };
      const hasProj = spec.proj != null;
      const isRunner = spec.mode === 'runner' && hasProj;
      const wantsDocs = !hasProj && $route.mode === 'docs';
      loading = launchLoading(spec.proj);
      const openedSession = await registry.project.openSession(spec);
      if (isRunner) {
        await bindRunnerSession(openedSession);
        ide.update((s) => ({ ...s, appMode: 'runner' }));
      } else {
        await bindDevSession(openedSession, {
          openEntry: Boolean(hasProj && openedSession.entryPath),
          syncUrl: !wantsDocs,
        });
        ide.update((s) => ({ ...s, appMode: wantsDocs ? 'docs' : hasProj ? 'develop' : 'manage' }));
      }
      loading = '';
      if (!isRunner) {
        void registry.projectCatalog.initialize(openedSession.root).catch((catalogError) => {
          consolePush('stderr', formatError(catalogError));
        });
      }
      if (isRunner && openedSession.entryPath) {
        await autoRunGui(openedSession.entryPath);
      }
    } catch (err) {
      error = formatError(err);
      loading = '';
    }
  });

  async function autoRunGui(target: string): Promise<void> {
    if (!registry) return;
    registry.runtime.clearConsole();
    try {
      stopRuntimePolling();
      await registry.runtime.runGui(target);
      startRuntimePolling();
    } catch (_) {}
  }

  // Sync hash route → appMode (for docs deep links and browser back/forward)
  const unsubRoute = route.subscribe(r => {
    if (r.mode === 'docs' && $ide.appMode !== 'docs') {
      ide.update(s => ({ ...s, appMode: 'docs' }));
      return;
    }
  });

  // Sync appMode → hash (when user clicks sidebar)
  let prevAppMode = $ide.appMode;
  const unsubIde = ide.subscribe(s => {
    if (s.appMode !== prevAppMode) {
      prevAppMode = s.appMode;
      setModeHash(s.appMode);
    }
  });

  onDestroy(() => {
    stopRuntimePolling();
    unsubRoute();
    unsubIde();
  });

  function shareInfoForMode(nextSessionInfo: SessionInfo, mode: StudioMode, baseUrl?: string): ShareInfo {
    return buildShareInfo(nextSessionInfo, { mode, baseUrl });
  }

  function bareModeUrl(mode: StudioMode): string {
    const url = new URL(window.location.href);
    url.search = '';
    url.hash = mode === 'runner' ? '#/runner' : '#/develop';
    return url.toString();
  }

  function syncBrowserUrlToSession(nextSessionInfo: SessionInfo, mode: StudioMode): void {
    const share = shareInfoForMode(nextSessionInfo, mode, window.location.href);
    const nextUrl = share.shareable && share.canonicalUrl
      ? share.canonicalUrl
      : bareModeUrl(mode);
    if (window.location.href !== nextUrl) {
      window.history.replaceState(null, '', nextUrl);
    }
  }

  async function bindRunnerSession(nextSessionInfo: SessionInfo): Promise<void> {
    if (!registry) return;
    sessionInfo = nextSessionInfo;
    registry.runtime.clearConsole();
    sessionOpen(
      nextSessionInfo.root,
      'runner',
      nextSessionInfo.entryPath ?? null,
      nextSessionInfo.projectMode,
      nextSessionInfo.source,
      shareInfoForMode(nextSessionInfo, 'runner'),
    );
    syncBrowserUrlToSession(nextSessionInfo, 'runner');
    currentDir = nextSessionInfo.root;
    explorerEntries = [];
    editorOpen('', '');
  }

  async function bindDevSession(
    nextSessionInfo: SessionInfo,
    options: { openEntry?: boolean; syncCatalogRoot?: boolean; syncUrl?: boolean } = {},
  ): Promise<void> {
    if (!registry) return;
    sessionInfo = nextSessionInfo;
    registry.runtime.clearConsole();
    sessionOpen(
      nextSessionInfo.root,
      'dev',
      nextSessionInfo.entryPath ?? null,
      nextSessionInfo.projectMode,
      nextSessionInfo.source,
      shareInfoForMode(nextSessionInfo, 'dev'),
    );
    if (options.syncUrl !== false) {
      syncBrowserUrlToSession(nextSessionInfo, 'dev');
    }
    sessionProjectHasGui = registry.projectCatalog.getSessionProjectConfig(nextSessionInfo).hasGui;
    ide.update((s) => ({ ...s, outputExpanded: false, previewCollapsed: false }));
    currentDir = nextSessionInfo.root;
    explorerEntries = [];
    editorOpen('', '');
    if (nextSessionInfo.projectMode === 'module') {
      await refreshDirectory(nextSessionInfo.root);
    }
    if (options.syncCatalogRoot) {
      void registry.projectCatalog.setRoot(nextSessionInfo.root).catch((catalogError) => {
        consolePush('stderr', formatError(catalogError));
      });
    }
    const shouldOpenEntry = Boolean(nextSessionInfo.entryPath && (options.openEntry || nextSessionInfo.projectMode === 'single-file'));
    if (shouldOpenEntry && nextSessionInfo.entryPath) {
      try {
        const content = await registry.workspace.readFile(nextSessionInfo.entryPath);
        editorOpen(nextSessionInfo.entryPath, content);
      } catch (_) {}
    }
  }

  async function refreshDirectory(path: string): Promise<void> {
    if (!registry) return;
    explorerEntries = await registry.workspace.list(path);
    currentDir = path;
  }

  async function openEntry(entry: FsEntry): Promise<void> {
    if (!registry) return;
    if (entry.isDir) { await refreshDirectory(entry.path); return; }
    const content = await registry.workspace.readFile(entry.path);
    editorOpen(entry.path, content);
  }

  async function saveActiveEditor(options: { quiet?: boolean } = {}): Promise<boolean> {
    if (!registry) return false;
    const activeFilePath = $editor.activeFilePath;
    const entryPath = sessionInfo?.entryPath ?? '';
    const targetFilePath = activeFilePath || entryPath;
    if (!targetFilePath) return false;
    if (!activeFilePath) {
      if ($editor.dirty) {
        throw new Error('Active editor is not bound to a file');
      }
      return true;
    }
    if (!$editor.dirty) return true;
    await registry.workspace.writeFile(activeFilePath, $editor.code);
    editorMarkSaved();
    if (!options.quiet) {
      consolePush('system', `Saved ${activeFilePath.split('/').pop() ?? activeFilePath}`);
    }
    return true;
  }

  async function flushEditorBeforeRun(): Promise<boolean> {
    if (!registry) return false;
    try {
      return await saveActiveEditor({ quiet: true });
    } catch (error) {
      consolePush('stderr', formatError(error));
      return false;
    }
  }

  async function openExample(source: string, filename: string, hasGui: boolean): Promise<void> {
    if (!registry) return;
    stopRuntimePolling();
    await registry.runtime.stop().catch(() => undefined);
    // Write to .examples/ (dot-prefixed → invisible to discoverProjects)
    const wsRoot = registry.project.bootstrapContext.workspaceRoot;
    const exDir = `${wsRoot}/.examples`;
    const filePath = `${exDir}/${filename}`;
    await registry.workspace.writeFile(filePath, source);
    const exSession: SessionInfo = {
      root: exDir,
      origin: 'workspace',
      projectMode: 'single-file',
      entryPath: filePath,
      singleFileRun: false,
      source: null,
      share: null,
    };
    await registry.projectCatalog.updateSessionProjectConfig(exSession, hasGui);
    await bindDevSession(exSession, { openEntry: true });
    ide.update((s) => ({ ...s, appMode: 'develop' }));
  }

  async function openManagedProject(project: ManagedProject): Promise<void> {
    if (!registry || !project.localPath) return;
    stopRuntimePolling();
    await registry.runtime.stop().catch(() => undefined);
    const openPath = project.type === 'single'
      ? (project.entryPath ?? project.localPath)
      : project.localPath;
    const openedSession = await registry.project.openSession({ proj: openPath, mode: 'dev' });
    registry.projectCatalog.trackRecentSessionTarget(openPath, openedSession);
    await bindDevSession(openedSession, { openEntry: true });
    ide.update((s) => ({ ...s, appMode: 'develop' }));
  }

  async function openLocalPath(path: string): Promise<void> {
    if (!registry) return;
    stopRuntimePolling();
    await registry.runtime.stop().catch(() => undefined);
    const openedSession = await registry.project.openSession({ proj: path, mode: 'dev' });
    registry.projectCatalog.trackRecentSessionTarget(path, openedSession);
    await bindDevSession(openedSession, { openEntry: true });
    ide.update((s) => ({ ...s, appMode: 'develop' }));
  }

  async function openFeaturedProject(url: string, hasGui: boolean): Promise<void> {
    if (!registry) return;
    stopRuntimePolling();
    await registry.runtime.stop().catch(() => undefined);
    const openedSession = await registry.project.openSession({ proj: url, mode: 'dev' });
    if (hasGui) {
      await registry.projectCatalog.updateSessionProjectConfig(openedSession, true);
    }
    await bindDevSession(openedSession, { openEntry: true });
    ide.update((s) => ({ ...s, appMode: 'develop' }));
  }

  async function playFeaturedProject(url: string, hasGui: boolean): Promise<void> {
    if (!registry || !hasGui) return;
    stopRuntimePolling();
    await registry.runtime.stop().catch(() => undefined);
    const playUrl = registry.backend.platform === 'wasm' || url !== BLOCKKART_QUICKPLAY_SPEC
      ? url
      : BLOCKKART_GITHUB_URL;
    const openedSession = await registry.project.openSession({ proj: playUrl, mode: 'runner' });
    await bindRunnerSession(openedSession);
    ide.update((s) => ({ ...s, appMode: 'runner' }));
    if (!openedSession.entryPath) {
      throw new Error('Featured project has no entry file');
    }
    registry.runtime.clearConsole();
    try {
      stopRuntimePolling();
      await registry.runtime.runGui(openedSession.entryPath);
      startRuntimePolling();
    } catch (_) {}
  }

  async function goParent(): Promise<void> {
    if (!sessionInfo || currentDir === sessionInfo.root) return;
    const idx = currentDir.lastIndexOf('/');
    const parent = idx > 0 ? currentDir.slice(0, idx) : '/';
    if (parent.startsWith(sessionInfo.root)) await refreshDirectory(parent);
  }

  function formatError(err: unknown): string {
    return err instanceof Error ? err.message : String(err);
  }

  function currentRunTarget(): string {
    const filePath = $editor.activeFilePath;
    if (filePath.endsWith('.vo')) return filePath;
    return sessionInfo?.entryPath ?? '';
  }

  async function runCode(): Promise<boolean> {
    if (!registry) return false;
    const target = currentRunTarget();
    if (!target) return false;
    registry.runtime.clearConsole();
    const flushed = await flushEditorBeforeRun();
    if (!flushed) return false;
    try {
      await registry.runtime.runConsole(target, 'vm');
      return true;
    } catch (_) {
      return false;
    }
  }

  async function runGui(): Promise<boolean> {
    if (!registry) return false;
    const target = currentRunTarget();
    if (!target) return false;
    registry.runtime.clearConsole();
    const flushed = await flushEditorBeforeRun();
    if (!flushed) return false;
    try {
      stopRuntimePolling();
      await registry.runtime.runGui(target);
      startRuntimePolling();
      return true;
    } catch (_) {
      return false;
    }
  }

  async function runProject(): Promise<void> {
    if (sessionProjectHasGui) {
      await runGui();
      return;
    }
    await runCode();
  }

  async function shareSession(): Promise<void> {
    if (!sessionInfo) {
      throw new Error('No active session to share');
    }
    const share = buildShareInfo(sessionInfo, {
      mode: $session.mode,
    });
    if (!share.shareable || !share.canonicalUrl) {
      throw new Error(share.reason ?? 'Session is not shareable');
    }
    if (!navigator.clipboard?.writeText) {
      throw new Error('Clipboard API is unavailable');
    }
    await navigator.clipboard.writeText(share.canonicalUrl);
    consolePush('system', 'Copied share link');
  }

  function openGitHubConnectModal(): void {
    showGitHubConnectModal = true;
  }

  function closeGitHubConnectModal(): void {
    showGitHubConnectModal = false;
  }

  async function connectGitHub(token: string): Promise<void> {
    if (!registry) return;
    try {
      await registry.projectCatalog.connectGitHub(token);
      showGitHubConnectModal = false;
    } catch {}
  }

  function disconnectGitHub(): void {
    if (!registry) return;
    confirmDialog = {
      title: 'Disconnect GitHub',
      message: 'Sign out of GitHub? Remote project metadata will no longer be synced.',
      danger: false,
      action: () => {
        confirmDialog = null;
        void registry.projectCatalog.disconnectGitHub().catch((connectError) => {
          consolePush('stderr', formatError(connectError));
        });
      },
    };
  }

  async function stopCode(): Promise<void> {
    if (!registry) return;
    stopRuntimePolling();
    await registry.runtime.stop().catch(() => undefined);
  }

  async function runFullscreen(): Promise<void> {
    if (!sessionProjectHasGui) return;
    const started = await runGui();
    if (started) {
      ide.update((s) => ({ ...s, outputExpanded: true, previewCollapsed: false }));
    }
  }

  function exitFullscreen(): void {
    ide.update((s) => ({ ...s, outputExpanded: false }));
  }

  async function setSessionProjectHasGui(hasGui: boolean): Promise<void> {
    if (!registry || !sessionInfo || $runtime.isRunning || sessionProjectHasGui === hasGui) {
      return;
    }
    sessionProjectHasGui = hasGui;
    if (hasGui) {
      ide.update((s) => ({ ...s, previewCollapsed: false }));
    } else {
      stopRuntimePolling();
      ide.update((s) => ({ ...s, outputExpanded: false, previewCollapsed: false }));
    }
    try {
      const nextConfig = await registry.projectCatalog.updateSessionProjectConfig(sessionInfo, hasGui);
      sessionProjectHasGui = nextConfig.hasGui;
    } catch (error) {
      sessionProjectHasGui = !hasGui;
      consolePush('stderr', formatError(error));
      if (!hasGui) {
        ide.update((s) => ({ ...s, previewCollapsed: false }));
      } else {
        ide.update((s) => ({ ...s, outputExpanded: false, previewCollapsed: false }));
      }
    }
  }

  function startRuntimePolling(): void {
    stopRuntimePolling();
    const generation = ++runtimePollGeneration;
    const pollOnce = async (): Promise<void> => {
      if (!registry || generation !== runtimePollGeneration) return;
      try {
        await registry.runtime.pollGuiRender();
      } catch (e) {
        consolePush('stderr', formatError(e));
        if (generation === runtimePollGeneration) {
          stopRuntimePolling();
        }
        return;
      }
      if (generation !== runtimePollGeneration) return;
      runtimePollTimer = window.setTimeout(() => {
        void pollOnce();
      }, 16);
    };
    void pollOnce();
  }

  function stopRuntimePolling(): void {
    runtimePollGeneration++;
    if (runtimePollTimer !== null) { window.clearTimeout(runtimePollTimer); runtimePollTimer = null; }
  }

  $: appMode = $ide.appMode;
  $: isGuiProject = sessionProjectHasGui;
  $: showExplorer = $session.projectMode === 'module';
  $: isSingleFileSession = $session.projectMode === 'single-file';
  $: outputExpanded = $ide.outputExpanded;
  $: previewCollapsed = $ide.previewCollapsed;
  $: previewTitle = $runtime.gui.entryPath
    ? ($runtime.gui.entryPath.split('/').pop() ?? '')
    : $session.entryPath
      ? ($session.entryPath.split('/').pop() ?? '')
      : $session.projectName;
</script>

<svelte:window
  on:keydown={(event) => {
    if (!(event.metaKey || event.ctrlKey) || event.key.toLowerCase() !== 's') return;
    if (appMode !== 'develop') return;
    event.preventDefault();
    void saveActiveEditor().catch((error) => {
      consolePush('stderr', formatError(error));
    });
  }}
/>

{#if error}
  <div class="splash">
    <div class="splash-card error">
      <div class="splash-vo">Vo<span>Studio</span></div>
      <p class="splash-error">{error}</p>
    </div>
  </div>
{:else if loading}
  <div class="splash">
    <div class="splash-card">
      <div class="splash-vo">Vo<span>Studio</span></div>
      <p class="splash-loading">{loading}</p>
    </div>
  </div>
{:else if appMode === 'runner' && registry}
  <RunnerModeLayout {registry} />
{:else}
  <div class="app">
    <Sidebar
      {githubStore}
      onConnectGitHub={openGitHubConnectModal}
      onDisconnectGitHub={disconnectGitHub}
    />

    <div class="main-area">
      {#if appMode === 'manage'}
        <!-- HOME: project management -->
        <Home
          projectCatalog={registry.projectCatalog}
          backend={registry.backend}
          platform={registry.backend.platform}
          onOpenProject={openManagedProject}
          onOpenLocalPath={openLocalPath}
          onOpenFeaturedProject={openFeaturedProject}
          onPlayFeaturedProject={playFeaturedProject}
          onOpenExample={openExample}
          onDocs={() => ide.update(s => ({ ...s, appMode: 'docs' }))}
          onDevelop={() => ide.update(s => ({ ...s, appMode: 'develop' }))}
          onConnectGitHub={openGitHubConnectModal}
        />

      {:else if appMode === 'develop'}
        <DevWorkbench
          {registry}
          explorerEntries={explorerEntries}
          {currentDir}
          sessionRoot={$session.root}
          {showExplorer}
          {isSingleFileSession}
          {isGuiProject}
          projectHasGui={sessionProjectHasGui}
          {previewCollapsed}
          {outputExpanded}
          {previewTitle}
          onSave={() => void saveActiveEditor().catch((error) => consolePush('stderr', formatError(error)))}
          onShare={() => void shareSession().catch((error) => consolePush('stderr', formatError(error)))}
          onRun={runProject}
          onRunFullscreen={runFullscreen}
          onStop={stopCode}
          onSetProjectHasGui={(hasGui) => void setSessionProjectHasGui(hasGui)}
          onOpenEntry={openEntry}
          onGoParent={goParent}
          onExitFullscreen={exitFullscreen}
          onTogglePreviewCollapsed={() => ide.update((s) => ({ ...s, previewCollapsed: !s.previewCollapsed }))}
        />

      {:else if appMode === 'docs'}
        <DocsPanel />
      {/if}
    </div>
  </div>
{/if}

{#if showGitHubConnectModal}
  <GitHubConnectModal
    connecting={$githubStore.connecting}
    error={$githubStore.error}
    on:close={closeGitHubConnectModal}
    on:submit={(event) => connectGitHub(event.detail.token)}
  />
{/if}

{#if confirmDialog}
  <div
    class="confirm-backdrop"
    role="button"
    tabindex="0"
    aria-label="Close"
    on:click|self={() => (confirmDialog = null)}
    on:keydown={(event) => event.key === 'Escape' && (confirmDialog = null)}
  >
    <div class="confirm-modal" role="dialog" aria-modal="true">
      <h3 class:danger={confirmDialog.danger}>{confirmDialog.title}</h3>
      <p class="confirm-msg">{confirmDialog.message}</p>
      <div class="confirm-actions">
        <button class="secondary" on:click={() => (confirmDialog = null)}>Cancel</button>
        <button class={confirmDialog.danger ? 'danger-btn' : 'primary'} on:click={confirmDialog.action}>
          {confirmDialog.title}
        </button>
      </div>
    </div>
  </div>
{/if}

<style>
  :global(*, *::before, *::after) { box-sizing: border-box; }
  :global(:root) {
    --studio-topbar-height: 44px;
  }
  :global(html, body) {
    height: 100%;
    margin: 0;
    padding: 0;
    background: #11111b;
    color: #cdd6f4;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
    font-size: 14px;
  }
  :global(#app) { height: 100%; display: flex; flex-direction: column; }

  /* Splash */
  .splash {
    display: grid;
    place-items: center;
    height: 100%;
    background: #11111b;
  }
  .splash-card {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 16px;
    padding: 48px 40px;
    border-radius: 20px;
    background: #181825;
    border: 1px solid #313244;
  }
  .splash-card.error { border-color: #f38ba8; }
  .splash-vo { font-size: 36px; font-weight: 900; color: #89b4fa; }
  .splash-vo span { font-weight: 300; color: #585b70; margin-left: 8px; font-size: 28px; }
  .splash-loading { color: #585b70; font-size: 14px; margin: 0; }
  .splash-error { color: #f38ba8; font-size: 13px; margin: 0; max-width: 400px; text-align: center; }

  /* App layout */
  .app { display: flex; height: 100%; overflow: hidden; }
  .main-area { flex: 1; display: flex; flex-direction: column; min-width: 0; overflow: hidden; }

  .confirm-backdrop {
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.72);
    backdrop-filter: blur(6px);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 210;
  }
  .confirm-modal {
    width: min(100%, 380px);
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 16px;
    box-shadow: 0 24px 64px rgba(0, 0, 0, 0.55);
    padding: 22px;
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
  }
  .primary,
  .secondary,
  .danger-btn {
    border: none;
    border-radius: 8px;
    font: inherit;
    font-size: 12px;
    font-weight: 700;
    padding: 8px 12px;
    cursor: pointer;
  }
  .primary {
    background: #89b4fa;
    color: #11111b;
  }
  .secondary {
    background: #313244;
    color: #cdd6f4;
  }
  .danger-btn {
    background: #f38ba8;
    color: #11111b;
  }
</style>
