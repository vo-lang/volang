<script lang="ts">
  import { onDestroy, onMount } from 'svelte';
  import { get, readable, type Readable } from 'svelte/store';
  import { createServiceRegistry, type ServiceRegistry } from './lib/services/service_registry';
  import type { BootstrapContext, FsEntry, PreparedSession, SessionInfo, ShareInfo, StudioMode } from './lib/types';
  import type { GitHubAccountState, ManagedProject } from './lib/project_catalog/types';
  import { ide, type AppMode } from './stores/ide';
  import {
    onBrowserHistoryNavigation,
    resolveStartupLaunch,
    route,
    setExampleHash,
    setModeHash,
    syncRouteFromLocation,
  } from './lib/router';
  import { console_, consolePush, type ConsoleLine } from './stores/console';
  import { editor, editorMarkSaved, editorOpen } from './stores/editor';
  import { session, sessionOpen } from './stores/session';
  import { runtime } from './stores/runtime';
  import { buildShareInfo, buildStudioLaunchUrl } from './lib/session_share';
  import { LatestTaskQueue } from './lib/latest_task_queue';
  import { quiesceRendererBridgeForSmoke, rendererBridgeSmokeState } from './lib/gui/renderer_bridge';
  import { rotateVoplayPerfEvidenceEpoch } from './lib/perf_report_bridge';
  import Sidebar from './components/Sidebar.svelte';
  import Home from './components/Home.svelte';
  import GitHubConnectModal from './components/home/GitHubConnectModal.svelte';
  import { STARTER_EXAMPLES, type StarterExample } from './components/home/content';

  type StudioBrowserSmokeDebugHook = {
    entryPath(): string | null;
    consoleLines(): ConsoleLine[];
    runtimeState(): unknown;
    dumpCurrent(): Promise<string>;
    dump(path: string): Promise<string>;
    quiesceRenderLoop(): { renderers: unknown[]; stopped: number; sessionId: number | null };
    rendererState(): { active: boolean; renderers: unknown[]; sessionId: number | null };
    rotatePerfEvidenceEpoch(): number;
  };

  type StudioBrowserSmokeDebugBackend = {
    dumpVo(path: string): Promise<string>;
    dumpGuiBytecode?(path: string): Promise<string>;
  };

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
  let DevWorkbenchView: typeof import('./components/DevWorkbench.svelte').default | null = null;
  let DocsPanelView: typeof import('./components/DocsPanel.svelte').default | null = null;
  let RunnerModeView: typeof import('./components/RunnerModeLayout.svelte').default | null = null;
  let loadingView: AppMode | null = null;
  let openingDefaultStarter = false;
  let activeBuiltinExampleId: string | null = null;
  let failedBuiltinExampleId: string | null = null;
  let seedDefaultStarter = false;
  const sessionNavigation = new LatestTaskQueue();
  const BROWSER_URL_SYNCHRONIZED_KEY = 'vo.studio.browser-url-synchronized.v1';
  const defaultStarter = STARTER_EXAMPLES.find((example) => example.id === 'channels') ?? STARTER_EXAMPLES[0];

  function navigateSession(
    operation: (isCurrent: () => boolean, commit: () => boolean) => Promise<void>,
  ): Promise<void> {
    return sessionNavigation.run(({ isLatest, commit }) => operation(isLatest, commit)).then(() => undefined);
  }

  function browserUrlWasSynchronized(): boolean {
    try {
      return window.sessionStorage.getItem(BROWSER_URL_SYNCHRONIZED_KEY) === '1';
    } catch {
      return false;
    }
  }

  function markBrowserUrlSynchronized(): void {
    try {
      window.sessionStorage.setItem(BROWSER_URL_SYNCHRONIZED_KEY, '1');
    } catch {}
  }

  async function ensureModeView(mode: AppMode): Promise<void> {
    if (mode === 'develop' && !DevWorkbenchView && loadingView !== mode) {
      loadingView = mode;
      try {
        DevWorkbenchView = (await import('./components/DevWorkbench.svelte')).default;
      } catch (viewError) {
        error = formatError(viewError);
      } finally {
        if (loadingView === mode) loadingView = null;
      }
      return;
    }
    if (mode === 'docs' && !DocsPanelView && loadingView !== mode) {
      loadingView = mode;
      try {
        DocsPanelView = (await import('./components/DocsPanel.svelte')).default;
      } catch (viewError) {
        error = formatError(viewError);
      } finally {
        if (loadingView === mode) loadingView = null;
      }
      return;
    }
    if (mode === 'runner' && !RunnerModeView && loadingView !== mode) {
      loadingView = mode;
      try {
        RunnerModeView = (await import('./components/RunnerModeLayout.svelte')).default;
      } catch (viewError) {
        error = formatError(viewError);
      } finally {
        if (loadingView === mode) loadingView = null;
      }
    }
  }

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
      refreshStudioBrowserSmokeDebugHook();
      githubStore = registry.projectCatalog.github;
      bootstrap = registry.project.bootstrapContext;
      const spec = resolveStartupLaunch({
        bootstrapLaunch: bootstrap.launch,
        bootstrapMode: bootstrap.mode,
        hash: window.location.hash,
        search: window.location.search,
        browserUrlWasSynchronized: browserUrlWasSynchronized(),
      });
      const hasProj = spec.proj != null;
      const isRunner = spec.mode === 'runner' && hasProj;
      const wantsDocs = !hasProj && $route.mode === 'docs';
      const wantsDevelop = !hasProj && $route.mode === 'develop';
      seedDefaultStarter = wantsDevelop;
      loading = launchLoading(spec.proj);
      const openedSession = await registry.project.openSession(spec);
      if (isRunner) {
        if (!openedSession.entryPath) {
          throw new Error('This project has no runnable entry file.');
        }
        await startGui(openedSession.entryPath);
        await bindRunnerSession(openedSession);
        ide.update((s) => ({ ...s, appMode: 'runner' }));
        startRuntimePolling();
      } else {
        await bindDevSession(openedSession, {
          openEntry: Boolean(hasProj && openedSession.entryPath),
          syncUrl: hasProj && !wantsDocs,
        });
        ide.update((s) => ({
          ...s,
          appMode: wantsDocs ? 'docs' : hasProj || wantsDevelop ? 'develop' : 'manage',
        }));
      }
      markBrowserUrlSynchronized();
      loading = '';
      if (!isRunner) {
        void registry.projectCatalog.initialize(openedSession.root).catch((catalogError) => {
          consolePush('stderr', formatError(catalogError));
        });
      }
    } catch (err) {
      error = formatError(err);
      loading = '';
    }
  });

  async function startGui(target: string): Promise<void> {
    if (!registry) throw new Error('Studio services are unavailable.');
    registry.runtime.clearConsole();
    stopRuntimePolling();
    await registry.runtime.runGui(target);
  }

  // The URL is the navigation authority for every interactive page.
  const unsubRoute = route.subscribe((nextRoute) => {
    if (nextRoute.mode === 'runner') return;
    if ($ide.appMode !== nextRoute.mode) {
      ide.update((state) => ({ ...state, appMode: nextRoute.mode }));
    }
  });
  const unsubHistoryNavigation = onBrowserHistoryNavigation(() => {
    sessionNavigation.invalidate();
  });

  // Sync appMode → hash (when user clicks sidebar)
  let prevAppMode = $ide.appMode;
  const unsubIde = ide.subscribe(s => {
    if (s.appMode !== prevAppMode) {
      prevAppMode = s.appMode;
      if (s.appMode === 'develop' && activeBuiltinExampleId) {
        setExampleHash(activeBuiltinExampleId);
      } else {
        setModeHash(s.appMode);
      }
    }
  });

  onDestroy(() => {
    sessionNavigation.invalidate();
    stopRuntimePolling();
    clearStudioBrowserSmokeDebugHook();
    unsubRoute();
    unsubHistoryNavigation();
    unsubIde();
  });

  function shareInfoForMode(nextSessionInfo: SessionInfo, mode: StudioMode, baseUrl?: string): ShareInfo {
    return buildShareInfo(nextSessionInfo, { mode, baseUrl });
  }

  function bareModeUrl(mode: StudioMode): string {
    const url = new URL(window.location.href);
    url.search = '';
    url.hash = mode === 'runner' ? '#/runner' : '#/develop';
    applyRunnerPerfDefaults(url, mode);
    return url.toString();
  }

  function recoverableSessionUrl(nextSessionInfo: SessionInfo, mode: StudioMode): string | null {
    const source = nextSessionInfo.source;
    if (source?.kind === 'path') {
      return buildStudioLaunchUrl({ proj: source.path, mode, baseUrl: window.location.href });
    }
    if (source?.kind === 'url') {
      return buildStudioLaunchUrl({ proj: source.url, mode, baseUrl: window.location.href });
    }
    if (source?.kind === 'github_repo') {
      return buildStudioLaunchUrl({ proj: source.htmlUrl, mode, baseUrl: window.location.href });
    }
    return null;
  }

  function syncBrowserUrlToSession(nextSessionInfo: SessionInfo, mode: StudioMode): void {
    const share = shareInfoForMode(nextSessionInfo, mode, window.location.href);
    let nextUrl = share.shareable && share.canonicalUrl
      ? share.canonicalUrl
      : recoverableSessionUrl(nextSessionInfo, mode) ?? bareModeUrl(mode);
    if (mode === 'runner') {
      const url = new URL(nextUrl);
      applyRunnerPerfDefaults(url, mode);
      preserveRunnerPerfParams(url, new URL(window.location.href));
      nextUrl = url.toString();
    }
    if (window.location.href !== nextUrl) {
      window.history.replaceState(null, '', nextUrl);
      syncRouteFromLocation();
    }
    markBrowserUrlSynchronized();
  }

  function applyRunnerPerfDefaults(url: URL, mode: StudioMode): void {
    if (mode !== 'runner' || !isLocalRunnerHost(url)) return;
    if (url.searchParams.has('voplayPerf') || url.searchParams.has('perf')) return;
    url.searchParams.set('voplayPerf', 'stats');
  }

  function preserveRunnerPerfParams(target: URL, source: URL): void {
    for (const [key, value] of source.searchParams) {
      if (isRunnerPerfParam(key)) {
        target.searchParams.set(key, value);
      }
    }
    const hashQueryOffset = source.hash.indexOf('?');
    if (hashQueryOffset >= 0) {
      const hashParams = new URLSearchParams(source.hash.slice(hashQueryOffset + 1));
      for (const [key, value] of hashParams) {
        if (isRunnerPerfParam(key)) {
          target.searchParams.set(key, value);
        }
      }
    }
  }

  function isRunnerPerfParam(name: string): boolean {
    return name === 'voplayPerf'
      || name === 'perf'
      || name === 'studioBrowserSmokeDebug'
      || name === 'voplayPerfOverlay'
      || name === 'rendererDebug'
      || name === 'debug'
      || name === 'voplayRendererPerf'
      || name === 'voplayPerfExperiment'
      || name === 'voplayPerfDiag'
      || name === 'voplayGcStress'
      || name === 'voGcStressEveryStep'
      || name.startsWith('voplayPerfDisable')
      || name.startsWith('voplayDisable');
  }

  function studioBrowserSmokeDebugEnabled(): boolean {
    try {
      return new URL(window.location.href).searchParams.get('studioBrowserSmokeDebug') === '1';
    } catch {
      return false;
    }
  }

  function studioBrowserSmokeDebugGlobal(): typeof globalThis & {
    __voStudioBrowserSmoke?: StudioBrowserSmokeDebugHook;
  } {
    return globalThis as typeof globalThis & {
      __voStudioBrowserSmoke?: StudioBrowserSmokeDebugHook;
    };
  }

  function clearStudioBrowserSmokeDebugHook(): void {
    delete studioBrowserSmokeDebugGlobal().__voStudioBrowserSmoke;
  }

  function refreshStudioBrowserSmokeDebugHook(): void {
    if (!registry || !studioBrowserSmokeDebugEnabled()) {
      clearStudioBrowserSmokeDebugHook();
      return;
    }
    studioBrowserSmokeDebugGlobal().__voStudioBrowserSmoke = {
      entryPath: () => sessionInfo?.entryPath ?? null,
      consoleLines: () => get(console_).lines,
      runtimeState: () => get(runtime),
      dumpCurrent: async () => {
        const entryPath = sessionInfo?.entryPath;
        if (!entryPath) {
          throw new Error('No active Studio entry path');
        }
        return dumpStudioBrowserSmokeEntry(entryPath);
      },
      dump: async (path: string) => dumpStudioBrowserSmokeEntry(path),
      quiesceRenderLoop: () => quiesceRendererBridgeForSmoke(),
      rendererState: () => rendererBridgeSmokeState(),
      rotatePerfEvidenceEpoch: () => rotateVoplayPerfEvidenceEpoch(),
    };
  }

  function dumpStudioBrowserSmokeEntry(path: string): Promise<string> {
    const backend = registry?.backend as StudioBrowserSmokeDebugBackend | undefined;
    if (!backend) {
      return Promise.reject(new Error('Studio registry is unavailable'));
    }
    if (backend.dumpGuiBytecode) {
      return backend.dumpGuiBytecode(path);
    }
    return backend.dumpVo(path);
  }

  function isLocalRunnerHost(url: URL): boolean {
    return url.hostname === 'localhost' || url.hostname === '127.0.0.1' || url.hostname === '::1' || url.hostname === '[::1]';
  }

  async function bindRunnerSession(
    nextSessionInfo: SessionInfo,
    options: { syncUrl?: boolean } = {},
  ): Promise<void> {
    if (!registry) return;
    activeBuiltinExampleId = null;
    seedDefaultStarter = false;
    sessionInfo = nextSessionInfo;
    sessionOpen(
      nextSessionInfo.root,
      'runner',
      nextSessionInfo.entryPath ?? null,
      nextSessionInfo.projectMode,
      nextSessionInfo.source,
      shareInfoForMode(nextSessionInfo, 'runner'),
    );
    if (options.syncUrl !== false) {
      syncBrowserUrlToSession(nextSessionInfo, 'runner');
    }
    refreshStudioBrowserSmokeDebugHook();
    currentDir = nextSessionInfo.root;
    explorerEntries = [];
    editorOpen('', '');
  }

  async function rollbackRunnerActivation(
    previousSession: SessionInfo | null,
    activatedSession: SessionInfo,
  ): Promise<void> {
    if (!registry) return;
    if (previousSession) {
      try {
        await registry.project.restoreSession(previousSession);
        return;
      } catch (rollbackError) {
        if (registry.project.sessionInfo === activatedSession) {
          await bindRunnerSession(activatedSession, { syncUrl: false });
        }
        throw rollbackError;
      }
    }
    await bindRunnerSession(activatedSession, { syncUrl: false });
  }

  interface PreparedDevSessionView {
    explorerEntries: FsEntry[];
    entry: { path: string; content: string } | null;
    projectHasGui: boolean;
  }

  interface DevSessionViewOptions {
    builtinExampleId?: string | null;
    isCurrent?: () => boolean;
    openEntry?: boolean;
    projectHasGui?: boolean;
    syncCatalogRoot?: boolean;
    syncUrl?: boolean;
  }

  async function prepareDevSessionView(
    nextSessionInfo: SessionInfo,
    options: DevSessionViewOptions = {},
    candidate?: PreparedSession,
  ): Promise<PreparedDevSessionView | null> {
    if (!registry) return null;
    const isCurrent = options.isCurrent ?? (() => true);
    if (!isCurrent()) return null;
    const nextProjectHasGui = options.projectHasGui
      ?? registry.projectCatalog.getSessionProjectConfig(nextSessionInfo).hasGui;
    const nextExplorerEntries = nextSessionInfo.projectMode === 'module'
      ? candidate
        ? await registry.project.listPreparedSessionDir(candidate, nextSessionInfo.root)
        : await registry.workspace.list(nextSessionInfo.root)
      : [];
    if (!isCurrent()) return null;
    const shouldOpenEntry = Boolean(
      nextSessionInfo.entryPath
      && (options.openEntry || nextSessionInfo.projectMode === 'single-file'),
    );
    const nextEntry = shouldOpenEntry && nextSessionInfo.entryPath
      ? {
          path: nextSessionInfo.entryPath,
          content: candidate
            ? await registry.project.readPreparedSessionFile(candidate, nextSessionInfo.entryPath)
            : await registry.workspace.readFile(nextSessionInfo.entryPath),
        }
      : null;
    if (!isCurrent()) return null;

    return {
      explorerEntries: nextExplorerEntries,
      entry: nextEntry,
      projectHasGui: nextProjectHasGui,
    };
  }

  function commitDevSessionView(
    nextSessionInfo: SessionInfo,
    prepared: PreparedDevSessionView,
    options: DevSessionViewOptions = {},
  ): void {
    if (!registry) return;
    activeBuiltinExampleId = options.builtinExampleId ?? null;
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
    refreshStudioBrowserSmokeDebugHook();
    sessionProjectHasGui = prepared.projectHasGui;
    ide.update((s) => ({ ...s, outputExpanded: false, previewCollapsed: false }));
    currentDir = nextSessionInfo.root;
    explorerEntries = prepared.explorerEntries;
    editorOpen(prepared.entry?.path ?? '', prepared.entry?.content ?? '');
    if (options.syncCatalogRoot) {
      void registry.projectCatalog.setRoot(nextSessionInfo.root).catch((catalogError) => {
        consolePush('stderr', formatError(catalogError));
      });
    }
  }

  async function bindDevSession(
    nextSessionInfo: SessionInfo,
    options: DevSessionViewOptions = {},
  ): Promise<void> {
    const prepared = await prepareDevSessionView(nextSessionInfo, options);
    if (prepared) commitDevSessionView(nextSessionInfo, prepared, options);
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

  async function discardPreparedSession(candidate: PreparedSession | null): Promise<void> {
    if (!registry || !candidate) return;
    await registry.project.discardPreparedSession(candidate).catch((discardError) => {
      consolePush('stderr', formatError(discardError));
    });
  }

  async function openExample(example: StarterExample): Promise<void> {
    if (!registry) return;
    failedBuiltinExampleId = null;
    return navigateSession(async (isCurrent, commit) => {
      if (!registry) return;
      let candidate: PreparedSession | null = null;
      let activated = false;
      stopRuntimePolling();
      await registry.runtime.stop();
      if (!isCurrent()) return;
      try {
        candidate = await registry.project.prepareBuiltinExampleSession({
          id: example.id,
          entryName: example.file,
          content: example.source,
        });
        if (!isCurrent()) return;
        await registry.projectCatalog.updateSessionProjectConfig(candidate.session, example.hasGui);
        if (!isCurrent()) return;
        const prepared = await prepareDevSessionView(candidate.session, {
          builtinExampleId: example.id,
          isCurrent,
          openEntry: true,
          projectHasGui: example.hasGui,
          syncUrl: false,
        }, candidate);
        if (!prepared || !commit()) return;
        const exSession = await registry.project.activatePreparedSession(candidate);
        activated = true;
        commitDevSessionView(exSession, prepared, {
          builtinExampleId: example.id,
          openEntry: true,
          projectHasGui: example.hasGui,
          syncUrl: false,
        });
        seedDefaultStarter = false;
        failedBuiltinExampleId = null;
        if (isCurrent()) {
          setExampleHash(example.id);
          ide.update((s) => ({ ...s, appMode: 'develop' }));
        }
      } finally {
        if (!activated) await discardPreparedSession(candidate);
      }
    });
  }

  async function ensureDevelopEntry(
    activeRegistry: ServiceRegistry | null,
    currentLoading: string,
    currentSession: SessionInfo | null,
    requestedExampleId: string | null,
    isOpening: boolean,
    currentExampleId: string | null,
    shouldSeedDefault: boolean,
    currentFailureId: string | null,
  ): Promise<void> {
    if (!activeRegistry || currentLoading || isOpening || !defaultStarter) return;
    if (requestedExampleId && currentExampleId === requestedExampleId) return;
    if (!requestedExampleId && !shouldSeedDefault && currentSession?.entryPath) return;
    if (!requestedExampleId && currentExampleId) return;
    const requestedExample = requestedExampleId
      ? STARTER_EXAMPLES.find((example) => example.id === requestedExampleId)
      : defaultStarter;
    const example = requestedExample ?? defaultStarter;
    if (currentFailureId === example.id) return;
    openingDefaultStarter = true;
    try {
      if (!requestedExample && requestedExampleId) {
        setExampleHash(defaultStarter.id, { replace: true });
      }
      await openExample(example);
    } catch (openError) {
      seedDefaultStarter = false;
      failedBuiltinExampleId = example.id;
      consolePush('stderr', formatError(openError));
    } finally {
      openingDefaultStarter = false;
    }
  }

  async function openManagedProject(project: ManagedProject): Promise<void> {
    if (!registry || !project.localPath) return;
    return navigateSession(async (isCurrent, commit) => {
      if (!registry || !project.localPath) return;
      let candidate: PreparedSession | null = null;
      let activated = false;
      stopRuntimePolling();
      await registry.runtime.stop();
      if (!isCurrent()) return;
      const openPath = project.type === 'single'
        ? (project.entryPath ?? project.localPath)
        : project.localPath;
      try {
        candidate = await registry.project.prepareSession({ proj: openPath, mode: 'dev' });
        if (!isCurrent()) return;
        const prepared = await prepareDevSessionView(
          candidate.session,
          { isCurrent, openEntry: true },
          candidate,
        );
        if (!prepared || !isCurrent()) return;
        registry.projectCatalog.trackRecentSessionTarget(openPath, candidate.session);
        if (!commit()) return;
        const openedSession = await registry.project.activatePreparedSession(candidate);
        activated = true;
        seedDefaultStarter = false;
        commitDevSessionView(openedSession, prepared, {
          openEntry: true,
          syncUrl: isCurrent(),
        });
        if (isCurrent()) ide.update((s) => ({ ...s, appMode: 'develop' }));
      } finally {
        if (!activated) await discardPreparedSession(candidate);
      }
    });
  }

  async function openLocalPath(path: string): Promise<void> {
    if (!registry) return;
    return navigateSession(async (isCurrent, commit) => {
      if (!registry) return;
      let candidate: PreparedSession | null = null;
      let activated = false;
      stopRuntimePolling();
      await registry.runtime.stop();
      if (!isCurrent()) return;
      try {
        candidate = await registry.project.prepareSession({ proj: path, mode: 'dev' });
        if (!isCurrent()) return;
        const prepared = await prepareDevSessionView(
          candidate.session,
          { isCurrent, openEntry: true },
          candidate,
        );
        if (!prepared || !isCurrent()) return;
        registry.projectCatalog.trackRecentSessionTarget(path, candidate.session);
        if (!commit()) return;
        const openedSession = await registry.project.activatePreparedSession(candidate);
        activated = true;
        seedDefaultStarter = false;
        commitDevSessionView(openedSession, prepared, {
          openEntry: true,
          syncUrl: isCurrent(),
        });
        if (isCurrent()) ide.update((s) => ({ ...s, appMode: 'develop' }));
      } finally {
        if (!activated) await discardPreparedSession(candidate);
      }
    });
  }

  async function openFeaturedProject(url: string, hasGui: boolean): Promise<void> {
    if (!registry) return;
    return navigateSession(async (isCurrent, commit) => {
      if (!registry) return;
      let candidate: PreparedSession | null = null;
      let activated = false;
      stopRuntimePolling();
      await registry.runtime.stop();
      if (!isCurrent()) return;
      try {
        candidate = await registry.project.prepareSession({ proj: url, mode: 'dev' });
        if (!isCurrent()) return;
        await registry.projectCatalog.updateSessionProjectConfig(candidate.session, hasGui);
        if (!isCurrent()) return;
        const prepared = await prepareDevSessionView(
          candidate.session,
          { isCurrent, openEntry: true, projectHasGui: hasGui },
          candidate,
        );
        if (!prepared || !commit()) return;
        const openedSession = await registry.project.activatePreparedSession(candidate);
        activated = true;
        seedDefaultStarter = false;
        commitDevSessionView(openedSession, prepared, {
          openEntry: true,
          projectHasGui: hasGui,
          syncUrl: isCurrent(),
        });
        if (isCurrent()) ide.update((s) => ({ ...s, appMode: 'develop' }));
      } finally {
        if (!activated) await discardPreparedSession(candidate);
      }
    });
  }

  async function playFeaturedProject(url: string, hasGui: boolean): Promise<void> {
    if (!registry || !hasGui) return;
    return navigateSession(async (isCurrent, commit) => {
      if (!registry) return;
      let candidate: PreparedSession | null = null;
      let activated = false;
      stopRuntimePolling();
      await registry.runtime.stop();
      if (!isCurrent()) return;
      const previousSession = registry.project.sessionInfo;
      try {
        candidate = await registry.project.prepareSession({ proj: url, mode: 'runner' });
        if (!isCurrent()) return;
        if (!candidate.session.entryPath) {
          throw new Error('This project has no runnable entry file.');
        }
        if (!commit()) return;
        const openedSession = await registry.project.activatePreparedSession(candidate);
        activated = true;
        if (!isCurrent()) {
          await rollbackRunnerActivation(previousSession, openedSession);
          return;
        }
        let guiStarted = false;
        try {
          await startGui(openedSession.entryPath!);
          guiStarted = true;
        } catch (runError) {
          await registry.runtime.stop().catch(() => undefined);
          try {
            await rollbackRunnerActivation(previousSession, openedSession);
          } catch (rollbackError) {
            throw new Error(
              `Runner start failed (${formatError(runError)}); restoring the previous session also failed (${formatError(rollbackError)})`,
            );
          }
          throw runError;
        }
        const stillLatest = isCurrent();
        if (!stillLatest) {
          await registry.runtime.stop().catch(() => undefined);
          await rollbackRunnerActivation(previousSession, openedSession);
          return;
        }
        await bindRunnerSession(openedSession);
        seedDefaultStarter = false;
        ide.update((s) => ({ ...s, appMode: 'runner' }));
        if (guiStarted) startRuntimePolling();
      } finally {
        if (!activated) await discardPreparedSession(candidate);
      }
    });
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
    const activeRegistry = registry;
    if (!activeRegistry) return;
    confirmDialog = {
      title: 'Disconnect GitHub',
      message: 'Sign out of GitHub? Remote project metadata will no longer be synced.',
      danger: false,
      action: () => {
        confirmDialog = null;
        void activeRegistry.projectCatalog.disconnectGitHub().catch((connectError) => {
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

  async function exitRunner(): Promise<void> {
    await stopCode();
    ide.update((state) => ({ ...state, appMode: 'manage', outputExpanded: false }));
  }

  function navigateMode(mode: AppMode): void {
    sessionNavigation.invalidate();
    ide.update((state) => ({ ...state, appMode: mode }));
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
  $: void ensureModeView(appMode);
  $: if (appMode === 'develop') {
    void ensureDevelopEntry(
      registry,
      loading,
      sessionInfo,
      $route.exampleId,
      openingDefaultStarter,
      activeBuiltinExampleId,
      seedDefaultStarter,
      failedBuiltinExampleId,
    );
  }
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
  $: pageTitle = appMode === 'manage'
    ? 'Vo — Code at the speed of thought'
    : appMode === 'docs'
      ? 'Documentation — Vo'
      : appMode === 'runner'
        ? `${previewTitle || 'Runner'} — Vo`
        : `${$session.entryPath?.split('/').pop() || 'Studio'} — Vo`;
</script>

<svelte:head>
  <title>{pageTitle}</title>
</svelte:head>

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
      <a class="splash-home" href="./#/">Back to home</a>
    </div>
  </div>
{:else if loading}
  <div class="splash">
    <div class="splash-card">
      <div class="splash-vo">Vo<span>Studio</span></div>
      <p class="splash-loading">{loading}</p>
    </div>
  </div>
{:else if !registry}
  <div class="splash">
    <div class="splash-card error">
      <div class="splash-vo">Vo<span>Studio</span></div>
      <p class="splash-error">Studio services are unavailable.</p>
    </div>
  </div>
{:else if appMode === 'runner'}
  {#if RunnerModeView}
    <svelte:component this={RunnerModeView} {registry} onExit={() => void exitRunner()} />
  {:else}
    <div class="route-loading" role="status">Loading runner…</div>
  {/if}
{:else}
  <div class="app">
    <Sidebar
      {githubStore}
      onConnectGitHub={openGitHubConnectModal}
      onDisconnectGitHub={disconnectGitHub}
      onNavigate={(mode: AppMode) => sessionNavigation.invalidate()}
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
          onDocs={() => navigateMode('docs')}
          onConnectGitHub={openGitHubConnectModal}
        />

      {:else if appMode === 'develop'}
        {#if DevWorkbenchView}
          <svelte:component
            this={DevWorkbenchView}
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
            onSetProjectHasGui={(hasGui: boolean) => void setSessionProjectHasGui(hasGui)}
            onOpenEntry={openEntry}
            onGoParent={goParent}
            onExitFullscreen={exitFullscreen}
            onTogglePreviewCollapsed={() => ide.update((s) => ({ ...s, previewCollapsed: !s.previewCollapsed }))}
          />
        {:else}
          <div class="route-loading" role="status">Loading editor…</div>
        {/if}

      {:else if appMode === 'docs'}
        {#if DocsPanelView}
          <svelte:component this={DocsPanelView} />
        {:else}
          <div class="route-loading" role="status">Loading documentation…</div>
        {/if}
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
  /* Splash */
  .splash {
    display: grid;
    place-items: center;
    min-height: 100dvh;
    padding: 24px;
    background:
      radial-gradient(circle at 50% 20%, rgba(124, 140, 255, 0.16), transparent 24rem),
      var(--surface-canvas);
  }
  .splash-card {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 16px;
    width: min(100%, 420px);
    padding: 42px 36px;
    border-radius: var(--radius-lg);
    background: color-mix(in srgb, var(--surface-raised) 82%, transparent);
    border: 1px solid var(--line);
    box-shadow: var(--shadow-xl);
  }
  .splash-card.error { border-color: color-mix(in srgb, var(--danger) 55%, transparent); }
  .splash-vo { font-size: 36px; font-weight: 900; color: var(--accent-soft); letter-spacing: -0.04em; }
  .splash-vo span { font-weight: 350; color: var(--text-muted); margin-left: 8px; font-size: 26px; }
  .splash-loading { color: var(--text-muted); font-size: 13px; margin: 0; }
  .splash-error { color: var(--danger); font-size: 13px; line-height: 1.55; margin: 0; max-width: 400px; text-align: center; }
  .splash-home {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    min-height: 38px;
    padding: 0 16px;
    border: 1px solid var(--line-strong);
    border-radius: 10px;
    color: var(--text-strong);
    background: var(--surface-hover);
    font-size: 12px;
    font-weight: 700;
    text-decoration: none;
  }
  .splash-home:hover { border-color: var(--accent); background: var(--accent-dim); }

  /* App layout */
  .app { display: flex; height: 100%; min-height: 100dvh; overflow: hidden; }
  .main-area { flex: 1; display: flex; flex-direction: column; min-width: 0; overflow: hidden; }
  .route-loading {
    flex: 1;
    display: grid;
    place-items: center;
    min-height: 0;
    color: var(--text-muted);
    background: var(--surface-canvas);
    font-size: 12px;
  }

  .confirm-backdrop {
    position: fixed;
    inset: 0;
    background: rgba(2, 4, 10, 0.76);
    backdrop-filter: blur(6px);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 210;
  }
  .confirm-modal {
    width: min(100%, 380px);
    background: var(--surface-raised);
    border: 1px solid var(--line);
    border-radius: 16px;
    box-shadow: 0 24px 64px rgba(0, 0, 0, 0.55);
    padding: 22px;
    display: flex;
    flex-direction: column;
    gap: 12px;
  }
  .confirm-modal h3 {
    margin: 0;
    color: var(--text-strong);
    font-size: 16px;
    font-weight: 700;
  }
  .confirm-modal h3.danger {
    color: var(--danger);
  }
  .confirm-msg {
    margin: 0;
    color: var(--text-muted);
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
    background: var(--accent);
    color: var(--text-strong);
  }
  .secondary {
    background: var(--surface-hover);
    color: var(--text);
  }
  .danger-btn {
    background: var(--danger);
    color: var(--surface-canvas);
  }

  @media (max-width: 720px) {
    .main-area { padding-bottom: calc(66px + env(safe-area-inset-bottom)); }
    .confirm-modal { width: calc(100% - 32px); max-height: calc(100dvh - 96px); overflow-y: auto; }
  }
</style>
