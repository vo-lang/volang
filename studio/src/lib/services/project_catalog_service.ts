import { get, writable, type Readable } from 'svelte/store';
import { editor, editorMarkSaved } from '../../stores/editor';
import { collectLocalProjectFiles, discoverWorkspaceProjects } from '../project_catalog/discovery';
import { addRecentProject, loadRecentProjects, recentProjectIdentity, type RecentProject } from '../project_catalog/recent';
import {
  defaultProjectConfig,
  deleteStoredProjectConfig,
  moveStoredProjectConfig,
  projectConfigKey,
  readStoredProjectConfig,
  sessionProjectConfigKey,
  writeStoredProjectConfig,
} from '../project_catalog/config';
import {
  GitHubRemoteClient,
  buildManifest,
  manifestKey,
} from '../project_catalog/github_remote';
import type {
  GitHubAccountState,
  ManifestProject,
  ManagedProject,
  ManagedProjectConfig,
  ProjectCatalogState,
  ProjectDiffEntry,
  ProjectDiffResult,
  RemoteMetadata,
} from '../project_catalog/types';
import { hashContent, hashFiles, projectKey, syncStateFromHashes } from '../project_catalog/types';
import type { Backend } from '../backend/backend';
import { buildGitHubRepoShareInfo } from '../session_share';
import type { SessionInfo, ShareInfo } from '../types';
import type { WorkspaceService } from './workspace_service';
import { formatError } from '../format_error';

function yieldToUI(): Promise<void> {
  return new Promise((resolve) => requestAnimationFrame(() => setTimeout(resolve, 0)));
}

export class ProjectCatalogService {
  readonly github: Readable<GitHubAccountState>;
  readonly catalog: Readable<ProjectCatalogState>;

  private readonly githubStore = writable<GitHubAccountState>({
    token: null,
    user: null,
    connecting: false,
    error: '',
  });

  private readonly catalogStore = writable<ProjectCatalogState>({
    root: '',
    projects: [],
    loading: false,
    remoteLoading: false,
    refreshing: false,
    error: '',
    busyKeys: [],
    checkingSyncKeys: [],
    manifestGistId: null,
  });

  private readonly remote: GitHubRemoteClient;
  private refreshGeneration = 0;
  private manifestPersistQueue: Promise<void> = Promise.resolve();

  constructor(
    private readonly backend: Backend,
    private readonly workspace: WorkspaceService,
  ) {
    this.github = { subscribe: this.githubStore.subscribe };
    this.catalog = { subscribe: this.catalogStore.subscribe };
    this.remote = new GitHubRemoteClient(backend);
  }

  async initialize(root: string): Promise<void> {
    const generation = ++this.refreshGeneration;
    const previousProjects = this.catalogSnapshot().projects;
    this.catalogStore.update((state) => ({ ...state, root, loading: true, error: '', checkingSyncKeys: [] }));
    const token = this.remote.loadStoredToken();
    if (!token) {
      await this.refresh(root);
      return;
    }

    this.githubStore.update((state) => ({ ...state, token, connecting: true, error: '' }));
    const [localProjects] = await Promise.all([
      discoverWorkspaceProjects(this.workspace)
        .then((projects) => mergeRecentIntoLocal(projects, root))
        .then((projects) => this.applyStoredConfigToLocals(projects, previousProjects))
        .catch((): ManagedProject[] => []),
      this.remote.getViewer(token).then(
        (user) => this.githubStore.set({ token, user, connecting: false, error: '' }),
        (error) => {
          this.remote.storeToken(null);
          this.githubStore.set({ token: null, user: null, connecting: false, error: formatError(error) });
        },
      ),
    ]);
    if (generation !== this.refreshGeneration) return;

    const github = this.githubSnapshot();
    if (!github.token || !github.user) {
      this.catalogStore.update((state) => ({
        ...state,
        projects: localProjects,
        manifestGistId: null,
        loading: false,
        refreshing: false,
        remoteLoading: false,
      }));
      return;
    }

    this.catalogStore.update((state) => ({
      ...state,
      projects: previewProjects(localProjects, previousProjects),
      loading: false,
      refreshing: false,
      remoteLoading: true,
    }));
    await yieldToUI();
    if (generation !== this.refreshGeneration) return;
    await this.refreshRemote(localProjects, generation, previousProjects);
  }

  async setRoot(root: string): Promise<void> {
    this.catalogStore.update((state) => ({ ...state, root }));
    await this.refresh(root);
  }

  async connectGitHub(token: string): Promise<void> {
    const trimmed = token.trim();
    if (!trimmed) throw new Error('GitHub token is required');
    this.githubStore.update((state) => ({ ...state, token: trimmed, connecting: true, error: '' }));
    try {
      const user = await this.remote.getViewer(trimmed);
      this.remote.storeToken(trimmed);
      this.githubStore.set({ token: trimmed, user, connecting: false, error: '' });
      await this.refresh();
    } catch (error) {
      this.githubStore.set({ token: null, user: null, connecting: false, error: formatError(error) });
      throw error;
    }
  }

  async disconnectGitHub(): Promise<void> {
    this.remote.storeToken(null);
    this.githubStore.set({ token: null, user: null, connecting: false, error: '' });
    this.catalogStore.update((state) => ({ ...state, manifestGistId: null }));
    await this.refresh();
  }

  async refresh(root = this.catalogSnapshot().root): Promise<void> {
    if (!root) return;
    const generation = ++this.refreshGeneration;
    const previousProjects = this.catalogSnapshot().projects;
    const hadProjects = previousProjects.length > 0;
    this.catalogStore.update((state) => ({
      ...state,
      root,
      loading: !hadProjects,
      refreshing: hadProjects,
      remoteLoading: false,
      error: '',
      checkingSyncKeys: [],
    }));

    await yieldToUI();
    if (generation !== this.refreshGeneration) return;

    let localProjects: ManagedProject[];
    try {
      const discovered = await discoverWorkspaceProjects(this.workspace);
      localProjects = this.applyStoredConfigToLocals(
        mergeRecentIntoLocal(discovered, root),
        previousProjects,
      );
    } catch (error) {
      if (generation !== this.refreshGeneration) return;
      this.catalogStore.update((state) => ({
        ...state,
        loading: false,
        refreshing: false,
        remoteLoading: false,
        error: formatError(error),
      }));
      return;
    }
    if (generation !== this.refreshGeneration) return;

    const github = this.githubSnapshot();
    if (!github.token || !github.user) {
      this.catalogStore.update((state) => ({
        ...state,
        projects: localProjects,
        manifestGistId: null,
        loading: false,
        refreshing: false,
        remoteLoading: false,
      }));
      return;
    }

    this.catalogStore.update((state) => hadProjects ? ({
      ...state,
      loading: false,
      refreshing: false,
      remoteLoading: true,
    }) : ({
      ...state,
      projects: previewProjects(localProjects, previousProjects),
      loading: false,
      refreshing: false,
      remoteLoading: true,
    }));
    await yieldToUI();
    if (generation !== this.refreshGeneration) return;

    await this.refreshRemote(localProjects, generation, previousProjects);
  }

  private async refreshRemote(
    localProjects: ManagedProject[],
    generation = this.refreshGeneration,
    previousProjects: ManagedProject[] = this.catalogSnapshot().projects,
  ): Promise<void> {
    const github = this.githubSnapshot();
    if (!github.token) {
      this.catalogStore.update((state) => ({ ...state, remoteLoading: false }));
      return;
    }
    try {
      const { gistId, manifest } = await this.remote.loadManifest(github.token);
      if (generation !== this.refreshGeneration) return;
      await yieldToUI();
      const remoteMetadata = await this.remote.fetchRemoteMetadata(github.token, manifest.projects);
      if (generation !== this.refreshGeneration) return;
      const merged = mergeProjects(localProjects, manifest.projects, remoteMetadata, previousProjects);
      this.catalogStore.update((state) => ({
        ...state,
        manifestGistId: gistId,
        projects: merged,
        remoteLoading: false,
        error: '',
      }));
      void this.verifySyncStatusInBackground(merged, generation);
    } catch (error) {
      if (generation !== this.refreshGeneration) return;
      this.catalogStore.update((state) => ({
        ...state,
        remoteLoading: false,
        error: formatError(error),
      }));
    }
  }

  async createSingleProject(name: string, location?: string): Promise<ManagedProject> {
    const projectName = assertProjectName(name);
    const root = location ?? this.requireRoot();
    const filePath = `${root}/${projectName}.vo`;
    const isExternal = location != null && normalizePath(root) !== normalizePath(this.catalogSnapshot().root);
    const shouldBypassWrite = normalizePath(root) !== normalizePath(this.workspace.root);
    if (shouldBypassWrite) {
      await this.backend.createProjectFiles([{ path: filePath, content: singleFileTemplate(projectName) }]);
    } else {
      await this.workspace.writeFile(filePath, singleFileTemplate(projectName));
    }
    const project: ManagedProject = {
      name: projectName,
      type: 'single',
      localPath: filePath,
      entryPath: filePath,
      remote: null,
      pushedAt: null,
      remoteUpdatedAt: null,
      syncedHash: null,
      currentLocalHash: null,
      currentRemoteHash: null,
      hasGui: false,
    };
    writeStoredProjectConfig(projectConfigKey(project), defaultProjectConfig());
    if (isExternal) {
      addRecentProject({ name: project.name, type: project.type, localPath: filePath, entryPath: filePath });
    }
    await this.refresh();
    return project;
  }

  async createModuleProject(name: string, location?: string): Promise<ManagedProject> {
    const projectName = assertProjectName(name);
    const moduleId = toVoIdentifier(projectName);
    const root = location ?? this.requireRoot();
    const dirPath = `${root}/${projectName}`;
    const isExternal = location != null && normalizePath(root) !== normalizePath(this.catalogSnapshot().root);
    const shouldBypassWrite = normalizePath(root) !== normalizePath(this.workspace.root);
    const modContent = `module ${moduleId}\n\nvo 1.0\n`;
    const mainContent = moduleTemplate(moduleId, projectName);
    if (shouldBypassWrite) {
      await this.backend.createProjectFiles([
        { path: `${dirPath}/vo.mod`, content: modContent },
        { path: `${dirPath}/main.vo`, content: mainContent },
      ]);
    } else {
      await this.workspace.mkdir(dirPath);
      await this.workspace.writeFile(`${dirPath}/vo.mod`, modContent);
      await this.workspace.writeFile(`${dirPath}/main.vo`, mainContent);
    }
    const entryPath = `${dirPath}/main.vo`;
    const project: ManagedProject = {
      name: projectName,
      type: 'module',
      localPath: dirPath,
      entryPath,
      remote: null,
      pushedAt: null,
      remoteUpdatedAt: null,
      syncedHash: null,
      currentLocalHash: null,
      currentRemoteHash: null,
      hasGui: false,
    };
    writeStoredProjectConfig(projectConfigKey(project), defaultProjectConfig());
    if (isExternal) {
      addRecentProject({ name: project.name, type: project.type, localPath: dirPath, entryPath });
    }
    await this.refresh();
    return project;
  }

  trackRecentSessionTarget(
    targetPath: string,
    sessionInfo: Pick<SessionInfo, 'projectMode' | 'root' | 'entryPath'>,
  ): void {
    const type = sessionInfo.projectMode === 'module' ? 'module' : 'single';
    const recentPath = type === 'single'
      ? (sessionInfo.entryPath ?? targetPath)
      : sessionInfo.root;
    const name = recentPath.split('/').pop() ?? recentPath;
    addRecentProject({
      name: type === 'single' ? name.replace(/\.vo$/, '') : name,
      type,
      localPath: recentPath,
      entryPath: sessionInfo.entryPath,
    });
  }

  trackRecentProject(project: { name: string; type: ManagedProject['type']; localPath: string; entryPath: string | null }): void {
    addRecentProject(project);
  }

  async getProjectShareInfo(project: ManagedProject): Promise<ShareInfo> {
    if (project.remote?.kind !== 'repo' || !project.remote.owner || !project.remote.repo) {
      return {
        canonicalUrl: '',
        shareable: false,
        reason: 'Only GitHub repo projects can be shared',
      };
    }
    const token = this.requireToken();
    const head = await this.remote.resolveRepoHead(token, project.remote.owner, project.remote.repo);
    return buildGitHubRepoShareInfo({
      owner: project.remote.owner,
      repo: project.remote.repo,
      commit: head.commit,
      mode: 'runner',
    });
  }

  getSessionProjectConfig(
    sessionInfo: Pick<SessionInfo, 'projectMode' | 'root' | 'entryPath'>,
  ): ManagedProjectConfig {
    const project = this.findProjectForSession(sessionInfo);
    if (project) {
      return { hasGui: project.hasGui };
    }
    return readStoredProjectConfig(sessionProjectConfigKey(sessionInfo)) ?? defaultProjectConfig();
  }

  async updateSessionProjectConfig(
    sessionInfo: Pick<SessionInfo, 'projectMode' | 'root' | 'entryPath'>,
    hasGui: ManagedProject['hasGui'],
  ): Promise<ManagedProjectConfig> {
    const config = { hasGui };
    writeStoredProjectConfig(sessionProjectConfigKey(sessionInfo), config);
    const project = this.findProjectForSession(sessionInfo);
    if (!project) {
      return config;
    }
    const updated = { ...project, hasGui };
    this.replaceProject(updated);
    if (updated.remote && this.githubSnapshot().token) {
      this.enqueueManifestPersist();
    }
    return { hasGui: updated.hasGui };
  }

  async ensureProjectReady(project: ManagedProject): Promise<ManagedProject> {
    if (project.localPath) return project;
    return this.pullProject(project);
  }

  async pushProject(project: ManagedProject): Promise<ManagedProject> {
    return this.withProjectBusy(project, async () => {
      const token = this.requireToken();
      const githubUser = this.githubSnapshot().user;
      if (!githubUser) throw new Error('GitHub account is not loaded');
      if (!project.localPath) throw new Error('Project has no local files to push');
      await this.flushEditorIfNeeded(project);
      const now = new Date().toISOString();
      let nextProject = project;

      if (project.type === 'single') {
        const content = await this.workspace.readFile(project.localPath);
        const filename = basename(project.localPath);
        const contentHash = hashContent(content);
        if (project.remote?.kind === 'gist' && project.remote.gistId) {
          await this.remote.updateSingleFileGist(token, project.remote.gistId, filename, content);
        } else {
          const gist = await this.remote.createGist(token, filename, content, project.name);
          nextProject = { ...project, remote: { kind: 'gist', gistId: gist.id } };
        }
        nextProject = stampProject(nextProject, contentHash, now);
      } else {
        const files = await collectLocalProjectFiles(this.workspace, project);
        const contentHash = hashFiles(files);
        if (project.remote?.kind === 'repo' && project.remote.owner && project.remote.repo) {
          await this.remote.pushRepoFiles(token, project.remote.owner, project.remote.repo, files);
        } else {
          let repo: { owner: { login: string }; name: string };
          try {
            repo = await this.remote.createRepo(token, project.name);
          } catch (error) {
            const text = formatError(error);
            if (!text.includes('already exists')) throw error;
            repo = { owner: { login: githubUser.login }, name: project.name };
          }
          await this.remote.pushRepoFiles(token, repo.owner.login, repo.name, files);
          nextProject = { ...project, remote: { kind: 'repo', owner: repo.owner.login, repo: repo.name } };
        }
        nextProject = stampProject(nextProject, contentHash, now);
      }

      this.replaceProject(nextProject);
      await this.persistManifest();
      await this.refresh();
      return this.requireProject(projectKey(nextProject));
    });
  }

  async pullProject(project: ManagedProject): Promise<ManagedProject> {
    return this.withProjectBusy(project, async () => {
      const token = this.requireToken();
      if (!project.remote) throw new Error('Project has no remote source');
      const root = this.requireRoot();
      const now = new Date().toISOString();
      let nextProject = project;
      const previousConfigKey = projectConfigKey(project);

      if (project.remote.kind === 'gist' && project.remote.gistId) {
        const files = normalizeRemoteProjectFiles(project, await this.remote.fetchGistFiles(token, project.remote.gistId));
        const targetPath = `${root}/${project.name}.vo`;
        const content = files[`${project.name}.vo`];
        if (project.localPath && project.localPath !== targetPath) {
          await removeIfExists(this.workspace, project.localPath, false);
        }
        await this.workspace.writeFile(targetPath, content);
        const contentHash = hashContent(content);
        nextProject = stampProject({
          ...project,
          localPath: targetPath,
          entryPath: targetPath,
        }, contentHash, now);
      } else if (project.remote.kind === 'repo' && project.remote.owner && project.remote.repo) {
        const files = await this.remote.pullRepoFiles(token, project.remote.owner, project.remote.repo);
        const dirPath = `${root}/${project.name}`;
        if (project.localPath && project.localPath !== dirPath) {
          await removeIfExists(this.workspace, project.localPath, true);
        }
        const written = await replaceRepoFiles(this.workspace, dirPath, files);
        const contentHash = hashFiles(files);
        nextProject = stampProject({
          ...project,
          localPath: dirPath,
          entryPath: chooseModuleEntryPath(written, dirPath),
        }, contentHash, now);
      } else {
        throw new Error('Invalid remote source');
      }

      moveStoredProjectConfig(previousConfigKey, projectConfigKey(nextProject));
      this.replaceProject(nextProject);
      await this.persistManifest();
      await this.refresh();
      return this.requireProject(projectKey(nextProject));
    });
  }

  async renameProject(project: ManagedProject, newName: string): Promise<ManagedProject> {
    return this.withProjectBusy(project, async () => {
      const targetName = assertProjectName(newName);
      if (targetName === project.name) return project;
      ensureUniqueName(this.catalogSnapshot().projects, project, targetName);
      let nextProject: ManagedProject = { ...project, name: targetName };
      const previousConfigKey = projectConfigKey(project);

      if (project.remote) {
        const token = this.requireToken();
        if (project.remote.kind === 'gist' && project.remote.gistId) {
          const files = await this.remote.fetchGistFiles(token, project.remote.gistId);
          const oldFilename = chooseSingleRemoteFile(files);
          await this.remote.renameSingleFileGist(token, project.remote.gistId, oldFilename, `${targetName}.vo`, files[oldFilename]);
        } else if (project.remote.kind === 'repo' && project.remote.owner && project.remote.repo) {
          const repo = await this.remote.renameRepo(token, project.remote.owner, project.remote.repo, targetName);
          nextProject = { ...nextProject, remote: { kind: 'repo', owner: repo.owner.login, repo: repo.name } };
        }
      }

      if (project.localPath) {
        const renamed = await renameLocalProject(this.workspace, project, targetName);
        this.remapEditorPaths(project, renamed.localPath ?? project.localPath);
        nextProject = { ...nextProject, ...renamed };
      }

      moveStoredProjectConfig(previousConfigKey, projectConfigKey(nextProject));
      this.replaceProject(nextProject, projectKey(project));
      await this.persistManifest();
      await this.refresh();
      return this.requireProject(projectKey(nextProject));
    });
  }

  async deleteProject(project: ManagedProject): Promise<void> {
    await this.withProjectBusy(project, async () => {
      if (!project.remote) {
        deleteStoredProjectConfig(projectConfigKey(project));
      }
      if (project.localPath) {
        await this.workspace.remove(project.localPath, project.type === 'module');
      }

      if (project.remote) {
        this.replaceProject({
          ...project,
          localPath: null,
          entryPath: null,
          currentLocalHash: null,
        });
        await this.persistManifest();
        await this.refresh();
        return;
      }

      this.catalogStore.update((state) => ({
        ...state,
        projects: state.projects.filter((entry) => projectKey(entry) !== projectKey(project)),
      }));
    });
  }

  async deleteRemoteProject(project: ManagedProject): Promise<void> {
    await this.withProjectBusy(project, async () => {
      if (!project.remote) throw new Error('Project has no cloud copy');
      const token = this.requireToken();
      await this.remote.deleteRemoteProject(token, project.remote);

      if (project.localPath) {
        this.replaceProject({
          ...project,
          remote: null,
          pushedAt: null,
          remoteUpdatedAt: null,
          syncedHash: null,
          currentRemoteHash: null,
        });
        await this.persistManifest();
        await this.refresh();
        return;
      }

      this.catalogStore.update((state) => ({
        ...state,
        projects: state.projects.filter((entry) => projectKey(entry) !== projectKey(project)),
      }));
      await this.persistManifest();
    });
  }

  async deleteEverywhere(project: ManagedProject): Promise<void> {
    await this.withProjectBusy(project, async () => {
      deleteStoredProjectConfig(projectConfigKey(project));
      if (project.remote) {
        const token = this.requireToken();
        await this.remote.deleteRemoteProject(token, project.remote);
      }

      if (project.localPath) {
        await this.workspace.remove(project.localPath, project.type === 'module');
      }

      this.catalogStore.update((state) => ({
        ...state,
        projects: state.projects.filter((entry) => projectKey(entry) !== projectKey(project)),
      }));
      await this.persistManifest();
    });
  }

  async forgetRemoteProject(project: ManagedProject): Promise<void> {
    if (project.localPath) throw new Error('Only remote-only projects can be forgotten');
    this.catalogStore.update((state) => ({
      ...state,
      projects: state.projects.filter((entry) => projectKey(entry) !== projectKey(project)),
    }));
    await this.persistManifest();
  }

  async loadDiff(project: ManagedProject): Promise<ProjectDiffResult> {
    const token = this.requireToken();
    const [localFiles, remoteFiles] = await Promise.all([
      project.localPath
        ? collectLocalProjectFiles(this.workspace, project)
        : Promise.resolve<Record<string, string>>({}),
      this.remote.fetchRemoteContent(token, project).then((files) => normalizeRemoteProjectFiles(project, files)),
    ]);
    const allPaths = new Set([...Object.keys(localFiles), ...Object.keys(remoteFiles)]);
    const entries: ProjectDiffEntry[] = [...allPaths].sort().map((path) => {
      const localContent = localFiles[path] ?? null;
      const remoteContent = remoteFiles[path] ?? null;
      const status = localContent == null
        ? 'removed'
        : remoteContent == null
          ? 'added'
          : localContent === remoteContent
            ? 'unchanged'
            : 'modified';
      return { path, localContent, remoteContent, status };
    });
    const currentLocalHash = project.localPath ? hashFiles(localFiles) : null;
    const currentRemoteHash = project.remote ? hashFiles(remoteFiles) : null;
    return {
      project,
      syncState: syncStateFromHashes(project, currentLocalHash, currentRemoteHash),
      entries,
    };
  }

  private async persistManifest(): Promise<void> {
    const token = this.githubSnapshot().token;
    if (!token) return;
    let gistId = this.catalogSnapshot().manifestGistId;
    if (!gistId) {
      const loaded = await this.remote.loadManifest(token);
      gistId = loaded.gistId;
      this.catalogStore.update((state) => ({ ...state, manifestGistId: gistId }));
    }
    await this.remote.saveManifest(token, gistId, buildManifest(this.catalogSnapshot().projects));
  }

  private enqueueManifestPersist(): void {
    this.manifestPersistQueue = this.manifestPersistQueue
      .catch(() => undefined)
      .then(async () => {
        try {
          await this.persistManifest();
        } catch (error) {
          this.catalogStore.update((state) => ({
            ...state,
            error: formatError(error),
          }));
        }
      });
  }

  private async flushEditorIfNeeded(project: ManagedProject): Promise<void> {
    const current = get(editor);
    if (!current.dirty || !current.activeFilePath) return;
    const matches = project.type === 'single'
      ? current.activeFilePath === project.localPath
      : project.localPath != null && current.activeFilePath.startsWith(`${project.localPath}/`);
    if (!matches) return;
    await this.workspace.writeFile(current.activeFilePath, current.code);
    editorMarkSaved();
  }

  private replaceProject(project: ManagedProject, previousKey = projectKey(project)): void {
    this.catalogStore.update((state) => ({
      ...state,
      projects: state.projects.some((entry) => projectKey(entry) === previousKey)
        ? state.projects.map((entry) => projectKey(entry) === previousKey ? project : entry)
        : [...state.projects, project],
    }));
  }

  private remapEditorPaths(project: ManagedProject, nextLocalPath: string): void {
    const current = get(editor);
    if (!current.activeFilePath) return;
    if (project.type === 'single' && current.activeFilePath === project.localPath) {
      editor.set({ ...current, activeFilePath: nextLocalPath });
      return;
    }
    if (project.type === 'module' && project.localPath && current.activeFilePath.startsWith(`${project.localPath}/`)) {
      const suffix = current.activeFilePath.slice(project.localPath.length);
      editor.set({ ...current, activeFilePath: `${nextLocalPath}${suffix}` });
    }
  }

  private async withProjectBusy<T>(project: ManagedProject, task: () => Promise<T>): Promise<T> {
    const key = projectKey(project);
    this.catalogStore.update((state) => ({ ...state, busyKeys: [...state.busyKeys, key] }));
    try {
      return await task();
    } finally {
      this.catalogStore.update((state) => ({
        ...state,
        busyKeys: state.busyKeys.filter((value) => value !== key),
      }));
    }
  }

  private requireProject(key: string): ManagedProject {
    const project = this.catalogSnapshot().projects.find((entry) => projectKey(entry) === key);
    if (!project) throw new Error(`Project not found: ${key}`);
    return project;
  }

  private requireToken(): string {
    const token = this.githubSnapshot().token;
    if (!token) throw new Error('GitHub token is required');
    return token;
  }

  private requireRoot(): string {
    const root = this.catalogSnapshot().root;
    if (!root) throw new Error('Project catalog root is not ready');
    return root;
  }

  private githubSnapshot(): GitHubAccountState {
    return get(this.githubStore);
  }

  private catalogSnapshot(): ProjectCatalogState {
    return get(this.catalogStore);
  }

  private async verifySyncStatusInBackground(
    projects: ManagedProject[],
    generation = this.refreshGeneration,
  ): Promise<void> {
    const candidateKeys = projects
      .filter((project) => project.remote && project.localPath)
      .map((project) => projectKey(project));
    if (candidateKeys.length === 0) return;

    this.catalogStore.update((state) => ({
      ...state,
      checkingSyncKeys: [...new Set([...state.checkingSyncKeys, ...candidateKeys])],
    }));

    for (const key of candidateKeys) {
      if (generation !== this.refreshGeneration) {
        this.catalogStore.update((state) => ({
          ...state,
          checkingSyncKeys: state.checkingSyncKeys.filter((value) => !candidateKeys.includes(value)),
        }));
        return;
      }

      const project = this.catalogSnapshot().projects.find((entry) => projectKey(entry) === key);
      if (!project?.remote || !project.localPath) {
        this.catalogStore.update((state) => ({
          ...state,
          checkingSyncKeys: state.checkingSyncKeys.filter((value) => value !== key),
        }));
        continue;
      }

      try {
        const currentLocalHash = await this.computeLocalProjectHash(project);
        if (generation !== this.refreshGeneration) return;
        this.catalogStore.update((state) => ({
          ...state,
          projects: state.projects.map((entry) => projectKey(entry) === key
            ? {
                ...entry,
                currentLocalHash,
                currentRemoteHash: entry.currentRemoteHash ?? entry.syncedHash ?? null,
              }
            : entry),
          checkingSyncKeys: state.checkingSyncKeys.filter((value) => value !== key),
        }));
      } catch {
        this.catalogStore.update((state) => ({
          ...state,
          checkingSyncKeys: state.checkingSyncKeys.filter((value) => value !== key),
        }));
      }
      await yieldToUI();
    }
  }

  private async computeLocalProjectHash(project: ManagedProject): Promise<string> {
    if (!project.localPath) throw new Error('Project has no local files');
    if (project.type === 'single') {
      return hashContent(await this.workspace.readFile(project.localPath));
    }
    return hashFiles(await collectLocalProjectFiles(this.workspace, project));
  }

  private applyStoredConfigToLocals(
    projects: ManagedProject[],
    previousProjects: ManagedProject[],
  ): ManagedProject[] {
    const previousByKey = new Map(previousProjects.map((project) => [projectKey(project), project] as const));
    return projects.map((project) => {
      const previousProject = previousByKey.get(projectKey(project));
      return {
        ...project,
        hasGui: resolveHasGui(project, null, previousProject),
      };
    });
  }

  private findProjectForSession(
    sessionInfo: Pick<SessionInfo, 'projectMode' | 'root' | 'entryPath'>,
  ): ManagedProject | undefined {
    const sessionRoot = normalizePath(sessionInfo.root);
    const sessionEntry = sessionInfo.entryPath ? normalizePath(sessionInfo.entryPath) : null;
    return this.catalogSnapshot().projects.find((project) => {
      if (sessionInfo.projectMode === 'module') {
        return project.type === 'module'
          && project.localPath != null
          && normalizePath(project.localPath) === sessionRoot;
      }
      if (project.type !== 'single') {
        return false;
      }
      const projectEntry = project.entryPath ?? project.localPath;
      return projectEntry != null && normalizePath(projectEntry) === (sessionEntry ?? sessionRoot);
    });
  }
}

function mergeRecentIntoLocal(discovered: ManagedProject[], catalogRoot: string): ManagedProject[] {
  const recent = loadRecentProjects();
  if (recent.length === 0) return discovered;
  const normalizedRoot = normalizePath(catalogRoot);
  const discoveredIdentities = new Set(discovered.map(projectLocalIdentity));
  const merged = [...discovered];
  for (const entry of recent) {
    const identity = recentProjectIdentity(entry);
    const scopePath = normalizePath(identity);
    if (
      scopePath === normalizedRoot
      || scopePath.startsWith(`${normalizedRoot}/`)
      || discoveredIdentities.has(identity)
    ) {
      continue;
    }
    discoveredIdentities.add(identity);
    merged.push({
      name: entry.name,
      type: entry.type,
      localPath: entry.localPath,
      entryPath: entry.entryPath,
      remote: null,
      pushedAt: null,
      remoteUpdatedAt: null,
      syncedHash: null,
      currentLocalHash: null,
      currentRemoteHash: null,
      hasGui: false,
    });
  }
  return merged;
}

function projectLocalIdentity(project: Pick<ManagedProject, 'type' | 'localPath' | 'entryPath'>): string {
  if (project.type === 'module') {
    return project.localPath ?? '';
  }
  return project.entryPath ?? project.localPath ?? '';
}

function mergeProjects(
  localProjects: ManagedProject[],
  manifestProjects: ManifestProject[],
  remoteMetadata: Map<string, RemoteMetadata>,
  previousProjects: ManagedProject[],
): ManagedProject[] {
  const previousByKey = new Map(previousProjects.map((project) => [projectKey(project), project] as const));
  const matched = new Set<string>();
  const merged: ManagedProject[] = [];

  for (const localProject of localProjects) {
    const manifestProject = manifestProjects.find((entry) => entry.name === localProject.name && entry.type === localProject.type);
    if (!manifestProject) {
      merged.push(localProject);
      continue;
    }
    const key = manifestKey(manifestProject.name, manifestProject.type);
    matched.add(key);
    const metadata = remoteMetadata.get(key);
    const previousProject = previousByKey.get(key);
    merged.push({
      ...localProject,
      remote: manifestProject.remote,
      pushedAt: manifestProject.pushedAt,
      remoteUpdatedAt: metadata?.updatedAt ?? previousProject?.remoteUpdatedAt ?? null,
      syncedHash: manifestProject.contentHash ?? previousProject?.syncedHash ?? null,
      currentLocalHash: previousProject?.currentLocalHash ?? localProject.currentLocalHash ?? null,
      currentRemoteHash: previousProject?.currentRemoteHash ?? manifestProject.contentHash ?? null,
      hasGui: resolveHasGui(localProject, manifestProject, previousProject),
    });
  }

  for (const manifestProject of manifestProjects) {
    const key = manifestKey(manifestProject.name, manifestProject.type);
    if (matched.has(key)) continue;
    const metadata = remoteMetadata.get(key);
    const previousProject = previousByKey.get(key);
    merged.push({
      name: manifestProject.name,
      type: manifestProject.type,
      localPath: null,
      entryPath: manifestProject.entryPath ?? null,
      remote: manifestProject.remote,
      pushedAt: manifestProject.pushedAt,
      remoteUpdatedAt: metadata?.updatedAt ?? previousProject?.remoteUpdatedAt ?? null,
      syncedHash: manifestProject.contentHash ?? previousProject?.syncedHash ?? null,
      currentLocalHash: previousProject?.currentLocalHash ?? null,
      currentRemoteHash: previousProject?.currentRemoteHash ?? manifestProject.contentHash ?? null,
      hasGui: resolveHasGui(null, manifestProject, previousProject),
    });
  }

  return merged.sort((left, right) => left.name.localeCompare(right.name));
}

function previewProjects(localProjects: ManagedProject[], previousProjects: ManagedProject[]): ManagedProject[] {
  const previousByKey = new Map(previousProjects.map((project) => [projectKey(project), project] as const));
  const matched = new Set<string>();
  const preview: ManagedProject[] = [];

  for (const localProject of localProjects) {
    const key = projectKey(localProject);
    matched.add(key);
    const prev = previousByKey.get(key);
    if (!prev) {
      preview.push(localProject);
      continue;
    }
    if (prev.localPath === localProject.localPath && prev.entryPath === localProject.entryPath) {
      preview.push(prev);
    } else {
      preview.push({
        ...prev,
        localPath: localProject.localPath,
        entryPath: localProject.entryPath,
      });
    }
  }

  for (const prev of previousProjects) {
    const key = projectKey(prev);
    if (matched.has(key) || !prev.remote) continue;
    if (prev.localPath === null) {
      preview.push(prev);
    } else {
      preview.push({ ...prev, localPath: null, entryPath: null, currentLocalHash: null });
    }
  }

  return preview.sort((left, right) => left.name.localeCompare(right.name));
}

function stampProject(project: ManagedProject, contentHash: string, timestamp: string): ManagedProject {
  return {
    ...project,
    pushedAt: timestamp,
    remoteUpdatedAt: timestamp,
    syncedHash: contentHash,
    currentLocalHash: contentHash,
    currentRemoteHash: contentHash,
  };
}

function basename(path: string): string {
  const index = path.lastIndexOf('/');
  return index >= 0 ? path.slice(index + 1) : path;
}

function projectShareEntryPath(project: Pick<ManagedProject, 'type' | 'localPath' | 'entryPath'>): string | null {
  if (!project.entryPath) {
    return null;
  }
  if (!project.localPath) {
    return project.entryPath;
  }
  if (project.type === 'module') {
    const root = normalizePath(project.localPath);
    const entry = normalizePath(project.entryPath);
    const prefix = `${root}/`;
    return entry.startsWith(prefix) ? entry.slice(prefix.length) : null;
  }
  return basename(project.entryPath);
}

function chooseSingleRemoteFile(files: Record<string, string>): string {
  const names = Object.keys(files);
  const preferred = names.find((name) => name.endsWith('.vo')) ?? names[0];
  if (!preferred) throw new Error('Remote single-file project is empty');
  if (preferred.includes('/') || preferred.includes('\\') || preferred === '.' || preferred === '..') {
    throw new Error(`Unsafe remote filename: ${preferred}`);
  }
  return preferred;
}

async function ensureDirectory(workspace: WorkspaceService, dirPath: string): Promise<void> {
  try {
    await workspace.mkdir(dirPath);
  } catch {
    return;
  }
}

async function removeIfExists(workspace: WorkspaceService, path: string, recursive: boolean): Promise<void> {
  try {
    await workspace.remove(path, recursive);
  } catch {
    return;
  }
}

async function writeRepoFiles(workspace: WorkspaceService, dirPath: string, files: Record<string, string>): Promise<string[]> {
  const written: string[] = [];
  for (const [relativePath, content] of Object.entries(files)) {
    const safePath = assertSafeRelativeRepoPath(relativePath);
    const absolutePath = `${dirPath}/${safePath}`;
    const parts = safePath.split('/');
    let currentDir = dirPath;
    for (let i = 0; i < parts.length - 1; i += 1) {
      currentDir += `/${parts[i]}`;
      await ensureDirectory(workspace, currentDir);
    }
    await workspace.writeFile(absolutePath, content);
    written.push(safePath);
  }
  return written;
}

async function replaceRepoFiles(workspace: WorkspaceService, dirPath: string, files: Record<string, string>): Promise<string[]> {
  await removeIfExists(workspace, dirPath, true);
  await ensureDirectory(workspace, dirPath);
  return writeRepoFiles(workspace, dirPath, files);
}

function chooseModuleEntryPath(files: string[], dirPath: string): string {
  if (files.includes('main.vo')) return `${dirPath}/main.vo`;
  if (files.includes('app.vo')) return `${dirPath}/app.vo`;
  return `${dirPath}/vo.mod`;
}

function normalizeRemoteProjectFiles(
  project: Pick<ManagedProject, 'name' | 'type'>,
  files: Record<string, string>,
): Record<string, string> {
  if (project.type !== 'single') return files;
  const remoteFile = chooseSingleRemoteFile(files);
  return {
    [`${project.name}.vo`]: files[remoteFile],
  };
}

async function renameLocalProject(
  workspace: WorkspaceService,
  project: ManagedProject,
  nextName: string,
): Promise<Pick<ManagedProject, 'localPath' | 'entryPath'>> {
  if (!project.localPath) {
    return { localPath: null, entryPath: null };
  }
  const parent = project.localPath.slice(0, Math.max(project.localPath.lastIndexOf('/'), 0));
  const nextLocalPath = project.type === 'single'
    ? `${parent}/${nextName}.vo`
    : `${parent}/${nextName}`;
  await workspace.rename(project.localPath, nextLocalPath);
  return {
    localPath: nextLocalPath,
    entryPath: project.type === 'single'
      ? nextLocalPath
      : project.entryPath == null
        ? `${nextLocalPath}/vo.mod`
        : `${nextLocalPath}${project.entryPath.slice(project.localPath.length)}`,
  };
}

function assertProjectName(value: string): string {
  const trimmed = value.trim();
  if (!trimmed) throw new Error('Project name is required');
  if (trimmed.includes('/') || trimmed.includes('\\')) throw new Error('Project name cannot contain path separators');
  if (trimmed === '.' || trimmed === '..') throw new Error('Project name is invalid');
  return trimmed;
}

function assertSafeRelativeRepoPath(value: string): string {
  const trimmed = value.trim();
  if (!trimmed) throw new Error('Remote path is invalid');
  if (trimmed.startsWith('/') || trimmed.startsWith('\\') || trimmed.includes('\\')) throw new Error(`Unsafe remote path: ${trimmed}`);
  const segments = trimmed.split('/');
  if (segments.some((segment) => !segment || segment === '.' || segment === '..')) {
    throw new Error(`Unsafe remote path: ${trimmed}`);
  }
  return trimmed;
}

function ensureUniqueName(projects: ManagedProject[], current: ManagedProject, nextName: string): void {
  const exists = projects.some((project) => project.type === current.type && project.name === nextName && projectKey(project) !== projectKey(current));
  if (exists) throw new Error('A project with the same name already exists');
}

function resolveHasGui(
  localProject: ManagedProject | null,
  manifestProject: ManifestProject | null,
  previousProject: ManagedProject | undefined,
): ManagedProject['hasGui'] {
  const storedConfig = localProject ? readStoredProjectConfig(projectConfigKey(localProject)) : null;
  return storedConfig?.hasGui
    ?? manifestProject?.hasGui
    ?? previousProject?.hasGui
    ?? localProject?.hasGui
    ?? false;
}

function normalizePath(path: string): string {
  const trimmed = path.trim();
  if (!trimmed) {
    return '/';
  }
  const absolute = trimmed.startsWith('/') ? trimmed : `/${trimmed}`;
  return absolute.endsWith('/') && absolute.length > 1 ? absolute.slice(0, -1) : absolute;
}

function toVoIdentifier(value: string): string {
  const normalized = value.replace(/[^A-Za-z0-9_]+/g, '_').replace(/^\d+/, '');
  return normalized || 'main';
}

function singleFileTemplate(name: string): string {
  return `package main\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Hello, ${name}!")\n}\n`;
}

function moduleTemplate(moduleId: string, displayName: string): string {
  return `package ${moduleId}\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Hello, ${displayName}!")\n}\n`;
}

