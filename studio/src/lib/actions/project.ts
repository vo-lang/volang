import { get } from 'svelte/store';
import { ide } from '../../stores/ide';
import { github, githubFetch, createGist, updateGist, fetchGistFiles, createRepo, gitPushFiles, gitPullFiles, renameGistFile, renameRepo, loadManifest, saveManifest, fetchGistMetadata, fetchRepoMetadata } from '../../stores/github';
import type { RemoteMetadata } from '../../stores/github';
import { projects, persistManifest, syncState, hashContent, hashFiles, discoverLocalProjects, mergeProjects } from '../../stores/projects';
import type { ProjectEntry } from '../../stores/projects';
import { bridge } from '../bridge';
import type { FsEntry } from '../bridge';
import { loadDir, collectProjectFiles } from './fs';
import { openSingleFile, openProject } from './workspace';

// =============================================================================
// Project management actions (push/pull/open/delete/rename with GitHub remote)
// =============================================================================

function assertSafeSingleFileName(name: string): string {
  const trimmed = name.trim();
  if (!trimmed) throw new Error('Invalid remote filename');
  if (trimmed.includes('/') || trimmed.includes('\\')) {
    throw new Error(`Invalid remote filename: ${trimmed}`);
  }
  if (trimmed === '.' || trimmed === '..') {
    throw new Error(`Invalid remote filename: ${trimmed}`);
  }
  return trimmed;
}

function assertSafeRelativeRepoPath(path: string): string {
  const trimmed = path.trim();
  if (!trimmed) throw new Error('Invalid remote path');
  if (trimmed.startsWith('/') || trimmed.startsWith('\\') || trimmed.includes('\\')) {
    throw new Error(`Invalid remote path: ${trimmed}`);
  }
  const segments = trimmed.split('/');
  if (segments.some(seg => !seg || seg === '.' || seg === '..')) {
    throw new Error(`Invalid remote path: ${trimmed}`);
  }
  return trimmed;
}

export async function pushProject(project: ProjectEntry): Promise<void> {
  const { token } = get(github);
  if (!token) throw new Error('GitHub token not set');
  if (!project.localPath) throw new Error('No local files to push');

  const s = get(ide);
  if (s.dirty && s.activeFilePath) {
    await bridge().fsWriteFile(s.activeFilePath, s.code);
    ide.update(st => ({ ...st, dirty: false }));
  }

  const b = bridge();
  const now = new Date().toISOString();
  let contentHash: string;

  if (project.type === 'single') {
    const filename = project.name + '.vo';
    const content = await b.fsReadFile(project.localPath);
    contentHash = hashContent(content);

    if (project.remote?.kind === 'gist' && project.remote.gistId) {
      await updateGist(token, project.remote.gistId, filename, content);
    } else {
      const result = await createGist(token, filename, content, project.name);
      project = { ...project, remote: { kind: 'gist', gistId: result.id } };
    }
  } else {
    const files = await collectProjectFiles(project.localPath);
    contentHash = hashFiles(files);

    if (project.remote?.kind === 'repo' && project.remote.owner && project.remote.repo) {
      await gitPushFiles(token, project.remote.owner, project.remote.repo, files);
    } else {
      const { user } = get(github);
      if (!user) throw new Error('GitHub user not loaded');
      let repo: { owner: { login: string }; name: string };
      try {
        repo = await createRepo(token, project.name);
      } catch (createErr: any) {
        const msg: string = createErr?.message ?? '';
        if (msg.includes('already exists') || msg.includes('creation failed')) {
          repo = await githubFetch(token, `/repos/${user.login}/${project.name}`);
        } else {
          throw createErr;
        }
      }
      await gitPushFiles(token, repo.owner.login, repo.name, files);
      project = { ...project, remote: { kind: 'repo', owner: repo.owner.login, repo: repo.name } };
    }
  }

  project = {
    ...project,
    pushedAt: now,
    syncedHash: contentHash,
    currentLocalHash: contentHash,
    currentRemoteHash: contentHash,
  };

  projects.update(s => ({
    ...s,
    projects: s.projects.map(p =>
      p.name === project.name && p.type === project.type ? project : p
    ),
  }));
  await persistManifest();
}

export async function pullProject(project: ProjectEntry, root: string): Promise<void> {
  const { token } = get(github);
  if (!token) throw new Error('GitHub token not set');
  if (!project.remote) throw new Error('No remote source to pull from');

  const b = bridge();
  const now = new Date().toISOString();
  let contentHash: string;

  if (project.remote.kind === 'gist' && project.remote.gistId) {
    const files = await fetchGistFiles(token, project.remote.gistId);
    const fileNames = Object.keys(files);
    const voFile = fileNames.find(f => f.endsWith('.vo')) ?? fileNames[0];
    if (!voFile) throw new Error('Gist has no files');
    const safeFileName = assertSafeSingleFileName(voFile);
    const localPath = root + '/' + safeFileName;
    await b.fsWriteFile(localPath, files[voFile]);
    contentHash = hashContent(files[voFile]);
    project = { ...project, localPath };
  } else if (project.remote.kind === 'repo' && project.remote.owner && project.remote.repo) {
    const files = await gitPullFiles(token, project.remote.owner, project.remote.repo);
    const safeEntries = Object.entries(files).map(([name, content]) => [
      assertSafeRelativeRepoPath(name),
      content,
    ] as const);
    const dir = root + '/' + project.name;
    try { await b.fsMkdir(dir); } catch { /* may already exist */ }
    const createdDirs = new Set<string>();
    for (const [name] of safeEntries) {
      if (name.includes('/')) {
        const parts = name.split('/');
        let cur = dir;
        for (let i = 0; i < parts.length - 1; i++) {
          cur += '/' + parts[i];
          if (!createdDirs.has(cur)) {
            try { await b.fsMkdir(cur); } catch { /* ok */ }
            createdDirs.add(cur);
          }
        }
      }
    }
    for (const [name, content] of safeEntries) {
      await b.fsWriteFile(dir + '/' + name, content);
    }
    contentHash = hashFiles(files);
    project = { ...project, localPath: dir };
  } else {
    throw new Error('Invalid remote source');
  }

  project = {
    ...project,
    pushedAt: now,
    syncedHash: contentHash,
    currentLocalHash: contentHash,
    currentRemoteHash: contentHash,
  };

  projects.update(s => ({
    ...s,
    projects: s.projects.map(p =>
      p.name === project.name && p.type === project.type ? project : p
    ),
  }));
  await persistManifest();
}

export async function openProjectEntry(project: ProjectEntry, root: string): Promise<void> {
  const state = syncState(project);

  if (state === 'remote-only') {
    await pullProject(project, root);
    const ps = get(projects);
    const updated = ps.projects.find(p => p.name === project.name && p.type === project.type);
    if (updated) project = updated;
  }

  if (!project.localPath) throw new Error('Project has no local path after pull');

  if (project.type === 'single') {
    await openSingleFile(project.localPath);
  } else {
    await openProject(project.localPath + '/vo.mod');
  }
}

export async function deleteProject(project: ProjectEntry, root: string): Promise<void> {
  const b = bridge();
  if (project.localPath) {
    const isDir = project.type === 'multi';
    await b.fsRemove(project.localPath, isDir);
  }

  projects.update(s => ({
    ...s,
    projects: s.projects.filter(p =>
      !(p.name === project.name && p.type === project.type)
    ),
  }));

  if (project.remote) {
    await persistManifest();
  }
}

export async function renameProject(project: ProjectEntry, newName: string, root: string): Promise<void> {
  const trimmed = newName.trim();
  if (!trimmed) throw new Error('Name is required');
  if (trimmed.includes('/')) throw new Error('Name cannot contain "/"');
  if (trimmed === project.name) return;

  const ps = get(projects);
  const duplicated = ps.projects.some(
    p => p.type === project.type && p.name === trimmed,
  );
  if (duplicated) throw new Error('Project name already exists');

  let renamed: ProjectEntry = { ...project, name: trimmed };

  if (project.remote) {
    const { token } = get(github);
    if (!token) throw new Error('GitHub token not set');

    if (project.remote.kind === 'gist' && project.remote.gistId) {
      const files = await fetchGistFiles(token, project.remote.gistId);
      const names = Object.keys(files);
      const oldFilename =
        names.find(n => n === `${project.name}.vo`) ??
        names.find(n => n.endsWith('.vo')) ??
        names[0];
      if (!oldFilename) throw new Error('Gist has no files to rename');
      await renameGistFile(
        token,
        project.remote.gistId,
        oldFilename,
        `${trimmed}.vo`,
      );
    } else if (project.remote.kind === 'repo' && project.remote.owner && project.remote.repo) {
      const renamedRepo = await renameRepo(
        token,
        project.remote.owner,
        project.remote.repo,
        trimmed,
      );
      renamed = {
        ...renamed,
        remote: {
          kind: 'repo',
          owner: renamedRepo.owner.login,
          repo: renamedRepo.name,
        },
      };
    } else {
      throw new Error('Invalid remote source');
    }
  }

  if (project.localPath) {
    const localDir = project.localPath.substring(0, project.localPath.lastIndexOf('/'));
    const newLocalPath = localDir + '/' + trimmed;
    await bridge().fsRename(project.localPath, newLocalPath);
    renamed = { ...renamed, localPath: newLocalPath };

    const s = get(ide);
    const activeFilePath = s.activeFilePath.startsWith(project.localPath + '/')
      ? newLocalPath + s.activeFilePath.slice(project.localPath.length)
      : s.activeFilePath === project.localPath
        ? newLocalPath
        : s.activeFilePath;
    const workspaceRoot = s.workspaceRoot === project.localPath
      ? newLocalPath
      : s.workspaceRoot;
    const editTarget = s.editTarget && s.editTarget.workspaceRoot === project.localPath
      ? { ...s.editTarget, workspaceRoot: newLocalPath }
      : s.editTarget;

    const dirCache: Record<string, FsEntry[]> = {};
    for (const [k, v] of Object.entries(s.dirCache)) {
      if (k === project.localPath || k.startsWith(project.localPath + '/')) continue;
      dirCache[k] = v;
    }
    const expandedDirs = new Set(
      [...s.expandedDirs].filter(d => d !== project.localPath && !d.startsWith(project.localPath + '/')),
    );

    ide.update(() => ({
      ...s,
      activeFilePath,
      workspaceRoot,
      editTarget,
      dirCache,
      expandedDirs,
    }));
  }

  projects.update(s => ({
    ...s,
    projects: s.projects.map(p =>
      p.name === project.name && p.type === project.type ? renamed : p,
    ),
  }));

  if (renamed.remote) {
    await persistManifest();
  }
  await loadProjects(root);
}

export async function loadProjects(root: string): Promise<void> {
  // Phase 1: local discovery — fast, no network, show projects immediately
  projects.update(s => ({ ...s, isLoading: true, error: '' }));

  let localProjects: ProjectEntry[];
  try {
    localProjects = await discoverLocalProjects(root);
  } catch (e: any) {
    projects.update(s => ({ ...s, isLoading: false, error: String(e.message ?? e) }));
    return;
  }

  const { token } = get(github);
  if (!token) {
    projects.update(s => ({
      ...s,
      manifestGistId: null,
      manifestLoaded: false,
      projects: localProjects,
      isLoading: false,
      isRemoteLoading: false,
    }));
    return;
  }

  // Show local projects immediately, start GitHub sync in background
  projects.update(s => ({
    ...s,
    projects: localProjects,
    isLoading: false,
    isRemoteLoading: true,
  }));

  // Phase 2: GitHub sync — background, non-blocking
  try {
    const { gistId, manifest } = await loadManifest(token);

    const remoteMetaMap = new Map<string, RemoteMetadata>();
    const fetches = manifest.projects.map(async (mp) => {
      const key = mp.name + ':' + mp.type;
      try {
        let meta: RemoteMetadata;
        if (mp.remote.kind === 'gist' && mp.remote.gistId) {
          meta = await fetchGistMetadata(token, mp.remote.gistId);
        } else if (mp.remote.kind === 'repo' && mp.remote.owner && mp.remote.repo) {
          meta = await fetchRepoMetadata(token, mp.remote.owner, mp.remote.repo);
        } else {
          return;
        }
        remoteMetaMap.set(key, meta);
      } catch {
        // Network error for this project — skip remote metadata
      }
    });
    await Promise.all(fetches);

    const merged = mergeProjects(localProjects, manifest.projects, remoteMetaMap);
    projects.update(s => ({
      ...s,
      manifestGistId: gistId,
      manifestLoaded: true,
      projects: merged,
      isRemoteLoading: false,
    }));
  } catch (e: any) {
    // GitHub fetch failed — local projects already visible, just clear remote loading flag
    projects.update(s => ({ ...s, isRemoteLoading: false }));
  }
}
