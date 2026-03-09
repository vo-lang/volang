import { get } from 'svelte/store';
import { ide } from '../../stores/ide';
import type { EditTarget, ProjectMode } from '../../stores/ide';
import { explorer } from '../../stores/explorer';
import { termSetCwd } from '../../stores/terminal';
import { bridge } from '../bridge';
import { dirname } from '../path_utils';
import { resolveWorkspaceOpenTarget } from '../workspace_target';
import { loadDir, openFile } from './fs';

// =============================================================================
// Workspace opening and project init actions
// =============================================================================

function findFirstVoFile(
  entries: { name: string; path: string; isDir: boolean }[],
  dirCache: Record<string, { name: string; path: string; isDir: boolean }[]>,
): string | null {
  for (const e of entries) {
    if (!e.isDir && e.name.endsWith('.vo')) return e.path;
  }
  for (const e of entries) {
    if (e.isDir) {
      const children = dirCache[e.path];
      if (children) {
        const found = findFirstVoFile(children, dirCache);
        if (found) return found;
      }
    }
  }
  return null;
}

function findRunEntryFile(
  entries: { name: string; path: string; isDir: boolean }[],
  dirCache: Record<string, { name: string; path: string; isDir: boolean }[]>,
): string | null {
  const mainFile = entries.find(e => !e.isDir && e.name === 'main.vo');
  if (mainFile) return mainFile.path;
  return findFirstVoFile(entries, dirCache);
}

async function expandParentDirs(projectRoot: string, filePath: string): Promise<Set<string>> {
  const expandedDirs = new Set<string>();
  if (!filePath.startsWith(projectRoot + '/')) return expandedDirs;

  const relPath = filePath.slice(projectRoot.length + 1);
  const segments = relPath.split('/');
  let cur = projectRoot;
  for (let i = 0; i < segments.length - 1; i++) {
    cur += '/' + segments[i];
    await loadDir(cur);
    expandedDirs.add(cur);
  }
  return expandedDirs;
}

async function tryStat(path: string): Promise<{ name: string; path: string; isDir: boolean } | null> {
  try {
    return await bridge().fsStat(path);
  } catch {
    return null;
  }
}

export async function openWorkspaceTarget(targetPath: string, entryPath?: string): Promise<void> {
  const target = await resolveWorkspaceOpenTarget(targetPath, entryPath, {
    listDir: loadDir,
    stat: (path: string) => bridge().fsStat(path),
    tryStat,
  });

  if (target.kind === 'project') {
    await openProject(target.voModPath, target.entryPath);
    return;
  }

  await openSingleFile(target.filePath);
}

export async function initWorkspace(): Promise<void> {
  const b = bridge();
  const root = b.workspaceRoot;

  ide.update(s => ({ ...s, workspaceRoot: root }));
  explorer.update(e => ({ ...e, localRoot: root, explorerCwd: root }));
  const rootEntries = await loadDir(root);

  const isMulti = rootEntries.some(e => !e.isDir && e.name === 'vo.mod');
  const mode: ProjectMode = isMulti ? 'multi' : 'single';
  const editTarget: EditTarget = { mode, workspaceRoot: root };
  ide.update(s => ({ ...s, projectMode: mode, editTarget }));

  if (isMulti) {
    const expandedDirs = new Set<string>();
    for (const e of rootEntries) {
      if (e.isDir) {
        await loadDir(e.path);
        expandedDirs.add(e.path);
      }
    }
    ide.update(s => ({ ...s, expandedDirs }));
  }

  const s = get(ide);
  const firstFile = findFirstVoFile(rootEntries, s.dirCache);
  const runEntryPath = mode === 'multi'
    ? findRunEntryFile(rootEntries, s.dirCache)
    : firstFile;
  ide.update(st => ({ ...st, runEntryPath: runEntryPath ?? '' }));
  if (firstFile) {
    await openFile(firstFile);
  }
}

export async function openSingleFile(path: string): Promise<void> {
  const workspaceRoot = dirname(path);
  const editTarget: EditTarget = { mode: 'single', workspaceRoot };
  ide.update(s => ({ ...s, workspaceRoot, projectMode: 'single', editTarget, runEntryPath: path }));
  termSetCwd(workspaceRoot);
  await openFile(path);
  explorer.update(e => ({ ...e, explorerCwd: workspaceRoot, appMode: 'develop' }));
}

export async function openProject(voModPath: string, entryPath?: string): Promise<void> {
  const projectRoot = dirname(voModPath);
  const editTarget: EditTarget = { mode: 'multi', workspaceRoot: projectRoot };
  ide.update(s => ({ ...s, workspaceRoot: projectRoot, editTarget }));
  termSetCwd(projectRoot);
  explorer.update(e => ({ ...e, explorerCwd: projectRoot }));
  const rootEntries = await loadDir(projectRoot);

  const expandedDirs = new Set<string>();
  for (const e of rootEntries) {
    if (e.isDir) {
      await loadDir(e.path);
      expandedDirs.add(e.path);
    }
  }

  const s = get(ide);
  const resolvedEntryPath = entryPath ?? findRunEntryFile(rootEntries, s.dirCache);
  if (resolvedEntryPath) {
    const parentDirs = await expandParentDirs(projectRoot, resolvedEntryPath);
    for (const dir of parentDirs) expandedDirs.add(dir);
    await openFile(resolvedEntryPath, { preserveRunEntry: true });
  }

  ide.update(st => ({
    ...st,
    projectMode: 'multi',
    expandedDirs,
    runEntryPath: resolvedEntryPath ?? '',
  }));
  explorer.update(e => ({ ...e, appMode: 'develop' }));
}

export async function convertToMultiProject(): Promise<void> {
  const { workspaceRoot, editTarget } = get(ide);
  const modContent = `module main\n\nvo 0.1\n`;
  await bridge().fsWriteFile(workspaceRoot + '/vo.mod', modContent);
  const entries = await loadDir(workspaceRoot);
  const expandedDirs = new Set<string>();
  for (const e of entries) {
    if (e.isDir) {
      await loadDir(e.path);
      expandedDirs.add(e.path);
    }
  }
  const newTarget: EditTarget | null = editTarget
    ? { ...editTarget, mode: 'multi' }
    : null;
  ide.update(s => ({
    ...s,
    projectMode: 'multi',
    expandedDirs,
    editTarget: newTarget,
    runEntryPath: s.activeFilePath || s.runEntryPath || workspaceRoot + '/main.vo',
  }));
}
