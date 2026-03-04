import { get } from 'svelte/store';
import { ide } from '../../stores/ide';
import type { EditTarget, ProjectMode } from '../../stores/ide';
import { explorer } from '../../stores/explorer';
import { bridge } from '../bridge';
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
  if (firstFile) {
    await openFile(firstFile);
  }
}

export async function openSingleFile(path: string): Promise<void> {
  const workspaceRoot = path.substring(0, path.lastIndexOf('/'));
  const editTarget: EditTarget = { mode: 'single', workspaceRoot };
  ide.update(s => ({ ...s, workspaceRoot, projectMode: 'single', editTarget }));
  await openFile(path);
  explorer.update(e => ({ ...e, appMode: 'develop' }));
}

export async function openProject(voModPath: string): Promise<void> {
  const projectRoot = voModPath.substring(0, voModPath.lastIndexOf('/'));
  const editTarget: EditTarget = { mode: 'multi', workspaceRoot: projectRoot };
  ide.update(s => ({ ...s, workspaceRoot: projectRoot, editTarget }));
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
  const firstFile = findFirstVoFile(rootEntries, s.dirCache);
  if (firstFile) {
    await openFile(firstFile);
  }

  ide.update(s => ({ ...s, projectMode: 'multi', expandedDirs }));
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
  ide.update(s => ({ ...s, projectMode: 'multi', expandedDirs, editTarget: newTarget }));
}
