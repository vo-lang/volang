import { get } from 'svelte/store';
import { ide } from '../../stores/ide';
import { explorer } from '../../stores/explorer';
import { bridge } from '../bridge';
import type { FsEntry } from '../bridge';

// =============================================================================
// Filesystem & editor actions
// =============================================================================

export async function loadDir(dirPath: string): Promise<FsEntry[]> {
  const entries = await bridge().fsListDir(dirPath);
  ide.update(s => ({
    ...s,
    dirCache: { ...s.dirCache, [dirPath]: entries },
  }));
  return entries;
}

export async function toggleDir(dirPath: string): Promise<void> {
  const s = get(ide);
  const isExpanded = s.expandedDirs.has(dirPath);
  if (isExpanded) {
    ide.update(s => {
      const expandedDirs = new Set(s.expandedDirs);
      expandedDirs.delete(dirPath);
      return { ...s, expandedDirs };
    });
  } else {
    if (!s.dirCache[dirPath]) {
      await loadDir(dirPath);
    }
    ide.update(s => {
      const expandedDirs = new Set(s.expandedDirs);
      expandedDirs.add(dirPath);
      return { ...s, expandedDirs };
    });
  }
}

export async function openFile(filePath: string): Promise<void> {
  const s = get(ide);
  if (s.dirty && s.activeFilePath) {
    await saveFile();
  }
  const content = await bridge().fsReadFile(filePath);
  ide.update(s => ({
    ...s,
    activeFilePath: filePath,
    code: content,
    dirty: false,
    guestRender: null,
    isGuiApp: false,
    isRunning: false,
  }));
}

export function onEditorChange(code: string): void {
  ide.update(s => ({ ...s, code, dirty: true }));
}

export async function saveFile(): Promise<void> {
  const s = get(ide);
  if (!s.activeFilePath || !s.dirty) return;
  await bridge().fsWriteFile(s.activeFilePath, s.code);
  ide.update(st => ({ ...st, dirty: false }));
}

export async function createFile(dirPath: string, fileName: string): Promise<void> {
  const fullPath = dirPath + '/' + fileName;
  await bridge().fsWriteFile(fullPath, '');
  await loadDir(dirPath);
  await openFile(fullPath);
}

export async function createDir(parentPath: string, dirName: string): Promise<void> {
  const fullPath = parentPath + '/' + dirName;
  await bridge().fsMkdir(fullPath);
  await loadDir(parentPath);
}

export async function deleteEntry(path: string, isDir: boolean): Promise<void> {
  await bridge().fsRemove(path, isDir);
  const parentPath = path.substring(0, path.lastIndexOf('/')) || '/';

  ide.update(s => {
    const newCache: Record<string, FsEntry[]> = {};
    for (const [k, v] of Object.entries(s.dirCache)) {
      if (k !== path && !k.startsWith(path + '/')) newCache[k] = v;
    }
    const newExpanded = new Set([...s.expandedDirs].filter(d => d !== path && !d.startsWith(path + '/')));
    return { ...s, dirCache: newCache, expandedDirs: newExpanded };
  });

  await loadDir(parentPath);

  const s = get(ide);
  if (s.activeFilePath === path || s.activeFilePath.startsWith(path + '/')) {
    ide.update(s => ({ ...s, activeFilePath: '', code: '', dirty: false }));
  }
  explorer.update(e => ({ ...e, contextMenu: null }));
}

export async function renameEntry(oldPath: string, newName: string): Promise<void> {
  const dir = oldPath.substring(0, oldPath.lastIndexOf('/'));
  const newPath = dir + '/' + newName;
  await bridge().fsRename(oldPath, newPath);

  ide.update(s => {
    const newCache: Record<string, FsEntry[]> = {};
    for (const [k, v] of Object.entries(s.dirCache)) {
      if (k !== oldPath && !k.startsWith(oldPath + '/')) newCache[k] = v;
    }
    const newExpanded = new Set([...s.expandedDirs].filter(d => d !== oldPath && !d.startsWith(oldPath + '/')));
    return { ...s, dirCache: newCache, expandedDirs: newExpanded };
  });

  await loadDir(dir);

  const s = get(ide);
  if (s.activeFilePath === oldPath) {
    await openFile(newPath);
  } else if (s.activeFilePath.startsWith(oldPath + '/')) {
    await openFile(newPath + s.activeFilePath.slice(oldPath.length));
  }
  explorer.update(e => ({ ...e, inlineInput: null }));
}

export function startCreate(dirPath: string, isDir: boolean): void {
  ide.update(s => {
    if (s.expandedDirs.has(dirPath)) return s;
    const expandedDirs = new Set(s.expandedDirs);
    expandedDirs.add(dirPath);
    return { ...s, expandedDirs };
  });
  explorer.update(e => ({
    ...e,
    contextMenu: null,
    inlineInput: {
      mode: isDir ? 'create-dir' : 'create-file',
      dirPath,
      initialValue: '',
    },
  }));
}

export function startRename(path: string, currentName: string): void {
  const dirPath = path.substring(0, path.lastIndexOf('/'));
  explorer.update(e => ({
    ...e,
    contextMenu: null,
    inlineInput: {
      mode: 'rename',
      dirPath,
      targetPath: path,
      initialValue: currentName,
    },
  }));
}

export async function commitInlineInput(value: string): Promise<void> {
  const { inlineInput } = get(explorer);
  if (!inlineInput || !value.trim()) {
    explorer.update(e => ({ ...e, inlineInput: null }));
    return;
  }
  const v = value.trim();
  if (inlineInput.mode === 'create-file') {
    await createFile(inlineInput.dirPath, v);
  } else if (inlineInput.mode === 'create-dir') {
    await createDir(inlineInput.dirPath, v);
  } else if (inlineInput.mode === 'rename' && inlineInput.targetPath) {
    await renameEntry(inlineInput.targetPath, v);
  }
  explorer.update(e => ({ ...e, inlineInput: null }));
}

export function cancelInlineInput(): void {
  explorer.update(e => ({ ...e, inlineInput: null }));
}

export async function collectProjectFiles(dirPath: string): Promise<Record<string, string>> {
  const b = bridge();
  const result: Record<string, string> = {};

  async function walk(dir: string, prefix: string) {
    const entries = await b.fsListDir(dir);
    for (const e of entries) {
      const relPath = prefix ? prefix + '/' + e.name : e.name;
      if (e.isDir) {
        await walk(e.path, relPath);
      } else {
        result[relPath] = await b.fsReadFile(e.path);
      }
    }
  }

  await walk(dirPath, '');
  return result;
}
