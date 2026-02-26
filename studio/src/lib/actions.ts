import { get } from 'svelte/store';
import { ide } from '../stores/ide';
import type { EditTarget, ProjectMode } from '../stores/ide';
import { explorer } from '../stores/explorer';
import { github, createGist, updateGist, fetchGistFiles, createRepo, gitPushFiles, gitPullFiles } from '../stores/github';
import { projects, persistManifest, loadProjects, syncState } from '../stores/projects';
import type { ProjectEntry } from '../stores/projects';
import { bridge } from './bridge';
import type { FsEntry } from './bridge';

function isGuiCode(code: string): boolean {
  return code.includes('"vogui"');
}

export const actions = {
  // =========================================================================
  // Workspace init — called once after bridge is ready
  // =========================================================================

  async initWorkspace() {
    const b = bridge();
    const root = b.workspaceRoot;

    // localRoot is the VFS root for local browsing — fixed for the session lifetime
    ide.update(s => ({ ...s, workspaceRoot: root }));
    explorer.update(e => ({ ...e, localRoot: root, explorerCwd: root }));
    const rootEntries = await this.loadDir(root);

    // Detect single-file vs multi-file project (vo.mod present = multi)
    const isMulti = rootEntries.some(e => !e.isDir && e.name === 'vo.mod');
    const mode: ProjectMode = isMulti ? 'multi' : 'single';
    const editTarget: EditTarget = { mode, workspaceRoot: root };
    ide.update(s => ({ ...s, projectMode: mode, editTarget }));

    if (isMulti) {
      const expandedDirs: string[] = [];
      for (const e of rootEntries) {
        if (e.isDir) {
          await this.loadDir(e.path);
          expandedDirs.push(e.path);
        }
      }
      ide.update(s => ({ ...s, expandedDirs }));
    }

    const s = get(ide);
    const firstFile = findFirstVoFile(rootEntries, s.dirCache);
    if (firstFile) {
      await this.openFile(firstFile);
    }
  },

  async openSingleFile(path: string) {
    const workspaceRoot = path.substring(0, path.lastIndexOf('/'));
    const editTarget: EditTarget = { mode: 'single', workspaceRoot };
    ide.update(s => ({ ...s, workspaceRoot, projectMode: 'single', editTarget }));
    await this.openFile(path);
    explorer.update(e => ({ ...e, appMode: 'develop' }));
  },

  async openProject(voModPath: string) {
    const projectRoot = voModPath.substring(0, voModPath.lastIndexOf('/'));
    const editTarget: EditTarget = { mode: 'multi', workspaceRoot: projectRoot };
    ide.update(s => ({ ...s, workspaceRoot: projectRoot, editTarget }));
    explorer.update(e => ({ ...e, explorerCwd: projectRoot }));
    const rootEntries = await this.loadDir(projectRoot);

    const expandedDirs: string[] = [];
    for (const e of rootEntries) {
      if (e.isDir) {
        await this.loadDir(e.path);
        expandedDirs.push(e.path);
      }
    }

    const s = get(ide);
    const firstFile = findFirstVoFile(rootEntries, s.dirCache);
    if (firstFile) {
      await this.openFile(firstFile);
    }

    ide.update(s => ({ ...s, projectMode: 'multi', expandedDirs }));
    explorer.update(e => ({ ...e, appMode: 'develop' }));
  },

  async convertToMultiProject() {
    const { workspaceRoot, editTarget } = get(ide);
    const modContent = `module main\n\nvo 0.1\n`;
    await bridge().fsWriteFile(workspaceRoot + '/vo.mod', modContent);
    const entries = await this.loadDir(workspaceRoot);
    const expandedDirs: string[] = [];
    for (const e of entries) {
      if (e.isDir) {
        await this.loadDir(e.path);
        expandedDirs.push(e.path);
      }
    }
    const newTarget: EditTarget | null = editTarget
      ? { ...editTarget, mode: 'multi' }
      : null;
    ide.update(s => ({ ...s, projectMode: 'multi', expandedDirs, editTarget: newTarget }));
  },

  // =========================================================================
  // Filesystem
  // =========================================================================

  async loadDir(dirPath: string) {
    const entries = await bridge().fsListDir(dirPath);
    ide.update(s => ({
      ...s,
      dirCache: { ...s.dirCache, [dirPath]: entries },
    }));
    return entries;
  },

  async toggleDir(dirPath: string) {
    const s = get(ide);
    const isExpanded = s.expandedDirs.includes(dirPath);
    if (isExpanded) {
      ide.update(s => ({
        ...s,
        expandedDirs: s.expandedDirs.filter(d => d !== dirPath),
      }));
    } else {
      if (!s.dirCache[dirPath]) {
        await this.loadDir(dirPath);
      }
      ide.update(s => ({
        ...s,
        expandedDirs: [...s.expandedDirs, dirPath],
      }));
    }
  },

  async openFile(filePath: string) {
    // Save current file if dirty
    const s = get(ide);
    if (s.dirty && s.activeFilePath) {
      await this.saveFile();
    }

    const content = await bridge().fsReadFile(filePath);
    ide.update(s => ({
      ...s,
      activeFilePath: filePath,
      code: content,
      dirty: false,
      output: '',
      compileError: '',
      guestRender: '',
      isGuiApp: false,
      isRunning: false,
    }));
  },

  onEditorChange(code: string) {
    ide.update(s => ({ ...s, code, dirty: true }));
  },

  async saveFile() {
    const s = get(ide);
    if (!s.activeFilePath || !s.dirty) return;
    await bridge().fsWriteFile(s.activeFilePath, s.code);
    ide.update(st => ({ ...st, dirty: false }));
  },

  // =========================================================================
  // Project management — push / pull / open from project list
  // =========================================================================

  async pushProject(project: ProjectEntry): Promise<void> {
    const { token } = get(github);
    if (!token) throw new Error('GitHub token not set');
    if (!project.localPath) throw new Error('No local files to push');

    const s = get(ide);
    // Flush unsaved edits
    if (s.dirty && s.activeFilePath) {
      await bridge().fsWriteFile(s.activeFilePath, s.code);
      ide.update(st => ({ ...st, dirty: false }));
    }

    const b = bridge();
    const now = new Date().toISOString();

    if (project.type === 'single') {
      const filename = project.name + '.vo';
      const content = await b.fsReadFile(project.localPath);

      if (project.remote?.kind === 'gist' && project.remote.gistId) {
        // Update existing gist
        await updateGist(token, project.remote.gistId, filename, content);
      } else {
        // Create new gist
        const result = await createGist(token, filename, content, project.name);
        project = { ...project, remote: { kind: 'gist', gistId: result.id } };
      }
      project = { ...project, pushedAt: now };
    } else {
      // Multi-file project: collect all files and git push to repo
      const files = await this.collectProjectFiles(project.localPath);

      if (project.remote?.kind === 'repo' && project.remote.owner && project.remote.repo) {
        // Push to existing repo
        await gitPushFiles(token, project.remote.owner, project.remote.repo, files);
      } else {
        // Create new repo and push
        const { user } = get(github);
        if (!user) throw new Error('GitHub user not loaded');
        const repo = await createRepo(token, project.name);
        await gitPushFiles(token, repo.owner.login, repo.name, files);
        project = { ...project, remote: { kind: 'repo', owner: repo.owner.login, repo: repo.name } };
      }
      project = { ...project, pushedAt: now };
    }

    // Update projects store and persist manifest
    projects.update(s => ({
      ...s,
      projects: s.projects.map(p =>
        p.name === project.name && p.type === project.type ? project : p
      ),
    }));
    await persistManifest();
  },

  async pullProject(project: ProjectEntry, root: string): Promise<void> {
    const { token } = get(github);
    if (!token) throw new Error('GitHub token not set');
    if (!project.remote) throw new Error('No remote source to pull from');

    const b = bridge();

    if (project.remote.kind === 'gist' && project.remote.gistId) {
      // Single-file: pull from gist
      const files = await fetchGistFiles(token, project.remote.gistId);
      const fileNames = Object.keys(files);
      const voFile = fileNames.find(f => f.endsWith('.vo')) ?? fileNames[0];
      if (!voFile) throw new Error('Gist has no files');
      const localPath = root + '/' + voFile;
      await b.fsWriteFile(localPath, files[voFile]);
      project = { ...project, localPath };
    } else if (project.remote.kind === 'repo' && project.remote.owner && project.remote.repo) {
      // Multi-file: pull from repo via Git Data API
      const files = await gitPullFiles(token, project.remote.owner, project.remote.repo);
      const dir = root + '/' + project.name;
      try { await b.fsMkdir(dir); } catch { /* may already exist */ }
      // Ensure all intermediate directories exist
      const createdDirs = new Set<string>();
      for (const name of Object.keys(files)) {
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
      for (const [name, content] of Object.entries(files)) {
        await b.fsWriteFile(dir + '/' + name, content);
      }
      project = { ...project, localPath: dir };
    } else {
      throw new Error('Invalid remote source');
    }

    // Update projects store
    projects.update(s => ({
      ...s,
      projects: s.projects.map(p =>
        p.name === project.name && p.type === project.type ? project : p
      ),
    }));
  },

  async openProjectEntry(project: ProjectEntry, root: string): Promise<void> {
    const state = syncState(project);

    // Remote-only: download first
    if (state === 'remote-only') {
      await this.pullProject(project, root);
      // Re-read the updated project entry
      const ps = get(projects);
      const updated = ps.projects.find(p => p.name === project.name && p.type === project.type);
      if (updated) project = updated;
    }

    if (!project.localPath) throw new Error('Project has no local path after pull');

    if (project.type === 'single') {
      await this.openSingleFile(project.localPath);
    } else {
      await this.openProject(project.localPath + '/vo.mod');
    }
  },

  async deleteProject(project: ProjectEntry, root: string): Promise<void> {
    const b = bridge();
    // Delete local files
    if (project.localPath) {
      const isDir = project.type === 'multi';
      await b.fsRemove(project.localPath, isDir);
    }

    // Remove from projects store
    projects.update(s => ({
      ...s,
      projects: s.projects.filter(p =>
        !(p.name === project.name && p.type === project.type)
      ),
    }));

    // If it had a remote, persist the updated manifest
    if (project.remote) {
      await persistManifest();
    }
  },

  async collectProjectFiles(dirPath: string): Promise<Record<string, string>> {
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
  },

  // =========================================================================
  // Execution — always saves before running
  // =========================================================================

  async runCode() {
    const s = get(ide);

    // Save current active file first
    if (s.dirty && s.activeFilePath) {
      await this.saveFile();
    }

    // For multi-file projects always compile from workspaceRoot/main.vo so all
    // package files are picked up.  Single-file mode uses the open file directly.
    const entryPath = s.projectMode === 'multi'
      ? s.workspaceRoot + '/main.vo'
      : s.activeFilePath;

    if (!entryPath) return;

    // GUI detection: for multi-file read main.vo; for single-file use in-memory code
    let codeToCheck = s.code;
    if (s.projectMode === 'multi' && s.activeFilePath !== entryPath) {
      try {
        codeToCheck = await bridge().fsReadFile(entryPath);
      } catch {
        codeToCheck = '';
      }
    }

    ide.update(s => ({
      ...s,
      isRunning: true,
      output: '',
      compileError: '',
      guestRender: '',
      isGuiApp: false,
    }));

    try {
      if (isGuiCode(codeToCheck)) {
        const json = await bridge().runGui(entryPath);
        ide.update(s => ({ ...s, isRunning: true, isGuiApp: true, guestRender: json }));
      } else {
        const output = await bridge().compileRun(entryPath);
        ide.update(s => ({ ...s, isRunning: false, isGuiApp: false, output }));
      }
    } catch (e: any) {
      ide.update(s => ({ ...s, isRunning: false, compileError: String(e) }));
    }
  },

  async stopCode() {
    try {
      await bridge().stopGui();
    } catch {
      // ignore errors on stop
    }
    ide.update(s => ({
      ...s,
      isRunning: false,
      isGuiApp: false,
      guestRender: '',
    }));
  },

  // =========================================================================
  // File creation
  // =========================================================================

  async createFile(dirPath: string, fileName: string) {
    const fullPath = dirPath + '/' + fileName;
    await bridge().fsWriteFile(fullPath, '');
    await this.loadDir(dirPath);
    await this.openFile(fullPath);
  },

  async createDir(parentPath: string, dirName: string) {
    const fullPath = parentPath + '/' + dirName;
    await bridge().fsMkdir(fullPath);
    await this.loadDir(parentPath);
  },

  async deleteEntry(path: string, isDir: boolean) {
    await bridge().fsRemove(path, isDir);
    const parentPath = path.substring(0, path.lastIndexOf('/')) || '/';

    // Clear stale cache/expanded state for deleted subtree
    ide.update(s => {
      const newCache: Record<string, FsEntry[]> = {};
      for (const [k, v] of Object.entries(s.dirCache)) {
        if (k !== path && !k.startsWith(path + '/')) newCache[k] = v;
      }
      const newExpanded = s.expandedDirs.filter(d => d !== path && !d.startsWith(path + '/'));
      return { ...s, dirCache: newCache, expandedDirs: newExpanded };
    });

    await this.loadDir(parentPath);

    const s = get(ide);
    if (s.activeFilePath === path || s.activeFilePath.startsWith(path + '/')) {
      ide.update(s => ({ ...s, activeFilePath: '', code: '', dirty: false }));
    }
    explorer.update(e => ({ ...e, contextMenu: null }));
  },

  // =========================================================================
  // Rename & inline-input actions
  // =========================================================================

  async renameEntry(oldPath: string, newName: string) {
    const dir = oldPath.substring(0, oldPath.lastIndexOf('/'));
    const newPath = dir + '/' + newName;
    await bridge().fsRename(oldPath, newPath);

    // Clear stale cache/expanded for renamed subtree
    ide.update(s => {
      const newCache: Record<string, FsEntry[]> = {};
      for (const [k, v] of Object.entries(s.dirCache)) {
        if (k !== oldPath && !k.startsWith(oldPath + '/')) newCache[k] = v;
      }
      const newExpanded = s.expandedDirs.filter(d => d !== oldPath && !d.startsWith(oldPath + '/'));
      return { ...s, dirCache: newCache, expandedDirs: newExpanded };
    });

    await this.loadDir(dir);

    const s = get(ide);
    if (s.activeFilePath === oldPath) {
      await this.openFile(newPath);
    } else if (s.activeFilePath.startsWith(oldPath + '/')) {
      await this.openFile(newPath + s.activeFilePath.slice(oldPath.length));
    }
    explorer.update(e => ({ ...e, inlineInput: null }));
  },

  startCreate(dirPath: string, isDir: boolean) {
    // Ensure the target directory is expanded so the input is visible
    ide.update(s => {
      if (s.expandedDirs.includes(dirPath)) return s;
      return { ...s, expandedDirs: [...s.expandedDirs, dirPath] };
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
  },

  startRename(path: string, currentName: string) {
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
  },

  async commitInlineInput(value: string) {
    const { inlineInput } = get(explorer);
    if (!inlineInput || !value.trim()) {
      explorer.update(e => ({ ...e, inlineInput: null }));
      return;
    }
    const v = value.trim();
    if (inlineInput.mode === 'create-file') {
      await this.createFile(inlineInput.dirPath, v);
    } else if (inlineInput.mode === 'create-dir') {
      await this.createDir(inlineInput.dirPath, v);
    } else if (inlineInput.mode === 'rename' && inlineInput.targetPath) {
      await this.renameEntry(inlineInput.targetPath, v);
    }
    explorer.update(e => ({ ...e, inlineInput: null }));
  },

  cancelInlineInput() {
    explorer.update(e => ({ ...e, inlineInput: null }));
  },
};

// Find first .vo file in root entries (non-recursive for simplicity)
function findFirstVoFile(
  entries: { name: string; path: string; isDir: boolean }[],
  _dirCache: Record<string, any>,
): string | null {
  for (const e of entries) {
    if (!e.isDir && e.name.endsWith('.vo')) return e.path;
  }
  // Check first directory
  for (const e of entries) {
    if (e.isDir) {
      const children = _dirCache[e.path];
      if (children) {
        const found = findFirstVoFile(children, _dirCache);
        if (found) return found;
      }
    }
  }
  return null;
}
