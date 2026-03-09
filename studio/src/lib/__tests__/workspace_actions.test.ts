import { beforeEach, describe, expect, it, vi } from 'vitest';
import { get } from 'svelte/store';
import { ide } from '../../stores/ide';
import { explorer } from '../../stores/explorer';

const mocks = vi.hoisted(() => ({
  termSetCwd: vi.fn(),
  fsStat: vi.fn(async (path: string) => ({
    name: path.split('/').pop() || path,
    path,
    isDir: path.endsWith('/project'),
  })),
  loadDir: vi.fn(async (path: string) => {
    if (path === '/workspace/project') {
      return [
        { name: 'main.vo', path: '/workspace/project/main.vo', isDir: false },
        { name: 'src', path: '/workspace/project/src', isDir: true },
      ];
    }
    if (path === '/workspace/project/src') {
      return [];
    }
    return [];
  }),
  openFile: vi.fn(async () => {}),
}));

vi.mock('../../stores/terminal', () => ({
  termSetCwd: mocks.termSetCwd,
}));

vi.mock('../bridge', () => ({
  bridge: () => ({
    fsStat: mocks.fsStat,
  }),
}));

vi.mock('../actions/fs', () => ({
  loadDir: mocks.loadDir,
  openFile: mocks.openFile,
}));

import { openProject, openSingleFile } from '../actions/workspace';

beforeEach(() => {
  vi.clearAllMocks();
  ide.set({
    workspaceRoot: '',
    projectMode: 'single',
    editTarget: null,
    dirCache: {},
    expandedDirs: new Set<string>(),
    activeFilePath: '',
    runEntryPath: '',
    code: '',
    dirty: false,
    consoleLines: [],
    runStatus: 'idle',
    runDurationMs: null,
    isRunning: false,
    isGuiApp: false,
    guestRender: null,
    outputExpanded: false,
    consoleShowTimestamps: true,
    consoleWordWrap: true,
  });
  explorer.set({
    appMode: 'manage',
    localRoot: '/workspace',
    explorerCwd: '/workspace',
    explorerHistoryBack: [],
    explorerHistoryFwd: [],
    explorerSelected: null,
    contextMenu: null,
    inlineInput: null,
  });
});

describe('workspace actions', () => {
  it('syncs terminal cwd when opening a single file', async () => {
    await openSingleFile('/workspace/demo/main.vo');

    expect(mocks.termSetCwd).toHaveBeenCalledWith('/workspace/demo');
    expect(get(ide).workspaceRoot).toBe('/workspace/demo');
    expect(get(ide).runEntryPath).toBe('/workspace/demo/main.vo');
    expect(get(explorer).explorerCwd).toBe('/workspace/demo');
  });

  it('syncs terminal cwd when opening a project target', async () => {
    await openProject('/workspace/project/vo.mod', '/workspace/project/main.vo');

    expect(mocks.termSetCwd).toHaveBeenCalledWith('/workspace/project');
    expect(get(ide).workspaceRoot).toBe('/workspace/project');
    expect(get(ide).projectMode).toBe('multi');
    expect(get(ide).runEntryPath).toBe('/workspace/project/main.vo');
    expect(get(explorer).explorerCwd).toBe('/workspace/project');
    expect(mocks.openFile).toHaveBeenCalledWith('/workspace/project/main.vo', { preserveRunEntry: true });
  });
});
