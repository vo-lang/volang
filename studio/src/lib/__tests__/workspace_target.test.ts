import { describe, expect, it } from 'vitest';
import { resolveWorkspaceOpenTarget, type WorkspaceFsStat, type WorkspaceTargetOps } from '../workspace_target';

function makeOps(entries: WorkspaceFsStat[]): WorkspaceTargetOps {
  const map = new Map(entries.map(entry => [entry.path, entry]));

  return {
    async stat(path: string): Promise<WorkspaceFsStat> {
      const entry = map.get(path);
      if (!entry) throw new Error(`not found: ${path}`);
      return entry;
    },
    async tryStat(path: string): Promise<WorkspaceFsStat | null> {
      return map.get(path) ?? null;
    },
    async listDir(path: string): Promise<WorkspaceFsStat[]> {
      const prefix = path === '/' ? '/' : `${path}/`;
      return entries.filter(entry => {
        if (!entry.path.startsWith(prefix)) return false;
        const rest = entry.path.slice(prefix.length);
        return rest.length > 0 && !rest.includes('/');
      });
    },
  };
}

describe('resolveWorkspaceOpenTarget', () => {
  it('opens a nested file as part of its containing project', async () => {
    const ops = makeOps([
      { name: 'project', path: '/workspace/project', isDir: true },
      { name: 'vo.mod', path: '/workspace/project/vo.mod', isDir: false },
      { name: 'examples', path: '/workspace/project/examples', isDir: true },
      { name: 'demo', path: '/workspace/project/examples/demo', isDir: true },
      { name: 'main.vo', path: '/workspace/project/examples/demo/main.vo', isDir: false },
    ]);

    await expect(resolveWorkspaceOpenTarget('/workspace/project/examples/demo/main.vo', undefined, ops)).resolves.toEqual({
      kind: 'project',
      voModPath: '/workspace/project/vo.mod',
      entryPath: '/workspace/project/examples/demo/main.vo',
    });
  });

  it('opens a nested directory inside a project using the discovered entry file', async () => {
    const ops = makeOps([
      { name: 'project', path: '/workspace/project', isDir: true },
      { name: 'vo.mod', path: '/workspace/project/vo.mod', isDir: false },
      { name: 'examples', path: '/workspace/project/examples', isDir: true },
      { name: 'demo', path: '/workspace/project/examples/demo', isDir: true },
      { name: 'main.vo', path: '/workspace/project/examples/demo/main.vo', isDir: false },
    ]);

    await expect(resolveWorkspaceOpenTarget('/workspace/project/examples/demo', undefined, ops)).resolves.toEqual({
      kind: 'project',
      voModPath: '/workspace/project/vo.mod',
      entryPath: '/workspace/project/examples/demo/main.vo',
    });
  });

  it('resolves a standalone directory to a single-file target when no project root exists', async () => {
    const ops = makeOps([
      { name: 'scratch', path: '/workspace/scratch', isDir: true },
      { name: 'nested', path: '/workspace/scratch/nested', isDir: true },
      { name: 'app.vo', path: '/workspace/scratch/nested/app.vo', isDir: false },
    ]);

    await expect(resolveWorkspaceOpenTarget('/workspace/scratch', undefined, ops)).resolves.toEqual({
      kind: 'single-file',
      filePath: '/workspace/scratch/nested/app.vo',
    });
  });

  it('resolves an explicit entry path relative to a project directory target', async () => {
    const ops = makeOps([
      { name: 'project', path: '/workspace/project', isDir: true },
      { name: 'vo.mod', path: '/workspace/project/vo.mod', isDir: false },
      { name: 'examples', path: '/workspace/project/examples', isDir: true },
      { name: 'demo', path: '/workspace/project/examples/demo', isDir: true },
      { name: 'main.vo', path: '/workspace/project/examples/demo/main.vo', isDir: false },
    ]);

    await expect(resolveWorkspaceOpenTarget('/workspace/project', 'examples/demo/main.vo', ops)).resolves.toEqual({
      kind: 'project',
      voModPath: '/workspace/project/vo.mod',
      entryPath: '/workspace/project/examples/demo/main.vo',
    });
  });
});
