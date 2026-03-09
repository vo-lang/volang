import { dirname, joinPath } from './path_utils';

export interface WorkspaceFsStat {
  name: string;
  path: string;
  isDir: boolean;
}

export interface WorkspaceTargetOps {
  listDir(path: string): Promise<WorkspaceFsStat[]>;
  stat(path: string): Promise<WorkspaceFsStat>;
  tryStat(path: string): Promise<WorkspaceFsStat | null>;
}

export type WorkspaceOpenTarget =
  | { kind: 'project'; voModPath: string; entryPath?: string }
  | { kind: 'single-file'; filePath: string };

async function findRunEntryInTree(dirPath: string, ops: WorkspaceTargetOps): Promise<string | null> {
  const entries = await ops.listDir(dirPath);
  const mainFile = entries.find(e => !e.isDir && e.name === 'main.vo');
  if (mainFile) return mainFile.path;

  const firstVoFile = entries.find(e => !e.isDir && e.name.endsWith('.vo'));
  if (firstVoFile) return firstVoFile.path;

  for (const entry of entries) {
    if (!entry.isDir) continue;
    const found = await findRunEntryInTree(entry.path, ops);
    if (found) return found;
  }

  return null;
}

async function findContainingProjectRoot(path: string, ops: WorkspaceTargetOps): Promise<string | null> {
  let current = path;
  while (true) {
    const voModStat = await ops.tryStat(joinPath(current, 'vo.mod'));
    if (voModStat && !voModStat.isDir) return current;
    const parent = dirname(current);
    if (parent === current) return null;
    current = parent;
  }
}

export async function resolveWorkspaceOpenTarget(
  targetPath: string,
  entryPath: string | undefined,
  ops: WorkspaceTargetOps,
): Promise<WorkspaceOpenTarget> {
  const stat = await ops.stat(targetPath);

  if (stat.name === 'vo.mod') {
    const projectRoot = dirname(stat.path);
    return {
      kind: 'project',
      voModPath: stat.path,
      entryPath: entryPath ? joinPath(projectRoot, entryPath) : undefined,
    };
  }

  if (stat.isDir) {
    const resolvedEntryTarget = entryPath ? joinPath(targetPath, entryPath) : undefined;
    const projectRoot = await findContainingProjectRoot(stat.path, ops);
    if (projectRoot) {
      const resolvedEntryPath = resolvedEntryTarget
        ?? (stat.path === projectRoot ? undefined : await findRunEntryInTree(stat.path, ops) ?? undefined);
      return {
        kind: 'project',
        voModPath: joinPath(projectRoot, 'vo.mod'),
        entryPath: resolvedEntryPath,
      };
    }
    if (resolvedEntryTarget) {
      return resolveWorkspaceOpenTarget(resolvedEntryTarget, undefined, ops);
    }
    const discoveredEntry = await findRunEntryInTree(stat.path, ops);
    if (discoveredEntry) {
      return resolveWorkspaceOpenTarget(discoveredEntry, undefined, ops);
    }
    throw new Error(`Directory does not contain a runnable Vo target: ${targetPath}`);
  }

  if (!stat.name.endsWith('.vo')) {
    throw new Error(`Unsupported launch target: ${targetPath}`);
  }

  const projectRoot = await findContainingProjectRoot(dirname(stat.path), ops);
  if (projectRoot) {
    return {
      kind: 'project',
      voModPath: joinPath(projectRoot, 'vo.mod'),
      entryPath: stat.path,
    };
  }

  return {
    kind: 'single-file',
    filePath: stat.path,
  };
}
