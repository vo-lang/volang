import type { WorkspaceService } from '../services/workspace_service';
import type { ManagedProject } from './types';

export async function discoverLocalProjects(workspace: WorkspaceService, root: string): Promise<ManagedProject[]> {
  let discovered = [] as Awaited<ReturnType<WorkspaceService['discoverProjects']>>;
  try {
    discovered = await workspace.discoverProjects(root);
  } catch {
    return [];
  }

  return discovered.map((entry) => ({
    name: entry.name,
    type: entry.type as ManagedProject['type'],
    localPath: entry.localPath,
    entryPath: entry.entryPath,
    remote: null,
    pushedAt: null,
    remoteUpdatedAt: null,
    syncedHash: null,
    currentLocalHash: null,
    currentRemoteHash: null,
    hasGui: false,
  }));
}

export async function collectLocalProjectFiles(workspace: WorkspaceService, project: ManagedProject): Promise<Record<string, string>> {
  if (!project.localPath) return {};
  if (project.type === 'single') {
    const content = await workspace.readFile(project.localPath);
    return { [`${project.name}.vo`]: content };
  }
  return collectDirectoryFiles(workspace, project.localPath);
}

export async function collectDirectoryFiles(workspace: WorkspaceService, dirPath: string): Promise<Record<string, string>> {
  const result: Record<string, string> = {};
  await walkDirectory(workspace, dirPath, '', result);
  return result;
}

const SKIP_DIR_NAMES = new Set(['.vo-cache', '.git', 'node_modules']);

async function walkDirectory(
  workspace: WorkspaceService,
  dirPath: string,
  prefix: string,
  result: Record<string, string>,
): Promise<void> {
  let entries = [] as Awaited<ReturnType<WorkspaceService['list']>>;
  try {
    entries = await workspace.list(dirPath);
  } catch {
    return;
  }

  for (const entry of entries) {
    if (entry.name.startsWith('.')) continue;
    if (entry.isDir) {
      if (SKIP_DIR_NAMES.has(entry.name)) continue;
      const nextPrefix = prefix ? `${prefix}/${entry.name}` : entry.name;
      await walkDirectory(workspace, entry.path, nextPrefix, result);
      continue;
    }
    const relativePath = prefix ? `${prefix}/${entry.name}` : entry.name;
    result[relativePath] = await safeReadFile(workspace, entry.path);
  }
}

async function safeReadFile(workspace: WorkspaceService, path: string): Promise<string> {
  try {
    return await workspace.readFile(path);
  } catch {
    return '';
  }
}
