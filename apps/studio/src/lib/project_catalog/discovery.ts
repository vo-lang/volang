import type { Backend } from '../backend/backend';
import type { DiscoveredProject } from '../types';
import type { ManagedProject } from './types';

export async function discoverWorkspaceProjects(backend: Backend): Promise<ManagedProject[]> {
  let discovered: DiscoveredProject[];
  try {
    discovered = await backend.discoverWorkspaceProjects();
  } catch {
    return [];
  }

  return mapDiscovered(discovered);
}

function mapDiscovered(discovered: DiscoveredProject[]): ManagedProject[] {
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

export async function collectLocalProjectFiles(backend: Backend, project: ManagedProject): Promise<Record<string, string>> {
  if (!project.localPath) return {};
  if (project.type === 'single') {
    const content = await backend.readFile(project.localPath);
    return { [`${project.name}.vo`]: content };
  }
  return collectDirectoryFiles(backend, project.localPath);
}

export async function collectDirectoryFiles(backend: Backend, dirPath: string): Promise<Record<string, string>> {
  const result: Record<string, string> = {};
  await walkDirectory(backend, dirPath, '', result);
  return result;
}

const SKIP_DIR_NAMES = new Set(['.volang', '.vo-cache', '.git', 'node_modules']);

async function walkDirectory(
  backend: Backend,
  dirPath: string,
  prefix: string,
  result: Record<string, string>,
): Promise<void> {
  let entries: Awaited<ReturnType<Backend['listDir']>>;
  try {
    entries = await backend.listDir(dirPath);
  } catch {
    return;
  }

  for (const entry of entries) {
    if (entry.name.startsWith('.')) continue;
    if (entry.isDir) {
      if (SKIP_DIR_NAMES.has(entry.name)) continue;
      const nextPrefix = prefix ? `${prefix}/${entry.name}` : entry.name;
      await walkDirectory(backend, entry.path, nextPrefix, result);
      continue;
    }
    const relativePath = prefix ? `${prefix}/${entry.name}` : entry.name;
    result[relativePath] = await safeReadFile(backend, entry.path);
  }
}

async function safeReadFile(backend: Backend, path: string): Promise<string> {
  try {
    return await backend.readFile(path);
  } catch {
    return '';
  }
}
