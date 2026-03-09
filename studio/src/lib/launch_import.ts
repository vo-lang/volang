import { get } from 'svelte/store';
import { github, gitPullFiles } from '../stores/github';
import { bridge } from './bridge';
import type { GitHubLaunchTarget, LocalLaunchTarget, StudioLaunchTarget } from './launch_protocol';
import { dirname, isPathWithinRoot, joinPath, sanitizePathSegment } from './path_utils';

function assertSafeImportRelativePath(path: string): string {
  const trimmed = path.trim();
  if (!trimmed) throw new Error('Invalid GitHub import path');
  if (trimmed.startsWith('/') || trimmed.includes('\\')) {
    throw new Error(`Invalid GitHub import path: ${trimmed}`);
  }
  const segments = trimmed.split('/');
  if (segments.some(seg => !seg || seg === '.' || seg === '..')) {
    throw new Error(`Invalid GitHub import path: ${trimmed}`);
  }
  return trimmed;
}

async function ensureDir(path: string): Promise<void> {
  await bridge().shell.exec({ kind: 'fs.mkdir', path, recursive: true });
}

async function tryRemove(path: string): Promise<void> {
  try {
    await bridge().fsRemove(path, true);
  } catch {
  }
}

function githubImportRoot(workspaceRoot: string, target: GitHubLaunchTarget): string {
  const ref = sanitizePathSegment(target.ref || 'default');
  return joinPath(
    workspaceRoot,
    `.studio/github/${sanitizePathSegment(target.owner)}/${sanitizePathSegment(target.repo)}/${ref}`,
  );
}

async function importGitHubTarget(target: GitHubLaunchTarget): Promise<LocalLaunchTarget> {
  const token = get(github).token;
  const files = await gitPullFiles(token, target.owner, target.repo, target.ref);
  const importRoot = githubImportRoot(bridge().workspaceRoot, target);

  await tryRemove(importRoot);
  await ensureDir(importRoot);

  for (const [relPath, content] of Object.entries(files)) {
    const safeRelPath = assertSafeImportRelativePath(relPath);
    const fullPath = joinPath(importRoot, safeRelPath);
    await ensureDir(dirname(fullPath));
    await bridge().fsWriteFile(fullPath, content);
  }

  return {
    kind: 'local',
    targetPath: target.subpath ? joinPath(importRoot, target.subpath) : importRoot,
    entryPath: target.entryPath,
  };
}

async function importLocalTarget(target: LocalLaunchTarget): Promise<LocalLaunchTarget> {
  const workspaceRoot = bridge().workspaceRoot;
  const targetPath = isPathWithinRoot(target.targetPath, workspaceRoot)
    ? target.targetPath
    : await bridge().materializeLocalLaunchTarget(target.targetPath);

  return {
    kind: 'local',
    targetPath,
    entryPath: target.entryPath,
  };
}

export async function importStudioLaunchTarget(target: StudioLaunchTarget): Promise<LocalLaunchTarget> {
  if (target.kind === 'local') return importLocalTarget(target);
  return importGitHubTarget(target);
}
