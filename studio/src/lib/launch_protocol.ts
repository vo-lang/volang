import { joinPath, normalizePath } from './path_utils';

export type LaunchAction = 'open' | 'run';

export interface LocalLaunchTarget {
  kind: 'local';
  targetPath: string;
  entryPath?: string;
}

export interface GitHubLaunchTarget {
  kind: 'github';
  owner: string;
  repo: string;
  ref?: string;
  subpath?: string;
  entryPath?: string;
}

export type StudioLaunchTarget = LocalLaunchTarget | GitHubLaunchTarget;

export interface StudioLaunchRequest {
  action: LaunchAction;
  target: StudioLaunchTarget;
  launchUrl: string;
}

const DEFAULT_LAUNCH_BASE_URL = 'http://localhost/';

function firstNonEmpty(...values: Array<string | null | undefined>): string | null {
  for (const value of values) {
    const trimmed = value?.trim();
    if (trimmed) return trimmed;
  }
  return null;
}

function isTauriRuntime(): boolean {
  if (typeof window === 'undefined') return false;
  const w = window as any;
  return Boolean(w.__TAURI__ || w.__TAURI_INTERNALS__);
}

function currentLaunchBaseUrl(): string {
  if (typeof window === 'undefined') return DEFAULT_LAUNCH_BASE_URL;
  return window.location.href;
}

function isGitHubUrl(raw: string): boolean {
  try {
    const url = new URL(raw);
    return url.protocol === 'https:' && (url.hostname === 'github.com' || url.hostname === 'www.github.com');
  } catch {
    return false;
  }
}

function normalizeFileUrlTarget(raw: string): string {
  const url = new URL(raw);
  return normalizePath(decodeURIComponent(url.pathname || '/'));
}

export function normalizeLocalLaunchPath(raw: string, workspaceRoot: string): string {
  const trimmed = raw.trim();
  if (!trimmed) throw new Error('Local launch target is empty');
  if (trimmed.startsWith('file://')) return normalizeFileUrlTarget(trimmed);
  if (trimmed.startsWith('/')) return normalizePath(trimmed);
  return joinPath(workspaceRoot, trimmed);
}

function parseGitHubLaunchTarget(
  raw: string,
  entryPath?: string,
  refOverride?: string,
): GitHubLaunchTarget {
  const url = new URL(raw);
  const segments = url.pathname.split('/').filter(Boolean);
  if (segments.length < 2) {
    throw new Error(`Unsupported GitHub target: ${raw}`);
  }

  const owner = segments[0];
  const repo = segments[1].replace(/\.git$/, '');
  let urlRef: string | undefined;
  let subpath: string | undefined;

  if ((segments[2] === 'tree' || segments[2] === 'blob') && segments.length >= 4) {
    const tail = segments.slice(3);
    if (tail.length > 0) {
      urlRef = tail.shift();
    }
    if (tail.length > 0) {
      subpath = normalizePath(tail.join('/'));
      if (subpath === '.') {
        subpath = undefined;
      }
    }
  }
  const ref = firstNonEmpty(refOverride, url.searchParams.get('ref'), urlRef) ?? undefined;

  return {
    kind: 'github',
    owner,
    repo,
    ref,
    subpath,
    entryPath: entryPath || undefined,
  };
}

export function hasStudioLaunchQuery(url: URL): boolean {
  return Boolean(firstNonEmpty(url.searchParams.get('run'), url.searchParams.get('open')));
}

export function parseStudioLaunchUrl(launchUrl: string, workspaceRoot: string): StudioLaunchRequest | null {
  const parsed = new URL(launchUrl, currentLaunchBaseUrl());
  const runTarget = firstNonEmpty(parsed.searchParams.get('run'));
  const openTarget = firstNonEmpty(parsed.searchParams.get('open'));
  const rawTarget = runTarget ?? openTarget;
  if (!rawTarget) return null;

  const action: LaunchAction = runTarget ? 'run' : 'open';
  const entryPath = firstNonEmpty(parsed.searchParams.get('entry')) ?? undefined;
  const ref = firstNonEmpty(parsed.searchParams.get('ref')) ?? undefined;

  const target: StudioLaunchTarget = isGitHubUrl(rawTarget)
    ? parseGitHubLaunchTarget(rawTarget, entryPath, ref)
    : {
        kind: 'local',
        targetPath: normalizeLocalLaunchPath(rawTarget, workspaceRoot),
        entryPath,
      };

  return {
    action,
    target,
    launchUrl: parsed.toString(),
  };
}

async function readNativeLaunchUrl(): Promise<string | null> {
  if (!isTauriRuntime()) return null;
  try {
    const { invoke } = await import('@tauri-apps/api/core');
    const launchUrl = await invoke<string | null>('cmd_get_launch_url');
    const trimmed = launchUrl?.trim();
    return trimmed || null;
  } catch {
    return null;
  }
}

export async function resolveInitialStudioLaunchUrl(): Promise<string | null> {
  if (typeof window !== 'undefined') {
    const current = new URL(window.location.href);
    if (hasStudioLaunchQuery(current)) return current.toString();
  }

  const envLaunchUrl = firstNonEmpty(import.meta.env.VITE_STUDIO_LAUNCH_URL);
  if (envLaunchUrl) return envLaunchUrl;

  return readNativeLaunchUrl();
}
