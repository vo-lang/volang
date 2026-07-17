import { VirtualFS } from '../../../../lang/crates/vo-web/js/vfs.ts';

import type { WindowVfsBackend } from './window_vfs_bindings';

export interface InMemoryWindowVfsFile {
  path: string;
  bytes: Uint8Array;
  mode?: number;
}

export interface InMemoryWindowVfsOptions {
  files?: InMemoryWindowVfsFile[];
  rootPath?: string;
}

const ROOT = '/';

function dirname(path: string): string {
  const end = path.length > 1 && path.endsWith('/') ? path.length - 1 : path.length;
  const index = path.lastIndexOf('/', end - 1);
  if (index < 0) return '.';
  if (index === 0) return ROOT;
  return path.slice(0, index);
}

function requireSuccess(error: string | null, operation: string, path: string): void {
  if (error) {
    throw new Error(`unable to ${operation} ${JSON.stringify(path)}: ${error}`);
  }
}

/**
 * Build the isolated VFS used by native Studio renderer sessions.
 *
 * The browser runtime and renderer bridge intentionally share `VirtualFS` so
 * open flags, descriptor identity, rename/unlink behavior, limits, cwd, and
 * permission checks stay identical at every WASM entry point. The instance is
 * left uninitialized, which keeps this snapshot backend memory-only and avoids
 * loading or persisting the page's OPFS tree.
 */
export function createInMemoryWindowVfsBackend(
  options: InMemoryWindowVfsOptions = {},
): WindowVfsBackend {
  const vfs = new VirtualFS();
  const rootPath = options.rootPath ?? ROOT;

  requireSuccess(vfs.mkdirAll(rootPath, 0o755), 'create VFS root', rootPath);
  requireSuccess(vfs.chdir(rootPath), 'select VFS root', rootPath);

  for (const file of options.files ?? []) {
    requireSuccess(vfs.mkdirAll(dirname(file.path), 0o755), 'create parent directory for', file.path);
    requireSuccess(
      vfs.writeFile(file.path, file.bytes, file.mode ?? 0o644),
      'seed VFS file',
      file.path,
    );
  }

  const [guestFloor, floorError] = vfs.getwd();
  requireSuccess(floorError, 'read VFS root', rootPath);
  const contains = (root: string, candidate: string): boolean => (
    root === ROOT || candidate === root || candidate.startsWith(`${root}/`)
  );
  const resolveGuestPath = (path: string): [string | null, string | null] => {
    if (typeof path !== 'string' || path.length === 0 || path.includes('\0')) {
      return [null, 'invalid argument'];
    }
    const [cwd, cwdError] = vfs.getwd();
    if (cwdError) return [null, cwdError];
    if (!contains(guestFloor, cwd)) return [null, 'permission denied'];
    const floorParts = guestFloor.split('/').filter(Boolean);
    const parts = path.startsWith('/') ? [...floorParts] : cwd.split('/').filter(Boolean);
    for (const part of path.split('/')) {
      if (!part || part === '.') continue;
      if (part === '..') {
        if (parts.length <= floorParts.length) return [null, 'permission denied'];
        parts.pop();
      } else {
        parts.push(part);
      }
    }
    const resolved = parts.length === 0 ? ROOT : `/${parts.join('/')}`;
    return contains(guestFloor, resolved) ? [resolved, null] : [null, 'permission denied'];
  };
  const guestGetwd = (): [string, string | null] => {
    const [cwd, error] = vfs.getwd();
    if (error) return ['', error];
    if (!contains(guestFloor, cwd)) return ['', 'permission denied'];
    if (guestFloor === ROOT) return [cwd, null];
    return [cwd.slice(guestFloor.length) || ROOT, null];
  };

  return Object.assign(vfs, { resolveGuestPath, guestGetwd });
}
