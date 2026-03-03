import {
  type FsOp,
  type FsListResult,
  type FsStatResult,
  ShellError,
} from '../protocol';

// =============================================================================
// WasmFsHandler — delegates to the JS VirtualFS instance
// =============================================================================

// Minimal interface covering the VFS methods we actually call.
export interface VfsLike {
  readDir(path: string): [[string, boolean][], string | null];
  readFile(path: string): [Uint8Array | null, string | null];
  writeFile(path: string, data: Uint8Array, mode: number): string | null;
  mkdirAll(path: string, mode: number): string | null;
  rename(oldPath: string, newPath: string): string | null;
  remove(path: string): string | null;
  removeAll(path: string): string | null;
}

export class WasmFsHandler {
  constructor(private readonly vfs: VfsLike) {}

  handle(op: FsOp, cwd: string): Promise<unknown> {
    switch (op.kind) {
      case 'fs.list':   return Promise.resolve(this.list(op.path, cwd));
      case 'fs.stat':   return Promise.resolve(this.stat(op.path, cwd));
      case 'fs.read':   return Promise.resolve(this.read(op.path, cwd));
      case 'fs.write':  return Promise.resolve(this.write(op.path, op.content, cwd));
      case 'fs.mkdir':  return Promise.resolve(this.mkdir(op.path, cwd));
      case 'fs.remove': return Promise.resolve(this.remove(op.path, op.recursive ?? false, cwd));
      case 'fs.rename': return Promise.resolve(this.rename(op.oldPath, op.newPath, cwd));
      case 'fs.copy':   return Promise.resolve(this.copy(op.src, op.dst, cwd));
    }
  }

  private list(path: string, cwd: string): FsListResult {
    const abs = resolve(cwd, path);
    const [entries, err] = this.vfs.readDir(abs);
    if (err) throw vfsError(err);
    return entries
      .map(([name, isDir]): FsStatResult => ({
        name,
        path: abs === '/' ? '/' + name : abs + '/' + name,
        isDir,
      }))
      .sort((a, b) => {
        if (a.isDir !== b.isDir) return a.isDir ? -1 : 1;
        return a.name.localeCompare(b.name);
      });
  }

  private stat(path: string, cwd: string): FsStatResult {
    const abs = resolve(cwd, path);
    const parts = abs.split('/').filter(Boolean);
    const name  = parts[parts.length - 1] ?? '';
    const parentPath = abs.substring(0, abs.lastIndexOf('/')) || '/';
    const [entries, err] = this.vfs.readDir(parentPath);
    if (err) throw vfsError(err);
    const entry = entries.find(([n]) => n === name);
    if (!entry) throw new ShellError('ERR_NOT_FOUND', `not found: ${path}`);
    return { name, path: abs, isDir: entry[1] };
  }

  private read(path: string, cwd: string): string {
    const abs = resolve(cwd, path);
    const [data, err] = this.vfs.readFile(abs);
    if (err || !data) throw vfsError(err ?? 'file not found');
    return new TextDecoder().decode(data);
  }

  private write(path: string, content: string, cwd: string): null {
    const abs = resolve(cwd, path);
    const err = this.vfs.writeFile(abs, new TextEncoder().encode(content), 0o644);
    if (err) throw vfsError(err);
    return null;
  }

  private mkdir(path: string, cwd: string): null {
    const abs = resolve(cwd, path);
    const err = this.vfs.mkdirAll(abs, 0o755);
    if (err) throw vfsError(err);
    return null;
  }

  private remove(path: string, recursive: boolean, cwd: string): null {
    const abs = resolve(cwd, path);
    const err = recursive ? this.vfs.removeAll(abs) : this.vfs.remove(abs);
    if (err) throw vfsError(err);
    return null;
  }

  private rename(oldPath: string, newPath: string, cwd: string): null {
    const absOld = resolve(cwd, oldPath);
    const absNew = resolve(cwd, newPath);
    const err = this.vfs.rename(absOld, absNew);
    if (err) throw vfsError(err);
    return null;
  }

  private copy(src: string, dst: string, cwd: string): null {
    const absSrc = resolve(cwd, src);
    const absDst = resolve(cwd, dst);
    const [data, readErr] = this.vfs.readFile(absSrc);
    if (readErr || !data) throw vfsError(readErr ?? 'source not found');
    const writeErr = this.vfs.writeFile(absDst, data, 0o644);
    if (writeErr) throw vfsError(writeErr);
    return null;
  }
}

// ── Helpers ───────────────────────────────────────────────────────────────────

function resolve(cwd: string, path: string): string {
  if (path.startsWith('/')) return normalize(path);
  return normalize((cwd.endsWith('/') ? cwd : cwd + '/') + path);
}

function normalize(path: string): string {
  const parts  = path.split('/');
  const stack: string[] = [];
  for (const p of parts) {
    if (p === '' || p === '.') continue;
    if (p === '..') { stack.pop(); continue; }
    stack.push(p);
  }
  return '/' + stack.join('/');
}

function vfsError(msg: string): ShellError {
  if (msg.includes('not found') || msg.includes('ENOENT')) {
    return new ShellError('ERR_NOT_FOUND', msg);
  }
  if (msg.includes('exists') || msg.includes('EEXIST')) {
    return new ShellError('ERR_ALREADY_EXISTS', msg);
  }
  if (msg.includes('permission') || msg.includes('EACCES')) {
    return new ShellError('ERR_ACCESS_DENIED', msg);
  }
  return new ShellError('ERR_INTERNAL', msg);
}
