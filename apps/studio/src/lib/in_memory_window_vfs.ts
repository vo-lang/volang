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

type OpenVfsFile = {
  path: string;
  flags: number;
  position: number;
};

const ROOT = '/';
const O_RDONLY = 0;
const O_APPEND = 8;
const O_CREATE = 16;
const O_EXCL = 32;
const O_TRUNC = 128;
const ERR_NOT_EXIST = 'file does not exist';
const ERR_EXIST = 'file already exists';
const ERR_NOT_DIR = 'not a directory';
const ERR_IS_DIR = 'is a directory';
const ERR_INVALID = 'invalid argument';
const ERR_BAD_FD = 'invalid file descriptor';

function normalizePath(path: string, rootPath: string): string {
  const normalized = path.replace(/\\/g, '/').trim();
  if (!normalized || normalized === '.') {
    return rootPath;
  }
  const absolute = normalized.startsWith(ROOT);
  const parts = absolute
    ? []
    : (rootPath === ROOT ? [] : rootPath.slice(1).split('/').filter(Boolean));
  for (const part of normalized.split('/')) {
    if (!part || part === '.') {
      continue;
    }
    if (part === '..') {
      if (parts.length > 0) {
        parts.pop();
      }
      continue;
    }
    parts.push(part);
  }
  return parts.length === 0 ? ROOT : `/${parts.join('/')}`;
}

function dirname(path: string): string {
  if (path === ROOT) {
    return ROOT;
  }
  const index = path.lastIndexOf(ROOT);
  return index <= 0 ? ROOT : path.slice(0, index);
}

function basename(path: string): string {
  if (path === ROOT) {
    return '';
  }
  return path.slice(path.lastIndexOf(ROOT) + 1);
}

export function createInMemoryWindowVfsBackend(
  options: InMemoryWindowVfsOptions = {},
): WindowVfsBackend {
  const rootPath = normalizePath(options.rootPath ?? ROOT, ROOT);
  const files = new Map<string, Uint8Array>();
  const fileModes = new Map<string, number>();
  const fileModTimes = new Map<string, number>();
  const directories = new Set<string>([ROOT]);
  const dirModes = new Map<string, number>([[ROOT, 0o755]]);
  const dirModTimes = new Map<string, number>([[ROOT, Date.now()]]);
  const openFiles = new Map<number, OpenVfsFile>();
  let nextFd = 100;

  const ensureDir = (path: string, mode = 0o755): string | null => {
    const normalized = normalizePath(path, rootPath);
    if (normalized === ROOT) {
      return null;
    }
    const parts = normalized.slice(1).split('/').filter(Boolean);
    let current = ROOT;
    for (const part of parts) {
      current = current === ROOT ? `/${part}` : `${current}/${part}`;
      if (files.has(current)) {
        return ERR_NOT_DIR;
      }
      if (!directories.has(current)) {
        directories.add(current);
        dirModes.set(current, mode);
        dirModTimes.set(current, Date.now());
      }
    }
    return null;
  };

  const setFile = (path: string, bytes: Uint8Array, mode: number): string | null => {
    const normalized = normalizePath(path, rootPath);
    if (directories.has(normalized)) {
      return ERR_IS_DIR;
    }
    const parentError = ensureDir(dirname(normalized));
    if (parentError) {
      return parentError;
    }
    files.set(normalized, new Uint8Array(bytes));
    fileModes.set(normalized, mode);
    fileModTimes.set(normalized, Date.now());
    return null;
  };

  const deleteFile = (path: string): void => {
    files.delete(path);
    fileModes.delete(path);
    fileModTimes.delete(path);
  };

  const listEntries = (path: string): Array<[string, boolean, number]> => {
    const normalized = normalizePath(path, rootPath);
    const entries = new Map<string, [string, boolean, number]>();
    for (const dir of directories) {
      if (dir === normalized || dirname(dir) !== normalized) {
        continue;
      }
      const name = basename(dir);
      entries.set(name, [name, true, dirModes.get(dir) ?? 0o755]);
    }
    for (const [filePath] of files) {
      if (dirname(filePath) !== normalized) {
        continue;
      }
      const name = basename(filePath);
      entries.set(name, [name, false, fileModes.get(filePath) ?? 0o644]);
    }
    return [...entries.values()].sort((a, b) => a[0].localeCompare(b[0]));
  };

  const writeBytes = (path: string, offset: number, data: Uint8Array): number => {
    const normalized = normalizePath(path, rootPath);
    const existing = files.get(normalized) ?? new Uint8Array(0);
    const next = new Uint8Array(Math.max(existing.length, offset + data.length));
    next.set(existing);
    next.set(data, offset);
    setFile(normalized, next, fileModes.get(normalized) ?? 0o644);
    return data.length;
  };

  const truncatePath = (path: string, size: number): string | null => {
    const normalized = normalizePath(path, rootPath);
    if (directories.has(normalized)) {
      return ERR_IS_DIR;
    }
    const existing = files.get(normalized);
    if (!existing) {
      return ERR_NOT_EXIST;
    }
    let next: Uint8Array;
    if (size < existing.length) {
      next = existing.slice(0, size);
    } else if (size > existing.length) {
      next = new Uint8Array(size);
      next.set(existing);
    } else {
      next = existing;
    }
    return setFile(normalized, next, fileModes.get(normalized) ?? 0o644);
  };

  const removeDirTree = (path: string): void => {
    const dirPaths = [...directories]
      .filter((dir) => dir === path || dir.startsWith(`${path}/`))
      .sort((a, b) => b.length - a.length);
    for (const dir of dirPaths) {
      if (dir === ROOT) {
        continue;
      }
      directories.delete(dir);
      dirModes.delete(dir);
      dirModTimes.delete(dir);
    }
  };

  const removeFileTree = (path: string): void => {
    for (const filePath of [...files.keys()]) {
      if (filePath === path || filePath.startsWith(`${path}/`)) {
        deleteFile(filePath);
      }
    }
  };

  ensureDir(rootPath);
  for (const file of options.files ?? []) {
    setFile(file.path, file.bytes, file.mode ?? 0o644);
  }

  let backend!: WindowVfsBackend;
  backend = {
    openFile(path: string, flags: number, mode: number): [number, string | null] {
      const normalized = normalizePath(path, rootPath);
      const access = flags & 0x3;
      const create = (flags & O_CREATE) !== 0;
      const excl = (flags & O_EXCL) !== 0;
      const trunc = (flags & O_TRUNC) !== 0;
      let bytes = files.get(normalized);
      if (!bytes) {
        if (!create) {
          return [-1, ERR_NOT_EXIST];
        }
        const createError = setFile(normalized, new Uint8Array(0), mode);
        if (createError) {
          return [-1, createError];
        }
        bytes = files.get(normalized) ?? new Uint8Array(0);
      } else {
        if (excl) {
          return [-1, ERR_EXIST];
        }
        if (directories.has(normalized)) {
          return [-1, ERR_IS_DIR];
        }
        if (trunc && access !== O_RDONLY) {
          const truncateError = setFile(normalized, new Uint8Array(0), fileModes.get(normalized) ?? mode);
          if (truncateError) {
            return [-1, truncateError];
          }
          bytes = files.get(normalized) ?? new Uint8Array(0);
        }
      }
      const fd = nextFd;
      nextFd += 1;
      openFiles.set(fd, {
        path: normalized,
        flags,
        position: (flags & O_APPEND) !== 0 ? bytes.length : 0,
      });
      return [fd, null];
    },
    read(fd: number, length: number): [Uint8Array | null, string | null] {
      const file = openFiles.get(fd);
      if (!file) {
        return [null, ERR_BAD_FD];
      }
      if (directories.has(file.path)) {
        return [null, ERR_IS_DIR];
      }
      const bytes = files.get(file.path) ?? new Uint8Array(0);
      const start = file.position;
      const end = Math.min(start + length, bytes.length);
      const chunk = bytes.slice(start, end);
      file.position = end;
      return [chunk, null];
    },
    write(fd: number, data: Uint8Array): [number, string | null] {
      const file = openFiles.get(fd);
      if (!file) {
        return [0, ERR_BAD_FD];
      }
      if (directories.has(file.path)) {
        return [0, ERR_IS_DIR];
      }
      if ((file.flags & 0x3) === O_RDONLY) {
        return [0, 'file not open for writing'];
      }
      const written = writeBytes(file.path, file.position, data);
      file.position += written;
      return [written, null];
    },
    readAt(fd: number, length: number, offset: number): [Uint8Array | null, string | null] {
      const file = openFiles.get(fd);
      if (!file) {
        return [null, ERR_BAD_FD];
      }
      if (directories.has(file.path)) {
        return [null, ERR_IS_DIR];
      }
      const bytes = files.get(file.path) ?? new Uint8Array(0);
      if (offset >= bytes.length) {
        return [new Uint8Array(0), null];
      }
      return [bytes.slice(offset, Math.min(offset + length, bytes.length)), null];
    },
    writeAt(fd: number, data: Uint8Array, offset: number): [number, string | null] {
      const file = openFiles.get(fd);
      if (!file) {
        return [0, ERR_BAD_FD];
      }
      if (directories.has(file.path)) {
        return [0, ERR_IS_DIR];
      }
      if ((file.flags & 0x3) === O_RDONLY) {
        return [0, 'file not open for writing'];
      }
      return [writeBytes(file.path, offset, data), null];
    },
    seek(fd: number, offset: number, whence: number): [number, string | null] {
      const file = openFiles.get(fd);
      if (!file) {
        return [-1, ERR_BAD_FD];
      }
      const bytes = files.get(file.path) ?? new Uint8Array(0);
      let nextPosition: number;
      switch (whence) {
        case 0:
          nextPosition = offset;
          break;
        case 1:
          nextPosition = file.position + offset;
          break;
        case 2:
          nextPosition = bytes.length + offset;
          break;
        default:
          return [-1, ERR_INVALID];
      }
      if (nextPosition < 0) {
        return [-1, ERR_INVALID];
      }
      file.position = nextPosition;
      return [nextPosition, null];
    },
    close(fd: number): string | null {
      return openFiles.delete(fd) ? null : ERR_BAD_FD;
    },
    sync(_fd: number): string | null {
      return null;
    },
    fstat(fd: number): [number, number, number, boolean, string | null] {
      const file = openFiles.get(fd);
      if (!file) {
        return [0, 0, 0, false, ERR_BAD_FD];
      }
      const isDir = directories.has(file.path);
      const size = isDir ? 0 : (files.get(file.path)?.length ?? 0);
      return [
        size,
        isDir ? (dirModes.get(file.path) ?? 0o755) : (fileModes.get(file.path) ?? 0o644),
        isDir ? (dirModTimes.get(file.path) ?? 0) : (fileModTimes.get(file.path) ?? 0),
        isDir,
        null,
      ];
    },
    ftruncate(fd: number, size: number): string | null {
      const file = openFiles.get(fd);
      if (!file) {
        return ERR_BAD_FD;
      }
      return truncatePath(file.path, size);
    },
    mkdir(path: string, mode: number): string | null {
      const normalized = normalizePath(path, rootPath);
      const parent = dirname(normalized);
      if (!directories.has(parent)) {
        return ERR_NOT_EXIST;
      }
      if (directories.has(normalized) || files.has(normalized)) {
        return ERR_EXIST;
      }
      directories.add(normalized);
      dirModes.set(normalized, mode);
      dirModTimes.set(normalized, Date.now());
      return null;
    },
    mkdirAll(path: string, mode: number): string | null {
      return ensureDir(path, mode);
    },
    remove(path: string): string | null {
      const normalized = normalizePath(path, rootPath);
      if (files.has(normalized)) {
        deleteFile(normalized);
        return null;
      }
      if (!directories.has(normalized)) {
        return ERR_NOT_EXIST;
      }
      if (listEntries(normalized).length > 0) {
        return 'directory not empty';
      }
      if (normalized === ROOT) {
        return ERR_INVALID;
      }
      directories.delete(normalized);
      dirModes.delete(normalized);
      dirModTimes.delete(normalized);
      return null;
    },
    removeAll(path: string): string | null {
      const normalized = normalizePath(path, rootPath);
      if (files.has(normalized)) {
        deleteFile(normalized);
        return null;
      }
      if (!directories.has(normalized)) {
        return ERR_NOT_EXIST;
      }
      if (normalized === ROOT) {
        removeFileTree(ROOT);
        removeDirTree(ROOT);
        return null;
      }
      removeFileTree(normalized);
      removeDirTree(normalized);
      return null;
    },
    rename(oldPath: string, newPath: string): string | null {
      const oldNormalized = normalizePath(oldPath, rootPath);
      const newNormalized = normalizePath(newPath, rootPath);
      if (files.has(oldNormalized)) {
        const error = setFile(newNormalized, files.get(oldNormalized) ?? new Uint8Array(0), fileModes.get(oldNormalized) ?? 0o644);
        if (error) {
          return error;
        }
        deleteFile(oldNormalized);
        return null;
      }
      if (!directories.has(oldNormalized)) {
        return ERR_NOT_EXIST;
      }
      if (oldNormalized === ROOT) {
        return ERR_INVALID;
      }
      const parentError = ensureDir(dirname(newNormalized));
      if (parentError) {
        return parentError;
      }
      const dirSubtree = [...directories]
        .filter((dir) => dir === oldNormalized || dir.startsWith(`${oldNormalized}/`))
        .sort((a, b) => a.length - b.length);
      const fileSubtree = [...files.keys()]
        .filter((filePath) => filePath.startsWith(`${oldNormalized}/`))
        .sort((a, b) => a.length - b.length);
      for (const dir of dirSubtree) {
        const target = `${newNormalized}${dir.slice(oldNormalized.length)}`;
        directories.add(target);
        dirModes.set(target, dirModes.get(dir) ?? 0o755);
        dirModTimes.set(target, dirModTimes.get(dir) ?? Date.now());
      }
      for (const filePath of fileSubtree) {
        const target = `${newNormalized}${filePath.slice(oldNormalized.length)}`;
        const error = setFile(target, files.get(filePath) ?? new Uint8Array(0), fileModes.get(filePath) ?? 0o644);
        if (error) {
          return error;
        }
      }
      removeFileTree(oldNormalized);
      removeDirTree(oldNormalized);
      return null;
    },
    stat(path: string): [string, number, number, number, boolean, string | null] {
      const normalized = normalizePath(path, rootPath);
      const name = basename(normalized);
      if (directories.has(normalized)) {
        return [name, 0, dirModes.get(normalized) ?? 0o755, dirModTimes.get(normalized) ?? 0, true, null];
      }
      const bytes = files.get(normalized);
      if (!bytes) {
        return ['', 0, 0, 0, false, ERR_NOT_EXIST];
      }
      return [name, bytes.length, fileModes.get(normalized) ?? 0o644, fileModTimes.get(normalized) ?? 0, false, null];
    },
    readDir(path: string): [Array<[string, boolean, number]>, string | null] {
      const normalized = normalizePath(path, rootPath);
      if (!directories.has(normalized)) {
        return [[], ERR_NOT_EXIST];
      }
      return [listEntries(normalized), null];
    },
    chmod(path: string, mode: number): string | null {
      const normalized = normalizePath(path, rootPath);
      if (directories.has(normalized)) {
        dirModes.set(normalized, mode);
        return null;
      }
      if (!files.has(normalized)) {
        return ERR_NOT_EXIST;
      }
      fileModes.set(normalized, mode);
      return null;
    },
    truncate(path: string, size: number): string | null {
      return truncatePath(path, size);
    },
    readFile(path: string): [Uint8Array | null, string | null] {
      const normalized = normalizePath(path, rootPath);
      if (directories.has(normalized)) {
        return [null, ERR_IS_DIR];
      }
      const bytes = files.get(normalized);
      if (!bytes) {
        return [null, ERR_NOT_EXIST];
      }
      return [new Uint8Array(bytes), null];
    },
    writeFile(path: string, data: Uint8Array, mode: number): string | null {
      return setFile(path, data, mode);
    },
  };

  return backend;
}
