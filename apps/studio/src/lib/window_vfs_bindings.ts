export interface WindowVfsBackend {
  openFile(path: string, flags: number, mode: number): [number, string | null];
  read(fd: number, length: number): [Uint8Array | null, string | null];
  write(fd: number, data: Uint8Array): [number, string | null];
  readAt(fd: number, length: number, offset: number): [Uint8Array | null, string | null];
  writeAt(fd: number, data: Uint8Array, offset: number): [number, string | null];
  seek(fd: number, offset: number, whence: number): [number, string | null];
  close(fd: number): string | null;
  sync(fd: number): string | null;
  fstat(fd: number): [number, number, number, boolean, string | null];
  ftruncate(fd: number, size: number): string | null;
  mkdir(path: string, mode: number): string | null;
  mkdirAll(path: string, mode: number): string | null;
  remove(path: string): string | null;
  removeAll(path: string): string | null;
  rename(oldPath: string, newPath: string): string | null;
  renameNoreplace(oldPath: string, newPath: string): string | null;
  stat(path: string): [string, number, number, number, boolean, string | null];
  readDir(path: string): [Array<[string, boolean, number]>, string | null];
  chmod(path: string, mode: number): string | null;
  truncate(path: string, size: number): string | null;
  getwd(): [string, string | null];
  chdir(path: string): string | null;
  readFile(path: string): [Uint8Array | null, string | null];
  readFileLimited(path: string, maxBytes: number): [Uint8Array | null, string | null];
  writeFile(path: string, data: Uint8Array, mode: number): string | null;
  resolveGuestPath(path: string): [string | null, string | null];
  guestGetwd(): [string, string | null];
}

let activeWindowVfsBackend: WindowVfsBackend | null = null;

function toNum(value: number | bigint): number {
  if (typeof value !== 'bigint') {
    return value;
  }
  if (value < BigInt(Number.MIN_SAFE_INTEGER) || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    return Number.NaN;
  }
  return Number(value);
}

function requireWindowVfsBackend(): WindowVfsBackend {
  if (!activeWindowVfsBackend) {
    throw new Error('Window VFS backend is not installed');
  }
  return activeWindowVfsBackend;
}

export function hasWindowVfsBindings(): boolean {
  const windowWithVfs = window as unknown as Record<string, unknown>;
  return [
    '_vfsOpenFile',
    '_vfsRead',
    '_vfsWrite',
    '_vfsReadAt',
    '_vfsWriteAt',
    '_vfsSeek',
    '_vfsClose',
    '_vfsSync',
    '_vfsFstat',
    '_vfsFtruncate',
    '_vfsMkdir',
    '_vfsMkdirAll',
    '_vfsRemove',
    '_vfsRemoveAll',
    '_vfsRename',
    '_vfsRenameNoreplace',
    '_vfsStat',
    '_vfsReadDir',
    '_vfsChmod',
    '_vfsTruncate',
    '_vfsGetwd',
    '_vfsChdir',
    '_vfsReadFile',
    '_vfsReadFileLimited',
    '_vfsWriteFile',
    '_vfsResolveGuestPath',
    '_vfsGuestGetwd',
  ].every((name) => typeof windowWithVfs[name] === 'function');
}

export function installWindowVfsBackend(backend: WindowVfsBackend): void {
  activeWindowVfsBackend = backend;
  const windowWithVfs = window as unknown as Record<string, unknown>;
  windowWithVfs._vfsOpenFile = (path: string, flags: number | bigint, mode: number | bigint) =>
    requireWindowVfsBackend().openFile(path, toNum(flags), toNum(mode));
  windowWithVfs._vfsRead = (fd: number | bigint, length: number | bigint) =>
    requireWindowVfsBackend().read(toNum(fd), toNum(length));
  windowWithVfs._vfsWrite = (fd: number | bigint, data: Uint8Array) =>
    requireWindowVfsBackend().write(toNum(fd), data);
  windowWithVfs._vfsReadAt = (fd: number | bigint, length: number | bigint, offset: number | bigint) =>
    requireWindowVfsBackend().readAt(toNum(fd), toNum(length), toNum(offset));
  windowWithVfs._vfsWriteAt = (fd: number | bigint, data: Uint8Array, offset: number | bigint) =>
    requireWindowVfsBackend().writeAt(toNum(fd), data, toNum(offset));
  windowWithVfs._vfsSeek = (fd: number | bigint, offset: number | bigint, whence: number | bigint) =>
    requireWindowVfsBackend().seek(toNum(fd), toNum(offset), toNum(whence));
  windowWithVfs._vfsClose = (fd: number | bigint) => requireWindowVfsBackend().close(toNum(fd));
  windowWithVfs._vfsSync = (fd: number | bigint) => requireWindowVfsBackend().sync(toNum(fd));
  windowWithVfs._vfsFstat = (fd: number | bigint) => requireWindowVfsBackend().fstat(toNum(fd));
  windowWithVfs._vfsFtruncate = (fd: number | bigint, size: number | bigint) =>
    requireWindowVfsBackend().ftruncate(toNum(fd), toNum(size));
  windowWithVfs._vfsMkdir = (path: string, mode: number | bigint) =>
    requireWindowVfsBackend().mkdir(path, toNum(mode));
  windowWithVfs._vfsMkdirAll = (path: string, mode: number | bigint) =>
    requireWindowVfsBackend().mkdirAll(path, toNum(mode));
  windowWithVfs._vfsRemove = (path: string) => requireWindowVfsBackend().remove(path);
  windowWithVfs._vfsRemoveAll = (path: string) => requireWindowVfsBackend().removeAll(path);
  windowWithVfs._vfsRename = (oldPath: string, newPath: string) => requireWindowVfsBackend().rename(oldPath, newPath);
  windowWithVfs._vfsRenameNoreplace = (oldPath: string, newPath: string) =>
    requireWindowVfsBackend().renameNoreplace(oldPath, newPath);
  windowWithVfs._vfsStat = (path: string) => requireWindowVfsBackend().stat(path);
  windowWithVfs._vfsReadDir = (path: string) => requireWindowVfsBackend().readDir(path);
  windowWithVfs._vfsChmod = (path: string, mode: number | bigint) => requireWindowVfsBackend().chmod(path, toNum(mode));
  windowWithVfs._vfsTruncate = (path: string, size: number | bigint) => requireWindowVfsBackend().truncate(path, toNum(size));
  windowWithVfs._vfsGetwd = () => requireWindowVfsBackend().getwd();
  windowWithVfs._vfsChdir = (path: string) => requireWindowVfsBackend().chdir(path);
  windowWithVfs._vfsReadFile = (path: string) => requireWindowVfsBackend().readFile(path);
  windowWithVfs._vfsReadFileLimited = (path: string, maxBytes: number | bigint) =>
    requireWindowVfsBackend().readFileLimited(path, toNum(maxBytes));
  windowWithVfs._vfsWriteFile = (path: string, data: Uint8Array, mode: number | bigint) =>
    requireWindowVfsBackend().writeFile(path, data, toNum(mode));
  windowWithVfs._vfsResolveGuestPath = (path: string) => requireWindowVfsBackend().resolveGuestPath(path);
  windowWithVfs._vfsGuestGetwd = () => requireWindowVfsBackend().guestGetwd();
}
