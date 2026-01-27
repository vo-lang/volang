// =============================================================================
// Virtual File System - 内存文件系统 + OPFS 持久化
// =============================================================================

interface FSNode {
  kind: 'file' | 'directory';
  mode: number;
  modTime: number;
  data?: Uint8Array;
  children?: Map<string, FSNode>;
}

interface OpenFile {
  path: string;
  node: FSNode;
  flags: number;
  position: number;
}

// O_* flags (must match vo-stdlib/src/os.rs)
const O_RDONLY = 0;
const O_WRONLY = 1;
const O_RDWR = 2;
const O_APPEND = 8;
const O_CREATE = 16;
const O_EXCL = 32;
const O_SYNC = 64;
const O_TRUNC = 128;

// Error messages
const ERR_NOT_EXIST = 'file does not exist';
const ERR_EXIST = 'file already exists';
const ERR_NOT_DIR = 'not a directory';
const ERR_IS_DIR = 'is a directory';
const ERR_INVALID = 'invalid argument';
const ERR_BAD_FD = 'invalid file descriptor';

export class VirtualFS {
  private root: FSNode;
  private openFiles = new Map<number, OpenFile>();
  private nextFd = 100;
  private dirty = false;
  private persistTimer: number | null = null;
  private initialized = false;

  constructor() {
    this.root = this.createDir(0o755);
  }

  // ===========================================================================
  // Initialization & Persistence
  // ===========================================================================

  async init(): Promise<void> {
    if (this.initialized) return;

    try {
      if (typeof navigator !== 'undefined' && navigator.storage?.getDirectory) {
        const opfsRoot = await navigator.storage.getDirectory();
        await this.loadFromOPFS(opfsRoot, this.root);
        console.log('[VFS] Loaded from OPFS');
      } else {
        console.log('[VFS] OPFS not available, using memory only');
      }
    } catch (e) {
      console.warn('[VFS] OPFS init failed, using memory only:', e);
    }

    this.initialized = true;

    // Register persistence hooks
    if (typeof window !== 'undefined') {
      window.addEventListener('beforeunload', () => this.forceFlush());
      document.addEventListener('visibilitychange', () => {
        if (document.hidden) this.forceFlush();
      });
    }
  }

  private async loadFromOPFS(
    opfsDir: FileSystemDirectoryHandle,
    memDir: FSNode
  ): Promise<void> {
    for await (const [name, handle] of (opfsDir as any).entries()) {
      if (handle.kind === 'file') {
        const fileHandle = handle as FileSystemFileHandle;
        const file = await fileHandle.getFile();
        const data = new Uint8Array(await file.arrayBuffer());
        memDir.children!.set(name, {
          kind: 'file',
          mode: 0o644,
          modTime: file.lastModified,
          data,
        });
      } else {
        const subDir = this.createDir(0o755);
        memDir.children!.set(name, subDir);
        await this.loadFromOPFS(handle as FileSystemDirectoryHandle, subDir);
      }
    }
  }

  private async persist(): Promise<void> {
    if (!this.dirty) return;

    try {
      if (typeof navigator !== 'undefined' && navigator.storage?.getDirectory) {
        const opfsRoot = await navigator.storage.getDirectory();
        await this.saveToOPFS(opfsRoot, this.root);
        this.dirty = false;
        console.log('[VFS] Persisted to OPFS');
      }
    } catch (e) {
      console.error('[VFS] Persist failed:', e);
    }
  }

  private async saveToOPFS(
    opfsDir: FileSystemDirectoryHandle,
    memDir: FSNode
  ): Promise<void> {
    // Get existing entries
    const existing = new Set<string>();
    for await (const [name] of (opfsDir as any).entries()) {
      existing.add(name);
    }

    // Sync memory → OPFS
    for (const [name, node] of memDir.children!) {
      existing.delete(name);

      if (node.kind === 'file') {
        const fileHandle = await opfsDir.getFileHandle(name, { create: true });
        const writable = await fileHandle.createWritable();
        await writable.write(node.data! as unknown as BufferSource);
        await writable.close();
      } else {
        const subDir = await opfsDir.getDirectoryHandle(name, { create: true });
        await this.saveToOPFS(subDir, node);
      }
    }

    // Remove deleted entries
    for (const name of existing) {
      await opfsDir.removeEntry(name, { recursive: true });
    }
  }

  private schedulePersist(): void {
    this.dirty = true;
    if (this.persistTimer === null) {
      this.persistTimer = window.setTimeout(() => {
        this.persistTimer = null;
        this.persist();
      }, 2000);
    }
  }

  async forceFlush(): Promise<void> {
    if (this.persistTimer !== null) {
      clearTimeout(this.persistTimer);
      this.persistTimer = null;
    }
    await this.persist();
  }

  // ===========================================================================
  // Path Utilities
  // ===========================================================================

  private normalizePath(path: string): string[] {
    const parts = path.split('/').filter((p) => p && p !== '.');
    const result: string[] = [];
    for (const part of parts) {
      if (part === '..') {
        result.pop();
      } else {
        result.push(part);
      }
    }
    return result;
  }

  private getNode(path: string): FSNode | null {
    const parts = this.normalizePath(path);
    let node = this.root;
    for (const part of parts) {
      if (node.kind !== 'directory') return null;
      const child = node.children!.get(part);
      if (!child) return null;
      node = child;
    }
    return node;
  }

  private getParentAndName(path: string): [FSNode | null, string] {
    const parts = this.normalizePath(path);
    if (parts.length === 0) return [null, ''];
    const name = parts.pop()!;
    let node = this.root;
    for (const part of parts) {
      if (node.kind !== 'directory') return [null, ''];
      const child = node.children!.get(part);
      if (!child) return [null, ''];
      node = child;
    }
    return [node, name];
  }

  private createDir(mode: number): FSNode {
    return { kind: 'directory', mode, modTime: Date.now(), children: new Map() };
  }

  private createFile(mode: number, data: Uint8Array = new Uint8Array(0)): FSNode {
    return { kind: 'file', mode, modTime: Date.now(), data };
  }

  // ===========================================================================
  // File Operations
  // ===========================================================================

  openFile(path: string, flags: number, mode: number): [number, string | null] {
    const [parent, name] = this.getParentAndName(path);
    if (!parent || parent.kind !== 'directory') {
      return [-1, ERR_NOT_EXIST];
    }

    let node = parent.children!.get(name);
    const access = flags & 0x3;
    const create = (flags & O_CREATE) !== 0;
    const excl = (flags & O_EXCL) !== 0;
    const trunc = (flags & O_TRUNC) !== 0;

    if (!node) {
      if (!create) return [-1, ERR_NOT_EXIST];
      node = this.createFile(mode);
      parent.children!.set(name, node);
      this.schedulePersist();
    } else {
      if (excl) return [-1, ERR_EXIST];
      if (node.kind === 'directory') return [-1, ERR_IS_DIR];
      if (trunc && access !== O_RDONLY) {
        node.data = new Uint8Array(0);
        node.modTime = Date.now();
        this.schedulePersist();
      }
    }

    const fd = this.nextFd++;
    const position = flags & O_APPEND ? node.data!.length : 0;
    this.openFiles.set(fd, { path, node, flags, position });
    return [fd, null];
  }

  read(fd: number, length: number): [Uint8Array | null, string | null] {
    const file = this.openFiles.get(fd);
    if (!file) return [null, ERR_BAD_FD];
    if (file.node.kind === 'directory') return [null, ERR_IS_DIR];

    const data = file.node.data!;
    const start = file.position;
    const end = Math.min(start + length, data.length);
    const chunk = data.slice(start, end);
    file.position = end;
    return [chunk, null];
  }

  write(fd: number, data: Uint8Array): [number, string | null] {
    const file = this.openFiles.get(fd);
    if (!file) return [0, ERR_BAD_FD];
    if (file.node.kind === 'directory') return [0, ERR_IS_DIR];

    const access = file.flags & 0x3;
    if (access === O_RDONLY) return [0, 'file not open for writing'];

    const existing = file.node.data!;
    const pos = file.position;
    const newLen = Math.max(existing.length, pos + data.length);

    if (newLen > existing.length) {
      const newData = new Uint8Array(newLen);
      newData.set(existing);
      file.node.data = newData;
    }

    file.node.data!.set(data, pos);
    file.position = pos + data.length;
    file.node.modTime = Date.now();
    this.schedulePersist();
    return [data.length, null];
  }

  readAt(fd: number, length: number, offset: number): [Uint8Array | null, string | null] {
    const file = this.openFiles.get(fd);
    if (!file) return [null, ERR_BAD_FD];
    if (file.node.kind === 'directory') return [null, ERR_IS_DIR];

    const data = file.node.data!;
    const start = offset;
    const end = Math.min(start + length, data.length);
    if (start >= data.length) return [new Uint8Array(0), null];
    return [data.slice(start, end), null];
  }

  writeAt(fd: number, data: Uint8Array, offset: number): [number, string | null] {
    const file = this.openFiles.get(fd);
    if (!file) return [0, ERR_BAD_FD];
    if (file.node.kind === 'directory') return [0, ERR_IS_DIR];

    const access = file.flags & 0x3;
    if (access === O_RDONLY) return [0, 'file not open for writing'];

    const existing = file.node.data!;
    const newLen = Math.max(existing.length, offset + data.length);

    if (newLen > existing.length) {
      const newData = new Uint8Array(newLen);
      newData.set(existing);
      file.node.data = newData;
    }

    file.node.data!.set(data, offset);
    file.node.modTime = Date.now();
    this.schedulePersist();
    return [data.length, null];
  }

  seek(fd: number, offset: number, whence: number): [number, string | null] {
    const file = this.openFiles.get(fd);
    if (!file) return [-1, ERR_BAD_FD];

    let newPos: number;
    switch (whence) {
      case 0:
        newPos = offset;
        break;
      case 1:
        newPos = file.position + offset;
        break;
      case 2:
        newPos = file.node.data!.length + offset;
        break;
      default:
        return [-1, ERR_INVALID];
    }

    if (newPos < 0) return [-1, ERR_INVALID];
    file.position = newPos;
    return [newPos, null];
  }

  close(fd: number): string | null {
    if (!this.openFiles.delete(fd)) return ERR_BAD_FD;
    return null;
  }

  sync(_fd: number): string | null {
    // Memory FS is always synced, just trigger persist
    this.schedulePersist();
    return null;
  }

  fstat(fd: number): [number, number, number, boolean, string | null] {
    const file = this.openFiles.get(fd);
    if (!file) return [0, 0, 0, false, ERR_BAD_FD];
    const n = file.node;
    const isDir = n.kind === 'directory';
    const size = isDir ? 0 : n.data!.length;
    return [size, n.mode, n.modTime, isDir, null];
  }

  ftruncate(fd: number, size: number): string | null {
    const file = this.openFiles.get(fd);
    if (!file) return ERR_BAD_FD;
    if (file.node.kind === 'directory') return ERR_IS_DIR;

    const existing = file.node.data!;
    if (size < existing.length) {
      file.node.data = existing.slice(0, size);
    } else if (size > existing.length) {
      const newData = new Uint8Array(size);
      newData.set(existing);
      file.node.data = newData;
    }
    file.node.modTime = Date.now();
    this.schedulePersist();
    return null;
  }

  // ===========================================================================
  // Directory Operations
  // ===========================================================================

  mkdir(path: string, mode: number): string | null {
    const [parent, name] = this.getParentAndName(path);
    if (!parent || parent.kind !== 'directory') return ERR_NOT_EXIST;
    if (parent.children!.has(name)) return ERR_EXIST;

    parent.children!.set(name, this.createDir(mode));
    this.schedulePersist();
    return null;
  }

  mkdirAll(path: string, mode: number): string | null {
    const parts = this.normalizePath(path);
    let node = this.root;
    for (const part of parts) {
      if (node.kind !== 'directory') return ERR_NOT_DIR;
      let child = node.children!.get(part);
      if (!child) {
        child = this.createDir(mode);
        node.children!.set(part, child);
        this.schedulePersist();
      } else if (child.kind !== 'directory') {
        return ERR_NOT_DIR;
      }
      node = child;
    }
    return null;
  }

  remove(path: string): string | null {
    const [parent, name] = this.getParentAndName(path);
    if (!parent || !name) return ERR_INVALID;
    const node = parent.children!.get(name);
    if (!node) return ERR_NOT_EXIST;
    if (node.kind === 'directory' && node.children!.size > 0) {
      return 'directory not empty';
    }
    parent.children!.delete(name);
    this.schedulePersist();
    return null;
  }

  removeAll(path: string): string | null {
    const [parent, name] = this.getParentAndName(path);
    if (!parent || !name) return ERR_INVALID;
    if (!parent.children!.has(name)) return ERR_NOT_EXIST;
    parent.children!.delete(name);
    this.schedulePersist();
    return null;
  }

  rename(oldPath: string, newPath: string): string | null {
    const [oldParent, oldName] = this.getParentAndName(oldPath);
    const [newParent, newName] = this.getParentAndName(newPath);
    if (!oldParent || !oldName || !newParent || !newName) return ERR_INVALID;

    const node = oldParent.children!.get(oldName);
    if (!node) return ERR_NOT_EXIST;

    oldParent.children!.delete(oldName);
    newParent.children!.set(newName, node);
    this.schedulePersist();
    return null;
  }

  stat(path: string): [string, number, number, number, boolean, string | null] {
    const parts = this.normalizePath(path);
    const name = parts.length > 0 ? parts[parts.length - 1] : '';
    const node = this.getNode(path);
    if (!node) return ['', 0, 0, 0, false, ERR_NOT_EXIST];
    const isDir = node.kind === 'directory';
    const size = isDir ? 0 : node.data!.length;
    return [name, size, node.mode, node.modTime, isDir, null];
  }

  readDir(path: string): [Array<[string, boolean, number]>, string | null] {
    const node = this.getNode(path);
    if (!node) return [[], ERR_NOT_EXIST];
    if (node.kind !== 'directory') return [[], ERR_NOT_DIR];

    const entries: Array<[string, boolean, number]> = [];
    for (const [name, child] of node.children!) {
      entries.push([name, child.kind === 'directory', child.mode]);
    }
    return [entries, null];
  }

  chmod(path: string, mode: number): string | null {
    const node = this.getNode(path);
    if (!node) return ERR_NOT_EXIST;
    node.mode = mode;
    this.schedulePersist();
    return null;
  }

  truncate(path: string, size: number): string | null {
    const node = this.getNode(path);
    if (!node) return ERR_NOT_EXIST;
    if (node.kind === 'directory') return ERR_IS_DIR;

    const existing = node.data!;
    if (size < existing.length) {
      node.data = existing.slice(0, size);
    } else if (size > existing.length) {
      const newData = new Uint8Array(size);
      newData.set(existing);
      node.data = newData;
    }
    node.modTime = Date.now();
    this.schedulePersist();
    return null;
  }

  // ===========================================================================
  // Convenience Methods
  // ===========================================================================

  readFile(path: string): [Uint8Array | null, string | null] {
    const node = this.getNode(path);
    if (!node) return [null, ERR_NOT_EXIST];
    if (node.kind === 'directory') return [null, ERR_IS_DIR];
    return [node.data!, null];
  }

  writeFile(path: string, data: Uint8Array, mode: number): string | null {
    // Ensure parent directories exist
    const parts = this.normalizePath(path);
    if (parts.length === 0) return ERR_INVALID;
    
    const fileName = parts.pop()!;
    let parent = this.root;
    
    for (const part of parts) {
      if (parent.kind !== 'directory') return ERR_NOT_DIR;
      let child = parent.children!.get(part);
      if (!child) {
        child = this.createDir(0o755);
        parent.children!.set(part, child);
      } else if (child.kind !== 'directory') {
        return ERR_NOT_DIR;
      }
      parent = child;
    }

    let node = parent.children!.get(fileName);
    if (node) {
      if (node.kind === 'directory') return ERR_IS_DIR;
      node.data = data;
      node.modTime = Date.now();
    } else {
      parent.children!.set(fileName, this.createFile(mode, data));
    }
    this.schedulePersist();
    return null;
  }
}

// ===========================================================================
// Global Instance & Bindings
// ===========================================================================

export const vfs = new VirtualFS();

// Convert BigInt to Number (wasm_bindgen passes i64 as BigInt)
const toNum = (v: any): number => typeof v === 'bigint' ? Number(v) : v;

/** Register VFS bindings on window for WASM to call */
export function registerVFSBindings(): void {
  const w = window as any;
  w._vfsOpenFile = (path: string, flags: any, mode: any) => vfs.openFile(path, toNum(flags), toNum(mode));
  w._vfsRead = (fd: any, length: any) => vfs.read(toNum(fd), toNum(length));
  w._vfsWrite = (fd: any, data: Uint8Array) => vfs.write(toNum(fd), data);
  w._vfsReadAt = (fd: any, length: any, offset: any) => vfs.readAt(toNum(fd), toNum(length), toNum(offset));
  w._vfsWriteAt = (fd: any, data: Uint8Array, offset: any) => vfs.writeAt(toNum(fd), data, toNum(offset));
  w._vfsSeek = (fd: any, offset: any, whence: any) => vfs.seek(toNum(fd), toNum(offset), toNum(whence));
  w._vfsClose = (fd: any) => vfs.close(toNum(fd));
  w._vfsSync = (fd: any) => vfs.sync(toNum(fd));
  w._vfsFstat = (fd: any) => vfs.fstat(toNum(fd));
  w._vfsFtruncate = (fd: any, size: any) => vfs.ftruncate(toNum(fd), toNum(size));
  w._vfsMkdir = (path: string, mode: any) => vfs.mkdir(path, toNum(mode));
  w._vfsMkdirAll = (path: string, mode: any) => vfs.mkdirAll(path, toNum(mode));
  w._vfsRemove = (path: string) => vfs.remove(path);
  w._vfsRemoveAll = (path: string) => vfs.removeAll(path);
  w._vfsRename = (oldPath: string, newPath: string) => vfs.rename(oldPath, newPath);
  w._vfsStat = (path: string) => vfs.stat(path);
  w._vfsReadDir = (path: string) => vfs.readDir(path);
  w._vfsChmod = (path: string, mode: any) => vfs.chmod(path, toNum(mode));
  w._vfsTruncate = (path: string, size: any) => vfs.truncate(path, toNum(size));
  w._vfsReadFile = (path: string) => vfs.readFile(path);
  w._vfsWriteFile = (path: string, data: Uint8Array, mode: any) => vfs.writeFile(path, data, toNum(mode));
}
