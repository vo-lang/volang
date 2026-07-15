// =============================================================================
// Virtual File System - bounded in-memory filesystem with OPFS persistence
// =============================================================================

interface FSNode {
  kind: 'file' | 'directory';
  mode: number;
  modTime: number;
  data?: Uint8Array;
  children?: Map<string, FSNode>;
  openCount: number;
  accounted: boolean;
  orphanRoot: boolean;
}

interface OpenFile {
  node: FSNode;
  flags: number;
  position: number;
}

interface ResolvedPath {
  parts: string[];
  absolute: string;
  trailingSlash: boolean;
}

interface LoadBudget {
  nodes: number;
  bytes: number;
}

interface PersistedNodeMetadata {
  mode: number;
  modTime: number;
  kind: 'file' | 'directory';
}

type PersistedMetadataEntry = [string, number, number, 'f' | 'd'];

// O_* flags (must match vo-stdlib/src/os.rs).
export const O_RDONLY = 0;
export const O_WRONLY = 1;
export const O_RDWR = 2;
export const O_APPEND = 8;
export const O_CREATE = 16;
export const O_EXCL = 32;
export const O_SYNC = 64;
export const O_TRUNC = 128;

const KNOWN_OPEN_FLAGS =
  O_WRONLY | O_RDWR | O_APPEND | O_CREATE | O_EXCL | O_SYNC | O_TRUNC;
const ACCESS_MASK = 0x3;
const MODE_SETUID = 1 << 23;
const MODE_SETGID = 1 << 22;
const MODE_STICKY = 1 << 20;
const MUTABLE_MODE_BITS = 0o777 | MODE_SETUID | MODE_SETGID | MODE_STICKY;

// Browser-side limits prevent one guest operation or persisted tree from
// exhausting the page's address space before Rust can enforce its own limits.
export const MAX_VFS_FILE_BYTES = 256 * 1024 * 1024;
export const MAX_VFS_IO_BYTES = 64 * 1024 * 1024;
export const MAX_VFS_TOTAL_BYTES = 512 * 1024 * 1024;
export const MAX_VFS_NODES = 100_000;
export const MAX_VFS_DIRECTORY_ENTRIES = 100_000;
const MAX_VFS_PATH_DEPTH = 256;
const MAX_VFS_PATH_LENGTH = 4096;
const MAX_VFS_NAME_LENGTH = 255;
const MAX_OPEN_FILES = 65_536;
const FIRST_FD = 100;
const LAST_FD = 0x7fffffff;
const OPFS_NAMESPACE = 'vo-web-vfs-v1';
const OPFS_DATA_DIRECTORY = 'data';
const OPFS_METADATA_FILE = 'metadata.json';
const MAX_OPFS_METADATA_BYTES = 64 * 1024 * 1024;

// Error text is part of the Rust bridge contract. Sentinel-compatible errors
// stay exact; detailed invalid-input errors retain the sentinel prefix.
const ERR_NOT_EXIST = 'file does not exist';
const ERR_EXIST = 'file already exists';
const ERR_PERMISSION = 'permission denied';
const ERR_INVALID = 'invalid argument';
const ERR_CLOSED = 'file already closed';
const ERR_NOT_DIR = 'not a directory';
const ERR_IS_DIR = 'is a directory';
const ERR_DIR_NOT_EMPTY = 'directory not empty';
const ERR_FILE_TOO_LARGE = 'file too large';
const ERR_OUT_OF_MEMORY = 'out of memory';
const ERR_TOO_MANY_FILES = 'too many open files';
const UTF8_ENCODER = new TextEncoder();

function invalidArgument(detail: string): string {
  return `${ERR_INVALID}: ${detail}`;
}

function isSafeIntegerInRange(value: number, min: number, max: number): boolean {
  return Number.isSafeInteger(value) && value >= min && value <= max;
}

function modeBits(mode: number): number | null {
  if (!Number.isSafeInteger(mode) || mode < 0 || mode > 0xffffffff) return null;
  return mode & MUTABLE_MODE_BITS;
}

function startsWithParts(path: readonly string[], prefix: readonly string[]): boolean {
  if (prefix.length > path.length) return false;
  for (let i = 0; i < prefix.length; i += 1) {
    if (path[i] !== prefix[i]) return false;
  }
  return true;
}

function compareUtf8Bytes(a: Uint8Array, b: Uint8Array): number {
  const length = Math.min(a.length, b.length);
  for (let i = 0; i < length; i += 1) {
    if (a[i] !== b[i]) return a[i] - b[i];
  }
  return a.length - b.length;
}

function sortByUtf8Name<T>(values: T[], nameOf: (value: T) => string): T[] {
  const encoded = new Map<string, Uint8Array>();
  for (const value of values) {
    const name = nameOf(value);
    if (!encoded.has(name)) encoded.set(name, UTF8_ENCODER.encode(name));
  }
  values.sort((left, right) =>
    compareUtf8Bytes(encoded.get(nameOf(left))!, encoded.get(nameOf(right))!)
  );
  return values;
}

export class VirtualFS {
  private root: FSNode;
  private liveNodes = 0;
  private liveBytes = 0;
  private cwdParts: string[] = [];
  private openFiles = new Map<number, OpenFile>();
  private nextFd = FIRST_FD;
  private dirty = false;
  private mutationVersion = 0;
  private persistTimer: number | null = null;
  private persistInFlight: Promise<void> | null = null;
  private initInFlight: Promise<void> | null = null;
  private initialized = false;
  private opfsAvailable = false;

  constructor() {
    this.root = this.createDir(0o755);
    this.resetAccounting();
    this.ensurePlatformDirectories();
  }

  // ===========================================================================
  // Initialization & Persistence
  // ===========================================================================

  async init(): Promise<void> {
    if (this.initialized) return;
    if (this.initInFlight) return this.initInFlight;

    const operation = this.initialize();
    this.initInFlight = operation;
    try {
      await operation;
    } finally {
      if (this.initInFlight === operation) this.initInFlight = null;
    }
  }

  private async initialize(): Promise<void> {

    this.opfsAvailable =
      typeof navigator !== 'undefined' && Boolean(navigator.storage?.getDirectory);

    if (this.opfsAvailable) {
      try {
        const { namespace, data } = await this.openOPFSStorage();
        const metadata = await this.loadMetadataFromOPFS(namespace);
        const rootMetadata = metadata.get('/');
        const loadedRoot = this.createDir(
          rootMetadata?.kind === 'directory' ? rootMetadata.mode : 0o755
        );
        if (rootMetadata?.kind === 'directory') {
          loadedRoot.modTime = rootMetadata.modTime;
        }
        const budget: LoadBudget = { nodes: 1, bytes: 0 };
        await this.loadFromOPFS(data, loadedRoot, budget, 0, 1, [], metadata);
        this.root = loadedRoot;
        this.resetAccounting();
        this.ensurePlatformDirectories();
        this.cwdParts = [];
        this.openFiles.clear();
        this.nextFd = FIRST_FD;
        this.dirty = false;
        console.log('[VFS] Loaded from OPFS');
      } catch (error) {
        this.opfsAvailable = false;
        console.warn('[VFS] OPFS init failed, using memory only:', error);
      }
    } else {
      console.log('[VFS] OPFS not available, using memory only');
    }

    this.initialized = true;

    if (typeof window !== 'undefined') {
      window.addEventListener('beforeunload', () => {
        // Browsers do not wait for asynchronous work during unload. Keep the
        // attempt best-effort and suppress a rejected Promise at this boundary.
        void this.forceFlush().catch((error) => {
          console.error('[VFS] Final OPFS checkpoint failed:', error);
        });
      });
    }
    if (typeof document !== 'undefined') {
      document.addEventListener('visibilitychange', () => {
        if (document.hidden) {
          void this.forceFlush().catch((error) => {
            console.error('[VFS] Background OPFS checkpoint failed:', error);
          });
        }
      });
    }
  }

  private async loadFromOPFS(
    opfsDir: FileSystemDirectoryHandle,
    memDir: FSNode,
    budget: LoadBudget,
    depth: number,
    pathLength: number,
    parentParts: readonly string[],
    metadata: ReadonlyMap<string, PersistedNodeMetadata>
  ): Promise<void> {
    if (depth > MAX_VFS_PATH_DEPTH) {
      throw new Error('OPFS directory depth exceeds the VFS limit');
    }

    for await (const [name, handle] of (opfsDir as any).entries()) {
      const nameBytes = typeof name === 'string' ? UTF8_ENCODER.encode(name).length : 0;
      if (
        typeof name !== 'string'
        || name.length === 0
        || nameBytes > MAX_VFS_NAME_LENGTH
        || name === '.'
        || name === '..'
        || name.includes('/')
        || name.includes('\0')
      ) {
        throw new Error('OPFS contains an invalid VFS entry name');
      }
      if (depth >= MAX_VFS_PATH_DEPTH) {
        throw new Error('OPFS directory depth exceeds the VFS limit');
      }
      const childPathLength = pathLength + (pathLength === 1 ? 0 : 1) + nameBytes;
      if (childPathLength > MAX_VFS_PATH_LENGTH) {
        throw new Error('OPFS path length exceeds the VFS limit');
      }
      budget.nodes += 1;
      if (budget.nodes > MAX_VFS_NODES) {
        throw new Error('OPFS node count exceeds the VFS limit');
      }
      const childParts = [...parentParts, name];
      const absolute = `/${childParts.join('/')}`;
      const persisted = metadata.get(absolute);

      if (handle.kind === 'file') {
        const file = await (handle as FileSystemFileHandle).getFile();
        if (file.size > MAX_VFS_FILE_BYTES) {
          throw new Error(`OPFS file ${name} exceeds the VFS file-size limit`);
        }
        budget.bytes += file.size;
        if (budget.bytes > MAX_VFS_TOTAL_BYTES) {
          throw new Error('OPFS contents exceed the VFS total-size limit');
        }
        const data = new Uint8Array(await file.arrayBuffer());
        memDir.children!.set(name, {
          kind: 'file',
          mode: persisted?.kind === 'file' ? persisted.mode : 0o644,
          modTime: persisted?.kind === 'file' ? persisted.modTime : file.lastModified,
          data,
          openCount: 0,
          accounted: false,
          orphanRoot: false,
        });
      } else if (handle.kind === 'directory') {
        const subDir = this.createDir(
          persisted?.kind === 'directory' ? persisted.mode : 0o755
        );
        if (persisted?.kind === 'directory') subDir.modTime = persisted.modTime;
        memDir.children!.set(name, subDir);
        await this.loadFromOPFS(
          handle as FileSystemDirectoryHandle,
          subDir,
          budget,
          depth + 1,
          childPathLength,
          childParts,
          metadata
        );
      } else {
        throw new Error('OPFS contains an unsupported entry kind');
      }
    }
  }

  private async persist(): Promise<void> {
    if (!this.dirty || !this.opfsAvailable) return;
    if (this.persistInFlight) {
      await this.persistInFlight;
      return;
    }

    const version = this.mutationVersion;
    const operation = (async () => {
      const { namespace, data } = await this.openOPFSStorage();
      await this.saveToOPFS(data, this.root);
      await this.saveMetadataToOPFS(namespace);
      if (this.mutationVersion === version) {
        this.dirty = false;
      }
    })();
    this.persistInFlight = operation;

    try {
      await operation;
      console.log('[VFS] Persisted to OPFS');
    } catch (error) {
      this.dirty = true;
      console.error('[VFS] Persist failed:', error);
      throw error;
    } finally {
      this.persistInFlight = null;
      if (this.dirty) this.schedulePersistTimer();
    }
  }

  private async openOPFSStorage(): Promise<{
    namespace: FileSystemDirectoryHandle;
    data: FileSystemDirectoryHandle;
  }> {
    const root = await navigator.storage.getDirectory();
    const namespace = await root.getDirectoryHandle(OPFS_NAMESPACE, { create: true });
    const data = await namespace.getDirectoryHandle(OPFS_DATA_DIRECTORY, { create: true });
    return { namespace, data };
  }

  private async loadMetadataFromOPFS(
    namespace: FileSystemDirectoryHandle
  ): Promise<Map<string, PersistedNodeMetadata>> {
    const metadata = new Map<string, PersistedNodeMetadata>();
    try {
      const handle = await namespace.getFileHandle(OPFS_METADATA_FILE);
      const file = await handle.getFile();
      if (file.size > MAX_OPFS_METADATA_BYTES) {
        throw new Error('OPFS VFS metadata exceeds its size limit');
      }
      const parsed: unknown = JSON.parse(await file.text());
      if (!parsed || typeof parsed !== 'object') {
        throw new Error('OPFS VFS metadata root is invalid');
      }
      const record = parsed as { version?: unknown; entries?: unknown };
      if (record.version !== 1 || !Array.isArray(record.entries)) {
        throw new Error('OPFS VFS metadata version is unsupported');
      }
      if (record.entries.length > MAX_VFS_NODES) {
        throw new Error('OPFS VFS metadata contains too many entries');
      }
      for (const rawEntry of record.entries) {
        if (!Array.isArray(rawEntry) || rawEntry.length !== 4) {
          throw new Error('OPFS VFS metadata entry is invalid');
        }
        const [path, rawMode, modTime, kind] = rawEntry as unknown[];
        const mode = typeof rawMode === 'number' ? modeBits(rawMode) : null;
        if (
          typeof path !== 'string'
          || !path.startsWith('/')
          || path.includes('\0')
          || UTF8_ENCODER.encode(path).length > MAX_VFS_PATH_LENGTH
          || mode === null
          || !Number.isSafeInteger(modTime)
          || (modTime as number) < 0
          || (kind !== 'f' && kind !== 'd')
        ) {
          throw new Error('OPFS VFS metadata entry fields are invalid');
        }
        metadata.set(path, {
          mode,
          modTime: modTime as number,
          kind: kind === 'f' ? 'file' : 'directory',
        });
      }
    } catch (error) {
      if ((error as { name?: unknown })?.name !== 'NotFoundError') {
        console.warn('[VFS] Ignoring invalid OPFS metadata:', error);
      }
      metadata.clear();
    }
    return metadata;
  }

  private async saveMetadataToOPFS(namespace: FileSystemDirectoryHandle): Promise<void> {
    const entries: PersistedMetadataEntry[] = [];
    const encoder = new TextEncoder();
    let estimatedBytes = 32;
    const visit = (node: FSNode, parts: readonly string[]): void => {
      const path = parts.length === 0 ? '/' : `/${parts.join('/')}`;
      estimatedBytes += encoder.encode(path).length + 48;
      if (estimatedBytes > MAX_OPFS_METADATA_BYTES) {
        throw new Error('VFS metadata exceeds the OPFS checkpoint limit');
      }
      entries.push([
        path,
        node.mode,
        node.modTime,
        node.kind === 'file' ? 'f' : 'd',
      ]);
      if (node.kind === 'directory') {
        const children = sortByUtf8Name(
          [...node.children!.entries()],
          ([name]) => name
        );
        for (const [name, child] of children) visit(child, [...parts, name]);
      }
    };
    visit(this.root, []);

    const bytes = encoder.encode(JSON.stringify({ version: 1, entries }));
    if (bytes.length > MAX_OPFS_METADATA_BYTES) {
      throw new Error('VFS metadata exceeds the OPFS checkpoint limit');
    }
    const handle = await namespace.getFileHandle(OPFS_METADATA_FILE, { create: true });
    await this.writeOPFSFile(handle, bytes);
  }

  private async writeOPFSFile(
    handle: FileSystemFileHandle,
    data: Uint8Array
  ): Promise<void> {
    const writable = await handle.createWritable();
    try {
      await writable.write(data as unknown as BufferSource);
      await writable.close();
    } catch (error) {
      try {
        await writable.abort(error);
      } catch {
        // Preserve the original storage error.
      }
      throw error;
    }
  }

  private async saveToOPFS(
    opfsDir: FileSystemDirectoryHandle,
    memDir: FSNode
  ): Promise<void> {
    const existing = new Map<string, 'file' | 'directory'>();
    for await (const [name, handle] of (opfsDir as any).entries()) {
      existing.set(name, handle.kind);
    }

    for (const [name, node] of memDir.children!) {
      const previousKind = existing.get(name);
      existing.delete(name);
      if (previousKind && previousKind !== node.kind) {
        await opfsDir.removeEntry(name, { recursive: true });
      }

      if (node.kind === 'file') {
        const fileHandle = await opfsDir.getFileHandle(name, { create: true });
        await this.writeOPFSFile(fileHandle, node.data!);
      } else {
        const subDir = await opfsDir.getDirectoryHandle(name, { create: true });
        await this.saveToOPFS(subDir, node);
      }
    }

    for (const name of existing.keys()) {
      await opfsDir.removeEntry(name, { recursive: true });
    }
  }

  private markDirty(): void {
    this.mutationVersion += 1;
    this.dirty = true;
    this.schedulePersistTimer();
  }

  private schedulePersistTimer(): void {
    if (!this.initialized || !this.opfsAvailable || this.persistTimer !== null) return;
    this.persistTimer = globalThis.setTimeout(() => {
      this.persistTimer = null;
      // The debounced checkpoint is best-effort. `forceFlush` remains the
      // explicit API whose rejection reports durability failure to callers.
      void this.persist().catch(() => {});
    }, 2000);
  }

  /**
   * Persist one stable VFS version to OPFS.
   *
   * The Promise resolves after every mutation visible when the checkpoint
   * settles has reached OPFS. It rejects on storage failures. Memory-only
   * hosts resolve immediately because they have no durable backend.
   */
  async forceFlush(): Promise<void> {
    if (this.persistTimer !== null) {
      globalThis.clearTimeout(this.persistTimer);
      this.persistTimer = null;
    }
    while (this.dirty && this.opfsAvailable) {
      await this.persist();
      if (this.persistInFlight) await this.persistInFlight;
    }
  }

  // ===========================================================================
  // Path & permission utilities
  // ===========================================================================

  private resolvePath(path: string): [ResolvedPath | null, string | null] {
    if (typeof path !== 'string' || path.includes('\0')) {
      return [null, invalidArgument('path contains invalid data')];
    }
    if (path.length === 0) return [null, ERR_NOT_EXIST];
    if (UTF8_ENCODER.encode(path).length > MAX_VFS_PATH_LENGTH) {
      return [null, invalidArgument('path is too long')];
    }

    const parts = path.startsWith('/') ? [] : [...this.cwdParts];
    for (const part of path.split('/')) {
      if (part.length === 0 || part === '.') continue;
      if (part === '..') {
        if (parts.length > 0) parts.pop();
        continue;
      }
      if (UTF8_ENCODER.encode(part).length > MAX_VFS_NAME_LENGTH) {
        return [null, invalidArgument('path component is too long')];
      }
      parts.push(part);
      if (parts.length > MAX_VFS_PATH_DEPTH) {
        return [null, invalidArgument('path is too deep')];
      }
    }

    const absolute = parts.length === 0 ? '/' : `/${parts.join('/')}`;
    if (UTF8_ENCODER.encode(absolute).length > MAX_VFS_PATH_LENGTH) {
      return [null, invalidArgument('path is too long')];
    }
    return [{ parts, absolute, trailingSlash: path.length > 1 && path.endsWith('/') }, null];
  }

  private lookup(parts: readonly string[]): [FSNode | null, string | null] {
    let node = this.root;
    for (const part of parts) {
      if (node.kind !== 'directory') return [null, ERR_NOT_DIR];
      if (!this.canSearch(node)) return [null, ERR_PERMISSION];
      const child = node.children!.get(part);
      if (!child) return [null, ERR_NOT_EXIST];
      node = child;
    }
    return [node, null];
  }

  private lookupResolved(path: ResolvedPath): [FSNode | null, string | null] {
    const [node, error] = this.lookup(path.parts);
    if (error || !node) return [null, error];
    if (path.trailingSlash && node.kind !== 'directory') return [null, ERR_NOT_DIR];
    return [node, null];
  }

  private lookupParent(
    path: ResolvedPath
  ): [FSNode | null, string, string | null] {
    if (path.parts.length === 0) return [null, '', ERR_INVALID];
    const name = path.parts[path.parts.length - 1];
    const [parent, error] = this.lookup(path.parts.slice(0, -1));
    if (error || !parent) return [null, name, error];
    if (parent.kind !== 'directory') return [null, name, ERR_NOT_DIR];
    if (!this.canSearch(parent)) return [null, name, ERR_PERMISSION];
    return [parent, name, null];
  }

  private canSearch(node: FSNode): boolean {
    return (node.mode & 0o111) !== 0;
  }

  private canRead(node: FSNode): boolean {
    return (node.mode & 0o444) !== 0;
  }

  private canWrite(node: FSNode): boolean {
    return (node.mode & 0o222) !== 0;
  }

  private canMutateDirectory(node: FSNode): boolean {
    return node.kind === 'directory' && this.canSearch(node) && this.canWrite(node);
  }

  private createDir(mode: number): FSNode {
    return {
      kind: 'directory',
      mode,
      modTime: Date.now(),
      children: new Map(),
      openCount: 0,
      accounted: false,
      orphanRoot: false,
    };
  }

  private createFile(mode: number, data: Uint8Array = new Uint8Array(0)): FSNode {
    return {
      kind: 'file',
      mode,
      modTime: Date.now(),
      data,
      openCount: 0,
      accounted: false,
      orphanRoot: false,
    };
  }

  private resetAccounting(): void {
    this.liveNodes = 0;
    this.liveBytes = 0;
    const visit = (node: FSNode): void => {
      node.openCount = 0;
      node.accounted = true;
      node.orphanRoot = false;
      this.liveNodes += 1;
      if (node.kind === 'file') {
        this.liveBytes += node.data!.length;
      } else {
        for (const child of node.children!.values()) visit(child);
      }
    };
    visit(this.root);
  }

  private canAllocate(nodes: number, bytes: number): boolean {
    return this.liveNodes + nodes <= MAX_VFS_NODES
      && this.liveBytes + bytes <= MAX_VFS_TOTAL_BYTES;
  }

  private accountNewNode(node: FSNode): void {
    node.accounted = true;
    node.orphanRoot = false;
    this.liveNodes += 1;
    if (node.kind === 'file') this.liveBytes += node.data!.length;
  }

  /**
   * Release a detached subtree while retaining any inode still reachable from
   * an open descriptor. An open directory retains its descendants; when its
   * last descriptor closes, the walk resumes and preserves independently open
   * descendants as new orphan roots.
   */
  private releaseDetached(node: FSNode): void {
    if (!node.accounted) return;
    if (node.openCount > 0) {
      node.orphanRoot = true;
      return;
    }

    node.accounted = false;
    node.orphanRoot = false;
    this.liveNodes -= 1;
    if (node.kind === 'file') {
      this.liveBytes -= node.data!.length;
      return;
    }
    for (const child of node.children!.values()) this.releaseDetached(child);
  }

  private attachNewChild(parent: FSNode, name: string, child: FSNode): void {
    parent.children!.set(name, child);
    this.accountNewNode(child);
  }

  private ensurePlatformDirectories(): void {
    const ensure = (parent: FSNode, name: string, mode: number): FSNode | null => {
      const existing = parent.children!.get(name);
      if (existing) return existing.kind === 'directory' ? existing : null;
      if (!this.canAllocate(1, 0)) return null;
      const directory = this.createDir(mode);
      this.attachNewChild(parent, name, directory);
      return directory;
    };

    const tmp = ensure(this.root, 'tmp', 0o777);
    if (tmp) ensure(tmp, 'cache', 0o755);
    const home = ensure(this.root, 'home', 0o755);
    if (home) ensure(home, 'config', 0o755);
  }

  private touch(node: FSNode): void {
    node.modTime = Date.now();
  }

  private allocateFd(): number | null {
    if (this.openFiles.size >= MAX_OPEN_FILES) return null;
    for (let attempts = 0; attempts <= MAX_OPEN_FILES; attempts += 1) {
      const candidate = this.nextFd;
      this.nextFd = candidate >= LAST_FD ? FIRST_FD : candidate + 1;
      if (!this.openFiles.has(candidate)) return candidate;
    }
    return null;
  }

  private resizeFile(node: FSNode, size: number): string | null {
    const existing = node.data!;
    if (size === existing.length) return null;
    const growth = size - existing.length;
    if (growth > 0 && !this.canAllocate(0, growth)) return ERR_OUT_OF_MEMORY;
    try {
      const next = new Uint8Array(size);
      next.set(existing.subarray(0, Math.min(existing.length, size)));
      node.data = next;
      this.liveBytes += growth;
      this.touch(node);
      this.markDirty();
      return null;
    } catch {
      return ERR_OUT_OF_MEMORY;
    }
  }

  private validateSize(size: number): string | null {
    if (!Number.isSafeInteger(size) || size < 0) {
      return invalidArgument('file size must be a non-negative safe integer');
    }
    if (size > MAX_VFS_FILE_BYTES) return ERR_FILE_TOO_LARGE;
    return null;
  }

  // ===========================================================================
  // File operations
  // ===========================================================================

  openFile(path: string, flags: number, mode: number): [number, string | null] {
    if (!Number.isSafeInteger(flags) || flags < 0 || (flags & ~KNOWN_OPEN_FLAGS) !== 0) {
      return [-1, invalidArgument('unsupported OpenFile flags')];
    }
    const access = flags & ACCESS_MASK;
    if (access > O_RDWR) return [-1, invalidArgument('invalid OpenFile access mode')];
    if ((flags & O_EXCL) !== 0 && (flags & O_CREATE) === 0) {
      return [-1, invalidArgument('O_EXCL requires O_CREATE')];
    }
    if ((flags & O_TRUNC) !== 0 && access === O_RDONLY) {
      return [-1, invalidArgument('O_TRUNC requires a writable access mode')];
    }
    const permissions = modeBits(mode);
    if (permissions === null) return [-1, invalidArgument('invalid file mode')];

    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return [-1, pathError];
    if (resolved.parts.length === 0) {
      if ((flags & O_CREATE) !== 0 && (flags & O_EXCL) !== 0) return [-1, ERR_EXIST];
      if (access !== O_RDONLY || (flags & O_TRUNC) !== 0) return [-1, ERR_IS_DIR];
      if (!this.canRead(this.root) || !this.canSearch(this.root)) return [-1, ERR_PERMISSION];
      const fd = this.allocateFd();
      if (fd === null) return [-1, ERR_TOO_MANY_FILES];
      this.root.openCount += 1;
      this.openFiles.set(fd, { node: this.root, flags, position: 0 });
      return [fd, null];
    }
    const [parent, name, parentError] = this.lookupParent(resolved);
    if (parentError || !parent) return [-1, parentError];

    let node = parent.children!.get(name);
    const create = (flags & O_CREATE) !== 0;
    const exclusive = (flags & O_EXCL) !== 0;
    const truncate = (flags & O_TRUNC) !== 0;
    const created = !node;

    if (!node) {
      if (!create) return [-1, ERR_NOT_EXIST];
      if (resolved.trailingSlash) return [-1, ERR_NOT_EXIST];
      if (!this.canMutateDirectory(parent)) return [-1, ERR_PERMISSION];
    } else {
      if (create && exclusive) return [-1, ERR_EXIST];
      if (resolved.trailingSlash && node.kind !== 'directory') return [-1, ERR_NOT_DIR];
      if (node.kind === 'directory') {
        if (access !== O_RDONLY || truncate) return [-1, ERR_IS_DIR];
        if (!this.canRead(node) || !this.canSearch(node)) return [-1, ERR_PERMISSION];
      } else {
        if ((access === O_RDONLY || access === O_RDWR) && !this.canRead(node)) {
          return [-1, ERR_PERMISSION];
        }
        if ((access === O_WRONLY || access === O_RDWR) && !this.canWrite(node)) {
          return [-1, ERR_PERMISSION];
        }
      }
    }

    const fd = this.allocateFd();
    if (fd === null) return [-1, ERR_TOO_MANY_FILES];

    if (created) {
      if (!this.canAllocate(1, 0)) return [-1, ERR_OUT_OF_MEMORY];
      node = this.createFile(permissions);
      this.attachNewChild(parent, name, node);
      this.touch(parent);
      this.markDirty();
    } else if (truncate && node!.kind === 'file') {
      this.liveBytes -= node!.data!.length;
      node!.data = new Uint8Array(0);
      this.touch(node!);
      this.markDirty();
    }

    node!.openCount += 1;
    this.openFiles.set(fd, { node: node!, flags, position: 0 });
    return [fd, null];
  }

  read(fd: number, length: number): [Uint8Array | null, string | null] {
    if (!isSafeIntegerInRange(length, 0, MAX_VFS_IO_BYTES)) {
      return [null, invalidArgument('read length is out of range')];
    }
    const file = this.openFiles.get(fd);
    if (!file) return [null, ERR_CLOSED];
    if ((file.flags & ACCESS_MASK) === O_WRONLY) return [null, ERR_PERMISSION];
    if (file.node.kind === 'directory') return [null, ERR_IS_DIR];

    const data = file.node.data!;
    const end = Math.min(file.position + length, data.length);
    const chunk = data.slice(file.position, end);
    file.position = end;
    return [chunk, null];
  }

  write(fd: number, data: Uint8Array): [number, string | null] {
    if (!(data instanceof Uint8Array) || data.length > MAX_VFS_IO_BYTES) {
      return [0, invalidArgument('write length is out of range')];
    }
    const file = this.openFiles.get(fd);
    if (!file) return [0, ERR_CLOSED];
    if ((file.flags & ACCESS_MASK) === O_RDONLY) return [0, ERR_PERMISSION];
    if (file.node.kind === 'directory') return [0, ERR_IS_DIR];
    if (data.length === 0) return [0, null];

    const existing = file.node.data!;
    const position = (file.flags & O_APPEND) !== 0 ? existing.length : file.position;
    const newLength = position + data.length;
    if (!Number.isSafeInteger(newLength) || newLength > MAX_VFS_FILE_BYTES) {
      return [0, ERR_FILE_TOO_LARGE];
    }
    const growth = Math.max(0, newLength - existing.length);
    if (growth > 0 && !this.canAllocate(0, growth)) return [0, ERR_OUT_OF_MEMORY];

    try {
      if (newLength > existing.length) {
        const next = new Uint8Array(newLength);
        next.set(existing);
        file.node.data = next;
        this.liveBytes += growth;
      }
      file.node.data!.set(data, position);
    } catch {
      return [0, ERR_OUT_OF_MEMORY];
    }

    file.position = newLength;
    this.touch(file.node);
    this.markDirty();
    return [data.length, null];
  }

  readAt(fd: number, length: number, offset: number): [Uint8Array | null, string | null] {
    if (!isSafeIntegerInRange(length, 0, MAX_VFS_IO_BYTES)) {
      return [null, invalidArgument('read length is out of range')];
    }
    if (!isSafeIntegerInRange(offset, 0, MAX_VFS_FILE_BYTES)) {
      return [null, invalidArgument('file offset is out of range')];
    }
    const file = this.openFiles.get(fd);
    if (!file) return [null, ERR_CLOSED];
    if ((file.flags & ACCESS_MASK) === O_WRONLY) return [null, ERR_PERMISSION];
    if (file.node.kind === 'directory') return [null, ERR_IS_DIR];

    const data = file.node.data!;
    if (offset >= data.length) return [new Uint8Array(0), null];
    return [data.slice(offset, Math.min(offset + length, data.length)), null];
  }

  writeAt(fd: number, data: Uint8Array, offset: number): [number, string | null] {
    if (!(data instanceof Uint8Array) || data.length > MAX_VFS_IO_BYTES) {
      return [0, invalidArgument('write length is out of range')];
    }
    if (!isSafeIntegerInRange(offset, 0, MAX_VFS_FILE_BYTES)) {
      return [0, invalidArgument('file offset is out of range')];
    }
    const file = this.openFiles.get(fd);
    if (!file) return [0, ERR_CLOSED];
    if ((file.flags & O_APPEND) !== 0) {
      return [0, invalidArgument('invalid use of WriteAt on file opened with O_APPEND')];
    }
    if ((file.flags & ACCESS_MASK) === O_RDONLY) return [0, ERR_PERMISSION];
    if (file.node.kind === 'directory') return [0, ERR_IS_DIR];
    if (data.length === 0) return [0, null];

    const existing = file.node.data!;
    const newLength = offset + data.length;
    if (!Number.isSafeInteger(newLength) || newLength > MAX_VFS_FILE_BYTES) {
      return [0, ERR_FILE_TOO_LARGE];
    }
    const growth = Math.max(0, newLength - existing.length);
    if (growth > 0 && !this.canAllocate(0, growth)) return [0, ERR_OUT_OF_MEMORY];

    try {
      if (newLength > existing.length) {
        const next = new Uint8Array(newLength);
        next.set(existing);
        file.node.data = next;
        this.liveBytes += growth;
      }
      file.node.data!.set(data, offset);
    } catch {
      return [0, ERR_OUT_OF_MEMORY];
    }

    this.touch(file.node);
    this.markDirty();
    return [data.length, null];
  }

  seek(fd: number, offset: number, whence: number): [number, string | null] {
    if (!Number.isSafeInteger(offset) || !Number.isSafeInteger(whence)) {
      return [-1, invalidArgument('seek arguments are out of range')];
    }
    const file = this.openFiles.get(fd);
    if (!file) return [-1, ERR_CLOSED];
    if (file.node.kind === 'directory') return [-1, ERR_IS_DIR];

    let position: number;
    if (whence === 0) position = offset;
    else if (whence === 1) position = file.position + offset;
    else if (whence === 2) position = file.node.data!.length + offset;
    else return [-1, invalidArgument('invalid whence')];

    if (!isSafeIntegerInRange(position, 0, MAX_VFS_FILE_BYTES)) {
      return [-1, invalidArgument('resulting file offset is out of range')];
    }
    file.position = position;
    return [position, null];
  }

  close(fd: number): string | null {
    if (!Number.isSafeInteger(fd)) return ERR_CLOSED;
    const file = this.openFiles.get(fd);
    if (!file) return ERR_CLOSED;
    this.openFiles.delete(fd);
    file.node.openCount -= 1;
    if (file.node.openCount === 0 && file.node.orphanRoot) {
      file.node.orphanRoot = false;
      this.releaseDetached(file.node);
    }
    return null;
  }

  sync(fd: number): string | null {
    if (!Number.isSafeInteger(fd) || !this.openFiles.has(fd)) return ERR_CLOSED;
    // This synchronous bridge is an immediate visibility barrier for the
    // authoritative in-memory VFS. Durable browser storage is asynchronous;
    // callers that own the JavaScript host must await `forceFlush()` for an
    // observable OPFS checkpoint. O_SYNC inherits the same contract on each
    // already-synchronous write.
    return null;
  }

  fstat(fd: number): [number, number, number, boolean, string | null] {
    const file = this.openFiles.get(fd);
    if (!file) return [0, 0, 0, false, ERR_CLOSED];
    const node = file.node;
    const isDir = node.kind === 'directory';
    return [isDir ? 0 : node.data!.length, node.mode, node.modTime, isDir, null];
  }

  ftruncate(fd: number, size: number): string | null {
    const sizeError = this.validateSize(size);
    if (sizeError) return sizeError;
    const file = this.openFiles.get(fd);
    if (!file) return ERR_CLOSED;
    if ((file.flags & ACCESS_MASK) === O_RDONLY) return ERR_PERMISSION;
    if (file.node.kind === 'directory') return ERR_IS_DIR;
    return this.resizeFile(file.node, size);
  }

  // ===========================================================================
  // Directory and path operations
  // ===========================================================================

  mkdir(path: string, mode: number): string | null {
    const permissions = modeBits(mode);
    if (permissions === null) return invalidArgument('invalid directory mode');
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return pathError;
    if (resolved.parts.length === 0) return ERR_EXIST;
    const [parent, name, parentError] = this.lookupParent(resolved);
    if (parentError || !parent) return parentError;
    if (!this.canMutateDirectory(parent)) return ERR_PERMISSION;
    if (parent.children!.has(name)) return ERR_EXIST;
    if (!this.canAllocate(1, 0)) return ERR_OUT_OF_MEMORY;

    this.attachNewChild(parent, name, this.createDir(permissions));
    this.touch(parent);
    this.markDirty();
    return null;
  }

  mkdirAll(path: string, mode: number): string | null {
    const permissions = modeBits(mode);
    if (permissions === null) return invalidArgument('invalid directory mode');
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return pathError;

    let node = this.root;
    for (const part of resolved.parts) {
      if (node.kind !== 'directory') return ERR_NOT_DIR;
      if (!this.canSearch(node)) return ERR_PERMISSION;
      let child = node.children!.get(part);
      if (!child) {
        if (!this.canMutateDirectory(node)) return ERR_PERMISSION;
        if (!this.canAllocate(1, 0)) return ERR_OUT_OF_MEMORY;
        child = this.createDir(permissions);
        this.attachNewChild(node, part, child);
        this.touch(node);
        this.markDirty();
      } else if (child.kind !== 'directory') {
        return ERR_NOT_DIR;
      }
      node = child;
    }
    return null;
  }

  remove(path: string): string | null {
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return pathError;
    if (resolved.parts.length === 0) return ERR_PERMISSION;
    const [parent, name, parentError] = this.lookupParent(resolved);
    if (parentError || !parent) return parentError;
    if (!this.canMutateDirectory(parent)) return ERR_PERMISSION;
    const node = parent.children!.get(name);
    if (!node) return ERR_NOT_EXIST;
    if (resolved.trailingSlash && node.kind !== 'directory') return ERR_NOT_DIR;
    if (node.kind === 'directory') {
      if (node.children!.size > 0) return ERR_DIR_NOT_EMPTY;
      if (startsWithParts(this.cwdParts, resolved.parts)) return ERR_PERMISSION;
    }

    parent.children!.delete(name);
    this.releaseDetached(node);
    this.touch(parent);
    this.markDirty();
    return null;
  }

  removeAll(path: string): string | null {
    if (path === '') return null;
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return pathError;
    if (resolved.parts.length === 0) return ERR_PERMISSION;

    const [node, lookupError] = this.lookupResolved(resolved);
    if (lookupError === ERR_NOT_EXIST) return null;
    if (lookupError || !node) return lookupError;
    if (node.kind === 'directory' && startsWithParts(this.cwdParts, resolved.parts)) {
      return ERR_PERMISSION;
    }

    const [parent, name, parentError] = this.lookupParent(resolved);
    if (parentError || !parent) return parentError;
    if (!this.canMutateDirectory(parent)) return ERR_PERMISSION;
    parent.children!.delete(name);
    this.releaseDetached(node);
    this.touch(parent);
    this.markDirty();
    return null;
  }

  rename(oldPath: string, newPath: string): string | null {
    return this.renameWithPolicy(oldPath, newPath, true);
  }

  renameNoreplace(oldPath: string, newPath: string): string | null {
    return this.renameWithPolicy(oldPath, newPath, false);
  }

  private renameWithPolicy(
    oldPath: string,
    newPath: string,
    replaceExisting: boolean,
  ): string | null {
    const [oldResolved, oldPathError] = this.resolvePath(oldPath);
    if (oldPathError || !oldResolved) return oldPathError;
    const [newResolved, newPathError] = this.resolvePath(newPath);
    if (newPathError || !newResolved) return newPathError;
    if (oldResolved.parts.length === 0 || newResolved.parts.length === 0) {
      return ERR_PERMISSION;
    }
    const [oldParent, oldName, oldParentError] = this.lookupParent(oldResolved);
    if (oldParentError || !oldParent) return oldParentError;
    const [newParent, newName, newParentError] = this.lookupParent(newResolved);
    if (newParentError || !newParent) return newParentError;
    if (!this.canMutateDirectory(oldParent) || !this.canMutateDirectory(newParent)) {
      return ERR_PERMISSION;
    }

    const source = oldParent.children!.get(oldName);
    if (!source) return ERR_NOT_EXIST;
    if (oldResolved.trailingSlash && source.kind !== 'directory') return ERR_NOT_DIR;
    if (
      source.kind === 'directory' &&
      newResolved.parts.length > oldResolved.parts.length &&
      startsWithParts(newResolved.parts, oldResolved.parts)
    ) {
      return invalidArgument('cannot move a directory into itself');
    }

    const target = newParent.children!.get(newName);
    if (target && !replaceExisting) return ERR_EXIST;
    if (newResolved.trailingSlash) {
      if (!target) return ERR_NOT_EXIST;
      if (target.kind !== 'directory') return ERR_NOT_DIR;
    }
    if (oldResolved.absolute === newResolved.absolute) return null;
    if (target) {
      if (source.kind === 'directory' && target.kind !== 'directory') return ERR_NOT_DIR;
      if (source.kind !== 'directory' && target.kind === 'directory') return ERR_IS_DIR;
      if (target.kind === 'directory' && target.children!.size > 0) {
        return ERR_DIR_NOT_EMPTY;
      }
      if (target.kind === 'directory' && startsWithParts(this.cwdParts, newResolved.parts)) {
        return ERR_PERMISSION;
      }
    }

    oldParent.children!.delete(oldName);
    if (target) this.releaseDetached(target);
    newParent.children!.set(newName, source);
    this.touch(oldParent);
    if (newParent !== oldParent) this.touch(newParent);

    if (source.kind === 'directory' && startsWithParts(this.cwdParts, oldResolved.parts)) {
      this.cwdParts = [
        ...newResolved.parts,
        ...this.cwdParts.slice(oldResolved.parts.length),
      ];
    }
    this.markDirty();
    return null;
  }

  stat(path: string): [string, number, number, number, boolean, string | null] {
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return ['', 0, 0, 0, false, pathError];
    const [node, lookupError] = this.lookupResolved(resolved);
    if (lookupError || !node) return ['', 0, 0, 0, false, lookupError];
    const isDir = node.kind === 'directory';
    const name = resolved.parts.length === 0 ? '/' : resolved.parts[resolved.parts.length - 1];
    return [name, isDir ? 0 : node.data!.length, node.mode, node.modTime, isDir, null];
  }

  readDir(path: string): [Array<[string, boolean, number]>, string | null] {
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return [[], pathError];
    const [node, lookupError] = this.lookupResolved(resolved);
    if (lookupError || !node) return [[], lookupError];
    if (node.kind !== 'directory') return [[], ERR_NOT_DIR];
    if (!this.canRead(node) || !this.canSearch(node)) return [[], ERR_PERMISSION];
    if (node.children!.size > MAX_VFS_DIRECTORY_ENTRIES) {
      return [[], 'directory contains too many entries'];
    }

    const entries: Array<[string, boolean, number]> = [];
    for (const [name, child] of node.children!) {
      entries.push([name, child.kind === 'directory', child.mode]);
    }
    sortByUtf8Name(entries, ([name]) => name);
    return [entries, null];
  }

  chmod(path: string, mode: number): string | null {
    const permissions = modeBits(mode);
    if (permissions === null) return invalidArgument('invalid file mode');
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return pathError;
    const [node, lookupError] = this.lookupResolved(resolved);
    if (lookupError || !node) return lookupError;
    node.mode = permissions;
    this.touch(node);
    this.markDirty();
    return null;
  }

  truncate(path: string, size: number): string | null {
    const sizeError = this.validateSize(size);
    if (sizeError) return sizeError;
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return pathError;
    const [node, lookupError] = this.lookupResolved(resolved);
    if (lookupError || !node) return lookupError;
    if (node.kind === 'directory') return ERR_IS_DIR;
    if (!this.canWrite(node)) return ERR_PERMISSION;
    return this.resizeFile(node, size);
  }

  getwd(): [string, string | null] {
    return [this.cwdParts.length === 0 ? '/' : `/${this.cwdParts.join('/')}`, null];
  }

  chdir(path: string): string | null {
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return pathError;
    const [node, lookupError] = this.lookupResolved(resolved);
    if (lookupError || !node) return lookupError;
    if (node.kind !== 'directory') return ERR_NOT_DIR;
    if (!this.canSearch(node)) return ERR_PERMISSION;
    this.cwdParts = [...resolved.parts];
    return null;
  }

  // ===========================================================================
  // Convenience methods
  // ===========================================================================

  readFile(path: string): [Uint8Array | null, string | null] {
    return this.readFileLimited(path, MAX_VFS_FILE_BYTES);
  }

  readFileLimited(path: string, maxBytes: number): [Uint8Array | null, string | null] {
    if (!isSafeIntegerInRange(maxBytes, 0, MAX_VFS_FILE_BYTES)) {
      return [null, invalidArgument('read limit is out of range')];
    }
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return [null, pathError];
    const [node, lookupError] = this.lookupResolved(resolved);
    if (lookupError || !node) return [null, lookupError];
    if (node.kind === 'directory') return [null, ERR_IS_DIR];
    if (!this.canRead(node)) return [null, ERR_PERMISSION];
    if (node.data!.length > maxBytes) return [null, ERR_FILE_TOO_LARGE];
    return [node.data!.slice(), null];
  }

  writeFile(path: string, data: Uint8Array, mode: number): string | null {
    if (!(data instanceof Uint8Array)) return invalidArgument('data must be bytes');
    if (data.length > MAX_VFS_FILE_BYTES) return ERR_FILE_TOO_LARGE;
    const permissions = modeBits(mode);
    if (permissions === null) return invalidArgument('invalid file mode');
    const [resolved, pathError] = this.resolvePath(path);
    if (pathError || !resolved) return pathError;
    if (resolved.parts.length === 0) return ERR_IS_DIR;
    if (resolved.trailingSlash) return ERR_NOT_DIR;
    const [parent, name, parentError] = this.lookupParent(resolved);
    if (parentError || !parent) return parentError;

    const existing = parent.children!.get(name);
    if (existing?.kind === 'directory') return ERR_IS_DIR;
    if (existing && !this.canWrite(existing)) return ERR_PERMISSION;
    if (!existing && !this.canMutateDirectory(parent)) return ERR_PERMISSION;

    let copied: Uint8Array;
    const previousSize = existing?.data!.length ?? 0;
    const growth = data.length - previousSize;
    if (!this.canAllocate(existing ? 0 : 1, Math.max(0, growth))) return ERR_OUT_OF_MEMORY;
    try {
      copied = new Uint8Array(data);
    } catch {
      return ERR_OUT_OF_MEMORY;
    }

    if (existing) {
      existing.data = copied;
      this.liveBytes += growth;
      this.touch(existing);
    } else {
      this.attachNewChild(parent, name, this.createFile(permissions, copied));
      this.touch(parent);
    }
    this.markDirty();
    return null;
  }
}

// ===========================================================================
// Global instance & WASM bindings
// ===========================================================================

export const vfs = new VirtualFS();

// wasm_bindgen passes i64 values as BigInt. Values outside JavaScript's exact
// integer range become NaN and are rejected by the operation-level validators.
const toSafeNumber = (value: unknown): number => {
  if (typeof value === 'bigint') {
    const number = Number(value);
    return Number.isSafeInteger(number) && BigInt(number) === value ? number : Number.NaN;
  }
  return typeof value === 'number' && Number.isSafeInteger(value) ? value : Number.NaN;
};

/** Register VFS bindings on window for WASM to call. */
export function registerVFSBindings(): void {
  if (typeof window === 'undefined') {
    throw new Error('VFS WASM bindings require a window global');
  }
  const host = window as any;
  host._vfsOpenFile = (path: string, flags: unknown, mode: unknown) =>
    vfs.openFile(path, toSafeNumber(flags), toSafeNumber(mode));
  host._vfsRead = (fd: unknown, length: unknown) =>
    vfs.read(toSafeNumber(fd), toSafeNumber(length));
  host._vfsWrite = (fd: unknown, data: Uint8Array) => vfs.write(toSafeNumber(fd), data);
  host._vfsReadAt = (fd: unknown, length: unknown, offset: unknown) =>
    vfs.readAt(toSafeNumber(fd), toSafeNumber(length), toSafeNumber(offset));
  host._vfsWriteAt = (fd: unknown, data: Uint8Array, offset: unknown) =>
    vfs.writeAt(toSafeNumber(fd), data, toSafeNumber(offset));
  host._vfsSeek = (fd: unknown, offset: unknown, whence: unknown) =>
    vfs.seek(toSafeNumber(fd), toSafeNumber(offset), toSafeNumber(whence));
  host._vfsClose = (fd: unknown) => vfs.close(toSafeNumber(fd));
  host._vfsSync = (fd: unknown) => vfs.sync(toSafeNumber(fd));
  host._vfsFstat = (fd: unknown) => vfs.fstat(toSafeNumber(fd));
  host._vfsFtruncate = (fd: unknown, size: unknown) =>
    vfs.ftruncate(toSafeNumber(fd), toSafeNumber(size));
  host._vfsMkdir = (path: string, mode: unknown) => vfs.mkdir(path, toSafeNumber(mode));
  host._vfsMkdirAll = (path: string, mode: unknown) =>
    vfs.mkdirAll(path, toSafeNumber(mode));
  host._vfsRemove = (path: string) => vfs.remove(path);
  host._vfsRemoveAll = (path: string) => vfs.removeAll(path);
  host._vfsRename = (oldPath: string, newPath: string) => vfs.rename(oldPath, newPath);
  host._vfsRenameNoreplace = (oldPath: string, newPath: string) =>
    vfs.renameNoreplace(oldPath, newPath);
  host._vfsStat = (path: string) => vfs.stat(path);
  host._vfsReadDir = (path: string) => vfs.readDir(path);
  host._vfsChmod = (path: string, mode: unknown) => vfs.chmod(path, toSafeNumber(mode));
  host._vfsTruncate = (path: string, size: unknown) =>
    vfs.truncate(path, toSafeNumber(size));
  host._vfsGetwd = () => vfs.getwd();
  host._vfsChdir = (path: string) => vfs.chdir(path);
  host._vfsReadFile = (path: string) => vfs.readFile(path);
  host._vfsReadFileLimited = (path: string, maxBytes: unknown) =>
    vfs.readFileLimited(path, toSafeNumber(maxBytes));
  host._vfsWriteFile = (path: string, data: Uint8Array, mode: unknown) =>
    vfs.writeFile(path, data, toSafeNumber(mode));
}
