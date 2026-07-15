import assert from 'node:assert/strict';
import { join, resolve } from 'node:path';
import test from 'node:test';

import { repositorySourcePath } from '../test_runner_paths.mjs';

import {
  MAX_VFS_FILE_BYTES,
  O_APPEND,
  O_CREATE,
  O_EXCL,
  O_RDONLY,
  O_RDWR,
  O_SYNC,
  O_TRUNC,
  O_WRONLY,
  VirtualFS,
} from '../dist/vfs.js';

const encode = (value) => new TextEncoder().encode(value);
const decode = (value) => new TextDecoder().decode(value);

test('WASM test source paths are canonical and repository-relative', () => {
  const repositoryRoot = resolve('/workspace/volang');
  assert.equal(
    repositorySourcePath(
      repositoryRoot,
      join(repositoryRoot, 'tests', 'lang', 'case.vo'),
    ),
    'tests/lang/case.vo',
  );
  assert.throws(
    () => repositorySourcePath(repositoryRoot, resolve(repositoryRoot, '..', 'outside.vo')),
    /outside the repository/,
  );
  assert.throws(
    () => repositorySourcePath(repositoryRoot, repositoryRoot),
    /outside the repository/,
  );
});

class MemoryOPFSFile {
  kind = 'file';

  constructor(controller, bytes = new Uint8Array(0)) {
    this.controller = controller;
    this.bytes = bytes.slice();
    this.lastModified = Date.now();
  }

  async getFile() {
    const bytes = this.bytes.slice();
    return {
      size: bytes.length,
      lastModified: this.lastModified,
      arrayBuffer: async () => bytes.buffer,
      text: async () => decode(bytes),
    };
  }

  async createWritable() {
    let pending = this.bytes.slice();
    let closed = false;
    return {
      write: async (value) => {
        if (this.controller.failNextWrite) {
          this.controller.failNextWrite = false;
          throw new Error('injected OPFS write failure');
        }
        pending = value instanceof Uint8Array
          ? value.slice()
          : new Uint8Array(value).slice();
      },
      close: async () => {
        if (closed) return;
        closed = true;
        this.bytes = pending;
        this.lastModified = Date.now();
      },
      abort: async () => {
        closed = true;
      },
    };
  }
}

class MemoryOPFSDirectory {
  kind = 'directory';

  constructor(controller) {
    this.controller = controller;
    this.children = new Map();
  }

  async *entries() {
    yield* this.children.entries();
  }

  async getDirectoryHandle(name, options = {}) {
    const existing = this.children.get(name);
    if (existing?.kind === 'directory') return existing;
    if (existing) throw Object.assign(new Error('wrong handle kind'), { name: 'TypeMismatchError' });
    if (!options.create) throw Object.assign(new Error('missing'), { name: 'NotFoundError' });
    const directory = new MemoryOPFSDirectory(this.controller);
    this.children.set(name, directory);
    return directory;
  }

  async getFileHandle(name, options = {}) {
    const existing = this.children.get(name);
    if (existing?.kind === 'file') return existing;
    if (existing) throw Object.assign(new Error('wrong handle kind'), { name: 'TypeMismatchError' });
    if (!options.create) throw Object.assign(new Error('missing'), { name: 'NotFoundError' });
    const file = new MemoryOPFSFile(this.controller);
    this.children.set(name, file);
    return file;
  }

  async removeEntry(name, options = {}) {
    const existing = this.children.get(name);
    if (!existing) throw Object.assign(new Error('missing'), { name: 'NotFoundError' });
    if (
      existing.kind === 'directory'
      && existing.children.size > 0
      && !options.recursive
    ) {
      throw new Error('directory not empty');
    }
    this.children.delete(name);
  }
}

async function withMockOPFS(run) {
  const controller = { failNextWrite: false, getDirectoryCalls: 0 };
  const root = new MemoryOPFSDirectory(controller);
  const previous = Object.getOwnPropertyDescriptor(globalThis, 'navigator');
  Object.defineProperty(globalThis, 'navigator', {
    configurable: true,
    value: {
      storage: {
        getDirectory: async () => {
          controller.getDirectoryCalls += 1;
          return root;
        },
      },
    },
  });
  try {
    await run({ controller, root });
  } finally {
    if (previous) Object.defineProperty(globalThis, 'navigator', previous);
    else delete globalThis.navigator;
  }
}

test('relative paths follow cwd and remain normalized at the VFS root', () => {
  const fs = new VirtualFS();
  assert.equal(fs.mkdirAll('/tmp/work/nested', 0o755), null);
  assert.equal(fs.chdir('/tmp/work/nested'), null);
  assert.deepEqual(fs.getwd(), ['/tmp/work/nested', null]);
  assert.equal(fs.writeFile('./value.txt', encode('ok'), 0o644), null);
  assert.equal(decode(fs.readFile('/tmp/work/nested/value.txt')[0]), 'ok');

  assert.equal(fs.chdir('../../..'), null);
  assert.deepEqual(fs.getwd(), ['/', null]);
  assert.equal(fs.chdir('/tmp/work/nested/value.txt'), 'not a directory');

  assert.equal(fs.mkdir('/tmp/locked', 0o600), null);
  assert.equal(fs.chdir('/tmp/locked'), 'permission denied');
  assert.equal(fs.chmod('/tmp/locked', 0o700), null);
  assert.equal(fs.writeFile('/tmp/locked/value', encode('hidden'), 0o644), null);
  assert.equal(fs.chmod('/tmp/locked', 0o600), null);
  assert.equal(fs.openFile('/tmp/locked/value', O_RDONLY, 0)[1], 'permission denied');
});

test('open modes, descriptor permissions, and truncation match the os contract', () => {
  const fs = new VirtualFS();
  assert.equal(fs.writeFile('/tmp/modes', encode('data'), 0o644), null);
  const specialMode = (1 << 23) | (1 << 20) | 0o640;
  assert.equal(fs.chmod('/tmp/modes', specialMode), null);
  assert.equal(fs.stat('/tmp/modes')[2], specialMode);
  assert.equal(fs.chmod('/tmp/modes', 0o644), null);

  assert.match(fs.openFile('/tmp/modes', O_EXCL, 0o644)[1], /O_EXCL requires O_CREATE/);
  assert.match(fs.openFile('/tmp/modes', O_RDONLY | O_TRUNC, 0o644)[1], /O_TRUNC/);
  assert.equal(fs.openFile('/tmp/modes', O_CREATE | O_EXCL | O_WRONLY, 0o644)[1], 'file already exists');

  const [readOnly] = fs.openFile('/tmp/modes', O_RDONLY, 0);
  assert.equal(fs.write(readOnly, encode('x'))[1], 'permission denied');
  assert.equal(fs.chmod('/tmp/modes', 0), null);
  assert.equal(decode(fs.read(readOnly, 4)[0]), 'data');
  assert.equal(fs.openFile('/tmp/modes', O_RDONLY, 0)[1], 'permission denied');
  assert.equal(fs.close(readOnly), null);
  assert.equal(fs.close(readOnly), 'file already closed');

  assert.equal(fs.chmod('/tmp/modes', 0o644), null);
  const [writeOnly] = fs.openFile('/tmp/modes', O_WRONLY, 0);
  assert.equal(fs.read(writeOnly, 1)[1], 'permission denied');
  assert.equal(fs.ftruncate(writeOnly, 2), null);
  assert.equal(fs.close(writeOnly), null);
  assert.equal(decode(fs.readFile('/tmp/modes')[0]), 'da');
});

test('append writes ignore seek while WriteAt rejects append descriptors', () => {
  const fs = new VirtualFS();
  assert.equal(fs.writeFile('/tmp/append', encode('a'), 0o644), null);
  const [fd, openError] = fs.openFile('/tmp/append', O_WRONLY | O_APPEND, 0);
  assert.equal(openError, null);
  assert.deepEqual(fs.seek(fd, 0, 0), [0, null]);
  assert.deepEqual(fs.write(fd, encode('b')), [1, null]);
  assert.match(fs.writeAt(fd, encode('x'), 0)[1], /invalid use of WriteAt/);
  assert.equal(decode(fs.readFile('/tmp/append')[0]), 'ab');

  const [readAppend] = fs.openFile('/tmp/append', O_RDONLY | O_APPEND, 0);
  assert.match(fs.writeAt(readAppend, encode('x'), 0)[1], /invalid use of WriteAt/);
});

test('ReadAt preserves sequential position and all offsets are bounded', () => {
  const fs = new VirtualFS();
  assert.equal(fs.writeFile('/tmp/read-at', encode('ab'), 0o644), null);
  const [fd] = fs.openFile('/tmp/read-at', O_RDONLY, 0);

  const [partial, partialError] = fs.readAt(fd, 4, 1);
  assert.equal(decode(partial), 'b');
  assert.equal(partialError, null);
  assert.equal(decode(fs.read(fd, 1)[0]), 'a');
  assert.match(fs.readAt(fd, 1, -1)[1], /file offset/);
  assert.match(fs.seek(fd, Number.MAX_SAFE_INTEGER, 0)[1], /file offset/);
  assert.equal(fs.ftruncate(fd, MAX_VFS_FILE_BYTES + 1), 'file too large');
});

test('rename validates the whole operation before mutating either parent', () => {
  const fs = new VirtualFS();
  assert.equal(fs.rename('/tmp/absent', '/tmp/absent'), 'file does not exist');
  assert.equal(fs.mkdirAll('/tmp/source/child', 0o755), null);
  assert.equal(fs.writeFile('/tmp/source/child/data', encode('value'), 0o644), null);
  assert.equal(fs.mkdirAll('/tmp/nonempty/child', 0o755), null);

  assert.equal(fs.rename('/tmp/source', '/tmp/nonempty'), 'directory not empty');
  assert.equal(fs.stat('/tmp/source')[5], null);
  assert.equal(fs.rename('/tmp/source', '/tmp/source'), null);
  assert.match(fs.rename('/tmp/source', '/tmp/source/child/moved'), /cannot move/);
  assert.equal(fs.stat('/tmp/source/child/data')[5], null);
  assert.equal(fs.writeFile('/tmp/plain-file', encode('x'), 0o644), null);
  assert.equal(fs.rename('/tmp/plain-file', '/tmp/plain-file/'), 'not a directory');
  assert.equal(fs.stat('/tmp/plain-file')[5], null);

  assert.equal(fs.chdir('/tmp/source/child'), null);
  assert.equal(fs.rename('/tmp/source', '/tmp/moved'), null);
  assert.deepEqual(fs.getwd(), ['/tmp/moved/child', null]);
  assert.equal(fs.removeAll('/tmp/moved'), 'permission denied');
  assert.equal(fs.chdir('/'), null);
  assert.equal(fs.removeAll('/tmp/moved'), null);
});

test('renameNoreplace publishes atomically without changing an existing target', () => {
  const fs = new VirtualFS();
  assert.equal(fs.writeFile('/tmp/source', encode('new'), 0o644), null);
  assert.equal(fs.writeFile('/tmp/target', encode('old'), 0o644), null);

  assert.equal(fs.renameNoreplace('/tmp/source', '/tmp/target'), 'file already exists');
  assert.equal(decode(fs.readFile('/tmp/source')[0]), 'new');
  assert.equal(decode(fs.readFile('/tmp/target')[0]), 'old');

  assert.equal(fs.renameNoreplace('/tmp/source', '/tmp/published'), null);
  assert.equal(fs.stat('/tmp/source')[5], 'file does not exist');
  assert.equal(decode(fs.readFile('/tmp/published')[0]), 'new');
});

test('open descriptors retain inode identity across unlink and replacement rename', () => {
  const fs = new VirtualFS();
  assert.equal(fs.writeFile('/tmp/open', encode('old'), 0o644), null);
  const [oldFd] = fs.openFile('/tmp/open', O_RDWR, 0);
  assert.equal(fs.remove('/tmp/open'), null);
  assert.equal(fs.writeFile('/tmp/open', encode('new'), 0o644), null);
  assert.equal(decode(fs.readAt(oldFd, 3, 0)[0]), 'old');
  assert.deepEqual(fs.writeAt(oldFd, encode('O'), 0), [1, null]);
  assert.equal(decode(fs.readFile('/tmp/open')[0]), 'new');
  assert.equal(fs.close(oldFd), null);

  assert.equal(fs.writeFile('/tmp/source-file', encode('source'), 0o644), null);
  const [targetFd] = fs.openFile('/tmp/open', O_RDONLY, 0);
  assert.equal(fs.rename('/tmp/source-file', '/tmp/open'), null);
  assert.equal(decode(fs.read(targetFd, 3)[0]), 'new');
  assert.equal(decode(fs.readFile('/tmp/open')[0]), 'source');
  assert.equal(fs.close(targetFd), null);
});

test('convenience reads are copies, parents are explicit, and directories are sorted', () => {
  const fs = new VirtualFS();
  assert.equal(fs.writeFile('/missing/file', encode('x'), 0o644), 'file does not exist');
  assert.equal(fs.mkdir('/tmp/sorted', 0o755), null);
  for (const name of ['z', 'é', 'a']) {
    assert.equal(fs.writeFile(`/tmp/sorted/${name}`, encode(name), 0o644), null);
  }

  const [first] = fs.readFile('/tmp/sorted/a');
  first[0] = 'x'.charCodeAt(0);
  assert.equal(decode(fs.readFile('/tmp/sorted/a')[0]), 'a');
  assert.deepEqual(
    fs.readDir('/tmp/sorted')[0].map(([name]) => name),
    ['a', 'z', 'é']
  );
  assert.equal(fs.removeAll('/tmp/absent'), null);
  assert.equal(fs.removeAll(''), null);
});

test('bounded reads and path limits fail before copying or mutating', () => {
  const fs = new VirtualFS();
  assert.equal(fs.writeFile('/tmp/bounded', encode('data'), 0o644), null);
  assert.deepEqual(fs.readFileLimited('/tmp/bounded', 3), [null, 'file too large']);
  assert.equal(decode(fs.readFileLimited('/tmp/bounded', 4)[0]), 'data');
  assert.match(fs.stat(`/tmp/${'x'.repeat(256)}`)[5], /path component is too long/);
  assert.match(fs.stat(`/tmp/${'😀'.repeat(64)}`)[5], /path component is too long/);
  assert.equal(fs.writeFile(`/tmp/${'😀'.repeat(63)}`, encode('ok'), 0o644), null);
  assert.match(fs.writeFile(`/${'x'.repeat(4096)}`, encode('x'), 0o644), /path is too long/);
  assert.equal(fs.stat('/tmp/bounded')[1], 4);
});

test('directory descriptors support Stat while byte I/O reports IsDir', () => {
  const fs = new VirtualFS();
  const [rootFd, rootError] = fs.openFile('.', O_RDONLY, 0);
  assert.equal(rootError, null);
  assert.equal(fs.fstat(rootFd)[3], true);
  const [fd, error] = fs.openFile('/tmp', O_RDONLY, 0);
  assert.equal(error, null);
  assert.equal(fs.fstat(fd)[3], true);
  assert.equal(fs.read(fd, 1)[1], 'is a directory');
  assert.equal(fs.seek(fd, 0, 0)[1], 'is a directory');
  assert.equal(fs.openFile('/tmp', O_RDWR, 0)[1], 'is a directory');
});

test('Sync and O_SYNC provide immediate memory visibility with an explicit async checkpoint', async () => {
  const fs = new VirtualFS();
  const [writer, openError] = fs.openFile(
    '/tmp/sync-visible',
    O_CREATE | O_WRONLY | O_SYNC,
    0o644
  );
  assert.equal(openError, null);
  assert.deepEqual(fs.write(writer, encode('visible')), [7, null]);

  const [reader, readerError] = fs.openFile('/tmp/sync-visible', O_RDONLY, 0);
  assert.equal(readerError, null);
  assert.equal(decode(fs.read(reader, 7)[0]), 'visible');
  assert.equal(fs.sync(writer), null);
  await fs.forceFlush();

  assert.equal(fs.close(writer), null);
  assert.equal(fs.sync(writer), 'file already closed');
  assert.equal(fs.close(reader), null);
});

test('OPFS checkpoints are scoped, preserve metadata, and report storage failure', async () => {
  await withMockOPFS(async ({ controller, root }) => {
    root.children.set('unrelated-origin-data', new MemoryOPFSFile(controller, encode('keep')));

    const fs = new VirtualFS();
    await Promise.all([fs.init(), fs.init()]);
    assert.equal(controller.getDirectoryCalls, 1);
    assert.equal(fs.writeFile('/tmp/persisted', encode('value'), 0o640), null);
    assert.equal(fs.chmod('/tmp', 0o711), null);
    const persistedTime = fs.stat('/tmp/persisted')[3];

    controller.failNextWrite = true;
    const originalConsoleError = console.error;
    console.error = () => {};
    try {
      await assert.rejects(fs.forceFlush(), /injected OPFS write failure/);
    } finally {
      console.error = originalConsoleError;
    }
    await fs.forceFlush();
    assert.equal(root.children.get('unrelated-origin-data').kind, 'file');
    assert.equal(root.children.get('vo-web-vfs-v1').kind, 'directory');

    const reloaded = new VirtualFS();
    await reloaded.init();
    assert.equal(decode(reloaded.readFile('/tmp/persisted')[0]), 'value');
    assert.equal(reloaded.stat('/tmp/persisted')[2], 0o640);
    assert.equal(reloaded.stat('/tmp/persisted')[3], persistedTime);
    assert.equal(reloaded.stat('/tmp')[2], 0o711);
  });
});
