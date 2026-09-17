import assert from 'node:assert/strict';
import test from 'node:test';
import {Zip, ZipPassThrough, unzipSync} from './node_modules/fflate/esm/browser.js';
import {archiveBrowserProject, inspectBrowserProjects, recoveryLimits} from '../../apps/studio/next/recovery-storage.js';
import {createRecoveryWatch} from '../../apps/studio/next/recovery-watch.js';

const library = {Zip, ZipPassThrough};
const fresh = () => new AbortController();
const missing = () => new DOMException('Missing entry', 'NotFoundError');
const file = (name, bytes, modified = Date.UTC(2025, 0, 2)) => ({name, kind:'file',
  async getFile() {return new File([bytes], name, {lastModified:modified});}});
const directory = (name, entries = []) => ({name, kind:'directory',
  async *entries() {for (const entry of entries) yield [entry.name, entry];},
  async getDirectoryHandle(name, options) {
    assert(!options?.create, 'recovery tried to create source storage');
    const entry = entries.find(entry => entry.name === name);
    if (!entry) throw missing();
    if (entry.kind !== 'directory') throw new DOMException('Wrong kind', 'TypeMismatchError');
    return entry;
  },
  createWritable() {assert.fail('recovery attempted a write');},
  removeEntry() {assert.fail('recovery attempted removal');},
});
const storage = entries => ({async getDirectory() {
  return directory('', [directory('vo-web-vfs-v1', [directory('data', [directory('workspace', entries)])])]);
}});
const read = async (store, name, limits = recoveryLimits, controller = fresh()) =>
  archiveBrowserProject(store, name, library, controller.signal, limits);

test('discovery is read-only and retains uncatalogued and unfinished directories', async () => {
  const catalog = {name:'.volang-studio-projects.json', kind:'file', getFile() {assert.fail('catalog normalization was invoked');}};
  const result = await inspectBrowserProjects(storage([directory('z'), catalog, directory('.studio-import-incomplete'), directory('café')]), fresh().signal);
  assert.deepEqual(result, [{name:'.studio-import-incomplete'}, {name:'café'}, {name:'z'}]);
  assert.deepEqual(await inspectBrowserProjects({getDirectory:async () => directory('')}, fresh().signal), []);
  await assert.rejects(inspectBrowserProjects({}, fresh().signal), /unavailable/);
  await assert.rejects(inspectBrowserProjects({getDirectory:async () => directory('', [file('vo-web-vfs-v1', '')])}, fresh().signal), {name:'TypeMismatchError'});
});

test('ZIP retains exact Unicode, binary, hidden, empty files and empty directories', async () => {
  const binary = Uint8Array.from([0, 255, 10, 127]);
  const store = storage([directory('café', [file('main.vo', 'package main\n// 中文'), file('.hidden', binary),
    file('empty.txt', ''), directory('empty-dir'), directory('.volang', [file('vo.lock', 'original lock')])])]);
  const archive = await read(store, 'café');
  const bytes = new Uint8Array(await archive.blob.arrayBuffer());
  const unpacked = unzipSync(bytes);
  assert.deepEqual(Object.keys(unpacked).sort(), ['café/', 'café/.hidden', 'café/.volang/', 'café/.volang/vo.lock', 'café/empty-dir/', 'café/empty.txt', 'café/main.vo'].sort());
  assert.deepEqual(unpacked['café/.hidden'], binary);
  assert.equal(new TextDecoder().decode(unpacked['café/main.vo']), 'package main\n// 中文');
  assert.equal(new TextDecoder().decode(unpacked['café/.volang/vo.lock']), 'original lock');
  assert.equal(unpacked['café/empty.txt'].length, 0);
  assert.equal(archive.files, 4);
  assert.equal(archive.bytes, bytes.length);
  assert.deepEqual(new Uint8Array(await (await read(store, 'café')).blob.arrayBuffer()), bytes, 'export changed the source or archive metadata');
});

test('entry, path, depth, file, total and archive limits reject complete export', async () => {
  const store = storage([directory('project', [file('first', '1234'), file('second', '5678'), directory('nested', [directory('deep')])])]);
  for (const limits of [{entries:2}, {pathBytes:10}, {depth:0}, {fileBytes:3}, {totalBytes:7}, {archiveBytes:20}]) {
    await assert.rejects(read(store, 'project', {...recoveryLimits, ...limits}), /limit|too many|too long|too deeply|smaller/);
  }
  for (const name of ['', '..', 'a/b', 'a\\b', 'a\0b', 'x'.repeat(256)]) await assert.rejects(read(store, name), /valid browser project/);
  await assert.rejects(inspectBrowserProjects(storage([directory('one'), directory('two')]), fresh().signal, {...recoveryLimits, projects:1}), /too many/);
  await assert.rejects(read(storage([directory('project', [file('same', ''), file('same', '')])]), 'project'), /repeated entry/);
});

test('aborted work does not open storage and pending native reads cancel promptly', async () => {
  const controller = fresh(); controller.abort(new Error('cancelled before open'));
  await assert.rejects(read({getDirectory() {assert.fail('storage opened after cancellation');}}, 'project', recoveryLimits, controller), /cancelled before open/);
  const pending = fresh();
  let entered, finish;
  const started = new Promise(resolve => {entered = resolve;});
  const work = inspectBrowserProjects({getDirectory() {entered(); return new Promise(resolve => {finish = resolve;});}}, pending.signal);
  await started;
  pending.abort(new Error('cancelled pending read'));
  await assert.rejects(work, /cancelled pending read/);
  finish(directory(''));
});

test('stream cancellation releases the reader and never returns a partial archive', async () => {
  const controller = fresh(); let entered, cancelled = false;
  const started = new Promise(resolve => {entered = resolve;});
  const blocked = {name:'pending', kind:'file', async getFile() {return {
    size:4, lastModified:Date.UTC(2025, 0, 2), stream() {return new ReadableStream({
      pull() {entered(); return new Promise(() => {});}, cancel() {cancelled = true;},
    });},
  };}};
  const work = read(storage([directory('project', [blocked])]), 'project', recoveryLimits, controller);
  await started; controller.abort(new Error('cancelled stream'));
  await assert.rejects(work, /cancelled stream/);
  assert(cancelled);
});

test('changed files and truncated streams fail instead of producing a valid-looking partial backup', async () => {
  let reads = 0;
  const changing = {name:'changed', kind:'file', async getFile() {
    return new File(['data'], 'changed', {lastModified:Date.UTC(2025, 0, ++reads)});
  }};
  await assert.rejects(read(storage([directory('project', [changing])]), 'project'), /changed while/);
  const short = {name:'short', kind:'file', async getFile() {return {
    size:8, lastModified:Date.UTC(2025, 0, 2), stream() {return new Blob(['four']).stream();},
  };}};
  await assert.rejects(read(storage([directory('project', [short])]), 'project'), /changed while/);
});

function recoveryWindow() {
  const urls = new Map(), revoked = [];
  let sequence = 0;
  return {navigator:{storage:storage([directory('project', [file('main.vo', 'package main')])])}, urls, revoked,
    URL:{createObjectURL(blob) {const url = 'blob:test/' + ++sequence; urls.set(url, blob); return url;},
      revokeObjectURL(url) {revoked.push(url); urls.delete(url);}}};
}
const request = operation => JSON.stringify({version:1, operation, project:'project'});
const observe = (provider, operation, controller = fresh()) => {
  const results = [];
  let delivered;
  const first = new Promise(resolve => {delivered = resolve;});
  provider(request(operation), controller.signal, (value, error = '') => {
    const result = {value, error}; results.push(result); delivered(result);
  });
  return {results, first, controller};
};

test('prepared downloads remain owned until cancellation; discovery omits the ZIP library', async t => {
  t.mock.timers.enable({apis:['setTimeout']});
  const window = recoveryWindow(); let imports = 0;
  const provider = createRecoveryWatch(window, {loadLibrary:async () => {imports++; return library;}});
  const scan = observe(provider, 'inspect');
  assert.deepEqual(JSON.parse((await scan.first).value).projects, [{name:'project'}]);
  assert.equal(imports, 0); scan.controller.abort();
  const first = observe(provider, 'export'), second = observe(provider, 'export');
  assert.equal((await first.first).error, ''); assert.equal((await second.first).error, '');
  const url = JSON.parse(first.results[0].value).url;
  t.mock.timers.tick(100000);
  assert.equal(first.results.length, 1); assert.equal(second.results.length, 1);
  assert.equal(window.urls.size, 2, 'the preparation deadline expired a completed download');
  first.controller.abort(); first.controller.abort();
  assert.deepEqual(window.revoked, [url]); assert.equal(window.urls.size, 1);
  second.controller.abort(); assert.equal(window.urls.size, 0);
});

test('a deadline ends a hung library load locally and ignores its late completion', async t => {
  t.mock.timers.enable({apis:['setTimeout']});
  const window = recoveryWindow(); let entered, finish;
  const started = new Promise(resolve => {entered = resolve;});
  const provider = createRecoveryWatch(window, {timeoutMilliseconds:10,
    loadLibrary() {entered(); return new Promise(resolve => {finish = resolve;});}});
  const pending = observe(provider, 'export');
  await started; t.mock.timers.tick(10);
  assert.match((await pending.first).error, /too long/);
  finish(library); await new Promise(setImmediate);
  assert.equal(pending.results.length, 1); assert.equal(window.urls.size, 0);
  pending.controller.abort();
});

test('leaving during preparation drops late results and rejects invalid requests locally', async () => {
  const window = recoveryWindow(); let entered, finish;
  const started = new Promise(resolve => {entered = resolve;});
  const provider = createRecoveryWatch(window, {loadLibrary() {entered(); return new Promise(resolve => {finish = resolve;});}});
  const pending = observe(provider, 'export');
  await started; pending.controller.abort(); finish(library);
  await new Promise(setImmediate);
  assert.equal(pending.results.length, 0); assert.equal(window.urls.size, 0);
  for (const value of ['null', '{}', '{', 'x'.repeat(4097), request('remove')]) {
    assert.throws(() => provider(value, fresh().signal, () => assert.fail('invalid request emitted a result')));
  }
});
