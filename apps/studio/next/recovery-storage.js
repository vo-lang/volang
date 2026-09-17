import {recoveryRead} from './recovery-cancel.js';

// Read the old browser namespace directly. Initializing its VFS would create
// directories, normalize the catalog and install background write callbacks.
const encoder = new TextEncoder();
export const recoveryLimits = Object.freeze({projects:4096, entries:32768, depth:256,
  pathBytes:4096, fileBytes:128 * 1024 * 1024, totalBytes:128 * 1024 * 1024,
  archiveBytes:160 * 1024 * 1024});

export function validRecoveryName(name) {
  return typeof name === 'string' && name !== '' && name !== '.' && name !== '..' &&
    !/[\\/\u0000-\u001f\u007f]/u.test(name) && encoder.encode(name).length <= 255;
}

async function workspace(storage, signal) {
  if (typeof storage?.getDirectory !== 'function') throw new Error('Browser project storage is unavailable.');
  let directory = await recoveryRead(() => storage.getDirectory(), signal);
  try {
    for (const name of ['vo-web-vfs-v1', 'data', 'workspace']) {
      directory = await recoveryRead(() => directory.getDirectoryHandle(name), signal);
    }
    return directory;
  } catch (error) {if (error?.name === 'NotFoundError') return null; throw error;}
}

async function children(directory, signal, budget) {
  const iterator = directory.entries(), entries = [], names = new Set();
  let done = false;
  try {
    for (;;) {
      const result = await recoveryRead(() => iterator.next(), signal);
      if (result.done) {done = true; break;}
      const [name, handle] = result.value;
      if (!validRecoveryName(name) || handle.name !== name || !['file','directory'].includes(handle.kind) || names.has(name)) {
        throw new Error('The stored project contains an invalid or repeated entry.');
      }
      if (++budget.count > budget.limit) throw new Error('The stored project has too many entries to export.');
      names.add(name); entries.push([name, handle]);
    }
  } finally {if (!done && iterator.return) void Promise.resolve(iterator.return()).catch(() => {});}
  return entries.sort(([first], [second]) => first < second ? -1 : first > second ? 1 : 0);
}

export async function inspectBrowserProjects(storage, signal, limits = recoveryLimits) {
  const directory = await workspace(storage, signal);
  if (!directory) return [];
  const entries = await children(directory, signal, {count:0, limit:limits.projects});
  // Keep incomplete and uncatalogued directories available for recovery.
  return entries.filter(([, handle]) => handle.kind === 'directory').map(([name]) => ({name}));
}

export async function archiveBrowserProject(storage, project, library, signal, limits = recoveryLimits) {
  if (!validRecoveryName(project)) throw new Error('Choose a valid browser project.');
  const directory = await workspace(storage, signal);
  if (!directory) throw new Error('The old browser workspace could not be found.');
  const source = await recoveryRead(() => directory.getDirectoryHandle(project), signal);
  const chunks = [], budget = {count:1, limit:limits.entries};
  let sourceBytes = 0, archiveBytes = 0, files = 0, finished = false;
  const zip = new library.Zip((error, bytes, final) => {
    if (error) throw error;
    signal.throwIfAborted();
    archiveBytes += bytes.length;
    if (archiveBytes > limits.archiveBytes) throw new Error('The project archive exceeds the download size limit.');
    chunks.push(bytes); finished = final;
  });
  const add = (path, modified) => {
    if (encoder.encode(path).length > limits.pathBytes) throw new Error('The stored project path is too long to export.');
    const entry = new library.ZipPassThrough(path);
    // ZIP timestamps have a narrower range than browser File timestamps.
    entry.mtime = new Date(Math.max(Date.UTC(1980, 0, 1), Math.min(modified ?? Date.UTC(1980, 0, 1), Date.UTC(2107, 11, 31))));
    zip.add(entry);
    return entry;
  };
  const walk = async (directory, path, depth) => {
    signal.throwIfAborted();
    if (depth > limits.depth) throw new Error('The stored project is nested too deeply to export.');
    add(path + '/').push(new Uint8Array(), true);
    for (const [name, handle] of await children(directory, signal, budget)) {
      const target = path + '/' + name;
      if (handle.kind === 'directory') {await walk(handle, target, depth + 1); continue;}
      const file = await recoveryRead(() => handle.getFile(), signal);
      if (!Number.isSafeInteger(file.size) || file.size < 0 || file.size > limits.fileBytes) {
        throw new Error('A stored file exceeds the 128 MiB export limit.');
      }
      sourceBytes += file.size;
      if (sourceBytes > limits.totalBytes) throw new Error('Choose a project smaller than 128 MiB for browser export.');
      const entry = add(target, file.lastModified), reader = file.stream().getReader();
      let bytesRead = 0;
      const cancel = () => {void reader.cancel(signal.reason).catch(() => {});};
      signal.addEventListener('abort', cancel, {once:true});
      try {
        for (;;) {
          const {done, value} = await recoveryRead(() => reader.read(), signal);
          if (done) break;
          bytesRead += value.length;
          if (bytesRead > file.size) throw new Error('A stored file changed while the archive was being prepared.');
          entry.push(value);
        }
        const current = await recoveryRead(() => handle.getFile(), signal);
        if (bytesRead !== file.size || current.size !== file.size || current.lastModified !== file.lastModified) {
          throw new Error('A stored file changed while the archive was being prepared. Save and close other Studio tabs, then retry.');
        }
        entry.push(new Uint8Array(), true); files++;
      } finally {
        signal.removeEventListener('abort', cancel);
        void reader.cancel().catch(() => {});
        reader.releaseLock();
      }
    }
  };
  try {
    await walk(source, project, 0);
    zip.end();
    signal.throwIfAborted();
    if (!finished) throw new Error('The project archive could not be completed.');
    return {blob:new Blob(chunks, {type:'application/zip'}), bytes:archiveBytes, sourceBytes, files};
  } finally {zip.terminate(); chunks.length = 0;}
}
