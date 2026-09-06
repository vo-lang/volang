import { createHash } from 'node:crypto';
import { lstat, readdir, readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';

const MAX_FILES = 512;
const MAX_BYTES = 128 * 1024 * 1024;

export async function deployedAssetRecords(root) {
  const records = [];
  let entriesSeen = 0;
  let totalBytes = 0;
  async function walk(directory, relative = '') {
    for (const entry of await readdir(directory, { withFileTypes: true })) {
      if (++entriesSeen > 2048) throw new Error('site has too many directory entries');
      if (entry.isSymbolicLink()) throw new Error('site artifact contains a symlink');
      const path = relative ? `${relative}/${entry.name}` : entry.name;
      // Pages configuration and build markers are not public application assets.
      if (['.volang-ui-build', '_headers', '_redirects', 'CNAME'].includes(path)) continue;
      if (entry.isDirectory()) await walk(join(directory, entry.name), path);
      else if (entry.isFile() && /\.(html|js|json|wasm|webmanifest|css|svg|png|ico|woff2?)$/.test(path)) {
        const metadata = await lstat(join(root, path));
        totalBytes += metadata.size;
        if (totalBytes > MAX_BYTES || records.length >= MAX_FILES) throw new Error('site exceeds verification bounds');
        const bytes = await readFile(join(root, path));
        if (bytes.length !== metadata.size) throw new Error(`site changed during verification: ${path}`);
        records.push({ path, size: bytes.length, sha256: createHash('sha256').update(bytes).digest('hex') });
      }
    }
  }
  await walk(resolve(root));
  records.sort((a, b) => a.path.localeCompare(b.path, 'en'));
  for (const required of ['index.html', 'app.js', 'app.wasm', 'runtime/pkg/vo_web_bg.wasm', 'service-worker.js']) {
    if (!records.some(record => record.path === required)) throw new Error(`site asset is absent: ${required}`);
  }
  return records;
}

export async function verifyDeployedArtifact(baseURL, records) {
  const base = new URL(baseURL);
  if (!['http:', 'https:'].includes(base.protocol) || base.username || base.password || base.search || base.hash) {
    throw new Error('deployed URL must be HTTP(S) without credentials, query or fragment');
  }
  if (!base.pathname.endsWith('/')) base.pathname += '/';
  if (!Array.isArray(records) || records.length === 0 || records.length > MAX_FILES) throw new Error('invalid asset set');
  let totalBytes = 0;
  const paths = new Set();
  for (const record of records) {
    if (!record || !/^[a-zA-Z0-9_./-]+$/.test(record.path) || record.path.startsWith('/') || record.path.split('/').some(part => !part || part === '.' || part === '..')
        || !Number.isSafeInteger(record.size) || record.size < 0 || record.size > MAX_BYTES
        || !/^[a-f0-9]{64}$/.test(record.sha256) || paths.has(record.path)) throw new Error('invalid asset record');
    paths.add(record.path);
    totalBytes += record.size;
  }
  if (totalBytes > MAX_BYTES) throw new Error('asset set exceeds verification bounds');
  let cursor = 0;
  const completed = [];
  const controller = new AbortController();
  const deadline = setTimeout(() => controller.abort(new Error('deployed artifact verification timed out')), 180000);
  try {
    await Promise.all(Array.from({ length: Math.min(4, records.length) }, async () => {
      while (cursor < records.length && !controller.signal.aborted) {
        const record = records[cursor++];
        const response = await fetch(new URL(record.path, base), { redirect: 'error', cache: 'no-store', signal: controller.signal });
        if (response.status !== 200 || !response.body) throw new Error(`deployed asset ${record.path}: HTTP ${response.status}`);
        const digest = createHash('sha256');
        let size = 0;
        for await (const chunk of response.body) {
          size += chunk.length;
          if (size > record.size) throw new Error(`deployed asset size mismatch: ${record.path}`);
          digest.update(chunk);
        }
        if (size !== record.size || digest.digest('hex') !== record.sha256) throw new Error(`deployed asset digest mismatch: ${record.path}`);
        completed.push(record);
      }
    }));
  } catch (error) {
    controller.abort(error);
    throw error;
  } finally { clearTimeout(deadline); }
  if (completed.length !== records.length) throw new Error('deployed verification is incomplete');
  return { complete: true, passed: true, files: completed.length, bytes: completed.reduce((sum, record) => sum + record.size, 0) };
}
