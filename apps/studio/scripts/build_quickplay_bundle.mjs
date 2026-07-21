import { Buffer } from 'node:buffer';
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { dirname, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { gzipSync } from 'node:zlib';

const MAX_FILES = 20_000;
const MAX_SOURCE_BYTES = 512 * 1024 * 1024;
const BLOCK_SIZE = 512;

export function selectQuickPlayFiles(paths) {
  const sorted = [...paths].sort(compareUtf8);
  const assetPackRoots = new Set(
    sorted
      .filter((path) => path.toLowerCase().endsWith('.vpak'))
      .map((path) => dirname(path) === '.' ? '' : dirname(path)),
  );
  return sorted.filter((path) => {
    if (path === '.DS_Store' || path.endsWith('/.DS_Store')) return false;
    if (path === 'docs' || path.startsWith('docs/') || path === 'tools' || path.startsWith('tools/')) return false;
    for (const root of assetPackRoots) {
      if (root && path.startsWith(`${root}/`) && !path.toLowerCase().endsWith('.vpak')) return false;
    }
    return true;
  });
}

export function selectWorkspaceModuleFiles(paths) {
  return [...paths].sort(compareUtf8).filter((path) => {
    const name = path.slice(path.lastIndexOf('/') + 1);
    if (name.endsWith('.vo')) return true;
    if (['vo.mod', 'vo.lock', 'vo.release.json'].includes(name)) return true;
    if (name.endsWith('.vpak')) return true;
    if (path.startsWith('assets/') && /\.(png|jpe?g|webp|glb|gltf|bin)$/i.test(name)) return true;
    if (path.startsWith('js/dist/')) return true;
    if (/^web-artifacts\/.+\.(wasm|js)$/.test(path)) return true;
    if (/^rust\/pkg[^/]*\/.+\.(wasm|js)$/.test(path)) return true;
    return !path.includes('/') && (name.endsWith('.wasm') || name.endsWith('.js'));
  });
}

export function buildTar(files) {
  const chunks = [];
  let totalBytes = 0;
  for (const file of files) {
    totalBytes += file.content.byteLength;
    if (!Number.isSafeInteger(totalBytes) || totalBytes > MAX_SOURCE_BYTES) {
      throw new Error('Quick Play source exceeds the 512 MiB limit');
    }
    const header = createTarHeader(file.path, file.content.byteLength, file.mode);
    chunks.push(header, file.content);
    const padding = (BLOCK_SIZE - (file.content.byteLength % BLOCK_SIZE)) % BLOCK_SIZE;
    if (padding > 0) chunks.push(Buffer.alloc(padding));
  }
  chunks.push(Buffer.alloc(BLOCK_SIZE * 2));
  return Buffer.concat(chunks);
}

function createTarHeader(path, size, mode) {
  const { name, prefix } = splitTarPath(path);
  const header = Buffer.alloc(BLOCK_SIZE);
  writeTarString(header, 0, 100, name);
  writeTarOctal(header, 100, 8, mode & 0o7777);
  writeTarOctal(header, 108, 8, 0);
  writeTarOctal(header, 116, 8, 0);
  writeTarOctal(header, 124, 12, size);
  writeTarOctal(header, 136, 12, 0);
  header.fill(0x20, 148, 156);
  header[156] = 0x30;
  writeTarString(header, 257, 6, 'ustar');
  writeTarString(header, 263, 2, '00');
  writeTarString(header, 265, 32, 'root');
  writeTarString(header, 297, 32, 'root');
  writeTarString(header, 345, 155, prefix);
  const checksum = header.reduce((sum, byte) => sum + byte, 0);
  const rendered = checksum.toString(8).padStart(6, '0');
  header.write(rendered, 148, 6, 'ascii');
  header[154] = 0;
  header[155] = 0x20;
  return header;
}

function splitTarPath(path) {
  validatePortablePath(path);
  if (Buffer.byteLength(path, 'utf8') <= 100) return { name: path, prefix: '' };
  for (let index = path.lastIndexOf('/'); index > 0; index = path.lastIndexOf('/', index - 1)) {
    const prefix = path.slice(0, index);
    const name = path.slice(index + 1);
    if (Buffer.byteLength(prefix, 'utf8') <= 155 && Buffer.byteLength(name, 'utf8') <= 100) {
      return { name, prefix };
    }
  }
  throw new Error(`Quick Play path does not fit the ustar header: ${path}`);
}

function validatePortablePath(path) {
  if (
    !path
    || path.startsWith('/')
    || path.includes('\\')
    || path.split('/').some((part) => !part || part === '.' || part === '..')
  ) {
    throw new Error(`Invalid Quick Play path: ${path}`);
  }
}

function writeTarString(buffer, offset, length, value) {
  const bytes = Buffer.from(value, 'utf8');
  if (bytes.byteLength > length) throw new Error(`Tar field is too long: ${value}`);
  bytes.copy(buffer, offset);
}

function writeTarOctal(buffer, offset, length, value) {
  if (!Number.isSafeInteger(value) || value < 0) throw new Error(`Invalid tar number: ${value}`);
  const rendered = value.toString(8).padStart(length - 1, '0');
  if (rendered.length >= length) throw new Error(`Tar number does not fit its field: ${value}`);
  buffer.write(rendered, offset, length - 1, 'ascii');
  buffer[offset + length - 1] = 0;
}

function compareUtf8(left, right) {
  return Buffer.compare(Buffer.from(left, 'utf8'), Buffer.from(right, 'utf8'));
}

function git(sourceRoot, args, encoding = 'utf8') {
  return execFileSync('git', ['-C', sourceRoot, ...args], {
    encoding,
    maxBuffer: 16 * 1024 * 1024,
    stdio: ['ignore', 'pipe', 'inherit'],
  });
}

function readGitIndex(sourceRoot) {
  const entries = new Map();
  const output = git(sourceRoot, ['ls-files', '--stage', '-z'], 'buffer').toString('utf8');
  for (const record of output.split('\0')) {
    if (!record) continue;
    const separator = record.indexOf('\t');
    const metadata = separator >= 0 ? record.slice(0, separator).split(' ') : [];
    const path = separator >= 0 ? record.slice(separator + 1) : '';
    if (!path || metadata.length !== 3 || metadata[2] !== '0') {
      throw new Error(`Unsupported Git index entry in ${sourceRoot}: ${record}`);
    }
    entries.set(path, metadata[0] === '100755' ? 0o755 : 0o644);
  }
  return entries;
}

function readSource(spec) {
  const sourceRoot = resolve(spec.root);
  const revision = git(sourceRoot, ['rev-parse', 'HEAD']).trim().toLowerCase();
  if (revision !== spec.revision) {
    throw new Error(`${spec.name} revision mismatch: expected ${spec.revision}, found ${revision}`);
  }
  const index = readGitIndex(sourceRoot);
  const selected = spec.project
    ? selectQuickPlayFiles(index.keys())
    : selectWorkspaceModuleFiles(index.keys());
  return selected.map((path) => {
    const absolute = resolve(sourceRoot, path);
    if (relative(sourceRoot, absolute).startsWith('..')) throw new Error(`Path escapes ${spec.name}: ${path}`);
    return {
      path: spec.prefix ? `${spec.prefix}/${path}` : path,
      content: readFileSync(absolute),
      mode: index.get(path),
    };
  });
}

function main() {
  const args = process.argv.slice(2);
  if (args.length !== 8 || args.some((value, index) => index % 2 === 1 && !/^[0-9a-f]{40}$/.test(value))) {
    throw new Error(
      'usage: npm run build:quickplay -- '
      + '<BlockKart checkout> <revision> <voplay checkout> <revision> '
      + '<vogui checkout> <revision> <vopack checkout> <revision>',
    );
  }
  const specs = [
    { name: 'BlockKart', root: args[0], revision: args[1], prefix: '', project: true },
    { name: 'voplay', root: args[2], revision: args[3], prefix: '.quickplay/voplay', project: false },
    { name: 'vogui', root: args[4], revision: args[5], prefix: '.quickplay/vogui', project: false },
    { name: 'vopack', root: args[6], revision: args[7], prefix: '.quickplay/vopack', project: false },
  ];
  const bundleRevision = createHash('sha1')
    .update(`${specs.map((spec) => spec.revision).join('\n')}\n`)
    .digest('hex');
  const files = specs.flatMap(readSource);
  files.push({
    path: 'vo.work',
    content: Buffer.from(
      'format = 1\nmembers = [".", ".quickplay/voplay", ".quickplay/vogui", ".quickplay/vopack"]\n',
      'utf8',
    ),
    mode: 0o644,
  });
  files.sort((left, right) => compareUtf8(left.path, right.path));
  if (files.length === 0 || files.length > MAX_FILES) {
    throw new Error(`Invalid Quick Play file count: ${files.length}`);
  }
  for (const required of ['main.vo', 'vo.mod', 'vo.lock', 'assets/blockkart.vpak']) {
    if (!files.some((file) => file.path === required)) throw new Error(`BlockKart is missing required file: ${required}`);
  }
  const outputDir = resolve(fileURLToPath(new URL('../public/quickplay/', import.meta.url)));
  const output = resolve(outputDir, `blockkart-${bundleRevision}.tar.gz`);
  rmSync(outputDir, { recursive: true, force: true });
  mkdirSync(outputDir, { recursive: true });
  writeFileSync(output, gzipSync(buildTar(files), { level: 9, mtime: 0 }));
  process.stdout.write(`${output}\n${files.length} files\n`);
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) main();
