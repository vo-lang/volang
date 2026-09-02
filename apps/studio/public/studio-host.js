import {
  analyzeProjectAutoInstall,
  compileProjectAutoInstall,
  flushVFS,
  init,
  initVFS,
  prepareWorkspaceLock,
  releaseBrowserFileHandle,
  resolveBrowserFileHandle,
  vfs,
} from '/runtime/dist/index.js';

const PROTOCOL = 'volang.studio.host.v3';
const encoder = new TextEncoder();
const decoder = new TextDecoder('utf-8', { fatal: true });
const MAX_FILE_BYTES = 4 * 1024 * 1024;
const MAX_STARTER_BYTES = 8 * 1024 * 1024;
const MAX_PROJECTS = 512;
const MAX_FILES = 5000;
const MAX_ARTIFACTS = 32;
const MAX_WORKER_REQUESTS = 16;
const MAX_OVERLAYS = 64;
const MAX_RUNS = 4;
const MAX_RUN_ARGUMENTS = 256;
const MAX_RUN_ARGUMENT_BYTES = 64 * 1024;
const MAX_PREVIEWS = 4;
const MAX_IMPORT_BYTES = 128 * 1024 * 1024;
const MAX_PROJECT_SOURCE_BYTES = 128 * 1024 * 1024;
const MAX_WORKSPACE_BUNDLE_BYTES = 128 * 1024 * 1024;
const MAX_WORKSPACE_BUNDLE_FILES = 20_000;
const MAX_IMPORT_DEPTH = 24;
const MAX_SHARE_FILES = 128;
const MAX_SHARE_BYTES = 48 * 1024;

function encode(value) { return encoder.encode(JSON.stringify(value)); }
function decode(payload) { return JSON.parse(decoder.decode(payload)); }
function check(error, operation) { if (error !== null) throw new Error(`${operation}: ${error}`); }
function readText(path) {
  const [bytes, error] = vfs.readFileLimited(path, MAX_FILE_BYTES);
  check(error, `read ${path}`);
  return decoder.decode(bytes);
}
function writeText(path, text) {
  const bytes = encoder.encode(text);
  if (bytes.byteLength > MAX_FILE_BYTES) throw new Error('file exceeds the Studio text limit');
  const slash = path.lastIndexOf('/');
  if (slash > 0) check(vfs.mkdirAll(path.slice(0, slash), 0o755), `create ${path}`);
  check(vfs.writeFile(path, bytes, 0o644), `write ${path}`);
}
function writeBytes(path, bytes) {
  if (bytes.byteLength > MAX_FILE_BYTES) throw new Error('file exceeds the Studio import limit');
  const slash = path.lastIndexOf('/');
  if (slash > 0) check(vfs.mkdirAll(path.slice(0, slash), 0o755), `create ${path}`);
  check(vfs.writeFile(path, bytes, 0o644), `write ${path}`);
}
function validRelative(path) {
  return typeof path === 'string' && path.length > 0 && path.length <= 4096
    && !path.startsWith('/') && !path.includes('\\')
    && path.split('/').every((part) => part.length > 0 && part !== '.' && part !== '..');
}
function reservedPortableName(name) {
  const stem = name.split('.', 1)[0].toUpperCase();
  return ['CON', 'PRN', 'AUX', 'NUL'].includes(stem) || /^(?:COM|LPT)[1-9]$/u.test(stem);
}
function validPortableRelative(path) {
  return validRelative(path) && path.split('/').every((part) => encoder.encode(part).byteLength <= 255
    && !part.endsWith('.') && !part.endsWith(' ')
    && !/[\u0000-\u001f<>:"|?*]/u.test(part) && !reservedPortableName(part));
}
function validName(name) {
  return typeof name === 'string' && /^[A-Za-z0-9_-]{1,128}$/u.test(name)
    && !reservedPortableName(name);
}
function validProjectID(id) { return typeof id === 'string' && /^[a-z0-9][a-z0-9_-]{0,127}$/u.test(id); }
function validDisplayName(name) {
  return typeof name === 'string' && name.length > 0 && name.length <= 255
    && name.trim().length > 0 && !/[\u0000-\u001f\u007f]/u.test(name);
}
function validateRunArguments(value) {
  if (!Array.isArray(value) || value.length > MAX_RUN_ARGUMENTS) {
    throw new Error('run arguments exceed the Studio limit');
  }
  let total = 0;
  for (const argument of value) {
    if (typeof argument !== 'string') throw new Error('run argument is invalid');
    total += encoder.encode(argument).byteLength;
    if (total > MAX_RUN_ARGUMENT_BYTES) throw new Error('run arguments exceed the Studio limit');
  }
  return value;
}
function requestGitHubToken(root) {
  const document = root.ownerDocument;
  const styleID = 'volang-studio-credential-style';
  if (document.getElementById(styleID) === null) {
    const style = document.createElement('style');
    style.id = styleID;
    style.textContent = '.volang-studio-credential::backdrop{background:rgba(0,0,0,.72)}';
    document.head.append(style);
  }
  return new Promise((resolve) => {
    const dialog = document.createElement('dialog');
    dialog.className = 'volang-studio-credential';
    dialog.setAttribute('aria-label', 'Connect GitHub account');
    Object.assign(dialog.style, {
      background: '#151c29', border: '1px solid #344158', borderRadius: '12px',
      boxShadow: '0 24px 80px rgba(0,0,0,.55)', color: '#edf2fa', maxWidth: '440px',
      padding: '24px', width: 'calc(100% - 40px)',
    });
    const form = document.createElement('form');
    form.method = 'dialog';
    form.style.display = 'grid';
    form.style.gap = '14px';
    const title = document.createElement('strong');
    title.textContent = 'Connect GitHub account';
    title.style.fontSize = '18px';
    const description = document.createElement('p');
    description.textContent = 'Paste a GitHub token for one-time account verification. Studio clears it immediately and never persists it.';
    description.style.cssText = 'color:#b9c3d5;line-height:1.5;margin:0';
    const input = document.createElement('input');
    input.type = 'password';
    input.autocomplete = 'off';
    input.spellcheck = false;
    input.placeholder = 'GitHub token';
    input.setAttribute('aria-label', 'GitHub token');
    input.style.cssText = 'background:#0f1520;border:1px solid #344158;border-radius:8px;color:#edf2fa;font:inherit;padding:10px 12px;width:100%;box-sizing:border-box';
    const feedback = document.createElement('div');
    feedback.setAttribute('role', 'alert');
    feedback.style.cssText = 'color:#ff8b91;min-height:20px';
    const actions = document.createElement('div');
    actions.style.cssText = 'display:flex;gap:10px;justify-content:flex-end';
    const cancel = document.createElement('button');
    cancel.type = 'button';
    cancel.textContent = 'Cancel';
    cancel.setAttribute('aria-label', 'Cancel GitHub connection');
    cancel.style.cssText = 'background:transparent;border:1px solid #56647b;border-radius:8px;color:#dbe3ef;cursor:pointer;padding:9px 14px';
    const connect = document.createElement('button');
    connect.type = 'submit';
    connect.textContent = 'Verify account';
    connect.setAttribute('aria-label', 'Verify GitHub account');
    connect.style.cssText = 'background:#6aa7ff;border:0;border-radius:8px;color:#08111f;cursor:pointer;font-weight:700;padding:9px 14px';
    actions.append(cancel, connect);
    form.append(title, description, input, feedback, actions);
    dialog.append(form);
    document.body.append(dialog);
    let settled = false;
    const finish = (value) => {
      if (settled) return;
      settled = true;
      input.value = '';
      dialog.close();
      dialog.remove();
      resolve(value);
    };
    cancel.addEventListener('click', () => finish(null));
    dialog.addEventListener('cancel', (event) => {
      event.preventDefault();
      finish(null);
    });
    form.addEventListener('submit', (event) => {
      event.preventDefault();
      const token = input.value.trim();
      if (token === '') {
        input.setAttribute('aria-invalid', 'true');
        feedback.textContent = 'Enter a GitHub token.';
        input.focus();
        return;
      }
      finish(token);
    });
    dialog.showModal();
    input.focus();
  });
}
function pathEndsWith(path, suffix) { return path === suffix || path.endsWith(`/${suffix}`); }
function sensitiveSharePath(path) {
  const lower = path.toLowerCase();
  const name = lower.slice(lower.lastIndexOf('/') + 1);
  return name === '.env' || name.startsWith('.env.') || name === '.npmrc'
    || name === '.pypirc' || name === '.netrc' || name === '_netrc'
    || name === '.git-credentials' || name === 'credentials' || name === 'credentials.json'
    || name === 'id_rsa' || name === 'id_dsa' || name === 'id_ecdsa' || name === 'id_ed25519'
    || pathEndsWith(lower, '.aws/credentials') || pathEndsWith(lower, '.cargo/credentials')
    || pathEndsWith(lower, '.cargo/credentials.toml') || pathEndsWith(lower, '.docker/config.json')
    || pathEndsWith(lower, '.kube/config') || lower.endsWith('.tfstate')
    || lower.endsWith('.pem') || lower.endsWith('.key')
    || lower.endsWith('.p12') || lower.endsWith('.pfx');
}
function compilerDiagnostic(path, message, code = 'web/compiler') {
  let line = 1;
  let column = 1;
  for (const match of message.matchAll(/(?:^|\s)at\s+.+:(\d+):(\d+)(?:\s|$)/gmu)) {
    const candidateLine = Number.parseInt(match[1], 10);
    const candidateColumn = Number.parseInt(match[2], 10);
    if (candidateLine > 0 && candidateColumn > 0) {
      line = candidateLine;
      column = candidateColumn;
    }
  }
  return {
    path,
    line,
    column,
    endLine: line,
    endColumn: column + 1,
    severity: 3,
    code,
    message,
  };
}
function base64UrlEncode(bytes) {
  let binary = '';
  for (let offset = 0; offset < bytes.byteLength; offset += 0x4000) {
    binary += String.fromCharCode(...bytes.subarray(offset, Math.min(bytes.byteLength, offset + 0x4000)));
  }
  return btoa(binary).replaceAll('+', '-').replaceAll('/', '_').replace(/=+$/u, '');
}
function base64UrlDecode(value) {
  if (typeof value !== 'string' || value.length === 0 || value.length > MAX_SHARE_BYTES * 2
    || !/^[A-Za-z0-9_-]+$/u.test(value)) throw new Error('shared project link is invalid');
  const padded = value.replaceAll('-', '+').replaceAll('_', '/') + '='.repeat((4 - value.length % 4) % 4);
  const binary = atob(padded);
  const bytes = new Uint8Array(binary.length);
  for (let index = 0; index < binary.length; index += 1) bytes[index] = binary.charCodeAt(index);
  return bytes;
}
function validateSharedFiles(files) {
  if (!Array.isArray(files) || files.length === 0 || files.length > MAX_SHARE_FILES) {
    throw new Error('shared project file list is invalid');
  }
  const paths = new Set();
  let total = 0;
  const result = files.map((file) => {
    if (!file || !validPortableRelative(file.path)
      || file.path === '.volang' || file.path.startsWith('.volang/')
      || typeof file.text !== 'string' || paths.has(file.path)) {
      throw new Error('shared project contains an invalid file');
    }
    paths.add(file.path);
    total += encoder.encode(file.text).byteLength;
    if (total > MAX_SHARE_BYTES) throw new Error('shared project exceeds the portable link limit');
    return { path: file.path, text: file.text };
  });
  if (!paths.has('main.vo') || !paths.has('vo.mod')) {
    throw new Error('shared project requires main.vo and vo.mod');
  }
  return result;
}
function validateStarterFiles(files) {
  if (files == null) return [];
  if (!Array.isArray(files) || files.length > MAX_FILES) throw new Error('starter file list is invalid');
  let total = 0;
  const paths = new Set();
  const validated = files.map((file) => {
    if (!file || !validPortableRelative(file.path)
      || file.path === '.volang' || file.path.startsWith('.volang/')
      || typeof file.text !== 'string') throw new Error('starter file is invalid');
    if (file.path === 'vo.mod' || file.path === 'vo.lock') throw new Error('starter cannot replace the project manifest or lock');
    if (paths.has(file.path)) throw new Error('starter contains a duplicate file path');
    paths.add(file.path);
    const bytes = encoder.encode(file.text).byteLength;
    if (bytes > MAX_FILE_BYTES) throw new Error('starter file exceeds the Studio text limit');
    total += bytes;
    if (total > MAX_STARTER_BYTES) throw new Error('starter files exceed the Studio project limit');
    return { path: file.path, text: file.text };
  });
  if (validated.length > 0 && !paths.has('main.vo')) throw new Error('starter must provide main.vo');
  return validated;
}
function starterNeedsOfficialUi(files) {
  return files.length === 0 || files.some((file) => file.path.endsWith('.vo')
    && file.text.includes('"github.com/vo-lang/ui'));
}
function validHandleName(name) {
  return typeof name === 'string' && name.length > 0 && encoder.encode(name).byteLength <= 255
    && name !== '.' && name !== '..' && !name.includes('/') && !name.includes('\\') && !name.includes('\0');
}
function projectRoot(id) { return `/workspace/${id}`; }
function projectPath(id, path, allowInternal = false) {
  if (!validRelative(path)) throw new Error('project path is invalid');
  if (!allowInternal && (path === '.volang' || path.startsWith('.volang/'))) {
    throw new Error('the .volang directory is reserved by Studio');
  }
  return `${projectRoot(id)}/${path}`;
}
function fileKind(path, directory) {
  if (directory) return 1;
  const name = path.slice(path.lastIndexOf('/') + 1);
  if (name === 'vo.mod' || name === 'vo.lock' || name === 'vo.work') return 2;
  if (path.endsWith('.md')) return 3;
  if (path.endsWith('.vo')) return 0;
  return 4;
}

function completeProjectDirectory(id) {
  const [, , , , rootIsDirectory, rootError] = vfs.stat(projectRoot(id));
  if (rootError !== null || !rootIsDirectory) return false;
  const [, , , , manifestIsDirectory, manifestError] = vfs.stat(projectPath(id, 'vo.mod'));
  return manifestError === null && !manifestIsDirectory;
}

function normalizeCatalog(value) {
  if (!Array.isArray(value)) return [];
  const ids = new Set();
  const result = [];
  for (const candidate of value) {
    if (result.length >= MAX_PROJECTS) break;
    if (!candidate || !validProjectID(candidate.id) || ids.has(candidate.id)
      || !completeProjectDirectory(candidate.id)) continue;
    ids.add(candidate.id);
    result.push({
      id: candidate.id,
      name: validDisplayName(candidate.name) ? candidate.name : candidate.id,
      pinned: candidate.pinned === true,
      lastOpenedUnixMillis: Number.isSafeInteger(candidate.lastOpenedUnixMillis)
        && candidate.lastOpenedUnixMillis >= 0 ? candidate.lastOpenedUnixMillis : 0,
      ...(candidate.shared === true ? { shared: true } : {}),
    });
  }
  return result;
}

function discoverCatalog() {
  const [entries, error] = vfs.readDir('/workspace');
  check(error, 'recover browser project catalog');
  return normalizeCatalog(entries
    .filter(([name, directory]) => directory && validProjectID(name))
    .map(([id]) => ({ id, name: id, pinned: false, lastOpenedUnixMillis: 0 })));
}

function cleanupTemporaryProjects() {
  const [entries, error] = vfs.readDir('/workspace');
  check(error, 'inspect temporary browser projects');
  let removed = false;
  for (const [name, directory] of entries) {
    if (directory && (name.startsWith('.studio-create-') || name.startsWith('.studio-share-')
      || name.startsWith('.studio-import-'))) {
      check(vfs.removeAll(`/workspace/${name}`), `remove abandoned project ${name}`);
      removed = true;
    }
  }
  return removed;
}

function listTree(root, includeInternal = false) {
  const output = [];
  const pending = [{ absolute: root, relative: '', depth: 0 }];
  while (pending.length > 0) {
    const directory = pending.shift();
    const [entries, error] = vfs.readDir(directory.absolute);
    check(error, `list ${directory.absolute}`);
    for (const [name, isDirectory] of entries) {
      if (!includeInternal && directory.relative === '' && name === '.volang') continue;
      const relative = directory.relative === '' ? name : `${directory.relative}/${name}`;
      const absolute = `${directory.absolute}/${name}`;
      const [, , , modified, , statError] = vfs.stat(absolute);
      check(statError, `stat ${absolute}`);
      output.push({ path: relative, name, kind: fileKind(relative, isDirectory), depth: directory.depth, modifiedUnixMillis: modified });
      if (output.length > MAX_FILES) throw new Error('project contains too many files');
      if (isDirectory) pending.push({ absolute, relative, depth: directory.depth + 1 });
    }
  }
  output.sort((left, right) => (left.path < right.path ? -1 : left.path > right.path ? 1 : 0));
  return output;
}

class WorkerQueue {
  constructor() {
    this.next = 1;
    this.pending = new Map();
    this.failure = null;
    this.worker = new Worker('/studio-compiler-worker.js', { type: 'module', name: 'volang-studio-compiler' });
    this.worker.onmessage = ({ data }) => {
      const pending = this.pending.get(data.id);
      if (!pending) return;
      this.pending.delete(data.id);
      if (data.ok) pending.resolve(data);
      else {
        const error = new Error(data.error ?? 'Studio worker failed');
        error.kind = data.failure ?? 'worker';
        pending.reject(error);
      }
    };
    this.worker.onerror = (event) => {
      this.failure = new Error(event.message || 'Studio worker stopped');
      this.failure.kind = 'worker';
      for (const pending of this.pending.values()) pending.reject(this.failure);
      this.pending.clear();
    };
    this.worker.onmessageerror = () => {
      this.failure = new Error('Studio worker returned an unreadable response');
      this.failure.kind = 'worker';
      for (const pending of this.pending.values()) pending.reject(this.failure);
      this.pending.clear();
    };
  }
  request(message, transfer = []) {
    if (this.failure) return Promise.reject(this.failure);
    if (this.pending.size >= MAX_WORKER_REQUESTS) {
      const error = new Error('Studio compiler queue is full');
      error.kind = 'queue';
      return Promise.reject(error);
    }
    const id = String(this.next++);
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      try {
        this.worker.postMessage({ ...message, id }, transfer);
      } catch (error) {
        this.pending.delete(id);
        reject(error);
      }
    });
  }
}

function installPreviewObserver(root, surfaces) {
  const sessions = new Map();
  let surfaceDocument;
  async function loadSurfaceDocument() {
    if (!surfaceDocument) {
      surfaceDocument = fetch('/studio-surface.html', { cache: 'no-store' }).then(async (response) => {
        if (!response.ok) throw new Error(`Studio preview surface is unavailable: HTTP ${response.status}`);
        const markup = await response.text();
        if (!markup.includes('id="preview-root"') || !markup.includes('/studio-surface.js')) {
          throw new Error('Studio preview surface is invalid');
        }
        return markup;
      });
    }
    return surfaceDocument;
  }
  async function attach(element) {
    const surface = element.getAttribute('data-volang-media-state');
    if (!surface || sessions.get(element)?.surface === surface) return;
    element.replaceChildren();
    const artifact = surfaces.get(surface);
    if (!artifact) return;
    const markup = await loadSurfaceDocument();
    if (!element.isConnected || element.getAttribute('data-volang-media-state') !== surface) return;
    const frame = element.ownerDocument.createElement('iframe');
    frame.title = 'Volang application preview';
    frame.style.cssText = 'display:block;width:100%;height:100%;min-width:0;min-height:0;border:0;background:#0b0e14';
    const payload = artifact.slice().buffer;
    frame.addEventListener('load', () => {
      frame.contentWindow?.postMessage({
        protocol: 'volang.studio.preview.v1',
        surface,
        artifact: payload,
      }, location.origin, [payload]);
    }, { once: true });
    frame.srcdoc = markup;
    element.replaceChildren(frame);
    sessions.set(element, { surface, frame });
  }
  function scan() {
    for (const element of root.querySelectorAll('[data-volang-platform-view][data-volang-content-type="volang-studio-preview/v1"]')) {
      void attach(element).catch((error) => { element.textContent = error.message; });
    }
    for (const [element, value] of sessions) {
      if (!element.isConnected || value.frame.parentElement !== element) sessions.delete(element);
    }
  }
  const observer = new MutationObserver(scan);
  observer.observe(root, { subtree: true, childList: true, attributes: true, attributeFilter: ['data-volang-content-type', 'data-volang-media-state'] });
  scan();
}

export async function createStudioHost({ root }) {
  await initVFS();
  check(vfs.mkdirAll('/workspace', 0o755), 'create browser workspace');
  if (cleanupTemporaryProjects()) await flushVFS();
  const catalogPath = '/workspace/.volang-studio-projects.json';
  let storedCatalog;
  try { storedCatalog = JSON.parse(readText(catalogPath)); } catch { storedCatalog = null; }
  let catalog = normalizeCatalog(storedCatalog);
  if (catalog.length === 0) catalog = discoverCatalog();
  if (catalog.length === 0) {
    catalog = [{ id: 'hello-studio', name: 'hello-studio', pinned: true, lastOpenedUnixMillis: Date.now() }];
    const project = projectRoot('hello-studio');
    check(vfs.mkdirAll(project, 0o755), 'create starter project');
    writeText(`${project}/main.vo`, 'package main\n\nimport "fmt"\n\nfunc main() { fmt.Println("Hello from browser Studio") }\n');
    writeText(`${project}/vo.mod`, 'format = 1\nmodule = "local/hello-studio"\nversion = "0.1.0"\nvo = "0.1.4"\n');
    writeText(`${project}/README.md`, '# Hello Studio\n\nPersistent browser project powered by Volang Web.\n');
    writeText(catalogPath, JSON.stringify(catalog));
    await flushVFS();
  } else if (JSON.stringify(catalog) !== JSON.stringify(storedCatalog)) {
    writeText(catalogPath, JSON.stringify(catalog));
    await flushVFS();
  }

  const shareValue = new URL(root.ownerDocument.defaultView?.location.href ?? location.href).hash;
  if (shareValue.startsWith('#share=')) {
    const payload = base64UrlDecode(shareValue.slice('#share='.length));
    const bundle = JSON.parse(decoder.decode(payload));
    if (bundle?.schema !== 'volang.studio.share.v1' || typeof bundle.name !== 'string') {
      throw new Error('shared project bundle is invalid');
    }
    const files = validateSharedFiles(bundle.files);
    const digest = Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256', payload)), (value) => value.toString(16).padStart(2, '0')).join('');
    const id = `shared-${digest.slice(0, 32)}`;
    const existing = catalog.find((value) => value.id === id);
    if (!existing && catalog.length >= MAX_PROJECTS) throw new Error('Studio project catalog is full');
    const value = existing ?? {
      id,
      name: validName(bundle.name) ? bundle.name : 'shared-project',
      pinned: true,
      lastOpenedUnixMillis: Date.now(),
      shared: true,
    };
    if (!existing) {
      const temporary = `/workspace/.studio-share-${id}-${Date.now()}`;
      check(vfs.mkdirAll(temporary, 0o755), 'prepare shared project');
      try {
        for (const file of files) writeText(`${temporary}/${file.path}`, file.text);
        check(vfs.renameNoreplace(temporary, projectRoot(id)), 'publish shared project');
      } catch (error) {
        vfs.removeAll(temporary);
        throw error;
      }
    }
    catalog = [value, ...catalog.filter((candidate) => candidate.id !== id)];
    writeText(catalogPath, JSON.stringify(catalog));
    await flushVFS();
  }

  const artifacts = new Map();
  const artifactOrder = [];
  const runs = new Map();
  let account = { provider: 'github', connected: false, login: '', name: '', avatarURL: '' };
  const surfaces = new Map();
  const worker = new WorkerQueue();
  let nextArtifact = 1;
  let nextRun = 1;
  let nextFallbackCompile = 1;
  let projectMutationBusy = false;
  let runtimeReady;
  const ensureRuntime = () => (runtimeReady ??= init(new URL('/runtime/pkg/vo_web_bg.wasm', location.origin)));
  let workspaceBundle;
  installPreviewObserver(root, surfaces);

  async function mutateProject(action) {
    if (projectMutationBusy) throw new Error('another project operation is still in progress');
    projectMutationBusy = true;
    try {
      return await action();
    } finally {
      projectMutationBusy = false;
    }
  }

  function project(id) {
    const value = catalog.find((candidate) => candidate.id === id);
    if (!value) throw new Error('project was not found');
    return value;
  }
  function projectRecord(value) {
    return { id: value.id, name: value.name, root: projectRoot(value.id), kind: 1, lastOpenedUnixMillis: value.lastOpenedUnixMillis ?? 0, pinned: value.pinned === true, managed: true };
  }
  function snapshotFiles(id, overlays = []) {
    if (!Array.isArray(overlays) || overlays.length > MAX_OVERLAYS) {
      throw new Error('compiler overlay list is invalid');
    }
    const overlayFiles = new Map();
    for (const overlay of overlays) {
      if (!overlay || !validPortableRelative(overlay.path) || !overlay.path.endsWith('.vo')
        || overlay.path === '.volang' || overlay.path.startsWith('.volang/')
        || typeof overlay.text !== 'string' || overlayFiles.has(overlay.path)
        || encoder.encode(overlay.text).byteLength > MAX_FILE_BYTES) {
        throw new Error('compiler overlay is invalid');
      }
      overlayFiles.set(overlay.path, overlay.text);
    }
    const files = [];
    let totalBytes = 0;
    for (const entry of listTree(projectRoot(id), true)) {
      if (entry.kind !== 0 && entry.kind !== 2) continue;
      const text = overlayFiles.has(entry.path)
        ? overlayFiles.get(entry.path)
        : readText(projectPath(id, entry.path, true));
      overlayFiles.delete(entry.path);
      totalBytes += encoder.encode(text).byteLength;
      if (totalBytes > MAX_PROJECT_SOURCE_BYTES) {
        throw new Error('project sources exceed the Studio compiler memory limit');
      }
      files.push({ path: entry.path, text });
    }
    if (overlayFiles.size > 0) throw new Error('compiler overlay file is unavailable');
    return files;
  }
  function writeSnapshotFiles(rootPath, files) {
    for (const file of files) {
      const slash = file.path.lastIndexOf('/');
      if (slash >= 0) check(vfs.mkdirAll(`${rootPath}/${file.path.slice(0, slash)}`, 0o755), 'create compiler source directory');
      writeBytes(`${rootPath}/${file.path}`, encoder.encode(file.text));
    }
  }
  function shareFiles(id) {
    return listTree(projectRoot(id))
      .filter((entry) => entry.kind !== 1 && !entry.path.startsWith('.volang/')
        && entry.path !== 'vo.lock' && entry.path !== 'vo.work')
      .map((entry) => ({ path: entry.path, text: readText(projectPath(id, entry.path)) }));
  }
  async function digestHex(bytes) {
    const digest = new Uint8Array(await crypto.subtle.digest('SHA-256', bytes));
    return Array.from(digest, (value) => value.toString(16).padStart(2, '0')).join('');
  }
  async function loadWorkspaceModules() {
    const indexResponse = await fetch('/runtime/workspace-modules/manifest.json', { cache: 'no-store' });
    if (!indexResponse.ok) {
      throw new Error(`official UI workspace bundle index is unavailable: HTTP ${indexResponse.status}`);
    }
    const index = await indexResponse.json();
    if (index?.schema !== 'volang.web-compiler-workspace-index/v1'
      || typeof index.bundle !== 'string' || !Number.isSafeInteger(index.bytes)
      || index.bytes <= 0 || index.bytes > MAX_WORKSPACE_BUNDLE_BYTES
      || typeof index.sha256 !== 'string' || !/^[0-9a-f]{64}$/u.test(index.sha256)
      || index.bundle !== `/runtime/workspace-modules/bundle-${index.sha256}.json`) {
      throw new Error('official UI workspace bundle index is invalid');
    }
    const response = await fetch(index.bundle, { cache: 'force-cache' });
    if (!response.ok) throw new Error(`official UI workspace bundle is unavailable: HTTP ${response.status}`);
    const declaredLength = Number(response.headers.get('content-length'));
    if (Number.isFinite(declaredLength) && declaredLength > MAX_WORKSPACE_BUNDLE_BYTES) {
      throw new Error('official UI workspace bundle exceeds its memory limit');
    }
    const bytes = new Uint8Array(await response.arrayBuffer());
    if (bytes.byteLength !== index.bytes || await digestHex(bytes) !== index.sha256) {
      throw new Error('official UI workspace bundle failed integrity verification');
    }
    const bundle = JSON.parse(decoder.decode(bytes));
    if (bundle?.schema !== 'volang.web-compiler-workspace/v2' || !Array.isArray(bundle.modules)) {
      throw new Error('official UI workspace bundle is invalid');
    }
    let fileCount = 0;
    let byteCount = 0;
    for (const module of bundle.modules) {
      if (!module || typeof module.path !== 'string' || typeof module.version !== 'string'
        || typeof module.intent !== 'string' || !Array.isArray(module.files)) {
        throw new Error('official UI workspace module is invalid');
      }
      const paths = new Set();
      for (const file of module.files) {
        if (!file || !validRelative(file.path) || paths.has(file.path)
          || typeof file.text !== 'string' || !Number.isSafeInteger(file.bytes)
          || file.bytes < 0 || typeof file.sha256 !== 'string'
          || !/^[0-9a-f]{64}$/u.test(file.sha256)) {
          throw new Error('official UI workspace module file is invalid');
        }
        paths.add(file.path);
        const sourceBytes = encoder.encode(file.text).byteLength;
        if (sourceBytes !== file.bytes) throw new Error('official UI workspace module file size is invalid');
        fileCount += 1;
        byteCount += sourceBytes;
        if (fileCount > MAX_WORKSPACE_BUNDLE_FILES || byteCount > MAX_WORKSPACE_BUNDLE_BYTES) {
          throw new Error('official UI workspace bundle exceeds its source limits');
        }
      }
      if (!paths.has('vo.mod')) throw new Error('official UI workspace module has no vo.mod');
    }
    return bundle;
  }
  function workspaceModules() {
    workspaceBundle ??= loadWorkspaceModules().catch((error) => {
      workspaceBundle = undefined;
      throw error;
    });
    return workspaceBundle;
  }
  async function provisionWorkspaceModules(id, rootPath = projectRoot(id)) {
    const [bundle] = await Promise.all([workspaceModules(), ensureRuntime()]);
    const manifests = [];
    const members = ['.'];
    for (let index = 0; index < bundle.modules.length; index += 1) {
      const module = bundle.modules[index];
      const member = `.volang/sdk/${index}`;
      members.push(member);
      for (const file of module.files) {
        const bytes = encoder.encode(file.text);
        writeBytes(`${rootPath}/${member}/${file.path}`, bytes);
      }
      manifests.push(readText(`${rootPath}/${member}/vo.mod`));
    }
    writeText(`${rootPath}/vo.work`, `format = 1\nmembers = [${members.map((value) => JSON.stringify(value)).join(', ')}]\n`);
    const rootMod = readText(`${rootPath}/vo.mod`);
    writeText(`${rootPath}/vo.lock`, prepareWorkspaceLock(rootMod, manifests));
  }
  async function importProject(token) {
    if (catalog.length >= MAX_PROJECTS) throw new Error('Studio project catalog is full');
    const handle = resolveBrowserFileHandle(token);
    if (!handle || typeof handle.entries !== 'function') throw new Error('selected browser directory capability expired');
    let idBase = handle.name.toLowerCase().replace(/[^a-z0-9_-]+/gu, '-').replace(/^-+|-+$/gu, '');
    if (!validName(idBase)) idBase = 'imported-project';
    let id = idBase;
    let suffix = 2;
    while (catalog.some((value) => value.id === id)) id = `${idBase}-${suffix++}`;
    const temporary = `/workspace/.studio-import-${id}-${Date.now()}`;
    check(vfs.mkdirAll(temporary, 0o755), 'create project import');
    let count = 0;
    let totalBytes = 0;
    const pending = [{ handle, relative: '', depth: 0 }];
    try {
      while (pending.length > 0) {
        const directory = pending.shift();
        if (directory.depth > MAX_IMPORT_DEPTH) throw new Error('project import exceeds the directory depth limit');
        for await (const [name, child] of directory.handle.entries()) {
          if (!validHandleName(name)) throw new Error('project import contains an invalid file name');
          if (['.git', '.volang', 'target', 'node_modules'].includes(name)) continue;
          count += 1;
          if (count > MAX_FILES) throw new Error('project import contains too many files');
          const relative = directory.relative === '' ? name : `${directory.relative}/${name}`;
          if (!validPortableRelative(relative)) {
            throw new Error(`project import path ${relative} is unavailable on every Studio platform`);
          }
          if (child.kind === 'directory' || typeof child.entries === 'function') {
            pending.push({ handle: child, relative, depth: directory.depth + 1 });
            continue;
          }
          if (typeof child.getFile !== 'function') throw new Error('project import contains an unreadable entry');
          const file = await child.getFile();
          if (file.size > MAX_FILE_BYTES || totalBytes + file.size > MAX_IMPORT_BYTES) throw new Error('project import exceeds its byte limit');
          const bytes = new Uint8Array(await file.arrayBuffer());
          totalBytes += bytes.byteLength;
          writeBytes(`${temporary}/${relative}`, bytes);
        }
      }
      const [, , , , , manifestError] = vfs.stat(`${temporary}/vo.mod`);
      check(manifestError, 'validate imported vo.mod');
      check(vfs.renameNoreplace(temporary, projectRoot(id)), 'publish imported project');
      const value = { id, name: handle.name, pinned: false, lastOpenedUnixMillis: Date.now() };
      catalog.push(value);
      writeText(catalogPath, JSON.stringify(catalog));
      await flushVFS();
      return projectRecord(value);
    } catch (error) {
      vfs.removeAll(temporary);
      throw error;
    } finally {
      releaseBrowserFileHandle(token);
    }
  }
  function rememberArtifact(bytecode, request) {
    const id = `web-artifact-${nextArtifact++}`;
    artifacts.set(id, bytecode);
    artifactOrder.push(id);
    while (artifactOrder.length > MAX_ARTIFACTS) artifacts.delete(artifactOrder.shift());
    return { id, kind: request.forPreview ? 3 : 0, entry: request.entry, bytes: [], diagnostics: [] };
  }
  async function compile(request, retainArtifact = true) {
    project(request.projectID);
    if (request.mode !== 0) throw new Error('compiler runtime mode is unavailable in Web Studio');
    if (!validPortableRelative(request.entry) || !request.entry.endsWith('.vo')
      || request.entry === '.volang' || request.entry.startsWith('.volang/')) {
      throw new Error('compiler entry path is invalid');
    }
    const manifest = readText(projectPath(request.projectID, 'vo.mod'));
    const [, , , , , lockError] = vfs.stat(projectPath(request.projectID, 'vo.lock'));
    if (lockError !== null && manifest.includes('[dependencies]')) {
      await provisionWorkspaceModules(request.projectID);
      await flushVFS();
    }
    const files = snapshotFiles(request.projectID, request.overlays ?? []);
    try {
      const response = await worker.request({ kind: 'compile', files, entry: request.entry });
      return retainArtifact ? rememberArtifact(new Uint8Array(response.bytecode), request) : null;
    } catch (workerError) {
      if (workerError.kind !== 'worker') throw workerError;
      await ensureRuntime();
      const fallbackRoot = `/__volang_studio_fallback/${nextFallbackCompile++}`;
      check(vfs.removeAll(fallbackRoot), 'clear fallback compiler snapshot');
      check(vfs.mkdirAll(fallbackRoot, 0o755), 'create fallback compiler snapshot');
      let bytecode;
      try {
        writeSnapshotFiles(fallbackRoot, files);
        const result = await compileProjectAutoInstall(request.entry, fallbackRoot);
        if (!result.success) throw new Error(result.errorMessage ?? workerError.message);
        bytecode = result.bytecode;
        if (!(bytecode instanceof Uint8Array)) throw new Error('browser compiler returned no bytecode');
      } finally {
        vfs.removeAll(fallbackRoot);
      }
      await flushVFS();
      return retainArtifact ? rememberArtifact(bytecode, request) : null;
    }
  }

  async function analyze(request) {
    project(request.projectID);
    if (!validPortableRelative(request.path) || !request.path.endsWith('.vo')
      || request.path === '.volang' || request.path.startsWith('.volang/')) {
      throw new Error('analysis path is invalid');
    }
    const manifest = readText(projectPath(request.projectID, 'vo.mod'));
    const [, , , , , lockError] = vfs.stat(projectPath(request.projectID, 'vo.lock'));
    if (lockError !== null && manifest.includes('[dependencies]')) {
      await provisionWorkspaceModules(request.projectID);
      await flushVFS();
    }
    const files = snapshotFiles(request.projectID, [{
      path: request.path,
      text: request.text,
      version: request.version,
    }]);
    try {
      await worker.request({ kind: 'analyze', files, entry: request.path });
    } catch (workerError) {
      if (workerError.kind !== 'worker') throw workerError;
      await ensureRuntime();
      const fallbackRoot = `/__volang_studio_fallback/${nextFallbackCompile++}`;
      check(vfs.removeAll(fallbackRoot), 'clear fallback analysis snapshot');
      check(vfs.mkdirAll(fallbackRoot, 0o755), 'create fallback analysis snapshot');
      try {
        writeSnapshotFiles(fallbackRoot, files);
        const result = await analyzeProjectAutoInstall(request.path, fallbackRoot);
        if (!result.success) throw new Error(result.errorMessage ?? workerError.message);
      } finally {
        vfs.removeAll(fallbackRoot);
      }
      await flushVFS();
    }
  }

  function startRunSession(bytecode, request) {
    const sessionID = `web-run-${nextRun++}`;
    const started = performance.now();
    const events = [{ kind: 0, text: 'Web VM session started', exitCode: 0, duration: 0, artifactID: '' }];
    if (request.artifact.kind === 3) {
      const duration = Math.max(1, Math.round((performance.now() - started) * 1_000_000));
      events.push({ kind: 4, text: 'preview ready', exitCode: 0, duration: 0, artifactID: request.artifact.id });
      events.push({ kind: 5, text: 'preview session ready', exitCode: 0, duration, artifactID: request.artifact.id });
      runs.set(sessionID, { events, done: true, worker: null });
      return sessionID;
    }
    const runWorker = new Worker('/studio-compiler-worker.js', { type: 'module', name: sessionID });
    const session = { events, done: false, worker: runWorker };
    runs.set(sessionID, session);
    const finish = (error, response) => {
      if (runs.get(sessionID) !== session || session.done) return;
      const duration = Math.max(1, Math.round((performance.now() - started) * 1_000_000));
      if (error) {
        events.push(
          { kind: 2, text: error.message, exitCode: 1, duration: 0, artifactID: '' },
          { kind: 5, text: 'process exited with errors', exitCode: 1, duration, artifactID: '' },
        );
      } else {
        if (response.run.stdout) events.push({ kind: 1, text: response.run.stdout.trimEnd(), exitCode: 0, duration: 0, artifactID: '' });
        if (response.run.stderr) events.push({ kind: 2, text: response.run.stderr.trimEnd(), exitCode: response.run.exitCode, duration: 0, artifactID: '' });
        events.push({ kind: 5, text: response.run.status === 'ok' ? 'process exited successfully' : 'process exited with errors', exitCode: response.run.exitCode, duration: response.run.duration, artifactID: '' });
      }
      session.done = true;
      runWorker.terminate();
      session.worker = null;
    };
    runWorker.onmessage = ({ data }) => {
      if (data?.id !== sessionID) return;
      finish(data.ok ? null : new Error(data.error ?? 'Studio run worker failed'), data);
    };
    runWorker.onerror = (event) => finish(new Error(event.message || 'Studio run worker stopped'));
    runWorker.onmessageerror = () => finish(new Error('Studio run worker returned an unreadable response'));
    try {
      const copy = bytecode.slice();
      runWorker.postMessage({ id: sessionID, kind: 'run', bytecode: copy.buffer, arguments: request.arguments ?? [] }, [copy.buffer]);
    } catch (error) {
      runs.delete(sessionID);
      runWorker.terminate();
      throw error;
    }
    return sessionID;
  }

  return async (service, operation, payload) => {
    if (service !== PROTOCOL) throw new Error(`unsupported Studio host protocol ${service}`);
    const request = decode(payload);
    if (operation === 'host.info') return encode({ info: { platform: 'web', persistent: true, runtimeModes: [0], canOpenLocal: typeof root.ownerDocument.defaultView?.showDirectoryPicker === 'function', canSyncRemote: false, canPreview: true } });
    if (operation === 'projects.list') return encode({ projects: catalog.map(projectRecord) });
    if (operation === 'projects.activate') {
      return mutateProject(async () => {
        const value = project(request.projectID);
        const previous = value.lastOpenedUnixMillis;
        value.lastOpenedUnixMillis = Date.now();
        try {
          writeText(catalogPath, JSON.stringify(catalog));
          await flushVFS();
        } catch (error) {
          value.lastOpenedUnixMillis = previous;
          throw error;
        }
        return encode({});
      });
    }
    if (operation === 'projects.create') {
      return mutateProject(async () => {
        if (!validName(request.name)) throw new Error('project name may contain letters, numbers, dash, and underscore');
        if (catalog.length >= MAX_PROJECTS) throw new Error('Studio project catalog is full');
        const starterFiles = validateStarterFiles(request.files);
        const needsOfficialUi = starterNeedsOfficialUi(starterFiles);
        const id = request.name.toLowerCase();
        if (catalog.some((value) => value.id === id)) throw new Error('project already exists');
        const value = { id, name: request.name, pinned: false, lastOpenedUnixMillis: Date.now() };
        const temporary = `/workspace/.studio-create-${id}-${Date.now()}`;
        let published = false;
        let cataloged = false;
        try {
          check(vfs.mkdirAll(temporary, 0o755), 'prepare project');
          if (starterFiles.length === 0) {
            writeText(`${temporary}/main.vo`, 'package main\n\nimport "github.com/vo-lang/ui"\n\nfunc App() ui.View {\n\treturn ui.Text("Hello from Volang UI")\n}\n\nfunc main() {\n\tif err := ui.Mount(App); err != nil { panic(err.Error()) }\n}\n');
          } else {
            for (const file of starterFiles) writeText(`${temporary}/${file.path}`, file.text);
          }
          const dependencies = needsOfficialUi
            ? '\n[dependencies]\n"github.com/vo-lang/ui" = "^0.1.4"\n'
            : '';
          writeText(`${temporary}/vo.mod`, `format = 1\nmodule = "local/${id}"\nversion = "0.1.0"\nvo = "0.1.4"\n${dependencies}`);
          if (needsOfficialUi) await provisionWorkspaceModules(id, temporary);
          check(vfs.renameNoreplace(temporary, projectRoot(id)), 'publish project');
          published = true;
          catalog.push(value);
          cataloged = true;
          writeText(catalogPath, JSON.stringify(catalog));
          await flushVFS();
          return encode({ project: projectRecord(value) });
        } catch (error) {
          vfs.removeAll(temporary);
          if (published) vfs.removeAll(projectRoot(id));
          if (cataloged) {
            catalog.splice(catalog.indexOf(value), 1);
            writeText(catalogPath, JSON.stringify(catalog));
            try { await flushVFS(); } catch { /* Preserve the original publication failure. */ }
          }
          throw error;
        }
      });
    }
    if (operation === 'projects.open') return mutateProject(async () => encode({ project: await importProject(request.root) }));
    if (operation === 'projects.rename') {
      return mutateProject(async () => {
        const value = project(request.projectID);
        if (!validName(request.name)) throw new Error('project name may contain letters, numbers, dash, and underscore');
        const nextID = request.name.toLowerCase();
        if (nextID !== value.id && catalog.some((candidate) => candidate.id === nextID)) throw new Error('project already exists');
        const previous = { id: value.id, name: value.name, lastOpenedUnixMillis: value.lastOpenedUnixMillis };
        let moved = false;
        try {
          if (nextID !== value.id) {
            check(vfs.renameNoreplace(projectRoot(value.id), projectRoot(nextID)), 'rename project');
            moved = true;
          }
          value.id = nextID;
          value.name = request.name;
          value.lastOpenedUnixMillis = Date.now();
          writeText(catalogPath, JSON.stringify(catalog));
          await flushVFS();
          return encode({ project: projectRecord(value) });
        } catch (error) {
          if (moved) vfs.renameNoreplace(projectRoot(nextID), projectRoot(previous.id));
          Object.assign(value, previous);
          throw error;
        }
      });
    }
    if (operation === 'projects.delete') {
      return mutateProject(async () => {
        const value = project(request.projectID);
        check(vfs.removeAll(projectRoot(value.id)), 'delete project');
        catalog.splice(catalog.indexOf(value), 1);
        writeText(catalogPath, JSON.stringify(catalog));
        await flushVFS();
        return encode({});
      });
    }
    if (operation === 'projects.forget') {
      return mutateProject(async () => {
        project(request.projectID);
        throw new Error('browser projects must be deleted instead of forgotten');
      });
    }
    if (operation === 'projects.share') {
      const value = project(request.projectID);
      let files;
      try {
        const snapshot = shareFiles(value.id);
        const sensitive = snapshot.find((file) => sensitiveSharePath(file.path));
        if (sensitive) throw new Error(`Remove sensitive file ${sensitive.path} before creating a portable link`);
        files = validateSharedFiles(snapshot);
      } catch (error) {
        return encode({ share: { shareable: false, developmentLink: '', runnerLink: '', reason: error.message } });
      }
      const payloadBytes = encoder.encode(JSON.stringify({ schema: 'volang.studio.share.v1', name: value.name, files }));
      if (payloadBytes.byteLength > MAX_SHARE_BYTES) {
        return encode({ share: { shareable: false, developmentLink: '', runnerLink: '', reason: 'The saved project is too large for a portable link. Publish it as a deployed application.' } });
      }
      const payloadValue = base64UrlEncode(payloadBytes);
      const origin = root.ownerDocument.defaultView?.location.origin ?? location.origin;
      return encode({ share: {
        shareable: true,
        developmentLink: `${origin}/#share=${payloadValue}`,
        runnerLink: `${origin}/runner#share=${payloadValue}`,
        reason: '',
      } });
    }
    if (operation === 'files.list') { project(request.projectID); return encode({ files: listTree(projectRoot(request.projectID)) }); }
    if (operation === 'files.read') { project(request.projectID); return encode({ text: readText(projectPath(request.projectID, request.path)) }); }
    if (operation === 'files.create') {
      project(request.projectID);
      if (!validPortableRelative(request.path)) throw new Error('file path must be available on every Studio platform');
      const path = projectPath(request.projectID, request.path);
      const [, , , , , existingError] = vfs.stat(path);
      if (existingError === null) throw new Error('file already exists');
      if (existingError !== 'file does not exist') check(existingError, `inspect ${path}`);
      writeText(path, request.text);
      await flushVFS();
      return encode({});
    }
    if (operation === 'files.write') { project(request.projectID); writeText(projectPath(request.projectID, request.path), request.text); await flushVFS(); return encode({}); }
    if (operation === 'files.rename') { project(request.projectID); if (!validPortableRelative(request.to)) throw new Error('file path must be available on every Studio platform'); check(vfs.renameNoreplace(projectPath(request.projectID, request.from), projectPath(request.projectID, request.to)), 'rename file'); await flushVFS(); return encode({}); }
    if (operation === 'files.delete') { project(request.projectID); check(vfs.removeAll(projectPath(request.projectID, request.path)), 'delete file'); await flushVFS(); return encode({}); }
    if (operation === 'language.analyze') {
      try { await analyze(request); return encode({ diagnostics: [] }); }
      catch (error) { return encode({ diagnostics: [compilerDiagnostic(request.path, error.message)] }); }
    }
    if (operation === 'compiler.compile') { const artifact = await compile(request); return encode({ ...artifact, artifact }); }
    if (operation === 'preview.open') {
      const bytecode = artifacts.get(request.id);
      if (!bytecode) throw new Error('compiled preview artifact is unavailable');
      const surfaceID = `web-preview-${request.id}`;
      if (!surfaces.has(surfaceID) && surfaces.size >= MAX_PREVIEWS) {
        throw new Error('too many Studio preview surfaces are active');
      }
      surfaces.set(surfaceID, bytecode);
      return encode({ surfaceID });
    }
    if (operation === 'preview.close') { surfaces.delete(request.surfaceID); return encode({}); }
    if (operation === 'run.start') {
      if (runs.size >= MAX_RUNS) throw new Error('too many Studio run sessions are active');
      if (request.mode !== 0) throw new Error('run mode is unavailable in Web Studio');
      request.arguments = validateRunArguments(request.arguments ?? []);
      const bytecode = artifacts.get(request.artifact.id);
      if (!bytecode) throw new Error(`compiled artifact ${request.artifact.id || '<empty>'} is unavailable; live artifacts: ${artifactOrder.join(', ')}`);
      return encode({ sessionID: startRunSession(bytecode, request) });
    }
    if (operation === 'run.next') {
      let session = runs.get(request.sessionID);
      if (!session) throw new Error('run session is unavailable');
      if (session.events.length === 0 && !session.done) {
        await new Promise((resolve) => setTimeout(resolve, 40));
        session = runs.get(request.sessionID);
        if (!session) return encode({ events: [], done: true });
      }
      const maximum = Number.isInteger(request.maximum) ? Math.max(1, Math.min(128, request.maximum)) : 1;
      const events = session.events.splice(0, maximum);
      const done = session.done && session.events.length === 0;
      if (done) runs.delete(request.sessionID);
      return encode({ events, done });
    }
    if (operation === 'run.stop') {
      const session = runs.get(request.sessionID);
      session?.worker?.terminate();
      runs.delete(request.sessionID);
      return encode({});
    }
    if (operation === 'account.state') return encode({ account });
    if (operation === 'account.connect') {
      const token = await requestGitHubToken(root);
      if (token === null) throw new Error('GitHub account connection was cancelled');
      const response = await fetch('https://api.github.com/user', {
        headers: {
          Accept: 'application/vnd.github+json',
          Authorization: `Bearer ${token}`,
          'X-GitHub-Api-Version': '2022-11-28',
        },
        cache: 'no-store',
      });
      if (!response.ok) throw new Error(`GitHub account connection failed: HTTP ${response.status}`);
      const user = await response.json();
      if (typeof user?.login !== 'string' || user.login === '') throw new Error('GitHub returned an invalid account response');
      account = {
        provider: 'github', connected: true, login: user.login,
        name: typeof user.name === 'string' ? user.name : '',
        avatarURL: typeof user.avatar_url === 'string' ? user.avatar_url : '',
      };
      return encode({ account });
    }
    if (operation === 'account.disconnect') {
      account = { provider: 'github', connected: false, login: '', name: '', avatarURL: '' };
      return encode({});
    }
    if (operation === 'remote.state') { project(request.projectID); return encode({ state: { provider: 'browser', repository: 'Persistent local workspace', branch: 'local', ahead: 0, behind: 0, dirty: false } }); }
    if (operation === 'remote.diff') throw new Error('remote diffs require a configured repository provider');
    if (operation === 'remote.pull' || operation === 'remote.push' || operation === 'remote.next' || operation === 'remote.stop') throw new Error('remote synchronization requires a configured repository provider');
    if (operation === 'remote.delete') throw new Error('cloud repository deletion requires a configured repository provider');
    throw new Error(`unsupported Studio host operation ${operation}`);
  };
}
