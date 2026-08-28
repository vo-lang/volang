import {
  compileProjectAutoInstall,
  flushVFS,
  init,
  initVFS,
  prepareWorkspaceLock,
  releaseBrowserFileHandle,
  resolveBrowserFileHandle,
  vfs,
} from '/runtime/dist/index.js';

const PROTOCOL = 'volang.studio.host.v1';
const encoder = new TextEncoder();
const decoder = new TextDecoder('utf-8', { fatal: true });
const MAX_FILE_BYTES = 4 * 1024 * 1024;
const MAX_STARTER_BYTES = 8 * 1024 * 1024;
const MAX_FILES = 5000;
const MAX_ARTIFACTS = 32;
const MAX_IMPORT_BYTES = 128 * 1024 * 1024;
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
function validName(name) { return typeof name === 'string' && /^[A-Za-z0-9_-]{1,128}$/u.test(name); }
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
    if (!file || !validRelative(file.path) || file.path.startsWith('.volang/')
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
    if (!file || !validRelative(file.path) || typeof file.text !== 'string') throw new Error('starter file is invalid');
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
function validHandleName(name) {
  return typeof name === 'string' && name.length > 0 && name.length <= 255
    && name !== '.' && name !== '..' && !name.includes('/') && !name.includes('\\') && !name.includes('\0');
}
function projectRoot(id) { return `/workspace/${id}`; }
function projectPath(id, path) {
  if (!validRelative(path)) throw new Error('project path is invalid');
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
  return output;
}

class WorkerQueue {
  constructor() {
    this.next = 1;
    this.pending = new Map();
    this.worker = new Worker('/studio-compiler-worker.js', { type: 'module', name: 'volang-studio-compiler' });
    this.worker.onmessage = ({ data }) => {
      const pending = this.pending.get(data.id);
      if (!pending) return;
      this.pending.delete(data.id);
      if (data.ok) pending.resolve(data);
      else pending.reject(new Error(data.error ?? 'Studio worker failed'));
    };
    this.worker.onerror = (event) => {
      for (const pending of this.pending.values()) pending.reject(new Error(event.message || 'Studio worker stopped'));
      this.pending.clear();
    };
  }
  request(message, transfer = []) {
    const id = String(this.next++);
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.worker.postMessage({ ...message, id }, transfer);
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
    frame.style.cssText = 'display:block;width:100%;height:100%;min-width:0;min-height:240px;border:0;background:#0b0e14';
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
  const catalogPath = '/workspace/.volang-studio-projects.json';
  let catalog;
  try { catalog = JSON.parse(readText(catalogPath)); } catch { catalog = []; }
  if (!Array.isArray(catalog) || catalog.length === 0) {
    catalog = [{ id: 'hello-studio', name: 'hello-studio', pinned: true, lastOpenedUnixMillis: Date.now() }];
    const project = projectRoot('hello-studio');
    check(vfs.mkdirAll(project, 0o755), 'create starter project');
    writeText(`${project}/main.vo`, 'package main\n\nimport "fmt"\n\nfunc main() { fmt.Println("Hello from browser Studio") }\n');
    writeText(`${project}/vo.mod`, 'format = 1\nmodule = "local/hello-studio"\nversion = "0.1.0"\nvo = "0.1.4"\n');
    writeText(`${project}/README.md`, '# Hello Studio\n\nPersistent browser project powered by Volang Web.\n');
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
    const id = `shared-${digest.slice(0, 12)}`;
    const existing = catalog.find((value) => value.id === id);
    const value = existing ?? {
      id,
      name: validName(bundle.name) ? bundle.name : 'shared-project',
      pinned: true,
      lastOpenedUnixMillis: Date.now(),
      shared: true,
    };
    if (!existing) {
      check(vfs.mkdirAll(projectRoot(id), 0o755), 'create shared project');
      for (const file of files) writeText(projectPath(id, file.path), file.text);
      await flushVFS();
    }
    catalog = [value, ...catalog.filter((candidate) => candidate.id !== id)];
  }

  const artifacts = new Map();
  const artifactOrder = [];
  const runs = new Map();
  const credentials = new Map();
  let account = { provider: 'github', connected: false, login: '', name: '', avatarURL: '' };
  const surfaces = new Map();
  const worker = new WorkerQueue();
  let nextArtifact = 1;
  let nextRun = 1;
  let runtimeReady;
  const ensureRuntime = () => (runtimeReady ??= init(new URL('/runtime/pkg/vo_web_bg.wasm', location.origin)));
  let workspaceBundle;
  installPreviewObserver(root, surfaces);

  function project(id) {
    const value = catalog.find((candidate) => candidate.id === id);
    if (!value) throw new Error('project was not found');
    return value;
  }
  function projectRecord(value) {
    return { id: value.id, name: value.name, root: projectRoot(value.id), kind: 1, lastOpenedUnixMillis: value.lastOpenedUnixMillis ?? 0, pinned: value.pinned === true, managed: true };
  }
  function snapshotFiles(id) {
    return listTree(projectRoot(id), true)
      .filter((entry) => entry.kind === 0 || entry.kind === 2)
      .map((entry) => ({ path: entry.path, text: readText(projectPath(id, entry.path)) }));
  }
  function shareFiles(id) {
    return listTree(projectRoot(id))
      .filter((entry) => entry.kind !== 1 && !entry.path.startsWith('.volang/')
        && entry.path !== 'vo.lock' && entry.path !== 'vo.work')
      .map((entry) => ({ path: entry.path, text: readText(projectPath(id, entry.path)) }));
  }
  async function workspaceModules() {
    if (workspaceBundle) return workspaceBundle;
    const response = await fetch('/runtime/workspace-modules/manifest.json', { cache: 'no-store' });
    if (!response.ok) throw new Error(`official UI workspace bundle is unavailable: HTTP ${response.status}`);
    const bundle = await response.json();
    if (bundle?.schema !== 'volang.web-compiler-workspace/v1' || !Array.isArray(bundle.modules)) {
      throw new Error('official UI workspace bundle is invalid');
    }
    workspaceBundle = bundle;
    return bundle;
  }
  async function digestHex(bytes) {
    const digest = new Uint8Array(await crypto.subtle.digest('SHA-256', bytes));
    return Array.from(digest, (value) => value.toString(16).padStart(2, '0')).join('');
  }
  async function provisionWorkspaceModules(id) {
    await ensureRuntime();
    const bundle = await workspaceModules();
    const manifests = [];
    const members = ['.'];
    for (let index = 0; index < bundle.modules.length; index += 1) {
      const module = bundle.modules[index];
      if (typeof module.path !== 'string' || !Array.isArray(module.files)) throw new Error('workspace module entry is invalid');
      const member = `.volang/sdk/${index}`;
      members.push(member);
      for (const file of module.files) {
        if (!validRelative(file.path) || typeof file.sha256 !== 'string') throw new Error('workspace module file entry is invalid');
        const response = await fetch(`${module.root}/${file.path}`, { cache: 'no-store' });
        if (!response.ok) throw new Error(`workspace module file ${file.path} is unavailable`);
        const bytes = new Uint8Array(await response.arrayBuffer());
        if (bytes.byteLength !== file.bytes || await digestHex(bytes) !== file.sha256) throw new Error(`workspace module file ${file.path} failed integrity verification`);
        writeBytes(`${projectRoot(id)}/${member}/${file.path}`, bytes);
      }
      manifests.push(readText(`${projectRoot(id)}/${member}/vo.mod`));
    }
    writeText(`${projectRoot(id)}/vo.work`, `format = 1\nmembers = [${members.map((value) => JSON.stringify(value)).join(', ')}]\n`);
    const rootMod = readText(`${projectRoot(id)}/vo.mod`);
    writeText(`${projectRoot(id)}/vo.lock`, prepareWorkspaceLock(rootMod, manifests));
  }
  async function importProject(token) {
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
  async function compile(request) {
    project(request.projectID);
    const manifest = readText(projectPath(request.projectID, 'vo.mod'));
    const [, , , , , lockError] = vfs.stat(projectPath(request.projectID, 'vo.lock'));
    if (lockError !== null && manifest.includes('[dependencies]')) {
      await provisionWorkspaceModules(request.projectID);
      await flushVFS();
    }
    const files = snapshotFiles(request.projectID);
    const overlay = request.overlays?.find((value) => value.path === request.entry);
    try {
      const response = await worker.request({ kind: 'compile', files, entry: request.entry, overlay });
      return rememberArtifact(new Uint8Array(response.bytecode), request);
    } catch (workerError) {
      await ensureRuntime();
      const result = await compileProjectAutoInstall(request.entry, projectRoot(request.projectID), overlay?.path, overlay?.text);
      if (!result.success) throw new Error(result.errorMessage ?? workerError.message);
      const bytecode = result.bytecode;
      if (!(bytecode instanceof Uint8Array)) throw new Error('browser compiler returned no bytecode');
      await flushVFS();
      return rememberArtifact(bytecode, request);
    }
  }

  return async (service, operation, payload) => {
    if (service !== PROTOCOL) throw new Error(`unsupported Studio host protocol ${service}`);
    const request = decode(payload);
    if (operation === 'host.info') return encode({ info: { platform: 'web', persistent: true, runtimeModes: [0], canOpenLocal: typeof root.ownerDocument.defaultView?.showDirectoryPicker === 'function', canSyncRemote: false, canPreview: true } });
    if (operation === 'projects.list') return encode({ projects: catalog.map(projectRecord) });
    if (operation === 'projects.create') {
      if (!validName(request.name)) throw new Error('project name may contain letters, numbers, dash, and underscore');
      const starterFiles = validateStarterFiles(request.files);
      const id = request.name.toLowerCase();
      if (catalog.some((value) => value.id === id)) throw new Error('project already exists');
      const value = { id, name: request.name, pinned: false, lastOpenedUnixMillis: Date.now() };
      try {
        check(vfs.mkdirAll(projectRoot(id), 0o755), 'create project');
        if (starterFiles.length === 0) {
          writeText(`${projectRoot(id)}/main.vo`, 'package main\n\nimport "github.com/vo-lang/ui"\n\nfunc App() ui.View {\n\treturn ui.Text("Hello from Volang UI")\n}\n\nfunc main() {\n\tif err := ui.Mount(App); err != nil { panic(err.Error()) }\n}\n');
        } else {
          for (const file of starterFiles) writeText(`${projectRoot(id)}/${file.path}`, file.text);
        }
        writeText(`${projectRoot(id)}/vo.mod`, `format = 1\nmodule = "local/${id}"\nversion = "0.1.0"\nvo = "0.1.4"\n\n[dependencies]\n"github.com/vo-lang/ui" = "^0.1.4"\n`);
        await provisionWorkspaceModules(id);
        catalog.push(value);
        writeText(catalogPath, JSON.stringify(catalog));
        await flushVFS();
        return encode({ project: projectRecord(value) });
      } catch (error) {
        vfs.removeAll(projectRoot(id));
        throw error;
      }
    }
    if (operation === 'projects.open') return encode({ project: await importProject(request.root) });
    if (operation === 'projects.rename') {
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
    }
    if (operation === 'projects.delete') {
      const value = project(request.projectID);
      check(vfs.removeAll(projectRoot(value.id)), 'delete project');
      catalog.splice(catalog.indexOf(value), 1);
      writeText(catalogPath, JSON.stringify(catalog));
      await flushVFS();
      return encode({});
    }
    if (operation === 'projects.forget') {
      const value = project(request.projectID);
      catalog.splice(catalog.indexOf(value), 1);
      writeText(catalogPath, JSON.stringify(catalog));
      await flushVFS();
      return encode({});
    }
    if (operation === 'projects.share') {
      const value = project(request.projectID);
      let files;
      try {
        files = validateSharedFiles(shareFiles(value.id));
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
    if (operation === 'files.write') { project(request.projectID); writeText(projectPath(request.projectID, request.path), request.text); await flushVFS(); return encode({}); }
    if (operation === 'files.rename') { project(request.projectID); check(vfs.renameNoreplace(projectPath(request.projectID, request.from), projectPath(request.projectID, request.to)), 'rename file'); await flushVFS(); return encode({}); }
    if (operation === 'files.delete') { project(request.projectID); check(vfs.removeAll(projectPath(request.projectID, request.path)), 'delete file'); await flushVFS(); return encode({}); }
    if (operation === 'language.analyze') {
      try { await compile({ projectID: request.projectID, entry: request.path, mode: 0, forPreview: false, overlays: [{ path: request.path, text: request.text, version: request.version }] }); return encode({ diagnostics: [] }); }
      catch (error) { return encode({ diagnostics: [{ path: request.path, line: 1, column: 1, endLine: 1, endColumn: 1, severity: 3, code: 'web/compiler', message: error.message }] }); }
    }
    if (operation === 'compiler.compile') { const artifact = await compile(request); return encode({ ...artifact, artifact }); }
    if (operation === 'preview.open') {
      const bytecode = artifacts.get(request.id);
      if (!bytecode) throw new Error('compiled preview artifact is unavailable');
      const surfaceID = `web-preview-${request.id}`;
      surfaces.set(surfaceID, bytecode);
      return encode({ surfaceID });
    }
    if (operation === 'preview.close') { surfaces.delete(request.surfaceID); return encode({}); }
    if (operation === 'run.start') {
      const bytecode = artifacts.get(request.artifact.id);
      if (!bytecode) throw new Error(`compiled artifact ${request.artifact.id || '<empty>'} is unavailable; live artifacts: ${artifactOrder.join(', ')}`);
      const sessionID = `web-run-${nextRun++}`;
      const events = [{ kind: 0, text: 'Web VM session started', exitCode: 0, duration: 0, artifactID: '' }];
      if (request.artifact.kind === 3) {
        events.push({ kind: 4, text: 'preview ready', exitCode: 0, duration: 0, artifactID: request.artifact.id });
        events.push({ kind: 5, text: 'preview session ready', exitCode: 0, duration: 0, artifactID: request.artifact.id });
      } else {
        try {
          const copy = bytecode.slice();
          const response = await worker.request({ kind: 'run', bytecode: copy.buffer, arguments: request.arguments ?? [] }, [copy.buffer]);
          if (response.run.stdout) events.push({ kind: 1, text: response.run.stdout.trimEnd(), exitCode: 0, duration: 0, artifactID: '' });
          if (response.run.stderr) events.push({ kind: 2, text: response.run.stderr.trimEnd(), exitCode: response.run.exitCode, duration: 0, artifactID: '' });
          events.push({ kind: 5, text: response.run.status === 'ok' ? 'process exited successfully' : 'process exited with errors', exitCode: response.run.exitCode, duration: 0, artifactID: '' });
        } catch (error) { events.push({ kind: 2, text: error.message, exitCode: 1, duration: 0, artifactID: '' }, { kind: 5, text: 'process exited with errors', exitCode: 1, duration: 0, artifactID: '' }); }
      }
      runs.set(sessionID, events);
      return encode({ sessionID });
    }
    if (operation === 'run.next') { const events = runs.get(request.sessionID) ?? []; const batch = events.splice(0, Math.max(1, Math.min(128, request.maximum))); if (events.length === 0) runs.delete(request.sessionID); return encode({ events: batch, done: events.length === 0 }); }
    if (operation === 'run.stop') { runs.delete(request.sessionID); return encode({}); }
    if (operation === 'account.state') return encode({ account });
    if (operation === 'account.connect') {
      const window = root.ownerDocument.defaultView;
      const token = window?.prompt('Paste a GitHub token for this Studio session. It remains inside the browser host and is cleared when the page closes.');
      if (typeof token !== 'string' || token.trim() === '') throw new Error('GitHub account connection was cancelled');
      const response = await fetch('https://api.github.com/user', {
        headers: {
          Accept: 'application/vnd.github+json',
          Authorization: `Bearer ${token.trim()}`,
          'X-GitHub-Api-Version': '2022-11-28',
        },
        cache: 'no-store',
      });
      if (!response.ok) throw new Error(`GitHub account connection failed: HTTP ${response.status}`);
      const user = await response.json();
      if (typeof user?.login !== 'string' || user.login === '') throw new Error('GitHub returned an invalid account response');
      credentials.set('github.token', token.trim());
      account = {
        provider: 'github', connected: true, login: user.login,
        name: typeof user.name === 'string' ? user.name : '',
        avatarURL: typeof user.avatar_url === 'string' ? user.avatar_url : '',
      };
      return encode({ account });
    }
    if (operation === 'account.disconnect') {
      credentials.delete('github.token');
      account = { provider: 'github', connected: false, login: '', name: '', avatarURL: '' };
      return encode({});
    }
    if (operation === 'remote.state') { project(request.projectID); return encode({ state: { provider: 'browser', repository: 'Persistent local workspace', branch: 'local', ahead: 0, behind: 0, dirty: false } }); }
    if (operation === 'remote.diff') throw new Error('remote diffs require a configured repository provider');
    if (operation === 'remote.pull' || operation === 'remote.push' || operation === 'remote.next' || operation === 'remote.stop') throw new Error('remote synchronization requires a configured repository provider');
    if (operation === 'remote.delete') throw new Error('cloud repository deletion requires a configured repository provider');
    if (operation === 'credentials.get') return encode({ value: credentials.get(request.key) ?? '', present: credentials.has(request.key) });
    if (operation === 'credentials.set') { credentials.set(request.key, request.value); return encode({}); }
    if (operation === 'credentials.delete') { credentials.delete(request.key); return encode({}); }
    throw new Error(`unsupported Studio host operation ${operation}`);
  };
}
