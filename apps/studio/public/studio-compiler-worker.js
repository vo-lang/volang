import { analyzeProject, compileProject, init, runWithArgs, vfs } from '/runtime/dist/index.js';

const encoder = new TextEncoder();
let initialized;

function ready() {
  initialized ??= init(new URL('/runtime/pkg/vo_web_bg.wasm', self.location.origin));
  return initialized;
}

function requireSuccess(error, operation) {
  if (error !== null) throw new Error(`${operation}: ${error}`);
}

function prepareProject(jobId, files) {
  const root = `/__volang_studio_worker/${jobId}`;
  requireSuccess(vfs.removeAll(root), 'clear compiler snapshot');
  requireSuccess(vfs.mkdirAll(root, 0o755), 'create compiler snapshot');
  try {
    for (const file of files) {
      const slash = file.path.lastIndexOf('/');
      if (slash >= 0) requireSuccess(vfs.mkdirAll(`${root}/${file.path.slice(0, slash)}`, 0o755), 'create source directory');
      requireSuccess(vfs.writeFile(`${root}/${file.path}`, encoder.encode(file.text), 0o644), 'write source snapshot');
    }
    return root;
  } catch (error) {
    vfs.removeAll(root);
    throw error;
  }
}

self.onmessage = async (event) => {
  const request = event.data;
  try {
    await ready();
    if (request.kind === 'compile') {
      const root = prepareProject(request.id, request.files);
      let bytecode;
      try {
        const result = compileProject(
          request.entry,
          root,
          '',
          request.overlay?.path,
          request.overlay?.text,
        );
        if (!result.success) {
          const error = new Error(result.errorMessage ?? 'browser compilation failed');
          error.kind = 'compile';
          throw error;
        }
        bytecode = result.bytecode;
      } finally {
        requireSuccess(vfs.removeAll(root), 'release compiler snapshot');
      }
      if (!(bytecode instanceof Uint8Array)) throw new Error('browser compiler returned no bytecode');
      self.postMessage({ id: request.id, ok: true, bytecode }, [bytecode.buffer]);
      return;
    }
    if (request.kind === 'analyze') {
      const root = prepareProject(request.id, request.files);
      try {
        const result = analyzeProject(request.entry, root);
        if (!result.success) {
          const error = new Error(result.errorMessage ?? 'browser analysis failed');
          error.kind = 'analysis';
          throw error;
        }
      } finally {
        requireSuccess(vfs.removeAll(root), 'release compiler snapshot');
      }
      self.postMessage({ id: request.id, ok: true });
      return;
    }
    if (request.kind === 'run') {
      const started = performance.now();
      const result = runWithArgs(new Uint8Array(request.bytecode), request.arguments ?? []);
      const duration = Math.max(1, Math.round((performance.now() - started) * 1_000_000));
      self.postMessage({
        id: request.id,
        ok: true,
        run: { status: result.status, stdout: result.stdout, stderr: result.stderr, exitCode: result.exitCode ?? 0, duration },
      });
      return;
    }
    throw new Error('unknown Studio worker request');
  } catch (cause) {
    const error = cause instanceof Error ? cause : new Error(String(cause));
    self.postMessage({ id: request?.id, ok: false, error: error.message, failure: error.kind ?? 'worker' });
  }
};
