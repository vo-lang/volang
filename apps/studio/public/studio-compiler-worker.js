import { compileProject, init, runWithArgs, vfs } from '/runtime/dist/index.js';

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
  for (const file of files) {
    const slash = file.path.lastIndexOf('/');
    if (slash >= 0) requireSuccess(vfs.mkdirAll(`${root}/${file.path.slice(0, slash)}`, 0o755), 'create source directory');
    requireSuccess(vfs.writeFile(`${root}/${file.path}`, encoder.encode(file.text), 0o644), 'write source snapshot');
  }
  return root;
}

self.onmessage = async (event) => {
  const request = event.data;
  try {
    await ready();
    if (request.kind === 'compile') {
      const root = prepareProject(request.id, request.files);
      const result = compileProject(
        request.entry,
        root,
        '',
        request.overlay?.path,
        request.overlay?.text,
      );
      if (!result.success) throw new Error(result.errorMessage ?? 'browser compilation failed');
      const bytecode = result.bytecode;
      if (!(bytecode instanceof Uint8Array)) throw new Error('browser compiler returned no bytecode');
      self.postMessage({ id: request.id, ok: true, bytecode }, [bytecode.buffer]);
      return;
    }
    if (request.kind === 'run') {
      const result = runWithArgs(new Uint8Array(request.bytecode), request.arguments ?? []);
      self.postMessage({
        id: request.id,
        ok: true,
        run: { status: result.status, stdout: result.stdout, stderr: result.stderr, exitCode: result.exitCode ?? 0 },
      });
      return;
    }
    throw new Error('unknown Studio worker request');
  } catch (cause) {
    const error = cause instanceof Error ? cause : new Error(String(cause));
    self.postMessage({ id: request?.id, ok: false, error: error.message });
  }
};
