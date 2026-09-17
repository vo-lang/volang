import assert from 'node:assert/strict';

export async function checkLocalApplicationCompiler(page, url) {
  await page.route('**/__local-compiler', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Local application compiler</title>' }));
  await page.goto(url + '/__local-compiler');
  const result = await page.evaluate(async () => {
    const script = URL.createObjectURL(new Blob([`
      self.onmessage = async () => {
        try {
          const runtime = await import('${location.origin}/compiler/vo_web.js');
          const { vfs, registerVFSBindings } = await import('${location.origin}/host/vfs.js');
          await runtime.default(); await vfs.init(); registerVFSBindings();
          const files = {
            '/application/vo.mod': 'format = 1\\nmodule = "local/browser-application"\\nversion = "0.1.0"\\nvo = "0.1.4"\\n',
            '/application/vo.work': 'format = 1\\nmembers = ["."]\\n',
            '/application/app/app.vo': 'package app\\nconst Value = 7\\n',
            '/application/main.vo': 'package main\\nimport "local/browser-application/app"\\nfunc main() { println(app.Value) }\\n',
            '/application/development/main.vo': 'package main\\nimport "local/browser-application/app"\\nfunc main() { println(app.Value + 1) }\\n',
          };
          for (const [path, source] of Object.entries(files)) {
            const error = vfs.mkdirAll(path.slice(0, path.lastIndexOf('/')), 0o755) ?? vfs.writeFile(path, new TextEncoder().encode(source), 0o644);
            if (error) throw new Error(error);
          }
          const outputs = [];
          for (const entry of ['main.vo', 'development/main.vo']) {
            const compiled = runtime.compileProject(entry, '/application', '');
            try {
              if (!compiled.success) throw new Error(compiled.errorMessage);
              const result = runtime.run(compiled.bytecode);
              try { if (result.status !== 'ok') throw new Error(result.stderr); outputs.push(result.stdout); }
              finally { result.free(); }
            } finally { compiled.free(); }
          }
          const diagnostics = [], structured = [];
          for (const source of [
            'package main\\nfunc main() { unused := 1; missing() }\\n',
            'package main\\nfunc main( {}\\n',
            'package main\\nimport "strings"\\nfunc main() { var text strings.Builder; _ = text; missing() }\\n',
          ]) {
            const compiled = runtime.compile(source, 'draft.vo');
            try {
              if (compiled.success) throw new Error('Invalid draft was accepted.');
              if (compiled.diagnosticsJson == null) throw new Error('Missing draft diagnostics: ' + compiled.errorMessage);
              diagnostics.push(compiled.errorMessage);
              structured.push(JSON.parse(compiled.diagnosticsJson));
            } finally { compiled.free(); }
          }
          const overlays = [];
          const overlay = 'package main\\nimport "local/browser-application/app"\\nfunc main() { _ = app.Value; _ = "中文🙂"; missing() }\\n';
          for (const compile of [runtime.compileProject, runtime.analyzeProject]) {
            const compiled = compile('main.vo', '/application', '', 'main.vo', overlay);
            try {
              if (compiled.success) throw new Error('Invalid overlay was accepted.');
              if (compiled.diagnosticsJson == null) throw new Error('Missing overlay diagnostics: ' + compiled.errorMessage);
              overlays.push(JSON.parse(compiled.diagnosticsJson));
            } finally { compiled.free(); }
            const saved = compile('main.vo', '/application', '');
            try {
              if (!saved.success || saved.diagnosticsJson != null) throw new Error('An overlay changed the saved project or left stale diagnostics.');
            } finally { saved.free(); }
          }
          const repaired = runtime.compile('package main\\nfunc main() { println(42) }\\n', 'draft.vo');
          try {
            if (!repaired.success || repaired.diagnosticsJson != null) throw new Error(repaired.errorMessage || 'Repair retained stale diagnostics.');
            const result = runtime.run(repaired.bytecode);
            try { if (result.status !== 'ok') throw new Error(result.stderr); outputs.push(result.stdout); }
            finally { result.free(); }
          } finally { repaired.free(); }
          let dynamicError;
          const dynamic = runtime.compile('package main\\nimport "dyn"\\nfunc main() { println(dyn.ErrNilBase.Error()) }\\n', 'main.vo');
          try {
            if (!dynamic.success || dynamic.diagnosticsJson != null) throw new Error('Dynamic sentinel dependency generated a compiler warning');
            const result = runtime.run(dynamic.bytecode);
            try { if (result.status !== 'ok') throw new Error(result.stderr); dynamicError = result.stdout; }
            finally { result.free(); }
          } finally { dynamic.free(); }
          const warnings = [];
          const warningSource = 'package main\\nfunc main() { unused := "中文🙂"; println(42) }\\n';
          for (const [kind, compile] of [
            ['source', () => runtime.compile(warningSource, 'main.vo')],
            ['project', () => runtime.compileProject('main.vo', '/application', '', 'main.vo', warningSource)],
            ['analysis', () => runtime.analyzeProject('main.vo', '/application', '', 'main.vo', warningSource)],
            ['installed-project', () => runtime.compileProjectAutoInstall('main.vo', '/application', 'main.vo', warningSource)],
            ['installed-analysis', () => runtime.analyzeProjectAutoInstall('main.vo', '/application', 'main.vo', warningSource)],
          ]) {
            const compiled = await compile();
            try {
              if (!compiled.success || !compiled.diagnosticsJson || compiled.errorMessage != null) throw new Error(kind + ' lost successful warnings');
              const runnable = !kind.includes('analysis');
              if (runnable !== !!compiled.bytecode) throw new Error(kind + ' changed bytecode availability');
              warnings.push({kind, source:warningSource, diagnostics:JSON.parse(compiled.diagnosticsJson)});
            } finally { compiled.free(); }
          }
          self.postMessage({ outputs, diagnostics, structured, overlays, overlay, warnings, dynamicError });
        } catch (error) { self.postMessage({ error: String(error.message ?? error) }); }
      };
    `], { type: 'text/javascript' }));
    const worker = new Worker(script, { type: 'module' });
    let timer;
    try {
      return await new Promise((resolve, reject) => {
        timer = setTimeout(() => reject(new Error('Browser project compilation exceeded 20 seconds.')), 20000);
        worker.onmessage = event => resolve(event.data);
        worker.onerror = event => { event.preventDefault(); reject(new Error(event.message)); };
        worker.postMessage({});
      });
    } finally { clearTimeout(timer); worker.terminate(); URL.revokeObjectURL(script); }
  });
  assert.equal(result.error, undefined);
  assert.deepEqual(result.outputs, ['7\n', '8\n', '42\n']);
  assert.equal(result.dynamicError,'dynamic access: base value is nil\n');
  assert.equal(result.warnings.length,5);
  for (const {kind,source,diagnostics} of result.warnings) {
    assert.equal(diagnostics.items.length,1,kind);
    const warning=diagnostics.items[0];
    assert.equal(warning.severity,'warning');
    assert.equal(warning.location.file,'main.vo');
    assert.equal(source.slice(warning.location.startByte,warning.location.endByte),'unused');
  }
  const [typeError, parseError, typeImportError] = result.diagnostics;
  assert.match(typeError, /type check failed: 1 error\(s\), 1 warning\(s\)/);
  const messages = typeError.split('\n').filter(line => line.startsWith('  - '));
  assert.equal(messages.length, 2);
  assert.match(messages[0], /draft\.vo:2:28: error\[E\d+\]: .*missing/);
  assert.match(messages[1], /draft\.vo:2:15: warning\[E\d+\]: .*unused/);
  assert.match(parseError, /parse error:.*\n\s+- draft\.vo:\d+:\d+: error\[E\d+\]:/);
  assert.match(typeImportError, /type check failed: 1 error\(s\)\n/);
  assert(!typeImportError.includes('warning') && !typeImportError.includes('imported but not used'));
  for (const diagnostic of [...result.structured, ...result.overlays]) {
    assert.equal(diagnostic.version, 1);
    assert.equal(diagnostic.positionEncoding, 'utf-16');
    assert.equal(diagnostic.items[0].severity, 'error');
    assert.equal(typeof diagnostic.items[0].code, 'number');
    assert.equal(typeof diagnostic.items[0].message, 'string');
    assert(diagnostic.items[0].location);
  }
  assert.deepEqual(result.structured[0].items.map(item => item.severity), ['error', 'warning']);
  assert.deepEqual(result.structured[0].items[0].location.start, {line:1, character:27});
  for (const overlay of result.overlays) {
    const location = overlay.items[0].location;
    assert.equal(location.file, 'main.vo');
    assert.deepEqual(location.start, {line:2, character:result.overlay.split('\n')[2].indexOf('missing')});
    assert.equal(location.end.character - location.start.character, 7);
    assert.equal(new TextDecoder().decode(new TextEncoder().encode(result.overlay).slice(location.startByte, location.endByte)), 'missing');
  }
  return { passed: true, contracts: ['memory-only-project', 'local-module-self-package', 'shared-application-two-entries', 'browser-compile-and-execute',
    'source-diagnostics-with-severity-and-code', 'errors-before-warnings', 'type-only-import-use', 'compile-after-invalid-draft',
    'structured-diagnostics-utf16', 'unsaved-overlay-diagnostics', 'diagnostic-snapshot-isolation', 'successful-compile-and-analysis-warnings'] };
}
