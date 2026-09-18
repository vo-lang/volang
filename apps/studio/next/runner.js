// A fresh worker owns compilation and execution for one run. Termination also
// stops synchronous infinite loops and releases the Wasm heap.
self.onmessage = async event => {
  self.onmessage = null;
  let diagnostics;
  try {
    const { default: init, compile, run } = await import('/compiler/vo_web.js');
    await init();
    self.postMessage({kind:'phase', phase:'compiling'});
    const compiled = compile(event.data.source, 'main.vo');
    let bytes;
    try {
      diagnostics = compiled.diagnosticsJson ? JSON.parse(compiled.diagnosticsJson) : undefined;
      if (!compiled.success) {
        self.postMessage({kind:'result',output:compiled.errorMessage || 'Could not compile this program.', diagnostics});
        return;
      }
      bytes = compiled.bytecode;
    } finally { compiled.free(); }
    self.postMessage({kind:'phase', phase:'running'});
    const result = run(bytes);
    try {
      if (result.status !== 'ok') throw new Error(result.stderr || `Program ended: ${result.status}`);
      self.postMessage({kind:'result', output: result.stdout || 'Program finished with no output.', diagnostics });
    } finally { result.free(); }
  } catch (error) { self.postMessage({kind:'result', output: String(error?.message ?? error), diagnostics }); }
};
