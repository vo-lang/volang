import { runWorkerVm } from '/host/ui_next/worker-vm.js';
import {prepareUiCompiler,writeCompilerFile} from './compiler-workspace.js';

// One run owns one memory-only compiler filesystem and VM. All project metadata
// comes from the packaged workspace, using the same lock validation as the CLI.
self.onmessage = async event => {
  self.onmessage = null;
  let diagnostics;
  try {
    const source = event.data.source;
    if (typeof source !== 'string' || source.length > 100000) throw new Error('Please keep this example under 100,000 characters.');
    const [runtime, filesystem] = await Promise.all([
      import('/compiler/vo_web.js'), import('/host/vfs.js'),
    ]);
    await runtime.default();
    await filesystem.vfs.init();
    filesystem.registerVFSBindings();
    const root = '/preview';
    await prepareUiCompiler(runtime,filesystem,root);
    writeCompilerFile(filesystem,`${root}/main.vo`,source);
    const compiled = runtime.compileProject('main.vo', root, '');
    let bytes;
    try {
      diagnostics = compiled.diagnosticsJson ? JSON.parse(compiled.diagnosticsJson) : undefined;
      if (!compiled.success) {
        throw new Error(compiled.errorMessage || 'Could not compile this example.');
      }
      bytes = compiled.bytecode;
    } finally { compiled.free(); }
    const vm = new runtime.VoVmIsland(bytes);
    let sentDiagnostics = false;
    try {
      await runWorkerVm(vm, {
        // Compilation metadata accompanies the first normal exchange or exit.
        // The generic UI transport keeps its existing message kinds and order.
        postMessage(message, transfer) {
          if (!sentDiagnostics) { message = {...message, diagnostics}; sentDiagnostics = true; }
          self.postMessage(message, transfer);
        },
        addEventListener: self.addEventListener.bind(self),
        removeEventListener: self.removeEventListener.bind(self),
      });
    } finally { vm.free(); }
  } catch (error) { self.postMessage({ kind: 'ui-exit', error: String(error?.message ?? error).slice(0, 65536), diagnostics }); }
};
