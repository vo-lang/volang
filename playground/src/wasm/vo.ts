// Vo WASM runtime wrapper

export type RunStatus = 'idle' | 'running' | 'success' | 'error';

export interface RunResult {
  status: 'ok' | 'error' | 'compile_error';
  stdout: string;
  stderr: string;
}

// WASM module instance
let wasmModule: any = null;

async function loadWasm(): Promise<any> {
  if (wasmModule) return wasmModule;

  try {
    const { default: init, compileAndRun, version } = await import('@vo-web/vo_web.js');
    await init();
    wasmModule = { compileAndRun, version };
    console.log('Vo WASM loaded:', version());
    return wasmModule;
  } catch (e) {
    console.error('Failed to load Vo WASM:', e);
    throw new Error('Failed to load Vo runtime. Please refresh the page.');
  }
}

export async function runCode(source: string): Promise<RunResult> {
  const wasm = await loadWasm();

  const result = wasm.compileAndRun(source, 'main.vo');

  return {
    status: result.status,
    stdout: result.stdout || '',
    stderr: result.stderr || '',
  };
}

export async function getVersion(): Promise<string> {
  const wasm = await loadWasm();
  return wasm.version();
}
