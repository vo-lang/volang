// =============================================================================
// vo-web JavaScript API
// =============================================================================

import { vfs, VirtualFS, registerVFSBindings } from './vfs';

// Re-export
export { vfs, VirtualFS, registerVFSBindings };

// WASM module reference
let wasmModule: typeof import('../pkg/vo_web') | null = null;

/**
 * Initialize vo-web runtime.
 * Must be called before using any other vo-web functions.
 */
export async function init(): Promise<void> {
  // 1. Initialize VFS (load from OPFS)
  await vfs.init();
  registerVFSBindings();

  // 2. Initialize WASM
  const wasm = await import('../pkg/vo_web');
  await wasm.default();
  wasmModule = wasm;
}

/**
 * Compile Vo source code to bytecode.
 */
export function compile(source: string, filename?: string) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.compile(source, filename);
}

/**
 * Run bytecode.
 */
export function run(bytecode: Uint8Array) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.run(bytecode);
}

/**
 * Compile and run in one step.
 */
export function compileAndRun(source: string, filename?: string) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.compileAndRun(source, filename);
}

/**
 * Get version string.
 */
export function version(): string {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.version();
}

/**
 * Force flush VFS to OPFS.
 */
export async function flushVFS(): Promise<void> {
  await vfs.forceFlush();
}
