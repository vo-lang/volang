import { vfs, VirtualFS, registerVFSBindings } from './vfs.js';
export { vfs, VirtualFS, registerVFSBindings };

let wasmModule: typeof import('../pkg/vo_web.js') | null = null;

/**
 * Initialize only the VFS layer (MemoryFS + OPFS + JavaScript-global bindings).
 * Use this when you load your own WASM (e.g. a custom build with extra externs).
 * Must be called before running any Vo program.
 */
export async function initVFS(): Promise<void> {
  await vfs.init();
  registerVFSBindings();
}

/**
 * Initialize vo-web runtime (VFS + built-in vo-web WASM).
 * Must be called before using any other vo-web functions.
 */
export async function init(runtime?: import('../pkg/vo_web.js').InitInput): Promise<void> {
  await initVFS();

  const wasm = await import('../pkg/vo_web.js');
  await wasm.default(runtime === undefined ? undefined : { module_or_path: runtime });
  wasmModule = wasm;
}

/**
 * Compile Vo source code to bytecode.
 */
export function compile(source: string, filename?: string) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.compile(source, filename);
}

/** Compile a complete project stored in the browser VFS. */
export function compileProject(
  entry: string,
  projectRoot: string,
  modRoot = '',
  overlayPath?: string,
  overlayText?: string,
) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.compileProject(entry, projectRoot, modRoot, overlayPath, overlayText);
}

/** Analyze one project package without requiring an executable entry point. */
export function analyzeProject(
  entry: string,
  projectRoot: string,
  modRoot = '',
  overlayPath?: string,
  overlayText?: string,
) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.analyzeProject(entry, projectRoot, modRoot, overlayPath, overlayText);
}

/** Install locked registry modules and compile a complete browser-VFS project. */
export function compileProjectAutoInstall(
  entry: string,
  projectRoot: string,
  overlayPath?: string,
  overlayText?: string,
) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.compileProjectAutoInstall(entry, projectRoot, overlayPath, overlayText);
}

/** Install locked registry modules and analyze one browser-VFS package. */
export function analyzeProjectAutoInstall(
  entry: string,
  projectRoot: string,
  overlayPath?: string,
  overlayText?: string,
) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.analyzeProjectAutoInstall(entry, projectRoot, overlayPath, overlayText);
}

/** Create a frozen workspace-only lock for packaged browser modules. */
export function prepareWorkspaceLock(rootMod: string, workspaceModules: readonly string[]): string {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.prepareWorkspaceLock(rootMod, Array.from(workspaceModules));
}

/**
 * Run bytecode.
 */
export function run(bytecode: Uint8Array) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.run(bytecode);
}

/** Run bytecode with explicit process arguments. */
export function runWithArgs(bytecode: Uint8Array, args: readonly string[] = []) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.runWithArgs(bytecode, Array.from(args));
}

/** Create a persistent VM/JIT development Island for an already compiled app. */
export function createVmIsland(bytecode: Uint8Array) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return new wasmModule.VoVmIsland(bytecode);
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
 * Await a durable VFS checkpoint in OPFS. Rejects when browser persistence
 * fails; resolves immediately on memory-only hosts. Vo `File.Sync` covers
 * immediate visibility inside the synchronous in-memory VFS.
 */
export async function flushVFS(): Promise<void> {
  await vfs.forceFlush();
}
