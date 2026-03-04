// =============================================================================
// VfsLike — minimal interface for the JS VirtualFS used by WasmVoHandler.init()
// =============================================================================

export interface VfsLike {
  readDir(path: string): [[string, boolean][], string | null];
  readFile(path: string): [Uint8Array | null, string | null];
  writeFile(path: string, data: Uint8Array, mode: number): string | null;
  mkdirAll(path: string, mode: number): string | null;
  rename(oldPath: string, newPath: string): string | null;
  remove(path: string): string | null;
  removeAll(path: string): string | null;
}
