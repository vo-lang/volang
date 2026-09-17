const generatedDirectories = new Set(['target','.volang','.vo-cache','.git','node_modules']);

// Compiler caches may sit beneath each independent entry in an ordinary project.
// Their writes are build output and must never escalate source HMR to a reload.
export function developmentPath(filename) {
  if (!filename) return undefined;
  const path = String(filename).replaceAll('\\','/');
  return path.split('/').some(part => generatedDirectories.has(part)) ? undefined : path;
}
