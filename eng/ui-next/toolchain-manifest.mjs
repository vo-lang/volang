import {resolve} from 'node:path';

export const toolchainSchema = 'volang.ui-toolchain.v2';
export const toolchainPathNames = ['compiler','ui','license','host','vm','plot','editor','cli','testing','testModule','testCLI'];
export function decodeToolchainManifest(bytes) {
  if (bytes.length > 8 * 1024 * 1024) throw new Error('UI toolchain manifest exceeds 8 MiB.');
  return JSON.parse(new TextDecoder('utf-8',{fatal:true}).decode(bytes));
}
export function portablePath(path) {
  return typeof path === 'string' && path.length > 0 && path.length <= 1024
    && !path.includes('\\') && !path.includes(':') && !/[\u0000-\u001f\u007f]/.test(path)
    && path.split('/').every(part => part && part !== '.' && part !== '..');
}
export function resolveToolchain(value,root) {
  if (!value || value.schema !== toolchainSchema || value.platform !== process.platform || value.arch !== process.arch
      || value.nodeMajor !== 24 || Number(process.versions.node.split('.')[0]) < value.nodeMajor
      || !Number.isInteger(value.wireVersion) || value.wireVersion < 1 || !Number.isInteger(value.serverProtocol) || value.serverProtocol < 1
      || !value.paths || Object.keys(value.paths).some(name => ![...toolchainPathNames,'desktop'].includes(name))
      || (value.paths.desktop !== undefined && !portablePath(value.paths.desktop))
      || toolchainPathNames.some(name => !portablePath(value.paths[name]))) throw new Error('Invalid or incompatible UI toolchain manifest. Use a matching complete installation with Node.js 24 or newer.');
  return {kind:'packaged',root,...Object.fromEntries(Object.entries(value.paths).map(([name,path]) => [name,resolve(root,path)]))};
}
