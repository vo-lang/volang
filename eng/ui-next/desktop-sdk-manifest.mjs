import {createHash} from 'node:crypto';
import {createReadStream} from 'node:fs';
import {readFile,stat} from 'node:fs/promises';
import {join} from 'node:path';
import {portablePath} from './toolchain-manifest.mjs';

export async function desktopArtifact(directory,path) {
  if (!portablePath(path)) throw new Error('Invalid desktop SDK resource path.');
  const sha = createHash('sha256');
  for await (const bytes of createReadStream(join(directory,path))) sha.update(bytes);
  return {path,bytes:(await stat(join(directory,path))).size,sha256:sha.digest('hex')};
}

export async function readDesktopSdk(directory) {
  const bytes = await readFile(join(directory,'desktop-sdk.json'));
  if (bytes.length > 1024 * 1024) throw new Error('Desktop SDK manifest exceeds 1 MiB.');
  const sdk = JSON.parse(bytes);
  if (sdk.schema !== 'volang.ui-desktop-sdk.v2' || sdk.platform !== process.platform || sdk.arch !== process.arch
      || !['dev','release-native'].includes(sdk.profile) || (!Number.isInteger(sdk.wireVersion) || sdk.wireVersion < 1)
      || !sdk.runner || (sdk.runtime !== null && !sdk.runtime)
      || !Array.isArray(sdk.nativeLink) || sdk.nativeLink.length > 256
      || sdk.nativeLink.some(value => typeof value !== 'string' || !value || value.length > 4096 || /[\x00-\x1f]/.test(value))) {
    throw new Error('Invalid or incompatible desktop SDK. Install the matching platform bundle.');
  }
  for (const resource of [sdk.runner,...(sdk.runtime ? [sdk.runtime] : [])]) {
    if (!portablePath(resource.path) || !Number.isSafeInteger(resource.bytes) || resource.bytes < 1 || !/^[a-f0-9]{64}$/.test(resource.sha256)) {
      throw new Error('Invalid desktop SDK artifact identity.');
    }
    const actual = await desktopArtifact(directory,resource.path);
    if (actual.sha256 !== resource.sha256 || actual.bytes !== resource.bytes) throw new Error(`Desktop SDK artifact mismatch: ${resource.path}`);
  }
  return sdk;
}
