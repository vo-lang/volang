import {join, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';

export const repositoryRoot = fileURLToPath(new URL('../../../', import.meta.url));

export function compilerPath() {
  if (process.env.VO_TEST_COMPILER) {
    return resolve(repositoryRoot, process.env.VO_TEST_COMPILER);
  }
  const profile = process.env.VO_TEST_PROFILE === 'release' ? 'release' : 'debug';
  const executable = process.platform === 'win32' ? 'vo.exe' : 'vo';
  const targetDirectory = resolve(repositoryRoot, process.env.CARGO_TARGET_DIR || 'target');
  return join(targetDirectory, profile, executable);
}
