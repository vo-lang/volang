import {spawn} from 'node:child_process';
import {toolchain} from './toolchain.mjs';

export function execute(executable, args, { cwd = toolchain.root, signal, env = process.env } = {}) {
  return new Promise((resolveRun, reject) => {
    const child = spawn(executable, args, { cwd, env, signal, stdio: ['ignore', 'pipe', 'pipe'] });
    let output = '', failure;
    const append = bytes => { output = (output + bytes).slice(-65_536); };
    child.stdout.on('data', append);
    child.stderr.on('data', append);
    // Cancellation emits error before the process exits. Wait for close before
    // a caller removes its staging directory or starts another compilation.
    child.once('error', error => { failure ??= signal?.aborted ? signal.reason : error; });
    child.once('close', code => {
      if (failure) reject(failure);
      else if (code === 0) resolveRun(output);
      else reject(new Error(output || `${executable} exited with status ${code}`));
    });
  });
}
