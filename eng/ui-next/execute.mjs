import {spawn} from 'node:child_process';
import {open} from 'node:fs/promises';
import {toolchain} from './toolchain.mjs';

// Human diagnostics retain a bounded tail. Structured stdout can instead go
// directly to a file, preserving every record without buffering it in memory.
export async function execute(executable, args, { cwd = toolchain.root, signal, env = process.env, stdoutFile } = {}) {
  const file = stdoutFile === undefined ? undefined : await open(stdoutFile, 'w');
  try {
    return await new Promise((resolveRun, reject) => {
      const child = spawn(executable, args, { cwd, env, signal, stdio: ['ignore', file?.fd ?? 'pipe', 'pipe'] });
      let output = '', failure;
      const append = bytes => { output = (output + bytes).slice(-65_536); };
      child.stdout?.on('data', append);
      child.stderr.on('data', append);
      // Cancellation emits error before the process exits. Wait for close before
      // a caller removes its staging directory or starts another compilation.
      child.once('error', error => { failure ??= signal?.aborted ? signal.reason : error; });
      child.once('close', code => {
        if (failure) reject(failure);
        else if (code === 0) resolveRun(output);
        else reject(new Error((output || `${executable} exited with status ${code}`)
          + (stdoutFile === undefined ? '' : `\nFull stdout: ${stdoutFile}`)));
      });
    });
  } finally { await file?.close(); }
}
