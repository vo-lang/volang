// A fresh worker owns each case's Wasm instance, VM, VFS and environment.
// The compiled module is shared through structured cloning; mutable state is not.
import { Worker } from 'node:worker_threads';
import { availableParallelism } from 'node:os';

export const defaultWorkers = Math.min(4, availableParallelism());
const maxLogBytes = 8 * 1024 * 1024;

export function parseWorkers(value) {
  const count = Number(value);
  if (!Number.isSafeInteger(count) || count < 1 || count > 8) {
    throw new Error('--jobs must be an integer between 1 and 8');
  }
  return count;
}

export async function mapBounded(jobs, workers, run) {
  parseWorkers(workers);
  let next = 0;
  const results = new Array(jobs.length);
  await Promise.all(Array.from({ length: Math.min(workers, jobs.length) }, async () => {
    while (next < jobs.length) {
      const index = next++;
      results[index] = await run(jobs[index], index);
    }
  }));
  return results;
}

export function executeWorker(workerURL, workerData, timeoutMilliseconds) {
  return new Promise((resolve, reject) => {
    const worker = new Worker(workerURL, {
      workerData,
      stdout: true,
      stderr: true,
      // Do not use SHARE_ENV: each worker receives its own environment copy.
    });
    let value;
    let messages = 0;
    let error;
    let stopping = false;
    let stdout = '';
    let stderr = '';
    let logBytes = 0;
    const stop = reason => {
      error ??= reason;
      if (stopping) return;
      stopping = true;
      // Resolve only at exit, after the timed-out instance has been destroyed.
      void worker.terminate().catch(terminationError => { error ??= terminationError; });
    };
    const timer = setTimeout(() => stop(new Error(`timed out after ${timeoutMilliseconds / 1000}s`)), timeoutMilliseconds);
    const capture = (stream, kind) => {
      stream.setEncoding('utf8');
      stream.on('data', chunk => {
        logBytes += Buffer.byteLength(chunk);
        if (logBytes > maxLogBytes) {
          stop(new Error(`worker diagnostic output exceeded ${maxLogBytes} bytes`));
        } else if (kind === 'stdout') stdout += chunk;
        else stderr += chunk;
      });
    };
    capture(worker.stdout, 'stdout');
    capture(worker.stderr, 'stderr');
    worker.on('message', message => {
      messages++;
      if (messages !== 1) stop(new Error('worker produced duplicate results'));
      else value = message;
    });
    worker.on('error', workerError => { error ??= workerError; });
    worker.on('exit', code => {
      clearTimeout(timer);
      if (!error && code !== 0) error = new Error(`worker exited with status ${code}`);
      if (!error && messages !== 1) error = new Error('worker exited without a result');
      if (error) reject(Object.assign(error, { workerStdout: stdout, workerStderr: stderr }));
      else resolve({ value, stdout, stderr });
    });
  });
}
