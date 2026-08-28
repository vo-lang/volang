#!/usr/bin/env node

import { existsSync, readFileSync } from 'node:fs';
import { availableParallelism } from 'node:os';
import { dirname, resolve } from 'node:path';
import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, '../../..');
const worker = resolve(scriptDirectory, 'aot_test_worker.mjs');
const green = '\x1b[32m';
const red = '\x1b[31m';
const reset = '\x1b[0m';

function loadArguments(args) {
  let planPath;
  let format = 'text';
  for (let index = 0; index < args.length; index += 1) {
    const argument = args[index];
    if (argument === '--plan') planPath = args[++index];
    else if (argument.startsWith('--plan=')) planPath = argument.slice(7);
    else if (argument === '--format') format = args[++index] ?? '';
    else if (argument.startsWith('--format=')) format = argument.slice(9);
    else throw new Error(`unknown argument: ${argument}`);
  }
  if (!planPath) throw new Error('usage: aot_test_runner.mjs --plan PLAN [--format text|json]');
  if (format !== 'text' && format !== 'json') throw new Error('--format must be text or json');
  const plan = JSON.parse(readFileSync(planPath, 'utf8'));
  if (plan.schema !== 'volang.test-plan.v1' || !Array.isArray(plan.jobs)) {
    throw new Error('unsupported Volang test plan');
  }
  return { plan, format };
}

function patternMatches(message, pattern) {
  let position = 0;
  for (const part of pattern.trim().split('X').filter(Boolean)) {
    const found = message.slice(position).indexOf(part);
    if (found < 0) return false;
    position += found + part.length;
  }
  return true;
}

function expectedPatterns(job) {
  if (Array.isArray(job.expect?.patterns)) return job.expect.patterns;
  return typeof job.expect?.pattern === 'string' ? [job.expect.pattern] : [];
}

function jsonJob(job, status, elapsedMs, result, error = '') {
  return {
    id: job.id,
    case_id: job.case_id,
    kind: job.kind,
    path: job.path,
    target: job.target,
    backend: job.backend,
    matrix: job.matrix ?? null,
    tags: Array.isArray(job.tags) ? job.tags : [],
    owner: job.owner ?? null,
    expect: job.expect ?? { kind: 'pass' },
    status,
    elapsed_ms: elapsedMs,
    stdout: result?.stdout ?? '',
    stderr: result?.stderr ?? '',
    error,
    skip_reason: null,
    failure_reason: status === 'failed' ? error : null,
    baseline: null,
    artifacts: [],
  };
}

function executeWorker(job) {
  const timeoutSeconds = Math.max(1, Math.trunc(Number(job.timeout_sec) || 1));
  return new Promise((resolvePromise, reject) => {
    const child = spawn(process.execPath, [worker], {
      cwd: repositoryRoot,
      env: process.env,
      stdio: ['pipe', 'pipe', 'pipe'],
    });
    let stdout = '';
    let stderr = '';
    let settled = false;
    const finish = (action) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      action();
    };
    const timer = setTimeout(() => {
      child.kill('SIGKILL');
      finish(() => reject(new Error(`timed out after ${timeoutSeconds}s`)));
    }, timeoutSeconds * 1000);
    child.stdout.setEncoding('utf8');
    child.stderr.setEncoding('utf8');
    child.stdout.on('data', (chunk) => { stdout += chunk; });
    child.stderr.on('data', (chunk) => { stderr += chunk; });
    child.on('error', (error) => finish(() => reject(error)));
    child.on('close', (code) => finish(() => {
      if (code !== 0) {
        reject(new Error(stderr.trim() || `AOT test worker exited with status ${code}`));
        return;
      }
      try {
        resolvePromise(JSON.parse(stdout));
      } catch (error) {
        reject(new Error(`AOT test worker emitted invalid JSON: ${error.message}`));
      }
    }));
    child.stdin.end(JSON.stringify({ path: job.path }));
  });
}

async function runJob(job, format) {
  const started = Date.now();
  const source = resolve(repositoryRoot, job.path);
  let result;
  let failure = '';
  if (job.kind !== 'file') failure = `unsupported AOT case kind ${job.kind}`;
  else if (!existsSync(source)) failure = `file not found: ${job.path}`;
  try {
    if (!failure) result = await executeWorker(job);
    const expectFailure = job.expect?.kind === 'fail';
    if (!failure && expectFailure) {
      const diagnostic = `${result.stderr ?? ''}\n${result.stdout ?? ''}`;
      const patterns = expectedPatterns(job);
      if (result.phase !== 'compile') failure = 'program compiled successfully';
      else if (patterns.length === 0 || !patterns.every((item) => patternMatches(diagnostic, item))) {
        failure = `compile diagnostic did not match: ${diagnostic.trim()}`;
      }
    } else if (!failure && result.phase === 'compile') {
      failure = `AOT compilation failed: ${(result.stderr || result.stdout).trim()}`;
    } else if (!failure && result.status !== 'ok') {
      failure = result.stderr || `AOT program exited with status ${result.exitCode}`;
    }
  } catch (error) {
    failure = error?.message ?? String(error);
  }
  const status = failure ? 'failed' : 'passed';
  if (format === 'text') {
    const marker = status === 'passed' ? `${green}✓${reset}` : `${red}✗${reset}`;
    const detail = failure ? ` ${failure.split('\n')[0]}` : '';
    console.log(`  ${marker} ${job.path} [wasm-aot]${detail}`);
  }
  return jsonJob(job, status, Date.now() - started, result, failure);
}

async function main() {
  const { plan, format } = loadArguments(process.argv.slice(2));
  if (plan.jobs.length === 0) throw new Error('AOT test plan contains no jobs');
  const configuredJobs = Number(process.env.VO_TEST_JOBS ?? '');
  const concurrency = Number.isSafeInteger(configuredJobs) && configuredJobs > 0
    ? Math.min(configuredJobs, 32)
    : Math.min(4, availableParallelism());
  if (format === 'text') {
    console.log(
      `Running ${plan.suite ?? 'selected'} Core Wasm AOT tests (${concurrency} workers)...\n`,
    );
  }
  const jobs = new Array(plan.jobs.length);
  let nextJob = 0;
  const worker = async () => {
    while (nextJob < plan.jobs.length) {
      const index = nextJob;
      nextJob += 1;
      jobs[index] = await runJob(plan.jobs[index], format);
    }
  };
  await Promise.all(Array.from(
    { length: Math.min(concurrency, plan.jobs.length) },
    () => worker(),
  ));
  const passed = jobs.filter((job) => job.status === 'passed').length;
  const failed = jobs.length - passed;
  if (format === 'json') {
    console.log(JSON.stringify({
      schema: 'volang.test-result.v1',
      suite: plan.suite ?? 'lang',
      passed,
      failed,
      skipped: 0,
      jobs,
    }, null, 2));
  } else {
    console.log(`\n${passed} passed, ${failed} failed`);
  }
  if (failed !== 0) process.exit(1);
}

main().catch((error) => {
  console.error(error?.stack ?? error);
  process.exit(2);
});
