#!/usr/bin/env node
import { spawn } from 'node:child_process';
import { readFile, writeFile, mkdir, open, rename } from 'node:fs/promises';
import { resolve, dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { checkDependencyPolicy } from './check-dependency-policy.mjs';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const output = resolve(process.argv[2] ?? join(root, 'target/ci/results/dependencies.json'));
const attempt = join(root, 'target/ci/dependency-audit', `${Date.now()}-${process.pid}`);
await mkdir(attempt, { recursive: true });
await mkdir(dirname(output), { recursive: true });
// Invalidate previous success even if the process is interrupted during an audit.
await writeFile(output, JSON.stringify({ schema: 'volang.dependency-result.v1', passed: false, complete: false, attempt }) + '\n');
const policy = JSON.parse(await readFile(new URL('./dependency-policy.json', import.meta.url), 'utf8'));
const reports = {};
const executions = [];
const failures = [];
const tasks = [
  ...policy.rust_lockfiles.map(path => ({ path, cwd: root, command: process.env.CARGO_AUDIT ?? 'cargo-audit',
    args: ['audit', '--json', '--file', path] })),
  ...policy.npm_workspaces.map(path => ({ path, cwd: join(root, path), command: 'npm', args: ['audit', '--json', '--audit-level=high'] })),
];
for (const [index, task] of tasks.entries()) {
  const stdout = join(attempt, `${index}.json`);
  const stderr = join(attempt, `${index}.stderr.log`);
  const out = await open(stdout, 'w'); const err = await open(stderr, 'w');
  const started = Date.now();
  const execution = { input: task.path, command: [task.command, ...task.args], stdout, stderr };
  try {
    const child = spawn(task.command, task.args, { cwd: task.cwd, stdio: ['ignore', out.fd, err.fd], timeout: 300_000 });
    const status = await new Promise((done, reject) => { child.once('error', reject); child.once('exit', (code, signal) => done({ code, signal })); });
    Object.assign(execution, status);
    reports[task.path] = JSON.parse(await readFile(stdout, 'utf8'));
    const report = reports[task.path];
    const expectedCode = policy.rust_lockfiles.includes(task.path)
      ? (report.vulnerabilities?.found === true ? 1 : 0)
      : (report.metadata?.vulnerabilities?.high > 0 || report.metadata?.vulnerabilities?.critical > 0 ? 1 : 0);
    if (status.signal || status.code !== expectedCode) failures.push(`${task.path}: audit exit disagrees with findings (${JSON.stringify(status)})`);
  } catch (error) {
    execution.error = String(error); failures.push(`${task.path}: ${error}`);
  } finally {
    execution.duration_ms = Date.now() - started; executions.push(execution);
    await out.close(); await err.close();
  }
}
let result;
try { result = checkDependencyPolicy(policy, reports); }
catch (error) { failures.push(String(error)); result = { schema: 'volang.dependency-result.v1', passed: false, complete: false, failures: [] }; }
result.failures.push(...failures);
Object.assign(result, { passed: result.passed && failures.length === 0, executions, attempt });
const temporary = `${output}.${process.pid}.tmp`;
await writeFile(temporary, JSON.stringify(result, null, 2) + '\n');
await rename(temporary, output);
console.log(`Audited ${executions.length} dependency inputs; ${result.accepted_warnings?.length ?? 0} reviewed warnings; ${result.failures.length} failures. Report: ${output}`);
if (!result.passed) { console.error(result.failures.join('\n')); process.exitCode = 1; }
