#!/usr/bin/env node
// WASM test runner for vo test cases
// Usage: node test_runner.mjs --plan <plan.json> [--format text|json] [--jobs 1..8]

import { readFileSync, existsSync } from "fs";
import { join, dirname, resolve } from "path";
import { fileURLToPath } from "url";
import { isMainThread, parentPort, workerData } from "node:worker_threads";
import { defaultWorkers, executeWorker, mapBounded, parseWorkers } from "./test_runner_pool.mjs";

import { repositorySourcePath } from "./test_runner_paths.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));

// Import wasm-pack generated module (ES module for --target web)
import init, { compileAndRun } from "./pkg/vo_web.js";
const voWeb = { compileAndRun };

const REPO_ROOT = resolve(__dirname, "../../..");
const TEST_DIR = join(__dirname, "../../test_data");

// Colors
const GREEN = "\x1b[32m";
const RED = "\x1b[31m";
const NC = "\x1b[0m";

function patternsForExpect(expect) {
  if (Array.isArray(expect?.patterns) && expect.patterns.length > 0) {
    return expect.patterns;
  }
  if (typeof expect?.pattern === "string" && expect.pattern.length > 0) {
    return [expect.pattern];
  }
  return [];
}

function patternMatches(message, pattern) {
  const trimmed = pattern.trim();
  if (trimmed.length === 0) {
    return true;
  }
  let index = 0;
  for (const part of trimmed.split("X").filter((segment) => segment.length > 0)) {
    const pos = message.slice(index).indexOf(part);
    if (pos === -1) {
      return false;
    }
    index += pos + part.length;
  }
  return true;
}

function patternsMatch(message, patterns) {
  if (patterns.length === 0) {
    return false;
  }
  return patterns.every((pattern) => patternMatches(message, pattern));
}

function resolveTestPath(file) {
  const repositoryPath = resolve(REPO_ROOT, file);
  return existsSync(repositoryPath) ? repositoryPath : resolve(TEST_DIR, file);
}

function jobTimeoutSeconds(job) {
  const parsed = Number(job.timeout_sec);
  if (!Number.isFinite(parsed) || parsed < 1) {
    return 1;
  }
  return Math.trunc(parsed);
}

async function withJobEnv(job, run) {
  const env =
    job.env && typeof job.env === "object" && !Array.isArray(job.env) ? job.env : {};
  const saved = new Map();
  for (const [key, value] of Object.entries(env)) {
    saved.set(
      key,
      Object.prototype.hasOwnProperty.call(process.env, key) ? process.env[key] : undefined,
    );
    process.env[key] = String(value);
  }
  try {
    return await run();
  } finally {
    for (const [key, value] of saved.entries()) {
      if (value === undefined) {
        delete process.env[key];
      } else {
        process.env[key] = value;
      }
    }
  }
}

async function compileAndRunJob(job, source, relPath) {
  return withJobEnv(job, () => voWeb.compileAndRun(source, relPath));
}

function jsonJob(job, status, elapsedMs, stdout, stderr, error) {
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
    expect: job.expect ?? { kind: "pass" },
    status,
    elapsed_ms: elapsedMs,
    stdout: stdout ?? "",
    stderr: stderr ?? "",
    error: error ?? "",
    skip_reason: null,
    failure_reason: status === "failed" && error ? error : null,
    baseline: null,
    artifacts: [],
  };
}

async function runPlanJob(job, format) {
  const start = Date.now();
  const emitText = format === "text";
  if (job.kind !== "file") {
    const message = `unsupported case kind ${job.kind}`;
    if (emitText) {
      console.log(`  ${RED}✗${NC} ${job.id} [wasm] ${message}`);
    }
    return jsonJob(job, "failed", Date.now() - start, "", "", message);
  }
  const fullPath = resolveTestPath(job.path);
  if (!existsSync(fullPath)) {
    const message = `file not found: ${job.path}`;
    if (emitText) {
      console.log(`  ${RED}✗${NC} ${job.id} [wasm] ${message}`);
    }
    return jsonJob(job, "failed", Date.now() - start, "", "", message);
  }

  const source = readFileSync(fullPath, "utf-8");
  let relPath;
  try {
    relPath = repositorySourcePath(REPO_ROOT, fullPath);
  } catch (error) {
    const message = error.message || String(error);
    if (emitText) {
      console.log(`  ${RED}✗${NC} ${job.id} [wasm] ${message}`);
    }
    return jsonJob(job, "failed", Date.now() - start, "", "", message);
  }
  const expectKind = job.expect?.kind ?? "pass";

  try {
    const result = await compileAndRunJob(job, source, relPath);
    if (expectKind === "fail") {
      const message = `${result.stderr ?? ""}\n${result.stdout ?? ""}`;
      const patterns = patternsForExpect(job.expect);
      if (result.status === "compile_error" && patternsMatch(message, patterns)) {
        if (emitText) {
          console.log(`  ${GREEN}✓${NC} ${relPath} [wasm compile-fail]`);
        }
        return jsonJob(
          job,
          "passed",
          Date.now() - start,
          result.stdout ?? "",
          result.stderr ?? "",
          "",
        );
      }
      const error = message.trim();
      if (emitText) {
        console.log(`  ${RED}✗${NC} ${relPath} [wasm compile-fail] ${error.slice(0, 80)}`);
      }
      return jsonJob(
        job,
        "failed",
        Date.now() - start,
        result.stdout ?? "",
        result.stderr ?? "",
        error,
      );
    }

    if (result.status === "ok") {
      if (emitText) {
        console.log(`  ${GREEN}✓${NC} ${relPath} [wasm]`);
      }
      return jsonJob(
        job,
        "passed",
        Date.now() - start,
        result.stdout ?? "",
        result.stderr ?? "",
        "",
      );
    }
    const error = result.stderr ?? "";
    if (emitText) {
      console.log(`  ${RED}✗${NC} ${relPath} [wasm] ${error.slice(0, 80)}`);
    }
    return jsonJob(
      job,
      "failed",
      Date.now() - start,
      result.stdout ?? "",
      result.stderr ?? "",
      error,
    );
  } catch (e) {
    const error = e.message || String(e);
    if (emitText) {
      console.log(`  ${RED}✗${NC} ${relPath} [wasm] ${error.slice(0, 80)}`);
    }
    return jsonJob(job, "failed", Date.now() - start, "", "", error);
  }
}

function loadPlan(args) {
  let planPath;
  let format = "text";
  let workers = defaultWorkers;
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "--plan") {
      i++;
      planPath = args[i];
    } else if (arg.startsWith("--plan=")) {
      planPath = arg.slice("--plan=".length);
    } else if (arg === "--format") {
      i++;
      format = args[i] ?? "";
    } else if (arg.startsWith("--format=")) {
      format = arg.slice("--format=".length);
    } else if (arg === "--jobs") {
      workers = parseWorkers(args[++i]);
    } else if (arg.startsWith("--jobs=")) {
      workers = parseWorkers(arg.slice(7));
    } else {
      console.error(`unknown argument: ${arg}`);
      process.exit(2);
    }
  }
  if (!planPath) {
    console.error("Usage: node test_runner.mjs --plan <plan.json> [--format text|json]");
    process.exit(2);
  }
  if (format !== "text" && format !== "json") {
    console.error("--format must be text or json");
    process.exit(2);
  }
  const plan = JSON.parse(readFileSync(planPath, "utf-8"));
  if (plan.schema !== "volang.test-plan.v1") {
    console.error(`Unsupported test plan schema: ${plan.schema}`);
    process.exit(2);
  }
  return { plan, format, workers };
}

async function main() {
  const args = process.argv.slice(2);
  const { plan, format, workers } = loadPlan(args);
  if (!Array.isArray(plan.jobs) || plan.jobs.length === 0) {
    console.error("WASM test plan contains no jobs");
    process.exit(2);
  }
  if (format === "text") {
    console.log(`Running ${plan.suite ?? "selected"} WASM tests...\n`);
  }
  const wasmPath = join(__dirname, "pkg", "vo_web_bg.wasm");
  const module = await WebAssembly.compile(readFileSync(wasmPath));
  const jobs = await mapBounded(plan.jobs, workers, async job => {
    const started = Date.now();
    let result;
    // Reject unsupported or missing inputs before allocating a Wasm instance.
    if (job.kind !== 'file' || !existsSync(resolveTestPath(job.path))) return runPlanJob(job, format);
    try {
      const output = await executeWorker(new URL(import.meta.url), { job, module }, jobTimeoutSeconds(job) * 1000);
      result = output.value;
      if (result?.id !== job.id || result?.case_id !== job.case_id || result?.kind !== job.kind
          || result?.path !== job.path || result?.backend !== job.backend || result?.target !== job.target
          || !['passed', 'failed'].includes(result?.status)
          || !['stdout', 'stderr', 'error'].every(key => typeof result[key] === 'string')) {
        throw new Error('worker result identity or status differs from its job');
      }
      if (output.stdout || output.stderr) result.stderr += output.stdout + output.stderr;
    } catch (error) {
      result = jsonJob(job, 'failed', Date.now() - started,
        error.workerStdout || '', error.workerStderr || '', error.message || String(error));
    }
    if (format === 'text') {
      console.log(`  ${result.status === 'passed' ? GREEN + '✓' : RED + '✗'}${NC} ${job.id} [wasm] ${result.error}`.trimEnd());
    }
    return result;
  });
  const passed = jobs.filter((job) => job.status === "passed").length;
  const failed = jobs.filter((job) => job.status === "failed").length;

  if (format === "json") {
    console.log(
      JSON.stringify(
        {
          schema: "volang.test-result.v1",
          suite: plan.suite ?? "lang",
          passed,
          failed,
          skipped: 0,
          jobs,
        },
        null,
        2,
      ),
    );
  } else if (failed > 0) {
    console.log("\nFailures:");
    for (const failure of jobs.filter((job) => job.status === "failed")) {
      console.log(`  ✗ ${failure.id} ${failure.error}`.trimEnd());
    }
    console.log(`\n${passed} passed, ${failed} failed`);
  } else {
    console.log(`\n${passed} passed, ${failed} failed`);
  }

  if (failed > 0) {
    // Let Node drain the full result to a pipe before terminating. Large failed
    // matrices otherwise lose the JSON tail when process.exit closes stdout.
    process.exitCode = 1;
  }
}

if (isMainThread) {
  main().catch(error => { console.error(error); process.exitCode = 1; });
} else {
  await init({ module_or_path: workerData.module });
  parentPort.postMessage(await runPlanJob(workerData.job, 'json'));
}
