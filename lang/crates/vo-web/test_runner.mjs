#!/usr/bin/env node
// WASM test runner for vo test cases
// Usage: node test_runner.mjs --plan <plan.json> [--format text|json]

import { readFileSync, existsSync } from "fs";
import { join, dirname, resolve } from "path";
import { fileURLToPath } from "url";

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

async function withJobTimeout(job, run) {
  const seconds = jobTimeoutSeconds(job);
  let timeout;
  try {
    return await Promise.race([
      Promise.resolve().then(run),
      new Promise((_, reject) => {
        timeout = setTimeout(() => {
          reject(new Error(`timed out after ${seconds}s`));
        }, seconds * 1000);
      }),
    ]);
  } finally {
    clearTimeout(timeout);
  }
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
  return withJobEnv(job, () =>
    withJobTimeout(job, () => voWeb.compileAndRun(source, relPath)),
  );
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
  return { plan, format };
}

async function main() {
  // Initialize WASM (required for --target web)
  // Node.js fetch doesn't support file:// URLs, so we read the WASM file manually
  const wasmPath = join(__dirname, "pkg", "vo_web_bg.wasm");
  const wasmBytes = readFileSync(wasmPath);
  await init({ module_or_path: wasmBytes });

  const args = process.argv.slice(2);
  const { plan, format } = loadPlan(args);
  if (!Array.isArray(plan.jobs) || plan.jobs.length === 0) {
    console.error("WASM test plan contains no jobs");
    process.exit(2);
  }
  if (format === "text") {
    console.log(`Running ${plan.suite ?? "selected"} WASM tests...\n`);
  }
  const jobs = [];
  for (const job of plan.jobs) {
    jobs.push(await runPlanJob(job, format));
  }
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
    process.exit(1);
  }
}

main();
