#!/usr/bin/env node
// WASM test runner for vo test cases
// Usage: node test_runner.mjs --plan <plan.json>

import { readFileSync, existsSync } from "fs";
import { join, relative, dirname, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));

// Import wasm-pack generated module (ES module for --target web)
import init, { compileAndRun } from "./pkg/vo_web.js";
const voWeb = { compileAndRun };

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
  return existsSync(file) ? resolve(file) : join(TEST_DIR, file);
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

async function runPlanJob(job) {
  if (job.kind !== "file") {
    const message = `unsupported case kind ${job.kind}`;
    console.log(`  ${RED}✗${NC} ${job.id} [wasm] ${message}`);
    return { passed: false, label: job.id, error: message };
  }
  const fullPath = resolveTestPath(job.path);
  if (!existsSync(fullPath)) {
    const message = `file not found: ${job.path}`;
    console.log(`  ${RED}✗${NC} ${job.id} [wasm] ${message}`);
    return { passed: false, label: job.id, error: message };
  }

  const source = readFileSync(fullPath, "utf-8");
  const relPath = relative(TEST_DIR, fullPath);
  const expectKind = job.expect?.kind ?? "pass";

  try {
    const result = await compileAndRunJob(job, source, relPath);
    if (expectKind === "fail") {
      const message = `${result.stderr ?? ""}\n${result.stdout ?? ""}`;
      const patterns = patternsForExpect(job.expect);
      if (result.status === "compile_error" && patternsMatch(message, patterns)) {
        console.log(`  ${GREEN}✓${NC} ${relPath} [wasm compile-fail]`);
        return { passed: true, label: relPath, error: "" };
      }
      const error = message.trim();
      console.log(`  ${RED}✗${NC} ${relPath} [wasm compile-fail] ${error.slice(0, 80)}`);
      return { passed: false, label: relPath, error };
    }

    if (result.status === "ok") {
      console.log(`  ${GREEN}✓${NC} ${relPath} [wasm]`);
      return { passed: true, label: relPath, error: "" };
    }
    const error = result.stderr ?? "";
    console.log(`  ${RED}✗${NC} ${relPath} [wasm] ${error.slice(0, 80)}`);
    return { passed: false, label: relPath, error };
  } catch (e) {
    const error = e.message || String(e);
    console.log(`  ${RED}✗${NC} ${relPath} [wasm] ${error.slice(0, 80)}`);
    return { passed: false, label: relPath, error };
  }
}

function loadPlan(args) {
  if (args.length !== 2 || args[0] !== "--plan") {
    console.error("Usage: node test_runner.mjs --plan <plan.json>");
    process.exit(2);
  }
  const planPath = args[1];
  if (!planPath) {
    console.error("--plan requires a path");
    process.exit(2);
  }
  const plan = JSON.parse(readFileSync(planPath, "utf-8"));
  if (plan.schema !== "volang.test-plan.v1") {
    console.error(`Unsupported test plan schema: ${plan.schema}`);
    process.exit(2);
  }
  return plan;
}

async function main() {
  // Initialize WASM (required for --target web)
  // Node.js fetch doesn't support file:// URLs, so we read the WASM file manually
  const wasmPath = join(__dirname, "pkg", "vo_web_bg.wasm");
  const wasmBytes = readFileSync(wasmPath);
  await init({ module_or_path: wasmBytes });

  const args = process.argv.slice(2);
  const plan = loadPlan(args);
  if (!Array.isArray(plan.jobs) || plan.jobs.length === 0) {
    console.error("WASM test plan contains no jobs");
    process.exit(2);
  }
  console.log(`Running ${plan.suite ?? "selected"} WASM tests...\n`);
  let passed = 0;
  let failed = 0;
  const failures = [];
  for (const job of plan.jobs) {
    const result = await runPlanJob(job);
    if (result.passed) {
      passed++;
    } else {
      failed++;
      failures.push(result);
    }
  }

  if (failures.length > 0) {
    console.log("\nFailures:");
    for (const failure of failures) {
      console.log(`  ✗ ${failure.label} ${failure.error}`.trimEnd());
    }
  }
  console.log(`\n${passed} passed, ${failed} failed`);

  if (failed > 0) {
    process.exit(1);
  }
}

main();
