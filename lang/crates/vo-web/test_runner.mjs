#!/usr/bin/env node
// WASM test runner for vo test cases
// Usage: node test_runner.mjs [test_file.vo]

import { readFileSync, readdirSync, statSync, existsSync } from "fs";
import { join, relative, dirname } from "path";
import { fileURLToPath } from "url";
import { createRequire } from "module";

const __dirname = dirname(fileURLToPath(import.meta.url));
const require = createRequire(import.meta.url);

// Import wasm-pack generated module (CommonJS)
const voWeb = require("./pkg/vo_web.js");

const TEST_DIR = join(__dirname, "../../test_data");

// Tests to skip in WASM (require std, native externs, GC debug, or edge cases)
const SKIP_TESTS = new Set([
  "gc_basic.vo",
  "gc_closure_capture.vo",
  "os_simple_test.vo",
  "os_test.vo",
  "stdlib/regexp.vo",
  "init_call_error.vo",
  "select_stmt_assert_fail.vo",
  "bug_ptr_recv_named_slice.vo",  // tests compile-time rejection
  "goto_stmt.vo",  // goto edge cases
]);

// Colors
const GREEN = "\x1b[32m";
const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const DIM = "\x1b[2m";
const NC = "\x1b[0m";

function findVoFiles(dir) {
  const files = [];
  
  if (!existsSync(dir)) return files;
  
  const entries = readdirSync(dir);
  for (const entry of entries) {
    const fullPath = join(dir, entry);
    const stat = statSync(fullPath);
    
    if (stat.isDirectory()) {
      // Skip proj_*, zip, typechecker directories
      if (!entry.startsWith("proj_") && !entry.startsWith("typechecker") && !entry.endsWith(".zip")) {
        files.push(...findVoFiles(fullPath));
      }
    } else if (entry.endsWith(".vo")) {
      files.push(fullPath);
    }
  }
  
  return files.sort();
}

async function runTest(filePath) {
  const source = readFileSync(filePath, "utf-8");
  const relPath = relative(TEST_DIR, filePath);
  
  // Skip known unsupported tests
  if (SKIP_TESTS.has(relPath)) {
    console.log(`  ${YELLOW}⊘${NC} ${relPath} [wasm skipped]`);
    return "skip";
  }
  
  try {
    const result = voWeb.compileAndRun(source, relPath);
    
    if (result.status === "ok") {
      console.log(`  ${GREEN}✓${NC} ${relPath} [wasm]`);
      return true;
    } else {
      console.log(`  ${RED}✗${NC} ${relPath} [wasm] ${result.stderr.slice(0, 60)}`);
      return false;
    }
  } catch (e) {
    console.log(`  ${RED}✗${NC} ${relPath} [wasm] ${e.message?.slice(0, 60) || e}`);
    return false;
  }
}

async function main() {
  const args = process.argv.slice(2);
  
  if (args.length > 0) {
    // Run single file
    const file = args[0];
    const fullPath = existsSync(file) ? file : join(TEST_DIR, file);
    if (!existsSync(fullPath)) {
      console.error(`File not found: ${file}`);
      process.exit(1);
    }
    await runTest(fullPath);
    return;
  }
  
  // Run all tests
  console.log(`Running WASM tests...\n`);
  
  const files = findVoFiles(TEST_DIR);
  let passed = 0;
  let failed = 0;
  let skipped = 0;
  
  for (const file of files) {
    const result = await runTest(file);
    if (result === "skip") {
      skipped++;
    } else if (result) {
      passed++;
    } else {
      failed++;
    }
  }
  
  console.log(`\n${passed} passed, ${failed} failed, ${skipped} skipped`);
  
  if (failed > 0) {
    process.exit(1);
  }
}

main();

