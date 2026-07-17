#!/usr/bin/env node
import { mkdirSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { validateQuickplay } from './quickplay_validate.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));

function option(name, fallback) {
  const index = process.argv.indexOf(name);
  return index === -1 ? fallback : path.resolve(process.argv[index + 1]);
}

const output = option('--out-dir', path.join(root, 'target', 'quickplay-source-audit'));
try {
  const result = validateQuickplay();
  mkdirSync(output, { recursive: true });
  writeFileSync(
    path.join(output, 'quickplay-source-audit.json'),
    `${JSON.stringify({ schemaVersion: 1, status: 'ok', issues: [], package: result }, null, 2)}\n`,
  );
  console.log('Quickplay source audit: ok');
} catch (error) {
  mkdirSync(output, { recursive: true });
  writeFileSync(
    path.join(output, 'quickplay-source-audit.json'),
    `${JSON.stringify({ schemaVersion: 1, status: 'failed', issues: [String(error?.message ?? error)] }, null, 2)}\n`,
  );
  console.error(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
}
