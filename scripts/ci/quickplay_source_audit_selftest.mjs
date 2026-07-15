#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../..', import.meta.url));

execFileSync(process.execPath, ['scripts/ci/quickplay_source_audit.mjs'], {
  cwd: root,
  env: {
    ...process.env,
    QUICKPLAY_SOURCE_AUDIT_BOUNDARY_SELFTEST: '1',
  },
  stdio: 'inherit',
  timeout: 30_000,
});

execFileSync(process.execPath, ['scripts/ci/quickplay_source_audit.mjs'], {
  cwd: root,
  env: {
    ...process.env,
    QUICKPLAY_SOURCE_AUDIT_CLI_PRODUCER_SELFTEST: '1',
  },
  stdio: 'inherit',
  timeout: 30_000,
});
