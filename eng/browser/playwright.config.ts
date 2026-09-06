import { defineConfig } from '@playwright/test';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';

const request = process.env.VO_BROWSER_REQUEST
  ? JSON.parse(readFileSync(process.env.VO_BROWSER_REQUEST, 'utf8')) : null;
const output = request?.diagnostics ?? resolve('target/ci/browser/default');
export default defineConfig({
  testDir: '.',
  testMatch: 'scenario.spec.ts',
  timeout: 10 * 60 * 1000,
  expect: { timeout: request?.timeout ?? 30_000 },
  workers: 1,
  fullyParallel: false,
  forbidOnly: Boolean(process.env.CI),
  retries: 0,
  outputDir: resolve(output, 'artifacts'),
  reporter: [
    ['line'],
    ['json', { outputFile: resolve(output, 'playwright.json') }],
    ['html', { outputFolder: resolve(output, 'html'), open: 'never' }],
    ...(process.env.PLAYWRIGHT_BLOB_OUTPUT_FILE ? [['blob'] as const] : []),
  ],
  use: {
    browserName: 'chromium',
    headless: true,
    viewport: { width: 1280, height: 900 },
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
    actionTimeout: request?.timeout ?? 30_000,
    navigationTimeout: request?.timeout ?? 30_000,
    launchOptions: {
      args: ['--enable-unsafe-webgpu', ...(process.platform === 'darwin' ? ['--use-angle=metal'] : [])],
    },
  },
});
