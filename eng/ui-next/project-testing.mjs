import { cp, mkdir, mkdtemp, readdir, realpath, writeFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { buildProject, checkProject, execute } from './project.mjs';
import { previewBuild } from './project-preview.mjs';
import {toolchain} from './toolchain.mjs';
import {pathToFileURL} from 'node:url';

export async function testProject(directory, { browsers, signal } = {}) {
  directory = await realpath(resolve(directory));
  browsers ??= process.env.UI_NEXT_BROWSER ? [process.env.UI_NEXT_BROWSER] : ['chromium', 'firefox', 'webkit'];
  if (!Array.isArray(browsers) || !browsers.length || new Set(browsers).size !== browsers.length || browsers.some(name => !['chromium', 'firefox', 'webkit'].includes(name))) {
    throw new Error('UI tests require chromium, firefox or webkit, without duplicate browser names.');
  }
  const testDir = join(directory, 'tests/browser');
  const entries = await readdir(testDir, { recursive: true }).catch(error => {
    if (error.code === 'ENOENT') throw new Error('Add a tests/browser/*.test.mjs file using the UI browser fixtures.');
    throw error;
  });
  if (!entries.some(name => name.endsWith('.test.mjs'))) throw new Error('No browser tests found in tests/browser/*.test.mjs.');
  signal?.throwIfAborted();
  await checkProject(directory, { signal });
  signal?.throwIfAborted();
  const distribution = await buildProject(directory, { signal });
  signal?.throwIfAborted();
  const parent = join(directory, 'target/ui-next/browser-tests');
  await mkdir(parent, { recursive: true });
  const output = await mkdtemp(join(parent, 'run-'));
  await cp(join(distribution, 'build-report.json'), join(output, 'build.json'));
  const server = await previewBuild(distribution);
  try {
    const config = {
      testDir, testMatch: '**/*.test.mjs', fullyParallel: false, workers: 1,
      retries: 0, timeout: 30000, expect: { timeout: 5000 }, forbidOnly: true,
      outputDir: join(output, 'results'),
      reporter: [['line'], ['json', { outputFile: join(output, 'report.json') }]],
      use: { baseURL: server.url, headless: true, actionTimeout: 15000,
        viewport: { width: 1280, height: 800 }, trace: 'retain-on-failure', screenshot: 'only-on-failure' },
      projects: browsers.flatMap(browserName => ['vm'].map(backend => ({
        name: `${browserName}-${backend}`, use: { browserName, backend },
      }))),
    };
    const path = join(output, 'playwright.config.mjs');
    await writeFile(path, `export default ${JSON.stringify(config, null, 2)};\n`);
    const log = await execute(process.execPath, [toolchain.testCLI, 'test', '--config', path], {
      cwd: directory, signal,
      env: { ...process.env, VO_UI_TESTING_MODULE:pathToFileURL(toolchain.testing).href,
        ...(process.env.PLAYWRIGHT_BROWSERS_PATH || toolchain.browserCache ? {PLAYWRIGHT_BROWSERS_PATH:process.env.PLAYWRIGHT_BROWSERS_PATH ?? toolchain.browserCache} : {}) },
    });
    return { output, log };
  } catch (error) {
    if (signal?.aborted) throw signal.reason;
    throw new Error(`${error.message}\nBrowser test artifacts: ${output}`, { cause: error });
  } finally { await server.close(); }
}
