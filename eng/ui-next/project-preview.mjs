import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import {compilerPath} from './toolchain.mjs';
import { serveFiles } from './static-server.mjs';
import { serveApplication } from './server-adapter.mjs';

export async function previewBuild(directory, options = {}) {
  const report = JSON.parse(await readFile(join(directory, 'build-report.json'), 'utf8'));
  return report.server
    ? serveApplication(directory, {executable:compilerPath(), ...options})
    : serveFiles(directory, options);
}
