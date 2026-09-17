import assert from 'node:assert/strict';
import { mkdtemp, writeFile, rm } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { compilerPath } from '../../lang/crates/vo-web/test_compiler.mjs';
import { createProject, execute } from './project.mjs';
import { root } from './server.mjs';

import {consoleOutputs} from './studio-example-expectations.mjs';
export {consoleOutputs} from './studio-example-expectations.mjs';

export async function studioExamples() {
  return JSON.parse(await execute(compilerPath(), ['run', 'apps/studio/next/tests/examples'], {
    env: { ...process.env, VOWORK: resolve(root, 'vo.work') },
  }));
}

export async function checkStudioExamples() {
  const catalog = await studioExamples();
  const temporary = await mkdtemp(resolve(root, 'target/ui-next/example-contract-'));
  const reports = [], ids = new Set();
  try {
    const project = await createProject(join(temporary, 'examples'));
    const env = { ...process.env, VOWORK: join(project, 'vo.work') };
    for (const [kind, entries] of Object.entries(catalog)) {
      assert(['console', 'ui'].includes(kind));
      for (const example of entries) {
        assert(!ids.has(example.id)); ids.add(example.id);
        assert(example.title && example.description && example.source.length <= 100000);
        await writeFile(join(project, 'main.vo'), example.source);
        await execute(compilerPath(), ['check', project], { cwd: project, env });
        if (kind === 'console') {
          assert.equal(await execute(compilerPath(), ['run', project], { cwd: project, env }), consoleOutputs[example.id], example.id);
        }
        reports.push({ id: example.id, kind, compiled: true, nativeExecuted: kind === 'console' });
      }
    }
    assert.equal(catalog.console.length, 6); assert.equal(catalog.ui.length, 3);
    await writeFile(resolve(root, 'target/ui-next/studio-examples-native-report.json'), JSON.stringify({ passed: true, reports }, null, 2) + '\n');
    return reports;
  } finally { await rm(temporary, { recursive: true, force: true }); }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  await checkStudioExamples();
  console.log('Studio examples: all nine compile; six console programs produce expected native output');
}
