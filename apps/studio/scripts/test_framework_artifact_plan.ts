import assert from 'node:assert/strict';

import { planFrameworkArtifacts } from '../src/lib/gui/framework_artifact_plan.ts';
import type { FrameworkContract } from '../src/lib/types.ts';

function contract(
  name: string,
  jsModules: Record<string, string>,
  entry: string | null = null,
): FrameworkContract {
  return {
    moduleKey: name,
    name,
    entry,
    providerRole: null,
    providerRoles: [],
    capabilities: [],
    roles: Object.keys(jsModules).sort(),
    jsModules,
  };
}

{
  const plan = planFrameworkArtifacts(contract('logic-only', {}), []);
  assert.equal(plan.needsVfs, false);
  assert.deepEqual(plan.hostBridgePaths, []);
  assert.deepEqual(plan.rendererPaths, []);
}

{
  const primary = contract('vogui', {
    host_bridge: '/providers/vogui/host.js',
    renderer: '/providers/vogui/renderer.js',
    protocol: '/providers/vogui/legacy-protocol.js',
  });
  const overlay = contract('diagnostics', {
    host_bridge: '/providers/shared/host.js',
    renderer: '/providers/diagnostics/renderer.js',
  });
  const duplicateBridge = contract('input-provider', {
    host_bridge: '/providers/shared/host.js',
  });
  const plan = planFrameworkArtifacts(primary, [primary, overlay, duplicateBridge]);
  assert.deepEqual(plan.frameworks.map((item) => item.name), [
    'vogui',
    'diagnostics',
    'input-provider',
  ]);
  assert.deepEqual(plan.hostBridgePaths, [
    '/providers/vogui/host.js',
    '/providers/shared/host.js',
  ]);
  assert.deepEqual(plan.rendererPaths, [
    '/providers/vogui/renderer.js',
    '/providers/diagnostics/renderer.js',
  ]);
  assert.equal(plan.needsVfs, true);
  assert.equal(
    [...plan.hostBridgePaths, ...plan.rendererPaths].includes('/providers/vogui/legacy-protocol.js'),
    false,
  );
}

assert.throws(
  () => planFrameworkArtifacts(
    contract('vogui', { renderer: '/providers/vogui/a.js' }, 'a'),
    [contract('vogui', { renderer: '/providers/vogui/b.js' }, 'b')],
  ),
  /conflicting runtime contracts/,
);

console.log('studio framework artifact plan: ok');
