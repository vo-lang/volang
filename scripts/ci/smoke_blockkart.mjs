#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../..', import.meta.url));

function fail(message) {
  console.error(`BlockKart smoke: ${message}`);
  process.exit(1);
}

function argValue(name) {
  const index = process.argv.indexOf(name);
  if (index === -1) {
    return '';
  }
  return process.argv[index + 1] ?? '';
}

function assert(condition, message) {
  if (!condition) {
    fail(message);
  }
}

async function fetchOk(url) {
  let lastStatus = '';
  for (let attempt = 1; attempt <= 8; attempt++) {
    try {
      const response = await fetch(url, { cache: 'no-store' });
      if (response.ok) {
        return response;
      }
      lastStatus = String(response.status);
    } catch (error) {
      lastStatus = error.message;
    }
    await new Promise((resolve) => setTimeout(resolve, attempt * 750));
  }
  fail(`${url} did not become available: ${lastStatus}`);
}

function joinUrl(base, path) {
  return new URL(path.replace(/^\//, ''), base.endsWith('/') ? base : `${base}/`).toString();
}

function runStaticSmoke() {
  const validate = spawnSync('node', ['scripts/ci/quickplay_validate.mjs'], {
    cwd: root,
    stdio: 'inherit',
  });
  if (validate.status !== 0) {
    process.exit(validate.status ?? 1);
  }

  const indexPath = join(root, 'studio/src/lib/quickplay.ts');
  const quickplayTs = readFileSync(indexPath, 'utf8');
  assert(quickplayTs.includes('?${params.toString()}'), 'manifest URLs must carry the build query');

  const artifactRoot = join(root, 'studio/public/quickplay/blockkart/artifacts');
  assert(existsSync(join(artifactRoot, 'github.com@vo-lang@voplay/v0.1.25/voplay_island.js')), 'missing voplay quickplay JS artifact');
  assert(existsSync(join(artifactRoot, 'github.com@vo-lang@voplay/v0.1.25/voplay_island_bg.wasm')), 'missing voplay quickplay WASM artifact');
  console.log('BlockKart smoke: static ok');
}

async function runHttpSmoke(baseUrl, buildId) {
  const appResponse = await fetchOk(baseUrl);
  const html = await appResponse.text();
  const scriptMatch = html.match(/<script[^>]+type="module"[^>]+src="([^"]+\/assets\/index-[^"]+\.js)"/);
  assert(scriptMatch, 'could not find Vite entry script');

  const scriptUrl = new URL(scriptMatch[1], baseUrl).toString();
  const script = await (await fetchOk(scriptUrl)).text();
  assert(script.includes('/quickplay/blockkart/project.json'), 'entry bundle does not reference BlockKart project manifest');
  assert(script.includes('/quickplay/blockkart/deps.json'), 'entry bundle does not reference BlockKart deps manifest');
  assert(script.includes('v0.1.25'), 'entry bundle does not reference current voplay quickplay artifacts');
  assert(!script.includes('v0.1.23'), 'entry bundle still references stale voplay v0.1.23 artifacts');
  if (buildId) {
    assert(script.includes(buildId), `entry bundle does not contain build id ${buildId}`);
  }

  const query = buildId ? `?build=${encodeURIComponent(buildId)}` : '';
  const project = await (await fetchOk(joinUrl(baseUrl, `/quickplay/blockkart/project.json${query}`))).json();
  const deps = await (await fetchOk(joinUrl(baseUrl, `/quickplay/blockkart/deps.json${query}`))).json();
  assert(project.name === 'BlockKart', 'remote project manifest is not BlockKart');
  assert(project.module === 'github.com/vo-lang/blockkart', 'remote project manifest module is wrong');

  const voplay = deps.modules?.find((mod) => mod.module === 'github.com/vo-lang/voplay');
  assert(voplay?.version === 'v0.1.25', 'remote deps manifest does not use voplay v0.1.25');
  for (const artifact of voplay.artifacts ?? []) {
    await fetchOk(joinUrl(baseUrl, artifact.url));
  }
  console.log('BlockKart smoke: remote ok');
}

const baseUrl = argValue('--base-url');
const buildId = argValue('--build-id');

if (!baseUrl) {
  runStaticSmoke();
} else {
  await runHttpSmoke(baseUrl, buildId);
}
