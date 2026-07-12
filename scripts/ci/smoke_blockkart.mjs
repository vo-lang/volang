#!/usr/bin/env node
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

function readJson(path) {
  try {
    return JSON.parse(readFileSync(path, 'utf8'));
  } catch (error) {
    fail(`cannot read JSON ${path}: ${error.message}`);
  }
}

function localPathForArtifact(url) {
  const prefix = '/quickplay/blockkart/';
  assert(url.startsWith(prefix), `artifact URL must be quickplay-local: ${url}`);
  return join(root, 'apps/studio/public/quickplay/blockkart', url.slice(prefix.length));
}

function moduleByName(deps, moduleName) {
  const modulePack = deps.modules?.find((mod) => mod.module === moduleName);
  assert(modulePack, `deps manifest is missing ${moduleName}`);
  return modulePack;
}

function webArtifacts(modulePack) {
  return (modulePack.artifacts ?? []).filter((artifact) => typeof artifact.url === 'string');
}

function moduleFileBytes(file) {
  if (file?.content != null) return Buffer.from(file.content, 'utf8');
  if (file?.contentBase64 != null) return Buffer.from(file.contentBase64, 'base64');
  return Buffer.alloc(0);
}

function assertBlockKartRuntimeAsset(project) {
  const files = new Map((project.files ?? []).map((file) => [file.path, file]));
  const runtimeAsset = files.get('assets/blockkart.vpak');
  assert(runtimeAsset, 'project package is missing assets/blockkart.vpak');
  assert(moduleFileBytes(runtimeAsset).byteLength > 1024 * 1024, 'assets/blockkart.vpak is not a full runtime asset pack');
  assert(!files.has('apps/studio/fixtures/blockkart/blockkart.vpak'), 'project package still embeds the Studio fixture path');
}

function requiredVoplayArtifacts(deps) {
  const voplay = moduleByName(deps, 'github.com/vo-lang/voplay');
  const artifacts = webArtifacts(voplay);
  const js = artifacts.find((artifact) => artifact.url.endsWith('/voplay_island.js'));
  const wasm = artifacts.find((artifact) => artifact.url.endsWith('/voplay_island_bg.wasm'));
  assert(js, 'deps manifest is missing voplay quickplay JS artifact');
  assert(wasm, 'deps manifest is missing voplay quickplay WASM artifact');
  return [js, wasm];
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
  const indexPath = join(root, 'apps/studio/src/lib/quickplay.ts');
  const quickplayTs = readFileSync(indexPath, 'utf8');
  assert(quickplayTs.includes('__STUDIO_BUILD_ID__'), 'manifest URLs must use the studio build id');
  assert(quickplayTs.includes("searchParams.set('build'"), 'manifest URLs must carry the build query');
  assert(!quickplayTs.includes('/quickplay/blockkart/artifacts/'), 'quickplay.ts must not hard-code artifact URLs');

  const deps = readJson(join(root, 'apps/studio/public/quickplay/blockkart/deps.json'));
  const project = readJson(join(root, 'apps/studio/public/quickplay/blockkart/project.json'));
  assertBlockKartRuntimeAsset(project);
  for (const artifact of requiredVoplayArtifacts(deps)) {
    const path = localPathForArtifact(artifact.url);
    assert(existsSync(path), `missing packaged artifact: ${artifact.url}`);
  }
  console.log('BlockKart smoke: static ok');
}

async function runHttpSmoke(baseUrl, buildId) {
  const appResponse = await fetchOk(baseUrl);
  const html = await appResponse.text();
  const scriptMatch = html.match(/<script[^>]+type="module"[^>]+src="([^"]*\/assets\/index-[^"]+\.js)"/);
  assert(scriptMatch, 'could not find Vite entry script');

  const scriptUrl = new URL(scriptMatch[1], baseUrl).toString();
  const script = await (await fetchOk(scriptUrl)).text();
  assert(script.includes('/quickplay/blockkart/project.json'), 'entry bundle does not reference BlockKart project manifest');
  assert(script.includes('/quickplay/blockkart/deps.json'), 'entry bundle does not reference BlockKart deps manifest');
  assert(!script.includes('/quickplay/blockkart/artifacts/'), 'entry bundle hard-codes BlockKart artifact URLs');
  if (buildId) {
    assert(script.includes(buildId), `entry bundle does not contain build id ${buildId}`);
  }

  const query = buildId ? `?build=${encodeURIComponent(buildId)}` : '';
  const wasmBuildId = (await (await fetchOk(joinUrl(baseUrl, `/wasm/vo_studio_wasm.build_id${query}`))).text()).trim();
  assert(wasmBuildId.length > 0, 'remote studio WASM build id is empty');
  assert(!script.includes(`${wasmBuildId}-qp-`), `entry bundle extends the WASM build id ${wasmBuildId}`);
  if (buildId) {
    assert(wasmBuildId === buildId, `remote studio WASM build id ${wasmBuildId} does not match expected ${buildId}`);
  }

  const project = await (await fetchOk(joinUrl(baseUrl, `/quickplay/blockkart/project.json${query}`))).json();
  const deps = await (await fetchOk(joinUrl(baseUrl, `/quickplay/blockkart/deps.json${query}`))).json();
  assert(project.name === 'BlockKart', 'remote project manifest is not BlockKart');
  assert(project.module === 'github.com/vo-lang/blockkart', 'remote project manifest module is wrong');
  assertBlockKartRuntimeAsset(project);

  requiredVoplayArtifacts(deps);
  for (const modulePack of deps.modules ?? []) {
    for (const artifact of webArtifacts(modulePack)) {
      await fetchOk(joinUrl(baseUrl, `${artifact.url}${query}`));
    }
  }
  console.log('BlockKart smoke: remote ok');
}

const baseUrl = argValue('--base-url') || process.env.BLOCKKART_SMOKE_BASE_URL || '';
const buildId = argValue('--build-id') || process.env.BLOCKKART_SMOKE_BUILD_ID || '';

if (!baseUrl) {
  runStaticSmoke();
} else {
  await runHttpSmoke(baseUrl, buildId);
}
