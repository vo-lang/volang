import { createHash } from 'node:crypto';
import { existsSync, readFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const scriptDir = dirname(fileURLToPath(import.meta.url));
export const STUDIO_ROOT = resolve(scriptDir, '..');

export const QUICKPLAY_PACKAGE_ROOT = 'public/quickplay/blockkart';
export const QUICKPLAY_PACKAGE_BASE_FILES = [
  'public/quickplay/blockkart/project.json',
  'public/quickplay/blockkart/deps.json',
];

export function quickplayPackageFiles({ studioRoot = STUDIO_ROOT } = {}) {
  const depsPath = resolve(studioRoot, QUICKPLAY_PACKAGE_ROOT, 'deps.json');
  if (!existsSync(depsPath)) {
    return QUICKPLAY_PACKAGE_BASE_FILES;
  }

  const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
  const prefix = '/quickplay/blockkart/';
  const files = [...QUICKPLAY_PACKAGE_BASE_FILES];
  for (const modulePack of deps.modules ?? []) {
    for (const artifact of modulePack.artifacts ?? []) {
      if (typeof artifact.url !== 'string' || !artifact.url.startsWith(prefix)) {
        throw new Error(`invalid quickplay artifact URL: ${artifact.url}`);
      }
      files.push(`${QUICKPLAY_PACKAGE_ROOT}/${artifact.url.slice(prefix.length)}`);
    }
  }
  return [...new Set(files)].sort();
}

export function readQuickplayPackageBuildId({ studioRoot = STUDIO_ROOT } = {}) {
  const hash = createHash('sha256');
  let sawFile = false;
  for (const file of quickplayPackageFiles({ studioRoot })) {
    const absolute = resolve(studioRoot, file);
    if (!existsSync(absolute)) {
      continue;
    }
    sawFile = true;
    hash.update(file);
    hash.update('\0');
    hash.update(readFileSync(absolute));
    hash.update('\0');
  }
  return sawFile ? `qp-${hash.digest('hex').slice(0, 12)}` : null;
}

export function resolveStudioBuildId(env = process.env, { studioRoot = STUDIO_ROOT } = {}) {
  const explicit = (env.VIBE_STUDIO_BUILD_ID ?? '').trim();
  if (explicit.length > 0) {
    return explicit;
  }

  const base = [env.GITHUB_SHA, env.GITHUB_RUN_ID, env.GITHUB_RUN_ATTEMPT]
    .map((value) => (value ?? '').trim())
    .filter((value) => value.length > 0)
    .join('-') || `local-${Date.now().toString(16)}`;

  const quickplayBuildId = readQuickplayPackageBuildId({ studioRoot });
  return [base, quickplayBuildId].filter((value) => typeof value === 'string' && value.length > 0).join('-');
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  console.log(resolveStudioBuildId(process.env));
}
