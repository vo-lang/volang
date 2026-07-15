import { createHash } from 'node:crypto';
import { VO_CLI_PRODUCER_TASK_INPUTS } from './quickplay_cli_producer_contract.mjs';
import { compareUtf8 } from './utf8_order.mjs';

export const QUICKPLAY_ARTIFACT_NAME = 'studio.quickplay.blockkart';
export const QUICKPLAY_ARTIFACT_PATH = 'apps/studio/public/quickplay/blockkart';
export const QUICKPLAY_GENERATOR_VERSION = 18;
export const QUICKPLAY_TASK_ID = 'quickplay-blockkart-package';
export const QUICKPLAY_GENERATOR_COMMAND = Object.freeze([
  'vo-dev',
  'task',
  'run',
  'task:quickplay-blockkart-package',
]);
export const QUICKPLAY_SOURCE_ROOTS = Object.freeze({
  volang: '.',
  blockKart: 'external:BlockKart',
  vogui: 'first-party:vogui',
  vopack: 'first-party:vopack',
  voplay: 'first-party:voplay',
});
export const QUICKPLAY_BLOCKKART_SOURCE_ALLOWLIST = Object.freeze([
  Object.freeze({
    path: 'tools/pack_primitive_assets.vo',
    reason: 'Asset-pack generation tool; quickplay runtime embeds the generated assets/blockkart.vpak payload.',
    expiresAt: '2027-01-31T00:00:00.000Z',
  }),
  Object.freeze({
    path: 'runtimepack/runtime_pack.vo',
    reason: 'Asset-pack schema package; quickplay runtime consumes its generated assets/blockkart.vpak payload.',
    expiresAt: '2027-01-31T00:00:00.000Z',
  }),
]);

export function quickplayBlockKartSourceAllowlist(sourcePaths) {
  if (!Array.isArray(sourcePaths) || sourcePaths.some((value) => typeof value !== 'string')) {
    throw new TypeError('BlockKart source paths must be an array of strings');
  }
  const available = new Set(sourcePaths);
  return QUICKPLAY_BLOCKKART_SOURCE_ALLOWLIST
    .filter((entry) => available.has(entry.path))
    .map((entry) => ({ ...entry }))
    .sort((left, right) => compareUtf8(left.path, right.path));
}

// These files define the in-repository packaging algorithm. The generator and
// validator hash the same set so any producer-rule change invalidates stale
// provenance.
export const QUICKPLAY_GENERATOR_AUXILIARY_SOURCE_INPUTS = Object.freeze([
  'scripts/ci/quickplay_module_cache.mjs',
  'scripts/ci/blockkart_vpak_build.mjs',
  'scripts/ci/blockkart_vpak_policy.mjs',
  'scripts/ci/blockkart_vpak_provenance_selftest.mjs',
]);
export const QUICKPLAY_GENERATOR_SOURCE_INPUTS = Object.freeze([
  'apps/studio/scripts/package_blockkart_quickplay.mjs',
  'scripts/ci/blockkart_vpak_build.mjs',
  'scripts/ci/blockkart_vpak_policy.mjs',
  'scripts/ci/blockkart_vpak_provenance_selftest.mjs',
  'scripts/ci/quickplay_artifact_paths.mjs',
  'scripts/ci/quickplay_cli_producer_contract.mjs',
  'scripts/ci/quickplay_generator_contract.mjs',
  'scripts/ci/quickplay_module_cache.mjs',
  'scripts/ci/quickplay_web_manifest_contract.mjs',
  'scripts/ci/portable_path_key.mjs',
  'scripts/ci/unicode_casefold_data.mjs',
  'scripts/ci/vo_lock_v2.mjs',
  'scripts/ci/utf8_order.mjs',
  'scripts/ci/source_bound_evidence.mjs',
  'scripts/ci/voplay_current_wasm.mjs',
  'eng/artifacts.toml',
  'eng/project.toml',
  'eng/tasks.toml',
  'eng/ci.toml',
  'scripts/ci/repo_roots.mjs',
]);

export const QUICKPLAY_GENERATOR_INPUTS = Object.freeze([
  ...QUICKPLAY_GENERATOR_SOURCE_INPUTS,
  ...VO_CLI_PRODUCER_TASK_INPUTS,
  'external:BlockKart',
  'external:BlockKart/tools/pack_primitive_assets.vo',
  'external:BlockKart/tools/generate_primitive_terrain.mjs',
  'external:BlockKart/tools/paint_terrain_textures.mjs',
  'external:BlockKart/tools/vpak_provenance.mjs',
  'external:BlockKart/tools/terrain_heightfield_spec.mjs',
  'external:BlockKart/tools/terrain_recipe.mjs',
  'external:BlockKart/terrain/recipes/primitive_concept_v1.json',
  'first-party:vogui',
  'first-party:vopack',
  'first-party:voplay',
  'module-cache:vopack',
  'module-cache:vogui',
  'module-cache:voplay',
]);

function validDigest(value) {
  return typeof value === 'string' && /^sha256:[0-9a-f]{64}$/.test(value);
}

export function quickplayGeneratorSourceDigest(entries) {
  if (!Array.isArray(entries) || entries.length !== QUICKPLAY_GENERATOR_SOURCE_INPUTS.length) {
    throw new Error('quickplay generator source entries do not match the contract length');
  }
  const canonical = entries
    .map((entry) => {
      if (
        !entry
        || typeof entry !== 'object'
        || typeof entry.path !== 'string'
        || !Number.isSafeInteger(entry.size)
        || entry.size < 0
        || !validDigest(entry.digest)
      ) {
        throw new Error('quickplay generator source entry is invalid');
      }
      return { digest: entry.digest, path: entry.path, size: entry.size };
    })
    .sort((left, right) => compareUtf8(left.path, right.path));
  const paths = canonical.map((entry) => entry.path);
  const expected = [...QUICKPLAY_GENERATOR_SOURCE_INPUTS].sort(compareUtf8);
  if (paths.some((value, index) => value !== expected[index])) {
    throw new Error('quickplay generator source entries do not match the contract paths');
  }
  return `sha256:${createHash('sha256').update(JSON.stringify(canonical), 'utf8').digest('hex')}`;
}
