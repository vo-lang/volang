import {
  QUICKPLAY_GENERATOR_SOURCE_INPUTS,
} from './quickplay_generator_contract.mjs';

// Producer and final readiness consumer share this exact scope. Extending the
// packaging algorithm automatically extends the source-audit evidence binding.
export const QUICKPLAY_SOURCE_AUDIT_GATE_FILES = Object.freeze([...new Set([
  'scripts/ci/quickplay_source_audit.mjs',
  'scripts/ci/quickplay_source_audit_selftest.mjs',
  'scripts/ci/quickplay_source_audit_scope.mjs',
  'scripts/ci/blockkart_vpak_policy.mjs',
  ...QUICKPLAY_GENERATOR_SOURCE_INPUTS,
])]);
