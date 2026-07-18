#!/usr/bin/env node
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import {
  mkdirSync,
  mkdtempSync,
  realpathSync,
  rmSync,
  symlinkSync,
  truncateSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {
  artifactSetDigest,
  fileSetDigest,
  parseGitFileList,
  SOURCE_BOUND_EVIDENCE_LIMITS,
  sourceBoundEvidence,
  sourceTreeDeclaredOutputPaths,
  sourceTreeDigest,
  sourceTreeDigestExcludingDeclaredOutputs,
  verifySourceBoundEvidence,
} from './source_bound_evidence.mjs';

const root = realpathSync.native(mkdtempSync(path.join(os.tmpdir(), 'volang-source-bound-evidence-')));
const alternateRepoRoot = mkdtempSync(path.join(os.tmpdir(), 'volang-source-bound-evidence-alt-'));
const previousRunId = process.env.VO_DEV_CI_RUN_ID;

function git(args, cwd = root) {
  return execFileSync('git', args, { cwd, encoding: 'utf8' }).trim();
}

try {
  writeFileSync(path.join(root, 'gate.mjs'), 'export const gate = true;\n');
  writeFileSync(path.join(root, 'gate-alt.mjs'), 'export const alternateGate = true;\n');
  writeFileSync(path.join(root, 'artifact.json'), '{"status":"pass"}\n');
  writeFileSync(path.join(root, 'artifact-alt.json'), '{"status":"alternate-pass"}\n');
  mkdirSync(path.join(root, 'web-artifacts'));
  writeFileSync(path.join(root, 'web-artifacts', 'voplay_island.js'), 'generated glue v1\n');
  writeFileSync(path.join(root, 'web-artifacts', 'voplay_island_bg.wasm'), 'generated wasm v1\n');
  git(['init', '-q']);
  git(['add', '.']);
  git(['-c', 'user.name=Evidence Test', '-c', 'user.email=evidence@example.invalid', 'commit', '-qm', 'fixture']);

  writeFileSync(path.join(alternateRepoRoot, 'source.txt'), 'alternate source\n');
  git(['init', '-q'], alternateRepoRoot);
  git(['add', '.'], alternateRepoRoot);
  git(
    ['-c', 'user.name=Evidence Test', '-c', 'user.email=evidence@example.invalid', 'commit', '-qm', 'alternate fixture'],
    alternateRepoRoot,
  );
  process.env.VO_DEV_CI_RUN_ID = 'selftest-run';

  const voplayOutputDeclaration = 'voplay.current-source-wasm';
  assert.deepEqual(sourceTreeDeclaredOutputPaths(voplayOutputDeclaration), [
    'web-artifacts/voplay_island.js',
    'web-artifacts/voplay_island_bg.wasm',
  ]);
  const completeSourceDigest = sourceTreeDigest(root);
  const declaredOutputScopedDigest = sourceTreeDigestExcludingDeclaredOutputs(
    root,
    voplayOutputDeclaration,
  );
  writeFileSync(path.join(root, 'web-artifacts', 'voplay_island.js'), 'generated glue v2\n');
  assert.notEqual(sourceTreeDigest(root), completeSourceDigest);
  assert.equal(
    sourceTreeDigestExcludingDeclaredOutputs(root, voplayOutputDeclaration),
    declaredOutputScopedDigest,
    'declared generated output bytes must not feed their source digest',
  );
  writeFileSync(path.join(root, 'web-artifacts', 'voplay_island.js'), 'generated glue v1\n');

  const extraOutput = path.join(root, 'web-artifacts', 'undeclared-output.wasm');
  writeFileSync(extraOutput, 'undeclared output\n');
  assert.notEqual(
    sourceTreeDigestExcludingDeclaredOutputs(root, voplayOutputDeclaration),
    declaredOutputScopedDigest,
    'undeclared generated-looking files must remain source inputs',
  );
  rmSync(extraOutput);

  writeFileSync(path.join(root, 'gate.mjs'), 'export const gate = false;\n');
  assert.notEqual(
    sourceTreeDigestExcludingDeclaredOutputs(root, voplayOutputDeclaration),
    declaredOutputScopedDigest,
    'ordinary tracked source drift must change the scoped digest',
  );
  writeFileSync(path.join(root, 'gate.mjs'), 'export const gate = true;\n');
  assert.throws(
    () => sourceTreeDigestExcludingDeclaredOutputs(root, 'unknown.output-declaration'),
    /unknown source-tree output declaration/,
  );
  const declaredJsOutput = path.join(root, 'web-artifacts', 'voplay_island.js');
  rmSync(declaredJsOutput);
  mkdirSync(declaredJsOutput);
  assert.throws(
    () => sourceTreeDigestExcludingDeclaredOutputs(root, voplayOutputDeclaration),
    /canonical tracked regular file/,
  );
  rmSync(declaredJsOutput, { recursive: true });
  writeFileSync(declaredJsOutput, 'generated glue v1\n');

  const evidence = sourceBoundEvidence({
    gate: 'selftest-gate',
    generatedAt: new Date().toISOString(),
    root,
    repos: [{ name: 'fixture', root }],
    gateFiles: ['gate.mjs'],
    artifacts: ['artifact.json'],
  });

  const expectedScope = {
    expectedGate: 'selftest-gate',
    expectedCiRunId: 'selftest-run',
    root,
    expectedGateFiles: ['gate.mjs'],
    expectedArtifacts: ['artifact.json'],
    expectedRepos: [{ name: 'fixture', root }],
  };
  const verifyEvidence = (candidate, overrides = {}) => verifySourceBoundEvidence({
    evidence: candidate,
    ...expectedScope,
    ...overrides,
  });

  assert.deepEqual(verifyEvidence(evidence), []);
  assert.deepEqual(verifyEvidence(evidence, {
    expectedGateFiles: [path.join(root, 'gate.mjs')],
    expectedArtifacts: [path.join(root, 'artifact.json')],
  }), [], 'in-root absolute expected inputs must normalize to the canonical scope');

  const absoluteInputEvidence = sourceBoundEvidence({
    gate: 'selftest-gate',
    generatedAt: new Date().toISOString(),
    root,
    repos: [{ name: 'fixture', root }],
    gateFiles: [path.join(root, 'gate.mjs')],
    artifacts: [path.join(root, 'artifact.json')],
  });
  assert.deepEqual(absoluteInputEvidence.inputs, {
    gateFiles: ['gate.mjs'],
    artifacts: ['artifact.json'],
  });

  const substitutedScopeEvidence = sourceBoundEvidence({
    gate: 'selftest-gate',
    generatedAt: new Date().toISOString(),
    root,
    repos: [{ name: 'fixture', root: alternateRepoRoot }],
    gateFiles: ['gate-alt.mjs'],
    artifacts: ['artifact-alt.json'],
  });
  const scopeSubstitutionIssues = verifyEvidence(substitutedScopeEvidence);
  assert(scopeSubstitutionIssues.some((issue) => issue.includes('gateFiles do not match expected')));
  assert(scopeSubstitutionIssues.some((issue) => issue.includes('artifacts do not match expected')));
  assert(scopeSubstitutionIssues.some((issue) => issue.includes('repos do not match expected')));

  assert(verifyEvidence(evidence, {
    expectedGateFiles: ['../escaped-gate.mjs'],
  }).some((issue) => issue.includes('parent-directory segment')));
  assert(verifyEvidence(evidence, {
    expectedArtifacts: [path.join(path.dirname(root), 'escaped-artifact.json')],
  }).some((issue) => issue.includes('escapes evidence root')));
  assert(verifyEvidence(evidence, {
    expectedRepos: [{ name: 'fixture', root: path.relative(process.cwd(), root) }],
  }).some((issue) => issue.includes('root must be canonical')));

  assert.throws(() => sourceBoundEvidence({
    gate: 'selftest-gate',
    generatedAt: new Date().toISOString(),
    root,
    repos: [{ name: 'fixture', root }],
    gateFiles: ['../escaped-gate.mjs'],
    artifacts: [],
  }), /parent-directory segment/);
  assert.throws(() => sourceBoundEvidence({
    gate: 'selftest-gate',
    generatedAt: new Date().toISOString(),
    root,
    repos: [{ name: 'fixture', root }],
    gateFiles: ['gate.mjs'],
    artifacts: [path.join(path.dirname(root), 'escaped-artifact.json')],
  }), /escapes evidence root/);

  const parentEscapeEvidence = structuredClone(evidence);
  parentEscapeEvidence.inputs.gateFiles = ['../escaped-gate.mjs'];
  assert(verifyEvidence(parentEscapeEvidence)
    .some((issue) => issue.includes('parent-directory segment')));

  const absoluteEscapeEvidence = structuredClone(evidence);
  absoluteEscapeEvidence.inputs.artifacts = [path.join(path.dirname(root), 'escaped-artifact.json')];
  assert(verifyEvidence(absoluteEscapeEvidence)
    .some((issue) => issue.includes('escapes evidence root')));

  assert(verifySourceBoundEvidence({
    evidence,
    expectedGate: 'selftest-gate',
    expectedCiRunId: 'selftest-run',
    root,
  }).some((issue) => issue.includes('expected freshEvidence scope is invalid')));

  const forgedDigest = structuredClone(evidence);
  forgedDigest.gateDigest = `sha256:${'0'.repeat(64)}`;
  assert(verifyEvidence(forgedDigest).some((issue) => issue.includes('gateDigest')));

  assert(verifyEvidence(evidence, {
    expectedCiRunId: 'different-run',
  }).some((issue) => issue.includes('ciRunId')));

  writeFileSync(path.join(root, 'artifact.json'), '{"status":"forged-pass"}\n');
  assert(verifyEvidence(evidence).some((issue) => issue.includes('artifactDigest')));

  assert.equal(
    fileSetDigest(root, [path.join(root, 'gate.mjs')]),
    fileSetDigest(root, ['gate.mjs']),
    'absolute gate paths must preserve the existing digest contract',
  );
  assert.equal(
    artifactSetDigest(root, [null, 'artifact.json']),
    artifactSetDigest(root, ['artifact.json']),
    'optional null artifacts must remain ignored',
  );

  const oversizedEvidenceInputs = structuredClone(evidence);
  oversizedEvidenceInputs.inputs.gateFiles = Array(
    SOURCE_BOUND_EVIDENCE_LIMITS.maxInputPaths + 1,
  ).fill('gate.mjs');
  assert.doesNotThrow(() => verifyEvidence(oversizedEvidenceInputs));
  assert(verifyEvidence(oversizedEvidenceInputs)
    .some((issue) => issue.includes('path input limit')));

  assert.throws(
    () => fileSetDigest(
      root,
      Array(SOURCE_BOUND_EVIDENCE_LIMITS.maxInputPaths + 1).fill('missing'),
    ),
    /path input limit/,
  );
  assert.throws(
    () => fileSetDigest(root, ['missing-gate.mjs']),
    /existing regular file/,
  );
  assert.throws(
    () => artifactSetDigest(root, ['missing-artifact']),
    /existing regular file or directory/,
  );

  const gitPathWidth = 4;
  const overflowingGitEntryCount = SOURCE_BOUND_EVIDENCE_LIMITS.maxTreeEntries + 1;
  const overflowingGitList = Buffer.alloc(
    overflowingGitEntryCount * (gitPathWidth + 1),
  );
  for (let index = 0; index < overflowingGitEntryCount; index += 1) {
    overflowingGitList.write(
      index.toString(36).padStart(gitPathWidth, '0'),
      index * (gitPathWidth + 1),
      gitPathWidth,
      'ascii',
    );
  }
  assert.throws(
    () => parseGitFileList(overflowingGitList, root),
    /Git source tree .*entry limit/,
  );
  assert.throws(
    () => parseGitFileList(Buffer.from([0xff, 0]), root),
    /not valid UTF-8/,
  );

  const directoryBudgetRoot = path.join(root, 'directory-budget');
  mkdirSync(directoryBudgetRoot);
  mkdirSync(path.join(directoryBudgetRoot, 'a'));
  mkdirSync(path.join(directoryBudgetRoot, 'b'));
  assert.doesNotThrow(() => artifactSetDigest(root, ['directory-budget'], {
    maxDirectoryEntries: 2,
    maxDirectoryObservationEntries: 2,
  }));
  assert.throws(
    () => artifactSetDigest(root, ['directory-budget'], {
      maxDirectoryEntries: 2,
      maxDirectoryObservationEntries: 1,
    }),
    /directory observation limit/,
  );
  mkdirSync(path.join(directoryBudgetRoot, 'c'));
  assert.throws(
    () => artifactSetDigest(root, ['directory-budget'], {
      maxDirectoryEntries: 2,
      maxDirectoryObservationEntries: 3,
    }),
    /traversal limit/,
  );

  const oversizedFile = path.join(root, 'oversized.bin');
  writeFileSync(oversizedFile, '');
  truncateSync(oversizedFile, SOURCE_BOUND_EVIDENCE_LIMITS.maxFileBytes + 1);
  assert.throws(
    () => fileSetDigest(root, ['oversized.bin']),
    /file limit/,
  );

  const aggregateDir = path.join(root, 'aggregate');
  mkdirSync(aggregateDir);
  const aggregateFileBytes = Math.floor(SOURCE_BOUND_EVIDENCE_LIMITS.maxTotalBytes / 3) + 1;
  for (const name of ['a.bin', 'b.bin', 'c.bin']) {
    const file = path.join(aggregateDir, name);
    writeFileSync(file, '');
    truncateSync(file, aggregateFileBytes);
  }
  assert.throws(
    () => artifactSetDigest(root, ['aggregate']),
    /aggregate limit/,
  );

  const deepRoot = path.join(root, 'deep');
  mkdirSync(deepRoot);
  let deepDir = deepRoot;
  for (let index = 0; index <= SOURCE_BOUND_EVIDENCE_LIMITS.maxDepth; index += 1) {
    deepDir = path.join(deepDir, 'd');
    mkdirSync(deepDir);
  }
  assert.throws(
    () => artifactSetDigest(root, ['deep']),
    /traversal limit/,
  );

  const outsideDir = path.join(root, 'outside');
  const linkedTree = path.join(root, 'linked-tree');
  mkdirSync(outsideDir);
  mkdirSync(linkedTree);
  writeFileSync(path.join(outsideDir, 'secret.txt'), 'first\n');
  let symlinksSupported = true;
  try {
    symlinkSync('../outside', path.join(linkedTree, 'outside'));
  } catch (error) {
    if (['EACCES', 'ENOSYS', 'EPERM'].includes(error?.code)) {
      symlinksSupported = false;
    } else {
      throw error;
    }
  }
  if (symlinksSupported) {
    assert.throws(
      () => artifactSetDigest(root, ['linked-tree']),
      /artifact tree entry .* must be a regular file or directory/,
      'artifact traversal must reject symbolic links',
    );

    symlinkSync('gate.mjs', path.join(root, 'gate-link.mjs'));
    assert.throws(
      () => fileSetDigest(root, ['gate-link.mjs']),
      /existing regular file/,
      'gate inputs must reject symbolic links',
    );

    const linkedParentTarget = path.join(root, 'linked-parent-target');
    mkdirSync(linkedParentTarget);
    writeFileSync(path.join(linkedParentTarget, 'gate.mjs'), 'export const linked = true;\n');
    symlinkSync('linked-parent-target', path.join(root, 'linked-parent'));
    assert.throws(
      () => fileSetDigest(root, ['linked-parent/gate.mjs']),
      /symbolic-link ancestor/,
      'gate inputs must reject symbolic-link ancestors',
    );
  }

  const emptyDir = path.join(root, 'empty');
  mkdirSync(emptyDir);
  const dotArtifactRoot = path.join(root, 'dot-artifact-root');
  mkdirSync(dotArtifactRoot);
  writeFileSync(path.join(dotArtifactRoot, 'artifact.txt'), 'artifact\n');
  assert.doesNotThrow(
    () => artifactSetDigest(dotArtifactRoot, ['.']),
    'the evidence root itself must be a valid explicit artifact directory',
  );
  assert.notEqual(
    artifactSetDigest(root, ['empty']),
    artifactSetDigest(root, []),
    'an empty artifact directory must remain distinguishable from no artifact',
  );

  console.log('source-bound-evidence selftest: ok');
} finally {
  if (previousRunId === undefined) {
    delete process.env.VO_DEV_CI_RUN_ID;
  } else {
    process.env.VO_DEV_CI_RUN_ID = previousRunId;
  }
  rmSync(root, { recursive: true, force: true });
  rmSync(alternateRepoRoot, { recursive: true, force: true });
}
