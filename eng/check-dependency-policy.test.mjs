import { test } from 'node:test';
import assert from 'node:assert/strict';
import { checkDependencyPolicy } from './check-dependency-policy.mjs';
const now = new Date('2026-09-05T00:00:00Z');
function fixture() {
  return {
    policy: { schema: 'volang.dependency-policy.v1', rust_lockfiles: ['Cargo.lock'], npm_workspaces: ['web'], warning_exceptions: [] },
    reports: {
      'Cargo.lock': { database: { 'last-commit': 'a'.repeat(40) }, lockfile: { 'dependency-count': 1 },
        settings: { target_arch: [], target_os: [], severity: null, ignore: [], informational_warnings: ['unmaintained', 'unsound', 'notice'] },
        vulnerabilities: { found: false, count: 0, list: [] }, warnings: {} },
      web: { auditReportVersion: 2, vulnerabilities: {}, metadata: { vulnerabilities: { info: 0, low: 0, moderate: 0, high: 0, critical: 0, total: 0 } } },
    },
  };
}
test('requires every declared audit input and unfiltered Rust findings', () => {
  const initial = fixture();
  assert.equal(checkDependencyPolicy(initial.policy, initial.reports, now).passed, true);
  for (const mutate of [
    x => delete x.reports.web,
    x => x.reports.extra = {},
    x => x.reports['Cargo.lock'].settings.ignore.push('RUSTSEC-2024-0429'),
    x => x.reports['Cargo.lock'].settings.target_os.push('linux'),
    x => x.reports['Cargo.lock'].settings.informational_warnings.pop(),
    x => delete x.reports['Cargo.lock'].warnings,
    x => x.reports.web.metadata.vulnerabilities.total = 1,
  ]) {
    const x = fixture(); mutate(x);
    assert.throws(() => checkDependencyPolicy(x.policy, x.reports, now));
  }
});
test('vulnerabilities cannot be exempted by a warning exception', () => {
  const x = fixture();
  x.reports['Cargo.lock'].vulnerabilities = { found: true, count: 1, list: [{ advisory: { id: 'RUSTSEC-TEST' } }] };
  assert.equal(checkDependencyPolicy(x.policy, x.reports, now).passed, false);
  const npm = fixture();
  npm.reports.web.vulnerabilities.bad = {};
  Object.assign(npm.reports.web.metadata.vulnerabilities, { high: 1, total: 1 });
  assert.equal(checkDependencyPolicy(npm.policy, npm.reports, now).passed, false);
});
test('warning reviews bind advisory, package, exact version, lockfile, kind and expiry', () => {
  const make = () => {
    const x = fixture();
    x.reports['Cargo.lock'].warnings.unsound = [{ advisory: { id: 'RUSTSEC-TEST' }, package: { name: 'glib', version: '0.18.5' } }];
    x.policy.warning_exceptions.push({ advisory: 'RUSTSEC-TEST', package: 'glib', version: '0.18.5', kind: 'unsound',
      lockfiles: ['Cargo.lock'], owner: 'ui', dependency_chain: ['host', 'glib'], reason: 'Tracked migration', reviewed: '2026-09-05', expires: '2026-10-05' });
    return x;
  };
  const ok = make(); assert.equal(checkDependencyPolicy(ok.policy, ok.reports, now).passed, true);
  for (const change of [
    { version: '0.18.6' }, { kind: 'notice' }, { owner: '' }, { lockfiles: ['fuzz/Cargo.lock'] },
    { expires: '2026-09-05' }, { expires: '2027-01-01' }, { expires: '2026-09-31' }, { reviewed: '2026-09-06' },
  ]) {
    const x = make(); Object.assign(x.policy.warning_exceptions[0], change);
    assert.equal(checkDependencyPolicy(x.policy, x.reports, now).passed, false, JSON.stringify(change));
  }
  const duplicate = make(); duplicate.policy.warning_exceptions.push(duplicate.policy.warning_exceptions[0]);
  assert.equal(checkDependencyPolicy(duplicate.policy, duplicate.reports, now).passed, false);
});
