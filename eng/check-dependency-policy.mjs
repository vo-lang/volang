#!/usr/bin/env node
import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

export function checkDependencyPolicy(policy, reports, now = new Date()) {
  if (policy.schema !== 'volang.dependency-policy.v1') throw new Error('unsupported dependency policy');
  const failures = [];
  const acceptedWarnings = [];
  const databases = [];
  const today = now.toISOString().slice(0, 10);
  const validDate = value => typeof value === 'string' && /^\d{4}-\d{2}-\d{2}$/.test(value)
    && Number.isFinite(Date.parse(value)) && new Date(value).toISOString().slice(0, 10) === value;
  const expected = [...policy.rust_lockfiles, ...policy.npm_workspaces];
  if (new Set(expected).size !== expected.length
    || Object.keys(reports).sort().join('\n') !== expected.sort().join('\n')) {
    throw new Error('audit coverage differs from declared lockfiles and workspaces');
  }
  for (const path of policy.rust_lockfiles) {
    const report = reports[path];
    if (!report || !/^[a-f0-9]{40}$/.test(report.database?.['last-commit'] ?? '')
      || !(report.lockfile?.['dependency-count'] > 0)
      || !Array.isArray(report.settings?.ignore) || report.settings.ignore.length !== 0
      || !Array.isArray(report.settings.target_arch) || report.settings.target_arch.length !== 0
      || !Array.isArray(report.settings.target_os) || report.settings.target_os.length !== 0
      || report.settings.severity !== null
      || [...(report.settings.informational_warnings ?? [])].sort().join(',') !== 'notice,unmaintained,unsound'
      || !report.warnings || typeof report.warnings !== 'object' || Array.isArray(report.warnings)
      || !Array.isArray(report.vulnerabilities?.list)
      || report.vulnerabilities.count !== report.vulnerabilities.list.length
      || report.vulnerabilities.found !== (report.vulnerabilities.count > 0)) {
      throw new Error(`invalid or filtered Cargo audit report: ${path}`);
    }
    databases.push({ lockfile: path, commit: report.database['last-commit'] });
    for (const finding of report.vulnerabilities.list) failures.push(`${path}: vulnerability ${finding.advisory.id}`);
    for (const [kind, findings] of Object.entries(report.warnings)) {
      if (!Array.isArray(findings)) throw new Error(`invalid warning list: ${path}`);
      for (const finding of findings) {
        const matching = policy.warning_exceptions.filter(exception =>
          exception.advisory === finding.advisory?.id && exception.kind === kind
          && exception.package === finding.package?.name && exception.version === finding.package?.version
          && exception.lockfiles.includes(path));
        if (matching.length !== 1) {
          failures.push(`${path}: unreviewed ${kind} ${finding.advisory?.id} ${finding.package?.name}`);
          continue;
        }
        const exception = matching[0];
        if (!exception.owner || !exception.reason || !exception.dependency_chain?.length
          || !validDate(exception.expires) || !validDate(exception.reviewed)
          || exception.reviewed > today || exception.expires <= today || exception.expires <= exception.reviewed
          || Date.parse(exception.expires) - Date.parse(exception.reviewed) > 90 * 86_400_000) {
          failures.push(`${path}: expired or incomplete review ${exception.advisory}`);
        } else acceptedWarnings.push({ lockfile: path, ...exception });
      }
    }
  }
  for (const path of policy.npm_workspaces) {
    const report = reports[path];
    const vulnerabilities = report?.metadata?.vulnerabilities;
    if (report?.auditReportVersion !== 2 || !vulnerabilities
      || ['info', 'low', 'moderate', 'high', 'critical', 'total'].some(key =>
        !Number.isSafeInteger(vulnerabilities[key]) || vulnerabilities[key] < 0)
      || vulnerabilities.total !== ['info', 'low', 'moderate', 'high', 'critical'].reduce((sum, key) => sum + vulnerabilities[key], 0)
      || !report.vulnerabilities || typeof report.vulnerabilities !== 'object'
      || Object.keys(report.vulnerabilities).length !== vulnerabilities.total) {
      throw new Error(`invalid npm audit report: ${path}`);
    }
    if (vulnerabilities.high || vulnerabilities.critical) failures.push(`${path}: high or critical npm vulnerabilities`);
  }
  return { schema: 'volang.dependency-result.v1', passed: failures.length === 0, complete: true,
    generated_at: now.toISOString(), audited: expected, databases, accepted_warnings: acceptedWarnings, failures };
}

async function main() {
  const [input, output] = process.argv.slice(2);
  if (!input || !output) throw new Error('usage: check-dependency-policy.mjs <report-map.json> <result.json>');
  const policy = JSON.parse(await readFile(new URL('./dependency-policy.json', import.meta.url), 'utf8'));
  const paths = JSON.parse(await readFile(input, 'utf8'));
  const reports = Object.fromEntries(await Promise.all(Object.entries(paths).map(async ([key, path]) =>
    [key, JSON.parse(await readFile(path, 'utf8'))])));
  const result = checkDependencyPolicy(policy, reports);
  await mkdir(dirname(output), { recursive: true });
  await writeFile(output, JSON.stringify(result, null, 2) + '\n');
  console.log(`Dependency policy: ${result.audited.length} inputs, ${result.accepted_warnings.length} reviewed warnings, ${result.failures.length} failures`);
  if (!result.passed) { console.error(result.failures.join('\n')); process.exitCode = 1; }
}
if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main().catch(error => { console.error(error); process.exitCode = 1; });
}
