const { mkdir, mkdtemp, writeFile, readFile, access } = require('node:fs/promises');
const { createHash } = require('node:crypto');
const { join, resolve } = require('node:path');
const { tmpdir } = require('node:os');
const { runTests } = require('@vscode/test-electron');

async function main() {
  const root = resolve(__dirname, '../../../..');
  const binary = resolve(process.env.VO_LSP_BINARY || join(root, 'target/debug', process.platform === 'win32' ? 'vo.exe' : 'vo'));
  await access(binary);
  const stage = join(root, 'target/ui-next/native-authoring-stage');
  await mkdir(stage, { recursive: true });
  const run = await mkdtemp(join(stage, 'vscode-'));
  // macOS Unix-domain socket paths are limited to 103 bytes. Keep the editor
  // profile short while the project itself exercises long Unicode paths.
  const profile = await mkdtemp(join(tmpdir(), 'vo-vsc-'));
  const workspace = join(run, 'Project 中文');
  await mkdir(join(workspace, '.vscode'), { recursive: true });
  await mkdir(join(workspace, 'lib'));
  await writeFile(join(workspace, 'vo.mod'), 'format = 1\nmodule = "local/editor"\nversion = "0.1.0"\nvo = "0.1.0"\n');
  await writeFile(join(workspace, 'main.vo'), 'package main\nfunc main() {}\n');
  await writeFile(join(workspace, 'lib/lib.vo'), 'package lib\nconst Value = 1\n');
  await writeFile(join(workspace, '.vscode/settings.json'), JSON.stringify({
    'volang.server.path': binary,
    'volang.workspace': 'off',
    'files.autoSave': 'off',
    'telemetry.telemetryLevel': 'off',
    'update.mode': 'none',
    'extensions.autoUpdate': false,
  }));
  const version = process.env.VO_VSCODE_VERSION || '1.90.2';
  const report = join(run, 'report.json');
  await runTests({
    version, cachePath: join(root, 'target/vscode-test'),
    extensionDevelopmentPath: resolve(process.env.VO_LSP_EXTENSION_PATH || join(__dirname, '..')),
    extensionTestsPath: join(__dirname, 'suite.cjs'),
    extensionTestsEnv: { VO_LSP_TEST_REPORT: report, VO_LSP_TEST_WORKSPACE: workspace, VOWORK: 'off' },
    launchArgs: [workspace, '--user-data-dir', join(profile, 'user'), '--extensions-dir', join(profile, 'extensions'),
      '--disable-extensions', '--skip-welcome', '--skip-release-notes', '--disable-workspace-trust'],
  });
  const result = JSON.parse(await readFile(report));
  result.compilerSha256 = createHash('sha256').update(await readFile(binary)).digest('hex');
  await writeFile(report, JSON.stringify(result, null, 2) + '\n');
  console.log(`VS Code authoring report: ${report}`);
}

main().catch(error => { console.error(error); process.exitCode = 1; });
