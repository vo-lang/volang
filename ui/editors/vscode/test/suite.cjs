const assert = require('node:assert/strict');
const { writeFile, readFile } = require('node:fs/promises');
const { join } = require('node:path');
const { createHash } = require('node:crypto');
const vscode = require('vscode');

async function waitFor(label, callback) {
  const deadline = Date.now() + 20000;
  let last;
  while (Date.now() < deadline) {
    last = await callback();
    if (last) return last;
    await new Promise(resolve => setTimeout(resolve, 60));
  }
  throw new Error(`Timed out: ${label}; last result ${JSON.stringify(last)}`);
}

async function replace(document, source) {
  const edit = new vscode.WorkspaceEdit();
  edit.replace(document.uri, new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)), source);
  assert(await vscode.workspace.applyEdit(edit));
}

async function definitions(document, needle) {
  return waitFor(`definition of ${needle}`, async () => {
    const offset = document.getText().lastIndexOf(needle);
    assert(offset >= 0);
    const items = await vscode.commands.executeCommand('vscode.executeDefinitionProvider', document.uri, document.positionAt(offset + 1));
    return items?.length ? items : null;
  });
}

async function run() {
  const started = Date.now();
  const workspace = process.env.VO_LSP_TEST_WORKSPACE;
  assert(workspace);
  const extension = vscode.extensions.getExtension('vo-lang.volang-ui-authoring');
  assert(extension);
  await extension.activate();
  const main = await vscode.workspace.openTextDocument(vscode.Uri.file(join(workspace, 'main.vo')));
  await vscode.window.showTextDocument(main);
  assert.equal(main.languageId, 'volang');

  await replace(main, 'package main\r\nimport "fmt"\r\nfunc main() { println("中文🙂"); fmt.Pr }\r\n');
  const completed = await waitFor('compiler member completion', async () => {
    const offset = main.getText().indexOf('fmt.Pr') + 6;
    const result = await vscode.commands.executeCommand('vscode.executeCompletionItemProvider', main.uri, main.positionAt(offset));
    return result?.items.find(item => item.label === 'Println');
  });
  assert.equal(main.getText(completed.range), 'Pr');
  assert.equal(completed.insertText, 'Println');

  await replace(main, 'package main\nimport "fmt"\nfunc main() { fmt.Println("中文🙂") }\n');
  const [stdlib] = await definitions(main, 'Println');
  assert.equal(stdlib.uri.scheme, 'volang-source');
  const readonly = await vscode.workspace.openTextDocument(stdlib.uri);
  assert.equal(readonly.getText(stdlib.range), 'Println');
  assert(readonly.getText().includes('func Println'));

  await replace(main, 'package main\nimport "fmt"\nfunc main() {}\n');
  await waitFor('successful import warning', () => vscode.languages.getDiagnostics(main.uri).some(item => item.severity === vscode.DiagnosticSeverity.Warning));
  await replace(main, 'package main\nfunc main() { println("中文🙂"); missingName() }\n');
  const diagnostic = await waitFor('source error range', () => vscode.languages.getDiagnostics(main.uri).find(item => item.message.includes('missingName')));
  assert.equal(main.getText(diagnostic.range), 'missingName');

  const stalePublications = [];
  let latestVersion;
  const subscription = vscode.languages.onDidChangeDiagnostics(event => {
    if (latestVersion === main.version && event.uris.some(uri => uri.toString() === main.uri.toString())) {
      stalePublications.push(...vscode.languages.getDiagnostics(main.uri).filter(item => item.message.includes('obsoleteName')));
    }
  });
  try {
    for (let index = 0; index < 4; index++) {
      await replace(main, `package main\nfunc main() { obsoleteName${index}() }\n`);
    }
    await replace(main, 'package main\nfunc main() { latestName() }\n');
    latestVersion = main.version;
    await waitFor('latest rapid edit diagnostics', () => vscode.languages.getDiagnostics(main.uri).some(item => item.message.includes('latestName')));
    assert.deepEqual(stalePublications, [], 'canceled reports must not publish after the latest edit');
  } finally {
    subscription.dispose();
  }

  const library = await vscode.workspace.openTextDocument(vscode.Uri.file(join(workspace, 'lib/lib.vo')));
  await vscode.window.showTextDocument(library);
  await replace(library, 'package lib\n// unsaved 中文🙂\nconst Changed = 3\n');
  await replace(main, 'package main\nimport "local/editor/lib"\nfunc main() { println(lib.Changed) }\n');
  const [local] = await definitions(main, 'Changed');
  assert.equal(local.uri.toString(), library.uri.toString());
  assert.equal(library.getText(local.range), 'Changed');
  await waitFor('repaired diagnostics', () => vscode.languages.getDiagnostics(main.uri).length === 0);

  await vscode.window.showTextDocument(library);
  await vscode.commands.executeCommand('workbench.action.revertAndCloseActiveEditor');
  await waitFor('closed import restores disk contents', () => vscode.languages.getDiagnostics(main.uri).some(item => item.message.includes('Changed')));
  await vscode.commands.executeCommand('volang.restartServer');
  await replace(main, 'package main\nimport "fmt"\nfunc main() { fmt.Println(42) }\n');
  const [afterRestart] = await definitions(main, 'Println');
  assert.equal(afterRestart.uri.scheme, 'volang-source');
  await waitFor('restart and repair clear errors', () => vscode.languages.getDiagnostics(main.uri).length === 0);
  assert.equal(await readFile(join(workspace, 'main.vo'), 'utf8'), 'package main\nfunc main() {}\n');
  assert.equal(await readFile(join(workspace, 'lib/lib.vo'), 'utf8'), 'package lib\nconst Value = 1\n');
  await writeFile(process.env.VO_LSP_TEST_REPORT, JSON.stringify({
    version: 1, vscode: vscode.version, platform: process.platform,
    extension: {
      path: extension.extensionPath,
      entrySha256: createHash('sha256').update(await readFile(join(extension.extensionPath, extension.packageJSON.main))).digest('hex'),
      manifestSha256: createHash('sha256').update(await readFile(join(extension.extensionPath, 'package.json'))).digest('hex'),
    },
    checks: { completion: true, unicodeRanges: true, stdlibSource: true, warnings: true,
      diagnostics: true, rapidEdits: true, unsavedImport: true, closeRecovery: true, restart: true, unchangedDisk: true },
    elapsedMs: Date.now() - started,
  }, null, 2));
}

module.exports = { run };
