const vscode = require('vscode');
const { LanguageClient } = require('vscode-languageclient/node');

let client;
let transitions = Promise.resolve();

function activate(context) {
  const output = vscode.window.createOutputChannel('Volang');
  const status = vscode.languages.createLanguageStatusItem('volang.server', { language: 'volang', scheme: 'file' });
  status.name = 'Volang language server';
  status.command = { command: 'volang.restartServer', title: 'Restart Volang language server' };
  const watchers = ['**/*.vo', '**/vo.mod', '**/vo.lock', '**/vo.work'].map(pattern => vscode.workspace.createFileSystemWatcher(pattern));
  context.subscriptions.push(output, status, ...watchers);

  async function restart() {
    if (client) {
      const previous = client;
      client = undefined;
      await previous.stop();
    }
    if (!vscode.workspace.isTrusted) {
      status.text = 'Vo';
      status.detail = 'Trust this workspace to enable compiler analysis';
      return;
    }
    const configuration = vscode.workspace.getConfiguration('volang');
    const command = configuration.get('server.path', 'vo').trim();
    const workspace = configuration.get('workspace', '').trim();
    const environment = { ...process.env };
    if (workspace) environment.VOWORK = workspace;
    status.text = 'Vo';
    status.busy = true;
    status.detail = 'Starting compiler analysis';
    const next = new LanguageClient('volang', 'Volang', {
      command, args: ['lsp', '--stdio'], options: { env: environment, shell: false },
    }, {
      documentSelector: [{ scheme: 'file', language: 'volang' }],
      synchronize: { fileEvents: watchers },
      diagnosticPullOptions: { onChange: true, onSave: true },
      middleware: {
        async provideDiagnostics(document, previousResultId, token, next) {
          const opened = document instanceof vscode.Uri
            ? vscode.workspace.textDocuments.find(item => item.uri.toString() === document.toString())
            : document;
          const version = opened?.version;
          const report = await next(document, previousResultId, token);
          if (token.isCancellationRequested || opened?.isClosed || opened?.version !== version) {
            throw new vscode.CancellationError();
          }
          return report;
        },
      },
      outputChannel: output,
    });
    client = next;
    try {
      await next.start();
      status.detail = 'Compiler completion, definitions, and diagnostics';
      status.severity = vscode.LanguageStatusSeverity.Information;
    } catch (error) {
      status.detail = 'Compiler unavailable; configure volang.server.path and restart';
      status.severity = vscode.LanguageStatusSeverity.Error;
      output.appendLine(String(error));
    } finally {
      status.busy = false;
    }
  }

  function scheduleRestart() {
    transitions = transitions.then(restart).catch(error => output.appendLine(String(error)));
    return transitions;
  }

  context.subscriptions.push(
    vscode.commands.registerCommand('volang.restartServer', scheduleRestart),
    vscode.workspace.onDidGrantWorkspaceTrust(scheduleRestart),
    vscode.workspace.onDidChangeConfiguration(event => {
      if (event.affectsConfiguration('volang.server.path') || event.affectsConfiguration('volang.workspace')) scheduleRestart();
    }),
    vscode.workspace.registerTextDocumentContentProvider('volang-source', {
      async provideTextDocumentContent(uri, token) {
        if (!client) throw new Error('Volang compiler is unavailable');
        return client.sendRequest('volang/source', { uri: uri.toString() }, token);
      },
    }),
  );
  return scheduleRestart();
}

async function deactivate() {
  await transitions;
  if (client) {
    const previous = client;
    client = undefined;
    await previous.stop();
  }
}

module.exports = { activate, deactivate };
