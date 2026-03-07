import { get } from 'svelte/store';
import { ide, consolePush, consolePushLines, consoleClear } from '../../stores/ide';
import { explorer } from '../../stores/explorer';
import { bridge } from '../bridge';
import { saveFile } from './fs';
import type { GuiCompileRunResult, GuiRunResult } from '../shell/protocol';

// =============================================================================
// Execution actions
// =============================================================================

function isGuiCode(code: string): boolean {
  return code.includes('vogui');
}

export async function runCode(): Promise<void> {
  const s = get(ide);

  if (s.dirty && s.activeFilePath) {
    await saveFile();
  }

  const entryPath = s.projectMode === 'multi'
    ? s.workspaceRoot + '/main.vo'
    : s.activeFilePath;

  if (!entryPath) return;

  let codeToCheck = s.code;
  if (s.projectMode === 'multi' && s.activeFilePath !== entryPath) {
    try {
      codeToCheck = await bridge().fsReadFile(entryPath);
    } catch {
      codeToCheck = '';
    }
  }

  consoleClear();
  const startTime = Date.now();
  ide.update(s => ({
    ...s,
    isRunning: true,
    runStatus: 'compiling',
    runDurationMs: null,
    guestRender: null,
    isGuiApp: false,
  }));

  try {
    const fileName = entryPath.split('/').pop() ?? entryPath;
    consolePush('system', `Compiling ${fileName}…`);

    if (isGuiCode(codeToCheck)) {
      const result = await bridge().shell.exec({ kind: 'gui.run', path: entryPath }) as GuiRunResult;
      const elapsed = Date.now() - startTime;
      consolePush('system', 'GUI app started');
      ide.update(s => ({
        ...s,
        isRunning: true,
        isGuiApp: true,
        guestRender: result.renderBytes,
        runStatus: 'running',
        runDurationMs: elapsed,
      }));
    } else {
      ide.update(s => ({ ...s, runStatus: 'running' }));
      const result = await bridge().shell.exec({ kind: 'gui.compileRun', path: entryPath }) as GuiCompileRunResult;
      const elapsed = Date.now() - startTime;
      consolePushLines('stdout', result.stdout);
      consolePush('success', `✓ Process exited`);
      ide.update(s => ({
        ...s,
        isRunning: false,
        isGuiApp: false,
        runStatus: 'done',
        runDurationMs: elapsed,
      }));
    }
  } catch (e: any) {
    const elapsed = Date.now() - startTime;
    const msg = String(e);
    consolePushLines('stderr', msg);
    consolePush('system', '✗ Failed');
    ide.update(s => ({
      ...s,
      isRunning: false,
      runStatus: 'error',
      runDurationMs: elapsed,
    }));
  }
}

export async function launchApp(): Promise<void> {
  explorer.update(e => ({ ...e, appMode: 'develop' }));
  ide.update(s => ({ ...s, outputExpanded: true }));
  await runCode();
}

export async function stopCode(): Promise<void> {
  try {
    await bridge().shell.exec({ kind: 'gui.stop' });
  } catch {
    // ignore errors on stop
  }
  consolePush('system', 'Stopped');
  ide.update(s => ({
    ...s,
    isRunning: false,
    isGuiApp: false,
    guestRender: null,
    runStatus: 'idle',
  }));
}
