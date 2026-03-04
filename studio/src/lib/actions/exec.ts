import { get } from 'svelte/store';
import { ide, consolePush, consolePushLines, consoleClear } from '../../stores/ide';
import { bridge } from '../bridge';
import { saveFile } from './fs';

// =============================================================================
// Execution actions
// =============================================================================

function isGuiCode(code: string): boolean {
  return code.includes('"vogui"');
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

  const fileName = entryPath.split('/').pop() ?? entryPath;
  consoleClear();
  consolePush('system', `Compiling ${fileName}…`);
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
    if (isGuiCode(codeToCheck)) {
      const bytes = await bridge().runGui(entryPath);
      const elapsed = Date.now() - startTime;
      consolePush('system', 'GUI app started');
      ide.update(s => ({
        ...s,
        isRunning: true,
        isGuiApp: true,
        guestRender: bytes,
        runStatus: 'running',
        runDurationMs: elapsed,
      }));
    } else {
      ide.update(s => ({ ...s, runStatus: 'running' }));
      const stdout = await bridge().compileRun(entryPath);
      const elapsed = Date.now() - startTime;
      consolePushLines('stdout', stdout);
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

export async function stopCode(): Promise<void> {
  try {
    await bridge().stopGui();
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
