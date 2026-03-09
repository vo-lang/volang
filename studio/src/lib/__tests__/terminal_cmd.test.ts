/**
 * Tests for terminal_cmd.ts — uses a mock ShellClient to exercise every
 * command path and verify that renderResult formats data correctly.
 *
 * Run with: npm test
 */
import { describe, it, expect, beforeEach, vi } from 'vitest';
import { get } from 'svelte/store';
import { terminal, termInit } from '../../stores/terminal';
import { executeCommand } from '../terminal_cmd';
import { ShellError } from '../shell/protocol';
import type { ShellClient } from '../shell/client';
import type { ShellOp, ShellEvent } from '../shell/protocol';

// ── Polyfill crypto.randomUUID ─────────────────────────────────────────────
(globalThis as any).crypto ??= { randomUUID: () => '00000000-0000-4000-8000-000000000000' };

// =============================================================================
// Mock ShellClient factory
// =============================================================================

type MockExecMap = Partial<Record<string, (op: ShellOp) => unknown>>;
type MockStreamEmits = ShellEvent[];

function makeMockShell(
  execMap: MockExecMap = {},
  streamEmits: MockStreamEmits = [],
): ShellClient {
  return {
    cwd:           '/workspace',
    workspaceRoot: '/workspace',
    cd:            vi.fn(),
    setEnv:        vi.fn(),
    supports:      vi.fn().mockReturnValue(true),

    exec: vi.fn(async (op: ShellOp) => {
      const handler = execMap[op.kind];
      if (!handler) throw new ShellError('ERR_NOT_SUPPORTED', `mock: no handler for ${op.kind}`);
      return handler(op);
    }),

    stream: vi.fn(async function*(op: ShellOp) {
      for (const ev of streamEmits) yield ev;
    }),
  } as unknown as ShellClient;
}

// ── Helpers ───────────────────────────────────────────────────────────────────

const WS = '/workspace';

function getLines() {
  return get(terminal).lines.map(l => ({ kind: l.kind, text: l.text }));
}

function getOutputLines() {
  return get(terminal).lines
    .filter(l => l.kind === 'output' || l.kind === 'stream-out')
    .map(l => l.text);
}

function getErrorLines() {
  return get(terminal).lines
    .filter(l => l.kind === 'error')
    .map(l => l.text);
}

async function run(shell: ShellClient, cmd: string) {
  return executeCommand(cmd, shell, WS, () => {});
}

// ── Setup ────────────────────────────────────────────────────────────────────

beforeEach(() => {
  termInit(WS);
});

// =============================================================================
// Built-in commands
// =============================================================================

describe('built-ins', () => {
  it('clear clears lines', async () => {
    const shell = makeMockShell();
    termInit(WS);
    await run(shell, 'clear');
    expect(getLines()).toHaveLength(0);
  });

  it('pwd prints cwd', async () => {
    const shell = makeMockShell();
    await run(shell, 'pwd');
    expect(getOutputLines()).toContain('/workspace');
  });

  it('echo prints joined tokens', async () => {
    const shell = makeMockShell();
    await run(shell, 'echo hello world');
    expect(getOutputLines()).toContain('hello world');
  });

  it('cd returns newCwd', async () => {
    const shell = makeMockShell();
    const result = await run(shell, 'cd src');
    expect(result.newCwd).toBe('/workspace/src');
  });

  it('cd ~ goes to workspace root', async () => {
    const shell = makeMockShell();
    const result = await run(shell, 'cd ~');
    expect(result.newCwd).toBe(WS);
  });

  it('unknown command shows error', async () => {
    const shell = makeMockShell();
    await run(shell, 'foobar something');
    expect(getErrorLines()[0]).toMatch(/unknown command/);
  });

  it('empty command is no-op', async () => {
    const shell = makeMockShell();
    const result = await run(shell, '   ');
    expect(result).toEqual({});
    expect(getLines()).toHaveLength(0);
  });
});

// =============================================================================
// fs commands
// =============================================================================

describe('fs.list (ls)', () => {
  it('displays directories with trailing slash', async () => {
    const shell = makeMockShell({
      'fs.list': () => [
        { name: 'src', path: '/workspace/src', isDir: true },
        { name: 'main.vo', path: '/workspace/main.vo', isDir: false },
      ],
    });
    await run(shell, 'ls');
    const out = getOutputLines();
    expect(out.some(l => l.includes('src'))).toBe(true);
    expect(out.some(l => l.includes('main.vo'))).toBe(true);
  });

  it('resolves relative path against cwd', async () => {
    const shell = makeMockShell({ 'fs.list': () => [] });
    await run(shell, 'ls src');
    expect(shell.exec).toHaveBeenCalledWith(
      expect.objectContaining({ kind: 'fs.list', path: '/workspace/src' }),
    );
  });

  it('passes through absolute path', async () => {
    const shell = makeMockShell({ 'fs.list': () => [] });
    await run(shell, 'ls /workspace/src');
    expect(shell.exec).toHaveBeenCalledWith(
      expect.objectContaining({ kind: 'fs.list', path: '/workspace/src' }),
    );
  });
});

describe('fs.stat', () => {
  it('shows file info', async () => {
    const shell = makeMockShell({
      'fs.stat': () => ({ name: 'main.vo', path: '/workspace/main.vo', isDir: false, size: 1024 }),
    });
    await run(shell, 'stat main.vo');
    const out = getOutputLines().join('\n');
    expect(out).toMatch(/main\.vo/);
    expect(out).toMatch(/1024/);
  });
});

describe('fs.read (cat)', () => {
  it('displays raw string content', async () => {
    const shell = makeMockShell({
      'fs.read': () => 'package main\n\nfunc main() {}',
    });
    await run(shell, 'cat main.vo');
    const out = getOutputLines().join('\n');
    expect(out).toContain('package main');
  });
});

describe('fs.mkdir / fs.remove / fs.rename', () => {
  it('mkdir is silent on success', async () => {
    const shell = makeMockShell({ 'fs.mkdir': () => null });
    await run(shell, 'mkdir newdir');
    expect(getErrorLines()).toHaveLength(0);
  });

  it('rm is silent on success', async () => {
    const shell = makeMockShell({ 'fs.remove': () => null });
    await run(shell, 'rm file.txt');
    expect(getErrorLines()).toHaveLength(0);
  });

  it('rm -r passes recursive=true', async () => {
    const shell = makeMockShell({ 'fs.remove': () => null });
    await run(shell, 'rm -r olddir');
    expect(shell.exec).toHaveBeenCalledWith(
      expect.objectContaining({ kind: 'fs.remove', recursive: true }),
    );
  });
});

// =============================================================================
// git commands
// =============================================================================

describe('git status', () => {
  it('formats status items', async () => {
    const shell = makeMockShell({
      'git.status': () => [
        { path: 'main.vo', status: 'M' },
        { path: 'readme.md', status: 'A' },
      ],
    });
    await run(shell, 'git status');
    const out = getOutputLines().join('\n');
    expect(out).toContain('main.vo');
    expect(out).toContain('M');
  });

  it('clean tree shows message', async () => {
    const shell = makeMockShell({ 'git.status': () => [] });
    await run(shell, 'git status');
    expect(getOutputLines()[0]).toMatch(/nothing to commit/);
  });
});

describe('git log', () => {
  it('formats commits', async () => {
    const shell = makeMockShell({
      'git.log': () => [
        { id: 'abc123def456', summary: 'initial commit', authorName: 'Alice', timeUnix: 1700000000 },
      ],
    });
    await run(shell, 'git log');
    const out = getOutputLines().join('\n');
    expect(out).toMatch(/abc123de/);
    expect(out).toContain('initial commit');
    expect(out).toContain('Alice');
  });
});

describe('git branch (list)', () => {
  it('formats branch list with * for head', async () => {
    const shell = makeMockShell({
      'git.branch': () => [
        { name: 'main', isHead: true },
        { name: 'dev', isHead: false },
      ],
    });
    await run(shell, 'git branch');
    const out = getOutputLines().join('\n');
    expect(out).toMatch(/\* main/);
    expect(out).toContain('dev');
  });
});

describe('git branch create (via gitOneShot → raw string)', () => {
  it('BUG: git branch -b create returns raw string data, must show it', async () => {
    const shell = makeMockShell({
      'git.branch': () => 'Switched to new branch "feature"',
    });
    await run(shell, 'git branch -b feature');
    const out = getOutputLines().join('\n');
    expect(out).toContain('feature');
  });
});

describe('git add / commit / diff / init / checkout (gitOneShot → raw string)', () => {
  it('BUG: git commit returns raw string, must display it', async () => {
    const shell = makeMockShell({
      'git.commit': () => '[main abc1234] my message\n 1 file changed',
    });
    await run(shell, 'git commit -m "my message"');
    const out = getOutputLines().join('\n');
    expect(out).toContain('[main abc1234]');
  });

  it('BUG: git add returns raw string (empty = success)', async () => {
    const shell = makeMockShell({ 'git.add': () => '' });
    await run(shell, 'git add .');
    expect(getErrorLines()).toHaveLength(0);
  });

  it('BUG: git diff returns raw string, must display it', async () => {
    const shell = makeMockShell({ 'git.diff': () => 'diff --git a/main.vo b/main.vo\n+++ b/main.vo' });
    await run(shell, 'git diff');
    const out = getOutputLines().join('\n');
    expect(out).toContain('diff --git');
  });

  it('BUG: git init returns raw string, must display it', async () => {
    const shell = makeMockShell({ 'git.init': () => 'Initialized empty Git repository' });
    await run(shell, 'git init');
    const out = getOutputLines().join('\n');
    expect(out).toContain('Initialized');
  });
});

describe('git push / pull / clone (STREAMING)', () => {
  it('BUG: git push must use shell.stream(), not shell.exec()', async () => {
    const streamEmits: ShellEvent[] = [
      { jobId: 'j1', kind: 'stdout', line: 'Enumerating objects: 3' },
      { jobId: 'j1', kind: 'done', exitCode: 0 },
    ];
    const shell = makeMockShell({}, streamEmits);
    await run(shell, 'git push');
    expect(shell.stream).toHaveBeenCalled();
    expect(shell.exec).not.toHaveBeenCalled();
    const out = getLines().filter(l => l.kind === 'stream-out').map(l => l.text);
    expect(out).toContain('Enumerating objects: 3');
  });

  it('BUG: git pull must use shell.stream()', async () => {
    const streamEmits: ShellEvent[] = [
      { jobId: 'j1', kind: 'done', exitCode: 0 },
    ];
    const shell = makeMockShell({}, streamEmits);
    await run(shell, 'git pull');
    expect(shell.stream).toHaveBeenCalled();
  });

  it('BUG: git clone must use shell.stream()', async () => {
    const streamEmits: ShellEvent[] = [
      { jobId: 'j1', kind: 'done', exitCode: 0 },
    ];
    const shell = makeMockShell({}, streamEmits);
    await run(shell, 'git clone https://github.com/example/repo.git');
    expect(shell.stream).toHaveBeenCalled();
  });
});

// =============================================================================
// vo commands
// =============================================================================

describe('vo run', () => {
  it('displays stdout output', async () => {
    const shell = makeMockShell({
      'vo.run': () => ({ stdout: 'Hello, World!' }),
    });
    await run(shell, 'vo run main.vo');
    expect(getOutputLines().join('\n')).toContain('Hello, World!');
  });
});

describe('vo check', () => {
  it('shows "no errors" on clean file', async () => {
    const shell = makeMockShell({
      'vo.check': () => ({ ok: true, diags: [] }),
    });
    await run(shell, 'vo check main.vo');
    const sys = get(terminal).lines.filter(l => l.kind === 'system').map(l => l.text);
    expect(sys.some(t => t.includes('no errors'))).toBe(true);
  });

  it('shows errors when diags has entries', async () => {
    const shell = makeMockShell({
      'vo.check': () => ({
        ok: false,
        diags: [{ file: 'main.vo', line: 3, col: 5, message: 'type mismatch', severity: 'error' }],
      }),
    });
    await run(shell, 'vo check main.vo');
    const errs = getErrorLines();
    expect(errs.some(e => e.includes('type mismatch'))).toBe(true);
  });
});

describe('vo version', () => {
  it('BUG: shows version when data is raw string (WASM path)', async () => {
    const shell = makeMockShell({
      'vo.version': () => '0.1.0',
    });
    await run(shell, 'vo version');
    expect(getOutputLines().join('\n')).toContain('0.1.0');
  });

  it('shows version when data is {version: string} (Tauri path)', async () => {
    const shell = makeMockShell({
      'vo.version': () => ({ version: '0.1.0' }),
    });
    await run(shell, 'vo version');
    expect(getOutputLines().join('\n')).toContain('0.1.0');
  });
});

// =============================================================================
// zip commands
// =============================================================================

describe('zip list', () => {
  it('formats zip entries', async () => {
    const shell = makeMockShell({
      'zip.list': () => [
        { name: 'main.vo', size: 512, compressedSize: 200, isDir: false },
        { name: 'src/', size: 0, compressedSize: 0, isDir: true },
      ],
    });
    await run(shell, 'zip list archive.zip');
    const out = getOutputLines().join('\n');
    expect(out).toContain('main.vo');
    expect(out).toContain('src/');
  });
});

// =============================================================================
// Error handling
// =============================================================================

describe('shell errors', () => {
  it('ShellError is shown with code + message', async () => {
    const shell = makeMockShell({
      'fs.list': () => { throw new ShellError('ERR_NOT_FOUND', 'directory not found'); },
    });
    await run(shell, 'ls missing-dir');
    const errs = getErrorLines();
    expect(errs.some(e => e.includes('ERR_NOT_FOUND'))).toBe(true);
  });

  it('usage error shown for missing args', async () => {
    const shell = makeMockShell();
    await run(shell, 'cat');
    const errs = getErrorLines();
    expect(errs.some(e => e.includes('usage'))).toBe(true);
  });

  it('git commit without -m shows usage', async () => {
    const shell = makeMockShell();
    await run(shell, 'git commit');
    const errs = getErrorLines();
    expect(errs.some(e => e.includes('usage'))).toBe(true);
  });
});

// =============================================================================
// Path resolution
// =============================================================================

describe('path resolution', () => {
  it('~/path expands to workspaceRoot/path', async () => {
    const shell = makeMockShell({ 'fs.list': () => [] });
    await run(shell, 'ls ~/src');
    expect(shell.exec).toHaveBeenCalledWith(
      expect.objectContaining({ path: '/workspace/src' }),
    );
  });

  it('.. goes up one level', async () => {
    const shell = makeMockShell({ 'fs.list': () => [] });
    const result = await run(shell, 'cd ..');
    expect(result.newCwd).toBe('/');
  });
});
