import type { ShellClient } from './shell/client';
import type { ShellEvent, ShellOp, VoCheckDiag, FsGrepMatch } from './shell/protocol';
import { ShellError } from './shell/protocol';
import {
  termPush, termClear, termSetBusy, termPushHistory,
  type TermLineKind,
} from '../stores/terminal';
import {
  C, humanSize, fileColor,
  formatFsListResult, formatStatResult,
  formatGitStatus, formatGitLog, formatGitBranch,
  formatZipList, formatGrepMatches,
} from './terminal_fmt';

// =============================================================================
// Terminal command executor — maps shell-like syntax to ShellOps
// =============================================================================

export interface ExecResult {
  lines: Array<{ kind: TermLineKind; text: string }>;
  newCwd?: string;
}

// Parse a raw command string into tokens, respecting single/double-quoted strings.
function tokenize(raw: string): string[] {
  const tokens: string[] = [];
  let cur = '';
  let quote: '"' | "'" | null = null;
  for (let i = 0; i < raw.length; i++) {
    const ch = raw[i];
    if (quote) {
      if (ch === quote) { quote = null; }
      else { cur += ch; }
    } else if (ch === '"' || ch === "'") {
      quote = ch;
    } else if (ch === ' ' || ch === '\t') {
      if (cur) { tokens.push(cur); cur = ''; }
    } else {
      cur += ch;
    }
  }
  if (cur) tokens.push(cur);
  return tokens;
}

// Resolve a user-typed path against the current cwd.
function resolvePath(cwd: string, p: string, workspaceRoot: string): string {
  if (p === '~' || p === '~/') return workspaceRoot;
  if (p.startsWith('~/')) return workspaceRoot + '/' + p.slice(2);
  if (p.startsWith('/')) return p;
  if (p === '..') return cwd.substring(0, cwd.lastIndexOf('/')) || '/';
  if (p === '.') return cwd;
  return cwd.endsWith('/') ? cwd + p : cwd + '/' + p;
}

// Display cwd relative to workspaceRoot for prompt readability.
export function displayCwd(cwd: string, workspaceRoot: string): string {
  if (cwd === workspaceRoot) return '~';
  if (cwd.startsWith(workspaceRoot + '/')) return '~/' + cwd.slice(workspaceRoot.length + 1);
  return cwd;
}

// =============================================================================
// Render HTTP response body: JSON pretty-print or plain text.
function renderHttpBody(headers: string[], body: string): void {
  const ct = headers.find(h => h.toLowerCase().startsWith('content-type:'))?.split(':').slice(1).join(':').trim() ?? '';
  if (ct.includes('application/json')) {
    try {
      const pretty = JSON.stringify(JSON.parse(body), null, 2);
      pretty.split('\n').forEach(l => termPush('output', l));
      return;
    } catch { /* fall through */ }
  }
  body.split('\n').slice(0, 500).forEach(l => termPush('output', l));
}

// Async tree / find / grep helpers (use existing shell ops)
// =============================================================================

async function buildTree(
  shell: ShellClient,
  dirPath: string,
  prefix: string,
  depth: number,
  maxDepth: number,
): Promise<string[]> {
  if (depth > maxDepth) return [`${prefix}    ${C.dim}…${C.reset}`];
  let entries: Array<{ name: string; isDir: boolean }>;
  try {
    entries = (await shell.exec({ kind: 'fs.list', path: dirPath })) as Array<{ name: string; isDir: boolean }>;
  } catch {
    return [`${prefix}    ${C.err}(unreadable)${C.reset}`];
  }
  const sorted = [...entries].sort((a, b) => Number(b.isDir) - Number(a.isDir) || a.name.localeCompare(b.name));
  const lines: string[] = [];
  for (let i = 0; i < sorted.length; i++) {
    const e = sorted[i];
    const last = i === sorted.length - 1;
    const connector = last ? '└── ' : '├── ';
    const col = fileColor(e.name, e.isDir);
    const label = e.isDir ? e.name + '/' : e.name;
    lines.push(`${prefix}${C.dim}${connector}${C.reset}${col}${label}${C.reset}`);
    if (e.isDir) {
      const childPrefix = prefix + (last ? '    ' : `${C.dim}│${C.reset}   `);
      const childPath   = dirPath.replace(/\/$/, '') + '/' + e.name;
      lines.push(...(await buildTree(shell, childPath, childPrefix, depth + 1, maxDepth)));
    }
  }
  return lines;
}

async function findFiles(
  shell: ShellClient,
  dirPath: string,
  nameRe: RegExp | null,
  typeFilter: 'f' | 'd' | null,
  results: string[],
  depth: number,
  maxDepth: number,
): Promise<void> {
  if (depth > maxDepth) return;
  let entries: Array<{ name: string; isDir: boolean }>;
  try {
    entries = (await shell.exec({ kind: 'fs.list', path: dirPath })) as Array<{ name: string; isDir: boolean }>;
  } catch {
    return;
  }
  for (const e of entries) {
    const fullPath = dirPath.replace(/\/$/, '') + '/' + e.name;
    const matchName = !nameRe || nameRe.test(e.name);
    const matchType = !typeFilter || (typeFilter === 'd' ? e.isDir : !e.isDir);
    if (matchName && matchType) results.push(fullPath);
    if (e.isDir) await findFiles(shell, fullPath, nameRe, typeFilter, results, depth + 1, maxDepth);
  }
}

// =============================================================================
// Main command dispatch
// =============================================================================

export async function executeCommand(
  raw: string,
  shell: ShellClient,
  workspaceRoot: string,
  onEvent: (ev: ShellEvent) => void,
): Promise<{ newCwd?: string; guiPath?: string }> {
  const trimmed = raw.trim();
  if (!trimmed) return {};

  termPushHistory(trimmed);
  const tokens = tokenize(trimmed);
  const cmd = tokens[0]?.toLowerCase() ?? '';

  // Built-ins that don't touch the shell
  if (cmd === 'clear') {
    termClear();
    return {};
  }

  if (cmd === 'pwd') {
    termPush('output', shell.cwd);
    return {};
  }

  if (cmd === 'echo') {
    termPush('output', tokens.slice(1).join(' '));
    return {};
  }

  if (cmd === 'cd') {
    const target = tokens[1] ?? workspaceRoot;
    const newCwd = resolvePath(shell.cwd, target, workspaceRoot);
    return { newCwd };
  }

  if (cmd === 'help') {
    printHelp();
    return {};
  }

  // ── Client-side built-ins (use existing shell ops) ──────────────────────────

  if (cmd === 'touch') {
    if (!tokens[1]) { termPush('error', 'usage: touch <path>'); return {}; }
    const abs = resolvePath(shell.cwd, tokens[1], workspaceRoot);
    termSetBusy(true);
    try {
      await shell.exec({ kind: 'fs.write', path: abs, content: '' });
    } catch (e) {
      // If file already exists that's fine — touch is idempotent
      if (!String(e).includes('ALREADY_EXISTS')) termPush('error', String(e));
    } finally { termSetBusy(false); }
    return {};
  }

  if (cmd === 'head' || cmd === 'tail') {
    let n = 10;
    let pathArg: string | undefined;
    for (let i = 1; i < tokens.length; i++) {
      if ((tokens[i] === '-n' || tokens[i] === '-N') && tokens[i + 1]) { n = parseInt(tokens[++i]); }
      else if (!pathArg) pathArg = tokens[i];
    }
    if (!pathArg) { termPush('error', `usage: ${cmd} [-n N] <path>`); return {}; }
    const abs = resolvePath(shell.cwd, pathArg, workspaceRoot);
    termSetBusy(true);
    try {
      const content = (await shell.exec({ kind: 'fs.read', path: abs })) as string;
      const lines   = content.split('\n');
      const slice   = cmd === 'head' ? lines.slice(0, n) : lines.slice(Math.max(0, lines.length - n));
      slice.forEach(l => termPush('output', l));
    } catch (e) { termPush('error', String(e)); }
    finally { termSetBusy(false); }
    return {};
  }

  if (cmd === 'wc') {
    if (!tokens[1]) { termPush('error', 'usage: wc <path>'); return {}; }
    const abs = resolvePath(shell.cwd, tokens[1], workspaceRoot);
    termSetBusy(true);
    try {
      const content = (await shell.exec({ kind: 'fs.read', path: abs })) as string;
      const lines = content.split('\n').length;
      const words = content.split(/\s+/).filter(Boolean).length;
      const chars = content.length;
      termPush('output', `${C.size}${String(lines).padStart(7)} ${String(words).padStart(7)} ${String(chars).padStart(7)}${C.reset}  ${tokens[1]}`);
    } catch (e) { termPush('error', String(e)); }
    finally { termSetBusy(false); }
    return {};
  }

  if (cmd === 'tree') {
    const dirArg  = tokens[1] ? resolvePath(shell.cwd, tokens[1], workspaceRoot) : shell.cwd;
    const maxD    = parseInt(tokens[tokens.indexOf('-L') + 1] ?? tokens[tokens.indexOf('--depth') + 1] ?? '4') || 4;
    const rel     = displayCwd(dirArg, workspaceRoot);
    termSetBusy(true);
    try {
      termPush('output', `${C.dir}${rel}/${C.reset}`);
      const lines = await buildTree(shell, dirArg, '', 0, maxD);
      lines.forEach(l => termPush('output', l));
    } catch (e) { termPush('error', String(e)); }
    finally { termSetBusy(false); }
    return {};
  }

  if (cmd === 'http') {
    const sub = tokens[1]?.toLowerCase();
    const url  = tokens[2];
    const HTTP_METHODS = ['get', 'post', 'put', 'patch', 'delete', 'head'];
    if (!sub || !url) {
      termPush('error', `usage: http <${HTTP_METHODS.join('|')}> <url> [-H "Key: Val"] [-d body] [-j]`);
      return {};
    }
    if (!HTTP_METHODS.includes(sub)) {
      termPush('error', `unknown method: ${sub}  (${HTTP_METHODS.join(', ')})`);
      return {};
    }
    const reqHeaders: Record<string, string> = {};
    let body: string | undefined;
    let corsProxy: string | undefined;
    for (let i = 3; i < tokens.length; i++) {
      if (tokens[i] === '-H' && tokens[i + 1]) {
        const raw = tokens[++i];
        const colon = raw.indexOf(':');
        if (colon > 0) reqHeaders[raw.slice(0, colon).trim()] = raw.slice(colon + 1).trim();
      } else if ((tokens[i] === '-d' || tokens[i] === '--data') && tokens[i + 1]) {
        body = tokens[++i];
      } else if (tokens[i] === '-j' || tokens[i] === '--json') {
        reqHeaders['Content-Type'] = 'application/json';
        reqHeaders['Accept']       = 'application/json';
      } else if (tokens[i] === '--proxy' && tokens[i + 1]) {
        corsProxy = tokens[++i];
      }
    }
    termSetBusy(true);
    const t0 = Date.now();
    try {
      if (shell.supports('http')) {
        // Tauri native path: native ureq, no CORS restriction
        const op: import('./shell/protocol').HttpOp = {
          kind: `http.${sub}` as any,
          url,
          ...(Object.keys(reqHeaders).length ? { headers: reqHeaders } : {}),
          ...(body ? { body } : {}),
        };
        const data = await shell.exec(op) as any;
        const elapsed = Date.now() - t0;
        const sc: number = data?.statusCode ?? 0;
        const statusCol = sc >= 200 && sc < 300 ? C.ok : sc >= 400 ? C.err : C.warn;
        termPush('output', `${statusCol}${sc} ${data?.status ?? ''}${C.reset}  ${C.dim}${elapsed}ms${C.reset}  ${C.dim}${url}${C.reset}`);
        if (sub === 'head') {
          ((data?.headers ?? []) as string[]).forEach(h => {
            const colon = h.indexOf(':');
            if (colon > 0) termPush('output', `${C.dim}${h.slice(0, colon)}:${C.reset}${h.slice(colon + 1)}`);
            else termPush('output', h);
          });
        } else {
          renderHttpBody(data?.headers ?? [], data?.body ?? '');
        }
      } else {
        // Browser fetch path: subject to CORS
        const fetchUrl = corsProxy ? corsProxy + encodeURIComponent(url) : url;
        if (corsProxy) termPush('system', `${C.dim}via proxy: ${corsProxy}${C.reset}`);
        const init: RequestInit = { method: sub.toUpperCase(), headers: reqHeaders };
        if (body) init.body = body;
        const resp = await fetch(fetchUrl, init);
        const elapsed = Date.now() - t0;
        const statusCol = resp.ok ? C.ok : resp.status >= 400 ? C.err : C.warn;
        termPush('output', `${statusCol}${resp.status} ${resp.statusText}${C.reset}  ${C.dim}${elapsed}ms${C.reset}  ${C.dim}${url}${C.reset}`);
        if (sub === 'head') {
          resp.headers.forEach((v, k) => termPush('output', `${C.dim}${k}:${C.reset} ${v}`));
        } else {
          const ct = resp.headers.get('content-type') ?? '';
          const headerLines = [`content-type: ${ct}`];
          const text = await resp.text();
          renderHttpBody(headerLines, text);
        }
      }
    } catch (e) {
      const msg = String(e);
      termPush('error', `${msg}`);
      if (!shell.supports('http') &&
          (msg.includes('Failed to fetch') || msg.includes('NetworkError') || msg.toLowerCase().includes('cors'))) {
        termPush('system', `${C.warn}CORS:${C.reset} browser blocks cross-origin requests to this server.`);
        termPush('system', `${C.dim}Use --proxy <proxyUrl> to route through a CORS proxy.${C.reset}`);
        termPush('system', `${C.dim}In Tauri desktop mode, http uses native ureq with no CORS restriction.${C.reset}`);
      }
    } finally {
      termSetBusy(false);
    }
    return {};
  }

  if (cmd === 'find') {
    let dirArg = shell.cwd;
    let nameGlob: string | null = null;
    let typeFilter: 'f' | 'd' | null = null;
    for (let i = 1; i < tokens.length; i++) {
      if (tokens[i] === '-name' && tokens[i + 1]) { nameGlob = tokens[++i]; }
      else if (tokens[i] === '-type' && tokens[i + 1]) { typeFilter = tokens[++i] as 'f' | 'd'; }
      else if (!tokens[i].startsWith('-')) dirArg = resolvePath(shell.cwd, tokens[i], workspaceRoot);
    }
    const nameRe = nameGlob
      ? new RegExp('^' + nameGlob.replace(/[.+^${}()|[\]\\]/g, '\\$&').replace(/\*/g, '.*').replace(/\?/g, '.') + '$')
      : null;
    termSetBusy(true);
    try {
      const results: string[] = [];
      await findFiles(shell, dirArg, nameRe, typeFilter, results, 0, 8);
      if (results.length === 0) { termPush('system', 'no matches'); }
      else results.forEach(p => termPush('output', `${fileColor(p, p.endsWith('/'))}${p}${C.reset}`));
    } catch (e) { termPush('error', String(e)); }
    finally { termSetBusy(false); }
    return {};
  }

  // Build a ShellOp from tokens
  let op: ShellOp;
  let streaming: boolean;

  try {
    const built = buildOp(cmd, tokens, shell.cwd, workspaceRoot);
    if (!built) {
      termPush('error', `unknown command: ${cmd}. Type 'help' for available commands.`);
      return {};
    }
    [op, streaming] = built;
  } catch (e) {
    termPush('error', `${e}`);
    return {};
  }

  termSetBusy(true);
  const t0 = Date.now();

  try {
    if (streaming) {
      for await (const ev of shell.stream(op)) {
        onEvent(ev);
        if (ev.kind === 'stdout') termPush('stream-out', ev.line);
        else if (ev.kind === 'stderr') termPush('stream-err', ev.line);
        else if (ev.kind === 'done') {
          const elapsed = Date.now() - t0;
          termPush('system', `exited ${ev.exitCode} · ${elapsed}ms`);
        } else if (ev.kind === 'fail') {
          termPush('error', ev.message);
        }
      }
    } else {
      const data = await shell.exec(op);
      renderResult(op, data);
      const elapsed = Date.now() - t0;
      if (elapsed > 200) termPush('system', `${elapsed}ms`);
    }
  } catch (e) {
    if (e instanceof ShellError) {
      if (e.code === 'ERR_NOT_SUPPORTED' && e.message.includes('GUI program') && op?.kind === 'vo.run') {
        return { guiPath: (op as any).path as string };
      }
      termPush('error', `${e.code}: ${e.message}`);
    } else {
      termPush('error', String(e));
    }
  } finally {
    termSetBusy(false);
  }

  return {};
}

// Build a ShellOp from tokenized input.
function buildOp(
  cmd: string,
  tokens: string[],
  cwd: string,
  workspaceRoot: string,
): [ShellOp, boolean] | null {
  const res = (p: string) => resolvePath(cwd, p, workspaceRoot);

  switch (cmd) {
    // ── fs ──────────────────────────────────────────────────────────────────
    case 'ls':
    case 'dir':
      return [{ kind: 'fs.list', path: res(tokens[1] ?? '.') }, false];

    case 'cat':
    case 'type':
      if (!tokens[1]) throw new Error('usage: cat <path>');
      return [{ kind: 'fs.read', path: res(tokens[1]) }, false];

    case 'stat':
      if (!tokens[1]) throw new Error('usage: stat <path>');
      return [{ kind: 'fs.stat', path: res(tokens[1]) }, false];

    case 'mkdir': {
      let path = tokens[1];
      let recursive = false;
      if (path === '-p') { recursive = true; path = tokens[2]; }
      if (!path) throw new Error('usage: mkdir [-p] <path>');
      return [{ kind: 'fs.mkdir', path: res(path), recursive }, false];
    }

    case 'rm':
    case 'del': {
      let path = tokens[1];
      let recursive = false;
      if (path === '-r' || path === '-rf') { recursive = true; path = tokens[2]; }
      if (!path) throw new Error('usage: rm [-r] <path>');
      return [{ kind: 'fs.remove', path: res(path), recursive }, false];
    }

    case 'mv':
    case 'move':
      if (!tokens[1] || !tokens[2]) throw new Error('usage: mv <old> <new>');
      return [{ kind: 'fs.rename', oldPath: res(tokens[1]), newPath: res(tokens[2]) }, false];

    case 'cp':
    case 'copy':
      if (!tokens[1] || !tokens[2]) throw new Error('usage: cp <src> <dst>');
      return [{ kind: 'fs.copy', src: res(tokens[1]), dst: res(tokens[2]) }, false];

    case 'grep': {
      let caseInsensitive = false;
      let recursive       = false;
      let fixedString     = false;
      let rawPat: string | undefined;
      const pathArgs: string[] = [];
      for (let i = 1; i < tokens.length; i++) {
        const t = tokens[i];
        if (t === '-i') { caseInsensitive = true; }
        else if (t === '-r' || t === '-R') { recursive = true; }
        else if (t === '-F') { fixedString = true; }
        else if (t === '-ri' || t === '-ir') { recursive = caseInsensitive = true; }
        else if (!rawPat) rawPat = t;
        else pathArgs.push(t);
      }
      if (!rawPat) throw new Error('usage: grep [-i] [-r] [-F] <pattern> <path...>');
      const target = pathArgs.length ? res(pathArgs[0]) : cwd;
      return [{ kind: 'fs.grep', path: target, pattern: rawPat, recursive, caseInsensitive, fixedString }, false];
    }

    // ── git ─────────────────────────────────────────────────────────────────
    case 'git': {
      const sub = tokens[1]?.toLowerCase();
      switch (sub) {
        case 'status':
          return [{ kind: 'git.status' }, false];
        case 'log': {
          const limitIdx = tokens.indexOf('--limit');
          const limit = limitIdx >= 0 ? parseInt(tokens[limitIdx + 1] ?? '20') : 20;
          return [{ kind: 'git.log', limit }, false];
        }
        case 'add': {
          const paths = tokens.slice(2);
          if (!paths.length) throw new Error('usage: git add <paths...>');
          return [{ kind: 'git.add', paths: paths.map(res) }, false];
        }
        case 'commit': {
          const mIdx = tokens.indexOf('-m');
          if (mIdx < 0 || !tokens[mIdx + 1]) throw new Error('usage: git commit -m <message>');
          const amend = tokens.includes('--amend');
          return [{ kind: 'git.commit', message: tokens[mIdx + 1], amend }, false];
        }
        case 'push': {
          const remote = tokens[2] ?? undefined;
          const branch = tokens[3] ?? undefined;
          const force  = tokens.includes('-f') || tokens.includes('--force');
          return [{ kind: 'git.push', remote, branch, force }, true];
        }
        case 'pull': {
          const remote = tokens[2] ?? undefined;
          const branch = tokens[3] ?? undefined;
          return [{ kind: 'git.pull', remote, branch }, true];
        }
        case 'diff':
          return [{ kind: 'git.diff', staged: tokens.includes('--staged') }, false];
        case 'checkout': {
          if (!tokens[2]) throw new Error('usage: git checkout [-b] <branch>');
          const create = tokens[2] === '-b';
          const branch = create ? tokens[3] : tokens[2];
          if (!branch) throw new Error('usage: git checkout [-b] <branch>');
          return [{ kind: 'git.checkout', branch, create }, false];
        }
        case 'branch': {
          const dIdx = tokens.indexOf('-d');
          if (dIdx >= 0) {
            const del = tokens[dIdx + 1];
            if (!del) throw new Error('usage: git branch -d <name>');
            return [{ kind: 'git.branch', delete: del }, false];
          }
          const bIdx = tokens.indexOf('-b');
          const create = bIdx >= 0 ? tokens[bIdx + 1] : undefined;
          return [{ kind: 'git.branch', create }, false];
        }
        case 'clone': {
          if (!tokens[2]) throw new Error('usage: git clone <url> [dest]');
          const dest = tokens[3] ? res(tokens[3]) : res(tokens[2].split('/').pop()!.replace(/\.git$/, ''));
          return [{ kind: 'git.clone', url: tokens[2], destPath: dest }, true];
        }
        case 'init':
          return [{ kind: 'git.init' }, false];
        default:
          throw new Error(`unknown git subcommand: ${sub ?? '(none)'}. Try: status, log, add, commit, push, pull, diff, checkout, branch, clone, init`);
      }
    }

    // ── vo ──────────────────────────────────────────────────────────────────
    case 'vo': {
      const sub = tokens[1]?.toLowerCase();
      switch (sub) {
        case 'run': {
          if (!tokens[2]) throw new Error('usage: vo run <path>');
          return [{ kind: 'vo.run', path: res(tokens[2]) }, false];
        }
        case 'check': {
          if (!tokens[2]) throw new Error('usage: vo check <path>');
          return [{ kind: 'vo.check', path: res(tokens[2]) }, false];
        }
        case 'build': {
          if (!tokens[2]) throw new Error('usage: vo build <path>');
          return [{ kind: 'vo.build', path: res(tokens[2]) }, false];
        }
        case 'init':
          return [{ kind: 'vo.init' }, false];
        case 'version':
          return [{ kind: 'vo.version' }, false];
        default:
          throw new Error(`unknown vo subcommand: ${sub ?? '(none)'}. Try: run, check, build, init, version`);
      }
    }

    // ── zip ─────────────────────────────────────────────────────────────────
    case 'zip': {
      const sub = tokens[1]?.toLowerCase();
      switch (sub) {
        case 'pack': {
          if (tokens.length < 4) throw new Error('usage: zip pack <output.zip> <files...>');
          return [{ kind: 'zip.pack', output: res(tokens[2]), inputs: tokens.slice(3).map(res) }, false];
        }
        case 'unpack': {
          if (!tokens[2]) throw new Error('usage: zip unpack <archive.zip> [destDir]');
          return [{ kind: 'zip.unpack', archive: res(tokens[2]), outputDir: res(tokens[3] ?? '.') }, false];
        }
        case 'list': {
          if (!tokens[2]) throw new Error('usage: zip list <archive.zip>');
          return [{ kind: 'zip.list', archive: res(tokens[2]) }, false];
        }
        default:
          throw new Error(`unknown zip subcommand: ${sub ?? '(none)'}. Try: pack, unpack, list`);
      }
    }

    // ── run (proc.spawn) ───────────────────────────────────────────────────
    case 'run': {
      if (!tokens[1]) throw new Error('usage: run <program> [args...]');
      return [{ kind: 'proc.spawn', program: tokens[1], args: tokens.slice(2) }, true];
    }

    default:
      return null;
  }
}

// Render the result of a non-streaming op to the terminal.
function renderResult(
  op: ShellOp,
  data: unknown,
): void {
  if (data == null) return;

  switch (op.kind) {
    case 'fs.list':
      for (const line of formatFsListResult(data)) termPush('output', line);
      break;

    case 'fs.stat':
      for (const line of formatStatResult(data)) termPush('output', line);
      break;

    case 'fs.read':
      termPush('output', String(data));
      break;

    case 'fs.readMany': {
      const entries = data as Array<{ path: string; content?: string; error?: string }>;
      for (const e of entries) {
        if ('error' in e) {
          termPush('error', `${e.path}: ${e.error}`);
        } else {
          termPush('output', `${C.dim}==> ${e.path} <==${C.reset}`);
          termPush('output', e.content ?? '');
        }
      }
      break;
    }

    case 'fs.grep': {
      const matches = data as FsGrepMatch[];
      if (!matches || matches.length === 0) {
        termPush('system', 'no matches');
      } else {
        const paths = new Set(matches.map(m => m.path));
        for (const line of formatGrepMatches(matches, paths.size > 1)) termPush('output', line);
      }
      break;
    }

    case 'fs.write':
    case 'fs.mkdir':
    case 'fs.remove':
    case 'fs.rename':
    case 'fs.copy':
      // Silent on success (Unix convention)
      break;

    case 'git.status':
      for (const line of formatGitStatus(data)) termPush('output', line);
      break;

    case 'git.log':
      for (const line of formatGitLog(data)) termPush('output', line);
      break;

    case 'git.branch':
      // list → array of {name, isHead}; create/delete → raw string from gitOneShot
      if (Array.isArray(data)) {
        for (const line of formatGitBranch(data)) termPush('output', line);
      } else if (typeof data === 'string' && data) {
        termPush('output', data);
      }
      break;

    case 'git.add':
    case 'git.commit':
    case 'git.diff':
    case 'git.checkout':
    case 'git.init': {
      // gitOneShot returns a raw string (combined CLI output)
      if (typeof data === 'string') {
        if (data) termPush('output', data);
      } else {
        const s = data as Record<string, unknown>;
        if (s?.output) termPush('output', String(s.output));
        else if (s?.message) termPush('output', String(s.message));
      }
      break;
    }

    case 'vo.run': {
      const s = data as { stdout?: string };
      if (s?.stdout) termPush('output', s.stdout);
      break;
    }

    case 'vo.check': {
      const s = data as { ok?: boolean; diags?: VoCheckDiag[] };
      if (!s?.diags || s.diags.length === 0) {
        termPush('system', 'no errors');
      } else {
        for (const d of s.diags) {
          const loc = d.file ? `${C.src}${d.file}${C.reset}${C.dim}:${C.reset}${C.lnum}${d.line}${C.reset}${C.dim}:${C.reset}${C.lnum}${d.col}${C.reset}${C.dim}:${C.reset} ` : '';
          const kind = d.severity === 'warning' ? 'warn' as const : 'error' as const;
          termPush(kind, loc + d.message);
        }
      }
      break;
    }

    case 'vo.build':
    case 'vo.init':
      termPush('system', 'done');
      break;

    case 'vo.version': {
      // Tauri returns {version: string}; WASM WasmVoHandler returns raw string
      if (typeof data === 'string') {
        termPush('output', data);
      } else {
        const s = data as { version?: string };
        if (s?.version) termPush('output', s.version);
      }
      break;
    }

    case 'zip.list':
      for (const line of formatZipList(data)) termPush('output', line);
      break;

    case 'zip.pack':
    case 'zip.unpack':
      termPush('system', 'done');
      break;

    default:
      if (data !== undefined && data !== null && data !== '') {
        termPush('output', JSON.stringify(data, null, 2));
      }
  }
}

const SHELL_COMMANDS = [
  'ls','cat','head','tail','stat','touch','mkdir','rm','mv','cp','tree','find','grep','wc',
  'echo','pwd','cd','clear','help',
  'http','git','vo','zip','run',
];

const SUBCOMMANDS: Record<string, string[]> = {
  http: ['get', 'post', 'put', 'patch', 'delete', 'head'],
  git:  ['status', 'log', 'add', 'commit', 'push', 'pull', 'diff', 'checkout', 'branch', 'clone', 'init'],
  vo:   ['run', 'check', 'build', 'init', 'version', 'test', 'bench', 'clean', 'get', 'dump', 'compile'],
  zip:  ['pack', 'unpack', 'list'],
};

export async function getCompletions(
  input: string,
  cwd: string,
  workspaceRoot: string,
  shell: ShellClient,
): Promise<{ completions: string[]; replaceFrom: number }> {
  const tokens  = tokenize(input);
  const inSpace = input.endsWith(' ');
  const isFirst = tokens.length === 0 || (tokens.length === 1 && !inSpace);

  if (isFirst) {
    const partial = tokens[0]?.toLowerCase() ?? '';
    const matches = SHELL_COMMANDS.filter(c => c.startsWith(partial));
    return { completions: matches, replaceFrom: input.length - partial.length };
  }

  // Subcommand completion (e.g. "http ", "git ch", "vo r")
  const isSecond = (tokens.length === 1 && inSpace) || (tokens.length === 2 && !inSpace);
  if (isSecond) {
    const firstTok = tokens[0]?.toLowerCase() ?? '';
    const subs = SUBCOMMANDS[firstTok];
    if (subs) {
      const partial = inSpace ? '' : (tokens[1] ?? '');
      const matches = subs.filter(s => s.startsWith(partial));
      return { completions: matches, replaceFrom: input.length - partial.length };
    }
  }

  // Complete the last path token
  const lastToken = inSpace ? '' : (tokens[tokens.length - 1] ?? '');
  const replaceFrom = input.length - lastToken.length;

  // Determine parent dir and prefix from the partial path
  let dirToList: string;
  let filePrefix: string;
  const sepIdx = lastToken.lastIndexOf('/');
  if (sepIdx >= 0) {
    const parentPart = lastToken.slice(0, sepIdx + 1);
    dirToList  = resolvePath(cwd, parentPart, workspaceRoot);
    filePrefix = lastToken.slice(sepIdx + 1);
  } else {
    dirToList  = cwd;
    filePrefix = lastToken;
  }

  try {
    const entries = (await shell.exec({ kind: 'fs.list', path: dirToList })) as Array<{ name: string; isDir: boolean }>;
    const matches = entries
      .filter(e => e.name.toLowerCase().startsWith(filePrefix.toLowerCase()))
      .map(e => {
        const base = lastToken.slice(0, sepIdx + 1);
        return base + e.name + (e.isDir ? '/' : '');
      })
      .sort((a, b) => a.localeCompare(b));
    return { completions: matches, replaceFrom };
  } catch {
    return { completions: [], replaceFrom };
  }
}

function printHelp(): void {
  const S = C.src, D = C.dim, R = C.reset, B = C.bold;
  const lines = [
    `${B}File system:${R}`,
    `  ${S}ls${R} [path]               list directory`,
    `  ${S}cat${R} <path>              read file`,
    `  ${S}head${R} [-n N] <path>      first N lines  ${D}(default 10)${R}`,
    `  ${S}tail${R} [-n N] <path>      last N lines   ${D}(default 10)${R}`,
    `  ${S}stat${R} <path>             file info`,
    `  ${S}touch${R} <path>            create/update file`,
    `  ${S}mkdir${R} [-p] <path>       create directory`,
    `  ${S}rm${R} [-r] <path>          remove file/dir`,
    `  ${S}mv${R} <old> <new>          rename/move`,
    `  ${S}cp${R} <src> <dst>          copy`,
    `  ${S}wc${R} <path>               count lines/words/chars`,
    '',
    `${B}HTTP:${R}`,
    `  ${S}http${R} <get|post|put|patch|delete|head> <url>  ${D}make HTTP request${R}`,
    `       ${D}-H "Key: Val"  add request header${R}`,
    `       ${D}-d <body>     request body${R}`,
    `       ${D}-j            set JSON content-type + accept${R}`,
    '',
    `${B}Search:${R}`,
    `  ${S}grep${R} [-i] [-r] <pat> <path>   search in file(s)`,
    `  ${S}find${R} [dir] [-name glob] [-type f|d]   find entries`,
    `  ${S}tree${R} [dir] [-L depth]   show directory tree`,
    '',
    `${B}Git:${R}`,
    `  ${S}git status${R}              working tree status`,
    `  ${S}git log${R}                 commit history`,
    `  ${S}git add${R} <paths...>      stage files`,
    `  ${S}git commit -m${R} <msg>     commit staged changes`,
    `  ${S}git push/pull${R}           sync with remote  ${D}(streaming)${R}`,
    `  ${S}git diff${R} [--staged]     show diff`,
    `  ${S}git checkout${R} [-b] <br>  switch/create branch`,
    `  ${S}git branch${R}              list/create/delete branches`,
    `  ${S}git clone${R} <url>         clone repository  ${D}(streaming)${R}`,
    `  ${S}git init${R}                init repository`,
    '',
    `${B}Vo toolchain:${R}`,
    `  ${S}vo run${R} <path>           compile & run`,
    `  ${S}vo check${R} <path>         type-check`,
    `  ${S}vo build${R} <path>         build binary`,
    `  ${S}vo init${R}                 init project`,
    `  ${S}vo version${R}              show version`,
    '',
    `${B}Archive:${R}`,
    `  ${S}zip pack${R} <out> <files...>    create zip`,
    `  ${S}zip unpack${R} <archive> [dest]  extract zip`,
    `  ${S}zip list${R} <archive>           list contents`,
    '',
    `${B}Process:${R}`,
    `  ${S}run${R} <program> [args...]       spawn process  ${D}(streaming)${R}`,
    '',
    `${B}Built-in:${R}`,
    `  ${S}cd${R} [path]   change directory`,
    `  ${S}pwd${R}         print working directory`,
    `  ${S}echo${R} <text> print text`,
    `  ${S}clear${R}       clear terminal  ${D}(also Ctrl+L)${R}`,
    `  ${S}help${R}        show this help`,
    '',
    `${D}Tab: complete commands & paths  ·  ↑↓: history  ·  Ctrl+L: clear${R}`,
  ];
  for (const line of lines) termPush('output', line);
}
