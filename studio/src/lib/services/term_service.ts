import { get, writable, type Readable } from 'svelte/store';

import type { Backend } from '../backend/backend';
import type { CompilerService } from './compiler_service';
import type { RuntimeService } from './runtime_service';
import type { WorkspaceService } from './workspace_service';
import { formatError } from '../format_error';

export interface TermLine {
  kind: 'input' | 'output' | 'error';
  text: string;
}

export interface TermState {
  cwd: string;
  history: string[];
  lastCommand: string | null;
  lines: TermLine[];
}

export class TermService {
  private readonly stateStore = writable<TermState>({
    cwd: '/',
    history: [],
    lastCommand: null,
    lines: [],
  });

  constructor(
    private readonly workspace: WorkspaceService,
    private readonly compiler: CompilerService,
    private readonly runtime: RuntimeService,
    private readonly backend: Backend,
  ) {
    this.syncCwd(workspace.root);
  }

  get state(): Readable<TermState> {
    return { subscribe: this.stateStore.subscribe };
  }

  get sessionRoot(): string {
    return this.workspace.root;
  }

  syncCwd(cwd: string): void {
    this.stateStore.update((state) => ({
      ...state,
      cwd,
    }));
  }

  async setCwd(path: string): Promise<void> {
    const target = this.resolvePath(path || '.');
    await this.workspace.list(target);
    this.syncCwd(target);
  }

  async execute(command: string): Promise<void> {
    const input = command.trim();
    if (!input) {
      return;
    }
    // Handle clear before appending input echo to avoid render-then-clear flash
    if (input === 'clear') {
      this.record(input);
      this.clear();
      return;
    }
    this.append('input', `${this.snapshot().cwd} $ ${input}`);
    this.record(input);
    const [program, ...args] = tokenize(input);
    try {
      switch (program) {
        case 'pwd':
          this.append('output', this.snapshot().cwd);
          return;
        case 'help':
          this.append('output', [
            'Studio TERM — Vo development commands',
            '',
            'File system:',
            '  ls [path]            list directory',
            '  cat <file>           read file',
            '  cd <dir>             change directory',
            '  pwd                  print working directory',
            '  mkdir <path>         create directory',
            '  rm [-r] <path>       remove file or directory',
            '  mv <src> <dst>       rename/move',
            '  cp <src> <dst>       copy',
            '  grep <pattern> [path]  search files',
            '',
            'Vo compiler:',
            '  vo check [path]      type-check',
            '  vo compile [path]    compile to bytecode',
            '  vo build [path]      build project',
            '  vo dump [path]       disassemble bytecode',
            '  vo run [path] [--mode=vm|jit]  run program',
            '  vo get <spec>        install module',
            '  vo init [name]       init new module',
            '  vo version           show compiler version',
            '',
            'Version control:',
            '  git status|add|commit|push|pull|log|diff|branch',
            '',
            'Other:',
            '  echo <text>          print text',
            '  clear                clear TERM output',
            '  help                 this help',
          ].join('\n'));
          return;
        case 'ls':
          await this.runLs(args[0]);
          return;
        case 'cat':
          await this.runCat(args[0]);
          return;
        case 'cd':
          await this.runCd(args[0]);
          return;
        case 'vo':
          await this.runVoCommand(args);
          return;
        case 'git':
          await this.runGit(args);
          return;
        case 'grep':
          await this.runGrep(args);
          return;
        case 'mkdir':
          await this.runMkdir(args[0]);
          return;
        case 'rm':
          await this.runRm(args);
          return;
        case 'mv':
          await this.runMv(args);
          return;
        case 'cp':
          await this.runCp(args);
          return;
        case 'echo':
          this.append('output', args.join(' '));
          return;
        case 'exit':
        case 'quit':
          window.dispatchEvent(new CustomEvent('studio:exit'));
          return;
        default:
          this.append('error', `Unknown command: ${program}. Type 'help' for available commands.`);
      }
    } catch (error) {
      this.append('error', formatError(error));
    }
  }

  clear(): void {
    this.stateStore.update((state) => ({
      ...state,
      lines: [],
    }));
  }

  private record(command: string): void {
    this.stateStore.update((state) => ({
      ...state,
      lastCommand: command,
      history: [...state.history, command],
    }));
  }

  private async runLs(pathArg?: string): Promise<void> {
    const target = this.resolvePath(pathArg ?? '.');
    const entries = await this.workspace.list(target);
    if (entries.length === 0) {
      this.append('output', '(empty)');
      return;
    }
    this.append(
      'output',
      entries
        .map((entry) => `${entry.isDir ? 'dir ' : 'file'} ${entry.name}`)
        .join('\n'),
    );
  }

  private async runCat(pathArg?: string): Promise<void> {
    if (!pathArg) {
      this.append('error', 'cat requires a file path');
      return;
    }
    const target = this.resolvePath(pathArg);
    const content = await this.workspace.readFile(target);
    this.append('output', content);
  }

  private async runCd(pathArg?: string): Promise<void> {
    await this.setCwd(pathArg ?? this.workspace.root);
  }

  private async runVoCommand(args: string[]): Promise<void> {
    const subcommand = args[0];
    switch (subcommand) {
      case 'check':
        await this.runVoCheck(args.slice(1));
        return;
      case 'run':
        await this.runVoRun(args.slice(1));
        return;
      case 'build':
        await this.runVoBuild(args.slice(1));
        return;
      case 'compile':
        await this.runVoCompile(args.slice(1));
        return;
      case 'dump':
        await this.runVoDump(args.slice(1));
        return;
      case 'version':
        this.append('output', await this.backend.voVersion());
        return;
      case 'init':
        await this.runVoInit(args.slice(1));
        return;
      case 'get':
        await this.runVoGet(args.slice(1));
        return;
      default:
        this.append('error', `Unsupported vo command: ${subcommand ?? '(missing)'}\nAvailable: check, run, build, compile, dump, version, init, get`);
    }
  }

  private async runVoCheck(args: string[]): Promise<void> {
    const target = this.resolvePath(args[0] ?? '.');
    const result = await this.compiler.check(target);
    if (result.ok) {
      this.append('output', `✓ Check passed: ${target}`);
    } else {
      result.errors.forEach((err) => this.append('error', `${err.file}:${err.line}: ${err.message}`));
    }
  }

  private async runVoRun(args: string[]): Promise<void> {
    const { mode, target } = parseVoRunArgs(args);
    const resolvedTarget = this.resolvePath(target ?? '.');
    this.append('output', `$ vo run ${resolvedTarget} --mode=${mode}`);
    const output = await this.runtime.runConsole(resolvedTarget, mode);
    if (output.trim().length > 0) {
      this.append('output', output);
      return;
    }
    this.append('output', `(completed ${mode})`);
  }

  private async runVoBuild(args: string[]): Promise<void> {
    let outputArg: string | undefined;
    let targetArg: string | undefined;
    for (let i = 0; i < args.length; i++) {
      const a = args[i];
      if (a.startsWith('-o=')) {
        outputArg = a.slice(3);
      } else if (a === '-o') {
        outputArg = args[++i];
      } else if (!targetArg) {
        targetArg = a;
      }
    }
    const target = this.resolvePath(targetArg ?? '.');
    const result = await this.compiler.build(target, outputArg);
    if (result.ok) {
      this.append('output', `✓ Built: ${result.outputPath ?? target}`);
    } else {
      result.errors.forEach((err) => this.append('error', `${err.file}:${err.line}: ${err.message}`));
    }
  }

  private async runVoCompile(args: string[]): Promise<void> {
    const target = this.resolvePath(args[0] ?? '.');
    const result = await this.compiler.compile(target);
    if (result.ok) {
      this.append('output', `✓ Compiled: ${result.outputPath ?? target}`);
    } else {
      result.errors.forEach((err) => this.append('error', `${err.file}:${err.line}: ${err.message}`));
    }
  }


  private async runVoDump(args: string[]): Promise<void> {
    const target = this.resolvePath(args[0] ?? '.');
    const result = await this.compiler.dump(target);
    this.append('output', result);
  }

  private async runVoInit(args: string[]): Promise<void> {
    const name = args[0];
    const target = this.resolvePath(args[1] ?? '.');
    const dir = await this.backend.voInit(target, name);
    this.append('output', `✓ Initialized: ${dir}`);
  }


  private async runVoGet(args: string[]): Promise<void> {
    const spec = args[0];
    if (!spec) {
      this.append('error', 'vo get requires a module spec');
      return;
    }
    this.append('output', `Installing ${spec}...`);
    const stream = this.backend.voGet(spec);
    for await (const event of stream) {
      if (event.kind === 'fetch') this.append('output', event.message);
      else if (event.kind === 'build') this.append('output', event.line);
      else if (event.kind === 'done') this.append('output', `✓ Installed: ${event.module}`);
      else if (event.kind === 'error') this.append('error', event.message);
    }
  }

  private async runGit(args: string[]): Promise<void> {
    const subcommand = args[0];
    if (!subcommand) {
      this.append('error', 'git requires a subcommand (status, add, commit, push, pull, log, diff, branch)');
      return;
    }
    let op: import('../types').GitOp;
    switch (subcommand) {
      case 'status': op = { kind: 'git.status' }; break;
      case 'add': {
        const paths = args.slice(1);
        if (paths.length === 0) { this.append('error', 'git add requires at least one path'); return; }
        op = { kind: 'git.add', paths };
        break;
      }
      case 'commit': {
        const mIdx = args.indexOf('-m');
        const message = mIdx >= 0 && mIdx + 1 < args.length
          ? args.slice(mIdx + 1).join(' ')
          : args.slice(1).join(' ');
        if (!message) { this.append('error', 'git commit requires a message (-m "message")'); return; }
        op = { kind: 'git.commit', message };
        break;
      }
      case 'push': op = { kind: 'git.push', remote: args[1], branch: args[2] }; break;
      case 'pull': op = { kind: 'git.pull', remote: args[1], branch: args[2] }; break;
      case 'log': op = { kind: 'git.log', limit: args[1] ? Number(args[1]) : undefined }; break;
      case 'diff': op = { kind: 'git.diff', path: args[1] }; break;
      case 'branch': {
        const branchArg = args[1];
        if (!branchArg) {
          op = { kind: 'git.branch', list: true };
        } else if (args[1] === '-d' || args[1] === '--delete') {
          if (!args[2]) { this.append('error', 'git branch -d requires a branch name'); return; }
          op = { kind: 'git.branch', delete: args[2] };
        } else {
          op = { kind: 'git.branch', create: branchArg };
        }
        break;
      }
      default:
        this.append('error', `Unknown git subcommand: ${subcommand}`);
        return;
    }
    const result = await this.backend.gitExec(op);
    if (result.output.trim()) this.append(result.ok ? 'output' : 'error', result.output);
    else if (!result.ok) this.append('error', `git ${subcommand} failed`);
  }

  private async runGrep(args: string[]): Promise<void> {
    const pattern = args[0];
    const pathArg = args[1] ?? '.';
    if (!pattern) {
      this.append('error', 'grep requires a pattern');
      return;
    }
    const target = this.resolvePath(pathArg);
    const matches = await this.workspace.grep(target, pattern);
    if (matches.length === 0) {
      this.append('output', '(no matches)');
    } else {
      matches.forEach((m) => this.append('output', `${m.path}:${m.line}: ${m.text}`));
    }
  }

  private async runMkdir(pathArg?: string): Promise<void> {
    if (!pathArg) { this.append('error', 'mkdir requires a path'); return; }
    await this.workspace.mkdir(this.resolvePath(pathArg));
    this.append('output', `Created: ${pathArg}`);
  }

  private async runRm(args: string[]): Promise<void> {
    const recursive = args.includes('-r') || args.includes('-rf');
    const pathArg = args.find((a) => !a.startsWith('-'));
    if (!pathArg) { this.append('error', 'rm requires a path'); return; }
    await this.workspace.remove(this.resolvePath(pathArg), recursive);
    this.append('output', `Removed: ${pathArg}`);
  }

  private async runMv(args: string[]): Promise<void> {
    if (args.length < 2) { this.append('error', 'mv requires src and dst'); return; }
    await this.workspace.rename(this.resolvePath(args[0]!), this.resolvePath(args[1]!));
    this.append('output', `Moved: ${args[0]} → ${args[1]}`);
  }

  private async runCp(args: string[]): Promise<void> {
    if (args.length < 2) { this.append('error', 'cp requires src and dst'); return; }
    await this.workspace.copy(this.resolvePath(args[0]!), this.resolvePath(args[1]!));
    this.append('output', `Copied: ${args[0]} → ${args[1]}`);
  }

  private append(kind: TermLine['kind'], text: string): void {
    this.stateStore.update((state) => ({
      ...state,
      lines: [...state.lines, { kind, text }],
    }));
  }

  private resolvePath(path: string): string {
    const value = path.trim();
    if (!value || value === '.') {
      return normalizePath(this.snapshot().cwd);
    }
    if (value.startsWith('/')) {
      return normalizePath(value);
    }
    return normalizePath(`${this.snapshot().cwd}/${value}`);
  }

  snapshot(): TermState {
    return get(this.stateStore);
  }
}

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let current = '';
  let inQuote: '"' | "'" | null = null;
  for (let i = 0; i < input.length; i++) {
    const ch = input[i];
    if (inQuote) {
      if (ch === inQuote) {
        inQuote = null;
      } else {
        current += ch;
      }
    } else if (ch === '"' || ch === "'") {
      inQuote = ch;
    } else if (/\s/.test(ch)) {
      if (current.length > 0) {
        tokens.push(current);
        current = '';
      }
    } else {
      current += ch;
    }
  }
  if (current.length > 0) {
    tokens.push(current);
  }
  return tokens;
}

function parseVoRunArgs(args: string[]): { mode: 'vm' | 'jit'; target: string | null } {
  let mode: 'vm' | 'jit' = 'vm';
  let target: string | null = null;
  for (let index = 0; index < args.length; index += 1) {
    const arg = args[index];
    if (arg.startsWith('--mode=')) {
      mode = parseRunMode(arg.slice('--mode='.length));
      continue;
    }
    if (arg === '--mode') {
      mode = parseRunMode(args[index + 1] ?? '');
      index += 1;
      continue;
    }
    if (!target) {
      target = arg;
    }
  }
  return { mode, target };
}

function normalizePath(path: string): string {
  const parts = path.split('/');
  const normalized: string[] = [];
  for (const part of parts) {
    if (!part || part === '.') {
      continue;
    }
    if (part === '..') {
      normalized.pop();
      continue;
    }
    normalized.push(part);
  }
  return `/${normalized.join('/')}` || '/';
}


function parseRunMode(value: string): 'vm' | 'jit' {
  if (value === 'vm' || value === 'jit') {
    return value;
  }
  throw new Error(`Unsupported run mode: ${value}`);
}
