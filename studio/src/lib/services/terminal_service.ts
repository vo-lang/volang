import { get, writable, type Readable } from 'svelte/store';

import type { Backend } from '../backend/backend';
import type { CompilerService } from './compiler_service';
import type { RuntimeService } from './runtime_service';
import type { WorkspaceService } from './workspace_service';
import { formatError } from '../format_error';

export interface TerminalLine {
  kind: 'input' | 'output' | 'error';
  text: string;
}

export interface TerminalState {
  cwd: string;
  history: string[];
  lastCommand: string | null;
  lines: TerminalLine[];
}

export class TerminalService {
  private readonly stateStore = writable<TerminalState>({
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

  get state(): Readable<TerminalState> {
    return { subscribe: this.stateStore.subscribe };
  }

  syncCwd(cwd: string): void {
    this.stateStore.update((state) => ({
      ...state,
      cwd,
    }));
  }

  async execute(command: string): Promise<void> {
    const input = command.trim();
    if (!input) {
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
            'Studio Terminal — Vo development commands',
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
            '  vo format [path]     format source',
            '  vo dump [path]       disassemble bytecode',
            '  vo run [path] [--mode=vm|jit]  run program',
            '  vo test [path]       run tests',
            '  vo get <spec>        install module',
            '  vo init [name]       init new module',
            '  vo version           show compiler version',
            '',
            'Version control:',
            '  git status|add|commit|push|pull|log|diff|branch',
            '',
            'Other:',
            '  echo <text>          print text',
            '  clear                clear terminal',
            '  help                 this help',
          ].join('\n'));
          return;
        case 'clear':
          this.clear();
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
    const target = this.resolvePath(pathArg ?? this.workspace.root);
    await this.workspace.list(target);
    this.syncCwd(target);
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
      case 'test':
        await this.runVoTest(args.slice(1));
        return;
      case 'build':
        await this.runVoBuild(args.slice(1));
        return;
      case 'compile':
        await this.runVoCompile(args.slice(1));
        return;
      case 'format':
        await this.runVoFormat(args.slice(1));
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
        this.append('error', `Unsupported vo command: ${subcommand ?? '(missing)'}\nAvailable: check, run, build, compile, format, dump, version, init, get`);
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
    const target = this.resolvePath(args[0] ?? '.');
    const outputArg = args.find((a) => a.startsWith('-o='))?.slice(3) ??
      args[args.indexOf('-o') + 1];
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

  private async runVoFormat(args: string[]): Promise<void> {
    const target = this.resolvePath(args[0] ?? '.');
    const result = await this.compiler.format(target);
    this.append('output', result);
  }

  private async runVoDump(args: string[]): Promise<void> {
    const target = this.resolvePath(args[0] ?? '.');
    const result = await this.compiler.dump(target);
    this.append('output', result);
  }

  private async runVoInit(args: string[]): Promise<void> {
    const target = this.resolvePath(args[0] ?? '.');
    const name = args[1];
    const dir = await this.backend.voInit(target, name);
    this.append('output', `✓ Initialized: ${dir}`);
  }

  private async runVoTest(args: string[]): Promise<void> {
    const target = this.resolvePath(args[0] ?? '.');
    this.append('output', `$ vo test ${target}`);
    const opts = { args: ['--test'] };
    const stream = this.backend.runVo(target, opts);
    for await (const event of stream) {
      if (event.kind === 'stdout') this.append('output', event.text);
      else if (event.kind === 'stderr') this.append('error', event.text);
      else if (event.kind === 'done') {
        this.append(event.exitCode === 0 ? 'output' : 'error',
          `Test finished (exit ${event.exitCode}, ${event.durationMs}ms)`);
      } else if (event.kind === 'error') this.append('error', event.message);
    }
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
      case 'add': op = { kind: 'git.add', paths: args.slice(1) }; break;
      case 'commit': op = { kind: 'git.commit', message: args.slice(1).join(' ') }; break;
      case 'push': op = { kind: 'git.push', remote: args[1], branch: args[2] }; break;
      case 'pull': op = { kind: 'git.pull', remote: args[1], branch: args[2] }; break;
      case 'log': op = { kind: 'git.log', limit: args[1] ? Number(args[1]) : undefined }; break;
      case 'diff': op = { kind: 'git.diff', path: args[1] }; break;
      case 'branch': op = { kind: 'git.branch', list: !args[1] }; break;
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

  private append(kind: TerminalLine['kind'], text: string): void {
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

  private snapshot(): TerminalState {
    return get(this.stateStore);
  }
}

function tokenize(input: string): string[] {
  return input
    .split(/\s+/)
    .map((part) => part.trim())
    .filter((part) => part.length > 0);
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
