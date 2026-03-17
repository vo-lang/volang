import { writable, type Readable } from 'svelte/store';

import type { Backend } from '../backend/backend';
import type { BuildResult, CheckResult, CompileResult } from '../types';
import { formatError } from '../format_error';

export type CompilerTask = 'check' | 'compile' | 'format' | 'build' | 'dump' | null;
export type CompilerStatus = 'idle' | 'running' | 'ready';

export interface CompilerState {
  status: CompilerStatus;
  lastTask: CompilerTask;
  lastTarget: string | null;
  lastCheckResult: CheckResult | null;
  lastCompileResult: CompileResult | null;
  lastBuildResult: BuildResult | null;
  lastDump: string | null;
  lastError: string | null;
}

export class CompilerService {
  private readonly stateStore = writable<CompilerState>({
    status: 'idle',
    lastTask: null,
    lastTarget: null,
    lastCheckResult: null,
    lastCompileResult: null,
    lastBuildResult: null,
    lastDump: null,
    lastError: null,
  });

  constructor(private readonly backend: Backend) {}

  get state(): Readable<CompilerState> {
    return { subscribe: this.stateStore.subscribe };
  }

  async check(target: string): Promise<CheckResult> {
    this.stateStore.update((s) => ({ ...s, status: 'running', lastTask: 'check', lastTarget: target, lastError: null }));
    try {
      const result = await this.backend.checkVo(target);
      this.stateStore.update((s) => ({ ...s, status: 'ready', lastCheckResult: result }));
      return result;
    } catch (error) {
      this.stateStore.update((s) => ({ ...s, status: 'ready', lastError: formatError(error) }));
      throw error;
    }
  }

  async compile(target: string): Promise<CompileResult> {
    this.stateStore.update((s) => ({ ...s, status: 'running', lastTask: 'compile', lastTarget: target, lastError: null }));
    try {
      const result = await this.backend.compileVo(target);
      this.stateStore.update((s) => ({ ...s, status: 'ready', lastCompileResult: result }));
      return result;
    } catch (error) {
      this.stateStore.update((s) => ({ ...s, status: 'ready', lastError: formatError(error) }));
      throw error;
    }
  }

  async format(target: string): Promise<string> {
    this.stateStore.update((s) => ({ ...s, status: 'running', lastTask: 'format', lastTarget: target, lastError: null }));
    try {
      const result = await this.backend.formatVo(target);
      this.stateStore.update((s) => ({ ...s, status: 'ready' }));
      return result;
    } catch (error) {
      this.stateStore.update((s) => ({ ...s, status: 'ready', lastError: formatError(error) }));
      throw error;
    }
  }

  async build(target: string, output?: string): Promise<BuildResult> {
    this.stateStore.update((s) => ({ ...s, status: 'running', lastTask: 'build', lastTarget: target, lastError: null }));
    try {
      const result = await this.backend.buildVo(target, output);
      this.stateStore.update((s) => ({ ...s, status: 'ready', lastBuildResult: result }));
      return result;
    } catch (error) {
      this.stateStore.update((s) => ({ ...s, status: 'ready', lastError: formatError(error) }));
      throw error;
    }
  }

  async dump(target: string): Promise<string> {
    this.stateStore.update((s) => ({ ...s, status: 'running', lastTask: 'dump', lastTarget: target, lastError: null }));
    try {
      const result = await this.backend.dumpVo(target);
      this.stateStore.update((s) => ({ ...s, status: 'ready', lastDump: result }));
      return result;
    } catch (error) {
      this.stateStore.update((s) => ({ ...s, status: 'ready', lastError: formatError(error) }));
      throw error;
    }
  }
}
