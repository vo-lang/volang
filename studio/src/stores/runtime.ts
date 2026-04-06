import { writable } from 'svelte/store';
import type { FrameworkContract } from '../lib/types';

export type RuntimeKind = 'console' | 'gui' | null;
export type RuntimeStatus = 'idle' | 'running' | 'ready';

export interface GuiRuntimeState {
  entryPath: string | null;
  moduleBytes: Uint8Array | null;
  renderBytes: Uint8Array | null;
  framework: FrameworkContract | null;
  sessionId: number | null;
  externalWidgetHandlerId: number | null;
}

export interface RuntimeState {
  status: RuntimeStatus;
  kind: RuntimeKind;
  target: string | null;
  runMode: 'vm' | 'jit' | null;
  isRunning: boolean;
  consoleLines: string[];
  lastError: string | null;
  gui: GuiRuntimeState;
}

export const IDLE_GUI: GuiRuntimeState = {
  entryPath: null,
  moduleBytes: null,
  renderBytes: null,
  framework: null,
  sessionId: null,
  externalWidgetHandlerId: null,
};

export const IDLE_RUNTIME: RuntimeState = {
  status: 'idle',
  kind: null,
  target: null,
  runMode: null,
  isRunning: false,
  consoleLines: [],
  lastError: null,
  gui: { ...IDLE_GUI },
};

export const runtime = writable<RuntimeState>({ ...IDLE_RUNTIME });
