import { writable } from 'svelte/store';
import type { FrameworkContract } from '../lib/types';

export type RuntimeKind = 'console' | 'gui' | null;
export type RuntimeStatus = 'idle' | 'running' | 'ready';

export interface RuntimeState {
  status: RuntimeStatus;
  kind: RuntimeKind;
  target: string | null;
  runMode: 'vm' | 'jit' | null;
  isRunning: boolean;
  consoleLines: string[];
  lastError: string | null;
  guiEntryPath: string | null;
  guiModuleBytes: Uint8Array | null;
  guiRenderBytes: Uint8Array | null;
  guiFramework: FrameworkContract | null;
  guiSessionId: number | null;
  guiExternalWidgetHandlerId: number | null;
}

export const IDLE_RUNTIME: RuntimeState = {
  status: 'idle',
  kind: null,
  target: null,
  runMode: null,
  isRunning: false,
  consoleLines: [],
  lastError: null,
  guiEntryPath: null,
  guiModuleBytes: null,
  guiRenderBytes: null,
  guiFramework: null,
  guiSessionId: null,
  guiExternalWidgetHandlerId: null,
};

export const runtime = writable<RuntimeState>({ ...IDLE_RUNTIME });
