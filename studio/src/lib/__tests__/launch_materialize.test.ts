import { afterEach, describe, expect, it, vi } from 'vitest';
import { writable } from 'svelte/store';

const mocks = vi.hoisted(() => ({
  materializeLocalLaunchTarget: vi.fn(async (path: string) => `/workspace/.studio/local${path}`),
  activateLocalDevMode: vi.fn(async (path: string) => path),
  resetLaunchMode: vi.fn(async () => {}),
  openWorkspaceTarget: vi.fn(async () => {}),
  runCode: vi.fn(async () => {}),
}));

vi.mock('../bridge', () => ({
  bridge: () => ({
    appWorkspaceRoot: '/workspace',
    workspaceRoot: '/workspace',
    materializeLocalLaunchTarget: mocks.materializeLocalLaunchTarget,
    activateLocalDevMode: mocks.activateLocalDevMode,
    resetLaunchMode: mocks.resetLaunchMode,
  }),
}));

vi.mock('../../stores/github', () => ({
  github: writable({ token: null, user: null, isLoading: false, error: '' }),
  gitPullFiles: vi.fn(),
}));

vi.mock('../actions/workspace', () => ({
  openWorkspaceTarget: mocks.openWorkspaceTarget,
}));

vi.mock('../actions/exec', () => ({
  runCode: mocks.runCode,
}));

import { executeStudioLaunch } from '../launch';

afterEach(() => {
  vi.clearAllMocks();
});

describe('executeStudioLaunch', () => {
  it('materializes local targets outside the workspace before opening them', async () => {
    const handled = await executeStudioLaunch('http://localhost/?open=file:///tmp/demo/main.vo');

    expect(handled).toBe(true);
    expect(mocks.resetLaunchMode).toHaveBeenCalledTimes(1);
    expect(mocks.materializeLocalLaunchTarget).toHaveBeenCalledWith('/tmp/demo/main.vo');
    expect(mocks.openWorkspaceTarget).toHaveBeenCalledWith('/workspace/.studio/local/tmp/demo/main.vo', undefined);
    expect(mocks.runCode).not.toHaveBeenCalled();
  });

  it('runs after opening when the launch action is run', async () => {
    const handled = await executeStudioLaunch('http://localhost/?run=file:///tmp/demo/main.vo');

    expect(handled).toBe(true);
    expect(mocks.resetLaunchMode).toHaveBeenCalledTimes(1);
    expect(mocks.materializeLocalLaunchTarget).toHaveBeenCalledWith('/tmp/demo/main.vo');
    expect(mocks.openWorkspaceTarget).toHaveBeenCalledWith('/workspace/.studio/local/tmp/demo/main.vo', undefined);
    expect(mocks.runCode).toHaveBeenCalledTimes(1);
  });

  it('activates local dev mode instead of materializing external targets', async () => {
    const handled = await executeStudioLaunch('http://localhost/?run=file:///tmp/dev/main.vo&mode=dev');

    expect(handled).toBe(true);
    expect(mocks.activateLocalDevMode).toHaveBeenCalledWith('/tmp/dev/main.vo');
    expect(mocks.materializeLocalLaunchTarget).not.toHaveBeenCalled();
    expect(mocks.openWorkspaceTarget).toHaveBeenCalledWith('/tmp/dev/main.vo', undefined);
    expect(mocks.runCode).toHaveBeenCalledTimes(1);
  });
});
