import { afterEach, describe, expect, it, vi } from 'vitest';
import { writable } from 'svelte/store';

const mocks = vi.hoisted(() => ({
  materializeLocalLaunchTarget: vi.fn(async (path: string) => `/workspace/.studio/local${path}`),
  openWorkspaceTarget: vi.fn(async () => {}),
  runCode: vi.fn(async () => {}),
}));

vi.mock('../bridge', () => ({
  bridge: () => ({
    workspaceRoot: '/workspace',
    materializeLocalLaunchTarget: mocks.materializeLocalLaunchTarget,
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
    expect(mocks.materializeLocalLaunchTarget).toHaveBeenCalledWith('/tmp/demo/main.vo');
    expect(mocks.openWorkspaceTarget).toHaveBeenCalledWith('/workspace/.studio/local/tmp/demo/main.vo', undefined);
    expect(mocks.runCode).not.toHaveBeenCalled();
  });

  it('runs after opening when the launch action is run', async () => {
    const handled = await executeStudioLaunch('http://localhost/?run=file:///tmp/demo/main.vo');

    expect(handled).toBe(true);
    expect(mocks.materializeLocalLaunchTarget).toHaveBeenCalledWith('/tmp/demo/main.vo');
    expect(mocks.openWorkspaceTarget).toHaveBeenCalledWith('/workspace/.studio/local/tmp/demo/main.vo', undefined);
    expect(mocks.runCode).toHaveBeenCalledTimes(1);
  });
});
