import { describe, expect, it, vi } from 'vitest';

vi.mock('../bridge', () => ({
  bridge: () => ({
    fsReadFile: vi.fn(),
    shell: { exec: vi.fn() },
  }),
}));

vi.mock('../actions/fs', () => ({
  saveFile: vi.fn(),
}));

import { resolveRunEntryPath } from '../actions/exec';

describe('resolveRunEntryPath', () => {
  it('uses the active file in single-file mode', () => {
    expect(resolveRunEntryPath({
      projectMode: 'single',
      activeFilePath: '/workspace/current.vo',
      runEntryPath: '/workspace/stale.vo',
    })).toBe('/workspace/current.vo');
  });

  it('uses the explicit run entry in multi-project mode', () => {
    expect(resolveRunEntryPath({
      projectMode: 'multi',
      activeFilePath: '/workspace/project/editor.vo',
      runEntryPath: '/workspace/project/main.vo',
    })).toBe('/workspace/project/main.vo');
  });

  it('falls back to the active file when multi-project run entry is empty', () => {
    expect(resolveRunEntryPath({
      projectMode: 'multi',
      activeFilePath: '/workspace/project/main.vo',
      runEntryPath: '',
    })).toBe('/workspace/project/main.vo');
  });
});
