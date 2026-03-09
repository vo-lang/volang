import { describe, expect, it } from 'vitest';
import { dirname, isPathWithinRoot, joinPath, normalizePath } from '../path_utils';

describe('path_utils', () => {
  it('normalizes lexical parent traversal for absolute paths', () => {
    expect(normalizePath('/workspace/project/../demo/main.vo')).toBe('/workspace/demo/main.vo');
  });

  it('normalizes lexical parent traversal for relative paths', () => {
    expect(normalizePath('./examples/../demo/main.vo')).toBe('demo/main.vo');
  });

  it('joins relative paths through the normalizer', () => {
    expect(joinPath('/workspace/project', '../demo/main.vo')).toBe('/workspace/demo/main.vo');
  });

  it('computes dirname after normalization', () => {
    expect(dirname('/workspace/project/../demo/main.vo')).toBe('/workspace/demo');
  });

  it('rejects paths that lexically escape the root', () => {
    expect(isPathWithinRoot('/workspace/project/../tmp/main.vo', '/workspace/project')).toBe(false);
  });

  it('accepts normalized descendants of the root', () => {
    expect(isPathWithinRoot('/workspace/project/src/../main.vo', '/workspace/project')).toBe(true);
  });
});
