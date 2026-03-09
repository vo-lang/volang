import { describe, it, expect } from 'vitest';
import { hasStudioLaunchQuery, normalizeLocalLaunchPath, parseStudioLaunchUrl } from '../launch_protocol';

describe('normalizeLocalLaunchPath', () => {
  it('resolves workspace-relative paths', () => {
    expect(normalizeLocalLaunchPath('examples/solana_wallet', '/workspace')).toBe('/workspace/examples/solana_wallet');
  });

  it('preserves absolute paths', () => {
    expect(normalizeLocalLaunchPath('/tmp/demo/main.vo', '/workspace')).toBe('/tmp/demo/main.vo');
  });

  it('decodes file URLs', () => {
    expect(normalizeLocalLaunchPath('file:///tmp/demo/main.vo', '/workspace')).toBe('/tmp/demo/main.vo');
  });

  it('normalizes relative path traversal against the workspace root', () => {
    expect(normalizeLocalLaunchPath('./examples/../demo/main.vo', '/workspace')).toBe('/workspace/demo/main.vo');
  });

  it('normalizes absolute paths with lexical parent traversal', () => {
    expect(normalizeLocalLaunchPath('/workspace/project/../demo/main.vo', '/workspace')).toBe('/workspace/demo/main.vo');
  });
});

describe('hasStudioLaunchQuery', () => {
  it('detects run launch queries', () => {
    expect(hasStudioLaunchQuery(new URL('http://localhost/?run=examples/counter.vo'))).toBe(true);
  });

  it('returns false when no launch query exists', () => {
    expect(hasStudioLaunchQuery(new URL('http://localhost/?foo=bar'))).toBe(false);
  });
});

describe('parseStudioLaunchUrl', () => {
  it('parses local run targets with entry override', () => {
    const result = parseStudioLaunchUrl(
      'http://localhost/?run=examples/solana_wallet&entry=main.vo',
      '/workspace',
    );

    expect(result).not.toBeNull();
    expect(result?.action).toBe('run');
    expect(result?.target).toEqual({
      kind: 'local',
      targetPath: '/workspace/examples/solana_wallet',
      entryPath: 'main.vo',
    });
  });

  it('parses local open file targets', () => {
    const result = parseStudioLaunchUrl(
      'http://localhost/?open=file:///tmp/demo/counter.vo',
      '/workspace',
    );

    expect(result).not.toBeNull();
    expect(result?.action).toBe('open');
    expect(result?.target).toEqual({
      kind: 'local',
      targetPath: '/tmp/demo/counter.vo',
      entryPath: undefined,
    });
  });

  it('parses GitHub tree URLs with explicit ref and entry', () => {
    const result = parseStudioLaunchUrl(
      'http://localhost/?run=https://github.com/vo-lang/volang/tree/main/examples/solana_wallet&entry=main.vo&ref=main',
      '/workspace',
    );

    expect(result).not.toBeNull();
    expect(result?.action).toBe('run');
    expect(result?.target).toEqual({
      kind: 'github',
      owner: 'vo-lang',
      repo: 'volang',
      ref: 'main',
      subpath: 'examples/solana_wallet',
      entryPath: 'main.vo',
    });
  });

  it('parses GitHub repo URLs without a subpath', () => {
    const result = parseStudioLaunchUrl(
      'http://localhost/?open=https://github.com/vo-lang/volang',
      '/workspace',
    );

    expect(result).not.toBeNull();
    expect(result?.target).toEqual({
      kind: 'github',
      owner: 'vo-lang',
      repo: 'volang',
      ref: undefined,
      subpath: undefined,
      entryPath: undefined,
    });
  });

  it('keeps the URL subpath stable when a ref override differs from the tree ref', () => {
    const result = parseStudioLaunchUrl(
      'http://localhost/?run=https://github.com/vo-lang/volang/tree/main/examples/solana_wallet&ref=dev',
      '/workspace',
    );

    expect(result).not.toBeNull();
    expect(result?.target).toEqual({
      kind: 'github',
      owner: 'vo-lang',
      repo: 'volang',
      ref: 'dev',
      subpath: 'examples/solana_wallet',
      entryPath: undefined,
    });
  });
});
