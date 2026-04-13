import type { SessionInfo, ShareInfo, StudioMode } from './types';

const SHARE_BASE_URL = 'https://volang.dev/';

export function buildGitHubRepoShareInfo(options: {
  owner: string;
  repo: string;
  commit: string | null;
  subdir?: string | null;
  mode?: StudioMode;
}): ShareInfo {
  if (!options.commit) {
    return {
      canonicalUrl: '',
      shareable: false,
      reason: 'GitHub session is not pinned to a commit',
    };
  }
  const url = new URL(SHARE_BASE_URL);
  const projectUrl = buildPinnedGitHubProjectUrl(options.owner, options.repo, options.commit, options.subdir ?? null);
  url.searchParams.set('mode', options.mode ?? 'runner');
  url.searchParams.set('proj', projectUrl);
  return {
    canonicalUrl: url.toString(),
    shareable: true,
  };
}

export function buildShareInfo(
  session: Pick<SessionInfo, 'root' | 'entryPath' | 'source'>,
  options: {
    mode?: StudioMode;
  } = {},
): ShareInfo {
  const source = session.source;
  if (!source) {
    return {
      canonicalUrl: '',
      shareable: false,
      reason: 'Session has no source provenance',
    };
  }
  if (source.kind !== 'github_repo') {
    return {
      canonicalUrl: '',
      shareable: false,
      reason: 'Only GitHub sessions can be shared',
    };
  }
  return buildGitHubRepoShareInfo({
    owner: source.owner,
    repo: source.repo,
    commit: source.resolvedCommit,
    subdir: source.subdir,
    mode: options.mode,
  });
}

function buildPinnedGitHubProjectUrl(owner: string, repo: string, commit: string, subdir: string | null): string {
  const url = new URL(`https://github.com/${owner}/${repo}`);
  const trimmedSubdir = subdir?.trim().replace(/^\/+|\/+$/g, '') ?? '';
  const encodedSubdir = trimmedSubdir
    ? trimmedSubdir.split('/').map((segment) => encodeURIComponent(segment)).join('/')
    : '';
  url.pathname = `/${owner}/${repo}/tree/${encodeURIComponent(commit)}${encodedSubdir ? `/${encodedSubdir}` : ''}`;
  return url.toString();
}
