import type { SessionInfo, ShareInfo, StudioMode } from './types';

const SHARE_BASE_URL = 'https://volang.dev/';

export function buildStudioLaunchUrl(options: {
  proj: string;
  mode: StudioMode;
  baseUrl?: string;
}): string {
  const url = new URL(options.baseUrl ?? SHARE_BASE_URL);
  url.search = '';
  url.hash = '';
  url.searchParams.set('mode', options.mode);
  url.searchParams.set('proj', options.proj);
  return url.toString();
}

export function buildGitHubRepoShareInfo(options: {
  owner: string;
  repo: string;
  commit: string | null;
  subdir?: string | null;
  mode?: StudioMode;
  baseUrl?: string;
}): ShareInfo {
  if (!options.commit) {
    return {
      canonicalUrl: '',
      shareable: false,
      reason: 'GitHub session is not pinned to a commit',
    };
  }
  const projectUrl = buildPinnedGitHubProjectUrl(options.owner, options.repo, options.commit, options.subdir ?? null);
  return {
    canonicalUrl: buildStudioLaunchUrl({
      proj: projectUrl,
      mode: options.mode ?? 'runner',
      baseUrl: options.baseUrl,
    }),
    shareable: true,
  };
}

export function buildShareInfo(
  session: Pick<SessionInfo, 'root' | 'entryPath' | 'source'>,
  options: {
    mode?: StudioMode;
    baseUrl?: string;
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
  if (source.kind === 'quickplay') {
    return {
      canonicalUrl: buildStudioLaunchUrl({
        proj: source.spec,
        mode: options.mode ?? 'runner',
        baseUrl: options.baseUrl,
      }),
      shareable: true,
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
    baseUrl: options.baseUrl,
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
