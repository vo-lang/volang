export const BLOCKKART_GITHUB_URL = 'https://github.com/vo-lang/BlockKart/tree/main';
export const BLOCKKART_QUICKPLAY_SPEC = 'vo:quickplay:blockkart';

export function staticPackageUrl(path: string): string {
  const url = new URL(path, window.location.origin);
  url.searchParams.set('build', __STUDIO_BUILD_ID__);
  if (url.origin === window.location.origin) {
    return `${url.pathname}${url.search}${url.hash}`;
  }
  return url.toString();
}

export const BLOCKKART_PROJECT_PACKAGE_URL = staticPackageUrl('/quickplay/blockkart/project.json');
export const BLOCKKART_DEPS_PACKAGE_URL = staticPackageUrl('/quickplay/blockkart/deps.json');

export const BLOCKKART_PREFETCH_URLS = [
  BLOCKKART_PROJECT_PACKAGE_URL,
  BLOCKKART_DEPS_PACKAGE_URL,
];
