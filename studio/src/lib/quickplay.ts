export const BLOCKKART_GITHUB_URL = 'https://github.com/vo-lang/BlockKart/tree/main';
export const BLOCKKART_QUICKPLAY_SPEC = 'vo:quickplay:blockkart';

function staticPackageUrl(path: string): string {
  const params = new URLSearchParams({ build: __STUDIO_BUILD_ID__ });
  return `${path}?${params.toString()}`;
}

export const BLOCKKART_PROJECT_PACKAGE_URL = staticPackageUrl('/quickplay/blockkart/project.json');
export const BLOCKKART_DEPS_PACKAGE_URL = staticPackageUrl('/quickplay/blockkart/deps.json');

export const BLOCKKART_PREFETCH_URLS = [
  BLOCKKART_PROJECT_PACKAGE_URL,
  BLOCKKART_DEPS_PACKAGE_URL,
  '/quickplay/blockkart/artifacts/github.com@vo-lang@vogui/v0.1.14/vogui.wasm',
  '/quickplay/blockkart/artifacts/github.com@vo-lang@voplay/v0.1.25/voplay_island.js',
  '/quickplay/blockkart/artifacts/github.com@vo-lang@voplay/v0.1.25/voplay_island_bg.wasm',
];
