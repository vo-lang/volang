#!/usr/bin/env python3
from __future__ import annotations

import json
import shutil
import sys
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen


def fetch_bytes(url: str) -> bytes:
    request = Request(
        url,
        headers={
            'User-Agent': 'volang-release-mirror',
            'Accept': 'application/vnd.github+json',
        },
    )
    try:
        with urlopen(request) as response:
            return response.read()
    except HTTPError as error:
        raise SystemExit(f'HTTP {error.code} for {url}') from error
    except URLError as error:
        raise SystemExit(f'failed to fetch {url}: {error}') from error


def fetch_json(url: str) -> dict:
    return json.loads(fetch_bytes(url).decode('utf-8'))


def parse_module_repo(module: str) -> tuple[str, str]:
    parts = module.split('/')
    if len(parts) != 3 or parts[0] != 'github.com':
        raise SystemExit(f'unsupported module path: {module}')
    return parts[1], parts[2]


def mirror_latest_release(module: str, out_root: Path) -> None:
    owner, repo = parse_module_repo(module)
    release = fetch_json(f'https://api.github.com/repos/{owner}/{repo}/releases/latest')
    tag = release.get('tag_name')
    assets = release.get('assets')
    if not isinstance(tag, str) or not tag:
        raise SystemExit(f'missing tag_name for latest release of {module}')
    if not isinstance(assets, list) or not assets:
        raise SystemExit(f'latest release for {module}@{tag} has no assets')

    release_root = out_root / owner / repo / 'releases' / 'download'
    if release_root.exists():
        shutil.rmtree(release_root)
    target_dir = release_root / tag
    target_dir.mkdir(parents=True, exist_ok=True)

    for asset in assets:
        name = asset.get('name')
        url = asset.get('browser_download_url')
        if not isinstance(name, str) or not isinstance(url, str):
            raise SystemExit(f'invalid asset metadata for {module}@{tag}')
        asset_path = target_dir / name
        asset_path.write_bytes(fetch_bytes(url))
        print(f'mirrored {module}@{tag} {name}')


def main() -> int:
    if len(sys.argv) < 3:
        print('usage: mirror_release_assets.py <out_root> <module> [<module> ...]', file=sys.stderr)
        return 1

    out_root = Path(sys.argv[1]).resolve()
    modules = sys.argv[2:]
    for module in modules:
        mirror_latest_release(module, out_root)
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
