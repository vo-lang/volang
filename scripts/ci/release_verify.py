#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


def candidate_roots() -> list[Path]:
    roots: list[Path] = []
    env_root = os.environ.get("CI_MODULE_ROOT")
    if env_root:
        roots.append(Path(env_root))
    roots.extend(
        [
            ROOT / "ci_modules",
            ROOT.parent,
        ]
    )
    return roots


def repo_path(repo: str) -> Path:
    for root in candidate_roots():
        path = root / repo
        if (path / "vo.mod").exists() or (path / ".git").exists():
            return path
    searched = ", ".join(str(root / repo) for root in candidate_roots())
    raise FileNotFoundError(f"could not find first-party repo {repo}; searched: {searched}")


def main() -> int:
    parser = argparse.ArgumentParser(description="Run release verify for a first-party Vo module")
    parser.add_argument("repo", choices=["vogui", "voplay", "vopack", "vostore"])
    args = parser.parse_args()

    try:
        module = repo_path(args.repo)
    except FileNotFoundError as exc:
        print(str(exc), file=sys.stderr)
        return 2

    cmd = ["cargo", "run", "-p", "vo", "--release", "--", "release", "verify", str(module)]
    print(f"release verify {args.repo}: {module}")
    return subprocess.run(cmd, cwd=ROOT).returncode


if __name__ == "__main__":
    raise SystemExit(main())
