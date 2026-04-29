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
        if (path / ".git").exists() or path.exists():
            return path
    searched = ", ".join(str(root / repo) for root in candidate_roots())
    raise FileNotFoundError(f"could not find first-party repo {repo}; searched: {searched}")


def main() -> int:
    parser = argparse.ArgumentParser(description="Run a command inside a first-party sibling module")
    parser.add_argument("repo")
    parser.add_argument("subdir")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()

    command = args.command
    if command and command[0] == "--":
        command = command[1:]
    if not command:
        print("missing command after --", file=sys.stderr)
        return 2

    try:
        cwd = repo_path(args.repo) / args.subdir
    except FileNotFoundError as exc:
        print(str(exc), file=sys.stderr)
        return 2
    if not cwd.exists():
        print(f"missing directory: {cwd}", file=sys.stderr)
        return 2
    return subprocess.run(command, cwd=cwd).returncode


if __name__ == "__main__":
    raise SystemExit(main())
