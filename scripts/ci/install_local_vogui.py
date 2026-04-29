#!/usr/bin/env python3
from __future__ import annotations

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


def vogui_js_path() -> Path:
    for root in candidate_roots():
        path = root / "vogui" / "js"
        if path.exists():
            return path
    searched = ", ".join(str(root / "vogui" / "js") for root in candidate_roots())
    raise FileNotFoundError(f"could not find vogui/js; searched: {searched}")


def main() -> int:
    try:
        package_path = vogui_js_path()
    except FileNotFoundError as exc:
        print(str(exc), file=sys.stderr)
        return 2
    return subprocess.run(
        ["npm", "install", "--no-save", str(package_path)],
        cwd=ROOT / "studio",
    ).returncode


if __name__ == "__main__":
    raise SystemExit(main())
