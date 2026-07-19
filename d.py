#!/usr/bin/env python3
"""Compatibility entrypoint for Volang developer commands.

All command ownership lives in `cmd/vo-dev`; this file only verifies it was
started from the repository root and dispatches the requested command.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parent


def main() -> int:
    if Path.cwd().resolve() != PROJECT_ROOT:
        print(f"Error: d.py must be run from repo root: {PROJECT_ROOT}", file=sys.stderr)
        print(f"Current directory: {Path.cwd()}", file=sys.stderr)
        print(f"Run: cd {PROJECT_ROOT} && ./d.py ...", file=sys.stderr)
        return 1

    cmd = [
        "cargo",
        "run",
        "-q",
        "-p",
        "vo-dev",
        "--locked",
        "--",
        "dpy",
        *sys.argv[1:],
    ]
    return subprocess.run(cmd, cwd=PROJECT_ROOT).returncode


if __name__ == "__main__":
    sys.exit(main())
