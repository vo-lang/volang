#!/usr/bin/env python3
from __future__ import annotations

import sys
from pathlib import Path

from plan import load_config, matrix_for, resolve_names


ROOT = Path(__file__).resolve().parents[2]


def main() -> int:
    for path in sorted((ROOT / "scripts" / "ci").glob("*.py")):
        compile(path.read_text(encoding="utf-8"), str(path), "exec")
    d_py = ROOT / "d_py.py"
    compile(d_py.read_text(encoding="utf-8"), str(d_py), "exec")

    config = load_config()
    for group in sorted(config.get("groups", {})):
        tasks = resolve_names([group], config)
        matrix_for(tasks, config)

    print("ci self-check: ok")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"ci self-check failed: {exc}", file=sys.stderr)
        raise
