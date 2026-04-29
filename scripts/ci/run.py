#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

from ci_config import load_ci_config
from plan import plan_tasks, resolve_names


ROOT = Path(__file__).resolve().parents[2]
TASKS_FILE = ROOT / "scripts" / "ci" / "tasks.toml"


class Colors:
    BOLD = "\033[1m"
    DIM = "\033[2m"
    GREEN = "\033[0;32m"
    RED = "\033[0;31m"
    CYAN = "\033[0;36m"
    NC = "\033[0m"

    @classmethod
    def disable(cls) -> None:
        cls.BOLD = cls.DIM = cls.GREEN = cls.RED = cls.CYAN = cls.NC = ""


if not sys.stdout.isatty():
    Colors.disable()


def load_config() -> dict[str, Any]:
    return load_ci_config(TASKS_FILE)


def display_cmd(cmd: list[str]) -> str:
    return " ".join(cmd)


def task_tools(name: str, tasks: dict[str, Any], seen: set[str] | None = None) -> set[str]:
    seen = seen or set()
    if name in seen:
        return set()
    seen.add(name)
    task = tasks[name]
    tools = set(task.get("tools", []))
    for dep in task.get("needs", []):
        tools.update(task_tools(dep, tasks, seen))
    return tools


def ensure_tools(task_name: str, task: dict[str, Any], tasks: dict[str, Any]) -> None:
    missing: list[str] = []
    for tool in sorted(task_tools(task_name, tasks)):
        if tool == "rust":
            continue
        cmd = "wasm-pack" if tool == "wasm-pack" else tool
        if shutil.which(cmd) is None:
            missing.append(cmd)
    if missing:
        print(f"{Colors.RED}missing CI tool(s): {', '.join(missing)}{Colors.NC}", file=sys.stderr)
        print(f"task: {task_name}", file=sys.stderr)
        sys.exit(127)


def run_task(name: str, config: dict[str, Any]) -> None:
    tasks = config["tasks"]
    task = tasks[name]
    ensure_tools(name, task, tasks)

    cwd = ROOT / task.get("cwd", ".")
    command = task["command"]
    title = task.get("title", name)
    print(f"\n{Colors.BOLD}==> {title}{Colors.NC}", flush=True)
    print(f"{Colors.DIM}{cwd.relative_to(ROOT)}$ {display_cmd(command)}{Colors.NC}", flush=True)

    start = time.monotonic()
    result = subprocess.run(command, cwd=cwd, env=os.environ.copy())
    elapsed = time.monotonic() - start
    if result.returncode != 0:
        print(f"{Colors.RED}failed:{Colors.NC} {name} ({elapsed:.1f}s)", file=sys.stderr)
        sys.exit(result.returncode)
    print(f"{Colors.GREEN}ok:{Colors.NC} {name} ({elapsed:.1f}s)")


def main() -> int:
    parser = argparse.ArgumentParser(description="Run Vo CI tasks")
    parser.add_argument(
        "selector",
        nargs="*",
        help="smart, a group name, task <name>, or one or more task/group names",
    )
    parser.add_argument("--base", default=os.environ.get("BASE_SHA"))
    parser.add_argument("--head", default=os.environ.get("HEAD_SHA") or os.environ.get("GITHUB_SHA"))
    args = parser.parse_args()

    config = load_config()
    selectors = args.selector or ["smart"]
    if selectors[0] == "task":
        if len(selectors) < 2:
            print("usage: run.py task <name> [<name>...]", file=sys.stderr)
            return 2
        selected = resolve_names(selectors[1:], config)
    elif selectors == ["smart"]:
        selected, files = plan_tasks("smart", config, base=args.base, head=args.head)
        if files:
            print("Changed files:", flush=True)
            for path in files:
                print(f"  {path}", flush=True)
    else:
        selected = resolve_names(selectors, config)

    print(f"{Colors.CYAN}CI tasks:{Colors.NC} {', '.join(selected) if selected else '(none)'}", flush=True)
    for name in selected:
        run_task(name, config)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
