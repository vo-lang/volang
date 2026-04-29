#!/usr/bin/env python3
from __future__ import annotations

import argparse
import fnmatch
import json
import os
import subprocess
import sys
from pathlib import Path
from typing import Any

from ci_config import load_ci_config


ROOT = Path(__file__).resolve().parents[2]
TASKS_FILE = ROOT / "scripts" / "ci" / "tasks.toml"


def load_config() -> dict[str, Any]:
    return load_ci_config(TASKS_FILE)


def git_lines(args: list[str], *, check: bool = False) -> list[str]:
    result = subprocess.run(
        ["git", *args],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    if check and result.returncode != 0:
        raise RuntimeError(result.stderr.strip() or f"git {' '.join(args)} failed")
    if result.returncode != 0:
        return []
    return [line.strip() for line in result.stdout.splitlines() if line.strip()]


def changed_files(base: str | None = None, head: str | None = None) -> list[str]:
    if base and set(base) == {"0"}:
        base = None
    if base and head:
        files = git_lines(["diff", "--name-only", f"{base}...{head}"])
        if files:
            return sorted(set(files))

    upstream = git_lines(["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"])
    files: list[str] = []
    if upstream:
        files.extend(git_lines(["diff", "--name-only", f"{upstream[0]}...HEAD"]))
    else:
        files.extend(git_lines(["diff", "--name-only", "HEAD~1...HEAD"]))

    files.extend(git_lines(["diff", "--name-only"]))
    files.extend(git_lines(["diff", "--cached", "--name-only"]))
    files.extend(git_lines(["ls-files", "--others", "--exclude-standard"]))
    return sorted(set(files))


def path_matches(path: str, pattern: str) -> bool:
    if pattern.endswith("/**"):
        prefix = pattern[:-3]
        return path == prefix.rstrip("/") or path.startswith(prefix)
    return fnmatch.fnmatch(path, pattern)


def task_matches(task: dict[str, Any], files: list[str]) -> bool:
    triggers = task.get("triggers", [])
    return any(path_matches(path, pattern) for path in files for pattern in triggers)


def add_task(name: str, tasks: dict[str, Any], out: list[str], seen: set[str]) -> None:
    if name not in tasks:
        raise KeyError(f"unknown CI task: {name}")
    for dep in tasks[name].get("needs", []):
        add_task(dep, tasks, out, seen)
    if name not in seen:
        seen.add(name)
        out.append(name)


def resolve_names(names: list[str], config: dict[str, Any]) -> list[str]:
    groups = config.get("groups", {})
    tasks = config.get("tasks", {})
    out: list[str] = []
    seen: set[str] = set()
    for name in names:
        if name in groups:
            for task_name in groups[name]:
                add_task(task_name, tasks, out, seen)
        else:
            add_task(name, tasks, out, seen)
    return out


def plan_tasks(mode: str, config: dict[str, Any], *, base: str | None = None, head: str | None = None) -> tuple[list[str], list[str]]:
    groups = config.get("groups", {})
    tasks = config.get("tasks", {})
    if mode != "smart":
        return resolve_names([mode], config), []

    files = changed_files(base, head)
    selected = list(config.get("settings", {}).get("smart_default", []))
    for name, task in tasks.items():
        if task.get("internal"):
            continue
        if task_matches(task, files):
            selected.append(name)

    return resolve_names(selected, config), files


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


def task_repos(name: str, tasks: dict[str, Any], seen: set[str] | None = None) -> set[str]:
    seen = seen or set()
    if name in seen:
        return set()
    seen.add(name)
    task = tasks[name]
    repos = {task["repo"]} if task.get("repo") else set()
    for dep in task.get("needs", []):
        repos.update(task_repos(dep, tasks, seen))
    return repos


def matrix_for(task_names: list[str], config: dict[str, Any]) -> dict[str, Any]:
    tasks = config["tasks"]
    include = []
    for name in task_names:
        task = tasks[name]
        tools = task_tools(name, tasks)
        repos = sorted(task_repos(name, tasks))
        include.append(
            {
                "task": name,
                "title": task.get("title", name),
                "tier": task.get("tier", "default"),
                "repo": repos[0] if repos else "",
                "rust": "rust" in tools,
                "node": "node" in tools,
                "wasm_pack": "wasm-pack" in tools,
            }
        )
    return {"include": include}


def write_github_output(values: dict[str, str]) -> None:
    output = os.environ.get("GITHUB_OUTPUT")
    if not output:
        for key, value in values.items():
            print(f"{key}={value}")
        return
    with open(output, "a", encoding="utf-8") as handle:
        for key, value in values.items():
            handle.write(f"{key}={value}\n")


def main() -> int:
    parser = argparse.ArgumentParser(description="Plan Vo CI tasks")
    parser.add_argument("--mode", default="smart")
    parser.add_argument("--base", default=os.environ.get("BASE_SHA"))
    parser.add_argument("--head", default=os.environ.get("HEAD_SHA") or os.environ.get("GITHUB_SHA"))
    parser.add_argument("--format", choices=["text", "json", "matrix"], default="text")
    parser.add_argument("--github-output", action="store_true")
    args = parser.parse_args()

    config = load_config()
    try:
        task_names, files = plan_tasks(args.mode, config, base=args.base, head=args.head)
    except KeyError as exc:
        print(str(exc), file=sys.stderr)
        return 2

    matrix = matrix_for(task_names, config)
    if args.github_output:
        write_github_output(
            {
                "tasks": json.dumps(task_names, separators=(",", ":")),
                "matrix": json.dumps(matrix, separators=(",", ":")),
                "has_tasks": "true" if task_names else "false",
            }
        )

    if args.format == "json":
        print(json.dumps({"tasks": task_names, "changed_files": files}, indent=2))
    elif args.format == "matrix":
        print(json.dumps(matrix, indent=2))
    else:
        if files:
            print("Changed files:")
            for path in files:
                print(f"  {path}")
            print()
        print("Planned CI tasks:")
        for name in task_names:
            title = config["tasks"][name].get("title", name)
            print(f"  {name}: {title}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
