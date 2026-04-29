from __future__ import annotations

import ast
from pathlib import Path
from typing import Any


def parse_value(raw: str) -> Any:
    value = raw.strip()
    if value == "true":
        return True
    if value == "false":
        return False
    if value.startswith("[") or value.startswith('"'):
        return ast.literal_eval(value)
    try:
        return int(value)
    except ValueError:
        return value


def table_for(root: dict[str, Any], name: str) -> dict[str, Any]:
    current = root
    for part in name.split("."):
        current = current.setdefault(part, {})
    return current


def load_ci_config(path: Path) -> dict[str, Any]:
    data: dict[str, Any] = {}
    current = data
    pending_key = ""
    pending_lines: list[str] = []

    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue

        if pending_key:
            pending_lines.append(line)
            if line == "]":
                current[pending_key] = parse_value(" ".join(pending_lines))
                pending_key = ""
                pending_lines = []
            continue

        if line.startswith("[") and line.endswith("]"):
            current = table_for(data, line[1:-1])
            continue

        key, sep, value = line.partition("=")
        if not sep:
            raise ValueError(f"invalid tasks.toml line: {raw_line}")
        key = key.strip()
        value = value.strip()
        if value == "[":
            pending_key = key
            pending_lines = ["["]
            continue
        current[key] = parse_value(value)

    if pending_key:
        raise ValueError(f"unterminated array for key: {pending_key}")
    return data
