#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "${SCRIPT_DIR}/.." && pwd)"
MODE="${1:-smart}"

cd "${REPO_ROOT}"
exec "${REPO_ROOT}/d.py" ci "${MODE}" "${@:2}"
