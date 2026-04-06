#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "${SCRIPT_DIR}/.." && pwd)"

chmod +x "${REPO_ROOT}/scripts/ci-local.sh"
chmod +x "${REPO_ROOT}/scripts/install-git-hooks.sh"
chmod +x "${REPO_ROOT}/.githooks/pre-commit"
chmod +x "${REPO_ROOT}/.githooks/pre-push"

git -C "${REPO_ROOT}" config core.hooksPath .githooks
printf 'Configured core.hooksPath=%s\n' "$(git -C "${REPO_ROOT}" config core.hooksPath)"
