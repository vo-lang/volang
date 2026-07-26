#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
  echo "usage: run-rewrite-profile-artifact-audit.sh voplay|vogui" >&2
  exit 2
fi

script_directory=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
volang_root=$(CDPATH= cd -- "${script_directory}/.." && pwd)
workspace_parent=$(CDPATH= cd -- "${volang_root}/.." && pwd)
target_triple=wasm32-unknown-unknown
audit_root="${TMPDIR:-/tmp}/volang-rewrite-profile-audit"
mkdir -p "${audit_root}"

measure_profile() {
  framework=$1
  package=$2
  feature=$3
  profile=$4
  gzip_limit=$5
  manifest="${workspace_parent}/${framework}/rust/Cargo.toml"
  target_root="${audit_root}/${framework}"
  artifact_stem=$(printf '%s' "${package}" | tr '-' '_')
  artifact="${target_root}/${target_triple}/release/${artifact_stem}.wasm"
  tree_file="${audit_root}/${framework}-${profile}.tree"

  CARGO_TARGET_DIR="${target_root}" cargo build \
    --manifest-path "${manifest}" \
    --release \
    --target "${target_triple}" \
    -p "${package}" \
    --no-default-features \
    --features "${feature}" \
    --locked

  cargo tree \
    --manifest-path "${manifest}" \
    --target "${target_triple}" \
    -p "${package}" \
    --no-default-features \
    --features "${feature}" \
    --locked > "${tree_file}"

  node "${script_directory}/report-profile-artifact-size.mjs" \
    --framework "${framework}" \
    --profile "${profile}" \
    --target "${target_triple}" \
    --artifact "${artifact}" \
    --dependency-tree "${tree_file}" \
    --gzip-limit "${gzip_limit}"
}

case "$1" in
  voplay)
    measure_profile voplay voplay-extension profile-core core 256000
    measure_profile voplay voplay-extension profile-2d 2d 921600
    measure_profile voplay voplay-extension profile-3d 3d 1572864
    measure_profile voplay voplay-extension profile-full full 1887436
    ;;
  vogui)
    measure_profile vogui vogui-extension profile-web-minimal web-minimal 0
    measure_profile vogui vogui-extension profile-web-full web-full 0
    ;;
  *)
    echo "unknown profile artifact suite: $1" >&2
    exit 2
    ;;
esac

printf '{"passed":true,"framework":"%s","target":"%s"}\n' "$1" "${target_triple}"
