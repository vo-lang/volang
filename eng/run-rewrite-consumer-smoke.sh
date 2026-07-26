#!/bin/sh
set -eu

script_directory=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
volang_root=$(CDPATH= cd -- "${script_directory}/.." && pwd)
workspace_parent=$(CDPATH= cd -- "${volang_root}/.." && pwd)
fixture_root="${volang_root}/eng/fixtures/rewrite-consumers"
build_root="${TMPDIR:-/tmp}/volang-rewrite-consumer-build"
smoke_root=$(mktemp -d "${TMPDIR:-/tmp}/volang-rewrite-consumer.XXXXXX")
export VO_GENERATOR_CACHE="${smoke_root}/generator-cache"
export VO_MOD_CACHE="${smoke_root}/module-cache"
mkdir -p "${VO_GENERATOR_CACHE}" "${VO_MOD_CACHE}"

cleanup() {
  rm -rf -- "${smoke_root}"
}
trap cleanup EXIT HUP INT TERM

CARGO_TARGET_DIR="${build_root}" cargo build --manifest-path "${volang_root}/Cargo.toml" -p vo --locked
CARGO_TARGET_DIR="${build_root}" cargo build --manifest-path "${workspace_parent}/vogui/rust/Cargo.toml" -p vogui-codegen --bin vogui-generator-provider --locked
CARGO_TARGET_DIR="${build_root}" cargo build --manifest-path "${workspace_parent}/voplay/rust/Cargo.toml" -p voplay-codegen --bin voplay-generator-provider --locked

run_smoke() {
  framework=$1
  provider=$2
  generator=$3
  case_root="${smoke_root}/${framework}"
  app_root="${case_root}/app"
  module_root="${case_root}/${framework}"

  mkdir -p "${app_root}/bin" "${module_root}"
  cp "${fixture_root}/${framework}/vo.work" "${case_root}/vo.work"
  cp "${fixture_root}/${framework}/vo.mod" "${app_root}/vo.mod"
  cp "${fixture_root}/${framework}/vo.generate.toml" "${app_root}/vo.generate.toml"
  cp "${fixture_root}/${framework}/main.vo" "${app_root}/main.vo"
  if [ "${framework}" = "vogui" ]; then
    cp "${fixture_root}/${framework}/app.schema.toml" "${app_root}/app.schema.toml"
  else
    cp "${fixture_root}/${framework}/components.toml" "${app_root}/components.toml"
  fi
  cp "${build_root}/debug/${generator}" "${app_root}/bin/${generator}"
  cp "${workspace_parent}/${framework}/vo.mod" "${module_root}/vo.mod"
  cp "${workspace_parent}/${framework}/${framework}.vo" "${module_root}/${framework}.vo"
  cp -R "${workspace_parent}/${framework}/vo" "${module_root}/vo"

  "${build_root}/debug/vo" work sync "${app_root}"
  "${build_root}/debug/vo" check "${app_root}"

  for output in ${provider}; do
    if [ -e "${app_root}/${output}" ]; then
      echo "consumer smoke wrote governed output into the application tree: ${app_root}/${output}" >&2
      return 1
    fi
  done
}

run_smoke vogui "smoke_app_app.vo generated/smoke_app_app.manifest" vogui-generator-provider
run_smoke voplay "position_component.vo smoke_game_game.vo generated/voplay_components.manifest" voplay-generator-provider

printf '%s\n' '{"passed":true,"vogui_profile":"headless","voplay_profile":"core","generated_outputs_written_to_app":false}'
