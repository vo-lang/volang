#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
  echo "usage: run-rewrite-protocol-fuzz.sh app|framework" >&2
  exit 2
fi

script_directory=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
volang_root=$(CDPATH= cd -- "${script_directory}/.." && pwd)
host_triple=$(rustc -vV | sed -n 's/^host: //p')
runs=100000
max_len=4096
seed=424242
export RUSTFLAGS="-C passes=sancov-module -C llvm-args=-sanitizer-coverage-level=3 -C llvm-args=-sanitizer-coverage-inline-8bit-counters"

case "$1" in
  app)
    manifest="${volang_root}/fuzz/app-protocol/Cargo.toml"
    target_root="${TMPDIR:-/tmp}/volang-rewrite-app-protocol-fuzz"
    binary=decoders
    ;;
  framework)
    manifest="${volang_root}/fuzz/framework-protocol/Cargo.toml"
    target_root="${TMPDIR:-/tmp}/volang-rewrite-framework-protocol-fuzz"
    binary=framework_decoders
    ;;
  *)
    echo "unknown protocol fuzz suite: $1" >&2
    exit 2
    ;;
esac

CARGO_TARGET_DIR="${target_root}" cargo build \
  --manifest-path "${manifest}" \
  --release \
  --offline \
  --locked \
  --bin "${binary}" \
  --target "${host_triple}"

"${target_root}/${host_triple}/release/${binary}" \
  "-runs=${runs}" \
  "-max_len=${max_len}" \
  "-seed=${seed}"

printf '{"passed":true,"suite":"%s","runs":%s,"max_len":%s,"seed":%s,"target":"%s"}\n' \
  "$1" "${runs}" "${max_len}" "${seed}" "${host_triple}"
