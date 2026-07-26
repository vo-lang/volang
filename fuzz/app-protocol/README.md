# App Protocol fuzzing

The `decoders` target sends raw bytes and mutations of valid App envelope,
ChannelOpen, and optional-section seeds through every decoder. Successful
ChannelOpen/Accept and optional-section decodes must round-trip exactly.

Build a coverage-instrumented target on Apple ARM64 without instrumenting Cargo
host build scripts:

```sh
CARGO_TARGET_DIR=/private/tmp/vo-app-protocol-fuzz-coverage \
RUSTFLAGS="-C passes=sancov-module -C llvm-args=-sanitizer-coverage-level=3 -C llvm-args=-sanitizer-coverage-inline-8bit-counters" \
cargo build --manifest-path fuzz/app-protocol/Cargo.toml --release --offline \
  --bin decoders --target aarch64-apple-darwin
```

Run the reproducible bounded campaign from the repository root:

```sh
/private/tmp/vo-app-protocol-fuzz-coverage/aarch64-apple-darwin/release/decoders \
  -runs=100000 -max_len=4096 -seed=424242
```

This stable-toolchain command provides libFuzzer sanitizer coverage. An ASan
campaign additionally requires a Rust toolchain and sanitizer runtime that
support `-Z sanitizer=address`.
