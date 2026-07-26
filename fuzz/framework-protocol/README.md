# Framework protocol fuzzing

The `framework_decoders` target exercises Vogui and Voplay packet decoders,
Vogui's bounded renderer transaction decoder, and Voplay's bounded partition
chunk decoder. Successful packet and partition decodes must preserve exact
length or encode/decode invariants; failed Vogui transactions must preserve the
renderer mirror's last-good revision.

Build an Apple ARM64 coverage target from the Volang repository root:

```sh
CARGO_TARGET_DIR=/private/tmp/vo-framework-protocol-fuzz-coverage \
RUSTFLAGS="-C passes=sancov-module -C llvm-args=-sanitizer-coverage-level=3 -C llvm-args=-sanitizer-coverage-inline-8bit-counters" \
cargo build --manifest-path fuzz/framework-protocol/Cargo.toml --release --offline \
  --bin framework_decoders --target aarch64-apple-darwin
```

Run a reproducible bounded campaign:

```sh
/private/tmp/vo-framework-protocol-fuzz-coverage/aarch64-apple-darwin/release/framework_decoders \
  -runs=100000 -max_len=4096 -seed=424242
```

This stable-toolchain command provides libFuzzer sanitizer coverage. An ASan
campaign additionally requires a Rust toolchain and sanitizer runtime that
support `-Z sanitizer=address`.
