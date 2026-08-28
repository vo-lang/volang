# Voplay protocol fuzzing

The `voplay_decoders` target exercises the Voplay packet decoder and bounded
partition chunk decoder. Successful packet and partition decodes must preserve
exact length or encode/decode invariants.

Install the repository-pinned fuzzing tools, then run the reproducible bounded
campaign from this directory:

```sh
rustup toolchain install nightly-2026-08-20 --profile minimal
cargo +nightly-2026-08-20 install cargo-fuzz --version 0.13.2 --locked
cargo +nightly-2026-08-20 fuzz run --fuzz-dir . voplay_decoders -- \
  -runs=100000 -max_len=4096 -timeout=10 -seed=424242
```

`cargo-fuzz` supplies libFuzzer coverage feedback and AddressSanitizer. The
nightly workflow runs this target independently and preserves crash artifacts.
