# App Protocol fuzzing

The `decoders` target sends raw bytes and mutations of valid App envelope,
ChannelOpen, and optional-section seeds through every decoder. Successful
ChannelOpen/Accept and optional-section decodes must round-trip exactly.

Install the repository-pinned fuzzing tools, then run the reproducible bounded
campaign from this directory:

```sh
rustup toolchain install nightly-2026-08-20 --profile minimal
cargo +nightly-2026-08-20 install cargo-fuzz --version 0.13.2 --locked
cargo +nightly-2026-08-20 fuzz run --fuzz-dir . decoders -- \
  -runs=100000 -max_len=4096 -timeout=10 -seed=424242
```

`cargo-fuzz` supplies libFuzzer coverage feedback and AddressSanitizer. The
nightly workflow runs this target independently and preserves crash artifacts.
