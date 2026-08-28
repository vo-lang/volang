# Volang UI artifact fuzzing

Install the repository-pinned fuzzing tools, then run the bounded VUA1 and VUB1
decoder target from this directory:

```sh
rustup toolchain install nightly-2026-08-20 --profile minimal
cargo +nightly-2026-08-20 install cargo-fuzz --version 0.13.2 --locked
cargo +nightly-2026-08-20 fuzz run --fuzz-dir . ui_artifact_decoders -- \
  -runs=100000 -max_len=65536 -timeout=10 -seed=424242
```

The target caps every decoder table below production limits so arbitrary input
cannot turn one fuzz iteration into an unbounded allocation or traversal.
`cargo-fuzz` supplies libFuzzer coverage feedback and AddressSanitizer; running
the target through an ordinary `cargo build --release` is not an equivalent
fuzzing campaign.
