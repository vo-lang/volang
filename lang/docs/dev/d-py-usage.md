# d.py

`d.py` is the stable compatibility wrapper at the repository root. It keeps the
short local commands while delegating all command behavior to `vo-dev dpy`.

Run it from the repository root:

```bash
./d.py test [target|alias] [--release] [-v|--verbose] [-j N|--jobs N] [--repeat N|-n N] [file-or-dir]
./d.py ci [smart|quality|test|site|pr|full|release-verify|task <task-name>|task:<task-name>]
./d.py bench [all|vo|score|<name>] [--all-langs]
./d.py loc [--with-tests]
./d.py clean [all|vo|rust|bench|junk]
./d.py studio [--build-wasm] [--build-only] [--runner] [project]
./d.py studio-native [--build-wasm] [--runner] [project]
./d.py studio-stop
./d.py gc-perf [--release] [--json] [--objects=N|--small|--large] [dead-sweep|live-chain|root-table|sparse-root-table|interior-root-table]
./d.py run <file.vo> [--mode=vm|jit] [--codegen]
./d.py vo <args...>
```

Language tests are manifest-driven. Source files live under `tests/lang/cases/`;
case metadata lives in `tests/lang/manifest.toml`. Add or remove both together.
The Python wrapper only checks that it is running from the repo root and then
executes `cargo run -q -p vo-dev -- dpy ...`. Compatibility parsing lives in
`cmd/vo-dev/src/dpy_compat.rs`; it consumes `--release` only for commands that
document it here, such as `test` and `gc-perf`. Forwarded `vo` and default
commands keep their original argument list.

`./d.py clean junk` removes local-only clutter such as `.DS_Store`,
`__pycache__`, `.pyc`, and the repo-root `.tmp/` directory. `clean vo` removes
Vo compile caches and simple bytecode leftovers. `clean bench` removes benchmark
results and build products. `clean all` combines junk, Vo, benchmark, and Rust
cleanup.

Examples:

```bash
./d.py test both
./d.py test jit --release
./d.py test compile tests/lang/cases/typechecker/goto_stmt.vo
./d.py test wasm tests/lang/cases/runtime/slice/append_self_slice.vo
./d.py ci smart
./d.py ci task cargo-check
./d.py ci task:studio-wasm-build
```

# vo (cmd/vo)

```bash
cargo run -p vo -- run <file> [--mode=jit] [--codegen]
cargo run -p vo -- build [path]
cargo run -p vo -- check [path]
cargo run -p vo -- dump <file.vob|file.vot>
cargo run -p vo -- compile <file.vot> [-o out.vob]
cargo run -p vo -- emit <file>
cargo run -p vo -- init <module-path>
cargo run -p vo -- mod add <module[@constraint]>
cargo run -p vo -- mod update [module]
cargo run -p vo -- mod sync [path]
cargo run -p vo -- mod verify [path]
cargo run -p vo -- mod download [path]
cargo run -p vo -- mod remove <module>
cargo run -p vo -- release verify [path]
cargo run -p vo -- release stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]
cargo run -p vo -- help
cargo run -p vo -- version
```
