# `vo get` Integration Test

Tests that `vo get` downloads, extracts, and registers a third-party module
so it can be imported and compiled by Vo programs.

## Prerequisites

Build the `vo` CLI:

```sh
cargo build -p vo
```

## Running the Test

```sh
# Auto-install path: @version in import triggers automatic download on first run
./target/debug/vo run tests/get/main.vo
# expected: fetching github.com/vo-lang/resvg v0.1.0...
#           PASS: resvg.Render returned N bytes

# Manual install path (equivalent, for CI or offline prep)
./target/debug/vo get github.com/vo-lang/resvg@v0.1.0
./target/debug/vo run tests/get/main.vo
# expected: PASS: resvg.Render returned N bytes (no fetch on second run)
```

## What is tested

| Step | What is verified |
|------|------------------|
| `vo run` (first) | `@v0.1.0` in import triggers auto-download + extraction into `~/.vo/mod/` |
| `vo run` (second) | Module already cached → no download, runs immediately |
| `vo get` | Explicit download; `vo.sum` entry written with `h1:SHA256(tarball)` |
| `vo check` | Import `"github.com/vo-lang/resvg@v0.1.0"` resolves via `ModSource` |
| `vo run` | `resvg.Render` returns non-empty PNG bytes at runtime |

## Notes

- Step 4 (`vo check`) works without the Rust extension (type-check only).
- Step 5 (`vo run`) requires `libvo_resvg.so` to be built and registered via `vo.ext.toml`.
- The GitHub repo `github.com/vo-lang/resvg` must exist and have a `v0.1.0` tag.
