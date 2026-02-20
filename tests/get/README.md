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
# 1. Download the module from GitHub
./target/debug/vo get github.com/vo-lang/resvg@v0.1.0

# 2. Verify the module was extracted
ls ~/.vo/mod/github.com/vo-lang/resvg/
# expected: resvg.vo  vo.mod  vo.ext.toml  ...

# 3. Verify vo.sum was updated
grep resvg ~/.vo/mod/vo.sum
# expected: github.com/vo-lang/resvg v0.1.0 h1:...

# 4. Type-check the test project (requires the module to be present)
./target/debug/vo check tests/get

# 5. Run the test (requires the resvg native extension .so to be built)
./target/debug/vo run tests/get/main.vo
# expected: PASS: resvg.Render returned N bytes
```

## What is tested

| Step | What is verified |
|------|-----------------|
| `vo get` | GitHub tarball download + extraction into `~/.vo/mod/` |
| `vo get` | `vo.sum` entry written with `h1:SHA256(tarball)` |
| `vo check` | Import `"github.com/vo-lang/resvg"` resolves via `ModSource` |
| `vo run` | `resvg.Render` returns non-empty PNG bytes at runtime |

## Notes

- Step 4 (`vo check`) works without the Rust extension (type-check only).
- Step 5 (`vo run`) requires `libvo_resvg.so` to be built and registered via `vo.ext.toml`.
- The GitHub repo `github.com/vo-lang/resvg` must exist and have a `v0.1.0` tag.
