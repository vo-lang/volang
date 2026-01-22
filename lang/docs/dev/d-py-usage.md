# d.py Usage Guide

Vo development tool for testing, benchmarking, and project management.

**Always run from repo root:**
```bash
cd /path/to/volang
./d.py <command>
```

---

## Test Commands

### Batch Testing

| Command | Description |
|---------|-------------|
| `./d.py test` | Run all tests (VM + JIT) |
| `./d.py test vm` | VM mode only |
| `./d.py test jit` | JIT mode only |
| `./d.py test gc` | GC tests only (`gc_*.vo`, enables `VO_GC_DEBUG`) |
| `./d.py test nostd` | no_std mode (vo-embed) |
| `./d.py test wasm` | WASM mode (vo-web) |

### Single File Testing

| Command | Description |
|---------|-------------|
| `./d.py test lang/test_data/foo.vo` | Run single file (VM + JIT) |
| `./d.py test vm lang/test_data/foo.vo` | Run single file (VM only) |
| `./d.py test jit lang/test_data/foo.vo` | Run single file (JIT only) |

### Options

| Option | Description |
|--------|-------------|
| `-v, --verbose` | Show detailed output |
| `--arch=32` | Run on 32-bit ARM (via QEMU) |

### Examples

```bash
./d.py test                           # All tests (VM + JIT)
./d.py test -v jit                    # JIT tests with verbose output
./d.py test vm lang/test_data/foo.vo  # Single file, VM only
./d.py test gc                        # GC stress tests
```

---

## Benchmark Commands

| Command | Description |
|---------|-------------|
| `./d.py bench` | Run all benchmarks (Vo + C + Go + Node) |
| `./d.py bench vo` | Vo + C only (VM = 100 baseline) |
| `./d.py bench fibonacci` | Single benchmark by name |
| `./d.py bench score` | Analyze existing results |
| `./d.py bench --all-langs` | Include Python and Ruby |

---

## Code Statistics

| Command | Description |
|---------|-------------|
| `./d.py loc` | Line count by crate |
| `./d.py loc --with-tests` | Include test files |

---

## Cache Management

| Command | Description |
|---------|-------------|
| `./d.py clean` | Clean all caches (vo + rust) |
| `./d.py clean vo` | Clean `.vo-cache` only |
| `./d.py clean rust` | Run `cargo clean` |

---

## Playground

| Command | Description |
|---------|-------------|
| `./d.py play` | Build WASM + start dev server |
| `./d.py play --build-only` | Build WASM only |

---

## Direct Cargo Commands

For more control over single file execution:

```bash
# VM mode
cargo run --bin vo lang/cli run lang/test_data/foo.vo --mode=vm

# JIT mode
cargo run --bin vo lang/cli run lang/test_data/foo.vo --mode=jit

# JIT with immediate compilation (matches d.py test jit)
VO_JIT_CALL_THRESHOLD=1 cargo run --bin vo lang/cli run lang/test_data/foo.vo --mode=jit
```

---

## Debug Options

```bash
# Print AST
cargo run --bin vo lang/cli run lang/test_data/foo.vo --ast

# Print bytecode
cargo run --bin vo lang/cli run lang/test_data/foo.vo --codegen

# Print Cranelift IR (JIT debugging)
cargo run --bin vo lang/cli run lang/test_data/foo.vo --mode=jit --cranelift-ir
```

---

## Environment Variables

| Variable | Description |
|----------|-------------|
| `VO_GC_DEBUG=1` | Enable GC debug logging |
| `VO_JIT_CALL_THRESHOLD=1` | Force immediate JIT compilation |

---

## Test Configuration

Test behavior is configured in `lang/test_data/_config.yaml`:

```yaml
tests:
  - file: some_test.vo
    skip: [jit]           # Skip JIT mode
  - file: fail_test.vo
    should_fail: true     # Expected to fail at compile time
  - file: archive.zip
    zip_root: src/        # Entry point within zip
```
