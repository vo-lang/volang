# GoX Test Framework

File-based integration tests for the GoX compiler.

## Directory Structure

```
test_data/
├── *.gox                 # Single-file tests
├── proj_<name>/          # Multi-file tests (folders starting with proj_)
│   ├── main.gox
│   └── helper.gox
├── codegen/              # Subdirectories are traversed recursively
│   └── basic/
│       └── arithmetic.gox
└── typecheck/
    └── 01_basic_types.gox
```

## Test File Format

### Single-File Tests (*.gox)

```gox
package main

func main() {
    x := 1 + 2
    assert(x == 3, "1 + 2 should be 3")
}

=== parser ===
// Expected AST output (optional)

=== typecheck ===
OK
// Or expected errors: [E2100] undefined

=== codegen ===
// Expected text-format bytecode (optional)

=== vm ===
[V1001] assertion failed
```

### Sections

All sections are **optional**:

| Section | Purpose |
|---------|---------|
| `=== parser ===` | Verify AST matches expected output |
| `=== typecheck ===` | Verify type errors or `OK` for no errors |
| `=== codegen ===` | Verify text-format bytecode output |
| `=== vm ===` | Verify expected VM error code (e.g., `[V1001]`) |

### Test Execution Flow

Each test file goes through:

1. **Parse** → If `=== parser ===` present, verify AST matches
2. **Type Check** → If `=== typecheck ===` present, verify errors/OK
3. **Compile** → If `=== codegen ===` present, verify bytecode text
4. **Run** → Execute code
   - If `=== vm ===` present, verify VM error matches
   - Otherwise, `assert` failures = test failure

**Code must run successfully unless `=== vm ===` expects an error.**

### Multi-File Tests (proj_* directories)

Directories starting with `proj_` are compiled as complete projects. Must contain `main.gox`.

```
test_data/
└── proj_myproject/
    ├── main.gox
    ├── helper.gox
    └── types.gox
```

The test runner traverses `test_data/` recursively:
- `*.gox` files → single-file tests
- `proj_*` directories → multi-file tests

## Running Tests

```bash
# Run all tests
cargo test -p gox-tests

# Run specific test category
cargo test -p gox-tests test_codegen
cargo test -p gox-tests test_parser
cargo test -p gox-tests test_typecheck
```

## Output Format

```
Running 50 tests...
  ✓ parser/01_empty.gox
  ✓ typecheck/01_basic_types.gox
  ✗ codegen/single/closure.gox
    assertion failed: expected 10, got 11
  ...

Results: 49 passed, 1 failed
FAILED (exit 1)
```

## Exit Codes

- `0` - All tests passed
- `1` - One or more tests failed

## Writing Tests

### Parser Test Example

```gox
package main

var x int

=== parser ===
File {
  package: "main"
  decls: [Var { names: ["x"], type: int }]
}
```

### Type Check Test Example

```gox
package main

func main() {
    var x int = "hello"  // type mismatch
}

=== typecheck ===
[E2203] cannot use string as int
```

### Codegen Test Example

```gox
package main

func main() {
    x := 1 + 2
    assert(x == 3, "addition works")
}
```

No sections needed - just uses `assert()` to verify runtime behavior.
