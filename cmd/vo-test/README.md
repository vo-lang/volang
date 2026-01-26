# vo-test - Parallel Test Runner for Vo

A high-performance parallel test runner for the Vo programming language, written in Vo itself.

## Features

- **True Parallel Execution** - Uses goroutine + worker pool pattern to fully utilize multi-core CPUs
- **Real-time Progress Display** - Collects and displays test results in real-time via channels
- **Comprehensive Test Support**:
  - VM and JIT modes
  - GC validation tests
  - no_std tests
  - should_fail tests (compile-time error detection)
  - Project directory tests (proj_*)
  - ZIP file tests
- **Configuration-driven** - Reads skip rules and special configs from `_config.yaml`

## Usage

### Build

```bash
# From repo root
cargo build --release -p vo-test
```

### Run Tests

```bash
# Run all tests (VM + JIT)
./target/release/vo-test both

# Run VM tests only
./target/release/vo-test vm

# Run JIT tests only
./target/release/vo-test jit

# Run GC validation tests
./target/release/vo-test gc

# Verbose output mode
./target/release/vo-test both -v

# Specify number of parallel workers
./target/release/vo-test both -j 8

# Run single file
./target/release/vo-test both test.vo
./target/release/vo-test vm lang/test_data/interface.vo
```

## Architecture

### File Structure

```
cmd/vo-test/
├── main.vo          # Entry point and CLI parsing
├── config.vo        # YAML config parser
├── runner.vo        # Parallel test execution engine
├── files.vo         # File system traversal
├── rust/            # Rust launcher
│   ├── Cargo.toml
│   └── src/main.rs
└── README.md
```

### Parallel Execution Flow

```
1. Collect test files
   ├─ .vo files (recursive)
   ├─ proj_* directories
   └─ .zip files

2. Create test jobs
   ├─ Based on mode (vm/jit/both)
   ├─ Apply skip rules
   └─ Handle should_fail markers

3. Parallel execution
   ├─ Start N worker goroutines
   ├─ Distribute jobs via channel
   └─ Collect results via channel

4. Real-time progress display
   └─ Show each test as it completes

5. Aggregate results
   ├─ Count passed/failed/skipped
   └─ List failed tests
```

### Worker Pool Pattern

```go
// 创建 channel
jobs := make(chan TestJob, len(jobs))
results := make(chan TestResult, len(jobs))

// 启动 workers
for i := 0; i < workers; i++ {
    go testWorker(jobs, results, verbose)
}

// 发送任务
for _, job := range jobs {
    jobs <- job
}
close(jobs)

// 收集结果（实时）
for i := 0; i < len(jobs); i++ {
    result := <-results
    printProgress(result, i+1, len(jobs))
}
```

## Performance Comparison

Compared to Python's `d.py test`:

- **Parallel Execution** - Python is serial, vo-test is truly parallel
- **Faster Startup** - Compiled Vo binary starts faster
- **Better Resource Utilization** - Fully utilizes multi-core CPUs

Expected performance gain: 6-7x speedup on 8-core CPU (accounting for I/O wait).

## Configuration File

`lang/test_data/_config.yaml` format:

```yaml
tests:
  - file: test.vo
    skip: [vm, jit]           # Skip certain modes
    should_fail: true         # Must fail at compile time
    reason: "explanation"     # Reason for skip/should_fail
  
  - file: zip/subdir.zip
    zip_root: src/            # Root directory inside ZIP
```

## Compatibility with d.py

vo-test is fully compatible with `d.py test` behavior:

- Same test discovery logic
- Same configuration file format
- Same output format
- Same exit codes (0 = success, 1 = failure)

Direct replacement:

```bash
# Old way
./d.py test both

# New way
./target/release/vo-test both
```

## Future Improvements

- [ ] Auto-detect CPU core count
- [ ] More precise time formatting
- [ ] Colored output support
- [ ] Detailed error messages for failed tests
- [ ] Test result caching (re-run only failures)
- [ ] JSON output format (for CI)
