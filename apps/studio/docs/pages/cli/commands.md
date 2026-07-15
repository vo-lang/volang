# CLI Commands

The `vo` command-line tool provides everything you need to develop, build, and manage Vo projects.

## Common Commands

### `vo run <file|dir> [args...]`

Run a Vo program.

```bash
vo run hello.vo
vo run .                        # run current directory (module project)
vo run main.vo --mode=jit       # run with JIT backend
vo run server.vo -- --port 8080 # pass args to program after --
vo run main.vo --codegen        # print bytecode instead of running
```

### `vo build [path] [-o out]`

Compile to a bytecode artifact (`.vob`) without running.

```bash
vo build .                # produces <module-name>.vob
vo build main.vo -o app.vob
```

### `vo check [path]`

Type-check without running. Returns exit code 0 on success.

```bash
vo check .
vo check main.vo
```

### `vo test [path] [--mode=jit]`

Run tests. Looks for a `tests/` directory by default.

```bash
vo test                   # run tests/ or current directory
vo test tests/unit.vo     # run specific test file
vo test --mode=jit        # run with JIT backend
```

### `vo fmt [file|dir...] [--check]`

Format Vo source files in place.

```bash
vo fmt .                  # format all .vo files recursively
vo fmt main.vo lib.vo     # format specific files
vo fmt --check            # check formatting without modifying (exit 1 if unformatted)
```

### `vo init <module-path>`

Initialize a new module project.

```bash
mkdir myapp
cd myapp
vo init github.com/your-name/myapp
```

Creates `vo.mod` in the current directory. Source files remain user-owned; add
`main.vo` or another package file separately.

## Module Commands

### `vo mod add <module[@version]>`

Add a dependency to `vo.mod`.

### `vo mod update [module]`

Update dependencies to latest compatible versions.

### `vo mod sync [path]`

Recompute the dependency graph. It refreshes `vo.lock` for external
dependencies and omits the file for an empty external graph.

### `vo mod download [path]`

Fetch all pinned dependencies. An empty external graph is a successful no-op.

### `vo mod verify [path]`

Verify the module graph, its canonical lock state, and cached dependency integrity.

### `vo mod remove <module>`

Remove a dependency.

## Advanced Commands

### `vo emit <file|dir> [-o out]`

Compile source to a bytecode binary file.

```bash
vo emit main.vo          # writes main.vob beside the source file
vo emit .                # writes <module-name>.vob in the current directory
vo emit . -o app.vob     # uses an explicit output path
```

`build` and `emit` replace their output atomically, so a failed write cannot
leave a partially written bytecode artifact at the destination.

### `vo dump <file.vob>`

Disassemble a compiled bytecode file for inspection.

### `vo version`

Show version, build commit, and build date.

### `vo help`

Show usage information.
