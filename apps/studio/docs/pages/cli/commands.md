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

### `vo mod add <module[@constraint]>`

Add or update a direct dependency, solve the graph, and write `vo.mod` and
`vo.lock`. Constraints use bare semantic versions, such as `1.2.3`, `^1.2.3`,
or `~1.2.3`. When the constraint is omitted, the command selects the latest
stable release and records the corresponding compatible constraint. Existing
unrelated selections remain stable while they satisfy the new graph.

### `vo mod update [module]`

Without a module, select the highest valid complete graph. With a module,
advance that selection while preserving every unrelated valid lock version.

### `vo mod sync [path]`

Repair the dependency graph from `vo.mod` while retaining prior versions that
remain valid. It refreshes `vo.lock` for external dependencies and omits the
file for an empty external graph.

### `vo mod fetch [path]`

Authenticate and fetch dependencies already pinned by `vo.lock` into the
module cache. This command does not solve or rewrite the graph. An empty graph
is a successful no-op.

### `vo mod verify [path]`

Read and verify the module graph, canonical lock state, and cached dependency
integrity without changing project or cache state.

### `vo mod remove <module>`

Remove a direct dependency, solve the remaining graph, and refresh
`vo.mod`/`vo.lock`. Remaining valid selections stay fixed.

### `vo mod tidy [path]`

Align direct dependencies with external imports, then solve and write the
graph. Dependencies that remain valid keep their selected versions.

### `vo mod why <module> [--declared]`

Explain the shortest dependency path that selected a module in the effective
build graph. `--declared` inspects the registry graph with workspaces disabled.
This command is read-only.

### `vo mod graph [path] [--declared]`

Print the effective build graph, including workspace-selected sources.
`--declared` inspects the registry graph with workspaces disabled.

### `vo mod snapshot [path] [--declared]`

Export the effective graph as canonical schema-v2 JSON for Studio and other
first-party tooling. `--declared` exports the registry view with workspaces
disabled. This command is read-only.

### `vo cache clean`

Explicitly remove every installed version from the active protocol cache. The
command prints the selected root; `VO_MOD_CACHE` selects an exact, non-empty
absolute alternative to the versioned per-user default.

## Release Commands

### `vo release verify [path]`

Verify that the module is ready to stage from committed source. The command
requires a clean `HEAD` and validates the committed `vo.mod`, the complete v3
`vo.lock` when registry dependencies exist, the dependency graph, publish
closure, and configured local build inputs. It creates no release output.

### `vo release stage [path] --version <version> --out-dir <dir> [...]`

Stage the immutable release asset set in a fresh output directory. The version
uses bare semantic-version spelling such as `1.2.3`; the corresponding Git tag
keeps the `v` prefix. Staging emits `vo.release.json` v2,
`vo.package.json` v1, the authenticated source archive, and every declared
extension artifact.

Use `--commit <sha>` to bind an explicit release commit and repeat
`--artifact KIND TARGET NAME PATH` for externally built artifact payloads.
Staging performs the final artifact identity and byte validation.

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
