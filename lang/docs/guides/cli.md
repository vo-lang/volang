# Command-line reference

`vo help` prints the current command catalog. Every command accepts `--help`
at its command boundary. This guide explains the stable workflows and leaves
the executable help as the final authority for flags.

## Develop and run

```sh
vo run <file|dir> [--mode=vm|jit] [-- args...]
vo check [path] [--read-only]
vo test [path] [--mode=vm|jit]
vo fmt [file|dir...] [--check]
```

`run` compiles and executes a source file or project. VM is optimized for
startup; JIT is intended for longer native sessions. Arguments after `--` are
passed to the Volang program.

`check` performs frontend and module validation without execution. `test` runs
the selected test entry or the project test convention. `fmt` rewrites source;
`--check` reports drift without writing.

`check --read-only` validates existing source and locked dependencies without
running generators, downloading modules or writing compilation caches. Prepare
missing dependencies and generated source explicitly before this check.

## Create projects

```sh
vo init <module-path>
vo ui new <path> [--module=local/name] \
  [--template=default|dashboard|media|studio]
```

`init` creates module intent in an existing directory. `ui new` creates a
complete official UI project, generates a lock, and materializes its bundled UI
dependency.

## Build artifacts

```sh
vo build [path] [-o output] [--target=TRIPLE] \
  [--kind=bin|object|bytecode] [--runtime=PATH] \
  [--link-arg=ARG]... [--windows-gui] [--no-cache]
```

The default kind is a Native AOT executable. `object` emits a relocatable
object, and `bytecode` emits the verified `.vob`
format. `emit bytecode` is the explicit low-level bytecode command; `dump`
disassembles a `.vob` file.

Custom native runtimes can supply platform library flags through repeated
`--link-arg=ARG` options. Each value reaches the linker as one literal argument,
in order; no shell expansion occurs. On Windows, `--windows-gui` selects a GUI
executable without a console window while preserving the normal `main` entry.
Both options apply only to native executable linking.

## Manage dependencies

```sh
vo mod add <module[@constraint]>
vo mod update [module]
vo mod remove <module>
vo mod tidy [path]
vo mod sync [path]
vo mod fetch [path]
vo mod verify [path]
vo mod graph [path] [--declared] [--json]
vo mod why <module> [--declared]
vo work sync [path]
vo work materialize [path]
```

Selection commands atomically update intent and lock state. `fetch` and
`verify` operate on the selected graph. `graph` and `why` are read-only.
`--declared` inspects registry intent with workspace selection disabled.

`vo cache clean` removes installed versions from the active protocol cache and
prints the selected cache root.

## Run generators

```sh
vo generate [path] [--write]
```

Projects declare governed schema generators in `vo.generate.toml`. A normal
build can materialize generated sources in isolated build state. `--write`
updates approved checked-in outputs when the project contract permits it.

## Build UI products

```sh
vo ui dev [path] [--addr=127.0.0.1:4173] [--open]
vo ui run [path] [--mode=vm|jit] [--no-watch]
vo ui test [path] [--mode=vm|jit] [interaction options]
vo ui inspect [path] [--target=portable|web|native] [--runtime]
vo ui doctor [path]
vo ui build [path] [-o dist]
vo ui package [path] [-o dist] [--target=TRIPLE] [--runtime=PATH]
```

`ui dev` serves the Web development projection with state-preserving rebuilds.
`ui run` opens the native development host. `ui test` drives semantic input and
can capture governed snapshots. `inspect` reports the view/runtime contract;
`doctor` diagnoses project and host readiness. `build` creates a bytecode and Wasm VM
Web bundle, and `package` creates a Native AOT desktop package.

`vo ui source` lists or exports official UIKit package source for inspection.

New projects created with `vo ui create` contain `ui-next.json` and select the
replacement UI tools. Their `vo ui check [path]` validates source entries and host
imports without running prerendering or writing a distribution. Use `build`
before `preview`. `vo ui doctor [path] [--target web|desktop] [--json]` reports
configuration and installation checks with repair guidance and a nonzero status
on failure. It does not change the project or install dependencies. Projects
without the new manifest retain the compatibility commands above.

## Release modules

```sh
vo release verify [path]
vo release stage [path] --out-dir <dir>
```

Verification checks committed source, module intent, lock completeness,
dependency closure, and configured build inputs. Staging creates the immutable
release asset set in a fresh output directory.

## Version and diagnostics

```sh
vo version
vo help
```

Automation should preserve command exit status and prefer structured output
where a command offers `--json`. Avoid parsing progress or human diagnostic
text as a stable protocol.
