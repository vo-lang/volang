# Installation

The `vo` executable contains the compiler, project tools, development VM, JIT
entry points, AOT builder, and official UI commands.

## Homebrew

The release contract publishes the macOS and Linux formula through the Volang
tap:

```sh
brew tap vo-lang/vo
brew install vo
```

Upgrade and verify it with:

```sh
brew update
brew upgrade vo
vo version
vo help
```

## Build from source

Clone the repository and use the Rust version pinned by
`rust-toolchain.toml`:

```sh
git clone https://github.com/vo-lang/volang.git
cd volang
cargo install --locked --path cmd/vo
vo version
```

Repository contributors normally keep build products inside the workspace:

```sh
cargo build -p vo --locked
./target/debug/vo version
```

The repository command `./d.py test smoke` exercises the maintained smoke
matrix. Contributor and CI details live in `docs/ci.md`.

## Verify a first program

Save this as `hello.vo`:

```vo
func main() {
    println("Volang is ready")
}
```

Then check and run it:

```sh
vo check hello.vo
vo run hello.vo
```

The expected output is `Volang is ready`.

## Optional Web prerequisites

Normal language and native UI development has no Node package dependency.
Repository maintainers who rebuild the browser runtime support package use the
tool versions governed by the repository. A released `vo ui build` consumes a
compatible packaged Web runtime directory and emits a self-contained Web
bundle.

## Editor and Studio

Volang Studio is the dogfood IDE built with the official UI framework. From a
source checkout, start it with:

```sh
cargo build -p vo --locked
./target/debug/vo ui dev apps/studio --open
```

Studio includes persistent projects, examples, analysis, VM/JIT execution,
preview, documentation, and release-oriented Web and desktop hosts. The CLI
remains the source of truth for compilation and module operations.

## Module cache

External dependency bytes are installed into the versioned Volang module
cache only after `vo mod sync` has selected them in `vo.lock`. Use
`vo mod fetch` to materialize a selected graph and `vo mod verify` to validate
it without changing project state. `VO_MOD_CACHE` may select an exact absolute
cache path for controlled environments.
