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

## UI toolchain

UI project commands require Node.js 24 or newer and a matching packaged UI
toolchain. Installing the language CLI alone does not install the Web compiler,
framework sources or browser tooling. Verify the complete installation first:

```sh
vo ui verify
vo ui create my-app
vo ui dev --project my-app
```

The toolchain is discovered beside the CLI installation. `VO_UI_TOOLCHAIN` may
select a complete toolchain directory containing `ui.mjs` and
`tools/toolchain.json`. Keep the compiler and tools from the same distribution.
Native desktop run/package commands additionally require its matching desktop
SDK; `vo ui doctor --project my-app --target desktop` checks those prerequisites.
See the [first application guide](../../../ui/next/guides/first-steps.md) for
checking, browser testing and building an application.

## Studio

[Volang Studio](https://volang.dev/studio/gallery) demonstrates UI components,
runs editable console and UI examples, and serves the language and framework
reference. Drafts stay in the browser. Browser execution uses the Wasm VM.

To develop Studio from a source checkout, follow the runtime prerequisites in
`ui/next/README.md`, then run `node eng/ui-next/cli.mjs dev` from the repository
root. Studio is a showcase and Playground; project creation and packaging use
the CLI toolchain described above.

## Module cache

External dependency bytes are installed into the versioned Volang module
cache only after `vo mod sync` has selected them in `vo.lock`. Use
`vo mod fetch` to materialize a selected graph and `vo mod verify` to validate
it without changing project state. `VO_MOD_CACHE` may select an exact absolute
cache path for controlled environments.
