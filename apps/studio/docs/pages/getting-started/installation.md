# Installation

## macOS (Homebrew)

```bash
brew tap vo-lang/vo
brew install vo
```

## From Source

Requires Rust toolchain (1.75+):

```bash
git clone https://github.com/vo-lang/volang.git
cd volang
cargo install --path cmd/vo
```

Verify the installation:

```bash
vo version
```

## Editor Support

Vo Studio is the official IDE — it runs as both a desktop app (via Tauri) and a web application at [volang.dev](https://volang.dev).
