# gox-cli

Command-line interface for the GoX compiler.

## Installation

```bash
cargo install --path .
```

Or run directly:

```bash
cargo run -p gox-cli -- <command>
```

## Commands

### Parse

Parse a GoX file and display the AST:

```bash
gox parse file.gox
gox parse --pretty file.gox    # Pretty print
gox parse --tokens file.gox    # Show token stream
```

### Check

Validate GoX syntax:

```bash
gox check file.gox
```

### Version

```bash
gox version
```

## Example

```bash
$ gox parse crates/gox-syntax/tests/test_data/hello.gox
=== AST for crates/gox-syntax/tests/test_data/hello.gox ===

package: main

declarations: 1
  func main()

✓ Parsed successfully
```
