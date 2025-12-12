# gox-syntax

Lexer, parser, and AST definitions for the GoX language.

## Components

- **Lexer** (`lexer.rs`): Tokenizes GoX source code with automatic semicolon insertion
- **Parser** (`parser/`): Recursive descent parser with Pratt expression parsing
- **AST** (`ast.rs`): Complete AST node definitions for GoX programs
- **Tokens** (`token.rs`): Token type definitions

## Usage

```rust
use gox_syntax::parser;

let source = r#"
package main;

func main() {
    println("Hello, World!");
}
"#;

let ast = parser::parse(source)?;
println!("{:?}", ast);
```

## Testing

```bash
# Run all tests (unit + integration)
cargo test -p gox-syntax

# Run integration tests with test data files
cargo test -p gox-syntax --test parser_integration
```

## Test Data

Sample GoX files for testing are in `tests/test_data/`:
- `hello.gox` - Hello world
- `fibonacci.gox` - Recursive function
- `structs.gox` - Struct types and methods
- `interfaces.gox` - Interface declarations
- `control_flow.gox` - Control flow statements
- `types.gox` - All type forms
