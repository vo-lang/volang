//! GoX Compiler CLI
//!
//! Command-line interface for the GoX compiler.

use clap::{Parser, Subcommand};
use std::fs;
use std::path::PathBuf;
use std::process;

#[derive(Parser)]
#[command(name = "gox")]
#[command(author = "GoX Team")]
#[command(version = "0.1.0")]
#[command(about = "GoX Language Compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Parse a GoX source file and display the AST
    Parse {
        /// Input source file (.gox)
        #[arg(value_name = "FILE")]
        file: PathBuf,

        /// Display token stream instead of AST
        #[arg(short, long)]
        tokens: bool,

        /// Pretty print the output
        #[arg(short, long)]
        pretty: bool,
    },

    /// Check a GoX source file for errors
    Check {
        /// Input source file (.gox)
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },

    /// Display version information
    Version,
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Parse {
            file,
            tokens,
            pretty,
        } => {
            cmd_parse(&file, tokens, pretty);
        }
        Commands::Check { file } => {
            cmd_check(&file);
        }
        Commands::Version => {
            println!("gox {}", env!("CARGO_PKG_VERSION"));
        }
    }
}

fn cmd_parse(file: &PathBuf, tokens: bool, pretty: bool) {
    // Read source file
    let source = match fs::read_to_string(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: could not read '{}': {}", file.display(), e);
            process::exit(1);
        }
    };

    if tokens {
        // Display token stream
        println!("=== Tokens for {} ===\n", file.display());
        let lexer = gox_syntax::lexer::Lexer::new(&source);
        for (i, token) in lexer.enumerate() {
            if pretty {
                println!("{:4}: {:?} @ {:?}", i, token.kind, token.span);
            } else {
                println!("{:?}", token.kind);
            }
        }
    } else {
        // Parse and display AST
        match gox_syntax::parser::parse(&source) {
            Ok(ast) => {
                println!("=== AST for {} ===\n", file.display());

                // Package
                if let Some(pkg) = &ast.package {
                    println!("package: {}", pkg.name.name);
                }

                // Imports
                if !ast.imports.is_empty() {
                    println!("\nimports:");
                    for imp in &ast.imports {
                        println!("  \"{}\"", imp.path);
                    }
                }

                // Declarations
                println!("\ndeclarations: {}", ast.decls.len());
                for decl in &ast.decls {
                    print_decl(decl, pretty);
                }

                println!("\n✓ Parsed successfully");
            }
            Err(e) => {
                eprintln!("error: {}", e.message);
                if let Some(span) = e.span {
                    // Find line and column
                    let (line, col) = find_line_col(&source, span.start);
                    eprintln!("  --> {}:{}:{}", file.display(), line, col);

                    // Show the problematic line
                    let lines: Vec<&str> = source.lines().collect();
                    if line > 0 && line <= lines.len() {
                        let code_line = lines[line - 1];
                        eprintln!("   |");
                        eprintln!("{:3}| {}", line, code_line);
                        eprintln!("   | {}^", " ".repeat(col - 1));
                    }
                }
                process::exit(1);
            }
        }
    }
}

fn cmd_check(file: &PathBuf) {
    // Read source file
    let source = match fs::read_to_string(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: could not read '{}': {}", file.display(), e);
            process::exit(1);
        }
    };

    // Parse
    match gox_syntax::parser::parse(&source) {
        Ok(ast) => {
            println!(
                "✓ {} parsed successfully ({} declarations)",
                file.display(),
                ast.decls.len()
            );
            // TODO: Add semantic analysis here
        }
        Err(e) => {
            eprintln!("error: {}", e.message);
            if let Some(span) = e.span {
                let (line, col) = find_line_col(&source, span.start);
                eprintln!("  --> {}:{}:{}", file.display(), line, col);
            }
            process::exit(1);
        }
    }
}

fn print_decl(decl: &gox_syntax::ast::TopDecl, _pretty: bool) {
    use gox_syntax::ast::TopDecl;

    match decl {
        TopDecl::Var(v) => {
            for spec in &v.specs {
                let ty = spec
                    .ty
                    .as_ref()
                    .map(|t| format!(" {}", type_name(t)))
                    .unwrap_or_default();
                println!("  var {}{}", spec.name.name, ty);
            }
        }
        TopDecl::Const(c) => {
            for spec in &c.specs {
                println!("  const {}", spec.name.name);
            }
        }
        TopDecl::Type(t) => {
            println!("  type {} = {}", t.name.name, type_name(&t.ty));
        }
        TopDecl::Interface(i) => {
            println!("  interface {} ({} methods)", i.name.name, i.elements.len());
        }
        TopDecl::Implements(i) => {
            let ifaces: Vec<_> = i.interfaces.iter().map(|id| id.name.as_str()).collect();
            println!("  implements {} : {}", i.type_name.name, ifaces.join(", "));
        }
        TopDecl::Func(f) => {
            let receiver = f
                .receiver
                .as_ref()
                .map(|r| format!("({} {}) ", r.name.name, r.ty.name))
                .unwrap_or_default();
            let params: Vec<_> = f
                .params
                .iter()
                .map(|p| format!("{} {}", p.name.name, type_name(&p.ty)))
                .collect();
            let result = f
                .result
                .as_ref()
                .map(|r| format!(" {}", result_name(r)))
                .unwrap_or_default();
            println!(
                "  func {}{}({}){}",
                receiver,
                f.name.name,
                params.join(", "),
                result
            );
        }
    }
}

fn type_name(ty: &gox_syntax::ast::Type) -> String {
    use gox_syntax::ast::Type;

    match ty {
        Type::Named(id) => id.name.clone(),
        Type::Array(a) => format!("[{}]{}", a.len, type_name(&a.elem)),
        Type::Slice(s) => format!("[]{}", type_name(&s.elem)),
        Type::Map(m) => format!("map[{}]{}", type_name(&m.key), type_name(&m.value)),
        Type::Func(f) => {
            let params: Vec<_> = f.params.iter().map(type_name).collect();
            let result = f
                .result
                .as_ref()
                .map(|r| format!(" {}", result_name(r)))
                .unwrap_or_default();
            format!("func({}){}", params.join(", "), result)
        }
        Type::Struct(s) => format!("struct{{{} fields}}", s.fields.len()),
    }
}

fn result_name(r: &gox_syntax::ast::ResultType) -> String {
    use gox_syntax::ast::ResultType;
    match r {
        ResultType::Single(ty) => type_name(ty),
        ResultType::Tuple(types, _) => {
            let names: Vec<_> = types.iter().map(type_name).collect();
            format!("({})", names.join(", "))
        }
    }
}

fn find_line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;

    for (i, ch) in source.chars().enumerate() {
        if i == offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    (line, col)
}
