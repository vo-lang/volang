//! Integration tests for the GoX parser using test data files.
//!
//! These tests read .gox files from the test_data directory and verify
//! they parse correctly.

use gox_syntax::parser;
use std::fs;
use std::path::PathBuf;

fn test_data_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test_data")
}

fn parse_file(name: &str) -> Result<gox_syntax::ast::File, String> {
    let path = test_data_dir().join(name);
    let source =
        fs::read_to_string(&path).map_err(|e| format!("Failed to read {}: {}", name, e))?;
    parser::parse(&source).map_err(|e| format!("Failed to parse {}: {:?}", name, e))
}

// ═══════════════════════════════════════════════════════════════════════════
// Individual File Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_parse_hello() {
    let file = parse_file("hello.gox").expect("should parse hello.gox");
    assert!(file.package.is_some());
    assert_eq!(file.package.as_ref().unwrap().name.name, "main");
    assert_eq!(file.decls.len(), 1); // main function
}

#[test]
fn test_parse_fibonacci() {
    let file = parse_file("fibonacci.gox").expect("should parse fibonacci.gox");
    assert_eq!(file.decls.len(), 2); // fib and main functions

    // Check fib function has recursion
    if let gox_syntax::ast::TopDecl::Func(f) = &file.decls[0] {
        assert_eq!(f.name.name, "fib");
        assert_eq!(f.params.len(), 1);
        assert!(f.result.is_some());
    }
}

#[test]
fn test_parse_structs() {
    let file = parse_file("structs.gox").expect("should parse structs.gox");

    // Count type declarations
    let type_count = file
        .decls
        .iter()
        .filter(|d| matches!(d, gox_syntax::ast::TopDecl::Type(_)))
        .count();
    assert_eq!(type_count, 3); // Person, Address, Employee

    // Count function declarations
    let func_count = file
        .decls
        .iter()
        .filter(|d| matches!(d, gox_syntax::ast::TopDecl::Func(_)))
        .count();
    assert!(func_count >= 3); // Greet, IsAdult, FullInfo, main
}

#[test]
fn test_parse_interfaces() {
    let file = parse_file("interfaces.gox").expect("should parse interfaces.gox");

    // Count interface declarations
    let interface_count = file
        .decls
        .iter()
        .filter(|d| matches!(d, gox_syntax::ast::TopDecl::Interface(_)))
        .count();
    assert!(interface_count >= 3); // Reader, Writer, ReadWriter, Closer

    // Check for implements declaration
    let implements_count = file
        .decls
        .iter()
        .filter(|d| matches!(d, gox_syntax::ast::TopDecl::Implements(_)))
        .count();
    assert_eq!(implements_count, 1); // File implements Reader, Writer, Closer
}

#[test]
fn test_parse_control_flow() {
    let file = parse_file("control_flow.gox").expect("should parse control_flow.gox");

    // Count functions
    let func_count = file
        .decls
        .iter()
        .filter(|d| matches!(d, gox_syntax::ast::TopDecl::Func(_)))
        .count();
    assert!(func_count >= 5); // classifyNumber, fizzbuzz, sumArray, findMax, bubbleSort, main
}

#[test]
fn test_parse_types() {
    let file = parse_file("types.gox").expect("should parse types.gox");

    // Should have constants
    let const_count = file
        .decls
        .iter()
        .filter(|d| matches!(d, gox_syntax::ast::TopDecl::Const(_)))
        .count();
    assert!(const_count >= 1);

    // Should have type aliases
    let type_count = file
        .decls
        .iter()
        .filter(|d| matches!(d, gox_syntax::ast::TopDecl::Type(_)))
        .count();
    assert!(type_count >= 4); // IntArray, StringSlice, IntMap, Handler, Matrix, Table
}

// ═══════════════════════════════════════════════════════════════════════════
// Bulk Test: Parse All Files
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_parse_all_test_files() {
    let test_dir = test_data_dir();
    let entries = fs::read_dir(&test_dir).expect("Failed to read test_data directory");

    let mut passed = 0;
    let mut failed = Vec::new();

    for entry in entries {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().map(|e| e == "gox").unwrap_or(false) {
            let filename = path.file_name().unwrap().to_string_lossy().to_string();
            let source = fs::read_to_string(&path).unwrap_or_else(|_| panic!("Failed to read {}", filename));

            match parser::parse(&source) {
                Ok(file) => {
                    println!(
                        "✓ {} - package: {:?}, {} decls",
                        filename,
                        file.package.as_ref().map(|p| &p.name.name),
                        file.decls.len()
                    );
                    passed += 1;
                }
                Err(e) => {
                    println!("✗ {} - {:?}", filename, e);
                    failed.push((filename, e));
                }
            }
        }
    }

    println!("\nResults: {} passed, {} failed", passed, failed.len());

    if !failed.is_empty() {
        for (name, err) in &failed {
            eprintln!("  {} failed: {:?}", name, err);
        }
        panic!("{} files failed to parse", failed.len());
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// AST Structure Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_fibonacci_ast_structure() {
    let file = parse_file("fibonacci.gox").expect("should parse");

    // Find main function
    let main = file
        .decls
        .iter()
        .find_map(|d| {
            if let gox_syntax::ast::TopDecl::Func(f) = d {
                if f.name.name == "main" {
                    Some(f)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .expect("should have main function");

    // main should have a for loop
    let has_for = main
        .body
        .stmts
        .iter()
        .any(|s| matches!(s, gox_syntax::ast::Stmt::For(_)));
    assert!(has_for, "main should contain a for loop");
}

#[test]
fn test_structs_ast_structure() {
    let file = parse_file("structs.gox").expect("should parse");

    // Find Person type
    let person = file
        .decls
        .iter()
        .find_map(|d| {
            if let gox_syntax::ast::TopDecl::Type(t) = d {
                if t.name.name == "Person" {
                    if let gox_syntax::ast::Type::Struct(s) = &t.ty {
                        return Some(s);
                    }
                }
            }
            None
        })
        .expect("should have Person struct");

    assert_eq!(person.fields.len(), 3); // name, age, email

    // Check email field has a tag
    let email_field = person
        .fields
        .iter()
        .find(|f| f.name.name == "email")
        .expect("should have email field");
    assert!(email_field.tag.is_some());
}
