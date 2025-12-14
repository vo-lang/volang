//! GoX compiler CLI.
//!
//! Commands:
//! - `gox init <module-path>` - Initialize a new module
//! - `gox get <module>@<version>` - Download a dependency
//! - `gox build` - Build the current module
//! - `gox check` - Type-check without building
//! - `gox run-bytecode --test <name>` - Run bytecode tests

use std::env;
use std::process;

use clap::{Parser, Subcommand};
use gox_module::{ModFile, ModuleResolver};

mod bytecode_tests;
mod bytecode_text;

#[derive(Parser)]
#[command(name = "gox")]
#[command(about = "GoX compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new GoX module
    Init {
        /// Module path (e.g., github.com/user/project)
        module_path: String,
    },

    /// Download a dependency and add it to gox.mod
    Get {
        /// Module and version (e.g., github.com/foo/bar@v1.2.3)
        module_version: String,
    },

    /// Build a GoX project
    Build {
        /// Path to project directory (default: current directory)
        #[arg(default_value = ".")]
        path: String,
    },

    /// Type-check the current module without building
    Check,
    
    /// Run bytecode tests for VM verification
    RunBytecode {
        /// Test name: arithmetic, factorial, ffi, channel, all
        #[arg(short, long, default_value = "all")]
        test: String,
    },
    
    /// Run a bytecode text file (.goxt)
    Run {
        /// Path to bytecode text file
        file: String,
    },
    
    /// Dump a bytecode file to text format
    Dump {
        /// Path to bytecode binary file (.goxb)
        file: String,
    },
    
    /// Compile bytecode text to binary
    Compile {
        /// Path to bytecode text file (.goxt)
        file: String,
        /// Output path (default: same name with .goxb extension)
        #[arg(short, long)]
        output: Option<String>,
    },
    
    /// Run all .goxt tests in a directory
    Test {
        /// Directory containing .goxt test files
        #[arg(default_value = "examples/tests")]
        dir: String,
    },
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Init { module_path } => cmd_init(&module_path),
        Commands::Get { module_version } => cmd_get(&module_version),
        Commands::Build { path } => cmd_build(&path),
        Commands::Check => cmd_check(),
        Commands::RunBytecode { test } => bytecode_tests::run_test(&test),
        Commands::Run { file } => cmd_run(&file),
        Commands::Dump { file } => cmd_dump(&file),
        Commands::Compile { file, output } => cmd_compile(&file, output),
        Commands::Test { dir } => cmd_test(&dir),
    };

    if let Err(e) = result {
        eprintln!("error: {}", e);
        process::exit(1);
    }
}

/// Initialize a new GoX module.
fn cmd_init(module_path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let cwd = env::current_dir()?;
    let mod_file_path = cwd.join("gox.mod");

    if mod_file_path.exists() {
        return Err(format!("gox.mod already exists in {}", cwd.display()).into());
    }

    // Validate module path
    if module_path.is_empty() {
        return Err("module path cannot be empty".into());
    }
    if module_path.starts_with("std/") || module_path == "std" {
        return Err("module path cannot start with 'std/' (reserved for standard library)".into());
    }

    let mod_file = ModFile::new(module_path.to_string());
    mod_file.write_file(&mod_file_path)?;

    println!("Initialized module {} in {}", module_path, cwd.display());
    Ok(())
}

/// Download a dependency and add it to gox.mod.
fn cmd_get(module_version: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Parse module@version
    let parts: Vec<&str> = module_version.splitn(2, '@').collect();
    if parts.len() != 2 {
        return Err(format!(
            "invalid format: expected <module>@<version>, got: {}",
            module_version
        ).into());
    }

    let module = parts[0];
    let version = parts[1];

    if !version.starts_with('v') {
        return Err(format!("version must start with 'v', got: {}", version).into());
    }

    let cwd = env::current_dir()?;
    let mod_file_path = cwd.join("gox.mod");

    // Load existing gox.mod
    let mut mod_file = ModFile::parse_file(&mod_file_path)?;

    // Create .goxdeps directory if needed
    let deps_dir = cwd.join(".goxdeps");
    std::fs::create_dir_all(&deps_dir)?;

    // TODO: Actually download the module from a registry
    // For now, just create a placeholder directory structure
    let module_dir = deps_dir.join(format!("{}@{}", module, version));
    if !module_dir.exists() {
        std::fs::create_dir_all(&module_dir)?;
        
        // Create a minimal gox.mod for the dependency
        let dep_mod = ModFile::new(module.to_string());
        dep_mod.write_file(module_dir.join("gox.mod"))?;
        
        println!("Created placeholder for {}@{}", module, version);
        println!("  (actual download not implemented yet)");
    }

    // Add to gox.mod
    mod_file.add_require(module.to_string(), version.to_string());
    mod_file.write_file(&mod_file_path)?;

    println!("Added {}@{} to gox.mod", module, version);
    Ok(())
}

/// Build a GoX project.
fn cmd_build(path: &str) -> Result<(), Box<dyn std::error::Error>> {
    use gox_common::vfs::{FileSet, RealFs};
    use gox_analysis::analyze_project;
    use gox_codegen_vm::compile_project;
    
    let project_dir = std::path::Path::new(path).canonicalize()?;
    let mod_file_path = project_dir.join("gox.mod");

    // Check if gox.mod exists (optional for simple projects)
    let module_name = if mod_file_path.exists() {
        let mod_file = ModFile::parse_file(&mod_file_path)?;
        println!("Building module: {}", mod_file.module);
        mod_file.module
    } else {
        println!("Building project in: {}", project_dir.display());
        "main".to_string()
    };

    // Collect all .gox files
    let fs = RealFs;
    let file_set = FileSet::collect(&fs, &project_dir)?;
    
    if file_set.files.is_empty() {
        return Err("no .gox files found".into());
    }
    
    println!("Found {} source files", file_set.files.len());
    
    // Analyze project
    let project = analyze_project(file_set).map_err(|e| format!("analysis error: {}", e))?;
    println!("Analyzed {} packages", project.packages.len());
    
    // Compile to bytecode
    let module = compile_project(&project).map_err(|e| format!("codegen error: {}", e))?;
    
    // Write to file
    let out_path = project_dir.join(format!("{}.goxc", module_name));
    let bytes = module.to_bytes();
    std::fs::write(&out_path, &bytes)?;
    
    println!("✓ Built {} ({} bytes)", out_path.display(), bytes.len());
    Ok(())
}

/// Type-check the current module without building.
fn cmd_check() -> Result<(), Box<dyn std::error::Error>> {
    let cwd = env::current_dir()?;
    let mod_file_path = cwd.join("gox.mod");

    // Load gox.mod
    let mod_file = ModFile::parse_file(&mod_file_path)?;
    println!("Checking module: {}", mod_file.module);

    // Create resolver and compute closure
    let resolver = ModuleResolver::new(&cwd);
    let _closure = resolver.compute_closure(&mod_file)?;

    // TODO: Find all .gox files and type-check them
    println!("\nType checking not implemented yet");
    Ok(())
}

/// Run a bytecode file (.goxc, .goxb, or .goxt).
fn cmd_run(file: &str) -> Result<(), Box<dyn std::error::Error>> {
    use gox_vm::{Module, VmResult};
    
    let module = if file.ends_with(".goxc") || file.ends_with(".goxb") {
        // Binary format
        let bytes = std::fs::read(file)?;
        Module::from_bytes(&bytes)?
    } else {
        // Text format (.goxt)
        let content = std::fs::read_to_string(file)?;
        bytecode_text::parse_text(&content).map_err(|e| e)?
    };
    
    println!("Running module: {}", module.name);
    
    let mut vm = gox_runtime_vm::create_vm();
    vm.load_module(module);
    
    match vm.run() {
        VmResult::Done => {
            println!("\n✓ Execution completed");
            Ok(())
        }
        VmResult::Panic(msg) => Err(format!("panic: {}", msg).into()),
        VmResult::Ok => Ok(()),
        VmResult::Yield => Err("unexpected yield".into()),
    }
}

/// Dump a bytecode file to text format.
fn cmd_dump(file: &str) -> Result<(), Box<dyn std::error::Error>> {
    use gox_vm::Module;
    
    let module = if file.ends_with(".goxb") {
        let bytes = std::fs::read(file)?;
        Module::from_bytes(&bytes)?
    } else {
        let content = std::fs::read_to_string(file)?;
        bytecode_text::parse_text(&content).map_err(|e| e)?
    };
    
    let text = bytecode_text::format_text(&module);
    print!("{}", text);
    Ok(())
}

/// Compile bytecode text to binary.
fn cmd_compile(file: &str, output: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let content = std::fs::read_to_string(file)?;
    let module = bytecode_text::parse_text(&content).map_err(|e| e)?;
    
    let out_path = output.unwrap_or_else(|| {
        let p = std::path::Path::new(file);
        p.with_extension("goxb").to_string_lossy().to_string()
    });
    
    let bytes = module.to_bytes();
    std::fs::write(&out_path, &bytes)?;
    
    println!("Compiled {} -> {} ({} bytes)", file, out_path, bytes.len());
    Ok(())
}

/// Run all .goxt tests in a directory.
fn cmd_test(dir: &str) -> Result<(), Box<dyn std::error::Error>> {
    use gox_vm::VmResult;
    use std::fs;
    
    let entries: Vec<_> = fs::read_dir(dir)?
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "goxt"))
        .collect();
    
    if entries.is_empty() {
        println!("No .goxt files found in {}", dir);
        return Ok(());
    }
    
    println!("=== Running {} .goxt tests in {} ===\n", entries.len(), dir);
    
    let mut passed = 0;
    let mut failed = 0;
    
    for entry in entries {
        let path = entry.path();
        let name = path.file_stem().unwrap().to_string_lossy();
        
        print!("  {} ... ", name);
        
        let content = fs::read_to_string(&path)?;
        let module = match bytecode_text::parse_text(&content) {
            Ok(m) => m,
            Err(e) => {
                println!("PARSE ERROR: {}", e);
                failed += 1;
                continue;
            }
        };
        
        let mut vm = gox_runtime_vm::create_vm();
        vm.load_module(module);
        
        match vm.run() {
            VmResult::Done | VmResult::Ok => {
                println!("✓");
                passed += 1;
            }
            VmResult::Panic(msg) => {
                println!("PANIC: {}", msg);
                failed += 1;
            }
            VmResult::Yield => {
                println!("YIELD (unexpected)");
                failed += 1;
            }
        }
    }
    
    println!("\n=== Results: {} passed, {} failed ===", passed, failed);
    
    if failed > 0 {
        Err(format!("{} tests failed", failed).into())
    } else {
        Ok(())
    }
}
