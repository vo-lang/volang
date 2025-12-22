//! `gox build` command - Build a GoX project.

use std::path::Path;
use gox_common::vfs::{FileSet, RealFs};
use gox_analysis::analyze_project;
use gox_module::{ModFile, VfsConfig};
use gox_codegen_vm::compile_project;
use gox_runtime_vm::extern_fn::StdMode;
use gox_vm::VmResult;

/// Build and run a GoX project.
///
/// # Arguments
/// * `path` - Path to project directory (default: current directory)
/// * `std_mode` - Stdlib mode: "core" or "full"
///
/// # Examples
/// ```text
/// gox build
/// gox build ./myproject
/// gox build --std=core
/// ```
pub fn run(path: &str, std_mode: StdMode) -> Result<(), Box<dyn std::error::Error>> {
    let project_dir = Path::new(path).canonicalize()?;
    let mod_file_path = project_dir.join("gox.mod");

    // Check if gox.mod exists (optional for simple projects)
    if mod_file_path.exists() {
        let mod_file = ModFile::parse_file(&mod_file_path)?;
        println!("Building module: {}", mod_file.module);
    } else {
        println!("Building project in: {}", project_dir.display());
    }

    // Collect all .gox files
    let fs = RealFs;
    let file_set = FileSet::collect(&fs, &project_dir)?;
    
    if file_set.files.is_empty() {
        return Err("no .gox files found".into());
    }
    
    println!("Found {} source files", file_set.files.len());
    
    // Initialize VFS
    let vfs_config = VfsConfig::from_env(project_dir.clone());
    let vfs = vfs_config.to_vfs();
    
    // Analyze project
    let project = analyze_project(file_set, &vfs).map_err(|e| format!("analysis error: {}", e))?;
    println!("Analyzed {} packages", project.packages.len());
    
    // Compile to bytecode
    let module = compile_project(&project).map_err(|e| format!("codegen error: {}", e))?;
    
    // During development: run directly without writing file
    println!("Running module: {} (std={})", module.name, if std_mode == StdMode::Core { "core" } else { "full" });
    let mut vm = gox_runtime_vm::create_vm_with_mode(std_mode);
    vm.load_module(module);
    match vm.run() {
        VmResult::Done => println!("\n✓ Execution completed"),
        VmResult::Panic(msg) => return Err(format!("panic: {}", msg).into()),
        VmResult::Ok => {}
        VmResult::Yield => return Err("unexpected yield".into()),
    }
    Ok(())
}
