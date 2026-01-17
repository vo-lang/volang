//! `vo build` command - Build a Vo project.

use std::path::Path;
use vo_common::vfs::{FileSet, RealFs};
use vo_analysis::analyze_project;
use vo_module::ModFile;
use vo_codegen::compile_project;
use vo_vm::vm::Vm;
use super::run::StdMode;
use crate::stdlib::create_resolver;
use crate::output::{TAG_OK, format_tag, ErrorKind, report_analysis_error};

/// Build and run a Vo project.
///
/// Returns true if successful, false if failed (error already reported).
pub fn run(path: &str, std_mode: StdMode) -> bool {
    let project_dir = match Path::new(path).canonicalize() {
        Ok(p) => p,
        Err(e) => {
            println!("{}", format_tag(ErrorKind::Io, None, &e.to_string()));
            return false;
        }
    };
    let mod_file_path = project_dir.join("vo.mod");

    // Check if vo.mod exists (optional for simple projects)
    if mod_file_path.exists() {
        match ModFile::parse_file(&mod_file_path) {
            Ok(mod_file) => println!("Building module: {}", mod_file.module),
            Err(e) => {
                println!("{}", format_tag(ErrorKind::Io, None, &e.to_string()));
                return false;
            }
        }
    } else {
        println!("Building project in: {}", project_dir.display());
    }

    // Collect all .vo files
    let fs = RealFs::new(&project_dir);
    let file_set = match FileSet::collect(&fs, Path::new("."), project_dir.clone()) {
        Ok(fs) => fs,
        Err(e) => {
            println!("{}", format_tag(ErrorKind::Io, None, &e.to_string()));
            return false;
        }
    };
    
    if file_set.files.is_empty() {
        println!("{}", format_tag(ErrorKind::Io, None, "no .vo files found"));
        return false;
    }
    
    println!("Found {} source files", file_set.files.len());
    
    // Initialize package resolver with embedded stdlib
    let resolver = create_resolver(&project_dir);
    
    // Analyze project
    let project = match analyze_project(file_set, &resolver) {
        Ok(p) => p,
        Err(e) => {
            report_analysis_error(&e);
            return false;
        }
    };
    println!("Analyzed {} packages", project.packages.len());
    
    // Compile to bytecode
    let module = match compile_project(&project) {
        Ok(m) => m,
        Err(e) => {
            println!("{}", format_tag(ErrorKind::Codegen, None, &format!("{:?}", e)));
            return false;
        }
    };
    
    // During development: run directly without writing file
    println!("Running module: {} (std={})", module.name, if std_mode == StdMode::Core { "core" } else { "full" });
    let mut vm = Vm::new();
    vm.load(module.clone());
    match vm.run() {
        Ok(()) => {
            println!("{}", TAG_OK);
            true
        }
        Err(e) => {
            println!("{}", format_tag(ErrorKind::Panic, None, &format!("{:?}", e)));
            false
        }
    }
}

