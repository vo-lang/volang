//! vo-test launcher - compiles and runs the Vo test runner (cmd/vo-test).

use std::env;
use std::path::PathBuf;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    // Find cmd/vo-test directory
    let vo_test_dir = find_vo_test_dir();
    
    // Prepend program name (os.Args[0])
    let mut full_args = vec!["vo-test".to_string()];
    full_args.extend(args.iter().skip(1).cloned());
    
    // Compile cmd/vo-test (no cache - always use fresh stdlib)
    let output = match vo_engine::compile(vo_test_dir.to_str().unwrap()) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("Failed to compile vo-test: {}", e);
            process::exit(1);
        }
    };
    
    // Run in VM mode
    let mode = vo_engine::RunMode::Vm;
    
    if let Err(e) = vo_engine::run(output, mode, full_args) {
        eprintln!("{}", e);
        process::exit(1);
    }
}

fn find_vo_test_dir() -> PathBuf {
    // Try relative to executable
    if let Ok(exe_path) = env::current_exe() {
        // Development: executable in target/debug or target/release
        // cmd/vo-test is relative to repo root
        let mut path = exe_path.clone();
        for _ in 0..4 {
            path.pop();
            let vo_test = path.join("cmd/vo-test");
            if vo_test.exists() && vo_test.join("main.vo").exists() {
                return vo_test;
            }
        }
    }
    
    // Try current directory
    let cwd_vo_test = PathBuf::from("cmd/vo-test");
    if cwd_vo_test.exists() && cwd_vo_test.join("main.vo").exists() {
        return cwd_vo_test;
    }
    
    eprintln!("Could not find cmd/vo-test directory");
    process::exit(1);
}
