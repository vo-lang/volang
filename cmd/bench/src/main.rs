//! Vo benchmark runner - directly compiles and runs .vo files.
//!
//! This avoids the nested VM call overhead of going through cmd/vo.
//!
//! Usage: vo-bench [--jit] <file.vo> [args...]

use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: vo-bench [--jit] <file.vo> [args...]");
        process::exit(1);
    }
    
    let mut use_jit = false;
    let mut file_idx = 1;
    
    // Parse --jit flag
    if args.len() > 1 && args[1] == "--jit" {
        use_jit = true;
        file_idx = 2;
    }
    
    if file_idx >= args.len() {
        eprintln!("Usage: vo-bench [--jit] <file.vo> [args...]");
        process::exit(1);
    }
    
    let vo_path = &args[file_idx];
    
    // Compile the .vo file
    let output = match vo_engine::compile_with_cache(vo_path) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("Compile error: {}", e);
            process::exit(1);
        }
    };
    
    // Choose run mode
    #[cfg(feature = "jit")]
    let mode = if use_jit {
        vo_engine::RunMode::Jit
    } else {
        vo_engine::RunMode::Vm
    };
    
    #[cfg(not(feature = "jit"))]
    let mode = {
        if use_jit {
            eprintln!("Warning: JIT not enabled, falling back to VM");
        }
        vo_engine::RunMode::Vm
    };
    
    // Pass remaining args as program args
    let program_args: Vec<String> = args.into_iter().skip(file_idx + 1).collect();
    
    // Run
    if let Err(e) = vo_engine::run(output, mode, program_args) {
        eprintln!("{}", e);
        process::exit(1);
    }
}
