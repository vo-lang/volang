//! Vo bytecode runner for testing no_std VM compatibility.
//!
//! This binary uses std for file I/O, but depends on vo-vm and vo-runtime
//! compiled with no_std (default-features = false) to verify no_std compatibility.
//!
//! Usage: vo-vm-runner <bytecode_file> [args...]

use std::env;
use std::fs;
use std::process;

use vo_vm::vm::Vm;
use vo_common_core::bytecode::Module;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: vo-vm-runner <bytecode_file> [args...]");
        process::exit(1);
    }
    
    let bytecode_path = &args[1];
    
    // Read bytecode file
    let bytecode = match fs::read(bytecode_path) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading bytecode file: {}", e);
            process::exit(1);
        }
    };
    
    // Deserialize module
    let module = match Module::deserialize(&bytecode) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Error deserializing bytecode: {:?}", e);
            process::exit(1);
        }
    };
    
    // Create VM and run
    let mut vm = Vm::new();
    
    // Pass remaining args as program args
    let program_args: Vec<String> = args.into_iter().skip(2).collect();
    vm.set_program_args(program_args);
    
    vm.load(module);
    
    if let Err(e) = vm.run() {
        eprintln!("Runtime error: {:?}", e);
        process::exit(1);
    }
    
    println!("[VO:OK]");
}
