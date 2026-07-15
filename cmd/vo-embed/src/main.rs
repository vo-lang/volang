//! Vo bytecode runner for testing no_std VM compatibility.
//!
//! This binary uses std for file I/O, but depends on vo-vm and vo-runtime
//! compiled with no_std (default-features = false) to verify no_std compatibility.
//!
//! Usage: vo-vm-runner <bytecode_file> [args...]

use std::env;
use std::ffi::OsString;
use std::path::PathBuf;
use std::process;

use vo_common_core::bytecode::Module;
use vo_vm::vm::{SchedulingOutcome, Vm};

fn main() {
    let args: Vec<OsString> = env::args_os().collect();

    if args.len() < 2 {
        eprintln!("Usage: vo-vm-runner <bytecode_file> [args...]");
        process::exit(1);
    }

    let bytecode_path = PathBuf::from(&args[1]);

    // Read bytecode file
    let bytecode = match vo_common_core::serialize::read_vob_file(&bytecode_path) {
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
            eprintln!("Error deserializing bytecode: {e}");
            process::exit(1);
        }
    };

    // Create VM and run
    let mut vm = Vm::new();

    // Pass remaining args as program args
    let program_args = args
        .into_iter()
        .skip(2)
        .map(os_arg_into_bytes)
        .collect::<Result<Vec<_>, _>>()
        .unwrap_or_else(|argument| {
            eprintln!(
                "Program argument cannot be represented as UTF-8 on this platform: {:?}",
                argument
            );
            process::exit(2);
        });
    vm.set_program_args_bytes(program_args);

    if let Err(e) = vm.load(module) {
        eprintln!("Runtime error: {:?}", e);
        process::exit(1);
    }

    match vm.run() {
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            process::exit(1);
        }
        Ok(SchedulingOutcome::Blocked) => {
            eprintln!("Runtime error: {:?}", vm.deadlock_err());
            process::exit(1);
        }
        Ok(SchedulingOutcome::Exited(code)) => process::exit(code),
        Ok(_) => {}
    }

    println!("[VO:OK]");
}

#[cfg(unix)]
fn os_arg_into_bytes(argument: OsString) -> Result<Vec<u8>, OsString> {
    use std::os::unix::ffi::OsStringExt;
    Ok(argument.into_vec())
}

#[cfg(not(unix))]
fn os_arg_into_bytes(argument: OsString) -> Result<Vec<u8>, OsString> {
    argument.into_string().map(String::into_bytes)
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::os::unix::ffi::OsStringExt;

    #[test]
    fn program_argument_bytes_round_trip() {
        assert_eq!(
            os_arg_into_bytes(OsString::from_vec(b"a\xffz".to_vec())).unwrap(),
            b"a\xffz"
        );
    }
}
