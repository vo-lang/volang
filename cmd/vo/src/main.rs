//! Vo CLI - Pure Rust implementation
//!
//! Commands:
//!   run <file>      Run a Vo program
//!   build [path]    Build and run a project
//!   check [path]    Type-check a project
//!   dump <file>     Dump bytecode to text
//!   compile <file>  Compile bytecode text to binary
//!   emit <file>     Compile source to bytecode binary
//!   init <path>     Initialize a new module
//!   get <module>    Download a dependency (not implemented)
//!   help            Show help
//!   version         Show version

use std::env;
use std::fs;
use std::process;

use vo_engine::{compile, format_text, run, Module, RunMode};

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.is_empty() {
        print_usage();
        process::exit(1);
    }

    let cmd = &args[0];
    let rest = &args[1..];

    let code = match cmd.as_str() {
        "run" => cmd_run(rest),
        "build" => cmd_build(rest),
        "check" => cmd_check(rest),
        "dump" => cmd_dump(rest),
        "compile" => cmd_compile(rest),
        "emit" => cmd_emit(rest),
        "init" => cmd_init(rest),
        "get" => cmd_get(rest),
        "-h" | "--help" | "help" => {
            print_usage();
            0
        }
        "-v" | "--version" | "version" => {
            println!("vo version 0.1.0");
            0
        }
        _ => {
            eprintln!("unknown command: {}", cmd);
            print_usage();
            1
        }
    };

    process::exit(code);
}

fn print_usage() {
    println!("Usage: vo <command> [arguments]");
    println!();
    println!("Commands:");
    println!("  run <file>      Run a Vo program");
    println!("  build [path]    Build a project");
    println!("  dump <file>     Dump bytecode to text");
    println!("  compile <file>  Compile bytecode text to binary");
    println!("  emit <file>     Compile source to bytecode binary");
    println!("  init <path>     Initialize a new module");
    println!("  get <module>    Download a dependency");
    println!("  check           Type-check current module");
    println!("  help            Show this help");
    println!("  version         Show version");
    println!();
    println!("Run 'vo <command> --help' for more information.");
}

fn cmd_run(args: &[String]) -> i32 {
    if args.is_empty() {
        eprintln!("usage: vo run <file> [--mode=jit] [--codegen]");
        return 1;
    }

    let file = &args[0];
    let mut mode = RunMode::Vm;
    let mut print_codegen = false;

    for arg in &args[1..] {
        if arg.starts_with("--mode=") {
            let m = &arg[7..];
            if m == "jit" {
                mode = RunMode::Jit;
            }
        } else if arg == "--codegen" {
            print_codegen = true;
        }
    }

    let output = match compile(file) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("[VO:COMPILE] {}", e);
            return 1;
        }
    };

    if print_codegen {
        println!("{}", format_text(&output.module));
        return 0;
    }

    if let Err(e) = run(output, mode, Vec::new()) {
        eprintln!("[VO:PANIC] {}", e);
        return 1;
    }

    println!("[VO:OK]");
    0
}

fn cmd_build(args: &[String]) -> i32 {
    let path = if args.is_empty() { "." } else { &args[0] };

    println!("Building project: {}", path);

    let output = match compile(path) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("[VO:COMPILE] {}", e);
            return 1;
        }
    };

    println!("Running module: {}", output.module.name);

    if let Err(e) = run(output, RunMode::Vm, Vec::new()) {
        eprintln!("[VO:PANIC] {}", e);
        return 1;
    }

    println!("[VO:OK]");
    0
}

fn cmd_check(args: &[String]) -> i32 {
    let path = if args.is_empty() { "." } else { &args[0] };

    println!("Checking project: {}", path);

    match compile(path) {
        Ok(_) => {
            println!("Check passed");
            0
        }
        Err(e) => {
            eprintln!("[VO:CHECK] {}", e);
            1
        }
    }
}

fn cmd_dump(args: &[String]) -> i32 {
    if args.is_empty() {
        eprintln!("usage: vo dump <file.vob>");
        return 1;
    }

    let file = &args[0];

    let bytes = match fs::read(file) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("[VO:IO] {}", e);
            return 1;
        }
    };

    let module = match Module::deserialize(&bytes) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("[VO:IO] {:?}", e);
            return 1;
        }
    };

    print!("{}", format_text(&module));
    0
}

fn cmd_compile(args: &[String]) -> i32 {
    if args.is_empty() {
        eprintln!("usage: vo compile <file.vot> [-o output.vob]");
        return 1;
    }

    let _file = &args[0];

    eprintln!("Bytecode text parsing not yet implemented");
    1
}

fn cmd_emit(args: &[String]) -> i32 {
    if args.is_empty() {
        eprintln!("usage: vo emit <file.vo|dir> [-o output.vob]");
        return 1;
    }

    let path = &args[0];
    let mut output_path = String::new();

    let mut i = 1;
    while i < args.len() {
        if args[i] == "-o" && i + 1 < args.len() {
            output_path = args[i + 1].clone();
            i += 2;
        } else {
            i += 1;
        }
    }

    if output_path.is_empty() {
        if path.ends_with(".vo") {
            output_path = format!("{}.vob", &path[..path.len() - 3]);
        } else {
            output_path = format!("{}.vob", path);
        }
    }

    let output = match compile(path) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("[VO:COMPILE] {}", e);
            return 1;
        }
    };

    let bytes = output.module.serialize();
    if let Err(e) = fs::write(&output_path, bytes) {
        eprintln!("[VO:IO] {}", e);
        return 1;
    }

    println!("Emitted {} -> {}", path, output_path);
    0
}

fn cmd_init(args: &[String]) -> i32 {
    if args.is_empty() {
        eprintln!("usage: vo init <module-path>");
        eprintln!("  e.g. vo init github.com/user/myapp");
        return 1;
    }

    let module_path = &args[0];
    let content = format!("module {}\n\nvo 0.1\n", module_path);

    if let Err(e) = fs::write("vo.mod", content) {
        eprintln!("[VO:IO] {}", e);
        return 1;
    }

    println!("Initialized module: {}", module_path);
    0
}

fn cmd_get(args: &[String]) -> i32 {
    if args.is_empty() {
        eprintln!("usage: vo get <module@version>");
        eprintln!("  e.g. vo get github.com/vo-lang/resvg@v0.1.0");
        return 1;
    }

    let spec = &args[0];
    let (module, version) = match parse_module_version(spec) {
        Ok(v) => v,
        Err(e) => { eprintln!("[VO:GET] {}", e); return 1; }
    };

    match vo_module::fetch::install_module(&module, &version) {
        Ok(target_dir) => {
            println!("get {} {}", module, version);
            println!("  -> {}", target_dir.display());
            0
        }
        Err(e) => {
            eprintln!("[VO:GET] {}", e);
            1
        }
    }
}

fn parse_module_version(spec: &str) -> Result<(String, String), String> {
    match spec.rsplit_once('@') {
        Some((m, v)) if !m.is_empty() && !v.is_empty() => Ok((m.to_string(), v.to_string())),
        _ => Err(format!(
            "invalid module spec {:?}: expected <module>@<version>, e.g. github.com/foo/bar@v1.0.0",
            spec
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_module_version_ok() {
        let (m, v) = parse_module_version("github.com/vo-lang/resvg@v0.1.0").unwrap();
        assert_eq!(m, "github.com/vo-lang/resvg");
        assert_eq!(v, "v0.1.0");
    }

    #[test]
    fn test_parse_module_version_no_at() {
        assert!(parse_module_version("github.com/vo-lang/resvg").is_err());
    }

    #[test]
    fn test_parse_module_version_empty_version() {
        assert!(parse_module_version("github.com/vo-lang/resvg@").is_err());
    }
}
