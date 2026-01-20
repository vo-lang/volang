//! Vo Launcher - Run any Vo program.
//!
//! Usage: vo [launcher-options] <path> [program-args...]
//!
//! Launcher options must come before the entry path.
//! Everything after the entry path is passed to the program via VO_ARGS.

use std::path::{Path, PathBuf};
use std::time::SystemTime;
use vo_launcher::{compile_source, run_module_with_extensions, CompileOutput, Module, RunMode};
use vo_runtime::ext_loader::ExtensionManifest;

const VERSION: &str = "0.1.0";

struct LauncherArgs {
    mode: RunMode,
    cache: bool,
    compile_only: Option<String>,  // Output bytecode to file, don't run
    entry: String,
    program_args: Vec<String>,
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    
    if args.is_empty() {
        print_help();
        std::process::exit(1);
    }
    
    match args[0].as_str() {
        "-v" | "--version" | "version" => {
            println!("vo {}", VERSION);
            return;
        }
        "-h" | "--help" | "help" => {
            print_help();
            return;
        }
        _ => {}
    }
    
    let parsed = parse_args(&args);
    
    let output = if parsed.cache {
        compile_with_cache(&parsed.entry)
    } else {
        compile_source(&parsed.entry)
    };
    
    let output = match output {
        Ok(o) => o,
        Err(e) => {
            eprintln!("vo: {}", e);
            std::process::exit(1);
        }
    };
    
    // If --compile-only, write bytecode and exit
    if let Some(out_path) = parsed.compile_only {
        let bytes = output.module.serialize();
        if let Err(e) = std::fs::write(&out_path, bytes) {
            eprintln!("vo: failed to write bytecode: {}", e);
            std::process::exit(1);
        }
        return;
    }
    
    if let Err(e) = run_module_with_extensions(output, parsed.mode, parsed.program_args) {
        eprintln!("vo: {}", e);
        std::process::exit(1);
    }
}

fn parse_args(args: &[String]) -> LauncherArgs {
    let mut mode = RunMode::Vm;
    let mut cache = false;
    let mut compile_only = None;
    let mut i = 0;
    
    while i < args.len() {
        let arg = &args[i];
        if let Some(m) = arg.strip_prefix("--mode=") {
            mode = match m {
                "jit" => RunMode::Jit,
                "vm" => RunMode::Vm,
                _ => {
                    eprintln!("vo: unknown mode '{}'", m);
                    std::process::exit(1);
                }
            };
            i += 1;
        } else if arg == "--cache" {
            cache = true;
            i += 1;
        } else if let Some(out) = arg.strip_prefix("--compile-only=") {
            compile_only = Some(out.to_string());
            i += 1;
        } else {
            break;
        }
    }
    
    if i >= args.len() {
        eprintln!("vo: missing entry path");
        std::process::exit(1);
    }
    
    let entry = args[i].clone();
    let program_args: Vec<String> = args[i..].to_vec();
    
    LauncherArgs { mode, cache, compile_only, entry, program_args }
}

fn compile_with_cache(entry: &str) -> Result<CompileOutput, vo_launcher::CompileError> {
    let entry_path = Path::new(entry);
    let cache_dir = source_root(entry_path).join(".vo-cache");
    let cache_file = cache_dir.join("module.voc");
    let meta_file = cache_dir.join("mtime");
    let ext_file = cache_dir.join("extensions");
    
    let current_mtime = source_mtime(entry_path);
    
    if let Some(output) = try_load_cache(&cache_file, &meta_file, &ext_file, entry_path, current_mtime) {
        return Ok(output);
    }
    
    let output = compile_source(entry)?;
    
    if let Some(mtime) = current_mtime {
        let _ = std::fs::create_dir_all(&cache_dir);
        let _ = std::fs::write(&cache_file, output.module.serialize());
        let _ = std::fs::write(&meta_file, mtime.to_string());
        save_extensions(&ext_file, &output.extensions);
    }
    
    Ok(output)
}

fn try_load_cache(
    cache_file: &Path,
    meta_file: &Path,
    ext_file: &Path,
    entry_path: &Path,
    current_mtime: Option<u64>,
) -> Option<CompileOutput> {
    let current = current_mtime?;
    let cached: u64 = std::fs::read_to_string(meta_file).ok()?.trim().parse().ok()?;
    if cached != current {
        return None;
    }
    
    // Invalidate cache if compiler binary is newer than cache
    let cache_mtime = cache_file.metadata().ok()?.modified().ok()?
        .duration_since(SystemTime::UNIX_EPOCH).ok()?.as_secs();
    if let Some(exe_mtime) = exe_mtime() {
        if exe_mtime > cache_mtime {
            return None;
        }
    }
    
    let bytes = std::fs::read(cache_file).ok()?;
    let module = Module::deserialize(&bytes).ok()?;
    Some(CompileOutput {
        module,
        source_root: source_root(entry_path),
        extensions: load_extensions(ext_file),
    })
}

fn exe_mtime() -> Option<u64> {
    let exe = std::env::current_exe().ok()?;
    exe.metadata().ok()?.modified().ok()?
        .duration_since(SystemTime::UNIX_EPOCH).ok()
        .map(|d| d.as_secs())
}

fn source_root(entry_path: &Path) -> PathBuf {
    let base = if entry_path.is_dir() {
        entry_path
    } else {
        entry_path.parent().unwrap_or(Path::new("."))
    };
    base.canonicalize().unwrap_or_else(|_| base.to_path_buf())
}

fn load_extensions(path: &Path) -> Vec<ExtensionManifest> {
    std::fs::read_to_string(path)
        .ok()
        .map(|s| {
            s.lines()
                .filter_map(|line| {
                    let (name, path) = line.split_once('\t')?;
                    Some(ExtensionManifest {
                        name: name.to_string(),
                        native_path: PathBuf::from(path),
                    })
                })
                .collect()
        })
        .unwrap_or_default()
}

fn save_extensions(path: &Path, extensions: &[ExtensionManifest]) {
    let data: String = extensions
        .iter()
        .map(|e| format!("{}\t{}", e.name, e.native_path.display()))
        .collect::<Vec<_>>()
        .join("\n");
    let _ = std::fs::write(path, data);
}

fn source_mtime(path: &Path) -> Option<u64> {
    fn file_mtime(path: &Path) -> Option<u64> {
        path.metadata().ok()?.modified().ok()?
            .duration_since(SystemTime::UNIX_EPOCH).ok()
            .map(|d| d.as_secs())
    }
    
    if path.is_file() {
        file_mtime(path)
    } else if path.is_dir() {
        std::fs::read_dir(path).ok()?
            .flatten()
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "vo"))
            .filter_map(|e| file_mtime(&e.path()))
            .max()
    } else {
        None
    }
}

fn print_help() {
    println!("Vo Launcher v{}", VERSION);
    println!();
    println!("Usage: vo [options] <path> [program-args...]");
    println!();
    println!("Options:");
    println!("  --mode=vm|jit   Execution mode (default: vm)");
    println!("  --cache         Enable bytecode caching");
    println!("  --version       Show version");
    println!("  --help          Show this help");
    println!();
    println!("Program receives os.Args = [path, program-args...]");
}
