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
use std::path::{Path, PathBuf};
use std::process;

use vo_engine::{compile, format_text, run, Module, RunMode};
use vo_release::{ArtifactInput, StageReleaseOptions};

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
        "mod" => cmd_mod(rest),
        "release" => cmd_release(rest),
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
    println!("  mod <subcommand> Explicit dependency lifecycle commands");
    println!("  release <subcommand> Release verification and staging commands");
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
    let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    match vo_module::ops::mod_init(&cwd, module_path, "^0.1.0") {
        Ok(()) => {
            println!("Initialized module: {}", module_path);
            0
        }
        Err(e) => {
            eprintln!("[VO:INIT] {}", e);
            1
        }
    }
}

fn cmd_mod(args: &[String]) -> i32 {
    if args.is_empty() {
        print_mod_usage();
        return 1;
    }

    match args[0].as_str() {
        "download" => cmd_mod_download(&args[1..]),
        "add" => cmd_mod_add(&args[1..]),
        "update" => cmd_mod_update(&args[1..]),
        "sync" => cmd_mod_sync(&args[1..]),
        "verify" => cmd_mod_verify(&args[1..]),
        "remove" => cmd_mod_remove(&args[1..]),
        "-h" | "--help" | "help" => {
            print_mod_usage();
            0
        }
        _ => {
            eprintln!("[VO:MOD] unknown subcommand: {}", args[0]);
            print_mod_usage();
            1
        }
    }
}

fn print_mod_usage() {
    println!("Usage: vo mod <subcommand> [arguments]");
    println!();
    println!("Subcommands:");
    println!("  download [path]  Fetch dependencies pinned by vo.lock into the module cache");
    println!("  add <module[@constraint]>  Add or update a direct dependency and refresh vo.lock");
    println!("  update [module]            Re-solve dependency constraints and refresh vo.lock");
    println!("  sync [path]                Recompute the full dependency graph and write vo.lock");
    println!("  verify [path]              Verify that vo.lock exactly matches the current vo.mod graph");
    println!("  remove <module>            Remove a direct dependency and refresh vo.lock");
}

fn cmd_mod_download(args: &[String]) -> i32 {
    let path = if args.is_empty() { "." } else { &args[0] };
    let project_root = match require_module_root_from_path(path, "VO:MOD:DOWNLOAD") {
        Ok(path) => path,
        Err(code) => return code,
    };

    let registry = vo_module::github_registry::GitHubRegistry::new();
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_download(&project_root, &cache_root, &registry) {
        Ok(()) => {
            println!("downloaded dependencies to {}", cache_root.display());
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:DOWNLOAD] {}", error);
            1
        }
    }
}

fn cmd_mod_add(args: &[String]) -> i32 {
    if args.len() != 1 {
        eprintln!("usage: vo mod add <module-path>[@constraint]");
        return 1;
    }
    let project_root = match require_module_root_from_path(".", "VO:MOD:ADD") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let (dep_path, constraint) = match args[0].rsplit_once('@') {
        Some((module, constraint)) => (module, Some(constraint)),
        None => (args[0].as_str(), None),
    };
    let registry = vo_module::github_registry::GitHubRegistry::new();
    match vo_module::ops::mod_add(&project_root, dep_path, constraint, &registry, "vo mod add") {
        Ok(()) => {
            println!("added {}", dep_path);
            print_lock_summary(&project_root);
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:ADD] {}", error);
            1
        }
    }
}

fn cmd_mod_update(args: &[String]) -> i32 {
    if args.len() > 1 {
        eprintln!("usage: vo mod update [module-path]");
        return 1;
    }
    let project_root = match require_module_root_from_path(".", "VO:MOD:UPDATE") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let target = args.first().map(|value| value.as_str());
    let registry = vo_module::github_registry::GitHubRegistry::new();
    match vo_module::ops::mod_update(&project_root, target, &registry, "vo mod update") {
        Ok(()) => {
            if let Some(t) = target {
                println!("updated {}", t);
            } else {
                println!("updated dependency graph");
            }
            print_lock_summary(&project_root);
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:UPDATE] {}", error);
            1
        }
    }
}

fn cmd_mod_sync(args: &[String]) -> i32 {
    if args.len() > 1 {
        eprintln!("usage: vo mod sync [path]");
        return 1;
    }
    let path = if args.is_empty() { "." } else { &args[0] };
    let project_root = match require_module_root_from_path(path, "VO:MOD:SYNC") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let registry = vo_module::github_registry::GitHubRegistry::new();
    match vo_module::ops::mod_sync(&project_root, &registry, "vo mod sync") {
        Ok(()) => {
            println!("synced {}", project_root.join("vo.lock").display());
            print_lock_summary(&project_root);
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:SYNC] {}", error);
            1
        }
    }
}

fn cmd_mod_verify(args: &[String]) -> i32 {
    if args.len() > 1 {
        eprintln!("usage: vo mod verify [path]");
        return 1;
    }
    let path = if args.is_empty() { "." } else { &args[0] };
    let project_root = match require_module_root_from_path(path, "VO:MOD:VERIFY") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_verify(&project_root, &cache_root) {
        Ok(()) => {
            println!("verified {}", project_root.join("vo.lock").display());
            print_lock_summary(&project_root);
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:VERIFY] {}", error);
            1
        }
    }
}

fn cmd_mod_remove(args: &[String]) -> i32 {
    if args.len() != 1 {
        eprintln!("usage: vo mod remove <module-path>");
        return 1;
    }
    let project_root = match require_module_root_from_path(".", "VO:MOD:REMOVE") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let registry = vo_module::github_registry::GitHubRegistry::new();
    match vo_module::ops::mod_remove(&project_root, &args[0], &registry, "vo mod remove") {
        Ok(()) => {
            println!("removed {}", args[0]);
            print_lock_summary(&project_root);
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:REMOVE] {}", error);
            1
        }
    }
}

fn cmd_release(args: &[String]) -> i32 {
    if args.is_empty() {
        print_release_usage();
        return 1;
    }

    match args[0].as_str() {
        "verify" => cmd_release_verify(&args[1..]),
        "stage" => cmd_release_stage(&args[1..]),
        "-h" | "--help" | "help" => {
            print_release_usage();
            0
        }
        _ => {
            eprintln!("[VO:RELEASE] unknown subcommand: {}", args[0]);
            print_release_usage();
            1
        }
    }
}

fn print_release_usage() {
    println!("Usage: vo release <subcommand> [arguments]");
    println!();
    println!("Subcommands:");
    println!("  verify [path]              Verify release policy and vo.lock freshness for a module repo");
    println!("  stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]");
}

fn cmd_release_verify(args: &[String]) -> i32 {
    if args.len() > 1 {
        eprintln!("usage: vo release verify [path]");
        return 1;
    }
    let path = if args.is_empty() { "." } else { &args[0] };
    let project_root = match require_module_root_from_path(path, "VO:RELEASE:VERIFY") {
        Ok(path) => path,
        Err(code) => return code,
    };
    match vo_release::verify_repo(&project_root) {
        Ok(()) => {
            println!("release ready {}", project_root.display());
            0
        }
        Err(error) => {
            eprintln!("[VO:RELEASE:VERIFY] {}", error);
            1
        }
    }
}

fn cmd_release_stage(args: &[String]) -> i32 {
    let mut path = ".";
    let mut index = 0;
    if !args.is_empty() && !args[0].starts_with('-') {
        path = &args[0];
        index = 1;
    }

    let cwd = match env::current_dir() {
        Ok(path) => path,
        Err(error) => {
            eprintln!("[VO:RELEASE:STAGE] failed to read current directory: {}", error);
            return 1;
        }
    };

    let mut version: Option<String> = None;
    let mut commit: Option<String> = None;
    let mut out_dir: Option<PathBuf> = None;
    let mut artifacts: Vec<ArtifactInput> = Vec::new();

    while index < args.len() {
        match args[index].as_str() {
            "--version" => {
                if index + 1 >= args.len() {
                    eprintln!("usage: vo release stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]");
                    return 1;
                }
                version = Some(args[index + 1].clone());
                index += 2;
            }
            "--commit" => {
                if index + 1 >= args.len() {
                    eprintln!("usage: vo release stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]");
                    return 1;
                }
                commit = Some(args[index + 1].clone());
                index += 2;
            }
            "--out-dir" => {
                if index + 1 >= args.len() {
                    eprintln!("usage: vo release stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]");
                    return 1;
                }
                out_dir = Some(resolve_cli_path(&cwd, &args[index + 1]));
                index += 2;
            }
            "--artifact" => {
                if index + 4 >= args.len() {
                    eprintln!("usage: vo release stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]");
                    return 1;
                }
                artifacts.push(ArtifactInput {
                    kind: args[index + 1].clone(),
                    target: args[index + 2].clone(),
                    name: args[index + 3].clone(),
                    path: resolve_cli_path(&cwd, &args[index + 4]),
                });
                index += 5;
            }
            "-h" | "--help" | "help" => {
                print_release_usage();
                return 0;
            }
            argument => {
                eprintln!("[VO:RELEASE:STAGE] unknown argument: {}", argument);
                eprintln!("usage: vo release stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]");
                return 1;
            }
        }
    }

    let Some(version) = version else {
        eprintln!("usage: vo release stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]");
        return 1;
    };
    let Some(out_dir) = out_dir else {
        eprintln!("usage: vo release stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]");
        return 1;
    };

    let project_root = match require_module_root_from_path(path, "VO:RELEASE:STAGE") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let options = StageReleaseOptions {
        version,
        commit,
        artifacts,
        out_dir,
    };
    match vo_release::stage_release(&project_root, &options) {
        Ok(staged) => {
            println!("staged release assets in {}", staged.out_dir.display());
            println!("source {}", staged.source_path.display());
            println!("manifest {}", staged.manifest_path.display());
            for artifact in &staged.artifacts {
                println!("artifact {}", artifact.output_path.display());
            }
            0
        }
        Err(error) => {
            eprintln!("[VO:RELEASE:STAGE] {}", error);
            1
        }
    }
}

fn cmd_get(args: &[String]) -> i32 {
    let _ = args;
    eprintln!("[VO:GET] `vo get` has been removed");
    eprintln!("[VO:GET] use `vo mod add <module[@constraint]>` to change direct dependencies");
    eprintln!("[VO:GET] use `vo mod sync` to refresh vo.lock and `vo mod download` to fill the cache");
    eprintln!("[VO:GET] dependency lifecycle now lives under `vo mod ...`");
    1
}


fn require_module_root_from_path(path: &str, scope: &str) -> Result<PathBuf, i32> {
    module_root_from_path(path).map_err(|error| {
        eprintln!("[{}] {}", scope, error);
        1
    })
}

fn resolve_cli_path(cwd: &Path, value: &str) -> PathBuf {
    let path = PathBuf::from(value);
    if path.is_absolute() {
        path
    } else {
        cwd.join(path)
    }
}

fn print_lock_summary(project_root: &Path) {
    match vo_module::ops::read_lock_file(project_root) {
        Ok(lock) => {
            println!("wrote vo.lock with {} resolved modules", lock.resolved.len());
            for module in &lock.resolved {
                println!("locked {}@{}", module.path, module.version);
            }
        }
        Err(e) => {
            eprintln!("warning: could not read vo.lock: {}", e);
        }
    }
}

fn module_root_from_path(path: &str) -> Result<PathBuf, String> {
    let path = Path::new(path);
    let mut dir = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent().unwrap_or(Path::new(".")).to_path_buf()
    };
    dir = dir.canonicalize().unwrap_or(dir);
    let search_start = dir.clone();
    let mut current = dir.as_path();
    loop {
        if current.join("vo.mod").is_file() {
            return Ok(current.to_path_buf());
        }
        match current.parent() {
            Some(parent) => current = parent,
            None => {
                return Err(format!(
                    "no vo.mod found from {}",
                    search_start.display(),
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(name: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!("vo-cli-{}-{}-{}", name, std::process::id(), nanos))
    }

    #[test]
    fn test_module_root_from_path_walks_up_to_vo_mod() {
        let root = unique_temp_dir("module-root");
        let nested = root.join("src/bin");
        fs::create_dir_all(&nested).unwrap();
        fs::write(root.join("vo.mod"), "module github.com/acme/app\n\nvo 0.1\n").unwrap();

        let resolved = module_root_from_path(nested.join("main.vo").to_str().unwrap()).unwrap();
        assert_eq!(resolved, root.canonicalize().unwrap());

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn test_module_root_from_path_errors_without_vo_mod() {
        let root = unique_temp_dir("plain-dir");
        let nested = root.join("src");
        fs::create_dir_all(&nested).unwrap();

        let error = module_root_from_path(nested.to_str().unwrap()).unwrap_err();
        assert!(error.contains("no vo.mod found"), "{error}");

        fs::remove_dir_all(&root).unwrap();
    }
}
