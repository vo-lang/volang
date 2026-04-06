//! Vo CLI
//!
//! Commands:
//!   run <file|dir>         Run a Vo program
//!   build [path] [-o out]  Compile to bytecode artifact (.vob)
//!   check [path]           Type-check without running
//!   test [path]            Run tests
//!   fmt [path...]          Format Vo source files
//!   init <module-path>     Initialize a new module
//!   mod <subcommand>       Dependency lifecycle commands
//!   release <subcommand>   Release verification and staging
//!   emit <file> [-o out]   Compile source to bytecode binary
//!   dump <file.vob>        Disassemble bytecode
//!   help                   Show help
//!   version                Show version

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use vo_engine::{
    check_with_auto_install, compile_with_auto_install, format_source, format_text, run, Module,
    RunMode,
};
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
        "test" => cmd_test(rest),
        "fmt" => cmd_fmt(rest),
        "dump" => cmd_dump(rest),
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
            print_version();
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
    println!("Vo Programming Language");
    println!();
    println!("Usage: vo <command> [arguments]");
    println!();
    println!("Common commands:");
    println!("  run <file|dir> [args...]  Run a Vo program");
    println!("  build [path] [-o out]     Compile to bytecode (.vob)");
    println!("  check [path]              Type-check without running");
    println!("  test [path]               Run tests");
    println!("  fmt [file|dir...]         Format Vo source files");
    println!("  init <module-path>        Initialize a new module");
    println!();
    println!("Module commands:");
    println!("  mod add <module[@ver]>    Add a dependency");
    println!("  mod update [module]       Update dependencies");
    println!("  mod sync [path]           Recompute and write vo.lock");
    println!("  mod download [path]       Fetch pinned dependencies");
    println!("  mod verify [path]         Verify vo.lock integrity");
    println!("  mod remove <module>       Remove a dependency");
    println!();
    println!("Advanced commands:");
    println!("  emit <file> [-o out]      Compile source to bytecode binary");
    println!("  dump <file.vob>           Disassemble bytecode");
    println!("  release verify [path]     Verify release readiness");
    println!("  release stage [path] ...  Stage release assets");
    println!();
    println!("  help                      Show this help");
    println!("  version                   Show version");
    println!();
    println!("Run 'vo <command> --help' for more information.");
}

fn print_version() {
    print!("vo version 0.1.0");
    if let Some(hash) = option_env!("VO_BUILD_COMMIT") {
        print!(" ({})", hash);
    }
    if let Some(date) = option_env!("VO_BUILD_DATE") {
        print!(" {}", date);
    }
    println!();
}

fn cmd_run(args: &[String]) -> i32 {
    if args.is_empty() {
        eprintln!("usage: vo run <file|dir> [--mode=jit] [--codegen] [-- args...]");
        return 1;
    }

    let file = &args[0];
    let mut mode = RunMode::Vm;
    let mut print_codegen = false;
    let mut program_args: Vec<String> = Vec::new();
    let mut saw_dashdash = false;

    for arg in &args[1..] {
        if saw_dashdash {
            program_args.push(arg.clone());
        } else if arg == "--" {
            saw_dashdash = true;
        } else if let Some(m) = arg.strip_prefix("--mode=") {
            if m == "jit" {
                mode = RunMode::Jit;
            }
        } else if arg == "--codegen" {
            print_codegen = true;
        } else {
            program_args.push(arg.clone());
        }
    }

    let output = match compile_with_auto_install(file) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("{}", e);
            return 1;
        }
    };

    if print_codegen {
        println!("{}", format_text(&output.module));
        return 0;
    }

    if let Err(e) = run(output, mode, program_args) {
        eprintln!("{}", e);
        return 1;
    }

    0
}

fn cmd_build(args: &[String]) -> i32 {
    let mut path = ".";
    let mut output_path = String::new();

    let mut i = 0;
    while i < args.len() {
        if args[i] == "-o" && i + 1 < args.len() {
            output_path = args[i + 1].clone();
            i += 2;
        } else if !args[i].starts_with('-') && path == "." {
            path = &args[i];
            i += 1;
        } else {
            i += 1;
        }
    }

    let output = match compile_with_auto_install(path) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("{}", e);
            return 1;
        }
    };

    if output_path.is_empty() {
        let name = &output.module.name;
        let base = if name.is_empty() {
            "out"
        } else {
            name.as_str()
        };
        output_path = format!("{}.vob", base);
    }

    let bytes = output.module.serialize();
    if let Err(e) = fs::write(&output_path, bytes) {
        eprintln!("[VO:IO] {}", e);
        return 1;
    }

    println!("{}", output_path);
    0
}

fn cmd_check(args: &[String]) -> i32 {
    let path = if args.is_empty() { "." } else { &args[0] };

    println!("Checking project: {}", path);
    match check_with_auto_install(path) {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{}", e);
            1
        }
    }
}

fn cmd_test(args: &[String]) -> i32 {
    let mut mode = RunMode::Vm;
    let mut path: Option<&str> = None;

    for arg in args {
        if let Some(m) = arg.strip_prefix("--mode=") {
            if m == "jit" {
                mode = RunMode::Jit;
            }
        } else if !arg.starts_with('-') && path.is_none() {
            path = Some(arg);
        }
    }

    // Resolve test target: explicit path, or tests/ subdir, or current dir
    let test_path = if let Some(p) = path {
        p.to_string()
    } else {
        let tests_dir = Path::new("tests");
        if tests_dir.is_dir() {
            "tests".to_string()
        } else {
            ".".to_string()
        }
    };

    let output = match compile_with_auto_install(&test_path) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("{}", e);
            return 1;
        }
    };

    if let Err(e) = run(output, mode, Vec::new()) {
        eprintln!("{}", e);
        return 1;
    }

    0
}

fn cmd_fmt(args: &[String]) -> i32 {
    let mut write_back = true;
    let mut paths: Vec<String> = Vec::new();

    for arg in args {
        if arg == "--check" {
            write_back = false;
        } else if !arg.starts_with('-') {
            paths.push(arg.clone());
        }
    }

    if paths.is_empty() {
        paths.push(".".to_string());
    }

    let mut files: Vec<PathBuf> = Vec::new();
    for p in &paths {
        let path = Path::new(p);
        if path.is_file() {
            files.push(path.to_path_buf());
        } else if path.is_dir() {
            collect_vo_files(path, &mut files);
        } else {
            eprintln!("not found: {}", p);
            return 1;
        }
    }

    if files.is_empty() {
        return 0;
    }

    let mut unformatted = 0;
    let mut errors = 0;

    for file in &files {
        let source = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("{}: {}", file.display(), e);
                errors += 1;
                continue;
            }
        };

        let formatted = match format_source(&source) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("{}: {}", file.display(), e);
                errors += 1;
                continue;
            }
        };

        if formatted != source {
            if write_back {
                if let Err(e) = fs::write(file, &formatted) {
                    eprintln!("{}: {}", file.display(), e);
                    errors += 1;
                } else {
                    println!("{}", file.display());
                }
            } else {
                println!("{}", file.display());
                unformatted += 1;
            }
        }
    }

    if errors > 0 {
        return 1;
    }
    if !write_back && unformatted > 0 {
        return 1;
    }
    0
}

fn collect_vo_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries {
        let entry = match entry {
            Ok(e) => e,
            Err(_) => continue,
        };
        let path = entry.path();
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if name_str.starts_with('.') {
            continue;
        }
        if path.is_dir() {
            collect_vo_files(&path, out);
        } else if name_str.ends_with(".vo") {
            out.push(path);
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

    let output = match compile_with_auto_install(path) {
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
        "tidy" => cmd_mod_tidy(&args[1..]),
        "why" => cmd_mod_why(&args[1..]),
        "clean" => cmd_mod_clean(&args[1..]),
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
    println!(
        "  download [path]            Fetch dependencies pinned by vo.lock into the module cache"
    );
    println!("  add <module[@constraint]>  Add or update a direct dependency and refresh vo.lock");
    println!("  update [module]            Re-solve dependency constraints and refresh vo.lock");
    println!("  sync [path]                Recompute the full dependency graph and write vo.lock");
    println!(
        "  verify [path]              Verify that vo.lock exactly matches the current vo.mod graph"
    );
    println!("  remove <module>            Remove a direct dependency and refresh vo.lock");
    println!(
        "  tidy [path]                Add missing and remove unused dependencies based on imports"
    );
    println!("  why <module>               Show why a module is in the dependency graph");
    println!(
        "  clean [--all]              Remove unused cached modules (--all removes everything)"
    );
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
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_add(
        &project_root,
        dep_path,
        constraint,
        &cache_root,
        &registry,
        "vo mod add",
    ) {
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
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_update(
        &project_root,
        target,
        &cache_root,
        &registry,
        "vo mod update",
    ) {
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
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_sync(&project_root, &cache_root, &registry, "vo mod sync") {
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
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_remove(
        &project_root,
        &args[0],
        &cache_root,
        &registry,
        "vo mod remove",
    ) {
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

fn cmd_mod_tidy(args: &[String]) -> i32 {
    let path = if args.is_empty() { "." } else { &args[0] };
    let project_root = match require_module_root_from_path(path, "VO:MOD:TIDY") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let registry = vo_module::github_registry::GitHubRegistry::new();
    let cache_root = vo_engine::default_mod_cache_root();
    let external_imports = match vo_engine::scan_external_imports(&project_root) {
        Ok(imports) => imports,
        Err(error) => {
            eprintln!("[VO:MOD:TIDY] {}", error);
            return 1;
        }
    };
    match vo_module::ops::mod_tidy(
        &project_root,
        &external_imports,
        &cache_root,
        &registry,
        "vo mod tidy",
    ) {
        Ok(result) => {
            for m in &result.added {
                println!("  + {}", m);
            }
            for m in &result.removed {
                println!("  - {}", m);
            }
            if result.added.is_empty() && result.removed.is_empty() {
                println!("no changes");
            }
            print_lock_summary(&project_root);
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:TIDY] {}", error);
            1
        }
    }
}

fn cmd_mod_why(args: &[String]) -> i32 {
    if args.len() != 1 {
        eprintln!("usage: vo mod why <module-path>");
        return 1;
    }
    let project_root = match require_module_root_from_path(".", "VO:MOD:WHY") {
        Ok(path) => path,
        Err(code) => return code,
    };
    match vo_module::ops::mod_why(&project_root, &args[0]) {
        Ok(chain) => {
            println!("{}", chain.join(" -> "));
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:WHY] {}", error);
            1
        }
    }
}

fn cmd_mod_clean(args: &[String]) -> i32 {
    if args.iter().any(|arg| arg != "--all") {
        eprintln!("usage: vo mod clean [--all]");
        return 1;
    }
    let keep_locked = !args.iter().any(|a| a == "--all");
    let project_root = if keep_locked {
        match require_module_root_from_path(".", "VO:MOD:CLEAN") {
            Ok(path) => path,
            Err(code) => return code,
        }
    } else {
        PathBuf::from(".")
    };
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_clean(&project_root, &cache_root, keep_locked) {
        Ok(result) => {
            if result.removed_dirs == 0 {
                println!("cache is clean");
            } else {
                println!("removed {} cached module version(s)", result.removed_dirs);
            }
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:CLEAN] {}", error);
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
            eprintln!(
                "[VO:RELEASE:STAGE] failed to read current directory: {}",
                error
            );
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
    eprintln!(
        "[VO:GET] use `vo mod sync` to refresh vo.lock and `vo mod download` to fill the cache"
    );
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
    match vo_module::project::read_lock_file(project_root) {
        Ok(lock) => {
            println!(
                "wrote vo.lock with {} resolved modules",
                lock.resolved.len()
            );
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
    let dir = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent().unwrap_or(Path::new(".")).to_path_buf()
    };
    let dir = dir.canonicalize().unwrap_or(dir);
    let search_start = dir.clone();
    vo_module::project::find_project_root(&dir)
        .ok_or_else(|| format!("no vo.mod found from {}", search_start.display()))
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
        fs::write(
            root.join("vo.mod"),
            "module github.com/acme/app\n\nvo 0.1\n",
        )
        .unwrap();

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
