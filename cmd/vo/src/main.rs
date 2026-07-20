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
//!   cache <subcommand>     Module cache maintenance
//!   release <subcommand>   Release verification and staging
//!   emit <file|dir> [-o out] Compile source to bytecode binary
//!   dump <file.vob>        Disassemble bytecode
//!   help                   Show help
//!   version                Show version

use std::env;
use std::ffi::{OsStr, OsString};
use std::fs::{self, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process;

use vo_engine::{
    check_path_with_auto_install, compile_path_with_auto_install, format_text, run,
    run_with_byte_args, Module, RunError, RunMode,
};
use vo_release::{ArtifactInput, StageReleaseOptions};
use vo_syntax::format_source;

fn main() {
    let args: Vec<OsString> = env::args_os().skip(1).collect();
    process::exit(run_cli(&args));
}

fn run_cli(args: &[OsString]) -> i32 {
    if args.is_empty() {
        print_usage();
        return 1;
    }

    let Some(cmd) = args[0].to_str() else {
        eprintln!("command name must be valid UTF-8");
        return 1;
    };
    let rest = &args[1..];

    let code = match cmd {
        "run" => cmd_run_os(rest),
        "build" => cmd_build(rest),
        "check" => cmd_check(rest),
        "test" => cmd_test(rest),
        "fmt" => cmd_fmt_os(rest),
        "dump" => cmd_dump(rest),
        "emit" => cmd_emit(rest),
        "init" => cmd_init(rest),
        "mod" => cmd_mod(rest),
        "work" => cmd_work(rest),
        "cache" => cmd_cache(rest),
        "release" => cmd_release(rest),
        "-h" | "--help" | "help" => {
            print_usage();
            0
        }
        "-v" | "--version" | "version" => {
            print_version();
            0
        }
        _ => {
            eprintln!("unknown command: {cmd}");
            print_usage();
            1
        }
    };

    code
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
    println!("  mod add <module[@constraint]>");
    println!("                             Add/update intent; retain unrelated valid versions");
    println!("  mod update [module]       Re-select all; named target preserves valid others");
    println!("                             Named target must remain in the result");
    println!("  mod sync [path]           Preserve valid versions; empty graph removes vo.lock");
    println!("  work sync [path]          Select one mixed workspace/registry lock graph");
    println!("  mod fetch [path]          Authenticate pinned dependencies into the cache");
    println!("  mod verify [path]         Verify graph, lock, and cached dependency bytes");
    println!("  mod remove <module>       Remove direct intent and solve the graph");
    println!("  mod tidy [path]           Align imports; retain surviving valid versions");
    println!("  mod why <module> [--declared]");
    println!("                             Explain the effective dependency selection");
    println!("  mod graph [path] [--declared] [--json]");
    println!("                             Print the effective dependency graph");
    println!("  cache clean               Remove the active protocol module cache");
    println!();
    println!("Advanced commands:");
    println!("  emit <file|dir> [-o out]  Compile source to bytecode binary");
    println!("  dump <file.vob>           Disassemble bytecode");
    println!("  release verify [path]     Verify committed release-source readiness");
    println!("  release stage [path] ...  Stage release assets");
    println!();
    println!("  help                      Show this help");
    println!("  version                   Show version");
    println!();
    println!("Run 'vo <command> --help' for more information.");
}

fn version_text() -> String {
    let mut text = format!("vo version {}", vo_module::TOOLCHAIN_VERSION);
    if let Some(hash) = option_env!("VO_BUILD_COMMIT") {
        text.push_str(" (");
        text.push_str(hash);
        text.push(')');
    }
    if let Some(date) = option_env!("VO_BUILD_DATE") {
        text.push(' ');
        text.push_str(date);
    }
    text
}

fn print_version() {
    let version = version_text();
    println!("{version}");
}

fn is_help_arg(arg: &str) -> bool {
    matches!(arg, "-h" | "--help" | "help")
}

fn is_help_os_arg(arg: &OsStr) -> bool {
    arg.to_str().is_some_and(is_help_arg)
}

fn help_only(args: &[OsString]) -> bool {
    args.len() == 1 && is_help_os_arg(&args[0])
}

fn starts_with_dash(arg: &OsStr) -> bool {
    arg.as_encoded_bytes().starts_with(b"-")
}

fn utf8_arg<'a>(arg: &'a OsStr, role: &str) -> Result<&'a str, String> {
    arg.to_str()
        .ok_or_else(|| format!("{role} must be valid UTF-8"))
}

fn report_unknown_option(command: &str, arg: &OsStr) {
    match utf8_arg(arg, &format!("{command} option name")) {
        Ok(arg) => eprintln!("unknown {command} option: {arg}"),
        Err(error) => eprintln!("{error}"),
    }
}

fn parse_run_mode(value: &str) -> Result<RunMode, String> {
    match value {
        "vm" => Ok(RunMode::Vm),
        "jit" => Ok(RunMode::Jit),
        _ => Err(format!(
            "invalid execution mode: {value} (expected vm or jit)"
        )),
    }
}

fn print_run_usage() {
    println!("usage: vo run <file|dir> [--mode=vm|jit] [--codegen] [-- args...]");
}

fn print_build_usage() {
    println!("usage: vo build [path] [-o output.vob]");
}

fn print_check_usage() {
    println!("usage: vo check [path]");
}

fn print_test_usage() {
    println!("usage: vo test [path] [--mode=vm|jit]");
}

fn default_module_output_path(module_name: &str) -> PathBuf {
    let base = if module_name.is_empty() {
        "out"
    } else {
        module_name
    };
    PathBuf::from(format!("{base}.vob"))
}

fn default_emit_output_path(input: &Path, module_name: &str) -> PathBuf {
    if input.is_file() {
        input.with_extension("vob")
    } else {
        default_module_output_path(module_name)
    }
}

fn cmd_run_os(args: &[OsString]) -> i32 {
    if args.len() == 1 && args[0].to_str().is_some_and(is_help_arg) {
        print_run_usage();
        return 0;
    }
    if args.is_empty() {
        print_run_usage();
        return 1;
    }
    let (file, command_args) = if args[0] == OsStr::new("--") {
        let Some(file) = args.get(1) else {
            eprintln!("`--` requires a following run path");
            print_run_usage();
            return 1;
        };
        (PathBuf::from(file), &args[2..])
    } else if starts_with_dash(&args[0]) {
        report_unknown_option("run", &args[0]);
        print_run_usage();
        return 1;
    } else {
        (PathBuf::from(&args[0]), &args[1..])
    };
    let mut mode = RunMode::Vm;
    let mut print_codegen = false;
    let mut program_args: Vec<Vec<u8>> = Vec::new();
    let mut saw_dashdash = false;

    for arg in command_args {
        if saw_dashdash {
            match os_arg_into_bytes(arg.clone()) {
                Ok(arg) => program_args.push(arg),
                Err(error) => {
                    eprintln!("{error}");
                    return 1;
                }
            }
        } else if arg == OsStr::new("--") {
            saw_dashdash = true;
        } else if arg.as_encoded_bytes().starts_with(b"--mode=") {
            let option = match utf8_arg(arg, "run execution mode") {
                Ok(option) => option,
                Err(error) => {
                    eprintln!("{error}");
                    return 1;
                }
            };
            let mode_value = option.strip_prefix("--mode=").unwrap();
            mode = match parse_run_mode(mode_value) {
                Ok(mode) => mode,
                Err(error) => {
                    eprintln!("{error}");
                    return 1;
                }
            };
        } else if arg == OsStr::new("--mode") {
            eprintln!("--mode requires a value in --mode=vm|jit form");
            return 1;
        } else if arg == OsStr::new("--codegen") {
            print_codegen = true;
        } else if starts_with_dash(arg) {
            report_unknown_option("run", arg);
            return 1;
        } else {
            match os_arg_into_bytes(arg.clone()) {
                Ok(arg) => program_args.push(arg),
                Err(error) => {
                    eprintln!("{error}");
                    return 1;
                }
            }
        }
    }

    let output = match compile_path_with_auto_install(&file) {
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

    match run_with_byte_args(output, mode, program_args) {
        Ok(()) => 0,
        Err(RunError::Exited(code)) => code,
        Err(error) => {
            eprintln!("{error}");
            1
        }
    }
}

fn os_arg_into_bytes(value: OsString) -> Result<Vec<u8>, String> {
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStringExt;
        Ok(value.into_vec())
    }

    #[cfg(not(unix))]
    {
        value.into_string().map(String::into_bytes).map_err(|_| {
            "program argument cannot be represented as UTF-8 on this platform".to_string()
        })
    }
}

fn cmd_build(args: &[OsString]) -> i32 {
    if help_only(args) {
        print_build_usage();
        return 0;
    }

    let mut path: Option<PathBuf> = None;
    let mut output_path: Option<PathBuf> = None;
    let mut options = true;

    let mut i = 0;
    while i < args.len() {
        if options && args[i] == OsStr::new("--") {
            options = false;
            i += 1;
        } else if options && args[i] == OsStr::new("-o") {
            if i + 1 >= args.len() {
                eprintln!("-o requires an output path");
                print_build_usage();
                return 1;
            }
            output_path = Some(PathBuf::from(&args[i + 1]));
            i += 2;
        } else if options && starts_with_dash(&args[i]) {
            report_unknown_option("build", &args[i]);
            print_build_usage();
            return 1;
        } else if path.is_none() {
            path = Some(PathBuf::from(&args[i]));
            i += 1;
        } else {
            eprintln!("unexpected build argument: {:?}", args[i]);
            print_build_usage();
            return 1;
        }
    }

    let path = path.unwrap_or_else(|| PathBuf::from("."));

    let output = match compile_path_with_auto_install(&path) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("{}", e);
            return 1;
        }
    };

    let output_path =
        output_path.unwrap_or_else(|| default_module_output_path(&output.module.name));

    let bytes = match output.module.serialize() {
        Ok(bytes) => bytes,
        Err(error) => {
            eprintln!("[VO:SERIALIZE] {error}");
            return 1;
        }
    };
    if let Err(e) = write_file_atomically(&output_path, &bytes) {
        eprintln!("[VO:IO] {}", e);
        return 1;
    }

    println!("{}", output_path.display());
    0
}

fn cmd_check(args: &[OsString]) -> i32 {
    if help_only(args) {
        print_check_usage();
        return 0;
    }

    let path = match args {
        [] => PathBuf::from("."),
        [path] if !starts_with_dash(path) => PathBuf::from(path),
        [separator, path] if separator == OsStr::new("--") => PathBuf::from(path),
        [option] if starts_with_dash(option) => {
            report_unknown_option("check", option);
            print_check_usage();
            return 1;
        }
        _ => {
            eprintln!("invalid check arguments");
            print_check_usage();
            return 1;
        }
    };

    println!("Checking project: {}", path.display());
    match check_path_with_auto_install(&path) {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{}", e);
            1
        }
    }
}

fn cmd_test(args: &[OsString]) -> i32 {
    if help_only(args) {
        print_test_usage();
        return 0;
    }
    let mut mode = RunMode::Vm;
    let mut path: Option<PathBuf> = None;
    let mut options = true;

    for arg in args {
        if options && arg == OsStr::new("--") {
            options = false;
        } else if options && arg.as_encoded_bytes().starts_with(b"--mode=") {
            let option = match utf8_arg(arg, "test execution mode") {
                Ok(option) => option,
                Err(error) => {
                    eprintln!("{error}");
                    return 1;
                }
            };
            let mode_value = option.strip_prefix("--mode=").unwrap();
            mode = match parse_run_mode(mode_value) {
                Ok(mode) => mode,
                Err(error) => {
                    eprintln!("{error}");
                    return 1;
                }
            };
        } else if options && arg == OsStr::new("--mode") {
            eprintln!("--mode requires a value in --mode=vm|jit form");
            return 1;
        } else if options && starts_with_dash(arg) {
            report_unknown_option("test", arg);
            return 1;
        } else if path.is_none() {
            path = Some(PathBuf::from(arg));
        } else {
            eprintln!("unexpected test argument: {arg:?}");
            print_test_usage();
            return 1;
        }
    }

    // Resolve test target: explicit path, or tests/ subdir, or current dir
    let test_path = path.unwrap_or_else(|| {
        let tests_dir = Path::new("tests");
        if tests_dir.is_dir() {
            PathBuf::from("tests")
        } else {
            PathBuf::from(".")
        }
    });

    let output = match compile_path_with_auto_install(&test_path) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("{}", e);
            return 1;
        }
    };

    match run(output, mode, Vec::new()) {
        Ok(()) => 0,
        Err(RunError::Exited(code)) => code,
        Err(error) => {
            eprintln!("{error}");
            1
        }
    }
}

fn cmd_fmt_os(args: &[OsString]) -> i32 {
    let (write_back, mut paths) = match parse_fmt_args(args) {
        Ok(FmtRequest::Help) => {
            print_fmt_usage();
            return 0;
        }
        Ok(FmtRequest::Run { write_back, paths }) => (write_back, paths),
        Err(error) => {
            eprintln!("{error}");
            print_fmt_usage();
            return 1;
        }
    };

    if paths.is_empty() {
        paths.push(OsString::from("."));
    }

    let mut files: Vec<PathBuf> = Vec::new();
    let mut walk_budget = FmtWalkBudget::default();
    for p in &paths {
        let path = Path::new(p);
        let metadata = match fs::symlink_metadata(path) {
            Ok(metadata) => metadata,
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                eprintln!("not found: {}", path.display());
                return 1;
            }
            Err(error) => {
                eprintln!("{}: {}", path.display(), error);
                return 1;
            }
        };
        if metadata.file_type().is_symlink() {
            eprintln!("{}: symbolic links are not followed", path.display());
            return 1;
        } else if metadata.is_file() {
            if let Err(error) = walk_budget.charge_file(path) {
                eprintln!("{}: {}", path.display(), error);
                return 1;
            }
            files.push(path.to_path_buf());
        } else if metadata.is_dir() {
            if let Err(error) = collect_vo_files_with_budget(
                path,
                &mut files,
                &mut walk_budget,
                FmtWalkLimits::PRODUCTION,
            ) {
                eprintln!("{}: {}", path.display(), error);
                return 1;
            }
        } else {
            eprintln!("{}: unsupported file type", path.display());
            return 1;
        }
    }

    files.sort();
    files.dedup();

    if files.is_empty() {
        return 0;
    }

    let mut unformatted = 0;
    let mut errors = 0;

    for file in &files {
        let source = match read_fmt_source(file) {
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
                if let Err(e) = write_file_atomically(file, formatted.as_bytes()) {
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

#[derive(Debug, PartialEq, Eq)]
enum FmtRequest {
    Help,
    Run {
        write_back: bool,
        paths: Vec<OsString>,
    },
}

fn parse_fmt_args(args: &[OsString]) -> Result<FmtRequest, String> {
    let mut write_back = true;
    let mut paths = Vec::new();
    let mut options = true;

    for arg in args {
        if options {
            match arg.to_str() {
                Some("-h" | "--help") => return Ok(FmtRequest::Help),
                Some("--check") => write_back = false,
                Some("--") => options = false,
                _ if starts_with_dash(arg) => {
                    let option = utf8_arg(arg, "fmt option name")?;
                    return Err(format!("unknown fmt option: {option}"));
                }
                _ => paths.push(arg.clone()),
            }
        } else {
            paths.push(arg.clone());
        }
    }

    Ok(FmtRequest::Run { write_back, paths })
}

fn print_fmt_usage() {
    println!("usage: vo fmt [--check] [--] [file|dir ...]");
    println!("  --check  report files whose formatting would change");
}

#[cfg(not(windows))]
fn replace_file_atomically(from: &Path, to: &Path) -> io::Result<()> {
    fs::rename(from, to)
}

#[cfg(windows)]
fn replace_file_atomically(from: &Path, to: &Path) -> io::Result<()> {
    use std::iter::once;
    use std::os::windows::ffi::OsStrExt;

    #[link(name = "kernel32")]
    extern "system" {
        fn MoveFileExW(existing: *const u16, replacement: *const u16, flags: u32) -> i32;
    }

    const MOVEFILE_REPLACE_EXISTING: u32 = 0x1;
    const MOVEFILE_WRITE_THROUGH: u32 = 0x8;
    let from = from
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let to = to
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let result = unsafe {
        MoveFileExW(
            from.as_ptr(),
            to.as_ptr(),
            MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH,
        )
    };
    if result == 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn write_file_atomically(path: &Path, contents: &[u8]) -> io::Result<()> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("source.vo");
    let permissions = fs::metadata(path)
        .ok()
        .map(|metadata| metadata.permissions());

    for attempt in 0..100u32 {
        let temp_path = parent.join(format!(
            ".{file_name}.atomic.{}.{}.tmp",
            process::id(),
            attempt
        ));
        let mut temp = match OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temp_path)
        {
            Ok(file) => file,
            Err(error) if error.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(error),
        };

        let result = (|| {
            if let Some(permissions) = permissions.clone() {
                fs::set_permissions(&temp_path, permissions)?;
            }
            temp.write_all(contents)?;
            temp.sync_all()?;
            drop(temp);
            replace_file_atomically(&temp_path, path)?;
            sync_parent_directory(path)
        })();

        if result.is_err() {
            let _ = fs::remove_file(&temp_path);
        }
        return result;
    }

    Err(io::Error::new(
        io::ErrorKind::AlreadyExists,
        "could not allocate a temporary output file",
    ))
}

fn sync_parent_directory(path: &Path) -> io::Result<()> {
    #[cfg(unix)]
    {
        let parent = path.parent().unwrap_or_else(|| Path::new("."));
        fs::File::open(parent)?.sync_all()?;
    }
    #[cfg(not(unix))]
    let _ = path;
    Ok(())
}

const MAX_FMT_SCAN_DEPTH: usize = vo_module::schema::MAX_PORTABLE_PATH_COMPONENTS;
const MAX_FMT_SCAN_ENTRIES: usize = 100_000;
const MAX_FMT_SOURCE_FILES: usize = 10_000;
const MAX_FMT_SOURCE_BYTES: usize = 16 * 1024 * 1024;

#[derive(Clone, Copy)]
struct FmtWalkLimits {
    max_depth: usize,
    max_entries: usize,
    max_files: usize,
}

impl FmtWalkLimits {
    const PRODUCTION: Self = Self {
        max_depth: MAX_FMT_SCAN_DEPTH,
        max_entries: MAX_FMT_SCAN_ENTRIES,
        max_files: MAX_FMT_SOURCE_FILES,
    };
}

#[derive(Default)]
struct FmtWalkBudget {
    entries: usize,
    files: usize,
}

impl FmtWalkBudget {
    fn charge_file(&mut self, path: &Path) -> io::Result<()> {
        self.files = self
            .files
            .checked_add(1)
            .ok_or_else(|| invalid_fmt_walk("formatter source-file count overflow"))?;
        if self.files > MAX_FMT_SOURCE_FILES {
            return Err(invalid_fmt_walk(format!(
                "formatter source scan exceeds the {MAX_FMT_SOURCE_FILES}-file limit at {}",
                path.display()
            )));
        }
        Ok(())
    }
}

#[cfg(test)]
fn collect_vo_files(dir: &Path, out: &mut Vec<PathBuf>) -> io::Result<()> {
    let mut budget = FmtWalkBudget {
        files: out.len(),
        ..FmtWalkBudget::default()
    };
    collect_vo_files_with_budget(dir, out, &mut budget, FmtWalkLimits::PRODUCTION)
}

fn collect_vo_files_with_budget(
    dir: &Path,
    out: &mut Vec<PathBuf>,
    budget: &mut FmtWalkBudget,
    limits: FmtWalkLimits,
) -> io::Result<()> {
    let mut pending = vec![(dir.to_path_buf(), 0usize)];
    while let Some((dir, depth)) = pending.pop() {
        if depth > limits.max_depth {
            return Err(invalid_fmt_walk(format!(
                "formatter source scan exceeds the {}-directory depth limit at {}",
                limits.max_depth,
                dir.display()
            )));
        }

        let mut entries = fs::read_dir(&dir)?.collect::<Result<Vec<_>, _>>()?;
        budget.entries = budget
            .entries
            .checked_add(entries.len())
            .ok_or_else(|| invalid_fmt_walk("formatter source entry count overflow"))?;
        if budget.entries > limits.max_entries {
            return Err(invalid_fmt_walk(format!(
                "formatter source scan exceeds the {}-entry limit",
                limits.max_entries
            )));
        }
        entries.sort_by_key(|entry| entry.file_name());

        let mut child_directories = Vec::new();
        for entry in entries {
            let path = entry.path();
            let name = entry.file_name();
            let file_type = entry.file_type()?;
            if file_type.is_symlink() {
                continue;
            }
            if file_type.is_dir() {
                if !should_skip_fmt_directory(&name) {
                    child_directories.push(path);
                }
            } else if file_type.is_file() && path.extension() == Some(OsStr::new("vo")) {
                budget.files = budget
                    .files
                    .checked_add(1)
                    .ok_or_else(|| invalid_fmt_walk("formatter source-file count overflow"))?;
                if budget.files > limits.max_files {
                    return Err(invalid_fmt_walk(format!(
                        "formatter source scan exceeds the {}-file limit at {}",
                        limits.max_files,
                        path.display()
                    )));
                }
                out.push(path);
            }
        }

        for child in child_directories.into_iter().rev() {
            pending.push((child, depth.saturating_add(1)));
        }
    }
    Ok(())
}

fn should_skip_fmt_directory(name: &OsStr) -> bool {
    name.as_encoded_bytes().first() == Some(&b'.')
        || matches!(
            name.to_str(),
            Some("target" | "node_modules" | "vendor" | "dist" | "build" | "coverage")
        )
}

fn invalid_fmt_walk(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message.into())
}

fn read_fmt_source(path: &Path) -> io::Result<String> {
    read_fmt_source_with_limit(path, MAX_FMT_SOURCE_BYTES)
}

fn read_fmt_source_with_limit(path: &Path, max_bytes: usize) -> io::Result<String> {
    let file = fs::File::open(path)?;
    let max_len = u64::try_from(max_bytes).unwrap_or(u64::MAX);
    if file.metadata()?.len() > max_len {
        return Err(invalid_fmt_walk(format!(
            "source file exceeds the {max_bytes}-byte formatter limit"
        )));
    }

    let read_limit = max_len.saturating_add(1);
    let mut source = String::new();
    file.take(read_limit).read_to_string(&mut source)?;
    if source.len() > max_bytes {
        return Err(invalid_fmt_walk(format!(
            "source file exceeds the {max_bytes}-byte formatter limit"
        )));
    }
    Ok(source)
}

fn cmd_dump(args: &[OsString]) -> i32 {
    if help_only(args) {
        println!("usage: vo dump <file.vob>");
        return 0;
    }
    let file = match args {
        [file] if !starts_with_dash(file) => PathBuf::from(file),
        [separator, file] if separator == OsStr::new("--") => PathBuf::from(file),
        [option] if starts_with_dash(option) => {
            report_unknown_option("dump", option);
            eprintln!("usage: vo dump <file.vob>");
            return 1;
        }
        _ => {
            eprintln!("usage: vo dump <file.vob>");
            return 1;
        }
    };

    let bytes = match vo_common_core::serialize::read_vob_file(&file) {
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
    if let Err(e) = vo_common_core::verifier::verify_module(&module) {
        eprintln!("[VO:BYTECODE] invalid bytecode: {}", e);
        return 1;
    }

    print!("{}", format_text(&module));
    0
}

fn cmd_emit(args: &[OsString]) -> i32 {
    if help_only(args) {
        println!("usage: vo emit <file.vo|dir> [-o output.vob]");
        return 0;
    }
    if args.is_empty() {
        eprintln!("usage: vo emit <file.vo|dir> [-o output.vob]");
        return 1;
    }
    let mut path: Option<PathBuf> = None;
    let mut output_path: Option<PathBuf> = None;
    let mut options = true;
    let mut i = 0;
    while i < args.len() {
        if options && args[i] == OsStr::new("--") {
            options = false;
            i += 1;
        } else if options && args[i] == OsStr::new("-o") {
            if i + 1 >= args.len() {
                eprintln!("-o requires an output path");
                return 1;
            }
            output_path = Some(PathBuf::from(&args[i + 1]));
            i += 2;
        } else if options && starts_with_dash(&args[i]) {
            report_unknown_option("emit", &args[i]);
            return 1;
        } else if path.is_none() {
            path = Some(PathBuf::from(&args[i]));
            i += 1;
        } else {
            eprintln!("unexpected emit argument: {:?}", args[i]);
            return 1;
        }
    }
    let Some(path) = path else {
        eprintln!("usage: vo emit <file.vo|dir> [-o output.vob]");
        return 1;
    };

    let output = match compile_path_with_auto_install(&path) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("[VO:COMPILE] {}", e);
            return 1;
        }
    };

    let output_path =
        output_path.unwrap_or_else(|| default_emit_output_path(&path, &output.module.name));

    let bytes = match output.module.serialize() {
        Ok(bytes) => bytes,
        Err(error) => {
            eprintln!("[VO:SERIALIZE] {error}");
            return 1;
        }
    };
    if let Err(e) = write_file_atomically(&output_path, &bytes) {
        eprintln!("[VO:IO] {}", e);
        return 1;
    }

    println!("Emitted {} -> {}", path.display(), output_path.display());
    0
}

fn cmd_init(args: &[OsString]) -> i32 {
    if help_only(args) {
        println!("usage: vo init <module-path>");
        println!("  e.g. vo init github.com/user/myapp");
        return 0;
    }
    if args.len() != 1 {
        eprintln!("usage: vo init <module-path>");
        eprintln!("  e.g. vo init github.com/user/myapp");
        return 1;
    }
    if starts_with_dash(&args[0]) {
        report_unknown_option("init", &args[0]);
        return 1;
    }

    let module_path = match utf8_arg(&args[0], "module path") {
        Ok(module_path) => module_path,
        Err(error) => {
            eprintln!("[VO:INIT] {error}");
            return 1;
        }
    };
    let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    match vo_module::ops::mod_init(&cwd, module_path) {
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

fn cmd_mod(args: &[OsString]) -> i32 {
    if args.is_empty() {
        print_mod_usage();
        return 1;
    }

    let subcommand = match utf8_arg(&args[0], "module subcommand") {
        Ok(subcommand) => subcommand,
        Err(error) => {
            eprintln!("[VO:MOD] {error}");
            return 1;
        }
    };
    if args.len() == 2
        && is_help_os_arg(&args[1])
        && matches!(
            subcommand,
            "fetch" | "add" | "update" | "sync" | "verify" | "remove" | "tidy" | "why" | "graph"
        )
    {
        print_mod_usage();
        return 0;
    }

    match subcommand {
        "fetch" => cmd_mod_fetch(&args[1..]),
        "add" => cmd_mod_add(&args[1..]),
        "update" => cmd_mod_update(&args[1..]),
        "sync" => cmd_mod_sync(&args[1..]),
        "verify" => cmd_mod_verify(&args[1..]),
        "remove" => cmd_mod_remove(&args[1..]),
        "tidy" => cmd_mod_tidy(&args[1..]),
        "why" => cmd_mod_why(&args[1..]),
        "graph" => cmd_mod_graph(&args[1..]),
        "-h" | "--help" | "help" => {
            print_mod_usage();
            0
        }
        _ => {
            eprintln!("[VO:MOD] unknown subcommand: {subcommand}");
            print_mod_usage();
            1
        }
    }
}

fn cmd_work(args: &[OsString]) -> i32 {
    if args.is_empty() || matches!(args[0].to_str(), Some("-h" | "--help" | "help")) {
        println!("Usage: vo work sync [path]");
        return 0;
    }
    if args[0] != "sync" {
        eprintln!("unknown work command: {}", args[0].to_string_lossy());
        return 1;
    }
    let path = match optional_path_argument(&args[1..], "work sync", "usage: vo work sync [path]") {
        Ok(path) => path,
        Err(()) => return 1,
    };
    let project_root = match require_module_root_from_path(&path, "VO:WORK:SYNC") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let registry = vo_module::github_registry::GitHubRegistry::new();
    match vo_module::ops::work_sync(&project_root, &registry) {
        Ok(vo_module::ops::LockFileStatus::Present) => {
            println!("synced {}", project_root.join("vo.lock").display());
            0
        }
        Ok(vo_module::ops::LockFileStatus::NotRequired) => {
            println!("workspace root has no dependencies; removed vo.lock");
            0
        }
        Err(error) => {
            eprintln!("[VO:WORK:SYNC] {error}");
            1
        }
    }
}

fn print_mod_usage() {
    println!("Usage: vo mod <subcommand> [arguments]");
    println!();
    println!("Subcommands:");
    println!(
        "  add <module[@constraint]>  Add/update direct intent; retain unrelated valid versions"
    );
    println!("  update [module]            Re-select all; named target preserves valid others");
    println!("                             Named target must remain in the resolved graph");
    println!(
        "  sync [path]                Preserve valid selections; empty graph removes any vo.lock"
    );
    println!(
        "  fetch [path]               Fetch dependencies pinned by vo.lock into the module cache"
    );
    println!("  verify [path]              Verify graph, lock state, and cached dependency bytes");
    println!(
        "  remove <module>            Remove direct intent; write or remove vo.lock as needed"
    );
    println!(
        "  tidy [path]                Match direct intent to imports; retain surviving versions"
    );
    println!("  why <module> [--declared]  Explain the effective dependency selection");
    println!("  graph [path] [--declared] [--json]");
    println!("                             Print text or canonical JSON");
}

fn optional_path_argument(args: &[OsString], command: &str, usage: &str) -> Result<PathBuf, ()> {
    match args {
        [] => Ok(PathBuf::from(".")),
        [path] if !starts_with_dash(path) => Ok(PathBuf::from(path)),
        [separator, path] if separator == OsStr::new("--") => Ok(PathBuf::from(path)),
        [option] if starts_with_dash(option) => {
            report_unknown_option(command, option);
            eprintln!("{usage}");
            Err(())
        }
        _ => {
            eprintln!("{usage}");
            Err(())
        }
    }
}

fn require_default_mod_cache_root(tag: &str) -> Result<PathBuf, i32> {
    vo_engine::default_mod_cache_root().map_err(|error| {
        eprintln!("[{tag}] {error}");
        1
    })
}

fn cmd_mod_fetch(args: &[OsString]) -> i32 {
    let path = match optional_path_argument(args, "mod fetch", "usage: vo mod fetch [path]") {
        Ok(path) => path,
        Err(()) => return 1,
    };
    let project_root = match require_module_root_from_path(&path, "VO:MOD:FETCH") {
        Ok(path) => path,
        Err(code) => return code,
    };

    let registry = vo_module::github_registry::GitHubRegistry::new();
    let cache_root = match require_default_mod_cache_root("VO:MOD:FETCH") {
        Ok(root) => root,
        Err(code) => return code,
    };
    match vo_module::ops::mod_fetch(&project_root, &cache_root, &registry) {
        Ok(vo_module::ops::LockFileStatus::Present) => {
            println!("fetched dependencies into {}", cache_root.display());
            0
        }
        Ok(vo_module::ops::LockFileStatus::NotRequired) => {
            println!("module has no external dependencies; nothing to fetch");
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:FETCH] {error}");
            1
        }
    }
}

fn cmd_mod_add(args: &[OsString]) -> i32 {
    if args.len() != 1 {
        eprintln!("usage: vo mod add <module-path>[@constraint]");
        return 1;
    }
    if starts_with_dash(&args[0]) {
        report_unknown_option("mod add", &args[0]);
        return 1;
    }
    let dependency = match utf8_arg(&args[0], "module path and constraint") {
        Ok(dependency) => dependency,
        Err(error) => {
            eprintln!("[VO:MOD:ADD] {error}");
            return 1;
        }
    };
    let project_root = match require_module_root_from_path(".", "VO:MOD:ADD") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let (dep_path, constraint) = match dependency.rsplit_once('@') {
        Some((module, constraint)) => (module, Some(constraint)),
        None => (dependency, None),
    };
    let registry = vo_module::github_registry::GitHubRegistry::new();
    match vo_module::ops::mod_add(&project_root, dep_path, constraint, &registry) {
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

fn cmd_mod_update(args: &[OsString]) -> i32 {
    if args.len() > 1 {
        eprintln!("usage: vo mod update [module-path]");
        return 1;
    }
    if args.first().is_some_and(|arg| starts_with_dash(arg)) {
        report_unknown_option("mod update", &args[0]);
        return 1;
    }
    let target = match args.first() {
        Some(value) => match utf8_arg(value, "module path") {
            Ok(value) => Some(value),
            Err(error) => {
                eprintln!("[VO:MOD:UPDATE] {error}");
                return 1;
            }
        },
        None => None,
    };
    let project_root = match require_module_root_from_path(".", "VO:MOD:UPDATE") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let registry = vo_module::github_registry::GitHubRegistry::new();
    match vo_module::ops::mod_update(&project_root, target, &registry) {
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

fn cmd_mod_sync(args: &[OsString]) -> i32 {
    let path = match optional_path_argument(args, "mod sync", "usage: vo mod sync [path]") {
        Ok(path) => path,
        Err(()) => return 1,
    };
    let project_root = match require_module_root_from_path(&path, "VO:MOD:SYNC") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let registry = vo_module::github_registry::GitHubRegistry::new();
    match vo_module::ops::mod_sync(&project_root, &registry) {
        Ok(vo_module::ops::LockFileStatus::Present) => {
            println!("synced {}", project_root.join("vo.lock").display());
            0
        }
        Ok(vo_module::ops::LockFileStatus::NotRequired) => {
            println!(
                "synchronized module graph; vo.lock is unnecessary without external dependencies"
            );
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:SYNC] {}", error);
            1
        }
    }
}

fn cmd_mod_verify(args: &[OsString]) -> i32 {
    let path = match optional_path_argument(args, "mod verify", "usage: vo mod verify [path]") {
        Ok(path) => path,
        Err(()) => return 1,
    };
    let project_root = match require_module_root_from_path(&path, "VO:MOD:VERIFY") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let cache_root = match require_default_mod_cache_root("VO:MOD:VERIFY") {
        Ok(root) => root,
        Err(code) => return code,
    };
    match vo_module::ops::mod_verify(&project_root, &cache_root) {
        Ok(vo_module::ops::LockFileStatus::Present) => {
            println!("verified {}", project_root.join("vo.lock").display());
            0
        }
        Ok(vo_module::ops::LockFileStatus::NotRequired) => {
            println!("verified module graph; vo.lock is unnecessary without external dependencies");
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:VERIFY] {}", error);
            1
        }
    }
}

fn cmd_mod_remove(args: &[OsString]) -> i32 {
    if args.len() != 1 {
        eprintln!("usage: vo mod remove <module-path>");
        return 1;
    }
    if starts_with_dash(&args[0]) {
        report_unknown_option("mod remove", &args[0]);
        return 1;
    }
    let module_path = match utf8_arg(&args[0], "module path") {
        Ok(module_path) => module_path,
        Err(error) => {
            eprintln!("[VO:MOD:REMOVE] {error}");
            return 1;
        }
    };
    let project_root = match require_module_root_from_path(".", "VO:MOD:REMOVE") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let registry = vo_module::github_registry::GitHubRegistry::new();
    match vo_module::ops::mod_remove(&project_root, module_path, &registry) {
        Ok(()) => {
            println!("removed {module_path}");
            print_lock_summary(&project_root);
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:REMOVE] {}", error);
            1
        }
    }
}

fn cmd_mod_tidy(args: &[OsString]) -> i32 {
    let path = match optional_path_argument(args, "mod tidy", "usage: vo mod tidy [path]") {
        Ok(path) => path,
        Err(()) => return 1,
    };
    let project_root = match require_module_root_from_path(&path, "VO:MOD:TIDY") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let registry = vo_module::github_registry::GitHubRegistry::new();
    match vo_module::ops::mod_tidy(&project_root, &registry) {
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

fn cmd_mod_why(args: &[OsString]) -> i32 {
    let (module_argument, declared) = match parse_why_options(args) {
        Ok(options) => options,
        Err(()) => return 1,
    };
    let module_path = match utf8_arg(module_argument, "module path") {
        Ok(module_path) => module_path,
        Err(error) => {
            eprintln!("[VO:MOD:WHY] {error}");
            return 1;
        }
    };
    let project_root = match require_module_root_from_path(".", "VO:MOD:WHY") {
        Ok(path) => path,
        Err(code) => return code,
    };
    let cache_root = match require_default_mod_cache_root("VO:MOD:WHY") {
        Ok(root) => root,
        Err(code) => return code,
    };
    match vo_module::ops::mod_why(
        &project_root,
        &cache_root,
        module_path,
        &graph_snapshot_options(declared),
    ) {
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

fn parse_why_options(args: &[OsString]) -> Result<(&OsStr, bool), ()> {
    let mut module = None;
    let mut declared = false;
    for argument in args {
        if argument == OsStr::new("--declared") {
            if declared {
                eprintln!("mod why: --declared may be specified once");
                return Err(());
            }
            declared = true;
        } else if starts_with_dash(argument) {
            report_unknown_option("mod why", argument);
            eprintln!("usage: vo mod why <module-path> [--declared]");
            return Err(());
        } else if module.replace(argument.as_os_str()).is_some() {
            eprintln!("usage: vo mod why <module-path> [--declared]");
            return Err(());
        }
    }
    module
        .ok_or_else(|| {
            eprintln!("usage: vo mod why <module-path> [--declared]");
        })
        .map(|module| (module, declared))
}

fn parse_graph_options(args: &[OsString], command: &str) -> Result<(PathBuf, bool, bool), ()> {
    let mut path = None;
    let mut declared = false;
    let mut json = false;
    let mut separator = false;
    for argument in args {
        if !separator && argument == OsStr::new("--") {
            if path.is_some() {
                eprintln!("usage: vo {command} [path] [--declared] [--json]");
                return Err(());
            }
            separator = true;
        } else if !separator && argument == OsStr::new("--declared") {
            if declared {
                eprintln!("{command}: --declared may be specified once");
                return Err(());
            }
            declared = true;
        } else if !separator && argument == OsStr::new("--json") {
            if json {
                eprintln!("{command}: --json may be specified once");
                return Err(());
            }
            json = true;
        } else if !separator && starts_with_dash(argument) {
            report_unknown_option(command, argument);
            return Err(());
        } else if path.replace(PathBuf::from(argument)).is_some() {
            eprintln!("usage: vo {command} [path] [--declared] [--json]");
            return Err(());
        }
    }
    if separator && path.is_none() {
        eprintln!("usage: vo {command} [path] [--declared] [--json]");
        return Err(());
    }
    Ok((path.unwrap_or_else(|| PathBuf::from(".")), declared, json))
}

fn graph_snapshot_options(declared: bool) -> vo_module::snapshot::SnapshotOptions {
    if declared {
        vo_module::snapshot::SnapshotOptions::declared()
    } else {
        vo_module::snapshot::SnapshotOptions {
            mode: vo_module::snapshot::GraphMode::Effective,
            workspace: vo_module::workspace::workspace_discovery_from_environment(),
        }
    }
}

fn capture_project_snapshot(
    args: &[OsString],
    command: &str,
) -> Result<(vo_module::snapshot::ProjectSnapshot, bool), i32> {
    let (path, declared, json) = parse_graph_options(args, command).map_err(|()| 1)?;
    let tag = "VO:MOD:GRAPH";
    let project_root = require_module_root_from_path(&path, tag)?;
    let cache_root = require_default_mod_cache_root(tag)?;
    let options = graph_snapshot_options(declared);
    vo_module::snapshot::ProjectSnapshot::capture(&project_root, &cache_root, &options)
        .map(|snapshot| (snapshot, json))
        .map_err(|error| {
            eprintln!("[{tag}] {error}");
            1
        })
}

fn cmd_mod_graph(args: &[OsString]) -> i32 {
    let (snapshot, json) = match capture_project_snapshot(args, "mod graph") {
        Ok(snapshot) => snapshot,
        Err(code) => return code,
    };
    if json {
        return match snapshot.render() {
            Ok(bytes) => match io::stdout().write_all(&bytes) {
                Ok(()) => 0,
                Err(error) => {
                    eprintln!("[VO:MOD:GRAPH] failed to write graph: {error}");
                    1
                }
            },
            Err(error) => {
                eprintln!("[VO:MOD:GRAPH] {error}");
                1
            }
        };
    }
    match vo_module::snapshot::render_graph(&snapshot) {
        Ok(graph) => {
            print!("{graph}");
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:GRAPH] {error}");
            1
        }
    }
}

fn cmd_cache(args: &[OsString]) -> i32 {
    if args.len() == 1 && is_help_os_arg(&args[0]) {
        print_cache_usage();
        return 0;
    }
    if args.len() == 2 && args[0] == OsStr::new("clean") && is_help_os_arg(&args[1]) {
        print_cache_usage();
        return 0;
    }
    if args.len() != 1 || args[0] != OsStr::new("clean") {
        print_cache_usage();
        return 1;
    }
    let cache_root = match require_default_mod_cache_root("VO:CACHE:CLEAN") {
        Ok(root) => root,
        Err(code) => return code,
    };
    match vo_module::ops::cache_clean(&cache_root) {
        Ok(result) => {
            if result.removed_dirs == 0 {
                println!("active module cache is clean: {}", cache_root.display());
            } else {
                println!(
                    "removed {} cached module version(s) from {}",
                    result.removed_dirs,
                    cache_root.display(),
                );
            }
            0
        }
        Err(error) => {
            eprintln!("[VO:CACHE:CLEAN] {error}");
            1
        }
    }
}

fn print_cache_usage() {
    println!("Usage: vo cache clean");
    println!();
    println!("Remove every installed module version from the active protocol cache.");
    match vo_engine::default_mod_cache_root() {
        Ok(root) => println!("Active cache root: {}", root.display()),
        Err(error) => println!("Active cache root is unavailable: {error}"),
    }
    println!("VO_MOD_CACHE selects an exact, non-empty absolute alternative root.");
}

fn cmd_release(args: &[OsString]) -> i32 {
    if args.is_empty() {
        print_release_usage();
        return 1;
    }
    let subcommand = match utf8_arg(&args[0], "release subcommand") {
        Ok(subcommand) => subcommand,
        Err(error) => {
            eprintln!("[VO:RELEASE] {error}");
            return 1;
        }
    };
    if args.len() == 2 && is_help_os_arg(&args[1]) && matches!(subcommand, "verify" | "stage") {
        print_release_usage();
        return 0;
    }

    match subcommand {
        "verify" => cmd_release_verify(&args[1..]),
        "stage" => cmd_release_stage(&args[1..]),
        "-h" | "--help" | "help" => {
            print_release_usage();
            0
        }
        _ => {
            eprintln!("[VO:RELEASE] unknown subcommand: {subcommand}");
            print_release_usage();
            1
        }
    }
}

fn print_release_usage() {
    println!("Usage: vo release <subcommand> [arguments]");
    println!();
    println!("Subcommands:");
    println!(
        "  verify [path]              Verify clean committed source, graph, and local build inputs"
    );
    println!("  stage [path] --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]");
}

fn cmd_release_verify(args: &[OsString]) -> i32 {
    let path =
        match optional_path_argument(args, "release verify", "usage: vo release verify [path]") {
            Ok(path) => path,
            Err(()) => return 1,
        };
    let project_root = match require_module_root_from_path(&path, "VO:RELEASE:VERIFY") {
        Ok(path) => path,
        Err(code) => return code,
    };
    match vo_release::verify_repo(&project_root) {
        Ok(()) => {
            println!("release source ready {}", project_root.display());
            0
        }
        Err(error) => {
            eprintln!("[VO:RELEASE:VERIFY] {}", error);
            1
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum StageCliRequest {
    Help,
    Run {
        project_path: PathBuf,
        options: StageReleaseOptions,
    },
}

const RELEASE_STAGE_USAGE: &str = "usage: vo release stage [path] --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]";

fn parse_release_stage_args(args: &[OsString], cwd: &Path) -> Result<StageCliRequest, String> {
    let mut project_path = PathBuf::from(".");
    let mut index = 0;
    if args.first() == Some(&OsString::from("--")) {
        let Some(path) = args.get(1) else {
            return Err("`--` requires a following release project path".to_string());
        };
        project_path = PathBuf::from(path);
        index = 2;
    } else if args.first().is_some_and(|arg| !starts_with_dash(arg)) {
        project_path = PathBuf::from(&args[0]);
        index = 1;
    }

    let mut commit: Option<String> = None;
    let mut out_dir: Option<PathBuf> = None;
    let mut artifacts: Vec<ArtifactInput> = Vec::new();

    while index < args.len() {
        let option = utf8_arg(&args[index], "release stage option name")?;
        match option {
            "--commit" => {
                if commit.is_some() {
                    return Err("`--commit` may be specified once".to_string());
                }
                let value = args
                    .get(index + 1)
                    .ok_or_else(|| "`--commit` requires a commit identifier".to_string())?;
                commit = Some(utf8_arg(value, "release commit")?.to_string());
                index += 2;
            }
            "--out-dir" => {
                if out_dir.is_some() {
                    return Err("`--out-dir` may be specified once".to_string());
                }
                let value = args
                    .get(index + 1)
                    .ok_or_else(|| "`--out-dir` requires an output path".to_string())?;
                out_dir = Some(resolve_cli_path(cwd, value));
                index += 2;
            }
            "--artifact" => {
                if index + 4 >= args.len() {
                    return Err("`--artifact` requires KIND TARGET NAME PATH arguments".to_string());
                }
                if artifacts.len() >= vo_module::MAX_MODULE_ARTIFACTS {
                    return Err(format!(
                        "release stage accepts at most {} artifacts",
                        vo_module::MAX_MODULE_ARTIFACTS,
                    ));
                }
                artifacts.push(ArtifactInput {
                    kind: utf8_arg(&args[index + 1], "artifact kind")?.to_string(),
                    target: utf8_arg(&args[index + 2], "artifact target")?.to_string(),
                    name: utf8_arg(&args[index + 3], "artifact name")?.to_string(),
                    path: resolve_cli_path(cwd, &args[index + 4]),
                });
                index += 5;
            }
            "-h" | "--help" | "help" => return Ok(StageCliRequest::Help),
            argument => return Err(format!("unknown release stage argument: {argument}")),
        }
    }

    let out_dir = out_dir.ok_or_else(|| "release stage requires `--out-dir`".to_string())?;
    Ok(StageCliRequest::Run {
        project_path,
        options: StageReleaseOptions {
            commit,
            artifacts,
            out_dir,
        },
    })
}

fn cmd_release_stage(args: &[OsString]) -> i32 {
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
    let (project_path, options) = match parse_release_stage_args(args, &cwd) {
        Ok(StageCliRequest::Help) => {
            print_release_usage();
            return 0;
        }
        Ok(StageCliRequest::Run {
            project_path,
            options,
        }) => (project_path, options),
        Err(error) => {
            eprintln!("[VO:RELEASE:STAGE] {error}");
            eprintln!("{RELEASE_STAGE_USAGE}");
            return 1;
        }
    };

    let project_root = match require_module_root_from_path(&project_path, "VO:RELEASE:STAGE") {
        Ok(path) => path,
        Err(code) => return code,
    };
    match vo_release::stage_release(&project_root, &options) {
        Ok(staged) => {
            println!("staged release assets in {}", staged.out_dir.display());
            for asset in staged.assets() {
                println!("asset {}", asset.display());
            }
            0
        }
        Err(error) => {
            eprintln!("[VO:RELEASE:STAGE] {}", error);
            1
        }
    }
}

fn require_module_root_from_path(path: impl AsRef<Path>, scope: &str) -> Result<PathBuf, i32> {
    module_root_from_path(path).map_err(|error| {
        eprintln!("[{}] {}", scope, error);
        1
    })
}

fn resolve_cli_path(cwd: &Path, value: &OsStr) -> PathBuf {
    let path = PathBuf::from(value);
    if path.is_absolute() {
        path
    } else {
        cwd.join(path)
    }
}

fn print_lock_summary(project_root: &Path) {
    match vo_module::project::read_lock_file_stable(project_root) {
        Ok(lock) => {
            println!("wrote vo.lock with {} resolved modules", lock.modules.len());
            for module in &lock.modules {
                println!("locked {}@{}", module.path, module.version);
            }
        }
        Err(vo_module::Error::Io(error)) if error.kind() == std::io::ErrorKind::NotFound => {
            println!("vo.lock omitted because the module has no external dependencies");
        }
        Err(e) => {
            eprintln!("warning: could not read vo.lock: {}", e);
        }
    }
}

fn module_root_from_path(path: impl AsRef<Path>) -> Result<PathBuf, String> {
    let path = path.as_ref();
    let metadata = fs::symlink_metadata(path)
        .map_err(|error| format!("cannot inspect input path {}: {error}", path.display()))?;
    let file_type = metadata.file_type();
    if file_type.is_symlink() {
        return Err(format!(
            "input path must not be a symbolic link or reparse point: {}",
            path.display()
        ));
    }
    let dir = if file_type.is_dir() {
        path.to_path_buf()
    } else if file_type.is_file() {
        path.parent()
            .ok_or_else(|| format!("input file has no parent directory: {}", path.display()))?
            .to_path_buf()
    } else {
        return Err(format!(
            "input path must be a regular file or directory: {}",
            path.display()
        ));
    };
    let dir = dir.canonicalize().map_err(|error| {
        format!(
            "cannot canonicalize input directory {}: {error}",
            dir.display()
        )
    })?;
    let search_start = dir.clone();
    vo_module::project::find_project_root(&dir)
        .map_err(|error| {
            format!(
                "project discovery failed from {}: {error}",
                search_start.display()
            )
        })?
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
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(nested.join("main.vo"), "package main\n").unwrap();

        let resolved = module_root_from_path(nested.join("main.vo")).unwrap();
        assert_eq!(resolved, root.canonicalize().unwrap());

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn test_module_root_from_path_rejects_missing_input() {
        let root = unique_temp_dir("missing-input");
        fs::create_dir_all(&root).unwrap();

        let error = module_root_from_path(root.join("missing.vo")).unwrap_err();
        assert!(error.contains("cannot inspect input path"), "{error}");

        fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn test_module_root_from_path_rejects_special_input() {
        let error = module_root_from_path("/dev/null").unwrap_err();
        assert!(error.contains("regular file or directory"), "{error}");
    }

    #[test]
    fn test_module_root_from_path_errors_without_vo_mod() {
        let root = unique_temp_dir("plain-dir");
        let nested = root.join("src");
        fs::create_dir_all(&nested).unwrap();

        let error = module_root_from_path(&nested).unwrap_err();
        assert!(error.contains("no vo.mod found"), "{error}");

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn formatter_write_replaces_atomically_and_cleans_temp_file() {
        let root = unique_temp_dir("fmt-atomic");
        fs::create_dir_all(&root).unwrap();
        let source = root.join("main.vo");
        fs::write(&source, "old").unwrap();

        write_file_atomically(&source, b"new source").unwrap();

        assert_eq!(fs::read_to_string(&source).unwrap(), "new source");
        assert_eq!(fs::read_dir(&root).unwrap().count(), 1);
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn bytecode_output_paths_share_module_naming_for_directories() {
        let root = unique_temp_dir("emit-output");
        let source = root.join("entry.vo");
        fs::create_dir_all(&root).unwrap();
        fs::write(&source, "package main\n").unwrap();

        assert_eq!(
            default_module_output_path("example"),
            PathBuf::from("example.vob")
        );
        assert_eq!(default_module_output_path(""), PathBuf::from("out.vob"));
        assert_eq!(
            default_emit_output_path(&root, "example"),
            PathBuf::from("example.vob")
        );
        assert_eq!(
            default_emit_output_path(&source, "ignored"),
            source.with_extension("vob")
        );

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn formatter_argument_parser_rejects_unknown_options_before_writes() {
        let root = unique_temp_dir("fmt-option");
        fs::create_dir_all(&root).unwrap();
        let source = root.join("main.vo");
        let original = "package main\nfunc main(){}\n";
        fs::write(&source, original).unwrap();

        let code = cmd_fmt_os(&[OsString::from("--unknown"), source.clone().into_os_string()]);

        assert_eq!(code, 1);
        assert_eq!(fs::read_to_string(&source).unwrap(), original);
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn formatter_argument_parser_supports_help_and_option_terminator() {
        assert_eq!(
            parse_fmt_args(&[OsString::from("--help")]),
            Ok(FmtRequest::Help)
        );
        assert_eq!(
            parse_fmt_args(&os_strings(&["--check", "--", "-source.vo"])),
            Ok(FmtRequest::Run {
                write_back: false,
                paths: vec![OsString::from("-source.vo")],
            })
        );
    }

    #[test]
    fn release_stage_rejects_duplicate_singleton_options() {
        for option in ["--commit", "--out-dir"] {
            let args = match option {
                "--commit" => os_strings(&[
                    "--commit",
                    "first",
                    "--commit",
                    "second",
                    "--out-dir",
                    "dist",
                ]),
                "--out-dir" => os_strings(&["--out-dir", "dist-a", "--out-dir", "dist-b"]),
                _ => unreachable!(),
            };
            assert_eq!(
                parse_release_stage_args(&args, Path::new(".")),
                Err(format!("`{option}` may be specified once")),
            );
        }
    }

    #[test]
    fn release_stage_bounds_artifact_arguments_during_parsing() {
        let mut args = os_strings(&["--out-dir", "dist"]);
        for index in 0..=vo_module::MAX_MODULE_ARTIFACTS {
            args.extend([
                OsString::from("--artifact"),
                OsString::from("wasm"),
                OsString::from("wasm32-unknown-unknown"),
                OsString::from(format!("artifact-{index}.wasm")),
                OsString::from(format!("artifacts/artifact-{index}.wasm")),
            ]);
        }

        assert_eq!(
            parse_release_stage_args(&args, Path::new(".")),
            Err(format!(
                "release stage accepts at most {} artifacts",
                vo_module::MAX_MODULE_ARTIFACTS,
            )),
        );
    }

    #[cfg(unix)]
    #[test]
    fn formatter_argument_parser_preserves_non_utf8_source_paths() {
        use std::os::unix::ffi::OsStringExt;

        let source = OsString::from_vec(b"source-\xff.vo".to_vec());
        assert_eq!(
            parse_fmt_args(std::slice::from_ref(&source)),
            Ok(FmtRequest::Run {
                write_back: true,
                paths: vec![source],
            })
        );
    }

    #[cfg(unix)]
    #[test]
    fn filesystem_argument_parsers_preserve_non_utf8_paths() {
        use std::os::unix::ffi::OsStringExt;

        let project = OsString::from_vec(b"project-\xff".to_vec());
        let leading_dash = OsString::from_vec(b"-project-\xfe".to_vec());
        assert_eq!(
            optional_path_argument(std::slice::from_ref(&project), "test", "usage: test"),
            Ok(PathBuf::from(&project))
        );
        assert_eq!(
            optional_path_argument(
                &[OsString::from("--"), leading_dash.clone()],
                "test",
                "usage: test"
            ),
            Ok(PathBuf::from(leading_dash))
        );

        let cwd = PathBuf::from("/tmp/vo-cli-argument-root");
        let out_dir = OsString::from_vec(b"dist-\xfd".to_vec());
        let artifact_path = OsString::from_vec(b"artifact-\xfc.wasm".to_vec());
        let request = parse_release_stage_args(
            &[
                project.clone(),
                OsString::from("--out-dir"),
                out_dir.clone(),
                OsString::from("--artifact"),
                OsString::from("wasm"),
                OsString::from("wasm32-unknown-unknown"),
                OsString::from("app.wasm"),
                artifact_path.clone(),
            ],
            &cwd,
        )
        .unwrap();
        let StageCliRequest::Run {
            project_path,
            options,
        } = request
        else {
            panic!("expected a release stage request");
        };
        assert_eq!(project_path, PathBuf::from(project));
        assert_eq!(options.out_dir, cwd.join(out_dir));
        assert_eq!(options.artifacts[0].path, cwd.join(artifact_path));
    }

    #[cfg(unix)]
    #[test]
    fn semantic_cli_fields_reject_non_utf8_with_precise_errors() {
        use std::os::unix::ffi::OsStringExt;

        let invalid = OsString::from_vec(b"value-\xff".to_vec());
        assert_eq!(
            utf8_arg(&invalid, "module path"),
            Err("module path must be valid UTF-8".to_string())
        );
        assert_eq!(
            parse_release_stage_args(
                &[
                    OsString::from("--commit"),
                    invalid.clone(),
                    OsString::from("--out-dir"),
                    OsString::from("dist"),
                ],
                Path::new("."),
            ),
            Err("release commit must be valid UTF-8".to_string())
        );

        let invalid_option = OsString::from_vec(b"--bad-\xfe".to_vec());
        assert_eq!(
            parse_release_stage_args(&[invalid_option], Path::new(".")),
            Err("release stage option name must be valid UTF-8".to_string())
        );
    }

    #[cfg(unix)]
    #[test]
    fn cli_dispatch_routes_non_utf8_paths_without_global_utf8_conversion() {
        use std::os::unix::ffi::OsStringExt;

        let missing = OsString::from_vec(b"missing-\xff".to_vec());
        for command in ["build", "check", "test", "dump", "emit"] {
            assert_eq!(
                run_cli(&[OsString::from(command), missing.clone()]),
                1,
                "{command}"
            );
        }
        assert_eq!(
            run_cli(&[
                OsString::from("mod"),
                OsString::from("sync"),
                missing.clone(),
            ]),
            1
        );
        assert_eq!(
            run_cli(&[OsString::from("release"), OsString::from("verify"), missing,]),
            1
        );
    }

    #[cfg(unix)]
    #[test]
    fn filesystem_commands_accept_non_utf8_paths_end_to_end_when_supported() {
        use std::os::unix::ffi::OsStringExt;

        let root = unique_temp_dir("non-utf8-e2e");
        fs::create_dir_all(&root).unwrap();
        let project = root.join(OsString::from_vec(b"project-\xff".to_vec()));
        if let Err(error) = fs::create_dir(&project) {
            let host_rejects_the_name = error.kind() == io::ErrorKind::PermissionDenied
                || (cfg!(target_vendor = "apple") && error.raw_os_error() == Some(92));
            if host_rejects_the_name {
                fs::remove_dir_all(root).unwrap();
                return;
            }
            panic!("create non-UTF-8 project directory: {error}");
        }

        fs::write(
            project.join("vo.mod"),
            format!(
                "format = 1\nmodule = \"github.com/acme/non-utf8-cli\"\nversion = \"0.1.0\"\nvo = \"{}\"\n",
                vo_module::TOOLCHAIN_CONSTRAINT
            ),
        )
        .unwrap();
        fs::write(project.join("main.vo"), "package main\n\nfunc main() {}\n").unwrap();

        let project_arg = project.clone().into_os_string();
        assert_eq!(cmd_check(std::slice::from_ref(&project_arg)), 0);
        assert_eq!(cmd_run_os(std::slice::from_ref(&project_arg)), 0);
        assert_eq!(cmd_test(std::slice::from_ref(&project_arg)), 0);
        assert_eq!(cmd_fmt_os(std::slice::from_ref(&project_arg)), 0);

        let build_output = project.join(OsString::from_vec(b"build-\xfe.vob".to_vec()));
        assert_eq!(
            cmd_build(&[
                project_arg.clone(),
                OsString::from("-o"),
                build_output.clone().into_os_string(),
            ]),
            0
        );
        assert_eq!(cmd_dump(&[build_output.into_os_string()]), 0);

        let emit_output = project.join(OsString::from_vec(b"emit-\xfd.vob".to_vec()));
        assert_eq!(
            cmd_emit(&[
                project_arg.clone(),
                OsString::from("-o"),
                emit_output.clone().into_os_string(),
            ]),
            0
        );
        assert!(emit_output.is_file());

        for subcommand in ["sync", "verify", "fetch", "tidy", "graph"] {
            assert_eq!(
                cmd_mod(&[OsString::from(subcommand), project_arg.clone(),]),
                0,
                "vo mod {subcommand}"
            );
        }

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn formatter_directory_walk_is_sorted_and_skips_large_dirs_and_symlinks() {
        let root = unique_temp_dir("fmt-walk");
        fs::create_dir_all(root.join("nested")).unwrap();
        fs::create_dir_all(root.join("target")).unwrap();
        fs::create_dir_all(root.join("node_modules")).unwrap();
        fs::write(root.join("z.vo"), "package z\n").unwrap();
        fs::write(root.join("nested/a.vo"), "package a\n").unwrap();
        fs::write(root.join("target/ignored.vo"), "package ignored\n").unwrap();
        fs::write(root.join("node_modules/ignored.vo"), "package ignored\n").unwrap();

        #[cfg(unix)]
        std::os::unix::fs::symlink(root.join("nested"), root.join("linked")).unwrap();

        let mut files = Vec::new();
        collect_vo_files(&root, &mut files).unwrap();
        files.sort();
        let relative: Vec<_> = files
            .iter()
            .map(|path| path.strip_prefix(&root).unwrap().to_path_buf())
            .collect();
        assert_eq!(
            relative,
            vec![PathBuf::from("nested/a.vo"), PathBuf::from("z.vo")]
        );
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn formatter_directory_walk_reports_read_errors() {
        let root = unique_temp_dir("fmt-missing");
        let error = collect_vo_files(&root, &mut Vec::new()).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::NotFound);
    }

    #[test]
    fn formatter_source_reads_are_bounded_before_formatting() {
        let root = unique_temp_dir("fmt-source-limit");
        fs::create_dir_all(&root).unwrap();
        let source = root.join("large.vo");
        fs::write(&source, "x".repeat(17)).unwrap();

        let error = read_fmt_source_with_limit(&source, 16).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::InvalidData);
        assert!(error.to_string().contains("16-byte formatter limit"));

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn formatter_directory_walk_enforces_shared_resource_limits() {
        let root = unique_temp_dir("fmt-limits");
        fs::create_dir_all(root.join("a/b/c")).unwrap();
        let limits = FmtWalkLimits {
            max_depth: 2,
            max_entries: 16,
            max_files: 16,
        };
        let error = collect_vo_files_with_budget(
            &root,
            &mut Vec::new(),
            &mut FmtWalkBudget::default(),
            limits,
        )
        .unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::InvalidData);
        assert!(error.to_string().contains("directory depth limit"));
        fs::remove_dir_all(root).unwrap();

        let root = unique_temp_dir("fmt-entry-limits");
        fs::create_dir_all(&root).unwrap();
        for index in 0..3 {
            fs::write(root.join(format!("entry-{index}")), "").unwrap();
        }
        let limits = FmtWalkLimits {
            max_depth: 2,
            max_entries: 2,
            max_files: 16,
        };
        let error = collect_vo_files_with_budget(
            &root,
            &mut Vec::new(),
            &mut FmtWalkBudget::default(),
            limits,
        )
        .unwrap_err();
        assert!(error.to_string().contains("entry limit"));
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn init_uses_the_module_protocol_toolchain_authority() {
        let root = unique_temp_dir("init-toolchain-version");
        fs::create_dir_all(&root).unwrap();
        vo_module::ops::mod_init(&root, "github.com/acme/current-version").unwrap();
        let mod_file = fs::read_to_string(root.join("vo.mod")).unwrap();
        assert_eq!(
            mod_file,
            format!(
                "format = 1\nmodule = \"github.com/acme/current-version\"\nversion = \"0.1.0\"\nvo = \"{}\"\n",
                vo_module::TOOLCHAIN_CONSTRAINT,
            ),
        );
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn version_output_uses_the_module_protocol_toolchain_authority() {
        let expected = format!("vo version {}", vo_module::TOOLCHAIN_VERSION);
        assert!(version_text().starts_with(&expected));
    }

    #[test]
    fn module_cli_accepts_the_canonical_lockless_empty_graph() {
        let root = unique_temp_dir("module-lockless");
        fs::create_dir_all(&root).unwrap();
        fs::write(
            root.join("vo.mod"),
            format!(
                "format = 1\nmodule = \"github.com/acme/empty\"\nversion = \"0.1.0\"\nvo = \"{}\"\n",
                vo_module::TOOLCHAIN_CONSTRAINT
            ),
        )
        .unwrap();
        let path = root.clone().into_os_string();

        assert_eq!(cmd_mod_sync(std::slice::from_ref(&path)), 0);
        assert!(!root.join("vo.lock").exists());
        fs::remove_file(root.join(".vo-project.lock")).unwrap();
        assert_eq!(cmd_mod_verify(std::slice::from_ref(&path)), 0);
        assert_eq!(cmd_mod_fetch(std::slice::from_ref(&path)), 0);
        assert!(!root.join("vo.lock").exists());
        assert!(!root.join(".vo-project.lock").exists());

        let (plan, json) = capture_project_snapshot(std::slice::from_ref(&path), "mod graph")
            .expect("lockless project snapshot");
        assert!(plan.modules.is_empty());
        assert!(!json);
        assert!(!root.join(".vo-project.lock").exists());

        fs::remove_dir_all(root).unwrap();
    }

    fn os_strings(values: &[&str]) -> Vec<OsString> {
        values.iter().map(OsString::from).collect()
    }

    #[test]
    fn command_help_returns_before_running_command_workflows() {
        let help = os_strings(&["--help"]);
        assert_eq!(cmd_run_os(&os_strings(&["--help"])), 0);
        assert_eq!(cmd_build(&help), 0);
        assert_eq!(cmd_check(&help), 0);
        assert_eq!(cmd_test(&help), 0);
        assert_eq!(cmd_dump(&help), 0);
        assert_eq!(cmd_emit(&help), 0);
        assert_eq!(cmd_init(&help), 0);
        assert_eq!(cmd_mod(&os_strings(&["fetch", "--help"])), 0);
        assert_eq!(cmd_mod(&os_strings(&["graph", "--help"])), 0);
        assert_eq!(cmd_cache(&os_strings(&["--help"])), 0);
        assert_eq!(cmd_release(&os_strings(&["verify", "--help"])), 0);
        assert_eq!(cmd_release(&os_strings(&["stage", "--help"])), 0);
        assert_eq!(
            parse_release_stage_args(&help, Path::new(".")),
            Ok(StageCliRequest::Help)
        );
    }

    #[test]
    fn execution_modes_are_strict() {
        assert!(parse_run_mode("vm").is_ok());
        assert!(parse_run_mode("jit").is_ok());
        assert!(parse_run_mode("banana").is_err());
        assert_eq!(cmd_run_os(&os_strings(&["missing.vo", "--mode=banana"])), 1);
        assert_eq!(cmd_test(&os_strings(&["--mode=banana"])), 1);
        assert_eq!(cmd_run_os(&os_strings(&["missing.vo", "--mode"])), 1);
    }

    #[test]
    fn commands_reject_unknown_options_and_extra_positions_early() {
        assert_eq!(cmd_run_os(&os_strings(&["missing.vo", "--unknown"])), 1);
        assert_eq!(cmd_build(&os_strings(&["--unknown"])), 1);
        assert_eq!(cmd_check(&os_strings(&["--unknown"])), 1);
        assert_eq!(cmd_test(&os_strings(&["--unknown"])), 1);
        assert_eq!(cmd_emit(&os_strings(&["missing.vo", "--unknown"])), 1);
        assert_eq!(cmd_init(&os_strings(&["github.com/acme/a", "extra"])), 1);
        assert_eq!(cmd_build(&os_strings(&["one", "two"])), 1);
        assert_eq!(cmd_check(&os_strings(&["one", "two"])), 1);
        assert_eq!(cmd_test(&os_strings(&["one", "two"])), 1);
        assert_eq!(cmd_mod(&os_strings(&["fetch", "one", "two"])), 1);
        assert_eq!(cmd_mod(&os_strings(&["tidy", "one", "two"])), 1);
        assert_eq!(cmd_mod(&os_strings(&["sync", "--unknown"])), 1);
        assert_eq!(cmd_mod(&os_strings(&["graph", "one", "two"])), 1);
        assert_eq!(cmd_mod(&os_strings(&["graph", "--effective"])), 1);
        assert_eq!(cmd_mod(&os_strings(&["why", "--effective"])), 1);
    }

    #[test]
    fn graph_inspection_defaults_to_effective_and_declared_is_explicit() {
        assert_eq!(
            parse_graph_options(&[], "mod graph"),
            Ok((PathBuf::from("."), false, false))
        );
        assert_eq!(
            parse_graph_options(&os_strings(&["project", "--declared"]), "mod graph"),
            Ok((PathBuf::from("project"), true, false))
        );
        assert_eq!(
            parse_graph_options(&os_strings(&["--declared", "--", "-project"]), "mod graph",),
            Ok((PathBuf::from("-project"), true, false))
        );
        assert_eq!(
            parse_graph_options(&os_strings(&["project", "--json"]), "mod graph"),
            Ok((PathBuf::from("project"), false, true))
        );
        assert!(parse_graph_options(&os_strings(&["--"]), "mod graph").is_err());
        assert!(
            parse_graph_options(&os_strings(&["project", "--", "extra"]), "mod graph").is_err()
        );
        assert!(
            parse_graph_options(&os_strings(&["--", "-project", "extra"]), "mod graph",).is_err()
        );
        assert_eq!(
            graph_snapshot_options(false).mode,
            vo_module::snapshot::GraphMode::Effective
        );
        assert_eq!(
            graph_snapshot_options(true).mode,
            vo_module::snapshot::GraphMode::Declared
        );

        let args = os_strings(&["github.com/acme/lib", "--declared"]);
        let (module, declared) = parse_why_options(&args).unwrap();
        assert_eq!(module, OsStr::new("github.com/acme/lib"));
        assert!(declared);
    }

    #[test]
    fn output_options_require_values() {
        assert_eq!(cmd_build(&os_strings(&["-o"])), 1);
        assert_eq!(cmd_emit(&os_strings(&["missing.vo", "-o"])), 1);
    }

    #[cfg(unix)]
    #[test]
    fn run_arguments_preserve_arbitrary_unix_bytes() {
        use std::os::unix::ffi::OsStringExt;

        let value = OsString::from_vec(b"a\xffz".to_vec());
        assert_eq!(os_arg_into_bytes(value).unwrap(), b"a\xffz");
    }

    #[test]
    fn run_command_preserves_guest_exit_status() {
        let root = unique_temp_dir("guest-exit-status");
        fs::create_dir_all(&root).unwrap();
        let source = root.join("main.vo");
        fs::write(
            &source,
            "package main\n\nimport \"os\"\n\nfunc main() { os.Exit(37) }\n",
        )
        .unwrap();

        assert_eq!(cmd_run_os(&[source.into_os_string()]), 37);
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn test_command_preserves_guest_exit_status() {
        let root = unique_temp_dir("test-guest-exit-status");
        fs::create_dir_all(&root).unwrap();
        let source = root.join("main.vo");
        fs::write(
            &source,
            "package main\n\nimport \"os\"\n\nfunc main() { os.Exit(41) }\n",
        )
        .unwrap();

        assert_eq!(cmd_test(&[source.into_os_string()]), 41);
        fs::remove_dir_all(root).unwrap();
    }
}
