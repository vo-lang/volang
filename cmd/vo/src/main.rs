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
    check_path_with_auto_install, compile_path_with_auto_install, format_source, format_text, run,
    run_with_byte_args, Module, RunError, RunMode,
};
use vo_release::{ArtifactInput, StageReleaseOptions};

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
    println!("  mod add <module[@ver]>    Add a dependency");
    println!("  mod update [module]       Update dependencies");
    println!("  mod sync [path]           Recompute the graph and refresh vo.lock");
    println!("  mod download [path]       Fetch pinned dependencies");
    println!("  mod verify [path]         Verify the module graph and vo.lock");
    println!("  mod remove <module>       Remove a dependency");
    println!();
    println!("Advanced commands:");
    println!("  emit <file|dir> [-o out]  Compile source to bytecode binary");
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
    print!("vo version {}", env!("CARGO_PKG_VERSION"));
    if let Some(hash) = option_env!("VO_BUILD_COMMIT") {
        print!(" ({})", hash);
    }
    if let Some(date) = option_env!("VO_BUILD_DATE") {
        print!(" {}", date);
    }
    println!();
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
    match vo_module::ops::mod_init(&cwd, module_path, current_toolchain_constraint()) {
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

fn current_toolchain_constraint() -> &'static str {
    concat!("^", env!("CARGO_PKG_VERSION"))
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
            "download" | "add" | "update" | "sync" | "verify" | "remove" | "tidy" | "why" | "clean"
        )
    {
        print_mod_usage();
        return 0;
    }

    match subcommand {
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
            eprintln!("[VO:MOD] unknown subcommand: {subcommand}");
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
    println!("  sync [path]                Recompute the graph and refresh or omit vo.lock");
    println!("  verify [path]              Verify the current vo.mod graph and its lock state");
    println!("  remove <module>            Remove a direct dependency and refresh vo.lock");
    println!(
        "  tidy [path]                Add missing and remove unused dependencies based on imports"
    );
    println!("  why <module>               Show why a module is in the dependency graph");
    println!(
        "  clean [--all]              Remove unused cached modules (--all removes everything)"
    );
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

fn cmd_mod_download(args: &[OsString]) -> i32 {
    let path = match optional_path_argument(args, "mod download", "usage: vo mod download [path]") {
        Ok(path) => path,
        Err(()) => return 1,
    };
    let project_root = match require_module_root_from_path(&path, "VO:MOD:DOWNLOAD") {
        Ok(path) => path,
        Err(code) => return code,
    };

    let registry = vo_module::github_registry::GitHubRegistry::new();
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_download(&project_root, &cache_root, &registry) {
        Ok(vo_module::ops::LockFileStatus::Present) => {
            println!("downloaded dependencies to {}", cache_root.display());
            0
        }
        Ok(vo_module::ops::LockFileStatus::NotRequired) => {
            println!("module has no external dependencies; nothing to download");
            0
        }
        Err(error) => {
            eprintln!("[VO:MOD:DOWNLOAD] {}", error);
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
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_sync(&project_root, &cache_root, &registry, "vo mod sync") {
        Ok(vo_module::ops::LockFileStatus::Present) => {
            println!("synced {}", project_root.join("vo.lock").display());
            print_lock_summary(&project_root);
            0
        }
        Ok(vo_module::ops::LockFileStatus::NotRequired) => {
            println!(
                "synchronized module graph; vo.lock is unnecessary without external requirements"
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
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_verify(&project_root, &cache_root) {
        Ok(vo_module::ops::LockFileStatus::Present) => {
            println!("verified {}", project_root.join("vo.lock").display());
            print_lock_summary(&project_root);
            0
        }
        Ok(vo_module::ops::LockFileStatus::NotRequired) => {
            println!("verified module graph; vo.lock is unnecessary without external requirements");
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
    let cache_root = vo_engine::default_mod_cache_root();
    match vo_module::ops::mod_remove(
        &project_root,
        module_path,
        &cache_root,
        &registry,
        "vo mod remove",
    ) {
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

fn cmd_mod_why(args: &[OsString]) -> i32 {
    if args.len() != 1 {
        eprintln!("usage: vo mod why <module-path>");
        return 1;
    }
    if starts_with_dash(&args[0]) {
        report_unknown_option("mod why", &args[0]);
        return 1;
    }
    let module_path = match utf8_arg(&args[0], "module path") {
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
    match vo_module::ops::mod_why(&project_root, module_path) {
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

fn cmd_mod_clean(args: &[OsString]) -> i32 {
    if args.len() > 1 {
        eprintln!("usage: vo mod clean [--all]");
        return 1;
    }
    if let Some(argument) = args.first() {
        let option = match utf8_arg(argument, "mod clean option name") {
            Ok(option) => option,
            Err(error) => {
                eprintln!("[VO:MOD:CLEAN] {error}");
                return 1;
            }
        };
        if option != "--all" {
            eprintln!("unknown mod clean option: {option}");
            eprintln!("usage: vo mod clean [--all]");
            return 1;
        }
    }
    let keep_locked = args.is_empty();
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
        "  verify [path]              Verify release policy and vo.lock freshness for a module repo"
    );
    println!(
        "  stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]"
    );
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
            println!("release ready {}", project_root.display());
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

const RELEASE_STAGE_USAGE: &str = "usage: vo release stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]";

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

    let mut version: Option<String> = None;
    let mut commit: Option<String> = None;
    let mut out_dir: Option<PathBuf> = None;
    let mut artifacts: Vec<ArtifactInput> = Vec::new();

    while index < args.len() {
        let option = utf8_arg(&args[index], "release stage option name")?;
        match option {
            "--version" => {
                let value = args
                    .get(index + 1)
                    .ok_or_else(|| "`--version` requires a release version".to_string())?;
                version = Some(utf8_arg(value, "release version")?.to_string());
                index += 2;
            }
            "--commit" => {
                let value = args
                    .get(index + 1)
                    .ok_or_else(|| "`--commit` requires a commit identifier".to_string())?;
                commit = Some(utf8_arg(value, "release commit")?.to_string());
                index += 2;
            }
            "--out-dir" => {
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

    let version = version.ok_or_else(|| "release stage requires `--version`".to_string())?;
    let out_dir = out_dir.ok_or_else(|| "release stage requires `--out-dir`".to_string())?;
    Ok(StageCliRequest::Run {
        project_path,
        options: StageReleaseOptions {
            version,
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

fn cmd_get(args: &[OsString]) -> i32 {
    let _ = args;
    eprintln!("[VO:GET] `vo get` has been removed");
    eprintln!("[VO:GET] use `vo mod add <module[@constraint]>` to change direct dependencies");
    eprintln!(
        "[VO:GET] use `vo mod sync` to refresh vo.lock and `vo mod download` to fill the cache"
    );
    eprintln!("[VO:GET] dependency lifecycle now lives under `vo mod ...`");
    1
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
        Err(vo_module::Error::Io(error)) if error.kind() == std::io::ErrorKind::NotFound => {
            println!("vo.lock omitted because the module has no external requirements");
        }
        Err(e) => {
            eprintln!("warning: could not read vo.lock: {}", e);
        }
    }
}

fn module_root_from_path(path: impl AsRef<Path>) -> Result<PathBuf, String> {
    let path = path.as_ref();
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

        let resolved = module_root_from_path(nested.join("main.vo")).unwrap();
        assert_eq!(resolved, root.canonicalize().unwrap());

        fs::remove_dir_all(&root).unwrap();
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
                OsString::from("--version"),
                OsString::from("v1.2.3"),
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
                    OsString::from("--version"),
                    invalid.clone(),
                    OsString::from("--out-dir"),
                    OsString::from("dist"),
                ],
                Path::new("."),
            ),
            Err("release version must be valid UTF-8".to_string())
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
                "module github.com/acme/non-utf8-cli\n\nvo {}\n",
                current_toolchain_constraint()
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

        for subcommand in ["sync", "verify", "download", "tidy"] {
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
    fn init_constraint_tracks_the_cli_toolchain_version() {
        assert_eq!(
            current_toolchain_constraint(),
            format!("^{}", env!("CARGO_PKG_VERSION"))
        );

        let root = unique_temp_dir("init-toolchain-version");
        fs::create_dir_all(&root).unwrap();
        vo_module::ops::mod_init(
            &root,
            "github.com/acme/current-version",
            current_toolchain_constraint(),
        )
        .unwrap();
        let mod_file = fs::read_to_string(root.join("vo.mod")).unwrap();
        assert!(
            mod_file.contains(&format!("vo ^{}", env!("CARGO_PKG_VERSION"))),
            "{mod_file}"
        );
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn module_cli_accepts_the_canonical_lockless_empty_graph() {
        let root = unique_temp_dir("module-lockless");
        fs::create_dir_all(&root).unwrap();
        fs::write(
            root.join("vo.mod"),
            format!(
                "module github.com/acme/empty\n\nvo {}\n",
                current_toolchain_constraint()
            ),
        )
        .unwrap();
        let path = root.clone().into_os_string();

        assert_eq!(cmd_mod_sync(std::slice::from_ref(&path)), 0);
        assert!(!root.join("vo.lock").exists());
        assert_eq!(cmd_mod_verify(std::slice::from_ref(&path)), 0);
        assert_eq!(cmd_mod_download(std::slice::from_ref(&path)), 0);
        assert!(!root.join("vo.lock").exists());

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
        assert_eq!(cmd_mod(&os_strings(&["download", "--help"])), 0);
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
        assert_eq!(cmd_mod(&os_strings(&["download", "one", "two"])), 1);
        assert_eq!(cmd_mod(&os_strings(&["tidy", "one", "two"])), 1);
        assert_eq!(cmd_mod(&os_strings(&["sync", "--unknown"])), 1);
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
