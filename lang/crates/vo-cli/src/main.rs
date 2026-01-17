//! Vo compiler CLI.
//!
//! A complete toolchain for the Vo programming language.
//!
//! # Commands
//!
//! - `vo init <module-path>` - Initialize a new module
//! - `vo get <module>@<version>` - Download a dependency
//! - `vo build [path]` - Build a project
//! - `vo check` - Type-check without building
//! - `vo run <file>` - Run a program
//! - `vo dump <file>` - Dump bytecode to text
//! - `vo compile <file>` - Compile bytecode text to binary
//!
//! # Examples
//!
//! ```text
//! vo init github.com/user/myapp
//! vo run hello.vo
//! vo run hello.vo --mode=jit
//! vo run hello.vo --ast
//! vo run myproject/
//! ```

use std::process;

use clap::{Parser, Subcommand, Args};
use commands::run::StdMode;

mod bytecode_tests;
mod output;
mod commands;
mod printer;
pub mod stdlib;

pub mod bytecode_text;

pub use printer::AstPrinter;
pub use commands::run::RunMode;
pub use vo_vm::bytecode::Module;

#[derive(Parser)]
#[command(name = "vo")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "Vo compiler and runtime", long_about = None)]
#[command(after_help = "Use 'vo <command> --help' for more information about a command.")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new Vo module
    ///
    /// Creates a vo.mod file in the current directory with the specified
    /// module path. The module path should follow Go conventions
    /// (e.g., github.com/user/project).
    #[command(after_help = "Example:\n  vo init github.com/user/myapp")]
    Init {
        /// Module path (e.g., github.com/user/project)
        module_path: String,
    },

    /// Download a dependency and add it to vo.mod
    ///
    /// Downloads the specified module at the given version and adds it
    /// to the require section of vo.mod.
    #[command(after_help = "Example:\n  vo get github.com/foo/bar@v1.2.3")]
    Get {
        /// Module and version (e.g., github.com/foo/bar@v1.2.3)
        module_version: String,
    },

    /// Build a Vo project
    ///
    /// Compiles all .vo files in the project directory and runs the result.
    #[command(after_help = "Examples:\n  vo build\n  vo build ./myproject\n  vo build --std=core")]
    Build {
        /// Path to project directory
        #[arg(default_value = ".")]
        path: String,

        /// Standard library mode
        #[arg(long, default_value = "full", value_parser = parse_std_mode)]
        #[arg(help = "Stdlib mode: 'core' (no OS deps) or 'full' (default)")]
        std: StdMode,
    },

    /// Type-check the current module without building
    ///
    /// Validates the module's type correctness without generating code.
    Check,

    /// Run a Vo program
    ///
    /// Executes a .vo source file, .vot bytecode text, .vob bytecode binary,
    /// or a project directory.
    #[command(after_help = "Examples:\n  vo run hello.vo\n  vo run hello.vo --mode=jit\n  vo run hello.vo --ast\n  vo run hello.vo --codegen\n  vo run myproject/")]
    Run(RunArgs),

    /// Dump a bytecode file to text format
    ///
    /// Converts a bytecode binary (.vob) or text (.vot) file to readable text.
    #[command(after_help = "Example:\n  vo dump program.vob")]
    Dump {
        /// Path to bytecode file (.vob or .vot)
        file: String,
    },

    /// Compile bytecode text to binary
    ///
    /// Converts a bytecode text file (.vot) to binary format (.vob).
    #[command(after_help = "Examples:\n  vo compile program.vot\n  vo compile program.vot -o output.vob")]
    Compile {
        /// Path to bytecode text file (.vot)
        file: String,

        /// Output path (default: same name with .vob extension)
        #[arg(short, long)]
        output: Option<String>,
    },

    /// Run internal bytecode tests for VM verification
    #[command(hide = true)]
    RunBytecode {
        /// Test name: arithmetic, factorial, ffi, channel, all
        #[arg(short, long, default_value = "all")]
        test: String,
    },
}

#[derive(Args)]
struct RunArgs {
    /// Path to .vo source, .vot/.vob bytecode, or project directory
    file: String,

    /// Execution mode
    #[arg(long, default_value = "vm", value_parser = parse_run_mode)]
    #[arg(help = "Execution mode: 'vm' (interpreter) or 'jit' (compiled)")]
    mode: RunMode,

    /// Standard library mode
    #[arg(long, default_value = "full", value_parser = parse_std_mode)]
    #[arg(help = "Stdlib mode: 'core' (no OS deps) or 'full' (default)")]
    std: StdMode,

    /// Print AST and exit (do not run)
    #[arg(long)]
    #[arg(help = "Parse source and print AST, then exit")]
    ast: bool,

    /// Print bytecode and exit (do not run)
    #[arg(long)]
    #[arg(help = "Compile source and print bytecode, then exit")]
    codegen: bool,
}

fn parse_std_mode(s: &str) -> Result<StdMode, String> {
    match s.to_lowercase().as_str() {
        "core" => Ok(StdMode::Core),
        "full" => Ok(StdMode::Full),
        _ => Err(format!("invalid std mode '{}', expected 'core' or 'full'", s)),
    }
}

fn parse_run_mode(s: &str) -> Result<RunMode, String> {
    s.parse()
}

fn main() {
    let cli = Cli::parse();

    // Commands return bool (true = success, error already reported)
    // or Result (error needs to be printed by main)
    let success = match cli.command {
        Commands::Init { module_path } => commands::init::run(&module_path).is_ok(),
        Commands::Get { module_version } => commands::get::run(&module_version).is_ok(),
        Commands::Build { path, std } => commands::build::run(&path, std),
        Commands::Check => commands::check::run().is_ok(),
        Commands::Run(args) => commands::run::run(
            &args.file,
            args.mode,
            args.std,
            args.ast,
            args.codegen,
        ),
        Commands::Dump { file } => commands::dump::run(&file).is_ok(),
        Commands::Compile { file, output } => commands::compile::run(&file, output).is_ok(),
        Commands::RunBytecode { test } => bytecode_tests::run_test(&test).is_ok(),
    };

    if !success {
        process::exit(1);
    }
}
