//! GoX compiler CLI.
//!
//! A complete toolchain for the GoX programming language.
//!
//! # Commands
//!
//! - `gox init <module-path>` - Initialize a new module
//! - `gox get <module>@<version>` - Download a dependency
//! - `gox build [path]` - Build a project
//! - `gox check` - Type-check without building
//! - `gox run <file>` - Run a program
//! - `gox dump <file>` - Dump bytecode to text
//! - `gox compile <file>` - Compile bytecode text to binary
//!
//! # Examples
//!
//! ```text
//! gox init github.com/user/myapp
//! gox run hello.gox
//! gox run hello.gox --mode=jit
//! gox run hello.gox --ast
//! gox run myproject/
//! ```

use std::process;

use clap::{Parser, Subcommand, Args};
use gox_runtime_vm::extern_fn::StdMode;

mod bytecode_tests;
mod commands;
mod printer;

pub mod bytecode_text;

pub use printer::AstPrinter;
pub use commands::run::RunMode;

#[derive(Parser)]
#[command(name = "gox")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "GoX compiler and runtime", long_about = None)]
#[command(after_help = "Use 'gox <command> --help' for more information about a command.")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new GoX module
    ///
    /// Creates a gox.mod file in the current directory with the specified
    /// module path. The module path should follow Go conventions
    /// (e.g., github.com/user/project).
    #[command(after_help = "Example:\n  gox init github.com/user/myapp")]
    Init {
        /// Module path (e.g., github.com/user/project)
        module_path: String,
    },

    /// Download a dependency and add it to gox.mod
    ///
    /// Downloads the specified module at the given version and adds it
    /// to the require section of gox.mod.
    #[command(after_help = "Example:\n  gox get github.com/foo/bar@v1.2.3")]
    Get {
        /// Module and version (e.g., github.com/foo/bar@v1.2.3)
        module_version: String,
    },

    /// Build a GoX project
    ///
    /// Compiles all .gox files in the project directory and runs the result.
    #[command(after_help = "Examples:\n  gox build\n  gox build ./myproject\n  gox build --std=core")]
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

    /// Run a GoX program
    ///
    /// Executes a .gox source file, .goxt bytecode text, .goxb bytecode binary,
    /// or a project directory.
    #[command(after_help = "Examples:\n  gox run hello.gox\n  gox run hello.gox --mode=jit\n  gox run hello.gox --ast\n  gox run hello.gox --codegen\n  gox run myproject/")]
    Run(RunArgs),

    /// Dump a bytecode file to text format
    ///
    /// Converts a bytecode binary (.goxb) or text (.goxt) file to readable text.
    #[command(after_help = "Example:\n  gox dump program.goxb")]
    Dump {
        /// Path to bytecode file (.goxb or .goxt)
        file: String,
    },

    /// Compile bytecode text to binary
    ///
    /// Converts a bytecode text file (.goxt) to binary format (.goxb).
    #[command(after_help = "Examples:\n  gox compile program.goxt\n  gox compile program.goxt -o output.goxb")]
    Compile {
        /// Path to bytecode text file (.goxt)
        file: String,

        /// Output path (default: same name with .goxb extension)
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
    /// Path to .gox source, .goxt/.goxb bytecode, or project directory
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

    let result = match cli.command {
        Commands::Init { module_path } => commands::init::run(&module_path),
        Commands::Get { module_version } => commands::get::run(&module_version),
        Commands::Build { path, std } => commands::build::run(&path, std),
        Commands::Check => commands::check::run(),
        Commands::Run(args) => commands::run::run(
            &args.file,
            args.mode,
            args.std,
            args.ast,
            args.codegen,
        ),
        Commands::Dump { file } => commands::dump::run(&file),
        Commands::Compile { file, output } => commands::compile::run(&file, output),
        Commands::RunBytecode { test } => bytecode_tests::run_test(&test),
    };

    if let Err(e) = result {
        eprintln!("error: {}", e);
        process::exit(1);
    }
}
