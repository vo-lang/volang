//! Vo CLI launcher - compiles and runs the Vo CLI (cmd/vo).

use std::env;
use std::path::PathBuf;
use std::process;

fn main() {
    // Force link vo-vox FFI functions
    vo_vox::ensure_linked();
    
    let args: Vec<String> = env::args().collect();
    
    // Parse launcher arguments (before cmd/vo arguments)
    let mut cmd_vo_path: Option<PathBuf> = None;
    let mut pass_args: Vec<String> = Vec::new();
    
    let mut i = 1;
    while i < args.len() {
        let arg = &args[i];
        
        if arg == "--cache" {
            // --cache <path>: use specified path as cmd/vo directory
            if i + 1 < args.len() {
                cmd_vo_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
                continue;
            }
        } else if arg.starts_with("--cache=") {
            cmd_vo_path = Some(PathBuf::from(arg.strip_prefix("--cache=").unwrap()));
            i += 1;
            continue;
        }
        
        // All other arguments go to cmd/vo
        pass_args.push(arg.clone());
        i += 1;
    }
    
    // Find cmd/vo directory
    let cmd_vo_path = cmd_vo_path.unwrap_or_else(find_cmd_vo);
    
    // Prepend program name (os.Args[0])
    let mut full_args = vec!["vo".to_string()];
    full_args.extend(pass_args);
    
    // Set environment variable so cmd/vo knows it's being run by the launcher
    env::set_var("VO_CLI_ROOT", &cmd_vo_path);
    
    // Compile cmd/vo with cache
    let output = match vo_engine::compile_with_cache(cmd_vo_path.to_str().unwrap()) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("Failed to compile vo CLI: {}", e);
            process::exit(1);
        }
    };
    
    // Always run cmd/vo in VM mode - it's the CLI, not the code being tested.
    // The --mode flag passed to cmd/vo controls how user code runs.
    let mode = vo_engine::RunMode::Vm;
    
    // Run
    if let Err(e) = vo_engine::run(output, mode, full_args) {
        eprintln!("{}", e);
        process::exit(1);
    }
}

fn find_cmd_vo() -> PathBuf {
    // Try relative to executable
    if let Ok(exe_path) = env::current_exe() {
        // Development: executable in target/debug or target/release
        // cmd/vo is relative to repo root
        let mut path = exe_path.clone();
        for _ in 0..4 {
            path.pop();
            let cmd_vo = path.join("cmd/vo");
            if cmd_vo.exists() && cmd_vo.join("main.vo").exists() {
                return cmd_vo;
            }
        }
    }
    
    // Try current directory
    let cwd_cmd_vo = PathBuf::from("cmd/vo");
    if cwd_cmd_vo.exists() && cwd_cmd_vo.join("main.vo").exists() {
        return cwd_cmd_vo;
    }
    
    eprintln!("Could not find cmd/vo directory");
    process::exit(1);
}
