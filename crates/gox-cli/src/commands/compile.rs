//! `gox compile` command - Compile bytecode text to binary.

use std::path::Path;
use crate::bytecode_text;

/// Compile bytecode text to binary.
///
/// # Arguments
/// * `file` - Path to bytecode text file (.goxt)
/// * `output` - Output path (default: same name with .goxb extension)
///
/// # Examples
/// ```text
/// gox compile program.goxt
/// gox compile program.goxt -o output.goxb
/// ```
pub fn run(file: &str, output: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let content = std::fs::read_to_string(file)?;
    let module = bytecode_text::parse_text(&content)?;
    
    let out_path = output.unwrap_or_else(|| {
        let p = Path::new(file);
        p.with_extension("goxb").to_string_lossy().to_string()
    });
    
    let bytes = module.to_bytes();
    std::fs::write(&out_path, &bytes)?;
    
    println!("Compiled {} -> {} ({} bytes)", file, out_path, bytes.len());
    Ok(())
}
