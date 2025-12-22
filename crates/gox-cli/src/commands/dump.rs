//! `gox dump` command - Dump a bytecode file to text format.

use gox_vm::Module;
use crate::bytecode_text;

/// Dump a bytecode file to text format.
///
/// # Arguments
/// * `file` - Path to bytecode binary file (.goxb) or text file (.goxt)
///
/// # Examples
/// ```text
/// gox dump program.goxb
/// gox dump program.goxt
/// ```
pub fn run(file: &str) -> Result<(), Box<dyn std::error::Error>> {
    let module = if file.ends_with(".goxb") {
        let bytes = std::fs::read(file)?;
        Module::from_bytes(&bytes)?
    } else {
        let content = std::fs::read_to_string(file)?;
        bytecode_text::parse_text(&content)?
    };
    
    let text = bytecode_text::format_text(&module);
    print!("{}", text);
    Ok(())
}
