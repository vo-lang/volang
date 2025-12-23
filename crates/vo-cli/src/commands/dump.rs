//! `vo dump` command - Dump a bytecode file to text format.

use vo_vm::Module;
use crate::bytecode_text;

/// Dump a bytecode file to text format.
///
/// # Arguments
/// * `file` - Path to bytecode binary file (.vob) or text file (.vot)
///
/// # Examples
/// ```text
/// vo dump program.vob
/// vo dump program.vot
/// ```
pub fn run(file: &str) -> Result<(), Box<dyn std::error::Error>> {
    let module = if file.ends_with(".vob") {
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
