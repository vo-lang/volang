//! fmt package native functions.
//!
//! Uses zero-copy native API for efficient string handling.

use gox_vm::{ExternCtx, ExternRegistry, ExternResult};

/// Register fmt functions.
pub fn register(registry: &mut ExternRegistry) {
    registry.register("fmt.Println", native_println);
    registry.register("fmt.Print", native_print);
    registry.register("fmt.Sprint", native_sprint);
    registry.register("fmt.Sprintln", native_sprintln);
}

/// fmt.Println(args...) - print with newline, returns byte count.
fn native_println(ctx: &mut ExternCtx) -> ExternResult {
    let output = ctx.format_all();
    println!("{}", output);
    ctx.ret_i64(0, (output.len() + 1) as i64); // +1 for newline
    ExternResult::Ok(1)
}

/// fmt.Print(args...) - print without newline, returns byte count.
fn native_print(ctx: &mut ExternCtx) -> ExternResult {
    let output = ctx.format_all();
    print!("{}", output);
    ctx.ret_i64(0, output.len() as i64);
    ExternResult::Ok(1)
}

/// fmt.Sprint(args...) - format to string.
fn native_sprint(ctx: &mut ExternCtx) -> ExternResult {
    let output = ctx.format_all();
    ctx.ret_string(0, &output);
    ExternResult::Ok(1)
}

/// fmt.Sprintln(args...) - format to string with newline.
fn native_sprintln(ctx: &mut ExternCtx) -> ExternResult {
    let mut output = ctx.format_all();
    output.push('\n');
    ctx.ret_string(0, &output);
    ExternResult::Ok(1)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_register() {
        let mut registry = ExternRegistry::new();
        register(&mut registry);
        
        assert!(registry.get("fmt.Println").is_some());
        assert!(registry.get("fmt.Print").is_some());
        assert!(registry.get("fmt.Sprint").is_some());
        assert!(registry.get("fmt.Sprintln").is_some());
    }
}
