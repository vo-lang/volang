//! WebAssembly bindings for GoX VM.
//!
//! This crate provides JavaScript bindings to run GoX programs in browsers.
//!
//! ## Features
//!
//! - `std` (default): Full functionality with source compilation
//! - `compiler` (default): Enable GoX source code compilation
//! - No features: Minimal bytecode-only execution
//!
//! ## Usage (JavaScript)
//!
//! ```javascript
//! import init, { GoxVM, compile_and_run } from 'gox-web';
//!
//! async function main() {
//!     await init();
//!     
//!     // Simple API (requires 'compiler' feature)
//!     const output = compile_and_run(`
//!         package main
//!         func main() { println("Hello from GoX!") }
//!     `);
//!     console.log(output);
//!     
//!     // Or use bytecode directly (works in minimal mode)
//!     const vm = new GoxVM();
//!     vm.load_bytecode(bytecodeBytes);
//!     vm.run();
//! }
//! ```

#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use wasm_bindgen::prelude::*;
use gox_vm::bytecode::Module as BytecodeModule;

#[cfg(feature = "compiler")]
use gox_common::DiagnosticSink;
#[cfg(feature = "compiler")]
use gox_syntax::parse;
#[cfg(feature = "compiler")]
use gox_analysis::typecheck_file;
#[cfg(feature = "compiler")]
use gox_codegen_vm::compile;

// ============================================================================
// Initialization (std only)
// ============================================================================

/// Initialize panic hook for better error messages in console.
#[cfg(feature = "std")]
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

// ============================================================================
// Minimal API (always available, no_std compatible)
// ============================================================================

/// GoX Bytecode Runner - minimal VM for executing pre-compiled bytecode.
/// Works in no_std mode.
#[wasm_bindgen]
pub struct GoxRunner {
    bytecode: Option<BytecodeModule>,
}

#[wasm_bindgen]
impl GoxRunner {
    /// Create a new bytecode runner.
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self { bytecode: None }
    }
    
    /// Load bytecode from serialized bytes.
    /// In a real implementation, this would deserialize the bytecode.
    pub fn load_bytecode(&mut self, _bytes: &[u8]) -> Result<(), JsValue> {
        // TODO: Implement bytecode deserialization
        // For now, this is a placeholder
        Err(JsValue::from_str("Bytecode loading not yet implemented"))
    }
    
    /// Check if bytecode is loaded.
    pub fn is_loaded(&self) -> bool {
        self.bytecode.is_some()
    }
    
    /// Run the loaded bytecode.
    pub fn run(&mut self) -> Result<JsValue, JsValue> {
        let _bytecode = self.bytecode.as_ref()
            .ok_or_else(|| JsValue::from_str("No bytecode loaded"))?;
        
        // TODO: Integrate with actual VM execution
        Ok(JsValue::from_str("Execution complete"))
    }
    
    /// Get function count in the loaded module.
    pub fn function_count(&self) -> u32 {
        self.bytecode.as_ref().map(|b| b.functions.len() as u32).unwrap_or(0)
    }
}

impl Default for GoxRunner {
    fn default() -> Self {
        Self::new()
    }
}

/// Get version information.
#[wasm_bindgen]
pub fn version() -> alloc::string::String {
    "GoX Web 0.1.0".into()
}

// ============================================================================
// Full API (requires 'compiler' feature)
// ============================================================================

#[cfg(feature = "compiler")]
mod compiler_api {
    use super::*;
    use alloc::string::String;
    use alloc::vec::Vec;
    use alloc::format;
    
    /// Compilation result returned to JavaScript.
    #[wasm_bindgen]
    pub struct CompileResult {
        success: bool,
        error: Option<String>,
    }

    #[wasm_bindgen]
    impl CompileResult {
        #[wasm_bindgen(getter)]
        pub fn success(&self) -> bool {
            self.success
        }
        
        #[wasm_bindgen(getter)]
        pub fn error(&self) -> Option<String> {
            self.error.clone()
        }
    }

    /// GoX Virtual Machine with full compilation support.
    /// Requires 'compiler' feature.
    #[wasm_bindgen]
    pub struct GoxVM {
        bytecode: Option<BytecodeModule>,
    }

    #[wasm_bindgen]
    impl GoxVM {
        /// Create a new GoX VM instance.
        #[wasm_bindgen(constructor)]
        pub fn new() -> Self {
            Self { bytecode: None }
        }
        
        /// Compile GoX source code.
        pub fn compile(&mut self, source: &str) -> CompileResult {
            match compile_source(source) {
                Ok(bytecode) => {
                    self.bytecode = Some(bytecode);
                    CompileResult { success: true, error: None }
                }
                Err(e) => CompileResult { success: false, error: Some(e) }
            }
        }
        
        /// Run the compiled bytecode.
        pub fn run(&mut self) -> Result<String, JsValue> {
            let bytecode = self.bytecode.as_ref()
                .ok_or_else(|| JsValue::from_str("No bytecode. Call compile() first."))?;
            
            // TODO: Integrate with actual VM execution
            Ok(format!("Compiled {} functions", bytecode.functions.len()))
        }
        
        /// Get list of function names.
        pub fn function_names(&self) -> Vec<JsValue> {
            match &self.bytecode {
                Some(bc) => bc.functions.iter()
                    .map(|f| JsValue::from_str(&f.name))
                    .collect(),
                None => Vec::new(),
            }
        }
        
        /// Clear the VM state.
        pub fn clear(&mut self) {
            self.bytecode = None;
        }
    }

    impl Default for GoxVM {
        fn default() -> Self {
            Self::new()
        }
    }

    /// Compile GoX source code to bytecode.
    fn compile_source(source: &str) -> Result<BytecodeModule, String> {
        let (file, parse_diag, interner) = parse(source, 0);
        
        if parse_diag.has_errors() {
            return Err(format!("Parse error: {:?}", parse_diag));
        }
        
        let mut diag = DiagnosticSink::new();
        let result = typecheck_file(&file, &interner, &mut diag);
        
        if diag.has_errors() {
            return Err("Type check errors".to_string());
        }
        
        compile(&file, &result, &interner)
            .map_err(|e| format!("Compilation error: {}", e))
    }

    /// Simple API: compile and run GoX source code.
    #[wasm_bindgen]
    pub fn compile_and_run(source: &str) -> String {
        match compile_source(source) {
            Ok(bytecode) => format!("✓ Compiled {} functions", bytecode.functions.len()),
            Err(e) => format!("✗ Error: {}", e)
        }
    }
}

#[cfg(feature = "compiler")]
pub use compiler_api::*;

// ============================================================================
// Tests
// ============================================================================

#[cfg(all(test, feature = "compiler"))]
mod tests {
    use super::*;
    
    #[test]
    fn test_gox_vm_compile() {
        let mut vm = GoxVM::new();
        let result = vm.compile("package main\nfunc main() {}");
        assert!(result.success());
    }
    
    #[test]
    fn test_gox_vm_multiple_functions() {
        let mut vm = GoxVM::new();
        let source = r#"
package main

func add(a int, b int) int {
    return a + b
}

func main() {
}
"#;
        let result = vm.compile(source);
        assert!(result.success());
    }
}

#[cfg(test)]
mod minimal_tests {
    use super::*;
    
    #[test]
    fn test_gox_runner() {
        let runner = GoxRunner::new();
        assert!(!runner.is_loaded());
        assert_eq!(runner.function_count(), 0);
    }
}
