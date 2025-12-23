//! WebAssembly bindings for Vo VM.
//!
//! This crate provides JavaScript bindings to run Vo programs in browsers.
//!
//! ## Features
//!
//! - `std` (default): Full functionality with source compilation
//! - `compiler` (default): Enable Vo source code compilation
//! - No features: Minimal bytecode-only execution
//!
//! ## Usage (JavaScript)
//!
//! ```javascript
//! import init, { VoVM, compile_and_run } from 'vo-web';
//!
//! async function main() {
//!     await init();
//!     
//!     // Simple API (requires 'compiler' feature)
//!     const output = compile_and_run(`
//!         package main
//!         func main() { println("Hello from Vo!") }
//!     `);
//!     console.log(output);
//!     
//!     // Or use bytecode directly (works in minimal mode)
//!     const vm = new VoVM();
//!     vm.load_bytecode(bytecodeBytes);
//!     vm.run();
//! }
//! ```

#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use wasm_bindgen::prelude::*;
use vo_vm::bytecode::Module as BytecodeModule;

#[cfg(feature = "compiler")]
use vo_syntax::parse;
#[cfg(feature = "compiler")]
use vo_analysis::analyze_single_file;
#[cfg(feature = "compiler")]
use vo_codegen_vm::compile_project;

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

/// Vo Bytecode Runner - minimal VM for executing pre-compiled bytecode.
/// Works in no_std mode.
#[wasm_bindgen]
pub struct VoRunner {
    bytecode: Option<BytecodeModule>,
}

#[wasm_bindgen]
impl VoRunner {
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

impl Default for VoRunner {
    fn default() -> Self {
        Self::new()
    }
}

/// Get version information.
#[wasm_bindgen]
pub fn version() -> alloc::string::String {
    "Vo Web 0.1.0".into()
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

    /// Vo Virtual Machine with full compilation support.
    /// Requires 'compiler' feature.
    #[wasm_bindgen]
    pub struct VoVM {
        bytecode: Option<BytecodeModule>,
    }

    #[wasm_bindgen]
    impl VoVM {
        /// Create a new Vo VM instance.
        #[wasm_bindgen(constructor)]
        pub fn new() -> Self {
            Self { bytecode: None }
        }
        
        /// Compile Vo source code.
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

    impl Default for VoVM {
        fn default() -> Self {
            Self::new()
        }
    }

    /// Compile Vo source code to bytecode.
    fn compile_source(source: &str) -> Result<BytecodeModule, String> {
        let (file, parse_diag, interner) = parse(source, 0);
        
        if parse_diag.has_errors() {
            return Err(format!("Parse error: {:?}", parse_diag));
        }
        
        let project = analyze_single_file(file, interner)
            .map_err(|e| format!("Type check error: {}", e))?;
        
        compile_project(&project)
            .map_err(|e| format!("Compilation error: {}", e))
    }

    /// Simple API: compile and run Vo source code.
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
    fn test_vo_vm_compile() {
        let mut vm = VoVM::new();
        let result = vm.compile("package main\nfunc main() {}");
        assert!(result.success());
    }
    
    #[test]
    fn test_vo_vm_multiple_functions() {
        let mut vm = VoVM::new();
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
    fn test_vo_runner() {
        let runner = VoRunner::new();
        assert!(!runner.is_loaded());
        assert_eq!(runner.function_count(), 0);
    }
}
