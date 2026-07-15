//! JS interop types and helpers for WASM bindings.

use wasm_bindgen::prelude::*;

/// Compilation result returned to JavaScript.
#[wasm_bindgen]
pub struct CompileResult {
    pub(crate) success: bool,
    pub(crate) bytecode: Option<Vec<u8>>,
    pub(crate) error_message: Option<String>,
    pub(crate) error_line: Option<u32>,
    pub(crate) error_column: Option<u32>,
}

#[wasm_bindgen]
impl CompileResult {
    #[wasm_bindgen(getter)]
    pub fn success(&self) -> bool {
        self.success
    }

    #[wasm_bindgen(getter)]
    pub fn bytecode(&self) -> Option<Vec<u8>> {
        self.bytecode.clone()
    }

    #[wasm_bindgen(getter, js_name = "errorMessage")]
    pub fn error_message(&self) -> Option<String> {
        self.error_message.clone()
    }

    #[wasm_bindgen(getter, js_name = "errorLine")]
    pub fn error_line(&self) -> Option<u32> {
        self.error_line
    }

    #[wasm_bindgen(getter, js_name = "errorColumn")]
    pub fn error_column(&self) -> Option<u32> {
        self.error_column
    }
}

/// Run result returned to JavaScript.
#[wasm_bindgen]
pub struct RunResult {
    pub(crate) status: String,
    pub(crate) stdout: String,
    pub(crate) stderr: String,
    pub(crate) exit_code: Option<i32>,
}

#[wasm_bindgen]
impl RunResult {
    #[wasm_bindgen(getter)]
    pub fn status(&self) -> String {
        self.status.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn stdout(&self) -> String {
        self.stdout.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn stderr(&self) -> String {
        self.stderr.clone()
    }

    /// Process exit status supplied by `os.Exit`, or `undefined` when execution
    /// completed without an explicit process exit.
    #[wasm_bindgen(getter, js_name = "exitCode")]
    pub fn exit_code(&self) -> Option<i32> {
        self.exit_code
    }
}

#[cfg(feature = "compiler")]
pub fn make_run_result_obj(
    status: &str,
    stdout: &str,
    stderr: &str,
    exit_code: Option<i32>,
) -> JsValue {
    let obj = js_sys::Object::new();
    js_sys::Reflect::set(
        &obj,
        &JsValue::from_str("status"),
        &JsValue::from_str(status),
    )
    .unwrap();
    js_sys::Reflect::set(
        &obj,
        &JsValue::from_str("stdout"),
        &JsValue::from_str(stdout),
    )
    .unwrap();
    js_sys::Reflect::set(
        &obj,
        &JsValue::from_str("stderr"),
        &JsValue::from_str(stderr),
    )
    .unwrap();
    let exit_code = exit_code
        .map(|code| JsValue::from_f64(f64::from(code)))
        .unwrap_or(JsValue::UNDEFINED);
    js_sys::Reflect::set(&obj, &JsValue::from_str("exitCode"), &exit_code).unwrap();
    obj.into()
}
