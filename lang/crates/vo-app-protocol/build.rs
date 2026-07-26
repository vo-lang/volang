use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    let manifest = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").expect("manifest directory"));
    let schema_path = manifest.join("../../protocol/app-runtime/app.schema.toml");
    println!("cargo:rerun-if-changed={}", schema_path.display());
    let text = fs::read_to_string(&schema_path).expect("read App Runtime schema");
    let schema = vo_schema_compiler::compile_app_schema(&text).expect("compile App Runtime schema");
    let output = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR")).join("generated.rs");
    fs::write(output, schema.render_rust()).expect("write generated App Runtime Rust contract");
}
