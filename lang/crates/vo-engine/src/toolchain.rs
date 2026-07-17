use std::fs;
use std::path::Path;
use std::sync::Arc;

use vo_runtime::output::CaptureSink;
use vo_stdlib::toolchain::{
    install_toolchain_host, is_toolchain_host_installed, ToolchainHost, ToolchainModule,
    ToolchainRunMode,
};
use vo_syntax::parser;

use crate::{compile_string, format_source, format_text, run, run_with_output, Module, RunMode};

struct EngineToolchainHost;

fn run_mode(mode: ToolchainRunMode) -> RunMode {
    match mode {
        ToolchainRunMode::Vm => RunMode::Vm,
        ToolchainRunMode::Jit => RunMode::Jit,
    }
}

fn parse_source(source: &str) -> Result<String, String> {
    let (file, diags, _) = parser::parse(source, 0);
    if diags.has_errors() {
        return Err(diags
            .iter()
            .map(|diag| diag.message.as_str())
            .collect::<Vec<_>>()
            .join("; "));
    }
    Ok(format!("{:#?}", file))
}

fn format_source_impl(source: &str) -> Result<String, String> {
    format_source(source)
}

fn init_project_impl(dir_path: &Path, mod_name: &str) -> Result<String, String> {
    // Validate before creating the directory, through the same authority that
    // will render the manifest below.
    vo_module::ops::initial_mod_file(mod_name).map_err(|error| error.to_string())?;

    let created_dir = !dir_path.exists();
    if created_dir {
        fs::create_dir_all(dir_path).map_err(|e| e.to_string())?;
    }

    let mut created: Vec<&str> = Vec::new();
    let mod_file = dir_path.join("vo.mod");
    let created_mod = !mod_file.exists();
    if created_mod {
        if let Err(error) = vo_module::ops::mod_init(dir_path, mod_name) {
            if created_dir {
                let _ = fs::remove_dir(dir_path);
            }
            return Err(error.to_string());
        }
        created.push("vo.mod");
    }

    let main_file = dir_path.join("main.vo");
    if !main_file.exists() {
        if let Err(error) = fs::write(
            &main_file,
            "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, Vo!\")\n}\n",
        ) {
            if created_mod {
                let _ = fs::remove_file(&mod_file);
            }
            if created_dir {
                let _ = fs::remove_dir(dir_path);
            }
            return Err(error.to_string());
        }
        created.push("main.vo");
    }

    Ok(created.join("\n"))
}

fn init_file_impl(file_path: &Path) -> Result<(), String> {
    if file_path.exists() {
        return Err(format!("file already exists: {file_path:?}"));
    }

    fs::write(
        file_path,
        "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, Vo!\")\n}\n",
    )
    .map_err(|e| e.to_string())
}

impl ToolchainHost for EngineToolchainHost {
    fn compile_file(&self, path: &Path) -> Result<ToolchainModule, String> {
        crate::compile::compile_path(path).map_err(|e| e.to_string())
    }

    fn compile_dir(&self, path: &Path) -> Result<ToolchainModule, String> {
        crate::compile::compile_path(path).map_err(|e| e.to_string())
    }

    fn compile_string(&self, code: &str) -> Result<ToolchainModule, String> {
        compile_string(code).map_err(|e| e.to_string())
    }

    fn run(&self, module: &ToolchainModule, mode: ToolchainRunMode) -> Result<(), String> {
        run(module.clone(), run_mode(mode), Vec::new()).map_err(|e| e.to_string())
    }

    fn run_capture(
        &self,
        module: &ToolchainModule,
        mode: ToolchainRunMode,
    ) -> Result<Vec<u8>, String> {
        let sink = CaptureSink::new();
        let result = run_with_output(module.clone(), run_mode(mode), Vec::new(), sink.clone());
        let output = sink.take_bytes();
        match result {
            Ok(()) => Ok(output),
            Err(err) => Err(err.to_string()),
        }
    }

    fn parse_file(&self, path: &Path) -> Result<String, String> {
        let source = vo_common::vfs::read_text_file(path).map_err(|e| e.to_string())?;
        parse_source(&source)
    }

    fn parse_string(&self, code: &str) -> Result<String, String> {
        parse_source(code)
    }

    fn format_source(&self, code: &str) -> Result<String, String> {
        format_source_impl(code)
    }

    fn format_bytecode(&self, module: &ToolchainModule) -> String {
        format_text(&module.module)
    }

    fn save_bytecode_text(&self, module: &ToolchainModule, path: &Path) -> Result<(), String> {
        fs::write(path, format_text(&module.module)).map_err(|e| e.to_string())
    }

    fn save_bytecode_binary(&self, module: &ToolchainModule, path: &Path) -> Result<(), String> {
        vo_common_core::verifier::verify_module(&module.module)
            .map_err(|err| format!("invalid bytecode: {err}"))?;
        let bytes = module
            .module
            .serialize()
            .map_err(|err| format!("failed to serialize bytecode: {err}"))?;
        fs::write(path, bytes).map_err(|e| e.to_string())
    }

    fn load_bytecode_binary(&self, path: &Path) -> Result<ToolchainModule, String> {
        let bytes = vo_common_core::serialize::read_vob_file(path).map_err(|e| e.to_string())?;
        let module = Module::deserialize(&bytes).map_err(|error| error.to_string())?;
        vo_common_core::verifier::verify_module(&module)
            .map_err(|err| format!("invalid bytecode: {err}"))?;
        let source_root = path.parent().unwrap_or(Path::new(".")).to_path_buf();
        Ok(ToolchainModule {
            module,
            source_root,
            extensions: Vec::new(),
            locked_modules: Vec::new(),
        })
    }

    fn init_project(&self, dir: &Path, mod_name: &str) -> Result<String, String> {
        init_project_impl(dir, mod_name)
    }

    fn init_file(&self, path: &Path) -> Result<(), String> {
        init_file_impl(path)
    }
}

pub fn ensure_toolchain_host_installed() {
    if is_toolchain_host_installed() {
        return;
    }
    install_toolchain_host(Arc::new(EngineToolchainHost));
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(name: &str) -> std::path::PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system clock")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "vo-engine-toolchain-{name}-{}-{nonce}",
            std::process::id()
        ))
    }

    #[test]
    fn init_file_uses_main_package_for_arbitrary_file_names() {
        let root = unique_temp_dir("init-file-package");
        fs::create_dir_all(&root).unwrap();

        for name in ["hello-world.vo", "程序.vo", ".vo"] {
            let path = root.join(name);
            init_file_impl(&path).unwrap();
            let source = fs::read_to_string(&path).unwrap();
            assert!(source.starts_with("package main\n"), "{name}: {source}");
            let (_, diagnostics, _) = parser::parse(&source, 0);
            assert!(!diagnostics.has_errors(), "{name}: {source}");
        }

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn init_project_is_valid_and_rejects_bad_modules_before_writing() {
        let root = unique_temp_dir("init-project");
        let invalid = root.join("invalid");
        let error = init_project_impl(&invalid, "not a module!").unwrap_err();
        assert!(!error.is_empty());
        assert!(!invalid.exists(), "invalid init left files behind");

        let valid = root.join("valid");
        let created = init_project_impl(&valid, "github.com/acme/demo").unwrap();
        assert_eq!(created, "vo.mod\nmain.vo");

        let manifest = fs::read_to_string(valid.join("vo.mod")).unwrap();
        assert!(
            manifest.contains("module = \"github.com/acme/demo\""),
            "{manifest}"
        );
        assert!(
            manifest.contains(&format!("vo = \"{}\"", vo_module::TOOLCHAIN_CONSTRAINT,)),
            "{manifest}"
        );
        let source = fs::read_to_string(valid.join("main.vo")).unwrap();
        let (_, diagnostics, _) = parser::parse(&source, 0);
        assert!(!diagnostics.has_errors(), "{source}");

        fs::remove_dir_all(root).unwrap();
    }
}
