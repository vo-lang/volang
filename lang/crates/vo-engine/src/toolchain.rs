use std::fs;
use std::path::Path;
use std::sync::Arc;

use vo_runtime::output::CaptureSink;
use vo_stdlib::toolchain::{
    install_toolchain_host, is_toolchain_host_installed, ToolchainHost, ToolchainModule,
    ToolchainRunMode,
};
use vo_syntax::parser;

use crate::{
    compile, compile_string, format_source, format_text, parse_text, run, run_with_output,
    CompileOutput, Module, RunMode,
};

struct EngineToolchainHost;

fn into_toolchain_module(output: CompileOutput) -> ToolchainModule {
    ToolchainModule {
        module: output.module,
        source_root: output.source_root,
        extensions: output.extensions,
        locked_modules: output.locked_modules,
    }
}

fn from_toolchain_module(module: &ToolchainModule) -> CompileOutput {
    CompileOutput {
        module: module.module.clone(),
        source_root: module.source_root.clone(),
        extensions: module.extensions.clone(),
        locked_modules: module.locked_modules.clone(),
    }
}

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

fn init_project_impl(dir: &str, mod_name: &str) -> Result<String, String> {
    let dir_path = Path::new(dir);
    if !dir_path.exists() {
        fs::create_dir_all(dir_path).map_err(|e| e.to_string())?;
    }

    let mut created: Vec<&str> = Vec::new();

    let main_file = dir_path.join("main.vo");
    if !main_file.exists() {
        fs::write(
            &main_file,
            "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, Vo!\")\n}\n",
        )
        .map_err(|e| e.to_string())?;
        created.push("main.vo");
    }

    let mod_file = dir_path.join("vo.mod");
    if !mod_file.exists() {
        vo_module::ops::mod_init(dir_path, mod_name, "0.1").map_err(|e| e.to_string())?;
        created.push("vo.mod");
    }

    Ok(created.join("\n"))
}

fn init_file_impl(path: &str) -> Result<(), String> {
    let file_path = Path::new(path);
    if file_path.exists() {
        return Err(format!("file already exists: {}", path));
    }

    let pkg = file_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");

    fs::write(
        file_path,
        format!(
            "package {}\n\nimport \"fmt\"\n\nfunc main() {{\n\tfmt.Println(\"Hello, Vo!\")\n}}\n",
            pkg,
        ),
    )
    .map_err(|e| e.to_string())
}

impl ToolchainHost for EngineToolchainHost {
    fn compile_file(&self, path: &str) -> Result<ToolchainModule, String> {
        compile(path)
            .map(into_toolchain_module)
            .map_err(|e| e.to_string())
    }

    fn compile_dir(&self, path: &str) -> Result<ToolchainModule, String> {
        compile(path)
            .map(into_toolchain_module)
            .map_err(|e| e.to_string())
    }

    fn compile_string(&self, code: &str) -> Result<ToolchainModule, String> {
        compile_string(code)
            .map(into_toolchain_module)
            .map_err(|e| e.to_string())
    }

    fn run(&self, module: &ToolchainModule, mode: ToolchainRunMode) -> Result<(), String> {
        run(from_toolchain_module(module), run_mode(mode), Vec::new()).map_err(|e| e.to_string())
    }

    fn run_capture(
        &self,
        module: &ToolchainModule,
        mode: ToolchainRunMode,
    ) -> Result<String, String> {
        let sink = CaptureSink::new();
        let result = run_with_output(
            from_toolchain_module(module),
            run_mode(mode),
            Vec::new(),
            sink.clone(),
        );
        let output = sink.take();
        match result {
            Ok(()) => Ok(output),
            Err(err) => Err(err.to_string()),
        }
    }

    fn parse_file(&self, path: &str) -> Result<String, String> {
        let source = fs::read_to_string(path).map_err(|e| e.to_string())?;
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

    fn save_bytecode_text(&self, module: &ToolchainModule, path: &str) -> Result<(), String> {
        fs::write(path, format_text(&module.module)).map_err(|e| e.to_string())
    }

    fn load_bytecode_text(&self, path: &str) -> Result<ToolchainModule, String> {
        let text = fs::read_to_string(path).map_err(|e| e.to_string())?;
        let module = parse_text(&text).map_err(|e| e.to_string())?;
        let source_root = Path::new(path)
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf();
        Ok(ToolchainModule {
            module,
            source_root,
            extensions: Vec::new(),
            locked_modules: Vec::new(),
        })
    }

    fn save_bytecode_binary(&self, module: &ToolchainModule, path: &str) -> Result<(), String> {
        fs::write(path, module.module.serialize()).map_err(|e| e.to_string())
    }

    fn load_bytecode_binary(&self, path: &str) -> Result<ToolchainModule, String> {
        let bytes = fs::read(path).map_err(|e| e.to_string())?;
        let module = Module::deserialize(&bytes).map_err(|e| format!("{:?}", e))?;
        let source_root = Path::new(path)
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf();
        Ok(ToolchainModule {
            module,
            source_root,
            extensions: Vec::new(),
            locked_modules: Vec::new(),
        })
    }

    fn init_project(&self, dir: &str, mod_name: &str) -> Result<String, String> {
        init_project_impl(dir, mod_name)
    }

    fn init_file(&self, path: &str) -> Result<(), String> {
        init_file_impl(path)
    }

    fn get(&self, spec: &str) -> Result<String, String> {
        let (module, version) = spec
            .rsplit_once('@')
            .filter(|(module, version)| !module.is_empty() && !version.is_empty())
            .ok_or_else(|| {
                format!(
                    "invalid spec {:?}: expected <module>@<version>, e.g. github.com/foo/bar@v0.1.0",
                    spec,
                )
            })?;
        install_module(module, version)
            .map(|path| path.to_string_lossy().to_string())
            .map_err(|e| e.to_string())
    }
}

pub fn install_module(module: &str, version: &str) -> Result<std::path::PathBuf, String> {
    use vo_module::github_registry::GitHubRegistry;
    use vo_module::identity::ModulePath;
    use vo_module::version::ExactVersion;

    let mp = ModulePath::parse(module).map_err(|e| format!("{e}"))?;
    let ev = ExactVersion::parse(version).map_err(|e| format!("{e}"))?;

    let registry = GitHubRegistry::new();
    let mod_cache = crate::compile::default_mod_cache_root();
    let installed = vo_module::cache::install::install_exact_module(
        &mod_cache,
        &registry,
        &mp,
        &ev,
        "vo get",
    )
    .map_err(|e| format!("{e}"))?;
    let manifests =
        vo_module::ext_manifest::discover_extensions(&installed.cache_dir).map_err(|e| e.to_string())?;
    let _ = crate::compile::prepare_native_extension_specs(
        &manifests,
        std::slice::from_ref(&installed.locked),
        &mod_cache,
    )
        .map_err(|e| e.to_string())?;

    Ok(installed.cache_dir)
}

pub fn ensure_toolchain_host_installed() {
    if is_toolchain_host_installed() {
        return;
    }
    install_toolchain_host(Arc::new(EngineToolchainHost));
}
