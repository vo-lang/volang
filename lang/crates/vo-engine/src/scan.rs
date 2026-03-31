use std::collections::BTreeSet;
use std::path::Path;

use vo_module::identity::{classify_import, ImportClass};
use vo_module::Error;

/// Scan all `.vo` source files in a project directory and return
/// the set of external import paths found.
pub fn scan_external_imports(project_dir: &Path) -> Result<BTreeSet<String>, Error> {
    let mut imports = BTreeSet::new();
    scan_external_imports_dir(project_dir, &mut imports)?;
    Ok(imports)
}

fn scan_external_imports_dir(dir: &Path, imports: &mut BTreeSet<String>) -> Result<(), Error> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            let name = path
                .file_name()
                .and_then(|value| value.to_str())
                .unwrap_or("");
            if name.starts_with('.')
                || name == "vendor"
                || name == "testdata"
                || name == "node_modules"
                || name == "target"
                || name == "dist"
            {
                continue;
            }
            scan_external_imports_dir(&path, imports)?;
            continue;
        }
        if path.extension().map(|value| value == "vo").unwrap_or(false) {
            scan_external_imports_file(&path, imports)?;
        }
    }
    Ok(())
}

fn scan_external_imports_file(path: &Path, imports: &mut BTreeSet<String>) -> Result<(), Error> {
    let content = std::fs::read_to_string(path)?;
    let (file, diagnostics, _) = vo_syntax::parse(&content, 0);
    if diagnostics.has_errors() {
        let detail = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.message.as_str())
            .collect::<Vec<_>>()
            .join("; ");
        return Err(Error::SourceScan(format!(
            "failed to parse {} while scanning imports: {}",
            path.display(),
            detail,
        )));
    }
    for import in &file.imports {
        let import_path = import.path.value.clone();
        if classify_import(&import_path)? == ImportClass::External {
            imports.insert(import_path);
        }
    }
    Ok(())
}
