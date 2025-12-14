# GoX Compilation Pipeline

## Overview

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│     VFS      │────▶│   Analysis   │────▶│   Codegen    │────▶│   Output     │
│  File Set    │     │  AST+Types   │     │ VM Bytecode  │     │   Module     │
└──────────────┘     └──────────────┘     └──────────────┘     └──────────────┘
```

## Phase 1: File Collection (gox-common)

**Input**: Project root directory  
**Output**: `FileSet`

```rust
// gox-common/src/vfs.rs

pub trait FileSystem {
    fn read_file(&self, path: &Path) -> io::Result<String>;
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>>;
    fn exists(&self, path: &Path) -> bool;
}

pub struct RealFs;      // Real file system
pub struct MemoryFs;    // In-memory file system (for testing)

pub struct FileSet {
    pub files: HashMap<PathBuf, String>,  // path -> content
    pub root: PathBuf,
}
```

## Phase 2: Parsing & Analysis (gox-analysis)

**Input**: `FileSet`  
**Output**: `Project`

### 2.1 Parse each file

```rust
// gox-syntax::parse() already exists

struct ParsedPackage {
    name: String,
    dir: PathBuf,
    files: Vec<(PathBuf, ast::File)>,
}
```

### 2.2 Resolve imports, build dependency graph

```rust
// gox-analysis/src/imports.rs

enum ImportPath {
    /// "./mylib" - relative to current package directory
    Local(PathBuf),
    /// "github.com/user/pkg" - external module
    External(String),
}

fn resolve_import(pkg_dir: &Path, import_str: &str) -> ImportPath {
    if import_str.starts_with("./") || import_str.starts_with("../") {
        ImportPath::Local(pkg_dir.join(import_str))
    } else {
        ImportPath::External(import_str.to_string())
    }
}
```

### 2.3 Topological sort, type check in order

```rust
// gox-analysis/src/project.rs

struct TypedPackage {
    name: String,
    files: Vec<ast::File>,
    types: TypeCheckResult,
    exports: HashMap<Symbol, Type>,  // Capitalized symbols are exported
}

struct Project {
    packages: Vec<TypedPackage>,  // Sorted by dependency order
    interner: SymbolInterner,
    main_package: String,
}

pub fn analyze_project(file_set: FileSet) -> Result<Project, AnalysisError>;
```

## Phase 3: Code Generation (gox-codegen-vm)

**Input**: `Project`  
**Output**: `Module`

```rust
// gox-codegen-vm/src/lib.rs

pub fn compile_project(project: &Project) -> Result<Module, CodegenError> {
    let mut module = Module::new(&project.main_package);
    
    // Compile each package in dependency order
    for pkg in &project.packages {
        compile_package(&mut module, pkg)?;
    }
    
    // Set entry point to main.main
    module.entry_func = find_main_func(&module)?;
    
    Ok(module)
}
```

## Phase 4: Output (gox-vm)

**Input**: `Module`  
**Output**: `.goxc` binary file

```rust
// gox-vm/src/bytecode.rs (read/write already exist)

module.write(&mut file)?;      // Serialize to file
Module::read(&mut file)?;      // Deserialize from file
```

## CLI Usage

```rust
// gox-cli/src/main.rs

// gox build ./myproject
fn cmd_build(path: &str) -> Result<()> {
    let vfs = RealFs;
    let file_set = vfs.collect_files(path)?;
    let project = analyze_project(file_set)?;
    let module = compile_project(&project)?;
    module.write_to_file(&format!("{}.goxc", project.main_package))?;
    Ok(())
}

// gox run myproject.goxc
fn cmd_run(path: &str) -> Result<()> {
    let module = Module::read_from_file(path)?;
    let mut vm = create_vm();
    vm.load_module(module);
    vm.run();
    Ok(())
}
```

## Code Location

| Phase | Crate | File | Content |
|-------|-------|------|---------|
| 1 | gox-common | `vfs.rs` | FileSystem trait, RealFs, MemoryFs |
| 2 | gox-syntax | `lib.rs` | parse() (existing) |
| 2 | gox-analysis | `project.rs` | Project, analyze_project() |
| 2 | gox-analysis | `imports.rs` | Import path resolution |
| 2 | gox-analysis | `deps.rs` | Dependency graph, topological sort |
| 3 | gox-codegen-vm | `lib.rs` | compile_project() |
| 4 | gox-vm | `bytecode.rs` | Module::read/write (existing) |
| CLI | gox-cli | `main.rs` | build/run commands |
