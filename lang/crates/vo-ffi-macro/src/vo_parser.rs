//! Vo function signature parser for extern function validation.
//!
//! Uses vo-syntax for proper parsing of .vo files.

use std::collections::HashMap;
use std::io::Read as _;
use std::path::{Path, PathBuf};

use sha2::{Digest as _, Sha256};
use vo_analysis::constant::{
    self, Value as ConstValue, MAX_CONSTANT_FOLD_WORK_BYTES, MAX_CONSTANT_SHIFT,
};
use vo_common::symbol::SymbolInterner;
use vo_syntax::{self, ast, TypeExprKind};

/// A parsed Vo function signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VoFuncSig {
    pub name: String,
    pub params: Vec<VoParam>,
    pub results: Vec<VoType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VoImport {
    pub alias: Option<String>,
    pub path: String,
}

/// A Vo function parameter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VoParam {
    pub name: String,
    pub ty: VoType,
}

/// A Vo type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VoType {
    // Primitive types
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    Bool,
    String,
    Any,
    Error,
    // Composite types
    Named(String),
    Pointer(Box<VoType>),
    Slice(Box<VoType>),
    Array(usize, Box<VoType>),
    Map(Box<VoType>, Box<VoType>),
    Chan(ChanDir, Box<VoType>),
    Port(ChanDir, Box<VoType>),
    Island,
    Func(Vec<VoType>, Vec<VoType>),
    /// Variadic parameter: ...T (e.g., ...interface{})
    Variadic(Box<VoType>),
    /// Struct type with field types
    Struct(Vec<VoType>),
}

/// Channel direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChanDir {
    /// Bidirectional: chan T
    Both,
    /// Send-only: chan<- T
    Send,
    /// Receive-only: <-chan T
    Recv,
}

impl std::fmt::Display for VoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VoType::Int => write!(f, "int"),
            VoType::Int8 => write!(f, "int8"),
            VoType::Int16 => write!(f, "int16"),
            VoType::Int32 => write!(f, "int32"),
            VoType::Int64 => write!(f, "int64"),
            VoType::Uint => write!(f, "uint"),
            VoType::Uint8 => write!(f, "uint8"),
            VoType::Uint16 => write!(f, "uint16"),
            VoType::Uint32 => write!(f, "uint32"),
            VoType::Uint64 => write!(f, "uint64"),
            VoType::Float32 => write!(f, "float32"),
            VoType::Float64 => write!(f, "float64"),
            VoType::Bool => write!(f, "bool"),
            VoType::String => write!(f, "string"),
            VoType::Any => write!(f, "any"),
            VoType::Error => write!(f, "error"),
            VoType::Named(name) => write!(f, "{}", name),
            VoType::Pointer(inner) => write!(f, "*{}", inner),
            VoType::Slice(inner) => write!(f, "[]{}", inner),
            VoType::Array(len, inner) => write!(f, "[{}]{}", len, inner),
            VoType::Map(k, v) => write!(f, "map[{}]{}", k, v),
            VoType::Chan(dir, inner) => match dir {
                ChanDir::Both => write!(f, "chan {}", inner),
                ChanDir::Send => write!(f, "chan<- {}", inner),
                ChanDir::Recv => write!(f, "<-chan {}", inner),
            },
            VoType::Port(dir, inner) => match dir {
                ChanDir::Both => write!(f, "port {}", inner),
                ChanDir::Send => write!(f, "port<- {}", inner),
                ChanDir::Recv => write!(f, "<-port {}", inner),
            },
            VoType::Func(params, results) => {
                write!(f, "func(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ")")?;
                if !results.is_empty() {
                    if results.len() == 1 {
                        write!(f, " {}", results[0])?;
                    } else {
                        write!(f, " (")?;
                        for (i, r) in results.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", r)?;
                        }
                        write!(f, ")")?;
                    }
                }
                Ok(())
            }
            VoType::Variadic(inner) => write!(f, "...{}", inner),
            VoType::Struct(fields) => write!(f, "struct{{{} fields}}", fields.len()),
            VoType::Island => write!(f, "island"),
        }
    }
}

impl VoType {
    /// Get the number of stack slots this type occupies.
    pub fn slot_count(&self, type_aliases: &HashMap<String, VoType>) -> Result<u16, String> {
        self.slot_count_inner(type_aliases, &mut Vec::new())
    }

    fn slot_count_inner(
        &self,
        type_aliases: &HashMap<String, VoType>,
        alias_stack: &mut Vec<String>,
    ) -> Result<u16, String> {
        match self {
            // Primitive types: 1 slot each
            VoType::Int
            | VoType::Int8
            | VoType::Int16
            | VoType::Int32
            | VoType::Int64
            | VoType::Uint
            | VoType::Uint8
            | VoType::Uint16
            | VoType::Uint32
            | VoType::Uint64
            | VoType::Float32
            | VoType::Float64
            | VoType::Bool
            | VoType::String => Ok(1),

            // Interfaces use two slots (metadata, data).
            VoType::Any | VoType::Error => Ok(2),

            // Reference types: 1 slot (GcRef)
            VoType::Pointer(_)
            | VoType::Slice(_)
            | VoType::Map(_, _)
            | VoType::Chan(_, _)
            | VoType::Port(_, _)
            | VoType::Island
            | VoType::Func(_, _) => Ok(1),

            // Array: elem_slots * length
            VoType::Array(len, elem) => {
                let elem_slots = elem.slot_count_inner(type_aliases, alias_stack)?;
                let len = u16::try_from(*len).map_err(|_| {
                    format!("array length {len} exceeds the FFI u16 slot address space")
                })?;
                elem_slots.checked_mul(len).ok_or_else(|| {
                    format!(
                        "array layout requires {} × {} slots, exceeding the FFI u16 slot address space",
                        elem_slots, len
                    )
                })
            }

            // Named type: resolve alias
            VoType::Named(name) => {
                if let Some(underlying) = type_aliases.get(name) {
                    if let Some(cycle_start) = alias_stack.iter().position(|entry| entry == name) {
                        let mut cycle = alias_stack[cycle_start..].to_vec();
                        cycle.push(name.clone());
                        return Err(format!(
                            "cyclic type layout while resolving `{name}`: {}",
                            cycle.join(" -> ")
                        ));
                    }
                    alias_stack.push(name.clone());
                    let result = underlying.slot_count_inner(type_aliases, alias_stack);
                    alias_stack.pop();
                    result
                } else {
                    Err(format!(
                        "cannot determine FFI layout for unresolved named type `{name}`"
                    ))
                }
            }

            // Variadic: treated as slice (1 slot)
            VoType::Variadic(_) => Ok(1),

            // Struct: sum of all field slots
            VoType::Struct(fields) => fields.iter().try_fold(0u16, |total, field| {
                let field_slots = field.slot_count_inner(type_aliases, alias_stack)?;
                total.checked_add(field_slots).ok_or_else(|| {
                    "struct layout exceeds the FFI u16 slot address space".to_string()
                })
            }),
        }
    }
}

struct ParsedVoFile {
    path: PathBuf,
    file: ast::File,
    interner: SymbolInterner,
}

struct ParsedPackage {
    name: String,
    files: Vec<ParsedVoFile>,
}

#[derive(Debug)]
struct CapturedVoSource {
    path: PathBuf,
    content: String,
}

#[derive(Debug)]
struct CapturedVoPackage {
    sources: Vec<CapturedVoSource>,
    source_digest: [u8; 32],
}

/// One immutable package parse used by FFI layout graph construction.
///
/// The package name, aliases, and imports all come from the same captured
/// source bytes. This avoids combining several live filesystem generations
/// when the layout resolver needs more than one view of a package.
pub(crate) struct ParsedLayoutPackage {
    pub(crate) name: String,
    pub(crate) aliases: HashMap<String, VoType>,
    pub(crate) imports: Vec<VoImport>,
    pub(crate) source_paths: Vec<PathBuf>,
    pub(crate) source_digest: [u8; 32],
}

#[derive(Clone)]
struct ConstDefinition {
    expression: Result<ConstExpr, String>,
    iota: usize,
}

#[derive(Clone)]
enum ConstExpr {
    Int(String),
    Rune(char),
    Ident(String),
    Unary(ast::UnaryOp, Box<ConstExpr>),
    Binary(Box<ConstExpr>, ast::BinaryOp, Box<ConstExpr>),
}

impl ConstExpr {
    fn from_ast(expression: &ast::Expr, interner: &SymbolInterner) -> Result<Self, String> {
        Ok(match &expression.kind {
            ast::ExprKind::IntLit(literal) => Self::Int(
                interner
                    .resolve(literal.raw)
                    .ok_or_else(|| {
                        "integer constant literal is missing from the symbol table".to_string()
                    })?
                    .to_string(),
            ),
            ast::ExprKind::RuneLit(literal) => Self::Rune(literal.value),
            ast::ExprKind::Ident(identifier) => Self::Ident(
                interner
                    .resolve(identifier.symbol)
                    .filter(|name| !name.is_empty())
                    .ok_or_else(|| {
                        "constant identifier is missing from the symbol table".to_string()
                    })?
                    .to_string(),
            ),
            ast::ExprKind::Unary(unary) => Self::Unary(
                unary.op,
                Box::new(Self::from_ast(&unary.operand, interner)?),
            ),
            ast::ExprKind::Binary(binary) => Self::Binary(
                Box::new(Self::from_ast(&binary.left, interner)?),
                binary.op,
                Box::new(Self::from_ast(&binary.right, interner)?),
            ),
            ast::ExprKind::Paren(inner) => Self::from_ast(inner, interner)?,
            _ => {
                return Err(
                    "array length supports package integer constants, literals, parentheses, and integer unary/binary operations"
                        .to_string(),
                )
            }
        })
    }
}

struct ConstantEvaluator {
    definitions: HashMap<String, ConstDefinition>,
    values: HashMap<String, ConstValue>,
    stack: Vec<String>,
    work_bytes: u64,
}

impl ConstantEvaluator {
    fn new(files: &[ParsedVoFile]) -> Result<Self, String> {
        Ok(Self {
            definitions: collect_const_definitions(files)?,
            values: HashMap::new(),
            stack: Vec::new(),
            work_bytes: 0,
        })
    }

    fn charge(&mut self, values: &[&ConstValue]) -> Result<(), String> {
        let additional = constant::constant_fold_work_bytes(values)
            .map_err(|error| format!("array length constant: {error}"))?;
        let total = self
            .work_bytes
            .checked_add(additional)
            .filter(|total| *total <= MAX_CONSTANT_FOLD_WORK_BYTES)
            .ok_or_else(|| {
                format!(
                    "array length constant-folding work exceeds the per-package limit of {MAX_CONSTANT_FOLD_WORK_BYTES} bytes"
                )
            })?;
        self.work_bytes = total;
        Ok(())
    }

    fn array_length(
        &mut self,
        expression: &ast::Expr,
        interner: &SymbolInterner,
    ) -> Result<usize, String> {
        let expression = ConstExpr::from_ast(expression, interner)?;
        let value = self.eval_expr(&expression, None)?;
        let (value, exact) = value.int_as_u64();
        if !exact {
            return Err(format!(
                "array length expression must evaluate to a non-negative integer; found `{value}`"
            ));
        }
        usize::try_from(value)
            .map_err(|_| format!("array length {value} does not fit this host's usize"))
    }

    fn eval_named(&mut self, name: &str) -> Result<ConstValue, String> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        }
        let definition = self.definitions.get(name).cloned().ok_or_else(|| {
            format!("array length references unresolved package constant `{name}`")
        })?;
        if let Some(cycle_start) = self.stack.iter().position(|entry| entry == name) {
            let mut cycle = self.stack[cycle_start..].to_vec();
            cycle.push(name.to_string());
            return Err(format!(
                "cyclic package constants in array length: {}",
                cycle.join(" -> ")
            ));
        }
        self.stack.push(name.to_string());
        let result = definition
            .expression
            .and_then(|expression| self.eval_expr(&expression, Some(definition.iota)));
        self.stack.pop();
        let value = result?;
        self.charge(&[&value])?;
        self.values.insert(name.to_string(), value.clone());
        Ok(value)
    }

    fn eval_expr(
        &mut self,
        expression: &ConstExpr,
        iota: Option<usize>,
    ) -> Result<ConstValue, String> {
        let value = match expression {
            ConstExpr::Int(raw) => {
                let value = constant::try_int_from_literal(raw)
                    .map_err(|error| format!("array length constant: {error}"))?;
                self.charge(&[&value])?;
                value
            }
            ConstExpr::Rune(rune) => {
                let value = constant::make_int64(i64::from(u32::from(*rune)));
                self.charge(&[&value])?;
                value
            }
            ConstExpr::Ident(name) => {
                let value = if name == "iota" {
                    let value = iota.ok_or_else(|| {
                        "`iota` is only valid inside a package const declaration".to_string()
                    })?;
                    let value = u64::try_from(value)
                        .map_err(|_| "`iota` value does not fit u64".to_string())?;
                    constant::make_uint64(value)
                } else {
                    self.eval_named(name)?
                };
                self.charge(&[&value])?;
                value
            }
            ConstExpr::Unary(operator, operand) => {
                let operand = self.eval_expr(operand, iota)?;
                let value = constant::try_unary_op(*operator, &operand, 0)
                    .map_err(|error| format!("array length constant: {error}"))?;
                self.charge(&[&operand, &value])?;
                value
            }
            ConstExpr::Binary(left, operator, right) => {
                let left = self.eval_expr(left, iota)?;
                let right = self.eval_expr(right, iota)?;
                let value = match operator {
                    ast::BinaryOp::Shl | ast::BinaryOp::Shr => {
                        let (shift, exact) = right.int_as_u64();
                        if !exact {
                            return Err("array length shift count must be a non-negative integer"
                                .to_string());
                        }
                        if shift > u64::from(MAX_CONSTANT_SHIFT) {
                            return Err(format!(
                                "array length shift count exceeds the language limit of {MAX_CONSTANT_SHIFT}"
                            ));
                        }
                        let shift = u32::try_from(shift)
                            .map_err(|_| "array length shift count exceeds u32::MAX".to_string())?;
                        constant::try_shift(&left, *operator, shift)
                            .map_err(|error| format!("array length constant: {error}"))?
                    }
                    ast::BinaryOp::Eq
                    | ast::BinaryOp::NotEq
                    | ast::BinaryOp::Lt
                    | ast::BinaryOp::LtEq
                    | ast::BinaryOp::Gt
                    | ast::BinaryOp::GtEq => {
                        constant::make_bool(constant::compare(&left, *operator, &right))
                    }
                    _ => constant::try_binary_op(&left, *operator, &right)
                        .map_err(|error| format!("array length constant: {error}"))?,
                };
                self.charge(&[&left, &right, &value])?;
                value
            }
        };
        if value.is_unknown() {
            Err(
                "array length constant expression is invalid or not an integer operation"
                    .to_string(),
            )
        } else {
            Ok(value)
        }
    }
}

fn collect_const_definitions(
    files: &[ParsedVoFile],
) -> Result<HashMap<String, ConstDefinition>, String> {
    let mut definitions = HashMap::new();
    for parsed in files {
        for declaration in &parsed.file.decls {
            let ast::Decl::Const(const_decl) = declaration else {
                continue;
            };
            let mut previous_values: Option<Vec<ast::Expr>> = None;
            for (iota, spec) in const_decl.specs.iter().enumerate() {
                let values = if spec.values.is_empty() {
                    previous_values.clone().ok_or_else(|| {
                        format!(
                            "{} starts a const declaration with an omitted initializer",
                            parsed.path.display()
                        )
                    })?
                } else {
                    previous_values = Some(spec.values.clone());
                    spec.values.clone()
                };
                if values.len() != spec.names.len() {
                    return Err(format!(
                        "{} has {} const names and {} initializer expressions; FFI array-length evaluation requires one expression per name",
                        parsed.path.display(),
                        spec.names.len(),
                        values.len()
                    ));
                }
                for (name, expression) in spec.names.iter().zip(values) {
                    let name = parsed.interner.resolve(name.symbol).unwrap_or_default();
                    if name == "_" {
                        continue;
                    }
                    if name.is_empty() {
                        return Err(format!(
                            "{} contains an unnamed package constant",
                            parsed.path.display()
                        ));
                    }
                    let definition = ConstDefinition {
                        expression: ConstExpr::from_ast(&expression, &parsed.interner),
                        iota,
                    };
                    if definitions.insert(name.to_string(), definition).is_some() {
                        return Err(format!("duplicate package constant declaration `{name}`"));
                    }
                }
            }
        }
    }
    Ok(definitions)
}

/// List a package's source files in a platform-independent byte order.
pub fn list_vo_source_paths(pkg_dir: &Path) -> Result<Vec<PathBuf>, String> {
    let entries = std::fs::read_dir(pkg_dir).map_err(|error| {
        format!(
            "failed to read Vo package directory {}: {error}",
            pkg_dir.display()
        )
    })?;
    let mut paths = Vec::new();
    let mut entry_count = 0usize;
    for entry in entries {
        entry_count = entry_count.checked_add(1).ok_or_else(|| {
            format!(
                "Vo package directory {} contains too many entries",
                pkg_dir.display()
            )
        })?;
        if entry_count > vo_common::vfs::MAX_DIRECTORY_ENTRIES {
            return Err(format!(
                "Vo package directory {} contains more than {} entries",
                pkg_dir.display(),
                vo_common::vfs::MAX_DIRECTORY_ENTRIES
            ));
        }
        let entry = entry.map_err(|error| {
            format!(
                "failed to read an entry in Vo package directory {}: {error}",
                pkg_dir.display()
            )
        })?;
        let path = entry.path();
        if path.extension().is_some_and(|extension| extension == "vo") {
            if paths.len() == vo_common::vfs::MAX_PACKAGE_SOURCE_FILES {
                return Err(format!(
                    "Vo package directory {} contains more than {} source files",
                    pkg_dir.display(),
                    vo_common::vfs::MAX_PACKAGE_SOURCE_FILES
                ));
            }
            let display = path.to_str().ok_or_else(|| {
                format!(
                    "Vo source path is not valid UTF-8 and cannot be tracked by Cargo: {:?}",
                    path
                )
            })?;
            paths.push((display.replace('\\', "/").into_bytes(), path));
        }
    }
    paths.sort_by(|(left_key, left_path), (right_key, right_path)| {
        left_key
            .cmp(right_key)
            .then_with(|| left_path.cmp(right_path))
    });
    let paths: Vec<_> = paths.into_iter().map(|(_, path)| path).collect();
    if paths.is_empty() {
        return Err(format!(
            "Vo package directory {} contains no .vo source files",
            pkg_dir.display()
        ));
    }
    Ok(paths)
}

fn hash_source_snapshot_field(hasher: &mut Sha256, bytes: &[u8]) -> Result<(), String> {
    let length = u64::try_from(bytes.len())
        .map_err(|_| "Vo source snapshot field length overflow".to_string())?;
    hasher.update(length.to_le_bytes());
    hasher.update(bytes);
    Ok(())
}

#[cfg(windows)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct WindowsFileInformation {
    volume: u64,
    file: [u8; 16],
    legacy_volume: u32,
    legacy_file: u64,
    links: u32,
    attributes: u32,
    size: u64,
    creation_time: u64,
    last_write_time: u64,
}

#[cfg(windows)]
fn windows_file_information(file: &std::fs::File) -> std::io::Result<WindowsFileInformation> {
    use std::os::windows::io::AsRawHandle as _;

    use windows_sys::Win32::Foundation::HANDLE;
    use windows_sys::Win32::Storage::FileSystem::{
        FileIdInfo, GetFileInformationByHandle, GetFileInformationByHandleEx,
        BY_HANDLE_FILE_INFORMATION, FILE_ID_INFO,
    };

    let handle = file.as_raw_handle() as HANDLE;
    let mut legacy = BY_HANDLE_FILE_INFORMATION::default();
    // SAFETY: `handle` remains live for the call and `legacy` is the exact
    // writable structure required by GetFileInformationByHandle.
    if unsafe { GetFileInformationByHandle(handle, &mut legacy) } == 0 {
        return Err(std::io::Error::last_os_error());
    }

    let mut identity = FILE_ID_INFO::default();
    // SAFETY: `handle` remains live, and `identity` is the exact writable
    // structure required by FileIdInfo. Unsupported file systems fall back to
    // the legacy volume/index pair queried above.
    let has_extended_identity = unsafe {
        GetFileInformationByHandleEx(
            handle,
            FileIdInfo,
            std::ptr::from_mut(&mut identity).cast(),
            u32::try_from(std::mem::size_of::<FILE_ID_INFO>())
                .expect("FILE_ID_INFO size fits in u32"),
        )
    } != 0;
    let join_u32 = |high: u32, low: u32| (u64::from(high) << 32) | u64::from(low);
    let legacy_file = join_u32(legacy.nFileIndexHigh, legacy.nFileIndexLow);
    if !has_extended_identity && legacy_file == u64::MAX {
        return Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "Windows file system did not provide a stable file identity",
        ));
    }
    Ok(WindowsFileInformation {
        volume: has_extended_identity
            .then_some(identity.VolumeSerialNumber)
            .unwrap_or_default(),
        file: has_extended_identity
            .then_some(identity.FileId.Identifier)
            .unwrap_or_default(),
        legacy_volume: legacy.dwVolumeSerialNumber,
        legacy_file,
        links: legacy.nNumberOfLinks,
        attributes: legacy.dwFileAttributes,
        size: join_u32(legacy.nFileSizeHigh, legacy.nFileSizeLow),
        creation_time: join_u32(
            legacy.ftCreationTime.dwHighDateTime,
            legacy.ftCreationTime.dwLowDateTime,
        ),
        last_write_time: join_u32(
            legacy.ftLastWriteTime.dwHighDateTime,
            legacy.ftLastWriteTime.dwLowDateTime,
        ),
    })
}

fn file_generation(file: &std::fs::File) -> std::io::Result<[u8; 32]> {
    let metadata = file.metadata()?;
    let mut hasher = Sha256::new();
    hasher.update(metadata.len().to_le_bytes());
    hasher.update([u8::from(metadata.permissions().readonly())]);

    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt as _;

        hasher.update(metadata.dev().to_le_bytes());
        hasher.update(metadata.ino().to_le_bytes());
        hasher.update(metadata.mode().to_le_bytes());
        hasher.update(metadata.nlink().to_le_bytes());
        hasher.update(metadata.mtime().to_le_bytes());
        hasher.update(metadata.mtime_nsec().to_le_bytes());
        hasher.update(metadata.ctime().to_le_bytes());
        hasher.update(metadata.ctime_nsec().to_le_bytes());
    }

    #[cfg(windows)]
    {
        let information = windows_file_information(file)?;
        hasher.update(information.volume.to_le_bytes());
        hasher.update(information.file);
        // FILE_ID_INFO is 128-bit on NTFS/ReFS, while FAT-family volumes may
        // return zero. The legacy index preserves identity on those volumes.
        hasher.update(information.legacy_volume.to_le_bytes());
        hasher.update(information.legacy_file.to_le_bytes());
        hasher.update(information.links.to_le_bytes());
        hasher.update(information.attributes.to_le_bytes());
        hasher.update(information.size.to_le_bytes());
        hasher.update(information.creation_time.to_le_bytes());
        hasher.update(information.last_write_time.to_le_bytes());
    }

    #[cfg(not(any(unix, windows)))]
    if let Ok(modified) = metadata.modified() {
        match modified.duration_since(std::time::UNIX_EPOCH) {
            Ok(duration) => {
                hasher.update([1]);
                hasher.update(duration.as_secs().to_le_bytes());
                hasher.update(duration.subsec_nanos().to_le_bytes());
            }
            Err(error) => {
                let duration = error.duration();
                hasher.update([2]);
                hasher.update(duration.as_secs().to_le_bytes());
                hasher.update(duration.subsec_nanos().to_le_bytes());
            }
        }
    }

    Ok(hasher.finalize().into())
}

fn read_stable_vo_source(path: &Path, max_bytes: usize) -> Result<CapturedVoSource, String> {
    let path_metadata = std::fs::symlink_metadata(path)
        .map_err(|error| format!("failed to inspect {}: {error}", path.display()))?;
    if path_metadata.file_type().is_symlink() || !path_metadata.is_file() {
        return Err(format!(
            "Vo source {} must be a regular file without a symbolic-link leaf",
            path.display()
        ));
    }
    let canonical = std::fs::canonicalize(path)
        .map_err(|error| format!("failed to canonicalize {}: {error}", path.display()))?;
    let mut file = std::fs::File::open(path)
        .map_err(|error| format!("failed to open {}: {error}", path.display()))?;
    let descriptor_metadata = file
        .metadata()
        .map_err(|error| format!("failed to inspect open source {}: {error}", path.display()))?;
    let generation = file_generation(&file)
        .map_err(|error| format!("failed to inspect open source {}: {error}", path.display()))?;
    let path_file = std::fs::File::open(path)
        .map_err(|error| format!("failed to reopen {}: {error}", path.display()))?;
    let path_generation = file_generation(&path_file).map_err(|error| {
        format!(
            "failed to inspect reopened source {}: {error}",
            path.display()
        )
    })?;
    if generation != path_generation {
        return Err(format!(
            "Vo source {} changed identity while being opened",
            path.display()
        ));
    }
    let expected_len = usize::try_from(descriptor_metadata.len()).map_err(|_| {
        format!(
            "Vo source {} is too large for this platform",
            path.display()
        )
    })?;
    if expected_len > max_bytes {
        return Err(format!(
            "Vo source {} exceeds the remaining {max_bytes}-byte package limit",
            path.display()
        ));
    }

    let read_limit = u64::try_from(max_bytes)
        .unwrap_or(u64::MAX)
        .saturating_add(1);
    let mut bytes = Vec::with_capacity(expected_len);
    (&mut file)
        .take(read_limit)
        .read_to_end(&mut bytes)
        .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
    if bytes.len() > max_bytes {
        return Err(format!(
            "Vo source {} exceeds the remaining {max_bytes}-byte package limit",
            path.display()
        ));
    }
    let descriptor_generation_after = file_generation(&file).map_err(|error| {
        format!(
            "failed to re-inspect open source {}: {error}",
            path.display()
        )
    })?;
    let path_after = std::fs::symlink_metadata(path)
        .map_err(|error| format!("failed to re-inspect {}: {error}", path.display()))?;
    let canonical_after = std::fs::canonicalize(path)
        .map_err(|error| format!("failed to re-canonicalize {}: {error}", path.display()))?;
    let path_file_after = std::fs::File::open(path)
        .map_err(|error| format!("failed to reopen {}: {error}", path.display()))?;
    let path_generation_after = file_generation(&path_file_after)
        .map_err(|error| format!("failed to re-inspect {}: {error}", path.display()))?;
    if bytes.len() != expected_len
        || descriptor_generation_after != generation
        || path_after.file_type().is_symlink()
        || !path_after.is_file()
        || path_generation_after != generation
        || canonical_after != canonical
    {
        return Err(format!(
            "Vo source {} changed identity or content while being captured",
            path.display()
        ));
    }
    let content = String::from_utf8(bytes).map_err(|error| {
        format!(
            "Vo source {} is not valid UTF-8: {}",
            path.display(),
            error.utf8_error()
        )
    })?;
    Ok(CapturedVoSource {
        path: canonical,
        content,
    })
}

fn capture_package_sources(pkg_dir: &Path) -> Result<CapturedVoPackage, String> {
    let source_paths = list_vo_source_paths(pkg_dir)?;
    let mut sources = Vec::with_capacity(source_paths.len());
    let mut remaining = vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES;
    for path in &source_paths {
        let source = read_stable_vo_source(path, remaining)?;
        remaining = remaining.checked_sub(source.content.len()).ok_or_else(|| {
            format!(
                "Vo package {} source size accounting underflow",
                pkg_dir.display()
            )
        })?;
        sources.push(source);
    }
    let final_paths = list_vo_source_paths(pkg_dir)?;
    if final_paths != source_paths {
        return Err(format!(
            "Vo package {} source membership changed while being captured",
            pkg_dir.display()
        ));
    }

    let mut hasher = Sha256::new();
    hash_source_snapshot_field(&mut hasher, b"vo-ffi-package-source-snapshot-v1")?;
    hash_source_snapshot_field(&mut hasher, &sources.len().to_le_bytes())?;
    for source in &sources {
        hash_source_snapshot_field(&mut hasher, source.path.as_os_str().as_encoded_bytes())?;
        hash_source_snapshot_field(&mut hasher, source.content.as_bytes())?;
    }
    Ok(CapturedVoPackage {
        sources,
        source_digest: hasher.finalize().into(),
    })
}

fn parse_captured_package(captured: &CapturedVoPackage) -> Result<ParsedPackage, String> {
    let mut files = Vec::with_capacity(captured.sources.len());
    let mut package_name: Option<String> = None;
    let mut type_declarations: HashMap<String, PathBuf> = HashMap::new();
    let mut extern_declarations: HashMap<String, PathBuf> = HashMap::new();

    for source in &captured.sources {
        let path = &source.path;
        let (file, diagnostics, interner) = vo_syntax::parse(&source.content, 0);
        if diagnostics.has_errors() {
            let detail = diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.is_error())
                .map(|diagnostic| diagnostic.message.as_str())
                .collect::<Vec<_>>()
                .join("; ");
            return Err(format!("failed to parse {}: {detail}", path.display()));
        }

        let current_package = file
            .package
            .as_ref()
            .and_then(|package| interner.resolve(package.symbol))
            .filter(|name| !name.is_empty())
            .ok_or_else(|| format!("{} has no package declaration", path.display()))?
            .to_string();
        if let Some(expected) = &package_name {
            if expected != &current_package {
                return Err(format!(
                    "package declaration mismatch: {} declares `{}`, expected `{}`",
                    path.display(),
                    current_package,
                    expected
                ));
            }
        } else {
            package_name = Some(current_package);
        }

        for declaration in &file.decls {
            let (kind, name) = match declaration {
                ast::Decl::Type(type_decl) => (
                    "type",
                    interner.resolve(type_decl.name.symbol).unwrap_or_default(),
                ),
                ast::Decl::Func(func_decl) if func_decl.is_extern() => (
                    "extern function",
                    interner.resolve(func_decl.name.symbol).unwrap_or_default(),
                ),
                _ => continue,
            };
            if name.is_empty() {
                return Err(format!(
                    "{} contains an unnamed {kind} declaration",
                    path.display()
                ));
            }
            let declarations = if kind == "type" {
                &mut type_declarations
            } else {
                &mut extern_declarations
            };
            if let Some(previous) = declarations.insert(name.to_string(), path.clone()) {
                return Err(format!(
                    "duplicate {kind} declaration `{name}` in {} and {}",
                    previous.display(),
                    path.display()
                ));
            }
        }

        files.push(ParsedVoFile {
            path: path.clone(),
            file,
            interner,
        });
    }

    Ok(ParsedPackage {
        name: package_name.expect("a non-empty source list sets the package name"),
        files,
    })
}

fn parse_package(pkg_dir: &Path) -> Result<ParsedPackage, String> {
    let captured = capture_package_sources(pkg_dir)?;
    parse_captured_package(&captured)
}

/// Parse all type aliases from a package directory.
/// Returns a map from type name to underlying type.
#[cfg(test)]
pub fn parse_type_aliases(pkg_dir: &Path) -> Result<HashMap<String, VoType>, String> {
    let package = parse_package(pkg_dir)?;
    parse_type_aliases_from_package(&package)
}

fn parse_type_aliases_from_package(
    package: &ParsedPackage,
) -> Result<HashMap<String, VoType>, String> {
    let mut aliases = HashMap::new();
    let mut constants = ConstantEvaluator::new(&package.files)?;
    for parsed in &package.files {
        parse_type_aliases_from_ast(
            &parsed.file,
            &mut aliases,
            &parsed.interner,
            &parsed.path,
            &mut constants,
        )?;
    }
    Ok(aliases)
}

/// Capture and parse every layout-relevant view of one package exactly once.
pub(crate) fn parse_layout_package(pkg_dir: &Path) -> Result<ParsedLayoutPackage, String> {
    let captured = capture_package_sources(pkg_dir)?;
    let source_paths = captured
        .sources
        .iter()
        .map(|source| source.path.clone())
        .collect();
    let source_digest = captured.source_digest;
    let package = parse_captured_package(&captured)?;
    let aliases = parse_type_aliases_from_package(&package)?;
    let imports = imports_from_parsed_package(&package);
    Ok(ParsedLayoutPackage {
        name: package.name,
        aliases,
        imports,
        source_paths,
        source_digest,
    })
}

/// Parse type aliases from AST using vo-syntax parser.
fn parse_type_aliases_from_ast(
    file: &ast::File,
    aliases: &mut HashMap<String, VoType>,
    interner: &SymbolInterner,
    path: &Path,
    constants: &mut ConstantEvaluator,
) -> Result<(), String> {
    for decl in &file.decls {
        if let ast::Decl::Type(type_decl) = decl {
            if let Some(name) = interner.resolve(type_decl.name.symbol) {
                let ty =
                    type_expr_to_vo_type(&type_decl.ty, interner, constants).map_err(|error| {
                        format!(
                            "cannot determine FFI layout for type `{name}` in {}: {error}",
                            path.display()
                        )
                    })?;
                aliases.insert(name.to_string(), ty);
            }
        }
    }
    Ok(())
}

/// Convert a parsed Vo type to the FFI layout model.
fn type_expr_to_vo_type(
    type_expr: &ast::TypeExpr,
    interner: &SymbolInterner,
    constants: &mut ConstantEvaluator,
) -> Result<VoType, String> {
    Ok(match &type_expr.kind {
        TypeExprKind::Ident(ident) => {
            let name = interner.resolve(ident.symbol).unwrap_or("");
            match name {
                "int" => VoType::Int,
                "int8" => VoType::Int8,
                "int16" => VoType::Int16,
                "int32" | "rune" => VoType::Int32,
                "int64" => VoType::Int64,
                "uint" => VoType::Uint,
                "uint8" | "byte" => VoType::Uint8,
                "uint16" => VoType::Uint16,
                "uint32" => VoType::Uint32,
                "uint64" => VoType::Uint64,
                "float32" => VoType::Float32,
                "float64" => VoType::Float64,
                "bool" => VoType::Bool,
                "string" => VoType::String,
                "any" => VoType::Any,
                "error" => VoType::Error,
                _ => VoType::Named(name.to_string()),
            }
        }
        TypeExprKind::Selector(sel) => {
            let pkg = interner.resolve(sel.pkg.symbol).unwrap_or("");
            let name = interner.resolve(sel.sel.symbol).unwrap_or("");
            VoType::Named(format!("{}.{}", pkg, name))
        }
        TypeExprKind::Array(arr) => {
            let len = parse_array_length(&arr.len, interner, constants)?;
            let elem = type_expr_to_vo_type(&arr.elem, interner, constants)?;
            VoType::Array(len, Box::new(elem))
        }
        TypeExprKind::Slice(elem) => {
            let elem_type = type_expr_to_vo_type(elem, interner, constants)?;
            VoType::Slice(Box::new(elem_type))
        }
        TypeExprKind::Map(map) => {
            let key = type_expr_to_vo_type(&map.key, interner, constants)?;
            let value = type_expr_to_vo_type(&map.value, interner, constants)?;
            VoType::Map(Box::new(key), Box::new(value))
        }
        TypeExprKind::Chan(chan) => {
            let elem = type_expr_to_vo_type(&chan.elem, interner, constants)?;
            let dir = match chan.dir {
                ast::ChanDir::Both => ChanDir::Both,
                ast::ChanDir::Send => ChanDir::Send,
                ast::ChanDir::Recv => ChanDir::Recv,
            };
            VoType::Chan(dir, Box::new(elem))
        }
        TypeExprKind::Port(port) => {
            let elem = type_expr_to_vo_type(&port.elem, interner, constants)?;
            let dir = match port.dir {
                ast::ChanDir::Both => ChanDir::Both,
                ast::ChanDir::Send => ChanDir::Send,
                ast::ChanDir::Recv => ChanDir::Recv,
            };
            VoType::Port(dir, Box::new(elem))
        }
        TypeExprKind::Func(func) => {
            let mut params = Vec::new();
            for param in &func.params {
                let ty = type_expr_to_vo_type(&param.ty, interner, constants)?;
                params.extend(std::iter::repeat_n(ty, param.names.len().max(1)));
            }
            let mut results = Vec::with_capacity(func.results.len());
            for result in &func.results {
                let ty = type_expr_to_vo_type(&result.ty, interner, constants)?;
                results.extend(std::iter::repeat_n(ty, result.names.len().max(1)));
            }
            VoType::Func(params, results)
        }
        TypeExprKind::Struct(struct_type) => {
            let mut field_types = Vec::new();
            for field in &struct_type.fields {
                let ty = type_expr_to_vo_type(&field.ty, interner, constants)?;
                field_types.extend(std::iter::repeat_n(ty, field.names.len().max(1)));
            }
            VoType::Struct(field_types)
        }
        TypeExprKind::Pointer(inner) => {
            let inner_type = type_expr_to_vo_type(inner, interner, constants)?;
            VoType::Pointer(Box::new(inner_type))
        }
        TypeExprKind::Interface(_) => VoType::Any, // interface is 2 slots like any
        TypeExprKind::Island => VoType::Island,
    })
}

fn parse_array_length(
    expr: &ast::Expr,
    interner: &SymbolInterner,
    constants: &mut ConstantEvaluator,
) -> Result<usize, String> {
    constants.array_length(expr, interner)
}

/// Find and parse extern functions from a package directory using vo-syntax parser.
pub fn find_extern_func(pkg_dir: &Path, func_name: &str) -> Result<VoFuncSig, String> {
    let package = parse_package(pkg_dir)?;
    let mut constants = ConstantEvaluator::new(&package.files)?;
    for parsed in &package.files {
        for decl in &parsed.file.decls {
            if let ast::Decl::Func(func_decl) = decl {
                let name = parsed.interner.resolve(func_decl.name.symbol).unwrap_or("");
                if name == func_name && func_decl.is_extern() {
                    return func_decl_to_vo_sig(func_decl, &parsed.interner, &mut constants)
                        .map_err(|error| {
                            format!(
                                "cannot determine FFI signature for extern `{func_name}` in {}: {error}",
                                parsed.path.display()
                            )
                        });
                }
            }
        }
    }
    Err(format!(
        "extern function `{func_name}` not found in package `{}` ({})",
        package.name,
        pkg_dir.display()
    ))
}

/// Convert vo-syntax FuncDecl to VoFuncSig.
fn func_decl_to_vo_sig(
    func_decl: &ast::FuncDecl,
    interner: &SymbolInterner,
    constants: &mut ConstantEvaluator,
) -> Result<VoFuncSig, String> {
    let name = interner
        .resolve(func_decl.name.symbol)
        .unwrap_or("")
        .to_string();

    let mut params = Vec::new();
    for param in &func_decl.sig.params {
        let ty = type_expr_to_vo_type(&param.ty, interner, constants)?;
        if param.names.is_empty() {
            params.push(VoParam {
                name: String::new(),
                ty,
            });
        } else {
            for param_name in &param.names {
                params.push(VoParam {
                    name: interner
                        .resolve(param_name.symbol)
                        .unwrap_or_default()
                        .to_string(),
                    ty: ty.clone(),
                });
            }
        }
    }

    // Handle variadic: wrap last param type in Variadic
    let params = if func_decl.sig.variadic && !params.is_empty() {
        let mut params = params;
        if let Some(last) = params.last_mut() {
            last.ty = VoType::Variadic(Box::new(last.ty.clone()));
        }
        params
    } else {
        params
    };

    let mut results = Vec::with_capacity(func_decl.sig.results.len());
    for result in &func_decl.sig.results {
        let ty = type_expr_to_vo_type(&result.ty, interner, constants)?;
        // The syntax parser expands each Go-style grouped result name into an
        // individual ResultParam; anonymous results also occupy one entry.
        results.push(ty);
    }

    Ok(VoFuncSig {
        name,
        params,
        results,
    })
}

fn imports_from_parsed_package(package: &ParsedPackage) -> Vec<VoImport> {
    let mut imports = Vec::new();
    for parsed in &package.files {
        for import in &parsed.file.imports {
            let path_str = import.path.value.clone();
            let alias = import.alias.as_ref().map(|alias| {
                parsed
                    .interner
                    .resolve(alias.symbol)
                    .unwrap_or("")
                    .to_string()
            });
            imports.push(VoImport {
                alias,
                path: path_str,
            });
        }
    }
    imports
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicU64, Ordering};

    use super::*;

    static NEXT_DIR: AtomicU64 = AtomicU64::new(0);

    struct TempPackage(PathBuf);

    impl TempPackage {
        fn new(label: &str) -> Self {
            let sequence = NEXT_DIR.fetch_add(1, Ordering::Relaxed);
            let path = std::env::temp_dir().join(format!(
                "volang-ffi-parser-{label}-{}-{sequence}",
                std::process::id()
            ));
            std::fs::create_dir(&path).unwrap();
            Self(path)
        }

        fn write(&self, name: &str, source: &str) {
            std::fs::write(self.0.join(name), source).unwrap();
        }
    }

    impl Drop for TempPackage {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn slot_count_rejects_array_struct_overflow_and_alias_cycles() {
        let aliases = HashMap::new();
        assert!(VoType::Array(65_536, Box::new(VoType::Int))
            .slot_count(&aliases)
            .is_err());
        assert!(VoType::Struct(vec![
            VoType::Array(40_000, Box::new(VoType::Int)),
            VoType::Array(40_000, Box::new(VoType::Int)),
        ])
        .slot_count(&aliases)
        .is_err());

        let aliases = HashMap::from([
            ("A".to_string(), VoType::Named("B".to_string())),
            ("B".to_string(), VoType::Named("A".to_string())),
        ]);
        let error = VoType::Named("A".into()).slot_count(&aliases).unwrap_err();
        assert!(error.contains("A -> B -> A"), "{error}");

        let error = VoType::Named("Missing".into())
            .slot_count(&HashMap::new())
            .unwrap_err();
        assert!(error.contains("unresolved named type `Missing`"), "{error}");
    }

    #[test]
    fn package_parser_is_sorted_strict_and_parses_full_integer_literal_grammar() {
        let package = TempPackage::new("strict");
        package.write("z.vo", "package sample\nfunc Z(v [0x1_0]int)\n");
        package.write("a.vo", "package sample\ntype Buffer [0o10]byte\n");

        let paths = list_vo_source_paths(&package.0).unwrap();
        assert_eq!(paths[0].file_name().unwrap(), "a.vo");
        assert_eq!(paths[1].file_name().unwrap(), "z.vo");
        let signature = find_extern_func(&package.0, "Z").unwrap();
        assert_eq!(
            signature.params[0].ty,
            VoType::Array(16, Box::new(VoType::Int))
        );
        assert_eq!(
            parse_type_aliases(&package.0).unwrap()["Buffer"],
            VoType::Array(8, Box::new(VoType::Uint8))
        );
    }

    #[test]
    fn captured_layout_parser_does_not_reopen_an_aba_rebound_source_path() {
        let package = TempPackage::new("captured-source-aba");
        package.write("sample.vo", "package sample\ntype Value [2]int\n");
        let source_path = package.0.join("sample.vo");
        let saved_path = package.0.join("saved.source");
        let captured = capture_package_sources(&package.0).unwrap();
        let expected_digest = captured.source_digest;

        std::fs::rename(&source_path, &saved_path).unwrap();
        std::fs::write(&source_path, "package sample\ntype Value [9]int\n").unwrap();
        let live = parse_layout_package(&package.0).unwrap();
        assert_eq!(
            live.aliases["Value"],
            VoType::Array(9, Box::new(VoType::Int))
        );

        let parsed = parse_captured_package(&captured).unwrap();
        let captured_aliases = parse_type_aliases_from_package(&parsed).unwrap();
        assert_eq!(
            captured_aliases["Value"],
            VoType::Array(2, Box::new(VoType::Int))
        );

        std::fs::remove_file(&source_path).unwrap();
        std::fs::rename(&saved_path, &source_path).unwrap();
        let restored = capture_package_sources(&package.0).unwrap();
        assert_eq!(restored.source_digest, expected_digest);
    }

    #[cfg(windows)]
    #[test]
    fn windows_generation_matches_hard_links() {
        let package = TempPackage::new("windows-hard-link-identity");
        package.write("source.vo", "package sample\n");
        let source = package.0.join("source.vo");
        let link = package.0.join("linked.vo");
        std::fs::hard_link(&source, &link).unwrap();

        let source = std::fs::File::open(source).unwrap();
        let link = std::fs::File::open(link).unwrap();
        assert_eq!(
            file_generation(&source).unwrap(),
            file_generation(&link).unwrap()
        );
    }

    #[cfg(windows)]
    #[test]
    fn windows_generation_distinguishes_equal_length_files() {
        let package = TempPackage::new("windows-distinct-identity");
        package.write("left.vo", "package left\n");
        package.write("right.vo", "package left\n");

        let left = std::fs::File::open(package.0.join("left.vo")).unwrap();
        let right = std::fs::File::open(package.0.join("right.vo")).unwrap();
        assert_ne!(
            file_generation(&left).unwrap(),
            file_generation(&right).unwrap()
        );
    }

    #[cfg(windows)]
    #[test]
    fn windows_generation_detects_path_rebind() {
        let package = TempPackage::new("windows-rebound-identity");
        package.write("source.vo", "package old\n");
        let source_path = package.0.join("source.vo");
        let saved_path = package.0.join("saved.source");
        let opened = std::fs::File::open(&source_path).unwrap();
        let original_generation = file_generation(&opened).unwrap();

        std::fs::rename(&source_path, saved_path).unwrap();
        package.write("source.vo", "package new\n");
        let rebound = std::fs::File::open(source_path).unwrap();
        assert_ne!(original_generation, file_generation(&rebound).unwrap());
    }

    #[cfg(unix)]
    #[test]
    fn package_capture_rejects_symbolic_link_sources() {
        use std::os::unix::fs::symlink;

        let package = TempPackage::new("symlink-source");
        package.write("real.source", "package sample\ntype Value int\n");
        symlink(package.0.join("real.source"), package.0.join("sample.vo")).unwrap();

        let error = capture_package_sources(&package.0).unwrap_err();
        assert!(error.contains("without a symbolic-link leaf"), "{error}");
    }

    #[test]
    fn package_parser_evaluates_constants_and_rejects_invalid_source() {
        let duplicate = TempPackage::new("duplicate");
        duplicate.write("b.vo", "package sample\nfunc F()\n");
        duplicate.write("a.vo", "package sample\nfunc F()\n");
        let error = find_extern_func(&duplicate.0, "F").unwrap_err();
        assert!(error.contains("duplicate extern function declaration `F`"));
        assert!(error.find("a.vo").unwrap() < error.find("b.vo").unwrap());

        let computed = TempPackage::new("computed-array");
        computed.write(
            "sample.vo",
            "package sample\nconst Base = 3\nconst N = (Base + 1) << 1\nfunc F(v [N]int) (x, y int, ok bool)\ntype Callback func() (a, b int, err error)\n",
        );
        let signature = find_extern_func(&computed.0, "F").unwrap();
        assert_eq!(
            signature.params[0].ty,
            VoType::Array(8, Box::new(VoType::Int))
        );
        assert_eq!(
            signature.results,
            vec![VoType::Int, VoType::Int, VoType::Bool]
        );
        assert_eq!(
            parse_type_aliases(&computed.0).unwrap()["Callback"],
            VoType::Func(Vec::new(), vec![VoType::Int, VoType::Int, VoType::Error])
        );

        let unknown = TempPackage::new("unknown-array-constant");
        unknown.write("sample.vo", "package sample\nfunc F(v [Missing]int)\n");
        let error = find_extern_func(&unknown.0, "F").unwrap_err();
        assert!(
            error.contains("unresolved package constant `Missing`"),
            "{error}"
        );

        let cyclic = TempPackage::new("cyclic-array-constant");
        cyclic.write(
            "sample.vo",
            "package sample\nconst A = B\nconst B = A\nfunc F(v [A]int)\n",
        );
        let error = find_extern_func(&cyclic.0, "F").unwrap_err();
        assert!(error.contains("A -> B -> A"), "{error}");

        let malformed = TempPackage::new("malformed");
        malformed.write("sample.vo", "package sample\nfunc F(\n");
        assert!(find_extern_func(&malformed.0, "F")
            .unwrap_err()
            .contains("failed to parse"));
    }

    #[test]
    fn package_constant_evaluator_preserves_resource_limit_diagnostics() {
        let oversized_shift = TempPackage::new("oversized-shift");
        oversized_shift.write(
            "sample.vo",
            "package sample\nconst N = 1 << 65536\nfunc F(v [N]int)\n",
        );
        let error = find_extern_func(&oversized_shift.0, "F").unwrap_err();
        assert!(
            error.contains("array length shift count exceeds the language limit of 65535"),
            "{error}"
        );

        let oversized_operation = TempPackage::new("oversized-operation");
        oversized_operation.write(
            "sample.vo",
            "package sample\nconst N = (1 << 65535) << 1\nfunc F(v [N]int)\n",
        );
        let error = find_extern_func(&oversized_operation.0, "F").unwrap_err();
        assert!(
            error.contains("exact-arithmetic limit of 65536 bits"),
            "{error}"
        );

        let oversized_literal = TempPackage::new("oversized-literal");
        let source = format!(
            "package sample\nconst N = 0x{}\nfunc F(v [N]int)\n",
            "f".repeat(16_385)
        );
        oversized_literal.write("sample.vo", &source);
        let error = find_extern_func(&oversized_literal.0, "F").unwrap_err();
        assert!(
            error.contains("exact-arithmetic limit of 65536 bits"),
            "{error}"
        );
    }

    #[test]
    fn package_constant_evaluator_bounds_aggregate_work_and_retention() {
        let mut evaluator = ConstantEvaluator {
            definitions: HashMap::from([(
                "N".to_string(),
                ConstDefinition {
                    expression: Ok(ConstExpr::Int("1".to_string())),
                    iota: 0,
                },
            )]),
            values: HashMap::new(),
            stack: Vec::new(),
            work_bytes: MAX_CONSTANT_FOLD_WORK_BYTES - 16,
        };

        assert_eq!(evaluator.eval_named("N").unwrap(), ConstValue::Int64(1));
        assert_eq!(evaluator.work_bytes, MAX_CONSTANT_FOLD_WORK_BYTES);
        assert!(evaluator.values.contains_key("N"));

        let error = evaluator
            .eval_expr(&ConstExpr::Ident("N".to_string()), None)
            .unwrap_err();
        assert!(
            error.contains("constant-folding work exceeds the per-package limit of 67108864 bytes"),
            "{error}"
        );

        evaluator.work_bytes = u64::MAX;
        let error = evaluator.charge(&[&constant::make_int64(1)]).unwrap_err();
        assert!(error.contains("per-package limit"), "{error}");
    }
}
