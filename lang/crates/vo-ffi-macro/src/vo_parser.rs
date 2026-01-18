//! Simple Vo function signature parser for extern function validation.
//!
//! This is a minimal parser that only extracts function signatures from .vo files.
//! It does not attempt to parse the full Vo syntax.

use std::path::Path;

/// A parsed Vo function signature.
#[derive(Debug, Clone)]
pub struct VoFuncSig {
    pub name: String,
    pub params: Vec<VoParam>,
    pub results: Vec<VoType>,
}

impl VoFuncSig {
    /// Create a placeholder signature from a Rust function.
    /// Used when Vo signature parsing fails (e.g., variadic functions).
    pub fn from_rust_fn(func: &syn::ItemFn) -> Self {
        let params = func.sig.inputs.iter().filter_map(|arg| {
            if let syn::FnArg::Typed(pat_type) = arg {
                let name = if let syn::Pat::Ident(ident) = &*pat_type.pat {
                    ident.ident.to_string()
                } else {
                    String::new()
                };
                let ty = rust_type_to_vo(&pat_type.ty);
                Some(VoParam { name, ty })
            } else {
                None
            }
        }).collect();

        let results = match &func.sig.output {
            syn::ReturnType::Default => Vec::new(),
            syn::ReturnType::Type(_, ty) => {
                if let syn::Type::Tuple(tuple) = &**ty {
                    tuple.elems.iter().map(rust_type_to_vo).collect()
                } else {
                    vec![rust_type_to_vo(ty)]
                }
            }
        };

        Self {
            name: func.sig.ident.to_string(),
            params,
            results,
        }
    }
}

fn rust_type_to_vo(ty: &syn::Type) -> VoType {
    match ty {
        syn::Type::Path(p) => {
            let ident = p.path.segments.last()
                .map(|s| s.ident.to_string())
                .unwrap_or_default();
            match ident.as_str() {
                "i64" | "isize" => VoType::Int64,
                "i32" => VoType::Int32,
                "i16" => VoType::Int16,
                "i8" => VoType::Int8,
                "u64" | "usize" => VoType::Uint64,
                "u32" => VoType::Uint32,
                "u16" => VoType::Uint16,
                "u8" => VoType::Uint8,
                "f64" => VoType::Float64,
                "f32" => VoType::Float32,
                "bool" => VoType::Bool,
                "String" => VoType::String,
                _ => VoType::Any,
            }
        }
        syn::Type::Reference(r) => {
            if let syn::Type::Path(p) = &*r.elem {
                let ident = p.path.segments.last()
                    .map(|s| s.ident.to_string())
                    .unwrap_or_default();
                if ident == "str" {
                    return VoType::String;
                }
            }
            VoType::Any
        }
        _ => VoType::Any,
    }
}

/// A Vo function parameter.
#[derive(Debug, Clone)]
pub struct VoParam {
    pub name: String,
    pub ty: VoType,
}

/// A Vo type.
#[derive(Debug, Clone)]
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
    // Composite types
    Named(String),
    Pointer(Box<VoType>),
    Slice(Box<VoType>),
    Array(usize, Box<VoType>),
    Map(Box<VoType>, Box<VoType>),
    Chan(ChanDir, Box<VoType>),
    Func(Vec<VoType>, Vec<VoType>),
    /// Variadic parameter: ...T (e.g., ...interface{})
    Variadic(Box<VoType>),
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
            VoType::Named(name) => write!(f, "{}", name),
            VoType::Pointer(inner) => write!(f, "*{}", inner),
            VoType::Slice(inner) => write!(f, "[]{}", inner),
            VoType::Array(len, inner) => write!(f, "[{}]{}", len, inner),
            VoType::Map(k, v) => write!(f, "map[{}]{}", k, v),
            VoType::Chan(dir, inner) => {
                match dir {
                    ChanDir::Both => write!(f, "chan {}", inner),
                    ChanDir::Send => write!(f, "chan<- {}", inner),
                    ChanDir::Recv => write!(f, "<-chan {}", inner),
                }
            }
            VoType::Func(params, results) => {
                write!(f, "func(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", p)?;
                }
                write!(f, ")")?;
                if !results.is_empty() {
                    if results.len() == 1 {
                        write!(f, " {}", results[0])?;
                    } else {
                        write!(f, " (")?;
                        for (i, r) in results.iter().enumerate() {
                            if i > 0 { write!(f, ", ")?; }
                            write!(f, "{}", r)?;
                        }
                        write!(f, ")")?;
                    }
                }
                Ok(())
            }
            VoType::Variadic(inner) => write!(f, "...{}", inner),
        }
    }
}

impl VoType {
    /// Get the number of stack slots this type occupies.
    pub fn slot_count(&self, type_aliases: &std::collections::HashMap<String, VoType>) -> u16 {
        match self {
            // Primitive types: 1 slot each
            VoType::Int | VoType::Int8 | VoType::Int16 | VoType::Int32 | VoType::Int64 |
            VoType::Uint | VoType::Uint8 | VoType::Uint16 | VoType::Uint32 | VoType::Uint64 |
            VoType::Float32 | VoType::Float64 | VoType::Bool | VoType::String => 1,
            
            // Any/interface: 2 slots (slot0=metadata, slot1=data)
            VoType::Any => 2,
            
            // Reference types: 1 slot (GcRef)
            VoType::Pointer(_) | VoType::Slice(_) | VoType::Map(_, _) | 
            VoType::Chan(_, _) | VoType::Func(_, _) => 1,
            
            // Array: elem_slots * length
            VoType::Array(len, elem) => {
                let elem_slots = elem.slot_count(type_aliases);
                elem_slots * (*len as u16)
            }
            
            // Named type: resolve alias
            VoType::Named(name) => {
                // Check for well-known types
                match name.as_str() {
                    "error" => 2, // error is an interface
                    _ => {
                        // Try to resolve type alias
                        if let Some(underlying) = type_aliases.get(name) {
                            underlying.slot_count(type_aliases)
                        } else {
                            // Unknown named type, assume it's a reference type (1 slot)
                            // This handles struct types passed by reference
                            1
                        }
                    }
                }
            }
            
            // Variadic: treated as slice (1 slot)
            VoType::Variadic(_) => 1,
        }
    }

    /// Parse a type string.
    pub fn parse(s: &str) -> Option<Self> {
        let s = s.trim();
        
        // Primitive types
        match s {
            "int" => return Some(VoType::Int),
            "int8" => return Some(VoType::Int8),
            "int16" => return Some(VoType::Int16),
            "int32" | "rune" => return Some(VoType::Int32),  // rune is alias for int32
            "int64" => return Some(VoType::Int64),
            "uint" => return Some(VoType::Uint),
            "uint8" | "byte" => return Some(VoType::Uint8),
            "uint16" => return Some(VoType::Uint16),
            "uint32" => return Some(VoType::Uint32),
            "uint64" => return Some(VoType::Uint64),
            "float32" => return Some(VoType::Float32),
            "float64" => return Some(VoType::Float64),
            "bool" => return Some(VoType::Bool),
            "string" => return Some(VoType::String),
            "any" | "interface{}" => return Some(VoType::Any),
            _ => {}
        }
        
        // Variadic: ...T
        if let Some(rest) = s.strip_prefix("...") {
            return VoType::parse(rest).map(|t| VoType::Variadic(Box::new(t)));
        }
        
        // Pointer: *T
        if let Some(rest) = s.strip_prefix('*') {
            return VoType::parse(rest).map(|t| VoType::Pointer(Box::new(t)));
        }
        
        // Receive-only channel: <-chan T
        if let Some(rest) = s.strip_prefix("<-chan") {
            return VoType::parse(rest).map(|t| VoType::Chan(ChanDir::Recv, Box::new(t)));
        }
        
        // Send-only channel: chan<- T
        if let Some(rest) = s.strip_prefix("chan<-") {
            return VoType::parse(rest).map(|t| VoType::Chan(ChanDir::Send, Box::new(t)));
        }
        
        // Bidirectional channel: chan T
        if let Some(rest) = s.strip_prefix("chan") {
            let rest = rest.trim_start();
            if !rest.is_empty() {
                return VoType::parse(rest).map(|t| VoType::Chan(ChanDir::Both, Box::new(t)));
            }
        }
        
        // Map: map[K]V
        if let Some(rest) = s.strip_prefix("map[") {
            if let Some(bracket_end) = find_matching_bracket(rest, '[', ']') {
                let key_str = &rest[..bracket_end];
                let val_str = &rest[bracket_end + 1..];
                let key = VoType::parse(key_str)?;
                let val = VoType::parse(val_str)?;
                return Some(VoType::Map(Box::new(key), Box::new(val)));
            }
        }
        
        // Slice: []T
        if let Some(rest) = s.strip_prefix("[]") {
            return VoType::parse(rest).map(|t| VoType::Slice(Box::new(t)));
        }
        
        // Array: [N]T
        if s.starts_with('[') {
            if let Some(bracket_end) = s.find(']') {
                let size_str = &s[1..bracket_end];
                if let Ok(size) = size_str.parse::<usize>() {
                    let elem_str = &s[bracket_end + 1..];
                    return VoType::parse(elem_str).map(|t| VoType::Array(size, Box::new(t)));
                }
            }
        }
        
        // Func: func(...) ...
        if let Some(rest) = s.strip_prefix("func") {
            return parse_func_type(rest.trim_start());
        }
        
        // Named type (identifier)
        if !s.is_empty() && (s.chars().next().unwrap().is_alphabetic() || s.starts_with('_')) {
            return Some(VoType::Named(s.to_string()));
        }
        
        None
    }

    /// Get the expected Rust type for this Vo type.
    pub fn expected_rust_type(&self) -> &'static str {
        match self {
            VoType::Int | VoType::Int64 => "i64",
            VoType::Int8 => "i8",
            VoType::Int16 => "i16",
            VoType::Int32 => "i32",
            VoType::Uint | VoType::Uint64 => "u64",
            VoType::Uint8 => "u8",
            VoType::Uint16 => "u16",
            VoType::Uint32 => "u32",
            VoType::Float32 => "f32",
            VoType::Float64 => "f64",
            VoType::Bool => "bool",
            VoType::String => "&str",
            VoType::Any => "any",
            VoType::Pointer(_) => "GcRef",
            VoType::Slice(inner) => match inner.as_ref() {
                VoType::Uint8 => "&[u8]",
                _ => "GcRef",
            },
            VoType::Array(_, _) => "GcRef",
            VoType::Map(_, _) => "GcRef",
            VoType::Chan(_, _) => "GcRef",
            VoType::Func(_, _) => "GcRef",
            VoType::Named(_) => "GcRef",
            VoType::Variadic(_) => "variadic",
        }
    }

}

/// Find matching bracket, handling nesting.
fn find_matching_bracket(s: &str, open: char, close: char) -> Option<usize> {
    let mut depth = 1;
    for (i, c) in s.char_indices() {
        if c == open {
            depth += 1;
        } else if c == close {
            depth -= 1;
            if depth == 0 {
                return Some(i);
            }
        }
    }
    None
}

/// Parse a function type: (params) result
fn parse_func_type(s: &str) -> Option<VoType> {
    // Expect (params) result
    if !s.starts_with('(') {
        return None;
    }
    
    let paren_end = find_matching_bracket(&s[1..], '(', ')')? + 1;
    let params_str = &s[1..paren_end];
    let result_str = s[paren_end + 1..].trim();
    
    // Parse params
    let params = if params_str.is_empty() {
        Vec::new()
    } else {
        params_str.split(',')
            .filter_map(|p| VoType::parse(p.trim()))
            .collect()
    };
    
    // Parse results
    let results = if result_str.is_empty() {
        Vec::new()
    } else if result_str.starts_with('(') && result_str.ends_with(')') {
        // Multiple results: (T1, T2)
        let inner = &result_str[1..result_str.len() - 1];
        inner.split(',')
            .filter_map(|r| VoType::parse(r.trim()))
            .collect()
    } else {
        // Single result
        vec![VoType::parse(result_str)?]
    };
    
    Some(VoType::Func(params, results))
}

/// Parsed struct field information.
#[derive(Debug, Clone)]
pub struct VoStructField {
    pub name: String,
    pub ty: VoType,
}

/// Parsed struct definition.
#[derive(Debug, Clone)]
pub struct VoStructDef {
    #[allow(dead_code)]
    pub name: String,
    pub fields: Vec<VoStructField>,
}

impl VoStructDef {
    /// Calculate field offsets based on type slot counts.
    pub fn field_offsets(&self, type_aliases: &std::collections::HashMap<String, VoType>) -> Vec<u16> {
        let mut offsets = Vec::new();
        let mut current_offset: u16 = 0;
        for field in &self.fields {
            offsets.push(current_offset);
            current_offset += field.ty.slot_count(type_aliases);
        }
        offsets
    }
    
    /// Get total slot count for this struct.
    pub fn total_slots(&self, type_aliases: &std::collections::HashMap<String, VoType>) -> u16 {
        self.fields.iter().map(|f| f.ty.slot_count(type_aliases)).sum()
    }
}

/// Find and parse a struct definition from a package directory.
pub fn find_struct_def(pkg_dir: &Path, struct_name: &str) -> Result<VoStructDef, String> {
    let entries = std::fs::read_dir(pkg_dir)
        .map_err(|e| format!("cannot read directory {:?}: {}", pkg_dir, e))?;
    
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().map(|e| e == "vo").unwrap_or(false) {
            if let Ok(content) = std::fs::read_to_string(&path) {
                if let Some(def) = parse_struct_def(&content, struct_name) {
                    return Ok(def);
                }
            }
        }
    }
    
    Err(format!("struct '{}' not found in {:?}", struct_name, pkg_dir))
}

/// Parse a struct definition from source code.
fn parse_struct_def(source: &str, struct_name: &str) -> Option<VoStructDef> {
    let lines: Vec<&str> = source.lines().collect();
    
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        
        // Look for "type StructName struct {"
        if let Some(rest) = trimmed.strip_prefix("type ") {
            let rest = rest.trim();
            if let Some(after_name) = rest.strip_prefix(struct_name) {
                let after_name = after_name.trim();
                if after_name.starts_with("struct") {
                    // Found the struct, now parse fields
                    let mut fields = Vec::new();
                    let mut j = i + 1;
                    
                    while j < lines.len() {
                        let field_line = lines[j].trim();
                        
                        // End of struct
                        if field_line == "}" || field_line.starts_with("}") {
                            break;
                        }
                        
                        // Skip empty lines and comments
                        if field_line.is_empty() || field_line.starts_with("//") {
                            j += 1;
                            continue;
                        }
                        
                        // Parse field: "FieldName Type" or "FieldName Type // comment"
                        let field_line = field_line.split("//").next().unwrap_or(field_line).trim();
                        let parts: Vec<&str> = field_line.splitn(2, char::is_whitespace).collect();
                        if parts.len() == 2 {
                            let field_name = parts[0].trim();
                            let field_type_str = parts[1].trim();
                            if let Some(field_ty) = VoType::parse(field_type_str) {
                                fields.push(VoStructField {
                                    name: field_name.to_string(),
                                    ty: field_ty,
                                });
                            }
                        }
                        
                        j += 1;
                    }
                    
                    return Some(VoStructDef {
                        name: struct_name.to_string(),
                        fields,
                    });
                }
            }
        }
    }
    
    None
}

/// Parse all type aliases from a package directory.
/// Returns a map from type name to underlying type.
pub fn parse_type_aliases(pkg_dir: &Path) -> std::collections::HashMap<String, VoType> {
    let mut aliases = std::collections::HashMap::new();
    
    let entries = match std::fs::read_dir(pkg_dir) {
        Ok(e) => e,
        Err(_) => return aliases,
    };
    
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().map(|e| e == "vo").unwrap_or(false) {
            if let Ok(content) = std::fs::read_to_string(&path) {
                parse_type_aliases_from_source(&content, &mut aliases);
            }
        }
    }
    
    aliases
}

/// Parse type aliases from Vo source code.
fn parse_type_aliases_from_source(source: &str, aliases: &mut std::collections::HashMap<String, VoType>) {
    for line in source.lines() {
        let trimmed = line.trim();
        
        // Look for "type Name underlying" pattern (simple type alias)
        if let Some(rest) = trimmed.strip_prefix("type ") {
            let rest = rest.trim();
            
            // Skip struct definitions: "type Name struct {"
            if rest.contains("struct") || rest.contains("interface") {
                continue;
            }
            
            // Parse "Name Type"
            let parts: Vec<&str> = rest.splitn(2, char::is_whitespace).collect();
            if parts.len() == 2 {
                let name = parts[0].trim();
                let type_str = parts[1].trim();
                
                if let Some(ty) = VoType::parse(type_str) {
                    aliases.insert(name.to_string(), ty);
                }
            }
        }
    }
}

/// Find and parse extern functions from a package directory.
pub fn find_extern_func(pkg_dir: &Path, func_name: &str) -> Result<VoFuncSig, String> {
    // Find all .vo files in the directory
    let entries = std::fs::read_dir(pkg_dir)
        .map_err(|e| format!("cannot read directory {:?}: {}", pkg_dir, e))?;
    
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().map(|e| e == "vo").unwrap_or(false) {
            if let Ok(content) = std::fs::read_to_string(&path) {
                if let Some(sig) = parse_extern_func(&content, func_name) {
                    return Ok(sig);
                }
            }
        }
    }
    
    Err(format!("extern function '{}' not found in {:?}", func_name, pkg_dir))
}

/// Parse a single extern function from Vo source code.
fn parse_extern_func(source: &str, func_name: &str) -> Option<VoFuncSig> {
    // Simple line-by-line search for function declarations
    let lines: Vec<&str> = source.lines().collect();
    
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        
        // Look for "func Name(" pattern
        if !trimmed.starts_with("func ") {
            continue;
        }
        
        // Extract function name
        let rest = &trimmed[5..].trim_start();
        let name_end = rest.find('(').unwrap_or(rest.len());
        let name = rest[..name_end].trim();
        
        if name != func_name {
            continue;
        }
        
        // Check if this is an extern function (no body)
        // Look for '{' on this line or following lines
        let mut has_body = false;
        let mut full_sig = trimmed.to_string();
        
        // Collect full signature (may span multiple lines)
        let mut j = i;
        while j < lines.len() {
            let l = lines[j];
            if l.contains('{') {
                has_body = true;
                break;
            }
            if j > i {
                full_sig.push_str(l.trim());
            }
            // Check if signature is complete (ends with ')' or has return type)
            if l.trim().ends_with(')') || l.contains(')') {
                // Check next non-empty line
                let mut k = j + 1;
                while k < lines.len() && lines[k].trim().is_empty() {
                    k += 1;
                }
                if k < lines.len() {
                    let next = lines[k].trim();
                    if next.starts_with('{') {
                        has_body = true;
                    } else if next.starts_with("func ") || next.starts_with("type ") || 
                              next.starts_with("var ") || next.starts_with("const ") ||
                              next.starts_with("//") || next.is_empty() {
                        // Next declaration, no body
                        break;
                    }
                }
                break;
            }
            j += 1;
        }
        
        if has_body {
            continue; // Not an extern function
        }
        
        // Parse the signature
        return parse_func_signature(&full_sig, name);
    }
    
    None
}

/// Parse a function signature string.
fn parse_func_signature(sig: &str, name: &str) -> Option<VoFuncSig> {
    // Format: "func Name(params) returns" or "func Name(params)"
    let rest = sig.strip_prefix("func ")?.trim_start();
    let rest = rest.strip_prefix(name)?.trim_start();
    
    // Find params between ( and )
    let params_start = rest.find('(')?;
    let params_end = rest.find(')')?;
    let params_str = &rest[params_start + 1..params_end];
    
    let params = parse_params(params_str);
    
    // Parse return types (after ')')
    let returns_str = rest[params_end + 1..].trim();
    let results = parse_returns(returns_str);
    
    Some(VoFuncSig {
        name: name.to_string(),
        params,
        results,
    })
}

/// Parse parameter list.
/// Handles Go-style shared types: "x, y float64" → two params both with float64
fn parse_params(s: &str) -> Vec<VoParam> {
    let s = s.trim();
    if s.is_empty() {
        return Vec::new();
    }
    
    // First pass: split by comma and collect (names, type_str) pairs
    let parts: Vec<&str> = s.split(',').map(|p| p.trim()).filter(|p| !p.is_empty()).collect();
    
    // For Go-style "x, y float64", we need to find the type from the last part that has one
    // and apply it backwards to parts without types
    let mut parsed: Vec<(Vec<String>, Option<VoType>)> = Vec::new();
    
    for part in &parts {
        let tokens: Vec<&str> = part.split_whitespace().collect();
        if tokens.len() >= 2 {
            // Has type: "name type" or "name1 name2 type" (shouldn't happen but handle it)
            let ty_str = tokens.last().unwrap();
            let ty = VoType::parse(ty_str);
            let names: Vec<String> = tokens[..tokens.len() - 1]
                .iter()
                .map(|n| n.trim_end_matches(',').to_string())
                .collect();
            parsed.push((names, ty));
        } else if tokens.len() == 1 {
            // Just a name (type comes from next group) or just a type
            let token = tokens[0];
            // Check if it's a known type
            if VoType::parse(token).is_some() && !token.chars().next().unwrap().is_lowercase() {
                // Likely a type (capitalized or known keyword)
                parsed.push((vec![], VoType::parse(token)));
            } else {
                // Just a name, type will be filled from next part
                parsed.push((vec![token.to_string()], None));
            }
        }
    }
    
    // Second pass: fill in missing types from the next part that has a type
    let mut result = Vec::new();
    let mut pending_names: Vec<String> = Vec::new();
    
    for (names, ty_opt) in parsed {
        if let Some(ty) = ty_opt {
            // First, flush pending names with this type
            for name in pending_names.drain(..) {
                result.push(VoParam { name, ty: ty.clone() });
            }
            // Then add current names with this type
            for name in names {
                result.push(VoParam { name, ty: ty.clone() });
            }
        } else {
            // No type yet, accumulate names
            pending_names.extend(names);
        }
    }
    
    // If there are still pending names without type, they become Any
    for name in pending_names {
        result.push(VoParam { name, ty: VoType::Any });
    }
    
    result
}

/// Parse return type list.
fn parse_returns(s: &str) -> Vec<VoType> {
    let s = s.trim();
    if s.is_empty() {
        return Vec::new();
    }
    
    // Check for parenthesized returns: (int, bool)
    if s.starts_with('(') && s.ends_with(')') {
        let inner = &s[1..s.len() - 1];
        return inner.split(',')
            .filter_map(|t| {
                let t = t.trim();
                // Handle named returns: "err error" -> just get type
                let ty_str = t.split_whitespace().last()?;
                VoType::parse(ty_str)
            })
            .collect();
    }
    
    // Single return type
    if let Some(ty) = VoType::parse(s) {
        vec![ty]
    } else {
        Vec::new()
    }
}

// =============================================================================
// Constant Parsing
// =============================================================================

/// Find and parse a constant value from a package directory.
pub fn find_const(pkg_dir: &Path, const_name: &str) -> Result<i64, String> {
    let entries = std::fs::read_dir(pkg_dir)
        .map_err(|e| format!("cannot read directory {:?}: {}", pkg_dir, e))?;
    
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().map(|e| e == "vo").unwrap_or(false) {
            if let Ok(content) = std::fs::read_to_string(&path) {
                if let Some(val) = parse_const_value(&content, const_name) {
                    return Ok(val);
                }
            }
        }
    }
    
    Err(format!("const '{}' not found in {:?}", const_name, pkg_dir))
}

/// Parse a constant value from Vo source code.
fn parse_const_value(source: &str, const_name: &str) -> Option<i64> {
    let lines: Vec<&str> = source.lines().collect();
    let mut in_const_block = false;
    
    for line in &lines {
        let trimmed = line.trim();
        
        // Start of const block: "const ("
        if trimmed == "const (" {
            in_const_block = true;
            continue;
        }
        
        // End of const block: ")"
        if in_const_block && trimmed == ")" {
            in_const_block = false;
            continue;
        }
        
        // Single const: "const Name = value" or "const Name type = value"
        if let Some(rest) = trimmed.strip_prefix("const ") {
            if !rest.starts_with('(') {
                if let Some(val) = parse_const_assignment(rest, const_name) {
                    return Some(val);
                }
            }
            continue;
        }
        
        // Inside const block: "Name = value" or "Name type = value"
        if in_const_block {
            if let Some(val) = parse_const_assignment(trimmed, const_name) {
                return Some(val);
            }
        }
    }
    
    None
}

/// Parse a single constant assignment line.
/// Formats: "Name = value", "Name type = value", "Name = value // comment"
fn parse_const_assignment(line: &str, const_name: &str) -> Option<i64> {
    // Remove trailing comment
    let line = line.split("//").next()?.trim();
    
    // Split by '='
    let (name_part, value_part) = line.split_once('=')?;
    let name_part = name_part.trim();
    let value_part = value_part.trim();
    
    // name_part could be "Name" or "Name type"
    let name = name_part.split_whitespace().next()?;
    
    if name != const_name {
        return None;
    }
    
    eval_const_expr(value_part)
}

/// Evaluate a simple constant expression.
fn eval_const_expr(expr: &str) -> Option<i64> {
    let expr = expr.trim();
    
    // Empty
    if expr.is_empty() {
        return None;
    }
    
    // Parenthesized expression
    if expr.starts_with('(') && expr.ends_with(')') {
        return eval_const_expr(&expr[1..expr.len()-1]);
    }
    
    // Bit OR: a | b
    if let Some((left, right)) = split_binary_op(expr, '|') {
        let l = eval_const_expr(left)?;
        let r = eval_const_expr(right)?;
        return Some(l | r);
    }
    
    // Bit AND: a & b
    if let Some((left, right)) = split_binary_op(expr, '&') {
        let l = eval_const_expr(left)?;
        let r = eval_const_expr(right)?;
        return Some(l & r);
    }
    
    // Bit shift left: a << b
    if let Some((left, right)) = expr.split_once("<<") {
        let l = eval_const_expr(left)?;
        let r = eval_const_expr(right)?;
        return Some(l << r);
    }
    
    // Bit shift right: a >> b
    if let Some((left, right)) = expr.split_once(">>") {
        let l = eval_const_expr(left)?;
        let r = eval_const_expr(right)?;
        return Some(l >> r);
    }
    
    // Addition: a + b
    if let Some((left, right)) = split_binary_op(expr, '+') {
        let l = eval_const_expr(left)?;
        let r = eval_const_expr(right)?;
        return Some(l + r);
    }
    
    // Subtraction: a - b (careful with negative numbers)
    if let Some((left, right)) = split_binary_op_careful(expr, '-') {
        let l = eval_const_expr(left)?;
        let r = eval_const_expr(right)?;
        return Some(l - r);
    }
    
    // Multiplication: a * b
    if let Some((left, right)) = split_binary_op(expr, '*') {
        let l = eval_const_expr(left)?;
        let r = eval_const_expr(right)?;
        return Some(l * r);
    }
    
    // Division: a / b
    if let Some((left, right)) = split_binary_op(expr, '/') {
        let l = eval_const_expr(left)?;
        let r = eval_const_expr(right)?;
        if r != 0 {
            return Some(l / r);
        }
        return None;
    }
    
    // Unary minus: -x
    if let Some(rest) = expr.strip_prefix('-') {
        let val = eval_const_expr(rest)?;
        return Some(-val);
    }
    
    // Character literal: 'x' or '\n' etc
    if expr.starts_with('\'') && expr.ends_with('\'') {
        return parse_char_literal(expr);
    }
    
    // Hex literal: 0x...
    if let Some(hex) = expr.strip_prefix("0x").or_else(|| expr.strip_prefix("0X")) {
        return i64::from_str_radix(hex, 16).ok();
    }
    
    // Octal literal: 0o...
    if let Some(oct) = expr.strip_prefix("0o").or_else(|| expr.strip_prefix("0O")) {
        return i64::from_str_radix(oct, 8).ok();
    }
    
    // Binary literal: 0b...
    if let Some(bin) = expr.strip_prefix("0b").or_else(|| expr.strip_prefix("0B")) {
        return i64::from_str_radix(bin, 2).ok();
    }
    
    // Integer literal
    expr.parse::<i64>().ok()
}

/// Split by binary operator, but only at top level (not inside parens or quotes).
fn split_binary_op(expr: &str, op: char) -> Option<(&str, &str)> {
    let mut depth = 0;
    let mut in_char = false;
    let mut prev_backslash = false;
    
    for (i, c) in expr.char_indices() {
        if in_char {
            if c == '\'' && !prev_backslash {
                in_char = false;
            }
            prev_backslash = c == '\\' && !prev_backslash;
            continue;
        }
        
        match c {
            '\'' => in_char = true,
            '(' => depth += 1,
            ')' => depth -= 1,
            _ if c == op && depth == 0 => {
                return Some((&expr[..i], &expr[i+1..]));
            }
            _ => {}
        }
    }
    None
}

/// Split by '-' but avoid splitting on negative number prefix.
fn split_binary_op_careful(expr: &str, op: char) -> Option<(&str, &str)> {
    let mut depth = 0;
    let mut in_char = false;
    let mut prev_backslash = false;
    let chars: Vec<char> = expr.chars().collect();
    
    for (i, &c) in chars.iter().enumerate() {
        if in_char {
            if c == '\'' && !prev_backslash {
                in_char = false;
            }
            prev_backslash = c == '\\' && !prev_backslash;
            continue;
        }
        
        match c {
            '\'' => in_char = true,
            '(' => depth += 1,
            ')' => depth -= 1,
            _ if c == op && depth == 0 && i > 0 => {
                // Make sure it's not a negative prefix (preceded by operator or start)
                // Look for the last non-whitespace character before this position
                let mut prev_idx = i - 1;
                while prev_idx > 0 && chars[prev_idx].is_whitespace() {
                    prev_idx -= 1;
                }
                let prev = chars.get(prev_idx).copied();
                if let Some(p) = prev {
                    if p.is_ascii_digit() || p == ')' || p == '\'' {
                        let byte_pos: usize = expr.char_indices()
                            .nth(i)
                            .map(|(pos, _)| pos)
                            .unwrap_or(0);
                        return Some((&expr[..byte_pos], &expr[byte_pos+1..]));
                    }
                }
            }
            _ => {}
        }
    }
    None
}

/// Parse character literal like 'x', '\n', '\t', '\\', '\''
fn parse_char_literal(s: &str) -> Option<i64> {
    if s.len() < 3 || !s.starts_with('\'') || !s.ends_with('\'') {
        return None;
    }
    
    let inner = &s[1..s.len()-1];
    
    if inner.starts_with('\\') {
        // Escape sequence
        match inner {
            "\\n" => Some('\n' as i64),
            "\\t" => Some('\t' as i64),
            "\\r" => Some('\r' as i64),
            "\\\\" => Some('\\' as i64),
            "\\'" => Some('\'' as i64),
            "\\\"" => Some('"' as i64),
            "\\0" => Some(0),
            _ => None,
        }
    } else if inner.len() == 1 {
        Some(inner.chars().next()? as i64)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_type() {
        assert!(matches!(VoType::parse("int"), Some(VoType::Int)));
        assert!(matches!(VoType::parse("string"), Some(VoType::String)));
        assert!(matches!(VoType::parse("[]byte"), Some(VoType::Slice(_))));
        assert!(matches!(VoType::parse("*int"), Some(VoType::Pointer(_))));
    }

    #[test]
    fn test_parse_variadic() {
        assert!(matches!(VoType::parse("...interface{}"), Some(VoType::Variadic(_))));
        assert!(matches!(VoType::parse("...int"), Some(VoType::Variadic(_))));
        assert!(matches!(VoType::parse("interface{}"), Some(VoType::Any)));
        
        if let Some(VoType::Variadic(inner)) = VoType::parse("...interface{}") {
            assert!(matches!(*inner, VoType::Any));
        } else {
            panic!("expected Variadic");
        }
    }

    #[test]
    fn test_parse_map() {
        assert!(matches!(VoType::parse("map[string]int"), Some(VoType::Map(_, _))));
        assert!(matches!(VoType::parse("map[int][]byte"), Some(VoType::Map(_, _))));
        
        if let Some(VoType::Map(k, v)) = VoType::parse("map[string]int") {
            assert!(matches!(*k, VoType::String));
            assert!(matches!(*v, VoType::Int));
        } else {
            panic!("expected Map");
        }
    }

    #[test]
    fn test_parse_chan() {
        assert!(matches!(VoType::parse("chan int"), Some(VoType::Chan(ChanDir::Both, _))));
        assert!(matches!(VoType::parse("chan<- int"), Some(VoType::Chan(ChanDir::Send, _))));
        assert!(matches!(VoType::parse("<-chan int"), Some(VoType::Chan(ChanDir::Recv, _))));
    }

    #[test]
    fn test_parse_func() {
        assert!(matches!(VoType::parse("func()"), Some(VoType::Func(_, _))));
        assert!(matches!(VoType::parse("func(int) bool"), Some(VoType::Func(_, _))));
        assert!(matches!(VoType::parse("func(int, string) (bool, error)"), Some(VoType::Func(_, _))));
        
        if let Some(VoType::Func(params, results)) = VoType::parse("func(int, string) bool") {
            assert_eq!(params.len(), 2);
            assert_eq!(results.len(), 1);
        } else {
            panic!("expected Func");
        }
    }

    #[test]
    fn test_parse_array() {
        assert!(matches!(VoType::parse("[10]int"), Some(VoType::Array(10, _))));
        assert!(matches!(VoType::parse("[256]byte"), Some(VoType::Array(256, _))));
    }

    #[test]
    fn test_parse_extern_func() {
        let source = r#"
package fmt

func Println(s string) int

func helper() {
    // has body
}
"#;
        let sig = parse_extern_func(source, "Println").unwrap();
        assert_eq!(sig.name, "Println");
        assert_eq!(sig.params.len(), 1);
        assert!(matches!(sig.params[0].ty, VoType::String));
        assert_eq!(sig.results.len(), 1);
        assert!(matches!(sig.results[0], VoType::Int));
    }

    #[test]
    fn test_parse_variadic_func() {
        let source = "func Print(a ...interface{}) int";
        let sig = parse_func_signature(source, "Print").unwrap();
        assert_eq!(sig.params.len(), 1);
        assert!(sig.params[0].ty.is_variadic());
        assert_eq!(sig.results.len(), 1);
    }

    #[test]
    fn test_parse_multi_return() {
        let source = "func Divmod(a, b int) (int, int)";
        let sig = parse_func_signature(source, "Divmod").unwrap();
        assert_eq!(sig.params.len(), 2);
        assert_eq!(sig.results.len(), 2);
    }

    #[test]
    fn test_eval_const_expr() {
        assert_eq!(eval_const_expr("3000"), Some(3000));
        assert_eq!(eval_const_expr("0"), Some(0));
        assert_eq!(eval_const_expr("-1"), Some(-1));
        assert_eq!(eval_const_expr("0x1F"), Some(31));
        assert_eq!(eval_const_expr("0xff"), Some(255));
        assert_eq!(eval_const_expr("1 << 3"), Some(8));
        assert_eq!(eval_const_expr("1 << 7"), Some(128));
        assert_eq!(eval_const_expr("'/'"), Some(47));
        assert_eq!(eval_const_expr("'\\n'"), Some(10));
        assert_eq!(eval_const_expr("'\\t'"), Some(9));
        assert_eq!(eval_const_expr("1 + 2"), Some(3));
        assert_eq!(eval_const_expr("10 - 3"), Some(7));
        assert_eq!(eval_const_expr("2 * 3"), Some(6));
        assert_eq!(eval_const_expr("10 / 2"), Some(5));
        assert_eq!(eval_const_expr("1 | 2"), Some(3));
        assert_eq!(eval_const_expr("3 & 1"), Some(1));
    }

    #[test]
    fn test_parse_const_value() {
        let source = r#"
package os

const (
    codeErrInvalid    = 3000 // Invalid argument
    codeErrPermission = 3001 // Permission denied
    O_RDONLY int = 0
    O_APPEND int = 1 << 3
)

const PathSeparator = '/'
"#;
        assert_eq!(parse_const_value(source, "codeErrInvalid"), Some(3000));
        assert_eq!(parse_const_value(source, "codeErrPermission"), Some(3001));
        assert_eq!(parse_const_value(source, "O_RDONLY"), Some(0));
        assert_eq!(parse_const_value(source, "O_APPEND"), Some(8));
        assert_eq!(parse_const_value(source, "PathSeparator"), Some(47));
        assert_eq!(parse_const_value(source, "NotExist"), None);
    }
}
