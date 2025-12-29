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
    pub is_extern: bool,
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
            is_extern: true,
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

impl VoType {
    /// Parse a type string.
    pub fn parse(s: &str) -> Option<Self> {
        let s = s.trim();
        
        // Primitive types
        match s {
            "int" => return Some(VoType::Int),
            "int8" => return Some(VoType::Int8),
            "int16" => return Some(VoType::Int16),
            "int32" => return Some(VoType::Int32),
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
            VoType::Slice(_) => "GcRef",
            VoType::Array(_, _) => "GcRef",
            VoType::Map(_, _) => "GcRef",
            VoType::Chan(_, _) => "GcRef",
            VoType::Func(_, _) => "GcRef",
            VoType::Named(_) => "GcRef",
            VoType::Variadic(_) => "variadic",
        }
    }

    /// Check if this type is variadic.
    pub fn is_variadic(&self) -> bool {
        matches!(self, VoType::Variadic(_))
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
        is_extern: true,
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
}
