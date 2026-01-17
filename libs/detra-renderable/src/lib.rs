//! Detra Renderable - shared types between Detra engine and renderers.
//!
//! This crate defines the contract between Detra and renderers:
//! - Detra outputs RuntimeNode (UI tree with layout hints and event bindings)
//! - Renderer handles layout, rendering, and user interaction
//! - Renderer triggers ActionCall back to Detra

use std::collections::HashMap;
use std::fmt;

// ============================================================================
// Value - Dynamic value type shared between Detra and Renderer
// ============================================================================

#[derive(Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Array(Vec<Value>),
    Map(HashMap<String, Value>),
    Struct(String, HashMap<String, Value>),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Int(n) => *n != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(a) => !a.is_empty(),
            Value::Map(m) => !m.is_empty(),
            Value::Struct(_, _) => true,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Map(_) => "map",
            Value::Struct(_, _) => "struct",
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            Value::Int(n) => Some(*n as f64),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Array(a) => Some(a),
            _ => None,
        }
    }

    pub fn as_map(&self) -> Option<&HashMap<String, Value>> {
        match self {
            Value::Map(m) => Some(m),
            _ => None,
        }
    }

    pub fn get_field(&self, name: &str) -> Option<&Value> {
        match self {
            Value::Map(m) => m.get(name),
            Value::Struct(_, fields) => fields.get(name),
            _ => None,
        }
    }

    pub fn get_index(&self, index: &Value) -> Option<&Value> {
        match (self, index) {
            (Value::Array(a), Value::Int(i)) => {
                let idx = *i as usize;
                a.get(idx)
            }
            (Value::Map(m), Value::String(k)) => m.get(k),
            _ => None,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{:?}", s),
            Value::Array(a) => write!(f, "{:?}", a),
            Value::Map(m) => write!(f, "{:?}", m),
            Value::Struct(name, fields) => write!(f, "{}({:?})", name, fields),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Null
    }
}

// ============================================================================
// ActionCall - Renderer triggers this back to Detra
// ============================================================================

#[derive(Debug, Clone)]
pub struct ActionCall {
    pub name: String,
    pub args: HashMap<String, Value>,
}

// ============================================================================
// RuntimeNode - Detra outputs this, Renderer consumes it
// ============================================================================

#[derive(Debug, Clone)]
pub struct RuntimeNode {
    /// Node type: "Row", "Column", "Button", "Input", "Text", etc.
    pub kind: String,
    
    /// Stable key for widget identity (used by renderer for state persistence)
    pub key: Option<Value>,
    
    /// Properties including:
    /// - Layout: width, height, flex, padding, spacing, align
    /// - Content: text, value, placeholder, src
    /// - Style: color, background, fontSize, bold, etc.
    pub props: HashMap<String, Value>,
    
    /// Event handlers: onClick, onChange, onFocus, etc.
    pub events: HashMap<String, ActionCall>,
    
    /// Child nodes
    pub children: Vec<RuntimeNode>,
}

impl RuntimeNode {
    pub fn empty() -> Self {
        RuntimeNode {
            kind: "Empty".to_string(),
            key: None,
            props: HashMap::new(),
            events: HashMap::new(),
            children: Vec::new(),
        }
    }
    
    pub fn get_string(&self, key: &str) -> Option<&str> {
        self.props.get(key).and_then(|v| v.as_string())
    }
    
    pub fn get_float(&self, key: &str) -> Option<f64> {
        self.props.get(key).and_then(|v| v.as_float())
    }
    
    pub fn get_bool(&self, key: &str) -> Option<bool> {
        self.props.get(key).and_then(|v| v.as_bool())
    }
}
