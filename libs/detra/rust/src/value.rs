//! Detra runtime values.

use std::collections::HashMap;

pub use detra_renderable::Value;

pub fn zero_value(ty: &crate::ast::Type) -> Value {
    use crate::ast::Type;
    match ty {
        Type::Bool => Value::Bool(false),
        Type::Int => Value::Int(0),
        Type::Float => Value::Float(0.0),
        Type::String => Value::String(String::new()),
        Type::Array(_) => Value::Array(Vec::new()),
        Type::Map(_, _) => Value::Map(HashMap::new()),
        Type::Named(name) => Value::Struct(name.clone(), HashMap::new()),
    }
}
