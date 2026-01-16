//! Detra Execution Engine - runs actions, rules, and evaluates views.

use std::collections::HashMap;
use crate::ast::*;
use crate::value::{Value, zero_value};

pub use detra_renderable::{ActionCall, RuntimeNode};

#[derive(Debug, Clone)]
pub struct State {
    pub fields: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub struct CommandCall {
    pub name: String,
    pub args: HashMap<String, Value>,
}

#[derive(Debug)]
pub struct ExecResult {
    pub state: State,
    pub tree: RuntimeNode,
    pub commands: Vec<CommandCall>,
    pub error: Option<ExecError>,
}

#[derive(Debug, Clone)]
pub struct ExecError {
    pub message: String,
    pub kind: String,
}

pub struct Executor<'a> {
    program: &'a Program,
    state: State,
    commands: Vec<CommandCall>,
    loop_vars: HashMap<String, Value>,
}

impl<'a> Executor<'a> {
    pub fn init_state(program: &Program) -> State {
        let mut fields = HashMap::new();
        for field in &program.state.fields {
            let value = match &field.default {
                Some(expr) => {
                    let mut exec = Executor {
                        program,
                        state: State { fields: HashMap::new() },
                        commands: Vec::new(),
                        loop_vars: HashMap::new(),
                    };
                    exec.eval_expr(expr).unwrap_or_else(|_| zero_value(&field.ty))
                }
                None => zero_value(&field.ty),
            };
            fields.insert(field.name.clone(), value);
        }
        State { fields }
    }

    pub fn execute(
        program: &Program,
        state: State,
        external: HashMap<String, Value>,
        action: Option<ActionCall>,
    ) -> ExecResult {
        let mut exec = Executor {
            program,
            state,
            commands: Vec::new(),
            loop_vars: HashMap::new(),
        };

        for (k, v) in external {
            exec.state.fields.insert(k, v);
        }

        if let Some(action) = action {
            if let Err(e) = exec.execute_action(&action) {
                return ExecResult {
                    state: exec.state,
                    tree: RuntimeNode::empty(),
                    commands: Vec::new(),
                    error: Some(e),
                };
            }
        }

        if let Err(e) = exec.execute_rules() {
            return ExecResult {
                state: exec.state,
                tree: RuntimeNode::empty(),
                commands: Vec::new(),
                error: Some(e),
            };
        }

        let tree = match exec.evaluate_main_view() {
            Ok(t) => t,
            Err(e) => {
                return ExecResult {
                    state: exec.state,
                    tree: RuntimeNode::empty(),
                    commands: Vec::new(),
                    error: Some(e),
                };
            }
        };

        ExecResult {
            state: exec.state,
            tree,
            commands: exec.commands,
            error: None,
        }
    }

    fn execute_action(&mut self, action: &ActionCall) -> Result<(), ExecError> {
        let decl = self.program.actions.iter()
            .find(|a| a.name == action.name)
            .ok_or_else(|| ExecError {
                message: format!("Unknown action: {}", action.name),
                kind: "panic".to_string(),
            })?;

        let mut params = HashMap::new();
        for param in &decl.params {
            let value = action.args.get(&param.name)
                .cloned()
                .or_else(|| param.default.as_ref().map(literal_to_value))
                .ok_or_else(|| ExecError {
                    message: format!("Missing required parameter: {}", param.name),
                    kind: "panic".to_string(),
                })?;
            params.insert(param.name.clone(), value);
        }

        let mut pending_sets = Vec::new();
        let mut pending_emits = Vec::new();

        for stmt in &decl.body {
            match stmt {
                ActionStmt::Require(expr) => {
                    let old_loop_vars = std::mem::take(&mut self.loop_vars);
                    self.loop_vars = params.clone();
                    let result = self.eval_expr(expr);
                    self.loop_vars = old_loop_vars;

                    let value = result.map_err(|msg| ExecError {
                        message: msg,
                        kind: "panic".to_string(),
                    })?;

                    if !value.is_truthy() {
                        return Err(ExecError {
                            message: "require condition failed".to_string(),
                            kind: "require".to_string(),
                        });
                    }
                }
                ActionStmt::Set { path, value } => {
                    let old_loop_vars = std::mem::take(&mut self.loop_vars);
                    self.loop_vars = params.clone();
                    let result = self.eval_expr(value);
                    self.loop_vars = old_loop_vars;

                    let val = result.map_err(|msg| ExecError {
                        message: msg,
                        kind: "panic".to_string(),
                    })?;
                    pending_sets.push((path.clone(), val));
                }
                ActionStmt::Emit { command, args } => {
                    let old_loop_vars = std::mem::take(&mut self.loop_vars);
                    self.loop_vars = params.clone();

                    let mut eval_args = HashMap::new();
                    for arg in args {
                        let val = self.eval_expr(&arg.value).map_err(|msg| ExecError {
                            message: msg,
                            kind: "panic".to_string(),
                        })?;
                        eval_args.insert(arg.name.clone(), val);
                    }

                    self.loop_vars = old_loop_vars;
                    pending_emits.push(CommandCall {
                        name: command.clone(),
                        args: eval_args,
                    });
                }
            }
        }

        for (path, value) in pending_sets {
            self.set_path(&path, value)?;
        }

        self.commands.extend(pending_emits);

        Ok(())
    }

    fn execute_rules(&mut self) -> Result<(), ExecError> {
        for rule in &self.program.rules {
            for stmt in &rule.body {
                match stmt {
                    RuleStmt::Derive { path, value } => {
                        let val = self.eval_expr(value).map_err(|msg| ExecError {
                            message: msg,
                            kind: "panic".to_string(),
                        })?;
                        self.set_path(path, val)?;
                    }
                    RuleStmt::Check { expr, message } => {
                        let val = self.eval_expr(expr).map_err(|msg| ExecError {
                            message: msg,
                            kind: "panic".to_string(),
                        })?;
                        if !val.is_truthy() {
                            return Err(ExecError {
                                message: message.clone().unwrap_or_else(|| "check failed".to_string()),
                                kind: "check".to_string(),
                            });
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn evaluate_main_view(&mut self) -> Result<RuntimeNode, ExecError> {
        let view = self.program.views.iter()
            .find(|v| v.name == "Main")
            .ok_or_else(|| ExecError {
                message: "Missing Main view".to_string(),
                kind: "panic".to_string(),
            })?;

        self.eval_node(&view.body)
    }

    fn eval_node(&mut self, node: &Node) -> Result<RuntimeNode, ExecError> {
        let mut props = HashMap::new();
        let mut events = HashMap::new();

        for prop in &node.props {
            match &prop.value {
                PropValue::Expr(expr) => {
                    let val = self.eval_expr(expr).map_err(|msg| ExecError {
                        message: msg,
                        kind: "panic".to_string(),
                    })?;
                    props.insert(prop.name.clone(), val);
                }
                PropValue::ActionRef(action_ref) => {
                    let mut args = HashMap::new();
                    for arg in &action_ref.args {
                        if let Expr::EventVar(_) = &arg.value {
                            args.insert(arg.name.clone(), Value::Null);
                        } else {
                            let val = self.eval_expr(&arg.value).map_err(|msg| ExecError {
                                message: msg,
                                kind: "panic".to_string(),
                            })?;
                            args.insert(arg.name.clone(), val);
                        }
                    }
                    events.insert(prop.name.clone(), ActionCall {
                        name: action_ref.name.clone(),
                        args,
                    });
                }
            }
        }

        let key = props.remove("key");

        let mut children = Vec::new();
        for child in &node.children {
            match child {
                ViewChild::Node(n) => {
                    children.push(self.eval_node(n)?);
                }
                ViewChild::If(view_if) => {
                    let cond = self.eval_expr(&view_if.cond).map_err(|msg| ExecError {
                        message: msg,
                        kind: "panic".to_string(),
                    })?;
                    if cond.is_truthy() {
                        for c in &view_if.then_body {
                            self.eval_view_child(c, &mut children)?;
                        }
                    } else if let Some(else_body) = &view_if.else_body {
                        for c in else_body {
                            self.eval_view_child(c, &mut children)?;
                        }
                    }
                }
                ViewChild::For(comp) => {
                    self.eval_comprehension(comp, &mut children)?;
                }
            }
        }

        Ok(RuntimeNode {
            kind: node.kind.clone(),
            key,
            props,
            events,
            children,
        })
    }

    fn eval_view_child(&mut self, child: &ViewChild, out: &mut Vec<RuntimeNode>) -> Result<(), ExecError> {
        match child {
            ViewChild::Node(n) => {
                out.push(self.eval_node(n)?);
            }
            ViewChild::If(view_if) => {
                let cond = self.eval_expr(&view_if.cond).map_err(|msg| ExecError {
                    message: msg,
                    kind: "panic".to_string(),
                })?;
                if cond.is_truthy() {
                    for c in &view_if.then_body {
                        self.eval_view_child(c, out)?;
                    }
                } else if let Some(else_body) = &view_if.else_body {
                    for c in else_body {
                        self.eval_view_child(c, out)?;
                    }
                }
            }
            ViewChild::For(comp) => {
                self.eval_comprehension(comp, out)?;
            }
        }
        Ok(())
    }

    fn eval_comprehension(&mut self, comp: &Comprehension, out: &mut Vec<RuntimeNode>) -> Result<(), ExecError> {
        let source = self.eval_expr(&comp.source).map_err(|msg| ExecError {
            message: msg,
            kind: "panic".to_string(),
        })?;

        let items: Vec<(Value, Value)> = match &source {
            Value::Array(arr) => arr.iter().enumerate()
                .map(|(i, v)| (Value::Int(i as i64), v.clone()))
                .collect(),
            Value::Map(m) => m.iter()
                .map(|(k, v)| (Value::String(k.clone()), v.clone()))
                .collect(),
            _ => return Err(ExecError {
                message: format!("Cannot iterate over {}", source.type_name()),
                kind: "panic".to_string(),
            }),
        };

        let mut filtered: Vec<(Value, Value)> = Vec::new();
        for (idx, val) in items {
            match &comp.binding {
                Binding::Single(name) => {
                    self.loop_vars.insert(name.clone(), val.clone());
                }
                Binding::Pair(idx_name, val_name) => {
                    self.loop_vars.insert(idx_name.clone(), idx.clone());
                    self.loop_vars.insert(val_name.clone(), val.clone());
                }
            }

            let mut pass = true;
            for filter in &comp.filters {
                let result = self.eval_expr(filter).map_err(|msg| ExecError {
                    message: msg,
                    kind: "panic".to_string(),
                })?;
                if !result.is_truthy() {
                    pass = false;
                    break;
                }
            }

            if pass {
                filtered.push((idx, val));
            }
        }

        if !comp.sorts.is_empty() {
            for sort in comp.sorts.iter().rev() {
                let sort_clone = sort.clone();
                filtered.sort_by(|(idx_a, val_a), (idx_b, val_b)| {
                    match &comp.binding {
                        Binding::Single(name) => {
                            self.loop_vars.insert(name.clone(), val_a.clone());
                        }
                        Binding::Pair(idx_name, val_name) => {
                            self.loop_vars.insert(idx_name.clone(), idx_a.clone());
                            self.loop_vars.insert(val_name.clone(), val_a.clone());
                        }
                    }
                    let key_a = self.eval_expr(&sort_clone.expr).unwrap_or(Value::Null);

                    match &comp.binding {
                        Binding::Single(name) => {
                            self.loop_vars.insert(name.clone(), val_b.clone());
                        }
                        Binding::Pair(idx_name, val_name) => {
                            self.loop_vars.insert(idx_name.clone(), idx_b.clone());
                            self.loop_vars.insert(val_name.clone(), val_b.clone());
                        }
                    }
                    let key_b = self.eval_expr(&sort_clone.expr).unwrap_or(Value::Null);

                    let ord = compare_values(&key_a, &key_b);
                    if sort_clone.desc { ord.reverse() } else { ord }
                });
            }
        }

        for (idx, val) in filtered {
            match &comp.binding {
                Binding::Single(name) => {
                    self.loop_vars.insert(name.clone(), val);
                }
                Binding::Pair(idx_name, val_name) => {
                    self.loop_vars.insert(idx_name.clone(), idx);
                    self.loop_vars.insert(val_name.clone(), val);
                }
            }
            out.push(self.eval_node(&comp.body)?);
        }

        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Literal(lit) => Ok(literal_to_value(lit)),
            Expr::Path(path) => self.eval_path(path),
            Expr::EventVar(name) => {
                Err(format!("Event variable ${} cannot be evaluated at compile time", name))
            }
            Expr::Binary { op, left, right } => {
                let l = self.eval_expr(left)?;
                let r = self.eval_expr(right)?;
                eval_binary_op(*op, l, r)
            }
            Expr::Unary { op, expr } => {
                let v = self.eval_expr(expr)?;
                eval_unary_op(*op, v)
            }
            Expr::Call { func, args } => {
                let mut eval_args = Vec::new();
                for arg in args {
                    eval_args.push(self.eval_expr(arg)?);
                }
                eval_builtin(func, eval_args)
            }
            Expr::Index { expr, index } => {
                let val = self.eval_expr(expr)?;
                let idx = self.eval_expr(index)?;
                val.get_index(&idx)
                    .cloned()
                    .ok_or_else(|| format!("Index out of bounds or invalid key"))
            }
            Expr::Field { expr, field } => {
                let val = self.eval_expr(expr)?;
                val.get_field(field)
                    .cloned()
                    .ok_or_else(|| format!("No field '{}' on value", field))
            }
            Expr::ArrayLit(elems) => {
                let mut arr = Vec::new();
                for e in elems {
                    arr.push(self.eval_expr(e)?);
                }
                Ok(Value::Array(arr))
            }
            Expr::MapLit(pairs) => {
                let mut map = HashMap::new();
                for (k, v) in pairs {
                    let key = self.eval_expr(k)?;
                    let key_str = match key {
                        Value::String(s) => s,
                        Value::Int(n) => n.to_string(),
                        Value::Bool(b) => b.to_string(),
                        _ => return Err("Map key must be string, int, or bool".to_string()),
                    };
                    map.insert(key_str, self.eval_expr(v)?);
                }
                Ok(Value::Map(map))
            }
            Expr::StructLit { ty, fields } => {
                let mut map = HashMap::new();
                for (name, expr) in fields {
                    map.insert(name.clone(), self.eval_expr(expr)?);
                }
                Ok(Value::Struct(ty.clone(), map))
            }
        }
    }

    fn eval_path(&self, path: &Path) -> Result<Value, String> {
        if path.segments.is_empty() {
            return Err("Empty path".to_string());
        }

        let first = &path.segments[0];

        if let Some(val) = self.loop_vars.get(first) {
            let mut current = val.clone();
            for seg in &path.segments[1..] {
                current = current.get_field(seg)
                    .cloned()
                    .ok_or_else(|| format!("No field '{}' on value", seg))?;
            }
            return Ok(current);
        }

        if first == "state" {
            if path.segments.len() < 2 {
                return Err("Expected field after 'state'".to_string());
            }
            let field_name = &path.segments[1];
            let mut current = self.state.fields.get(field_name)
                .cloned()
                .ok_or_else(|| format!("No state field '{}'", field_name))?;
            for seg in &path.segments[2..] {
                current = current.get_field(seg)
                    .cloned()
                    .ok_or_else(|| format!("No field '{}' on value", seg))?;
            }
            return Ok(current);
        }

        Err(format!("Unknown identifier: {}", first))
    }

    fn set_path(&mut self, path: &Path, value: Value) -> Result<(), ExecError> {
        if path.segments.len() < 2 || path.segments[0] != "state" {
            return Err(ExecError {
                message: "set target must start with 'state.'".to_string(),
                kind: "panic".to_string(),
            });
        }

        let field_name = &path.segments[1];

        if path.segments.len() == 2 {
            self.state.fields.insert(field_name.clone(), value);
            return Ok(());
        }

        let mut current = self.state.fields.get_mut(field_name)
            .ok_or_else(|| ExecError {
                message: format!("No state field '{}'", field_name),
                kind: "panic".to_string(),
            })?;

        for i in 2..path.segments.len() - 1 {
            let seg = &path.segments[i];
            current = match current {
                Value::Map(m) => m.get_mut(seg).ok_or_else(|| ExecError {
                    message: format!("No field '{}'", seg),
                    kind: "panic".to_string(),
                })?,
                Value::Struct(_, fields) => fields.get_mut(seg).ok_or_else(|| ExecError {
                    message: format!("No field '{}'", seg),
                    kind: "panic".to_string(),
                })?,
                _ => return Err(ExecError {
                    message: format!("Cannot access field '{}' on {}", seg, current.type_name()),
                    kind: "panic".to_string(),
                }),
            };
        }

        let last_seg = &path.segments[path.segments.len() - 1];
        match current {
            Value::Map(m) => { m.insert(last_seg.clone(), value); }
            Value::Struct(_, fields) => { fields.insert(last_seg.clone(), value); }
            _ => return Err(ExecError {
                message: format!("Cannot set field '{}' on {}", last_seg, current.type_name()),
                kind: "panic".to_string(),
            }),
        }

        Ok(())
    }
}


fn literal_to_value(lit: &Literal) -> Value {
    match lit {
        Literal::Bool(b) => Value::Bool(*b),
        Literal::Int(n) => Value::Int(*n),
        Literal::Float(f) => Value::Float(*f),
        Literal::String(s) => Value::String(s.clone()),
    }
}

fn eval_binary_op(op: BinOp, left: Value, right: Value) -> Result<Value, String> {
    match op {
        BinOp::Add => match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
            _ => Err("Invalid operands for +".to_string()),
        },
        BinOp::Sub => match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 - b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f64)),
            _ => Err("Invalid operands for -".to_string()),
        },
        BinOp::Mul => match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 * b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f64)),
            _ => Err("Invalid operands for *".to_string()),
        },
        BinOp::Div => match (left, right) {
            (Value::Int(a), Value::Int(b)) => {
                if b == 0 { return Err("Division by zero".to_string()); }
                Ok(Value::Int(a / b))
            }
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 / b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / b as f64)),
            _ => Err("Invalid operands for /".to_string()),
        },
        BinOp::Mod => match (left, right) {
            (Value::Int(a), Value::Int(b)) => {
                if b == 0 { return Err("Modulo by zero".to_string()); }
                Ok(Value::Int(a % b))
            }
            _ => Err("Invalid operands for %".to_string()),
        },
        BinOp::Eq => Ok(Value::Bool(values_equal(&left, &right))),
        BinOp::Ne => Ok(Value::Bool(!values_equal(&left, &right))),
        BinOp::Lt => match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a < b)),
            _ => Err("Invalid operands for <".to_string()),
        },
        BinOp::Le => match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a <= b)),
            _ => Err("Invalid operands for <=".to_string()),
        },
        BinOp::Gt => match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a > b)),
            _ => Err("Invalid operands for >".to_string()),
        },
        BinOp::Ge => match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a >= b)),
            _ => Err("Invalid operands for >=".to_string()),
        },
        BinOp::And => Ok(Value::Bool(left.is_truthy() && right.is_truthy())),
        BinOp::Or => Ok(Value::Bool(left.is_truthy() || right.is_truthy())),
    }
}

fn eval_unary_op(op: UnaryOp, val: Value) -> Result<Value, String> {
    match op {
        UnaryOp::Not => Ok(Value::Bool(!val.is_truthy())),
        UnaryOp::Neg => match val {
            Value::Int(n) => Ok(Value::Int(-n)),
            Value::Float(f) => Ok(Value::Float(-f)),
            _ => Err("Invalid operand for unary -".to_string()),
        },
    }
}

fn eval_builtin(name: &str, args: Vec<Value>) -> Result<Value, String> {
    match name {
        "len" => {
            if args.len() != 1 {
                return Err("len() takes 1 argument".to_string());
            }
            match &args[0] {
                Value::String(s) => Ok(Value::Int(s.len() as i64)),
                Value::Array(a) => Ok(Value::Int(a.len() as i64)),
                Value::Map(m) => Ok(Value::Int(m.len() as i64)),
                _ => Err("len() requires string, array, or map".to_string()),
            }
        }
        "string" => {
            if args.len() != 1 {
                return Err("string() takes 1 argument".to_string());
            }
            let s = match &args[0] {
                Value::Bool(b) => b.to_string(),
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::String(s) => s.clone(),
                _ => return Err("Cannot convert to string".to_string()),
            };
            Ok(Value::String(s))
        }
        "int" => {
            if args.len() != 1 {
                return Err("int() takes 1 argument".to_string());
            }
            let n = match &args[0] {
                Value::Int(n) => *n,
                Value::Float(f) => *f as i64,
                Value::String(s) => s.parse().map_err(|_| "Cannot parse int")?,
                Value::Bool(b) => if *b { 1 } else { 0 },
                _ => return Err("Cannot convert to int".to_string()),
            };
            Ok(Value::Int(n))
        }
        "float" => {
            if args.len() != 1 {
                return Err("float() takes 1 argument".to_string());
            }
            let f = match &args[0] {
                Value::Int(n) => *n as f64,
                Value::Float(f) => *f,
                Value::String(s) => s.parse().map_err(|_| "Cannot parse float")?,
                _ => return Err("Cannot convert to float".to_string()),
            };
            Ok(Value::Float(f))
        }
        "append" => {
            if args.len() != 2 {
                return Err("append() takes 2 arguments".to_string());
            }
            match (&args[0], &args[1]) {
                (Value::Array(arr), val) => {
                    let mut new_arr = arr.clone();
                    new_arr.push(val.clone());
                    Ok(Value::Array(new_arr))
                }
                _ => Err("append() requires array as first argument".to_string()),
            }
        }
        _ => Err(format!("Unknown function: {}", name)),
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Array(a), Value::Array(b)) => {
            if a.len() != b.len() { return false; }
            a.iter().zip(b.iter()).all(|(x, y)| values_equal(x, y))
        }
        (Value::Map(a), Value::Map(b)) => {
            if a.len() != b.len() { return false; }
            a.iter().all(|(k, v)| b.get(k).map(|bv| values_equal(v, bv)).unwrap_or(false))
        }
        (Value::Struct(name_a, a), Value::Struct(name_b, b)) => {
            if name_a != name_b { return false; }
            if a.len() != b.len() { return false; }
            a.iter().all(|(k, v)| b.get(k).map(|bv| values_equal(v, bv)).unwrap_or(false))
        }
        _ => false,
    }
}

fn compare_values(a: &Value, b: &Value) -> std::cmp::Ordering {
    use std::cmp::Ordering;
    match (a, b) {
        (Value::Int(a), Value::Int(b)) => a.cmp(b),
        (Value::Float(a), Value::Float(b)) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
        (Value::String(a), Value::String(b)) => a.cmp(b),
        (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
        _ => Ordering::Equal,
    }
}
