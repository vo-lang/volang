//! Local operand facts only. Control-flow joins discard all facts; unknown
//! operators fall back to the generic typed barrier without guessing arity.
use wasmparser::Operator;

#[derive(Clone, Copy, Debug, Default)]
pub(super) enum Value {
    #[default]
    Unknown,
    Constant(i64),
    Frame(i64),
}
#[derive(Default)]
pub(super) struct Stack {
    values: Vec<Value>,
}
impl Stack {
    pub fn operand(&self, from_top: usize) -> Value {
        self.values
            .len()
            .checked_sub(from_top + 1)
            .and_then(|index| self.values.get(index))
            .copied()
            .unwrap_or_default()
    }
    pub fn advance(&mut self, op: &Operator<'_>, call_arity: Option<(u32, u32)>) {
        let result = match op {
            Operator::LocalGet { local_index: 0 } => Some(Value::Frame(0)),
            Operator::I32Const { value } => Some(Value::Constant(i64::from(*value))),
            Operator::I64Const { value } => Some(Value::Constant(*value)),
            Operator::LocalTee { .. } => Some(self.operand(0)),
            Operator::I32Add | Operator::I32Sub => {
                let right = self.operand(0);
                let left = self.operand(1);
                match (left, right) {
                    (Value::Frame(offset), Value::Constant(delta)) => {
                        let delta = if matches!(op, Operator::I32Sub) {
                            delta.checked_neg()
                        } else {
                            Some(delta)
                        };
                        Some(
                            delta
                                .and_then(|delta| offset.checked_add(delta))
                                .map(Value::Frame)
                                .unwrap_or_default(),
                        )
                    }
                    _ => None,
                }
            }
            _ => None,
        };
        // Never carry an address assumption across an edge or polymorphic
        // stack boundary, including branches with values still on the stack.
        if matches!(
            op,
            Operator::Block { .. }
                | Operator::Loop { .. }
                | Operator::If { .. }
                | Operator::Else
                | Operator::End
                | Operator::Br { .. }
                | Operator::BrIf { .. }
                | Operator::BrTable { .. }
                | Operator::Return
                | Operator::Unreachable
        ) {
            self.values.clear();
            return;
        }
        let arity = if matches!(op, Operator::Call { .. }) {
            call_arity
        } else {
            fixed_arity(op)
        };
        let Some((inputs, outputs)) = arity else {
            self.values.clear();
            return;
        };
        if inputs as usize > self.values.len() {
            self.values.clear();
        } else {
            self.values.truncate(self.values.len() - inputs as usize);
        }
        for _ in 0..outputs {
            self.values.push(result.unwrap_or_default());
        }
    }
}

// Consume wasmparser's own operator table so new instructions conservatively
// lose facts instead of silently inheriting an obsolete hand-written arity.
fn fixed_arity(op: &Operator<'_>) -> Option<(u32, u32)> {
    macro_rules! arity {
        (arity $inputs:tt -> $outputs:tt) => {
            Some(($inputs, $outputs))
        };
        (arity custom) => {
            None
        };
    }
    macro_rules! operators {
        ($(@$proposal:ident $name:ident $({ $($field:ident: $type:ty),* })? => $visit:ident ($($annotation:tt)*))*) => {
            match op { $(Operator::$name $({ $($field: _),* })? => arity!($($annotation)*),)* _ => None }
        };
    }
    wasmparser::for_each_operator!(operators)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frame_addresses_survive_scalar_operands_and_calls_but_not_joins() {
        let mut stack = Stack::default();
        stack.advance(&Operator::LocalGet { local_index: 0 }, None);
        stack.advance(&Operator::I32Const { value: 16 }, None);
        stack.advance(&Operator::I32Add, None);
        stack.advance(&Operator::I64Const { value: 123 }, None);
        stack.advance(&Operator::Call { function_index: 2 }, Some((1, 1)));
        assert!(matches!(stack.operand(1), Value::Frame(16)));
        stack.advance(
            &Operator::If {
                blockty: wasmparser::BlockType::Empty,
            },
            None,
        );
        assert!(matches!(stack.operand(0), Value::Unknown));
        stack.advance(&Operator::I64Const { value: 7 }, None);
        stack.advance(&Operator::End, None);
        assert!(matches!(stack.operand(0), Value::Unknown));
    }
}
