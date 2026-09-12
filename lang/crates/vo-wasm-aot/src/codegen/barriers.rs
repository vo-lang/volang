//! Insert the typed memory boundary after stores without changing function indexes.
//! Scalar arithmetic stays untouched; the host classifies only the written slots.
use super::*;
mod facts;
use wasm_encoder::reencode::{self, Reencode};
use wasmparser::{Operator, Parser, Payload};

pub(super) fn instrument_memory(
    bytes: &[u8],
    barrier_global: u32,
    barrier_pages: u32,
    frame_descriptor: u32,
    frames: BTreeMap<u32, Vec<u8>>,
) -> Result<Module, WasmAotError> {
    let mut parameters = Vec::new();
    let mut results = Vec::new();
    let mut functions = Vec::new();
    for payload in Parser::new(0).parse_all(bytes) {
        match payload.map_err(|error| WasmAotError::InvalidModule(error.to_string()))? {
            Payload::TypeSection(section) => {
                for ty in section.into_iter_err_on_gc_types() {
                    let ty = ty.map_err(|error| WasmAotError::InvalidModule(error.to_string()))?;
                    parameters.push(ty.params().len() as u32);
                    results.push(ty.results().len() as u32);
                }
            }
            Payload::FunctionSection(section) => {
                for ty in section {
                    functions
                        .push(ty.map_err(|error| WasmAotError::InvalidModule(error.to_string()))?);
                }
            }
            _ => {}
        }
    }
    let mut encoder = MemoryBarriers {
        parameters,
        results,
        frames,
        functions,
        next: 0,
        barrier_global,
        barrier_pages,
        frame_descriptor,
    };
    let mut module = Module::new();
    encoder
        .parse_core_module(&mut module, Parser::new(0), bytes)
        .map_err(|error| {
            WasmAotError::InvalidModule(format!("memory barrier encoding: {error}"))
        })?;
    Ok(module)
}

struct MemoryBarriers {
    parameters: Vec<u32>,
    results: Vec<u32>,
    frames: BTreeMap<u32, Vec<u8>>,
    functions: Vec<u32>,
    next: usize,
    barrier_global: u32,
    barrier_pages: u32,
    frame_descriptor: u32,
}
impl Reencode for MemoryBarriers {
    type Error = core::convert::Infallible;
    fn parse_function_section(
        &mut self,
        functions: &mut FunctionSection,
        section: wasmparser::FunctionSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        reencode::utils::parse_function_section(self, functions, section)?;
        functions.function(3); // (written address, bytes, transfer copy) -> status
        Ok(())
    }
    fn parse_code_section(
        &mut self,
        code: &mut CodeSection,
        section: wasmparser::CodeSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        reencode::utils::parse_code_section(self, code, section)?;
        code.function(&compile_store_barrier(
            self.barrier_pages,
            self.barrier_global,
            self.frame_descriptor,
        ));
        Ok(())
    }
    fn parse_function_body(
        &mut self,
        code: &mut CodeSection,
        function: wasmparser::FunctionBody<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        let index = self.next as u32 + 1;
        let mut count = self.parameters[self.functions[self.next] as usize];
        self.next += 1;
        let mut locals = Vec::new();
        for local in function.get_locals_reader()? {
            let (n, ty) = local?;
            count += n;
            locals.push((n, self.val_type(ty)?));
        }
        // The scratch indices follow every original parameter and local.
        let address = count;
        let value32 = count + 1;
        let length = count + 2;
        let value64 = count + 3;
        let float32 = count + 4;
        let float64 = count + 5;
        locals.extend([
            (3, ValType::I32),
            (1, ValType::I64),
            (1, ValType::F32),
            (1, ValType::F64),
        ]);
        let mut body = Function::new(locals);
        let mut scan = function.get_operators_reader()?;
        let mut stable_frame = true;
        while !scan.eof() {
            if matches!(
                scan.read()?,
                Operator::LocalSet { local_index: 0 } | Operator::LocalTee { local_index: 0 }
            ) {
                stable_frame = false;
            }
        }
        let frame = if stable_frame {
            self.frames.remove(&index)
        } else {
            None
        };
        let mut facts = facts::Stack::default();
        let mut reader = function.get_operators_reader()?;
        while !reader.eof() {
            let op = reader.read()?;
            let store_address = facts.operand(1);
            let copy_address = facts.operand(2);
            let top = facts.operand(0);
            let call_arity = if let Operator::Call { function_index } = op {
                let ty = if function_index == 0 {
                    0
                } else {
                    self.functions[function_index as usize - 1] as usize
                };
                Some((self.parameters[ty], self.results[ty]))
            } else {
                None
            };
            facts.advance(&op, call_arity);
            let store = match &op {
                Operator::I32Store { memarg } => Some((*memarg, value32, 4)),
                Operator::I32Store8 { memarg } => Some((*memarg, value32, 1)),
                Operator::I32Store16 { memarg } => Some((*memarg, value32, 2)),
                Operator::I64Store { memarg } => Some((*memarg, value64, 8)),
                Operator::I64Store8 { memarg } => Some((*memarg, value64, 1)),
                Operator::I64Store16 { memarg } => Some((*memarg, value64, 2)),
                Operator::I64Store32 { memarg } => Some((*memarg, value64, 4)),
                Operator::F32Store { memarg } => Some((*memarg, float32, 4)),
                Operator::F64Store { memarg } => Some((*memarg, float64, 8)),
                _ => None,
            };
            if let Some((memarg, value, bytes)) = store {
                let root =
                    frame_write(frame.as_deref(), store_address, memarg.offset, bytes as u32);
                let no_reference = (matches!(top, facts::Value::Constant(0))
                    && matches!(op, Operator::I64Store { .. }))
                    || root == Some(false);
                if no_reference {
                    body.instruction(&self.instruction(op)?);
                    continue;
                }
                body.instruction(&W::LocalSet(value))
                    .instruction(&W::LocalTee(address))
                    .instruction(&W::LocalGet(value))
                    .instruction(&self.instruction(op)?);
                if index != DEEP_CLONE_FUNCTION_INDEX && !no_reference {
                    body.instruction(&W::GlobalGet(self.barrier_global));
                    if root.is_some() {
                        body.instruction(&W::I32Const(2)).instruction(&W::I32And);
                    }
                    body.instruction(&W::If(BlockType::Empty));
                    body.instruction(&W::LocalGet(address))
                        .instruction(&W::I32Const(memarg.offset as i32))
                        .instruction(&W::I32Add)
                        .instruction(&W::I32Const(bytes))
                        .instruction(&W::I32Const(0))
                        .instruction(&W::Call(self.functions.len() as u32 + 1))
                        .instruction(&W::Drop)
                        .instruction(&W::End);
                }
            } else if matches!(
                op,
                Operator::MemoryCopy { .. } | Operator::MemoryFill { .. }
            ) && index != DEEP_CLONE_FUNCTION_INDEX
            {
                let copy = matches!(op, Operator::MemoryCopy { .. });
                let root = match top {
                    facts::Value::Constant(length)
                        if length >= 0 && length <= i64::from(u32::MAX) =>
                    {
                        frame_write(frame.as_deref(), copy_address, 0, length as u32)
                    }
                    _ => None,
                };
                if root == Some(false) {
                    body.instruction(&self.instruction(op)?);
                    continue;
                }
                body.instruction(&W::LocalSet(length))
                    .instruction(&W::LocalSet(value32))
                    .instruction(&W::LocalTee(address))
                    .instruction(&W::LocalGet(value32))
                    .instruction(&W::LocalGet(length))
                    .instruction(&self.instruction(op)?)
                    .instruction(&W::GlobalGet(self.barrier_global));
                if root.is_some() {
                    body.instruction(&W::I32Const(2)).instruction(&W::I32And);
                }
                // Zero filling only removes edges. A new-value barrier has
                // nothing to shade, even for a large GC-bearing allocation.
                if !copy {
                    body.instruction(&W::LocalGet(value32))
                        .instruction(&W::I32Const(255))
                        .instruction(&W::I32And)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::I32Eqz)
                        .instruction(&W::I32Mul);
                }
                body.instruction(&W::If(BlockType::Empty));
                body.instruction(&W::LocalGet(address))
                    .instruction(&W::LocalGet(length))
                    .instruction(&W::I32Const(i32::from(copy)))
                    .instruction(&W::Call(self.functions.len() as u32 + 1));
                body.instruction(&W::Drop).instruction(&W::End);
            } else {
                body.instruction(&self.instruction(op)?);
            }
        }
        code.function(&body);
        Ok(())
    }
}

/// Consult conservative slot hints entirely in generated code. The fallback
/// retains exact host-owned membership, typed references and Island checks.
fn compile_store_barrier(pages: u32, barrier_global: u32, frame_descriptor: u32) -> Function {
    const HEADER: u32 = 3;
    const OFFSET: u32 = 4;
    const LAST: u32 = 5;
    let mut body = Function::new([(3, ValType::I32)]);
    body.instruction(&W::Block(BlockType::Empty))
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32Const(16))
        .instruction(&W::I32ShrU)
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Shl)
        .instruction(&W::I32Load(MemArg {
            offset: u64::from(pages),
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalTee(HEADER))
        .instruction(&W::I32Eqz)
        .instruction(&W::BrIf(0))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Const(-2))
        .instruction(&W::I32And)
        .instruction(&W::Else)
        .instruction(&W::I32Const(0))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalGet(0))
        .instruction(&W::I32And)
        .instruction(&W::End)
        .instruction(&W::LocalSet(HEADER))
        // Root writes only shade during an active cycle; heap writes also
        // maintain old-to-young remembered parents between cycles.
        .instruction(&W::GlobalGet(barrier_global))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32And)
        .instruction(&W::I32Eqz)
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(frame_descriptor as i32))
        .instruction(&W::I32Eq)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(0))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(32))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(OFFSET))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End)
        // Reject ranges spanning allocations/headers. Generated scalar and
        // container writes stay within their exact allocation extent.
        .instruction(&W::LocalGet(OFFSET))
        .instruction(&W::LocalGet(1))
        .instruction(&W::I32Add)
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(LAST))
        .instruction(&W::LocalGet(OFFSET))
        .instruction(&W::I32LtU)
        .instruction(&W::BrIf(0))
        .instruction(&W::LocalGet(OFFSET))
        .instruction(&W::I32Const(256))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(-1))
        .instruction(&W::LocalGet(OFFSET))
        .instruction(&W::I32Const(3))
        .instruction(&W::I32ShrU)
        .instruction(&W::I32Shl)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(LAST))
        .instruction(&W::I32Const(256))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(-1))
        .instruction(&W::I32Const(31))
        .instruction(&W::LocalGet(LAST))
        .instruction(&W::I32Const(3))
        .instruction(&W::I32ShrU)
        .instruction(&W::I32Sub)
        .instruction(&W::I32ShrU)
        .instruction(&W::Else)
        .instruction(&W::I32Const(-1))
        .instruction(&W::End)
        .instruction(&W::I32And)
        .instruction(&W::BrIf(1))
        .instruction(&W::End)
        .instruction(&W::LocalGet(LAST))
        .instruction(&W::I32Const(256))
        .instruction(&W::I32GeU)
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Const(256))
        .instruction(&W::I32GtU)
        .instruction(&W::I32And)
        .instruction(&W::LocalGet(OFFSET))
        .instruction(&W::LocalGet(HEADER))
        .instruction(&W::I32Load(MemArg {
            offset: 24,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32LtU)
        .instruction(&W::I32And)
        .instruction(&W::BrIf(0))
        .instruction(&W::I32Const(0))
        .instruction(&W::Return)
        .instruction(&W::End);
    emit_memory_call(
        &mut body,
        MEMORY_WRITE,
        &[W::LocalGet(0), W::LocalGet(1), W::LocalGet(2)],
    );
    body.instruction(&W::End);
    body
}

/// Classify only ranges proven to remain in this function's frame. The
/// authenticated layout describes physical slots, including interface pairs.
fn frame_write(
    layout: Option<&[u8]>,
    address: facts::Value,
    offset: u64,
    bytes: u32,
) -> Option<bool> {
    let layout = layout?;
    let facts::Value::Frame(base) = address else {
        return None;
    };
    let start = base.checked_add(i64::try_from(offset).ok()?)?;
    let end = start.checked_add(i64::from(bytes))?;
    if start < 0 || end > layout.len() as i64 * 8 {
        return None;
    }
    Some(
        layout[start as usize / 8..(end as usize).div_ceil(8)]
            .iter()
            .any(|slot| matches!(slot, 1 | 2 | 4)),
    )
}
