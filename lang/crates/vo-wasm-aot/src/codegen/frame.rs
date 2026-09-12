//! Materialized-frame instruction dispatch and its verified context.
use super::*;

mod collections;
mod control;
mod interface;
mod memory;
mod scheduler;

#[derive(Clone, Copy)]
pub(super) struct FrameContext<'a> {
    pub(super) module: &'a ModuleAnalysis<'a>,
    pub(super) resolved_externs: &'a ResolvedExternTable,
    pub(super) function_id: u32,
    pub(super) function: &'a FunctionDef,
    pub(super) pc: usize,
    pub(super) current_block: u32,
    pub(super) by_pc: &'a BTreeMap<usize, u32>,
    pub(super) loop_depth: u32,
    pub(super) function_indices: &'a BTreeMap<u32, u32>,
    pub(super) materialized: &'a BTreeSet<u32>,
    pub(super) runtime_globals: RuntimeGlobals,
    pub(super) static_data: &'a StaticData,
    pub(super) allocation_descriptors: &'a AllocationDescriptors,
}

pub(super) fn compile_frame_instruction(
    body: &mut Function,
    context: FrameContext<'_>,
    instruction: vo_common_core::Instruction,
) -> Result<bool, WasmAotError> {
    let FrameContext {
        function,
        pc,
        current_block,
        static_data,
        ..
    } = context;
    let opcode = instruction.opcode();
    if emit_scalar_arithmetic(body, instruction, ScalarStorage::Frame, |body, status| {
        return_runtime_panic(
            body,
            static_data.runtime_panic_refs[status as usize],
            current_block,
        );
    }) {
        return Ok(false);
    }
    match opcode {
        Opcode::Hint
        | Opcode::LoadInt
        | Opcode::LoadConst
        | Opcode::Copy
        | Opcode::CopyN
        | Opcode::SlotGet
        | Opcode::SlotSet
        | Opcode::SlotGetN
        | Opcode::SlotSetN
        | Opcode::GlobalGet
        | Opcode::GlobalGetN
        | Opcode::GlobalSet
        | Opcode::GlobalSetN
        | Opcode::PtrNew
        | Opcode::PtrGet
        | Opcode::PtrGetN
        | Opcode::PtrSet
        | Opcode::PtrSetN
        | Opcode::PtrAdd
        | Opcode::IndexCheck => memory::compile(body, context, instruction),
        Opcode::Jump
        | Opcode::JumpIf
        | Opcode::JumpIfNot
        | Opcode::ForLoop
        | Opcode::Call
        | Opcode::CallExtern
        | Opcode::CallClosure
        | Opcode::CallIface
        | Opcode::DeferPush
        | Opcode::ErrDeferPush
        | Opcode::Panic
        | Opcode::Recover
        | Opcode::Return
        | Opcode::ClosureNew
        | Opcode::ClosureGet => control::compile(body, context, instruction),
        Opcode::StrNew
        | Opcode::StrLen
        | Opcode::StrIndex
        | Opcode::StrDecodeRune
        | Opcode::StrConcat
        | Opcode::StrSlice
        | Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe
        | Opcode::ArrayNew
        | Opcode::SliceNew
        | Opcode::ArrayGet
        | Opcode::SliceGet
        | Opcode::ArraySet
        | Opcode::SliceSet
        | Opcode::ArrayAddr
        | Opcode::SliceAddr
        | Opcode::SliceLen
        | Opcode::SliceCap
        | Opcode::SliceAppend
        | Opcode::SliceSlice
        | Opcode::MapNew
        | Opcode::MapGet
        | Opcode::MapSet
        | Opcode::MapDelete
        | Opcode::MapLen
        | Opcode::MapIterInit
        | Opcode::MapIterNext => collections::compile(body, context, instruction),
        Opcode::QueueNew
        | Opcode::QueueSend
        | Opcode::QueueRecv
        | Opcode::QueueClose
        | Opcode::QueueLen
        | Opcode::QueueCap
        | Opcode::SelectBegin
        | Opcode::SelectSend
        | Opcode::SelectRecv
        | Opcode::SelectExec
        | Opcode::IslandNew
        | Opcode::GoIsland
        | Opcode::GoStart => scheduler::compile(body, context, instruction),
        Opcode::IfaceAssign | Opcode::IfaceAssert | Opcode::IfaceEq => {
            interface::compile(body, context, instruction)
        }
        _ => Err(WasmAotError::UnsupportedOpcode {
            function: function.name.clone(),
            pc,
            opcode,
        }),
    }
}
