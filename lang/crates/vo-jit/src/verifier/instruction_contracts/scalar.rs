use super::*;

pub(super) fn verify(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    let func = ctx.func;
    let vo_module = ctx._vo_module;
    let pc = ctx.pc;
    let inst = ctx.inst;
    let opcode = ctx.opcode;

    match opcode {
        Opcode::LoadInt => verify_load_int_contract(func, pc, inst),
        Opcode::LoadConst => verify_load_const_contract(func, vo_module, pc, inst),
        Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::DivI
        | Opcode::DivU
        | Opcode::ModI
        | Opcode::ModU
        | Opcode::LtI
        | Opcode::LtU
        | Opcode::LeI
        | Opcode::LeU
        | Opcode::GtI
        | Opcode::GtU
        | Opcode::GeI
        | Opcode::GeU
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU => verify_binary_slot_contract(
            func,
            pc,
            opcode,
            inst,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            scalar_destination_access(opcode),
            "scalar lhs",
            "scalar rhs",
        ),
        Opcode::EqI | Opcode::NeI | Opcode::And | Opcode::Or | Opcode::Xor | Opcode::AndNot => {
            verify_binary_one_of_slot_contract(
                func,
                pc,
                opcode,
                inst,
                &[SlotType::Value],
                RAW_I64_SLOTS,
                RAW_I64_SLOTS,
                scalar_destination_access(opcode),
                "raw lhs",
                "raw rhs",
            )
        }
        Opcode::NegI | Opcode::BoolNot => verify_unary_slot_contract(
            func,
            pc,
            opcode,
            inst,
            SlotType::Value,
            SlotType::Value,
            scalar_destination_access(opcode),
            "scalar source",
        ),
        Opcode::Not => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            &[SlotType::Value],
            RAW_I64_SLOTS,
            scalar_destination_access(opcode),
            "raw source",
        ),
        Opcode::AddF | Opcode::SubF | Opcode::MulF | Opcode::DivF => {
            verify_binary_one_of_slot_contract(
                func,
                pc,
                opcode,
                inst,
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                scalar_destination_access(opcode),
                "float lhs",
                "float rhs",
            )
        }
        Opcode::NegF => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            FLOAT_STORAGE_SLOTS,
            scalar_destination_access(opcode),
            "float source",
        ),
        Opcode::EqF | Opcode::NeF | Opcode::LtF | Opcode::LeF | Opcode::GtF | Opcode::GeF => {
            verify_binary_one_of_slot_contract(
                func,
                pc,
                opcode,
                inst,
                &[SlotType::Value],
                FLOAT_STORAGE_SLOTS,
                FLOAT_STORAGE_SLOTS,
                scalar_destination_access(opcode),
                "float lhs",
                "float rhs",
            )
        }
        Opcode::ConvI2F => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            &[SlotType::Value],
            "ConvI2F destination",
            "ConvI2F source",
        ),
        Opcode::ConvF2I => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            &[SlotType::Value],
            FLOAT_STORAGE_SLOTS,
            "ConvF2I destination",
            "ConvF2I source",
        ),
        Opcode::ConvF64F32 => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            FLOAT_STORAGE_SLOTS,
            "ConvF64F32 destination",
            "ConvF64F32 source",
        ),
        Opcode::ConvF32F64 => verify_unary_one_of_slot_contract(
            func,
            pc,
            opcode,
            inst,
            FLOAT_STORAGE_SLOTS,
            &[SlotType::Value, SlotType::Float],
            "ConvF32F64 destination",
            "ConvF32F64 source",
        ),
        Opcode::Trunc => verify_unary_slot_contract(
            func,
            pc,
            opcode,
            inst,
            SlotType::Value,
            SlotType::Value,
            "Trunc destination",
            "Trunc source",
        ),
        Opcode::IndexCheck => {
            verify_layout(
                func,
                pc,
                opcode,
                inst.a,
                &[SlotType::Value],
                "IndexCheck index",
            )?;
            verify_layout(
                func,
                pc,
                opcode,
                inst.b,
                &[SlotType::Value],
                "IndexCheck length",
            )
        }
        Opcode::Copy => verify_copy_contract(func, pc, opcode, inst),
        Opcode::CopyN => verify_copy_n_contract(func, pc, opcode, inst),
        other => unreachable!("scalar verifier received {other:?}"),
    }
}
