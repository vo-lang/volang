use vo_runtime::instruction::Opcode;

use crate::capability::OpcodeCapability;
use crate::contract::EffectContract;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpcodeSemantics {
    pub opcode: Opcode,
    pub capability: OpcodeCapability,
    pub contract: EffectContract,
}
