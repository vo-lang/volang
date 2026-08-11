use super::*;
use crate::capability::{BackendStatus, RuntimePathPolicy};
use vo_runtime::instruction::Opcode;

#[test]
fn semantic_matrix_covers_every_valid_opcode_in_order() {
    let rows = opcode_semantic_rows();
    assert_eq!(rows.len(), Opcode::COUNT);
    for (raw, row) in rows.iter().enumerate() {
        let opcode = Opcode::from_u8(raw as u8);
        assert_eq!(row.opcode, opcode);
        assert_eq!(*row, opcode_semantics(opcode));
        assert_ne!(row.capability.backend, BackendStatus::Unsupported);
    }
}

#[test]
fn invalid_opcode_has_an_explicit_rejection_contract() {
    let row = opcode_semantics(Opcode::Invalid);
    assert_eq!(row.capability.backend, BackendStatus::Unsupported);
    assert_eq!(
        row.capability.runtime_path,
        RuntimePathPolicy::InvalidOpcode
    );
}

#[test]
fn public_matrix_matches_direct_lookup() {
    let matrix = opcode_semantic_matrix();
    assert_eq!(matrix.len(), Opcode::COUNT);
    for row in matrix {
        assert_eq!(row, opcode_semantics(row.opcode));
        assert_eq!(row.register_effects, opcode_register_effects(row.opcode));
    }
}
