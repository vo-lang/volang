#[cfg(feature = "jit")]
#[test]
fn vm_pending_terminal_txn_002_pending_effects_declare_terminal_policy() {
    let src = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../runtime_boundary.rs"
    ));

    assert!(src.contains("enum PendingTransitionTerminalPolicy"));
    assert!(src.contains("CommitOnLanguagePanic"));
    assert!(src.contains("CommitOnAnyTerminal"));
    assert!(src.contains("DiscardOnTerminal"));
    assert!(src.contains("pending_terminal_policy"));
    assert!(
            src.contains("ExecResult::JitError")
                && src.contains("PendingTransitionTerminalPolicy::CommitOnAnyTerminal"),
            "terminal JIT infra failures must commit only explicitly terminal-safe compensation effects"
        );
    assert!(
        src.contains("ExecResult::Panic")
            && src.contains("PendingTransitionTerminalPolicy::CommitOnLanguagePanic"),
        "language panic must be allowed to commit effects already observed by JIT bytecode"
    );
}
