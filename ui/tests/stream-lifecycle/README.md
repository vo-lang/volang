# Structured stream lifecycle probe

This permanent contract application verifies that a post-commit worker emits
typed values through the bounded component mailbox, that the UI Island applies
one accepted value per revision, and that normal completion becomes terminal
without direct worker mutation of mounted state.
