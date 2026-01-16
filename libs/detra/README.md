# Detra Engine

Detra is the execution engine for the Detra UI language. It compiles Detra source into an internal program representation, executes deterministic actions and rules, and produces a platform-agnostic UI Tree plus an emit command queue.

## Responsibilities
- Parse and compile Detra source to Program IR.
- Execute action statements atomically.
- Apply rule sets to a fixed point.
- Evaluate the root view to a UI Tree.
- Return emit commands for host-managed IO.

## Public Vo API
- Compile(source string) Program
- InitState(program Program) State
- Execute(program Program, state State, external External, action ActionCall) ExecResult

See types.vo and engine.vo for the Vo interface.

## Spec
Full language specification: spec/detra-spec.md.

## Build (Rust)
From repo root:
- cargo build -p detra

The shared library is produced at target/debug/libdetra.so.
