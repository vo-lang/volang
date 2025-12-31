# Bug: JIT Return Instruction Format & Infinite Compile Retry

## Date
2025-12-31

## Summary
Two bugs in JIT compiler caused crashes:
1. Return instruction operand misinterpretation
2. Infinite compilation retry on failure

## Bug 1: Return Instruction Format

### Symptom
`jit/recursion.vo` test crashed with segfault when running `fib(10)`.

### Root Cause
In `func_compiler.rs`, the `ret` function incorrectly interpreted the Return instruction format:

**Wrong:**
```rust
fn ret(&mut self, inst: &Instruction) {
    let ret_slots = inst.a as usize;  // WRONG: inst.a is NOT ret_slots
    let src_start = inst.b as usize;
    // ...
}
```

**Correct:**
```rust
fn ret(&mut self, inst: &Instruction) {
    let ret_slots = self.func_def.ret_slots as usize;  // From function definition
    let ret_reg = inst.a as usize;  // inst.a is the source register
    // ...
}
```

### Return Instruction Format
```
Return r0, count=1
       ^^ inst.a = source register (r0)
```
- `inst.a`: Source register containing return value(s)
- `ret_slots`: Number of slots to return, from `func_def.ret_slots`

## Bug 2: Infinite Compilation Retry

### Symptom
When JIT compilation failed (e.g., unsupported instruction), the same function would be compiled again on every call, causing stack overflow.

### Root Cause
In `jit_mgr.rs`, `compile_full` did not update `state` on compilation failure:

**Wrong:**
```rust
// Compile
self.compiler.compile(func_id, func_def, module)?;  // If fails, state stays Interpreted
```

**Correct:**
```rust
// Compile
if let Err(e) = self.compiler.compile(func_id, func_def, module) {
    info.state = CompileState::Unsupported;  // Mark as unsupported to prevent retry
    return Err(e);
}
```

### Why This Causes Infinite Loop
1. Function `fib` called, `call_count >= threshold`, `state == Interpreted` → compile
2. Compilation fails (e.g., unsupported opcode) → returns `Err`
3. `state` remains `Interpreted` (bug!)
4. Recursive call to `fib` → `call_count >= threshold`, `state == Interpreted` → compile again
5. Loop continues until stack overflow

## Fix Locations
- `crates/vo-jit/src/func_compiler.rs`: Fix Return instruction interpretation
- `crates/vo-vm/src/vm/jit_mgr.rs`: Update state on compilation failure

## Related Commit
Reference: commit `255082f` "fix(vo-jit): fix multiple bugs in JIT instruction translation"

## Lesson Learned
- Always check instruction format against bytecode spec
- Compilation failure should be recorded to prevent retry loops
