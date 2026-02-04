# Hint Instruction Optimization Design

## Problem Statement

Current loop bytecode executes 2 Hint instructions per iteration:

```
pc=0: LoadInt idx, 0
pc=1: HINT_LOOP_BEGIN        ← executed every iteration (7 cycles wasted)
pc=2: GeI cmp, idx, limit
pc=3: JumpIf cmp, exit
      ...body...
pc=6: LoadInt one, 1
pc=7: AddI idx, idx, one
pc=8: HINT_LOOP_END          ← executed every iteration (wasted)
pc=9: Jump pc_-8             → jumps to pc=1
```

**Per-iteration instructions**: 7 (including 2 Hints)

## Goal

Reduce per-iteration instructions by:
1. Moving HINT_LOOP_BEGIN outside the loop (executed once)
2. Removing HINT_LOOP_END entirely
3. Moving hotspot detection to Jump instruction

## Target Structure

```
pc=0: LoadInt idx, 0
pc=1: HINT_LOOP_BEGIN        ← executed ONCE (loop metadata for JIT analysis)
pc=2: GeI cmp, idx, limit    ← loop_start (Jump target)
pc=3: JumpIf cmp, exit
      ...body...
pc=6: LoadInt one, 1
pc=7: AddI idx, idx, one
pc=8: Jump pc_-6             → jumps to pc=2, hotspot detection HERE
```

**Per-iteration instructions**: 5 (no Hints in hot path)

## Design Details

### 1. HINT_LOOP_BEGIN Changes

**Purpose**: Provide loop metadata for JIT static analysis (not runtime hotspot detection)

**Timing**: Executed once before loop entry

**Format** (unchanged):
```
flags = HINT_LOOP_BEGIN (1)
a = (depth << 4) | loop_flags
bc = exit_pc (32-bit)
```

**loop_flags**:
- bit 0: has_defer
- bit 1: has_labeled_break
- bit 2: has_labeled_continue

**New field needed**: `end_pc` for JIT liveness analysis

**Solution**: Encode `end_pc` as offset from `begin_pc` in upper bits of `a`:
```
a = (end_offset << 8) | (depth << 4) | loop_flags
```
Where `end_offset = end_pc - begin_pc` (max 255 instructions, sufficient for most loops)

### 2. HINT_LOOP_END Removal

**Current usage**: JIT uses HINT_LOOP_END position as `end_pc`

**After removal**: JIT reads `end_pc` from HINT_LOOP_BEGIN's encoded offset

**Fallback**: If loop is larger than 255 instructions, `end_offset = 0` signals JIT to scan for back-edge Jump

### 3. Jump Instruction Changes

**New behavior for back-edge Jump** (offset < 0):

```rust
Opcode::Jump => {
    let offset = inst.imm32();
    let target_pc = (frame.pc as i64 + offset as i64 - 1) as usize;
    
    #[cfg(feature = "jit")]
    if offset < 0 {  // Back-edge = loop iteration
        // Hotspot detection and OSR trigger
        if let Some(result) = self.try_loop_osr_by_target(fiber_id, func_id, target_pc, bp) {
            match result {
                OSR_RESULT_FRAME_CHANGED => { refetch!(); continue; }
                OSR_RESULT_WAITIO => { return Block(Io(token)); }
                exit_pc => { frame.pc = exit_pc; refetch!(); continue; }
            }
        }
    }
    
    frame.pc = target_pc;
}
```

### 4. JIT Loop Analysis Changes

**Current** (`loop_analysis.rs`):
```rust
HINT_LOOP_BEGIN => { stack.push(builder); }
HINT_LOOP_END => { loops.push(builder.finish(pc, code)); }
```

**After**:
```rust
HINT_LOOP_BEGIN => {
    let end_offset = (inst.a >> 8) as usize;
    let end_pc = if end_offset > 0 { pc + end_offset } else { find_back_edge(code, pc) };
    loops.push(LoopInfo { begin_pc: pc + 1, end_pc, ... });
}
// No HINT_LOOP_END handling needed
```

**Note**: `begin_pc` is now `pc + 1` (the instruction after HINT_LOOP_BEGIN, i.e., the condition check)

### 5. Loop Lookup Changes

**Current**: `find_loop_by_header(loops, hint_pc)` matches `begin_pc == hint_pc`

**After**: `find_loop_by_target(loops, jump_target)` matches `begin_pc == jump_target`

Since `begin_pc` is now the condition check (not HINT position), Jump target matches directly.

## Files to Modify

| File | Changes |
|------|---------|
| `vo-codegen/src/func.rs` | `enter_loop`: emit HINT before loop_start, encode end_offset |
| `vo-codegen/src/func.rs` | `exit_loop`: remove `emit_hint_loop_end` call |
| `vo-codegen/src/stmt/for_range.rs` | `IndexLoop::begin`: set loop_start after HINT |
| `vo-codegen/src/stmt/for_loop.rs` | Same pattern for three-part for loops |
| `vo-vm/src/vm/mod.rs` | `Opcode::Hint`: make HINT_LOOP_BEGIN a no-op |
| `vo-vm/src/vm/mod.rs` | `Opcode::Jump`: add back-edge detection and OSR |
| `vo-jit/src/loop_analysis.rs` | Read end_pc from HINT, remove HINT_LOOP_END logic |
| `vo-jit/src/jit_mgr.rs` | `find_loop`: use new lookup by target |

## Edge Cases

### 1. Nested Loops
- Each loop has its own HINT_LOOP_BEGIN with unique begin_pc
- Jump targets correctly identify which loop

### 2. Large Loops (> 255 instructions)
- `end_offset = 0` signals fallback
- JIT scans forward from begin_pc to find back-edge Jump

### 3. Infinite Loops (no condition)
- `exit_pc = 0` in HINT_LOOP_BEGIN (existing convention)
- JIT handles appropriately

### 4. Multiple Exit Points
- `exit_pc` in HINT is the primary exit (after condition fails)
- break statements create additional exits (handled by JIT)

### 5. Labeled break/continue
- Flags in HINT_LOOP_BEGIN indicate presence
- JIT marks loop as non-jittable if present

## Performance Impact

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Instructions per iteration | 7 | 5 | -28% |
| Hint executions per iteration | 2 | 0 | -100% |
| Jump overhead (JIT mode) | 0 | ~5 cycles | Negligible |

## Testing Plan

1. Run all VM tests: `./d.py test vm`
2. Run all JIT tests: `./d.py test jit`
3. Verify loop benchmarks (sieve, fannkuch) show improvement
4. Verify nested loops work correctly
5. Verify labeled break/continue still prevent JIT

## Future: ForLoop Instruction

After this optimization, ForLoop instruction can be added:

```
pc=0: LoadInt idx, 0
pc=1: HINT_LOOP_BEGIN
pc=2: LtI cmp, idx, limit
pc=3: JumpIfNot cmp, exit
      ...body...
pc=6: ForLoop idx, limit, -4   ← replaces LoadInt+AddI+Jump, does OSR
```

ForLoop combines:
- `idx++`
- `if idx < limit goto loop_start`
- Hotspot detection (inherits from Jump optimization)

**Per-iteration with ForLoop**: 1 instruction (vs current 7)
