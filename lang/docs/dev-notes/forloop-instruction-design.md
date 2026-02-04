# ForLoop Instruction Design

## Overview

Add a `ForLoop` instruction that combines `idx++` + comparison + conditional jump into one instruction.

**Key optimization**: ForLoop jumps directly to body_start (skipping initial bounds check), reducing per-iteration overhead from **5 instructions to 1** (80% reduction).

---

## Current State (After Hint Optimization)

```
pc=0: LoadInt idx, 0
pc=1: HINT_LOOP                    ← executed once
pc=2: GeI cmp, idx, limit          ← loop_start - EVERY iteration
pc=3: JumpIf cmp, exit             ← EVERY iteration
      ...body...
pc=N-2: LoadInt one, 1             ← EVERY iteration
pc=N-1: AddI idx, idx, one         ← EVERY iteration
pc=N:   Jump loop_start            ← EVERY iteration (back-edge → OSR)
exit:
```

**Per-iteration**: 5 instructions (GeI + JumpIf + LoadInt + AddI + Jump)

---

## Target State

```
pc=0: LoadInt idx, 0
pc=1: LtI cmp, idx, limit          ← ONCE (initial bounds check)
pc=2: JumpIfNot cmp, exit          ← ONCE
pc=3: HINT_LOOP                    ← begin_pc = body_start (pc=4)
pc=4: [body_start]                 ← ForLoop target, JIT OSR entry
      ...body...
pc=N:   ForLoop idx, limit, offset ← EVERY iteration
[pc=N+1: Trunc idx, width]         ← only for int8/16/32
exit:
```

**Per-iteration**: 1 instruction (or 2 for small integers)

---

## ForLoop Instruction

### Format

```
┌────────┬────────┬────────┬────────┬────────┐
│ op (8) │flags(8)│ a (16) │ b (16) │ c (16) │
└────────┴────────┴────────┴────────┴────────┘

a = idx_slot
b = limit_slot
c = jump_offset (signed 16-bit, relative to pc+1)

flags:
  bit 0: 0=signed, 1=unsigned
  bit 1: 0=increment (+1), 1=decrement (-1)
```

### Semantics

```rust
fn exec_forloop(idx_slot, limit_slot, offset: i16, flags: u8) {
    let idx = stack[idx_slot];
    let limit = stack[limit_slot];
    
    // 1. Increment/decrement
    let next = if (flags & 0x02) != 0 {
        idx.wrapping_sub(1)
    } else {
        idx.wrapping_add(1)
    };
    stack[idx_slot] = next;
    
    // 2. Compare
    let signed = (flags & 0x01) == 0;
    let decrement = (flags & 0x02) != 0;
    let cont = if decrement {
        if signed { (next as i64) > (limit as i64) } else { next > limit }
    } else {
        if signed { (next as i64) < (limit as i64) } else { next < limit }
    };
    
    // 3. Conditional jump
    if cont { pc += offset; }
}
```

---

## Small Integer Types (Phase 2)

**Note**: `for i := range n` always uses `int` (64-bit) index. Small integer types only apply to three-part for loops like `for i := int8(0); i < 10; i++`.

ForLoop + Trunc for small integers (Phase 2 only):

| 类型 | 指令 |
|------|------|
| int64/uint64 | ForLoop |
| int8/16/32 | ForLoop + Trunc |

现有 `Trunc` 指令 flags：
- `0x81` = int8, `0x82` = int16, `0x84` = int32
- `0x01` = uint8, `0x02` = uint16, `0x04` = uint32

---

## Codegen Changes

### IndexLoop::begin() - 改变顺序

```rust
// 当前
func.emit_op(Opcode::LoadInt, idx_slot, 0, 0);
func.enter_loop(0, label);                    // ← HINT_LOOP
let loop_start = func.current_pc();           // ← 条件检查
func.set_loop_start(loop_start);
func.emit_op(Opcode::GeI, cmp_slot, idx_slot, len_slot);
let end_jump = func.emit_jump(Opcode::JumpIf, cmp_slot);

// ForLoop 后
func.emit_op(Opcode::LoadInt, idx_slot, 0, 0);
// 初始边界检查在 HINT_LOOP 之前
let cmp_slot = func.alloc_slots(&[SlotType::Value]);
func.emit_op(Opcode::LtI, cmp_slot, idx_slot, len_slot);
let end_jump = func.emit_jump(Opcode::JumpIfNot, cmp_slot);
// HINT_LOOP 在边界检查之后
func.enter_loop(0, label);
let body_start = func.current_pc();           // ← body 开始
func.set_loop_start(body_start);
```

### IndexLoop::end() - 生成 ForLoop

```rust
// 当前
let post_pc = func.current_pc();
func.emit_op(Opcode::LoadInt, one, 1, 0);
func.emit_op(Opcode::AddI, idx_slot, idx_slot, one);
let exit_info = func.exit_loop();
let end_pc = func.current_pc();
func.emit_jump_to(Opcode::Jump, 0, loop_start);

// ForLoop 后
let post_pc = func.current_pc();              // ← continue 目标
let exit_info = func.exit_loop();
let end_pc = func.current_pc();
let offset = (body_start as i32) - (end_pc as i32 + 1);
func.emit_forloop(idx_slot, limit_slot, offset as i16, flags);
// 小整数类型：追加 Trunc
if let Some(trunc_flags) = trunc_flags_for_type(idx_type) {
    func.emit_with_flags(Opcode::Trunc, trunc_flags, idx_slot, idx_slot, 0);
}
```

---

## HINT_LOOP 变化

| 字段 | 当前 | ForLoop 后 |
|------|------|------------|
| begin_pc | 条件检查 (GeI) | body_start |
| end_pc | Jump 位置 | ForLoop 位置 |
| exit_pc | 循环后第一条 | 循环后第一条 |

HINT_LOOP 仍然保留，提供 JIT 静态分析所需的元数据。

---

## JIT OSR

### 当前

```rust
Opcode::Jump => {
    if offset < 0 {  // back-edge
        // target_pc = loop_start (条件检查)
        self.try_loop_osr(fiber_id, func_id, target_pc, bp);
    }
}
```

### ForLoop 后

```rust
Opcode::ForLoop => {
    // ... increment + compare ...
    if cont {
        let target_pc = (pc as i64 + offset as i64) as usize;
        // target_pc = body_start (跳过条件检查)
        #[cfg(feature = "jit")]
        if let Some(result) = self.try_loop_osr(fiber_id, func_id, target_pc, bp) {
            // ... handle OSR result ...
        }
        pc = target_pc;
    }
}
```

OSR 入口从"条件检查"变为"body_start"，与 ForLoop 跳转目标一致。

---

## break/continue

| 语句 | 当前跳转目标 | ForLoop 后 |
|------|-------------|------------|
| break | exit_pc | exit_pc (不变) |
| continue | post_pc (LoadInt) | post_pc (ForLoop) |

continue 跳到 ForLoop 指令，执行 `idx++; if idx < limit goto body_start`。

---

## Loop Patterns

### 1. for i := range n

```go
for i := range n { body }
```

```
LoadInt idx, 0
LtI cmp, idx, n
JumpIfNot cmp, exit
HINT_LOOP                  // begin_pc = body_start
body_start:
  Copy i, idx              // if key variable used
  ...body...
  ForLoop idx, n, offset   // → body_start
exit:
```

### 2. for i := range arr/slice (index only)

Same as above, with `len := SliceLen arr` before loop.

### 3. for i := 0; i < n; i++ (Phase 2)

Pattern recognition required - can reuse ForLoop.

### 4. Decrementing loop (Phase 2)

```go
for i := n-1; i >= 0; i-- { }
```

flags = 0x02 (decrement), uses `>` comparison.

---

## Files to Modify

| File | Change |
|------|--------|
| `vo-common-core/src/instruction.rs` | Add `ForLoop` opcode |
| `vo-vm/src/vm/mod.rs` | Implement ForLoop + OSR |
| `vo-codegen/src/func.rs` | Add `emit_forloop()` |
| `vo-codegen/src/stmt/for_range.rs` | Reorder: bounds check → HINT → body → ForLoop |
| `vo-codegen/src/type_info.rs` | Add `trunc_flags_for_type()` |
| `vo-jit/src/translate.rs` | Translate ForLoop |
| `vo-jit/src/loop_analysis.rs` | Add ForLoop to read/write analysis |

---

## Edge Cases

### 1. Empty range (n ≤ 0)

Initial bounds check (`LtI` + `JumpIfNot`) handles this - jumps to exit.

### 2. Single iteration (n = 1)

- Initial: 0 < 1 → enter
- ForLoop: idx=1, 1 < 1 → false → exit

### 3. Integer overflow (int8)

ForLoop increments as i64, then Trunc handles wrap-around.
Comparison uses pre-truncated value (correct Go semantics).

### 4. Nested loops

Each loop has own HINT_LOOP + ForLoop. OSR uses target_pc to find correct loop.

### 5. Labeled break/continue

Sets flags in HINT_LOOP. JIT marks loop non-jittable. VM still works correctly.

---

## Performance

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Per-iteration | 5 | 1 | **-80%** |
| Small int | 5 | 2 | **-60%** |

---

## Implementation Order

1. Add `ForLoop` opcode
2. Implement VM execution + OSR
3. Add `emit_forloop()` helper
4. Add `trunc_flags_for_type()` helper
5. Modify `IndexLoop` (begin + end)
6. Run VM tests
7. Add JIT translation
8. Update loop_analysis
9. Run JIT tests
10. Run benchmarks
