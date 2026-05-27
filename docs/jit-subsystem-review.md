# JIT 子系统调研与重构建议

日期：2026-05-27

## 范围

本次复核覆盖 `vo-jit` 的 full-function JIT、loop OSR、shared translator、direct JIT-to-JIT call、dynamic call inline cache、JIT/VM frame materialization、CopyN 编码与 loop liveness。结论基于源码阅读和现有 JIT/OSR 测试验证。

## 优先级一：必须修复

### 1. `reg_consts` 是线性状态，不能跨控制流粘滞

`reg_consts` 同时服务两类需求：算术安全检查优化（除零、移位）和需要编译期常量的元数据解码（map/slice element metadata）。旧实现只在 `LoadConst` 写入时记录，普通写入和控制流 merge 不系统失效，容易把某个分支里的常量误用于 join block。后果是 JIT 可能跳过本应保留的运行期 panic 检查。

修复方向：把常量事实建模成按 PC 的前向数据流，控制流 merge 只保留所有前驱一致的 `(reg, const)`；translator 的实际写入仍负责清除当前 slot，`LoadInt`/`LoadConst`/`Copy`/`CopyN` 再重新设置事实。这样不牺牲 map/slice metadata 在安全控制流里的 JIT 能力。

修复前失败用例：

- `cargo test -p vo-jit jit_shift_precheck_ignores_stale_branch_constant_fact`
- 旧实现实际结果：false 分支的动态 `-1` shift 被 stale `const 64` 覆盖，JIT 返回 `Ok`；修复后返回 `JitResult::Panic`。

补充覆盖：

- `tests/lang/cases/bugs/2026_05_27_jit_const_fact_control_flow.vo`
- 这个 `.vo` 用例用于跨 VM/JIT/WASM 保护控制流语义，但已经验证它在修复前也通过，不能当作 pre-fix failing repro。

### 2. `CopyN` 的 canonical count 在 `c`，JIT/OSR 不能读 `flags`

代码生成有两条路径：常规 helper 会把小 count 镜像到 `flags`，而转换等路径使用 `emit_op(Opcode::CopyN, dst, src, slots)`，也就是 `flags=0,c=slots`。旧 loop liveness 读 `flags`，会在这些路径把 CopyN 当成 0 slot copy，导致 OSR live-in/live-out 缺槽。另一个独立问题是 full-function JIT 的 `CopyN` 逐 slot 边读边写，遇到重叠区间时不满足 VM `ptr::copy` 的 memmove 语义。

修复方向：在 `Instruction` 上提供 `copy_n_count()`，所有 VM/JIT/analysis 都读这个统一入口；translator 对 CopyN 先读完整源临时，再写目标，保持和 VM `ptr::copy` 一样支持 overlap。

修复前失败用例：

- `cargo test -p vo-jit copy_n`
  - 旧实现 `test_get_read_regs_copy_n_canonical_count` 得到 `[]`，修复后得到 `[10, 11, 12]`。
  - 旧实现 `test_get_write_regs_multi_copy_n_large_canonical_count` 把 300 slots 截断成 255，修复后保留 300。
- `cargo test -p vo-jit jit_copy_n_overlap_matches_memmove_semantics`
  - 旧实现重叠 copy 返回 `[1, 1, 1]`；修复后返回 `[1, 2, 3]`。

补充覆盖：

- `tests/lang/cases/bugs/2026_05_27_jit_copy_n_canonical_osr.vo`
- 这个 `.vo` 用例用于继续覆盖源码层面的转换和 OSR 路径，但已经验证它在修复前也通过，不能当作 pre-fix failing repro。

### 3. direct JIT-to-JIT call 只检查 stack limit，不检查当前 fiber stack capacity

direct call 快路径会内联更新 `ctx.jit_bp/fiber_sp`，但旧实现只检查 `MAX_JIT_NATIVE_STACK_SLOTS`，没有确认当前 `fiber.stack.len()` 足够。深递归或连续 direct calls 后，如果 callee 发生 WaitIo/Call/Panic 并需要 spill/materialize 到 fiber.stack，就可能在未扩容的区间写入。

修复方向：在 full-function self recursion、known direct call、dynamic IC hit fast path统一走 `emit_stack_capacity_check`。当 `new_sp > ctx.stack_cap` 时，不降级 JIT 编译能力，而是走已有 VM call/prepare fallback，让 VM 的 `push_frame` 扩容并保持 frame materialization 语义。

修复前失败用例：

- `./d.py test jit tests/lang/cases/bugs/2026_05_27_jit_direct_stack_capacity_waitio.vo`
- 旧实现实际失败：`FFI post-call violation: resume_io_token was not consumed`，根因是 capacity 不足时仍走 direct JIT fast path，WaitIo materialization 看到不一致 frame 状态；修复后通过。

## 修复前验证口径

我用 `/private/tmp/volang-prefix` 的干净 `HEAD` worktree 只加测试、不加修复做了反证：

1. `jit_shift_precheck_ignores_stale_branch_constant_fact`：旧实现失败，`left: Ok, right: Panic`。
2. `jit_copy_n_overlap_matches_memmove_semantics`：旧实现失败，`left: [1, 1, 1], right: [1, 2, 3]`。
3. `copy_n` 单测组：旧实现两个 canonical count 用例失败。
4. `2026_05_27_jit_direct_stack_capacity_waitio.vo`：旧实现 JIT 目标失败。
5. `2026_05_27_jit_const_fact_control_flow.vo` 和 `2026_05_27_jit_copy_n_canonical_osr.vo`：旧实现也通过，所以只保留为覆盖用例，不再称为 P1 pre-fix repro。

## 优先级二：建议后续重构

1. helper call effect 分类：当前 `emit_funcref_call` 对所有 helper 全量 spill，正确但偏重。建议给 helper 标注 `may_gc/may_suspend/may_realloc_stack/may_observe_frame/may_write_slots`，只在需要时 spill 或 reload。
2. call lowering 继续收敛：direct call、self recursion、IC hit 都有相似的 ctx 更新、capacity/depth guard、OK/non-OK 分支。建议抽出 typed call plan，减少三处逻辑漂移。
3. metadata 解码不要长期依赖“值寄存器常量表”：map/slice metadata 最终应从 bytecode operand 或专门 metadata table 解码，`reg_consts` 只负责优化事实。
4. loop liveness 与 translator 写集共享：现在二者各自维护 opcode 写集，后续应抽成 `InstructionEffects`，统一 read/write/may_call/may_suspend/may_memory_alias。
5. JIT 回归测试分层：把 “source-level behavior”、“bytecode encoding”、“IR/unit analysis” 三层分开，避免只有 source 测试时漏掉 encoding 级别的退化。

## 修复原则

本轮修复不移除 JIT 能力。遇到 capacity 不足或动态 IC fast path 不满足条件时，选择 fallback 到已有 VM materialization 路径；遇到控制流常量不确定时，仅关闭该点的常量优化，不把函数整体踢出 JIT。
