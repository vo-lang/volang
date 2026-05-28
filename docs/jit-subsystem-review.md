# JIT 子系统调研与重构建议

日期：2026-05-27

## 范围

本次复核覆盖 `vo-jit` 的 full-function JIT、loop OSR、shared translator、direct JIT-to-JIT call、dynamic call inline cache、JIT/VM frame materialization、CopyN 编码与 loop liveness。结论基于源码阅读和现有 JIT/OSR 测试验证。

## 2026-05-28 strict JIT 当前状态

当前状态：本轮重新分析中已知 P1/P2 问题已完成修复，没有再发现必须关闭 JIT 能力才能规避的严重问题。JIT runtime panic 通过 typed trap ABI 回传 `RuntimeTrapKind`、动态 panic 参数和 bytecode pc；显式 `panic()` 与 extern `ExternResult::Panic` 使用独立的 `user_panic_pc` 记录用户 panic pc，避免复用 WaitIo/Replay/call materialization 的 `call_resume_pc`，也避免把 user panic 混入 typed trap 的 `runtime_trap_pc`。VM 侧在 unwind 前恢复 trap kind、panic message 和 source location，避免除零、负 shift、bounds、nil map write、unhashable/uncomparable、type assertion、make slice/chan/port、queue callback panic、builtin assert panic 等路径落回默认 nil pointer 或泛化 `runtime error: JIT panic` 文案。`RunMode::Jit` 下 JIT 初始化失败会直接返回 runtime error；feature 关闭时 `RunMode::Jit` 也 fail fast，不再 warning 后回退 VM。hot full-function 编译失败、InvalidMetadata、LoopAnalysis、Codegen/Internal、OSR bad metadata/bad LoopEnd/missing layout、JIT extern `NotRegistered` 等错误会 fail fast 到 `VmError::Jit`，只保留 cold/not-hot、WaitIo/WaitQueue/Yield/Replay、explicit VM call fallback、OSR normal exit、stack-capacity trampoline 等语义 fallback。

完成项：

1. 新增 `JitRuntimeTrapKind` 与 `vo_runtime_trap` helper，JIT lowering 对除零、bounds、负 shift、nil map write、unhashable/uncomparable、type assertion、make slice/chan/port 和 queue callback trap 记录 typed kind、消息参数和 pc。make slice/chan/port 使用 runtime helper 返回的 `alloc_error` code 生成消息，和 VM 的 `makeslice_error_message` / `make_queue_error_message` 分类一致。
2. `setup_jit_panic` 保留 VM/JIT trap kind。typed runtime trap 使用 `runtime_trap_pc` 设置 panic source location；显式 `panic()`、builtin/extern panic 使用 `user_panic_pc`；`call_resume_pc` 只保留为旧路径兜底，不再作为 extern panic 的主位置来源。callback trap 改为 `set_recoverable_trap`，不再退化成普通 user panic。
3. `JitManager::resolve_call` 改为 `Result<Option<JitFunc>, JitError>`：`Ok(None)` 只表示 cold interpreter fallback，compile/metadata/internal/codegen 失败会向 VM 传播。dynamic closure/interface callee 预编译错误也会返回 visible JIT error。
4. loop OSR 的 `get_or_compile_loop` 改为 fail-fast：坏 `LoopEnd`、缺 metadata/layout/extern、slot overflow、compile failure 和 previously failed loop 不再 mark failed 后静默解释执行。
5. `LoopEnd` invariant 集中到 verifier：metadata 长度/kind/range/offset 一致性之外，`end_pc` 必须是跳回 `begin_pc` 的 `Jump` 或 `ForLoop` back-edge；compact offset 和 explicit metadata 都受同一校验保护。
6. loop analysis 的 public API 收紧为 `try_analyze_loops_with_module`；无 module extern/context 的 helper 只保留给本模块测试使用，避免生产代码误用空 extern/metadata context。
7. codegen 的 dynamic elem metadata overflow 不再返回 `JitInstructionMetadata::None`，改为 fail-fast；`loop_compiler` 的 explicit `Panic` lowering 缺少 panic helper 时与 full compiler 一样直接报 invariant 失败。
8. `Vm::with_best_effort_jit_config` / `init_jit` 明确保留非严格、机会性 JIT 语义；严格路径使用 `try_with_jit_config` / `try_init_jit`。island thread 继承 JIT config 时使用严格初始化，失败以明确 fatal message 终止该 island thread。
9. JIT extern `NotRegistered` 视为 JIT/registry 内部不变量破坏，返回 `JitResult::JitError`，调度层转成 `VmError::Jit`；不允许再落到泛化 `runtime error: JIT panic`，用户代码不能 recover 掉该基础设施错误。
10. capability matrix 继续和 call route/dynamic call/addr lowering/helper allocation tests 同步；`tests/lang/cases/jit/runtime_traps.vo` 现在断言 VM/JIT recover message 精确一致，覆盖 make slice/chan/port 和 send-on-closed queue callback。
11. JIT extern `ExternResult::Panic` 会记录当前 `CallExtern` pc 到 `user_panic_pc`，不再通过 `call_resume_pc - 1` 推断 source location；显式 `panic()` lowering 同样写入当前 bytecode pc。
12. Queue callback 的 `QueueAction::Trap(kind)` / `QueueRecvCoreResult::Trap(kind)` 统一补齐 typed trap payload，不再存在理论不可达分支静默返回缺少 payload 的 `JitResult::Panic`。JIT stack overflow callback 也记录 trap pc 并以 `RuntimeTrapKind::StackOverflow` 暴露。

新增关键回归：

- `vo-engine::run::tests::jit_division_by_zero_preserves_runtime_trap_kind_message_and_location`：修复前 JIT 除零会报告 nil pointer；修复后和 VM 一致为 `DivisionByZero`、`runtime error: integer divide by zero`，且 error location 一致。
- `vo-engine::run::tests::{jit_negative_shift,jit_bounds_check,jit_nil_map_write,jit_type_assertion,jit_interface_eq,jit_map_hash,jit_queue_callback}_preserves_runtime_trap_kind_message_and_location`：覆盖 runtime trap kind/message/location parity。
- `vo-engine::run::tests::{jit_make_slice_negative_len,jit_make_slice_len_larger_than_cap,jit_make_chan_negative_size,jit_make_port_negative_size}_preserves_runtime_trap_kind_message_and_location`：修复前 JIT 只返回 `runtime error: makeslice` / `makechan` / `makeport`；修复后精确匹配 VM 的 `len out of range`、`len larger than cap`、`size out of range`。
- `vo-engine::run::tests::jit_extern_assert_panic_preserves_message_and_location`：修复前 JIT builtin assert 的 extern panic location 为上一条指令 pc；修复后 message 与 location 均和 VM 一致。
- `vo-engine::run::tests::jit_explicit_panic_preserves_message_and_location`：保护显式 user panic 的 message/location parity，避免 `user_panic_pc` 机制只覆盖 extern panic。
- 既有语言回归 `dyn-call-return-multi::jit` 保护 variable-size `dyn_call` extern 的每个 call site 使用独立 ret-slot metadata，避免同名 extern 的较大返回槽数污染较小返回槽数的 JIT 写集校验。
- 既有语言回归 `skill-debug-vo.2026-01-23-zero-size-type::jit`、`skill-debug-vo.2026-01-28-1420-empty-struct-slice::jit`、`empty-struct-chan-signal::jit` 和 `dyn.dyn-empty-struct::jit` 保护 JIT 接受 zero-size slice/array element layout，并在 bounds check 后按 0-slot get/set/append 语义执行。
- `vo-engine::run::tests::{strict_jit_full_compile_invalid_metadata_fails_fast,strict_jit_osr_loop_analysis_error_fails_fast,strict_jit_dynamic_callee_precompile_error_fails_fast,strict_jit_extern_not_registered_fails_fast}`：覆盖 full JIT、OSR、dynamic callee precompile、extern registry invariant 的 strict fail-fast。
- `vo-engine::run::no_jit_tests::jit_mode_without_jit_feature_fails_fast`：`cargo test -p vo-engine --no-default-features` 下确认 `RunMode::Jit` 不再 warning 后回退 VM。
- `vo-vm::vm::island_thread::tests::island_jit_config_init_error_is_propagated`：覆盖 island JIT config 初始化失败传播 helper，生产入口使用同一 helper 并以明确 fatal message 终止。
- `vo-jit::verifier` LoopEnd back-edge tests、`vo-codegen::func::tests::elem_layout_metadata_overflow_is_not_silently_dropped`、`tests/lang/cases/jit/runtime_traps.vo`。语言回归用例现在包含 recover 到 builtin assert extern panic 的用户可见文案 `"assertion failed: boom"`。

本轮验证命令：

- `cargo test -p vo-common-core`
- `cargo test -p vo-jit`
- `cargo test -p vo-codegen`
- `cargo test -p vo-vm --features jit`
- `cargo test -p vo-engine`
- `cargo test -p vo-engine --no-default-features`
- `./d.py test tests/lang/cases/jit`
- `./d.py test`（全量 lang manifest；覆盖 VM/JIT/OSR/nostd/WASM 中未被 `tests/lang/cases/jit` 子集覆盖的回归）
- `./d.py test tests/lang/cases/bugs`（需要 loopback preflight，已在允许本地网络后通过）
- `git diff --check`

当前 review 结论：扫描 `vo-jit`、VM JIT runtime、`jit_mgr`、codegen metadata、bytecode serialization 后，未发现新的 silent fallback、metadata 猜测、panic payload 缺失或必须牺牲 JIT 覆盖面的语义偏差。source-level `recover()` 对 runtime trap payload 的具体字符串由 `tests/lang/cases/jit/runtime_traps.vo` 精确断言；Rust 层 VM/JIT parity 测试覆盖 runtime trap、显式 user panic 和 extern panic 的 message/location。

合法 fallback 清单：cold/not-hot interpreter execution、WaitIo/WaitQueue/Yield/Replay、regular/prepared call VM materialization、OSR normal exit、direct-call stack-capacity trampoline、以及显式命名的 best-effort embedding API。这些 fallback 是调度、host I/O、stack capacity 或 embedding 语义边界，不是 JIT 编译失败的静默吞错。

当前验证覆盖：本轮要求的 `cargo test -p vo-jit`、`cargo test -p vo-vm --features jit`、`cargo test -p vo-engine`、`cargo test -p vo-engine --no-default-features`、`cargo test -p vo-common-core`、`cargo test -p vo-codegen`、`./d.py test tests/lang/cases/jit`、`./d.py test tests/lang/cases/bugs`、`./d.py test` 全量 lang manifest 和 `git diff --check` 构成本轮回归门槛。单点反证已经覆盖 extern assert panic location：旧实现 JIT 为 `Some((0, 4))`，VM 为 `Some((0, 5))`；修复后两者一致。全量 lang manifest 的反证覆盖额外发现的 `dyn_call` ret-slot metadata 污染和 zero-size element layout 支持缺口。

剩余风险：JIT helper effect 分类仍偏保守，部分 helper 仍采用全量 spill；未来新增 callback 或 typed trap 时必须继续显式设置 panic pc 与 payload。当前未发现需要移除 JIT opcode 能力的残留风险。

## 2026-05-28 后续重构完成项

本轮后续重构没有移除已有 JIT opcode 能力；唯一收紧的是 `has_defer` 的自递归 direct-native call 优化：这类调用现在走 VM fallback，以保证 defer/recover 依赖的真实 `CallFrame` 深度不被 native shadow call chain 隐藏。函数本身仍可 JIT，降级的是不安全的 direct-call 快路径。

完成项：

1. 增加 `verify_jit_metadata`，在 full-function JIT 与 loop OSR 编译入口校验 metadata 长度、opcode kind、metadata kind、elem layout，以及 effects 推导出的 slot 读写是否落在 `local_slots` 内。zero-size element layout 是合法语言语义：`ElemLayout { elem_bytes: 0 }` 现在被建模为 0-slot layout，JIT lowering 对 get/set 在 bounds check 后不读写 value slot；不合法的是 zero-size layout 搭配 sign-extension。
2. 增加机器可读 `capability_matrix()` / `opcode_capability()`。每个 `Opcode` 必须显式声明 full JIT、OSR 与 fallback 策略，新增 opcode 漏声明会在编译期 match exhaustiveness 或单测中暴露。
3. 把复杂 opcode 的 packed 语义集中到 `Instruction` accessor：static call func id、closure func id、defer/go shared call shape、packed arg/ret slots、queue recv/select recv flags、MapNew/MapIterNext slot counts。VM、JIT、effects、formatter 改为复用同一语义入口。
4. 拆分 `translate.rs`：scalar、memory、conversions、collections、runtime ops 独立成 opcode-family lowering 文件；父模块只保留 dispatcher 与少量 shared checks。
5. 新增 `FunctionAnalysis`，full-function JIT 与 loop OSR 共享 metadata-aware reg const facts、effects 和 `memory_only_start` 推导。loop analysis 现在从 `Module` 读取 extern return slot metadata。
6. 建立 fallback reason 可观测性：`JitFallbackReason` 和 `JitFallbackReasonStats` 记录 cold interpreter fallback、unsupported/compile failure、regular/prepared call fallback、yield/block、WaitIo/WaitQueue/Replay、loop not hot/compile failed。
7. 系统化 frame materialization invariant：`materialized_jit_frame_invariants` 校验 materialization 后 `resume_stack` 清空、frame func_id 有效、GC scan extent 不越过当前 `fiber.sp`、scan slot 不超过函数 metadata、最内层 frame 的 local extent 被 `fiber.sp` 覆盖，并显式允许 borrowed-call parent 的完整 local extent 高于当前 callee `sp`。
8. 统一 static call route：`CallPlan` 先选择 `SelfRecursiveNative`、`KnownDirectJit`、`DynamicJitTable`、`VmFallback`，full-function 与 OSR call lowering 共享同一 plan。closure/interface dynamic call 也有 `DynamicCallPlan` 统一 packed arg/ret/resume_pc 解码。
9. 固化 runtime ABI 边界：`runtime_symbol_names()` 与 `jit_context_abi_fields()` 提供 helper symbol 和 `JitContext` offset 的机器可读 manifest，runtime/JIT 单测保证 manifest 与注册表同步。
10. defer/recover JIT 支持选择为保留支持，不拒绝。已有 `DeferPush`/`ErrDeferPush`/`Recover` lowering 继续使用 runtime callback；新增递归 defer fallback 回归用例保护 direct-call 收紧后的 frame 深度语义。
11. 增加 JIT 专项 benchmark manifest：`jit-map`、`jit-slice`、`jit-call`、`jit-loop`、`jit-copy`，用于后续 `./d.py bench vo --jit-hot` 做性能回归观察。
12. 移除 loop analysis 对未编码 `HINT_LOOP` end offset 的 back-edge 扫描路径。大循环必须由 codegen 写入 `JitInstructionMetadata::LoopEnd`；缺失、与紧凑 offset 不一致、或指向的 end 不是 `Jump`/`ForLoop` back-edge 时直接 fail fast，同时保留大循环 OSR 能力。

测试分层：

- source behavior：新增 `tests/lang/cases/bugs/2026_05_28_jit_recursive_defer_fallback.vo`，并保留既有 WaitIo、direct-call fallback、nested JIT calls、panic/recover、loop defer/OSR cases。
- bytecode/codegen encoding：`vo-common-core` 的 complex instruction accessor/metadata serialization 单测，`vo-codegen` 继续覆盖 JIT metadata 生成。
- vo-jit unit/effects/liveness：metadata verifier、capability matrix、call plan route、dynamic call plan、shared analysis、metadata-aware effects、loop liveness 和 `LoopEnd` fail-fast 单测。
- VM/JIT frame/runtime boundary：`vo-vm --features jit` 覆盖 fallback stats、nested materialization invariant、borrowed-parent materialization invariant；`vo-runtime` 覆盖 JIT runtime symbol/ABI manifests。

结构性重构说明：

- metadata verifier、capability matrix、translate split、ABI manifest、benchmark manifest 是 invariant/maintainability 改动，不对应单一 pre-fix failing runtime case；因此用结构性单测、manifest lint 和编译期 exhaustiveness 作为保护。
- packed metadata register 路径已不再作为 JIT layout fallback。动态 element/map layout 必须来自 per-instruction `jit_metadata`；缺失时 verifier/effects/loop analysis 显式报错，fail fast。
- `HINT_LOOP` 的未编码 end offset 不再通过扫描 bytecode 恢复。`end_offset == 0` 表示必须存在 `LoopEnd` metadata；这是显式 metadata 边界，不是兼容兜底。

## 优先级一：必须修复

### 1. `reg_consts` 是线性状态，不能跨控制流粘滞

`reg_consts` 只服务算术安全检查优化（除零、移位等）。旧实现还把它当作 map/slice layout metadata 的兼容来源；这个路径已经移除，动态 layout 只认 `jit_metadata`。旧的常量事实如果只在 `LoadConst` 写入时记录，普通写入和控制流 merge 不系统失效，仍容易把某个分支里的常量误用于 join block。后果是 JIT 可能跳过本应保留的运行期 panic 检查。

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
3. 保持 metadata fail-fast 边界：map/slice 动态 layout 和大循环 `LoopEnd` 只从 `jit_metadata` 解码，`reg_consts` 只负责优化事实；新增 opcode 或新动态 layout 不允许回到“值寄存器常量表”兜底。
4. loop liveness 与 translator 写集共享：现在二者各自维护 opcode 写集，后续应抽成 `InstructionEffects`，统一 read/write/may_call/may_suspend/may_memory_alias。
5. JIT 回归测试分层：把 “source-level behavior”、“bytecode encoding”、“IR/unit analysis” 三层分开，避免只有 source 测试时漏掉 encoding 级别的退化。

## 修复原则

本轮修复不移除 JIT 能力。遇到 capacity 不足或动态 IC fast path 不满足条件时，选择 fallback 到已有 VM materialization 路径；遇到控制流常量不确定时，仅关闭该点的常量优化，不把函数整体踢出 JIT。
