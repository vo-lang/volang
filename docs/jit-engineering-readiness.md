# JIT 工程可用性边界

日期：2026-06-02（本轮收口审计始于 2026-06-01）

本说明记录 Volang JIT 当前进入工程可用状态依赖的硬边界。结论基于本轮对 `vo-jit` verifier/lowering、`vo-vm` JIT bridge、`vo-common-core` bytecode serialization、`vo-codegen` metadata 生产路径的复核和回归测试。

## 已建立的不变量

`FunctionDef` 是 JIT 的 ABI 输入。严格 verifier 会先校验函数级不变量，再检查单条指令：

- `local_slots == slot_types.len()`，`param_slots <= local_slots`，`recv_slots <= param_slots`。
- `ret_slots == ret_slot_types.len()`；error return slot 必须是 return 区间内的双槽 interface。
- `gc_scan_slots` 与 `borrowed_scan_slots_prefix` 必须由 `slot_types` 派生一致。
- `has_defer`、`has_calls`、`has_call_extern` 必须由 bytecode 派生一致。
- heap return 的 GcRef 起点、数量和 `heap_ret_slots` 必须自洽，且对应 local slot 必须是 `GcRef`。
- closure function 必须保留 slot 0 为 `GcRef` closure self。
- cross-island `capture_types` / `param_types` 的 `ValueMeta` 与 `ValueRttid` 低 8 位必须是合法 `ValueKind`。

## Fail-fast 边界

反序列化边界现在拒绝非法 `SlotType`、`ValueKind`、`RuntimeType`、`ChanDir`、`ExtSlotKind` 和未知 JIT metadata tag。`ValueMeta` / `ValueRttid` raw 值会先验证低 8 位 kind tag，再进入 runtime 对象。

JIT verifier 覆盖以下 slot/layout 合约：

- `Copy` 与 `CopyN` 都要求源/目标 layout 精确一致，禁止 `GcRef`、`Value`、`Interface*`、`Float` 错配。
- `CallClosure` callee、`GoIsland` island/closure 操作数必须来自 `GcRef` slot。
- `JumpIf` / `JumpIfNot` 条件、`ForLoop` index/limit、`MapLen` destination 必须是 `Value` slot。
- `JumpIf` / `JumpIfNot` / `ForLoop` 的 semantics matrix 必须声明 `LocalSlotLayout` verifier requirement，并且 fail-fast policy 必须覆盖 `LayoutMismatch`。这使 verifier 规则、effects/contract graph 和 lowering 文档无法再次静默漂移。
- `IfaceAssign` flags 必须是合法 `ValueKind`；interface source 必须是 `[Interface0, Interface1]`；引用/boxed 类型 source 必须是 `GcRef`；float source 必须是 `Float`；primitive source 必须是 `Value`。
- `Return` flags 只能使用 `RETURN_FLAG_ERROR_RETURN` 与 `RETURN_FLAG_HEAP_RETURNS`；未知 flag bit 在 strict verifier 中返回 `InvalidInstructionFlags`，不得进入 lowering 或解释器兼容路径。
- 已移除的 map metadata tag 不再是兼容格式；反序列化和 VM 共享 `ModuleVerifier` 都必须在执行前拒绝它们，strict JIT verifier 只处理当前精确 layout。

VM/JIT bridge 对 nil 和错类型 GC 对象分层处理：nil closure/island 走语言 runtime trap；非 nil 但 GC header kind 不匹配的 closure/island 对象走 JIT infra error，避免在 helper 边界猜 header layout。

## 当前架构边界

- `vo-common-core::verifier::ModuleVerifier` 是 VM 共享的 bytecode/module 合法性入口，并返回 `VerifiedModule` token。普通 VM `load`、best-effort JIT `load`、strict JIT `load` / `try_init_jit` 都必须先通过它；strict JIT 随后只追加 lowering capability、helper/ABI、OSR/frame/side-exit/direct-call 等 JIT 专属检查。
- `vo-jit/src/metadata_contract.rs` 是当前 opcode metadata requirement 的单一事实源。`semantics` 从这里读取 requirement，`verifier` 从这里拒绝 missing/wrong metadata，`contract_graph` 只列当前 strict lowering schema。
- `vo-jit/src/verifier.rs` 只保留 strict JIT 公开入口和模块级扫描；`vo-common-core/src/verifier.rs` 负责 VM 通用 function invariant、per-PC metadata kind、slot/layout/global range、call/extern/return/transfer、branch target、GC layout/barrier 和派生字段一致性；`verifier/metadata_checks.rs` 负责 JIT lowering metadata gate 与 `LoopEnd` consistency；`verifier/errors.rs` 负责 strict JIT 错误类型和显示文本。
- `call_helpers.rs` 保留实际 call lowering 编排；`call_helpers/plan.rs` 封装 static/dynamic call shape 与 direct/IC/VM side-exit route；`call_helpers/callback_abi.rs` 封装 JitContext callback ABI manifest、callsite kind 校验和 checked/raw/returning wrapper；`call_helpers/result_flow.rs` 封装 checked helper result、non-OK JIT result materialization 和通用 `JitResult` flow。closure/interface dynamic call 共享 IC scratch、IC entry、hit/miss branch、prepare callback 参数和 return-copy skeleton；nil check、receiver/method key、slot0/capture 差异仍显式保留。
- `vo-jit/src/helpers.rs` 用单一 helper table 展开 runtime helper 名字、`HelperFuncIds` 字段、Cranelift import declaration 和 per-function `HelperFuncs` refs；helper signature/panic/GC/scheduling ABI 继续来自 `vo-runtime::jit_api::runtime_helper_abi_fields()` manifest。
- `vo-jit/src/compile_common.rs` 是 full-function compiler 与 loop OSR compiler 的共享 compile fact 层，负责变量声明、float slot 判断、branch/ForLoop target 校验和 flow fact 应用；OSR 的 range-exit 与 loop-exit ABI 仍留在 `loop_compiler.rs`。
- `vo-jit/src/test_fixtures.rs` 是共享 JIT 测试 builder。默认从 `code` / `slot_types` 派生 `gc_scan_slots`、`borrowed_scan_slots_prefix`、`has_defer`、`has_calls`、`has_call_extern` 和 `jit_metadata` 长度；需要测试 drift 时，测试必须显式破坏构造后的字段。
- `vo_common_core::bytecode::ReturnFlags` 封装 Return bit schema。`ModuleVerifier` 使用 `from_bits` 拒绝未知位；解释器只接收已经通过验证的当前 schema。
- `VmJitState::{Disabled, BestEffort, Strict}` 是 VM JIT 状态模型。`Vm` 不再用 `Option<JitManager> + bool` 表达组合状态；`jit` 字段仍保持在 VM 结构首位，确保 JIT code memory drop 顺序安全。
- VM full-function JIT 与 OSR 的 non-OK `JitResult` handling 共享 bridge transition 层；`Ok` 分支分别适配 full return copy 与 OSR `loop_exit_pc`。runtime stats 使用 side-exit 命名；测试 manifest 的历史 side-exit 观测 key 只在测试边界映射到 semantic side-exit stats，不记录 compile/metadata/internal ABI failure。

## Metadata 生产边界

`vo-codegen` 生成的 conditional branch 会先把非 `Value` 条件规范化到 `Value` bool slot，避免 JIT 把 `GcRef` / interface header 当整数条件读取。

`vo-codegen` 追加的 fallthrough `Return` 必须使用函数真实 return layout：void 函数直接返回；有返回值的函数分配对应 `ret_slot_types` 的 return buffer，并依赖 frame zero-init 形成类型正确的零值。禁止用 `LoadInt 0` 把所有 typed return slot 粗暴初始化成 `Value` slot。

`fail` / error propagation 的零值返回槽必须通过 `FuncBuilder::emit_zero_slots` 共享同一发射入口。当前 VM/JIT 把 `LoadInt 0` 定义为任意单槽 layout 的 canonical zero bit-pattern；生成端不能在 return lowering 中重新展开裸循环，避免 typed return buffer 与零值 ABI 再次分叉。

Static `Call` bytecode 必须通过 `FuncBuilder::emit_static_call` 发射，callee id 与 packed arg/ret shape 不能在 expression、wrapper、entry 或 method-value 生成路径中各自编码。动态 closure/interface/extern call 继续通过对应 typed builder 写入 `CallLayout` / `CallExternLayout` metadata。

`FuncBuilder` / wrapper 代码是 instruction 与 `jit_metadata` 的 typed emit API 边界。loop hint metadata patch 通过 builder 内部 typed patch 方法完成；method-value embedded-interface wrapper 通过 `FuncBuilder::new_closure`、`emit_ptr_get`、`emit_call_iface` 和 dynamic call buffer helper 构造，不再手工按 pc 写 `jit_metadata`。

`TransferType.rttid_raw` 现在由 packed `ValueRttid` 生成，而不是裸 RTTID。这样 cross-island/defer/closure metadata 的 `meta_raw` 和 `rttid_raw` 都携带合法 kind tag，反序列化和 verifier 可以同一套规则 fail-fast。

逃逸本地数组在执行期的表达式结果是堆数组对象引用，slot layout 必须是单槽 `GcRef`；不能再按静态数组元素展开为 `Value` 临时槽。接口 value-receiver wrapper 的首个 data 参数也必须按实际 interface data layout 生成：boxed struct/array 和引用型 receiver 使用 `GcRef`，float receiver 使用 `Float`，primitive receiver 使用 `Value`。

## 回归覆盖

新增/扩展的 Rust 回归覆盖：

- `vo-common-core::serialize::tests::malformed_*`、`rejects_jit_metadata_length_drift_during_deserialize`、`removed_map_metadata_tags_are_rejected`、`rejects_non_current_bytecode_versions`：非法 slot/kind/runtime/metadata tag、current-version metadata 长度漂移、移除的 metadata tag、非当前 bytecode version 都必须报错。
- `vo-common-core::bytecode::tests::return_flags_reject_unknown_bits`：`ReturnFlags` 必须严格拒绝未知 bit。
- `vo-jit::semantics::tests::control_flow_value_slots_declare_layout_and_fail_fast_policy`：`JumpIf` / `JumpIfNot` / `ForLoop` 同时声明 `LocalSlotLayout` requirement 和 `LayoutMismatch` fail-fast。
- `vo-jit::semantics::tests::semantic_matrix_uses_metadata_contract_as_requirement_source`、`metadata_requirements_match_verifier_gate`：semantics 不得手写 metadata requirement mirror，必须使用 `metadata_contract`。
- `vo-jit::metadata::tests::metadata_facts_read_expected_instruction_layouts` 与 effects metadata tests：JIT effects/loop analysis 通过 `MetadataFacts` 读取当前需要的 layout facts，不重新引入局部 `JitInstructionMetadata` decode table。
- `vo-jit::call_helpers::result_flow::tests::result_flow_is_not_inlined_back_into_call_helpers_root`：non-OK result flow 必须留在 `result_flow` 模块，避免 call lowering root 重新膨胀。
- `vo-jit::test_fixtures::tests::builder_derives_jit_function_fields_from_code_and_slot_layout`：测试 builder 必须自动维护 JIT 派生字段。
- `vo-jit::verifier::tests::verify_module_rejects_invalid_function_through_shared_strict_entry`：公开 strict module verifier 必须扫描整模块，不能只验证单函数编译入口。
- `vo-jit::verifier::tests::verified_module_token_detects_metadata_mutation`：`VerifiedModule` token 必须在 module metadata 变更后失效，并触发 strict verifier 重新拒绝错误 metadata。
- `vo-jit::verifier::tests::rejects_*`：strict JIT capability、Copy layout、closure/island GcRef 操作数、整数 opcode 的 Value layout、IfaceAssign 严格 flags/source layout、Return unknown flags 和 JIT ABI contract。
- `vo-codegen::tests::generated_*`：生成的 `FunctionDef` 派生字段、JIT branch condition slot、packed transfer RTTID、static call shape 和 Return ABI。
- `vo-codegen::tests::escaped_local_array_slice_does_not_copy_gcref_into_value_temp` 与 `named_reference_iface_wrapper_uses_gcref_data_slot`：生成端不得把 `GcRef` 数组/接口 data slot 拷入 `Value` 临时。
- `vo-codegen::tests::fallthrough_return_for_typed_result_preserves_slot_layout`：typed fallthrough return 不得再用 `LoadInt` 污染 `Float` / `GcRef` / interface return layout。
- `vo-codegen::tests::fail_error_return_zero_values_keep_declared_typed_layout` 与 `codegen_error_zeroing_goes_through_func_builder_helper`：`fail` 的零值返回缓冲必须保留 declared typed layout，并通过 builder helper 共享零值发射入口。
- `vo-codegen::func::tests::packed_operand_slot_counts_use_checked_encoders`：codegen 侧的 static `Call` / `CallExtern` / `GoIsland` 等 packed operand 必须走 checked builder helper，不能在各生成路径手写 encoder。
- `vo-vm::vm::jit::tests::call_kind_side_exit_mapping_is_shared_by_full_and_osr_result_handling` 与 OSR panic location tests：full-function 与 OSR non-OK `JitResult` 共享 transition handling，compile/internal errors 不进入 semantic side-exit stats，Panic 仍要求 typed payload 与 location。
- `vo-vm --features jit` callback tests：nil 与错类型 closure/island 在 helper 边界 fail-fast。

端到端语言回归：

- `tests/lang/cases/bugs/2026_06_01_jit_strict_contract_boundaries.vo` 覆盖 pointer nil branch、error/interface branch、map length、interface assignment、dynamic closure call 的 strict JIT 路径。

本轮验证门槛：

- `cargo test -p vo-common-core`
- `cargo test -p vo-jit`
- `cargo test -p vo-codegen`
- `cargo test -p vo-vm --features jit`
- `cargo test -p vo-engine --features jit`
- `cargo fmt --all`
- `git diff --check`
- `./d.py test`
- `./d.py test osr`

## 2026-06-01 strict JIT 收口审计

本轮从当前源码重新审计 strict JIT 入口，不以解释器 fallback 或宽松 metadata 接受掩盖错误。覆盖范围包括 `vo-common-core` bytecode/serialize、`vo-jit` verifier/contract graph、`vo-vm` strict load/init/dispatch、`vo-engine::RunMode::Jit`、`vo-codegen` metadata 生产、JIT/VM return/call/branch/loop/GC frame 边界，以及 runtime helper/callback ABI manifest。

### 本轮新增架构不变量

- 所有 VM `load` / `load_with_extensions` 都必须在接收 module 前通过 `vo-common-core::verifier::ModuleVerifier`；strict JIT `load` / `try_init_jit` 在共享验证通过后再运行 JIT 专属 verifier 并初始化 dispatch table。`RunMode::Jit` 只能使用 strict 构造入口。
- Best-effort API 只控制 JIT 初始化失败时是否退回解释器；它不能跳过 `ModuleVerifier`，也不能接受非法 bytecode/module。
- Versioned bytecode 只接受当前版本；携带 `jit_metadata` table 时必须满足 `jit_metadata.len() == code.len()`。
- 已移除的 map metadata tag 不是兼容 schema；反序列化或 `ModuleVerifier` 必须拒绝，strict `contract_graph` 只描述当前 lowering 合同。
- Semantics、verifier、effects、capability 和 contract graph 对 layout-sensitive control flow 必须同表自证：`JumpIf`、`JumpIfNot`、`ForLoop` 读取 `Value` slot，声明 `LocalSlotLayout`，并在 `LayoutMismatch` 上 fail fast。
- `Return` flags 必须由 `RETURN_FLAGS_ALLOWED` 集中定义；strict verifier 在 return shape、heap return layout 和 lowering 之前先拒绝未知 flag bit。
- 每条 non-heap `Return` 必须精确编码 `func.ret_slots`；heap return 必须精确匹配 `heap_ret_gcref_start` 与 `heap_ret_gcref_count`，且对应 local slots 是 `GcRef`。
- Static `Call` 的 callee、arg slots、ret slots 以 callee `FunctionDef` 为 ABI authority；小形状必须精确镜像到 packed shape field，大于 255 的形状必须把 packed mirror 置零，禁止截断。
- Cross-island/closure transfer metadata 必须保持 `capture_types.len() == capture_slot_types.len()`；存在 `param_types` 时，其总 slots 必须精确匹配 ABI 参数区。普通方法 wrapper 允许省略 receiver slots，method expression wrapper 允许显式 receiver slots；两种形态都必须由 `recv_slots` / closure self slot 推导，不能宽松接受任意 slot 总数。`ValueMeta` 与 `ValueRttid` 的 `ValueKind` tag 必须合法且一致。
- Codegen 必须给每个函数和闭包追加一个合法 fallthrough `Return`。即使最后一条用户路径是 `Return` / `Panic`，条件分支也可能跳到函数尾；branch target 必须落在真实指令上，不能落到 `code.len()`。
- Codegen 生成 `__island_init__` / `__entry__` 调用 `__init__`、用户 init 和 `main` 时，必须以 callee `FunctionDef` 为 ABI authority 分配 discard return buffer，并写入精确 static call shape；入口函数不能用 `ret_slots = 0` 的旧裸调用掩盖返回值。
- Runtime helper ABI 与 JitContext callback ABI 的不可恢复 invariant 继续由 manifest tests 覆盖；metadata、slot layout、call shape、helper/callback 返回 sentinel、GC frame/materialization 等可恢复/输入相关错误必须转成 `JitError` / `VmError::Jit`。

### 本轮修复与反证测试

- Serializer metadata 长度漂移：`rejects_jit_metadata_length_drift_during_deserialize` 修复前会接受 current-version module 的空 metadata table，修复后反序列化返回 `InvalidJitMetadata`。
- Semantics/verifier 漂移：`control_flow_value_slots_declare_layout_and_fail_fast_policy` 与扩展后的 `typed_slot_lowerings_declare_layout_verifier_requirements` 修复前会发现 `JumpIf` / `JumpIfNot` / `ForLoop` 缺少 `LocalSlotLayout` requirement 或 `LayoutMismatch` fail-fast，修复后矩阵自证通过。
- Return unknown flags：`rejects_return_unknown_flags_before_jit_lowering` 和 `strict_jit_load_rejects_return_unknown_flags_before_interpreter_dispatch` 修复前会让未知 Return flag 进入 strict JIT load/lowering，修复后分别在 verifier 与 VM strict load 边界报错。
- Return shape 漂移：`rejects_return_slot_count_drift_before_jit_lowering` 和 `rejects_heap_return_gcref_start_drift_before_jit_lowering` 修复前会放过错误 return ABI，修复后 strict verifier fail fast。
- Static call packed mirror 截断：`rejects_large_static_call_with_nonzero_packed_shape_mirror` 修复前会接受大 call shape 的非零 packed mirror，修复后要求置零。
- Transfer metadata 漂移：`rejects_param_transfer_slot_drift_before_jit_load`、`rejects_capture_transfer_shape_drift_before_closure_gc_layout`、`rejects_transfer_value_kind_drift_before_island_payload_lowering` 修复前会放过 slot/kind 漂移，修复后在 function invariant 阶段拒绝。
- Method expression transfer schema：全量 `vo-test-osr` 暴露 method expression wrapper 的 `param_types` 会显式携带 receiver；`accepts_method_expression_transfer_metadata_with_explicit_receiver` 固定该合法形态，同时保持普通 slot drift 测试继续失败。
- Removed metadata schema 漂移：`removed_map_metadata_tags_are_rejected` 与 current metadata contract graph 测试修复前会放过已移除的 map metadata tag 或把非当前 schema 纳入 contract graph，修复后旧 tag/旧版本在反序列化或 VM 共享验证边界被拒绝。
- Strict VM load/init fail-open：`strict_jit_load_rejects_invalid_metadata_before_interpreter_dispatch`、`strict_try_init_jit_after_load_rejects_invalid_metadata` 修复前会让 invalid metadata module 被 strict VM 接受，修复后在 load/init 边界报 `VmError::Jit`。
- Strict post-load init dispatch table：`strict_try_init_jit_after_load_initializes_loaded_module_tables` 防止 loaded module 后开启 strict JIT 时 dispatch table 为空。
- Best-effort 严格策略隔离：`best_effort_jit_config_skips_strict_jit_only_load_policy`、`best_effort_init_jit_after_load_initializes_without_strict_metadata_validation` 明确 best-effort 只跳过 strict JIT-only policy，不能跳过 VM 共享 `ModuleVerifier`。
- Missing static callee：`strict_jit_load_rejects_missing_static_call_target_before_dispatch` 修复后在 strict load 期间拒绝 out-of-range `Call`，不等到 dispatch/index panic。
- Codegen fallthrough return：`conditional_tail_terminator_keeps_valid_fallthrough_return_target` 与语言用例 `bugs.2026-05-10-large-struct-return-slots` 修复前会出现 branch target == `code.len()` 或 invalid return ABI，修复后 VM/JIT/OSR 都通过。
- Typed fallthrough return：`fallthrough_return_for_typed_result_preserves_slot_layout` 修复前会用 `LoadInt 0` 初始化 typed return slot，修复后按 `ret_slot_types` 分配 return buffer，保持 `Float` / `GcRef` / interface slot layout。
- Entry static call shape：全量 `vo-test-osr` 曾暴露 62 个入口函数调用返回值 `main` 时 `ret_slots` mirror 为 0 的失败；`entry_call_to_returning_main_uses_exact_static_call_shape` 和 `generated_function_defs_satisfy_jit_verifier_invariants` 修复前可复现，修复后入口调用按 callee return layout 分配 discard buffer 并精确编码。

### 验证矩阵

| 层 | 命令 | 本轮结果 |
| --- | --- | --- |
| bytecode/serializer | `cargo test -p vo-common-core` | 通过，30 passed |
| JIT verifier/contract/effects/lowering unit | `cargo test -p vo-jit` | 通过，198 passed |
| codegen metadata/function layout | `cargo test -p vo-codegen` | 通过，14 unit + 56 integration，1 doctest ignored |
| VM strict JIT bridge/callback/materialization | `cargo test -p vo-vm --features jit` | 通过，119 passed |
| engine `RunMode::Jit` strict errors/trap parity | `cargo test -p vo-engine --features jit` | 通过，60 passed |
| formatting/hygiene | `cargo fmt --all` / `git diff --check` | 通过 |
| repo language VM/JIT | `./d.py test` | 通过，2073 passed |
| repo OSR | `./d.py test osr` | 通过，1039 passed |
| language strict contract | `tests/lang/cases/bugs/2026_06_01_jit_strict_contract_boundaries.vo` | 通过，由 full `both` / `osr` 覆盖 |
| context/time JIT timing robustness | `tests/lang/cases/stdlib/context.vo`、`tests/lang/cases/stdlib/time.vo` | 通过，由 full `both` / `osr` 覆盖；短超时过期路径保持独立覆盖 |

## 剩余低风险事项

`ValueKind::from_u8` / `SlotType::from_u8` 仍保留为 runtime 便利 API，但严格输入边界不得直接依赖它们；反序列化、JIT verifier、strict bridge callback 必须使用显式 `try_from` / header-kind 校验。

已移除的 map metadata tag 与非当前 bytecode version 不再进入工具链诊断路径；反序列化或 `ModuleVerifier` 必须直接拒绝。

合法 side exit 仍限于 cold/not-hot、WaitIo/WaitQueue/Yield/Replay、regular/prepared VM call materialization、OSR normal exit、stack-capacity trampoline 和显式 best-effort embedding API。JIT compile/metadata/internal ABI 失败不属于 side-exit stats。

## 最终审计结论

本轮复核后未发现仍需阻断交付的 P0/P1/P2 JIT 问题。当前工程边界是：所有可由 bytecode/module/codegen 输入触发的 metadata、slot layout、call/return/transfer、OSR、GC frame 和 helper/callback ABI 错误必须在 serializer、VM 共享 `ModuleVerifier`、strict JIT verifier、VM load/init 或 JIT compile 边界返回 `SerializeError` / `JitError` / `VmError::Jit`；best-effort 模式只影响 JIT 初始化失败时是否继续解释执行，不允许跳过 VM 通用 bytecode/module 验证。
