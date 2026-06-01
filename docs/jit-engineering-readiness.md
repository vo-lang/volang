# JIT 工程可用性边界

日期：2026-06-01

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
- `IfaceAssign` flags 必须是合法 `ValueKind`；interface source 必须是 `[Interface0, Interface1]`；引用/boxed 类型 source 必须是 `GcRef`；float source 必须是 `Float`；primitive source 必须是 `Value`。
- `LegacyMap*` metadata 只保留为反序列化兼容格式；strict JIT verifier 返回 `UnsupportedLegacyMetadata`，不会在 `RunMode::Jit` 或严格测试路径中被误当成精确 layout。

VM/JIT bridge 对 nil 和错类型 GC 对象分层处理：nil closure/island 走语言 runtime trap；非 nil 但 GC header kind 不匹配的 closure/island 对象走 JIT infra error，避免在 helper 边界猜 header layout。

## Metadata 生产边界

`vo-codegen` 生成的 conditional branch 会先把非 `Value` 条件规范化到 `Value` bool slot，避免 JIT 把 `GcRef` / interface header 当整数条件读取。

`TransferType.rttid_raw` 现在由 packed `ValueRttid` 生成，而不是裸 RTTID。这样 cross-island/defer/closure metadata 的 `meta_raw` 和 `rttid_raw` 都携带合法 kind tag，反序列化和 verifier 可以同一套规则 fail-fast。

逃逸本地数组在执行期的表达式结果是堆数组对象引用，slot layout 必须是单槽 `GcRef`；不能再按静态数组元素展开为 `Value` 临时槽。接口 value-receiver wrapper 的首个 data 参数也必须按实际 interface data layout 生成：boxed struct/array 和引用型 receiver 使用 `GcRef`，float receiver 使用 `Float`，primitive receiver 使用 `Value`。

## 回归覆盖

新增/扩展的 Rust 回归覆盖：

- `vo-common-core::serialize::tests::malformed_*`：非法 slot/kind/runtime/metadata tag 反序列化必须报错。
- `vo-jit::verifier::tests::rejects_*`：函数级 invariant、Copy layout、closure/island GcRef 操作数、整数 opcode 的 Value layout、IfaceAssign 严格 flags/source layout、LegacyMap strict rejection。
- `vo-codegen::tests::generated_*`：生成的 `FunctionDef` 派生字段、JIT branch condition slot、packed transfer RTTID。
- `vo-codegen::tests::escaped_local_array_slice_does_not_copy_gcref_into_value_temp` 与 `named_reference_iface_wrapper_uses_gcref_data_slot`：生成端不得把 `GcRef` 数组/接口 data slot 拷入 `Value` 临时。
- `vo-vm --features jit` callback tests：nil 与错类型 closure/island 在 helper 边界 fail-fast。

端到端语言回归：

- `tests/lang/cases/bugs/2026_06_01_jit_strict_contract_boundaries.vo` 覆盖 pointer nil branch、error/interface branch、map length、interface assignment、dynamic closure call 的 strict JIT 路径。

本轮验证门槛：

- `cargo test -p vo-jit`
- `cargo test -p vo-codegen`
- `cargo test -p vo-vm --features jit`
- `./d.py test jit`

## 剩余低风险事项

`ValueKind::from_u8` / `SlotType::from_u8` 仍保留为 runtime 便利 API，但严格输入边界不得直接依赖它们；反序列化、JIT verifier、strict bridge callback 必须使用显式 `try_from` / header-kind 校验。

`LegacyMap*` 仍可从旧模块反序列化出来，目的是给工具链提供诊断和兼容读取；执行严格 JIT 前会被 verifier 明确拒绝。

合法 fallback 仍限于 cold/not-hot、WaitIo/WaitQueue/Yield/Replay、regular/prepared VM call materialization、OSR normal exit、stack-capacity trampoline 和显式 best-effort embedding API。JIT compile/metadata/internal ABI 失败不属于 fallback。
