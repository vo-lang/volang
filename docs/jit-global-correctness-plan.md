# JIT correctness architecture

日期：2026-05-31

## 目标

这次 hardening 的目标不是继续补单点 bug，而是把 JIT correctness 从人工记忆推进到机器可检查的架构约束。JIT 相关语义现在必须能从一个统一 policy 查到，并且在 opcode、metadata、VM/JIT entry、runtime helper、callback、GC/root/frame、OSR 和 trap 边界发生漂移时让测试失败。

## 权威边界

当前实现把 JIT correctness 拆成以下源文件共同约束：

- `lang/crates/vo-jit/src/semantics.rs`：每个 opcode 的全局语义矩阵。
- `lang/crates/vo-jit/src/capability.rs`：full-function JIT、OSR、fallback policy 的显式能力声明。
- `lang/crates/vo-jit/src/contract.rs`：may_gc、may_alloc、may_panic、may_unwind、may_schedule、observe_frame、needs_frame、slot/type metadata、write barrier、interface/closure effect。
- `lang/crates/vo-jit/src/contract_graph.rs`：opcode、packed operand、metadata、runtime helper、callback、callback callsite、GC entry、JIT entry policy 的可枚举合同图。
- `lang/crates/vo-jit/src/effects.rs`：输入/输出 slot、memory sync、call effect。
- `lang/crates/vo-jit/src/verifier.rs`：metadata、constant/function/extern/global index、branch target、slot range、precise layout、interface pair、write barrier layout 的 JIT 准入门。
- `lang/crates/vo-jit/src/helpers.rs`：从 runtime helper ABI manifest 生成 Cranelift import signature，并注册 runtime symbol。
- `lang/crates/vo-jit/src/call_helpers.rs`：JitContext callback callsite manifest、callback signature 生成、pre-call spill/frame-sync policy、JIT-to-JIT/direct/prepared-call lowering。
- `lang/crates/vo-runtime/src/jit_api.rs`：runtime helper symbol manifest、`JitContext` ABI field manifest、callback/context dependency manifest。
- `lang/crates/vo-vm/src/vm/jit/**` 与 `lang/crates/vo-vm/src/vm/jit_mgr.rs`：VM/JIT entry/exit、OSR、materialized frame、callback implementation、strict fail-fast。
- `lang/crates/vo-common-core/src/instruction.rs`：packed operand 的 checked ABI encoder，尤其是 queue/select recv、map iter、IfaceAssert、u8 slot-count mirror。
- `lang/crates/vo-codegen/src/func.rs` 与相关 lowering：bytecode flags 发射点必须调用 checked encoder 或 typed emit helper，不能裸 cast slot count。

任何新增 opcode、runtime helper、callback、JIT entry route 或 bytecode metadata 形状，都必须同步更新这些矩阵/manifest。`cargo test -p vo-jit` 和 `cargo test -p vo-runtime` 会检查核心一致性。

## Opcode 语义矩阵

`OpcodeSemantics` 每行覆盖一个 `Opcode`，包括：

- packed operand 解释。
- VM 语义来源和 JIT lowering owner。
- JIT metadata 需求。
- verifier 需求。
- 输入/输出 slot 形状、memory sync、may_call。
- runtime helper / JitContext callback / direct JIT entry / VM call request / inline cache 依赖。
- helper 返回值策略。
- frame policy。
- trap policy。
- fail-fast 条件。
- capability 与 effect contract。

矩阵测试会检查：

- 所有合法 opcode 都有 full-JIT 和 OSR policy。
- `Invalid` opcode 只能显式 unsupported。
- capability、contract、metadata requirement、effect analysis 与矩阵一致。
- helper 名称必须存在于 runtime symbol manifest。
- callback/context field 名称必须存在于 runtime callback manifest。
- metadata/helper/callback/GC-frame 敏感 opcode 必须声明对应 fail-fast 条件。

## Runtime callback manifest

`jit_callback_abi_fields()` 是 `JitContext` callback 边界的机器可读 manifest。它列出：

- callback/context field 名称。
- infra-error id。
- 返回值策略。
- ABI 参数与返回类型。
- 是否 may_gc。
- 是否 may_schedule。
- 是否 observe_frame。

`JitContext::validate_required_callbacks()` 现在遍历该 manifest，不再维护独立手写 if 链。缺失 callback 或 `ic_table` 会在 VM 构造 JIT context 的边界 fail fast，而不是等生成代码读空指针。

`call_helpers.rs` 还维护 `jit_context_callback_callsites()`。每条 JitContext callback lowering 必须带 typed callsite descriptor，descriptor 声明 concrete lowering、callback kind 与调用策略（checked JitResult、returning JitResult、raw frame callback）。wrapper 会从 callback ABI manifest 生成 Cranelift signature，并校验返回策略；may_gc/observes_frame 的 raw callback 也统一走 pre-call spill/clear-reg-const policy。

## Runtime helper ABI manifest

`runtime_helper_abi_fields()` 是 runtime helper C ABI 的唯一机器源。它列出 helper symbol、参数、返回值、return policy、panic policy、may_gc、observes_frame。`vo-jit/src/helpers.rs` 不再手写 Cranelift signature；`declare_helpers()` 逐个从 manifest 生成 import signature。`declared_helper_import_signatures_are_manifest_generated` 会把所有 JIT 声明的 helper 用 manifest signature 重新声明一次，任何参数/返回类型漂移都会在 `cargo test -p vo-jit` 失败。

## Fail-fast policy

以下情况必须在正确边界变成 `JitError`、`VmError::Jit` 或 recoverable runtime panic，不允许 panic、默认零值、silent fallback 或 debug-only 检查：

- metadata 缺失或 kind 不匹配。
- slot layout、interface pair、global layout、return layout 不一致。
- constant/function/extern/global id 缺失。
- branch/OSR target 非法。
- helper symbol 或 callback/context field 缺失。
- callback 返回 `JitResult::JitError` 或设置 infra-error sentinel。
- runtime helper 返回 `JIT_HELPER_U64_ERROR` sentinel 时 lowering 必须转成 `JitResult::JitError`，不能继续解释 helper 的普通返回值。
- direct JIT entry、inline cache、prepared call、materialized frame 不变量破坏。
- GC/write-barrier/frame contract 不满足。
- unsupported opcode 或非法 JIT entry。

合法 side exit 仍然保留，但必须被命名：cold/not-hot 解释执行、VM call materialization、WaitIo/WaitQueue/Yield/Replay、OSR normal exit、direct-call stack-capacity trampoline、以及显式 best-effort embedding API。

## GC/root/frame invariants

GC 是非移动、增量、精确 slot 扫描。JIT 相关规则：

- may_gc/may_alloc/may_schedule/may_observe_frame 的 opcode 不允许走 frame-elision direct-call 快路。
- helper call 默认 spill caller frame；只有明确标注 `FrameIndependent` 的 helper 可以跳过 spill。
- JitContext callback call 默认不能裸 `call_indirect`；所有 checked/returning/raw callback 必须通过 callsite descriptor wrapper，由 ABI/effect policy 决定 signature 和 frame-sync。
- interface pair 不允许被 multi-slot copy、metadata layout、return buffer、map/array/slice value range 切开。
- array/slice/map multi-slot 写入必须用 typed metadata write barrier，不能用 raw per-slot conservative fallback。
- `PtrSet` 必须在 source 可能是 GC-backed 值时带 write barrier flag；`PtrSetN` 当前只允许非-GC-backed multi-slot source。
- materialized JIT frame 必须保持 frame/bp/sp/root 扫描不变量。
- defer args、panic payload、recover result、select queues、wait registrations、island endpoints 和 JIT panic messages 都属于 root-sensitive 边界。

## Trap 与 host-trap policy

JIT 不能依赖宿主 trap 表达语言 panic。当前 policy：

- Div/Mod zero、signed MIN/-1、negative/large shift、bounds、nil pointer、nil map write、type assertion、unhashable/uncomparable 等路径通过 runtime trap helper 或 checked helper 转换成 VM 等价 panic。
- `ConvF2I` 使用 saturating lowering，匹配 Rust/VM cast 边界，包括 NaN 和 Inf。
- array/slice bounds 在读取 header 长度前做 nil-aware guard。
- dynamic shift 在发射 shift IR 前选择 safe shift amount。

对应静态 denylist 和差分测试在 `vo-jit`、`vo-engine` 中覆盖。

## OSR policy

OSR 不再只依赖普通 JIT 编译成功：

- `LoopEnd` metadata 必须存在、kind 正确，并且指向真实 backedge。
- encoded loop end 与 metadata loop end 不一致时 fail fast。
- loop begin/end/live-in/live-out 必须在函数 code/local slot 范围内。
- loop analysis 和 compile failure 在 strict JIT/OSR path 中 surfaced 为 JIT error，不作为 silent fallback。
- OSR 与 full-function JIT 共用 opcode capability 和 verifier metadata gate。

## 测试策略

新增测试覆盖以下层：

- `vo-jit`：semantic matrix、capability、effect、metadata verifier、helper/callback dependency、host-trap denylist、write barrier contract、OSR metadata。
- `vo-runtime`：runtime symbol manifest、callback manifest、missing callback infra-error。
- `vo-runtime`：map JIT helper exact layout，禁止 key/value slot-count mismatch 走 min-copy 或默认成功。
- `vo-vm`：JIT context construction、materialized frame、JIT manager out-of-range ids、callback errors、malformed bytecode/module fail-fast、GC root safety。
- `vo-codegen`：global array precise `GcRef` slot metadata，packed operand slot-count checked encoder，stack-array index snapshot width。
- `vo-engine`：VM/JIT output and panic/trap parity for key runtime errors and float-to-int edges.
- language cases：dynamic call/interface result stability, select/island output determinism for manifest-driven JIT/OSR runs.

## 2026-05-30/31 audit delta

本轮重新审计期间新增并修复了以下合同违例，每项都有 fail-before-pass 语言 testcase 或 contract-audit test 进入常规测试路径：

- Verifier 宽放行：`verify_slot_contract` 曾以 `_ => Ok(())` 接受新增 opcode，`vo-jit` 的 `verifier_slot_contract_has_no_wildcard_allowlist` 先失败后通过；修复为显式 opcode 分支，并补上 `PtrNew`、`SliceSlice`、`MapIterNext` 的 slot/layout 检查。
- Map helper exact layout：`vo_map_get` / `vo_map_set` / `vo_map_iter_next` 曾对 runtime map metadata 与 JIT metadata mismatch 走 `.min()`/partial copy，`vo-runtime` 的 `jit_map_helpers_do_not_min_copy_layout_mismatches` 先失败后通过；修复为 exact key/value slot-count authority，mismatch 设置 invalid-metadata infra-error 并返回 `JIT_HELPER_U64_ERROR`。
- Map helper sentinel lowering：`vo-jit` 的语义矩阵新增 `HelperReturnPolicy::U64JitErrorSentinel`，`u64_jit_error_sentinel_helpers_are_checked_by_lowering` 强制 `MapGet`、`MapSet`、`MapDelete`、`MapIterNext` lowering 在继续使用 helper 结果前检查 sentinel。
- Packed operand 截断：`vo-codegen` 的 `packed_operand_slot_counts_use_checked_encoders` 先在 `MapIterNext` 的 `kn|vn<<4` 裸 cast 上失败后通过；修复为 `vo-common-core::instruction` 的 checked encoder，并把 queue recv/send/new、IfaceAssert、GlobalGetN/GlobalSetN、SlotGetN/SlotSetN、CopyN mirror 等边界收敛到 checked emit helper。
- `CallExtern` / `GoIsland` / `GlobalGetN` packed operand 截断：同一个 `packed_operand_slot_counts_use_checked_encoders` meta-test 扩展到 extern call、defer extern wrapper、dynamic call/method、dynamic field/index、panic/conversion、cross-island go、package global load 路径后先失败后通过；修复为 `FuncBuilder::emit_call_extern`、`emit_go_island`、`emit_global_get` 的统一 checked emitter，extern id 与 u8 slot-count 越界在 codegen 边界 fail fast。
- Loop-analysis test fixture 截断盲点：`vo-jit` 的 `loop_analysis_test_fixtures_use_checked_packed_operands` 先在测试 helper 的 `n.min(u8::MAX)` 饱和 flags 上失败后通过；修复为 `copy_n_mirror_flags`，需要构造 malformed bytecode 的测试现在显式手写 malformed flags，避免测试夹断掩盖 analyzer 合同。
- `JitResult` callback 裸调：`SelectBegin` / `SelectSend` / `SelectRecv`、queue/go/defer/recover/iface/island runtime ops，以及 `CallExtern`、closure/interface prepare callback、push-resume callback 曾分散手写或遗漏 `JitResult` 检查；`runtime_ops_jit_result_helpers_use_typed_checked_lowering` 和 `call_helpers_jit_result_callbacks_use_typed_checked_lowering` 先失败后通过。修复后 direct helper、indirect callback、trap-like return callback 分别只能通过 typed helper 发射，裸 `check_call_result` 只允许存在于 helper wrapper 内部。
- Queue/select/interface verifier 合同：`QueueSend`、`QueueRecv`、`SelectSend`、`IfaceAssert`、`IfaceEq`、`Recover` 等曾可能绕过精确 slot/interface-pair gate；`rejects_queue_select_iface_contract_mismatches` 先失败后通过，并把这些 opcode 加入显式 verifier 分支。
- Runtime copy overlap：`vo_copy` 曾使用 `copy_nonoverlapping`，对同一 backing store 的重叠 slice copy 可能产生 UB/错误结果；`vo-runtime` 的 `jit_copy_helper_uses_overlap_safe_memmove_semantics` 先失败后通过，修复为 overlap-safe `core::ptr::copy`，保持 JIT helper 与语言 copy 语义一致。
- GC/layout release fail-open：`gc_types.rs` 曾用 `debug_assert`、missing-`StructMeta` skip、closure all-GcRef legacy fallback、struct barrier conservative fallback 在 release 下继续；`gc_layout_metadata_drift_has_no_release_skip_or_legacy_fallback` 先失败后通过。修复后 array/map/queue object layout、closure capture layout、array/map/queue struct element metadata、struct scanner metadata、typed struct write barrier 都在 metadata drift 时 release fail fast；runtime 测试改为提供精确 `StructMeta` 或显式期待 fail-fast。
- Global array `ValueMeta` drift：全局数组初始化曾把 element RTTID 写入 array header 的 `ValueMeta.meta_id`，而 GC/JIT typed barrier 需要的是 `StructMeta`/`InterfaceMeta` id；`vo-codegen` 的 `global_named_struct_array_elem_meta_uses_struct_meta_id_not_rttid` 先失败后通过。修复后 `compile_global_array_init` 使用 `compute_value_meta_raw` 作为唯一 layout authority，`tests/lang/cases/island_basic.vo` 的 JIT 运行不再在 typed barrier 上 abort。
- Typed metadata write-barrier ABI：`vo_gc_typed_write_barrier_by_meta` 曾是 `extern "C" -> ()`，metadata drift 会在 runtime panic 并跨 C ABI abort，JIT lowering 也无法检查结果；`vo-runtime` 的 `typed_write_barrier_helper_reports_invalid_struct_meta_as_jit_error` 和 `vo-jit` 的 `array_slice_multi_slot_barriers_use_typed_metadata_helper` 先失败后通过。修复后 helper 返回 `JitResult`，invalid metadata 设置 infra-error sentinel，lowering 必须通过 `emit_checked_jit_result_helper_call` 检查结果，`vo_map_set` 也使用 non-panicking typed barrier 内核避免 helper 内部 fail-open 或 abort。
- FFI/dynamic runtime metadata fallback：`ExternCallContext::box_to_interface`、`value_meta_for_value_rttid`、dynamic call result boxing 和 dynamic interface field set 曾在 RTTID 到 `StructMeta`/`InterfaceMeta` 解析失败时 `unwrap_or(0)`，dynamic struct field barrier layout 还会 `unwrap_or_default()` 跳过写屏障；`get_type_slot_count` 对 missing named/struct metadata 还会返回保守 slot 数，`box_to_interface` 会对缺失 raw slot 补零，并把 packed array 当作 `u64` buffer 直接写；`ret_ref` 与 FFI container GC element width 也存在 debug-only 检查，basic RTTID lookup 缺失会退回 0。`vo-runtime` 的 `ffi_runtime_metadata_does_not_fallback_to_meta_zero`、`box_to_interface_raw_slots_are_exact_layout_authority` 和 `vo_array_bounds_are_release_checked` 先失败后通过。修复后这些路径通过 `require_*_meta_id_from_rttid`/exact slot count/release assert fail fast，field/return/array boxing 都要求精确 layout。
- FFI container debug-only bounds：`VoArray<T, N>::get/set` 曾只用 `debug_assert!(idx < N)`，release 下会让越界进入底层 unsafe array access；`vo-runtime` 的 `vo_array_bounds_are_release_checked` 先失败后通过。修复后 FFI array accessor 在 release 下也用 `assert!` fail fast。
- Cross-island queue transfer metadata skip：`prepare_value_queue_handles_for_transfer` 曾在缺失 `StructMeta`、缺失嵌套 struct RTTID、field layout 超出 value slots、invalid pointer、zero-byte sequence 等路径上 `return`/`continue`，可能让嵌套 queue handle 没有安装 `HomeInfo` 就进入 pack/send；`vo-vm` 的 `missing_nested_struct_runtime_type_does_not_guess_meta_zero` 改为 fail-before-pass 的 error-path 测试，`queue_handle_transfer_has_no_metadata_skip_paths` 约束源码不再静默 skip。修复后 queue transfer preparation 返回 `Result<(), String>`，JIT `go island` callback 映射为 infra `JitError`，VM/inter-island endpoint 路径 fail fast。
- JIT-reachable queue send panic barrier：`queue_send_core` 被 `jit_queue_send` 复用，但本地 send 写屏障曾调用 panic 版 `typed_write_barrier_by_meta`，metadata drift 会跨 extern C unwind/abort；`vo-vm` 的 `queue_send_core_reports_typed_barrier_metadata_errors_without_unwinding` 和 `queue_send_core_uses_result_typed_barrier_for_jit_callback_path` 先失败后通过。修复后该路径使用 `try_typed_write_barrier_by_meta`，错误返回 `QueueExecResult::Malformed` 并由 JIT callback helper 转成 `JitResult::JitError`。
- Pack/unpack sequence exact layout：`pack.rs` 对 struct slice/array element layout 在 missing `StructMeta` 时回退到 `elem_bytes.div_ceil(SLOT_BYTES)`，空 struct sequence 会完全跳过 metadata 校验，unpack sequence encoding 也只用 `debug_assert_eq!`；`vo-runtime` 的 `empty_struct_sequences_validate_exact_metadata` 和 `sequence_layout_contracts_do_not_fall_back_or_debug_assert_only` 先失败后通过。修复后 struct sequence slots 只能来自 exact `StructMeta`，空序列也校验 metadata，invalid sequence encoding 在 release 下硬失败。
- Cross-island non-sendable metadata preflight：remote queue send 和 `GoIsland` transfer 曾只为可能含 queue handle 的 metadata 做 preparation，`Interface`/`Closure`/`Island`/`Channel` 等当前 pack ABI 不能安全表达的 transfer metadata 可能直接进入 `pack_slots` panic，或把 nil `chan` metadata 编成 payload；`vo-vm` 的 `remote_queue_send_rejects_non_sendable_metadata_before_pack`、`go_island_transfer_rejects_non_sendable_metadata_before_pack` 和 `vo-runtime` 的 `test_pack_queue_handle_rejects_nil_chan_metadata` 先失败后通过。修复后 transfer preparation 统一用 sendability preflight 返回 `Malformed`/infra `JitError`，runtime pack 自身也拒绝 `Channel` metadata，避免 JIT callback 跨 ABI unwind 或非法 payload drift。
- Anonymous interface `RuntimeType` metadata drift：`type_interner.rs` 曾在找不到 `interface_meta_ids[type_key]` 时 `unwrap_or(0)`，会把匿名非空 interface 的 runtime type 静默指向 meta 0；`vo-codegen` 的 `anonymous_interface_runtime_type_uses_exact_interface_meta_id` 和 `runtime_interface_types_do_not_fallback_to_meta_zero` 先失败后通过。修复后 `InternContext` 带有可写 `interface_metas`/`interface_meta_ids`，type interner 会为匿名非空 interface 创建 exact `InterfaceMeta`，空 interface 仍显式使用 meta 0 作为无方法 fast path。
- Codegen unresolved layout metadata defaulting：`type_interner.rs`、`type_info.rs`、`wrapper.rs` 和 promoted/interface method metadata 路径曾把 unresolved array len、missing signature tuple、missing method signature type 或 missing tuple element type 转成 0 slot、empty signature 或 `filter_map` skip；`FuncBuilder::get_slot_types` 曾对超出已知 layout 的范围裁剪并用 `SlotType::Value` 补齐，slot/layout mismatch 还存在若干 `debug_assert!`/`debug_assert_eq!` release 漏网。`vo-codegen` 的 `codegen_layout_metadata_does_not_default_unresolved_shapes_to_zero` 先失败后通过。修复后这些路径在 codegen 边界 fail fast，runtime type/signature/layout metadata 不再被默认空形状、Value padding 或 debug-only 检查掩盖。
- Endpoint request backing debug-only invariant：`handle_endpoint_request_command` 曾只用 `debug_assert!` 确认 endpoint registry 的 live entry 指向 LOCAL queue，release 下若 registry drift 到 REMOTE proxy 会继续进入 local-state path；`vo-vm` 的 `endpoint_request_backing_invariant_is_not_debug_only` 先失败后通过。修复后该 invariant 用 release `assert!` fail fast，跨 island endpoint 状态错误不会被 release 构建静默放过。
- Dynamic closure replay slot drift：`call_protocol`、`CallObject` protocol replay 和 `dyn.Call` return packing 曾对 closure replay 结果用 `.min()` 截断、缺槽补零或 `unwrap_or(0)`，使错误签名/metadata 状态可能变成看似成功的零值返回；`vo-runtime` 的 `exact_replay_slots_accepts_only_exact_width` 和 `dynamic_replay_contract_has_no_min_copy_or_zero_fill` 先失败后通过。修复后 replay 结果必须精确匹配协议或 closure signature 派生出的槽数，slot-count drift 通过动态错误返回，不进入返回打包。
- Dynamic sequence element layout drift：全量 manifest 复跑时 `tests/lang/cases/dyn/dyn_access_slice_interface_elem.vo` 暴露出 `[]error` 动态索引路径把 container 中的 16-byte interface 元素按 RTTID slot-count 读成 3 槽，`box_to_interface` 的 exact-width assert 因而 fail-before-pass。修复后 slice/array 动态索引统一通过 `sequence_elem_raw_slots`，读取宽度来自 container `elem_bytes`，逻辑宽度来自 `box_to_interface` 合同，physical/logical drift 在 release 下 fail fast；`dynamic_sequence_indexing_uses_exact_container_element_layout` 防止该路径再退回 RTTID slot-count copy width。
- Stack-array index snapshot layout drift：最终 fail-open 扫描发现 `snapshot_lvalue_index` 曾对 stack array LHS index snapshot 使用 `1.min(*elem_slots)`，导致 `[N]struct{}` 这类零槽元素把索引 operand 宽度错误绑定到元素布局。`vo-codegen` 的 `stack_array_index_snapshot_width_is_not_derived_from_element_layout` 作为 contract-audit test 先失败后通过；修复后索引 snapshot 固定复制一槽 index operand，并新增 `tests/lang/cases/bugs/2026_05_31_stack_array_empty_struct_index_snapshot.vo` 覆盖空结构体数组 LHS store 的 bounds panic 边界。
- JitContext indirect callback pre-call spill：`emit_checked_jit_result_indirect_callback_call` 曾只在 callback 返回 non-OK 后通过 `check_call_result` spill，导致 `prepare_*` / `push_resume_point_fn` 这类 may_gc/observes_frame callback 能在 caller SSA-only state 未同步时进入 VM/GC。修复后 checked 和 returning JitResult callback wrapper 在 `call_indirect` 前无条件 `spill_all_vars()`，call 后清理寄存器常量；raw frame callback 也必须通过 `emit_raw_jit_context_callback_call`，由 manifest 的 may_gc/observes_frame 决定 pre-call spill。`call_helpers_jit_result_callbacks_use_typed_checked_lowering` 与 `callback_callsite_edges_are_manifest_backed_frame_sync_boundaries` 把该策略纳入常规 contract gate。
- Runtime helper ABI signature drift：helper import signature 曾在 `vo-jit/src/helpers.rs` 手写重复维护，runtime helper 参数/返回类型漂移可能只在运行期显现。修复后 helper signature 从 `runtime_helper_abi_fields()` 生成，`declared_helper_import_signatures_are_manifest_generated` 会在 Cranelift import 声明边界检查所有 helper ABI。
- Callback ABI signature drift：callback return policy 已有 manifest，但参数/返回类型仍曾分散在 `call_helpers.rs` 的手写 signature helper 中。修复后 `JitCallbackAbiField` 包含 `params`/`ret`，`import_callback_sig` 只从 manifest 生成 signature；`jit_callback_abi_manifest_is_sorted_unique_and_machine_readable` 校验 callback 的首参、返回类型与 return policy 一致。
- Callback lowering graph 只描述不枚举：早期 gate 用源码字符串确认某些 function body 调用了 typed wrapper，容易形成假闭环。修复后 `jit_context_callback_callsites()` 枚举真实 JitContext callback lowering 边界，`contract_graph.rs` 生成 `JitContextCallbackCallsite` edges 并校验 ABI policy、spill policy、concrete lowering consumer；源码字符串检查不再是 callback correctness 的 primary gate。

这些修复没有关闭 JIT、OSR 或 container lowering 能力；对当前 bytecode 格式无法表达的 slot count，策略是 codegen/verifier/runtime fail fast，而不是截断或 fallback。

## 保留能力

本轮 hardening 没有为了规避风险关闭 JIT 功能。以下能力仍保留，并通过 policy 命名其边界：

- full-function JIT 和 loop OSR。
- JIT-to-JIT static direct call。
- dynamic closure/interface inline cache。
- prepared VM-frame call fallback。
- extern call wait/replay。
- defer/recover、select/channel、go/island、map/container/pointer lowering。

## 最终验证

2026-05-31 的 closure 验证执行结果：

- `cargo test -p vo-runtime`：83 unit + 3 gc_perf + 3 doctests ignored，通过。
- `cargo test -p vo-codegen`：14 unit + 41 integration + 1 doctest ignored，通过。
- `cargo test -p vo-vm --features jit`：107 unit + doctests，通过。
- `cargo test -p vo-jit`：161 unit + doctests，通过。
- `./d.py test both -j4`：2065/2065，通过。
- `./d.py test osr -j4`：1035/1035，通过。
- `./d.py test gc -j4`：26/26，通过。
- `cargo test --workspace --all-targets --exclude vo-playground`：通过。
- `git diff --check`：无 whitespace/error 输出。

## Still-open correctness risk

2026-05-31 全局 review 结论：

- P0：无已知 open item。
- P1：无已知 open item。JitContext callback pre-call spill、runtime helper ABI drift、callback ABI drift 已由 executable contract/test 覆盖。
- P2：仍有少量生产路径旁的源码 denylist/source-lint 测试，用于防止历史危险字符串回归；它们不是 correctness 的唯一来源，primary gate 已由 semantic matrix、contract graph、ABI manifest、verifier、typed wrapper 和语言差分测试承担。后续可继续把这些 denylist 收敛成 lowering descriptor 或 IR-level inspection，降低维护成本。

最终 fail-open 扫描仍会命中少量字符串，但已逐项复核，不构成 P0/P1 合同违例：

- `debug_assert` / `unwrap_or(0)` 命中位于 contract-audit 测试中的禁止字符串，测试用来确保生产路径不再出现这些模式。
- `copy_nonoverlapping` 命中位于 pack/unpack、cross-island transfer、FFI byte-buffer copy 等精确 byte serialization 路径；这些 copy 目标是新分配或明确不重叠的 buffer，不是 layout min-copy 或 fallback。
- `.min()` 命中位于 JIT 分析/translator 的范围裁剪、spill bound 计算、诊断 slice 生成，以及 codegen 对超过 u8 ABI 的宽 copy 分块；这些路径不改变语义值宽度，不掩盖 metadata/layout drift。stack-array index snapshot 的历史 `.min(*elem_slots)` 已由 `vo-codegen` contract-audit test 禁止。
- `unwrap_or_default()` 命中位于 endpoint peer-drain 空集合、私有包路径默认值、closure capture 空集合等非 JIT layout authority 边界。
- `vo-codegen/src` 已无生产 `debug_assert!`/`debug_assert_eq!` 命中；`vo-jit/src/verifier.rs` 的 `_ => Ok(())` 只存在于源码扫描测试字符串中。

## 后续强化

全局 review 未发现剩余 P0/P1 合同违例、fail-open 逻辑或 JIT/OSR 能力退化。下面是非阻塞强化项，用于继续降低未来漂移概率：

- 将 `semantics.rs` 的 helper/callback dependency 从字符串升级成强类型 enum 或生成代码，以进一步减少拼写 drift。
- 把 runtime helper callsite 也升级成类似 JitContext callback 的 typed callsite descriptor，进一步减少源码 denylist 测试。
- 把 `call_helpers.rs` 的 route state machine 拆成更小模块，并为每条 route 建独立 table-driven 测试。
- 为 VM/JIT/OSR 差分测试增加 property-style case generation，覆盖更多组合型 bytecode。
- 为 no_std/embed JIT 禁用边界增加专门文档和测试矩阵。
