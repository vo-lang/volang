# 当前未提交变更合并前硬化与重构开发计划

Status, 2026-06-24: 本文档记录对当前 worktree 未提交变更的
source-backed 架构审查结果，并把审查结论转化为可执行的开发计划。
它不是用户侧语言规范，也不是已完成状态说明。当前源码、`eng/*.toml`、
`cmd/vo-dev`、`tests/lang/manifest.toml` 和实际 CI 结果始终优先于本文档。

本文档的目标是回答一个具体问题：当前变更主要声称是完善、硬化和增强，
为什么 review surface 达到约 11.9 万行，以及在合并前如何把这些变更收敛到
可维护、可验证、可审查的形态。

## 覆盖范围和输入事实

本计划基于 2026-06-24 对当前 worktree 的完整路径覆盖审查：

- tracked 变更文件：279 个
- untracked 文件：35 个，全部是新增测试或测试目录入口
- tracked diff：52,759 行新增，26,166 行删除
- untracked 文件行数：66,450 行
- 当前可见新增/未跟踪 review surface：119,209 行

按子系统分工覆盖的审查面：

- compiler/core/JIT：`vo-analysis`、`vo-syntax`、`vo-codegen`、
  `vo-common-core`、`vo-engine`、`vo-jit`、`vo-source-contract`、`cmd/vo`
- VM：`vo-vm`、runtime boundary、scheduler、JIT callbacks、frame call、
  source contract tests
- runtime/stdlib/module：`vo-runtime`、`vo-stdlib`、`vo-ffi-macro`、
  `vo-module`
- web/app/studio：`vo-web`、`vo-web/runtime-wasm`、`vo-app-runtime`、
  `apps/studio`、`apps/playground-legacy`
- engineering/docs：`.github`、`cmd/vo-dev`、`cmd/vo-test`、`eng/*.toml`、
  `scripts/ci`、`lang/docs`、generated docs、quickplay artifacts、skills
- test architecture：全部新增和修改的 Rust tests、language cases、
  `tests/lang/manifest.toml`

最大 untracked 测试文件：

| File | Lines |
| ---- | ----- |
| `lang/crates/vo-vm/src/vm/jit/callbacks/select/tests.rs` | 9,246 |
| `lang/crates/vo-vm/src/vm/tests.rs` | 5,736 |
| `lang/crates/vo-vm/src/runtime_boundary/tests.rs` | 5,138 |
| `lang/crates/vo-common-core/src/verifier/tests.rs` | 4,214 |
| `lang/crates/vo-vm/src/scheduler/tests.rs` | 3,463 |
| `lang/crates/vo-jit/src/tests.rs` | 3,149 |
| `lang/crates/vo-vm/src/vm/island_shared/tests.rs` | 3,088 |
| `cmd/vo-dev/src/lint_system/tests.rs` | 2,354 |

## 总体判断

当前 10 万行级别的膨胀主要不是新用户功能，而是三类工程增强叠加在一起：

1. 大量生产就绪、JIT/VM、runtime boundary、GC、ABI、workflow 相关测试直接
   挤在生产模块旁边的大型 `tests.rs` 文件里。
2. source-contract、compact scanner、evidence、readiness policy、selector 等
   验证逻辑在 Rust、JS、YAML、TOML 和测试 helper 中重复实现。
3. ABI、effect manifest、runtime layout、JIT helper ABI、host event DTO、
   CI task policy 等事实源还没有收敛到稳定的单一权威层。

因此，正确处理方式不是简单删除测试或回退增强，而是先把测试、事实源和
工程策略分层。合并前要保证可审查和可验证；合并后再做更深的架构收敛。

## 开发原则

1. 先固定 review surface。
   所有参与验证的新增测试文件必须 staged 或明确从变更中移除。不能让核心
   行为依赖 untracked 文件。

2. 行为修复和机械拆分分开提交。
   大文件拆分、helper 搬迁、module re-export 调整应该尽量不改变断言逻辑。
   语义修复单独提交，并带目标测试。

3. 事实源单向流动。
   `eng/*.toml` 是工程 policy 数据层，`cmd/vo-dev` 是解释器；workflow YAML
   和 JS 脚本不应复制 policy。

4. 热路径谨慎，冷路径优先。
   VM/JIT interpreter dispatch、JIT lowering、GC scan 等热路径不做大面积
   抽象化重写。先拆 runtime boundary、测试、manifest、policy、helper。

5. 生成产物只由生成器改。
   generated Playground docs、quickplay artifact、gate evidence 必须通过
   声明的 generator 或 validator 维护，不手写绕过 provenance。

6. 测试 helper 不应掩盖测试盲点。
   source scanner、module builder、bytecode fixture 可以共享，但每个危险边界
   仍保留本地 red-team probe，避免 helper 和被测代码共享同一个错误假设。

## 合并前阻塞项

### B0. 固定 worktree 和测试纳入范围

问题：

- 当前有 35 个 untracked 文件，约 66,450 行。它们多数是新增测试，但不会被
  普通 diff review 或 CI 变更检查可靠覆盖。

计划：

1. 将所有确认需要的 untracked 测试文件纳入版本控制。
2. 对每个 untracked 测试目录确认 `mod tests;` 或 `#[cfg(test)] mod tests;`
   的入口已经生效。
3. 对不应进入当前合并的测试文件，明确删除或移到后续分支，不保留游离状态。

验收：

- `git status --short` 中不再存在与当前变更相关的 `?? .../tests.rs`。
- `cargo test -p <crate>` 能实际编译这些测试入口。

### B1. 修复已发现的 P1 正确性风险

这些问题不属于架构美化，应该在合并前修：

1. `CallIface` / itab verifier ABI 没有绑定 interface method signature。
2. fixed-array pack validation 接受非 inline array encoding。
3. `production-readiness.yml` 的 release/site metadata checkout 假设不稳，
   多 checkout 或缺少 first-party checkout 时可能失败。

计划：

- 为每个问题补最小复现测试。
- 修复行为。
- 避免把这些修复混进大规模测试拆分提交。

验收：

- 对应 Rust unit test 或 manifest case 失败后通过。
- `cargo test -p vo-common-core`、`cargo test -p vo-runtime`、
  `cargo run -q -p vo-dev -- task plan pr --changed` 覆盖相关路径。

### B2. 收敛 source-contract 和 compact scanner helper

问题：

- compact source scanner/helper 在 `vo-source-contract`、`vo-vm`、`vo-jit`、
  `cmd/vo-dev` 测试里重复出现。
- 一些 API 名带版本后缀，例如 `_062`，容易继续扩散。

计划：

1. 在 `lang/crates/vo-source-contract` 提供稳定 test-facing helper：
   - `compact_rust_source_for_contract`
   - `compact_contains`
   - `compact_pattern_position`
   - `compact_pattern_positions`
   - `compact_region_between`
   - delimiter close / line-number 辅助函数
2. 保留需要历史兼容的旧名时，只在内部做 wrapper，不让新测试继续调用版本名。
3. 替换 VM、JIT、vo-dev 测试中的重复 helper。
4. 保留每个子系统自己的危险模式断言，不把所有 case 都变成一个通用 smoke。

验收：

- `rg 'compact_.*_0[0-9]+' lang/crates cmd tests` 不再出现新测试依赖。
- `cargo test -p vo-source-contract`
- `cargo test -p vo-vm --features jit source_contract`
- `cargo test -p vo-jit source_contract`
- `cargo test -p vo-dev lint_system`

### B3. 拆分最大测试文件

问题：

- 单个 `tests.rs` 文件超过 3,000 到 9,000 行，review 噪音高，后续修复难以
  定位，也会让小行为变更看起来像巨型功能变更。

计划：

第一批只做机械拆分：

1. `lang/crates/vo-vm/src/vm/jit/callbacks/select/tests.rs`
   拆为：
   - `tests/mod.rs`
   - `callback_state.rs`
   - `scheduler_boundary_scanner.rs`
   - `rollback_dirty_roots.rs`
   - `abi_width.rs`
   - `source_contract.rs`

2. `lang/crates/vo-common-core/src/verifier/tests.rs`
   拆为：
   - `tests/mod.rs`
   - `externs.rs`
   - `calls.rs`
   - `interface.rs`
   - `collections.rs`
   - `runtime_types.rs`
   - `debug_info.rs`
   - `fuzz.rs`

3. `lang/crates/vo-jit/src/tests.rs`
   拆为：
   - `tests/mod.rs`
   - `call_materialization.rs`
   - `contract_graph.rs`
   - `metadata.rs`
   - `source_contract.rs`
   - `fixtures.rs` 或复用现有 `test_fixtures.rs`

4. 对 `runtime_boundary/tests.rs`、`vm/tests.rs`、`scheduler/tests.rs` 做第二批
   拆分，按 command、endpoint、pending transition、rollback、GC roots、
   island wake 等主题分文件。

不做：

- 不把 `.vo` regression case 抽成 helper。
- 不把 `tests/lang/manifest.toml` 立刻生成化。
- 不把负向 verifier probe 过度抽象成一个 builder，从而掩盖非法字段组合。

验收：

- 文件移动提交中断言文本和测试逻辑不发生语义变化。
- `cargo test -p vo-vm --features jit`
- `cargo test -p vo-common-core verifier`
- `cargo test -p vo-jit`

### B4. Engineering policy 从 JS/YAML 回收到 `vo-dev`

问题：

- `scripts/ci/docs_lint.mjs` 现在承担 docs lint、source contract、VM signoff、
  Studio contract、ABI spec、workflow lint 等职责。
- production readiness selector 同时出现在 YAML、Rust、JS 中。
- evidence 写入和 schema 校验在 Rust/JS 双实现。

计划：

1. 新增或完善 `vo-dev task evidence verify`。
2. 将 VM production gate evidence schema、source hash、required task set 的验证
   放到 `cmd/vo-dev`。
3. `docs_lint.mjs` 只保留 docs/generation consistency 和调用 `vo-dev` 的薄壳。
4. `production-readiness.yml` 消费 `vo-dev task final-selectors` 或等价 JSON 输出，
   不硬编码任务列表。
5. 将 changed-prefix readiness policy 从 Rust 常量回收到 `eng/ci.toml` 或
   明确的 policy table。

验收：

- `.github/workflows/production-readiness.yml` 不再维护独立 selector 真相。
- `scripts/ci/docs_lint.mjs` 不再解析大量 Rust/TS/spec 源码事实。
- `cargo run -q -p vo-dev -- task plan pr --changed`
- `cargo run -q -p vo-dev -- verify plan pr`
- `./d.py ci task docs-lint`

## 架构重构工作流

### W1. VM runtime boundary 与 JIT pending transition

当前问题：

- `vo-vm/src/runtime_boundary.rs` 同时包含类型定义、pending transition、
  preflight、apply、command handling、wake handling、island staging。
- preflight 和 apply 对 command/endpoint 的逻辑存在双实现趋势。
- interpreter queue/select 与 JIT callback queue/select 在行为投影上重复。

开发计划：

1. 先做文件级分层，不改变行为：
   - `runtime_boundary/types.rs`
   - `runtime_boundary/pending.rs`
   - `runtime_boundary/preflight.rs`
   - `runtime_boundary/apply.rs`
   - `runtime_boundary/command.rs`
   - `runtime_boundary/wake.rs`
   - `runtime_boundary/island_staging.rs`
2. 引入 `RuntimeTransitionIntent` 或等价 builder，用来表达 JIT callback 发布的
   pending scheduler-visible work。
3. 中期引入 `RuntimeTransitionPlan`，让 preflight 输出可应用计划，apply 只执行
   已验证计划，减少重复分支。
4. 为 queue/select 建立投影层：
   - `QueueActionProjection`
   - `SelectResultProjection`
   解释器和 JIT callback 都从同一个投影生成 scheduler-visible transition。

验收：

- 文件拆分提交不改变 public behavior。
- `cargo test -p vo-vm runtime_boundary`
- `cargo test -p vo-vm --features jit runtime_boundary`
- `cargo test -p vo-vm --features jit queue`
- `cargo test -p vo-vm --features jit select`
- 相关 `.vo` cases 通过 `./d.py test both`、`./d.py test jit`、`./d.py test osr`。

风险控制：

- 不把已经提交到 queue-local object 的状态误认为 pending transition。
- terminal discard 只能丢弃尚未 commit 的 pending work。
- 任何 suspend/block/yield 新路径都必须同时审 GC roots、dirty epoch、
  pending extern payload 和 wake key validation。

### W2. Frame call、extern replay 与 GC-visible payload

当前问题：

- `vo-vm/src/frame_call.rs` 混合 frame builder、extern replay args、
  GC-visible payload、closure validator。
- replay payload 与 GC root scanning 高耦合，后续修复容易遗漏 slot type。

开发计划：

1. 机械拆分：
   - `frame_call/builder.rs`
   - `frame_call/extern_replay.rs`
   - `frame_call/gc_payload.rs`
   - `frame_call/closure_validate.rs`
2. 为 replay result/payload 引入明确的 typed payload 结构，携带 slot types。
3. 对 extern closure replay、defer/panic、JIT materialization 做一组交叉测试。

验收：

- `cargo test -p vo-vm frame_call`
- `cargo test -p vo-vm --features jit extern`
- `cargo test -p vo-runtime ffi`
- `./d.py test gc`

### W3. ABI、slot width 和 verifier 事实源

当前问题：

- slot/ABI width 诊断和限制分布在 analysis、codegen、common-core verifier、
  bytecode、runtime/JIT helpers。
- `vo-codegen` 从 `vo_vm::bytecode` 读取部分事实，依赖边界不理想。
- `vo-common-core/src/verifier.rs` 过大，混合 module/function/call/collection/
  runtime fact 验证。

开发计划：

1. 短期止血：
   - 将 bytecode ABI 事实回收到 `vo-common-core::bytecode`。
   - 避免 `vo-codegen` 直接依赖 VM 层事实。
   - 引入 `AbiWidth` / `SlotCount` 这类 typed wrapper，减少裸 `u8`、`u16`
     分散比较。
2. 拆分 verifier：
   - `verifier/mod.rs`
   - `verifier/module.rs`
   - `verifier/function.rs`
   - `verifier/calls.rs`
   - `verifier/collections.rs`
   - `verifier/facts.rs`
   - `verifier/debug_info.rs`
3. 中期建立 engine-level verified bytecode API：
   - verified load
   - verified save
   - verified cache format
   - verified text/serialization path

验收：

- `cargo test -p vo-common-core`
- `cargo test -p vo-codegen`
- `cargo run -q -p vo-dev -- test lint --suite lang --strict`
- slot-limit language cases 在 native/JIT/OSR 目标下维持一致诊断。

### W4. Runtime layout、pack、JIT ABI 与 GC facts

当前问题：

- value layout 事实在 `vo-common-core::bytecode`、`vo-runtime::pack`、
  `gc_types`、`jit_api` 中重复计算。
- object physical layout 分散在 array/slice/map/queue_state。
- JIT helper ABI table 大量手写，容易和 runtime layout 漂移。
- GC root freshness 使用布尔/dirty flag 风格，缺少类型化 epoch token。

开发计划：

1. 引入 `TypeLayoutView` 或等价只读视图：
   - value RTTID 到 slot layout
   - inline/heap-backed 区分
   - GC scan shape
2. 为 object physical layout 建 manifest/API：
   - array
   - slice
   - map
   - queue state
   - interface
   - closure
3. 让 JIT helper ABI 从 manifest/table 生成或集中声明。
4. 把 GC root freshness 升级为 typed epoch/dirty token，避免跨 boundary 手写
   flag 协议。

验收：

- `cargo test -p vo-runtime pack`
- `cargo test -p vo-runtime gc`
- `cargo test -p vo-jit`
- `cargo test -p vo-vm --features jit`
- `./d.py test gc`
- `cargo run -q -p vo-dev -- task run gc-contract`

### W5. FFI、stdlib effect manifest 与 wasm host profile

当前问题：

- `vo-stdlib/src/extern_manifest.rs` 手写 effect manifest。
- `vo-ffi-macro` 解析 `effects(...)`，runtime provider 也有 effect 字段。
- `vo-web/runtime-wasm` 和 Studio wasm bridge 手动列 extern module registrars。

开发计划：

1. 短期增加 std feature manifest 测试，确保当前手写表和注册表一致。
2. 中期让 macro/provider 导出或生成 effect manifest entry。
3. 为 wasm/native 建立 explicit host profile：
   - native profile
   - wasm browser profile
   - Studio profile
   - legacy playground profile
4. `vo-stdlib::extern_manifest::EFFECT_MANIFEST` 成为消费产物，不再是孤立手写表。

验收：

- `cargo test -p vo-ffi-macro`
- `cargo test -p vo-runtime ffi`
- `cargo test -p vo-stdlib`
- `./d.py test wasm`
- `cargo check -p vo-web --target wasm32-unknown-unknown`
- `cargo run -q -p vo-dev -- task run stdlib-contract`

### W6. Web/App/Studio host event DTO

当前问题：

- `vo-app-runtime` mailbox、Studio wasm JS object、`vo-web` island、TS interface、
  quickplay generated dependency artifact 各自表达 host event shape。
- legacy playground 仍有独立 GUI/session glue，没有完全复用 `vo-app-runtime`。

开发计划：

1. 定义统一 `HostEvent` DTO schema。
2. Rust 侧：
   - `vo-app-runtime/src/mailbox.rs`
   - `vo-web/src/island.rs`
   - `apps/studio/wasm/src/lib.rs`
3. TS 侧：
   - `apps/studio/src/lib/studio_wasm.ts`
4. 加 field-level contract test，防止 Rust/TS 字段漂移。
5. legacy playground 逐步迁移到 `vo-app-runtime` GUI session。
6. 拆分 `vo-web/src/browser_runtime.rs`：
   - data/model
   - graph split/merge
   - snapshot
   - artifact intent
   - framework planning

验收：

- `cargo check -p vo-web --target wasm32-unknown-unknown`
- `./d.py ci task wasm-check`
- `./d.py ci task studio-wasm-build`
- `./d.py ci task studio-build`
- `./d.py ci task quickplay-validate`
- `./d.py ci task blockkart-smoke-static`

### W7. Module cache graph populate coverage

当前问题：

- `vo-module/src/cache/install.rs` 的 graph populate / parallel cache 行为有新增
  或修改，但现有测试更偏 exact/extract/download。

开发计划：

1. 增加 graph-level populate test：
   - 多依赖
   - already cached
   - partial missing
   - parallel install
   - checksum/provenance mismatch
2. 将 cache fixture 与 temp project lifecycle helper 收敛到 module test support。

验收：

- `cargo test -p vo-module`
- `cargo run -q -p vo-dev -- lint all`
- 如涉及 CLI 生命周期，补 `./d.py vo mod sync/verify/download` focused checks。

## 测试系统重构计划

### 目标状态

测试代码应该形成四层：

1. 单 crate unit tests。
   用于 bytecode verifier、JIT semantics、runtime pack/gc、VM boundary helper。

2. 子系统 contract tests。
   用于 source-contract、stdlib-contract、gc-contract、app-contract、
   readiness/evidence policy。

3. language regression corpus。
   `.vo` 文件保持单文件、显式 manifest metadata，覆盖用户可见行为。

4. generated/artifact validation。
   用于 generated docs、quickplay、production gate evidence。

### 支持库边界

允许抽取：

- `vo-source-contract` test-facing scanner helpers
- `vo-vm/src/test_support/`
- `vo-jit/src/test_fixtures.rs` 继续扩展
- `vo-web/src/test_support/`
- `vo-codegen/tests/support/`，仅当 integration helper 继续膨胀

不建议抽取：

- `.vo` regression case
- `tests/lang/manifest.toml` 的 owner/tags/reason metadata
- 每个 verifier negative probe 的非法结构细节
- 每个 source scanner red-team pattern

### 拆分顺序

1. 先 stage untracked 测试。
2. 机械拆分最大 `tests.rs`。
3. 抽 `vo-source-contract` helper。
4. 抽 `vo-vm`/`vo-web` test support。
5. 再做语义修复和事实源收敛。

### 验收命令

```sh
cargo test -p vo-source-contract
cargo test -p vo-common-core
cargo test -p vo-jit
cargo test -p vo-vm --features jit
cargo test -p vo-runtime
cargo test -p vo-web
cargo run -q -p vo-dev -- test lint --suite lang --strict
./d.py test both
./d.py test jit
./d.py test osr
./d.py test wasm
./d.py test gc
```

## 工程系统和 CI 计划

### 目标状态

- `eng/*.toml` 是数据层。
- `cmd/vo-dev` 是 task/test/tool/artifact/release/evidence 的解释器。
- `d.py` 是兼容 wrapper。
- GitHub workflow 只消费 `vo-dev` 产出的 matrix、selector、metadata。
- JS docs scripts 只负责 docs/generation consistency 和调用 `vo-dev` 的薄壳。

### 具体工作

1. `eng/tasks.toml`：
   - selector 和 `[[group]]` metadata 不再双写。
   - group metadata 只描述 owner/title/tags/selection policy。

2. `eng/ci.toml`：
   - changed-prefix readiness policy 数据化。
   - Rust 常量只作为解析后的 runtime representation。

3. `eng/toolchains.toml`：
   - wasm-pack、Node、Rust cache workspace、bootstrap policy 明确化。
   - workflow provision lint 消费同一份数据。

4. `eng/tests.toml`：
   - runner-owned env key union 由 test plan 输出。
   - `cmd/vo-test` 不再硬编码独立 env 清理列表。

5. gate evidence：
   - `vm-production-gate-evidence/*.json` 纳入 artifact/evidence policy。
   - 增加 validate-only mode，避免 task run 在无意中重写 evidence。

### 验收命令

```sh
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- task plan pr
cargo run -q -p vo-dev -- task plan pr --changed
cargo run -q -p vo-dev -- ci matrix pr --base <sha> --head <sha>
cargo run -q -p vo-dev -- verify plan pr
./d.py help
./d.py ci task docs-lint
```

最终关闭命令：

```sh
cargo run -q -p vo-dev -- task run contract
cargo run -q -p vo-dev -- task run site
cargo run -q -p vo-dev -- task run release-verify
```

## 文档和生成产物计划

### 当前风险

- generated Playground docs mirror 不能作为源文档修改。
- quickplay BlockKart artifact 有 provenance，需要 validator 覆盖。
- production gate evidence 是生成/验证产物，但当前 artifact policy 表达还不充分。

### 计划

1. 将本文档作为当前未提交变更的总计划。
2. 保持专题文档继续承担各自领域：
   - `vm-runtime-boundary-architecture.md`
   - `vm-runtime-boundary-repair-plan.md`
   - `vm-runtime-hardening-plan.md`
   - `vm-module-verifier-hardening-plan.md`
   - `jit-fact-source.md`
   - `extern-effect-jit-routing-design.md`
   - `stdlib-development-plan.md`
3. 修改 generated docs 时通过 `scripts/ci/docs_sync.mjs` 或 declared generator。
4. 修改 quickplay artifact 时运行 quickplay validator。
5. 将 gate evidence 纳入 `eng/artifacts.toml` 或独立 evidence policy。

验收：

```sh
./d.py ci task docs-lint
node scripts/ci/docs_sync.mjs --check
./d.py ci task quickplay-validate
```

## 分阶段实施计划

### Phase 0. Review surface freeze

目标：

- 没有游离测试文件。
- 所有新增测试都可被 cargo 或 vo-dev 编译/发现。
- 当前变更按可审查主题分组。

交付：

- staged tests 或明确移除的 deferred tests
- 更新后的 worktree inventory
- 初始 narrow validation 结果

退出条件：

- `git status --short` 中无意外 `??`
- `cargo test -p vo-source-contract`
- `cargo run -q -p vo-dev -- task plan pr --changed`

### Phase 1. Merge blockers

目标：

- 修复 P1 正确性风险。
- 收敛 source-contract helper。
- 避免 production-readiness workflow 在真实 CI 中漂移失败。

交付：

- `CallIface`/itab signature verifier fix
- fixed-array pack validation fix
- production readiness checkout/metadata fix
- `vo-source-contract` stable test helper API

退出条件：

- 对应 regression tests 通过
- `./d.py ci task docs-lint`
- `cargo test -p vo-common-core`
- `cargo test -p vo-runtime`
- `cargo test -p vo-vm --features jit`

### Phase 2. Test decomposition

目标：

- 最大测试文件拆分到主题模块。
- 共享 helper 不再复制。
- 机械移动和语义改动隔离。

交付：

- `vo-vm` select/runtime boundary/scheduler tests split
- `vo-common-core` verifier tests split
- `vo-jit` root tests split
- `vo-vm` and `vo-web` test support skeletons

退出条件：

- `cargo test -p vo-vm --features jit`
- `cargo test -p vo-common-core`
- `cargo test -p vo-jit`
- `cargo run -q -p vo-dev -- test lint --suite lang --strict`

### Phase 3. Fact-source consolidation

目标：

- ABI、layout、effect、JIT helper ABI 的事实源不再多头维护。

交付：

- `AbiWidth` / `SlotCount` typed wrapper
- bytecode ABI facts moved to `vo-common-core`
- verifier module split
- runtime `TypeLayoutView` proposal or initial implementation
- stdlib effect manifest consistency test

退出条件：

- `cargo check --workspace --all-targets --exclude vo-playground`
- `cargo test -p vo-codegen`
- `cargo test -p vo-common-core`
- `cargo test -p vo-runtime`
- `cargo test -p vo-stdlib`
- `./d.py test both`

### Phase 4. Runtime boundary and web/app contract cleanup

目标：

- VM runtime boundary 分层。
- JIT pending transition 表达明确。
- Host event DTO 建立 schema。

交付：

- `runtime_boundary/` module split
- transition intent/builder
- queue/select projection layer
- `HostEvent` DTO contract test
- wasm host profile split

退出条件：

- `cargo test -p vo-vm --features jit`
- `./d.py test jit`
- `./d.py test osr`
- `./d.py test wasm`
- `./d.py ci task studio-build`
- `./d.py ci task quickplay-validate`

### Phase 5. Engineering policy cleanup

目标：

- policy 数据化。
- workflow/JS 不再复制 Rust policy。
- evidence/artifact 进入 declared validation。

交付：

- `vo-dev task evidence verify`
- `production-readiness.yml` consumes `vo-dev` selector output
- `docs_lint.mjs` slimmed down
- `eng/ci.toml` / `eng/tasks.toml` policy normalization
- evidence artifact policy

退出条件：

- `cargo run -q -p vo-dev -- lint all`
- `cargo run -q -p vo-dev -- task plan pr --changed`
- `cargo run -q -p vo-dev -- verify plan pr`
- `cargo run -q -p vo-dev -- task run contract`
- `cargo run -q -p vo-dev -- task run site`
- `cargo run -q -p vo-dev -- task run release-verify`

### Phase 6. Final integration gate

目标：

- 当前变更可以被审查、测试和长期维护。

最终验收建议：

```sh
cargo fmt --all -- --check
cargo check --workspace --all-targets --exclude vo-playground
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- test lint --suite lang --strict
./d.py test both
./d.py test jit
./d.py test osr
./d.py test nostd
./d.py test wasm
./d.py test gc
cargo run -q -p vo-dev -- task run contract
cargo run -q -p vo-dev -- task run site
cargo run -q -p vo-dev -- task run release-verify
```

## PR/commit 拆分建议

即使在同一个本地开发流里完成，也建议 review 时拆成以下逻辑单元：

1. Inventory and staging。
   只纳入 untracked tests、更新 plan，不做行为变更。

2. Correctness blockers。
   修 P1，补 regression tests。

3. Test decomposition。
   机械拆分测试文件和测试 support。

4. Source-contract helper consolidation。
   收敛 compact scanner helper，替换重复调用点。

5. Engineering policy consolidation。
   `vo-dev` evidence/selector/workflow/docs_lint cleanup。

6. ABI/layout/effect fact-source cleanup。
   bytecode ABI、slot wrapper、stdlib effect manifest、runtime layout tests。

7. VM runtime boundary cleanup。
   module split、transition intent、queue/select projection。

8. Web/app/studio contract cleanup。
   HostEvent DTO、wasm host profile、quickplay/studio validations。

## 风险矩阵

| Risk | Impact | Mitigation |
| ---- | ------ | ---------- |
| 测试拆分时误改断言 | 高 | 机械移动单独提交；先跑拆分前后同一测试集合 |
| 共享 helper 掩盖非法结构 | 高 | 保留每个子系统本地 red-team case |
| workflow selector 漂移 | 高 | YAML 只消费 `vo-dev` 输出 |
| docs_lint 继续承载源码 policy | 中高 | policy 迁回 `cmd/vo-dev`，JS 只做薄壳 |
| VM pending transition 语义被过度统一 | 高 | 明确区分 committed object state 和 pending scheduler-visible work |
| runtime layout/JIT ABI 漂移 | 高 | 建立 manifest/view，JIT helper ABI 由同一事实源消费 |
| generated artifact 被手写修改 | 中高 | 通过 declared generator/provenance validator |
| manifest 生成化导致 owner/tag/reason 丢失 | 中 | 暂不生成 `tests/lang/manifest.toml` |

## Definition of Done

当前未提交变更可以认为达到合并前成熟状态，需要同时满足：

1. 无相关 untracked 测试文件。
2. P1 正确性风险有 regression coverage 并已修复。
3. 最大测试文件已拆分到主题模块。
4. source-contract scanner/helper 不再在 VM/JIT/vo-dev 测试中大面积复制。
5. engineering policy 的权威层回到 `eng/*.toml` 和 `cmd/vo-dev`。
6. workflow YAML 不硬编码 production gate selector。
7. docs_lint 不再直接解析大量源码事实作为 policy enforcement。
8. ABI/layout/effect/HostEvent DTO 至少有一致性测试，后续 manifest 化路径明确。
9. generated docs、quickplay、gate evidence 的产物边界明确。
10. 最终集成 gate 至少运行并记录：
    - `cargo check --workspace --all-targets --exclude vo-playground`
    - `cargo run -q -p vo-dev -- lint all`
    - `cargo run -q -p vo-dev -- test lint --suite lang --strict`
    - `./d.py test both`
    - `./d.py test jit`
    - `./d.py test osr`
    - `./d.py test wasm`
    - `./d.py test gc`
    - `cargo run -q -p vo-dev -- task run contract`
    - `cargo run -q -p vo-dev -- task run site`
    - `cargo run -q -p vo-dev -- task run release-verify`

## 当前不建议做的事

1. 不建议为了降低行数删除 regression tests。
2. 不建议把所有测试 helper 抽成一个超通用 DSL。
3. 不建议把 `tests/lang/manifest.toml` 立即生成化。
4. 不建议在 `d.py`、workflow YAML 或 docs JS 中继续添加 policy。
5. 不建议把 VM/JIT hot dispatch 作为第一批重构目标。
6. 不建议手写修改 generated Playground docs 或 quickplay artifact。
7. 不建议把本文档当成完成证明。完成状态必须以源码、测试和 gate 结果为准。
