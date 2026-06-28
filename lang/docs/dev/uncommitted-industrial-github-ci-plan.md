# 当前未提交变更工业级收口、GitHub 发布与 CI 全绿计划

Status, 2026-06-26: 本文档是
`lang/docs/dev/uncommitted-hardening-refactor-plan.md` 的执行收口计划。
目标不是新增功能，而是把当前巨大 worktree 整理到工业级可审查、可验证、
可上传 GitHub、且 CI 全绿的状态。

本文档不是完成证明。完成状态始终以当前源码、`eng/*.toml`、`cmd/vo-dev`、
`tests/lang/manifest.toml`、本地 gate 输出、GitHub PR 状态和 CI 结果为准。

## 目标

最终目标：

1. 当前未提交变更被整理成清晰、可 review、可 bisect 的提交序列。
2. 本地验证和 GitHub CI 使用同一套 `vo-dev`/`eng/*.toml` 事实源。
3. 不再有游离测试、手写生成产物、重复 policy、陈旧 gate evidence。
4. 所有合并前 blocker 有 regression coverage 和明确验证记录。
5. 分支上传到 GitHub，打开 PR，CI 全部通过。
6. PR 描述能解释为什么这批变更很大、每个 commit 做什么、如何复现验证。

非目标：

- 不新增用户可见功能。
- 不通过删除 regression tests 降低 review surface。
- 不把 `tests/lang/manifest.toml` 立即生成化。
- 不在 `d.py`、workflow YAML 或 docs JS 中继续增加新 policy。
- 不手写 generated Playground docs、quickplay artifacts 或 gate evidence。
- 不把还没验证的文档声明当成完成证明。

## 当前快照

最近盘点时的事实：

- `git diff HEAD --shortstat`: `409 files changed, 133908 insertions(+), 19715 deletions(-)`。
- 无 untracked 文件。
- `contract` 已在 timeout 修正后完整通过。
- 当前本地已通过：
  - `cargo fmt --all -- --check`
  - `git diff --check`
  - `cargo run -q -p vo-dev -- lint all`
  - `cargo run -q -p vo-dev -- test lint --suite lang --strict`
  - `cargo run -q -p vo-dev -- verify plan pr`
  - `cargo check --workspace --all-targets --exclude vo-playground`
- checked-in `vm-production-gate-evidence/*.json` 中的 `site` 和
  `release-verify` 曾经通过，但 `source_state` 不等于当前 source state，
  不能作为当前最终闭环证明。

当前最明显剩余风险：

- `compact_*_062` 版本后缀 helper 仍在 `vo-source-contract`、VM scheduler、
  JIT select source scanner 测试中扩散。
- staged 和 unstaged 状态交错，存在 `AM`、`MM` 文件，不能直接提交当前 index。
- `scripts/ci/docs_lint.mjs` 仍然较大，需要确认是否还承担源码 policy。
- generated docs、quickplay artifact、gate evidence 必须由 declared generator
  或 validator 当前化。
- 本地 `site`、`release-verify`、可选 `vm-production` 需要在最终 source state
  下重跑。

## 工业级要求

### Reviewability

- 每个 commit 只表达一个原因：行为修复、机械移动、helper 收敛、policy 迁移、
  generated artifact 更新、evidence 当前化必须分开。
- 机械拆分 commit 不改变断言逻辑；语义修复 commit 必须带最小 regression test。
- PR 描述必须包含变更地图、验证矩阵、剩余风险和生成产物来源。

### Determinism

- 本地命令、GitHub workflow、CI matrix、tool bootstrap 都由 `eng/*.toml` 和
  `cmd/vo-dev` 驱动。
- workflow YAML 只消费 `vo-dev` 产出的 matrix/metadata，不维护独立 selector。
- docs JS 脚本只做 docs/generation consistency 或调用 `vo-dev`，不新增源码 policy。

### Evidence

- `contract`、`site`、`release-verify`、必要时 `vm-production` 的 evidence 必须在
  同一最终 source state 下生成。
- evidence JSON 必须能被 `vo-dev` 或对应 validator 重新验证。
- PR 描述中记录本地 gate 的命令和最终状态，不复制冗长日志。

### CI Parity

- PR 前本地至少跑完整合并前 gate。
- PR 后以 GitHub Actions 为准，任何失败必须回到 source/config 修复，而不是绕过 CI。
- CI rerun 只能用于确认 flaky 或外部瞬时失败；产品或配置失败必须改代码。

## 阶段计划

### Phase 0. 冻结与盘点

目标：确认当前 worktree 没有游离输入，建立可执行的拆分地图。

步骤：

1. 记录当前 inventory：
   - `git status --short`
   - `git diff HEAD --shortstat`
   - `git diff --cached --shortstat`
   - `git diff --shortstat`
   - `git ls-files --others --exclude-standard`
2. 确认无 untracked 测试或生成产物。
3. 生成 changed task plan：
   - `cargo run -q -p vo-dev -- task plan pr --changed`
   - `cargo run -q -p vo-dev -- verify plan pr`
4. 输出 commit map 草案，按路径归类到后续提交。

退出条件：

- 无 `??`。
- 每个 changed path 都能归到一个计划 commit。
- `verify plan pr` 通过。

### Phase 1. Index 与提交边界整理

目标：把当前 staged/unstaged 混合状态整理成可审查提交序列。

步骤：

1. 不 reset、不 checkout、不丢弃用户改动。
2. 只用路径级或 hunk 级 staging 整理提交。
3. 每个 commit 前运行该 commit 对应的最小验证。
4. 每个 commit message 写清楚：
   - change type
   - affected subsystem
   - validation
   - whether semantic behavior changed

建议提交顺序：

1. `docs: record industrial GitHub CI closure plan`
2. `test: include hardening regression inventory`
3. `fix: close pre-merge correctness blockers`
4. `test: split oversized VM/JIT/common-core tests`
5. `test: consolidate source-contract helpers`
6. `eng: centralize task and CI policy in vo-dev`
7. `runtime/jit: harden ABI layout and GC-visible contracts`
8. `web/studio: harden host event and wasm contracts`
9. `docs/artifacts: refresh generated docs quickplay and gate evidence`

退出条件：

- `git status --short` 不再有难以解释的 `AM`/`MM` 混合状态。
- 每个提交可以独立描述和 review。
- 机械提交与语义提交分离。

### Phase 2. 合并前 blocker 收尾

目标：关闭当前计划中的合并前 blocker。

必须完成：

1. `CallIface` / itab verifier ABI 绑定 interface method signature，并保留
   regression test。
2. fixed-array pack validation 拒绝非 inline array encoding，并保留 regression test。
3. `production-readiness.yml` 只消费 `vo-dev` final matrix/metadata。
4. `compact_*_062` 不再作为新测试依赖扩散。

命令：

```sh
rg 'compact_.*_0[0-9]+' lang/crates cmd tests
cargo test -p vo-source-contract
cargo test -p vo-common-core
cargo test -p vo-runtime
cargo test -p vo-vm --features jit
cargo test -p vo-jit
cargo test -p vo-dev lint_system
cargo run -q -p vo-dev -- task plan pr --changed
cargo run -q -p vo-dev -- verify plan pr
```

退出条件：

- blocker 对应 tests 全部通过。
- version-suffixed compact helper 只允许存在于内部兼容 wrapper 或历史命名测试中；
  新调用点必须使用稳定 helper API。
- workflow selector 不在 YAML 中硬编码。

### Phase 3. 工程 policy 收口

目标：把 CI/task/evidence/docs policy 收回 `eng/*.toml` 和 `cmd/vo-dev`。

步骤：

1. 审计 `scripts/ci/docs_lint.mjs`：
   - 保留 docs sync/generation consistency。
   - 移除或薄壳化 source-contract、VM signoff、Studio contract、ABI policy。
2. 审计 `.github/workflows/*.yml`：
   - 只调用 `vo-dev` matrix、metadata、tool bootstrap、task run。
   - 不维护独立 task selector 或 checkout policy。
3. 审计 `eng/ci.toml`、`eng/tasks.toml`、`eng/toolchains.toml`：
   - changed-prefix policy 数据化。
   - tool/cache/bootstrap policy 数据化。
   - top-level group metadata 完整。
4. 增加或完善 evidence validate-only 路径，避免 task run 无意重写 evidence。

命令：

```sh
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- task stats
cargo run -q -p vo-dev -- task coverage
cargo run -q -p vo-dev -- task plan pr
cargo run -q -p vo-dev -- task plan pr --changed
cargo run -q -p vo-dev -- verify plan pr
./d.py help
./d.py ci task docs-lint
```

退出条件：

- `vo-dev lint all` 通过。
- task metadata coverage 无缺失。
- workflow 和 JS 不再是 policy authority。

### Phase 4. 生成产物与 evidence 当前化

目标：让 generated docs、quickplay artifact、gate evidence 与最终源码一致。

步骤：

1. 通过 declared generator/validator 更新 generated Playground docs。
2. 通过 quickplay validator 更新 BlockKart checked-in artifact。
3. 在最终 source state 下重新生成 gate evidence。
4. 确认 evidence 不包含过期 `source_state`。

命令：

```sh
node scripts/ci/docs_sync.mjs --check
./d.py ci task docs-lint
./d.py ci task quickplay-validate
./d.py ci task blockkart-smoke-static
cargo run -q -p vo-dev -- task run contract
cargo run -q -p vo-dev -- task run site
cargo run -q -p vo-dev -- task run release-verify
```

可选但推荐：

```sh
cargo run -q -p vo-dev -- task run vm-production
```

退出条件：

- generated docs 和 source docs 一致。
- quickplay provenance validator 通过。
- `contract`、`site`、`release-verify` evidence 共享最终 source state。

### Phase 5. 本地最终 gate

目标：在上传 GitHub 前完成本地工业级验收。

必须跑：

```sh
cargo fmt --all -- --check
git diff --check
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

推荐补充：

```sh
cargo run -q -p vo-dev -- task run vm-production
cargo run -q -p vo-dev -- verify run quality
```

退出条件：

- 所有命令通过。
- 失败项有明确修复 commit。
- 最终验证结果记录到 PR 描述草案。

### Phase 6. 上传 GitHub 与打开 PR

目标：把整理后的提交序列上传到 GitHub，并让 CI 接管最终证明。

步骤：

1. 确认当前分支。如果需要新分支，使用 `codex/` 前缀，例如：
   - `codex/industrial-hardening-ci-green`
2. 确认 commit 序列：
   - `git log --oneline --decorate --max-count=20`
   - `git show --stat --summary HEAD`
3. 确认无意外文件：
   - `git status --short`
   - `git diff --stat main...HEAD`
4. push 分支到 GitHub。
5. 创建 draft PR。
6. PR 描述必须包含：
   - summary
   - commit map
   - local validation
   - generated artifact provenance
   - evidence source state
   - known residual risks
   - reviewer guide by subsystem

PR 描述模板：

```md
## Summary

- Harden VM/JIT/runtime/module/web engineering contracts without adding user-facing features.
- Split large hardening tests and centralize CI/task policy through vo-dev.
- Refresh generated docs, quickplay artifacts, and gate evidence from declared validators.

## Commit Map

- Inventory and staging:
- Correctness blockers:
- Test decomposition:
- Source-contract helper consolidation:
- Engineering policy:
- Runtime/JIT/ABI hardening:
- Web/Studio/artifacts:
- Evidence:

## Local Validation

- [ ] cargo fmt --all -- --check
- [ ] git diff --check
- [ ] cargo check --workspace --all-targets --exclude vo-playground
- [ ] cargo run -q -p vo-dev -- lint all
- [ ] cargo run -q -p vo-dev -- test lint --suite lang --strict
- [ ] ./d.py test both
- [ ] ./d.py test jit
- [ ] ./d.py test osr
- [ ] ./d.py test nostd
- [ ] ./d.py test wasm
- [ ] ./d.py test gc
- [ ] cargo run -q -p vo-dev -- task run contract
- [ ] cargo run -q -p vo-dev -- task run site
- [ ] cargo run -q -p vo-dev -- task run release-verify

## Generated Artifacts

- Generated docs:
- Quickplay artifacts:
- Gate evidence:

## CI Notes

- Workflow selectors are emitted by vo-dev.
- CI failures should be fixed in source/config, not bypassed in YAML.
```

退出条件：

- PR exists on GitHub.
- CI starts with expected matrix.
- No required check is skipped unexpectedly.

### Phase 7. CI 全绿处置

目标：让 GitHub CI 全部通过，并保留可审计的失败处理记录。

步骤：

1. 读取失败 check 的 task name 和 log。
2. 用同一 `vo-dev` task 在本地复现。
3. 判断失败类型：
   - source failure: 修代码或测试。
   - policy/config failure: 修 `eng/*.toml` 或 `cmd/vo-dev`。
   - generated artifact drift: 运行 declared generator/validator。
   - external flaky: rerun job，并在 PR comment 记录原因。
4. 每个 CI 修复单独 commit，避免和 unrelated cleanup 混合。
5. push 后等待 CI 全部重新通过。

退出条件：

- GitHub PR required checks 全绿。
- 没有 pending required check。
- 没有 skipped required gate。
- PR 描述中的 validation checklist 与最新结果一致。

## Reviewer Guide

建议 reviewer 按以下顺序看：

1. correctness blocker commit。
2. test decomposition commit，只检查机械移动和入口是否正确。
3. source-contract helper commit，确认 helper 没掩盖 red-team probes。
4. engineering policy commit，确认 policy authority 在 `eng/*.toml` 和 `cmd/vo-dev`。
5. runtime/JIT/ABI commit，重点看 GC roots、slot layout、JIT callback ABI。
6. web/studio commit，重点看 host event DTO、WASM provider、quickplay provenance。
7. generated/evidence commit，只按 declared generator/validator 审。

## Release Readiness Definition of Done

这批变更达到工业级可合并状态，需要同时满足：

1. 无 untracked 文件。
2. 提交序列按主题拆分，机械移动和行为修复分离。
3. P1 blocker 有 regression coverage。
4. 最大测试文件已按主题拆分，剩余大测试文件有明确归属。
5. source-contract helper API 稳定，版本后缀 helper 不再扩散。
6. workflow YAML 不硬编码 gate selector。
7. docs JS 不承担源码 policy authority。
8. `eng/*.toml` 和 `cmd/vo-dev` 是 task/test/tool/evidence policy 权威层。
9. generated docs、quickplay artifacts、gate evidence 由 declared generator 或 validator 维护。
10. 本地最终 gate 全部通过。
11. GitHub PR CI required checks 全部通过。
12. PR 描述包含 commit map、validation、artifact provenance 和 residual risks。

## 风险与缓解

| Risk | Impact | Mitigation |
| ---- | ------ | ---------- |
| 提交边界混乱导致 review 不可控 | 高 | 先 commit map，再按主题 stage；不直接提交当前混合 index |
| CI 与本地 gate 不一致 | 高 | GitHub workflow 只消费 `vo-dev` matrix/metadata |
| generated artifact 被手写污染 | 高 | 只通过 declared generator/validator 更新 |
| source-contract helper 掩盖危险模式 | 高 | 保留子系统本地 red-team probes |
| evidence source state 过期 | 高 | 最终 source state 下重跑 `contract`、`site`、`release-verify` |
| flaky 被误判为产品失败 | 中 | 先本地复现；确认 external flaky 后再 rerun |
| 巨大 PR 难以 review | 高 | commit map、reviewer guide、按 subsystem 拆说明 |

## 下一步执行顺序

1. 当前化 `site` 和 `release-verify`，确认最终 gate 差距。
2. 收敛 `compact_*_062` helper。
3. 审计并瘦身 `docs_lint.mjs` 的 policy 职责。
4. 整理 staged/unstaged 混合状态，形成 commit map。
5. 按 commit map 逐个提交并跑最小验证。
6. 跑本地最终 gate。
7. push 到 GitHub，创建 draft PR。
8. 跟 CI，修到全绿。
