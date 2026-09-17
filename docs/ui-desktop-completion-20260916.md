# UI 原生桌面交付与 Studio 联调

日期：2026-09-16。环境：macOS arm64。此报告覆盖新 `ui/next` 路径的本地桌面
交付，承接 [Web 完整交付报告](ui-web-completion-20260916.md)。当前使用 dev SDK；
这些结果验证功能与生命周期，不用于声明发布包体积或性能收益。

同日项目工具收尾后重新通过全部 78 项桌面检查；新增只读检查及 Web/桌面
诊断证据，工具包更新为 622 文件。具体结果和编译器范围见
[项目工具报告](ui-project-tooling-20260916.md)。下文保留此前统一链接阶段的记录。

## 已实现

- `vo ui run/package` 识别普通 `ui-next.json` 项目，分别默认 JIT / Native AOT，
  支持显式 VM/JIT/Native AOT。复用应用 Vo 源码、HTML/CSS、初始化数据、服务与
  可选控件。多页面或服务端项目需要显式桌面入口。
- 新 `vo-ui-desktop-runtime` 负责分发组合，独立预构建 SDK 负责原生 launcher 与
  静态运行库。用户项目无需构建 Rust；生成的应用无需 Node、编译器、仓库或本地
  HTTP 服务。macOS 生成 `.app`，其他平台已有便携目录装配代码。
- 资源清单检查路径、大小、摘要及后端；字节码先通过通用验证，再进行目标验证。
  构建失败保留旧产物，整包支持搬移和中文路径。原生 AOT SDK 排除旧 UI 内核、
  Vo codegen 和 Cranelift 编译器依赖。
- Studio 使用同一项目打包器，主界面原生执行，23 篇文档离线交付。Playground
  的编译器与示例使用独立、可取消的 Wasm VM Worker；编辑器、草稿和 UI 预览
  复用 Web 实现。测试构建与普通应用输出分开。
- 桌面导航按本地资源地址适配，共用路由、查询参数、历史记录和滚动恢复。修复
  路由链接触发整页加载后挂起原生会话的问题；整页离开会释放会话，macOS 渲染
  进程终止也会退出窗口。Studio 的桌面错误页提示重新打开应用。
- 宿主入口保留 ES Module 语义，修复 `import.meta.url` 及异步入口的装配问题。
  支持 Wasm 资源 MIME、离线 Worker 和独立 iframe UI 预览。
- 三个平台统一通过编译器进行 Native AOT 链接。SDK 记录 Rust 给出的系统库参数，
  Windows 生成 `.lib` 并复用 MSVC 工具发现；GUI 入口保留 `main` 调用约定。
  工具包和 SDK 共用目录发布逻辑，处理 Windows 无法替换已有空目录的行为。

## 验证

| 检查 | 本地结果 |
| --- | --- |
| UI 工具及基础契约 | 150 / 150 通过 |
| 原生 CLI 及链接回归 | 77 / 77 通过，覆盖参数保真、失败时保留原产物及 MSVC GUI 入口参数 |
| CI 计划、执行及证据逻辑 | 41 / 41 通过；工作流语法与嵌入脚本检查通过 |
| 原生会话、WebView、分发运行库 | 25 / 25 通过，启用全部特性 |
| 桌面 JS 关闭、错误、取消、序号及页面卸载 | Chromium / Firefox / WebKit 合计 15 项通过 |
| Web 导航回归 | 三引擎通过历史、查询、片段滚动、根隔离与订阅释放检查 |
| 可搬移工具包及独立应用 | 42 / 42 通过：VM/JIT/Native AOT、公开 CLI、可选控件、失败构建保留、资源损坏与异常退出 |
| Studio 独立桌面应用 | VM / JIT / Native AOT 各通过 12 项交互与生命周期检查 |
| 严格 TypeScript、原生 Clippy、格式及仓库 lint | 通过 |

桌面 JS 生命周期检查已接入 Web 核心 CI。三项 `ui-desktop-rewrite-*` 任务已声明
Linux/macOS/Windows 的原生测试、SDK 构建、完整分发与 Studio 窗口验收，进入 PR
影响选择以及 merge/main 配置。CI 汇总拒绝缺失后端、重复场景、平台混用和产物
摘要不一致；本地尚未运行托管平台任务，当前没有 Windows/Linux 的成功运行证据。

Studio Native AOT 本次记录 166,464 次原生函数进入、83,154 次静态续执行，
JIT 编译次数为零。该记录确认执行路径；原生续执行与通用运行库仍保留既定回退，
不代表每条语言操作均为静态机器码。

统一链接后的最终汇总为 78 / 78（42 项独立分发加 36 项 Studio），记录在
`target/ci/results/ui-desktop-rewrite.json`。全新构建的 SDK 与窗口验收使用的 SDK
在 launcher、静态运行库和链接参数上完全一致。独立工具包通过 617 个文件的
公开完整性校验。当前 SDK 仍使用 dev 配置，托管任务将使用 release-native。

原始结果写入 `target/ui-completion/desktop-*.log`、
`target/ui-completion/navigation-browser-final.log`、
`target/ui-next/desktop-delivery/report.json` 和
`target/ui-next/studio-desktop/report.json`。最后两项由各自完整交付驱动更新，
需要以 `passed`、平台、后端及实际产物摘要共同判断。
本轮增量日志为 `target/ui-completion/cross-platform-*.log`；最终 CI 计划快照为
`target/ci/ui-desktop-pr-plan.json`、`target/ci/ui-desktop-main-plan.json`。

Studio 验收产物保存在 `target/ui-next/studio-desktop/check-{vm,jit,aot}`。
普通应用构建位于 `target/ui-next/studio-desktop/dist-jit/Application.app`，
使用原生 JIT，包含离线文档及可选 Playground 运行资源。
便携归档为 `target/ci/artifacts/ui-studio-desktop-macos-arm64-preview.tar.gz`；
本地检查日志与清单另存为 `target/ci/artifacts/ui-desktop-preview-evidence.tar.gz`。

复现入口见 [桌面指南](../ui/next/desktop.md)。桌面交付测试覆盖实际系统 WebView
中的 DOM 交互；测试进程有独立超时，原生执行结果按 VM/JIT/Native AOT 分别检查。
本轮没有重新跑完整 Web CI，也没有测量桌面与 JavaScript 的性能差距。

## 仍需完成的范围

1. Windows/Linux 的实际窗口及独立包验收；Windows Native AOT SDK 和链接路径已实现，
   需要托管 Windows 任务验证实际 MSVC/WebView2 行为。
2. 真实键鼠/触摸输入、IME、屏幕阅读器和最终绘制验收。本机脚本测试不能代替这些证据。
3. 稳定公共 API 的版本迁移、剩余旧消费者和旧内核退场；新路径仍为 preview。
4. 安装器、签名、公证和公开发布，以及绑定同一提交的托管 CI 证据。

旧产品认证不自动适用于新实现；当前本地交付不代表整个 P7 或产品认证完成。
