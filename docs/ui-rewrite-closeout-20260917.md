# UI 重写本地交付收尾

日期：2026-09-17。承接 [Web 完整交付](ui-web-completion-20260916.md)、
[桌面交付](ui-desktop-completion-20260916.md)与[项目诊断](ui-project-tooling-20260916.md)。
本报告记录部署前的本地实现、产物和检查；后续托管 CI、部署与签名分别记录自己的证据。

## 交付范围

新框架使用同一份 Vo 组件、状态、协调与生命周期实现；浏览器和系统 WebView
负责 DOM、原生输入和窄平台服务。Web 使用 Wasm VM；原生继续提供 VM、JIT
与 Native AOT。Core Wasm AOT 已移除。

Web 已提供组件组合、作用域/身份、状态与派生、Effect/Resource、数据缓存、
表单与校验、路由/SSR、局部失败恢复、常用 kit、虚拟列表及可选编辑器/图表/Canvas。
项目创建、只读检查、诊断、开发重载、生产构建、浏览器测试和可搬移工具包均有
公开入口。Studio 已重写为 Gallery、Docs 和 Playground，包含草稿、编译/诊断、
预览和旧数据导出；新应用不包含 Git、账户和远程仓库业务。

桌面消费同一应用和 DOM 宿主，通过独立 SDK 打包。应用运行不依赖源码仓库、
Cargo、Node 或本地服务器。macOS 提供 `.app`；Linux/Windows 提供便携目录。
本轮使用 `release-native` SDK 与发布配置编译器，最终本地交付验收已通过。

## 本轮修改

- 外部 HTTP(S) 导航和新窗口链接交给系统浏览器，应用保留当前文档与状态。
  使用一个有界后台队列，限制 URL 和排队数量，关闭时取消尚未开始的请求。
  嵌入方可替换或关闭处理器，无需增加 UI 线协议或另一套应用状态。
- Windows 启动线程初始化并释放 COM，使用系统 Shell API；macOS/Linux
  分别调用系统打开器。Windows 初始化遵循
  [ShellExecute 文档](https://learn.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-shellexecutew)。
  Linux 需要 `xdg-open` 和默认浏览器。
- 启用系统 WebView 的剪贴板能力，保留 macOS 标准编辑菜单与快捷键。
  物理键盘/剪贴板和辅助技术验收仍按实际平台记录。
- 桌面交付驱动增加开发用窗口回归：普通链接、新窗口链接、内部片段、非 Web
  导航拒绝，以及随后的输入、状态、重排、错误恢复和正常关闭。测试回调只记录
  浏览器交接，不访问外部网站，也不进入生产 SDK 的启动器。
- 补齐迁移指南中的配置、命令、包职责、状态/生命周期、测试与部署切换说明。
  旧命令在 stderr 提示迁移入口，机器输出继续留在 stdout；新命令帮助无需
  Node 或工具包。迁移指南与发布兼容策略一起交付，Studio 文档由正式生成器更新。
- 桌面存储按稳定应用标识隔离。Windows/Linux 使用用户数据目录，macOS 使用
  独立 WebKit 数据存储；搬迁、重打包和重启不会改变存储位置。无应用标识的
  嵌入预览使用临时存储，普通项目模板在创建时生成唯一标识。
- 新 SDK/application manifest 为 v2，混用旧版本会直接诊断。macOS 持久应用
  要求 macOS 14 及以上，包清单和运行时都检查这一前提。旧预览数据保留原样，
  升级前需从旧应用导出重要草稿；新配置不会自动混入未知来源的旧存储。
- 更新当前迁移清单和 CI 说明，区分已经交付的实现、兼容期和仍需实机验证的项目。

## 已完成的检查

| 检查 | 当前结果 |
| --- | --- |
| CLI 单元与集成 | 79 项通过，包含旧命令输出与新命令帮助回归 |
| 原生会话、窗口、分发运行库 | 28 项通过，启用全部特性；无窗口默认特性另有 11 项通过 |
| UI 工具 Node 契约 | 154 项通过 |
| 可搬移工具包，发布配置编译器 | 620 文件完整性检查，60 个公开浏览器用例和 3 个编辑器用例通过；包含完整迁移程序与源码重载 |
| 优化版 SDK，发布配置编译器 | 52 项桌面交付检查通过，覆盖 VM/JIT/Native AOT、外链、存储、可选控件和故障场景 |
| Studio 三原生后端 | VM/JIT/Native AOT 各 12 项，共 36 项通过；桌面汇总 88 项 |
| Web Studio 最终站点 | Chromium/Firefox/WebKit 全部通过；29 页、编辑器、Worker、恢复/导出及迁移文档；站点目录身份与体积预算通过 |
| 最终工具包归档 | 623 个文件逐一验证；实际解压后再次通过公开 `vo ui verify` |
| Windows 交叉编译 | WebView 和桌面运行库的全部特性/目标通过 `x86_64-pc-windows-gnu` 检查 |
| 严格检查与治理 | Native Clippy、格式、工作树完整 lint 和声明验证通过；声明验证针对保留的兼容基线 |

Windows 检查使用锁定依赖和临时 Zig 0.14.1 交叉 C 工具链。最初缺少依赖和
交叉 C 编译器的失败日志保留；最终检查包括 WebView2、JIT、Native AOT 和
Windows 专用外链代码。运行时现有 Windows 条件编译仍产生 6 条未使用代码警告。
这项检查没有执行 MSVC 链接、Windows 窗口或安装程序。

SDK 启动器从调试配置的 65,612,400 字节变为 17,010,688 字节，静态运行库从
311,030,112 字节变为 133,652,440 字节。两者使用不同构建配置；这些数值表示
产物体积，不能充当应用速度提升比例。本轮没有重跑完整 Web 性能基准。

## 发布配置最终验收

全部交付检查使用同一份 `target/release/vo`，SHA-256：
`dc6010d2027134e13862fbb2833851e1f69e4275482469ce0b400564c210fe55`。
SDK 为 `target/ui-next/desktop-sdk-release-v2-20260917`：

| 产物 | SHA-256 |
| --- | --- |
| 原生启动器 | `8fbac3b7834af2254603b1c54ab98b4a48a24a8aeedf1bd67b208b608078a0e0` |
| Native AOT 静态运行库 | `2233ff26f2f104831ee42ddf23035870ebf6aa986cfc1d920732ebbe38c11512` |
| Web＋桌面工具包归档 | `ee56d568a66a0c6a3f1b11900194bce4e4934bf2d6bfa9b015a53b66509923a1` |
| Studio Native AOT 应用归档 | `55175930c2792bdbf69afd7db5f386365b3a07c3b5385e65827bc293e0713ee5` |
| Web Studio 静态归档 | `651d4381ca93302e9ba58d405ffe6c98487b836be487ad02c8836fb250ec9f0b` |

工具包为 `target/ci/artifacts/ui-rewrite-toolchain-macos-arm64-preview.tar.gz`，
74,360,598 字节。它直接来自通过 52 项交付检查的搬迁工具包，随后再次解压验证；
归档没有重新生成另一份未经测试的工具。

Studio 正常应用位于 `target/ui-next/studio-desktop/dist-aot/Application.app`，
归档为 `target/ci/artifacts/ui-studio-desktop-macos-arm64-preview.tar.gz`，
26,777,242 字节。它包含 23 篇离线文档，使用 Native AOT，已重新解压并验证
全部资源与启动器，不包含自动退出的测试脚本。仓库默认 `desktop-sdk` 也已更新为
同一份优化版 v2 SDK；旧 SDK 与旧交付包另行保留。

Web Studio 归档为 `target/ci/artifacts/ui-web-rewrite-studio-static.tar.gz`，
14,384,724 字节，283 个文件；已实际解压并逐文件验证。包含 23 篇文档与
29 个有效页面，三浏览器中的挂载、编辑器、语言服务、Worker 和恢复流程通过。
站点候选位于 `target/ci/artifacts/site`，完整目录 25,295,739 字节，低于
67,000,000 字节预算；文件身份、HTTP 资源类型和交互前后完整性检查通过。
这些包均可直接解压到各自的空目录；站点包需由 HTTP 静态服务器托管。

本轮新增存储检查使用生产启动器：写入 localStorage 和 IndexedDB，关闭应用，
重打包并搬迁后读取相同内容；另一应用标识从空存储启动。应用值包含中文和 emoji。
三后端执行统计同时证明 JIT/Native AOT 进入原生代码，AOT 没有即时编译。

核心复现入口：

```sh
VO_TEST_PROFILE=release node eng/ui-next/toolchain-contracts.mjs
VO_TEST_PROFILE=release VO_UI_DESKTOP_SDK=target/ui-next/desktop-sdk-release-v2-20260917 node eng/ui-next/desktop-package-contracts.mjs
VO_TEST_PROFILE=release VO_UI_DESKTOP_SDK=target/ui-next/desktop-sdk-release-v2-20260917 node eng/ui-next/studio-desktop-contracts.mjs
VO_TEST_PROFILE=release VO_UI_DESKTOP_SDK=target/ui-next/desktop-sdk-release-v2-20260917 node eng/ui-next/desktop-ci-report.mjs
```

Web 报告位于 `target/ui-next/toolchain-project/report.json`；三浏览器 Studio 报告为
`target/ui-next/studio-static-check/report.json`，站点报告为
`target/ci/results/ui-web-site/report.json`；桌面汇总位于
`target/ci/results/ui-desktop-rewrite.json`；各案例日志位于
`target/ui-next/desktop-delivery/run-mu4e60cf`，Studio 窗口日志位于
`target/ui-next/studio-desktop`。本轮检查日志保存在 `target/ui-completion` 中
带 `20260917` 的文件，前一版报告已另存，失败的探索日志也保留。
报告、日志、适配器/工具源码与输入摘要归档为
`target/ci/artifacts/ui-rewrite-closeout-20260917-evidence.tar.gz`。

## 外部验收与兼容边界

- Linux/Windows 真窗口、MSVC 链接、实际 IME/读屏与移动设备绘制，仍需要对应
  环境；本机 DOM 脚本和 Windows 交叉编译不提供这些证据。三平台 CI 驱动已接入。
- 旧稳定源码 API 保留兼容实现。完整迁移说明已交付；正式弃用需随版本发布，
  按[发布策略](../ui/docs/release-policy.md)保留至少一个 minor release，再在
  符合条件的 major release 移除。专业媒体、旧工作区和多窗口使用者没有自动转换。
- 签名、公证、安装器与公开发布尚未执行。当前工作树的本地报告不构成绑定同一
  已提交候选的托管 CI 认证；新 UI 继续标记 preview。

可选的自动静态提取、增量编辑器传输、图表二进制数据与 Canvas 区域更新按实测
收益决定，当前交付使用已经验证的通用实现。
