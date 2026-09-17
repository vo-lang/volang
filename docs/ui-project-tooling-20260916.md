# UI 项目检查与诊断

日期：2026-09-16。范围：新 `ui-next.json` 项目的开发工具和 Web/桌面交付入口。
承接 [Web 交付](ui-web-completion-20260916.md)与[桌面交付](ui-desktop-completion-20260916.md)。

## 修改与行为

- `vo ui doctor` 根据项目清单选择新工具，支持 Web、桌面和 JSON 诊断。它检查
  编译器、工具包清单、项目配置、框架版本、HTML、Web 运行时或桌面 SDK，并一起
  报告相互独立的错误及修复建议。没有新清单的旧项目保留兼容入口。
- `vo ui check` 检查声明的生产、开发、预渲染、服务端和桌面入口，以及格式、
  HTML、静态页面路径和宿主导入。它不运行预渲染、不生成分发目录，也不改写旧产物。
- 编译器增加显式 `vo check --read-only`，检查已有源码与依赖，跳过生成器、下载和
  编译缓存写入。UI 检查使用此模式。普通编译继续使用原有生成和缓存流程。
- 项目配置及 HTML 的加载、结构验证和 Web 构建准备收拢到共享模块；检查、构建、
  诊断与桌面打包消费经过验证的配置。桌面配置和进程执行分别独立，诊断无需加载
  打包器。错误配置不会继续回退到旧 UI。
- 新增公开 CLI 故障回归，覆盖错误配置、缺失宿主导入和桌面入口类型错误。
  使用主动 panic 的预渲染程序以及项目前后逐文件摘要，验证检查没有执行应用
  程序，也没有写入项目。Web 工具包和桌面分发使用同一个回归入口，CI 汇总要求
  这些证据完整。

此前依赖 `check` 顺带生成分发目录的脚本，需要显式执行 `build`。
缺失依赖或生成源码需先准备好。具体命令见[项目诊断指南](../ui/next/diagnosis.md)。

## 验证记录

本机 macOS arm64，使用已有 dev 桌面 SDK；没有重新测量性能。

| 检查 | 结果 |
| --- | --- |
| CLI 单元与参数/链接集成 | 78 项通过 |
| UI 工具 Node 契约 | 154 项通过 |
| 严格 Clippy、格式、文档生成和工作树完整 lint | 通过 |
| 可搬移工具包、三浏览器、源码重载 | 60 个公开浏览器用例、3 个编辑器用例通过；页面、Canvas、图表、滚动及变高列表重载通过 |
| 桌面独立分发 | 42 项通过，包含三后端及可搬移工具包的项目诊断 |
| Studio 三后端 | VM / JIT / Native AOT 各 12 项通过；桌面汇总 78 项通过 |

日志位于 `target/ui-completion/diagnosis-*.log`。首次完整工具包检查发现编译缓存
写入，修复后保留原始失败日志并重跑。文档测试曾在重新生成结束前启动，旧摘要
检查失败；最终生成完成后重新执行，154 项全部通过。
工具包编辑器用例还发现一处依赖 `check` 生成分发目录的旧调用，现已显式调用
`build`；其余调用点和用户文档已逐项核对，失败日志同样保留。

桌面原始记录为 `target/ui-next/desktop-delivery/report.json`，本轮目录为
`target/ui-next/desktop-delivery/run-mu492gen`。带 SDK 的工具包通过 622 文件清单
校验；Web 和桌面诊断、只读检查及故障诊断均已通过。
Web 工具包为 619 文件；与桌面工具包的全部共同文件逐项一致，差异仅为桌面
SDK 和包说明。两组测试使用相同编译器，SHA-256 为
`51e9d2483af1c530f7cf75fa42b24e408c24a522c363e46b2c70a896c0748c03`。
完整工具包回归结果为 `target/ui-next/toolchain-project/report.json`。
Studio 本轮 Native AOT 记录 164,257 次原生函数进入、82,081 次静态续执行，
JIT 编译次数为零。桌面汇总位于 `target/ci/results/ui-desktop-rewrite.json`；
这项记录验证执行路径，未据此推导性能或纯静态覆盖率。

普通 Studio JIT 应用已更新到 `target/ui-next/studio-desktop/dist-jit/Application.app`，
包含 23 篇文档且不含测试脚本。便携归档
`target/ci/artifacts/ui-studio-desktop-macos-arm64-preview.tar.gz` 为 22,736,640 字节，
SHA-256 为 `a2ea8f490f0267953074cfd7d440a80d6e38f4530a69c23fe835ecfff002c070`。
实际归档中的 64 个构建文件已按构建清单逐项核对；旧归档另存于
`target/ui-completion/studio-before-diagnosis.tar.gz`。

本轮结果、公开浏览器报告、窗口日志、工具包清单及相关源码摘要归档为
`target/ci/artifacts/ui-project-tooling-evidence.tar.gz`，119,217 字节，SHA-256 为
`27faa881c937d3eeaea87503e822a5ca15f0c873a1b83183aad18000ea2c4334`。

本轮只覆盖改动涉及的工具链和应用交付。既有完整 Web 框架矩阵及性能结果保持
各自原始记录，不能将它们的源码身份改写成本轮身份。Windows/Linux 托管窗口、
物理输入/辅助技术、稳定 API 迁移、签名及公开发布继续保留各自验收边界。
