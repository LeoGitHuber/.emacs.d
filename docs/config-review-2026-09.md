# Emacs 配置检查与改进

日期：2026-09-13。目标：兼顾 Verilog／编程、Org／LaTeX／阅读，保留键位和界面，兼容当前安装的 GNU Emacs 32.0.50 与 NeoEmacs 31.1。

## 已落实的改进

| 原来的问题 | 本次改进 | 对使用的影响 |
|---|---|---|
| `init.el` 同时包含数百行诊断渲染与 LaTeX 实现 | 拆出 `init-diagnostics.el` 与 `init-tex.el` | 定位与维护更方便，定制外观保留 |
| `config.el` 是另一套过期入口，引用缺失模块 | 归档旧入口及 16 个未加载模块 | 当前入口唯一，旧内容可查阅 |
| 启动期清空文件处理器，手动调整 GC 阈值 | 保留文件处理器，删除启动期 GC 调优、恢复 hook 与 Org-roam GC 阈值覆盖 | 文件协议处理保持正常，GC 使用运行时与包的默认策略 |
| 相同 GC helper 定义两遍且未启用 | 移到归档 helper 文件 | 不再存在误导性的另一套 GC 配置 |
| Org-roam 在每次启动时同步笔记库 | 首次加载 Org-roam 时再启用 autosync | `C-c n f` 保持；首次使用承担索引开销 |
| Magit、Dape、Org-noter、Nov、Calibre、字典在启动时加载 | 使用命令自动加载；字典在首次 `C-c d` 时初始化 | 保留原入口，减少启动依赖 |
| 注释说 LaTeX 延迟加载，但立即加载了预览、折叠和工具栏 | AUCTeX 先注册模式，首次使用 LaTeX 时加载具体配置；移除主动加载工具栏 | 保留 LaTeXMk、CDLaTeX、RefTeX、折叠和预览 |
| AUCTeX 源码检出缺少生成的 autoload 文件 | 显式注册主要 TeX 模式入口 | 新会话也能自动进入 AUCTeX |
| 行高亮通过多个 hook 修改全局模式的局部变量 | 使用 Emacs 31 的 `global-hl-line-buffers` | 阅读与终端模式的排除逻辑集中管理 |
| Markdown 被通用尾随空白清理覆盖 | 排除 Markdown，其他文本继续使用内置清理模式 | 保留行末双空格表示的强制换行 |
| Eglot 与 Apheleia 可能同时格式化同一缓冲区 | Apheleia 启用时 Eglot 保存 hook 让出格式化职责；退出 Eglot 时移除 hook | Verilog 外部格式化仍由 Verible 执行 |
| Eglot 格式化会因光标位于空行而跳过 | 去除与光标位置无关的限制 | 保存行为一致 |
| Corfu 与 Eglot 的发送延迟都为 0 | 改为 0.1 秒／0.2 秒 | 合并快速输入期间的连续请求 |
| 多种补全风格混用，Orderless 常被前面的 flex 截获 | 通用候选用 Orderless，Eglot 候选用内置 flex，文件用 partial-completion | 保留 Corfu／Vertico，按候选类型匹配 |
| 文件扩展名直接指向 TS 模式，绕过语法可用性判断；TSX 指向 TypeScript 模式 | YAML、Lua、TS、TSX 交给 treesit-auto；Scala 增加语法探测与经典模式回退 | 缺少语法时可回退或按原策略提示安装 |
| 定制 Flymake 内部 fancy 渲染，但实际使用普通行尾显示 | 归档未使用的渲染与相关主题代码，改用内置显示；兼容新版 `flymake-inline-diagnostics` | 保留当前行尾外观、边栏图标和主题配色，减少内部接口依赖 |
| 自动加载更新命令引用不存在的旧目录 | `update-all-autoloads` 调用当前 `generate-autoloads` | 更新命令不再改变工作目录或访问旧路径 |
| `custom.el` 不存在就中断启动 | 允许该可选文件不存在 | 新检出可以启动 |
| XeLaTeX 的 SyncTeX 参数拼为 `--syntex=1` | 改为 `-synctex=1` | 修正反向定位参数 |
| Org 中残留 Company 后端、重复 load-path 和 Transient 强制重载 | 删除未使用设置，提前统一库路径，正常按依赖加载 | 避免后加载新版 Transient 覆盖已加载定义 |
| 编辑生成文件可能启用大量显示扩展 | 启用内置 `global-so-long-mode` | 对超长行文件减少昂贵的显示处理 |

本轮没有更新或删除第三方 Git 子模块。部分子模块已有本地改动；仅凭“当前没 require”不足以断定其中的交互命令与依赖可以删除。

当前 GNU Emacs 32.0.50 的 `(featurep 'mps)` 为 `t`，NeoEmacs 31.1 为 `nil`。GNU Emacs 安装版本的变量文档及本地 `D:/emacs/src/alloc.c` 明确说明：`gc-cons-threshold` 和 `gc-cons-percentage` 对 MPS GC 无效。因此删除此前保留的 64 MiB 启动额度，不再额外设置 GC 参数；NeoEmacs 也保留自身默认值。

## 适合当前配置的新功能

已利用 Emacs 31 的行高亮缓冲区筛选与 `completion-pcm-leading-wildcard`。Eglot 使用当前版本改进过的内置 `flex`。你原来已在使用 EditorConfig、Which-key、Elisp 语义高亮、Flymake 行内诊断和 `delete-trailing-whitespace-mode`，这些无需再装同类插件。功能与版本以安装目录的 NEWS 和 [Emacs 31 NEWS](https://raw.githubusercontent.com/emacs-mirror/emacs/emacs-31/etc/NEWS) 为依据。

可继续试用但没有默认打开的功能：

- **内置补全预览**：`M-x completion-preview-mode`。适合希望以行内文字预览候选的缓冲区。你目前已经启用 Corfu 自动弹窗，因此先在单个缓冲区试用，避免两个自动前端抢占注意力。见 [GNU Emacs Symbol Completion](https://www.gnu.org/s/emacs/manual/html_node/emacs/Symbol-Completion.html)。
- **Emacs 31 项目命令**：`project-root-find-file`、`project-save-some-buffers`、`project-customize-dirlocals`。可减少为常见项目操作添加自定义函数的需要。现有 `C-x p` 项目入口保留。
- **User Lisp 目录**：Emacs 31 可自动整理 `user-lisp/` 下的自动加载与字节编译。适合未来放置小型个人命令。本次未迁移整个配置，避免自动编译目录同时包含归档或初始化副作用。
- **TTY child frames**：GNU Emacs 31 新增该能力，终端中的 Corfu 可以受益；实际以 `(featurep 'tty-child-frames)` 为准，不能只看 NeoEmacs 报告的版本号。

## 保留与后续候选

- 保留 Super-save：当前依赖切换缓冲区、失焦和空闲保存；内置 `auto-save-visited-mode` 不能直接覆盖全部触发行为。
- 保留 Corfu／Vertico／Consult／Orderless／Embark：它们承担不同职责，不是五套重复补全框架。
- 保留 Meow、Puni、Hydra、Dirvish、主题和 Segment Line，避免改变操作习惯与界面。
- 后续按用户要求，已将 `lisp/auctex-latexmk.el`、本地 `pdf2dsc.ps` 副本及 5 个 `self-develop/` 旧实验归档。`self-develop/all-the-icons-diy.el` 仍有条件加载入口，予以保留。清单见 `archive/legacy/README.md`。
- 保留 Linux 分支和其现有个人路径。文献库仍有 `/run/media/kunh/...` 等 Linux 路径，Windows 下需要按真实 Zotero 库位置配置；此次没有猜测或迁移个人文献数据。
- 第三方旧包仍可能报告 `when-let`／`if-let` 弃用提示。恢复 warning 可见性后能看到这些提示；更新它们应分别检查上游变动和已有本地修改。
- Eglot 的自动启动范围仍沿用原有编程 hook。若某种语言没有安装服务端，可以从该语言的 hook 入手限制自动启动；不用为了消除提示切换到另一套 LSP 客户端。

## 验证方式与限制

回归脚本：`tests/run-config-tests.el` 和 `tests/config-tests.el`。覆盖完整配置加载、保留 GC 默认值与文件处理器、LF、延迟加载入口、Markdown 空格、格式化职责、Scala 语法回退、Flymake 实际诊断渲染与主题、Org／LaTeX 模式以及主要键位。

测试使用临时历史目录，并抑制真实 Org-roam 数据库同步。批处理时间仅用于定位启动工作量，不代表 GUI 冷启动耗时。真实窗口中的字体、child frame、LSP 服务响应、Verible 格式化、LaTeX 编译和字典查询需在正常会话中使用验证。

更改在重启后完整生效。重启前保存正在编辑的内容；不要在已有完整会话中反复 `eval-buffer` 整份 `init.el` 来衡量启动性能。

### GC 清理后的运行结果

| 运行时 | MPS | 结果 | 批处理初始化时间 | 初始化后已加载 feature |
|---|---|---|---|---|
| GNU Emacs 32.0.50 | 已启用 | 16／16 通过 | 15.580 秒 | 469 |
| NeoEmacs 31.1 | 未启用 | 16／16 通过 | 13.312 秒 | 465 |

以上是本次环境的一次批处理观测，不是 GUI 性能基准，也不是两个运行时的性能排名。两个运行时都通过了首次 LaTeX 模式加载、延迟包加载、Org 编辑、LF 和格式化行为检查。`git diff --check` 通过。第三方包仍有原有的过时宏／类型与 lexical-binding 提示，未批量改写这些依赖。

后续 Flymake 专项精简与兼容性验证见 [Flymake 检查记录](flymake-review-2026-09.md)。
