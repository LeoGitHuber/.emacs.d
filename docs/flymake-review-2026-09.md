# Flymake 配置检查

2026-09-13。按用户选择，保留当前普通行尾显示，归档未使用的自定义箭头与彩色背景渲染。

## 版本与上游变化

当前 GNU Emacs 32.0.50 附带 Flymake 1.4.5，NeoEmacs 31.1 附带 1.4.6。两者都已支持内置 `fancy`、`flymake-diagnostic-format-alist`、点击边栏定位诊断和诊断列表自动列宽。这些能力属于 [Emacs 31 的 Flymake 更新](https://raw.githubusercontent.com/emacs-mirror/emacs/emacs-31/etc/NEWS)，无需为它们额外实现一套列表或格式化器。

本次另取 [Emacs 上游 Flymake 源码](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/flymake.el) 的 1.4.7 副本核对。该版本将显示选项改为 `flymake-inline-diagnostics`，普通行尾样式的值为 `eol`；支持按 `current`／`current-line`／其他位置分别选用样式，行内 face 也改名。旧变量名虽然保留别名，旧值 `t` 不再是普通行尾显示的有效样式。

## 本次改动

- 初次精简把 `lisp/init-diagnostics.el` 从 522 行减到 198 行；保留单图标筛选和空白字体修复后为 235 行。此前虽然安装了 fancy 渲染 advice，显示选项却为 `t`，因此普通使用并未调用它。
- 自定义渲染、混色计算、相关主题函数和未启用的 Flycheck 设置保存到 `archive/legacy/flymake-fancy.el`。单色主题中对应的旧 face 覆盖以注释保存，活动配置不再引用这些 face。
- 新版设置 `flymake-inline-diagnostics` 为 `eol`；旧版继续设置 `flymake-show-diagnostics-at-end-of-line` 为 `t`。按实际接口检测，不按 Emacs 主版本号猜测。
- 保留错误／警告／提示的边栏图标、字体回退、行尾字号和主题配色。主题更新会解析新版 face 别名。
- 边栏图标的类型属性在初始化时设置一次，移除每个缓冲区重复执行的同步 hook。删除与运行时默认值相同的 0.5 秒检查间隔和自动调整边栏宽度设置。
- diff-hl 的显示位置和现有诊断按键保留。Flymake 继续使用左侧 margin：内置 `auto` 在 GUI 中优先使用 fringe，会影响现在与 diff-hl 的位置分工。

## 左侧图标数量与显示

原始 Flymake 为每条诊断的 overlay 设置一个 margin `before-string`。同一行的错误、警告和提示会同时提供多个不同颜色的图标；设置 overlay 优先级不能让这些独立字符串变成一个图标。最小复现确认同一行有两个诊断图标。

现在每行只显示严重程度最高的一项，默认顺序为错误、警告、提示。只隐藏额外的 margin 字符串，全部诊断、下划线、行尾摘要和原生鼠标交互继续保留。修复不修改 diff-hl 的 fringe／右侧 margin 显示。

Flymake 当前没有“margin 每行合并为一个图标”的内置变量，`short` 只影响行尾文字。用户选择保留 margin 字体图标，因此保留一个必要的筛选函数，复用 `flymake-diagnostics`、内置严重程度和 overlay 数据。在原有 `flymake--handle-report`、`flymake-after-change-function` 后调用它，支持诊断撤销和编辑拆行／合行；未增加状态变量、编辑跟踪函数或局部 hook，也未替换原生报告处理和行尾渲染函数。

用户重启后进一步确认：只有一个图标，但旁边仍有彩色线条或色块；临时关闭 diff-hl 后现象仍在。曾尝试给外层字符串设置 `default` face，并追加 margin 空白，用户重启后反馈无效，已撤回该尝试及其专用测试。

### GNU Emacs：已在实际 GUI 中定位并修复

使用当前配置启动 GNU Emacs，检查 `init.el` 第 1783、1785 行的实际 Flymake overlay，每行确实只有一个警告图标。内层显示字符串原为警告字形加 U+200A 窄空格。Flymake 给整个内层字符串重新设置 face，使空格也继承 `diagnostics-warn` 的 `Symbols Nerd Font Mono` 字体；预先给图标字符单独设置 face 无法阻止这一步。

实际 GUI 中的字体对象确认：当前安装的该字体包含 U+F071 警告字形，却不包含 U+0020 普通空格和 U+200A 窄空格。移除内层空格时杂色消失，恢复时重新出现；缺字框被边栏裁切，表现为图标旁的彩色线条。仅去掉空格又会让 Flymake 把边栏缩到一列，裁切图标本身。

现在保留一个普通空格维持两列宽度，在已有筛选函数中将这个空格的 face 设为 `(margin default)`，图标本身继续使用 Nerd Font。GNU Emacs GUI 的 `font-at` 确认两者分别使用 `Symbols Nerd Font Mono` 和 `Cascadia Mono`；恢复正常字号后，实际问题行显示为完整图标，旁边杂色消失。没有新增 advice、hook 或外层填充字符串，鼠标 keymap 仍由 Flymake 提供。

### NeoEmacs：仍有独立的字体显示异常

NeoEmacs 31.1 的实际 GUI 已加载同一份修复，空白的 face 正确，但警告图标本身仍显示成橙色框的一部分。将同一个 U+F071 字符直接放进正文，分别使用 `diagnostics-warn` 和显式 `(:family "Symbols Nerd Font Mono")`，同样显示异常，因而此现象已经超出 Flymake margin 的显示逻辑。

实际检查确认图标 face 中的字体名称正确，`find-font` 也能找到已安装的 `NFM.ttf`。临时把边栏从两列加宽到八列，或将 `neomacs-font-backend` 从 `cosmic` 切到 `emacs`，都没有恢复字形。检查后已恢复两列和 `cosmic`。这些证据将剩余问题定位到当前 NeoEmacs 的字体显示层；具体字体解析或光栅化实现原因尚未确定，本次不宣称 NeoEmacs 的 GUI 问题已经修复，也不为此继续堆叠 Flymake advice。

忽略目录 `var/config-review/` 中保留 GUI 属性检查记录。临时加载入口不属于日常配置，检查后删除。NeoEmacs 中的只读复现缓冲区保留了 margin、正文 face、正文显式字体三种显示，便于后续对照。

## 可使用的内置功能

`C-c l e` 继续打开当前缓冲区诊断列表。项目范围可用 `M-x flymake-show-project-diagnostics`，错误详情仍可通过 ElDoc 查看。默认格式已经让行尾较简短、详情保留完整消息，因此没有再覆盖一份相同的 `flymake-diagnostic-format-alist`。

将来使用包含新接口的 Flymake 时，可以单独在某个缓冲区试用：

```elisp
(setq-local flymake-inline-diagnostics
            '((current-line . fancy) (t . short)))
```

这会让当前行显示内置引导线，其他行显示最严重的一条诊断。当前安装的 1.4.5／1.4.6 尚不支持该组合，本次默认显示不采用它。

## 验证范围

| 运行时 | Flymake | 结果 |
|---|---|---|
| GNU Emacs 32.0.50 | 安装版本 1.4.5 | 19／19 通过 |
| NeoEmacs 31.1 | 安装版本 1.4.6 | 19／19 通过 |
| GNU Emacs 32.0.50 | 上游测试副本 1.4.7 | 19／19 通过 |
| NeoEmacs 31.1 | 上游测试副本 1.4.7 | 19／19 通过 |

测试现在实际报告同一行的错误和警告，检查生成的行尾 overlay 是否包含两条摘要、是否仍为单行、完整消息是否保留，以及边栏图标与主题 face。新增检查同一行只显示最高级别图标、多个后端独立撤销诊断后的图标恢复、空报告清理，以及编辑拆行／合行后的图标数量。现有测试同时验证空白使用正文字体、两列字符串宽度和原生鼠标 keymap。旧测试仅检查 advice 是否安装，无法发现“装上了渲染器却未使用”的问题。

上游测试副本仅位于忽略的 `var/config-review/flymake-upstream/`，通过测试进程的 `-L` 参数加载。1.4.7 新增了后端内容信任检查；测试后端只返回固定文本，按该接口声明安全，不更改实际后端或全局信任策略。实际安装目录和日常启动的 Flymake 版本保持不变。

上游副本 SHA-256：`D01C23B75225ACC9DC48B9A4AD99CC462264F3669B55BA042E08A2D04477E0B3`。

以上四组批处理测试已在最终配置上重新通过。GNU Emacs 的实际 GUI 对照通过；NeoEmacs 的字体显示异常仍在，不能用批处理结果替代这一结论。
