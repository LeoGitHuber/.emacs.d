# 旧配置归档

2026-09-13 根据当前 `early-init.el → init.el` 的加载关系整理。

此目录不在 `load-path` 中，也不参与启动。累计 24 个旧文件按原路径保留，另有 `helpers.el` 保存旧函数片段。移动前后核对过 SHA-256，文件内容未改写。

- `config.el`：旧的第二套入口，仍引用已经不存在的 `init-meow.el`、`init-dired.el`、`init-eaf.el`。
- `lisp/init-base.el`、`init-gc.el`：旧基础设置、另一套保存与 GC 策略。
- `lisp/init-flycheck.el`、`init-flymake.el`、`init-lsp.el`：旧诊断与 LSP 组合；当前入口采用 Eglot、Flymake、Corfu。
- `lisp/init-keybindings.el`、`init-hydra.el`、`init-icons.el`：当前入口已包含对应配置。
- `lisp/init-chinese.el`、`init-input.el`、`init-org.el`、`init-latex.el`、`init-reader.el`、`init-verilog.el`：未被当前入口加载的旧模块版本。
- `lisp/init-abandoned.el`：原有弃用设置备忘。
- `lisp/init-pdump.el`：旧 dump 实验脚本，按源码表单猜测加载顺序，不适合作为当前双运行时的启动入口。
- `helpers.el`：从当前 `init-func.el` 移出的未引用 LSP booster helper、重复 GC helper、旧字体实现。

这里的文件用来查阅与恢复特定片段，不是一套可以整体重新加载的配置。恢复时先确认对应行为没有在当前入口中重复定义。当前使用的 `init-func.el`、`init-startup.el` 和 `init-theme.el` 仍在根目录的 `lisp/` 下。

## 补充归档

根据用户要求，再检查主配置、延迟加载入口与仓库内依赖后，补充归档以下 7 个文件。本次只移动文件并更新说明，没有改写活动配置。

| 原路径（相对此仓库根目录） | 归档依据 |
|---|---|
| `lisp/auctex-latexmk.el` | 未被活动入口 require 或调用；当前 `init-tex.el` 直接配置 AUCTeX 的 LaTeXMk 命令。 |
| `lisp/pdf2dsc.ps` | 没有指向此本地副本的配置或脚本引用；AUCTeX 中的 `pdf2dsc` 是外部命令入口。 |
| `self-develop/modeline-setting.el` | 旧状态栏配置，当前使用 Segment Line。 |
| `self-develop/emacs-nf-diy.el` | 只被上面的旧状态栏加载，一并归档。 |
| `self-develop/tooltip.el` | 未接入 load-path 的旧 tooltip 实现副本，当前使用运行时提供的 tooltip。 |
| `self-develop/tooltip-box.el.bak` | 未被加载的 tooltip 实验备份。 |
| `self-develop/vim-bar.el` | 未被主配置、hook 或自动加载入口引用的标签栏实验。 |

仍保留在活动目录中的条件依赖：`self-develop/all-the-icons-diy.el` 由 `init.el` 的 `with-eval-after-load` 引用；`lisp/latex-node.el` 由交互命令自动加载，并使用 `lisp/template.tex`。没有把第三方包、Rime 数据或维护脚本仅因未在启动时加载就判为无用。
