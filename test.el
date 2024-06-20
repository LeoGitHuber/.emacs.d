;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/dash.el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/ht.el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/f.el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/s.el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/hydra")
(add-to-list 'load-path "~/.emacs.d/site-lisp/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/spinner.el")
(load "~/.emacs.d/site-lisp/lsp-mode/clients/lsp-verilog.el")
(load "~/.emacs.d/site-lisp/lsp-mode/lsp-lens.el")
(load "~/.emacs.d/site-lisp/lsp-mode/lsp-modeline.el")
(load "~/.emacs.d/site-lisp/lsp-mode/lsp-headerline.el")

(require 'lsp-mode)
