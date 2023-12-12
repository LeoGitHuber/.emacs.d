;;; Config.el --- Emacs configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(let ((file-name-handler-alist nil))
  ;; (require 'package)
  ;; (package-initialize)

  (setq package-archives
        '(("gnu"    .
           "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
          ("nongnu" .
           "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
          ("melpa"  .
           "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/"))
        ;; “Gnu”应该和“melpa”同优先级, 从而默认选取二者中较新的 package.
        package-archive-priorities '(("gnu"    . 1)
                                     ("nongnu" . 0)
                                     ("melpa"  . 1))
        package-menu-hide-low-priority t
        ;; 暂时不知道检查签名有什么用,先关了再说.
        package-check-signature nil)

  ;;; Emacs Default Setting
  (load "~/.emacs.d/lisp/init-func.el")

  (let ((packages (find-subdir-recursively "~/.emacs.d/site-lisp")))
    (setq load-path (append load-path packages)))
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/treemacs/src/elisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/treemacs/src/extra/")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-mode/clients")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-mode/scripts")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-mode/docs")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/vertico/extensions")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/themes/themes")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/pdf-tools/lisp")
  (load "~/.emacs.d/site-lisp/loaddefs.el")
  (load "~/.emacs.d/lisp/init-gc.el")
  (load "~/.emacs.d/lisp/init-flymake.el")
  (load "~/.emacs.d/lisp/init-icons.el")
  (load "~/.emacs.d/lisp/init-meow.el")
  (load "~/.emacs.d/lisp/init-keybindings.el")
  (load "~/.emacs.d/lisp/init-lsp.el")
  (load "~/.emacs.d/lisp/init-dired.el")
  (load "~/.emacs.d/lisp/init-chinese.el")
  (load "~/.emacs.d/lisp/init-input.el")
  (load "~/.emacs.d/lisp/init-eaf.el")
  (load "~/.emacs.d/lisp/init-latex.el")
  (load "~/.emacs.d/lisp/init-org.el")
  (load "~/.emacs.d/lisp/init-verilog.el")
  (load "~/.emacs.d/lisp/init-reader.el")
  (load "~/.emacs.d/lisp/init-hydra.el")
  (load "~/.emacs.d/lisp/latex-node.el")
  (load "~/.emacs.d/lisp/init-base.el")
  )

(provide 'config)
;;; config.el ends here.
