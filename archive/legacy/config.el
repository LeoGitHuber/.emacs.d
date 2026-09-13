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
  (load (expand-file-name "lisp/init-func.el" user-emacs-directory))

  (let ((packages (find-subdir-recursively (my/emacs-path "site-lisp"))))
    (setq load-path (append load-path packages)))
  (dolist (path
           (mapcar #'my/emacs-path
                   '("site-lisp"
                     "site-lisp/treemacs/src/elisp"
                     "site-lisp/treemacs/src/extra/"
                     "site-lisp/lsp-mode/clients"
                     "site-lisp/lsp-mode/scripts"
                     "site-lisp/lsp-mode/docs"
                     "site-lisp/vertico/extensions"
                     "site-lisp/themes/themes"
                     "site-lisp/pdf-tools/lisp"
                     "site-lisp/verilog-ext")))
    (add-to-list 'load-path path))
  (load (my/emacs-path "site-lisp/loaddefs.el"))
  (load (my/emacs-path "lisp/init-gc.el"))
  (load (my/emacs-path "lisp/init-flymake.el"))
  (load (my/emacs-path "lisp/init-icons.el"))
  (load (my/emacs-path "lisp/init-meow.el"))
  (load (my/emacs-path "lisp/init-keybindings.el"))
  (load (my/emacs-path "lisp/init-lsp.el"))
  (load (my/emacs-path "lisp/init-dired.el"))
  (load (my/emacs-path "lisp/init-chinese.el"))
  (load (my/emacs-path "lisp/init-input.el"))
  (load (my/emacs-path "lisp/init-eaf.el"))
  (load (my/emacs-path "lisp/init-org.el"))
  (load (my/emacs-path "lisp/init-verilog.el"))
  (load (my/emacs-path "lisp/init-reader.el"))
  (load (my/emacs-path "lisp/init-hydra.el"))
  (load (my/emacs-path "lisp/latex-node.el"))
  (load (my/emacs-path "lisp/init-latex.el"))
  (load (my/emacs-path "lisp/init-base.el"))
  )

(provide 'config)
;;; config.el ends here.
