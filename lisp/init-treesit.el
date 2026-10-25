;;; init-treesit.el --- Tree-sitter setup and migration -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep one language list; treesit-auto delegates builds to Emacs itself.

;;; Code:

(require 'cl-lib)
(require 'treesit-auto)

(autoload 'qml-ts-mode "qml-ts-mode" nil t)
(autoload 'verilog-ts-mode "verilog-ts-mode" nil t)

(setq treesit-font-lock-level 4
      treesit-auto-install 'prompt
      treesit-auto-langs
      '(bash c cpp css go gomod gowork html java javascript jsdoc json lua
        markdown markdown-inline python qmljs ruby rust scala systemverilog
        toml tsx typescript yaml))

;; Supplement the package's recipes without editing its submodule.
(let ((recipes
       (list
        (make-treesit-auto-recipe
         :lang 'markdown :ts-mode 'markdown-ts-mode
         :remap '(markdown-mode gfm-mode) :requires 'markdown-inline
         :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
         :abi14-revision "v0.4.1" :source-dir "tree-sitter-markdown/src")
        (make-treesit-auto-recipe
         :lang 'markdown-inline :ts-mode 'markdown-ts-mode
         :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
         :abi14-revision "v0.4.1" :source-dir "tree-sitter-markdown-inline/src")
        (make-treesit-auto-recipe
         :lang 'qmljs :ts-mode 'qml-ts-mode
         :url "https://github.com/yuja/tree-sitter-qmljs"
         :ext "\\.qml\\'")
        (make-treesit-auto-recipe
         :lang 'systemverilog :ts-mode 'verilog-ts-mode :remap 'verilog-mode
         :url "https://github.com/gmlarumbe/tree-sitter-systemverilog"
         :ext "\\.s?vh?\\'"))))
  (setq treesit-auto-recipe-list
        (append recipes
                (cl-remove-if
                 (lambda (recipe)
                   (memq (treesit-auto-recipe-lang recipe)
                         '(markdown markdown-inline qmljs verilog systemverilog jsdoc)))
                 treesit-auto-recipe-list)
                ;; Keep the JavaScript recipe first for their shared major mode.
                (list (make-treesit-auto-recipe
                       :lang 'jsdoc :ts-mode 'js-ts-mode
                       :url "https://github.com/tree-sitter/tree-sitter-jsdoc")))))
(setf (treesit-auto-recipe-requires
       (cl-find 'javascript treesit-auto-recipe-list :key #'treesit-auto-recipe-lang))
      'jsdoc)

(defun my/treesit-install-grammars ()
  "Install missing grammars in `treesit-auto-langs' using Emacs's installer.
Load mode definitions first so their tested grammar recipes take precedence.
Requires Git and native C/C++ compilers on PATH.  Return missing languages."
  (interactive)
  (unless (treesit-available-p)
    (user-error "This Emacs cannot use Tree-sitter"))
  (unless (executable-find "git")
    (user-error "Install Git and put it on PATH before installing grammars"))
  (dolist (compiler (list (or (executable-find "cc") (executable-find "gcc"))
                          (or (executable-find "c++") (executable-find "g++"))))
    (unless compiler
      (user-error "Install a C/C++ toolchain and put its bin directory on PATH"))
    (when (eq system-type 'windows-nt)
      (with-temp-buffer
        (unless (and (eq 0 (call-process compiler nil t nil "-dumpmachine"))
                     (string-match-p "mingw" (buffer-string)))
          (user-error "Use MinGW-w64/UCRT64 ahead of Cygwin on PATH; selected: %s" compiler)))))
  (dolist (recipe treesit-auto-recipe-list)
    (when (memq (treesit-auto-recipe-lang recipe) treesit-auto-langs)
      (let* ((mode (treesit-auto-recipe-ts-mode recipe))
             (definition (and (fboundp mode) (symbol-function mode))))
        (when (autoloadp definition)
          (autoload-do-load definition mode)))))
  (treesit-auto-install-all)
  (let ((missing (seq-remove #'treesit-language-available-p treesit-auto-langs)))
    (if missing
        (message "Missing grammars: %s; see *Warnings* and *Messages*. Run again to retry."
                 (mapconcat #'symbol-name missing ", "))
      (message "All %d configured grammars are available; reopen files to use them"
               (length treesit-auto-langs)))
    missing))

(treesit-auto-add-to-auto-mode-alist treesit-auto-langs)
(global-treesit-auto-mode 1)

(provide 'init-treesit)
;;; init-treesit.el ends here
