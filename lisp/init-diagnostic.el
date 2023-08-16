;;; init-diagnostic --- Init for Diagnostic
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/flycheck")
;; (require 'flycheck)

;; (with-eval-after-load 'flycheck
;;   (flycheck-define-checker c/c++-gcc
;;     "A C/C++ syntax checker using GCC.

;;   Requires GCC 4.4 or newer.  See URL `https://gcc.gnu.org/'."
;;     :command ("gcc"
;;   			  "-fshow-column"
;;   			  "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
;;   			  (option "-std=" flycheck-gcc-language-standard concat)
;;   			  (option-flag "-pedantic" flycheck-gcc-pedantic)
;;   			  (option-flag "-pedantic-errors" flycheck-gcc-pedantic-errors)
;;   			  (option-flag "-fno-exceptions" flycheck-gcc-no-exceptions)
;;   			  (option-flag "-fno-rtti" flycheck-gcc-no-rtti)
;;   			  (option-flag "-fopenmp" flycheck-gcc-openmp)
;;   			  (option-list "-include" flycheck-gcc-includes)
;;   			  (option-list "-W" flycheck-gcc-warnings concat)
;;   			  (option-list "-D" flycheck-gcc-definitions concat)
;;   			  (option-list "-I" flycheck-gcc-include-path)
;;   			  (eval flycheck-gcc-args)
;;   			  "-x" (eval
;;   				    (pcase major-mode
;;   					  (`c-mode "c")
;;   					  (`c++-mode "c++")
;;   					  (`c-ts-mode "c")
;;   					  (`c++-ts-mode "c++")))
;;   			  ;; GCC performs full checking only when actually compiling, so
;;   			  ;; `-fsyntax-only' is not enough. Just let it generate assembly
;;   			  ;; code.
;;   			  "-S" "-o" null-device
;;   			  ;; Read from standard input
;;   			  "-")
;;     :standard-input t
;;     :error-patterns
;;     ((info line-start (or "<stdin>" (file-name))
;;   		   ":" line (optional ":" column)
;;   		   ": note: " (message) line-end)
;;      (warning line-start (or "<stdin>" (file-name))
;;   			  ":" line (optional ":" column)
;;   			  ": warning: " (message (one-or-more (not (any "\n["))))
;;   			  (optional "[" (id (one-or-more not-newline)) "]") line-end)
;;      (error line-start (or "<stdin>" (file-name))
;;   		    ":" line (optional ":" column)
;;   		    ": " (or "fatal error" "error") ": " (message) line-end))
;;     :modes (c-mode c++-mode c-ts-mode c++-ts-mode)
;;     :next-checkers ((warning . c/c++-cppcheck))))

;; (global-flycheck-mode)
;; (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)
;; (with-eval-after-load 'flycheck (setq flycheck-indication-mode 'left-margin))

(with-eval-after-load 'flymake
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package))))

  (defun sanityinc/enable-flymake-flycheck ()
    (setq-local flymake-diagnostic-functions
                (seq-uniq (append flymake-diagnostic-functions
                                  (flymake-flycheck-all-chained-diagnostic-functions))))))

(add-to-list 'load-path "~/.emacs.d/site-lisp/flymake-flycheck")

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'flymake-mode))

;; (dolist (hook '(prog-mode-hook text-mode-hook))
;;   (add-hook hook 'flycheck-mode))


(setq eldoc-documentation-function 'eldoc-documentation-compose)

(add-hook 'flymake-mode-hook
          (lambda ()
            (sanityinc/enable-flymake-flycheck)
            ;; (setq-default eldoc-documentation-functions
            ;;               (cons 'flymake-eldoc-function
            ;;                     (delq 'flymake-eldoc-function eldoc-documentation-functions)))
            ))


(provide 'init-diagnostic)
;;; init-diagnostic.el ends here.
