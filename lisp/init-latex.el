;;; init-latex --- Init for LaTeX
;;; Commentary:
;;; Code:

;;; LaTeX

;; On demand loading, leads to faster startup time.
(pdf-loader-install)

(eval-after-load 'info
  '(add-to-list 'Info-directory-list "/usr/local/share/info"))

(eval-after-load "tex-mode"
  '(progn
     (load "auctex.el" nil t t)
     (load "preview-latex.el" nil t t)
     ;; (require 'org-table)
     ))

;; (defun orgtbl-next-field-maybe ()
;;   "Combine `lsp-bridge-mode', `cdlatex-mode' and `orgtlr-mode'."
;;   (interactive)
;;   (if (and (bound-and-true-p lsp-bridge-mode)
;;            (acm-frame-visible-p acm-menu-frame))
;;       (acm-complete)
;;     (if (bound-and-true-p cdlatex-mode)
;;         (cdlatex-tab)
;;       (org-table-next-field))))

(with-eval-after-load 'tex
  (add-hook 'cdlatex-tab-hook
            (lambda ()
              (and (bound-and-true-p lsp-bidge-mode)
                   (acm-frame-visible-p acm-menu-frame))))
  (load "~/.emacs.d/lisp/auctex-latexmk.el")
  (setq TeX-auto-save t
        TeX-parse-self t
        ;; TeX-fold-auto t
        TeX-expand-list '(("%x" TeX-active-master-with-quotes "xdv" t))
        preview-image-type 'dvipng
        ;; preview-pdf-color-adjust-method nil
        )
  (setq-default TeX-master nil
                TeX-engine 'xetex
                preview-scale-function 0.6
                ;; preview-LaTeX-command '("%`%l -no-pdf \"\\nonstopmode\\nofiles\
                ;; \\PassOptionsToPackage{" ("," . preview-required-option-list) "}{preview}\
                ;; \\AtBeginDocument{\\ifx\\ifPreview\\undefined"
                ;; preview-default-preamble "\\fi}\"%' \"\\detokenize{\" %(t-filename-only) \"}\"")
                ;; preview-dvipng-command "dvipng -picky -noghostscript %x -o %m/prev%%03d.png"
                )
  ;; (add-to-list 'TeX-view-program-list '("sioyek" "sioyek --page %(outpage) %o"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Sioyek"))
  (with-eval-after-load 'eaf
    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf")))
  )


(dolist (hook '(LaTeX-mode-hook TeX-mode-hook tex-mode-hook))
  (add-hook hook
            (lambda()
              (auctex-latexmk-setup)
              (electric-indent-local-mode)
              (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex -shell-escape --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
              (setq TeX-command-default "XeLaTeX")
              (add-to-list 'texmathp-tex-commands1 '("lstlisting" env-off))
              (TeX-global-PDF-mode)
              (TeX-fold-mode 1)
              (turn-on-cdlatex)
              (turn-on-reftex)
              ;; (keymap-set cdlatex-mode-map "<tab>" 'cdlatex-tab-maybe)
              ;; (turn-on-orgtbl)
              ;; (keymap-set orgtbl-mode-map "<tab>" 'orgtbl-next-field-maybe)
              )))

(add-hook 'tex-mode-hook
          (lambda()
            (electric-indent-local-mode)
            (setq display-tex-shell-buffer-action nil)
            (visual-line-mode)
            (TeX-fold-mode 1)
            (turn-on-cdlatex)
            (turn-on-reftex)
            ;; (turn-on-orgtbl)
            ;; (keymap-set orgtbl-mode-map "<tab>" 'orgtbl-next-field-maybe)
            ))

(provide 'init-latex)
;;; init-latex.el ends here.
