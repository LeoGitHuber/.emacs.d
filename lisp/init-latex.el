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
     ))

(with-eval-after-load 'tex
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
  (add-to-list 'TeX-view-program-list '("sioyek" "sioyek --new-window --page %(outpage) %o"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "sioyek"))
  (with-eval-after-load 'eaf
    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex -shell-escape --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-command-default "XeLaTeX"
                  )
            (TeX-global-PDF-mode)
            (TeX-fold-mode 1)
            'turn-on-cdlatex
            'turn-on-reftex
            ))

(add-hook 'TeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-command-default "XeLaTeX")
            (TeX-global-PDF-mode)
            (TeX-fold-mode 1)
            'turn-on-cdlatex
            'turn-on-reftex
            ))

(add-hook 'tex-mode-hook
          (lambda()
            (setq display-tex-shell-buffer-action nil)
            (visual-line-mode)
            (TeX-fold-mode 1)
            'turn-on-cdlatex
            'turn-on-reftex
            ))

(provide 'init-latex)
;;; init-latex.el ends here.
