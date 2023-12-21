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
        TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-to-list 'TeX-view-program-list '("sioyek" "sioyek --new-window --page %(outpage) %o"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "sioyek"))
  (with-eval-after-load 'eaf
    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex -shell-escape --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-command-default "XeLaTeX")
            (TeX-global-PDF-mode)
            (TeX-fold-mode)
            'turn-on-cdlatex ;; 设置cdlatex
            ))

(add-hook 'TeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-command-default "XeLaTeX")
            (TeX-global-PDF-mode)
            (TeX-fold-mode)
            'turn-on-cdlatex  ;; 设置cdlatex
            ))

(add-hook 'tex-mode-hook
          (lambda()
            (setq display-tex-shell-buffer-action nil)
            (visual-line-mode)
            (TeX-fold-mode)
            'turn-on-cdlatex  ;; 设置cdlatex
            ))

(provide 'init-latex)
;;; init-latex.el ends here.
