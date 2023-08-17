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
     (load "auctex.el" nil nil t)
     (load "preview-latex.el" nil nil t)))


(with-eval-after-load 'tex
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil)
  (with-eval-after-load 'eaf
    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

(add-hook 'LaTeX-mode-hook
          (lambda()
			(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex -shell-escape --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
			(setq TeX-command-default "XeLaTeX")
            (TeX-global-PDF-mode)
            (TeX-fold-mode)
		    'turn-on-cdlatex  ;; 设置cdlatex
		    ))

;; (add-hook 'TeX-mode-hook
;;           (lambda()
;; 			(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
;; 			(setq TeX-command-default "XeLaTeX")
;;             (TeX-fold-mode)
;; 		    'turn-on-cdlatex  ;; 设置cdlatex
;; 		    ))

(provide 'init-latex)
;;; init-latex.el ends here.
