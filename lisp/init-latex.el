;;; init-latex --- Init for LaTeX
;;; Commentary:
;;; Code:

;;; LaTeX

(with-eval-after-load 'tex
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil))

(add-hook 'LaTeX-mode-hook
          (lambda()
			(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
			(setq TeX-command-default "XeLaTeX")
		    'turn-on-cdlatex  ;; 设置cdlatex
		    ))

(add-hook 'TeX-mode-hook
          (lambda()
			(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
			(setq TeX-command-default "XeLaTeX")
		    'turn-on-cdlatex  ;; 设置cdlatex
		    ))

(provide 'init-latex)
;;; init-latex.el ends here.
