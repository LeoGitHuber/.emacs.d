;;; init-latex --- Init for LaTeX
;;; Commentary:
;;; Code:

;;; LaTeX

(defun rime-predicate-tex-advance-p ()
  "If point is inside a (La)TeX math environment, or a (La)TeX command."
  (if (derived-mode-p 'tex-mode)
      (or (and (featurep 'tex-site)
               (texmathp))
          (and rime--current-input-key
               (or (= #x24 rime--current-input-key)
                   (= #x5c rime--current-input-key))
               (or (= (point) (line-beginning-position))
                   (= #x20 (char-before))
                   (rime-predicate-after-ascii-char-p)))
          (and (> (point) (save-excursion (back-to-indentation) (point)))
               (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
                 (or (or (string-match-p "[\x5c][\x21-\x24\x26-\x7a\x7c\x7e]*$" string)
                         (if (string-match-p "[\x5c][a-zA-Z\x23\x40]+[\x7b][^\x7d\x25]*$" string)
                             (if (and (string-match-p "[\x5c]\\(begin\\)\\|\\(end\\)[\x7b]" string)
                                      (= (char-before) #x7b))
                                 t
                               (if (> (char-before) #x7b)
                                   (and rime--current-input-key
                                        (or (= #x7e rime--current-input-key)
                                            (= #x7c rime--current-input-key)))
                                 ())
                               ())))
                     (string-match-p "[a-zA-Z][0-9\x21-\x23\x25-\x2f\x3a-\x40\x5b-\x60\x7a\x7c\7e\x7f]*$" string)
                     ))))
    (rime-predicate-after-ascii-char-p)))

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

;; (add-hook 'TeX-mode-hook
;;           (lambda()
;; 			(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
;; 			(setq TeX-command-default "XeLaTeX")
;;             (TeX-fold-mode)
;; 		    'turn-on-cdlatex  ;; 设置cdlatex
;; 		    ))

(provide 'init-latex)
;;; init-latex.el ends here.
