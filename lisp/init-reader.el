;;; init-reader --- Init for Reader

;;; Commentary:

;;; Code:

;;; Ebook Reader

(with-eval-after-load 'pdf-tools
  (setq pdf-view-use-scaling t
		pdf-anot-list-format '((page . 3)
							   (type . 10)
							   (contents . 50)
							   (date . 24)))
  (pdf-tools-install))

;; (require 'calibredb)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/nov-xwidget")
;; (require 'nov-xwidget)
;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; ;; (evil-set-initial-state 'nov-mode 'emacs)
;; ;; (add-hook 'nov-mode-hook '(lambda() (turn-off-evil-mode)))
;; ;; (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;; (evil-define-key 'normal nov-mode-map (kbd "o") 'nov-xwidget-view)
;; (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
;; (add-hook 'nov-xwidget-webkit-mode-hook '(lambda() (xwidget-webkit-zoom (xwidget-webkit-current-session) 1.5)))
;; (setq nov-text-width t)
;; (setq visual-fill-column-center-text t)
;; ;; (add-hook 'nov-mode-hook 'visual-line-mode)
;; ;; (add-hook 'nov-mode-hook 'visual-fill-column-mode)
;; ;; (add-hook 'nov-mode-hook '(lambda() (set-fill-column 100)))

(provide 'init-reader)
;;; init-reader.el ends here.
