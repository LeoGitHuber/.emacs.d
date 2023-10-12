;;; init-icons --- Init for icons
;;; Commentary:
;;; Code:


(add-to-list 'load-path "~/.emacs.d/site-lisp/nerd-icons.el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/treemacs-nerd-icons")
(add-to-list 'load-path "~/.emacs.d/site-lisp/nerd-icons-dired")

(setq nerd-icons-font-family "JetBrainsMono Nerd Font")

(require 'nerd-icons)

(load "~/.emacs.d/self-develop/modeline-setting.el")

(add-hook 'dired-mode-hook 'nerd-icons-dired-mode)

(with-eval-after-load 'treemacs
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))

(add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode)

;; (require 'all-the-icons)
(with-eval-after-load 'all-the-icons (load "~/.emacs.d/self-develop/all-the-icons-diy.el"))

;; (with-eval-after-load 'nerd-icons
;; (setq fc-info (nerd-icons-codicon "nf-cod-question" :face '(:inherit flycheck-info-my))
;; 		fc-warning (nerd-icons-codicon "nf-cod-warning" :face '(:inherit flycheck-warn))
;; 		fc-error (nerd-icons-codicon "nf-cod-error" :face '(:inherit flycheck-error-my)))
;; )

(with-eval-after-load 'tab-bar
  (setq tab-bar-close-button
        (propertize " 󰅖" :help-echo "Close tab"
                    'close-tab t
                    'mouse-face 'highlight
                    'pointer 'vdrag
                    ))
  ;; modified_icon = '●'
  ;; close_icon = ''
  )

(provide 'init-icons)
;;; init-icons.el ends here.
