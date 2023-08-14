;;; init-input --- Init for input method
;;; Commentary:
;;; Code:

;; Input Method
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-rime")

(with-eval-after-load 'rime
  (keymap-set rime-mode-map "C-`" 'rime-send-keybinding)
  ;; (define-key rime-mode-map (kbd "Shift") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "C-t") 'rime-inline-ascii)
  (define-key minibuffer-mode-map (kbd "C-t") 'rime-inline-ascii)
  (setq default-input-method "rime"
		rime-user-data-dir "~/.local/share/fcitx5/rime"    ;; "~/.emacs.d/rime/"
		rime-show-candidate 'posframe
		rime-show-preedit 't
		rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "C-h")
		rime-posframe-properties (list :internal-border-width 3)
		rime-posframe-style 'vertical
		;; (list :font "Source Han Serif SC"
		;; :background-color "#333333"
		;; :internal-border-width 10)
		rime-disable-predicates
		'(;; rime-predicate-evil-mode-p
		  rime-predicate-space-after-cc-p
		  rime-predicate-current-uppercase-letter-p
		  rime-predicate-after-alphabet-char-p
		  rime-predicate-prog-in-code-p
		  rime-predicate-hydra-p
		  ;; rime-predicate-evil-mode-p
		  ;; meow-normal-mode-p
		  ;; rime-predicate-prog-in-code-p
		  ;; rime-predicate-tex-math-or-command-p
		  )
		rime-deactivate-when-exit-minibuffer nil
		rime-inline-ascii-trigger 'shift-l
		)
  ;; (set-face-attribute 'rime-comment-face nil :foreground "#dcdccc")
  )

;; (keymap-global-set "C-\\" 'rime-commit-and-toggle-input-method)
(defun rime-commit1-and-toggle-input-method ()
  "Commit the 1st item if exists, then toggle input method."
  (interactive)
  (require 'rime)
  (ignore-errors (rime-commit1))
  (toggle-input-method))
;; (autoload 'rime-commit1-and-toggle-input-method "rime" nil t)
(keymap-global-set "C-\\" 'rime-commit1-and-toggle-input-method)
(with-eval-after-load 'rime (keymap-set rime-mode-map "M-y" 'rime-force-enable))

;; (require 'pyim-wbdict)
;; (require 'pyim)
;; (setq default-input-method "pyim")
;; (setq pyim-page-length 7)
;; (setq pyim-page-posframe-border-width 3)
;; (pyim-default-scheme 'wubi)
;; (setq pyim-page-tooltip 'posframe)
;; (if (string-equal (symbol-name (car custom-enabled-themes)) "modus-operandi")
;; (progn
;; (set-face-attribute 'pyim-page nil :inherit 'default :background "#EEE1B3" :foreground "#000000")
;; (set-face-attribute 'pyim-page-border nil :inherit 'pyim-page :background "#000000"))
;; (set-face-attribute 'pyim-page-border nil :inherit 'pyim-page :background "#D7DCC8"))
;;
;; ;; (pyim-wbdict-v86-single-enable)
;; (pyim-wbdict-v98-morphe-enable)
;; (setq-default pyim-english-input-switch-functions
;; '(pyim-probe-isearch-mode
;; pyim-probe-dynamic-english
;; pyim-probe-programe-mode
;; pyim-probe-org-structure-template))
;; (global-set-key "\C-\\" 'toggle-input-method)
;; (global-set-key "\M-i" #'pyim-convert-string-at-point)

;; (global-set-key "\M-p" 'pyim-process-toggle-input-ascii)
;; (global-set-key "\M-j" 'pyim-toggle-input-ascii)

(provide 'init-input)
;;; init-input.el ends here.
