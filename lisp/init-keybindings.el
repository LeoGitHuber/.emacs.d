;;; init-keybindings --- Init for Keybindings
;;; Commentary:
;;; Code:

;;; Puni
(add-to-list 'load-path "~/.emacs.d/site-lisp/puni")
;; (require 'puni)

(if (bound-and-true-p meow-mode)
	(progn
	  (keymap-global-unset "M-l")
	  (keymap-global-unset "M-c")
	  (keymap-global-unset "M-u")
	  (keymap-global-unset "C-w")
	  (keymap-global-unset "M-w")
	  (keymap-global-unset "C-k")
	  (keymap-global-set "C-h" 'backward-delete-char-untabify)
	  (keymap-global-set "C-w" 'kill-or-save)
	  (keymap-global-set "M-w" 'puni-kill-region)
	  (keymap-global-set "C-k" 'puni-kill-line)
	  (keymap-global-set "M-l" 'downcase-any)
	  (keymap-global-set "M-u" 'upcase-any)
	  (keymap-global-set "M-c" 'capitalize-any))
  (progn
	(keymap-global-unset "M-l")
	(keymap-global-unset "M-c")
	(keymap-global-unset "M-u")
	(keymap-global-unset "C-w")
	(keymap-global-unset "M-w")
	(keymap-global-unset "C-k")
	(keymap-global-set "C-h" 'backward-delete-char-untabify)
	(keymap-global-set "C-w" 'kill-or-save)
	(keymap-global-set "M-w" 'puni-kill-region)
	(keymap-global-set "C-k" 'puni-kill-line)
	(keymap-global-set "M-l" 'downcase-any)
	(keymap-global-set "M-u" 'upcase-any)
	(keymap-global-set "M-c" 'capitalize-any)
    (keymap-global-set "M-j" 'open-newline-above)
    (keymap-global-set "C-j" 'open-newline-below)
    (keymap-global-set "C-x C-r" 'restart-emacs)
    (keymap-global-set "M-o" 'other-window)
    (keymap-global-set "M-N" 'windmove-down)
    (keymap-global-set "M-P" 'windmove-up)
    (keymap-global-set "M-I" 'windmove-right)
    (keymap-global-set "M-O" 'windmove-left)
    ;;replace =isearch-delete-char= with =isearch-del-char=
    (keymap-set isearch-mode-map "C-h" 'isearch-del-char)
    ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/avy")

    ;; Avy
    (keymap-set isearch-mode-map "M-j" 'avy-isearch)
    (keymap-global-set "C-'" 'avy-goto-char-in-line)
    ;; (global-set-key (kbd "C-:") 'avy-goto-char)
    ;; (global-set-key (kbd "M-g c") 'avy-goto-char-timer)
    ;; (global-set-key (kbd "M-g w") 'avy-goto-word-1)
    ;; (global-set-key (kbd "M-g e") 'avy-goto-word-0)
    ;; (global-set-key (kbd "M-g f") 'avy-goto-line)
    ;; (global-set-key (kbd "C-c C-j") 'avy-resume)
    ))

;;; Helpful
(keymap-global-set "M-?" 'help-command)
(with-eval-after-load 'help
  (define-key global-map [remap describe-function] 'helpful-function)
  (define-key global-map [remap describe-key] 'helpful-key)
  (define-key global-map [remap describe-variable] 'helpful-variable)
  (define-key global-map [remap describe-command] 'helpful-command))

(provide 'init-keybindings)
;;; init-keybindings.el ends here.
