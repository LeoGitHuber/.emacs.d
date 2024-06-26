;;; init-keybindings --- Init for Keybindings
;;; Commentary:
;;; Code:

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/combobulate")

(unless (bound-and-true-p meow-mode)
  (progn
    (keymap-global-set "M-j" 'open-newline-above)
    (keymap-global-set "C-j" 'open-newline-below)
    ;; (keymap-global-set "M-N" 'windmove-down)
    ;; (keymap-global-set "M-P" 'windmove-up)
    ;; (keymap-global-set "M-I" 'windmove-right)
    ;; (keymap-global-set "M-O" 'windmove-left)
    ;;replace =isearch-delete-char= with =isearch-del-char=
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

(keymap-global-set "C-k" 'smart-kill-line)
;; (keymap-global-set "M-l" 'downcase-any)
;; (keymap-global-set "M-c" 'capitalize-any)
(keymap-global-set "C-w" 'kill-or-save)
(add-hook 'puni-mode-hook
          (lambda ()
            (keymap-set puni-mode-map "M-w" 'puni-kill-region)
            (keymap-set puni-mode-map "M-k" 'puni-backward-kill-line)
            (keymap-unset puni-mode-map "C-w")))

(keymap-set isearch-mode-map "C-h" 'isearch-del-char)
(keymap-global-set "C-h" 'backward-delete-char-untabify)
(keymap-global-set "C-x k" 'kill-current-buffer)
(keymap-global-set "C-x C-r" 'restart-emacs)
(keymap-global-set "C-c g" 'consult-ripgrep)
(keymap-global-set "C-c f" 'consult-fd)
;;; Efficiency
(keymap-global-set "C-x f" 'find-file)
(keymap-global-set "C-z" 'vundo)
(global-set-key [remap comment-dwim] 'comment-or-uncomment)

;;; Fingertip
;; (dolist (hook '(emacs-lisp-mode-hook c-mode-hook lisp-mode-hook))
;;   (add-hook hook 'fingertip-mode))
(with-eval-after-load 'fingertip
  ;; 移动
  ;; ("M-n" . fingertip-jump-left)
  ;; ("M-p" . fingertip-jump-right)
  ;; 符号插入
  (keymap-set fingertip-mode-map "%" 'fingertip-match-paren)       ;括号跳转
  (keymap-set fingertip-mode-map "(" 'fingertip-open-round)        ;智能 (
  (keymap-set fingertip-mode-map "[" 'fingertip-open-bracket)      ;智能 [
  (keymap-set fingertip-mode-map "{" 'fingertip-open-curly)        ;智能 {
  (keymap-set fingertip-mode-map ")" 'fingertip-close-round)       ;智能 )
  (keymap-set fingertip-mode-map "]" 'fingertip-close-bracket)     ;智能 ]
  (keymap-set fingertip-mode-map "}" 'fingertip-close-curly)       ;智能 }
  (keymap-set fingertip-mode-map "\"" 'fingertip-double-quote)     ;智能 "
  (keymap-set fingertip-mode-map "'" 'fingertip-single-quote)      ;智能 '
  (keymap-set fingertip-mode-map "=" 'fingertip-equal)             ;智能 =
  (keymap-set fingertip-mode-map "SPC" 'fingertip-space)           ;智能 space
  (keymap-set fingertip-mode-map "RET" 'fingertip-newline)         ;智能 newline
  ;; 删除
  ;; ("M-o" . fingertip-backward-delete) ;向后删除
  ;; ("C-d" . fingertip-forward-delete)  ;向前删除
  ;; ("C-k" . fingertip-kill)            ;向前kill
  ;; 包围
  ;; ("M-\"" . fingertip-wrap-double-quote) ;用 " " 包围对象, 或跳出字符串
  ;; ("M-'" . fingertip-wrap-single-quote) ;用 ' ' 包围对象, 或跳出字符串
  ;; ("M-[" . fingertip-wrap-bracket)      ;用 [ ] 包围对象
  ;; ("M-{" . fingertip-wrap-curly)        ;用 { } 包围对象
  ;; ("M-(" . fingertip-wrap-round)        ;用 ( ) 包围对象
  ;; ("M-)" . fingertip-unwrap)            ;去掉包围对象
  ;; 跳出并换行缩进
  ;; ("M-:" . fingertip-jump-out-pair-and-newline) ;跳出括号并换行
  ;; 向父节点跳动
  ;; ("C-j" . fingertip-jump-up)
  )

;;; Helpful

(keymap-global-set "M-?" 'help-command)

(with-eval-after-load 'help
  (define-key global-map [remap describe-function] 'helpful-function)
  (define-key global-map [remap describe-key] 'helpful-key)
  (define-key global-map [remap describe-variable] 'helpful-variable)
  (define-key global-map [remap describe-command] 'helpful-command))

(with-eval-after-load 'init-func
  (define-key global-map [remap upcase-word] 'upcase-any)
  (define-key global-map [remap downcase-word] 'downcase-any)
  (define-key global-map [remap capitalize-word] 'capitalize-any))

(provide 'init-keybindings)
;;; init-keybindings.el ends here.
