;;; Init.el --- Emacs Configuration --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/benchmark-init-el")

(defvar windows-system-p
  (string-equal system-type "windows-nt")
  "Judge whether it's Windows system.")

;; (if windows-system-p
;;     (progn
;;       (require 'benchmark-init)
;;       (prefer-coding-system 'utf-8))
;;   (require 'benchmark-init-loaddefs)
;;   )

;; (require 'benchmark-init)

;; (benchmark-init/activate)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
(setq custom-file "~/.emacs.d/custom.el"
      load-prefer-newer t)
(load "~/.emacs.d/custom.el")

(let ((file-name-handler-alist nil))
  ;; (require 'package)
  ;; (package-initialize)

  ;; (setq package-archives
  ;;       '(("gnu"    .
  ;;          "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
  ;;         ("nongnu" .
  ;;          "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
  ;;         ("melpa"  .
  ;;          "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/"))
  ;;       ;; “Gnu”应该和“melpa”同优先级, 从而默认选取二者中较新的 package.
  ;;       package-archive-priorities '(("gnu"    . 1)
  ;;                                    ("nongnu" . 0)
  ;;                                    ("melpa"  . 1))
  ;;       package-menu-hide-low-priority t
  ;;       ;; 暂时不知道检查签名有什么用,先关了再说.
  ;;       package-check-signature nil
  ;;       eldoc-documentation-function 'eldoc-documentation-compose
  ;;       )

  ;;; Emacs Default Setting
  (load "~/.emacs.d/lisp/init-func.el")

  (let ((packages (find-subdir-recursively "~/.emacs.d/site-lisp")))
    (setq load-path (append load-path packages)))
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/treemacs/src/elisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/treemacs/src/extra/")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-mode/clients")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-mode/scripts")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-mode/docs")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/vertico/extensions")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/themes/themes")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/pdf-tools/lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/verilog-ext")
  (load "~/.emacs.d/site-lisp/loaddefs.el")
  (setq eat-kill-buffer-on-exit t
        css-indent-offset 2
        set-mark-command-repeat-pop t
        other-window-scroll-default 'get-lru-window
        backup-directory-alist '(("." . "~/.emacs.d/backup"))
        ispell-dictionary "en_US"
        ;; ispell-program-name "hunspell"
        ;; package-quickstart nil
        )

  ;;; @1. GC

  ;; (add-hook 'minibuffer-setup-hook 'gc-minibuffer-setup-hook)
  ;; (add-hook 'minibuffer-exit-hook 'gc-minibuffer-exit-hook)

  ;;; @2. flymake and flycheck

  (with-eval-after-load 'flycheck
    (flycheck-def-config-file-var flycheck-verilog-verilator-command-file verilog-verilator "commands.f")
    (flycheck-define-checker verilog-verilator
      "A Verilog syntax checker using the Verilator Verilog HDL simulator.

  See URL `https://www.veripool.org/wiki/verilator'."
      ;; https://verilator.org/guide/latest/exe_verilator.html
      ;;   The three flags -y, +incdir+<dir> and -I<dir> have similar effect;
      ;;   +incdir+<dir> and -y are fairly standard across Verilog tools while -I<dir> is used by many C++ compilers.
      :command ("verilator" "--lint-only" "-Wall" "-Wno-fatal" "--timing"
                "--bbox-unsup" ; Blackbox unsupported language features to avoid errors on verification sources
                "--bbox-sys"  ;  Blackbox unknown $system calls
                (option-list "-I" nil concat)
                (option-list "-I" nil concat)
                (config-file "-f" flycheck-verilog-verilator-command-file)
                (eval (remove buffer-file-name nil))
                (eval (remove buffer-file-name nil))
                source)
      :error-patterns
      ((warning line-start "%Warning-" (zero-or-more not-newline) ": " (file-name) ":" line ":" column ": " (message) line-end)
       (error   line-start "%Error: Internal Error: "                  (file-name) ":" line ":" column ": " (message) line-end)
       (error   line-start "%Error: "                                  (file-name) ":" line ":" column ": " (message) line-end)
       (error   line-start "%Error-"   (zero-or-more not-newline) ": " (file-name) ":" line ":" column ": " (message) line-end))
      :modes (verilog-mode verilog-ts-mode))
    (add-hook 'flycheck-mode-hook '(lambda ()
                                     (flycheck-set-indication-mode 'left-margin)))
    )

  (add-to-list 'load-path "~/.emacs.d/site-lisp/nerd-icons.el")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/treemacs-nerd-icons")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/nerd-icons-dired")

  (require 'nerd-icons)
  (setq nerd-icons-font-family "CaskaydiaCove Nerd Font")
  ;; (setq nerd-icons-font-family "PragmataProLiga Nerd Font Mono")

  (defface diagnostics-error
    '(
      (((background dark)) :background "#090c10" :foreground "#f85149")
      (((background light)) :foreground "#cb2431")
      )
    "Face for flymake Error."
    :group 'flymake)

  (defface diagnostics-warn
    '(
      (((background dark)) :background "#090c10" :foreground "#f0883e")
      (((background light)) :foreground "#bf8803")
      )
    "Face for flymake Warn."
    :group 'flymake)

  (defface diagnostics-info
    '((((background dark)) :background "#090c10" :foreground "#75beff" :color "#000000")
      (((background light)) :foreground "#1155ff" :color "white")
      )
    "Face for flymake Info."
    :group 'flymake)

  (setq flymake-no-changes-timeout 0.1
        flymake-indicator-type 'margins
        flymake-autoresize-margins t
        flymake-margin-indicators-string
        ;; `((error "​​​​󰅙" compilation-error)
        ;;   (warning "​​​​" compilation-warning)
        ;;   (note "​​​​" compilation-info))
        ;; `((error "​​​​󰅙​​​​" diagnostics-error)
        `((error "​​​󰅙​​​​​" diagnostics-error)
          ;; (warning "​​​​​​​​" diagnostics-warn)
          (warning "​​​​​​​​" diagnostics-warn)
          ;; (note "​​​​​​​​" diagnostics-info)
          (note "​​​​​​" diagnostics-info)
          )
        ;; `((error ,(nerd-icons-octicon "nf-oct-x_circle_fill") diagnostics-error)
        ;;   (warning ,(nerd-icons-faicon "nf-fa-warning") diagnostics-warn)
        ;;   (note ,(nerd-icons-faicon "nf-fa-info") diagnostics-info))
        flymake-show-diagnostics-at-end-of-line t
        )

  ;; (setq-default left-margin-width 1)

  ;; (with-eval-after-load 'flymake
  ;;   (defvar verilog--flymake-proc nil
  ;;     "A flymake verilog process.")

  ;;   (defvar verilog-flymake-command '("verilator" "--lint-only" "-Wall" "-Wno-fatal" "--timing"
  ;;                                      "--bbox-unsup" "--bbox-sys")
  ;;     "Command for verilog's flymake.")

  ;;   (defvar verilog--flymake-output-buffer " *stderr of verilog-flymake*"
  ;;     "Buffer for verilog's flymake output.")

  ;;   (defun verilog-flymake-done (report-fn
  ;;                                source-buffer
  ;;                                output-buffer)
  ;;     (with-current-buffer source-buffer
  ;;       (save-excursion
  ;;         (save-restriction
  ;;           (with-current-buffer output-buffer
  ;;             (goto-char (point-min))
  ;;             (let ((diags))
  ;;               (while (search-forward-regexp
  ;;                       "^\\(%.*\\): .*:\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
  ;;                       nil t)
  ;;                 (let* ((msg (match-string 4))
  ;;                        (level-msg (match-string 1))
  ;;                        (line (string-to-number (match-string 2)))
  ;;                        (locate-string)
  ;;                        (cal)
  ;;                        (beg)
  ;;                        (end)
  ;;                        (level))
  ;;                   (setq level (cond
  ;;                                ((string-match-p "%Error" level-msg) ':error)
  ;;                                ((string-match-p "%Warning" level-msg) ':warning)
  ;;                                (t :note)))
  ;;                   (search-forward-regexp "\\(\\^~*\\)" nil t)
  ;;                   (setq cal (length (match-string 1)))
  ;;                   (let ((current (point))
  ;;                         (line-beginning (line-beginning-position)))
  ;;                     (forward-line -1)
  ;;                     (forward-char (- current line-beginning))
  ;;                     (setq locate-string (buffer-substring-no-properties (- (point) cal) (point))))
  ;;                   (setq beg (with-current-buffer source-buffer
  ;;                               (save-excursion
  ;;                                 (save-restriction
  ;;                                   (goto-char (point-min))
  ;;                                   (or (equal line 1)
  ;;                                       (forward-line (- line 1)))
  ;;                                   (search-forward locate-string nil t)
  ;;                                   (- (point) cal)
  ;;                                   ))))
  ;;                   (setq end (+ beg cal))
  ;;                   (setq diags
  ;;                         (cons (flymake-make-diagnostic
  ;;                                source-buffer beg end level msg)
  ;;                               diags))
  ;;                   ))
  ;;               (funcall report-fn diags)
  ;;               )))))
  ;;     )

  ;;   (defun verilog-flymake-detect (report-fn &rest _args)
  ;;     "A Flymake backend for verilog.
  ;; Spawn an verilog lsp process that byte-compiles a file representing the
  ;; current buffer state and calls REPORT-FN when done."
  ;;     (when verilog--flymake-proc
  ;;       (when (process-live-p verilog--flymake-proc)
  ;;         (kill-process verilog--flymake-proc)))
  ;;     (let ((source-buffer (current-buffer))
  ;;           (coding-system-for-write 'utf-8-unix)
  ;;           (coding-system-for-read 'utf-8))
  ;;       (save-restriction
  ;;         (widen)
  ;;         (let* ((output-buffer (generate-new-buffer " *verilog-flymake*")))
  ;;           (setq verilog--flymake-proc
  ;;                 (make-process
  ;;                  :name "verilog-flymake-process"
  ;;                  :buffer output-buffer
  ;;                  :command (append verilog-flymake-command
  ;;                                   (list (buffer-file-name source-buffer)))
  ;;                  :connection-type 'pipe
  ;;                  :sentinel
  ;;                  (lambda (proc _event)
  ;;                    (unless (process-live-p proc)
  ;;                      (unwind-protect
  ;;                          (cond
  ;;                           ((not (and (buffer-live-p source-buffer)
  ;;                                      (eq proc (with-current-buffer source-buffer
  ;;                                                 verilog--flymake-proc))))
  ;;                            (flymake-log :warning
  ;;                                         "verilog-flymake process %s obsolete" proc))
  ;;                           ((memq (process-status proc) '(exit signal))
  ;;                            (verilog-flymake-done report-fn
  ;;                                                  source-buffer
  ;;                                                  verilog--flymake-output-buffer
  ;;                                                  ))
  ;;                           (t
  ;;                            (funcall report-fn
  ;;                                     :panic
  ;;                                     :explanation
  ;;                                     (format "process %s died" proc))))
  ;;                        (kill-buffer verilog--flymake-output-buffer)
  ;;                        )))
  ;;                  :stderr verilog--flymake-output-buffer
  ;;                  :noquery t))))))

  ;;   (defun verilog-setup-flymake-backend ()
  ;;     (add-hook 'flymake-diagnostic-functions 'verilog-flymake-detect nil t))

  ;;   ;; (add-hook 'verilog-mode-hook 'verilog-setup-flymake-backend)

  ;;   ;; (defun sanityinc/enable-flymake-flycheck ()
  ;;   ;;   (setq-local flymake-diagnostic-functions
  ;;   ;;               (seq-uniq (append flymake-diagnostic-functions
  ;;   ;;                                 (flymake-flycheck-all-chained-diagnostic-functions)))))
  ;;   )

  (dolist (hook '(prog-mode-hook))
    (add-hook hook 'flymake-mode))

  (require 'project)

  ;;; @3. ICONS

  ;; (load "~/.emacs.d/self-develop/modeline-setting.el")


  (with-eval-after-load 'treemacs
    (require 'treemacs-nerd-icons)
    (treemacs-load-theme "nerd-icons"))
  (add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode)

  (with-eval-after-load 'all-the-icons (load "~/.emacs.d/self-develop/all-the-icons-diy.el"))

  (load "~/.emacs.d/lisp/init-startup.el")

  ;;; @4. MEOW

  (require 'meow)
  (meow-setup)

  (setq meow-use-cursor-position-hack t
        meow-use-clipboard t
        meow-use-enhanced-selection-effect t
        meow--kbd-kill-region "M-w"
        meow--kbd-kill-ring-save "C-w")

  (meow-global-mode)

  ;; (setq xclip-method 'wl-copy
  ;;       xclip-program "wl-copy")
  ;; (xclip-mode)

  ;;; @5. KEYBINDINGS

  ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/combobulate")

  (keymap-global-set "M-p" 'pop-to-mark-command)

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
      (keymap-global-set "M-'" 'avy-goto-char-in-line)
      ;; (global-set-key (kbd "C-:") 'avy-goto-char)
      ;; (global-set-key (kbd "M-g c") 'avy-goto-char-timer)
      ;; (global-set-key (kbd "M-g w") 'avy-goto-word-1)
      ;; (global-set-key (kbd "M-g e") 'avy-goto-word-0)
      ;; (global-set-key (kbd "M-g f") 'avy-goto-line)
      ;; (global-set-key (kbd "C-c C-j") 'avy-resume)
      ))
  (keymap-global-set "M-'" 'avy-goto-char-in-line)
  (keymap-global-set "C-s" 'isearch-forward-regexp)
  (keymap-global-set "C-r" 'isearch-backward-regexp)
  (keymap-global-set "C-k" 'smart-kill-line)
  (keymap-global-set "C-w" 'kill-or-save)

  (keymap-set isearch-mode-map "C-h" 'isearch-del-char)
  (keymap-global-set "C-h" 'backward-delete-char-untabify)
  (keymap-global-set "C-x k" 'kill-current-buffer)
  (keymap-global-set "C-x C-r" 'restart-emacs)
  (keymap-global-set "C-c g" 'consult-ripgrep)
  (keymap-global-set "C-c f" 'consult-fd)
  ;; @ Efficiency
  (keymap-global-set "C-x f" 'find-file)
  (keymap-global-set "C-z" 'vundo)
  (global-set-key [remap comment-dwim] 'comment-or-uncomment)

  ;; @ Fingertip
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

  ;; @ Helpful

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

  ;;; @6. LSP

  (lsp-enable-startup)

  (with-eval-after-load 'corfu
    (setq corfu-auto t
          corfu-cycle t
          ;; corfu-quit-no-match 'separator  ;; t
          corfu-auto-prefix 2
          corfu-auto-delay 0
          corfu-preview-current t
          corfu-on-exact-match 'show
          ;; corfu-quit-no-match t
          ;; corfu-preselect 'prompt
          ;; corfu-quit-at-boundary t
          )
    (keymap-set corfu-map "<tab>" 'corfu-insert)
    ;; (keymap-set corfu-map "<backtab>" 'corfu-previous)
    ;; (keymap-set corfu-map "S-<return>" 'corfu-insert)
    ;; (keymap-unset corfu-map "RET")
    (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
    (with-eval-after-load 'corfu-popupinfo
      (setq corfu-popupinfo-delay '(0.1 . 0.1)))
    (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter))

  (yas-global-mode 1)
  (yas-minor-mode-on)

  (with-eval-after-load 'yasnippet
    (add-hook 'yas-keymap-disable-hook
              (lambda()
                (or
                 (when (boundp 'corfu--frame)
                   (and
                    (frame-live-p corfu--frame)
                    (frame-visible-p corfu--frame)))
                 (when (boundp 'acm-menu-frame)
                   (and (frame-live-p acm-menu-frame)
                        (frame-visible-p acm-menu-frame)))))))

  (with-eval-after-load 'tempel
    (defun tempel-setup-capf ()
      ;; Add the Tempel Capf to `completion-at-point-functions'.
      ;; `tempel-expand' only triggers on exact matches. Alternatively use
      ;; `tempel-complete' if you want to see all matches, but then you
      ;; should also configure `tempel-trigger-prefix', such that Tempel
      ;; does not trigger too often when you don't expect it. NOTE: We add
      ;; `tempel-expand' *before* the main programming mode Capf, such
      ;; that it will be tried first.
      (setq-local completion-at-point-functions
                  (cons #'tempel-complete
                        completion-at-point-functions)))
    (add-hook 'prog-mode-hook 'tempel-setup-capf)
    (add-hook 'text-mode-hook 'tempel-setup-capf))

  (require 'cape)
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)

  (require 'company)
  (with-eval-after-load 'eglot
    (setq eglot-send-changes-idle-time 0
          eglot-code-action-indications '(eglot-hint))
    (add-to-list 'eglot-server-programs
                 '((tex-mode context-mode texinfo-mode bibtex-mode)
                   . ("texlab")))
    ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (defun my/eglot-capf ()
      ;; (setq-local completion-at-point-functions
      ;;             (list (cape-capf-super
      ;;                    #'eglot-completion-at-point
      ;;                    ;; #'tempel-complete
      ;;                    ;; (mapcar #'cape-company-to-capf
      ;;                    ;;         (list #'company-yasnippet))
      ;;                    (cape-company-to-capf #'company-yasnippet)
      ;;                    #'cape-file)))
      ;; (let* ((temp-functions '()))
      ;;   (dolist (fun completion-at-point-functions)
      ;;     (when (not (eq fun 't))
      ;;       (add-to-list 'temp-functions fun)))
      ;;   (add-to-list 'temp-functions (cape-company-to-capf #'company-yasnippet))
      ;;   (add-to-list 'temp-functions (cape-company-to-capf #'company-files))
      ;;   (setq-local completion-at-point-functions (list (apply #'cape-capf-super temp-functions)))
      ;; (setq temp-functions (append (list (cape-capf-super
      ;;                                     (cape-company-to-capf #'company-yasnippet)
      ;;                                     (cape-company-to-capf #'company-files)
      ;;                                     ))
      ;;                              (reverse temp-functions)))
      (setq-local completion-at-point-functions (list (cape-capf-super
                                                       #'eglot-completion-at-point
                                                       (cape-company-to-capf #'company-yasnippet)
                                                       (cape-company-to-capf #'company-files))))
      ;; (setq-local completion-at-point-functions
      ;;             (append (list
      ;;                      ;; #'tempel-complete
      ;;                      ;; (mapcar #'cape-company-to-capf
      ;;                      ;;         (list #'company-yasnippet))
      ;;                      (cape-capf-super
      ;;                       (cape-company-to-capf #'company-yasnippet)
      ;;                       (cape-company-to-capf #'company-files)
      ;;                       #'cape-file))
      ;;                     completion-at-point-functions))
      )
    (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
    (eglot-booster-mode)
    )

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              ;; (add-to-list 'completion-at-point-functions #'cape-tex)
              (setq-local completion-at-point-functions
                          (append (list #'cape-tex) completion-at-point-functions))
              ))

  (with-eval-after-load 'lsp-bridge
    (add-hook 'lsp-bridge-mode-hook 'yas/minor-mode)
    (add-hook 'c++-ts-mode-hook
              (lambda ()
                (setq-local lsp-bridge-inlay-hint-overlays t)
                ))
    (keymap-set yas-keymap "<tab>" 'acm-complete-or-expand-yas-snippet)
    (setq ;; acm-candidate-match-function 'orderless-flex
     ;; acm-enable-icon t
     ;; acm-enable-doc t
     acm-enable-yas t
     acm-enable-tempel t
     acm-enable-quick-access nil
     acm-enable-search-file-words t
     acm-enable-telega nil
     acm-enable-tabnine nil
     acm-enable-citre t
     acm-enable-capf t
     ;; lsp-bridge-enable-log t
     lsp-bridge-enable-signature-help t
     lsp-bridge-enable-inlay-hint t
     lsp-bridge-enable-diagnostics nil
     lsp-bridge-complete-manually nil
     ;; lsp-bridge-enable-profile t
     ;; lsp-bridge-multi-lang-server-mode-list nil
     acm-backend-lsp-candidate-min-length 2
     acm-backend-elisp-candidate-min-length 2
     acm-backend-search-file-words-candidate-min-length 3
     acm-backend-yas-candidate-min-length 1
     ;; lsp-bridge-python-command "python3"
     ;; This will cause `org-roam-node-find' went wrong and I don't know why.
     ;; lsp-bridge-enable-org-babel t
     ;; lsp-bridge-c-lsp-server "clangd"
     )
    )

  ;;; @7. DIRED

  (setq dired-mouse-drag-files t
        mouse-drag-and-drop-region-cross-program t
        delete-by-moving-to-trash t
        dired-movement-style 'cycle
        dired-omit-mode t
        )
  (add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish/extensions")
  ;; (add-hook 'dired-mode-hook
  ;;           (lambda ()
  ;;             (or (boundp 'diredfl-mode)
  ;;                 (load "~/.emacs.d/site-lisp/diredfl/diredfl.el"))
  ;;             (toggle-truncate-lines)
  ;;             (diredfl-mode)
  ;;             ;; (require 'image-dired)
  ;;             ;; (require 'dirvish)
  ;;             ))
  ;; (with-eval-after-load 'dired
  ;;   (setq dired-listing-switches
  ;;         "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group"
  ;;         dired-dwim-target t
  ;;         dired-mouse-drag-files t
  ;;         dired-auto-revert-buffer t
  ;;         dired-do-revert-buffer t
  ;;         mouse-drag-and-drop-region-cross-program t
  ;;         dired-kill-when-opening-new-dired-buffer t
  ;;         dired-recursive-copies 'always
  ;;         ;; dired-recursive-deletes 'always
  ;;         image-dired-thumb-size 256
  ;;         image-dired-marking-shows-next nil)
  ;;   (defun dired-open-externally (&optional arg)
  ;;     "Open marked or current file in operating system's default application."
  ;;     (interactive "P")
  ;;     (dired-map-over-marks
  ;;      (embark-open-externally (dired-get-filename))
  ;;      arg))
  ;;   (keymap-set dired-mode-map "e" 'dired-open-externally))
  (dirvish-override-dired-mode)

  (with-eval-after-load 'dirvish
    ;;   ;; (dirvish-peek-mode)
    ;;   ;; (require 'dirvish-side)
    ;;   (dirvish-override-dired-mode)
    ;;   (dirvish-side-follow-mode)
    ;;   ;; (add-hook 'dirvish-setup-hook 'dirvish-emerge-mode)
    (setq dirvish-attributes '(vc-state nerd-icons git-msg file-size subtree-state collapse file-time)
          dirvish-side-attributes '(vc-state nerd-icons subtree-state collapse)
          ;;         dirvish-emerge-groups '(("Recent files" (predicate . recent-files-2h))
          ;;                                 ("Video" (extensions "mp4" "mkv" "webm"))
          ;;                                 ("Pictures" (extensions "jpg" "png" "jpeg" "svg" "gif"))
          ;;                                 ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
          ;;                                 ("Archives" (extensions "gz" "rar" "zip")))
          ;;         dirvish-path-separators '(" ~" " /" "/")
          ;;         ;; dirvish-hide-details nil
          ;;         dirvish-mode-line-height 20
          ;;         ;; dirvish-show-media-properties t
          ;;         ;; Turn off media cache, but it will slow down the speed of media preview
          ;;         dirvish-media-auto-cache-threshold nil
          ;;         ;; dirvish-preview-dispatch (remove 'epub dirvish-preview-dispatch)
          )
    (keymap-set dirvish-mode-map "TAB" #'dirvish-toggle-subtree)
    )

  (unless (bound-and-true-p dirvish-override-dired-mode)
    (add-hook 'dired-mode-hook 'nerd-icons-dired-mode))

  ;; @8. CHINESE

  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation")
  (setq cns-prog "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cnws"
        cns-dict-directory "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cppjieba/dict"
        cns-recent-segmentation-limit 20
        cns-debug nil)

  (require 'cns nil t)
  (when (featurep 'cns)
    (add-hook 'find-file-hook 'cns-auto-enable))

  ;; @9. INPUT

  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-rime")

  (with-eval-after-load 'rime
    (set-face-attribute 'rime-default-face nil :height 1.2)
    (set-face-attribute 'rime-highlight-candidate-face nil :height 1.2)
    (set-face-attribute 'rime-preedit-face nil :underline t
                        :inverse-video 'unspecified)
    (defun rime-predicate-meow-mode-p ()
      "Detect whether the current buffer is in `meow' state.

  Include `meow-normal-state' , `meow-motion-state'.

  Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
      (and (fboundp 'meow-mode)
           (or (meow-normal-mode-p)
               (meow-beacon-mode-p))))
    (defun rime-predicate-tex-advance-p ()
      "If point is inside a (La)TeX math environment, or a (La)TeX command."
      (if (derived-mode-p 'tex-mode)
          (or (and (featurep 'tex-site)
                   (texmathp))
              (and rime--current-input-key
                   (or (= #x24 rime--current-input-key)
                       (= #x5c rime--current-input-key))
                   (or (= (point) (line-beginning-position))
                       (= #xff0c (char-before))
                       (= #x3001 (char-before))
                       (= #x3002 (char-before))
                       (= #xff08 (char-before))
                       (= #xff1b (char-before))
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
    (keymap-set rime-mode-map "s-`" 'rime-send-keybinding)
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
          rime-disable-predicates
          '(rime-predicate-space-after-cc-p
            rime-predicate-current-uppercase-letter-p
            ;; rime-predicate-after-alphabet-char-p
            ;; rime-predicate-after-ascii-char-p
            rime-predicate-prog-in-code-p
            rime-predicate-hydra-p
            ;; rime-predicate-evil-mode-p
            rime-predicate-meow-mode-p
            rime-predicate-tex-advance-p
            )
          ;; rime-deactivate-when-exit-minibuffer t
          rime-inline-ascii-trigger 'shift-l
          )
    (keymap-set rime-mode-map "M-o" 'rime-force-enable)
    ;; (with-eval-after-load 'tex
    ;;   (add-to-list 'rime-disable-predicates
    ;;                'rime-predicate-tex-advance-p))
    )

  (defun rime-commit1-and-toggle-input-method ()
    "Commit the 1st item if exists, then toggle input method."
    (interactive)
    (require 'rime)
    (ignore-errors (rime-commit1))
    (toggle-input-method))

  (keymap-global-set "C-\\" 'rime-commit1-and-toggle-input-method)

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

  ;; @10. EAF

  ;; (require 'eaf)

  (with-eval-after-load 'eaf
    (require 'eaf-pdf-viewer)
    (setq eaf-pdf-show-progress-on-page nil))

  ;; @11. ORG

  (add-to-list 'load-path "~/.emacs.d/site-lisp/org-roam")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/org-modern-indent")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/org-modern")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/org-appear")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/org-bars")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacsql")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/org-visual-outline")

  ;; Hide spaces of chinese inline block
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
  ;;                            (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
  ;;                           ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
  ;;                            (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
  ;;                         'append)

  (with-eval-after-load 'org
    (global-org-modern-mode)
    (setq org-hide-emphasis-markers t
          org-pretty-entities t
          prettify-symbols-mode t
          prettify-symbols-unprettify-at-point 'right-edge
          org-image-actual-width nil
          ;; org-todo-keywords '((sequence "     " "     "))
          ;; org-preview-latex-process-alist '((dvipng :programs
          ;;                                           ("latex" "dvipng")
          ;;                                           :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
          ;;                                           (1.0 . 1.0)
          ;;                                           :latex-compiler
          ;;                                           ("latex -interaction nonstopmode -output-directory %o %f")
          ;;                                           :image-converter
          ;;                                           ("dvipng -D %D -T tight -o %O %f")
          ;;                                           :transparent-image-converter
          ;;                                           ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
          ;;                                   (dvisvgm :programs
          ;;                                            ("latex" "dvisvgm")
          ;;                                            :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
          ;;                                            (1.0 . 1.0)
          ;;                                            :latex-compiler
          ;;                                            ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
          ;;                                            :image-converter
          ;;                                            ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
          ;;                                   (imagemagick :programs
          ;;                                                ("latex" "convert")
          ;;                                                :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
          ;;                                                (1.0 . 1.0)
          ;;                                                :latex-compiler
          ;;                                                ("pdflatex -interaction nonstopmode -output-directory %o %f")
          ;;                                                :image-converter
          ;;                                                ("convert -density %D -trim -antialias %f -quality 100 %O")))
          ;; org-preview-latex-default-process 'dvisvgm
          org-format-latex-header "\\documentclass[10pt]{article}\n\\usepackage[usenames]{color}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\pagestyle{empty}  % do not remove\n% The settings below are copied from fullpage.sty\n
          \\usepackage{xeCJK,tikz,caption,float,makecell,circuitikz,array}\n
          \\usetikzlibrary{shapes,arrows,calc,arrows.meta}\n
          \\usetikzlibrary{circuits.logic.IEC,calc}\n
          \\renewcommand{\\arraystretch}{1.3}\n
          \\setlength{\\textwidth}{\\paperwidth}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}\n"
          )
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       ))
    (keymap-set org-mode-map "C-c b" 'org-cite-insert))

  (add-hook 'org-mode-hook
            (lambda ()
              (electric-indent-local-mode)
              (setq-local company-backends '(company-files company-keywords))
              (setq org-appear-autolinks t)
              (org-appear-mode)
              (org-cdlatex-mode)
              (custom-theme-set-faces
               'user
               ;;  ;; '(fixed-pitch ((t (:family "Input Mono" :height 0.9))))
               ;;  ;; '(variable-pitch ((t (:family "Palatino Linotype" :height 1.0))))
               '(org-block ((t (:inherit fixed-pitch))))
               '(org-code ((((background light))
                            (:foreground "#1F2328"
                                         :background "#EFF1F3"
                                         :inherit fixed-pitch))
                           (((background dark)) (:inherit fixed-pitch))))
               ;; '(org-table ((t (:inherit variable-pitch))))
               '(org-special-keyword ((t (:inherit fixed-pitch))))
               '(org-verbatim
                 ((((background light))
                   (:foreground "#e74c3c"
                                :box (:line-width 1 :color "#e1e4e5")
                                :inherit fixed-pitch))
                  (((background dark))
                   (:background "#343942"
                                :foreground "#E6EDF3"
                                :inherit fixed-pitch))))
               '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
               '(org-block-begin-line ((t (:inherit fixed-pitch))))
               '(org-block-end-line ((t (:inherit fixed-pitch))))
               '(fill-column-indicator ((t (:inherit (shadow fixed-pitch))))))
              (variable-pitch-mode)
              ;; (company-mode)
              ;; (corfu-mode)
              (visual-line-mode)
              (valign-mode)))

  ;; (with-eval-after-load 'org
  ;; (defun org-buffer-face-mode-variable ()
  ;; (interactive)
  ;; (make-face 'width-font-face)
  ;; (set-face-attribute 'width-font-face nil :font (font-spec :name "LXGW WenKai Mono";; :name "Sarasa Mono SC"
  ;; ;; :weight 'semibold
  ;; :size 13.0))  ;; 等距更纱黑体
  ;; (setq buffer-face-mode-face 'width-font-face)
  ;; (buffer-face-mode))
  ;; (add-hook 'org-mode-hook 'org-buffer-face-mode-variable)

  ;; (setq org-hide-emphasis-markers t))

  ;; Org-superstar
  ;; (add-hook 'org-mode-hook 'org-superstar-mode)

  ;; Org-modern

  (with-eval-after-load 'org-modern
    (setq org-modern-todo t
          org-modern-table nil
          org-modern-tag t
          org-modern-priority t
          org-modern-keyword t
          org-modern-block-name t
          org-modern-horizontal-rule t
          org-modern-statistics t
          org-modern-timestamp t
          ;; org-modern-hide-stars t
          org-modern-checkbox nil
          ;; org-modern-star nil
          org-modern-list
          '(
            ;; (?- . "-")
            (?* . "•")
            (?+ . "‣"))))

  ;; (add-hook 'org-agenda-finalize-hook 'org-modern-agenda)

  ;; (require 'org-bars)
  ;; (add-hook 'org-mode-hook #'org-bars-mode)

  ;; (add-hook 'org-mode-hook 'word-wrap-whitespace-mode)

  (setq-default org-startup-folded 'overview
                org-startup-with-inline-images t
                org-startup-indented t)
  ;; (setq-default org-highlight-latex-and-related '(native latex script entities))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local time-stamp-active t
                          time-stamp-start "#\\+MODIFIED: [ \t]*"
                          time-stamp-end "$"
                          time-stamp-format "\[%Y-%m-%d %3a %H:%M\]")
              (setq org-list-allow-alphabetical t)
              (add-hook 'before-save-hook 'time-stamp nil 'local)))

  (add-hook 'org-mode-hook #'org-modern-indent-mode 100)

  (with-eval-after-load 'denote
    (setq denote-directory "/run/media/kunh/Elements/Personal/denote"))

  (with-eval-after-load 'org-noter
    (setq org-noter-notes-search-path '("/run/media/kunh/Elements/Personal/denote")
          org-noter-auto-save-last-location t)
    )

  (keymap-global-set "C-c n n" 'org-noter)
  (keymap-global-set "C-c n f" 'org-roam-node-find)
  (setq org-roam-directory "/run/media/kunh/Elements/Personal/org-roam"    ; 设置 org-roam 笔记的默认目录，缺省值 /home/leo/org-roam
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-mode-sections '(org-roam-backlinks-section
                                 org-roam-reflinks-section
                                 org-roam-unlinked-references-section))

  ;; (add-to-list 'display-buffer-alist
  ;;        '("\\*org-roam*\\*"
  ;;          (display-buffer-in-side-window)
  ;;          (side . right)
  ;;          (window-width . 0.15)))

  ;; (with-eval-after-load 'org-roam
  ;;   Auto toggle org-roam-buffer.
  ;;   (defun my/org-roam-buffer-show (_)
  ;;   (if (and
  ;;      Don't do anything if we're in the minibuffer or in the calendar
  ;;      (not (minibufferp))
  ;;      (not (> 120 (frame-width)))
  ;;      (not (bound-and-true-p olivetti-mode))
  ;;      (not (derived-mode-p 'calendar-mode))
  ;;      Show org-roam buffer iff the current buffer has a org-roam file
  ;;      (xor (org-roam-file-p) (eq 'visible (org-roam-buffer--visibility))))
  ;;     (org-roam-buffer-toggle)))
  ;;   (add-hook 'window-buffer-change-functions 'my/org-roam-buffer-show))

  (with-eval-after-load 'org-roam
    (add-hook 'org-roam-mode-hook (lambda ()
                                    ;; (turn-on-visual-line-mode)
                                    (word-wrap-whitespace-mode)))
    (setq org-roam-database-connector 'sqlite-builtin
          org-roam-node-display-template
          (concat "${title:*} "
                  (propertize "${tags:10}" 'face 'org-tag))
          org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n")
             :unnarrowed t)
            ("b" "Books" plain "* Related Information\n\nAuthor: %^{Author}\nVersion: %^{Version}\n\n* Notes\n%?"
             :target (file+head "books/${slug}.org"
                                "#+TITLE: ${title}\n#+FILETAGS: %^{}\n#+CREATED: %U\n#+MODIFIED: \n\n")
             :unnarrowed t)
            ("t" "Trifles" entry "* Notes:\n%?"
             :target (file+head "Trifles/${slug}.org"
                                "#+TITLE: ${title}\n#+FILETAGS: %^g\n#+CREATED: %U\n#+MODIFIED: \n\n")
             :unnarrowed t)
            ("p" "Programming" entry "* Notes:\n%?"
             :target (file+head "Programming/${slug}.org"
                                "#+TITLE: ${title}\n#+FILETAGS: %^g\n#+CREATED: %U\n#+MODIFIED: \n\n")
             :unnarrowed t)
            ("k" "Knowledge" entry "* Notes:\n%?"
             :target (file+head "Knowledge/${slug}.org"
                                "#+TITLE: ${title}\n#+FILETAGS: %^g\n#+CREATED: %U\n#+MODIFIED: \n\n")
             :unnarrowed t)))
    (org-roam-db-autosync-mode))

    ;;; @12. VERILOG

  (add-hook 'verilog-ts-mode-hook
            (lambda ()
              (require 'eglot)
              (setq indent-bars-spacing-override verilog-ts-indent-level
                    verilog-auto-endcomments nil
                    verilog-case-indent 4
                    verilog-indent-level 4
                    verilog-indent-level-module 4
                    verilog-indent-level-behavioral 4
                    verilog-indent-level-declaration 4
                    verilog-cexp-indent 4
                    verilog-auto-newline nil
                    )
              (indent-bars-reset)))

    ;;; @13. READER

  (with-eval-after-load 'pdf-tools
    (setq pdf-view-use-scaling t
          pdf-view-continuous t
          pdf-anot-list-format '((page . 3)
                                 (type . 10)
                                 (contents . 50)
                                 (date . 24)))
    (pdf-tools-install))

  ;; (require 'calibredb)

  (add-to-list 'load-path "~/.emacs.d/site-lisp/nov-xwidget")
  ;; (require 'nov-xwidget)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
  ;; ;; (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  ;; (evil-define-key 'normal nov-mode-map (kbd "o") 'nov-xwidget-view)
  ;; (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
  ;; (add-hook 'nov-xwidget-webkit-mode-hook '(lambda() (xwidget-webkit-zoom (xwidget-webkit-current-session) 1.5)))
  ;; (setq visual-fill-column-center-text t)
  (setq typescript-ts-mode-indent-offset 4
        css-indent-offset 4)
  (with-eval-after-load 'nov
    (setq nov-text-width t)
    (add-hook 'nov-mode-hook 'visual-line-mode)
    (add-hook 'nov-mode-hook 'visual-fill-column-mode)
    (add-hook 'nov-mode-hook '(lambda() (set-fill-column 100)))
    )

    ;;; @14. HYDRA

  (add-to-list 'load-path "~/.emacs.d/site-lisp/hydra")
  (require 'hydra)

  (defhydra hydra-avy (global-map "M-g" :exit t :hint nil)
    "
   Line^^       Region^^        Goto
  ----------------------------------------------------------
   [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
   [_m_] move   [_M_] move      [_w_] word        [_W_] any word
   [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
    ("c" avy-goto-char-timer)
    ("C" avy-goto-char)
    ("w" avy-goto-word-1)
    ("W" avy-goto-word-0)
    ("l" avy-goto-line)
    ("L" avy-goto-end-of-line)
    ("m" avy-move-line)
    ("M" avy-move-region)
    ("k" avy-kill-whole-line)
    ("K" avy-kill-region)
    ("y" avy-copy-line)
    ("Y" avy-copy-region))

  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")
    ("r" (text-scale-set 0) "reset")
    ("q" nil "quit"))

  (defhydra hydra-window (:color pink)
    "
         Split:                   Move:
    ╭──────────────────────╯ ╭──────────────────────╯
       _v_ vertical            _j_ down
       _h_ horizontal          _k_ up
       _V_ even vertical       _J_ swap down
       _H_ even horizontal     _K_ swap up
       _s_ swap                _L_ swap right
    ╰──────────────────────╮ _l_ right
       _D_lt   _d_lt all         _o_nly this
       _B_ur   _b_ur all         _a_ce  this
       _m_inimize              _z_en
       _q_uit                  _f_ullscreen
    "
    ("<left>" windmove-left)
    ("<down>" windmove-down)
    ("<up>" windmove-up)
    ("<right>" windmove-right)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("o" delete-other-windows)
    ("a" ace-swap-window)
    ("D" delete-window :color blue)
    ("B" balance-windows)
    ("s" ace-swap-window)
    ("m" minimize-window)
    ("z" toggle-maximize-buffer)
    ("f" toggle-frame-fullscreen)
    ("v" split-window-right)
    ("h" split-window-below)
    ("V" split-window-right-and-focus)
    ("H" split-window-below-and-focus)
    ("d" kill-buffer-and-window)
    ("b" kill-buffer-and-window)
    ("L" transpose-frame)
    ("K" buf-move-up)
    ("J" buf-move-down)
    ("q" nil :color blue))

  (global-set-key (kbd "C-c w") 'hydra-window/body)

  (defhydra hydra-eMove ()
    "
   Line^^           char^^              word^^              Page^^
  ---------------------------------------------------------------------
   [_j_] next       [_l_] forward       [_F_] Forward         [_v_] up
   [_k_] previous   [_h_] backward      [_B_] Backward        [_V_] down
   "
    ("j" next-line nil)
    ("k" previous-line nil)
    ("v" scroll-uppp nil)
    ("V" scroll-down nil)
    ("l" forward-char nil)
    ("h" backward-char nil)
    ("F" forward-word nil)
    ("B" backward-word nil)
    ("q" nil "quit")
    )

  (keymap-global-set "C-M-;" 'hydra-eMove/body)

  ;; (require 'rect)

  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode t)
                                       :color pink
                                       :post (deactivate-mark))
    "
    ^_k_^     _d_elete    _s_tring
  _h_   _l_   _o_k        _y_ank
    ^_j_^     _n_ew-copy  _r_eset
  ^^^^        _e_xchange  _u_ndo
  ^^^^        ^ ^         _p_aste
  "
    ("h" rectangle-backward-char nil)
    ("l" rectangle-forward-char nil)
    ("k" rectangle-previous-line nil)
    ("j" rectangle-next-line nil)
    ("e" hydra-ex-point-mark nil)
    ("n" copy-rectangle-as-kill nil)
    ("d" delete-rectangle nil)
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode t)) nil)
    ("y" yank-rectangle nil)
    ("u" undo nil)
    ("s" string-rectangle nil)
    ("p" kill-rectangle nil)
    ("o" nil nil))

  (keymap-global-set "C-x SPC" 'hydra-rectangle/body)

    ;;; @15. LATEX-NODE
  (load "~/.emacs.d/lisp/latex-node.el")

  ;;; @16. LATEX
  ;; On demand loading, leads to faster startup time.
  (with-eval-after-load 'citar
    (setq org-cite-global-bibliography '("/run/media/kunh/Elements/Zotero Bib/My Library.bib")
          citar-notes-paths '("/run/media/kunh/Elements/Personal/denote")
          citar-library-paths '("/run/media/kunh/Elements/Zotero Library")
          org-cite-insert-processor 'citar
          org-cite-follow-processor 'citar
          org-cite-activate-processor 'citar
          citar-bibliography org-cite-global-bibliography)
    (citar-embark-mode)
    (citar-denote-mode)
    )
  ;; (add-hook 'LaTeX-mode-hook 'citar-capf-setup)
  (add-hook 'org-mode-hook 'citar-capf-setup)
  ;; (keymap-global-set "C-c c c" 'citar-create-note)
  (keymap-global-set "C-c c n" 'citar-denote-open-note)
  (keymap-global-set "C-c c d" 'citar-denote-dwim)
  (keymap-global-set "C-c c e" 'citar-denote-open-reference-entry)
  (keymap-global-set "C-c c a" 'citar-denote-add-citekey)
  (keymap-global-set "C-c c k" 'citar-denote-remove-citekey)
  (keymap-global-set "C-c c r" 'citar-denote-find-reference)
  (keymap-global-set "C-c c l" 'citar-denote-link-reference)
  (keymap-global-set "C-c c f" 'citar-denote-find-citation)
  (keymap-global-set "C-c c x" 'citar-denote-nocite)
  (keymap-global-set "C-c c y" 'citar-denote-cite-nocite)
  (keymap-global-set "C-c c z" 'citar-denote-nobib)

  (pdf-loader-install)

  (eval-after-load "tex-mode"
    '(progn
       (load "auctex.el" nil t t)
       (load "preview-latex.el" nil t t)
       ;; (require 'org-table)
       ))

  (defun orgtbl-next-field-maybe ()
    "Combine `lsp-bridge-mode', `cdlatex-mode' and `orgtlr-mode'."
    (interactive)
    (if (and (bound-and-true-p lsp-bridge-mode)
             (acm-frame-visible-p acm-menu-frame))
        (acm-complete)
      (if (bound-and-true-p cdlatex-mode)
          (cdlatex-tab)
        (org-table-next-field))))

  (with-eval-after-load 'tex
    ;; (load "auctex.el" nil t t)
    ;; (load "preview-latex.el" nil t t)
    (add-hook 'cdlatex-tab-hook
              (lambda ()
                (and (bound-and-true-p lsp-bidge-mode)
                     (acm-frame-visible-p acm-menu-frame))))
    ;; (load "~/.emacs.d/lisp/auctex-latexmk.el")
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-fold-auto t
          TeX-expand-list '(("%x" TeX-active-master-with-quotes "xdv" t))
          preview-image-type 'dvipng
          ;; preview-scale 1.5
          ;; preview-pdf-color-adjust-method nil
          cdlatex-paired-parens "$[{("
          )
    (setq-default TeX-master t
                  TeX-engine 'xetex
                  ;; preview-scale-function 0.6
                  ;; preview-LaTeX-command '("%`%l -no-pdf \"\\nonstopmode\\nofiles\
                  ;; \\PassOptionsToPackage{" ("," . preview-required-option-list) "}{preview}\
                  ;; \\AtBeginDocument{\\ifx\\ifPreview\\undefined"
                  ;; preview-default-preamble "\\fi}\"%' \"\\detokenize{\" %(t-filename-only) \"}\"")
                  ;; preview-dvipng-command "dvipng -picky -noghostscript %x -o %m/prev%%03d.png"
                  )
    ;; (add-to-list 'TeX-view-program-list '("sioyek" "sioyek --page %(outpage) %o"))
    ;; (add-to-list 'TeX-view-program-selection '(output-pdf "Sioyek"))
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
    (with-eval-after-load 'eaf
      (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
      (add-to-list 'TeX-view-program-selection '(output-pdf "eaf")))
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex -shell-escape --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
    (add-to-list 'texmathp-tex-commands1 '("lstlisting" env-off))
    )

  (setq adaptive-wrap-extra-indent 0)

  (dolist (hook '(LaTeX-mode-hook tex-mode-hook))
    (add-hook hook
              (lambda()
                ;; (auctex-latexmk-setup)
                ;; (electric-indent-local-mode)
                (electric-indent-mode -1)
                (adaptive-wrap-prefix-mode)
                (setq TeX-command-default "XeLaTeX")
                ;; (TeX-global-PDF-mode)
                (TeX-PDF-mode-on)
                (TeX-fold-mode 1)
                (turn-on-cdlatex)
                (turn-on-reftex)
                (visual-line-mode)
                ;; (keymap-set cdlatex-mode-map "<tab>" 'cdlatex-tab-maybe)
                ;; (turn-on-orgtbl)
                ;; (keymap-set orgtbl-mode-map "<tab>" 'orgtbl-next-field-maybe)
                )))

  ;;; @17. BASE
  ;; (add-hook 'emacs-startup-hook
  ;;           (lambda () (setq gc-cons-threshold better-gc-cons-threshold)))

  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  (electric-pair-mode)
  (pixel-scroll-precision-mode)
  (setq pixel-scroll-precision-interpolate-page t)
  (defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
  (defalias 'scroll-down-command 'pixel-scroll-interpolate-up)
  (global-subword-mode)
  (global-auto-revert-mode)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-interval 1)

  (when (eq system-type 'gnu/linux)
    (with-eval-after-load 'info
      (add-to-list 'Info-directory-list "/usr/local/texlive/2024/texmf-dist/doc/info")
      (add-to-list 'Info-directory-list "/usr/local/share/info")))


  (setq hl-line-sticky-flag nil
        hl-line-overlay nil)
  (global-hl-line-mode)
  (dolist
      (hook
       '(eshell-mode-hook shell-mode-hook term-mode-hook
                          messages-buffer-mode-hook
                          eat-mode-hook))
    (add-hook hook (lambda ()
                     (setq-local global-hl-line-mode nil))))

  (setq electric-pair-inhibit-predicate
        'electric-pair-conservative-inhibit
        scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively 97
        eldoc-idle-delay 0.2)

  (delete-selection-mode)

  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-delay 0
        show-paren-context-when-offscreen 'child-frame
        blink-matching-paren-highlight-offscreen t
        show-paren--context-child-frame-parameters
        '((visibility) (width . 0) (height . 0) (min-width . t)
          (min-height . t) (no-accept-focus . t) (no-focus-on-map . t)
          (border-width . 0) (child-frame-border-width . 1) (left-fringe . 0)
          (right-fringe . 0) (vertical-scroll-bars) (horizontal-scroll-bars)
          (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
          (no-other-frame . t) (no-other-window . t)
          (no-delete-other-windows . t) (unsplittable . t) (undecorated . t)
          (cursor-type) (no-special-glyphs . t) (desktop-dont-save . t)
          (child-frame-border-width 3)))

  (with-eval-after-load 'auto-save
    (setq auto-save-delete-trailing-whitespace t
          auto-save-disable-predicates
          '((lambda ()
              (string-suffix-p
               "gpg"
               (file-name-extension (buffer-name)) t)))))

  (setq
   ;; auto-save-timeout 30
   ;; auto-save-interval 10
   auto-save-no-message t
   )

  (setq auto-save-visited-interval 1)
  (auto-save-visited-mode)

  (add-hook 'before-save-hook
            (lambda ()
              (delete-trailing-whitespace
               (point-min)
               (- (line-beginning-position) 1))
              (delete-trailing-whitespace
               (+ (point) 1)
               (point-max))))

  (dolist (hook '(TeX-mode-hook dired-mode-hook))
    (add-hook hook 'toggle-truncate-lines))

  ;; (add-hook 'emacs-startup-hook ;; 'after-init-hook
  ;;           (lambda ()
  ;;             (unless (bound-and-true-p recentf-mode)
  ;;               (recentf-mode t)
  ;;               (setq recentf-max-saved-items 1000
  ;;                 recentf-exclude `("/tmp/" "/ssh:"
  ;;                                       ,(concat user-emacs-directory
  ;;                                                "lib/.*-autoloads\\.el\\'"))))
  ;;             (unless (boundp 'no-littering-etc-directory)
  ;;               ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/no-littering")
  ;;               ;; (rquire 'no-littering)
  ;;               (load "~/.emacs.d/site-lisp/no-littering/no-littering.el")
  ;;               (with-eval-after-load 'no-littering
  ;;               (add-to-list 'recentf-exclude no-littering-var-directory)
  ;;               (add-to-list 'recentf-exclude no-littering-etc-directory)))
  ;;             ;; (fset 'yes-or-no-p 'y-or-n-p)
  ;;             (unless (bound-and-true-p save-place-mode)
  ;;               (save-place-mode t))
  ;;             (unless (bound-and-true-p savehist-mode)
  ;;               (setq history-length 10000
  ;;                 history-delete-duplicates t
  ;;                 savehist-save-minibuffer-history t)
  ;;               (savehist-mode t))))


  ;; (rquire 'no-littering)
  (repeat-mode)
  (load "~/.emacs.d/site-lisp/no-littering/no-littering.el")
  (with-eval-after-load 'no-littering
    (recentf-mode t)
    (setq recentf-max-saved-items 1000
          recentf-exclude `("/tmp/" "/ssh:"
                            ,(concat user-emacs-directory
                                     "lib/.*-autoloads\\.el\\'"))
          yas-snippet-dirs (list (expand-file-name "~/.emacs.d/snippets")))
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (require 'saveplace-pdf-view)
  (save-place-mode t)
  (setq history-length 10000
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (savehist-mode t)

  (setq recentf-max-menu-items 5
        ring-bell-function 'ignore
        isearch-lazy-count t
        isearch-wrap-pause 'no
        lazy-highlight-cleanup nil
        ;; 处理中英文断行不分割问题，需要开启 toggle-word-wrap 和
        ;; visual-line-mode 才能体现
        word-wrap-by-category t)

  (setq-default tab-width 4
                tab-always-indent t
                tab-first-completion 'word-or-paren-or-punct
                indent-tabs-mode nil
                ;; bidi-display-reordering 'left-to-right
                bidi-display-reordering nil
                ;; bidi-paragraph-direction 'left-to-right
                ;; bidi-paragraph-direction nil
                ;; display-fill-column-indicator-character 124
                fringe-indicator-alist
                '((truncation () right-arrow)
                  (continuation () right-curly-arrow)
                  (overlay-arrow . right-triangle) (up . up-arrow)
                  (down . down-arrow)
                  (top top-left-angle top-right-angle)
                  (bottom bottom-left-angle bottom-right-angle
                          top-right-angle
                          top-left-angle)
                  (top-bottom left-bracket right-bracket top-right-angle
                              top-left-angle)
                  (empty-line . empty-line) (unknown . question-mark))
                visual-fill-column-center-text t)

  (dolist (mode '(prog-mode-hook TeX-mode-hook cuda-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode t))))

  (desktop-save-mode 1)
  (with-eval-after-load 'desktop
    (setq desktop-restore-frames nil))

  (setq comment-auto-fill-only-comments t)
  (setq whitespace-style '(face trailing))
  ;; (setq-default whitespace-style
  ;;               '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))
  (setq-default whitespace-display-mappings
                '(
                  ;; space -> · else .
                  (space-mark 32 [183] [46])
                  ;; new line -> ¬ else $
                  (newline-mark ?\n [172 ?\n] [36 ?\n])
                  ;; carriage return (Windows) -> ¶ else #
                  (newline-mark ?\r [182] [35])
                  ;; tabs -> » else >
                  (tab-mark ?\t [187 ?\t] [62 ?\t])))
  (add-hook 'prog-mode-hook #'whitespace-mode)

  (add-hook 'prog-mode-hook #'symbol-overlay-mode)

  ;; (electric-indent-mode -1)
  ;; (follow-mode)

  ;; Smoothly scrolling over image
  (unless (>= (string-to-number emacs-version) 30)
    (add-hook 'text-mode-hook
              (lambda ()
                (or (boundp 'iscroll-mode)
                    (load "~/.emacs.d/site-lisp/iscroll/iscroll.el"))
                (iscroll-mode))))

  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))


  (add-to-list 'load-path "~/.emacs.d/site-lisp/with-editor/lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/dash.el")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/compat")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
  ;; (require 'magit)

  (with-eval-after-load 'magit
    (setq magit-diff-refine-hunk t
          magit-log-section-commit-count 20
          magit-status-sections-hook '(magit-insert-status-headers
                                       magit-insert-merge-log
                                       magit-insert-rebase-sequence
                                       magit-insert-am-sequence
                                       magit-insert-sequencer-sequence
                                       magit-insert-bisect-output
                                       magit-insert-bisect-rest
                                       magit-insert-bisect-log
                                       magit-insert-untracked-files
                                       magit-insert-unstaged-changes
                                       magit-insert-staged-changes
                                       magit-insert-stashes
                                       magit-insert-unpushed-to-pushremote
                                       ;; magit-insert-unpushed-to-upstream-or-recent
                                       magit-insert-unpushed-to-upstream
                                       magit-insert-recent-commits
                                       magit-insert-unpulled-from-pushremote
                                       magit-insert-unpulled-from-upstream)))

  (dolist
      (hook
       '(emacs-lisp-mode-hook
         yuck-mode-hook
         python-ts-mode python-mode
         scss-mode-hook))
    (add-hook hook 'aggressive-indent-mode))
  ;; (dolist (mode '(verilog-mode org-mode term-mode))
  ;;   (add-to-list 'aggressive-indent-excluded-modes mode))

    ;;; Visual Repalcement
  (keymap-global-set "C-c r" 'vr/replace)
  (keymap-global-set "C-c m" 'vr/mc-mark)

  ;; (add-hook 'prog-mode-hook 'hs-minor-mode)

  ;; 折叠代码块，以下是额外启用了 :box t 属性使得提示更加明显
  (defconst hideshow-folded-face
    '(:inherit font-lock-comment-face :box t))

  (setq hs-set-up-overlay 'hideshow-folded-overlay-fn)

  (with-eval-after-load 'multiple-cursors
    (keymap-global-set "C-S-c C-S-c" 'mc/edit-lines)
    (keymap-global-set "C->" 'mc/mark-next-like-this)
    (keymap-set mc/keymap (kbd "<return>") nil))

  (global-set-key [remap move-beginning-of-line]
                  'mwim-beginning-of-code-or-line-or-comment)
  (global-set-key [remap move-end-of-line] 'mwim-end-of-code-or-line)

  ;; (c-add-style
  ;;  "freebsd"
  ;;  '(;; "bsd"
  ;;    (c-basic-offset . 4)    ; 设置缩进为4个空格
  ;;    (c-offsets-alist
  ;;     (statement-cont . c-lineup-assignments)  ; 连续语句的缩进方式
  ;;     (case-label . 0)        ; case标签缩进为0
  ;;     (substatement-open . 0) ; 子语句的开放括号不缩进
  ;;     (arglist-intro . +)     ; 函数参数列表的起始缩进
  ;;     (arglist-cont . 0)      ; 函数参数列表的续行缩进
  ;;     (arglist-cont-nonempty . c-lineup-arglist)  ; 函数参数列表非空时的续行缩进方式
  ;;     (arglist-close . 0)     ; 函数参数列表的关闭括号不缩进
  ;;     (inextern-lang . 0)     ; 在extern语言中的缩进
  ;;     (inline-open . 0)       ; 内联函数的开放括号不缩进
  ;;     (namespace-open . 0)    ; 命名空间的开放括号不缩进
  ;;     (innamespace . 0)       ; 在命名空间中的缩进
  ;;     (label . 0)             ; 标签不缩进
  ;;     )))

  ;; (add-to-list 'c-default-style '(c-mode . "freebsd"))
  ;; (add-to-list 'c-default-style '(c++-mode . "freebsd"))

  (when (treesit-available-p)
    (setq major-mode-remap-alist
          '((c-mode          . c-ts-mode)
            (c++-mode        . c++-ts-mode)
            (c-or-c++-mode . c-or-c++-ts-mode)
            (conf-toml-mode  . toml-ts-mode)
            (csharp-mode     . csharp-ts-mode)
            (css-mode        . css-ts-mode)
            (java-mode       . java-ts-mode)
            (js-mode         . js-ts-mode)
            (javascript-mode . js-ts-mode)
            (js-json-mode    . json-ts-mode)
            (python-mode     . python-ts-mode)
            (ruby-mode       . ruby-ts-mode)
            (sh-mode         . bash-ts-mode)
            (verilog-mode    . verilog-ts-mode)
            )
          treesit-font-lock-level 4
          )
    ;; (add-hook 'emacs-lisp-mode-hook
    ;;           (lambda () (treesit-parser-create 'elisp)))
    )

  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "Go-Translate"
          "*compilation*"
          "*Warning*"
          "*tex-shell*"
          "*Compile-Log*"
          "*xref*"
          help-mode
          helpful-mode
          compilation-mode
          youdao-dictionary-mode))
  (defun popper--fit-window-width (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (frame-height)
     (floor (frame-height) 6)
     (floor (frame-width) 2)
     ;; (floor (frame-width) 2)
     (floor (* (frame-width) 17) 35)))

  (defun popper--auto-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 2)
     (floor (* (frame-height) 2) 5)))

  (defun popper-display-popup-adaptive (buffer &optional alist)
    "Display popup-buffer BUFFER at the bottom of the screen.
  ALIST is an association list of action symbols and values.  See
  Info node `(elisp) Buffer Display Action Alists' for details of
  such alists."
    (if (and (> (window-pixel-height) (window-pixel-width))
             (or (and popper-open-popup-alist
                      (eq (window-parameter (caar popper-open-popup-alist) 'window-side)
                          'bottom))
                 (not popper-open-popup-alist)))
        (display-buffer-in-side-window
         buffer
         (append alist
                 `((window-height . popper--auto-fit-window-height)
                   (side . bottom)
                   (slot . 1))))
      (display-buffer-in-side-window
       buffer
       (append alist
               `((window-width . popper--fit-window-width)
                 (side . right)
                 (slot . 1))))))

  (setq popper-display-function 'popper-display-popup-adaptive
        fit-window-to-buffer-horizontally t)
  ;; (keymap-global-set "C-`" 'popper-toggle)
  (keymap-global-set "C-<tab>" 'popper-toggle)
  (keymap-global-set "M-`" 'popper-cycle)
  (keymap-global-set "C-M-`" 'popper-toggle-type)
  (popper-mode +1)
  (popper-echo-mode +1)

  (define-key global-map [remap list-buffers] 'ibuffer)

  (require 'indent-bars)
  (add-hook 'prog-mode-hook 'indent-bars-mode)
  (with-eval-after-load 'indent-bars
    (setq ;; indent-bars-pattern "."
     ;; indent-bars-highlight-current-depth
     ;; '(:face default :blend 0.4)
     indent-bars-treesit-support t
     indent-bars-no-descend-string t
     indent-bars-treesit-ignore-blank-lines-types '("module")
     indent-bars-width-frac 0.2
     indent-bars-color '(highlight :face-bg t :blend 0.7)
     indent-bars-display-on-blank-lines t
     ;; indent-bars-prefer-character t
     ;; indent-bars-no-stipple-char ?\⎸
     ))

  ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/consult")
  ;; (require 'consult)
  ;; (load "~/.emacs.d/site-lisp/consult/consult-xref.el")

  (autoload 'consult-buffer "consult" nil t)
  (autoload 'consult-line "consult" nil t)
  (keymap-global-set "C-x l" 'consult-line)
  (keymap-global-set "C-x b" 'consult-buffer)
  (with-eval-after-load 'consult
    (consult-customize
     consult-buffer
     :preview-key '(:debounce 0.4 "M-."))
    (setq consult-project-function
          (lambda (may-prompt)
            (or
             (vc-root-dir)
             (consult--default-project-function may-prompt)))))
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref)

  (keymap-global-set "C-." 'embark-act)
  (with-eval-after-load 'embark
    (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))

  ;;; Theme
  (dolist (hook '(prog-mode-hook text-mode-hook cuda-mode-hook))
    (add-hook hook 'colorful-mode))
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  ;; (require 'color-theme-sanityinc-tomorrow)
  ;; (color-theme-sanityinc-tomorrow-bright)
  ;; (color-theme-sanityinc-tomorrow-bright)

  ;; @ Minibuffer Setting
  ;; (load "~/.emacs.d/site-lisp/vertico/extensions/vertico-directory.el")

  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))))

  (if (boundp 'vertico-mode)
      (progn
        (require 'orderless)
        (setq enable-recursive-minibuffers t)
        (vertico-mode)
        (vertico-reverse-mode)
        (vertico-indexed-mode)
        (vertico-mouse-mode)
        (vertico-multiform-mode)
        (marginalia-mode)
        (setq vertico-multiform-categories
              '((file grid)
                (consult-grep buffer)
                (consult-ripgrep buffer))
              vertico-cycle t)
        (keymap-global-set "C-'" #'vertico-suspend)
        (keymap-set vertico-map "?" #'minibuffer-completion-help)
        (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
        (keymap-set vertico-map "M-TAB" #'minibuffer-complete)
        (keymap-set vertico-map "C-w" 'vertico-directory-delete-word))
    (keymap-set minibuffer-mode-map "C-w" 'backward-kill-word))
  (and (boundp 'puni-mode)
       (require 'puni)
       (puni-global-mode))
  (with-eval-after-load 'puni
    (add-hook 'puni-mode-hook
              (lambda ()
                (keymap-set puni-mode-map "M-w" 'puni-kill-region)
                (keymap-set puni-mode-map "M-k" 'puni-backward-kill-line)
                (keymap-unset puni-mode-map "C-w")
                ))
    (dolist (hook '(term-mode-hook minibuffer-mode-hook))
      (add-hook hook #'puni-disable-puni-mode)))

  (setq-default completion-styles '(orderless basic))
  (setq completion-styles '(basic partial-completion orderless)
        completion-category-overrides
        '((file (styles basic partial-completion))))

  ;; @ Windows Control

  ;; (load "~/.emacs.d/site-lisp/emacs-winum/winum.el")
  ;; (winum-mode)

  ;; @ UI
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'display-fill-column-indicator-mode))
  ;; (global-display-fill-column-indicator-mode)

  (defvar themes_chosen
    '(;;; Light theme
      ;; modus-operandi-tritanopia
      ef-spring
      ;; Dark theme
      ;; manoj-dark
      ;; doom-rouge
      modus-vivendi-tritanopia
      )
    "Set for themes for dark and light mode.")

  (setup-display-graphic)
  (add-hook 'server-after-make-frame-hook 'setup-display-graphic)

  ;; (load "~/.emacs.d/lisp/ultimate-tab.el")

  ;; (load "~/.emacs.d/site-lisp/password-store/contrib/emacs/password-store.el")

  ;; (dolist (hook '(
  ;;           ;; completion-list-mode-hook
  ;;           ;; completion-in-region-mode-hook
  ;;           term-mode-hook
  ;;           ;; shell-mode-hook
  ;;           messages-buffer-mode-hook
  ;;           org-roam-mode-hook))
  ;;   (add-hook hook 'hide-mode-line-mode))

  (require 'citre)
  (require 'citre-config)
  (with-eval-after-load 'citre
    (setq citre-use-project-root-when-creating-tags t
          citre-prompt-language-for-ctags-command t)
    (when (and (eq system-type 'gnu/linux) (file-exists-p "/opt/bin/ctags"))
      (setq citre-ctags-program "/opt/bin/ctags"))
    )

  (when (boundp 'hl-todo)
    (global-hl-todo-mode))

  (with-eval-after-load 'highlight-indent-guides
    (setq highlight-indent-guides-method 'character
          highlight-indent-guides-responsive 'top
          highlight-indent-guides-suppress-auto-error t)))

(provide 'init)
;;; init.el ends here
