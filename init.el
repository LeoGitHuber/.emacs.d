;;; Init.el --- Emacs Configuration --- -*- lexical-binding: t; -*-
;;; Commentary:

(add-to-list 'load-path "~/.emacs.d/site-lisp/benchmark-init-el")

(defvar windows-system-p
  (string-equal system-type "windows-nt")
  "Judge whether it's Windows system.")

;; (if windows-system-p
;;     (progn
;;       (require 'benchmark-init)
;;       (prefer-coding-system 'utf-8))
;;   (require 'benchmark-init-loaddefs)
;;   )

(require 'benchmark-init)

(benchmark-init/activate)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
(setq custom-file "~/.emacs.d/custom.el"
      load-prefer-newer t)
(load "~/.emacs.d/custom.el")

(let ((file-name-handler-alist nil))
  ;; (require 'package)
  ;; (package-initialize)

  (setq package-archives
        '(("gnu"    .
           "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
          ("nongnu" .
           "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
          ("melpa"  .
           "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/"))
        ;; “Gnu”应该和“melpa”同优先级, 从而默认选取二者中较新的 package.
        package-archive-priorities '(("gnu"    . 1)
                                     ("nongnu" . 0)
                                     ("melpa"  . 1))
        package-menu-hide-low-priority t
        ;; 暂时不知道检查签名有什么用,先关了再说.
        package-check-signature nil
        eldoc-documentation-function 'eldoc-documentation-compose
        )

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
  ;; (load "~/.emacs.d/lisp/init-gc.el")
  ;; (load "~/.emacs.d/lisp/init-icons.el")
  ;; (load "~/.emacs.d/lisp/init-meow.el")
  ;; (load "~/.emacs.d/lisp/init-keybindings.el")
  ;; (load "~/.emacs.d/lisp/init-lsp.el")
  ;; (load "~/.emacs.d/lisp/init-dired.el")
  ;; (load "~/.emacs.d/lisp/init-chinese.el")
  ;; (load "~/.emacs.d/lisp/init-input.el")
  ;; (load "~/.emacs.d/lisp/init-eaf.el")
  ;; (load "~/.emacs.d/lisp/init-org.el")
  ;; (load "~/.emacs.d/lisp/init-verilog.el")
  ;; (load "~/.emacs.d/lisp/init-reader.el")
  ;; (load "~/.emacs.d/lisp/init-hydra.el")
  ;; (load "~/.emacs.d/lisp/latex-node.el")
  ;; (load "~/.emacs.d/lisp/init-latex.el")
  ;; (load "~/.emacs.d/lisp/init-base.el")
  (setq eat-kill-buffer-on-exit t
        css-indent-offset 2
        set-mark-command-repeat-pop t)

  ;;; @1. GC

  (add-hook 'minibuffer-setup-hook 'gc-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook 'gc-minibuffer-exit-hook)

  ;;; @2. flymake

  (with-eval-after-load 'flymake
    (setq flymake-no-changes-timeout nil)

    (with-eval-after-load 'flycheck
      (setq-default flycheck-disabled-checkers
                    (append (default-value 'flycheck-disabled-checkers)
                            '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package))))

    (defvar verilog--flymake-proc nil
      "A flymake verilog process.")

    (defvar verilog-flymake-command '("verilator" "--lint-only" "-Wall")
      "Command for verilog's flymake.")

    (defvar verilog--flymake-output-buffer " *stderr of verilog-flymake*"
      "Buffer for verilog's flymake output.")

    (defun verilog-flymake-done (report-fn
                                 source-buffer
                                 output-buffer)
      (with-current-buffer source-buffer
        (save-excursion
          (save-restriction
            (with-current-buffer output-buffer
              (goto-char (point-min))
              (let ((diags))
                (while (search-forward-regexp
                        "^\\(%.*\\): .*:\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
                        nil t)
                  (let* ((msg (match-string 4))
                         (level-msg (match-string 1))
                         (line (string-to-number (match-string 2)))
                         (locate-string)
                         (cal)
                         (beg)
                         (end)
                         (level))
                    (setq level (cond
                                 ((string-match-p "%Error" level-msg) ':error)
                                 ((string-match-p "%Warning" level-msg) ':warning)
                                 (t :note)))
                    (search-forward-regexp "\\(\\^~*\\)" nil t)
                    (setq cal (length (match-string 1)))
                    (let ((current (point))
                          (line-beginning (line-beginning-position)))
                      (forward-line -1)
                      (forward-char (- current line-beginning))
                      (setq locate-string (buffer-substring-no-properties (- (point) cal) (point))))
                    (setq beg (with-current-buffer source-buffer
                                (save-excursion
                                  (save-restriction
                                    (goto-char (point-min))
                                    (or (equal line 1)
                                        (forward-line (- line 1)))
                                    (search-forward locate-string nil t)
                                    (- (point) cal)
                                    ))))
                    (setq end (+ beg cal))
                    (setq diags
                          (cons (flymake-make-diagnostic
                                 source-buffer beg end level msg)
                                diags))
                    ))
                (funcall report-fn diags)
                )))))
      )

    (defun verilog-flymake-detect (report-fn &rest _args)
      "A Flymake backend for verilog.
Spawn an verilog lsp process that byte-compiles a file representing the
current buffer state and calls REPORT-FN when done."
      (when verilog--flymake-proc
        (when (process-live-p verilog--flymake-proc)
          (kill-process verilog--flymake-proc)))
      (let ((source-buffer (current-buffer))
            (coding-system-for-write 'utf-8-unix)
            (coding-system-for-read 'utf-8))
        (save-restriction
          (widen)
          (let* ((output-buffer (generate-new-buffer " *verilog-flymake*")))
            (setq verilog--flymake-proc
                  (make-process
                   :name "verilog-flymake-process"
                   :buffer output-buffer
                   :command (append verilog-flymake-command
                                    (list (buffer-file-name source-buffer)))
                   :connection-type 'pipe
                   :sentinel
                   (lambda (proc _event)
                     (unless (process-live-p proc)
                       (unwind-protect
                           (cond
                            ((not (and (buffer-live-p source-buffer)
                                       (eq proc (with-current-buffer source-buffer
                                                  verilog--flymake-proc))))
                             (flymake-log :warning
                                          "verilog-flymake process %s obsolete" proc))
                            ((memq (process-status proc) '(exit signal))
                             (verilog-flymake-done report-fn
                                                   source-buffer
                                                   verilog--flymake-output-buffer
                                                   ))
                            (t
                             (funcall report-fn
                                      :panic
                                      :explanation
                                      (format "process %s died" proc))))
                         (kill-buffer verilog--flymake-output-buffer)
                         )))
                   :stderr verilog--flymake-output-buffer
                   :noquery t))))))

    (defun verilog-setup-flymake-backend ()
      (add-hook 'flymake-diagnostic-functions 'verilog-flymake-detect nil t))

    ;; (add-hook 'verilog-mode-hook 'verilog-setup-flymake-backend)

    (defun sanityinc/enable-flymake-flycheck ()
      (setq-local flymake-diagnostic-functions
                  (seq-uniq (append flymake-diagnostic-functions
                                    (flymake-flycheck-all-chained-diagnostic-functions))))))

  (dolist (hook '(prog-mode-hook))
    (add-hook hook 'flymake-mode))


  ;;; @3. ICONS

  (add-to-list 'load-path "~/.emacs.d/site-lisp/nerd-icons.el")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/treemacs-nerd-icons")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/nerd-icons-dired")

  (setq nerd-icons-font-family "BlexMono Nerd Font")

  (require 'nerd-icons)

  ;; (load "~/.emacs.d/self-develop/modeline-setting.el")

  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)

  (with-eval-after-load 'treemacs
    (require 'treemacs-nerd-icons)
    (treemacs-load-theme "nerd-icons"))

  (add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode)

  ;; (require 'all-the-icons)
  (with-eval-after-load 'all-the-icons (load "~/.emacs.d/self-develop/all-the-icons-diy.el"))

  ;; (with-eval-after-load 'nerd-icons
  ;;   (setq fc-info (nerd-icons-codicon "nf-cod-question" :face '(:inherit flycheck-info-my))
  ;;     fc-warning (nerd-icons-codicon "nf-cod-warning" :face '(:inherit flycheck-warn))
  ;;     fc-error (nerd-icons-codicon "nf-cod-error" :face '(:inherit flycheck-error-my)))
  ;;   )

  (load "~/.emacs.d/lisp/init-startup.el")

  ;;; @4. MEOW

  (require 'meow)
  (meow-setup)

  (setq meow-use-cursor-position-hack t
        meow-use-enhanced-selection-effect t
        meow--kbd-kill-region "M-w"
        meow--kbd-kill-ring-save "C-w")

  (meow-global-mode)

  ;;; @5. KEYBINDINGS

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
  (keymap-global-set "C-x k" 'kill-this-buffer)
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

  ;; (lsp-enable-startup)

  (with-eval-after-load 'lsp-mode
    (with-eval-after-load 'lsp-ui
      (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
      (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
    (setq lsp-keymap-prefix "C-c l"
          ;; lsp-keep-workspace-alive nil
          ;; lsp-signature-auto-activate nil
          lsp-modeline-code-actions-enable nil
          lsp-modeline-diagnostics-enable nil
          lsp-modeline-workspace-status-enable nil
          lsp-headerline-breadcrumb-enable t
          lsp-semantic-tokens-enable t
          ;; lsp-progress-spinner-type 'progress-bar-filled
          lsp-enable-file-watchers nil
          lsp-enable-folding nil
          lsp-enable-symbol-highlighting nil
          lsp-enable-text-document-color nil

          lsp-enable-indentation nil
          lsp-enable-on-type-formatting nil

          lsp-enable-indentation nil

          ;; For diagnostics
          ;; lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

          ;; Reference Lens
          ;; lsp-lens-enable nil

          ;; ui
          ;; lsp-ui-doc-show-with-cursor nil

          lsp-completion-provider :none
          lsp-prefer-flymake t
          lsp-ui-flycheck-enable nil
          lsp-enable-relative-indentation t
          )
    ;; (keymap-set lsp-mode-map "C-c C-d" 'lsp-describe-thing-at-point)
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)

    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 (executable-find "emacs-lsp-booster"))
            (progn
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
    (add-hook 'lsp-mode-hook 'corfu-mode))

  (with-eval-after-load 'corfu
    (setq corfu-auto t
          corfu-cycle t
          ;; corfu-quit-no-match 'separator  ;; t
          corfu-auto-prefix 1
          corfu-auto-delay 0
          corfu-preview-current t
          corfu-quit-no-match t
          ;; corfu-preselect 'prompt
          corfu-quit-at-boundary t)
    (when (boundp 'meow-insert-exit-hook)
      (add-hook 'meow-insert-exit-hook 'corfu-quit))
    (keymap-set corfu-map "<tab>" 'corfu-insert)
    ;; (keymap-set corfu-map "<backtab>" 'corfu-previous)
    ;; (keymap-set corfu-map "S-<return>" 'corfu-insert)
    ;; (keymap-unset corfu-map "RET")
    (add-to-list 'completion-at-point-functions 'cape-file)
    ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
    (with-eval-after-load 'corfu-popupinfo
      (setq corfu-popupinfo-delay '(0.1 . 0.1)))
    ;; (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)
    (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter))

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
                        (frame-visible-p acm-menu-frame)))
                 ))))

  ;; (add-to-list 'project-vc-extra-root-markers "tsconfig.json")
  ;; (add-to-list 'project-vc-extra-root-markers "jsconfig.json")
  (with-eval-after-load 'eglot
    (setq eglot-send-changes-idle-time 0)
    (add-to-list 'eglot-server-programs
                 '((tex-mode context-mode texinfo-mode bibtex-mode)
                   . ("texlab")))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (add-hook 'eglot-managed-mode-hook 'corfu-mode)
    (add-hook 'eglot-managed-mode-hook 'yas-minor-mode)
    (when (and windows-system-p (string-match-p "29" emacs-version))
      (eglot-booster-mode))
    )

  (lsp-enable-startup)

  (add-hook 'company-mode-hook
            (lambda ()
              (setq company-tooltip-align-annotations t
                    company-tooltip-limit 12
                    company-idle-delay 0
                    company-echo-delay (if (display-graphic-p) nil 0)
                    company-minimum-prefix-length 1
                    company-icon-margin 3
                    company-require-match nil
                    company-dabbrev-ignore-case nil
                    company-dabbrev-downcase nil
                    company-global-modes '(not erc-mode message-mode help-mode
                                               gud-mode eshell-mode shell-mode)
                    company-backends '((company-capf :with company-yasnippet)
                                       (company-dabbrev-code company-keywords company-files)
                                       company-dabbrev))
              (define-key company-active-map (kbd "C-h") 'delete-backward-char)
              (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
              ;; (setq company-box-icons-alist 'company-box-icons-idea)
              (setq company-box-scrollbar 'inherit)
              (company-box-mode t)))

  ;; (defun company-completion-styles (capf-fn &rest args)
  ;;   (let ((completion-styles '(basic partial-completion)))
  ;;     (apply capf-fn args)))

  ;; (advice-add 'company-capf :around #'company-completion-styles)

  (with-eval-after-load 'lsp-bridge
    (add-hook 'lsp-bridge-mode-hook 'yas/minor-mode)
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
     ;; lsp-bridge-enable-log t
     lsp-bridge-enable-signature-help t
     lsp-bridge-enable-diagnostics nil
     lsp-bridge-complete-manually nil
     ;; lsp-bridge-enable-profile t
     ;; lsp-bridge-multi-lang-server-mode-list nil
     acm-backend-lsp-candidate-min-length 2
     acm-backend-elisp-candidate-min-length 2
     acm-backend-search-file-words-candidate-min-length 3
     acm-backend-yas-candidate-min-length 1
     lsp-bridge-python-command "python"
     ;; This will cause `org-roam-node-find' get wrong and I don't know why.
     ;; lsp-bridge-enable-org-babel t
     ;; lsp-bridge-c-lsp-server "clangd"
     ;; lsp-bridge-user-langserver-dir "~/.emacs.d/lisp/langserver"
     ;; lsp-bridge-user-multiserver-dir "~/.emacs.d/lisp/multilangserver"
     )
    ;; (add-to-list 'lsp-bridge-multi-lang-server-mode-list
    ;;              '((verilog-mode) . "verilog"))
    ;; (add-to-list 'lsp-bridge-multi-lang-server-extension-list
    ;;              '(("v" "sv") . "verilog"))
    ;; (setf (cdr (assoc 'verilog-mode lsp-bridge-single-lang-server-mode-list)) '("svlangserver"))
    ;; (add-to-list 'lsp-bridge-single-lang-server-mode-list '((verilog-mode) . "svlangserver"))
    ;; (add-to-list 'lsp-bridge-single-lang-server-mode-list '((verilog-mode) . "verible"))
    ;; (add-to-list 'lsp-bridge-single-lang-server-mode-list '((verilog-mode) . "veridian"))
    ;; (add-to-list 'lsp-bridge-single-lang-server-mode-list '((verilog-mode) . "svls"))
    )

  (with-eval-after-load 'kind-icon
    (setq kind-icon-use-icons nil
          kind-icon-mapping
          `(
            (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
            (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
            (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
            (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
            (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
            (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
            (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
            (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
            (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
            (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
            (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
            (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
            (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
            (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
            (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
            (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
            (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
            (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
            (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
            (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
            (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
            (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
            (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
            (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
            (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
            (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
            (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
            (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
            (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
            (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
            (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
            (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
            (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
            (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
            (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
            (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face))))

  ;;; @7. DIRED

  (add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish/extensions")
  (add-hook 'dired-mode-hook
            (lambda ()
              (or (boundp 'diredfl-mode)
                  (load "~/.emacs.d/site-lisp/diredfl/diredfl.el"))
              (toggle-truncate-lines)
              (diredfl-mode)
              ;; (require 'image-dired)
              ;; (require 'dirvish)
              ))
  (with-eval-after-load 'dired
    (setq dired-listing-switches
          "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group"
          dired-dwim-target t
          dired-mouse-drag-files t
          dired-auto-revert-buffer t
          dired-do-revert-buffer t
          mouse-drag-and-drop-region-cross-program t
          dired-kill-when-opening-new-dired-buffer t
          dired-recursive-copies 'always
          ;; dired-recursive-deletes 'always
          delete-by-moving-to-trash t
          image-dired-thumb-size 256
          image-dired-marking-shows-next nil)
    (defun dired-open-externally (&optional arg)
      "Open marked or current file in operating system's default application."
      (interactive "P")
      (dired-map-over-marks
       (embark-open-externally (dired-get-filename))
       arg))
    (keymap-set dired-mode-map "e" 'dired-open-externally))

  (with-eval-after-load 'dirvish
    ;; (dirvish-peek-mode)
    ;; (require 'dirvish-side)
    (dirvish-override-dired-mode)
    (dirvish-side-follow-mode)
    ;; (add-hook 'dirvish-setup-hook 'dirvish-emerge-mode)
    (setq dirvish-attributes '(vc-state nerd-icons file-size subtree-state collapse file-time)
          dirvish-side-width 35
          dirvish-emerge-groups '(("Recent files" (predicate . recent-files-2h))
                                  ("Video" (extensions "mp4" "mkv" "webm"))
                                  ("Pictures" (extensions "jpg" "png" "jpeg" "svg" "gif"))
                                  ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
                                  ("Archives" (extensions "gz" "rar" "zip")))
          dirvish-path-separators '(" ~" " /" "/")
          ;; dirvish-hide-details nil
          dirvish-mode-line-height 20
          ;; dirvish-show-media-properties t
          ;; Turn off media cache, but it will slow down the speed of media preview
          dirvish-media-auto-cache-threshold nil
          ;; dirvish-preview-dispatch (remove 'epub dirvish-preview-dispatch)
          )
    (keymap-set dirvish-mode-map "TAB" #'dirvish-toggle-subtree))

  ;;; @8. CHINESE

  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation")
  (setq cns-prog "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cnws"
        cns-dict-directory "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cppjieba/dict"
        ;; To use other program for word segmentation, set cns-process-shell-command:
        ;; cns-process-shell-command "word_segmentation_program arg1 arg2..."
        ;; disable debug output, default is t
        cns-recent-segmentation-limit 20
        cns-debug nil)

  (require 'cns nil t)

  ;;; @9. INPUT

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
           (meow-normal-mode-p)
           ))
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
    (keymap-set rime-mode-map "s-`" 'rime-send-keybinding)
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
    (keymap-set rime-mode-map "M-y" 'rime-force-enable)
    ;; (with-eval-after-load 'tex
    ;;   (add-to-list 'rime-disable-predicates
    ;;                'rime-predicate-tex-advance-p))
    )

  ;; (keymap-global-set "C-\\" 'rime-commit-and-toggle-input-method)
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

  ;;; @10. EAF

  ;; (require 'eaf)

  ;; (with-eval-after-load 'eaf
  ;;   (require 'eaf-pdf-viewer)
  ;;   (setq eaf-pdf-show-progress-on-page nil))

  ;;; @11. ORG

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
          org-preview-latex-process-alist '((dvipng :programs
                                                    ("latex" "dvipng")
                                                    :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
                                                    (1.0 . 1.0)
                                                    :latex-compiler
                                                    ("latex -interaction nonstopmode -output-directory %o %f")
                                                    :image-converter
                                                    ("dvipng -D %D -T tight -o %O %f")
                                                    :transparent-image-converter
                                                    ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
                                            (dvisvgm :programs
                                                     ("latex" "dvisvgm")
                                                     :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
                                                     (1.0 . 1.0)
                                                     :latex-compiler
                                                     ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                                                     :image-converter
                                                     ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
                                            (imagemagick :programs
                                                         ("latex" "convert")
                                                         :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                                                         (1.0 . 1.0)
                                                         :latex-compiler
                                                         ("pdflatex -interaction nonstopmode -output-directory %o %f")
                                                         :image-converter
                                                         ("convert -density %D -trim -antialias %f -quality 100 %O")))
          ;; org-preview-latex-default-process 'dvisvgm
          org-format-latex-header "\\documentclass[10pt]{article}\n\\usepackage[usenames]{color}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\pagestyle{empty}  % do not remove\n% The settings below are copied from fullpage.sty\n
        \\usepackage{xeCJK,tikz,caption,float,makecell,circuitikz,array}\n
        \\usetikzlibrary{shapes,arrows,calc,arrows.meta}\n
        \\usetikzlibrary{circuits.logic.IEC,calc}\n
        \\renewcommand{\\arraystretch}{1.3}\n
        \\setlength{\\textwidth}{\\paperwidth}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}\n")
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (jupyter . t)))
    )

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
                  (((background dark)) (:inherit fixed-pitch))))
               '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
               '(org-block-begin-line ((t (:inherit fixed-pitch))))
               '(org-block-end-line ((t (:inherit fixed-pitch))))
               '(fill-column-indicator ((t (:inherit (shadow fixed-pitch))))))
              (variable-pitch-mode)
              ;; (company-mode)
              ;; (corfu-mode)
              ;; (visual-line-mode)
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
            (?+ . "‣"))
          ))


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
              (add-hook 'before-save-hook 'time-stamp nil 'local)
              ))

  (add-hook 'org-mode-hook #'org-modern-indent-mode 100)

  (keymap-global-set "C-c n f" 'org-roam-node-find)
  (setq org-roam-directory "~/Documents/Personal/org-roam"    ; 设置 org-roam 笔记的默认目录，缺省值 /home/leo/org-roam
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
  ;;   (add-hook 'window-buffer-change-functions 'my/org-roam-buffer-show)
  ;;   )

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
             :unnarrowed t)))
    (org-roam-db-autosync-mode))

  ;;; @12. VERILOG

  (with-eval-after-load 'verilog-mode
    ;; (with-eval-after-load 'lsp-mode
    ;;   (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
    ;;   (lsp-register-client
    ;;    (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
    ;;             :major-modes '(verilog-mode)
    ;;             :priority -1)))
    (with-eval-after-load 'lsp-mode
      (require 'lsp-verilog)
      (custom-set-variables
       '(lsp-clients-svlangserver-launchConfiguration "verilator -sv --lint-only -Wall")
       '(lsp-clients-svlangserver-formatCommand "verible-verilog-format")))
    (setq verilog-indent-lists t
          verilog-auto-delete-trailing-whitespace t
          verilog-align-ifelse t
          verilog-auto-inst-param-value t
          verilog-auto-inst-vector t
          verilog-auto-lineup 'all
          verilog-auto-newline t
          verilog-auto-save-policy nil
          verilog-auto-template-warn-unused t
          verilog-case-indent 2
          verilog-cexp-indent 2
          verilog-highlight-grouping-keywords t
          verilog-highlight-modules t
          verilog-indent-level 3
          verilog-indent-level-behavioral 3
          verilog-indent-level-declaration 3
          verilog-indent-level-module 3
          ;; verilog-tab-to-comment t
          verilog-ext-feature-list '(;; font-lock
                                     xref
                                     capf
                                     hierarchy
                                     eglot
                                     ;; lsp
                                     flycheck
                                     ;; beautify
                                     navigation
                                     ;; template
                                     ;; formatter
                                     ;; compilation
                                     ;; imenu
                                     which-func
                                     hideshow
                                     typedefs
                                     time-stamp
                                     block-end-comments
                                     ports
                                     )
          verilog-ext-hierarchy-backend 'builtin
          )
    ;; (require 'verilog-ext)
    ;; (verilog-ext-mode-setup)
    ;; (verilog-ext-eglot-set-server 've-veridian)
    ;; (verilog-ext-eglot-set-server 've-svlangserver)

    (add-hook 'verilog-mode-hook
              (lambda ()
                (when indent-bars-mode
                  (setq-local indent-bars-spacing-override verilog-indent-level))))

    (add-hook 'verilog-mode-hook
              (lambda ()
                (eglot-ensure)
                (add-to-list 'eglot-server-programs
                             ;; '(verilog-mode . ("svlangserver"))
                             ;; '(verilog-mode . ("svls"))
                             ;; '(verilog-mode . ("vls"))
                             '(verilog-mode . ("veridian"))
                             )
                (setq eglot-workspace-configuration
                      '(:veridian
                        (:settings
                         (:syntax
                          (:enabled :json-true
                                    :path "verible-verilog-syntax")
                          :format:
                          (:enabled :json-true
                                    :path "verible-verilog-format")
                          ;; :diagnostics
                          ;; (:enabled :json-false)
                          ))))
                ;;             ;; (setq eglot-workspace-configuration
                ;;             ;;       '(:svlangserver
                ;;             ;;         (:settings
                ;;             ;;          (:systemverilog.includeIndexing:
                ;;             ;;           ["**/*.{sv,svh,v,vh}", "*.{sv,svh,v,vh}"],
                ;;             ;;           :systemverilog.launchConfiguration:
                ;;             ;;           "verilator -sv -Wall --lint-only",
                ;;             ;;           :systemverilog.formatCommand:
                ;;             ;;           "verible-verilog-format"))))
                ;;             ;; (setq eglot-workspace-configuration
                ;;             ;;       '(:svls
                ;;             ;;         (:settings
                ;;             ;;          (:systemverilog.launchConfiguration:
                ;;             ;;           "verilator -sv -Wall --lint-only",
                ;;             ;;           :systemverilog.formatCommand:
                ;;             ;;           "verible-verilog-format"))))
                )
              )
    )

  ;;; @13. READER

  (with-eval-after-load 'pdf-tools
    (setq pdf-view-use-scaling t
          pdf-view-continuous nil
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
  (pdf-loader-install)

  (eval-after-load 'info
    '(add-to-list 'Info-directory-list "/usr/local/share/info"))

  (eval-after-load "tex-mode"
    '(progn
       (load "auctex.el" nil t t)
       (load "preview-latex.el" nil t t)
       ;; (require 'org-table)
       ))

  ;; (defun orgtbl-next-field-maybe ()
  ;;   "Combine `lsp-bridge-mode', `cdlatex-mode' and `orgtlr-mode'."
  ;;   (interactive)
  ;;   (if (and (bound-and-true-p lsp-bridge-mode)
  ;;            (acm-frame-visible-p acm-menu-frame))
  ;;       (acm-complete)
  ;;     (if (bound-and-true-p cdlatex-mode)
  ;;         (cdlatex-tab)
  ;;       (org-table-next-field))))

  (with-eval-after-load 'tex
    (add-hook 'cdlatex-tab-hook
              (lambda ()
                (and (bound-and-true-p lsp-bidge-mode)
                     (acm-frame-visible-p acm-menu-frame))))
    (load "~/.emacs.d/lisp/auctex-latexmk.el")
    (setq TeX-auto-save t
          TeX-parse-self t
          ;; TeX-fold-auto t
          TeX-expand-list '(("%x" TeX-active-master-with-quotes "xdv" t))
          preview-image-type 'dvipng
          ;; preview-pdf-color-adjust-method nil
          )
    (setq-default TeX-master nil
                  TeX-engine 'xetex
                  preview-scale-function 0.6
                  ;; preview-LaTeX-command '("%`%l -no-pdf \"\\nonstopmode\\nofiles\
                  ;; \\PassOptionsToPackage{" ("," . preview-required-option-list) "}{preview}\
                  ;; \\AtBeginDocument{\\ifx\\ifPreview\\undefined"
                  ;; preview-default-preamble "\\fi}\"%' \"\\detokenize{\" %(t-filename-only) \"}\"")
                  ;; preview-dvipng-command "dvipng -picky -noghostscript %x -o %m/prev%%03d.png"
                  )
    ;; (add-to-list 'TeX-view-program-list '("sioyek" "sioyek --page %(outpage) %o"))
    (add-to-list 'TeX-view-program-selection '(output-pdf "Sioyek"))
    (with-eval-after-load 'eaf
      (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
      (add-to-list 'TeX-view-program-selection '(output-pdf "eaf")))
    )


  (dolist (hook '(LaTeX-mode-hook TeX-mode-hook tex-mode-hook))
    (add-hook hook
              (lambda()
                (auctex-latexmk-setup)
                (electric-indent-local-mode)
                (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex -shell-escape --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
                (setq TeX-command-default "XeLaTeX")
                (add-to-list 'texmathp-tex-commands1 '("lstlisting" env-off))
                (TeX-global-PDF-mode)
                (TeX-fold-mode 1)
                (turn-on-cdlatex)
                (turn-on-reftex)
                ;; (keymap-set cdlatex-mode-map "<tab>" 'cdlatex-tab-maybe)
                ;; (turn-on-orgtbl)
                ;; (keymap-set orgtbl-mode-map "<tab>" 'orgtbl-next-field-maybe)
                )))

  (add-hook 'tex-mode-hook
            (lambda()
              (electric-indent-local-mode)
              (setq display-tex-shell-buffer-action nil)
              (visual-line-mode)
              (TeX-fold-mode 1)
              (turn-on-cdlatex)
              (turn-on-reftex)
              ;; (turn-on-orgtbl)
              ;; (keymap-set orgtbl-mode-map "<tab>" 'orgtbl-next-field-maybe)
              ))

  ;;; @17. BASE
  ;; (defun enable-after-meow ()
  ;;   "Modes enable after meow insert."
  ;;   (unless (bound-and-true-p lsp-bridge-mode)
  ;;     (global-lsp-bridge-mode)
  ;;     (lsp-bridge-mode)
  ;;     )
  ;;   (remove-hook 'meow-insert-enter-hook #'enable-after-meow))

  ;; (when (bound-and-true-p meow-mode)
  ;;   (add-hook 'meow-insert-enter-hook #'enable-after-meow))

  ;; (global-lsp-bridge-mode)

  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold better-gc-cons-threshold)))

  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  ;; Emacs 28 后不再需要设置系统编码，以下是以前的设置
  ;; UTF-8 as default encoding
  ;; (set-language-environment "UTF-8")
  ;; (set-default-coding-systems 'utf-8)
  ;; (set-keyboard-coding-system 'utf-8-unix)
  ;; (prefer-coding-system 'utf-8)
  ;; do this especially on Windows, else python output problem
  ;; (set-terminal-coding-system 'utf-8-unix)

  (electric-pair-mode)
  (pixel-scroll-precision-mode)
  (global-subword-mode)
  (global-auto-revert-mode)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-interval 1)

  (with-eval-after-load 'info
    (add-to-list 'Info-directory-list "/usr/local/texlive/2023/texmf-dist/doc/info"))

  ;; (setq hl-line-range-function 'hl-current-line-range)
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

  ;; (setq-default cursor-type '(bar . 3))

  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-delay 0
        show-paren-context-when-offscreen 'child-frame
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


  ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/auto-save")
  ;; (require 'auto-save)
  ;; (auto-save-enable)
  (with-eval-after-load 'auto-save
    (setq auto-save-delete-trailing-whitespace t
          auto-save-disable-predicates
          '((lambda ()
              (string-suffix-p
               "gpg"
               (file-name-extension (buffer-name)) t)))))

  (setq auto-save-visited-interval 1
        ;; auto-save-timeout 30
        ;; auto-save-interval 10
        auto-save-no-message t
        )

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
                                     "lib/.*-autoloads\\.el\\'")))
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
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

  ;; (setq desktop-path (list user-emacs-directory))
  ;;   desktop-auto-save-timeout 600)
  ;; (desktop-save-mode 1)

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


  (add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/with-editor/lisp")
  (dolist
      (hook
       '(emacs-lisp-mode-hook
         yuck-mode-hook
         python-ts-mode python-mode
         scss-mode-hook))
    (add-hook hook 'aggressive-indent-mode))
  ;; (dolist (mode '(verilog-mode org-mode term-mode))
  ;;   (add-to-list 'aggressive-indent-excluded-modes mode))

  (dolist (hook '(term-mode-hook))
    (add-hook hook #'puni-disable-puni-mode))

  ;;; Visual Repalcement
  (keymap-global-set "C-c r" 'vr/replace)
  (keymap-global-set "C-c m" 'vr/mc-mark)

  (add-hook 'prog-mode-hook 'hs-minor-mode)

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

  (c-add-style
   "microsoft"
   '("stroustrup"
     (c-offsets-alist
      (access-label . /)
      (innamespace . -)
      (inline-open . 0)
      (inher-cont . c-lineup-multi-inher)
      (arglist-cont-nonempty . +)
      (template-args-cont . +))
     )
   )

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
            (sh-mode         . bash-ts-mode))
          treesit-font-lock-level 4
          )
    ;; (add-hook 'emacs-lisp-mode-hook
    ;;           (lambda () (treesit-parser-create 'elisp)))
    )

  ;; (defun packages-load-after-minibuffer ()
  ;;   (when (file-exists-p "~/.emacs.d/site-lisp/emacs-which-key/which-key.el")
  ;;     (load "~/.emacs.d/site-lisp/emacs-which-key/which-key.el")
  ;;     (which-key-mode t)
  ;;     (setq which-key-max-description-length 30
  ;;         which-key-show-remaining-keys t)
  ;;     )
  ;;   (when (file-exists-p "~/.emacs.d/site-lisp/popper")
  ;;     (popper-mode +1))
  ;;   (remove-hook 'minibuffer-setup-hook 'packages-load-after-minibuffer))

  ;; (add-hook 'minibuffer-setup-hook 'packages-load-after-minibuffer)

  (when (file-exists-p "~/.emacs.d/site-lisp/emacs-which-key/which-key.el")
    (load "~/.emacs.d/site-lisp/emacs-which-key/which-key.el")
    (which-key-mode t)
    (setq which-key-max-description-length 30
          which-key-show-remaining-keys t)
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
     (floor (* (frame-width) 17) 35)
     ))

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
  (keymap-global-set "C-`" 'popper-toggle)
  (keymap-global-set "M-`" 'popper-cycle)
  (keymap-global-set "C-M-`" 'popper-toggle-type)
  (popper-mode +1)
  (popper-echo-mode +1)

  (define-key global-map [remap list-buffers] 'ibuffer)

  (add-hook 'prog-mode-hook 'indent-bars-mode)
  (with-eval-after-load 'indent-bars
    (setq ;; indent-bars-pattern "."
     ;; indent-bars-highlight-current-depth
     ;; '(:face default :blend 0.4)
     indent-bars-treesit-support t
     indent-bars-no-descend-string t
     indent-bars-treesit-ignore-blank-lines-types '("module")
     indent-bars-width-frac 0.15
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
    (setq xref-show-xrefs-function 'consult-xref
          xref-show-definitions-function 'consult-xref))

  ;;; Theme
  (dolist (hook '(prog-mode-hook text-mode-hook cuda-mode-hook))
    ;; (add-hook hook 'rainbow-mode)
    (add-hook hook 'colorful-mode)
    )
  ;; (load "~/.emacs.d/site-lisp/rainbow-delimiters/rainbow-delimiters.el")
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  ;; (require 'color-theme-sanityinc-tomorrow)
  ;; (color-theme-sanityinc-tomorrow-bright)
  ;; (color-theme-sanityinc-tomorrow-bright)

  ;; @ Minibuffer Setting
  ;; (load "~/.emacs.d/site-lisp/vertico/extensions/vertico-directory.el")

  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))))

  (defun vertico-lsp-enable ()
    ;; (and (functionp 'lsp-bridge-mode)
    ;;      (global-lsp-bridge-mode))
    (and (functionp 'corfu-mode)
         (global-corfu-mode))
    (and (boundp 'puni-mode)
         (puni-global-mode))
    (and (boundp 'vertico-mode)
         (progn
           (require 'orderless)
           (vertico-mode)
           (vertico-reverse-mode)
           (vertico-indexed-mode)
           (vertico-mouse-mode)
           (marginalia-mode)
           (keymap-set vertico-map "?" #'minibuffer-completion-help)
           (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
           (keymap-set vertico-map "M-TAB" #'minibuffer-complete)
           (keymap-set vertico-map "C-w" 'vertico-directory-delete-word))
         (keymap-set minibuffer-mode-map "C-w" 'backward-kill-word))
    (remove-hook 'pre-command-hook #'vertico-lsp-enable))

  (add-hook 'pre-command-hook #'vertico-lsp-enable)

  (with-eval-after-load 'vertico (setq vertico-cycle t))
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
    (setq citre-ctags-program "/usr/bin/ctags"
          citre-use-project-root-when-creating-tags t
          citre-prompt-language-for-ctags-command t))

  ;; (with-eval-after-load 'citre-ctags
  ;;   (setq citre-ctags-program "/usr/bin/ctags"))

  (when (boundp 'hl-todo)
    (global-hl-todo-mode))

  (with-eval-after-load 'highlight-indent-guides
    (setq highlight-indent-guides-method 'character
          highlight-indent-guides-responsive 'top
          highlight-indent-guides-suppress-auto-error t))
  )
