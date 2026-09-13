;;; Init.el --- Emacs Configuration --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:



;; Index:
;; 01 Startup and Bootstrap
;; 02 Core Behavior
;; 03 Editing Workflow
;; 04 Completion and Navigation
;; 05 Programming Languages
;; 06 Input and Reading
;; 07 Knowledge Management
;; 08 UI Themes and Platform

;; Add new settings under the nearest numbered section below.

;;; ============================================================================
;;; 01 Startup and Bootstrap
;;; ============================================================================

;;; Bootstrap

(defvar windows-system-p (eq system-type 'windows-nt)
  "Judge whether it's Windows system.")

(defvar non-android-p (not (eq system-type 'android))
  "Judge whether it isn't Android system.")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      load-prefer-newer t)

(load custom-file 'noerror 'nomessage)

(when (not non-android-p)
  (setopt tool-bar-position 'bottom)
  (setq touch-screen-display-keyboard t))

;;; Load path and core helpers
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-func)

(let ((packages (find-subdir-recursively (my/emacs-path "site-lisp"))))
  (setq load-path (append load-path packages)))

(dolist (path
         (mapcar #'my/emacs-path
                 '("site-lisp"
                   "site-lisp/treemacs/src/elisp"
                   "site-lisp/treemacs/src/extra/"
                   "site-lisp/lsp-mode/clients"
                   "site-lisp/lsp-mode/scripts"
                   "site-lisp/lsp-mode/docs"
                   "site-lisp/vertico/extensions"
                   "site-lisp/magit/lisp"
                   "site-lisp/with-editor/lisp"
                   "site-lisp/transient/lisp"
                   "site-lisp/themes/themes"
                   "site-lisp/pdf-tools/lisp")))
  (add-to-list 'load-path path))

;; Recent Standard Themes releases depend on the Modus support library.
;; NeoEmacs bundles it alongside the built-in themes, outside `load-path'.
(let ((built-in-themes-directory (expand-file-name "themes" data-directory)))
  (when (file-directory-p built-in-themes-directory)
    (add-to-list 'load-path built-in-themes-directory)))

(defun my/refresh-elisp-flymake-byte-compile-load-path ()
  "Keep Elisp Flymake byte compilation aligned with the current `load-path'."
  (setq elisp-flymake-byte-compile-load-path
        (cons "./"
              (mapcar #'expand-file-name load-path))))

(my/refresh-elisp-flymake-byte-compile-load-path)

(when (and non-android-p
           (not windows-system-p)
           (or (display-graphic-p) (daemonp)))
  (require 'exec-path-from-shell)
  (add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
  (exec-path-from-shell-initialize))


;;; Core settings
(setq set-mark-command-repeat-pop t
      other-window-scroll-default 'get-lru-window
      backup-directory-alist `(("." . ,(my/emacs-path "backup")))
      ;; package-quickstart nil
      nobreak-char-display nil)

(require 'init-diagnostics)

(require 'project)

;;; Icons

(with-eval-after-load 'treemacs
  (when my/use-nerd-icons-p
    (require 'treemacs-nerd-icons)
    (treemacs-load-theme "nerd-icons")))

(with-eval-after-load 'all-the-icons
  (load (my/emacs-path "self-develop/all-the-icons-diy.el")))

(require 'init-startup)

;;; ============================================================================
;;; 02 Core Behavior
;;; ============================================================================

;;; Core behavior

(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

(require 'elec-pair)
(require 'pixel-scroll)
(electric-pair-mode)

(pixel-scroll-precision-mode)

(setq pixel-scroll-precision-interpolate-page t
      pixel-scroll-precision-use-momentum nil
      redisplay-skip-fontification-on-input t)

(defun my/scroll-command-with-remap (command)
  "Call COMMAND while honoring any active command remapping."
  (let ((remapped (or (command-remapping command) command)))
    (call-interactively remapped)))

(defun my/scroll-down-centralize ()
  "Scroll down one page and recenter, similar to emacs-solo."
  (interactive)
  (my/scroll-command-with-remap 'scroll-up-command)
  (recenter))

(defun my/scroll-up-centralize ()
  "Scroll up one page and keep the viewport centered when possible."
  (interactive)
  (my/scroll-command-with-remap 'scroll-down-command)
  (unless (= (window-start) (point-min))
    (recenter))
  (when (= (window-start) (point-min))
    (let ((midpoint (/ (window-height) 2)))
      (goto-char (window-start))
      (forward-line midpoint)
      (recenter midpoint))))

(keymap-global-set "C-v" #'my/scroll-down-centralize)
(keymap-global-set "M-v" #'my/scroll-up-centralize)

(global-subword-mode)

(require 'autorevert)
(global-auto-revert-mode)

(setq global-auto-revert-non-file-buffers t
      auto-revert-interval 1)
(require 'hl-line)
(setq hl-line-sticky-flag nil)
(require 'init-editing)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
      scroll-preserve-screen-position t
      scroll-margin 5
      scroll-conservatively 8
      eldoc-idle-delay 0.2)

(defun my/term-reset-scrolling-vars ()
  "Use emacs-solo-like local scrolling behavior in term buffers."
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (setq-local scroll-step 0))

(defun my/eshell-reset-scrolling-vars ()
  "Use emacs-solo-like local scrolling behavior in Eshell."
  (setq-local scroll-conservatively 0)
  (setq-local scroll-margin 0))

(add-hook 'term-mode-hook #'my/term-reset-scrolling-vars)
(add-hook 'eshell-mode-hook #'my/eshell-reset-scrolling-vars)

(delete-selection-mode)

(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      show-paren-delay 0
      show-paren-context-when-offscreen 'child-frame
      blink-matching-paren-highlight-offscreen t
      show-paren--context-child-frame-parameters
      '((visibility)
        (width . 0)
        (height . 0)
        (min-width . t)
        (min-height . t)
        (no-accept-focus . t)
        (no-focus-on-map . t)
        (border-width . 0)
        (child-frame-border-width . 1)
        (left-fringe . 0)
        (right-fringe . 0)
        (vertical-scroll-bars)
        (horizontal-scroll-bars)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (tab-bar-lines . 0)
        (no-other-frame . t)
        (no-other-window . t)
        (no-delete-other-windows . t)
        (unsplittable . t)
        (undecorated . t)
        (cursor-type)
        (no-special-glyphs . t)
        (desktop-dont-save . t)
        (child-frame-border-width 3)))

(require 'super-save)

(setq auto-save-default nil
      save-silently t
      auto-save-no-message t
      super-save-silent t
      super-save-auto-save-when-idle t
      super-save-idle-duration 5
      super-save-when-focus-lost t
      super-save-when-buffer-switched t
      super-save-delete-trailing-whitespace nil
      super-save-exclude '(".gpg\\'"))

(auto-save-visited-mode -1)
(super-save-mode 1)

(dolist (hook '(TeX-mode-hook dired-mode-hook markdown-mode-hook markdown-view-mode-hook))
  (add-hook hook #'(lambda ()
                     (setq truncate-lines t))))


(repeat-mode)

(require 'no-littering)
(require 'recentf)

(with-eval-after-load 'no-littering
  (recentf-mode t)
  (setq recentf-max-saved-items 1000
        recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'"))
        yas-snippet-dirs (list (my/emacs-path "snippets")))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(with-eval-after-load 'pdf-view
  (require 'saveplace-pdf-view))

(save-place-mode t)

(require 'savehist)
(setq history-length 1000
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      savehist-additional-variables '(search-ring regexp-search-ring kill-ring))

(savehist-mode t)

(setq recentf-max-menu-items 5
      ring-bell-function 'ignore
      isearch-lazy-count t
      isearch-wrap-pause 'no
      lazy-highlight-cleanup nil
      ;; 处理中英文断行不分割问题，需要开启 toggle-word-wrap 和
      ;; visual-line-mode 才能体现
      word-wrap-by-category t
      read-process-output-max (* 4 1024 1024)
      save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      ffap-machine-p-known 'reject
      window-combination-resize t)

(setq-default tab-width 4
              tab-always-indent t
              tab-first-completion 'word-or-paren-or-punct
              indent-tabs-mode nil
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              ;; display-fill-column-indicator-character 124
              fringe-indicator-alist
              '((truncation () right-arrow)
                (continuation () right-curly-arrow)
                (overlay-arrow . right-triangle)
                (up . up-arrow)
                (down . down-arrow)
                (top top-left-angle top-right-angle)
                (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                (empty-line . empty-line)
                (unknown . question-mark))
              ;; visual-fill-column-center-text t
              )

(editorconfig-mode 1)

(dolist (mode
         '(prog-mode-hook
           toml-ts-mode-hook
           TeX-mode-hook
           cuda-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode t))))

(require 'desktop)
(desktop-save-mode 1)

(setq desktop-restore-frames nil)

(setq comment-auto-fill-only-comments t)

(require 'whitespace)
(setq whitespace-style '(face trailing))

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

(autoload 'symbol-overlay-mode "symbol-overlay" nil t)
(add-hook 'prog-mode-hook #'symbol-overlay-mode)


;; Smoothly scrolling over image
(unless (>= (string-to-number emacs-version) 30)
  (add-hook
   'text-mode-hook
   (lambda ()
     (or (boundp 'iscroll-mode)
         (load (my/emacs-path "site-lisp/iscroll/iscroll.el")))
     (iscroll-mode))))

(defvar magit-diff-refine-hunk)
(defvar magit-log-section-commit-count)
(defvar magit-auto-revert-counter)
(defvar magit-status-sections-hook)
(defvar magit-display-buffer-function)

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (setq magit-diff-refine-hunk t
        magit-log-section-commit-count 20
        magit-auto-revert-counter 10
        magit-status-sections-hook
        '(magit-insert-status-headers
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
          magit-insert-unpulled-from-upstream)
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(require 'aggressive-indent)

(dolist (hook '(emacs-lisp-mode-hook yuck-mode-hook))
  (add-hook hook 'aggressive-indent-mode))

;;; ============================================================================
;;; 03 Editing Workflow
;;; ============================================================================

;;; Meow
(require 'meow)

(meow-setup)

(meow-global-mode 1)

(setq meow-use-cursor-position-hack t
      meow-use-clipboard t
      meow-use-enhanced-selection-effect t
      meow--kbd-kill-region "M-w"
      meow--kbd-kill-ring-save "C-w")

;;; Keybindings

(require 'puni)
(require 'vundo)
(require 'avy)

(keymap-global-set "M-p" 'pop-to-mark-command)

(keymap-global-set "M-o" 'other-window)

(unless (bound-and-true-p meow-mode)
  (progn
    (keymap-global-set "M-j" 'open-newline-above)
    (keymap-global-set "C-j" 'open-newline-below)
    (keymap-set isearch-mode-map "M-j" 'avy-isearch)
    ))

(keymap-global-set "M-'" 'avy-goto-char-in-line)

(keymap-global-set "C-s" 'isearch-forward-regexp)

(keymap-global-set "C-r" 'isearch-backward-regexp)

(keymap-global-set "C-k" 'smart-kill-line)

(keymap-global-set "C-w" 'kill-or-save)

(keymap-set isearch-mode-map "C-h" 'isearch-del-char)

(keymap-set isearch-mode-map "C-'" 'avy-isearch)

(keymap-global-set "C-h" 'backward-delete-char-untabify)

(keymap-global-set "C-x k" 'kill-current-buffer)

(keymap-global-set "C-x C-r" 'restart-emacs)

(keymap-global-set "C-c g" 'consult-ripgrep)

(keymap-global-set "C-c f" 'consult-fd)

;; Efficiency
(keymap-global-set "C-x f" 'find-file)

(keymap-global-set "C-z" 'vundo)

(global-set-key [remap comment-dwim] 'comment-line)

(require 'subr-x)
(require 'newcomment)

(defun my/section-comment ()
  "Insert a section comment with current mode's comment style."
  (interactive)
  (comment-normalize-vars)
  (let* ((indent (make-string (current-indentation) ?\s))
         (start (string-trim-right comment-start))
         (end (let ((e (string-trim (or comment-end ""))))
                (if (string-empty-p e) "" (concat " " e))))
         (bar (make-string 30 ?-))
         title-pos)
    (insert start " " bar end "\n")
    (insert indent start " ")
    (setq title-pos (point))
    (insert end "\n")
    (insert indent start " " bar end)
    (goto-char title-pos)))

(global-set-key (kbd "C-c s") #'my/section-comment)

(defun my/c-c++-line-comment-setup ()
  "Use // comments as the default line comment style in C/C++ buffers."
  (setq-local comment-start "// "
              comment-end ""))

(dolist (hook '(c-mode-hook
                c-ts-mode-hook
                c++-mode-hook
                c++-ts-mode-hook))
  (add-hook hook #'my/c-c++-line-comment-setup))

;;; Fingertip
(defvar fingertip-mode-map)

(with-eval-after-load 'fingertip
  ;; 移动
  ;; 符号插入
  (keymap-set fingertip-mode-map "%" 'fingertip-match-paren)   ;括号跳转
  (keymap-set fingertip-mode-map "(" 'fingertip-open-round)    ;智能 (
  (keymap-set fingertip-mode-map "[" 'fingertip-open-bracket)  ;智能 [
  (keymap-set fingertip-mode-map "{" 'fingertip-open-curly)    ;智能 {
  (keymap-set fingertip-mode-map ")" 'fingertip-close-round)   ;智能 )
  (keymap-set fingertip-mode-map "]" 'fingertip-close-bracket) ;智能 ]
  (keymap-set fingertip-mode-map "}" 'fingertip-close-curly)   ;智能 }
  (keymap-set fingertip-mode-map "\"" 'fingertip-double-quote) ;智能 "
  (keymap-set fingertip-mode-map "'" 'fingertip-single-quote)  ;智能 '
  (keymap-set fingertip-mode-map "=" 'fingertip-equal)         ;智能 =
  (keymap-set fingertip-mode-map "SPC" 'fingertip-space)       ;智能 space
  (keymap-set fingertip-mode-map "RET" 'fingertip-newline)     ;智能 newline
  ;; 删除
  ;; 包围
  ;; 跳出并换行缩进
  ;; 向父节点跳动
  )

;;; Helpful

(require 'which-key)
(which-key-mode)
(setq help-window-select t)
(require 'helpful)

(keymap-global-set "C-?" 'help-command)

(with-eval-after-load 'help
  (define-key global-map [remap describe-function] 'helpful-function)
  (define-key global-map [remap describe-key] 'helpful-key)
  (define-key global-map [remap describe-variable] 'helpful-variable)
  (define-key global-map [remap describe-command] 'helpful-command))

(with-eval-after-load 'init-func
  (define-key global-map [remap upcase-word] 'upcase-any)
  (define-key global-map [remap downcase-word] 'downcase-any)
  (define-key global-map [remap capitalize-word] 'capitalize-any))

;;; Hydra
(require 'hydra)

(defhydra hydra-avy (global-map "M-g"
                                :exit t
                                :hint nil)
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

(defhydra hydra-zoom (global-map "C-c z")
  "Zoom."
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
  ("v" scroll-up nil)
  ("V" scroll-down nil)
  ("l" forward-char nil)
  ("h" backward-char nil)
  ("F" forward-word nil)
  ("B" backward-word nil)
  ("q" nil "quit"))

(keymap-global-set "C-M-;" 'hydra-eMove/body)

(require 'rect)

(defhydra hydra-rectangle
  (:body-pre (rectangle-mark-mode t)
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
         (rectangle-mark-mode t))
   nil)
  ("y" kill-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" yank-rectangle nil)
  ("o" nil nil))

(keymap-global-set "C-x SPC" 'hydra-rectangle/body)



;;; ============================================================================
;;; 04 Completion and Navigation
;;; ============================================================================

;;; Completion / LSP

(require 'markdown-mode)

(define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do)

(add-to-list 'load-path (my/emacs-path "site-lisp/corfu/extensions"))
(my/refresh-elisp-flymake-byte-compile-load-path)

(require 'corfu)
(require 'corfu-popupinfo)
(require 'yasnippet)
(require 'yasnippet-capf)

(yas-global-mode 1)

(with-eval-after-load 'tempel
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))


(require 'cape)

(add-hook 'completion-at-point-functions #'cape-file)

(lsp-enable-startup)

(with-eval-after-load 'corfu
  (setq corfu-auto t
        corfu-cycle t
        ;; corfu-quit-no-match 'separator  ;; t
        corfu-auto-prefix 2
        corfu-auto-delay 0.1
        corfu-preview-current t
        corfu-on-exact-match 'show
        ;; corfu-quit-no-match t
        ;; corfu-preselect 'prompt
        ;; corfu-quit-at-boundary t
        )
  (keymap-set corfu-map "<tab>" 'corfu-insert)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  (with-eval-after-load 'corfu-popupinfo
    (setq corfu-popupinfo-delay '(2.0 . 0.5)))
  (with-eval-after-load 'kind-icon
    (setq corfu-margin-formatters '(kind-icon-margin-formatter)))
  (with-eval-after-load 'nerd-icons-corfu
    (when my/use-nerd-icons-p
      (setq corfu-margin-formatters '(nerd-icons-corfu-formatter))))
  )

(defun yas-setup-capf ()
  "Set capf for yasnippets."
  (setq-local completion-at-point-functions
              (cons #'yasnippet-capf
                    (remove #'yasnippet-capf completion-at-point-functions))))

(add-hook 'prog-mode-hook 'yas-setup-capf)

(defun my/verilog-completion-setup ()
  "Enable Corfu and YASnippet completion reliably in Verilog buffers."
  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode 1))
  (yas-setup-capf)
  (when (fboundp 'corfu-mode)
    (setq-local corfu-auto t)
    (corfu-mode 1)))

(dolist (hook '(verilog-mode-hook verilog-ts-mode-hook))
  (add-hook hook #'my/verilog-completion-setup))

(defun my/verilog-eglot-completion-setup ()
  "Restore Verilog snippet CAPF after Eglot adds its completion backend."
  (when (derived-mode-p 'verilog-mode 'verilog-ts-mode)
    (my/verilog-completion-setup)))

(add-hook 'eglot-managed-mode-hook #'my/verilog-eglot-completion-setup)

(setq yas-prompt-functions '(yas-no-prompt))

(require 'eglot)
(autoload 'dape "dape" nil t)
(require 'apheleia)

(defconst my/verilog-format-command "verible-verilog-format"
  "External formatter used for Verilog and SystemVerilog buffers.")

(setf (alist-get 'verible-verilog-format apheleia-formatters)
      `(,my/verilog-format-command
        "--stdin_name" filepath
        "--column_limit=100"
        "--indentation_spaces=2"
        "--assignment_statement_alignment=align"
        "--module_net_variable_alignment=align"
        "--port_declarations_alignment=align"
        "--port_declarations_right_align_packed_dimensions=true"
        "--named_port_alignment=align"
        "--named_parameter_alignment=align"
        "--struct_union_members_alignment=align"
        "--case_items_alignment=align"
        "--formal_parameters_alignment=align"
        "--formal_parameters_indentation=indent"
        "--port_declarations_indentation=indent"
        "-"))

(dolist (mode '(verilog-mode verilog-ts-mode))
  (setf (alist-get mode apheleia-mode-alist) 'verible-verilog-format))

(defun my/verilog-external-format-setup ()
  "Enable external formatting for Verilog and SystemVerilog buffers."
  (when (executable-find my/verilog-format-command)
    (apheleia-mode 1)))

(dolist (hook '(verilog-mode-hook verilog-ts-mode-hook))
  (add-hook hook #'my/verilog-external-format-setup))

(defun my/format-buffer ()
  "Format the current buffer using the configured formatter."
  (interactive)
  (if (derived-mode-p 'verilog-mode 'verilog-ts-mode)
      (progn
        (unless (executable-find my/verilog-format-command)
          (user-error "Cannot find %s" my/verilog-format-command))
        (apheleia-format-buffer 'verible-verilog-format))
    (eglot-format-buffer)))

(with-eval-after-load 'eglot
  (setq my/pyright-uvx-command '("pyright-langserver" "--stdio"))
  (add-to-list
   'eglot-server-programs
   '((verilog-mode verilog-ts-mode)
     .
     ("slang-server")))
  (add-to-list
   'eglot-server-programs
   '((scala-mode scala-ts-mode) . ("metals-emacs")))
  ;; (add-to-list 'eglot-server-programs `((LaTeX-mode latex-mode tex-mode) . ("texlab")))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (setq project-vc-extra-root-markers '("Cargo.toml" ".dir-locals.el" "build.mill"))
  (setq eglot-send-changes-idle-time 0.2
        eglot-code-action-indications '(eglot-hint))
  )

;;; Minibuffer / Completion UI
(dolist (feature
         '(vertico
           vertico-grid
           vertico-directory
           vertico-reverse
           vertico-indexed
           vertico-mouse
           vertico-buffer
           vertico-multiform
           vertico-sort
           vertico-suspend
           vertico-repeat
           embark
           marginalia
           standard-themes
           rainbow-delimiters
           visual-fill-column
           colorful-mode
           indent-bars
           indent-bars-ts
           symbol-overlay
           aggressive-indent
           orderless))
  (require feature))

(defun my/marginalia-safe-file-owner (original attributes)
  "Call ORIGINAL with ATTRIBUTES without failing on invalid Windows GIDs."
  (condition-case nil
      (funcall original attributes)
    (error nil)))

(when (and windows-system-p
           (not (advice-member-p #'my/marginalia-safe-file-owner
                                 'marginalia--file-owner)))
  (advice-add 'marginalia--file-owner
              :around #'my/marginalia-safe-file-owner))

;; (setq-default completion-styles '(basic partial-completion orderless))
(setq-default completion-styles '(orderless flex basic))
(setf (alist-get 'file completion-category-overrides)
      '((styles partial-completion basic))
      (alist-get 'eglot completion-category-overrides)
      '((styles flex basic)))

(require 'wgrep)
(setq consult-ripgrep-args
      "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
 --smart-case --no-heading --with-filename --line-number --search-zip --no-ignore-parent")

;; (setq completion-styles '(basic partial-completion orderless)
;;       completion-category-overrides '((file (styles basic partial-completion))))

(if (boundp 'vertico-mode)
    (progn
      (require 'orderless)
      (setq enable-recursive-minibuffers t)
      (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
      (vertico-mode)
      (vertico-reverse-mode)
      (vertico-indexed-mode)
      (vertico-mouse-mode)
      (vertico-multiform-mode)
      (marginalia-mode)
      (setq vertico-multiform-categories
            '((file
               (vertico-sort-function . vertico-sort-directories-first)
               (:keymap . vertico-directory-map))
              (consult-grep buffer)
              (consult-ripgrep buffer))
            vertico-cycle t
            vertico-sort-function 'vertico-sort-history-alpha)
      (keymap-global-set "C-'" #'vertico-suspend)
      (keymap-set vertico-map "?" #'minibuffer-completion-help)
      (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
      (keymap-set vertico-map "M-TAB" #'minibuffer-complete)
      (keymap-set vertico-map "C-w" 'vertico-directory-delete-word)
      (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
      (keymap-global-set "C-c v" #'vertico-repeat)
      )
  (keymap-set minibuffer-mode-map "C-w" 'backward-kill-word)
  )

(and (boundp 'puni-mode)
     (require 'puni)
     (puni-global-mode))

(with-eval-after-load 'puni
  (add-hook
   'puni-mode-hook
   (lambda ()
     (keymap-set puni-mode-map "M-w" 'puni-kill-region)
     (keymap-set puni-mode-map "M-k" 'puni-backward-kill-line)
     (keymap-unset puni-mode-map "C-w")))
  (dolist (hook '(term-mode-hook minibuffer-mode-hook))
    (add-hook hook #'puni-disable-puni-mode)))

;;; Visual Replacement
(keymap-global-set "C-c r" 'replace-regexp)

(require 'hideshow)

(dolist (hook
         '(emacs-lisp-mode-hook
           c-mode-hook
           c-ts-mode-hook
           c++-mode-hook
           c++-ts-mode-hook
           python-mode-hook
           python-ts-mode-hook
           rust-ts-mode-hook
           verilog-mode-hook
           verilog-ts-mode-hook))
  (add-hook hook 'hs-minor-mode))

;; 折叠代码块，以下是额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face '(:inherit font-lock-comment-face
                                 :box t))

(setq hs-set-up-overlay 'hideshow-folded-overlay-fn)

(keymap-set hs-minor-mode-map "C-<tab>" 'hs-toggle-hiding)

(require 'mwim)

(global-set-key [remap move-beginning-of-line] 'mwim-beginning-of-code-or-line-or-comment)

(global-set-key [remap move-end-of-line] 'mwim-end-of-code-or-line)

(require 'popper)
(require 'popper-echo)

;; Keep popup management close to the common public Popper setups:
;; a small popup allowlist, project grouping, and standard global keys.
(setq popper-reference-buffers
      '("\\*Messages\\*"
        "\\*Warnings\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*Compile-Log\\*"
        "\\*xref\\*"
        "\\*tex-shell\\*"
        "Go-Translate"
        help-mode
        helpful-mode
        compilation-mode
        flymake-diagnostics-buffer-mode
        youdao-dictionary-mode)
      popper-group-function #'popper-group-by-project
      popper-display-control t
      popper-tab-line-mode nil
      ;; Keep Popper from prepending its raw "POP" token; Segment Line renders it.
      popper-mode-line "")

(keymap-global-set "C-`" #'popper-toggle)
(keymap-global-set "M-`" #'popper-cycle)
(keymap-set window-prefix-map "p" #'popper-toggle-type)

(add-hook 'after-init-hook #'popper-mode)
(add-hook 'after-init-hook #'popper-echo-mode)

(define-key global-map [remap list-buffers] 'ibuffer)

(when non-android-p
  (require 'indent-bars)
  (add-hook 'prog-mode-hook 'indent-bars-mode))

(with-eval-after-load 'indent-bars
  (setq ;; indent-bars-pattern "."
   ;; indent-bars-highlight-current-depth
   ;; '(:face default :blend 0.4)
   indent-bars-treesit-support t
   indent-bars-no-descend-string t
   indent-bars-no-descend-lists t
   indent-bars-treesit-ignore-blank-lines-types '("module")
   indent-bars-width-frac 0.2
   indent-bars-color '(highlight :face-bg t :blend 0.7)
   indent-bars-display-on-blank-lines t)
  (add-hook 'verilog-mode-hook
            (lambda ()
              (setq-local indent-bars-spacing-override verilog-indent-level)))
  (add-hook 'verilog-ts-mode-hook
            (lambda ()
              (setq-local indent-bars-spacing-override verilog-ts-indent-level))))

(require 'consult)
(require 'consult-xref)
(require 'consult-imenu)
(require 'consult-eglot)
(require 'project)

(autoload 'eldoc-box-help-at-point "eldoc-box" nil t)

(keymap-global-set "C-x l" 'consult-line)

;; `consult-buffer' 同时展示 buffer 和虚拟 file source：
;; - `B SPC' 仅看当前 project 的 buffers
;; - `F SPC' 仅看当前 project 的 files/recent files
;; - `p SPC' 同时看当前 project 相关 source（buffers/files/roots）
(keymap-global-set "C-x b" 'consult-buffer)

(with-eval-after-load 'consult
  (consult-customize consult-buffer :preview-key '(:debounce 0.4 "M-."))
  (setq consult-project-function
        (lambda (may-prompt) (or (vc-root-dir) (consult--default-project-function may-prompt)))))

;; (add-to-list 'display-buffer-alist
;;              '((category . xref-jump)
;;                (display-buffer-reuse-window
;;                 display-buffer-in-previous-window
;;                 display-buffer-same-window)
;;                (reusable-frames . nil)
;;                (inhibit-switch-frame . t)))

(setq xref-show-xrefs-function 'consult-xref
      xref-show-definitions-function 'consult-xref)

(keymap-global-set "M-." 'xref-find-definitions)
(keymap-global-set "M-," 'xref-go-back)
(keymap-global-set "C-M-," 'xref-go-forward)
(keymap-global-set "M-?" 'xref-find-references)

(keymap-set project-prefix-map "b" 'consult-project-buffer)

(keymap-global-set "C-c l d" 'xref-find-definitions)
(keymap-global-set "C-c l r" 'xref-find-references)
(keymap-global-set "C-c l b" 'xref-go-back)
(keymap-global-set "C-c h" 'eldoc-box-help-at-point)
(keymap-global-set "C-c l h" 'eldoc-box-help-at-point)
(keymap-global-set "C-c l a" 'eglot-code-actions)
(keymap-global-set "C-c l R" 'eglot-rename)
(keymap-global-set "C-c l s" 'consult-eglot-symbols)
(keymap-global-set "C-c l e" 'flymake-show-buffer-diagnostics)
(keymap-global-set "C-c l f" 'my/format-buffer)

(keymap-global-set "C-." 'embark-act)

(with-eval-after-load 'embark
  (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))

;;; Appearance helpers
(dolist (hook '(prog-mode-hook text-mode-hook cuda-mode-hook))
  (add-hook hook 'colorful-mode))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)



;;; ============================================================================
;;; 05 Programming Languages
;;; ============================================================================

;;; Language modes / Treesit
(setq elisp-fontify-semantically t)

(require 'scala-mode)
(require 'scala-ts-mode)
(require 'kdl-mode)
(require 'yuck-mode)

(when (treesit-available-p)
  (require 'treesit-auto)
  (setq treesit-font-lock-level 4
        treesit-auto-install 'prompt
        treesit-auto-langs '(bash
                             c
                             cpp
                             css
                             go
                             java
                             javascript
                             json
                             lua
                             python
                             ruby
                             rust
                             scala
                             toml
                             tsx
                             typescript
                             yaml
                             verilog))
  (treesit-auto-add-to-auto-mode-alist treesit-auto-langs)
  (global-treesit-auto-mode 1)
  (require 'qml-ts-mode))

(defun my/open-pdf-with-external-app ()
  "Open the current PDF with the system's external PDF application."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a PDF file"))
  (let ((file (expand-file-name buffer-file-name)))
    (cond
     (windows-system-p
      (w32-shell-execute "open" file))
     ((eq system-type 'darwin)
      (start-process "open-pdf" nil "open" file))
     ((executable-find "xdg-open")
      (start-process "open-pdf" nil "xdg-open" file))
     (t
      (user-error "No external PDF opener found"))))
  (run-at-time 0 nil #'kill-buffer (current-buffer)))

(defun my/open-pdf ()
  "Open PDFs with `pdf-view-mode' in GUI Linux, otherwise use an external app."
  (interactive)
  (if (and (eq system-type 'gnu/linux) (display-graphic-p))
      (pdf-view-mode)
    (my/open-pdf-with-external-app)))

(dolist (entry
         `(("\\.pdf\\'" . my/open-pdf)
           ("\\.scala\\'" . my/scala-mode)
           ("\\.sbt\\'" . my/scala-mode)
           ("\\.sc\\'" . my/scala-mode)
           ("\\.mill\\'" . my/scala-mode)
           ("\\.kdl\\'" . kdl-mode)
           ("\\.do\\'" . tcl-mode)
           ("\\.xdc\\'" . tcl-mode)
           ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
           ("README\\.md\\'" . gfm-mode)))
  (add-to-list 'auto-mode-alist entry))

(with-eval-after-load 'typescript-ts-mode
  (setq typescript-ts-mode-indent-offset 4))

(with-eval-after-load 'css-mode
  (setq css-indent-offset 4))

;;; Scala / Chisel
(setq scala-ts-indent-offset 2)

(with-eval-after-load 'scala-mode
  (setq scala-indent:step 2
        scala-indent:align-parameters nil
        scala-indent:align-forms nil
        scala-indent:indent-value-expression nil))

(dolist (hook '(scala-mode-hook scala-ts-mode-hook))
  (add-hook hook
            (lambda ()
              (setq-local tab-width 2
                          indent-tabs-mode nil
                          indent-bars-spacing-override 2))))

(require 'jarchive)
(jarchive-mode 1)

;;; Verilog
(require 'verilog-ts-mode)

(setq verilog-indent-level 2
      verilog-indent-level-declaration 2
      verilog-indent-level-module 2
      verilog-indent-level-behavioral 2
      verilog-indent-level-directive 0
      verilog-case-indent 2
      verilog-cexp-indent 2
      verilog-indent-lists nil
      verilog-auto-newline nil
      verilog-ts-indent-level 2)

(defun my/verilog-indent-setup ()
  "Keep Verilog buffer indentation aligned with verible-verilog-format."
  (setq-local tab-width 2
              indent-tabs-mode nil))

(dolist (hook '(verilog-mode-hook verilog-ts-mode-hook))
  (add-hook hook #'my/verilog-indent-setup))

(defconst verilog-hs-block-start-keywords-re
  (eval-when-compile
    (concat
     "\\(" "\\(" (regexp-opt '("(" "{" "[")) "\\)" "\\|" "\\("
     (verilog-regexp-words
      '("begin"
        "fork"
        "clocking"
        "function"
        "module"
        "covergroup"
        "property"
        "task"
        "generate"
        "`ifdef"
        "`ifndef"))
     "\\)" "\\)")))

(defconst verilog-hs-block-end-keywords-re
  (eval-when-compile
    (concat
     "\\(" "\\(" (regexp-opt '(")" "}" "]")) "\\)" "\\|" "\\("
     (verilog-regexp-words
      '("end"
        "join"
        "join_any"
        "join_none"
        "endclocking"
        "endfunction"
        "endmodule"
        "endgroup"
        "endproperty"
        "endtask"
        "endgenerate"
        "`endif"))
     "\\)" "\\)")))

(defun verilog-ext-hs-setup ()
  "Configure `hideshow'."
  (dolist (mode
           '((verilog-mode . verilog-forward-sexp-function)
             (verilog-ts-mode . verilog-ts-forward-sexp)))
    (add-to-list
     'hs-special-modes-alist
     `(,(car mode)
       ,verilog-hs-block-start-keywords-re
       ,verilog-hs-block-end-keywords-re
       nil
       ,(cdr mode)))))

(verilog-ext-hs-setup)


;;; ============================================================================
;;; 06 Input and Reading
;;; ============================================================================

;;; Dired

(setq dired-mouse-drag-files t
      mouse-drag-and-drop-region-cross-program t
      delete-by-moving-to-trash t
      dired-movement-style 'cycle
      dired-omit-mode t)

(add-to-list 'load-path (my/emacs-path "site-lisp/dirvish/extensions"))
(my/refresh-elisp-flymake-byte-compile-load-path)
(require 'dirvish)
(require 'dirvish-side)

(dirvish-override-dired-mode)

(with-eval-after-load 'dirvish
  (setq dirvish-attributes (if my/use-nerd-icons-p
                               '(vc-state nerd-icons git-msg file-size subtree-state collapse file-time)
                             '(vc-state git-msg file-size subtree-state collapse file-time))
        dirvish-side-attributes (if my/use-nerd-icons-p
                                    '(vc-state nerd-icons subtree-state collapse)
                                  '(vc-state subtree-state collapse))
        )
  (keymap-set dirvish-mode-map "TAB" #'dirvish-toggle-subtree))

(when (and my/use-nerd-icons-p
           (not (bound-and-true-p dirvish-override-dired-mode)))
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode))

;;; Chinese

(setq cns-recent-segmentation-limit 20
      cns-debug nil
      cns-process-type 'shell)

(let ((cns-root (my/emacs-path "site-lisp/emacs-chinese-word-segmentation")))
  (if windows-system-p
      (setq cns-cmdproxy-shell-path "D:/App/cygwin64/bin/bash.exe"
            cns-prog (my/windows-path-to-cygwin
                      (expand-file-name "cnws.exe" cns-root))
            cns-dict-directory (my/windows-path-to-cygwin
                                (expand-file-name "cppjieba/dict" cns-root)))
    (setq cns-prog (expand-file-name "cnws" cns-root)
          cns-dict-directory (expand-file-name "cppjieba/dict" cns-root))))

(when non-android-p
  (require 'cns nil t)
  (when (featurep 'cns)
    (add-hook 'find-file-hook 'cns-auto-enable)))

;;; Input method

(with-eval-after-load 'rime
  (set-face-attribute 'rime-preedit-face nil
                      :underline t
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
                 (or (= (point)
                        (line-beginning-position))
                     (= #xff0c (char-before))
                     (= #x3001 (char-before))
                     (= #x3002 (char-before))
                     (= #xff08 (char-before))
                     (= #xff1b (char-before))
                     (= #x20 (char-before))
                     (rime-predicate-after-ascii-char-p)))
            (and (> (point)
                    (save-excursion
                      (back-to-indentation)
                      (point)))
                 (let ((string
                        (buffer-substring (point)
                                          (max (line-beginning-position)
                                               (- (point) 80)))))
                   (or (or (string-match-p "[\x5c][\x21-\x24\x26-\x7a\x7c\x7e]*$" string)
                           (if (string-match-p "[\x5c][a-zA-Z\x23\x40]+[\x7b][^\x7d\x25]*$" string)
                               (if (and (string-match-p
                                         "[\x5c]\\(begin\\)\\|\\(end\\)[\x7b]" string)
                                        (= (char-before) #x7b))
                                   t
                                 (if (> (char-before) #x7b)
                                     (and rime--current-input-key
                                          (or (= #x7e rime--current-input-key)
                                              (= #x7c rime--current-input-key)))
                                   ())
                                 ())))
                       (string-match-p
                        "[a-zA-Z][0-9\x21-\x23\x25-\x2f\x3a-\x40\x5b-\x60\x7a\x7c\7e\x7f]*$"
                        string)))))
      (rime-predicate-after-ascii-char-p)))
  (keymap-set rime-mode-map "s-`" 'rime-send-keybinding)
  (keymap-set rime-mode-map "C-`" 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "C-t") 'rime-inline-ascii)
  (define-key minibuffer-mode-map (kbd "C-t") 'rime-inline-ascii)
  (setq default-input-method "rime"
        rime-user-data-dir (my/emacs-path "rime")
        rime-show-candidate 'posframe
        rime-show-preedit 't
        rime-translate-keybindings
        '("C-f"
          "C-b"
          "C-n"
          "C-p"
          "C-g"
          "<left>"
          "<right>"
          "<up>"
          "<down>"
          "<prior>"
          "<next>"
          "<delete>"
          "C-h")
        rime-posframe-properties (list :internal-border-width 3)
        rime-posframe-style 'vertical
        rime-disable-predicates
        '(rime-predicate-space-after-cc-p
          rime-predicate-current-uppercase-letter-p
          ;; rime-predicate-after-alphabet-char-p
          ;; rime-predicate-after-ascii-char-p
          rime-predicate-prog-in-code-p rime-predicate-hydra-p
          ;; rime-predicate-evil-mode-p
          rime-predicate-meow-mode-p rime-predicate-tex-advance-p)
        ;; rime-deactivate-when-exit-minibuffer t
        rime-inline-ascii-trigger 'shift-l)
  (keymap-set rime-mode-map "M-o" 'rime-force-enable)
  )

(defun rime-commit1-and-toggle-input-method ()
  "Commit the 1st item if exists, then toggle input method."
  (interactive)
  (require 'rime)
  (ignore-errors
    (rime-commit1))
  (toggle-input-method))

(keymap-global-set "C-\\" 'rime-commit1-and-toggle-input-method)


;;; Reading / Documents
(dolist (feature '(eldoc-box eldoc-mouse shrface))
  (require feature))
(autoload 'calibredb "calibredb" nil t)
(autoload 'nov-mode "nov" nil t)

(unless windows-system-p
  (require 'nov-xwidget nil t))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(with-eval-after-load 'nov
  (setq nov-text-width t)
  (add-hook 'nov-mode-hook #'eldoc-mode)
  (add-hook 'nov-mode-hook #'eldoc-box-hover-mode)
  (add-hook 'nov-mode-hook #'visual-line-mode)
  (add-hook
   'nov-mode-hook
   #'(lambda ()
       (setq-local line-spacing 0.25)
       (face-remap-add-relative 'variable-pitch
        :family "Charter"
        :height 1.2))))

(when (eq system-type 'gnu/linux)
  (dolist (feature
           '(pdf-tools
             pdf-roll
             pdf-history
             pdf-view
             pdf-occur
             pdf-isearch
             pdf-links
             pdf-outline
             pdf-misc
             pdf-annot
             pdf-sync
             pdf-cache))
    (require feature))
  (add-hook 'pdf-view-mode-hook 'pdf-history-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-isearch-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-links-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-outline-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-misc-size-indication-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-misc-context-menu-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-sync-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-cache-prefetch-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-view-roll-minor-mode))



;;; ============================================================================
;;; 07 Knowledge Management
;;; ============================================================================

;;; Org
(dolist (path
         (mapcar #'my/emacs-path
                 '("site-lisp/org-roam"
                   "site-lisp/org-modern-indent"
                   "site-lisp/org-modern"
                   "site-lisp/org-appear"
                   "site-lisp/org-bars"
                   "site-lisp/emacsql"
                   "site-lisp/org-visual-outline")))
  (add-to-list 'load-path path))
(my/refresh-elisp-flymake-byte-compile-load-path)

(require 'org)
(require 'valign)
(require 'org-appear)
(require 'org-modern)
(require 'org-modern-indent)
(require 'olivetti)
(require 'mixed-pitch)

(setq olivetti-style 'fancy
      olivetti-margin-width 5)

(with-eval-after-load 'mixed-pitch
  (with-eval-after-load 'corfu
    ;; mixed-pitch 通过 face-remapping-alist 把 default 变成 variable-pitch，
    ;; corfu 的 popup buffer 继承后就会变比例字体；这里直接在 corfu buffer 里禁用 remap。
    (define-advice corfu--make-buffer (:around (oldfun &rest args))
      (let ((face-remapping-alist nil))
        (apply oldfun args)))))

(with-eval-after-load 'org
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        prettify-symbols-mode t
        prettify-symbols-unprettify-at-point 'right-edge
        org-image-actual-width nil
        org-list-allow-alphabetical t
        org-format-latex-header
        "\\documentclass[10pt]{article}\n\\usepackage[usenames]{color}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\pagestyle{empty}  % do not remove\n% The settings below are copied from fullpage.sty\n
          \\usepackage{xeCJK,tikz,caption,float,makecell,circuitikz,array}\n
          \\usetikzlibrary{shapes,arrows,calc,arrows.meta}\n
          \\usetikzlibrary{circuits.logic.IEC,calc}\n
          \\renewcommand{\\arraystretch}{1.3}\n
          \\setlength{\\textwidth}{\\paperwidth}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}\n"
        org-appear-autolinks t)
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (python . t)))
  (keymap-set org-mode-map "C-c b" 'org-cite-insert))

(add-hook
 'org-mode-hook
 (lambda ()
   (setq-local time-stamp-active t
               time-stamp-start "#\\+MODIFIED: [ \t]*"
               time-stamp-end "$"
               time-stamp-format "\[%Y-%m-%d %3a %H:%M\]")
   (electric-indent-local-mode)
   (org-appear-mode)
   (org-cdlatex-mode)
   (olivetti-mode)
   (mixed-pitch-mode)
   (if (and (boundp 'my/theme-preset)
            (eq my/theme-preset 'matrix))
       (custom-theme-set-faces
        'user
        '(org-verbatim
          ((t (:foreground "#00ff71"
                           :background "#001900"
                           :box (:line-width (3 . 1)
                                             :color "#001900")
                           :inherit fixed-pitch)))))
     (custom-theme-set-faces
      'user
      '(org-verbatim
        ((((background light))
          (:foreground "#9e3a00"
                       :background "#faece4"
                       :box (:line-width (3 . 1)
                                         :color "#faece4")))
         (((background dark))
          (:foreground "#ec9369"
                       :background "#1c130f"
                       :box (:line-width (3 . 1)
                                         :color "#1c130f")))))))
   (visual-line-mode)
   (valign-mode)))


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
        org-modern-checkbox nil
        org-modern-list
        '(
          (?* . "•")
          (?+ . "‣"))))




(setq-default org-startup-folded 'overview
              org-startup-with-inline-images t
              org-startup-indented t)


(add-hook
 'org-mode-hook
 (lambda ()
   (add-hook 'before-save-hook 'time-stamp nil 'local)))

(setq denote-directory "~/Documents/Personal/denote")

(autoload 'org-noter "org-noter" nil t)

(with-eval-after-load 'org-noter
  (setq org-noter-notes-search-path `(,denote-directory)
        org-noter-auto-save-last-location t))

(autoload 'magit-status "magit" nil t)
(autoload 'magit-dispatch "magit" nil t)
(autoload 'magit-file-dispatch "magit" nil t)
(autoload 'org-roam-node-find "org-roam" nil t)
(autoload 'org-roam-capture "org-roam" nil t)
(autoload 'org-roam-buffer-toggle "org-roam" nil t)

(keymap-global-set "C-c n n" 'org-noter)

(keymap-global-set "C-c n f" 'org-roam-node-find)

(setq org-roam-mode-sections '(org-roam-backlinks-section org-roam-reflinks-section org-roam-unlinked-references-section))

(with-eval-after-load 'org-roam
  (add-hook
   'org-roam-mode-hook
   (lambda ()
     (word-wrap-whitespace-mode)))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-capture-templates
        '(("d"
           "default"
           plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("b"
           "Books"
           plain
           "* Related Information\n\nAuthor: %^{Author}\nVersion: %^{Version}\n\n* Notes\n%?"
           :target
           (file+head
            "books/${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: %^{}\n#+CREATED: %U\n#+MODIFIED: \n\n")
           :unnarrowed t)
          ("t" "Trifles" entry "* Notes:\n%?"
           :target
           (file+head
            "Trifles/${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: %^g\n#+CREATED: %U\n#+MODIFIED: \n\n")
           :unnarrowed t)
          ("p" "Programming" entry "* Notes:\n%?"
           :target
           (file+head
            "Programming/${slug}.org"
            "#+TITLE: ${title}\n#+FILETAGS: %^g\n#+CREATED: %U\n#+MODIFIED: \n\n")
           :unnarrowed t)
          ("k" "Knowledge" entry "* Notes:\n%?"
           :target
           (file+head
            "Knowledge/${slug}.org"
            "#+TITLE: ${title}\n#+FILETAGS: %^g\n#+CREATED: %U\n#+MODIFIED: \n\n")
           :unnarrowed t))))

;;; LaTeX node
(autoload 'latex-node-find-file "latex-node" nil t)

;;; LaTeX
;; On demand loading, leads to faster startup time.
(with-eval-after-load 'citar
  (setq org-cite-global-bibliography '("/run/media/kunh/Elements/Zotero Bib/My Library.bib")
        citar-notes-paths '("~/Documents/Personal/denote")
        citar-library-paths '("/run/media/kunh/Elements/Zotero Library")
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        citar-bibliography org-cite-global-bibliography)
  (require 'citar-embark)
  (citar-embark-mode)
  (require 'citar-denote)
  (citar-denote-mode))


;; Register AUCTeX file modes now; load previews and folding on first use.
(require 'auctex)
;; This source checkout may not contain generated auctex-autoloads.el.
;; Clear built-in aliases before registering the AUCTeX entry points.
(TeX--alias-overlapped-modes)
(autoload 'TeX-tex-mode "init-tex" nil t)
(autoload 'plain-TeX-mode "init-tex" nil t)
(autoload 'LaTeX-mode "init-tex" nil t)
(autoload 'docTeX-mode "init-tex" nil t)
(autoload 'Texinfo-mode "init-tex" nil t)

;;; ============================================================================
;;; 08 UI Themes and Platform
;;; ============================================================================

;;; Mode line
(require 'segment-line)
(setq segment-line-enable-icons my/use-nerd-icons-p)
(segment-line-mode 1)

(require 'ligature)

;; ligature.el requires explicit rules; enabling alone does not add mappings.
(defconst my/prog-mode-ligatures
  '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
    ":::" "::=" ":=" "=:=" "==>" "=!=" "=>>=" "<="
    "=<<" "=/=" ">-" ">>-" ">>=" ">>>" "<*" "<*>"
    "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->"
    "<+" "<+>" "</" "</>" "/>" "/*" "*/" "//" "///"
    "&&" "||" "!!" "!=" "!==" "==" "===" "=>"
    "=<<" "=>>" ">=" ">>" "<<<" ">>>" "->" "->>"
    "<-" "<<-" "<=>" "<=<" "<==" "<||" "<&&" "++"
    "--" "::" "..." ".." "__")
  "Ligatures enabled in programming modes.")

(ligature-set-ligatures 't '("www"))
(defconst my/scala-ligatures
  (remove ">=" my/prog-mode-ligatures)
  "Ligatures for Scala modes where >= should stay literal.")

(defconst my/verilog-ligatures
  (remove ">=" (remove "<=" my/prog-mode-ligatures))
  "Ligatures for Verilog modes where <= and >= should stay literal.")

(ligature-set-ligatures
 '(scala-mode scala-ts-mode)
 my/scala-ligatures)
(ligature-set-ligatures
 '(verilog-mode verilog-ts-mode)
 my/verilog-ligatures)
(ligature-set-ligatures
 'prog-mode
 my/prog-mode-ligatures)
(global-ligature-mode t)

;;; Layout

(dolist (hook '(prog-mode-hook))
  (add-hook hook 'display-fill-column-indicator-mode))

;;; Themes
(require 'color-theme-sanityinc-tomorrow)
(require 'ashen)
(require 'gruber-darker-theme)
(require 'catppuccin-theme)
(require 'doric-themes)
(require 'init-theme)

(with-eval-after-load 'citre
  (setq citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)
  (defvar citre-elisp-backend
    (citre-xref-backend-to-citre-backend
     ;; This is the xref backend name
     'elisp
     ;; A function to tell if the backend is usable
     (lambda ()
       (derived-mode-p 'emacs-lisp-mode))))
  ;; Register the backend, which means to bind it with the symbol `elisp'.
  (citre-register-backend 'elisp citre-elisp-backend)
  ;; Add Elisp to the backend lists.
  (setq citre-find-definition-backends '(elisp eglot tags global))
  (setq citre-find-reference-backends '(elisp eglot global)))

(when (boundp 'hl-todo)
  (global-hl-todo-mode))

(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-suppress-auto-error t))


(if (eq system-type 'gnu/linux)
    (progn
      (when (file-exists-p "/opt/bin/ctags")
        (setq citre-ctags-program "/opt/bin/ctags"))
      (setq org-roam-directory "~/Documents/Personal/org-roam")
      (setup-display-graphic nil nil 6 10 my/theme-preset 105)
      (add-hook
       'server-after-make-frame-hook
       #'(lambda ()
           (setup-display-graphic nil nil 6 10 my/theme-preset 105)))
      (add-to-list 'exec-path "/home/kunh/.local/bin")
      (add-to-list 'exec-path "/home/kunh/.cargo/bin"))
  (progn
    (setq org-roam-directory "d:/Documents/Personal/org-roam")
    (setup-display-graphic nil nil 6 17 my/theme-preset 105)
    (add-hook
     'server-after-make-frame-hook
     #'(lambda ()
         (setup-display-graphic nil nil 6 17 my/theme-preset 105)))))

;; NeoEmacs may expose `sqlite-available-p' while being built without SQLite.
;; Starting autosync without a back-end aborts the remainder of this init file.
(with-eval-after-load 'org-roam
  (when (and (fboundp 'sqlite-available-p)
             (sqlite-available-p))
    (setq org-roam-database-connector 'sqlite-builtin)
    (org-roam-db-autosync-mode 1)))

(setq lexdb-dictionaries
      '((:id
         ldoce
         :type ldoce
         :name "朗文当代"
         :db-file "~/dicts/LDOCE6.db"
         :audio-dir "~/dicts/audio/"
         :priority 1)
        (:id oald :type oald :name "牛津双解" :db-file "~/dicts/OALD4_EC.db" :priority 2)
        (:id ode :type ode :name "牛津英语" :db-file "~/dicts/ODE_Living_Online.db" :priority 3)))

(defvar my/lexdb-initialized nil
  "Non-nil after dictionary adapters have been initialized.")

(defun my/lexdb-search ()
  "Load dictionary adapters on the first lookup."
  (interactive)
  (unless my/lexdb-initialized
    (require 'lexdb-ldoce)
    (require 'lexdb-oald)
    (require 'lexdb-ode)
    (lexdb-init)
    (setq my/lexdb-initialized t))
  (call-interactively #'lexdb-search))

(keymap-global-set "C-c d" #'my/lexdb-search)


(provide 'init)
;;; init.el ends here
