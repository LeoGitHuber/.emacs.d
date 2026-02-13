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

;;; Bootstrap

(defvar windows-system-p (eq system-type 'windows-nt)
  "Judge whether it's Windows system.")

(defvar non-android-p (not (eq system-type 'android))
  "Judge whether it isn't Android system.")

(setq custom-file "~/.emacs.d/custom.el"
      load-prefer-newer t)

(load custom-file)

(when (not non-android-p)
  (setopt tool-bar-position 'bottom)
  (setq touch-screen-display-keyboard t))

;;; Load path and core helpers
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-func)

(let ((packages (find-subdir-recursively "~/.emacs.d/site-lisp")))
  (setq load-path (append load-path packages)))

(dolist (path
         '("~/.emacs.d/site-lisp"
           "~/.emacs.d/site-lisp/treemacs/src/elisp"
           "~/.emacs.d/site-lisp/treemacs/src/extra/"
           "~/.emacs.d/site-lisp/lsp-mode/clients"
           "~/.emacs.d/site-lisp/lsp-mode/scripts"
           "~/.emacs.d/site-lisp/lsp-mode/docs"
           "~/.emacs.d/site-lisp/vertico/extensions"
           "~/.emacs.d/site-lisp/themes/themes"
           "~/.emacs.d/site-lisp/pdf-tools/lisp"))
  (add-to-list 'load-path path))


;;; Core settings
(setq eat-kill-buffer-on-exit t
      css-indent-offset 2
      set-mark-command-repeat-pop t
      other-window-scroll-default 'get-lru-window
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      ispell-dictionary "en_US"
      ;; ispell-program-name "hunspell"
      ;; package-quickstart nil
      nobreak-char-display nil)

;;; GC / Performance
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold better-gc-cons-threshold
         gc-cons-percentage 0.1)))

(add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)

(add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)

;;; Diagnostics / icons
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook '(lambda ()
                                   (flycheck-set-indication-mode 'left-margin))))

(require 'nerd-icons)
(require 'nerd-icons-corfu)

(setq nerd-icons-font-family
      (if (eq system-type 'gnu/linux)
          "Consolas Nerd Font Mono"
        "Symbols Nerd Font Mono"))

(defface diagnostics-error
  `(
    (((background dark))
     :foreground "#f85149"
     :family ,nerd-icons-font-family)
    (((background light))
     :foreground "#cb2431"
     :family ,nerd-icons-font-family))
  "Face for flymake Error."
  :group 'flymake)

(defface diagnostics-warn
  `(
    (((background dark))
     :foreground "#f0883e"
     :family ,nerd-icons-font-family)
    (((background light))
     :foreground "#bf8803"
     :family ,nerd-icons-font-family))
  "Face for flymake Warn."
  :group 'flymake)

(defface diagnostics-info
  `((((background dark))
     :foreground "#75beff"
     :family ,nerd-icons-font-family)
    (((background light))
     :foreground "#1155ff"
     :family ,nerd-icons-font-family))
  "Face for flymake Info."
  :group 'flymake)

(setq flymake-no-changes-timeout 0.5
      flymake-indicator-type 'margins
      flymake-autoresize-margins t
      flymake-margin-indicators-string
      `((error "​​​​󰅙​​​​" diagnostics-error)
        (warning "​​ " diagnostics-warn)
        (note "​​​​​​​" diagnostics-info))
      flymake-show-diagnostics-at-end-of-line 'fancy
      elisp-flymake-byte-compile-load-path (cons "./" load-path))

(require 'flymake)

(dolist (hook '(prog-mode-hook))
  (add-hook hook 'flymake-mode))

(set-face-attribute 'flymake-end-of-line-diagnostics-face nil :box nil)

(defun flymake--rgb-to-hex (r g b)
  "Convert R G B components to hex color string."
  (format "#%02x%02x%02x" r g b))

(defun flymake--darken-bg (bg percent)
  "Darken BG with PERCENT for flymake eol."
  (apply #'flymake--rgb-to-hex
         (mapcar
          (lambda (component)
            (min 255 (floor (* component (- 100 percent) 0.01))))
          (mapcar (lambda (x)
                    (/ x 256))
                  (color-values bg)))))

(defun flymake--lighten-fg (bg percent)
  "Lighten BG with PERCENT for flymake eol."
  (apply #'flymake--rgb-to-hex
         (mapcar
          (lambda (component)
            (let ((c (floor (* component (+ 100 percent) 0.01))))
              (if (equal c 0)
                  200
                (min 255 c))))
          (mapcar (lambda (x)
                    (/ x 256))
                  (color-values bg)))))

(require 'project)

;;; Icons

(with-eval-after-load 'treemacs
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))

(with-eval-after-load 'all-the-icons
  (load "~/.emacs.d/self-develop/all-the-icons-diy.el"))

(require 'init-startup)


;;; Core behavior

(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
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



(setq hl-line-sticky-flag nil
      hl-line-overlay nil)

(global-hl-line-mode)

(dolist (hook
         '(eshell-mode-hook
           shell-mode-hook
           term-mode-hook
           messages-buffer-mode-hook
           eat-mode-hook
           org-mode-hook
           markdown-mode-hook
           markdown-view-mode-hook
           nov-mode-hook
           tex-mode-hook
           TeX-mode-hook
           LaTeX-mode-hook))
  (add-hook hook (lambda ()
                   (setq-local global-hl-line-mode nil))))

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
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

(with-eval-after-load 'auto-save
  (setq auto-save-delete-trailing-whitespace t
        auto-save-disable-predicates '((lambda () (string-suffix-p "gpg" (file-name-extension (buffer-name)) t)))))

(setq
 ;; auto-save-timeout 30
 ;; auto-save-interval 10
 auto-save-default nil
 save-silently t
 auto-save-no-message t)

(setq auto-save-visited-interval 1)

(auto-save-visited-mode)

(add-hook
 'before-save-hook
 (lambda ()
   (delete-trailing-whitespace (point-min)
                               (- (line-beginning-position) 1))
   (delete-trailing-whitespace (+ (point) 1)
                               (point-max))))

(dolist (hook '(TeX-mode-hook dired-mode-hook markdown-mode-hook markdown-view-mode-hook))
  (add-hook hook '(lambda ()
                    (setq truncate-lines t))))


(repeat-mode)

(require 'no-littering)

(with-eval-after-load 'no-littering
  (recentf-mode t)
  (setq recentf-max-saved-items 1000
        recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'"))
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

(dolist (mode
         '(prog-mode-hook
           toml-ts-mode-hook
           ;; TeX-mode-hook
           cuda-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode t))))

(desktop-save-mode 1)

(with-eval-after-load 'desktop
  (setq desktop-restore-frames nil))

(setq comment-auto-fill-only-comments t)

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
         (load "~/.emacs.d/site-lisp/iscroll/iscroll.el"))
     (iscroll-mode))))

(with-eval-after-load 'magit
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
          magit-insert-unpulled-from-upstream)))

(require 'aggressive-indent)

(dolist (hook '(emacs-lisp-mode-hook yuck-mode-hook python-ts-mode python-mode scss-mode-hook))
  (add-hook hook 'aggressive-indent-mode))




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

(global-set-key [remap comment-dwim] 'comment-or-uncomment)

;;; Fingertip
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

(require 'helpful)

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

(defhydra hydra-zoom (global-map "<f2>")
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
  ("v" scroll-uppp nil)
  ("V" scroll-down nil)
  ("l" forward-char nil)
  ("h" backward-char nil)
  ("F" forward-word nil)
  ("B" backward-word nil)
  ("q" nil "quit"))

(keymap-global-set "C-M-;" 'hydra-eMove/body)

(require 'rect)

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
         (rectangle-mark-mode t))
   nil)
  ("y" kill-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" yank-rectangle nil)
  ("o" nil nil))

(keymap-global-set "C-x SPC" 'hydra-rectangle/body)



;;; Completion / LSP

(require 'markdown-mode)

(define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do)

(add-to-list 'load-path "~/.emacs.d/site-lisp/corfu/extensions")

(require 'corfu)
(require 'corfu-popupinfo)
(require 'yasnippet)
(require 'yasnippet-capf)

(yas-global-mode 1)

(yas-minor-mode-on)

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
        corfu-auto-delay 0
        corfu-preview-current t
        corfu-on-exact-match 'show
        ;; corfu-quit-no-match t
        ;; corfu-preselect 'prompt
        ;; corfu-quit-at-boundary t
        )
  (keymap-set corfu-map "<tab>" 'corfu-insert)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  (with-eval-after-load 'corfu-popupinfo
    (setq corfu-popupinfo-delay '(0.1 . 0.1)))
  (setq nerd-icons-corfu--space (propertize " " 'display '(space :width 0.8)))
  (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter))

(defun yas-setup-capf ()
  "Set capf for yasnippets."
  (setq-local completion-at-point-functions (cons #'yasnippet-capf completion-at-point-functions)))

(add-hook 'prog-mode-hook 'yas-setup-capf)

(setq yas-prompt-functions '(yas-no-prompt))

(require 'eglot)
(require 'eglot-booster)
(require 'dape)

(with-eval-after-load 'eglot
  (setq my/pyright-uvx-command '("pyright-langserver" "--stdio"))
  (add-to-list
   'eglot-server-programs
   '((verilog-mode verilog-ts-mode)
     .
     ("verible-verilog-ls" "--push_diagnostic_notifications" "--rules"
     "-explicit-function-lifetime,
                  -explicit-parameter-storage-type,
                  -unpacked-dimensions-range-ordering,
                  -forbid-line-continuations,
                  -parameter-name-style,
                  -line-length,
                  -always-comb"
      )))
  (add-to-list
   'eglot-server-programs
   `((scala-mode scala-ts-mode)
     .
     ("env" "JAVA_HOME=/usr/lib64/jvm/java-21-openjdk-21" "metals-emacs")))
  (setq project-vc-extra-root-markers '(".dir-locals.el"))
  (setq eglot-send-changes-idle-time 0
        eglot-code-action-indications '(eglot-hint))
  )

(with-eval-after-load 'lsp-bridge
  (add-hook 'lsp-bridge-mode-hook 'yas/minor-mode)
  (add-hook 'c++-ts-mode-hook (lambda ()
                                (setq-local lsp-bridge-inlay-hint-overlays t)))
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
   lsp-bridge-enable-log t
   lsp-bridge-log-level 'debug
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
   ))

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

(setq-default completion-styles '(basic partial-completion orderless))

(setq completion-styles '(basic partial-completion orderless)
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
      (setq vertico-multiform-categories '((file grid) (consult-grep buffer) (consult-ripgrep buffer))
            vertico-cycle t
            vertico-sort-function 'vertico-sort-history-alpha)
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
   win (frame-height)
   (floor (frame-height) 6)
   (floor (frame-width) 2)
   (floor (* (frame-width) 17) 35)))

(defun popper--auto-fit-window-height (win)
  "Determine the height of popup window WIN by fitting it to the buffer's content."
  (fit-window-to-buffer win (floor (frame-height) 2)
                        (floor (* (frame-height) 2) 5)))

(defun popper-display-popup-adaptive (buffer &optional alist)
  "Display popup-buffer BUFFER at the bottom of the screen.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists."
  (if (and (> (window-pixel-height)
              (window-pixel-width))
           (or (and popper-open-popup-alist
                    (eq (window-parameter (caar popper-open-popup-alist) 'window-side) 'bottom))
               (not popper-open-popup-alist)))
      (display-buffer-in-side-window
       buffer
       (append
        alist `((window-height . popper--auto-fit-window-height)
                (side . bottom)
                (slot . 1))))
    (display-buffer-in-side-window
     buffer (append alist `((window-width . popper--fit-window-width)
                            (side . right)
                            (slot . 1))))))

(setq popper-display-function 'popper-display-popup-adaptive
      fit-window-to-buffer-horizontally t)

(require 'popper)
(require 'popper-echo)

(keymap-global-set "M-<tab>" 'popper-toggle)

(keymap-global-set "M-`" 'popper-cycle)

(keymap-global-set "C-M-`" 'popper-toggle-type)

(popper-mode +1)

(popper-echo-mode +1)

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
  (defun indent-bars--guess-spacing ()
    "Get indentation spacing of current buffer.
Adapted from `highlight-indentation-mode'."
    (cond (indent-bars-spacing-override)
          ((and (derived-mode-p 'verilog-mode)
                (boundp 'verilog-indent-level))
           verilog-indent-level)
          ((and (derived-mode-p 'ada-mode)
                (boundp 'ada-indent))
           ada-indent)
          ((and (derived-mode-p 'ada-ts-mode)
                (boundp 'ada-ts-mode-indent-offset))
           ada-ts-mode-indent-offset)
          ((and (derived-mode-p 'gpr-mode)
                (boundp 'gpr-indent))
           gpr-indent)
          ((and (derived-mode-p 'gpr-ts-mode)
                (boundp 'gpr-ts-mode-indent-offset))
           gpr-ts-mode-indent-offset)
          ((and (derived-mode-p 'python-mode)
                (boundp 'py-indent-offset))
           py-indent-offset)
          ((and (derived-mode-p 'python-mode 'python-base-mode)
                (boundp 'python-indent-offset))
           python-indent-offset)
          ((and (derived-mode-p 'ruby-mode)
                (boundp 'ruby-indent-level))
           ruby-indent-level)
          ((and (derived-mode-p 'scala-mode)
                (boundp 'scala-indent:step))
           scala-indent:step)
          ((and (derived-mode-p 'scala-mode)
                (boundp 'scala-mode-indent:step))
           scala-mode-indent:step)
          ((and (derived-mode-p 'scala-ts-mode)
                (boundp 'scala-ts-indent-offset))
           scala-ts-indent-offset)
          ((and (derived-mode-p 'rust-ts-mode)
                (boundp 'rust-ts-mode-indent-offset))
           rust-ts-mode-indent-offset)
          ((and (or (derived-mode-p 'scss-mode)
                    (derived-mode-p 'css-mode))
                (boundp 'css-indent-offset))
           css-indent-offset)
          ((and (derived-mode-p 'nxml-mode)
                (boundp 'nxml-child-indent))
           nxml-child-indent)
          ((and (derived-mode-p 'coffee-mode)
                (boundp 'coffee-tab-width))
           coffee-tab-width)
          ((and (derived-mode-p 'js-mode)
                (boundp 'js-indent-level))
           js-indent-level)
          ((and (derived-mode-p 'js2-mode)
                (boundp 'js2-basic-offset))
           js2-basic-offset)
          ((and (derived-mode-p 'typescript-ts-mode)
                (boundp 'typescript-ts-mode-indent-offset))
           typescript-ts-mode-indent-offset)
          ((and (derived-mode-p 'sws-mode)
                (boundp 'sws-tab-width))
           sws-tab-width)
          ((and (derived-mode-p 'web-mode)
                (boundp 'web-mode-markup-indent-offset))
           web-mode-markup-indent-offset)
          ((and (derived-mode-p 'web-mode)
                (boundp 'web-mode-html-offset)) ; old var
           web-mode-html-offset)
          ((and (local-variable-p 'c-basic-offset)
                (numberp c-basic-offset))
           c-basic-offset)
          ((and (local-variable-p 'c-ts-common-indent-offset)
                (symbolp c-ts-common-indent-offset)
                (numberp (symbol-value c-ts-common-indent-offset)))
           (symbol-value c-ts-common-indent-offset))
          ((and (derived-mode-p 'yaml-mode)
                (boundp 'yaml-indent-offset))
           yaml-indent-offset)
          ((and (derived-mode-p 'yaml-pro-mode)
                (boundp 'yaml-pro-indent))
           yaml-pro-indent)
          ((and (derived-mode-p 'elixir-mode)
                (boundp 'elixir-smie-indent-basic))
           elixir-smie-indent-basic)
          ((and (derived-mode-p 'lisp-data-mode)
                (boundp 'lisp-body-indent))
           lisp-body-indent)
          ((and (derived-mode-p 'cobol-mode)
                (boundp 'cobol-tab-width))
           cobol-tab-width)
          ((or (derived-mode-p 'go-ts-mode)
               (derived-mode-p 'go-mode))
           tab-width)
          ((derived-mode-p 'nix-mode)
           tab-width)
          ((derived-mode-p 'makefile-mode)
           tab-width)
          ((and (derived-mode-p 'nix-ts-mode)
                (boundp 'nix-ts-mode-indent-offset))
           nix-ts-mode-indent-offset)
          ((and (derived-mode-p 'json-ts-mode)
                (boundp 'json-ts-mode-indent-offset))
           json-ts-mode-indent-offset)
          ((and (derived-mode-p 'json-mode)
                (boundp 'js-indent-level))
           js-indent-level)
          ((and (derived-mode-p 'sh-base-mode)
                (boundp 'sh-basic-offset))
           sh-basic-offset)
          ((and (derived-mode-p 'java-ts-mode)
                (boundp 'java-ts-mode-indent-offset))
           java-ts-mode-indent-offset)
          ((and (derived-mode-p 'tcl-mode)
                (boundp 'tcl-indent-level))
           tcl-indent-level)
          ((and (derived-mode-p 'haml-mode)
                (boundp 'haml-indent-offset))
           haml-indent-offset)
          ((and (boundp 'standard-indent) standard-indent))
          (t
           4)))                         ; backup
  )

(require 'consult)
(require 'consult-xref)

(keymap-global-set "C-x l" 'consult-line)

(keymap-global-set "C-x b" 'consult-buffer)

(with-eval-after-load 'consult
  (consult-customize consult-buffer :preview-key '(:debounce 0.4 "M-."))
  (setq consult-project-function
        (lambda (may-prompt) (or (vc-root-dir) (consult--default-project-function may-prompt)))))

(setq xref-show-xrefs-function 'consult-xref
      xref-show-definitions-function 'consult-xref)

(keymap-global-set "C-." 'embark-act)

(with-eval-after-load 'embark
  (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))

;;; Appearance helpers
(dolist (hook '(prog-mode-hook text-mode-hook cuda-mode-hook))
  (add-hook hook 'colorful-mode))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)



;;; Language modes / Treesit
(setq elisp-fontify-semantically t)

(require 'scala-ts-mode)
(require 'kdl-mode)
(require 'yuck-mode)

(when (treesit-available-p)
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (css-mode . css-ts-mode)
          (java-mode . java-ts-mode)
          (js-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (sh-mode . bash-ts-mode)
          (verilog-mode . verilog-ts-mode))
        treesit-font-lock-level 4)
  (require 'qml-ts-mode))

(dolist (entry
         '(("\\.pdf\\'" . pdf-view-mode)
           ("\\.ya?ml\\'" . yaml-ts-mode)
           ("\\.lua\\'" . lua-ts-mode)
           ("\\.scala\\'" . scala-mode)
           ("\\.kdl\\'" . kdl-mode)
           ("\\.do\\'" . tcl-mode)
           ("\\.xdc\\'" . tcl-mode)
           ("\\.ts\\'" . typescript-ts-mode)
           ("\\.tsx\\'" . typescript-ts-mode)
           ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
           ("README\\.md\\'" . gfm-mode)))
  (add-to-list 'auto-mode-alist entry))

(with-eval-after-load 'typescript-ts-mode
  (setq typescript-ts-mode-indent-offset 4))

(with-eval-after-load 'css-mode
  (setq css-indent-offset 4))

;;; Verilog
(require 'verilog-ts-mode)

(setq verilog-indent-level 2
      verilog-indent-level-declaration 2
      verilog-indent-level-module 2
      verilog-indent-level-behavioral 2
      verilog-auto-newline nil
      verilog-ts-indent-level 2)

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


(dolist (hook '(prog-mode-hook))
  (add-hook hook #'hs-minor-mode))

;;; Dired

(setq dired-mouse-drag-files t
      mouse-drag-and-drop-region-cross-program t
      delete-by-moving-to-trash t
      dired-movement-style 'cycle
      dired-omit-mode t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish/extensions")
(require 'dirvish)
(require 'dirvish-side)

(dirvish-override-dired-mode)

(with-eval-after-load 'dirvish
  (setq dirvish-attributes '(vc-state nerd-icons git-msg file-size subtree-state collapse file-time)
        dirvish-side-attributes '(vc-state nerd-icons subtree-state collapse)
        )
  (keymap-set dirvish-mode-map "TAB" #'dirvish-toggle-subtree))

(unless (bound-and-true-p dirvish-override-dired-mode)
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode))

(defun set-font-for-dired ()
  "Set font for Dired."
  (face-remap-add-relative 'default
                           :family "IBM Plex Mono"
                           :height 95))

(add-hook 'dired-mode-hook 'set-font-for-dired)



;;; Chinese

(setq cns-prog "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cnws"
      cns-dict-directory "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cppjieba/dict"
      cns-recent-segmentation-limit 20
      cns-debug nil
      cns-process-type 'shell)

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
        rime-user-data-dir "~/.emacs.d/rime" ;; "~/.emacs.d/rime/"
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
(dolist (feature '(eldoc-box eldoc-mouse calibredb nov nov-xwidget shrface nov-highlights etaf))
  (require feature))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(with-eval-after-load 'nov
  (setq nov-text-width t)
  (nov-highlights-global-mode-enable)
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



;;; Org
(dolist (path
         '("~/.emacs.d/site-lisp/org-roam"
           "~/.emacs.d/site-lisp/org-modern-indent"
           "~/.emacs.d/site-lisp/org-modern"
           "~/.emacs.d/site-lisp/org-appear"
           "~/.emacs.d/site-lisp/org-bars"
           "~/.emacs.d/site-lisp/emacsql"
           "~/.emacs.d/site-lisp/org-visual-outline"))
  (add-to-list 'load-path path))

(require 'org)
(require 'valign)
(require 'org-appear)
(require 'org-modern)
(require 'org-modern-indent)
(require 'olivetti)
(require 'mixed-pitch)

(setq olivetti-style 'fancy
      olivetti-margin-width 5)

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
               time-stamp-format "\[%Y-%m-%d %3a %H:%M\]"
               company-backends '(company-files company-keywords))
   (electric-indent-local-mode)
   (org-appear-mode)
   (org-cdlatex-mode)
   (olivetti-mode)
   (mixed-pitch-mode)
   (custom-theme-set-faces
    'user
    '(org-verbatim
      ((((background light))
        (:foreground "#9e3a00"
         :background "#faece4"
         :box (:line-width (3 . 1)
               :color "#faece4")))
       (((background dark))
        (
         :foreground
         "#ec9369"
         :background "#1c130f"
         :box (:line-width (3 . 1)
               :color "#1c130f")))))
    )
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

(require 'org-noter)

(with-eval-after-load 'org-noter
  (setq org-noter-notes-search-path `(,denote-directory)
        org-noter-auto-save-last-location t))

(dolist (path '("~/.emacs.d/site-lisp/with-editor/lisp" "~/.emacs.d/site-lisp/magit/lisp"))
  (add-to-list 'load-path path))

(load "~/.emacs.d/site-lisp/transient/lisp/transient.el")

(require 'magit)
(require 'org-roam)

(keymap-global-set "C-c n n" 'org-noter)

(keymap-global-set "C-c n f" 'org-roam-node-find)

(setq org-roam-db-gc-threshold most-positive-fixnum
      org-roam-mode-sections '(org-roam-backlinks-section org-roam-reflinks-section org-roam-unlinked-references-section))

(with-eval-after-load 'org-roam
  (add-hook
   'org-roam-mode-hook
   (lambda ()
     (word-wrap-whitespace-mode)))
  (setq org-roam-database-connector 'sqlite-builtin
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
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
(load "~/.emacs.d/lisp/latex-node.el")

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


(require 'auctex)
(require 'cdlatex)
(require 'tex-fold)
(require 'font-latex)
(require 'tex-bar)
(require 'preview)
(require 'color)

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
  (add-hook
   'cdlatex-tab-hook
   (lambda ()
     (and (bound-and-true-p lsp-bidge-mode)
          (acm-frame-visible-p acm-menu-frame))))
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-fold-auto t
        cdlatex-paired-parens "$[{("
        TeX-save-query nil
        tex-suscript-height-ratio 0.8)
  (setq preview-scale-function 1.3)
  (custom-set-variables
   '(preview-LaTeX-command
     '("%`%l --output-format=dvi \"\\nonstopmode\\nofiles\\PassOptionsToPackage{"
       ("," . preview-required-option-list)
       "}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined"
       preview-default-preamble
       "\\fi}\"%' \"\\detokenize{\" %(t-filename-only) \"}\"")))
  (setq preview-image-type 'dvi*
        preview-dvi*-command #'preview-dvisvgm-command
        preview-dvi*-image-type 'svg)
  (defun my-preview-force-default-face (ov &rest _)
    "Make SVG `currentColor' resolve from `default' face, not font-lock faces."
    (when (eq (overlay-get ov 'preview-state) 'active)
      (overlay-put ov 'face 'default)))
  (advice-add 'preview-toggle :after #'my-preview-force-default-face)
  (if (boundp 'preview-dvi*-command)
      (setq preview-dvi*-image-type 'svg
            preview-dvi*-command
            (lambda ()
              (preview-dvisvgm-command
               "dvisvgm --no-fonts --currentcolor %d --page=- --output=\"%m/prev%3p.svg\"")))
    (setq preview-dvipng-image-type 'svg
          preview-dvipng-command
          (lambda ()
            (preview-dvisvgm-command
             "dvisvgm --no-fonts --currentcolor %d --page=- --output=\"%m/prev%3p.svg\""))))
  (setq-default TeX-master t
                TeX-engine 'luatex
                TeX-command-extra-options "-synctex=1 -shell-escape")
  (if windows-system-p
      (add-to-list 'TeX-view-program-selection '(output-pdf "Sioyek"))
    (add-to-list 'TeX-view-program-selection '(output-pdf "Okular")))
  (with-eval-after-load 'eaf
    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf")))
  (add-to-list
   'TeX-command-list
   '("XeLaTeX" "%`xelatex -shell-escape --syntex=1%(mode)%' %t" TeX-run-TeX nil t))
  (setcdr
   (assoc "LaTeXMk" TeX-command-list)
   '("latexmk -lualatex -pvc -view=none %(latexmk-out) %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t"
     TeX-run-TeX
     nil
     (LaTeX-mode docTeX-mode)
     :help "Run LaTeXMk"))
  (add-to-list 'texmathp-tex-commands1 '("lstlisting" env-off)))

(with-eval-after-load 'tex-fold
  (setq TeX-fold-macro-spec-list
        (mapcar
         (lambda (x)
           (if (equal (cadr x) '("item"))
               '(("[1]||•" . (1 . 0)) ("item"))
             x))
         TeX-fold-macro-spec-list))
  (defun my-tex-fold--prefix-like-title (prefix title)
    "Return PREFIX+TITLE, where PREFIX uses the same face as TITLE."
    (let* ((face (or (get-text-property 0 'face title)
                     (get-text-property 0 'font-lock-face title)))
           (pfx
            (if face
                (propertize prefix 'face face)
              prefix)))
      (concat pfx title)))
  (defun my-tex-fold-section (title)
    (my-tex-fold--prefix-like-title "§ " title))
  (defun my-tex-fold-subsection (title)
    (my-tex-fold--prefix-like-title "§§ " title))
  (defun my-tex-fold-subsubsection (title)
    (my-tex-fold--prefix-like-title "§§§ " title))
  (defun my-tex-fold-paragraph (title)
    (my-tex-fold--prefix-like-title "¶ " title))
  ;; 放到表头，覆盖默认规则
  (dolist (spec
           '((my-tex-fold-section ("section" "section*"))
             (my-tex-fold-subsection ("subsection" "subsection*"))
             (my-tex-fold-subsubsection ("subsubsection" "subsubsection*"))
             (my-tex-fold-paragraph ("paragraph" "paragraph*"))
             ("~" ("textasciitilde"))
             ("^" ("textasciicircum"))
             ("\\" ("textbackslash"))
             ("|" ("textbar"))
             (" " (";"))
             (" " ("quad"))
             ("  " ("qquad"))))
    (push spec LaTeX-fold-macro-spec-list))
  (dolist (x
           '((("∧" . 0)
              ("land"))
             (("∨" . 0)
              ("lor"))
             (1 ("textnormal" "mathrm"))
             ))
    (add-to-list 'LaTeX-fold-math-spec-list x))
  (dolist (x
           '((("╭•────▶" "╰•────▶")
              ("itemize"))
             (("╭≣────▶" "╰≣────▶")
              ("enumerate"))
             (("╭⌁────▶" "╰⌁────▶")
              ("description"))
             (("↱" "↳")
              ("equation" "equation*"))
             (("≡" "≡")
              ("align" "align*" "aligned"))
             (("Σ" "Σ")
              ("gather" "gather*"))
             (("⋯" "⋯")
              ("multline" "multline*"))))
    (add-to-list 'TeX-fold-begin-end-spec-list x))
  (defvar my/TeX-fold-clearpage-string
    "────────────────────────────────  clearpage  ────────────────────────────────")
  (defun my/TeX-fold--macro-face ()
    "取当前宏（\\...）开头处的 face，用来给占位符上色。"
    (let* ((beg
            (ignore-errors
              (TeX-find-macro-start)))  ; AUCTeX 的宏起点定位
           (pos (or beg (point))))
      (or (get-text-property pos 'face)
          (get-text-property pos 'font-lock-face)
          'font-lock-keyword-face)))
  (defun my/TeX-fold-clearpage (&rest _args)
    (propertize my/TeX-fold-clearpage-string 'face (my/TeX-fold--macro-face)))
  (add-to-list 'LaTeX-fold-macro-spec-list '(my/TeX-fold-clearpage ("clearpage")))
  (defvar my-tex-fold-empty-braces-scope 'math
    "Scope for folding empty braces after control words.
Possible values: 'math (default) or 'all.")
  (defun my-tex-fold--in-math-p (pos)
    "Non-nil if POS is in TeX math mode."
    (save-excursion
      (goto-char pos)
      (texmathp)))
  (defun my-tex-fold--already-folded-p (beg end)
    "Check whether region already has our TeX-fold terminator overlay."
    (cl-some
     (lambda (ov)
       (and (eq (overlay-get ov 'category) 'TeX-fold)
            (overlay-get ov 'my-tex-fold-terminator)))
     (overlays-in beg end)))
  (defun my-tex-fold--zero-overlay (beg end)
    "Hide region [BEG, END) with a TeX-fold overlay displaying zero width."
    (unless (or (>= beg end)
                (my-tex-fold--already-folded-p beg end))
      (let* ((priority (TeX-overlay-prioritize beg end))
             (ov (make-overlay beg end (current-buffer) t nil)))
        (overlay-put ov 'category 'TeX-fold)
        (overlay-put ov 'priority priority)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'my-tex-fold-terminator t)
        ;; Zero-width placeholder (looks like “gone”)
        (overlay-put ov 'TeX-fold-display-string-spec "\u200B")
        (TeX-fold-hide-item ov)
        ov)))
  (defun my-tex-fold-fold-terminators (&optional beg end)
    "Fold empty {} and terminator whitespace *in math* after TeX-fold runs."
    (interactive)
    (when (bound-and-true-p TeX-fold-mode)
      (save-excursion
        (let ((beg (or beg (point-min)))
              (end (or end (point-max))))
          ;; 1) Fold empty {} right after control words: \foo{}
          ;;    Default: only in math (safer). Set `my-tex-fold-empty-braces-scope` to 'all if you want everywhere.
          (goto-char beg)
          (while (re-search-forward "\\\\[A-Za-z@]+\\({}\\)" end t)
            (let ((b (match-beginning 1))
                  (e (match-end 1)))
              (when (or (eq my-tex-fold-empty-braces-scope 'all)
                        (my-tex-fold--in-math-p b))
                (my-tex-fold--zero-overlay b e))))
          ;; 2a) Same-line terminator whitespace (NO newline):
          ;;     \cmd<space/tab><letter>  (letter would otherwise extend control word)
          (goto-char beg)
          (while (re-search-forward "\\\\[A-Za-z@]+\\([ \t]+\\)\\([A-Za-z@]\\)" end t)
            (let ((b (match-beginning 1))
                  (e (match-end 1)))
              (when (my-tex-fold--in-math-p b)
                (my-tex-fold--zero-overlay b e))))
          ;; 2b) Multi-line: keep the newline, fold indentation only:
          ;;     \cmd\n[ \t]+<letter>  -> hide only the spaces after \n
          (goto-char beg)
          (while (re-search-forward "\\\\[A-Za-z@]+\\(\n[ \t]+\\)\\([A-Za-z@]\\)" end t)
            (let* ((grp-b (match-beginning 1))
                   (grp-e (match-end 1))
                   (indent-b (1+ grp-b)) ;; skip the newline itself
                   (indent-e grp-e))
              (when (and (< indent-b indent-e)
                         (my-tex-fold--in-math-p grp-b))
                (my-tex-fold--zero-overlay indent-b indent-e))))))))
  (defun my-tex-fold--after-fold (&rest _)
    (my-tex-fold-fold-terminators (point-min)
                                  (point-max)))
  ;; Make it work for both buffer-fold and region-fold
  (advice-add 'TeX-fold-buffer :after #'my-tex-fold--after-fold)
  (advice-add 'TeX-fold-region :after #'my-tex-fold--after-fold))

(with-eval-after-load 'font-latex
  (setq font-latex-fontify-sectioning 1.2)
  (font-latex-update-sectioning-faces))

(with-eval-after-load 'tex-mode
  (dolist (pair
           '(("\\land" . ?∧)
             ("\\lor" . ?∨)
             ))
    (add-to-list 'tex--prettify-symbols-alist pair)))

(dolist (hook '(LaTeX-mode-hook TeX-mode-hook tex-mode-hook))
  (add-hook
   hook
   (lambda ()
     (setq-local TeX-command-default "LaTeXMk" ;; "XeLaTeX"
                 )
     (setq TeX-electric-math t)
     (font-latex-add-keywords '(("CJKunderline" "{")) 'underline-command)
     (electric-indent-mode -1)
     (TeX-PDF-mode-on)
     (LaTeX-math-mode 1)
     (TeX-fold-mode 1)
     (turn-on-cdlatex)
     (turn-on-reftex)
     (visual-line-mode)
     (outline-minor-mode)
     (olivetti-mode)
     (mixed-pitch-mode)
     (face-remap-add-relative 'font-latex-superscript-face '(:height 0.8))
     (face-remap-add-relative 'font-latex-subscript-face '(:height 0.8))
     (run-with-idle-timer 0 nil
                          (lambda (buf)
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (font-lock-ensure) ; 先确保字体化
                                )))
                          (current-buffer)))))

(keymap-set outline-minor-mode-map "C-<tab>" 'outline-toggle-children)



;;; Layout


(dolist (hook '(prog-mode-hook))
  (add-hook hook 'display-fill-column-indicator-mode))

;;; Themes
(defvar themes_chosen
  '(;;; Light theme
    ;; modus-operandi-tritanopia
    ef-spring
    ;; Dark theme
    ;; manoj-dark
    ;; doom-rouge
    modus-vivendi-tritanopia)
  "Set for themes for dark and light mode.")

(require 'color-theme-sanityinc-tomorrow)
(require 'rose-pine)
(require 'gruber-darker-theme)
(when non-android-p
  (require 'citre)
  (require 'citre-config)
  (defun citre-jump+ ()
    (interactive)
    (condition-case _
        (citre-jump)
      (error
       (let* ((xref-prompt-for-identifier nil))
         (call-interactively #'xref-find-definitions)))))
  (keymap-global-set "M-." 'citre-jump+)
  (keymap-global-set "M-," 'citre-jump-back))

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
      (setup-display-graphic nil nil 6 17 nil 13)
      (add-hook
       'server-after-make-frame-hook '(lambda ()
                                        (setup-display-graphic nil nil 6 17 nil 13)))
      (add-to-list 'exec-path "/home/kunh/.local/bin")
      (add-to-list 'exec-path "/home/kunh/.cargo/bin"))
  (progn
    (setq org-roam-directory "d:/Documents/Personal/org-roam")
    (setup-display-graphic nil nil 6 17 nil 26)
    (add-hook
     'server-after-make-frame-hook '(lambda ()
                                      (setup-display-graphic nil nil 6 17 nil 26)))))

(with-eval-after-load 'eglot
  (eglot-booster-mode))

(org-roam-db-autosync-mode)

(require 'lexdb-ldoce)
(require 'lexdb-oald)
(require 'lexdb-ode)

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

(lexdb-init)

(keymap-global-set "C-c d" 'lexdb-search)


(provide 'init)
;;; init.el ends here
