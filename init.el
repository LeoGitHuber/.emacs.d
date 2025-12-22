;;; Init.el --- Emacs Configuration --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/benchmark-init-el")

(defvar windows-system-p
  (eq system-type 'windows-nt)
  "Judge whether it's Windows system.")

(defvar android-system-p
  (not (eq system-type 'android))
  "Judge whether it's Android system.")

;; (if windows-system-p
;;     (progn
;;       (require 'benchmark-init)
;;       (prefer-coding-system 'utf-8))
;;   (require 'benchmark-init-loaddefs)
;;   )

;; (benchmark-init/activate)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
(setq custom-file "~/.emacs.d/custom.el"
      load-prefer-newer t)
(load "~/.emacs.d/custom.el")

(when (not android-system-p)
  (setopt tool-bar-position 'bottom)
  (setq touch-screen-display-keyboard t))
  ;;; Emacs Default Setting
;; (load "~/.emacs.d/lisp/init-func.el")
(add-to-list 'load-path "~/.emacs.d/lisp")
;; (add-to-list 'load-path "~/.emacs.d/lisp/init-func.el")
(require 'init-func)
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
;; (load "~/.emacs.d/site-lisp/loaddefs.el")
(setq eat-kill-buffer-on-exit t
      css-indent-offset 2
      set-mark-command-repeat-pop t
      other-window-scroll-default 'get-lru-window
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      ispell-dictionary "en_US"
      ;; ispell-program-name "hunspell"
      ;; package-quickstart nil
      )

;;; @2. flymake and flycheck
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook '(lambda ()
                                   (flycheck-set-indication-mode 'left-margin))))

(require 'nerd-icons)
(require 'nerd-icons-corfu)
;; (setq nerd-icons-font-family "CaskaydiaCove Nerd Font Mono")
;; (setq nerd-icons-font-family "PragmataProLiga Nerd Font Mono")
(setq nerd-icons-font-family
      (if (eq system-type 'gnu/linux)
          "Consolas Nerd Font Mono"
        "Symbols Nerd Font Mono"))

(defface diagnostics-error
  `(
    ;; (((background dark)) :background "#090c10" :foreground "#f85149" :family ,nerd-icons-font-family)
    ;; (((background light)) :foreground "#cb2431" :family ,nerd-icons-font-family)
    (((background dark)) :foreground "#f85149" :family ,nerd-icons-font-family)
    (((background light)) :foreground "#cb2431" :family ,nerd-icons-font-family)
    )
  "Face for flymake Error."
  :group 'flymake)

(defface diagnostics-warn
  `(
    ;; (((background dark)) :background "#090c10" :foreground "#f0883e" :family ,nerd-icons-font-family)
    ;; (((background light)) :foreground "#bf8803" :family ,nerd-icons-font-family)
    (((background dark)) :foreground "#f0883e" :family ,nerd-icons-font-family)
    (((background light)) :foreground "#bf8803" :family ,nerd-icons-font-family)
    )
  "Face for flymake Warn."
  :group 'flymake)

(defface diagnostics-info
  ;; `((((background dark)) :background "#090c10" :foreground "#75beff" :color "#000000" :family ,nerd-icons-font-family)
  ;;   (((background light)) :foreground "#1155ff" :color "white" :family ,nerd-icons-font-family)
  `((((background dark)) :foreground "#75beff" :family ,nerd-icons-font-family)
    (((background light)) :foreground "#1155ff" :family ,nerd-icons-font-family)
    )
  "Face for flymake Info."
  :group 'flymake)

(setq flymake-no-changes-timeout nil
      flymake-indicator-type 'margins
      flymake-autoresize-margins t
      flymake-margin-indicators-string
      ;; `((error "​​​​󰅙" compilation-error)
      ;;   (warning "​​​​" compilation-warning)
      ;;   (note "​​​​" compilation-info))
      ;; `((error "​​​​󰅙​​​​" diagnostics-error)
      `((error "​​​​󰅙​​​​" diagnostics-error)
        ;; (warning "​​​​​​​​" diagnostics-warn)
        ;; (warning "​​​​​​​​" diagnostics-warn)
        (warning "​​ " diagnostics-warn)
        ;; (note "​​​​​​​​" diagnostics-info)
        (note "​​​​​​​" diagnostics-info)
        )
      ;; `((error ,(nerd-icons-octicon "nf-oct-x_circle_fill") diagnostics-error)
      ;;   (warning ,(nerd-icons-faicon "nf-fa-warning") diagnostics-warn)
      ;;   (note ,(nerd-icons-faicon "nf-fa-info") diagnostics-info))
      flymake-show-diagnostics-at-end-of-line 'fancy
      elisp-flymake-byte-compile-load-path (cons "./" load-path)
      )

(require 'flymake)
(dolist (hook '(prog-mode-hook))
  (add-hook hook 'flymake-mode))

(set-face-attribute 'flymake-end-of-line-diagnostics-face nil :box nil)

(defun flymake--rgb-to-hex (r g b)
  "Convert R G B components to hex color string."
  (format "#%02x%02x%02x" r g b))

(defun flymake--darken-bg (bg percent)
  "Darken BG with PERCENT for flymake eol."
  (apply #'flymake--rgb-to-hex (mapcar (lambda (component)
                                         (min 255
                                              (floor (* component (- 100 percent) 0.01))))
                                       (mapcar (lambda (x) (/ x 256)) (color-values bg)))))

(defun flymake--lighten-fg (bg percent)
  "Lighten BG with PERCENT for flymake eol."
  (apply #'flymake--rgb-to-hex (mapcar (lambda (component)
                                         (let ((c (floor (* component (+ 100 percent) 0.01))))
                                           (if (equal c 0)
                                               200
                                             (min 255 c))))
                                       (mapcar (lambda (x) (/ x 256)) (color-values bg)))))

;; (require 'flyover)
;; (setq flyover-virtual-line-icon "╰──")
;; (add-hook 'flymake-mode-hook #'flyover-mode)

(require 'project)

  ;;; @3. ICONS

;; (load "~/.emacs.d/self-develop/modeline-setting.el")

(with-eval-after-load 'treemacs
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))
(add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode)

(with-eval-after-load 'all-the-icons (load "~/.emacs.d/self-develop/all-the-icons-diy.el"))

;; (require 'init-startup)
(load "~/.emacs.d/lisp/init-startup.el")

  ;;; @4. MEOW
(require 'meow)
(meow-setup)
(meow-global-mode 1)

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

(require 'puni)
(require 'vundo)
(require 'avy)
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

(setq elisp-fontify-semantically t)
;; (require 'scala-mode)
(require 'scala-ts-mode)
(require 'kdl-mode)
(require 'yuck-mode)
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
  (require 'qml-ts-mode)
  )

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdl-mode))
(add-to-list 'auto-mode-alist '("\\.do\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.xdc\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(with-eval-after-load 'typescript-ts-mode
  (setq typescript-ts-mode-indent-offset 4))
(with-eval-after-load 'css-mode
  (setq css-indent-offset 4))


(require 'eldoc-box)
(require 'calibredb)
(require 'nov)
(require 'nov-xwidget)
(require 'shrface)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; ;; (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;; (evil-define-key 'normal nov-mode-map (kbd "o") 'nov-xwidget-view)
;; (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
;; (add-hook 'nov-xwidget-webkit-mode-hook '(lambda() (xwidget-webkit-zoom (xwidget-webkit-current-session) 1.5)))
;; (setq visual-fill-column-center-text t)
(with-eval-after-load 'shrface
  (defun shrface-nov-setup ()
    (unless shrface-toggle-bullets
      (shrface-regexp))
    (set-visited-file-name nil t)
    (setq tab-width 8)
    (if (string-equal system-type "android")
        (setq-local touch-screen-enable-hscroll nil)))

  (defun shrface-remove-blank-lines-at-the-end (start end)
    "A fix for `shr--remove-blank-lines-at-the-end' which will remove image at the end of the document."
    (save-restriction
      (save-excursion
        (narrow-to-region start end)
        (goto-char end)
        (when (and (re-search-backward "[^ \n]" nil t)
                   (not (eobp)))
          (forward-line 1)
          (delete-region (point) (min (1+ (point)) (point-max)))))))

  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   ;; (indent-rigidly (point-min) (point-max) 2)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       ;; (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code)
       ;; (propertize "#+END_SRC" 'face 'org-block-end-line )
       )
      (shr-ensure-newline)
      (setq end (point))
      (pcase (frame-parameter nil 'background-mode)
        ('light
         (add-face-text-property start end '(:background "#D8DEE9" :extend t)))
        ('dark
         (add-face-text-property start end '(:background "#292b2e" :extend t))))
      (shr-ensure-newline)
      (insert "\n")))

  (defvar shrface-nov-rendering-functions
    (append '((img . nov-render-img)
              (svg . nov-render-svg)
              (title . nov-render-title)
              (pre . shrface-shr-tag-pre-highlight)
              (code . shrface-tag-code)
              (form . eww-tag-form)
              (input . eww-tag-input)
              (button . eww-form-submit)
              (textarea . eww-tag-textarea)
              (select . eww-tag-select)
              (link . eww-tag-link)
              (meta . eww-tag-meta))
            shrface-supported-faces-alist))

  (defun shrface-nov-render-html ()
    (require 'eww)
    (let ((shrface-org nil)
          (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
          (shr-table-vertical-line "|")
          (shr-width 7000) ;; make it large enough, it would not fill the column (use visual-line-mode/writeroom-mode instead)
          (shr-indentation 0) ;; remove all unnecessary indentation
          (tab-width 8)
          (shr-external-rendering-functions shrface-nov-rendering-functions)
          (shrface-toggle-bullets nil)
          (shrface-href-versatile t)
          (shr-use-fonts nil)           ; nil to use default font
          (shr-map nov-mode-map))

      ;; HACK: `shr-external-rendering-functions' doesn't cover
      ;; every usage of `shr-tag-img'
      (cl-letf (((symbol-function 'shr-tag-img) 'nov-render-img))
        (shr-render-region (point-min) (point-max)))
      ;; workaround, need a delay to update the header line
      ;; (run-with-timer 0.01 nil 'shrface-update-header-line)
      ;; workaround, show annotations when document updates
      (shrface-show-all-annotations)))
  )

(with-eval-after-load 'nov
  (setq nov-text-width t)
  (add-hook 'nov-mode-hook #'eldoc-mode)
  (add-hook 'nov-mode-hook #'eldoc-box-hover-mode)
  ;; (add-hook 'nov-mode-hook #'org-indent-mode)
  ;; (add-hook 'nov-mode-hook #'shrface-nov-setup)
  ;; (setq nov-render-html-function #'shrface-nov-render-html)
  ;; (advice-add 'shr--remove-blank-lines-at-the-end :override #'shrface-remove-blank-lines-at-the-end)
  (add-hook 'nov-mode-hook #'visual-line-mode)
  ;; (add-hook 'nov-mode-hook #'visual-fill-column-mode)
  ;; (add-hook 'nov-mode-hook #'(lambda() (set-fill-column 150)))
  (add-hook 'nov-mode-hook #'(lambda ()
                               (face-remap-add-relative 'variable-pitch :family "Charter" :height 1.4)))
  )
(when (eq system-type 'gnu/linux)
  (require 'pdf-tools)
  (require 'pdf-occur)
  (require 'pdf-history)
  (require 'pdf-isearch)
  (require 'pdf-links)
  (require 'pdf-outline)
  (require 'pdf-misc)
  (require 'pdf-annot)
  (require 'pdf-sync)
  (require 'pdf-cache)
  ;; (require 'pdf-virtual)
  ;; (require 'pdf-loader)
  ;; (with-eval-after-load 'pdf-tools
  ;;   (setq pdf-view-use-scaling t
  ;;         pdf-view-continuous t
  ;;         pdf-anot-list-format '((page . 3)
  ;;                                (type . 10)
  ;;                                (contents . 50)
  ;;                                (date . 24)))
  ;;   (pdf-tools-install))

  ;; (pdf-loader-install)
  (add-hook 'pdf-view-mode-hook 'pdf-history-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-isearch-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-links-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-outline-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-misc-size-indication-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-misc-context-menu-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-sync-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-cache-prefetch-minor-mode)
  )

;;; @6. LSP

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
                (cons #'tempel-complete
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;; (with-eval-after-load 'yasnippet
;;   (add-hook 'yas-keymap-disable-hook
;;             (lambda()
;;               (or
;;                (when (boundp 'corfu--frame)
;;                  (and
;;                   (frame-live-p corfu--frame)
;;                   (frame-visible-p corfu--frame)))
;;                (when (boundp 'acm-menu-frame)
;;                  (and (frame-live-p acm-menu-frame)
;;                       (frame-visible-p acm-menu-frame)))))))

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
  ;; (keymap-set corfu-map "<backtab>" 'corfu-previous)
  ;; (keymap-set corfu-map "S-<return>" 'corfu-insert)
  ;; (keymap-unset corfu-map "RET")
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  (with-eval-after-load 'corfu-popupinfo
    (setq corfu-popupinfo-delay '(0.1 . 0.1)))
  (setq nerd-icons-corfu--space (propertize " " 'display '(space :width 0.8)))
  (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter))

(defun yas-setup-capf ()
  "Set capf for yasnippets."
  (setq-local completion-at-point-functions
              (cons #'yasnippet-capf
                    completion-at-point-functions)))

(add-hook 'prog-mode-hook 'yas-setup-capf)

(setq yas-prompt-functions '(yas-no-prompt))

(require 'eglot)
(require 'dape)
(with-eval-after-load 'eglot
  (setq my/pyright-uvx-command '("uvx" "--from" "pyright" "pyright-langserver" "--stdio"))
  (add-to-list 'eglot-server-programs `(python-ts-mode . ,my/pyright-uvx-command))
  (add-to-list 'eglot-server-programs
               '((verilog-mode verilog-ts-mode) .
                 ("verible-verilog-ls"
                  "--push_diagnostic_notifications"
                  "--rules"
                  "-explicit-function-lifetime,
                  -explicit-parameter-storage-type,
                  -unpacked-dimensions-range-ordering,
                  -forbid-line-continuations,
                  -parameter-name-style,
                  -line-length,
                  -always-comb"
                  ;; "--assignment_statement_alignment" "align"
                  ;; "--case_items_alignment" "align"
                  ;; "--class_member_variable_alignment" "align"
                  ;; "--distribution_items_alignment" "align"
                  ;; "--enum_assignment_statement_alignment" "align"
                  ;; "--formal_parameters_alignment" "align"
                  ;; "--module_net_variable_alignment" "align"
                  ;; "--named_parameter_alignment" "align"
                  ;; "--named_port_alignment" "align"
                  ;; "--port_declarations_alignment" "align"
                  ;; "--struct_union_members_alignment" "align"
                  ;; "--try_wrap_long_lines" "align"
                  )
                 ))
  ;; (add-to-list 'eglot-server-programs
  ;;              `((scala-mode scala-ts-mode)
  ;;                . ,(alist-get 'scala-mode eglot-server-programs)))
  (add-to-list 'eglot-server-programs
               `((scala-mode scala-ts-mode)
                 . ("env" "JAVA_HOME=/usr/lib64/jvm/java-21-openjdk-21" "metals-emacs")))
  (setq project-vc-extra-root-markers '(".dir-locals.el"))
  (setq eglot-send-changes-idle-time 0
        eglot-code-action-indications '(eglot-hint))
  ;; (cl-defmethod eglot-handle-notification :after
  ;;   (_server (_method (eql textDocument/publishDiagnostics)) &key uri
  ;;            &allow-other-keys)
  ;;   (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
  ;;     (with-current-buffer buffer
  ;;       (if (and (eq nil flymake-no-changes-timeout)
  ;;                (not (buffer-modified-p)))
  ;;           (flymake-start t)))))
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; (eglot-booster-mode)
  )

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
   )
  )

  ;;; @7. DIRED

(setq dired-mouse-drag-files t
      mouse-drag-and-drop-region-cross-program t
      delete-by-moving-to-trash t
      dired-movement-style 'cycle
      dired-omit-mode t
      )
(add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish/extensions")
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (or (boundp 'diredfl-mode)
;;                 (load "~/.emacs.d/site-lisp/diredfl/diredfl.el"))
;;             (toggle-truncate-lines)
;;             (diredfl-mode)
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
(require 'dirvish)
(require 'dirvish-side)
(dirvish-override-dired-mode)

(with-eval-after-load 'dirvish
  ;;   ;; (dirvish-peek-mode)
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

(setq cns-prog "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cnws"
      cns-dict-directory "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cppjieba/dict"
      cns-recent-segmentation-limit 20
      cns-debug nil)

(when android-system-p
  (require 'cns nil t)
  (when (featurep 'cns)
    (add-hook 'find-file-hook 'cns-auto-enable)))

;; @9. INPUT

(with-eval-after-load 'rime
  ;; (set-face-attribute 'rime-default-face nil :height 1.2)
  ;; (set-face-attribute 'rime-highlight-candidate-face nil :height 1.2)
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
        rime-user-data-dir "~/.emacs.d/rime"    ;; "~/.emacs.d/rime/"
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

;; (global-set-key "\C-\\" 'toggle-input-method)

;; @11. ORG
;; (require 'markdown-ts-mode)
;; (markdown-ts-setup)
(require 'org)
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
  (require 'org-modern)
  (require 'org-modern-indent)
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
            (require 'org-appear)
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
            (visual-line-mode)
            (require 'valign)
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

(with-eval-after-load 'org-modern-indent
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(setq denote-directory "~/Documents/Personal/denote")

(require 'org-noter)
(with-eval-after-load 'org-noter
  (setq org-noter-notes-search-path `(,denote-directory)
        org-noter-auto-save-last-location t)
  )

(add-to-list 'load-path "~/.emacs.d/site-lisp/with-editor/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(load "~/.emacs.d/site-lisp/transient/lisp/transient.el")
(require 'magit)
(require 'org-roam)
(keymap-global-set "C-c n n" 'org-noter)
(keymap-global-set "C-c n f" 'org-roam-node-find)
(setq org-roam-db-gc-threshold most-positive-fixnum
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
           :unnarrowed t))))

;;; @12. VERILOG
(require 'verilog-ts-mode)
(setq verilog-indent-level 2
      verilog-indent-level-declaration 2
      verilog-indent-level-module 2
      verilog-indent-level-behavioral 2
      verilog-auto-newline nil
      verilog-ts-indent-level 2
      )

(defconst verilog-hs-block-start-keywords-re
  (eval-when-compile
    (concat "\\("
            "\\(" (regexp-opt '("(" "{" "[")) "\\)"
            "\\|"
            "\\(" (verilog-regexp-words
                   '("begin"
                     "fork"
                     "clocking"
                     "function"
                     "module"
                     "covergroup"
                     "property"
                     "task"
                     "generate"
                     "`ifdef" "`ifndef"))
            "\\)" "\\)")))

(defconst verilog-hs-block-end-keywords-re
  (eval-when-compile
    (concat "\\("
            "\\(" (regexp-opt '(")" "}" "]")) "\\)"
            "\\|"
            "\\(" (verilog-regexp-words
                   '("end"
                     "join" "join_any" "join_none"
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
  (dolist (mode '((verilog-mode    . verilog-forward-sexp-function)
                  (verilog-ts-mode . verilog-ts-forward-sexp)))
    (add-to-list 'hs-special-modes-alist `(,(car mode)
                                           ,verilog-hs-block-start-keywords-re
                                           ,verilog-hs-block-end-keywords-re
                                           nil
                                           ,(cdr mode))))
  (dolist (hook '(verilog-mode-hook verilog-ts-mode-hook))
    (add-hook hook #'hs-minor-mode))
  ;; Workaround to enable `hideshow' on first file visit with lazy loading using
  ;; :config section with `use-package'
  (when (member major-mode '(verilog-mode verilog-ts-mode))
    (hs-minor-mode 1)))
(verilog-ext-hs-setup)
;; (setq verilog-linter "verilator --lint-only")

;;; @14. HYDRA
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
  ("q" nil "quit")
  )

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
         (rectangle-mark-mode t)) nil)
  ("y" kill-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" yank-rectangle nil)
  ("o" nil nil))

(keymap-global-set "C-x SPC" 'hydra-rectangle/body)

    ;;; @15. LATEX-NODE
;; (require 'latex-node)
(load "~/.emacs.d/lisp/latex-node.el")

  ;;; @16. LATEX
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
  (citar-denote-mode)
  )

;; (require 'citar-capf)
;; (add-hook 'org-mode-hook 'citar-capf-setup)
;; ;; (keymap-global-set "C-c c c" 'citar-create-note)
;; (keymap-global-set "C-c c n" 'citar-denote-open-note)
;; (keymap-global-set "C-c c d" 'citar-denote-dwim)
;; (keymap-global-set "C-c c e" 'citar-denote-open-reference-entry)
;; (keymap-global-set "C-c c a" 'citar-denote-add-citekey)
;; (keymap-global-set "C-c c k" 'citar-denote-remove-citekey)
;; (keymap-global-set "C-c c r" 'citar-denote-find-reference)
;; (keymap-global-set "C-c c l" 'citar-denote-link-reference)
;; (keymap-global-set "C-c c f" 'citar-denote-find-citation)
;; (keymap-global-set "C-c c x" 'citar-denote-nocite)
;; (keymap-global-set "C-c c y" 'citar-denote-cite-nocite)
;; (keymap-global-set "C-c c z" 'citar-denote-nobib)

(require 'auctex)
(require 'cdlatex)
(require 'tex-fold)
(require 'font-latex)
(require 'tex-bar)

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
  (add-hook 'cdlatex-tab-hook
            (lambda ()
              (and (bound-and-true-p lsp-bidge-mode)
                   (acm-frame-visible-p acm-menu-frame))))
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

(dolist (hook '(LaTeX-mode-hook tex-mode-hook))
  (add-hook hook
            (lambda()
              ;; (auctex-latexmk-setup)
              ;; (electric-indent-local-mode)
              (electric-indent-mode -1)
              ;; (require 'adaptive-warp)
              ;; (setq adaptive-wrap-extra-indent 0)
              ;; (adaptive-wrap-prefix-mode)
              (setq TeX-command-default "XeLaTeX")
              ;; (TeX-global-PDF-mode)
              (TeX-PDF-mode-on)
              (LaTeX-math-mode 1)
              (setq TeX-electric-math t)
              (TeX-fold-mode 1)
              (turn-on-cdlatex)
              (turn-on-reftex)
              (visual-line-mode)
              (outline-minor-mode)
              ;; (keymap-set cdlatex-mode-map "<tab>" 'cdlatex-tab-maybe)
              ;; (turn-on-orgtbl)
              ;; (keymap-set orgtbl-mode-map "<tab>" 'orgtbl-next-field-maybe)
              ;; (citar-capf-setup)
              ;; (setq-local completion-at-point-functions
              ;;             (append (list #'cape-tex) completion-at-point-functions))
              )))
(keymap-set outline-minor-mode-map "C-<tab>" 'outline-toggle-children)
;; (keymap-set outline-minor-mode-map "M-[" 'outline-toggle-children)

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

;; (when (eq system-type 'gnu/linux)
;;   (with-eval-after-load 'info
;;     (add-to-list 'Info-directory-list "/usr/local/texlive/2024/texmf-dist/doc/info")
;;     (add-to-list 'Info-directory-list "/usr/local/share/info")))


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
 auto-save-default nil
 save-silently t
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


(repeat-mode)
(require 'no-littering)
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

(with-eval-after-load 'magit
  (setq magit-diff-refine-hunk t
        magit-log-section-commit-count 20
        magit-auto-revert-counter 10
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

(require 'aggressive-indent)
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
(keymap-global-set "C-c r" 'replace-regexp)

(require 'hideshow)
(dolist (hook '(emacs-lisp-mode-hook c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook
                                     verilog-mode-hook verilog-ts-mode-hook))
  (add-hook hook 'hs-minor-mode))
;; 折叠代码块，以下是额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face
  '(:inherit font-lock-comment-face :box t))

(setq hs-set-up-overlay 'hideshow-folded-overlay-fn)
;; (keymap-global-set "C-<tab>" 'hs-toggle-hiding)
(keymap-set hs-minor-mode-map "C-<tab>" 'hs-toggle-hiding)

(require 'mwim)
(global-set-key [remap move-beginning-of-line]
                'mwim-beginning-of-code-or-line-or-comment)
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

(require 'popper)
(require 'popper-echo)
(keymap-global-set "M-<tab>" 'popper-toggle)
(keymap-global-set "M-`" 'popper-cycle)
(keymap-global-set "C-M-`" 'popper-toggle-type)
;; (global-tab-line-mode +1)
(popper-mode +1)
(popper-echo-mode +1)

(define-key global-map [remap list-buffers] 'ibuffer)

(when android-system-p
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
   indent-bars-display-on-blank-lines t
   )
  (defun indent-bars--guess-spacing ()
    "Get indentation spacing of current buffer.
Adapted from `highlight-indentation-mode'."
    (cond
     (indent-bars-spacing-override)
     ((and (derived-mode-p 'verilog-mode) (boundp 'verilog-indent-level))
      verilog-indent-level)
     ((and (derived-mode-p 'ada-mode) (boundp 'ada-indent))
      ada-indent)
     ((and (derived-mode-p 'ada-ts-mode) (boundp 'ada-ts-mode-indent-offset))
      ada-ts-mode-indent-offset)
     ((and (derived-mode-p 'gpr-mode) (boundp 'gpr-indent))
      gpr-indent)
     ((and (derived-mode-p 'gpr-ts-mode) (boundp 'gpr-ts-mode-indent-offset))
      gpr-ts-mode-indent-offset)
     ((and (derived-mode-p 'python-mode) (boundp 'py-indent-offset))
      py-indent-offset)
     ((and (derived-mode-p 'python-mode 'python-base-mode) (boundp 'python-indent-offset))
      python-indent-offset)
     ((and (derived-mode-p 'ruby-mode) (boundp 'ruby-indent-level))
      ruby-indent-level)
     ((and (derived-mode-p 'scala-mode) (boundp 'scala-indent:step))
      scala-indent:step)
     ((and (derived-mode-p 'scala-mode) (boundp 'scala-mode-indent:step))
      scala-mode-indent:step)
     ((and (derived-mode-p 'scala-ts-mode) (boundp 'scala-ts-indent-offset))
      scala-ts-indent-offset)
     ((and (derived-mode-p 'rust-ts-mode) (boundp 'rust-ts-mode-indent-offset))
      rust-ts-mode-indent-offset)
     ((and (or (derived-mode-p 'scss-mode) (derived-mode-p 'css-mode))
	       (boundp 'css-indent-offset))
      css-indent-offset)
     ((and (derived-mode-p 'nxml-mode) (boundp 'nxml-child-indent))
      nxml-child-indent)
     ((and (derived-mode-p 'coffee-mode) (boundp 'coffee-tab-width))
      coffee-tab-width)
     ((and (derived-mode-p 'js-mode) (boundp 'js-indent-level))
      js-indent-level)
     ((and (derived-mode-p 'js2-mode) (boundp 'js2-basic-offset))
      js2-basic-offset)
     ((and (derived-mode-p 'typescript-ts-mode) (boundp 'typescript-ts-mode-indent-offset))
      typescript-ts-mode-indent-offset)
     ((and (derived-mode-p 'sws-mode) (boundp 'sws-tab-width))
      sws-tab-width)
     ((and (derived-mode-p 'web-mode) (boundp 'web-mode-markup-indent-offset))
      web-mode-markup-indent-offset)
     ((and (derived-mode-p 'web-mode) (boundp 'web-mode-html-offset)) ; old var
      web-mode-html-offset)
     ((and (local-variable-p 'c-basic-offset) (numberp c-basic-offset))
      c-basic-offset)
     ((and (local-variable-p 'c-ts-common-indent-offset)
	       (symbolp c-ts-common-indent-offset)
	       (numberp (symbol-value c-ts-common-indent-offset)))
      (symbol-value c-ts-common-indent-offset))
     ((and (derived-mode-p 'yaml-mode) (boundp 'yaml-indent-offset))
      yaml-indent-offset)
     ((and (derived-mode-p 'yaml-pro-mode) (boundp 'yaml-pro-indent))
      yaml-pro-indent)
     ((and (derived-mode-p 'elixir-mode) (boundp 'elixir-smie-indent-basic))
      elixir-smie-indent-basic)
     ((and (derived-mode-p 'lisp-data-mode) (boundp 'lisp-body-indent))
      lisp-body-indent)
     ((and (derived-mode-p 'cobol-mode) (boundp 'cobol-tab-width))
      cobol-tab-width)
     ((or (derived-mode-p 'go-ts-mode) (derived-mode-p 'go-mode))
      tab-width)
     ((derived-mode-p 'nix-mode)
      tab-width)
     ((derived-mode-p 'makefile-mode)
      tab-width)
     ((and (derived-mode-p 'nix-ts-mode) (boundp 'nix-ts-mode-indent-offset))
      nix-ts-mode-indent-offset)
     ((and (derived-mode-p 'json-ts-mode) (boundp 'json-ts-mode-indent-offset))
      json-ts-mode-indent-offset)
     ((and (derived-mode-p 'json-mode) (boundp 'js-indent-level))
      js-indent-level)
     ((and (derived-mode-p 'sh-base-mode) (boundp 'sh-basic-offset))
      sh-basic-offset)
     ((and (derived-mode-p 'java-ts-mode) (boundp 'java-ts-mode-indent-offset))
      java-ts-mode-indent-offset)
     ((and (derived-mode-p 'tcl-mode) (boundp 'tcl-indent-level))
      tcl-indent-level)
     ((and (derived-mode-p 'haml-mode) (boundp 'haml-indent-offset))
      haml-indent-offset)
     ((and (boundp 'standard-indent) standard-indent))
     (t 4))) 				; backup
  )

(require 'consult)
(require 'consult-xref)
;; (autoload 'consult-buffer "consult" nil t)
;; (autoload 'consult-line "consult" nil t)
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
(require 'vertico)
(require 'vertico-grid)
(require 'vertico-directory)
(require 'vertico-reverse)
(require 'vertico-indexed)
(require 'vertico-mouse)
(require 'vertico-buffer)
(require 'vertico-multiform)
(require 'vertico-sort)
(require 'vertico-suspend)
(require 'embark)
(require 'marginalia)
(require 'standard-themes)
(require 'rainbow-delimiters)
(require 'visual-fill-column)
(require 'colorful-mode)
(require 'indent-bars)
(require 'indent-bars-ts)
(require 'symbol-overlay)
(require 'aggressive-indent)
(require 'orderless)

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

;; (require 'winum)
;; (require 'ace-window)

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

;; (require 'timu-macos-theme)
;; (require 'nordic-night-theme)
(require 'color-theme-sanityinc-tomorrow)
(require 'rose-pine)

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

(when android-system-p
  (require 'citre)
  (require 'citre-config)
  (defun citre-jump+ ()
    (interactive)
    (condition-case _
        (citre-jump)
      (error (let* ((xref-prompt-for-identifier nil))
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
     (lambda () (derived-mode-p 'emacs-lisp-mode))))

  ;; Register the backend, which means to bind it with the symbol `elisp'.
  (citre-register-backend 'elisp citre-elisp-backend)
  ;; Add Elisp to the backend lists.
  (setq citre-find-definition-backends '(elisp eglot tags global))
  (setq citre-find-reference-backends '(elisp eglot global))
  )

(when (boundp 'hl-todo)
  (global-hl-todo-mode))

(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-suppress-auto-error t))

(require 'eat)
(if (eq system-type 'gnu/linux)
    (progn
      (when (file-exists-p "/opt/bin/ctags")
        (setq citre-ctags-program "/opt/bin/ctags"))
      (setq org-roam-directory "~/Documents/Personal/org-roam")
      (setup-display-graphic nil nil 6 17 nil 16)
      (add-hook 'server-after-make-frame-hook
                '(lambda ()
                   (setup-display-graphic nil nil 6 17 nil 16)))
      )
  (progn
    (setq org-roam-directory "d:/Documents/Personal/org-roam")
    (setup-display-graphic nil nil 6 17 nil 26)
    (add-hook 'server-after-make-frame-hook
              '(lambda ()
                 (setup-display-graphic nil nil 6 17 nil 26)))
    )
  )
(org-roam-db-autosync-mode)

(provide 'init)
;;; init.el ends here
