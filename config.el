;; -*- lexical-binding: t; -*-
;;; Config.el --- Emacs configuration file
;;; Commentary:

;;; Code:

;;; Basic

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
      package-check-signature nil)

;;; Emacs Default Setting
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/mwim.el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/compat")
(add-to-list 'load-path "~/.emacs.d/site-lisp/f.el")

(dolist
    (file (cddr (directory-files "~/.emacs.d/lisp" t nil nil nil)))
  (or (string-match "abandoned" file)
      (load file)))

(when (display-graphic-p)
  (set-en_cn-font "Input Mono" "HarmonyOS Sans SC" 12.0)
  ;; Maple Mono NF --- Maple Mono SC NF, HarmonyOS Sans SC
  ;; PragmataPro Mono Liga --- SimHei
  ;; Hack --- HarmonyOS Sans SC
  ;; JetBrainsMono Nerd Font
  ;; "Iosevka Fixed"    ;; Input Mono

  ;; ;; Enable Ligatures Feature, more info: https://github.com/mickeynp/ligature.el
  ;; (global-ligature-mode)
  ;; ;; PragmataPro Font Ligature Support
  ;; (ligature-pragmatapro-setup)

  ;; Don't use help echo tooltips
  ;; (setq x-gtk-use-system-tooltips nil)
  )

;; (let ((normal-gc-cons-threshold (* 32 1024 1024))
;;   	  (init-gc-cons-threshold (* 256 1024 1024)))
;;   ;; (progn (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'emacs-startup-hook
;;   			(lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; (setq frame-title-format
;; 	  '((:eval (if (buffer-file-name)
;; 				   (abbreviate-file-name (buffer-file-name))
;; 				 "%b"))))

;; (setq garbage-collection-messages t)
;; (defvar k-gc-timer
;;   (run-with-idle-timer 15 t 'garbage-collect))

;; (defvar better-gc-cons-threshold most-positive-fixnum ; 128mb
;;   "The default value to use for `gc-cons-threshold'.
;; If you experience freezing, decrease this.  If you experience stuttering, increase this.")


;; 开启 minibuffer 的时候不要 gc
;; (defun gc-minibuffer-setup-hook ()
;;   (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

;; (defun gc-minibuffer-exit-hook ()
;;   (garbage-collect)
;;   (setq gc-cons-threshold better-gc-cons-threshold))

;; (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)

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
;; (global-auto-revert-mode)

;; (setq hl-line-range-function 'hl-current-line-range)
(global-hl-line-mode)
(dolist
    (hook
     '(eshell-mode-hook shell-mode-hook term-mode-hook
                        messages-buffer-mode-hook))
  (add-hook hook (lambda ()
				   (setq-local global-hl-line-mode nil))))

(setq electric-pair-inhibit-predicate
      'electric-pair-conservative-inhibit
	  scroll-preserve-screen-position t
	  scroll-margin 0
	  scroll-conservatively 97)

(delete-selection-mode)

;; (setq-default cursor-type '(bar . 3))

(setq show-paren-when-point-inside-paren t
  	  show-paren-when-point-in-periphery t
  	  show-paren-context-when-offscreen 'child-frame)

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

(auto-save-visited-mode)
(setq auto-save-visited-interval 3)
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace (point-min)
                                        (- (line-beginning-position) 1))
            (delete-trailing-whitespace (+ (line-end-position) 1)
                                        (point-max))))

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/no-littering")
;; (require 'no-littering)

(add-hook 'minibuffer-mode-hook ;; 'after-init-hook
          (lambda ()
            (unless (bound-and-true-p recentf-mode)
              (recentf-mode t)
              (setq recentf-max-saved-items 1000
		            recentf-exclude `("/tmp/" "/ssh:"
                                      ,(concat user-emacs-directory
                                               "lib/.*-autoloads\\.el\\'"))))
            (unless (boundp 'no-littering-etc-directory)
              (load "~/.emacs.d/site-lisp/no-littering/no-littering.el")
              (with-eval-after-load 'no-littering
	            (add-to-list 'recentf-exclude no-littering-var-directory)
	            (add-to-list 'recentf-exclude no-littering-etc-directory)))
            ;; (fset 'yes-or-no-p 'y-or-n-p)
            (unless (bound-and-true-p save-place-mode)
              (save-place-mode t))
            (unless (bound-and-true-p savehist-mode)
              (setq history-length 10000
		            history-delete-duplicates t
		            savehist-save-minibuffer-history t)
              (savehist-mode t))
            ))

;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (setq history-length 10000
;; 				  history-delete-duplicates t
;; 				  savehist-save-minibuffer-history t)
;;             (savehist-mode t)))

(setq history-delete-duplicates t
	  recentf-max-menu-items 5
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
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
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
                (empty-line . empty-line) (unknown . question-mark)))

(dolist (mode '(prog-mode-hook TeX-mode-hook cuda-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode t))))

;; (setq desktop-path (list user-emacs-directory))
;; 	desktop-auto-save-timeout 600)
;; (desktop-save-mode 1)


;;; Efficiency
;; (load "~/.emacs.d/site-lisp/vundo/vundo.el")
(keymap-global-set "C-x f" 'find-file)
(keymap-global-set "C-u" 'undo)
(keymap-global-set "C-z" 'vundo)

(global-set-key [remap comment-dwim] 'comment-or-uncomment)
(setq comment-auto-fill-only-comments t)
(dolist (hook '(prog-mode-hook))
  (add-hook hook #'whitespace-mode))
(setq whitespace-style '(face trailing))

(electric-indent-mode -1)

;; (follow-mode)

;; Smoothly scrolling over image
(add-hook 'text-mode-hook
          (lambda ()
            (or (boundp 'iscroll-mode)
                (load "~/.emacs.d/site-lisp/iscroll/iscroll.el"))
            (iscroll-mode)))

(setq auto-mode-alist
	  (cons '("\\.pdf\\'" . pdf-view-mode) auto-mode-alist))

(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/with-editor/lisp")
;; (require 'magit-autoloads)

;; (load
;;  "~/.emacs.d/site-lisp/aggressive-indent-mode/aggressive-indent.el")
;; (global-aggressive-indent-mode t)
(dolist
    (hook
     '(emacs-lisp-mode-hook c++-ts-mode-hook c-ts-mode-hook
                            cuda-mode-hook))
  (add-hook hook 'aggressive-indent-mode))
;; (dolist (mode '(verilog-mode org-mode term-mode))
;;   (add-to-list 'aggressive-indent-excluded-modes mode))

(dolist (hook '(term-mode-hook))
  (add-hook hook #'puni-disable-puni-mode))

;; (load "~/.emacs.d/site-lisp/browse-kill-ring/browse-kill-ring.el")
(keymap-global-set "C-c k" 'browse-kill-ring)
(add-hook 'after-init-hook 'browse-kill-ring-default-keybindings)
(with-eval-after-load 'browse-kill-ring
  (setq browse-kill-ring-separator "------------------------------"
  		browse-kill-ring-separator-face 'shadow))

;; Meow
;; (load "~/.emacs.d/meow.el")

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

(c-add-style "microsoft"
  			 '("stroustrup"
  			   (c-offsets-alist
  				(access-label . /)
  				(innamespace . -)
  				(inline-open . 0)
  				(inher-cont . c-lineup-multi-inher)
  				(arglist-cont-nonempty . +)
  				(template-args-cont . +))))

(when (treesit-available-p)
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (csharp-mode     . csharp-ts-mode)
          (css-mode        . css-ts-mode)
          (java-mode       . java-ts-mode)
          (js-mode         . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          (ruby-mode       . ruby-ts-mode)
          (sh-mode         . bash-ts-mode))))

;; (add-hook 'c-ts-mode-hook (lambda () (setq c-ts-mode-indent-offset 4)))

;; (load "~/.emacs.d/site-lisp/emacs-which-key/which-key.el")
(when (display-graphic-p)
  (setq frame-title-format
  	    '((:eval (if (buffer-file-name)
  				     (abbreviate-file-name
				      (buffer-name))
  				   "%b"))))
  (with-eval-after-load 'which-key
    (which-key-mode t)
	(setq which-key-max-description-length 30
		  which-key-show-remaining-keys t)))

(define-key global-map [remap list-buffers] 'ibuffer)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/consult")
;; (require 'consult)
;; (load "~/.emacs.d/site-lisp/consult/consult-xref.el")

;; (autoload 'consult-buffer "consult" nil t)
;; (autoload 'consult-line "consult" nil t)
(keymap-global-set "C-x l" 'consult-line)
(keymap-global-set "C-x b" 'consult-buffer)
(with-eval-after-load 'consult
  (consult-customize
   consult-buffer
   :preview-key '(:debounce 0.4 "M-."))
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref))

;;; Theme
;; (dolist (hook '(prog-mode-hook text-mode-hook cuda-mode-hook))
;;   (add-hook hook 'rainbow-mode))
;; (load "~/.emacs.d/site-lisp/rainbow-delimiters/rainbow-delimiters.el")
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; (require 'color-theme-sanityinc-tomorrow)
;; (color-theme-sanityinc-tomorrow-bright)
;; (color-theme-sanityinc-tomorrow-bright)

;;; Minibuffer Setting
;; (load "~/.emacs.d/site-lisp/vertico/vertico.el")
;; (load "~/.emacs.d/site-lisp/vertico/extensions/vertico-directory.el")

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/vertico")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/marginalia/marginalia.el")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/orderless/orderless.el")
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))
(add-hook 'pre-command-hook
          (lambda ()
            (vertico-mode t)
            (marginalia-mode)))
(with-eval-after-load 'vertico (setq vertico-cycle t))
(if (bound-and-true-p vertico-mode)
    (keymap-set vertico-map "C-w" 'vertico-directory-delete-word)
  (keymap-set minibuffer-mode-map "C-w" 'backward-kill-word))
(setq-default completion-styles '(orderless basic))
(setq completion-styles '(basic partial-completion orderless)
      completion-category-overrides
      '((file (styles basic partial-completion))))

;; (defun sanityinc/use-orderless-in-minibuffer ()
;;   "Setup orderless for minibuffer."
;;   (setq-local completion-styles '(substring orderless)))
;; (add-hook 'minibuffer-setup-hook 'sanityinc/use-orderless-in-minibuffer)

;;; Windows Control

(load "~/.emacs.d/site-lisp/emacs-winum/winum.el")
(winum-mode)

;;; UI
(setq fill-column 80)
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'display-fill-column-indicator-mode))
;; (global-display-fill-column-indicator-mode)

(defvar themes_chosen
  '(modus-operandi-tritanopia manoj-dark)
  "Set for themes for dark and light mode.")

(if (or
     (>= (string-to-number (substring (current-time-string) 11 13)) 19)
     (<= (string-to-number (substring (current-time-string) 11 13))
         6))
	(progn
      (if (equal (cadr themes_chosen) 'modus-vivendi)
          (progn
            (setq modus-themes-org-blocks 'gray-background
	              modus-themes-bold-constructs t
	              modus-themes-italic-constructs t)
            (load-theme (car (cdr themes_chosen)) t))
        (when (equal (cadr themes_chosen) 'manoj-dark)
          (load-theme (car (cdr themes_chosen)) t)
          (set-face-foreground 'hl-line 'unspecified)))
      (set-face-attribute 'mode-line nil
  					      ;; :background "#0A0E12"
                          :background "black"
                          :box nil
  					      :font (font-spec
  						  	     ;; "JetBrainsMono Nerd Font" "Monego Ligatures" "Maple Mono NF"
							     :name
							     "JetBrainsMono Nerd Font"
							     ;; :weight 'normal
  						  	     :size
							     12.0))
      ;; (unless (symbol-value x-gtk-use-system-tooltips)
      ;; 	(set-face-attribute 'tooltip nil))
      )
  (progn
    ;; (load-theme (car themes_chosen) t)
    (when (eq custom-enabled-themes nil)
      ;; (set-face-bold 'font-lock-keyword-face t)
      ;; (set-face-bold 'font-lock-builtin-face t)
      (set-face-background 'highlight "#DFEAEC")
      (set-face-attribute 'line-number-current-line nil :foreground
                          "#000000" :background "#C4C4C4" :weight
                          'bold))
    (setq modus-themes-org-blocks 'gray-background
	      modus-themes-bold-constructs t
          modus-themes-italic-constructs t)
    (set-face-attribute 'mode-line nil
  					    ;; :background "#F4F7FA"
					    :background "white"
					    :box nil
					    :font (font-spec
  							   :name
							   "JetBrainsMono Nerd Font"
							   :size 12.0))))
(set-face-attribute 'mode-line-inactive nil :inherit 'mode-line :box
                    nil)
;; (set-face-attribute 'fringe nil :background 'unspecified)
;; (set-face-attribute 'line-number nil :background 'unspecified)

(setq x-underline-at-descent-line t)

;; (dolist (hook '(prog-mode-hook cuda-mode-hook))
;;   (add-hook hook 'highlight-indent-guides-mode))

(dolist (hook '(
				;; completion-list-mode-hook
				;; completion-in-region-mode-hook
				term-mode-hook
				;; shell-mode-hook
				messages-buffer-mode-hook
				org-roam-mode-hook))
  (add-hook hook 'hide-mode-line-mode))

(when (boundp 'hl-todo)
  (global-hl-todo-mode))

(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'character
		highlight-indent-guides-responsive 'top
		highlight-indent-guides-suppress-auto-error t))

(provide 'config)
;;; config.el ends here.
