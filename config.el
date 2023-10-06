;;; Config.el --- Emacs configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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
        package-check-signature nil)

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
  (load "~/.emacs.d/site-lisp/loaddefs.el")
  (load "~/.emacs.d/lisp/init-gc.el")
  (load "~/.emacs.d/lisp/init-diagnostic.el")
  (load "~/.emacs.d/lisp/init-icons.el")
  (load "~/.emacs.d/lisp/init-keybindings.el")
  (load "~/.emacs.d/lisp/init-meow.el")
  (load "~/.emacs.d/lisp/init-lsp.el")
  (load "~/.emacs.d/lisp/init-dired.el")
  (load "~/.emacs.d/lisp/init-chinese.el")
  (load "~/.emacs.d/lisp/init-input.el")
  (load "~/.emacs.d/lisp/init-eaf.el")
  (load "~/.emacs.d/lisp/init-latex.el")
  (load "~/.emacs.d/lisp/init-org.el")
  (load "~/.emacs.d/lisp/init-verilog.el")
  (load "~/.emacs.d/lisp/init-reader.el")
  (load "~/.emacs.d/lisp/init-hydra.el")
  (load "~/.emacs.d/site-lisp/emacs-which-key/which-key.el")

  (defun enable-after-meow ()
    "Modes enable after meow insert."
    (unless (bound-and-true-p lsp-bridge-mode)
      (add-hook 'emacs-lisp-mode-hook 'lsp-bridge-mode)
      (add-hook 'c-ts-mode-hook 'lsp-bridge-mode)
      (add-hook 'c++-ts-mode-hook 'lsp-bridge-mode)
      ;; (add-hook 'verilog-mode-hook 'lsp-bridge-mode)
      (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode
                            ;; 'verilog-mode
                            'c-ts-mode 'c++-ts-mode)
        (lsp-bridge-mode))
      (with-current-buffer (get-buffer-create "*scratch*")
        (lsp-bridge-mode)))
    (remove-hook 'meow-insert-enter-hook #'enable-after-meow))

  (when (bound-and-true-p meow-mode)
    (add-hook 'meow-insert-enter-hook #'enable-after-meow))

  (when (display-graphic-p)
    (set-en_cn-font "Input Mono" "LXGW WenKai Screen" 12.0)
    ;; Maple Mono NF --- Maple Mono SC NF, HarmonyOS Sans SC
    ;; PragmataPro Mono Liga --- SimHei
    ;; Hack --- HarmonyOS Sans SC
    ;; JetBrainsMono Nerd Font
    ;; "Iosevka Fixed"    ;; Input Mono
    (setq frame-title-format
  	      '((:eval (if (buffer-file-name)
  				       (abbreviate-file-name
				        (buffer-name))
  				     "%b"))))

    ;; ;; Enable Ligatures Feature, more info: https://github.com/mickeynp/ligature.el
    ;; (global-ligature-mode)
    ;; ;; PragmataPro Font Ligature Support
    ;; (ligature-pragmatapro-setup)
    ;; Set icon for truncation
    ;; (setq truncate-string-ellipsis (nerd-icons-mdicon "nf-md-arrow_down_right"))

    (setq x-underline-at-descent-line t)
    ;; Don't use help echo tooltips
    ;; (setq x-gtk-use-system-tooltips nil)
    ;; (unless (symbol-value x-gtk-use-system-tooltips)
    ;; 	(set-face-attribute 'tooltip nil))
    )

  (add-hook 'emacs-startup-hook
  		    (lambda () (setq gc-cons-threshold better-gc-cons-threshold)))

  (setq frame-title-format
	    '((:eval (if (buffer-file-name)
				     (abbreviate-file-name (buffer-file-name))
				   "%b"))))

  (add-hook 'minibuffer-setup-hook 'gc-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook 'gc-minibuffer-exit-hook)

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
	    scroll-conservatively 97
        eldoc-idle-delay 0.2)

  (delete-selection-mode)

  ;; (setq-default cursor-type '(bar . 3))

  (setq show-paren-when-point-inside-paren t
  	    show-paren-when-point-in-periphery t
        show-paren-delay 0
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
  ;; 		            recentf-exclude `("/tmp/" "/ssh:"
  ;;                                       ,(concat user-emacs-directory
  ;;                                                "lib/.*-autoloads\\.el\\'"))))
  ;;             (unless (boundp 'no-littering-etc-directory)
  ;;               ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/no-littering")
  ;;               ;; (rquire 'no-littering)
  ;;               (load "~/.emacs.d/site-lisp/no-littering/no-littering.el")
  ;;               (with-eval-after-load 'no-littering
  ;; 	            (add-to-list 'recentf-exclude no-littering-var-directory)
  ;; 	            (add-to-list 'recentf-exclude no-littering-etc-directory)))
  ;;             ;; (fset 'yes-or-no-p 'y-or-n-p)
  ;;             (unless (bound-and-true-p save-place-mode)
  ;;               (save-place-mode t))
  ;;             (unless (bound-and-true-p savehist-mode)
  ;;               (setq history-length 10000
  ;; 		            history-delete-duplicates t
  ;; 		            savehist-save-minibuffer-history t)
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
  (keymap-global-set "C-x f" 'find-file)
  (keymap-global-set "C-z" 'vundo)

  (global-set-key [remap comment-dwim] 'comment-or-uncomment)
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

  (electric-indent-mode -1)
  ;; (follow-mode)

  ;; Smoothly scrolling over image
  (unless (>= (string-to-number emacs-version) 30)
    (add-hook 'text-mode-hook
              (lambda ()
                (or (boundp 'iscroll-mode)
                    (load "~/.emacs.d/site-lisp/iscroll/iscroll.el"))
                (iscroll-mode))))

  (setq auto-mode-alist
	    (cons '("\\.pdf\\'" . pdf-view-mode) auto-mode-alist))

  (add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/with-editor/lisp")
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
  ;; (browse-kill-ring-default-keybindings)
  (add-hook 'after-init-hook 'browse-kill-ring-default-keybindings)
  (with-eval-after-load 'browse-kill-ring
    (setq browse-kill-ring-separator "------------------------------"
  		  browse-kill-ring-separator-face 'shadow))

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
            (sh-mode         . bash-ts-mode)))
    (add-hook 'emacs-lisp-mode-hook (lambda () (treesit-parser-create 'elisp))))


  ;; (add-hook 'c-ts-mode-hook (lambda () (setq c-ts-mode-indent-offset 4)))

  (with-eval-after-load 'which-key
    (which-key-mode t)
	(setq which-key-max-description-length 30
		  which-key-show-remaining-keys t))

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
    (setq xref-show-xrefs-function 'consult-xref
		  xref-show-definitions-function 'consult-xref))

  ;;; Theme
  (dolist (hook '(prog-mode-hook text-mode-hook cuda-mode-hook))
    (add-hook hook 'rainbow-mode))
  ;; (load "~/.emacs.d/site-lisp/rainbow-delimiters/rainbow-delimiters.el")
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  ;; (require 'color-theme-sanityinc-tomorrow)
  ;; (color-theme-sanityinc-tomorrow-bright)
  ;; (color-theme-sanityinc-tomorrow-bright)

;;; Minibuffer Setting
  ;; (load "~/.emacs.d/site-lisp/vertico/extensions/vertico-directory.el")

  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))))

  (defun vertico-enable ()
    (if (boundp vertico-mode)
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
    (remove-hook 'pre-command-hook #'vertico-enable))

  (add-hook 'pre-command-hook #'vertico-enable)

  (with-eval-after-load 'vertico (setq vertico-cycle t))
  (setq-default completion-styles '(orderless basic))
  (setq completion-styles '(basic partial-completion orderless)
        completion-category-overrides
        '((file (styles basic partial-completion))))

  ;;; Windows Control

  (load "~/.emacs.d/site-lisp/emacs-winum/winum.el")
  (winum-mode)

  ;;; UI
  (setq fill-column 80)
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'display-fill-column-indicator-mode))
  ;; (global-display-fill-column-indicator-mode)

  (defvar themes_chosen
    '(;;; Light theme
      ;; modus-operandi-tritanopia
      doom-rouge
      ;;; Dark theme
      ;; manoj-dark
      ;; doom-rouge
      modus-vivendi
      )
    "Set for themes for dark and light mode.")
  (require 'doom-themes)
  (if (or
       (>= (string-to-number (substring (current-time-string) 11 13)) 18)
       (<= (string-to-number (substring (current-time-string) 11 13)) 6))
	  (progn
        (if (equal (cadr themes_chosen) 'modus-vivendi)
            (progn
              (setq modus-themes-org-blocks 'gray-background
	                modus-themes-bold-constructs t
	                modus-themes-italic-constructs t)
              (load-theme (car (cdr themes_chosen)) t)
              (set-face-attribute 'modus-themes-heading-1 nil :height 1.25))
          (if (equal (cadr themes_chosen) 'manoj-dark)
              (progn
                (load-theme (car (cdr themes_chosen)) t)
                (set-face-foreground 'hl-line 'unspecified)
                (set-face-background 'fringe 'unspecified))
            (progn
              (load-theme (cadr themes_chosen) t)
              (setq doom-rouge-brighter-comments t
                    doom-rouge-brighter-tabs t)))))
    (progn
      ;; (load-theme (car themes_chosen) t)
      (when (eq custom-enabled-themes nil)
        ;; (set-face-bold 'font-lock-keyword-face t)
        ;; (set-face-bold 'font-lock-builtin-face t)
        (set-face-background 'highlight "#DFEAEC")
        (set-face-background 'fringe 'unspecified)
        (set-face-attribute 'line-number-current-line nil :foreground
                            "#000000" :background "#C4C4C4" :weight
                            'bold)
        (set-face-bold 'font-lock-keyword-face 't)
        )
      (setq modus-themes-org-blocks 'gray-background
	        modus-themes-bold-constructs t
            modus-themes-italic-constructs t)))

  (if (equal (frame-parameter nil 'background-mode) 'dark)
      (set-face-attribute 'mode-line nil
                          :background "black"
                          :box nil
    				      :font (font-spec
    						     ;; "JetBrainsMono Nerd Font" "Monego Ligatures" "Maple Mono NF"
							     :name
							     "JetBrainsMono Nerd Font"
                                 :size
							     11.0)
                          :underline
                          (face-foreground 'mode-line-emphasis))
    (progn
      (set-face-attribute 'mode-line nil
    				      ;; :background "#F4F7FA"
					      :background "white"
					      :box nil
					      :font (font-spec
    						     :name
							     "JetBrainsMono Nerd Font"
							     :size 11.0))
      (with-eval-after-load 'org
        (set-face-attribute 'org-verbatim nil
                            :foreground "#e74c3c"
                            :box '(:line-width 1 :color "#e1e4e5")))
      (with-eval-after-load 'tab-bar
        (set-face-attribute 'tab-bar nil
                            :font (font-spec
                                   :name
                                   "JetBrainsMono Nerd Font"
                                   :size 11.0))
        (set-face-attribute 'tab-bar-tab nil
                            :background (face-attribute 'default :background)
                            :box 'unspecified))))

  (set-face-attribute
   'mode-line-inactive nil
   :inherit 'mode-line
   :box nil)

  ;; (dolist (hook '(prog-mode-hook cuda-mode-hook))
  ;;   (add-hook hook 'highlight-indent-guides-mode))

  ;; (dolist (hook '(
  ;;   			  ;; completion-list-mode-hook
  ;;   			  ;; completion-in-region-mode-hook
  ;;   			  term-mode-hook
  ;;   			  ;; shell-mode-hook
  ;;   			  messages-buffer-mode-hook
  ;;   			  org-roam-mode-hook))
  ;;   (add-hook hook 'hide-mode-line-mode))

  (when (boundp 'hl-todo)
    (global-hl-todo-mode))

  (with-eval-after-load 'highlight-indent-guides
    (setq highlight-indent-guides-method 'character
		  highlight-indent-guides-responsive 'top
		  highlight-indent-guides-suppress-auto-error t))
  )

(provide 'config)
;;; config.el ends here.
