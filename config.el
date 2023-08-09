;; -*- lexical-binding: t; -*-
;;; Config.el --- Emacs configuration file
;;; Commentary:
;;
;; Nothing to say
;;

;;; Code:

;;; Basic

(package-initialize)

(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)
(setq-default pgtk-wait-for-event-timeout 0)

(require 'gcmh)
(with-eval-after-load 'gcmh (setq gcmh-auto-idle-delay-factor 10
								  gcmh-high-cons-threshold (* 16 1024 1024)))
(gcmh-mode t)

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

(setq frame-title-format
  	  '((:eval (if (buffer-file-name)
  				   (abbreviate-file-name (buffer-name))
  				 "%b"))))

(setq confirm-kill-processes t  ;; 退出自动杀线程
	  ;; 取消光标闪烁
	  blink-cursor-mode nil
	  package-enable-at-startup nil
	  ;; 使用字体缓存
	  inhibit-compacting-font-caches t
	  use-short-answers t
	  ;; 平滑地进行半屏滚动，避免滚动后 recenter 操作
	  scroll-step 1
      scroll-margin 1
      hscroll-step 1
      hscroll-margin 1
	  scroll-conservatively 1000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
	  ;; Don't popup dialog
	  use-dialog-box nil
	  ;; 粘贴于光标处,而不是鼠标指针处
	  mouse-yank-at-point t
	  global-auto-revert-non-file-buffers t
	  fast-but-imprecise-scrolling t
	  jit-lock-defer-time 0
	  ;; 提高 IO 性能
	  process-adaptive-read-buffering nil
	  read-process-output-max (* 1024 1024)
	  ;; 避免 redefine warning
	  ;; ad-redefinition-action 'accept
      )

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
(global-hl-line-mode)
(dolist (hook '(eshell-mode-hook shell-mode-hook term-mode-hook messages-buffer-mode-hook))
  (add-hook hook (lambda ()
				   (setq-local global-hl-line-mode nil))))

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
	  scroll-preserve-screen-position t
	  scroll-margin 0
	  scroll-conservatively 97)

(delete-selection-mode)

;; (setq-default cursor-type '(bar . 3))
;; (display-time-mode 1)
;; (setq display-time-day-and-date t)

(add-hook 'prog-mode-hook
		  (lambda ()
			(setq-local show-paren-when-point-inside-paren t
  						show-paren-when-point-in-periphery t
  						show-paren-context-when-offscreen 'overlay
						)))

(add-hook 'cuda-mode-hook
		  (lambda ()
			(setq-local show-paren-when-point-inside-paren t
  						show-paren-when-point-in-periphery t
  						show-paren-context-when-offscreen 'overlay  ;; child-frame
						)))

(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-save")
(require 'auto-save)
(auto-save-enable)

(setq auto-save-delete-trailing-whitespace t)
(setq auto-save-disable-predicates
      '((lambda ()
		  (string-suffix-p
		   "gpg"
		   (file-name-extension (buffer-name)) t))))

;; (setq make-backup-files t)			;; 设置是否生成备份文件，例如 configuration.org~

(require 'no-littering-autoloads)
;; (require 'no-littering)

(add-hook 'after-init-hook (lambda ()
			                 (recentf-mode t)
			                 (setq recentf-max-saved-items 1000
				                   recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'")))
			                 (with-eval-after-load 'no-littering
			                   (add-to-list 'recentf-exclude no-littering-var-directory)
			                   (add-to-list 'recentf-exclude no-littering-etc-directory))
			                 ;; (fset 'yes-or-no-p 'y-or-n-p)
			                 (save-place-mode t)))

(add-hook 'minibuffer-setup-hook (lambda ()
                                   (setq history-length 10000
				                         history-delete-duplicates t
				                         savehist-save-minibuffer-history t)
                                   (savehist-mode t)))

(setq history-delete-duplicates t
	  recentf-max-menu-items 5
	  ring-bell-function 'ignore
	  isearch-lazy-count t
	  lazy-highlight-cleanup nil
      ;; 处理中英文断行不分割问题，需要开启 toggle-word-wrap 和 visual-line-mode 才能体现
	  word-wrap-by-category t)

(setq-default tab-width 4
			  tab-always-indent t
			  tab-first-completion 'word-or-paren-or-punct
			  indent-tabs-mode nil
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(dolist (mode '(prog-mode-hook TeX-mode-hook cuda-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode t))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)
(add-to-list 'package-archives '("elpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/") t)
(add-to-list 'package-archives '("non-elpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/") t)
(add-to-list 'package-archives '("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org") t)

;; Original packages repository
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)



(keymap-global-set "C-x C-r" 'restart-emacs)
(keymap-global-set "M-o" 'other-window)
(keymap-global-set "M-N" 'windmove-down)
(keymap-global-set "M-P" 'windmove-up)
(keymap-global-set "M-I" 'windmove-right)
(keymap-global-set "M-O" 'windmove-left)
;;replace =isearch-delete-char= with =isearch-del-char=
(keymap-set isearch-mode-map "C-h" 'isearch-del-char)

(setq desktop-path (list user-emacs-directory))
;; 	desktop-auto-save-timeout 600)
;; (desktop-save-mode 1)


;; ;;; Efficiency

(keymap-global-set "C-x f" 'find-file)
(keymap-global-set "C-z" 'undo)

(defun comment-or-uncomment ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (if (save-excursion
          (beginning-of-line)
          (looking-at "\\s-*$"))
        (call-interactively 'comment-dwim)
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

(global-set-key [remap comment-dwim] 'comment-or-uncomment)
(setq comment-auto-fill-only-comments t)
(dolist (hook '(prog-mode-hook))
  (add-hook hook #'whitespace-mode))
(setq whitespace-style '(face trailing))

(electric-indent-mode -1)

;; (follow-mode)

;; ;; Smoothly scrolling over image
(require 'iscroll)
(add-hook 'text-mode-hook 'iscroll-mode)

(setq auto-mode-alist
	  (cons '("\\.pdf\\'" . pdf-view-mode) auto-mode-alist))

(run-with-idle-timer 10 nil #'require 'magit)

;; (global-aggressive-indent-mode t)
(dolist (hook '(emacs-lisp-mode-hook c++-ts-mode-hook c-ts-mode-hook cuda-mode-hook))
  (add-hook hook 'aggressive-indent-mode))
;; (dolist (mode '(verilog-mode org-mode term-mode))
;;   (add-to-list 'aggressive-indent-excluded-modes mode))

(dolist (hook '(term-mode-hook))
  (add-hook hook #'puni-disable-puni-mode))

(keymap-global-set "C-c k" 'browse-kill-ring)
(add-hook 'after-init-hook 'browse-kill-ring-default-keybindings)
(with-eval-after-load 'browse-kill-ring (setq browse-kill-ring-separator "------------------------------"
  											  browse-kill-ring-separator-face 'shadow))



;; Meow
;; (load "~/.emacs.d/meow.el")

;;; Visual Repalcement

(keymap-global-set "C-c r" 'vr/replace)
(keymap-global-set "C-c m" 'vr/mc-mark)

(add-hook 'prog-mode-hook 'hs-minor-mode)

(keymap-set minibuffer-mode-map "C-w" 'backward-kill-word)

;; 折叠代码块，以下是额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face '(:inherit font-lock-comment-face :box t))

;;;###autoload
(defun hideshow-folded-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
  		   (info (format " ... #%d " nlines)))
  	  (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

(setq hs-set-up-overlay 'hideshow-folded-overlay-fn)

(run-with-timer 3.5 nil #'require 'avy)
(with-eval-after-load 'avy (avy-setup-default))

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
 Line^^           char^^              word^^		          Page^^
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
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil))

(keymap-global-set "C-x SPC" 'hydra-rectangle/body)

(with-eval-after-load 'avy
  (keymap-set isearch-mode-map "M-j" 'avy-isearch)
  (keymap-global-set "C-'" 'avy-goto-char-in-line)
  ;; (global-set-key (kbd "C-:") 'avy-goto-char)
  ;; (global-set-key (kbd "M-g c") 'avy-goto-char-timer)
  ;; (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  ;; (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  ;; (global-set-key (kbd "M-g f") 'avy-goto-line)
  ;; (global-set-key (kbd "C-c C-j") 'avy-resume)
  )

(with-eval-after-load 'multiple-cursors
  (keymap-global-set "C-S-c C-S-c" 'mc/edit-lines)
  (keymap-global-set "C->" 'mc/mark-next-like-this)
  (keymap-set mc/keymap (kbd "<return>") nil)
  )

(global-set-key [remap move-beginning-of-line] 'mwim-beginning-of-code-or-line)
(global-set-key [remap move-end-of-line] 'mwim-end-of-code-or-line)

;;;###autoload
(defun open-newline-above (arg)
  "Move to the previous line (like vi) and then opens a line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (if (not (member major-mode '(org-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))
;;;###autoload
(defun open-newline-below (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (if (not (member major-mode '(org-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))

(keymap-global-set "M-j" 'open-newline-above)
(keymap-global-set "C-j" 'open-newline-below)

;; (require 'emacsql-sqlite-builtin)

(flycheck-define-checker c/c++-gcc
  "A C/C++ syntax checker using GCC.

  Requires GCC 4.4 or newer.  See URL `https://gcc.gnu.org/'."
  :command ("gcc"
  			"-fshow-column"
  			"-iquote" (eval (flycheck-c/c++-quoted-include-directory))
  			(option "-std=" flycheck-gcc-language-standard concat)
  			(option-flag "-pedantic" flycheck-gcc-pedantic)
  			(option-flag "-pedantic-errors" flycheck-gcc-pedantic-errors)
  			(option-flag "-fno-exceptions" flycheck-gcc-no-exceptions)
  			(option-flag "-fno-rtti" flycheck-gcc-no-rtti)
  			(option-flag "-fopenmp" flycheck-gcc-openmp)
  			(option-list "-include" flycheck-gcc-includes)
  			(option-list "-W" flycheck-gcc-warnings concat)
  			(option-list "-D" flycheck-gcc-definitions concat)
  			(option-list "-I" flycheck-gcc-include-path)
  			(eval flycheck-gcc-args)
  			"-x" (eval
  				  (pcase major-mode
  					(`c-mode "c")
  					(`c++-mode "c++")
  					(`c-ts-mode "c")
  					(`c++-ts-mode "c++")))
  			;; GCC performs full checking only when actually compiling, so
  			;; `-fsyntax-only' is not enough. Just let it generate assembly
  			;; code.
  			"-S" "-o" null-device
  			;; Read from standard input
  			"-")
  :standard-input t
  :error-patterns
  ((info line-start (or "<stdin>" (file-name))
  		 ":" line (optional ":" column)
  		 ": note: " (message) line-end)
   (warning line-start (or "<stdin>" (file-name))
  			":" line (optional ":" column)
  			": warning: " (message (one-or-more (not (any "\n["))))
  			(optional "[" (id (one-or-more not-newline)) "]") line-end)
   (error line-start (or "<stdin>" (file-name))
  		  ":" line (optional ":" column)
  		  ": " (or "fatal error" "error") ": " (message) line-end))
  :modes (c-mode c++-mode c-ts-mode c++-ts-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

(global-flycheck-mode)
;; (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)
;; (with-eval-after-load 'flycheck (setq flycheck-indication-mode 'left-margin))

(add-to-list 'treesit-extra-load-path "/usr/local/lib")

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
  )

;; (add-hook 'c-ts-mode-hook (lambda () (setq c-ts-mode-indent-offset 4)))

(defun insert-tab-char()
  "Insert a tab char. (ASCII 9, \t)."
  (interactive)
  (insert "\t"))


;; (evil-mode t)
(with-eval-after-load 'evil
  (evil-set-undo-system 'undo-redo)
  (setq-default evil-want-abbrev-expand-on-insert-exit nil)
  (setq-default evil-want-keybinding nil)
  (evil-define-key 'normal sort-tab-mode-map (kbd "[ b") 'sort-tab-select-prev-tab)
  (evil-define-key 'normal sort-tab-mode-map (kbd "] b") 'sort-tab-select-next-tab)
  (evil-define-key 'normal sort-tab-mode-map (kbd "[ B") 'sort-tab-select-first-tab)
  (evil-define-key 'normal sort-tab-mode-map (kbd "] B") 'sort-tab-select-last-tab)
  (require 'evil-nerd-commenter)
  (keymap-global-set "M-/" 'evil-comment-or-uncomment-lines)
  ;; (evil-define-key 'insert 'prog-mode-map (kbd "TAB") 'insert-tab-char)
  (evil-define-key '(insert normal) org-mode-map (kbd "TAB") 'org-cycle)
  (require 'evil-nerd-commenter))

;; (require 'cape)
(with-eval-after-load 'cape (add-to-list 'completion-at-point-functions #'cape-file))

(with-eval-after-load 'dired
  (require 'image-dired)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group"
        dired-dwim-target t
        dired-mouse-drag-files t
        mouse-drag-and-drop-region-cross-program t
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t
        dired-dwim-target t
        image-dired-thumb-size 256
        image-dired-marking-shows-next nil))
(add-hook 'dired-mode-hook #'diredfl-global-mode)
(add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish")
(add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish/extensions")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/Autoloads/dirvish-autoload.el")
;; (require 'dirvish-autoload)
;; (autoload 'dired "dirvish" nil t)
;; (autoload 'dirvish "dirvish" nil t)
;; (autoload 'dirvish-side "dirvish-side" nil t)
;; (require 'dirvish)
;; (require 'dirvish-side)
(with-eval-after-load 'dirvish
  ;; (dirvish-peek-mode)
  (dirvish-override-dired-mode)
  (dirvish-side-follow-mode))
;; (dirvish-override-dired-mode)
;; (dirvish-side-follow-mode)
;; (dirvish-peek-mode)
(with-eval-after-load 'dirvish
  (add-hook 'dirvish-setup-hook 'dirvish-emerge-mode)
  (setq dirvish-attributes '(vc-state file-size git-msg subtree-state collapse file-time)
		dirvish-side-width 30)
  (keymap-set dirvish-mode-map "TAB" #'dirvish-toggle-subtree))
;; (require 'which-key)
(run-with-timer 2 nil #'require 'which-key)
(when (display-graphic-p)
  (with-eval-after-load 'which-key
    (which-key-mode t)
	(setq which-key-max-description-length 30
		  which-key-show-remaining-keys t)
    ;; (when (package-installed-p 'which-key-posframe)
	;;   (which-key-posframe-mode)
	;;   (setq which-key-posframe-border-width 3
	;; 	    which-key-posframe-poshandler #'posframe-poshandler-frame-bottom-center
	;; 	    which-key-posframe-parameters '((left-fringe . 8)
	;; 									    (right-fringe . 8))))
    ))

;; (keymap-global-set "C-x C-b" 'ibuffer)
(define-key global-map [remap list-buffers] 'ibuffer)

(require 'consult)
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

(require 'orderless)

;;; Theme

(dolist (hook '(prog-mode-hook text-mode-hook cuda-mode-hook))
  (add-hook hook #'rainbow-mode))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/doom-themes")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/third-party-themes")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/lambda-themes/")
;; (require 'lambda-themes)
;; (load-theme 'paperlike t)
;; (require 'color-theme-sanityinc-tomorrow)
;; (color-theme-sanityinc-tomorrow-bright)
;; (color-theme-sanityinc-tomorrow-bright)



(defvar themes_chosen
  '(modus-operandi modus-vivendi)
  "Set for themes for dark and light mode.")

;;; Minibuffer Setting

;; (ivy-mode 1)
(add-hook 'pre-command-hook (vertico-mode t))
(with-eval-after-load 'vertico (setq vertico-cycle t))
(setq-default completion-styles '(orderless basic))
(setq completion-styles '(basic partial-completion orderless)
      completion-category-overrides '((file (styles basic partial-completion))))

;; (defun sanityinc/use-orderless-in-minibuffer ()
;;   "Setup orderless for minibuffer."
;;   (setq-local completion-styles '(substring orderless)))
;; (add-hook 'minibuffer-setup-hook 'sanityinc/use-orderless-in-minibuffer)

(when (package-installed-p 'vertico)
  (marginalia-mode))

;;; Windows Control

(with-eval-after-load 'winum
  (setq winum-ignored-buffers '("*sort-tab*")
		winum-auto-setup-mode-line nil)
  (defun +win-num ()
	(let ((n (winum-get-number)))
  	  (alist-get
  	   n
  	   '(
		 (0 . "🄌")
		 (1 . "❶")
		 (2 . "❷")
  		 (3 . "❸")
  		 (4 . "❹")
  		 (5 . "❺")
  		 (6 . "❻")
  		 (7 . "❼")
  		 (8 . "❽")
  		 (9 . "❾")
		 ))))
  (add-hook 'winum-mode-hook '(lambda () (setq winum-auto-setup-mode-line nil))))
(winum-mode)


(with-eval-after-load 'popper
  (setq popper-reference-buffers '("\\*Messages\\*"
								   "Output\\*$"
								   "\\*Async Shell Command\\*"
								   "Go-Translate"
								   help-mode
								   helpful-mode
								   compilation-mode
								   youdao-dictionary-mode)
		popper-window-height  (lambda (win)
								(fit-window-to-buffer
								 win
								 (floor (frame-height) 2))))
  (keymap-global-set "M-`" 'popper-toggle-latest)  ;; shadow tmm-menubar
  (keymap-global-set "C-M-`" 'popper-toggle-type))
(popper-mode t)
(popper-echo-mode t)

;;; UI

;;;###autoload
(defun set-en_cn-font (en-font cn-font f-size)
  "EN-FONT for English, CN-FONT for Chinese, F-SIZE represents font size.
Set Font for both of English and Chinese characters."
  (set-face-attribute
   'default nil
   :font (font-spec
		  :name en-font
		  :weight 'normal
  		  :slant 'normal
  		  :size f-size))

  (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font "fontset-default" charset (font-spec :family cn-font))
	))

(when (display-graphic-p)
  (set-en_cn-font "PragmataPro Mono Liga" "SimHei" 14.0)
  ;; Maple Mono NF --- Maple Mono SC NF, HarmonyOS Sans SC
  ;; PragmataPro --- SimHei
  ;; Hack --- HarmonyOS Sans SC
  ;; JetBrainsMono Nerd Font
  ;; "Iosevka Fixed"    ;; Input Mono

  ;; ;; Enable Ligatures Feature, more info: https://github.com/mickeynp/ligature.el
  ;; (global-ligature-mode)
  ;; ;; PragmataPro Font Ligature Support
  ;; (ligature-pragmatapro-setup)

  ;; Don't use help echo tooltips
  (setq x-gtk-use-system-tooltips nil)
  )

(if (or (>= (string-to-number (substring (current-time-string) 11 13)) 19) (<= (string-to-number (substring (current-time-string) 11 13)) 6))
	(progn
      (setq modus-themes-org-blocks 'gray-background)
	  (setq modus-themes-bold-constructs t)
	  (setq modus-themes-italic-constructs t)
      (load-theme (car (cdr themes_chosen)) t)
      (set-face-attribute 'fringe nil :background 'unspecified)
	  (set-face-attribute 'line-number nil :background 'unspecified)
      (set-face-attribute 'mode-line nil
  					      :background "#0A0E12"
                          ;; :background "black"
					      ;; :overline "white"
					      :box nil
  					      :font (font-spec
  						  	     ;; "JetBrainsMono Nerd Font" "Monego Ligatures"
							     :name "Maple Mono NF"
							     ;; :weight 'normal
  						  	     :size 13.0))
      ;; (unless (symbol-value x-gtk-use-system-tooltips)
      ;; 	(set-face-attribute 'tooltip nil))
      )
  (progn
	(setq modus-themes-org-blocks 'gray-background)
	(setq modus-themes-bold-constructs t)
	(setq modus-themes-italic-constructs t)
    (load-theme (car themes_chosen) t)
    (set-face-attribute 'fringe nil :background 'unspecified)
	(set-face-attribute 'line-number nil :background 'unspecified)
    (set-face-attribute 'mode-line nil
  					    ;; :background "#F4F7FA"
					    :background "white"
					    :box nil
					    :font (font-spec
  							   :name "Maple Mono NF"
							   :size 13.0))))
(set-face-attribute 'mode-line-inactive nil :inherit 'mode-line :box nil)

(setq x-underline-at-descent-line t)

;; (require 'all-the-icons)

(add-to-list 'load-path "~/.emacs.d/site-lisp/nerd-icons")
(add-to-list 'load-path "~/.emacs.d/site-lisp/treemacs-nerd-icons")
(add-to-list 'load-path "~/.emacs.d/site-lisp/nerd-icons-dired")
;; (require 'nerd-icons-dired)
;; (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)
;; (require 'treemacs-nerd-icons)
;; (treemacs-load-theme "nerd-icons")

(require 'nerd-icons)
(load "~/.emacs.d/self-develop/modeline-setting.elc")
(run-with-timer 5 nil #'(lambda ()
						  (require 'nerd-icons-dired)
						  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)))
(with-eval-after-load 'treemacs
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))
(add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
;; (if (package-installed-p 'nerd-icons)
;; 	(progn
;; 	  (require 'nerd-icons)
;; 	  (load "~/.emacs.d/self-develop/modeline-setting.elc")
;; 	  (run-with-timer 5 nil #'(lambda ()
;; 								(require 'nerd-icons-dired)
;; 								(add-hook 'dired-mode-hook 'nerd-icons-dired-mode)))
;; 	  (with-eval-after-load 'treemacs
;; 		(require 'treemacs-nerd-icons)
;; 		(treemacs-load-theme "nerd-icons"))
;; 	  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)))

(with-eval-after-load 'all-the-icons (load "~/.emacs.d/self-develop/all-the-icons-diy.el"))

;; (with-eval-after-load 'nerd-icons
;; (setq fc-info (nerd-icons-codicon "nf-cod-question" :face '(:inherit flycheck-info-my))
;; 		fc-warning (nerd-icons-codicon "nf-cod-warning" :face '(:inherit flycheck-warn))
;; 		fc-error (nerd-icons-codicon "nf-cod-error" :face '(:inherit flycheck-error-my)))
;; )

(with-eval-after-load 'sort-tab
  (dolist (face '(sort-tab-other-tab-face sort-tab-current-tab-face sort-tab-separator-face))
	(set-face-attribute face nil :font (font-spec
										:name "JetBrains Mono"
										:size 10.0))
	))

;; (dolist (hook '(prog-mode-hook cuda-mode-hook))
;;   (add-hook hook 'highlight-indent-guides-mode))

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

(dolist (hook '(
				;; completion-list-mode-hook
				;; completion-in-region-mode-hook
				term-mode-hook
				;; shell-mode-hook
				messages-buffer-mode-hook
				org-roam-mode-hook ))
  (add-hook hook #'hide-mode-line-mode))

(when (package-installed-p 'hl-todo)
  (global-hl-todo-mode))

(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'character
		highlight-indent-guides-responsive 'top
		highlight-indent-guides-suppress-auto-error t))

;;; Coding Relpated

;; (require 'lsp-mode)
;; (require 'lsp-ui)

(with-eval-after-load 'verilog-mode
  ;;   (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
  ;; 										:major-modes '(verilog-mode)
  ;; 										:priority -1))
  (setq verilog-indent-lists nil)
  )

;; (add-hook 'verilog-mode-hook '(lambda ()
;; (lsp)
;; (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
;; ))
(with-eval-after-load 'lsp-mode
  (dolist (hook '(prog-mdoe-hook cuda-mode-hook TeX-mode-hook))
	'(lambda ()
	   (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
		 (lsp-deferred))))
  (dolist (hook '(markdown-mode-hook yaml-ts-mode yaml-mode))
	(add-hook hook #'lsp-deferred))
  (setq lsp-prefer-flymake nil
		lsp-ui-flycheck-enable t
		lsp-disabled-clients '(svlangserver))
  (keymap-set lsp-mode-map "C-c C-d" #'lsp-describe-thing-at-point)
  )

(dolist (hook '(cuda-mode-hook))  ;; prog-mode-hook  TeX-mode-hook
  (add-hook hook 'yas-minor-mode))

(with-eval-after-load 'lsp-mode
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
		lsp-lens-enable nil

		;; ui
		lsp-ui-doc-show-with-cursor nil
		)
  )

(add-hook 'lsp-mode-hook #'(lambda ()
							 (setq lsp-enable-relative-indentation t)
							 (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
							 (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

							 ;; For diagnostics
							 (lsp-diagnostics-mode -1)
							 ))

;; (dolist (completion '(company-mode corfu-mode))
;;   (with-eval-after-load completion
;; 	(let ((hls '(emacs-lisp-mode-hook lisp-mode-hook TeX-mode-hook verilog-mode-hook)))
;; 	  (dolist (mode hls)
;; 		(add-hook mode #completion)
;; 		)
;; 	  )))

(with-eval-after-load 'corfu
  (setq corfu-auto t
		corfu-cycle t
		corfu-quit-no-match t  ;; 'separator
		corfu-auto-prefix 2
		corfu-auto-delay 0
		corfu-preview-current t
		)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

(add-hook 'corfu-mode-hook
		  (lambda ()
			(corfu-popupinfo-mode)
			(setq corfu-popupinfo-delay '(0.2 . 0.1))
			))
;; (global-corfu-mode)

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
				  )

			(define-key company-active-map (kbd "C-h") 'delete-backward-char)
			(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
			;; (setq company-box-icons-alist 'company-box-icons-idea)
			(setq company-box-scrollbar 'inherit)
			(company-box-mode t)
			)
		  )

(add-hook 'prog-mode-hook (lambda ()
							(setq company-backends '((company-capf :with company-yasnippet)
													 (company-dabbrev-code company-keywords company-files)
													 company-dabbrev))
							(toggle-truncate-lines)
							))

(defun company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))

(advice-add 'company-capf :around #'company-completion-styles)

(add-to-list 'load-path "/home/Leo/.emacs.d/site-lisp/lsp-bridge/")

;; (require 'lsp-bridge)
(run-with-timer 1.7 nil #'require 'lsp-bridge)

;; (with-eval-after-load 'lsp-bridge
;;   (when (treesit-available-p)
;; 	(let ((lsp-bridge-temp-list lsp-bridge-default-mode-hooks))
;; 	  (dolist (hook lsp-bridge-temp-list)
;; 		(add-to-list 'lsp-bridge-default-mode-hooks (intern (string-replace "mode" "ts-mode" (symbol-name hook))))))))

(add-hook 'lsp-bridge-mode-hook '(lambda ()
								   (yas/minor-mode t)
								   (setq lsp-bridge-enable-diagnostics nil
										 lsp-bridge-c-lsp-server "ccls")
								   ))

(with-eval-after-load 'lsp-bridge
  (global-lsp-bridge-mode)
  (with-current-buffer (get-buffer-create "*scratch*")
	(lsp-bridge-mode))
  (setq acm-candidate-match-function 'orderless-flex
		;; acm-enable-icon t
		;; acm-enable-doc t
		acm-enable-yas t
		acm-enable-tempel t
		acm-enable-quick-access nil
		acm-enable-search-file-words t
		acm-enable-telega nil
		acm-enable-tabnine nil
		;; lsp-bridge-enable-log t
		lsp-bridge-enable-signature-help t
		lsp-bridge-enable-diagnostics nil
		lsp-bridge-complete-manually nil
		lsp-bridge-enable-profile t
		;; lsp-bridge-multi-lang-server-mode-list nil
		acm-backend-lsp-candidate-min-length 2
		acm-backend-elisp-candidate-min-length 2
		acm-backend-search-file-words-candidate-min-length 3
		acm-backend-yas-candidate-min-length 1

		;; This will cause `org-roam-node-find' get wrong and I don't know why.
		;; lsp-bridge-enable-org-babel t
		))

;; Input Method

(run-with-timer 3 nil #'(lambda ()
						  ;; (require 'posframe)
						  (require 'rime)))

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
  (ignore-errors (rime-commit1))
  (toggle-input-method)
  )
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


;; ;;; Org Mode

;; Hide spaces of chinese inline block
(font-lock-add-keywords 'org-mode
                        '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                          ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
                           (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                        'append)

;; (custom-theme-set-faces
;; 'user
;; ;; '(fixed-pitch ((t (:family "Essential PragmataPro" :height 1.0))))
;; ;; '(variable-pitch ((t (:family "Bookerly" :height 1.0))))
;; '(fixed-pitch ((t (:family "Source Code Pro" :height 1.0))))
;; '(variable-pitch ((t (:family "Source Code Pro" :height 1.0))))
;; '(org-table ((t (:inherit fixed-pitch))))
;; '(org-tag ((t (:inherit fixed-pitch))))
;; '(org-verbatim ((t (:inherit fixed-pitch))))
;; '(org-src ((t (:inherit fixed-pitch)))))

(with-eval-after-load 'org
  ;; (set-face-attribute 'org-table nil :font (font-spec :name "Sarasa Mono SC" ; LXGW WenKai
  ;; 													  :weight 'semibold
  ;; 													  :size 13.0)
  ;;  )
  (setq org-hide-emphasis-markers t
		org-pretty-entities t
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
												   (1.1 . 1.1)
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
		org-preview-latex-default-process 'dvisvgm
		org-format-latex-header "\\documentclass[10pt]{article}\n\\usepackage[usenames]{color}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\pagestyle{empty}  % do not remove\n% The settings below are copied from fullpage.sty\n
\\usepackage{xeCJK,tikz,caption,float,makecell,circuitikz,array}\n
\\usetikzlibrary{shapes,arrows,calc,arrows.meta}\n
\\usetikzlibrary{circuits.logic.IEC,calc}\n
\\renewcommand{\\arraystretch}{1.3}\n
\\setlength{\\textwidth}{\\paperwidth}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}\n"))

;; (add-hook 'org-mode-hook 'variable-pitch-mode)

(add-hook 'org-mode-hook 'electric-indent-local-mode)
(add-hook 'org-mode-hook
		  (lambda ()
			(setq-local company-backends '(company-files company-keywords))
			(setq )
			(org-appear-mode)
			;; (company-mode)
			;; (corfu-mode)
			(visual-line-mode)
			)
		  )

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

;; (require 'valign)
;; (add-hook 'org-mode-hook #'valign-mode)

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
		org-modern-hide-stars nil
		org-modern-checkbox nil
		;; org-modern-star t
		org-modern-list
		'(
		  ;; (?- . "-")
		  (?* . "•")
		  (?+ . "‣"))
		))

(add-to-list 'load-path "~/.emacs.d/site-lisp/org-modern-indent")
;; (add-hook 'org-agenda-finalize-hook 'org-modern-agenda)


(add-to-list 'load-path "~/.emacs.d/site-lisp/org-bars")
;; (require 'org-bars)
;; (add-hook 'org-mode-hook #'org-bars-mode)


;; (add-hook 'org-mode-hook 'visual-line-mode)
;; (add-hook 'org-mode-hook 'word-wrap-whitespace-mode)

(setq-default org-startup-folded 'overview
			  org-startup-with-inline-images t
              org-startup-indented t)
;; (setq-default org-highlight-latex-and-related '(native latex script entities))

(add-hook 'org-mode-hook
		  #'(lambda ()
			  (setq-local time-stamp-active t
						  time-stamp-start "#\\+MODIFIED: [ \t]*"
						  time-stamp-end "$"
						  time-stamp-format "\[%Y-%m-%d %3a %H:%M\]")
			  (setq org-list-allow-alphabetical t)
			  (add-hook 'before-save-hook 'time-stamp nil 'local)
			  (org-modern-mode)
			  ))

(run-with-timer 2 nil #'(lambda ()
                          (require 'org-contrib-autoloads)
                          (require 'org-modern-indent)))
(add-hook 'org-mode-hook #'org-modern-indent-mode 100)

;; (require 'org-roam)
(setq org-roam-directory "~/Personal/org-roam") ; 设置 org-roam 笔记的默认目录，缺省值 /home/leo/org-roam
(autoload 'org-roam-node-find "org-roam" nil t)
(keymap-global-set "C-c n f" 'org-roam-node-find)
(with-eval-after-load 'org-roam
  (add-hook 'org-roam-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'org-roam-mode-hook 'word-wrap-whitespace-mode)

  (org-roam-db-autosync-mode)

  (setq org-roam-db-gc-threshold most-positive-fixnum))

(setq org-roam-mode-sections '(org-roam-backlinks-section
							   org-roam-reflinks-section
							   org-roam-unlinked-references-section))

;; (add-to-list 'display-buffer-alist
;; 			 '("\\*org-roam*\\*"
;; 			   (display-buffer-in-side-window)
;; 			   (side . right)
;; 			   (window-width . 0.15)))

;; (with-eval-after-load 'org-roam
;;   Auto toggle org-roam-buffer.
;;   (defun my/org-roam-buffer-show (_)
;; 	(if (and
;; 		 Don't do anything if we're in the minibuffer or in the calendar
;; 		 (not (minibufferp))
;; 		 (not (> 120 (frame-width)))
;; 		 (not (bound-and-true-p olivetti-mode))
;; 		 (not (derived-mode-p 'calendar-mode))
;; 		 Show org-roam buffer iff the current buffer has a org-roam file
;; 		 (xor (org-roam-file-p) (eq 'visible (org-roam-buffer--visibility))))
;; 		(org-roam-buffer-toggle)))
;;   (add-hook 'window-buffer-change-functions 'my/org-roam-buffer-show)
;;   )

(with-eval-after-load 'org-roam
  (setq org-roam-database-connector 'sqlite-builtin)
  (setq org-roam-node-display-template
		(concat "${title:*} "
				(propertize "${tags:10}" 'face 'org-tag))))

(with-eval-after-load 'org-roam
  (setq org-roam-capture-templates
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
		   :unnarrowed t))))


;;; LaTeX

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; 设置xelatex为默认编辑命令，evince为pdf文件阅读器
(setq TeX-output-view-style (quote (("^pdf$" "." "envince %o %(outpage)"))))

(setq TeX-view-program-selection '(((output-dvi has-no-display-manager)
									"dvi2tty")
								   ((output-dvi style-pstricks)
									"dvips and gv")
								   (output-dvi "xdvi")
								   (output-pdf "Okular")
								   (output-html "xdg-open")))

(add-hook 'LaTeX-mode-hook (lambda()
							 (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
							 (setq TeX-command-default "XeLaTeX"))
		  'turn-on-cdlatex  ;; 设置cdlatex
		  )

(add-hook 'TeX-mode-hook (lambda()
						   (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
						   (setq TeX-command-default "XeLaTeX"))
		  'turn-on-cdlatex  ;; 设置cdlatex
		  )


;;; Ebook Reader

(with-eval-after-load 'pdf-tools
  (setq pdf-view-use-scaling t
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


;;; Helpful
(keymap-global-set "M-'" 'help-command)
(run-with-timer 1.7 nil #'require 'helpful)
(with-eval-after-load 'help
  (define-key global-map [remap describe-function] 'helpful-function)
  (define-key global-map [remap describe-key] 'helpful-key)
  (define-key global-map [remap describe-variable] 'helpful-variable)
  (define-key global-map [remap describe-command] 'helpful-command))


;;; Emacs Default Setting
;;;###autoload
(defun downcase-any (arg)
  "Downcase any situation."
  (interactive "p")
  (if (region-active-p)
	  (downcase-region (region-beginning) (region-end))
	(progn
	  (left-word)
	  (downcase-word arg)
	  )
	)
  )
;;;###autoload
(defun upcase-any (arg)
  "Upcase any situation."
  (interactive "p")
  (if (region-active-p)
	  (upcase-region (region-beginning) (region-end))
	(progn
	  (left-word)
	  (upcase-word arg)
	  )
	)
  )
;;;###autoload
(defun capitalize-any (arg)
  "Capitalize any situation."
  (interactive "p")
  (if (region-active-p)
	  (capitalize-region (region-beginning) (region-end))
	(progn
	  (left-word)
	  (capitalize-word arg)
	  )
	)
  )

;; Puni
;; (dolist (hook '(prog-mode-hook TeX-mode-hook))
;;   (add-hook hook #'puni-mode))

(if (not (boundp 'meow-mode))
	(progn
	  (keymap-global-unset "M-l")
	  (keymap-global-unset "M-c")
	  (keymap-global-unset "M-u")
	  (keymap-global-unset "C-w")
	  (keymap-global-unset "M-w")
	  (keymap-global-unset "C-k")
	  (keymap-global-set "C-h" 'backward-delete-char-untabify)
	  (keymap-global-set "C-w" 'kill-ring-save)
	  (keymap-global-set "M-w" 'puni-kill-region)
	  (keymap-global-set "C-k" 'puni-kill-line)
	  (keymap-global-set "M-l" 'downcase-any)
	  (keymap-global-set "M-c" 'upcase-any)
	  (keymap-global-set "M-u" 'capitalize-any))
  (unless 'meow-mode
	(progn
	  (keymap-global-unset "M-l")
	  (keymap-global-unset "M-c")
	  (keymap-global-unset "M-u")
	  (keymap-global-unset "C-w")
	  (keymap-global-unset "M-w")
	  (keymap-global-unset "C-k")
	  (keymap-global-set "C-h" 'backward-delete-char-untabify)
	  (keymap-global-set "C-w" 'kill-ring-save)
	  (keymap-global-set "M-w" 'puni-kill-region)
	  (keymap-global-set "C-k" 'puni-kill-line)
	  (keymap-global-set "M-l" 'downcase-any)
	  (keymap-global-set "M-c" 'upcase-any)
	  (keymap-global-set "M-u" 'capitalize-any))))


;;; Emacs-Chinese

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation")
(setq cns-prog "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cnws"
      cns-dict-directory "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cppjieba/dict"
      ;; To use other program for word segmentation, set cns-process-shell-command:
      ;; cns-process-shell-command "word_segmentation_program arg1 arg2..."
      ;; disable debug output, default is t
      cns-debug nil)
(require 'cns)
(when (featurep 'cns)
  (add-hook 'find-file-hook 'cns-auto-enable))
(run-with-timer 5 nil #'global-cns-mode nil)

;;; Abandoned Setting

;;; Emacs-application-framework

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework")
;; (require 'eaf)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (with-eval-after-load 'evil-mode (require 'eaf-evil))
;; (setq-default eaf-webengine-default-zoom 2.0)
;; (add-hook 'eaf-mode-hook '(lambda() ((local-set-key (kbd "\C-h") 'backward-delete-char) (local-set-key (kbd "\M-?") 'help-command))))

;;; Eshell

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/aweshell")
;; (require 'aweshell)

;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-14"))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'python-mode-hook 'eglot-ensure)


;;; Lsp-mode
;; (add-hook 'c-mode-hook #'lsp)
;; (add-hook 'c-mode-hook (lambda () (require 'ccls) (lsp-mode)))
;; (setq ccls-executable "ccls")
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

;; (global-company-mode)
;; (add-hook 'prog-mode-hook 'company-mode)
;; (add-hook 'emacs-lisp-mode 'company-mode)
;; (add-hook 'company-mode-hook 'company-box-mode)
;; (add-hook 'lsp-mode-hook 'company-mode)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/awesome-tab")
;; (require 'awesome-tab)
;; (awesome-tab-mode t)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/sort-tab")
;; (require 'sort-tab)
;; (with-eval-after-load 'sort-tab
;; (sort-tab-mode t)
;; (define-key sort-tab-mode-map (kbd "C-]") 'sort-tab-select-next-tab)
;; (keymap-set sort-tab-mode-map "M-[" 'sort-tab-select-prev-tab)
;; (centaur-tabs-mode t)
;; (setq centaur-tabs-cycle-scope 'tabs)
;; (setq centaur-tabs-style "bar")
;; (setq centaur-tabs-set-icons t)
;; (setq centaur-tabs-set-bar 'left)
;;  )

(provide 'config)
;;; config.el ends here
