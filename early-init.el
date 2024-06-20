;;; early-init --- Early Init ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))

(if (string-equal system-type "windows-nt")
    (progn
      (add-to-list 'default-frame-alist '(width . 180))
      (add-to-list 'default-frame-alist '(height . 50)))
  ;; (add-to-list 'default-frame-alist '(alpha-background . 60))
  )

(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist '(height . 60))

;; 以下方式与上面相同
;; (push '(undecorated . t) default-frame-alist)
;; (push '(vertical-scroll-bar) default-frame-alist)
;; (push '(alpha-background . 85) default-frame-alist)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)

;; (load-theme 'modus-operandi t)

(setq gc-cons-threshold (* 80 1024 1024)  ;; most-positive-fixnum
      gc-cons-percentage 0.6
      ;; garbage-collection-messages t ;; for debug
      package-quickstart nil
      ;; package-enable-at-startup nil
      ;; package--init-file-ensure t
      inhibit-startup-message t
      ;; inhibit-startup-screen t
      ;; initial-buffer-choice "*Emacs*"
      ;; auto-mode-case-fold nil
      ;; Remove irreleant command line options for faster startup
      ;; command-line-x-option-alist nil
      confirm-kill-processes t  ;; 退出自动杀线程
      ;; 取消光标闪烁
      blink-cursor-mode nil
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
      ;; 让窗口启动更平滑
      frame-inhibit-implied-resize t
      ;; Don't popup dialog
      use-dialog-box nil
      ;; 粘贴于光标处,而不是鼠标指针处
      mouse-yank-at-point t
      ;; global-auto-revert-non-file-buffers t
      ;; fast-but-imprecise-scrolling t
      jit-lock-defer-time 0
      ;; 提高 IO 性能
      process-adaptive-read-buffering nil
      read-process-output-max (* 1024 1024)
      ;; 避免 redefine warning
      ;; ad-redefinition-action 'accept
      ;; make-backup-files t   ;; 设置是否生成备份文件，例如 configuration.org~
      auto-mode-case-fold nil
      )

(setq-default
 pgtk-wait-for-event-timeout 0
 truncate-lines t
 fill-column 100
 ;; mode-line-format nil
 )

(setenv "LSP_USE_PLISTS" "true")    ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization

;;; early-init.el ends here.
