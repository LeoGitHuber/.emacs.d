;;; early-init --- Early Init ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;;; Security
;; Allow potentially-dangerous elisp features for *my* config files only.
(let ((dir (abbreviate-file-name (file-name-as-directory user-emacs-directory))))
  (cond
   ((boundp 'trusted-content) (add-to-list 'trusted-content dir))
   ((boundp 'trusted-files)   (add-to-list 'trusted-files dir))))

;;; Frame/UI defaults
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(unless (eq system-type 'android)
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0)))
(scroll-bar-mode -1)

;;; Startup optimizations
(defvar my/original-file-name-handler-alist file-name-handler-alist)
(defvar my/original-gc-cons-threshold gc-cons-threshold)
(defvar my/original-gc-cons-percentage gc-cons-percentage)
(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/original-file-name-handler-alist
                  gc-cons-threshold my/original-gc-cons-threshold
                  gc-cons-percentage my/original-gc-cons-percentage)))

;; Frame size / appearance examples
;; (when (eq system-type 'windows-nt)
;;   (add-to-list 'default-frame-alist '(width . 180))
;;   (add-to-list 'default-frame-alist '(height . 50)))
;; (add-to-list 'default-frame-alist '(alpha-background . 90))
;; (push '(undecorated . t) default-frame-alist)
;; (push '(vertical-scroll-bar) default-frame-alist)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Startup / interaction
(setq package-quickstart nil
      ;; package-enable-at-startup nil
      ;; package--init-file-ensure t
      inhibit-startup-message t
      ;; inhibit-startup-screen t
      ;; initial-buffer-choice "*Emacs*"
      confirm-kill-processes t  ;; 退出自动杀线程
      blink-cursor-mode nil
      use-short-answers t
      use-dialog-box nil
      mouse-yank-at-point t
      auto-mode-case-fold nil
      warning-minimum-level :error)

;;; Scrolling behavior
(setq scroll-step 1
      scroll-margin 1
      hscroll-step 1
      hscroll-margin 1
      scroll-conservatively 1000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;;; Performance / IO
(setq inhibit-compacting-font-caches t
      jit-lock-defer-time 0
      process-adaptive-read-buffering nil
      read-process-output-max (* 1024 1024)
      frame-inhibit-implied-resize t)

(setq-default pgtk-wait-for-event-timeout 0
              truncate-lines t
              fill-column 100
              ;; imagemagick-render-type 1
              ;; mode-line-format nil
              )

;;; Environment
(setenv "LSP_USE_PLISTS" "true")    ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization

;;; early-init.el ends here.
