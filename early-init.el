;;; early-init --- Early Init ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Index:
;; 01 Security
;; 02 Frame/UI Defaults
;; 03 Startup / Interaction
;; 04 Scrolling Behavior
;; 05 Performance / IO
;; 06 Environment

;;; ============================================================================
;;; 01 Security
;;; ============================================================================
;; Allow potentially-dangerous elisp features for *my* config files only.
(let ((dir (abbreviate-file-name (file-name-as-directory user-emacs-directory))))
  (cond
   ((boundp 'trusted-content) (add-to-list 'trusted-content dir))
   ((boundp 'trusted-files)   (add-to-list 'trusted-files dir))))

;;; ============================================================================
;;; 02 Frame/UI Defaults
;;; ============================================================================
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(unless (eq system-type 'android)
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0)))
(scroll-bar-mode -1)

;; Frame size / appearance examples
;; (when (eq system-type 'windows-nt)
;;   (add-to-list 'default-frame-alist '(width . 180))
;;   (add-to-list 'default-frame-alist '(height . 50)))
;; (add-to-list 'default-frame-alist '(alpha-background . 94))
;; (push '(undecorated . t) default-frame-alist)
;; (push '(vertical-scroll-bar) default-frame-alist)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; ============================================================================
;;; 03 Startup / Interaction
;;; ============================================================================
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
      auto-mode-case-fold nil)

;;; ============================================================================
;;; 04 Scrolling Behavior
;;; ============================================================================
(setq scroll-step 1
      hscroll-step 1
      hscroll-margin 1
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;;; ============================================================================
;;; 05 Performance / IO
;;; ============================================================================
(setq inhibit-compacting-font-caches t
      jit-lock-defer-time 0
      process-adaptive-read-buffering nil
      ;; frame-inhibit-implied-resize t
      )

(setq-default truncate-lines t
              fill-column 100
              ;; imagemagick-render-type 1
              ;; mode-line-format nil
              )

;;; ============================================================================
;;; 06 Environment
;;; ============================================================================
;; Use UTF-8 with LF line endings for new files, including on Windows.
(setq-default buffer-file-coding-system 'utf-8-unix)

(setenv "LSP_USE_PLISTS" "true")    ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization

;;; early-init.el ends here.
