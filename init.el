;;; package --- Summary
;;; Commentary:

;; (setq package-enable-at-startup nil)

;; (async-bytecomp-package-mode 1)

;;; Dump For Fast
(defvar +dumped-load-path nil
  "Not nil when using dump.")

(defun add-subdirs-to-load-path(dir)
  "Recursive add directories to `load-path`."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(when +dumped-load-path
  ;;恢复 load-path
  (setq load-path +dumped-load-path)
  ;; 修改一下报错等级，这个读者按心意加，不影响 dump
  (setq warning-minimum-level :emergency)
  ;; 一些功能失常的 mode，需要重新开启

  (global-font-lock-mode t)
  (transient-mark-mode t)

  ;; 设置 load-path
  (let ((gc-cons-threshold most-positive-fixnum)
        (file-name-handler-alist nil)))

  ;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
  (load "~/.emacs.d/config_for_dump.el")
  )

(unless +dumped-load-path
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)
  ;; (add-subdirs-to-load-path "~/.emacs.d/elpa/")
  ;; (org-babel-load-file "~/.emacs.d/configuration.org")
  (load "~/.emacs.d/config.el")
  )
