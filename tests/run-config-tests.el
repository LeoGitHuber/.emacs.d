;;; run-config-tests.el --- Isolated configuration checks -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(setq user-emacs-directory
      (file-name-directory
       (directory-file-name (file-name-directory load-file-name)))
      default-directory user-emacs-directory)

(defvar config-test-sandbox (make-temp-file "emacs-config-test-" t))
(defvar config-test-original-handlers file-name-handler-alist)
(defvar config-test-original-gc gc-cons-threshold)
(defvar config-test-original-gc-percentage gc-cons-percentage)
(defvar config-test-startup-features nil)

;; Keep persistence and database work away from the real session's state.
(setq no-littering-var-directory (expand-file-name "var/" config-test-sandbox)
      no-littering-etc-directory (expand-file-name "etc/" config-test-sandbox)
      url-configuration-directory (expand-file-name "url/" config-test-sandbox)
      org-roam-db-location (expand-file-name "org-roam.db" config-test-sandbox)
      org-id-locations-file (expand-file-name "org-id-locations" config-test-sandbox))
(with-eval-after-load 'org-roam-db
  (advice-add 'org-roam-db-autosync-mode :override #'ignore))

(let ((start (current-time)))
  (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
  (load (expand-file-name "init.el" user-emacs-directory) nil t)
  (setq config-test-startup-features (copy-sequence features))
  (run-hooks 'after-init-hook 'emacs-startup-hook)
  (princ (format "STARTUP Emacs %s: %.3fs, %d features, MPS=%S, GC=%S/%S\n"
                 emacs-version (float-time (time-since start))
                 (length config-test-startup-features) (featurep 'mps)
                 gc-cons-threshold gc-cons-percentage)))

;; These tests exercise initialization and mode hooks, not a live GUI session.
(setq kill-emacs-hook nil)
(load (expand-file-name "tests/config-tests.el" user-emacs-directory) nil t)
(ert-run-tests-batch-and-exit)
