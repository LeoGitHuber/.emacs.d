;;; config-tests.el --- Configuration regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(ert-deftest config-startup-keeps-gc-defaults-and-handlers ()
  (should (= gc-cons-threshold config-test-original-gc))
  (should (= gc-cons-percentage config-test-original-gc-percentage))
  (dolist (handler config-test-original-handlers)
    (should (member handler file-name-handler-alist))))

(ert-deftest config-heavy-features-load-on-demand ()
  (dolist (feature '(org-roam magit dape lexdb init-tex nov calibredb))
    (should-not (memq feature config-test-startup-features)))
  (dolist (command '(magit-status dape org-noter org-roam-node-find
                    nov-mode calibredb latex-node-find-file))
    (should (commandp command))))

(ert-deftest config-active-files-have-valid-syntax-and-lf ()
  (dolist (file (append '("init.el" "early-init.el")
                       (directory-files "lisp" t "\\.el\\'")))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (should-not (search-forward "\r" nil t))
      (erase-buffer)
      (insert-file-contents file)
      (emacs-lisp-mode)
      (check-parens))))

(ert-deftest config-new-files-use-utf8-lf ()
  (with-temp-buffer
    (should (eq buffer-file-coding-system 'utf-8-unix))
    (insert "first\nsecond\n")
    (let ((file (expand-file-name "new.txt" config-test-sandbox)))
      (write-region (point-min) (point-max) file nil 'silent)
      (erase-buffer)
      (insert-file-contents-literally file)
      (should (equal (buffer-string) "first\nsecond\n")))))

(ert-deftest config-markdown-preserves-hard-breaks ()
  (with-temp-buffer
    (markdown-mode)
    (insert "first  \nsecond\n")
    (run-hooks 'before-save-hook)
    (should (equal (buffer-string) "first  \nsecond\n"))
    (should-not (bound-and-true-p delete-trailing-whitespace-mode))))

(ert-deftest config-text-cleans-trailing-whitespace ()
  (with-temp-buffer
    (text-mode)
    (insert "first  \nsecond\n")
    (run-hooks 'before-save-hook)
    (should (equal (buffer-string) "first\nsecond\n"))))

(ert-deftest config-eglot-formatting-has-one-owner ()
  (with-temp-buffer
    (setq major-mode 'verilog-mode)
    (let ((apheleia-mode t) (calls 0))
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                ((symbol-function 'eglot-server-capable) (lambda (&rest _) t))
                ((symbol-function 'eglot-format-buffer)
                 (lambda () (cl-incf calls))))
        (eglot-format-buffer-maybe)
        (should (= calls 0))
        (setq apheleia-mode nil)
        ;; Formatting should also work with point on an empty line.
        (eglot-format-buffer-maybe)
        (should (= calls 1))))))

(ert-deftest config-deferred-packages-can-load ()
  (dolist (command '(magit-status dape org-noter org-roam-node-find
                    nov-mode calibredb latex-node-find-file))
    (autoload-do-load (symbol-function command) command)
    (should (commandp command))))

(ert-deftest config-eglot-removes-format-hook-on-disconnect ()
  (with-temp-buffer
    (setq major-mode 'verilog-mode)
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t)))
      (setup-eglot-format-on-save)
      (should (memq #'eglot-format-buffer-maybe before-save-hook)))
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil)))
      (setup-eglot-format-on-save)
      (should-not (memq #'eglot-format-buffer-maybe before-save-hook)))))

(ert-deftest config-scala-without-grammar-falls-back ()
  (let ((chosen nil))
    (cl-letf (((symbol-function 'treesit-ready-p) (lambda (&rest _) nil))
              ((symbol-function 'scala-mode) (lambda () (setq chosen 'scala)))
              ((symbol-function 'scala-ts-mode) (lambda () (setq chosen 'treesit))))
      (my/scala-mode)
      (should (eq chosen 'scala)))))

(ert-deftest config-scala-with-grammar-uses-treesit ()
  (let ((chosen nil))
    (cl-letf (((symbol-function 'treesit-ready-p) (lambda (&rest _) t))
              ((symbol-function 'scala-mode) (lambda () (setq chosen 'scala)))
              ((symbol-function 'scala-ts-mode) (lambda () (setq chosen 'treesit))))
      (my/scala-mode)
      (should (eq chosen 'treesit)))))

(ert-deftest config-flymake-renderer-is-reversible ()
  (unwind-protect
      (progn
        (my/flymake-fancy-mode -1)
        (should (fboundp 'flymake--eol-draw-fancy))
        (should-not (advice-member-p #'my/flymake--eol-draw-fancy
                                     'flymake--eol-draw-fancy))
        (my/flymake-fancy-mode 1)
        (should my/flymake-fancy-mode)
        (should (advice-member-p #'my/flymake--eol-draw-fancy
                                 'flymake--eol-draw-fancy)))
    (my/flymake-fancy-mode 1)))

(ert-deftest config-org-workflow-still-initializes ()
  (with-temp-buffer
    (org-mode)
    (should (bound-and-true-p org-cdlatex-mode))
    (should (bound-and-true-p mixed-pitch-mode))
    (should-not (my/hl-line-buffer-p (current-buffer)))
    (should (memq 'time-stamp before-save-hook))))

(ert-deftest config-latex-workflow-loads-on-first-use ()
  (with-temp-buffer
    (LaTeX-mode)
    (should (featurep 'init-tex))
    (should (bound-and-true-p cdlatex-mode))
    (should (bound-and-true-p TeX-fold-mode))
    (should (equal TeX-command-default "LaTeXMk"))
    (should (string-match-p "-synctex=1" (cadr (assoc "XeLaTeX" TeX-command-list))))))

(ert-deftest config-main-keybindings-are-preserved ()
  (dolist (binding '(("C-c l f" . my/format-buffer)
                     ("C-c n f" . org-roam-node-find)
                     ("C-c n n" . org-noter)
                     ("C-x b" . consult-buffer)
                     ("C-c d" . my/lexdb-search)))
    (should (eq (keymap-lookup global-map (car binding)) (cdr binding)))))

(ert-deftest config-legacy-paths-are-not-loaded ()
  (dolist (entry load-history)
    (when (stringp (car entry))
      (should-not (string-match-p "/archive/legacy/" (car entry))))))

;;; config-tests.el ends here
