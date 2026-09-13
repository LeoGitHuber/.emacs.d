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
  (dolist (file (append '("init.el" "early-init.el"
                         "site-lisp/segment-line/segment-line.el")
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

(defun config-test-flymake-backend (report-fn &rest _)
  "Report two diagnostics on one line without launching a subprocess."
  ;; Use a named backend, as real backends do.  Capturing these diagnostics in
  ;; an anonymous backend creates a cycle when Flymake stores the backend in
  ;; each diagnostic; NeoEmacs cannot handle that test fixture.
  (funcall report-fn
           (list (flymake-make-diagnostic
                  (current-buffer) 1 4 :error
                  '("checker" "E42" "First error\nDetailed explanation"))
                 (flymake-make-diagnostic
                  (current-buffer) 5 10 :warning "Second warning"))))

;; This backend only returns fixed test data; it never evaluates buffer text.
(put 'config-test-flymake-backend 'flymake-always-safe t)

(defun config-test-flymake-margin-overlays ()
  "Return diagnostic overlays that currently display a margin icon."
  (cl-remove-if-not
   (lambda (ov)
     (let ((icon (overlay-get ov 'before-string)))
       (and (overlay-get ov 'flymake-diagnostic)
            (stringp icon) (> (length icon) 0)
            (equal (car-safe (get-text-property 0 'display icon))
                   '(margin left-margin)))))
   (overlays-in (point-min) (point-max))))

(ert-deftest config-flymake-renders-native-eol-diagnostics ()
  ;; Exercise reporting and real overlays: an installed renderer can still be
  ;; bypassed if the configured display style does not select it.
  (let ((flymake-start-on-flymake-mode nil)
        (flymake-no-changes-timeout nil))
    (with-temp-buffer
      (insert "bad value\n")
      (setq-local flymake-diagnostic-functions '(config-test-flymake-backend))
      (unwind-protect
          (progn
            (flymake-mode 1)
            (flymake-start)
            (should (= (length (flymake-diagnostics)) 2))
            (let ((icons (config-test-flymake-margin-overlays)))
              (should (= (length icons) 1))
              (should (eq (flymake-diagnostic-type
                           (overlay-get (car icons) 'flymake-diagnostic))
                          :error))
              (let* ((carrier (overlay-get (car icons) 'before-string))
                     (glyph (cadr (get-text-property 0 'display carrier))))
                ;; The padding must resolve to a text font, not the icon font.
                (should (equal (face-attribute-merged-with
                                :family 'unspecified
                                (get-text-property 1 'face glyph))
                               (face-attribute 'default :family nil t)))
                (should (keymapp (get-text-property 0 'keymap glyph)))))
            (let* ((eol-overlays
                    (cl-remove-if-not
                     (lambda (ov) (overlay-get ov 'flymake--eol-overlay))
                     (overlays-in (point-min) (point-max))))
                   (display (overlay-get (car eol-overlays) 'display)))
              (should (= (length eol-overlays) 1))
              (should (stringp display))
              (should (string-match-p "First error" display))
              (should (string-match-p "Second warning" display))
              (should-not (string-match-p "Detailed explanation" display))
              (should (= (cl-count ?\n display) 1)))
            ;; Full messages remain available even when inline text is short.
            (should (cl-some
                     (lambda (diag)
                       (string-match-p "Detailed explanation"
                                       (flymake-diagnostic-text diag)))
                     (flymake-diagnostics)))
            (should (equal (buffer-string) "bad value\n"))
            (should-not (advice-member-p 'my/flymake--eol-draw-fancy
                                         'flymake--eol-draw-fancy)))
        (flymake-mode -1)))))

(defvar-local config-test-flymake-error-report nil)
(defvar-local config-test-flymake-warning-report nil)

(defun config-test-flymake-error-backend (report-fn &rest _)
  "Report an error independently of the warning backend."
  (setq config-test-flymake-error-report report-fn)
  (funcall report-fn
           (list (flymake-make-diagnostic (current-buffer) 1 4 :error "Error"))))

(defun config-test-flymake-warning-backend (report-fn &rest _)
  "Report a warning and a note on separate lines."
  (setq config-test-flymake-warning-report report-fn)
  (funcall report-fn
           (list (flymake-make-diagnostic (current-buffer) 5 10 :warning "Warning")
                 (flymake-make-diagnostic (current-buffer) 11 15 :note "Note"))))

(put 'config-test-flymake-error-backend 'flymake-always-safe t)
(put 'config-test-flymake-warning-backend 'flymake-always-safe t)

(ert-deftest config-flymake-promotes-remaining-margin-icon ()
  (let ((flymake-start-on-flymake-mode nil)
        (flymake-no-changes-timeout nil))
    (with-temp-buffer
      (insert "bad value\nnext line\n")
      (setq-local flymake-diagnostic-functions
                  '(config-test-flymake-error-backend
                    config-test-flymake-warning-backend))
      (unwind-protect
          (progn
            (flymake-mode 1)
            (flymake-start)
            (should (= (length (flymake-diagnostics)) 3))
            (should (= (length (config-test-flymake-margin-overlays)) 2))
            ;; Only the error backend reports again; the warning overlay is
            ;; reused, so its previously hidden icon must be restored.
            (funcall config-test-flymake-error-report nil
                     :region (cons (point-min) (point-max)))
            (should (= (length (flymake-diagnostics)) 2))
            (let ((icons (config-test-flymake-margin-overlays)))
              (should (= (length icons) 2))
              (should (cl-some
                       (lambda (ov)
                         (eq (flymake-diagnostic-type
                              (overlay-get ov 'flymake-diagnostic)) :warning))
                       icons)))
            (funcall config-test-flymake-warning-report nil
                     :region (cons (point-min) (point-max)))
            (should-not (config-test-flymake-margin-overlays)))
        (flymake-mode -1))
      (should-not (memq #'flymake-after-change-function after-change-functions)))))

(ert-deftest config-flymake-regroups-icons-after-line-edits ()
  (let ((flymake-start-on-flymake-mode nil)
        (flymake-no-changes-timeout nil))
    (with-temp-buffer
      (insert "bad value\n")
      (setq-local flymake-diagnostic-functions '(config-test-flymake-backend))
      (unwind-protect
          (progn
            (flymake-mode 1)
            (flymake-start)
            (should (= (length (config-test-flymake-margin-overlays)) 1))
            (goto-char 4)
            (insert "\n")
            (should (= (length (config-test-flymake-margin-overlays)) 2))
            (delete-region 4 5)
            (should (= (length (config-test-flymake-margin-overlays)) 1))
            (should (= (length (flymake-diagnostics)) 2)))
        (flymake-mode -1)))))

(ert-deftest config-flymake-keeps-margin-icons-and-theme-faces ()
  (my/flymake-refresh-theme)
  (should (= (face-attribute
              (my/flymake-inline-face 'flymake-end-of-line-diagnostics-face)
              :height) 0.9))
  (dolist (spec '((flymake-error error flymake-error-echo-at-eol)
                  (flymake-warning warning flymake-warning-echo-at-eol)
                  (flymake-note note flymake-note-echo-at-eol)))
    (should (equal (get (car spec) 'flymake-margin-string)
                   (alist-get (cadr spec) flymake-margin-indicators-string)))
    ;; Keep two columns for Nerd Font glyphs without an unsupported U+200A.
    (let ((icon (car (get (car spec) 'flymake-margin-string))))
      (should (= (length icon) 2))
      (should (= (aref icon 1) ?\s))
      (should (= (string-width icon) 2)))
    (let ((face (my/flymake-inline-face (caddr spec))))
      (should (facep face))
      (should (stringp (face-attribute face :foreground)))
      (should (eq (face-attribute face :weight) 'medium)))))

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

(ert-deftest config-diff-hl-fringe-uses-solid-background-bars ()
  (with-temp-buffer
    (insert "added\nchanged\ndeleted\n")
    (let ((diff-hl-fallback-to-margin nil)
          (diff-hl-side 'left))
      (dolist (highlight (list (default-value 'diff-hl-highlight-function)
                              (default-value 'diff-hl-highlight-reference-function)))
        (dolist (type '(insert change delete))
          (let ((overlay (make-overlay (point-min) (point-min))))
            (funcall highlight overlay type 'empty)
            (let ((display (get-text-property
                            0 'display (overlay-get overlay 'before-string))))
              (should (equal display
                             `((left-fringe diff-hl-bmp-empty
                                            ,(intern (format "diff-hl-%s" type)))))))
            (delete-overlay overlay)))))))

(ert-deftest config-theme-presets-do-not-leak-colors ()
  (my/ensure-modus-themes)
  (let ((original-themes (copy-sequence custom-enabled-themes))
        (user-settings (copy-tree (get 'user 'theme-settings)))
        (color-icons nerd-icons-color-icons)
        (palette-overrides (copy-tree modus-themes-common-palette-overrides)))
    (unwind-protect
        (dolist (theme '(doric-walnut doric-marble modus-vivendi modus-operandi))
          (my/apply-theme-preset theme)
          (let* ((faces '(default mode-line mode-line-active mode-line-inactive
                         diff-hl-insert diff-hl-change diff-hl-delete))
                 (baseline (mapcar #'face-all-attributes faces)))
            (dolist (preset '(matrix gits))
              (my/apply-theme-preset preset)
              (should (equal (get 'user 'theme-settings) user-settings))
              (should (equal modus-themes-common-palette-overrides palette-overrides))
              (my/apply-theme-preset theme)
              (should (equal custom-enabled-themes (list theme)))
              (should (eq nerd-icons-color-icons color-icons))
              (should (equal (mapcar #'face-all-attributes faces) baseline)))
            (dolist (face '(segment-line-chip-muted segment-line-chip-accent
                            segment-line-chip-subtle segment-line-chip-warning
                            segment-line-chip-error segment-line-state-normal
                            segment-line-state-insert))
              (should (eq (face-attribute face :inherit) 'mode-line-active))
              (let* ((foreground (segment-line--face-color face :foreground))
                     (background (segment-line--face-color 'mode-line-active :background))
                     (alpha (if (eq (face-attribute face :weight) 'bold)
                                segment-line-state-tint-alpha
                              segment-line-chip-tint-alpha)))
                (should (equal (face-attribute face :background)
                               (segment-line--blend foreground background alpha)))))
            (should (equal (face-attribute 'segment-line-inactive :background nil t)
                           (face-attribute 'mode-line-inactive :background nil t)))
            (should (equal (face-attribute 'segment-line-chip-error :foreground nil t)
                           (segment-line--face-color
                            'error :foreground
                            (segment-line--face-color 'mode-line-active :foreground))))))
      (my/disable-active-themes)
      (mapc #'enable-theme (reverse original-themes))
      (my/run-theme-dependent-refresh))))

(ert-deftest config-segment-line-icons-keep-font-without-fixed-colors ()
  (require 'nerd-icons)
  (with-temp-buffer
    (setq buffer-file-name "example.el")
    (cl-letf (((symbol-function 'segment-line--icons-available-p) (lambda () t)))
      (let* ((icon (segment-line--icon-for-buffer))
             (font-face (get-text-property 0 'face icon))
             (chip (segment-line--chip icon 'segment-line-inactive 0)))
        (should (stringp (plist-get font-face :family)))
        (should-not (plist-get font-face :inherit))
        (should-not (plist-get font-face :foreground))
        (should (member font-face (get-text-property 0 'face chip)))
        (should (memq 'segment-line-inactive (get-text-property 0 'face chip)))))))

;;; config-tests.el ends here
