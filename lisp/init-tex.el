;;; init-tex.el --- AUCTeX formatting and previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Autoloaded by the AUCTeX mode entry points before their first invocation.

;;; Code:
(require 'init-func)
(require 'project)

(require 'tex)
(require 'latex)
(require 'plain-tex)
(require 'tex-info)
(require 'cdlatex)
(require 'tex-fold)
(require 'font-latex)
(require 'preview)
(require 'color)

(defconst my-latexmk-config-files '(".latexmkrc" "latexmkrc" ".latexmk")
  "Project-local Latexmk configuration filenames.")

(defconst my-latexmk-auto-read-config-files '(".latexmkrc" "latexmkrc")
  "Latexmk configuration filenames read automatically from the command directory.")

(defun my-latexmk--command-directory ()
  "Return the directory where AUCTeX will run Latexmk."
  (file-name-as-directory
   (expand-file-name
    (or (when (fboundp 'TeX-master-directory)
          (ignore-errors (TeX-master-directory)))
        default-directory))))

(defun my-latexmk--project-root (&optional directory)
  "Return the project root for DIRECTORY, or nil outside a project."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name (or directory default-directory)))))
    (when-let* ((project (and (fboundp 'project-current)
                              (project-current nil))))
      (cond
       ((fboundp 'project-root) (project-root project))
       ((fboundp 'project-roots) (car (project-roots project)))))))

(defun my-latexmk--config-in-directory (directory)
  "Return a Latexmk config file in DIRECTORY, or nil."
  (catch 'config
    (dolist (name my-latexmk-config-files)
      (let ((file (expand-file-name name directory)))
        (when (file-regular-p file)
          (throw 'config file))))
    nil))

(defun my-latexmk--locate-config ()
  "Return the nearest project-local Latexmk config for the current TeX buffer."
  (let* ((start-dir (my-latexmk--command-directory))
         (project-root (when-let* ((root (my-latexmk--project-root start-dir)))
                         (file-name-as-directory (expand-file-name root))))
         (home-dir (file-name-as-directory (expand-file-name "~")))
         (stop-dir (or project-root home-dir))
         (dir start-dir))
    (catch 'config
      (while dir
        (when-let* ((config (my-latexmk--config-in-directory dir)))
          (throw 'config config))
        (let ((parent (file-name-directory (directory-file-name dir))))
          (if (or (and stop-dir (file-equal-p dir stop-dir))
                  (null parent)
                  (file-equal-p parent dir))
              (throw 'config nil)
            (setq dir (file-name-as-directory parent))))))))

(defun my-latexmk-options ()
  "Use a project Latexmk config when present, otherwise use AUCTeX defaults."
  (if-let* ((config (my-latexmk--locate-config)))
      (let ((config-dir (file-name-as-directory (file-name-directory config))))
        (if (and (member (file-name-nondirectory config)
                         my-latexmk-auto-read-config-files)
                 (file-equal-p config-dir (my-latexmk--command-directory)))
            ""
          (concat "-r " (shell-quote-argument config))))
    (replace-regexp-in-string "\\`[ \t\n]+" ""
                              (TeX-command-expand "%(latexmk-out)"))))

(with-eval-after-load 'tex
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-fold-auto t
        cdlatex-paired-parens "$[{("
        TeX-save-query nil
        tex-suscript-height-ratio 0.8)
  (setq preview-scale-function 1.3)
  (custom-set-variables
   '(preview-LaTeX-command
     '("%`%l --output-format=dvi \"\\nonstopmode\\nofiles\\PassOptionsToPackage{"
       ("," . preview-required-option-list)
       "}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined"
       preview-default-preamble
       "\\fi}\"%' \"\\detokenize{\" %(t-filename-only) \"}\"")))
  (setq preview-image-type 'dvi*
        preview-dvi*-command #'preview-dvisvgm-command
        preview-dvi*-image-type 'svg)
  (defun my-preview-force-default-face (ov &rest _)
    "Make SVG `currentColor' resolve from `default' face, not font-lock faces."
    (when (eq (overlay-get ov 'preview-state) 'active)
      (overlay-put ov 'face 'default)))
  (advice-add 'preview-toggle :after #'my-preview-force-default-face)
  (if (boundp 'preview-dvi*-command)
      (setq preview-dvi*-image-type 'svg
            preview-dvi*-command
            (lambda ()
              (preview-dvisvgm-command
               "dvisvgm --no-fonts --currentcolor %d --page=- --output=\"%m/prev%3p.svg\"")))
    (setq preview-dvipng-image-type 'svg
          preview-dvipng-command
          (lambda ()
            (preview-dvisvgm-command
             "dvisvgm --no-fonts --currentcolor %d --page=- --output=\"%m/prev%3p.svg\""))))
  (setq-default TeX-master t
                TeX-engine 'luatex
                TeX-command-extra-options "-synctex=1 -shell-escape")
  (if windows-system-p
      (add-to-list 'TeX-view-program-selection '(output-pdf "Sioyek"))
    (add-to-list 'TeX-view-program-selection '(output-pdf "Okular")))
  (with-eval-after-load 'eaf
    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf")))
  (add-to-list
   'TeX-command-list
   '("XeLaTeX" "%`xelatex -shell-escape -synctex=1%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-expand-list '("%(my-latexmk-options)" my-latexmk-options))
  (setcdr
   (assoc "LaTeXMk" TeX-command-list)
   '("latexmk -pvc -view=none %(my-latexmk-options) %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t"
     TeX-run-TeX
     nil
     (LaTeX-mode docTeX-mode)
     :help "Run LaTeXMk"))
  (add-to-list 'texmathp-tex-commands1 '("lstlisting" env-off)))

(with-eval-after-load 'tex-fold
  (setq TeX-fold-macro-spec-list
        (mapcar
         (lambda (x)
           (if (equal (cadr x) '("item"))
               '(("[1]||•" . (1 . 0)) ("item"))
             x))
         TeX-fold-macro-spec-list))
  (defun my-tex-fold--prefix-like-title (prefix title)
    "Return PREFIX+TITLE, where PREFIX uses the same face as TITLE."
    (let* ((face (or (get-text-property 0 'face title)
                     (get-text-property 0 'font-lock-face title)))
           (pfx
            (if face
                (propertize prefix 'face face)
              prefix)))
      (concat pfx title)))
  (defun my-tex-fold-section (title)
    (my-tex-fold--prefix-like-title "§ " title))
  (defun my-tex-fold-subsection (title)
    (my-tex-fold--prefix-like-title "§§ " title))
  (defun my-tex-fold-subsubsection (title)
    (my-tex-fold--prefix-like-title "§§§ " title))
  (defun my-tex-fold-paragraph (title)
    (my-tex-fold--prefix-like-title "¶ " title))
  ;; 放到表头，覆盖默认规则
  (dolist (spec
           '((my-tex-fold-section ("section" "section*"))
             (my-tex-fold-subsection ("subsection" "subsection*"))
             (my-tex-fold-subsubsection ("subsubsection" "subsubsection*"))
             (my-tex-fold-paragraph ("paragraph" "paragraph*"))
             ("~" ("textasciitilde"))
             ("^" ("textasciicircum"))
             ("\\" ("textbackslash"))
             ("|" ("textbar"))
             (" " (";"))
             (" " ("quad"))
             ("  " ("qquad"))))
    (push spec LaTeX-fold-macro-spec-list))
  (dolist (x
           '((("∧" . 0)
              ("land"))
             (("∨" . 0)
              ("lor"))
             (1 ("textnormal" "mathrm"))
             ))
    (add-to-list 'LaTeX-fold-math-spec-list x))
  (dolist (x
           '((("╭•────▶" "╰•────▶")
              ("itemize"))
             (("╭≣────▶" "╰≣────▶")
              ("enumerate"))
             (("╭⌁────▶" "╰⌁────▶")
              ("description"))
             (("↱" "↳")
              ("equation" "equation*"))
             (("≡" "≡")
              ("align" "align*" "aligned"))
             (("Σ" "Σ")
              ("gather" "gather*"))
             (("⋯" "⋯")
              ("multline" "multline*"))))
    (add-to-list 'TeX-fold-begin-end-spec-list x))
  (defvar my/TeX-fold-clearpage-string
    "────────────────────────────────  clearpage  ────────────────────────────────")
  (defun my/TeX-fold--macro-face ()
    "取当前宏（\\...）开头处的 face，用来给占位符上色。"
    (let* ((beg
            (ignore-errors
              (TeX-find-macro-start)))  ; AUCTeX 的宏起点定位
           (pos (or beg (point))))
      (or (get-text-property pos 'face)
          (get-text-property pos 'font-lock-face)
          'font-lock-keyword-face)))
  (defun my/TeX-fold-clearpage (&rest _args)
    (propertize my/TeX-fold-clearpage-string 'face (my/TeX-fold--macro-face)))
  (add-to-list 'LaTeX-fold-macro-spec-list '(my/TeX-fold-clearpage ("clearpage")))
  (defvar my-tex-fold-empty-braces-scope 'math
    "Scope for folding empty braces after control words.
Possible values: 'math (default) or 'all.")
  (defun my-tex-fold--in-math-p (pos)
    "Non-nil if POS is in TeX math mode."
    (save-excursion
      (goto-char pos)
      (texmathp)))
  (defun my-tex-fold--already-folded-p (beg end)
    "Check whether region already has our TeX-fold terminator overlay."
    (cl-some
     (lambda (ov)
       (and (eq (overlay-get ov 'category) 'TeX-fold)
            (overlay-get ov 'my-tex-fold-terminator)))
     (overlays-in beg end)))
  (defun my-tex-fold--zero-overlay (beg end)
    "Hide region [BEG, END) with a TeX-fold overlay displaying zero width."
    (unless (or (>= beg end)
                (my-tex-fold--already-folded-p beg end))
      (let* ((priority (TeX-overlay-prioritize beg end))
             (ov (make-overlay beg end (current-buffer) t nil)))
        (overlay-put ov 'category 'TeX-fold)
        (overlay-put ov 'priority priority)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'my-tex-fold-terminator t)
        ;; Zero-width placeholder (looks like “gone”)
        (overlay-put ov 'TeX-fold-display-string-spec "\u200B")
        (TeX-fold-hide-item ov)
        ov)))
  (defun my-tex-fold-fold-terminators (&optional beg end)
    "Fold empty {} and terminator whitespace *in math* after TeX-fold runs."
    (interactive)
    (when (bound-and-true-p TeX-fold-mode)
      (save-excursion
        (let ((beg (or beg (point-min)))
              (end (or end (point-max))))
          ;; 1) Fold empty {} right after control words: \foo{}
          ;;    Default: only in math (safer). Set `my-tex-fold-empty-braces-scope` to 'all if you want everywhere.
          (goto-char beg)
          (while (re-search-forward "\\\\[A-Za-z@]+\\({}\\)" end t)
            (let ((b (match-beginning 1))
                  (e (match-end 1)))
              (when (or (eq my-tex-fold-empty-braces-scope 'all)
                        (my-tex-fold--in-math-p b))
                (my-tex-fold--zero-overlay b e))))
          ;; 2a) Same-line terminator whitespace (NO newline):
          ;;     \cmd<space/tab><letter>  (letter would otherwise extend control word)
          (goto-char beg)
          (while (re-search-forward "\\\\[A-Za-z@]+\\([ \t]+\\)\\([A-Za-z@]\\)" end t)
            (let ((b (match-beginning 1))
                  (e (match-end 1)))
              (when (my-tex-fold--in-math-p b)
                (my-tex-fold--zero-overlay b e))))
          ;; 2b) Multi-line: keep the newline, fold indentation only:
          ;;     \cmd\n[ \t]+<letter>  -> hide only the spaces after \n
          (goto-char beg)
          (while (re-search-forward "\\\\[A-Za-z@]+\\(\n[ \t]+\\)\\([A-Za-z@]\\)" end t)
            (let* ((grp-b (match-beginning 1))
                   (grp-e (match-end 1))
                   (indent-b (1+ grp-b)) ;; skip the newline itself
                   (indent-e grp-e))
              (when (and (< indent-b indent-e)
                         (my-tex-fold--in-math-p grp-b))
                (my-tex-fold--zero-overlay indent-b indent-e))))))))
  (defun my-tex-fold--after-fold (&rest _)
    (my-tex-fold-fold-terminators (point-min)
                                  (point-max)))
  ;; Make it work for both buffer-fold and region-fold
  (advice-add 'TeX-fold-buffer :after #'my-tex-fold--after-fold)
  (advice-add 'TeX-fold-region :after #'my-tex-fold--after-fold))

(with-eval-after-load 'font-latex
  (setq font-latex-fontify-sectioning 1.2)
  (font-latex-update-sectioning-faces))

(with-eval-after-load 'tex-mode
  (dolist (pair
           '(("\\land" . ?∧)
             ("\\lor" . ?∨)
             ))
    (add-to-list 'tex--prettify-symbols-alist pair)))

(dolist (hook '(LaTeX-mode-hook))
  (add-hook
   hook
   (lambda ()
     (setq-local TeX-command-default "LaTeXMk" ;; "XeLaTeX"
                 )
     (setq TeX-electric-math t)
     (font-latex-add-keywords '(("CJKunderline" "{")) 'underline-command)
     (electric-indent-local-mode -1)
     (TeX-PDF-mode-on)
     (LaTeX-math-mode 1)
     (TeX-fold-mode 1)
     (turn-on-cdlatex)
     (turn-on-reftex)
     ;; (visual-line-mode)
     ;; (outline-minor-mode)
     ;; (olivetti-mode)
     ;; (mixed-pitch-mode)
     ;; (face-remap-add-relative 'font-latex-superscript-face '(:height 0.8))
     ;; (face-remap-add-relative 'font-latex-subscript-face '(:height 0.8))
     (run-with-idle-timer 0 nil
                          (lambda (buf)
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (font-lock-ensure) ; 先确保字体化
                                )))
                          (current-buffer)))))

(keymap-set outline-minor-mode-map "C-<tab>" 'outline-toggle-children)



(provide 'init-tex)
;;; init-tex.el ends here
