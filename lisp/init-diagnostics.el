;;; init-diagnostics.el --- Flymake and diff indicators -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the existing diagnostic appearance in its own module.

;;; Code:
(require 'init-func)

;;; Diagnostics / icons
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'(lambda ()
                                    (flycheck-set-indication-mode 'left-margin))))

(require 'nerd-icons)
(require 'nerd-icons-corfu)
;; (require 'kind-icon)

(setq nerd-icons-font-family
      (or (my/detect-nerd-icons-font-family)
          "Symbols Nerd Font Mono"))

(defvar my/use-nerd-icons-p
  (my/nerd-icons-font-available-p)
  "Whether a usable Nerd Font is available for icon UI.")

(defface diagnostics-error
  `((((background dark))
     :foreground "#f85149"
     ,@(when my/use-nerd-icons-p
         `(:family ,nerd-icons-font-family)))
    (((background light))
     :foreground "#cb2431"
     ,@(when my/use-nerd-icons-p
         `(:family ,nerd-icons-font-family))))
  "Face for flymake Error."
  :group 'flymake)

(defface diagnostics-warn
  `((((background dark))
     :foreground "#f0883e"
     ,@(when my/use-nerd-icons-p
         `(:family ,nerd-icons-font-family)))
    (((background light))
     :foreground "#bf8803"
     ,@(when my/use-nerd-icons-p
         `(:family ,nerd-icons-font-family))))
  "Face for flymake Warn."
  :group 'flymake)

(defface diagnostics-info
  `((((background dark))
     :foreground "#75beff"
     ,@(when my/use-nerd-icons-p
         `(:family ,nerd-icons-font-family)))
    (((background light))
     :foreground "#1155ff"
     ,@(when my/use-nerd-icons-p
         `(:family ,nerd-icons-font-family))))
  "Face for flymake Info."
  :group 'flymake)

(require 'flymake)
(require 'color)

(setq flymake-no-changes-timeout 0.5
      flymake-indicator-type 'margins
      flymake-autoresize-margins t
      flymake-margin-indicators-string
      (if my/use-nerd-icons-p
          `((error ,(concat (my/nerd-icon-glyph "󰅙" "E") "\u200A") diagnostics-error)
            (warning ,(concat (my/nerd-icon-glyph "" "W") "\u200A") diagnostics-warn)
            (note ,(concat (my/nerd-icon-glyph "" "N") "\u200A") diagnostics-info))
        `((error ,(concat "E" "\u200A") diagnostics-error)
          (warning ,(concat "W" "\u200A") diagnostics-warn)
          (note ,(concat "N" "\u200A") diagnostics-info)))
      flymake-show-diagnostics-at-end-of-line t)

(defun my/flymake-apply-margin-indicators ()
  "Sync custom Flymake margin icons into diagnostic type properties."
  (put 'flymake-error 'flymake-margin-string
       (alist-get 'error flymake-margin-indicators-string))
  (put 'flymake-warning 'flymake-margin-string
       (alist-get 'warning flymake-margin-indicators-string))
  (put 'flymake-note 'flymake-margin-string
       (alist-get 'note flymake-margin-indicators-string)))

(my/flymake-apply-margin-indicators)

(dolist (hook '(prog-mode-hook))
  (add-hook hook 'flymake-mode))

(add-hook 'flymake-mode-hook #'my/flymake-apply-margin-indicators)

(require 'diff-hl)
(require 'diff-hl-dired)
(require 'diff-hl-flydiff)
(require 'diff-hl-margin)

;; Keep VCS change markers visible across GUI and terminal frames without
;; competing with the current Flymake left-margin indicators.
(setq diff-hl-side 'left)
(when (fboundp 'set-fringe-style)
  (set-fringe-style '(2 . nil)))
(setq diff-hl-bmp-max-width 2)
(setq diff-hl-highlight-function #'diff-hl-highlight-on-fringe
      diff-hl-highlight-reference-function #'diff-hl-highlight-on-fringe
      diff-hl-draw-borders nil)

(defun my/diff-hl-setup-display-backend ()
  "Use fringe highlights in GUI frames and right-margin highlights in terminals."
  (if (display-graphic-p)
      (progn
        (when (bound-and-true-p diff-hl-margin-local-mode)
          (diff-hl-margin-local-mode -1))
        (setq-local diff-hl-side 'left
                    diff-hl-highlight-function #'diff-hl-highlight-on-fringe
                    diff-hl-highlight-reference-function #'diff-hl-highlight-on-fringe)
        (when (fboundp 'set-fringe-style)
          (set-fringe-style '(2 . nil))))
    (when (and (bound-and-true-p diff-hl-margin-local-mode)
               (not (eq diff-hl-side 'right)))
      (diff-hl-margin-local-mode -1))
    (setq-local diff-hl-side 'right)
    (unless (bound-and-true-p diff-hl-margin-local-mode)
      (diff-hl-margin-local-mode 1))))

(add-hook 'diff-hl-mode-on-hook #'my/diff-hl-setup-display-backend)
(add-hook 'diff-hl-dired-mode-on-hook #'my/diff-hl-setup-display-backend)

(dolist (face '(diff-hl-insert
                diff-hl-delete
                diff-hl-change
                diff-hl-reference-insert
                diff-hl-reference-delete
                diff-hl-reference-change))
  (set-face-attribute face nil :background nil :box nil))

(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1)

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote))

(defvar my/flymake-fancy-max-width 88
  "Maximum width for custom Flymake fancy inline diagnostics.")

(defface my/flymake-fancy-prefix-face
  '((t :inherit shadow :height 1.0))
  "Face for prefix text in custom Flymake fancy diagnostics."
  :group 'flymake)

(defface my/flymake-fancy-error-face
  '((t :inherit default :height 0.9 :box nil))
  "Base face for Flymake fancy error chips."
  :group 'flymake)

(defface my/flymake-fancy-warning-face
  '((t :inherit default :height 0.9 :box nil))
  "Base face for Flymake fancy warning chips."
  :group 'flymake)

(defface my/flymake-fancy-note-face
  '((t :inherit default :height 0.9 :box nil))
  "Base face for Flymake fancy note chips."
  :group 'flymake)

(defface my/flymake-fancy-error-strong-face
  '((t :inherit my/flymake-fancy-error-face :weight semibold))
  "Strong face for Flymake fancy error icons."
  :group 'flymake)

(defface my/flymake-fancy-warning-strong-face
  '((t :inherit my/flymake-fancy-warning-face :weight semibold))
  "Strong face for Flymake fancy warning icons."
  :group 'flymake)

(defface my/flymake-fancy-note-strong-face
  '((t :inherit my/flymake-fancy-note-face :weight semibold))
  "Strong face for Flymake fancy note icons."
  :group 'flymake)

(defface my/flymake-fancy-error-guide-face
  '((t :inherit my/flymake-fancy-prefix-face))
  "Guide face for Flymake fancy error diagnostics."
  :group 'flymake)

(defface my/flymake-fancy-warning-guide-face
  '((t :inherit my/flymake-fancy-prefix-face))
  "Guide face for Flymake fancy warning diagnostics."
  :group 'flymake)

(defface my/flymake-fancy-note-guide-face
  '((t :inherit my/flymake-fancy-prefix-face))
  "Guide face for Flymake fancy note diagnostics."
  :group 'flymake)

(defvar my/flymake-fancy-guide "╰──▶ "
  "Guide prefix used by custom Flymake fancy diagnostics.")

(defvar my/flymake-fancy-icon-text-gap "  "
  "Spacing inserted between the severity icon and diagnostic text.")

(defvar my/flymake-fancy-text-right-padding " "
  "Padding appended after diagnostic text in custom Flymake fancy lines.")

(defvar my/flymake-fancy-background-ratio 0.18
  "Blend ratio of severity foreground color into the default background.")

(defvar my/flymake-fancy-error-icon (my/nerd-icon-glyph "" "E")
  "Icon used for Flymake error diagnostics.")

(defvar my/flymake-fancy-warning-icon (my/nerd-icon-glyph "" "W")
  "Icon used for Flymake warning diagnostics.")

(defvar my/flymake-fancy-note-icon (my/nerd-icon-glyph "" "N")
  "Icon used for Flymake note diagnostics.")

(defvar my/flymake-fancy-theme-hooks-installed nil
  "Non-nil when Flymake fancy theme refresh hooks are installed.")

(defun my/flymake--category (diag)
  "Return Flymake category symbol for DIAG."
  (or (flymake--lookup-type-property
       (flymake-diagnostic-type diag)
       'flymake-category)
      'flymake-note))

(defun my/flymake--face-color (face attribute &optional fallback)
  "Return FACE ATTRIBUTE as a concrete color string, or FALLBACK."
  (if (facep face)
      (let ((value (face-attribute face attribute nil t)))
        (if (or (not value)
                (eq value 'unspecified)
                (eq value 'unspecified-fg)
                (eq value 'unspecified-bg))
            fallback
          value))
    fallback))

(defun my/flymake--level (diag)
  "Return normalized severity level for DIAG."
  (pcase (my/flymake--category diag)
    ('flymake-error 'error)
    ('flymake-warning 'warning)
    (_ 'info)))

(defun my/flymake--theme-foreground (level)
  "Return theme foreground color for severity LEVEL."
  (or
   (pcase level
     ('error
      (or (my/flymake--face-color 'flymake-error :foreground)
          (my/flymake--face-color 'error :foreground)
          (my/flymake--face-color 'font-lock-warning-face :foreground)))
     ('warning
      (or (my/flymake--face-color 'flymake-warning :foreground)
          (my/flymake--face-color 'warning :foreground)
          (my/flymake--face-color 'font-lock-type-face :foreground)))
     (_
      (or (my/flymake--face-color 'flymake-note :foreground)
          (my/flymake--face-color 'success :foreground)
          (my/flymake--face-color 'link :foreground)
          (my/flymake--face-color 'font-lock-function-name-face :foreground))))
   (pcase level
     ('error "#ff6c6b")
     ('warning "#ECBE7B")
     (_ "#98be65"))))

(defun my/flymake--default-background ()
  "Return current default background color."
  (let ((color (my/flymake--face-color 'default :background)))
    (if (or (not color) (eq color 'unspecified))
        (if (eq (frame-parameter nil 'background-mode) 'light)
            "#ffffff"
          "#000000")
      color)))

(defun my/flymake--blend-colors (fg bg alpha)
  "Blend FG into BG using ALPHA and return a hex color."
  (let* ((fg-rgb (or (color-name-to-rgb fg) '(1.0 1.0 1.0)))
         (bg-rgb (or (color-name-to-rgb bg) '(0.0 0.0 0.0)))
         (mixed
          (cl-mapcar (lambda (f b)
                       (+ (* alpha f) (* (- 1 alpha) b)))
                     fg-rgb bg-rgb)))
    (color-rgb-to-hex (nth 0 mixed) (nth 1 mixed) (nth 2 mixed) 2)))

(defun my/flymake--severity (diag)
  "Return normalized severity symbol for DIAG."
  (pcase (my/flymake--level diag)
    ('error 'error)
    ('warning 'warning)
    (_ 'note)))

(defun my/flymake--chip-face (severity)
  "Return Flymake fancy chip face for SEVERITY."
  (pcase severity
    ('error 'my/flymake-fancy-error-face)
    ('warning 'my/flymake-fancy-warning-face)
    (_ 'my/flymake-fancy-note-face)))

(defun my/flymake--chip-strong-face (severity)
  "Return Flymake fancy strong chip face for SEVERITY."
  (pcase severity
    ('error 'my/flymake-fancy-error-strong-face)
    ('warning 'my/flymake-fancy-warning-strong-face)
    (_ 'my/flymake-fancy-note-strong-face)))

(defun my/flymake--guide-face-symbol (severity)
  "Return Flymake fancy guide face for SEVERITY."
  (pcase severity
    ('error 'my/flymake-fancy-error-guide-face)
    ('warning 'my/flymake-fancy-warning-guide-face)
    (_ 'my/flymake-fancy-note-guide-face)))

(defun my/flymake-refresh-fancy-theme (&optional _theme)
  "Refresh Flymake fancy faces from the current theme."
  (interactive)
  (let* ((default-bg (my/flymake--default-background))
         (shadow-fg (or (my/flymake--face-color 'shadow :foreground)
                        (my/flymake--face-color 'mode-line-inactive :foreground)
                        (my/flymake--face-color 'default :foreground)))
         (error-fg (my/flymake--theme-foreground 'error))
         (warning-fg (my/flymake--theme-foreground 'warning))
         (note-fg (my/flymake--theme-foreground 'info))
         (error-bg (my/flymake--blend-colors error-fg default-bg my/flymake-fancy-background-ratio))
         (warning-bg (my/flymake--blend-colors warning-fg default-bg my/flymake-fancy-background-ratio))
         (note-bg (my/flymake--blend-colors note-fg default-bg my/flymake-fancy-background-ratio)))
    (set-face-attribute 'my/flymake-fancy-prefix-face nil
                        :inherit 'shadow
                        :foreground shadow-fg
                        :height 1.0
                        :box nil
                        :underline nil
                        :overline nil
                        :strike-through nil)
    (dolist (spec `((my/flymake-fancy-error-face ,error-fg ,error-bg)
                    (my/flymake-fancy-warning-face ,warning-fg ,warning-bg)
                    (my/flymake-fancy-note-face ,note-fg ,note-bg)))
      (pcase-let ((`(,face ,fg ,bg) spec))
        (set-face-attribute face nil
                            :inherit 'default
                            :foreground fg
                            :background bg
                            :height 0.9
                            :box nil
                            :underline nil
                            :overline nil
                            :strike-through nil)))
    (dolist (spec `((my/flymake-fancy-error-strong-face my/flymake-fancy-error-face)
                    (my/flymake-fancy-warning-strong-face my/flymake-fancy-warning-face)
                    (my/flymake-fancy-note-strong-face my/flymake-fancy-note-face)))
      (pcase-let ((`(,face ,base-face) spec))
        (set-face-attribute face nil
                            :inherit base-face
                            :weight 'semibold
                            :box nil)))
    (dolist (spec `((my/flymake-fancy-error-guide-face ,error-fg)
                    (my/flymake-fancy-warning-guide-face ,warning-fg)
                    (my/flymake-fancy-note-guide-face ,note-fg)))
      (pcase-let ((`(,face ,fg) spec))
        (set-face-attribute face nil
                            :inherit 'my/flymake-fancy-prefix-face
                            :foreground fg
                            :box nil
                            :underline nil
                            :overline nil
                            :strike-through nil)))
    (set-face-attribute 'flymake-end-of-line-diagnostics-face nil
                        :inherit 'default
                        :height 0.9
                        :box nil
                        :foreground 'unspecified
                        :background 'unspecified)
    (set-face-attribute 'flymake-error-echo-at-eol nil
                        :inherit 'error
                        :foreground error-fg
                        :weight 'medium
                        :box nil)
    (set-face-attribute 'flymake-warning-echo-at-eol nil
                        :inherit 'warning
                        :foreground warning-fg
                        :weight 'medium
                        :box nil)
    (set-face-attribute 'flymake-note-echo-at-eol nil
                        :inherit 'success
                        :foreground note-fg
                        :weight 'medium
                        :box nil)
    (set-face-attribute 'flymake-eol-information-face nil
                        :inherit 'shadow
                        :foreground shadow-fg
                        :slant 'normal
                        :box nil))
  (force-mode-line-update t))

(defun my/flymake--theme-refresh-hook (&rest _)
  "Refresh Flymake fancy faces after theme changes."
  (unless (bound-and-true-p my/theme-refresh-suppressed)
    (my/flymake-refresh-fancy-theme)))

(defun my/flymake-install-theme-hooks ()
  "Install theme hooks used by Flymake fancy rendering."
  (unless my/flymake-fancy-theme-hooks-installed
    (add-hook 'enable-theme-functions #'my/flymake--theme-refresh-hook)
    (add-hook 'disable-theme-functions #'my/flymake--theme-refresh-hook)
    (setq my/flymake-fancy-theme-hooks-installed t)))

(defun my/flymake--guide-face (diag)
  "Return a guide face for DIAG using theme-derived severity colors."
  (my/flymake--guide-face-symbol (my/flymake--severity diag)))

(defun my/flymake--icon (diag)
  "Return a severity icon string for DIAG."
  (pcase (my/flymake--category diag)
    ('flymake-error my/flymake-fancy-error-icon)
    ('flymake-warning my/flymake-fancy-warning-icon)
    (_ my/flymake-fancy-note-icon)))

(defun my/flymake--indent (diag)
  "Return the actual diagnostic column for DIAG."
  (with-current-buffer (flymake-diagnostic-buffer diag)
    (save-excursion
      (goto-char (flymake-diagnostic-beg diag))
      (current-column))))

(defun my/flymake--guide-key (diag)
  "Return a key identifying DIAG's guide position as `(LINE . COLUMN)'."
  (with-current-buffer (flymake-diagnostic-buffer diag)
    (save-excursion
      (goto-char (flymake-diagnostic-beg diag))
      (cons (line-number-at-pos) (current-column)))))

(defun my/flymake--clean-summary (text)
  "Normalize Flymake diagnostic TEXT into a compact single line."
  (replace-regexp-in-string
   "[[:space:]\n\r\t]+"
   " "
   (string-trim (substring-no-properties text))))

(defun my/flymake--guide-prefix (diag show-guide)
  "Return guide prefix for DIAG.
When SHOW-GUIDE is nil, return an equally wide blank prefix."
  (let ((guide-face (my/flymake--guide-face diag)))
    (if show-guide
        (propertize my/flymake-fancy-guide 'face guide-face)
      (propertize (make-string (string-width my/flymake-fancy-guide) ?\s)
                  'face guide-face))))

(defun my/flymake--overlay-guide-column (line diag)
  "Overlay a vertical guide for DIAG onto rendered LINE."
  (let* ((column (my/flymake--indent diag))
         (guide-face (my/flymake--guide-face diag))
         (line (copy-sequence line)))
    (when (<= (length line) column)
      (setq line (concat line (make-string (1+ (- column (length line))) ?\s))))
    (concat (substring line 0 column)
            (propertize "│" 'face guide-face)
            (substring line (1+ column)))))

(defun my/flymake--render-inline-line (diag summarize-fn &optional show-guide)
  "Render DIAG using SUMMARIZE-FN as a lightweight fancy line.
When SHOW-GUIDE is non-nil, render the guide arrow prefix."
  (let* ((severity (my/flymake--severity diag))
         (chip-face (my/flymake--chip-face severity))
         (chip-strong-face (my/flymake--chip-strong-face severity))
         (indent (make-string (my/flymake--indent diag) ?\s))
         (icon (my/flymake--icon diag))
         (summary (my/flymake--clean-summary (funcall summarize-fn diag)))
         (body (truncate-string-to-width summary my/flymake-fancy-max-width nil nil "…")))
    (concat indent
            (my/flymake--guide-prefix diag show-guide)
            (propertize " " 'face chip-face)
            (my/append-face icon chip-strong-face)
            (propertize my/flymake-fancy-icon-text-gap 'face chip-face)
            (propertize body 'face chip-face)
            (propertize my/flymake-fancy-text-right-padding 'face chip-face))))

(defun my/flymake--eol-draw-fancy (diags summarize-fn)
  "Render DIAGS with SUMMARIZE-FN using a lightweight inline style."
  (let ((sorted (cl-sort (copy-sequence diags) #'> :key #'flymake-diagnostic-beg))
        (previous-key nil)
        (lines nil))
    (dolist (diag sorted)
      (let* ((guide-key (my/flymake--guide-key diag))
             (show-guide (not (equal guide-key previous-key))))
        (when show-guide
          (setq lines (mapcar (lambda (line)
                                (my/flymake--overlay-guide-column line diag))
                              lines)))
        (push (my/flymake--render-inline-line diag summarize-fn show-guide) lines)
        (setq previous-key guide-key)))
    (concat
     " \n"
     (string-join (nreverse lines) "\n"))))

(defvar my/flymake-stock-fancy-function
  (when (fboundp 'flymake--eol-draw-fancy)
    (symbol-function 'flymake--eol-draw-fancy))
  "Stock renderer captured before installing compatibility advice.")

(define-minor-mode my/flymake-fancy-mode
  "Use the custom diagnostic renderer while keeping Flymake's original."
  :global t
  :group 'flymake
  (if my/flymake-fancy-mode
      (if (and my/flymake-stock-fancy-function
               (equal (func-arity my/flymake-stock-fancy-function) '(2 . 2)))
          (advice-add 'flymake--eol-draw-fancy :override
                      #'my/flymake--eol-draw-fancy)
        (setq my/flymake-fancy-mode nil)
        (display-warning 'init-diagnostics
                         "Flymake renderer API changed; using its built-in renderer."))
    (advice-remove 'flymake--eol-draw-fancy #'my/flymake--eol-draw-fancy)))

(my/flymake-fancy-mode 1)

(my/flymake-install-theme-hooks)
(my/flymake-refresh-fancy-theme)

(provide 'init-diagnostics)
;;; init-diagnostics.el ends here
