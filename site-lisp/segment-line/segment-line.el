;;; segment-line.el --- Heirline-inspired composable mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Local
;; Keywords: mode-line, convenience, faces
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; `segment-line-mode' provides a composable mode-line with small, reusable
;; components.  The design is inspired by Heirline's "segments and conditions"
;; model, but implemented in plain Emacs Lisp for the native mode-line.
;;
;; A segment can be either:
;;
;; - a function symbol returning a string or nil
;; - an anonymous function returning a string or nil
;; - a property list with optional keys:
;;   :when    predicate/function/value deciding visibility
;;   :text    string/function/symbol used as content
;;   :face    face applied to the rendered content
;;   :prefix  string placed before content
;;   :suffix  string placed after content
;;   :pad     integer or cons cell (LEFT . RIGHT) padding in spaces
;;
;; Example:
;;
;;   (setq segment-line-left
;;         '(segment-line-segment-state
;;           segment-line-segment-buffer
;;           (:when segment-line--project-name
;;            :text segment-line--project-name
;;            :face segment-line-chip-subtle
;;            :pad (1 . 1))))

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'subr-x)
(require 'project)
(require 'vc)
(require 'vc-git nil t)

(defgroup segment-line nil
  "Heirline-inspired composable mode-line."
  :group 'mode-line
  :prefix "segment-line-")

(defcustom segment-line-left
  '(segment-line-segment-state
    segment-line-segment-input-method
    segment-line-segment-buffer
    segment-line-segment-project
    segment-line-segment-vc)
  "Left-hand side component list."
  :type '(repeat sexp)
  :group 'segment-line)

(defcustom segment-line-right
  '(segment-line-segment-flags
    segment-line-segment-region
    segment-line-segment-process
    segment-line-segment-lsp
    segment-line-segment-diagnostics
    segment-line-segment-coding
    segment-line-segment-major-mode
    segment-line-segment-position)
  "Right-hand side component list."
  :type '(repeat sexp)
  :group 'segment-line)

(defcustom segment-line-separator ""
  "String inserted between rendered components."
  :type 'string
  :group 'segment-line)

(defcustom segment-line-chip-padding '(1 . 1)
  "Default padding used for rendered chips."
  :type '(choice integer (cons integer integer))
  :group 'segment-line)

(defcustom segment-line-buffer-name-max-width 40
  "Maximum width for displayed buffer names."
  :type 'integer
  :group 'segment-line)

(defcustom segment-line-project-name-max-width 18
  "Maximum width for displayed project names."
  :type 'integer
  :group 'segment-line)

(defcustom segment-line-symbol-name-max-width 32
  "Maximum width for displayed current symbol names."
  :type 'integer
  :group 'segment-line)

(defcustom segment-line-position-min-line-width 3
  "Minimum width reserved for the line number in the position segment."
  :type 'integer
  :group 'segment-line)

(defcustom segment-line-position-column-width 3
  "Width reserved for the column number in the position segment."
  :type 'integer
  :group 'segment-line)

(defcustom segment-line-enable-icons t
  "Whether to use Nerd Font icons when available."
  :type 'boolean
  :group 'segment-line)

(defcustom segment-line-enable-which-function t
  "Whether `segment-line-mode' should show current symbol information."
  :type 'boolean
  :group 'segment-line)

(defface segment-line-chip-muted
  '((((background dark)) :inherit mode-line :background "#313244" :foreground "#cdd6f4" :box nil)
    (((background light)) :inherit mode-line :background "#dce0e8" :foreground "#4c4f69" :box nil))
  "Muted chip face."
  :group 'segment-line)

(defface segment-line-chip-accent
  '((((background dark)) :inherit mode-line :background "#45475a" :foreground "#f5e0dc" :weight semibold :box nil)
    (((background light)) :inherit mode-line :background "#bcc0cc" :foreground "#1e66f5" :weight semibold :box nil))
  "Accent chip face."
  :group 'segment-line)

(defface segment-line-chip-subtle
  '((((background dark)) :inherit mode-line :background "#1e1e2e" :foreground "#a6adc8" :box nil)
    (((background light)) :inherit mode-line :background "#eff1f5" :foreground "#6c6f85" :box nil))
  "Subtle chip face."
  :group 'segment-line)

(defface segment-line-chip-note
  '((((background dark)) :inherit mode-line :background "#24304a" :foreground "#89b4fa" :box nil)
    (((background light)) :inherit mode-line :background "#dce7f7" :foreground "#1d4ed8" :box nil))
  "Informational chip face."
  :group 'segment-line)

(defface segment-line-chip-success
  '((((background dark)) :inherit mode-line :background "#1d3b2f" :foreground "#a6e3a1" :box nil)
    (((background light)) :inherit mode-line :background "#d8f3dc" :foreground "#2d6a4f" :box nil))
  "Success chip face."
  :group 'segment-line)

(defface segment-line-chip-warning
  '((((background dark)) :inherit mode-line :background "#433015" :foreground "#f9e2af" :box nil)
    (((background light)) :inherit mode-line :background "#fef3c7" :foreground "#92400e" :box nil))
  "Warning chip face."
  :group 'segment-line)

(defface segment-line-chip-error
  '((((background dark)) :inherit mode-line :background "#492020" :foreground "#f38ba8" :box nil)
    (((background light)) :inherit mode-line :background "#fee2e2" :foreground "#b91c1c" :box nil))
  "Error chip face."
  :group 'segment-line)

(defface segment-line-state-normal
  '((((background dark)) :inherit mode-line :background "#1f3a5f" :foreground "#89b4fa" :weight bold :box nil)
    (((background light)) :inherit mode-line :background "#dbeafe" :foreground "#1d4ed8" :weight bold :box nil))
  "Normal-state chip face."
  :group 'segment-line)

(defface segment-line-state-insert
  '((((background dark)) :inherit mode-line :background "#18352a" :foreground "#a6e3a1" :weight bold :box nil)
    (((background light)) :inherit mode-line :background "#dcfce7" :foreground "#15803d" :weight bold :box nil))
  "Insert-state chip face."
  :group 'segment-line)

(defface segment-line-state-motion
  '((((background dark)) :inherit mode-line :background "#3f2e17" :foreground "#f9e2af" :weight bold :box nil)
    (((background light)) :inherit mode-line :background "#fef3c7" :foreground "#a16207" :weight bold :box nil))
  "Motion-state chip face."
  :group 'segment-line)

(defface segment-line-state-visual
  '((((background dark)) :inherit mode-line :background "#40294a" :foreground "#f5c2e7" :weight bold :box nil)
    (((background light)) :inherit mode-line :background "#f3e8ff" :foreground "#9333ea" :weight bold :box nil))
  "Visual-state chip face."
  :group 'segment-line)

(defface segment-line-state-replace
  '((((background dark)) :inherit mode-line :background "#4b2424" :foreground "#f38ba8" :weight bold :box nil)
    (((background light)) :inherit mode-line :background "#fee2e2" :foreground "#dc2626" :weight bold :box nil))
  "Replace-state chip face."
  :group 'segment-line)

(defface segment-line-state-command
  '((((background dark)) :inherit mode-line :background "#2d3148" :foreground "#cba6f7" :weight bold :box nil)
    (((background light)) :inherit mode-line :background "#ede9fe" :foreground "#7c3aed" :weight bold :box nil))
  "Command-state chip face."
  :group 'segment-line)

(defface segment-line-inactive
  '((t :inherit mode-line-inactive :box nil))
  "Fallback face for inactive windows."
  :group 'segment-line)

(defcustom segment-line-chip-tint-alpha 0.14
  "Blend ratio used for status chips derived from the active theme."
  :type 'number
  :group 'segment-line)

(defcustom segment-line-state-tint-alpha 0.22
  "Blend ratio used for modal state chips derived from the active theme."
  :type 'number
  :group 'segment-line)

(defvar segment-line--saved-mode-line-format nil
  "Saved default `mode-line-format' before enabling `segment-line-mode'.")

(defvar segment-line--last-error nil
  "Last rendering error captured by `segment-line--safe-format'.")

(defvar segment-line--enabled-which-function nil
  "Non-nil when `segment-line-mode' enabled `which-function-mode'.")

(defvar-local segment-line--vc-branch-cache nil
  "Cached Git branch name for the current buffer.")

(defvar segment-line--theme-hooks-installed nil
  "Non-nil when Segment Line theme refresh hooks are installed.")

(defun segment-line--face-color (face attribute &optional fallback)
  "Return FACE ATTRIBUTE as a concrete color string, falling back to FALLBACK."
  (let ((value (face-attribute face attribute nil t)))
    (if (or (not value) (eq value 'unspecified) (eq value 'unspecified-fg) (eq value 'unspecified-bg))
        fallback
      value)))

(defun segment-line--blend (fg bg alpha)
  "Blend FG into BG using ALPHA and return a hex color."
  (let* ((fg-rgb (or (color-name-to-rgb fg) '(1.0 1.0 1.0)))
         (bg-rgb (or (color-name-to-rgb bg) '(0.0 0.0 0.0)))
         (mixed (cl-mapcar (lambda (f b)
                             (+ (* alpha f) (* (- 1 alpha) b)))
                           fg-rgb bg-rgb)))
    (color-rgb-to-hex (nth 0 mixed) (nth 1 mixed) (nth 2 mixed) 2)))

(defun segment-line--theme-palette ()
  "Return a palette derived from the current theme faces."
  (let* ((default-bg (or (segment-line--face-color 'default :background)
                         (if (eq (frame-parameter nil 'background-mode) 'light)
                             "#ffffff"
                           "#000000")))
         (default-fg (or (segment-line--face-color 'default :foreground)
                         (if (eq (frame-parameter nil 'background-mode) 'light)
                             "#000000"
                           "#ffffff")))
         (mode-line-bg (or (segment-line--face-color 'mode-line :background)
                           default-bg))
         (mode-line-fg (or (segment-line--face-color 'mode-line :foreground)
                           default-fg))
         (mode-line-inactive-bg
          (or (segment-line--face-color 'mode-line-inactive :background)
              (segment-line--blend default-bg mode-line-bg 0.65)))
         (mode-line-inactive-fg
          (or (segment-line--face-color 'mode-line-inactive :foreground)
              (segment-line--face-color 'shadow :foreground)
              (segment-line--blend default-fg mode-line-bg 0.45)))
         (shadow-fg
          (or (segment-line--face-color 'shadow :foreground)
              mode-line-inactive-fg
              mode-line-fg))
         (accent-fg
          (or (segment-line--face-color 'mode-line-emphasis :foreground)
              (segment-line--face-color 'link :foreground)
              (segment-line--face-color 'font-lock-keyword-face :foreground)
              mode-line-fg))
         (note-fg
          (or (segment-line--face-color 'link :foreground)
              (segment-line--face-color 'font-lock-function-name-face :foreground)
              accent-fg))
         (success-fg
          (or (segment-line--face-color 'success :foreground)
              (segment-line--face-color 'font-lock-string-face :foreground)
              note-fg))
         (warning-fg
          (or (segment-line--face-color 'warning :foreground)
              (segment-line--face-color 'font-lock-type-face :foreground)
              success-fg))
         (error-fg
          (or (segment-line--face-color 'error :foreground)
              (segment-line--face-color 'font-lock-warning-face :foreground)
              warning-fg))
         (visual-fg
          (or (segment-line--face-color 'font-lock-keyword-face :foreground)
              (segment-line--face-color 'region :foreground)
              accent-fg))
         (command-fg
          (or (segment-line--face-color 'font-lock-builtin-face :foreground)
              visual-fg)))
    (list :mode-line-bg mode-line-bg
          :mode-line-fg mode-line-fg
          :inactive-bg mode-line-inactive-bg
          :inactive-fg mode-line-inactive-fg
          :shadow-fg shadow-fg
          :accent-fg accent-fg
          :note-fg note-fg
          :success-fg success-fg
          :warning-fg warning-fg
          :error-fg error-fg
          :visual-fg visual-fg
          :command-fg command-fg)))

(defun segment-line--apply-chip-face (face fg bg &optional weight)
  "Apply derived chip FACE using FG, BG, and optional WEIGHT."
  (set-face-attribute face nil
                      :inherit 'mode-line
                      :foreground fg
                      :background bg
                      :weight (or weight 'normal)
                      :box nil))

(defun segment-line-refresh-faces (&optional _theme)
  "Recompute Segment Line faces from the current theme."
  (interactive)
  (let* ((palette (segment-line--theme-palette))
         (mode-line-bg (plist-get palette :mode-line-bg))
         (mode-line-fg (plist-get palette :mode-line-fg))
         (inactive-bg (plist-get palette :inactive-bg))
         (inactive-fg (plist-get palette :inactive-fg))
         (shadow-fg (plist-get palette :shadow-fg))
         (accent-fg (plist-get palette :accent-fg))
         (note-fg (plist-get palette :note-fg))
         (success-fg (plist-get palette :success-fg))
         (warning-fg (plist-get palette :warning-fg))
         (error-fg (plist-get palette :error-fg))
         (visual-fg (plist-get palette :visual-fg))
         (command-fg (plist-get palette :command-fg)))
    (segment-line--apply-chip-face
     'segment-line-chip-muted
     mode-line-fg
     (segment-line--blend mode-line-fg mode-line-bg 0.08))
    (segment-line--apply-chip-face
     'segment-line-chip-accent
     accent-fg
     (segment-line--blend accent-fg mode-line-bg segment-line-chip-tint-alpha)
     'semibold)
    (segment-line--apply-chip-face
     'segment-line-chip-subtle
     shadow-fg
     inactive-bg)
    (segment-line--apply-chip-face
     'segment-line-chip-note
     note-fg
     (segment-line--blend note-fg mode-line-bg segment-line-chip-tint-alpha))
    (segment-line--apply-chip-face
     'segment-line-chip-success
     success-fg
     (segment-line--blend success-fg mode-line-bg segment-line-chip-tint-alpha))
    (segment-line--apply-chip-face
     'segment-line-chip-warning
     warning-fg
     (segment-line--blend warning-fg mode-line-bg segment-line-chip-tint-alpha))
    (segment-line--apply-chip-face
     'segment-line-chip-error
     error-fg
     (segment-line--blend error-fg mode-line-bg segment-line-chip-tint-alpha))
    (segment-line--apply-chip-face
     'segment-line-state-normal
     note-fg
     (segment-line--blend note-fg mode-line-bg segment-line-state-tint-alpha)
     'bold)
    (segment-line--apply-chip-face
     'segment-line-state-insert
     success-fg
     (segment-line--blend success-fg mode-line-bg segment-line-state-tint-alpha)
     'bold)
    (segment-line--apply-chip-face
     'segment-line-state-motion
     warning-fg
     (segment-line--blend warning-fg mode-line-bg segment-line-state-tint-alpha)
     'bold)
    (segment-line--apply-chip-face
     'segment-line-state-visual
     visual-fg
     (segment-line--blend visual-fg mode-line-bg segment-line-state-tint-alpha)
     'bold)
    (segment-line--apply-chip-face
     'segment-line-state-replace
     error-fg
     (segment-line--blend error-fg mode-line-bg segment-line-state-tint-alpha)
     'bold)
    (segment-line--apply-chip-face
     'segment-line-state-command
     command-fg
     (segment-line--blend command-fg mode-line-bg segment-line-state-tint-alpha)
     'bold)
    (set-face-attribute 'segment-line-inactive nil
                        :inherit 'mode-line-inactive
                        :foreground inactive-fg
                        :background inactive-bg
                        :box nil))
  (force-mode-line-update t))

(defun segment-line--theme-refresh-hook (&rest _)
  "Refresh Segment Line faces after theme changes."
  (unless (bound-and-true-p my/theme-refresh-suppressed)
    (segment-line-refresh-faces)))

(defun segment-line--install-theme-hooks ()
  "Install theme hooks used by Segment Line."
  (unless segment-line--theme-hooks-installed
    (add-hook 'enable-theme-functions #'segment-line--theme-refresh-hook)
    (add-hook 'disable-theme-functions #'segment-line--theme-refresh-hook)
    (setq segment-line--theme-hooks-installed t)))

(defun segment-line--remove-theme-hooks ()
  "Remove theme hooks used by Segment Line."
  (when segment-line--theme-hooks-installed
    (remove-hook 'enable-theme-functions #'segment-line--theme-refresh-hook)
    (remove-hook 'disable-theme-functions #'segment-line--theme-refresh-hook)
    (setq segment-line--theme-hooks-installed nil)))

(defun segment-line--parse-vc-mode-branch ()
  "Return the branch name encoded in `vc-mode', or nil."
  (when vc-mode
    (string-trim (replace-regexp-in-string "^[[:space:]]*[^:]+[:-]" "" vc-mode))))

(defun segment-line-refresh-vc-branch ()
  "Refresh the cached Git branch for the current buffer.
This keeps expensive VC fallback work out of the mode-line redisplay path."
  (setq-local
   segment-line--vc-branch-cache
   (or (segment-line--parse-vc-mode-branch)
       (when (featurep 'vc-git)
         (when-let* ((file-or-dir (or buffer-file-name default-directory))
                     (root (ignore-errors (vc-git-root file-or-dir))))
           (let ((default-directory root))
             (ignore-errors (vc-git-working-branch))))))))

(defun segment-line--active-p ()
  "Return non-nil when rendering the selected window's mode-line."
  (if (fboundp 'mode-line-window-selected-p)
      (mode-line-window-selected-p)
    (eq (selected-window) (frame-selected-window))))

(defun segment-line--icons-available-p ()
  "Return non-nil when `nerd-icons' and a usable Nerd Font are available."
  (and segment-line-enable-icons
       (display-graphic-p)
       (require 'nerd-icons nil t)
       (boundp 'nerd-icons-font-family)
       nerd-icons-font-family
       (find-font (font-spec :family nerd-icons-font-family))))

(defun segment-line--glyph (icon fallback)
  "Return ICON when Nerd Font rendering is available, else FALLBACK."
  (if (segment-line--icons-available-p) icon fallback))

(defun segment-line--pad (text pad)
  "Pad TEXT using PAD.
PAD can be nil, an integer, or a cons cell (LEFT . RIGHT)."
  (pcase pad
    ((and (pred integerp) n)
     (concat (make-string n ?\s) text (make-string n ?\s)))
    (`(,left . ,right)
     (concat (make-string (or left 0) ?\s) text (make-string (or right 0) ?\s)))
    (_ text)))

(defun segment-line--truncate (text width)
  "Truncate TEXT to WIDTH, preserving a single ellipsis when needed."
  (truncate-string-to-width text width nil nil t))

(defun segment-line--eval-maybe (value)
  "Evaluate VALUE when it is a function or symbol.
Otherwise return VALUE itself."
  (cond
   ((functionp value) (funcall value))
   ((and (symbolp value) (fboundp value)) (funcall value))
   (t value)))

(defun segment-line--primary-text (value)
  "Extract the primary text label from VALUE."
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   ((and (consp value) (keywordp (car value))) nil)
    ((listp value)
    (catch 'found
      (dolist (item value)
        (when-let* ((text (segment-line--primary-text item)))
          (throw 'found text)))
      nil))
   (t (format "%s" value))))

(defun segment-line--truthy-p (value)
  "Return non-nil when VALUE or its evaluation is non-nil."
  (let ((result (segment-line--eval-maybe value)))
    (and result (not (equal result "")))))

(defun segment-line--render-plist (component)
  "Render COMPONENT property list into a propertized string."
  (when (segment-line--truthy-p (plist-get component :when))
    (let* ((text (segment-line--eval-maybe (plist-get component :text)))
           (text (and text (format "%s" text)))
           (face (or (plist-get component :face)
                     (if (segment-line--active-p)
                         'mode-line
                       'segment-line-inactive)))
           (prefix (or (plist-get component :prefix) ""))
           (suffix (or (plist-get component :suffix) ""))
           (pad (plist-get component :pad)))
      (when (and text (not (string-empty-p text)))
        (propertize
         (segment-line--pad (concat prefix text suffix) pad)
         'face face)))))

(defun segment-line--render-component (component)
  "Render COMPONENT into a string or nil."
  (condition-case err
      (cond
       ((null component) nil)
       ((and (listp component) (keywordp (car component)))
        (segment-line--render-plist component))
       ((symbolp component)
        (when (fboundp component)
          (funcall component)))
       ((functionp component)
        (funcall component))
       ((stringp component) component)
       (t nil))
    (error
     (setq segment-line--last-error err)
     nil)))

(defun segment-line--render-list (components)
  "Render COMPONENTS and join them with `segment-line-separator'."
  (string-join
   (delq nil (mapcar #'segment-line--render-component components))
   segment-line-separator))

(defun segment-line--chip (text face &optional pad)
  "Render TEXT with FACE and optional PAD."
  (when (and text (not (string-empty-p text)))
    (propertize (segment-line--pad text (or pad segment-line-chip-padding)) 'face face)))

(defun segment-line--icon-for-buffer ()
  "Return an icon for the current buffer or nil.
Prefer `nerd-icons' when available."
  (when (segment-line--icons-available-p)
    (condition-case nil
        (cond
         ((buffer-file-name)
          (nerd-icons-icon-for-file (file-name-nondirectory (buffer-file-name))
                                    :height 0.9 :v-adjust 0.0))
         (t
          (nerd-icons-icon-for-mode major-mode :height 0.9 :v-adjust 0.0)))
      (error nil))))

(defun segment-line--project-name ()
  "Return the current project name, or nil when unavailable."
  (when-let* ((project (ignore-errors (project-current nil)))
              (root (ignore-errors (project-root project)))
              (name (file-name-nondirectory (directory-file-name root))))
    (segment-line--truncate name segment-line-project-name-max-width)))

(defun segment-line--current-symbol ()
  "Return current function or symbol name."
  (when segment-line-enable-which-function
    (when-let* ((name (or (and (bound-and-true-p which-function-mode)
                               (ignore-errors (which-function)))
                          (ignore-errors (add-log-current-defun)))))
      (segment-line--truncate (string-clean-whitespace name)
                              segment-line-symbol-name-max-width))))

(defun segment-line--vc-branch ()
  "Return the current Git branch for the buffer's repository."
  (or (segment-line--parse-vc-mode-branch)
      segment-line--vc-branch-cache))

(defun segment-line--flymake-counts ()
  "Return a plist with Flymake counters for the current buffer."
  (when (bound-and-true-p flymake-mode)
    (let ((errors 0)
          (warnings 0)
          (notes 0))
      (dolist (diag (flymake-diagnostics))
        (pcase (flymake--lookup-type-property
                (flymake-diagnostic-type diag)
                'flymake-category)
          ('flymake-error (cl-incf errors))
          ('flymake-warning (cl-incf warnings))
          ('flymake-note (cl-incf notes))
          (_ (cl-incf notes))))
      (list :error errors :warning warnings :note notes))))

(defun segment-line--position-string ()
  "Return fixed-width line/column summary."
  (let* ((line (line-number-at-pos))
         (column (current-column))
         (line-width (max segment-line-position-min-line-width
                          (length (number-to-string
                                   (line-number-at-pos (point-max))))))
         (column-width segment-line-position-column-width))
    (format (concat "%" (number-to-string line-width)
                    "d/%-" (number-to-string column-width)
                    "d")
            line
            column)))

(defun segment-line--coding-string ()
  "Return file coding system plus EOL marker."
  (let* ((coding (or buffer-file-coding-system buffer-file-name-coding-system 'utf-8-unix))
         (name (symbol-name coding))
         (base (replace-regexp-in-string "-\\(unix\\|dos\\|mac\\)$" "" name))
         (eol (pcase (coding-system-eol-type coding)
                (0 "LF")
                (1 "CRLF")
                (2 "CR")
                (_ "?"))))
    (upcase (format "ENC %s/%s" base eol))))

(defun segment-line--region-string ()
  "Return active region summary."
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (chars (abs (- end beg)))
           (lines (max 1 (count-lines beg end))))
      (format "SEL %dL %dC" lines chars))))

(defun segment-line--flag-string ()
  "Return transient editing flags."
  (let ((parts (delq nil
                     (list (and defining-kbd-macro "REC")
                           (and (buffer-narrowed-p) "NRW")
                           (and (file-remote-p default-directory) "SSH")))))
    (when parts
      (string-join parts " "))))

(defun segment-line--process-string ()
  "Return process status for the current buffer."
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (pcase (process-status proc)
      ('run (format "PROC %s" (process-name proc)))
      ('open (format "PROC %s" (process-name proc)))
      ('listen (format "PROC %s" (process-name proc)))
      ('connect (format "PROC %s" (process-name proc)))
      ('stop (format "STOP %s" (process-name proc)))
      ('exit (format "EXIT %s" (process-name proc)))
      ('signal (format "SIG %s" (process-name proc)))
      (_ nil))))

(defun segment-line--input-method-string ()
  "Return input method label."
  (cond
   ((and current-input-method
         (string= current-input-method "rime")
         (bound-and-true-p rime-mode))
    (if (and (fboundp 'rime--should-enable-p)
             (fboundp 'rime--should-inline-ascii-p)
             (rime--should-enable-p)
             (not (rime--should-inline-ascii-p)))
        "中"
      "英"))
   (current-input-method-title current-input-method-title)
   (current-input-method current-input-method)
   (t nil)))

(defun segment-line--eglot-server-name (server)
  "Return a friendly language server name for Eglot SERVER."
  (or (and server
           (fboundp 'eglot--server-info)
           (ignore-errors
             (plist-get (eglot--server-info server) :name)))
      (and server
           (fboundp 'jsonrpc-name)
           (ignore-errors (jsonrpc-name server)))))

(defun segment-line--eglot-configured-p ()
  "Return non-nil when current major mode has an Eglot server configured."
  (and (featurep 'eglot)
       (fboundp 'eglot--lookup-mode)
       (ignore-errors
         (cdr (eglot--lookup-mode major-mode)))))

(defun segment-line--lsp-status ()
  "Return a cons cell (LABEL . FACE) describing current LSP client/server."
  (cond
   ((and (featurep 'eglot)
         (fboundp 'eglot-managed-p)
         (eglot-managed-p))
    (let* ((server (ignore-errors (eglot-current-server)))
           (server-name (segment-line--eglot-server-name server))
           (running (and server
                         (fboundp 'jsonrpc-running-p)
                         (ignore-errors (jsonrpc-running-p server)))))
      (cons (format "EGLOT %s"
                    (or server-name
                        (and running "SERVER")
                        "WAIT"))
            (if running
                'segment-line-chip-success
              'segment-line-chip-warning))))
   ((segment-line--eglot-configured-p)
    '("EGLOT OFF" . segment-line-chip-subtle))
   (t nil)))

(defun segment-line--major-mode-label ()
  "Return mode label with optional icon."
  (let* ((icon (segment-line--icon-for-buffer))
         (label (cond
                 ((stringp mode-name) mode-name)
                 ((symbolp mode-name) (symbol-name mode-name))
                 (t (or (segment-line--primary-text mode-name)
                        (format-mode-line mode-name))))))
    (string-join (delq nil (list (and icon (not (string-empty-p icon)) icon)
                                 (and label (not (string-empty-p label)) label)))
                 " ")))

(defun segment-line--buffer-label ()
  "Return current buffer label with icon."
  (let* ((name (segment-line--truncate (buffer-name) segment-line-buffer-name-max-width))
         (icon (segment-line--icon-for-buffer))
         (mark (cond (buffer-read-only
                      (segment-line--glyph "" "RO"))
                     ((buffer-modified-p)
                      (segment-line--glyph "●" "*"))
                     (t nil))))
    (string-join
     (delq nil (list (and icon (not (string-empty-p icon)) icon) name mark))
     " ")))

(defun segment-line--state-label-and-face ()
  "Return a cons cell (LABEL . FACE) for the current editing state."
  (cond
   ((bound-and-true-p meow-insert-mode) '("INS" . segment-line-state-insert))
   ((bound-and-true-p meow-normal-mode) '("NOR" . segment-line-state-normal))
   ((bound-and-true-p meow-motion-mode) '("MOT" . segment-line-state-motion))
   ((bound-and-true-p meow-visual-mode) '("VIS" . segment-line-state-visual))
   ((bound-and-true-p meow-keypad-mode) '("CMD" . segment-line-state-command))
   ((bound-and-true-p overwrite-mode) '("REP" . segment-line-state-replace))
   ((use-region-p) '("SEL" . segment-line-state-visual))
   (t '("EMA" . segment-line-chip-accent))))

(defun segment-line-segment-state ()
  "Render modal state segment."
  (pcase-let ((`(,label . ,face) (segment-line--state-label-and-face)))
    (segment-line--chip label (if (segment-line--active-p) face 'segment-line-inactive))))

(defun segment-line-segment-buffer ()
  "Render buffer segment."
  (segment-line--chip
   (segment-line--buffer-label)
   (if (segment-line--active-p) 'segment-line-chip-accent 'segment-line-inactive)
   segment-line-chip-padding))

(defun segment-line-segment-input-method ()
  "Render input method segment."
  (when-let* ((label (segment-line--input-method-string)))
    (segment-line--chip
     label
     (if (segment-line--active-p) 'segment-line-chip-muted 'segment-line-inactive))))

(defun segment-line-segment-symbol ()
  "Render current symbol segment."
  (when-let* ((symbol (segment-line--current-symbol)))
    (segment-line--chip
     symbol
     (if (segment-line--active-p) 'segment-line-chip-muted 'segment-line-inactive))))

(defun segment-line-segment-project ()
  "Render current project segment."
  (when-let* ((name (segment-line--project-name)))
    (segment-line--chip
     (format "%s %s" (segment-line--glyph "󰉋" "PRJ") name)
     (if (segment-line--active-p) 'segment-line-chip-subtle 'segment-line-inactive))))

(defun segment-line-segment-vc ()
  "Render VC branch segment."
  (when-let* ((branch (segment-line--vc-branch)))
    (let ((face (if (buffer-modified-p)
                    'segment-line-chip-warning
                  'segment-line-chip-success)))
      (segment-line--chip
       (format "%s %s" (segment-line--glyph "" "GIT") branch)
       (if (segment-line--active-p) face 'segment-line-inactive)))))

(defun segment-line-segment-diagnostics ()
  "Render diagnostics segment for Flymake buffers."
  (when-let* ((counts (segment-line--flymake-counts)))
    (let* ((errors (plist-get counts :error))
           (warnings (plist-get counts :warning))
           (notes (plist-get counts :note))
           (parts (list (segment-line--chip
                         (format "%s %d"
                                 (segment-line--glyph "󰅚" "E")
                                 errors)
                         (if (segment-line--active-p)
                             'segment-line-chip-error
                           'segment-line-inactive)
                         segment-line-chip-padding)
                        (segment-line--chip
                         (format "%s %d"
                                 (segment-line--glyph "󰀪" "W")
                                 warnings)
                         (if (segment-line--active-p)
                             'segment-line-chip-warning
                           'segment-line-inactive)
                         segment-line-chip-padding)
                        (segment-line--chip
                         (format "%s %d"
                                 (segment-line--glyph "󰋽" "N")
                                 notes)
                         (if (segment-line--active-p)
                             'segment-line-chip-note
                           'segment-line-inactive)
                         segment-line-chip-padding))))
      (apply #'concat parts))))

(defun segment-line-segment-flags ()
  "Render transient buffer flags."
  (when-let* ((flags (segment-line--flag-string)))
    (segment-line--chip
     flags
     (if (segment-line--active-p) 'segment-line-chip-warning 'segment-line-inactive))))

(defun segment-line-segment-region ()
  "Render active region statistics."
  (when-let* ((region (segment-line--region-string)))
    (segment-line--chip
     region
     (if (segment-line--active-p) 'segment-line-chip-subtle 'segment-line-inactive))))

(defun segment-line-segment-process ()
  "Render process state."
  (when-let* ((process (segment-line--process-string)))
    (segment-line--chip
     process
     (if (segment-line--active-p) 'segment-line-chip-subtle 'segment-line-inactive))))

(defun segment-line-segment-lsp ()
  "Render LSP segment."
  (when-let* ((status (segment-line--lsp-status)))
    (segment-line--chip
     (car status)
     (if (segment-line--active-p) (cdr status) 'segment-line-inactive))))

(defun segment-line-segment-coding ()
  "Render coding system segment."
  (segment-line--chip
   (segment-line--coding-string)
   (if (segment-line--active-p) 'segment-line-chip-subtle 'segment-line-inactive)))

(defun segment-line-segment-major-mode ()
  "Render major mode segment."
  (segment-line--chip
   (segment-line--major-mode-label)
   (if (segment-line--active-p) 'segment-line-chip-muted 'segment-line-inactive)))

(defun segment-line-segment-position ()
  "Render cursor position segment."
  (segment-line--chip
   (segment-line--position-string)
   (if (segment-line--active-p) 'segment-line-chip-accent 'segment-line-inactive)))

(defun segment-line-format ()
  "Render the complete mode-line value."
  (let ((left (segment-line--render-list segment-line-left))
        (right (segment-line--render-list segment-line-right)))
    (cons left right)))

(defun segment-line--safe-left ()
  "Render the left side of the mode-line."
  (condition-case err
      (or (segment-line--render-list segment-line-left) "")
    (error
     (setq segment-line--last-error err)
     (propertize
      (format " %s " (buffer-name))
      'face (if (segment-line--active-p) 'mode-line 'segment-line-inactive)))))

(defun segment-line--safe-right ()
  "Render the right side of the mode-line."
  (condition-case err
      (or (segment-line--render-list segment-line-right) "")
    (error
     (setq segment-line--last-error err)
     (propertize " mode-line error " 'face 'error))))

(defun segment-line--safe-format ()
  "Render the mode-line and downgrade failures to a tiny fallback segment."
  (condition-case err
      (segment-line-format)
    (error
     (setq segment-line--last-error err)
     (cons
      (propertize
       (format " %s " (buffer-name))
       'face (if (segment-line--active-p) 'mode-line 'segment-line-inactive))
      (propertize " mode-line error " 'face 'error)))))

(defvar segment-line--mode-line
  '((:eval (segment-line--safe-left))
    mode-line-format-right-align
    (:eval (segment-line--safe-right)))
  "Mode-line value installed by `segment-line-mode'.")

(put 'segment-line--mode-line 'risky-local-variable t)

;;;###autoload
(define-minor-mode segment-line-mode
  "Toggle the composable Segment Line mode-line."
  :global t
  :group 'segment-line
  (if segment-line-mode
      (progn
        (segment-line--install-theme-hooks)
        (when (and segment-line-enable-which-function
                   (require 'which-func nil t)
                   (not (bound-and-true-p which-function-mode)))
          (which-function-mode 1)
          (setq segment-line--enabled-which-function t))
        (add-hook 'find-file-hook #'segment-line-refresh-vc-branch)
        (add-hook 'after-save-hook #'segment-line-refresh-vc-branch)
        (add-hook 'after-revert-hook #'segment-line-refresh-vc-branch)
        (setq segment-line--saved-mode-line-format (default-value 'mode-line-format))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (segment-line-refresh-vc-branch)))
        (segment-line-refresh-faces)
        (setq-default mode-line-format segment-line--mode-line))
    (remove-hook 'find-file-hook #'segment-line-refresh-vc-branch)
    (remove-hook 'after-save-hook #'segment-line-refresh-vc-branch)
    (remove-hook 'after-revert-hook #'segment-line-refresh-vc-branch)
    (segment-line--remove-theme-hooks)
    (when segment-line--enabled-which-function
      (which-function-mode -1)
      (setq segment-line--enabled-which-function nil))
    (when segment-line--saved-mode-line-format
      (setq-default mode-line-format segment-line--saved-mode-line-format)))
  (force-mode-line-update t))

(provide 'segment-line)
;;; segment-line.el ends here.
