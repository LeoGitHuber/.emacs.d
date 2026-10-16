;;; init-diagnostics.el --- Flymake and diff indicators -*- lexical-binding: t; -*-

;;; Commentary:
;; Use built-in Flymake rendering with the existing margin icons and colors.

;;; Code:
(require 'init-func)

;;; Diagnostics / icons
(require 'nerd-icons)
(require 'nerd-icons-corfu)

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

;; Reserve room for the glyph; the padding gets a text face below because
;; Symbols Nerd Font Mono has no space glyph in the installed Windows font.
(setq flymake-indicator-type 'margins
      flymake-margin-indicators-string
      `((error ,(concat (my/nerd-icon-glyph "󰅙" "E") " ") diagnostics-error)
        (warning ,(concat (my/nerd-icon-glyph "" "W") " ") diagnostics-warn)
        (note ,(concat (my/nerd-icon-glyph "" "N") " ") diagnostics-info)))

;; Flymake copies these values into type properties when the library loads.
;; Update them once after setting our icons, rather than on every mode hook.
(dolist (spec '((flymake-error . error)
                (flymake-warning . warning)
                (flymake-note . note)))
  (put (car spec) 'flymake-margin-string
       (alist-get (cdr spec) flymake-margin-indicators-string)))

;; Expand the current line with native guides; keep other lines compact.
(if (boundp 'flymake-inline-diagnostics)
    (progn
      (setq flymake-inline-diagnostics
            '((current-line . fancy) (t . short)))
      (setf (alist-get :inline-fancy flymake-diagnostic-format-alist)
            '(origin code message)))
  (setq flymake-show-diagnostics-at-end-of-line t))

(defun my/flymake-refresh-margin-icons (&rest _)
  "Keep the most severe margin icon per line, preserving all diagnostics."
  (when (and flymake-mode (eq flymake-indicator-type 'margins))
    (save-excursion
      (save-restriction
        (widen)
        (let ((seen (make-hash-table :test #'eql)))
          (dolist (diag (cl-sort (flymake-diagnostics) #'>
                                :key (lambda (d)
                                       (flymake--severity (flymake-diagnostic-type d)))))
            (let* ((ov (flymake--diag-overlay diag))
                   (icon (or (overlay-get ov 'my/flymake-margin-before-string)
                             (overlay-get ov 'before-string))))
              (when (and (stringp icon) (> (length icon) 0)
                         (equal (car-safe (get-text-property 0 'display icon))
                                (list 'margin flymake-margin-indicator-position)))
                ;; Flymake replaces the string's face, including its padding.
                (let ((glyph (cadr (get-text-property 0 'display icon))))
                  (when (and (stringp glyph) (string-suffix-p " " glyph))
                    (put-text-property (1- (length glyph)) (length glyph)
                                       'face (if (facep 'margin)
                                                 '(margin default) 'default)
                                       glyph)))
                (goto-char (overlay-start ov))
                (let ((line (line-beginning-position)))
                  ;; Retain hidden icons so they can return after a more
                  ;; severe diagnostic from another backend disappears.
                  (overlay-put ov 'my/flymake-margin-before-string icon)
                  (overlay-put ov 'before-string (unless (gethash line seen) icon))
                  (puthash line t seen))))))))))

;; Follow Flymake's own report and edit callbacks.
(advice-add 'flymake--handle-report :after #'my/flymake-refresh-margin-icons)
(advice-add 'flymake-after-change-function :after #'my/flymake-refresh-margin-icons)

(add-hook 'prog-mode-hook #'flymake-mode)

(require 'diff-hl)
(require 'diff-hl-dired)
(require 'diff-hl-flydiff)
(require 'diff-hl-margin)

;; Keep VCS change markers visible across GUI and terminal frames without
;; competing with the current Flymake left-margin indicators.
(setq diff-hl-side 'left)
;; Use theme backgrounds as solid color bars in the left fringe.
(when (fboundp 'set-fringe-style)
  (set-fringe-style '(8 . nil)))
(setq diff-hl-highlight-function #'diff-hl-highlight-on-fringe-flat
      diff-hl-highlight-reference-function #'diff-hl-highlight-on-fringe-flat
      diff-hl-fringe-flat-bmp 'diff-hl-bmp-empty
      diff-hl-draw-borders nil)

(defun my/diff-hl-setup-display-backend ()
  "Use fringe highlights in GUI frames and right-margin highlights in terminals."
  (if (display-graphic-p)
      (progn
        (when (bound-and-true-p diff-hl-margin-local-mode)
          (diff-hl-margin-local-mode -1))
        (setq-local diff-hl-side 'left
                    diff-hl-highlight-function #'diff-hl-highlight-on-fringe-flat
                    diff-hl-highlight-reference-function #'diff-hl-highlight-on-fringe-flat)
        (when (fboundp 'set-fringe-style)
          (set-fringe-style '(8 . nil))))
    (when (and (bound-and-true-p diff-hl-margin-local-mode)
               (not (eq diff-hl-side 'right)))
      (diff-hl-margin-local-mode -1))
    (setq-local diff-hl-side 'right)
    (unless (bound-and-true-p diff-hl-margin-local-mode)
      (diff-hl-margin-local-mode 1))))

(add-hook 'diff-hl-mode-on-hook #'my/diff-hl-setup-display-backend)
(add-hook 'diff-hl-dired-mode-on-hook #'my/diff-hl-setup-display-backend)

(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1)

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote))

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

(defun my/flymake-inline-face (face)
  "Resolve FACE to the name used by this Flymake version."
  (or (get face 'face-alias)
      (and (facep face) face)
      (alist-get face '((flymake-inline-annotation-face . flymake-end-of-line-diagnostics-face)
                        (flymake-error-inline . flymake-error-echo-at-eol)
                        (flymake-warning-inline . flymake-warning-echo-at-eol)
                        (flymake-note-inline . flymake-note-echo-at-eol)))
      face))

(defun my/flymake-refresh-theme (&optional _theme)
  "Keep built-in diagnostic text consistent with the current theme."
  (interactive)
  (set-face-attribute (my/flymake-inline-face 'flymake-inline-annotation-face)
                      nil :inherit 'default :height 0.9 :box nil
                      :foreground 'unspecified :background 'unspecified)
  (dolist (spec '((flymake-error-inline error error)
                  (flymake-warning-inline warning warning)
                  (flymake-note-inline success info)))
    (set-face-attribute (my/flymake-inline-face (car spec)) nil
                        :inherit (list (my/flymake-inline-face 'flymake-inline-annotation-face)
                                       (cadr spec))
                        :foreground (my/flymake--theme-foreground (caddr spec))
                        :weight 'medium :box nil))
  (set-face-attribute 'flymake-eol-information-face nil
                      :inherit 'shadow
                      :foreground (or (my/flymake--face-color 'shadow :foreground)
                                      (my/flymake--face-color 'mode-line-inactive :foreground)
                                      (my/flymake--face-color 'default :foreground))
                      :slant 'normal :box nil))

(defun my/flymake--theme-refresh-hook (&rest _)
  "Refresh diagnostic faces after a theme change."
  (unless (bound-and-true-p my/theme-refresh-suppressed)
    (my/flymake-refresh-theme)))

(add-hook 'enable-theme-functions #'my/flymake--theme-refresh-hook)
(add-hook 'disable-theme-functions #'my/flymake--theme-refresh-hook)
(my/flymake-refresh-theme)

(provide 'init-diagnostics)
;;; init-diagnostics.el ends here
