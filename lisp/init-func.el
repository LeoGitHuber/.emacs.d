;;; init-func --- Init for function  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)
(require 'cl-seq)

;;;###autoload
(defun kill-or-save (arg)
  "Kill or save ARG words."
  (interactive "p")
  (if (region-active-p)
      (call-interactively 'kill-ring-save)
    (if (bound-and-true-p cns-mode)
        (cns-backward-kill-word arg)
      (if (bound-and-true-p puni-mode)
          (puni-backward-kill-word arg)
        (backward-kill-word arg)))))

;;;###autoload
(defun smart-kill-line (&optional args)
  "Kill ARGS line in a smart way."
  (interactive "p")
  (if (bound-and-true-p puni-mode)
      (puni-kill-line args)
    (if (bound-and-true-p fingertip-mode)
        (fingertip-kill)
      (kill-line args))))

;;;###autoload
(defun downcase-any (arg)
  "Downcase any situation with ARG words."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (downcase-region (region-beginning) (region-end))
      (progn
        (left-word)
        (downcase-word arg)))))

;;;###autoload
(defun upcase-any (arg)
  "Upcase any situation with ARG words."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (upcase-region (region-beginning) (region-end))
      (progn
        (left-word)
        (upcase-word arg)))))

;;;###autoload
(defun capitalize-any (arg)
  "Capitalize any situation with ARG words."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (capitalize-region (region-beginning) (region-end))
      (progn
        (left-word)
        (capitalize-word arg)))))

;;;###autoload
(defun open-newline-above (arg)
  "Move to the previous ARG line (like vi) and then opens a line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (if (not (member major-mode '(org-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))

;;;###autoload
(defun open-newline-below (arg)
  "Move to the next ARG line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (if (not (member major-mode '(org-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))

(defvar code-font "Consolas"  ;; "Fantasque Sans Mono", "InputMono"
  "Font for coding.")

(defvar cjk-font "Sarasa Gothic SC"  ;; "FZYouSongJ GBK"
  "CJK font.")

(defvar serif-font "Bookerly" ;; Palatino Linotype
  "Serif font.")

(defvar cjk-sans-font "LXGW WenKai Screen"
  "CJK sans font.")

(defvar verbatim-font "Source Han Sans CN"
  "Font for verbatim.")

;; Maple Mono NF --- Maple Mono SC NF, HarmonyOS Sans SC
;; PragmataPro Mono Liga --- SimHei
;; Hack --- HarmonyOS Sans SC
;; JetBrainsMono NF
;; Iosevka Fixed --- Input Mono

;;;###autoload
(defun set-en_cn-font (en-font cn-font serif-font sans-font verbatim-font en-size cn-size)
  "EN-FONT, CN-FONT mean font-family.  EN-SIZE, CN-SIZE mean font size.
And Set SERIF-FONT, SANS-FONT and VERBATIM-FONT."
  (set-face-attribute
   'default nil
   :font (font-spec
          :name en-font
          :weight 'regular
          ;; :slant 'normal
          :size en-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (if (equal en-size cn-size)
        (set-fontset-font "fontset-default" charset (font-spec :family cn-font))
      (set-fontset-font "fontset-default" charset (font-spec :family cn-font :size cn-size))))
  (create-fontset-from-fontset-spec
   (font-xlfd-name
    (font-spec :family en-font
               :registry "fontset-variable pitch verbatim")))
  (set-fontset-font "fontset-variable pitch verbatim" 'han
                    (font-spec :family verbatim-font))
  (set-fontset-font "fontset-variable pitch verbatim" 'cjk-misc
                    (font-spec :family verbatim-font))
  (dolist (sp `(("regular" . ,cn-font)
                ("italic" . ,sans-font)
                ;; ("verbatim" . ,verbatim-font)
		        ))
    (let ((registry (concat "fontset-variable pitch " (car sp))))
      (create-fontset-from-fontset-spec
       (font-xlfd-name
        (font-spec :family serif-font
                   :registry registry)))
      (if (equal en-size cn-size)
          (progn
            (set-fontset-font registry 'han
                              (font-spec :family (cdr sp)))
            (set-fontset-font registry 'cjk-misc
                              (font-spec :family (cdr sp))))
        (progn
          (set-fontset-font registry 'han
                            (font-spec :family (cdr sp) :size cn-size))
          (set-fontset-font registry 'cjk-misc
                            (font-spec :family (cdr sp) :size cn-size)))
        )
      ))
  (with-eval-after-load 'org
    (set-face-attribute 'variable-pitch nil
                        :family serif-font
                        :fontset "fontset-variable pitch regular")
    (set-face-attribute 'fixed-pitch nil
                        :family en-font
                        ;; :fontset "fontset-variable pitch regular"
                        )
    (defface org-emphasis-italic
      '((default :inherit italic))
      "My italic emphasis for Org.")
    (set-face-attribute 'org-emphasis-italic nil :fontset "fontset-variable pitch italic")
    (defface org-emphasis-verbatim
      '((default :inherit org-verbatim))
      "My verbatim emphasis for Org.")
    (set-face-attribute 'org-verbatim nil :fontset "fontset-variable pitch verbatim")
    (defface org-emphasis-code
      '((default :inherit org-code))
      "My code emphasis for Org.")
    (set-face-attribute 'org-code nil :fontset "fontset-variable pitch verbatim")
    (set-face-attribute 'org-block nil :fontset "fontset-variable pitch verbatim")

    (setq org-emphasis-alist
          '(("*" bold)
            ("/" org-emphasis-italic)
            ("_" underline)
            ("=" org-verbatim verbatim)
            ("~" org-code verbatim)
            ("+" (:strike-through t))))
    ;; (set-face-attribute 'fixed-pitch nil
    ;;                     :font (font-spec :name en-font))
    ;; (set-face-attribute 'org-block nil
    ;;                     :fontset )
    )
  )

(defun insert-tab-char()
  "Insert a tab char. (ASCII 9, \t)."
  (interactive)
  (insert "\t"))

;;;###autoload
(defun comment-or-uncomment ()
  (interactive)
  (if (region-active-p)
      (progn
        (kill-ring-save (region-beginning) (region-end))
        (comment-or-uncomment-region (region-beginning) (region-end)))
    (if (save-excursion
          (beginning-of-line)
          (looking-at "\\s-*$"))
        (progn
          (call-interactively 'comment-dwim)
          (next-line))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

;;;###autoload
(defun hideshow-folded-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

;;;###autoload
(defun update-all-autoloads ()
  (interactive)
  (cd "~/.emacs.d")
  (let ((generated-autoload-file
         (expand-file-name "loaddefs.el")))
    (when (not (file-exists-p generated-autoload-file))
      (with-current-buffer (find-file-noselect generated-autoload-file)
        (insert ";;") ;; create the file with non-zero size to appease autoload
(save-buffer)))
(mapcar #'update-directory-autoloads
        '("" "modes" "git/org-fu"))

(cd "personal")
(setq generated-autoload-file (expand-file-name "loaddefs.el"))
(update-directory-autoloads "")))

;;;###autoload
(defun hl-current-line-range ()
  "Function for `hl-line-range-function'."
  (cons (line-beginning-position) (+ 1 (line-end-position))))

(defun get-pure-cons (list1 list2)
  "Combine LIST1 and LIST2.
The resulting list contains all items that appear in LIST1 but not LIST2."
  (let ((result '())
        (num1 (length list1))
        (num2 (length list2))
        (compute 0)
        (n1 0)
        (n2 0))
    (while (<= n1 num1)
      (while (<= n2 num2)
        (if (eq (nth n1 list1) (nth n2 list2))
            (progn
              (setq n2 (+ 1 num2))
              (setq compute 1))
          (setq n2 (+ n2 1))))
      (when (equal compute 0)
        (setq result (cons (nth n1 list1) result)))
      (setq n1 (+ n1 1))
      (setq n2 0)
      (setq compute 0))
    result))

(defun screen-capture (place &rest _)
  "Use grim to capture screen and store it into PLACE."
  (interactive
   (find-file-read-args "Store into: "
                        (confirm-nonexistent-file-or-buffer)))
  (shell-command (format "grim -l 0 -g \"$(slurp)\" %s" place) nil nil)
  (kill-new (format "[[file:%s][]]" place)))

(defun find-subdir-recursively (dir)
  "Find all subdirectories in DIR.

Dot-directories and directories contain `.nosearch' will be skipped."
  (thread-last (directory-files dir nil)
               (cl-remove-if (lambda (f)
                               (string-prefix-p "." f)))
               (mapcar (lambda (d) (expand-file-name d dir)))
               (cl-remove-if-not #'file-directory-p)
               (cl-remove-if (lambda (d)
                               (string-suffix-p "test" d)))
               ;; (cl-remove-if (lambda (d)
               ;;                 (string-suffix-p "helpful" d)))
               (cl-remove-if (lambda (d)
                               (file-exists-p (expand-file-name ".nosearch"
                                                                d))))))

(defun find-dir-recursively (dir &optional but)
  "Find all `.el' files in DIR and its subdirectories."
  ;; (let ((subdir (mapcar #'abbreviate-file-name (find-subdir-recursively dir))))
  (let* ((but (or but 0))
         (subdir (butlast (mapcar #'abbreviate-file-name (find-subdir-recursively dir)) but)))
    (nconc subdir
           (mapcar #'abbreviate-file-name (mapcan #'find-dir-recursively subdir)))))

(defvar autoloads-file "~/.emacs.d/site-lisp/loaddefs.el"
  "File with all of autoload setting.")

(defvar site-lisp-directory "~/.emacs.d/site-lisp"
  "Directory contained of all third party packages.")

(defun generate-autoloads (&optional dir target)
  "Generate autoload files recursively for all package in DIR to file TARGET.

If DIR is omitted, use `cm/site-lisp-directory' as DIR, if target is ommitted
use `cm/autoloads-file' as TARGET."
  (interactive)
  (let* ((target (or target autoloads-file))
         (dir (or dir site-lisp-directory)))
    (loaddefs-generate (find-dir-recursively dir) target nil nil nil t)))

;; (defun recentf-time-sort ()
;;   "Sort recentf file list."
;;   (when recentf-mode
;;     (thread-last
;;       recentf-list
;;       ;; Use modification time, since getting file access time seems to count as
;;       ;; accessing the file, ruining future uses.
;;       (mapcar (lambda (f)
;;                 (cons f (file-attribute-access-time (file-attributes f)))))
;;       (seq-sort (pcase-lambda (`(,f1 . ,t1) `(,f2 . ,t2))
;;                   ;; Want existing, most recent, local files first.
;;                   (cond ((or (not (file-exists-p f1))
;;                              (file-remote-p f1))
;;                          nil)
;;                         ((or (not (file-exists-p f2))
;;                              (file-remote-p f2))
;;                          t)
;;                         (t (time-less-p t2 t1)))))
;;       (mapcar #'car))))

(defun meow-yank-forward ()
  "Yank forward."
  (interactive)
  (let ((select-enable-clipboard meow-use-clipboard))
    (save-excursion
      (meow--execute-kbd-macro meow--kbd-yank))
    ))

(defvar meow--selection-record nil
  "Variable that saves marker before meow exit selection.")

(defvar meow--selection-count nil
  "Variable that saves last position before meow exit selection.")

(defun meow--pre-cancel-selection()
  "Save positions of meow selection before its exit."
  (when meow--selection-history
    (let* ((start (cadar meow--selection-history))
           (end (cadr (cdar meow--selection-history)))
           (line (+ (count-lines start end) 1)))
      (setq meow--selection-record
            (set-marker (mark-marker) start))
      (if (> start end)
          (setq meow--selection-count (- line))
        (setq meow--selection-count line))
      (deactivate-mark t)
      )))

(defun meow-reselect ()
  "Meow reselect region."
  (interactive)
  (goto-char meow--selection-record)
  (meow-line meow--selection-count)
  )

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("M-q" . ignore)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-forward)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("M-q" . ignore)
   '("<escape>" . ignore))
  (meow-define-keys 'insert '("M-q" . meow-insert-exit))
  (meow-define-keys 'normal '("V" . meow-reselect))
  (advice-add 'meow--cancel-selection :before (lambda() (meow--pre-cancel-selection)))
  ;; (add-hook 'meow-insert-exit-hook
  ;;           (lambda ()
  ;;             (and buffer-file-name
  ;;                  (save-buffer))))
  )

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(defun lsp-enable-startup ()
  "Enable `eglot' or `lsp-mode' for LSP."
  ;; (and (functionp 'lsp-bridge-mode)
  ;;      (global-lsp-bridge-mode))
  ;; (require 'lsp-bridge)
  (with-eval-after-load 'lsp-bridge
    (remove-hook 'lsp-bridge-default-mode-hooks 'LaTeX-mode-hook)
    (remove-hook 'lsp-bridge-default-mode-hooks 'latex-mode-hook)
    (remove-hook 'lsp-bridge-default-mode-hooks 'Tex-latex-mode-hook)
    (remove-hook 'lsp-bridge-default-mode-hooks 'typescript-ts-mode-hook)
    (remove-hook 'lsp-bridge-default-mode-hooks 'typescript-mode-hook)
    (global-lsp-bridge-mode))
  (dolist (hook '(prog-mdoe-hook
                  cuda-mode-hook
                  TeX-mode-hook
                  c-ts-mode-hook c++-ts-mode-hook
                  python-ts-mode-hook python-ts-mode-hook
                  emacs-lisp-mode-hook
                  js-mode-hook
                  js-ts-mode-hook))
    ;; (and (functionp 'corfu-mode)
    ;;      (global-corfu-mode))
    (when (intern-soft "global-corfu-mode")
      (and (functionp 'corfu-mode)
           (add-hook hook 'corfu-mode)))
    (add-hook hook (lambda ()
                     (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode
                                             ;; 'verilog-mode
                                             'makefile-mode 'snippet-mode)
                       (with-eval-after-load 'lsp
                         (lsp-deferred))
                       (eglot-ensure)))))
  (with-eval-after-load 'lsp-mode
    (with-eval-after-load 'lsp-ui
      (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
      (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
    (setq lsp-keymap-prefix "C-c l"
          ;; lsp-keep-workspace-alive nil
          ;; lsp-signature-auto-activate nil
          lsp-modeline-code-actions-enable nil
          lsp-modeline-diagnostics-enable nil
          lsp-modeline-workspace-status-enable nil
          lsp-headerline-breadcrumb-enable t
          lsp-semantic-tokens-enable t
          ;; lsp-progress-spinner-type 'progress-bar-filled
          lsp-enable-file-watchers nil
          lsp-enable-folding nil
          lsp-enable-symbol-highlighting nil
          lsp-enable-text-document-color nil
          lsp-enable-indentation nil
          lsp-enable-on-type-formatting nil
          lsp-enable-indentation nil
          ;; lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)  ;; For diagnostics
          ;; lsp-lens-enable nil  ;; Reference Lens
          ;; lsp-ui-doc-show-with-cursor nil  ;; ui
          lsp-completion-provider :none
          lsp-prefer-flymake t
          lsp-ui-flycheck-enable nil
          lsp-enable-relative-indentation t)
    ;; (keymap-set lsp-mode-map "C-c C-d" 'lsp-describe-thing-at-point)
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
    (add-hook 'lsp-mode-hook 'corfu-mode)))

(defvar better-gc-cons-threshold (* 32 1024 1024) ;; 128mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.
If you experience stuttering, increase this.")

;; 开启 minibuffer 的时候不要 gc
(defun gc-minibuffer-setup-hook ()
  "Turn off garbage collection during setup minibuffer."
  (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

(defun gc-minibuffer-exit-hook ()
  "Turn on garbage collection after minibuffer exit."
  (garbage-collect)
  (setq gc-cons-threshold better-gc-cons-threshold))

(defun setup-display-graphic (modelineq cfborderq dayon dayoff themesetq)
  "Setup display graphic for GUI Emacs and Emacsclient with MODELINEQ, CFBORDERQ, DAYON, DAYOFF, THEMESETQ."
  (when (display-graphic-p)
    ;; (if (not windows-system-p)
    (set-en_cn-font code-font cjk-font serif-font
                    cjk-sans-font verbatim-font 12.0 12.0)
    ;;   )
    (setq frame-title-format
          '((:eval (if (buffer-file-name)
                       (abbreviate-file-name
                        (buffer-name))
                     "%b")))
          bidi-inhibit-bpa t
          long-line-threshold 1000
          large-hscroll-threshold 1000
          syntax-wholeline-max 1000)

    ;; ;; Enable Ligatures Feature, more info: https://github.com/mickeynp/ligature.el
    ;; (global-ligature-mode)
    ;; ;; PragmataPro Font Ligature Support
    ;; (ligature-pragmatapro-setup)
    ;; Set icon for truncation
    ;; (setq truncate-string-ellipsis (nerd-icons-mdicon "nf-md-arrow_down_right"))

    (setq x-underline-at-descent-line t)

    ;; Don't use help echo tooltips
    ;; (setq x-gtk-use-system-tooltips nil)
    ;; (unless (symbol-value x-gtk-use-system-tooltips)
    ;;   (set-face-attribute 'tooltip nil))

    ;; Set this face for `show-paren-mode' context appearance when offscreen.
    (when cfborderq
      (let ((mode-line-box-p (face-attribute 'mode-line-highlight :box)))
        (when (not (eq mode-line-box-p 'unspecified))
          (if (consp mode-line-box-p)
              (set-face-attribute 'child-frame-border
                                  nil
                                  :background (nth 3 mode-line-box-p))
            (set-face-attribute 'child-frame-border
                                nil
                                :background mode-line-box-p)
            ))))
    (if themesetq
        (if (or
             (>= (string-to-number (substring (current-time-string) 11 13)) dayoff)
             (<= (string-to-number (substring (current-time-string) 11 13)) dayon))
            (if (string-prefix-p "modus" (symbol-name (cadr themes_chosen)))
                (progn
                  (setq modus-themes-org-blocks 'gray-background
                        modus-themes-bold-constructs t
                        modus-themes-italic-constructs t)
                  (require-theme 'modus-themes)
                  (set-face-attribute 'modus-themes-heading-1 nil :height 1.25))
              (if (equal (cadr themes_chosen) 'manoj-dark)
                  (progn
                    (load-theme (car (cdr themes_chosen)) t)
                    (set-face-foreground 'hl-line 'unspecified)
                    (set-face-background 'fringe 'unspecified))
                ))
          (progn
            ;; (load-theme (car themes_chosen) t)
            (when (eq custom-enabled-themes nil)
              ;; (set-face-bold 'font-lock-keyword-face t)
              ;; (set-face-bold 'font-lock-builtin-face t)
              (set-face-background 'highlight "#DFEAEC")
              (set-face-background 'fringe 'unspecified)
              (set-face-attribute 'line-number-current-line nil :foreground
                                  "#000000" :background "#C4C4C4" :weight
                                  'bold)
              (set-face-bold 'font-lock-keyword-face 't)
              )
            (setq modus-themes-org-blocks 'gray-background
                  modus-themes-bold-constructs t
                  modus-themes-italic-constructs t)))
      (if (or
           (>= (string-to-number (substring (current-time-string) 11 13)) dayoff)
           (<= (string-to-number (substring (current-time-string) 11 13)) dayon))
          (standard-themes-select 'standard-dark)
        (standard-themes-select 'standard-light)))
    (when modelineq
      (if (equal (frame-parameter nil 'background-mode) 'dark)
          (set-face-attribute 'mode-line nil
                              :background "black"
                              :box nil
                              :font (font-spec
                                     :name
                                     code-font
                                     :size
                                     11.0)
                              :underline
                              (face-foreground 'mode-line-emphasis))
        (progn
          (set-face-attribute 'mode-line nil
                              :background "#F4F7FA"
                              ;; :background "white"
                              :box nil
                              :font (font-spec
                                     :name
                                     code-font
                                     :size 11.0))
          ))
      (set-face-attribute
       'mode-line-inactive nil
       :inherit 'mode-line
       :box nil))
    ))

(provide 'init-func)
;;; init-func.el ends here.
