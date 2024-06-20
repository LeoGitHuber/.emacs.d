;;; init-func --- Init for function  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

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

;;;###autoload
(defun set-en_cn-font (en-font cn-font serif-font sans-font source-font f-size)
  "EN-FONT for English, CN-FONT for Chinese, F-SIZE represents font size.
Set Font for both of English and Chinese characters."
  (set-face-attribute
   'default nil
   :font (font-spec
          :name en-font
          :weight 'regular
          ;; :slant 'normal
          :size f-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font "fontset-default" charset (font-spec :family cn-font)))
  (create-fontset-from-fontset-spec
   (font-xlfd-name
    (font-spec :family en-font
               :registry "fontset-variable pitch verbatim")))
  (set-fontset-font "fontset-variable pitch verbatim" 'han
                    (font-spec :family source-font))
  (set-fontset-font "fontset-variable pitch verbatim" 'cjk-misc
                    (font-spec :family source-font))
  (dolist (sp `(("regular" . ,cn-font)
                ("italic" . ,sans-font)
                ;; ("verbatim" . ,source-font)
		        ))
    (let ((registry (concat "fontset-variable pitch " (car sp))))
      (create-fontset-from-fontset-spec
       (font-xlfd-name
        (font-spec :family serif-font
                   :registry registry)))
      (set-fontset-font registry 'han
                        (font-spec :family (cdr sp)))
      (set-fontset-font registry 'cjk-misc
                        (font-spec :family (cdr sp)))))
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
  (add-hook 'meow-insert-exit-hook
            (lambda ()
              (and buffer-file-name
                   (save-buffer))))
  )

(defun lsp-enable-startup ()
  "Enable `eglot' or `lsp-mode' for LSP."
  (dolist (hook '(prog-mdoe-hook
                  cuda-mode-hook
                  TeX-mode-hook
                  c-ts-mode-hook c++-ts-mode-hook
                  python-ts-mode-hook python-ts-mode-hook
                  emacs-lisp-mode-hook
                  js-mode-hook
                  js-ts-mode-hook))
    (add-hook hook 'corfu-mode)
    (add-hook hook (lambda ()
                     (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode
                                             ;; 'verilog-mode
                                             'makefile-mode 'snippet-mode)
                       ;; (lsp-deferred)
                       (eglot-ensure)
                       )))))

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

(defun setup-display-graphic ()
  "Setup display graphic for GUI Emacs and Emacsclient."
  (when (display-graphic-p)
    ;; (if (not windows-system-p)
    ;; (set-en_cn-font "BlexMono Nerd Font" "Source Han Serif CN" "Input Serif"
    ;;                 "LXGW WenKai Screen" "Source Han Sans CN" 12.0)
    ;; (set-en_cn-font "InputMono" "Source Han Serif CN" "Palatino Linotyp"
    ;;                 "LXGW WenKai Screen" "Source Han Sans CN" 12.0)
    (set-en_cn-font "PragmataPro Nerd Font" "FZLTHProGBK" "Input Serif"
                    "LXGW WenKai Screen" "Source Han Sans CN" 13.0)
    ;;   )
    ;; Maple Mono NF --- Maple Mono SC NF, HarmonyOS Sans SC
    ;; PragmataPro Mono Liga --- SimHei
    ;; Hack --- HarmonyOS Sans SC
    ;; JetBrainsMono NF
    ;; "Iosevka Fixed"    ;; Input Mono
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
    (let ((mode-line-box-p (face-attribute 'mode-line-highlight :box)))
      (when (not (eq mode-line-box-p 'unspecified))
        (if (consp mode-line-box-p)
            (set-face-attribute 'child-frame-border
                                nil
                                :background (nth 3 mode-line-box-p))
          (set-face-attribute 'child-frame-border
                              nil
                              :background mode-line-box-p)
          )))
    (if (or
         (>= (string-to-number (substring (current-time-string) 11 13)) 19)
         (<= (string-to-number (substring (current-time-string) 11 13)) 6))
        (if (string-prefix-p "modus" (symbol-name (cadr themes_chosen)))
            (progn
              (setq modus-themes-org-blocks 'gray-background
                    modus-themes-bold-constructs t
                    modus-themes-italic-constructs t)
              (require-theme 'modus-themes)
              (if (string-match-p "29" emacs-version)
                  (load-theme 'modus-vivendi)
                (load-theme (car (cdr themes_chosen)) t))
              (set-face-attribute 'modus-themes-heading-1 nil :height 1.25))
          (if (equal (cadr themes_chosen) 'manoj-dark)
              (progn
                (load-theme (car (cdr themes_chosen)) t)
                (set-face-foreground 'hl-line 'unspecified)
                (set-face-background 'fringe 'unspecified))
            (if (string-match "ef-" (symbol-name (cadr themes_chosen)))
                (ef-themes-select-dark (cadr themes_chosen))
              (progn
                (load-theme (cadr themes_chosen) t)
                (setq doom-rouge-brighter-comments t
                      doom-rouge-brighter-tabs t))
              )))
      (progn
        ;; (load-theme (car themes_chosen) t)
        ;; (ef-themes-select-light (car themes_chosen))
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
    (if (equal (frame-parameter nil 'background-mode) 'dark)
        (set-face-attribute 'mode-line nil
                            :background "black"
                            :box nil
                            :font (font-spec
                                   ;; "JetBrainsMono NF" "Monego Ligatures" "Maple Mono NF"
                                   :name
                                   "BlexMono Nerd Font"
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
                                   "BlexMono Nerd Font"
                                   :size 11.0))
        ))
    (set-face-attribute
     'mode-line-inactive nil
     :inherit 'mode-line
     :box nil)
    ))

(provide 'init-func)
;;; init-func.el ends here.
