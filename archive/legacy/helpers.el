;;; helpers.el --- Retired helpers; do not load at startup -*- lexical-binding: t; -*-

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and
         (not test?) ;; for check lsp-server-present?
         (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
         lsp-use-plists
         (not (functionp 'json-rpc-connection)) ;; native json-rpc
         (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))


;;; ============================================================================
;;; 07 GC and Performance
;;; ============================================================================

;;; GC / performance

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


;;;###autoload
(defun set-en_cn-font_old (en-font cn-font serif-font sans-font verbatim-font en-size cn-size)
  "EN-FONT, CN-FONT mean font-family.  EN-SIZE, CN-SIZE mean font size.
And Set SERIF-FONT, SANS-FONT and VERBATIM-FONT."
  (set-face-attribute 'default nil
                      :font
                      (font-spec
                       :name en-font
                       :weight 'regular
                       ;; :slant 'normal
                       :size en-size))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (if (equal en-size cn-size)
        (set-fontset-font "fontset-default" charset (font-spec :family cn-font))
      (set-fontset-font "fontset-default" charset (font-spec
                                                   :family cn-font
                                                   :size cn-size))))
  (create-fontset-from-fontset-spec
   (font-xlfd-name (font-spec
                    :family en-font
                    :registry "fontset-variable pitch verbatim")))
  (set-fontset-font "fontset-variable pitch verbatim" 'han (font-spec :family verbatim-font))
  (set-fontset-font "fontset-variable pitch verbatim" 'cjk-misc (font-spec :family verbatim-font))
  (dolist (sp
           `(("regular" . ,cn-font)
             ("italic" . ,sans-font)))
    (let ((registry (concat "fontset-variable pitch " (car sp))))
      (create-fontset-from-fontset-spec
       (font-xlfd-name (font-spec
                        :family serif-font
                        :registry registry)))
      (if (equal en-size cn-size)
          (progn
            (set-fontset-font registry 'han (font-spec :family (cdr sp)))
            (set-fontset-font registry 'cjk-misc (font-spec :family (cdr sp))))
        (progn
          (set-fontset-font registry 'han (font-spec
                                           :family (cdr sp)
                                           :size cn-size))
          (set-fontset-font registry 'cjk-misc (font-spec
                                                :family (cdr sp)
                                                :size cn-size))))))
  (with-eval-after-load 'org
    (set-face-attribute 'variable-pitch nil
                        :family serif-font
                        :fontset "fontset-variable pitch regular")
    (set-face-attribute 'fixed-pitch nil
                        :family en-font
                        ;; :fontset "fontset-variable pitch regular"
                        )
    (defface org-emphasis-italic '((default :inherit italic))
      "My italic emphasis for Org.")
    (set-face-attribute 'org-emphasis-italic nil :fontset "fontset-variable pitch italic")
    (set-face-attribute 'org-verbatim nil :fontset "fontset-variable pitch verbatim")
    (set-face-attribute 'org-code nil :fontset "fontset-variable pitch verbatim")
    (set-face-attribute 'org-block nil :fontset "fontset-variable pitch verbatim")
    (setq org-emphasis-alist
          '(("*" bold)
            ("/" org-emphasis-italic)
            ("_" underline)
            ("=" org-verbatim verbatim)
            ("~" org-code verbatim)
            ("+" (:strike-through t))))))
