;;; init-input --- Init for input method
;;; Commentary:
;;; Code:

;; Input Method
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-rime")

(with-eval-after-load 'rime
  (set-face-attribute 'rime-default-face nil :height 1.2)
  (set-face-attribute 'rime-highlight-candidate-face nil :height 1.2)
  (set-face-attribute 'rime-preedit-face nil :underline t
                      :inverse-video 'unspecified)
  (defun rime-predicate-meow-mode-p ()
    "Detect whether the current buffer is in `meow' state.

Include `meow-normal-state' , `meow-motion-state'.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
    (and (fboundp 'meow-mode)
         (meow-normal-mode-p)
         ))
  (defun rime-predicate-tex-advance-p ()
    "If point is inside a (La)TeX math environment, or a (La)TeX command."
    (if (derived-mode-p 'tex-mode)
        (or (and (featurep 'tex-site)
                 (texmathp))
            (and rime--current-input-key
                 (or (= #x24 rime--current-input-key)
                     (= #x5c rime--current-input-key))
                 (or (= (point) (line-beginning-position))
                     (= #x20 (char-before))
                     (rime-predicate-after-ascii-char-p)))
            (and (> (point) (save-excursion (back-to-indentation) (point)))
                 (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
                   (or (or (string-match-p "[\x5c][\x21-\x24\x26-\x7a\x7c\x7e]*$" string)
                           (if (string-match-p "[\x5c][a-zA-Z\x23\x40]+[\x7b][^\x7d\x25]*$" string)
                               (if (and (string-match-p "[\x5c]\\(begin\\)\\|\\(end\\)[\x7b]" string)
                                        (= (char-before) #x7b))
                                   t
                                 (if (> (char-before) #x7b)
                                     (and rime--current-input-key
                                          (or (= #x7e rime--current-input-key)
                                              (= #x7c rime--current-input-key)))
                                   ())
                                 ())))
                       (string-match-p "[a-zA-Z][0-9\x21-\x23\x25-\x2f\x3a-\x40\x5b-\x60\x7a\x7c\7e\x7f]*$" string)
                       ))))
      (rime-predicate-after-ascii-char-p)))
  (keymap-set rime-mode-map "s-`" 'rime-send-keybinding)
  ;; (define-key rime-mode-map (kbd "Shift") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "C-t") 'rime-inline-ascii)
  (define-key minibuffer-mode-map (kbd "C-t") 'rime-inline-ascii)
  (setq default-input-method "rime"
		rime-user-data-dir "~/.local/share/fcitx5/rime"    ;; "~/.emacs.d/rime/"
		rime-show-candidate 'posframe
		rime-show-preedit 't
		rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "C-h")
		rime-posframe-properties (list :internal-border-width 3)
		rime-posframe-style 'vertical
		;; (list :font "Source Han Serif SC"
		;; :background-color "#333333"
		;; :internal-border-width 10)
		rime-disable-predicates
		'(rime-predicate-space-after-cc-p
		  rime-predicate-current-uppercase-letter-p
		  ;; rime-predicate-after-alphabet-char-p
          ;; rime-predicate-after-ascii-char-p
		  rime-predicate-prog-in-code-p
		  rime-predicate-hydra-p
		  ;; rime-predicate-evil-mode-p
          rime-predicate-meow-mode-p
          rime-predicate-tex-advance-p
		  )
		;; rime-deactivate-when-exit-minibuffer t
		rime-inline-ascii-trigger 'shift-l
		)
  (keymap-set rime-mode-map "M-y" 'rime-force-enable)
  ;; (with-eval-after-load 'tex
  ;;   (add-to-list 'rime-disable-predicates
  ;;                'rime-predicate-tex-advance-p))
  )

;; (keymap-global-set "C-\\" 'rime-commit-and-toggle-input-method)
(defun rime-commit1-and-toggle-input-method ()
  "Commit the 1st item if exists, then toggle input method."
  (interactive)
  (require 'rime)
  (ignore-errors (rime-commit1))
  (toggle-input-method))

(keymap-global-set "C-\\" 'rime-commit1-and-toggle-input-method)

;; (require 'pyim-wbdict)
;; (require 'pyim)
;; (setq default-input-method "pyim")
;; (setq pyim-page-length 7)
;; (setq pyim-page-posframe-border-width 3)
;; (pyim-default-scheme 'wubi)
;; (setq pyim-page-tooltip 'posframe)
;; (if (string-equal (symbol-name (car custom-enabled-themes)) "modus-operandi")
;; (progn
;; (set-face-attribute 'pyim-page nil :inherit 'default :background "#EEE1B3" :foreground "#000000")
;; (set-face-attribute 'pyim-page-border nil :inherit 'pyim-page :background "#000000"))
;; (set-face-attribute 'pyim-page-border nil :inherit 'pyim-page :background "#D7DCC8"))
;;
;; ;; (pyim-wbdict-v86-single-enable)
;; (pyim-wbdict-v98-morphe-enable)
;; (setq-default pyim-english-input-switch-functions
;; '(pyim-probe-isearch-mode
;; pyim-probe-dynamic-english
;; pyim-probe-programe-mode
;; pyim-probe-org-structure-template))
;; (global-set-key "\C-\\" 'toggle-input-method)
;; (global-set-key "\M-i" #'pyim-convert-string-at-point)

;; (global-set-key "\M-p" 'pyim-process-toggle-input-ascii)
;; (global-set-key "\M-j" 'pyim-toggle-input-ascii)

(provide 'init-input)
;;; init-input.el ends here.
