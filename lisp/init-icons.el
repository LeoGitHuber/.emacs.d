;;; init-icons --- Init for icons
;;; Commentary:
;;; Code:

(defvar mine-emacs-logo
  (propertize
   "███████╗███╗░░░███╗░█████╗░░█████╗░░██████╗
██╔════╝████╗░████║██╔══██╗██╔══██╗██╔════╝
█████╗░░██╔████╔██║███████║██║░░╚═╝╚█████╗░
██╔══╝░░██║╚██╔╝██║██╔══██║██║░░██╗░╚═══██╗
███████╗██║░╚═╝░██║██║░░██║╚█████╔╝██████╔╝
╚══════╝╚═╝░░░░░╚═╝╚═╝░░╚═╝░╚════╝░╚═════╝░"
   'face '(:inherit font-lock-string-face))
  "ASCII Art logo for EMACS.")

(defun initial-startup-screen ()
  "Initial startup buffer."
  (with-current-buffer "*scratch*"
    (rename-buffer "*Emacs*")
    (fundamental-mode)
    (erase-buffer)
    ;; (display-line-numbers-mode -1)
    (visual-fill-column-mode)
    (auto-save-mode -1)
    (newline 4)
    (insert (propertize mine-emacs-logo))
    (let ((final-line (line-number-at-pos))
          (space-num
           (format (concat "%"
                           (number-to-string
                            (/ (- fill-column
                                  (- (line-end-position)
                                     (line-beginning-position))
                                  2)
                               2))
                           "s")
                   " ")))
      (goto-char (point-min))
      (while (> final-line 0)
        (insert space-num)
        (forward-line)
        (setq final-line (- final-line 1))))
    (newline 1)
    (let* ((rec_file_list file-name-history)
           ;; (rec_file_list (recentf-time-sort))
           (top-n (if (> (length rec_file_list) 5) 5 (length rec_file_list)))
           (file-time-list
            (mapcar (lambda (f)
                      ;; Copy from `marginalia--time' function
                      (let ((time (file-attribute-modification-time (file-attributes f)))
                            (time--relative
                             '((100 "sec" 1) (6000 "min" 60.0) (108000 "hour" 3600.0)
                               (34560000 "day" 86400.0) (nil "year" 31557600.0))))
                        (if (< (float-time (time-since time)) 1209600)
                            (progn
                              (setq time (max 0 (float-time (time-since time))))
                              (let ((sts time--relative) here)
                                (while (and (car (setq here (pop sts)))
                                            (<= (car here) time)))
                                (setq time (round time (caddr here)))
                                (cons (format "%s %s%s ago" time (cadr here) (if (= time 1) "" "s")) f)))
                          (let ((system-time-locale "C"))
                            (cons
                             (format-time-string
                              (if (> (decoded-time-year (decode-time (current-time)))
                                     (decoded-time-year (decode-time time)))
                                  " %Y %b %d"
                                "%b %d %H:%M")
                              time)
                             f)))))
                    (butlast rec_file_list (- (length rec_file_list) top-n)))))
      (insert (concat (make-string 8 ?─)
                      " "
                      (nerd-icons-octicon "nf-oct-history")
                      " "
                      (propertize "Recent Files " 'face 'bold 'display '(space-width 0.5))
                      (make-string 47 ?─))
              "\n")
      (add-text-properties (- (line-beginning-position) fill-column 1)
                           (point)
                           '(line-height 1.5 line-spacing 0.3))
      (seq-do
       (lambda (c)
         (let* ((time (car c))
                (file (cdr c))
                (len (length (concat time file))))
           (insert (propertize (concat
                                (format "%8s%s " " "
                                        (nerd-icons-icon-for-file file))
                                (propertize (if (and (< len (- fill-column 18))
                                                     (< (length file) 31))
                                                file
                                              (concat (truncate-string-to-width file 28)
                                                      "..."))
                                            'follow-link 't
                                            'face '(:inherit link :underline nil))
                                (concat (propertize " " 'display
                                                    `(space :align-to ,(- fill-column
                                                                          (length time)
                                                                          8)))
                                        (propertize time 'face '(:inherit font-lock-keyword-face)
                                                    'pointer 'arrow
                                                    'help-echo
                                                    (format-time-string
                                                     "%Y-%m-%d %T"
                                                     (file-attribute-modification-time (file-attributes file)))
                                                    ))
                                (format "%8s" " "))
                               ;; 'mouse-face `(:background ,(face-background 'highlight)
                               ;;                           :foreground nil)
                               'mouse-face 'highlight
                               'help-echo file
                               'keymap
                               (let ((map (make-sparse-keymap)))
                                 (define-key map (kbd "RET")
                                             '(lambda ()
                                                (interactive)
                                                (find-file (get-text-property (point) 'help-echo))))
                                 map)
                               ))
           (insert "\n")))
       file-time-list))
    (setq buffer-read-only t))
  (remove-hook 'emacs-startup-hook 'initial-startup-screen))

(add-hook 'emacs-startup-hook 'initial-startup-screen)

(add-to-list 'load-path "~/.emacs.d/site-lisp/nerd-icons.el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/treemacs-nerd-icons")
(add-to-list 'load-path "~/.emacs.d/site-lisp/nerd-icons-dired")

(setq nerd-icons-font-family "JetBrainsMono Nerd Font")

(require 'nerd-icons)

;; (load "~/.emacs.d/self-develop/modeline-setting.el")

(add-hook 'dired-mode-hook 'nerd-icons-dired-mode)

(with-eval-after-load 'treemacs
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))

(add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode)

;; (require 'all-the-icons)
(with-eval-after-load 'all-the-icons (load "~/.emacs.d/self-develop/all-the-icons-diy.el"))

;; (with-eval-after-load 'nerd-icons
;;   (setq fc-info (nerd-icons-codicon "nf-cod-question" :face '(:inherit flycheck-info-my))
;; 		fc-warning (nerd-icons-codicon "nf-cod-warning" :face '(:inherit flycheck-warn))
;; 		fc-error (nerd-icons-codicon "nf-cod-error" :face '(:inherit flycheck-error-my)))
;;   )

(with-eval-after-load 'tab-bar
  (set-face-attribute 'tab-bar-tab nil :slant 'italic :weight 'bold)
  (set-face-attribute 'tab-bar-tab-inactive nil :slant 'normal :weight 'normal)
  (defun tab-bar-tab-name-format-face (name tab _i)
    "Apply the face to the tab name.
It uses the function `tab-bar-tab-face-function'."
    (add-face-text-property
     0 (length name) (funcall tab-bar-tab-face-function tab) t name)
    name)
  (defvar tab-bar-name-hints-format " %d. "
    "Set format for tab-bar-name-hints.")
  (defface tab-bar-name-hints-face
    '((t :slant italic))
    "Face for tab-bar-hints."
    :group 'tab-bar-faces)
  (defun tab-bar-tab-name-format-hints (name _tab i)
    "Show absolute numbers on tabs in the tab bar before the tab name.
It has effect when `tab-bar-tab-hints' is non-nil."
    (if tab-bar-tab-hints
        (concat
         (let ((i (format tab-bar-name-hints-format i)))
           (add-face-text-property 0 (length i) 'tab-bar-name-hints-face t i)
           i)
         name)
      name))
  (defun tab-bar-tab-name-format-close-button (name tab _i)
    "Show the tab close button.
The variable `tab-bar-close-button-show' defines when to show it."
    (if (and tab-bar-close-button-show
             (not (eq tab-bar-close-button-show
                      (if (eq (car tab) 'current-tab) 'non-selected 'selected)))
             tab-bar-close-button)
        (concat name tab-bar-close-button)
      name))
  (setq tab-bar-close-button
        ;; (propertize " 󰅖 " :help-echo "Close tab"
        ;;             'close-tab t
        ;;             'mouse-face 'highlight
        ;;             'pointer 'vdrag
        ;;             )
        (propertize " ✗ " :help-echo "Close tab"
                    'slant 'italic
                    'close-tab t
                    'mouse-face 'highlight
                    'pointer 'vdrag
                    )
        ;; (propertize "  " :help-echo "Close tab"
        ;;             'close-tab t
        ;;             'mouse-face 'highlight
        ;;             ;; 'pointer 'vdrag
        ;;             )
        tab-bar-separator
        (propertize " " 'face '(:inherit 'variable-pitch))
        tab-bar-auto-width nil
        tab-bar-tab-name-truncated-max 25
        tab-bar-tab-hints t
        tab-bar-new-button-show nil)
  ;; modified_icon = '●'
  ;; close_icon = ''
  )

(provide 'init-icons)
;;; init-icons.el ends here.
