;;; init-startup --- Init for Startup  -*- lexical-binding: t; -*-
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

(defvar emacs-startup-icon-position 4
  "Position for Emacs Startup's icon.")

(defvar emacs-startup-space 20
  "Spaces for Emacs Startup.")

(defun initial-startup-screen ()
  "Initial startup buffer."
  (with-current-buffer "*scratch*"
    (rename-buffer "*Emacs*")
    (fundamental-mode)
    (erase-buffer)
    ;; (when (bound-and-true-p meow-mode)
    ;;   (meow-insert-mode))
    ;; (keymap-local-set "h" 'backward-char)
    ;; (keymap-local-set "l" 'forward-char)
    ;; (keymap-local-set "j" 'next-line)
    ;; (keymap-local-set "k" 'previous-line)
    ;; (keymap-local-set "s" 'scratch-buffer)
    ;; (keymap-local-set "f" 'find-file)
    ;; (if (functionp 'consult-buffer)
    ;;     (keymap-local-set "b" 'consult-buffer)
    ;;   (keymap-local-set "b" 'switch-to-buffer))
    ;; (keymap-local-set "d"
    ;;                   (lambda ()
    ;;                     (interactive)
    ;;                     (kill-this-buffer)
    ;;                     (if (functionp 'consult-buffer)
    ;;                         (consult-buffer)
    ;;                       (call-interactively 'switch-to-buffer))))
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
      (insert (concat
               (make-string 18 ? )
               (make-string emacs-startup-icon-position ?─)
               " "
               (nerd-icons-octicon "nf-oct-history")
               " "
               (propertize "Recent Files " 'face 'bold 'display '(space-width 0.5))
               (make-string
                (- fill-column emacs-startup-icon-position 50)
                ?─)
               )
              "\n")
      ;; (add-text-properties (- (line-beginning-position) fill-column 1)
      ;;                      (point)
      ;;                      '(line-height 1.5 line-spacing 0.3))
      (add-text-properties (- (line-beginning-position) (- fill-column 30))
                           (point)
                           '(line-height 1.5 line-spacing 0.3))
      (seq-do
       (lambda (c)
         (let* ((time (car c))
                (file (cdr c))
                (len (length (concat time file))))
           (insert (propertize (concat
                                (format (concat
                                         (make-string emacs-startup-space ? )
                                         "%s ")
                                        ;; (nerd-icons-icon-for-file file)
                                        (if (directory-name-p file)
                                            (nerd-icons-icon-for-dir file)
                                          (nerd-icons-icon-for-file file)))
                                (propertize (if (and (< len (- fill-column 18))
                                                     (< (length file) 31))
                                                file
                                              (concat (truncate-string-to-width file 28)
                                                      "..."))
                                            ;; 'follow-link t
                                            'face
                                            '(:inherit link :underline nil))
                                (concat (propertize " " 'display
                                                    `(space :align-to ,(- fill-column
                                                                          (length time)
                                                                          emacs-startup-space)))
                                        (propertize time 'face '(:inherit font-lock-keyword-face)
                                                    'pointer 'arrow
                                                    'help-echo
                                                    (format-time-string
                                                     "%Y-%m-%d %T"
                                                     (file-attribute-modification-time (file-attributes file)))
                                                    ))
                                (make-string emacs-startup-space ? )
                                )
                               ;; 'mouse-face `(:background ,(face-background 'highlight)
                               ;;                           :foreground nil)
                               'mouse-face 'highlight
                               'cursor nil
                               'help-echo file
                               'follow-link nil
                               'keymap
                               (define-keymap
                                 "<return>"
                                 (lambda ()
                                   (interactive)
                                   (find-file (get-text-property (point) 'help-echo)))
                                 "<mouse-1>"
                                 ;; #'scratch-open
                                 (lambda (e)
                                   ;; (interactive (list last-nonmenu-event))
                                   (interactive "e")
                                   (find-file (get-text-property
                                               (posn-point (event-start e))
                                               'help-echo))
                                   )
                                 )
                               ))
           (insert "\n")))
       file-time-list))
    (setq buffer-read-only t))
  (remove-hook 'emacs-startup-hook 'initial-startup-screen)
  (prefer-coding-system 'gbk)
  (prefer-coding-system 'utf-8))

(add-hook 'emacs-startup-hook 'initial-startup-screen)

(provide 'init-startup)
;;; init-startup.el ends here.
