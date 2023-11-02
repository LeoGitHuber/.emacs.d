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
(defun set-en_cn-font (en-font cn-font f-size)
  "EN-FONT for English, CN-FONT for Chinese, F-SIZE represents font size.
Set Font for both of English and Chinese characters."
  (set-face-attribute
   'default nil
   :font (font-spec
		  :name en-font
		  :weight 'normal
  		  :slant 'normal
  		  :size f-size))

  (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font "fontset-default" charset (font-spec :family cn-font))))

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
                               (string-prefix-p "test" d)))
               (cl-remove-if (lambda (d)
                               (file-exists-p (expand-file-name ".nosearch"
                                                                d))))))

(defun find-dir-recursively (dir)
  "Find all `.el' files in DIR and its subdirectories."
  (let ((subdir (find-subdir-recursively dir)))
    (nconc subdir
           (mapcan #'find-dir-recursively subdir))))

(defvar autoloads-file "/home/Leo/.emacs.d/site-lisp/loaddefs.el"
  "File with all of autoload setting.")

(defvar site-lisp-directory "/home/Leo/.emacs.d/site-lisp"
  "Directory contained of all third party packages.")

(defun generate-autoloads (&optional dir target)
  "Generate autoload files recursively for all package in DIR to file TARGET.

If DIR is omitted, use `cm/site-lisp-directory' as DIR, if target is ommitted
use `cm/autoloads-file' as TARGET."
  (interactive)
  (let* ((target (or target autoloads-file))
         (dir (or dir site-lisp-directory)))
    (loaddefs-generate (find-dir-recursively dir) target nil nil nil t)))

(provide 'init-func)
;;; init-func.el ends here.
