;;; package --- Sumary
;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun emacs-nf-diy-for-mode (mode &rest arg-overrides)
  "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (cdr (or (assoc mode nerd-icons-mode-icon-alist)
						(assoc (get mode 'derived-mode-parent) nerd-icons-mode-icon-alist))))
		 (args (cdr icon)))
	(when arg-overrides
	  (setq args (append
				  `(,(car args))
				  (cons (car arg-overrides)
						`(,(append (car (cdr arg-overrides))
								   '(:foreground)
								   `(,(face-attribute (car (nthcdr 2 args)) :foreground))))
						))))
	(if icon (apply (car icon) args) mode)))

(defun emacs-nf-diy-for-file (file &rest arg-overrides)
  "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((ext (file-name-extension file))
         (icon (or (nerd-icons-match-to-alist file nerd-icons-regexp-icon-alist)
                   (and ext
                        (cdr (assoc (downcase ext)
                                    nerd-icons-extension-icon-alist)))
                   nerd-icons-default-file-icon))
         (args (cdr icon)))
    ;; (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
	(when arg-overrides
	  (setq args (append
				  `(,(car args))
				  (cons (car arg-overrides)
						`(,(append (car (cdr arg-overrides))
								   '(:foreground)
								   `(,(face-attribute (car (nthcdr 2 args)) :foreground))))
						))))
    (apply (car icon) args)))

(defun ml-fill-space (right-format)
  "Return empty space on the right with RIGHT-FORMAT.
Using FACE and leaving RESERVE space."
  (propertize " "
  			  ;; 'display `((space :align-to (- (+ right right-fringe right-margin),(/ (string-pixel-width (format-mode-line right-format)) (window-font-width nil 'mode-line) 1.0))))
  			  'display `((space :align-to (- (+ right right-fringe right-margin),(/ (string-pixel-width (format-mode-line right-format)) 10.1 1.0))))
  			  ;; 'display `((space :align-to (- right, (string-width (format-mode-line right-format)))))
  			  ;; 'display `((space :align-to (- (+ right right-fringe right-margin)
  			  ;; 							 (,(string-pixel-width (format-mode-line right-format))))))
  			  ))

(provide 'emacs-nf-diy)
;;; emacs-nf-diy.el ends here
