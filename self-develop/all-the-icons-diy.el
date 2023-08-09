;;;###autoload
(defun target-all-the-icons-icon-for-mode (mode &rest arg-overrides)
  "Similar to Func all-the-icons-icon-for-mode."
  (let* ((icon (cdr (or (assoc mode all-the-icons-mode-icon-alist)
  						(assoc (get mode 'derived-mode-parent) all-the-icons-mode-icon-alist))))
  		 (args (cdr icon)))
    (when arg-overrides (if (member :face arg-overrides)
  							(if (member :foreground (nth 1 (member :face arg-overrides)))
  								(setq args (append `(,(car args)) arg-overrides (seq-subseq args 0 (- (safe-length args) 2))))
  							  (progn
  								(let* ((args-temp (member :face arg-overrides))
  									   (args-rest (car (cdr args-temp))))
  								  (setq arg-overrides (butlast arg-overrides (safe-length args-temp)))
  								  (setq args-temp (nthcdr 2 args-temp))
  								  (if args-temp
  									  (progn
  										(setq args-rest (append (append args-rest (list :foreground)) (list (face-foreground (nth (- (safe-length icon) 1) icon)))))
  										(setq args-temp (append (list args-rest) args-temp))
  										)
  									(setq args-temp (list (append (append args-rest (list :foreground)) (list (face-foreground (nth (- (safe-length icon) 1) icon))))))
  									)
  								  (setq arg-overrides (append arg-overrides '(:face) args-temp))
  								  )
  								(setq args (append `(,(car args)) arg-overrides (butlast (cdr args) 2))))
  							  )
  						  (setq args (append `(,(car args)) arg-overrides (cdr args)))
  						  ))
    (if icon (apply (car icon) args) mode)))

;;;###autoload
(defun target-all-the-icons-icon-for-file (file &rest arg-overrides)
  "Get the formatted icon for FILE.
	   ARG-OVERRIDES should be a plist containining `:height',
	   `:v-adjust' or `:face' properties like in the normal icon
	   inserting functions."
  (let* ((ext (file-name-extension file))
		 (icon (or (all-the-icons-match-to-alist file all-the-icons-regexp-icon-alist)
				   (and ext
						(cdr (assoc (downcase ext)
									all-the-icons-extension-icon-alist)))
				   all-the-icons-default-file-icon))
		 (args (cdr icon)))
	(when arg-overrides (if (member :face arg-overrides)
							(if (member :foreground (nth 1 (member :face arg-overrides)))
								(setq args (append `(,(car args)) arg-overrides (seq-subseq args 0 (- (safe-length args) 2))))
							  (progn
								(let* ((args-temp (member :face arg-overrides))
									   (args-rest (car (cdr args-temp))))
								  (setq arg-overrides (butlast arg-overrides (safe-length args-temp)))
								  (setq args-temp (nthcdr 2 args-temp))
								  (if args-temp
									  (progn
										(setq args-rest (append (append args-rest (list :foreground)) (list (face-foreground (nth (- (safe-length icon) 1) icon)))))
										(setq args-temp (append (list args-rest) args-temp))
										)
									(setq args-temp (list (append (append args-rest (list :foreground)) (list (face-foreground (nth (- (safe-length icon) 1) icon))))))
									)
								  (setq arg-overrides (append arg-overrides '(:face) args-temp))
								  )
								(setq args (append `(,(car args)) arg-overrides (butlast (cdr args) 2))))
							  )
						  (setq args (append `(,(car args)) arg-overrides (cdr args)))
						  ))
	(apply (car icon) args)))
