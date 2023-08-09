;;; package --- Sumary
;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(load "~/.emacs.d/self-develop/emacs-nf-diy.el")

(defgroup mode-line-personal nil
  "Personal Mode Line Setting."
  :prefix "mode-line-area-"
  :group 'basic-faces
  :group 'appearance)

(defface buffer-status
  '((default :font "JetBrainsMono Nerd Font-12")
	(((background dark)) :foreground "#9CC0E8")
    (((background light)) :foreground "#194374"))
  "Face for foreground of buffer status.")

(defface mode-line-area-0
  '((((background dark)) :background "#665c54")
    (((background light)) :background "#ebdbb2"))
  "Face for default background of personal mode line."
  :group 'mode-line-personal)

(defface mode-line-area-1
  '(
	;; (((background dark)) :background "#83a598" :foreground "#1d2021")
	;; (((background light)) :background "#076678" :foreground "#f9f5d7")
	(((background dark)) :background "#1C2F45" :foreground "#589EDD")
	(((background light)) :background "#CEE1F8" :foreground "#20396A"))
  "Face for personal mode line setting area 1."
  :group 'mode-line-personal)

(defface mode-line-area-1-separator-2
  '(
	(((background dark)) :foreground "#1C2F45" :inherit mode-line-area-2)
	(((background light)) :foreground "#CEE1F8" :inherit mode-line-area-2))
  "Face for personal mode line setting area 1."
  :group 'mode-line-personal)

(defface mode-line-area-1-separator-3
  '(
	(((background dark)) :foreground "#1C2F45" :inherit mode-line-area-3)
	(((background light)) :foreground "#CEE1F8" :inherit mode-line-area-3))
  "Face for personal mode line setting area 1."
  :group 'mode-line-personal)

(defface mode-line-area-2
  '(
	;; (((background dark)) :background "#504945" :foreground "#d5c4a1")
	;; (((background dark)) :background "#1C2F45" :foreground "#589EDD")
	(((background dark)) :background "#244032" :foreground "#4CCA64")
    ;; (((background light)) :background "#bdae93" :foreground "#665c54")
	(((background light)) :background "#D4F8DB" :foreground "#809E3E"))
  "Face for personal mode line setting area 2."
  :group 'mode-line-personal)

(defface mode-line-area-2-separator
  '(
	(((background dark)) :foreground "#244032" :background "#341A00")
	(((background light)) :foreground "#D4F8DB" :inherit 'mode-line))
  "Face for personal mode line setting area 2."
  :group 'mode-line-personal)

(defface mode-line-area-3
  '(
	;; (((background dark)) :background "#3c3836" :foreground "#ebdbb2")
	;; (((background dark)) :background "#244032" :foreground "#4CCA64")
	(((background dark)) :background "#341A00" :foreground "#E3B341")
	;; (((background light)) :background "#d5c4a1" :foreground "#504945")
	;; (((background light)) :background "#FFF5B1" :foreground "#CDA71C")
	(((background light)) :foreground "#1A478D" :inherit 'mode-line))
  "Face for personal mode line setting area 3."
  :group 'mode-line-personal)

(with-eval-after-load 'rime
  (set-face-attribute 'rime-indicator-face nil
					  :font (font-spec
							 :name "JetBrainsMono Nerd Font"
							 :size 11.0))
  (defface input-method-indicator-cn-face
	'(
	  (((background dark)) :foreground "#d65d0e" :inherit rime-indicator-face)
      (((background light)) :foreground "#af3a03" :inherit rime-indicator-face))
	"Face for personal mode line setting area 2."
	:group 'mode-line-personal))

(defface input-method-indicator-en-face
  '(
	(((background dark)) :foreground "#b16286" :inherit mode-line)
    (((background light)) :foreground "#8f3f71" :inherit mode-line))
  "Face for personal mode line setting area 2."
  :group 'mode-line-personal)

(defface mode-line-area-4
  '((((background dark)) :foreground "#d5c4a1")
    (((background light)) :foreground "#504945"))
  "Face fore personal mode line setting area 5."
  :group 'mode-line-personal)

(defface mode-line-area-5
  '((((background dark)) :background "#8ec07c" :foreground "#1d2021")
    ;; (((background light)) :background "#427b58" :foreground "#f9f5d7")
	(((background light)) :background "#0969DA" :foreground "#E1FDFF")
	)
  "Face for personal mode line setting area 5."
  :group 'mode-line-personal)

(defface mode-line-seperator
  '((((background dark)) :foreground "#83a598")
    (((background light)) :foreground "#076678"))
  "Face for personal mode line setting separator."
  :group 'mode-line-personal)

(defgroup flycheck-mode-line nil
  "Personal Mode Line Setting."
  :prefix "flycheck-"
  :group 'basic-faces
  :group 'appearance)

(defface flycheck-error-my
  '(
	(((background dark)) :background "#090c10" :foreground "#f85149")
    ;; (((background light)) :foreground "#cb2431" :background "#f6f8fa")
	(((background light)) :foreground "#cb2431")
	)
  "Face for flycheck Error."
  :group 'flycheck-mode-line
  )

(defface flycheck-warn
  '(
	(((background dark)) :background "#090c10" :foreground "#f0883e")
    ;; (((background light)) :foreground "#bf8803" :background "#f6f8fa")
	(((background light)) :foreground "#bf8803")
	)
  "Face for flycheck Warn."
  :group 'flycheck-mode-line
  )

(defface flycheck-info-my
  '((((background dark)) :background "#090c10" :foreground "#75beff")
    ;; (((background light)) :foreground "#75beff" :background "#f6f8fa")
	(((background light)) :foreground "#75beff")
	)
  "Face for flycheck Info."
  :group 'flycheck-mode-line
  )

(defvar sys-type
  (let ((st (format "%s" system-type)))
	(cond ((string-equal "gnu" st) (nerd-icons-devicon "nf-dev-gnu" :face `(:inherit mode-line-area-4)))
		  ((string-match "linux" st) (nerd-icons-devicon "nf-dev-linux" :face `(:inherit mode-line-area-4)))
		  ((string-match "ms-dos" st) (nerd-icons-devicon "nf-dev-windows" :face `(:inherit mode-line-area-4)))
		  ((string-match "darwin" st) (nerd-icons-devicon "nf-dev-apple" :face `(:inherit mode-line-area-4)))))
  "Get the icon for current `system-type'.")

(defvar mode-line-sep
  (nerd-icons-powerline "nf-pl-left_hard_divider")
  "Set the separator icon for mode-line area.")

(defvar fc-info
  (nerd-icons-codicon "nf-cod-question" :face '(:inherit flycheck-info-my))
  "Set icon for Flycheck Info.")

(defvar fc-warning
  (nerd-icons-codicon "nf-cod-warning" :face '(:inherit flycheck-warn))
  "Set icon for Flycheck Waining.")

(defvar fc-error
  (nerd-icons-codicon "nf-cod-error" :face '(:inherit flycheck-error-my))
  "Set icon for Flycheck Errors.")

(defvar Read-only
  ;; (propertize (nerd-icons-faicon "nf-fa-minus_square") 'face '(:inherit buffer-status))
  (nerd-icons-faicon "nf-fa-minus_square" :face `(:inherit buffer-status))
  "Set icon for read-only buffer.")

  (defvar Buffer-modified
	;; (propertize (nerd-icons-faicon "nf-fa-plus_square") 'face '(:inherit buffer-status))
	(nerd-icons-faicon "nf-fa-plus_square" :face `(:inherit buffer-status))
	"Set icon for modified buffer.")

;; (defvar winum-list
;;   (list (cons 0 (propertize "🄌" 'face '(:inherit mode-line-area-1)))
;; 		(cons 1 (nerd-icons-mdicon "nf-md-numeric_1_circle" :face '(:inherit mode-line-area-1)))
;; 		(cons 2 (nerd-icons-mdicon "nf-md-numeric_2_circle" :face '(:inherit mode-line-area-1)))
;; 		(cons 3 (nerd-icons-mdicon "nf-md-numeric_3_circle" :face '(:inherit mode-line-area-1)))
;; 		(cons 4 (nerd-icons-mdicon "nf-md-numeric_4_circle" :face '(:inherit mode-line-area-1)))
;; 		(cons 5 (nerd-icons-mdicon "nf-md-numeric_5_circle" :face '(:inherit mode-line-area-1)))
;; 		(cons 6 (nerd-icons-mdicon "nf-md-numeric_6_circle" :face '(:inherit mode-line-area-1)))
;; 		(cons 7 (nerd-icons-mdicon "nf-md-numeric_7_circle" :face '(:inherit mode-line-area-1)))
;; 		(cons 8 (nerd-icons-mdicon "nf-md-numeric_8_circle" :face '(:inherit mode-line-area-1)))
;; 		(cons 9 (nerd-icons-mdicon "nf-md-numeric_9_circle" :face '(:inherit mode-line-area-1))))
;;   "Set 0-9 icons for winum mdoe.")

(defvar winum-list
  (list (cons 0 (propertize " 🄌 " 'face '(:inherit mode-line-area-1)))
		(cons 1 (concat (propertize " " 'face 'mode-line-area-1) (propertize (nerd-icons-mdicon "nf-md-numeric_1_circle" :face '(:inherit mode-line-area-1)) 'help-echo "Windows Number") (propertize " " 'face 'mode-line-area-1)))
		(cons 2 (concat (propertize " " 'face 'mode-line-area-1) (propertize (nerd-icons-mdicon "nf-md-numeric_2_circle" :face '(:inherit mode-line-area-1)) 'help-echo "Windows Number") (propertize " " 'face 'mode-line-area-1)))
		(cons 3 (concat (propertize " " 'face 'mode-line-area-1) (propertize (nerd-icons-mdicon "nf-md-numeric_3_circle" :face '(:inherit mode-line-area-1)) 'help-echo "Windows Number") (propertize " " 'face 'mode-line-area-1)))
		(cons 4 (concat (propertize " " 'face 'mode-line-area-1) (propertize (nerd-icons-mdicon "nf-md-numeric_4_circle" :face '(:inherit mode-line-area-1)) 'help-echo "Windows Number") (propertize " " 'face 'mode-line-area-1)))
		(cons 5 (concat (propertize " " 'face 'mode-line-area-1) (propertize (nerd-icons-mdicon "nf-md-numeric_5_circle" :face '(:inherit mode-line-area-1)) 'help-echo "Windows Number") (propertize " " 'face 'mode-line-area-1)))
		(cons 6 (concat (propertize " " 'face 'mode-line-area-1) (propertize (nerd-icons-mdicon "nf-md-numeric_6_circle" :face '(:inherit mode-line-area-1)) 'help-echo "Windows Number") (propertize " " 'face 'mode-line-area-1)))
		(cons 7 (concat (propertize " " 'face 'mode-line-area-1) (propertize (nerd-icons-mdicon "nf-md-numeric_7_circle" :face '(:inherit mode-line-area-1)) 'help-echo "Windows Number") (propertize " " 'face 'mode-line-area-1)))
		(cons 8 (concat (propertize " " 'face 'mode-line-area-1) (propertize (nerd-icons-mdicon "nf-md-numeric_8_circle" :face '(:inherit mode-line-area-1)) 'help-echo "Windows Number") (propertize " " 'face 'mode-line-area-1)))
		(cons 9 (concat (propertize " " 'face 'mode-line-area-1) (propertize (nerd-icons-mdicon "nf-md-numeric_9_circle" :face '(:inherit mode-line-area-1)) 'help-echo "Windows Number") (propertize " " 'face 'mode-line-area-1))))
  "Set 0-9 icons for winum mdoe.")

(setq-default
 mode-line-format
 '((:eval
	(concat
	 (when (and (bound-and-true-p winum-mode) (winum-get-number))
	   (concat (alist-get (winum-get-number) winum-list)
			   (if (buffer-file-name)
				   (propertize mode-line-sep 'face '(:inherit mode-line-area-1-separator-2))
				 (propertize mode-line-sep 'face '(:inherit mode-line-area-1-separator-3)))))

	 ;; Directory
	 (if (buffer-file-name)
		 (concat
		  (propertize (format " %s " (abbreviate-file-name (file-name-directory (buffer-file-name)))) 'face `(:inherit mode-line-area-2) 'help-echo "Current Directory")
		  (propertize mode-line-sep 'face '(:inherit mode-line-area-2-separator))
		  ))
	 (propertize " " 'face `(:inherit mode-line-area-3))

	 ;; Buffer Icon
	 (propertize (if (or (buffer-file-name) (nerd-icons-auto-mode-match?))
					 (emacs-nf-diy-for-file (buffer-name) :face `(:background ,(face-attribute 'mode-line-area-3 :background)))
				   (emacs-nf-diy-for-mode major-mode :face `(:background ,(face-attribute 'mode-line-area-3 :background)))
				   )
				 'help-echo (symbol-name (symbol-value 'major-mode)))
	 ;; Buffer Name
	 (propertize " %b " 'face `(:inherit mode-line-area-3) 'help-echo "Current Buffer Name")
	 " "
	 (cond (buffer-read-only Read-only)
		   ((buffer-modified-p) Buffer-modified))
	 (let*
		 ((right-part
		   '(
			 ;; Git
			 (:eval
			  (concat
			   (when vc-mode
				 (cond
				  ((string-match "Git[:-]" vc-mode)
				   (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
					 (concat
					  (nerd-icons-devicon "nf-dev-git")
					  " · "
					  (nerd-icons-octicon "nf-oct-git_branch")
					  (propertize (format " %s" branch) 'face `(:inherit mode-line))
					  (propertize " |" 'face `(:inherit mode-line-seperator)))))
				  ((string-match "SVN-" vc-mode)
				   (let ((revision (cadr (split-string vc-mode "-"))))
					 (concat
					  (format " %s" (nerd-icons-faicon "nf-fa-cloud"))
					  (format " · %s" revision))))
				  (t (format "%s" vc-mode))))
			   " "

			   ;; Flycheck Information
			   (pcase flycheck-last-status-change
				 (`finished
				  (if flycheck-current-errors
					  (let-alist (flycheck-count-errors flycheck-current-errors)
						(let ((i (or .info 0))
							  (w (or .warning 0))
							  (e (or .error 0)))
						  (concat
						   (propertize fc-info
									   'help-echo "Show Flycheck Error"
									   'local-map (make-mode-line-mouse-map
												   'mouse-1 (lambda() (interactive) (flycheck-list-errors))))
						   (propertize (concat " " (number-to-string i) " ")
									   'help-echo "Show Flycheck Error"
									   'local-map (make-mode-line-mouse-map
												   'mouse-1 (lambda() (interactive) (flycheck-list-errors)))
									   'face '(:inherit flycheck-info-my))
						   (propertize
							fc-warning
							'help-echo "Show Flycheck Error"
							'local-map (make-mode-line-mouse-map
										'mouse-1 (lambda() (interactive) (flycheck-list-errors))))
						   (propertize (concat  " " (number-to-string w) " ")
									   'help-echo "Show Flycheck Error"
									   'local-map (make-mode-line-mouse-map
												   'mouse-1 (lambda() (interactive) (flycheck-list-errors)))
									   'face '(:inherit flycheck-warn))
						   (propertize fc-error
									   'help-echo "Show Flycheck Error"
									   'local-map (make-mode-line-mouse-map 'mouse-1 (lambda() (interactive) (flycheck-list-errors))))
						   (propertize (concat " " (number-to-string e))
									   'help-echo "Show Flycheck Error"
									   'local-map (make-mode-line-mouse-map 'mouse-1 (lambda() (interactive) (flycheck-list-errors)))
									   'face '(:inherit flycheck-error-my))
						   (propertize " | " 'face `(:inherit mode-line-seperator)))))
					(concat (propertize "✔ No Issues")
							(propertize " | " 'face `(:inherit mode-line-seperator)))))
				 (`running (concat (propertize "⟲ Running")
								   (propertize " | " 'face `(:inherit mode-line-seperator))))
				 (`no-checker "")
				 ;; (`no-checker "⚠ No Checker")
				 (`not-checked (concat
								(propertize (nerd-icons-mdicon "nf-mdi-eye_off"))
								(propertize " Disabled")
								(propertize " | " 'face `(:inherit mode-line-seperator))))
				 ;; (`not-checked "⚠ Disabled")
				 (`errored  (concat (propertize "✖ Error" 'face `())
									(propertize " | " 'face `(:inherit mode-line-seperator))))
				 ;; (`interrupted "⛔ Interrupted")
				 (`interrupted (concat
								(propertize (nerd-icons-octicon "nf-oct-circle_slash"))
								(propertize " Interrupted")))
				 (`suspinnnncious (concat
								   (propertize (nerd-icons-faicon "nf-fa-question"))
								   (propertize " Suspicious")
								   (propertize " | " 'face `(:inherit mode-line-seperator)))))

			   ;; Input Method
			   (if (and (equal current-input-method "rime")
						(bound-and-true-p rime-mode))
				   (if (and (rime--should-enable-p)
							(not (rime--should-inline-ascii-p)))
					   (format "%s%s"
							   (propertize (format "%s-CN" (rime-lighter))
										   'face `(:inherit input-method-indicator-cn-face))
							   (propertize " | " 'face `(:inherit mode-line-seperator)))
					 (format "%s%s"
							 (propertize (format "%s-EN" (rime-lighter))
										 'face `(:inherit input-method-indicator-en-face))
							 (propertize " | " 'face `(:inherit mode-line-seperator)))))
			   ;; System && Encoding
			   ;; (propertize (let ((buf-coding (format "%s" buffer-file-coding-system)))
			   ;; 				(if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
			   ;; 					(upcase (concat (string-trim-right buf-coding (concat "-" (match-string 1 buf-coding))) (format "[%s] " system-type)))
			   ;; 				  (upcase (concat buf-coding (format "[%s] " system-type))))) 'face `(:inherit mode-line-area-4))
			   (propertize (let ((buf-coding (format "%s" buffer-file-coding-system)))
							 (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
								 (string-trim-right buf-coding (concat "-" (match-string 1 buf-coding)))
							   buf-coding)) 'face `(:inherit mode-line-area-4))
			   "  "
			   sys-type
			   " "
			   ;;  (propertize " %3l:%3c " 'face '(:background "#928374" :faceground "#fbf1c7"))

			   ;; Time
			   ;; (let* ((hour (string-to-number (format-time-string "%I")))
			   ;; 		  (icon (nerd-icons-wicon (format "nf-weather-time_%s" hour))))
			   ;; 	 (concat
			   ;; 	  (propertize (format-time-string " %H:%M") 'face `(:inherit mode-line-area-5))
			   ;; 	  (propertize " " 'face `(:inherit mode-line-area-5))
			   ;; 	  (propertize (nerd-icons-wicon (format "nf-weather-time_%s" hour) :face '(:inherit mode-line-area-5)))))

			   ;; File Line
			   (propertize (format " %s" (car (buffer-line-statistics))) 'face `(:inherit mode-line-area-5)
						   'help-echo "Buffer Line Length")
			   (propertize " " 'face `(:inherit mode-line-area-5)))))))
	   (concat (ml-fill-space right-part)
			   (format-mode-line right-part)))))))

(provide 'modeline-setting)
;;; modeline-setting.el ends here
