;;; package --- Sumary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(load "~/.emacs.d/self-develop/emacs-nf-diy.el")

(defgroup mode-line-personal nil
  "Personal Mode Line Setting."
  :prefix "mode-line-area-"
  :group 'basic-faces
  :group 'appearance)

(defface buffer-status
  '(;; (default :font "JetBrainsMono Nerd Font-12")
	(((background dark)) :foreground "#9CC0E8")
    (((background light)) :foreground "#194374"))
  "Face for foreground of buffer status.")

(defface mode-line-area-0
  '((((background dark)) :background "#665c54")
    (((background light)) :background "#ebdbb2"))
  "Face for default background of personal mode line."
  :group 'mode-line-personal)

(defface mode-line-area-1
  '(;; (((background dark)) :background "#83a598" :foreground "#1d2021")
	;; (((background light)) :background "#076678" :foreground "#f9f5d7")
	;; (((background dark)) :background "#1C2F45" :foreground "#589EDD")
	;; (((background light)) :background "#CEE1F8" :foreground "#20396A")
    (((background dark)) :background "#71B7FF" :foreground "#251113")
    (((background light)) :background "#0349B4" :foreground "#E0FDFF")
    )
  "Face for personal mode line setting area 1."
  :group 'mode-line-personal)

(defface mode-line-area-1-separator-2
  '(;; (((background dark)) :foreground "#1C2F45" :inherit mode-line-area-2)
	;; (((background light)) :foreground "#CEE1F8" :inherit mode-line-area-2)
    (((background dark)) :foreground "#71B7FF" :inherit mode-line-area-2)
	(((background light)) :foreground "#0349B4" :inherit mode-line-area-2)
    )
  "Face for personal mode line setting area 1."
  :group 'mode-line-personal)

(defface mode-line-area-1-separator-3
  '(;; (((background dark)) :foreground "#1C2F45" :inherit mode-line-area-3)
    ;; (((background dark)) :foreground "#1C2F45" :inherit mode-line)
	;; (((background light)) :foreground "#CEE1F8" :inherit mode-line-area-3)
    (((background dark)) :foreground "#71B7FF" :inherit mode-line)
	(((background light)) :foreground "#0349B4" :inherit mode-line-area-2)
    )
  "Face for personal mode line setting area 1."
  :group 'mode-line-personal)

(defface mode-line-area-1-separator-2
  '(;; (((background dark)) :foreground "#1C2F45" :inherit mode-line-area-2)
	;; (((background light)) :foreground "#CEE1F8" :inherit mode-line-area-2)
    (((background dark)) :foreground "#71B7FF" :inherit mode-line-area-2)
	(((background light)) :foreground "#0349B4" :inherit mode-line-area-2)
    )
  "Face for personal mode line setting area 1."
  :group 'mode-line-personal)

(defface mode-line-git
  '((((background dark)) :background "#1F2E40" :foreground "#4788CD")
    (((background light)) :background "#CDDBF0" :foreground "#5074C3"))
  "Face for personal mode line setting git."
  :group 'mode-line-personal)

(defface ml-git-insert
  '(
	(((background dark)) :background "#0A2516" :foreground "#09B239")
    (((background light)) :background "#DAF7E0" :foreground "#285F25")
	)
  "Face for personal mode line setting git."
  :group 'mode-line-personal)

(defface ml-git-delete
  '(
    (((background dark)) :background "#2F1A1D" :foreground "#FF6A69")
    (((background light)) :background "#FFECEB" :foreground "#D22844"))
  "Face for personal mode line setting git."
  :group 'mode-line-personal)

(defface diagnostic-error-face
  '((default :inherit mode-line-git)
    (((background dark)) :foreground "#FF6A69")
    (((background light)) :foreground "#D12954"))
  "Face for personal mode line setting diagnostic error."
  :group 'mode-line-personal)

(defface diagnostic-warning-face
  '((default :inherit mode-line-git)
    (((background dark)) :foreground "#C09914")
    (((background light)) :foreground "#834707"))
  "Face for personal mode line setting diagnostic warning."
  :group 'mode-line-personal)

(defface diagnostic-info-face
  '((default :inherit mode-line-git)
    (((background dark)) :foreground "#409eff")  ;; "#0a0c10"
    (((background light)) :foreground "#0349b4") ;; "#0e1116"
    )
  "Face for personal mode line setting diagnostic info."
  :group 'mode-line-personal)

(defvar diagnostic-error
  "󰅚 "
  "Icon for diagnostic error.")

(defvar diagnostic-warning
  "󰀪 "
  "Icon for diagnostic warning.")

(defvar diagnostic-info
  "󰋽 "
  "Icon for diagnostic info.")

(defvar diagnostic-hint
  "󰌶 "
  "Icon for diagnostic hint.")

(defface mode-line-area-2-separator
  '(
	(((background dark)) :foreground "#244032" :background "#341A00")
	(((background light)) :foreground "#D4F8DB" :inherit 'mode-line))
  "Face for personal mode line setting area 2."
  :group 'mode-line-personal)

(defface mode-line-area-3
  '(
    ;; (((background dark)) :background "#341A00" :foreground "#E3B341")
    (((background dakr)) :inherit 'mode-line)
    (((background light)) :foreground "#1A478D" :inherit 'mode-line)
    )
  "Face for personal mode line setting area 3."
  :group 'mode-line-personal)

(defface mode-line-buffer
  '(
    (((background dark)) :foreground "#174287")
    (((background light)) :foreground "#455C9A"))
  "Face for personal mode line buffer part."
  :group 'mode-line-personal)

(with-eval-after-load 'rime
  (set-face-attribute 'rime-indicator-face nil
					  :font (font-spec
							 :name "JetBrainsMono Nerd Font"))
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

(defface mode-line-right-1
  '((((background dark)) :background "#71B7FF" :foreground "black")
    (((background light)) :background "#0969DA" :foreground "#E1FDFF")
	)
  "Face for personal mode line setting right area 1."
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
  :group 'flycheck-mode-line)

(defface flycheck-warn
  '(
	(((background dark)) :background "#090c10" :foreground "#f0883e")
    ;; (((background light)) :foreground "#bf8803" :background "#f6f8fa")
	(((background light)) :foreground "#bf8803")
	)
  "Face for flycheck Warn."
  :group 'flycheck-mode-line)

(defface flycheck-info-my
  '((((background dark)) :background "#090c10" :foreground "#75beff")
    ;; (((background light)) :foreground "#75beff" :background "#f6f8fa")
	(((background light)) :foreground "#75beff")
	)
  "Face for flycheck Info."
  :group 'flycheck-mode-line)

(with-eval-after-load 'winum
  (setq winum-ignored-buffers '("*sort-tab*")
		winum-auto-setup-mode-line nil)
  (defun +win-num ()
	(let ((n (winum-get-number)))
  	  (alist-get
  	   n
  	   '(
		 (0 . "🄌")
		 (1 . "❶")
		 (2 . "❷")
  		 (3 . "❸")
  		 (4 . "❹")
  		 (5 . "❺")
  		 (6 . "❻")
  		 (7 . "❼")
  		 (8 . "❽")
  		 (9 . "❾")
		 ))))
  (add-hook 'winum-mode-hook (lambda () (setq winum-auto-setup-mode-line nil))))

(defvar sys-type
  (let ((st (format "%s" system-type)))
	;; (cond ((string-equal "gnu" st) (nerd-icons-devicon "nf-dev-gnu" :face `(:inherit mode-line-area-4)))
	;; 	  ((string-match "linux" st) (nerd-icons-devicon "nf-dev-linux" :face `(:inherit mode-line-area-4)))
	;; 	  ((string-match "ms-dos" st) (nerd-icons-devicon "nf-dev-windows" :face `(:inherit mode-line-area-4)))
	;; 	  ((string-match "darwin" st) (nerd-icons-devicon "nf-dev-apple" :face `(:inherit mode-line-area-4))))
    (format " %s "
            (cond ((string-equal "gnu" st) (nerd-icons-devicon "nf-dev-gnu"))
		          ((string-match "linux" st) (nerd-icons-devicon "nf-dev-linux"))
		          ((string-match "ms-dos" st) (nerd-icons-devicon "nf-dev-windows"))
		          ((string-match "darwin" st) (nerd-icons-devicon "nf-dev-apple")))))
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
  (nerd-icons-faicon "nf-fa-minus_square" :face '(:inherit buffer-status))
  "Set icon for read-only buffer.")

(defvar Buffer-modified
  ;; (propertize (nerd-icons-faicon "nf-fa-plus_square") 'face '(:inherit buffer-status))
  (nerd-icons-faicon "nf-fa-plus_square" :face '(:inherit buffer-status))
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
		(cons 9 (concat (propertize " " 'face 'mode-line-area-1) (propertize (nerd-icons-mdicon "nf-md-numeric_9_circle" :face '(:inherit mode-line-area-1)) 'help-echo "Windows Number") (propertize " " 'face 'mode-line-area-1)))
        )
  "Set 0-9 icons for winum mdoe.")

;; (setq-default
;;  mode-line-format
;;  '((:eval
;; 	(concat
;; 	 (when (and (bound-and-true-p winum-mode) (winum-get-number))
;; 	   (concat (alist-get (winum-get-number) winum-list)
;; 			   (if (buffer-file-name)
;; 				   (propertize mode-line-sep 'face '(:inherit mode-line-area-1-separator-2))
;; 				 (propertize mode-line-sep 'face '(:inherit mode-line-area-1-separator-3)))))

;; 	 ;; Directory
;; 	 (if (buffer-file-name)
;; 		 (concat
;; 		  (propertize (format " %s " (abbreviate-file-name (file-name-directory (buffer-file-name)))) 'face `(:inherit mode-line-area-2) 'help-echo "Current Directory")
;; 		  (propertize mode-line-sep 'face '(:inherit mode-line-area-2-separator))
;; 		  ))
;; 	 (propertize " " 'face `(:inherit mode-line-area-3))

;; 	 ;; Buffer Icon
;; 	 (propertize (if (or (buffer-file-name) (nerd-icons-auto-mode-match?))
;; 					 (emacs-nf-diy-for-file (buffer-name) :face `(:background ,(face-attribute 'mode-line-area-3 :background)))
;; 				   (emacs-nf-diy-for-mode major-mode :face `(:background ,(face-attribute 'mode-line-area-3 :background)))
;; 				   )
;; 				 'help-echo (symbol-name (symbol-value 'major-mode)))
;; 	 ;; Buffer Name
;; 	 (propertize " %b " 'face `(:inherit mode-line-area-3) 'help-echo "Current Buffer Name")
;; 	 " "
;; 	 (cond (buffer-read-only Read-only)
;; 		   ((buffer-modified-p) Buffer-modified))
;; 	 (let*
;; 		 ((right-part
;; 		   '(
;; 			 ;; Git\
;; 			 (:eval
;; 			  (concat
;; 			   (when vc-mode
;; 				 (cond
;; 				  ((string-match "Git[:-]" vc-mode)
;; 				   (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
;; 					 (concat
;; 					  (nerd-icons-devicon "nf-dev-git")
;; 					  " · "
;; 					  (nerd-icons-octicon "nf-oct-git_branch")
;; 					  (propertize (format " %s" branch) 'face `(:inherit mode-line))
;; 					  (propertize " |" 'face `(:inherit mode-line-seperator)))))
;; 				  ((string-match "SVN-" vc-mode)
;; 				   (let ((revision (cadr (split-string vc-mode "-"))))
;; 					 (concat
;; 					  (format " %s" (nerd-icons-faicon "nf-fa-cloud"))
;; 					  (format " · %s" revision))))
;; 				  (t (format "%s" vc-mode))))
;; 			   " "

;; 			   ;; Flycheck Information
;; 			   (when (bound-and-true-p flycheck-mode)
;;                  (pcase flycheck-last-status-change
;; 				   (`finished
;; 				    (if flycheck-current-errors
;; 					    (let-alist (flycheck-count-errors flycheck-current-errors)
;; 						  (let ((i (or .info 0))
;; 							    (w (or .warning 0))
;; 							    (e (or .error 0)))
;; 						    (concat
;; 						     (propertize fc-info
;; 									     'help-echo "Show Flycheck Error"
;; 									     'local-map (make-mode-line-mouse-map
;; 												     'mouse-1 (lambda() (interactive) (flycheck-list-errors))))
;; 						     (propertize (concat " " (number-to-string i) " ")
;; 									     'help-echo "Show Flycheck Error"
;; 									     'local-map (make-mode-line-mouse-map
;; 												     'mouse-1 (lambda() (interactive) (flycheck-list-errors)))
;; 									     'face '(:inherit flycheck-info-my))
;; 						     (propertize
;; 							  fc-warning
;; 							  'help-echo "Show Flycheck Error"
;; 							  'local-map (make-mode-line-mouse-map
;; 										  'mouse-1 (lambda() (interactive) (flycheck-list-errors))))
;; 						     (propertize (concat  " " (number-to-string w) " ")
;; 									     'help-echo "Show Flycheck Error"
;; 									     'local-map (make-mode-line-mouse-map
;; 												     'mouse-1 (lambda() (interactive) (flycheck-list-errors)))
;; 									     'face '(:inherit flycheck-warn))
;; 						     (propertize fc-error
;; 									     'help-echo "Show Flycheck Error"
;; 									     'local-map (make-mode-line-mouse-map 'mouse-1 (lambda() (interactive) (flycheck-list-errors))))
;; 						     (propertize (concat " " (number-to-string e))
;; 									     'help-echo "Show Flycheck Error"
;; 									     'local-map (make-mode-line-mouse-map 'mouse-1 (lambda() (interactive) (flycheck-list-errors)))
;; 									     'face '(:inherit flycheck-error-my))
;; 						     (propertize " | " 'face `(:inherit mode-line-seperator)))))
;; 					  (concat (propertize "✔ No Issues")
;; 							  (propertize " | " 'face `(:inherit mode-line-seperator)))))
;; 				   (`running (concat (propertize "⟲ Running")
;; 								     (propertize " | " 'face `(:inherit mode-line-seperator))))
;; 				   (`no-checker "")
;; 				   ;; (`no-checker "⚠ No Checker")
;; 				   (`not-checked (concat
;; 								  (propertize (nerd-icons-mdicon "nf-md-eye_off"))
;; 								  (propertize " Disabled")
;; 								  (propertize " | " 'face `(:inherit mode-line-seperator))))
;; 				   ;; (`not-checked "⚠ Disabled")
;; 				   (`errored  (concat (propertize "✖ Error" 'face `())
;; 									  (propertize " | " 'face `(:inherit mode-line-seperator))))
;; 				   ;; (`interrupted "⛔ Interrupted")
;; 				   (`interrupted (concat
;; 								  (propertize (nerd-icons-octicon "nf-oct-circle_slash"))
;; 								  (propertize " Interrupted")))
;; 				   (`suspinnnncious (concat
;; 								     (propertize (nerd-icons-faicon "nf-fa-question"))
;; 								     (propertize " Suspicious")
;; 								     (propertize " | " 'face `(:inherit mode-line-seperator))))))

;; 			   ;; Input Method
;; 			   (if (and (equal current-input-method "rime")
;; 						(bound-and-true-p rime-mode))
;; 				   (if (and (rime--should-enable-p)
;; 							(not (rime--should-inline-ascii-p)))
;; 					   (format "%s%s"
;; 							   (propertize (format "%s-CN" (rime-lighter))
;; 										   'face `(:inherit input-method-indicator-cn-face))
;; 							   (propertize " | " 'face `(:inherit mode-line-seperator)))
;; 					 (format "%s%s"
;; 							 (propertize (format "%s-EN" (rime-lighter))
;; 										 'face `(:inherit input-method-indicator-en-face))
;; 							 (propertize " | " 'face `(:inherit mode-line-seperator)))))
;; 			   ;; System && Encoding
;; 			   ;; (propertize (let ((buf-coding (format "%s" buffer-file-coding-system)))
;; 			   ;; 				(if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
;; 			   ;; 					(upcase (concat (string-trim-right buf-coding (concat "-" (match-string 1 buf-coding))) (format "[%s] " system-type)))
;; 			   ;; 				  (upcase (concat buf-coding (format "[%s] " system-type))))) 'face `(:inherit mode-line-area-4))
;; 			   (propertize (let ((buf-coding (format "%s" buffer-file-coding-system)))
;; 							 (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
;; 								 (string-trim-right buf-coding (concat "-" (match-string 1 buf-coding)))
;; 							   buf-coding)) 'face `(:inherit mode-line-area-4))
;; 			   "  "
;; 			   sys-type
;; 			   " "
;; 			   ;;  (propertize " %3l:%3c " 'face '(:background "#928374" :faceground "#fbf1c7"))

;; 			   ;; Time
;; 			   ;; (let* ((hour (string-to-number (format-time-string "%I")))
;; 			   ;; 		  (icon (nerd-icons-wicon (format "nf-weather-time_%s" hour))))
;; 			   ;; 	 (concat
;; 			   ;; 	  (propertize (format-time-string " %H:%M") 'face `(:inherit mode-line-area-5))
;; 			   ;; 	  (propertize " " 'face `(:inherit mode-line-area-5))
;; 			   ;; 	  (propertize (nerd-icons-wicon (format "nf-weather-time_%s" hour) :face '(:inherit mode-line-area-5)))))

;; 			   ;; File Line
;; 			   (propertize (format " %s" (car (buffer-line-statistics))) 'face `(:inherit mode-line-area-5)
;; 						   'help-echo "Buffer Line Length")
;; 			   (propertize " " 'face `(:inherit mode-line-area-5)))))))
;; 	   (concat (ml-fill-space right-part)
;; 			   (format-mode-line right-part)))))))

(setq-default
 mode-line-format
 '((:eval
	(concat
	 (when (and (bound-and-true-p winum-mode) (winum-get-number))
	   (alist-get (winum-get-number) winum-list)
	   ;; (if (buffer-file-name)
	   ;;     (propertize mode-line-sep 'face '(:inherit mode-line-area-1-separator-2))
	   ;;   (propertize mode-line-sep 'face '(:inherit mode-line-area-1-separator-3)))
       )
     ;; Git
     (when vc-mode
       (concat
        (propertize mode-line-sep
                    'face
                    `(:background ,(face-background 'mode-line-git)
                                  :foreground ,(face-background 'mode-line-area-1)))
        (propertize
	     (cond
	      ((string-match "Git[:-]" vc-mode)
	       (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
		     (format " %s %s "
		             "" ;; (nerd-icons-powerline "nf-pl-branch")
                     branch)))
	      ((string-match "SVN-" vc-mode)
	       (let ((revision (cadr (split-string vc-mode "-"))))
		     (format " %s · %s" (nerd-icons-faicon "nf-fa-cloud") revision)))
	      (t (format "%s" vc-mode)))
         'face '(:inherit mode-line-git))
        (let* ((stat (car
                      (process-lines "git"
                                     "diff" "--numstat" buffer-file-name))))
          (if stat
              (let ((stat (string-split stat)))
                (concat (propertize (format " +%s " (car stat)) 'face '(:inherit ml-git-insert))
                        (propertize (format "-%s " (cadr stat)) 'face '(:inherit ml-git-delete))))
            ""))))

     (if (bound-and-true-p flymake-mode)
         (let* ((known (hash-table-keys flymake--state))
                (running (flymake-running-backends))
                (disabled (flymake-disabled-backends))
                (reported (flymake-reporting-backends))
                ;; (all-disabled (and disabled (null running)))
                ;; (some-waiting (get-pure-cons running reported))  ;; Affect performance
                (icon
                 (format "%s"
                         (cond
                          ;; (some-waiting
                          ;;  (propertize "⟲ " 'face '(:inherit font-lock-doc-face)))
                          ((null known)
                           (propertize "󰳤 " 'face '(:inherit error)))
                          ;; (all-disabled
                          ;;  (propertize "󰀧 " 'face '(:inherit warning)))
                          (t (let ((.error 0)
                                   (.warning 0)
                                   (.note 0))
                               (progn
                                 (let* ((warning-level (warning-numeric-level :warning))
                                        (note-level (warning-numeric-level :debug))
                                        (error-level (warning-numeric-level :error))
                                        (states (hash-table-values flymake--state)))
                                   (dolist (state states)
                                     (dolist (diag (flymake--state-diags state))
                                       (let ((severity (flymake--lookup-type-property
                                                        (flymake--diag-type diag)
                                                        'severity
                                                        error-level)))
                                         (cond ((> severity warning-level)
                                                (setq .error (+ 1 .error)))
                                               ((> severity note-level)
                                                (setq .warning (+ 1 .warning)))
                                               (t (setq .note (+ 1 .note))))))))
                                 (if (> (+ .error .warning .note) 0)
                                     (concat
                                      (unless vc-mode
                                        (propertize mode-line-sep
                                                    'face
                                                    `(:background
                                                      ,(face-background 'mode-line-git)
                                                      :foreground
                                                      ,(face-background 'mode-line-area-1))))
                                      (propertize " "
                                                  'face '(:inherit mode-line-git))
                                      (if (= .error 0)
                                          ""
                                        (propertize
                                         (format "%s%d " diagnostic-error .error)
                                         'face '(:inherit diagnostic-error-face)))
                                      (if (= .warning 0)
                                          ""
                                        (propertize
                                         (format "%s%d " diagnostic-warning .warning)
                                         'face '(:inherit diagnostic-warning-face)))
                                      (if (= .note 0)
                                          ""
                                        (propertize
                                         (format "%s%d " diagnostic-info .note)
                                         'face '(:inherit diagnostic-info-face)))
                                      (propertize mode-line-sep
                                                  'face
                                                  `(:foreground
                                                    ,(face-background 'mode-line-git))))
                                   ;; (nerd-icons-mdicon "nf-md-check_circle_outline"
                                   ;;                    :face '(:inherit success))
                                   (unless vc-mode
                                     (propertize mode-line-sep 'face '(:inherit mode-line-area-1-separator-3)))
                                   ))))))))
           (propertize
            icon
            'help-echo (concat "Flymake\n"
                               (cond
                                ;; (some-waiting "Checking...")
                                ((null known) "No Checker")
                                ;; (all-disabled "All Checkers Disabled")
                                (t (format "%d/%d backends running
           mouse-1: Display minor mode menu
           mouse-2: Show help for minor mode"
                                           (length running) (length known)))))
            'mouse-face 'mode-line-highlight
            'local-map (let ((map (make-sparse-keymap)))
                         (define-key map [mode-line down-mouse-1]
                                     flymake-menu)
                         (define-key map [mode-line mouse-2]
                                     (lambda ()
                                       (interactive)
                                       (describe-function 'flymake-mode)))
                         map)))
       (propertize mode-line-sep 'face '(:inherit mode-line-area-1-separator-3)))

     ;; Directory
     ;; (if (buffer-file-name)
     ;;     (concat
     ;;      (propertize (format " %s " (abbreviate-file-name (file-name-directory (buffer-file-name)))) 'face `(:inherit mode-line-area-2) 'help-echo "Current Directory")
     ;;      (propertize mode-line-sep 'face '(:inherit mode-line-area-2-separator))
     ;;      ))
     ;; (propertize " " 'face `(:inherit mode-line-area-3))

     ;; Buffer Name
     (if buffer-file-name
         (propertize (format " %s " buffer-file-truename) 'face '(:inherit mode-line-buffer) 'help-echo "Current Editing File")
       (propertize " %b " 'face '(:inherit mode-line-buffer) 'help-echo "Current Editing Buffer"))
     (cond (buffer-read-only Read-only)
	       ((buffer-modified-p) Buffer-modified))))

   mode-line-format-right-align


   (:eval
    (concat
     ;; ;; Git
     ;;   (when vc-mode
     ;;     (cond
     ;;      ((string-match "Git[:-]" vc-mode)
     ;;       (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
     ;;  	   (concat
     ;;  	    (nerd-icons-devicon "nf-dev-git")
     ;;  	    " · "
     ;;  	    (nerd-icons-octicon "nf-oct-git_branch")
     ;;  	    (propertize (format " %s" branch) 'face `(:inherit mode-line))
     ;;  	    (propertize " |" 'face `(:inherit mode-line-seperator)))))
     ;;      ((string-match "SVN-" vc-mode)
     ;;       (let ((revision (cadr (split-string vc-mode "-"))))
     ;;  	   (concat
     ;;  	    (format " %s" (nerd-icons-faicon "nf-fa-cloud"))
     ;;  	    (format " · %s" revision))))
     ;;      (t (format "%s" vc-mode))))
     ;;   " "

     ;; Flycheck Information
     (when (bound-and-true-p flycheck-mode)
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
					    (propertize (nerd-icons-mdicon "nf-md-eye_off"))
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
						   (propertize " | " 'face `(:inherit mode-line-seperator))))))

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
				     buf-coding))
                 'face '(:inherit mode-line-area-4))
     sys-type
     ;;  (propertize " %3l:%3c " 'face '(:background "#928374" :faceground "#fbf1c7"))

     ;; Major Mode
     (format-mode-line mode-name)
     (format " %s "
	         (propertize (if (or (buffer-file-name) (nerd-icons-auto-mode-match?))
	    			         (emacs-nf-diy-for-file (buffer-name)
                                                    :face `(:background ,(face-attribute 'mode-line-area-3 :background)))
	    		           (emacs-nf-diy-for-mode major-mode
                                                  :face `(:background ,(face-attribute 'mode-line-area-3 :background))))
	    		         'help-echo (symbol-name (symbol-value 'major-mode))))
     ;; Time
     ;; (let* ((hour (string-to-number (format-time-string "%I")))
     ;; 		  (icon (nerd-icons-wicon (format "nf-weather-time_%s" hour))))
     ;; 	 (concat
     ;; 	  (propertize (format-time-string " %H:%M") 'face `(:inherit mode-line-area-5))
     ;; 	  (propertize " " 'face `(:inherit mode-line-area-5))
     ;; 	  (propertize (nerd-icons-wicon (format "nf-weather-time_%s" hour) :face '(:inherit mode-line-area-5)))))

     ;; File Line
     (propertize (format " %s " (car (buffer-line-statistics))) 'face `(:inherit mode-line-area-1)
			     'help-echo "Buffer Line Length")

     ;; Range
     ))))

(provide 'modeline-setting)
;;; modeline-setting.el ends here
