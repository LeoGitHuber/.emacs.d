;;; init-org --- Init for Org
;;; Commentary:
;;; Code:

;;; Org Mode

(add-to-list 'load-path "~/.emacs.d/site-lisp/org-roam")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-modern-indent")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-modern")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-appear")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-bars")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacsql")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-visual-outline")

;; Hide spaces of chinese inline block
;; (font-lock-add-keywords 'org-mode
;;                         '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
;;                            (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
;;                           ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
;;                            (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
;;                         'append)

;; (custom-theme-set-faces
;; 'user
;; ;; '(fixed-pitch ((t (:family "Essential PragmataPro" :height 1.0))))
;; ;; '(variable-pitch ((t (:family "Bookerly" :height 1.0))))
;; '(fixed-pitch ((t (:family "Source Code Pro" :height 1.0))))
;; '(variable-pitch ((t (:family "Source Code Pro" :height 1.0))))
;; '(org-table ((t (:inherit fixed-pitch))))
;; '(org-tag ((t (:inherit fixed-pitch))))
;; '(org-verbatim ((t (:inherit fixed-pitch))))
;; '(org-src ((t (:inherit fixed-pitch)))))

(with-eval-after-load 'org
  ;; (set-face-attribute 'org-table nil :font (font-spec :name "Sarasa Mono SC" ; LXGW WenKai
  ;; 													  :weight 'semibold
  ;; 													  :size 13.0)
  ;;  )
  (setq org-hide-emphasis-markers t
		org-pretty-entities t
        prettify-symbols-mode t
        prettify-symbols-unprettify-at-point 'right-edge
		org-image-actual-width nil
		;; org-todo-keywords '((sequence "     " "     "))
		org-preview-latex-process-alist '((dvipng :programs
												  ("latex" "dvipng")
												  :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
												  (1.0 . 1.0)
												  :latex-compiler
												  ("latex -interaction nonstopmode -output-directory %o %f")
												  :image-converter
												  ("dvipng -D %D -T tight -o %O %f")
												  :transparent-image-converter
												  ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
										  (dvisvgm :programs
												   ("latex" "dvisvgm")
												   :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
												   (1.0 . 1.0)
												   :latex-compiler
												   ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
												   :image-converter
												   ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
										  (imagemagick :programs
													   ("latex" "convert")
													   :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
													   (1.0 . 1.0)
													   :latex-compiler
													   ("pdflatex -interaction nonstopmode -output-directory %o %f")
													   :image-converter
													   ("convert -density %D -trim -antialias %f -quality 100 %O")))
		org-preview-latex-default-process 'dvisvgm
		org-format-latex-header "\\documentclass[10pt]{article}\n\\usepackage[usenames]{color}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\pagestyle{empty}  % do not remove\n% The settings below are copied from fullpage.sty\n
        \\usepackage{xeCJK,tikz,caption,float,makecell,circuitikz,array}\n
        \\usetikzlibrary{shapes,arrows,calc,arrows.meta}\n
        \\usetikzlibrary{circuits.logic.IEC,calc}\n
        \\renewcommand{\\arraystretch}{1.3}\n
        \\setlength{\\textwidth}{\\paperwidth}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}\n")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (jupyter . t)))
  )

;; (add-hook 'org-mode-hook 'variable-pitch-mode)

(add-hook 'org-mode-hook
		  (lambda ()
            (electric-indent-local-mode)
		    (setq-local company-backends '(company-files company-keywords))
            (setq org-appear-autolinks t)
		    (org-appear-mode)
		    ;; (company-mode)
		    ;; (corfu-mode)
		    ;; (visual-line-mode)
            (valign-mode)))

;; (with-eval-after-load 'org
;; (defun org-buffer-face-mode-variable ()
;; (interactive)
;; (make-face 'width-font-face)
;; (set-face-attribute 'width-font-face nil :font (font-spec :name "LXGW WenKai Mono";; :name "Sarasa Mono SC"
;; ;; :weight 'semibold
;; :size 13.0))  ;; 等距更纱黑体
;; (setq buffer-face-mode-face 'width-font-face)
;; (buffer-face-mode))
;; (add-hook 'org-mode-hook 'org-buffer-face-mode-variable)

;; (setq org-hide-emphasis-markers t))

;; Org-superstar
;; (add-hook 'org-mode-hook 'org-superstar-mode)

;; Org-modern

(with-eval-after-load 'org-modern
  (setq org-modern-todo t
		org-modern-table nil
		org-modern-tag t
		org-modern-priority t
		org-modern-keyword t
		org-modern-block-name t
		org-modern-horizontal-rule t
		org-modern-statistics t
		org-modern-timestamp t
		;; org-modern-hide-stars t
		org-modern-checkbox nil
		;; org-modern-star nil
		org-modern-list
		'(
		  ;; (?- . "-")
		  (?* . "•")
		  (?+ . "‣"))
		))


;; (add-hook 'org-agenda-finalize-hook 'org-modern-agenda)

;; (require 'org-bars)
;; (add-hook 'org-mode-hook #'org-bars-mode)

;; (add-hook 'org-mode-hook 'word-wrap-whitespace-mode)

(setq-default org-startup-folded 'overview
			  org-startup-with-inline-images t
              org-startup-indented t)
;; (setq-default org-highlight-latex-and-related '(native latex script entities))

(add-hook 'org-mode-hook
		  (lambda ()
			(setq-local time-stamp-active t
						time-stamp-start "#\\+MODIFIED: [ \t]*"
						time-stamp-end "$"
						time-stamp-format "\[%Y-%m-%d %3a %H:%M\]")
			(setq org-list-allow-alphabetical t)
			(add-hook 'before-save-hook 'time-stamp nil 'local)
			(org-modern-mode)
			))

;; (run-with-timer 2 nil #'(lambda ()
;; (require 'org-contrib)
;; (require 'org-modern-indent)))
(add-hook 'org-mode-hook 'org-modern-indent-mode 100)

(keymap-global-set "C-c n f" 'org-roam-node-find)
(setq org-roam-directory "~/Personal/org-roam"    ; 设置 org-roam 笔记的默认目录，缺省值 /home/leo/org-roam
      org-roam-db-gc-threshold most-positive-fixnum
      org-roam-mode-sections '(org-roam-backlinks-section
							   org-roam-reflinks-section
							   org-roam-unlinked-references-section))
(with-eval-after-load 'org-roam
  (add-hook 'org-roam-mode-hook (lambda ()
                                  ;; (turn-on-visual-line-mode)
                                  (word-wrap-whitespace-mode)))
  (org-roam-db-autosync-mode))

;; (add-to-list 'display-buffer-alist
;; 			 '("\\*org-roam*\\*"
;; 			   (display-buffer-in-side-window)
;; 			   (side . right)
;; 			   (window-width . 0.15)))

;; (with-eval-after-load 'org-roam
;;   Auto toggle org-roam-buffer.
;;   (defun my/org-roam-buffer-show (_)
;; 	(if (and
;; 		 Don't do anything if we're in the minibuffer or in the calendar
;; 		 (not (minibufferp))
;; 		 (not (> 120 (frame-width)))
;; 		 (not (bound-and-true-p olivetti-mode))
;; 		 (not (derived-mode-p 'calendar-mode))
;; 		 Show org-roam buffer iff the current buffer has a org-roam file
;; 		 (xor (org-roam-file-p) (eq 'visible (org-roam-buffer--visibility))))
;; 		(org-roam-buffer-toggle)))
;;   (add-hook 'window-buffer-change-functions 'my/org-roam-buffer-show)
;;   )

(with-eval-after-load 'org-roam
  (setq org-roam-database-connector 'sqlite-builtin)
  (setq org-roam-node-display-template
		(concat "${title:*} "
				(propertize "${tags:10}" 'face 'org-tag))))

(with-eval-after-load 'org-roam
  (setq org-roam-capture-templates
		'(("d" "default" plain "%?"
		   :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							  "#+title: ${title}\n")
		   :unnarrowed t)
		  ("b" "Books" plain "* Related Information\n\nAuthor: %^{Author}\nVersion: %^{Version}\n\n* Notes\n%?"
		   :target (file+head "books/${slug}.org"
							  "#+TITLE: ${title}\n#+FILETAGS: %^{}\n#+CREATED: %U\n#+MODIFIED: \n\n")
		   :unnarrowed t)
		  ("t" "Trifles" entry "* Notes:\n%?"
		   :target (file+head "Trifles/${slug}.org"
							  "#+TITLE: ${title}\n#+FILETAGS: %^g\n#+CREATED: %U\n#+MODIFIED: \n\n")
		   :unnarrowed t)
		  ("p" "Programming" entry "* Notes:\n%?"
		   :target (file+head "Programming/${slug}.org"
							  "#+TITLE: ${title}\n#+FILETAGS: %^g\n#+CREATED: %U\n#+MODIFIED: \n\n")
		   :unnarrowed t))))

(provide 'init-org)
;;; init-org.el ends here.
