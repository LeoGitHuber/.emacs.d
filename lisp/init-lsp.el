;;; init-lsp -- Init for Lsp
;;; Commentary:
;;; Code:

;;; Coding Relpated

;; (require 'lsp-mode)
;; (require 'lsp-ui)

(with-eval-after-load 'verilog-mode
  ;;   (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
  ;; 										:major-modes '(verilog-mode)
  ;; 										:priority -1))
  (setq verilog-indent-lists nil))

;; (add-hook 'verilog-mode-hook '(lambda ()
;; (lsp)
;; (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
;; ))
(with-eval-after-load 'lsp-mode
  (dolist (hook '(prog-mdoe-hook cuda-mode-hook TeX-mode-hook))
	'(lambda ()
	   (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
		 (lsp-deferred))))
  (dolist (hook '(markdown-mode-hook yaml-ts-mode yaml-mode))
	(add-hook hook #'lsp-deferred))
  (setq lsp-prefer-flymake nil
		lsp-ui-flycheck-enable t
		lsp-disabled-clients '(svlangserver))
  (keymap-set lsp-mode-map "C-c C-d" #'lsp-describe-thing-at-point)
  )

(dolist (hook '(cuda-mode-hook))  ;; prog-mode-hook  TeX-mode-hook
  (add-hook hook 'yas-minor-mode))

(with-eval-after-load 'lsp-mode
  (setq lsp-keymap-prefix "C-c l"
		;; lsp-keep-workspace-alive nil
		;; lsp-signature-auto-activate nil
		lsp-modeline-code-actions-enable nil
		lsp-modeline-diagnostics-enable nil
		lsp-modeline-workspace-status-enable nil
		lsp-headerline-breadcrumb-enable t

		lsp-semantic-tokens-enable t
		;; lsp-progress-spinner-type 'progress-bar-filled

		lsp-enable-file-watchers nil
		lsp-enable-folding nil
		lsp-enable-symbol-highlighting nil
		lsp-enable-text-document-color nil

		lsp-enable-indentation nil
		lsp-enable-on-type-formatting nil

		lsp-enable-indentation nil

		;; For diagnostics
		;; lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

		;; Reference Lens
		lsp-lens-enable nil

		;; ui
		lsp-ui-doc-show-with-cursor nil
		))

(add-hook 'lsp-mode-hook #'(lambda ()
							 (setq lsp-enable-relative-indentation t)
							 (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
							 (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

							 ;; For diagnostics
							 (lsp-diagnostics-mode -1)
							 ))

;; (dolist (completion '(company-mode corfu-mode))
;;   (with-eval-after-load completion
;; 	(let ((hls '(emacs-lisp-mode-hook lisp-mode-hook TeX-mode-hook verilog-mode-hook)))
;; 	  (dolist (mode hls)
;; 		(add-hook mode #completion)
;; 		)
;; 	  )))

(with-eval-after-load 'corfu
  (setq corfu-auto t
		corfu-cycle t
		corfu-quit-no-match t  ;; 'separator
		corfu-auto-prefix 2
		corfu-auto-delay 0
		corfu-preview-current t
		)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

(add-hook 'corfu-mode-hook
		  (lambda ()
			(corfu-popupinfo-mode)
			(setq corfu-popupinfo-delay '(0.2 . 0.1))
			))
;; (global-corfu-mode)

(add-hook 'company-mode-hook
		  (lambda ()
			(setq company-tooltip-align-annotations t
				  company-tooltip-limit 12
				  company-idle-delay 0
				  company-echo-delay (if (display-graphic-p) nil 0)
				  company-minimum-prefix-length 1
				  company-icon-margin 3
				  company-require-match nil
				  company-dabbrev-ignore-case nil
				  company-dabbrev-downcase nil
				  company-global-modes '(not erc-mode message-mode help-mode
											 gud-mode eshell-mode shell-mode)
				  company-backends '((company-capf :with company-yasnippet)
									 (company-dabbrev-code company-keywords company-files)
									 company-dabbrev))
			(define-key company-active-map (kbd "C-h") 'delete-backward-char)
			(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
			;; (setq company-box-icons-alist 'company-box-icons-idea)
			(setq company-box-scrollbar 'inherit)
			(company-box-mode t)))

;; (require 'cape)
(with-eval-after-load 'cape (add-to-list 'completion-at-point-functions #'cape-file))

(add-hook 'prog-mode-hook 'toggle-truncate-lines)

(defun company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))

(advice-add 'company-capf :around #'company-completion-styles)

(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet")
(add-to-list 'load-path "~/.emacs.d/site-lisp/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge/")

(require 'lsp-bridge)

;; (with-eval-after-load 'lsp-bridge
;;   (when (treesit-available-p)
;; 	(let ((lsp-bridge-temp-list lsp-bridge-default-mode-hooks))
;; 	  (dolist (hook lsp-bridge-temp-list)
;; 		(add-to-list 'lsp-bridge-default-mode-hooks (intern (string-replace "mode" "ts-mode" (symbol-name hook))))))))

(add-hook 'lsp-bridge-mode-hook '(lambda ()
								   (yas/minor-mode t)
								   (setq lsp-bridge-enable-diagnostics nil
										 lsp-bridge-c-lsp-server "ccls")))

(with-eval-after-load 'lsp-bridge
  (global-lsp-bridge-mode)
  (with-current-buffer (get-buffer-create "*scratch*")
	(lsp-bridge-mode))
  (setq acm-candidate-match-function 'orderless-flex
		;; acm-enable-icon t
		;; acm-enable-doc t
		acm-enable-yas t
		acm-enable-tempel t
		acm-enable-quick-access nil
		acm-enable-search-file-words t
		acm-enable-telega nil
		acm-enable-tabnine nil
		;; lsp-bridge-enable-log t
		lsp-bridge-enable-signature-help t
		lsp-bridge-enable-diagnostics nil
		lsp-bridge-complete-manually nil
		lsp-bridge-enable-profile t
		;; lsp-bridge-multi-lang-server-mode-list nil
		acm-backend-lsp-candidate-min-length 2
		acm-backend-elisp-candidate-min-length 2
		acm-backend-search-file-words-candidate-min-length 3
		acm-backend-yas-candidate-min-length 1

		;; This will cause `org-roam-node-find' get wrong and I don't know why.
		;; lsp-bridge-enable-org-babel t
		))


(with-eval-after-load 'kind-icon
  (setq kind-icon-use-icons nil
		kind-icon-mapping
		`(
		  (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
		  (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
		  (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
		  (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
		  (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
		  (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
		  (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
		  (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
		  (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
		  (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
		  (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
		  (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
		  (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
		  (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
		  (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
		  (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
		  (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
		  (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
		  (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
		  (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
		  (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
		  (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
		  (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
		  (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
		  (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
		  (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
		  (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
		  (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
		  (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
		  (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
		  (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
		  (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
		  (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
		  (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
		  (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
		  (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face))))

(provide 'init-lsp)
;;; init-lsp.el ends here