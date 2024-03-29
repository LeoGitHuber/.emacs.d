;;; init-lsp -- Init for Lsp
;;; Commentary:
;;; Code:

;;; Coding Relpated

(defun lsp-enable-startup ()
  "Enable `eglot' or `lsp-mode' for LSP."
  (dolist (hook '(prog-mdoe-hook cuda-mode-hook TeX-mode-hook c-ts-mode-hook c++-ts-mode-hook))
    (add-hook hook (lambda ()
                     (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode
                                             'verilog-mode
                                             'makefile-mode 'snippet-mode)
                       ;; (lsp-deferred)
                       (eglot-ensure)
                       )))))

;; (lsp-enable-startup)

(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'lsp-ui
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
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
        ;; lsp-lens-enable nil

        ;; ui
        ;; lsp-ui-doc-show-with-cursor nil

        lsp-completion-provider :none
        lsp-prefer-flymake t
        lsp-ui-flycheck-enable nil
        lsp-enable-relative-indentation t
        )
  ;; (keymap-set lsp-mode-map "C-c C-d" 'lsp-describe-thing-at-point)
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (add-hook 'lsp-mode-hook 'corfu-mode))

(with-eval-after-load 'corfu
  (setq corfu-auto t
        corfu-cycle t
        ;; corfu-quit-no-match 'separator  ;; t
        corfu-auto-prefix 1
        corfu-auto-delay 0
        corfu-preview-current t
        corfu-quit-no-match t
        ;; corfu-preselect 'prompt
        corfu-quit-at-boundary t)
  (when (boundp 'meow-insert-exit-hook)
    (add-hook 'meow-insert-exit-hook 'corfu-quit))
  (keymap-set corfu-map "<tab>" 'corfu-insert)
  ;; (keymap-set corfu-map "<backtab>" 'corfu-previous)
  ;; (keymap-set corfu-map "S-<return>" 'corfu-insert)
  ;; (keymap-unset corfu-map "RET")
  (add-to-list 'completion-at-point-functions 'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  (with-eval-after-load 'corfu-popupinfo
    (setq corfu-popupinfo-delay '(0.1 . 0.1)))
  ;; (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)
  (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter))

(with-eval-after-load 'yasnippet
  (add-hook 'yas-keymap-disable-hook
            (lambda()
              (or
               (when (boundp 'corfu--frame)
                 (and
                  (frame-live-p corfu--frame)
                  (frame-visible-p corfu--frame)))
               (when (boundp 'acm-menu-frame)
                 (and (frame-live-p acm-menu-frame)
                      (frame-visible-p acm-menu-frame)))
               ))))

(with-eval-after-load 'eglot
  (setq eglot-send-changes-idle-time 0)
  (add-to-list 'eglot-server-programs
               '((tex-mode context-mode texinfo-mode bibtex-mode)
                 . ("digestif")))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-hook 'eglot-managed-mode-hook 'corfu-mode)
  (add-hook 'eglot-managed-mode-hook 'yas-minor-mode)
  (eglot-booster-mode))

;; (dolist (completion '(company-mode corfu-mode))
;;   (with-eval-after-load completion
;;   (let ((hls '(emacs-lisp-mode-hook lisp-mode-hook TeX-mode-hook verilog-mode-hook)))
;;     (dolist (mode hls)
;;     (add-hook mode #completion)
;;     )
;;     )))

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

;; (defun company-completion-styles (capf-fn &rest args)
;;   (let ((completion-styles '(basic partial-completion)))
;;     (apply capf-fn args)))

;; (advice-add 'company-capf :around #'company-completion-styles)

(with-eval-after-load 'lsp-bridge
  (add-hook 'lsp-bridge-mode-hook 'yas/minor-mode)
  (keymap-set yas-keymap "<tab>" 'acm-complete-or-expand-yas-snippet)
  (setq acm-candidate-match-function 'orderless-flex
        ;; acm-enable-icon t
        ;; acm-enable-doc t
        acm-enable-yas t
        acm-enable-tempel t
        acm-enable-quick-access nil
        acm-enable-search-file-words t
        acm-enable-telega nil
        acm-enable-tabnine nil
        acm-enable-citre t
        ;; lsp-bridge-enable-log t
        lsp-bridge-enable-signature-help t
        lsp-bridge-enable-diagnostics nil
        lsp-bridge-complete-manually nil
        ;; lsp-bridge-enable-profile t
        ;; lsp-bridge-multi-lang-server-mode-list nil
        acm-backend-lsp-candidate-min-length 2
        acm-backend-elisp-candidate-min-length 2
        acm-backend-search-file-words-candidate-min-length 3
        acm-backend-yas-candidate-min-length 1
        lsp-bridge-python-command "python"
        ;; This will cause `org-roam-node-find' get wrong and I don't know why.
        ;; lsp-bridge-enable-org-babel t
        ;; lsp-bridge-c-lsp-server "clangd"
        ;; lsp-bridge-user-langserver-dir "~/.emacs.d/lisp/langserver"
        ;; lsp-bridge-user-multiserver-dir "~/.emacs.d/lisp/multilangserver"
        )
  ;; (add-to-list 'lsp-bridge-multi-lang-server-mode-list
  ;;              '((verilog-mode) . "verilog"))
  ;; (add-to-list 'lsp-bridge-multi-lang-server-extension-list
  ;;              '(("v" "sv") . "verilog"))
  ;; (setf (cdr (assoc 'verilog-mode lsp-bridge-single-lang-server-mode-list)) '("svlangserver"))
  ;; (add-to-list 'lsp-bridge-single-lang-server-mode-list '((verilog-mode) . "svlangserver"))
  ;; (add-to-list 'lsp-bridge-single-lang-server-mode-list '((verilog-mode) . "verible"))
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '((verilog-mode) . "veridian"))
  ;; (add-to-list 'lsp-bridge-single-lang-server-mode-list '((verilog-mode) . "svls"))
  )

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
