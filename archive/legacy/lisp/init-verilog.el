;;; init-verilog --- Init for Verilog ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'verilog-mode
  ;; (with-eval-after-load 'lsp-mode
  ;;   (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
  ;;   (lsp-register-client
  ;;    (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
  ;;   				  :major-modes '(verilog-mode)
  ;;   				  :priority -1)))
  (with-eval-after-load 'lsp-mode
    (require 'lsp-verilog)
    (custom-set-variables
     '(lsp-clients-svlangserver-launchConfiguration "verilator -sv --lint-only -Wall")
     '(lsp-clients-svlangserver-formatCommand "verible-verilog-format")))
  (setq verilog-indent-lists nil
        verilog-auto-delete-trailing-whitespace t
        verilog-align-ifelse t
        verilog-auto-delete-trailing-whitespace t
        verilog-auto-inst-param-value t
        verilog-auto-inst-vector nil
        verilog-auto-lineup (quote all)
        ;; verilog-auto-newline nil
        verilog-auto-save-policy nil
        verilog-auto-template-warn-unused t
        verilog-case-indent 2
        verilog-cexp-indent 2
        verilog-highlight-grouping-keywords t
        verilog-highlight-modules t
        verilog-indent-level 2
        verilog-indent-level-behavioral 2
        verilog-indent-level-declaration 2
        verilog-indent-level-module 2
        ;; verilog-tab-to-comment t
        )
  (setq verilog-ext-feature-list
        '(;; font-lock
          xref
          capf
          hierarchy
          eglot
          ;; lsp
          flycheck
          ;; beautify
          navigation
          template
          ;; formatter
          compilation
          ;; imenu
          which-func
          hideshow
          typedefs
          time-stamp
          block-end-comments
          ports))
  (require 'verilog-ext)
  (verilog-ext-mode-setup)
  (verilog-ext-eglot-set-server 've-veridian)
  ;; (add-hook 'verilog-mode-hook
  ;;           (lambda ()
  ;;             (eglot-ensure)
  ;;             (add-to-list 'eglot-server-programs
  ;;                          ;; '(verilog-mode . ("svlangserver"))
  ;;                          '(verilog-mode . ("svls"))
  ;;                          ;; '(verilog-mode . ("vls"))
  ;;                          ;; '(verilog-mode . ("veridian"))
  ;;                          )
  ;;             ;; (setq eglot-workspace-configuration
  ;;             ;;       '(:veridian
  ;;             ;;         (:settings
  ;;             ;;          (:syntax
  ;;             ;;           (:enabled :json-false)
  ;;             ;;           ;; (:enabled :json-true
  ;;             ;;           ;;           :path "verible-verilog-syntax")
  ;;             ;;           :format:
  ;;             ;;           (:enabled :json-true
  ;;             ;;                     :path "verible-verilog-format")
  ;;             ;;           ;; :diagnostics
  ;;             ;;           ;; (:enabled :json-false)
  ;;             ;;           ))))
  ;;             ;; (setq eglot-workspace-configuration
  ;;             ;;       '(:svlangserver
  ;;             ;;         (:settings
  ;;             ;;          (:systemverilog.includeIndexing:
  ;;             ;;           ["**/*.{sv,svh,v,vh}", "*.{sv,svh,v,vh}"],
  ;;             ;;           :systemverilog.launchConfiguration:
  ;;             ;;           "verilator -sv -Wall --lint-only",
  ;;             ;;           :systemverilog.formatCommand:
  ;;             ;;           "verible-verilog-format"))))
  ;;             (setq eglot-workspace-configuration
  ;;                   '(:svls
  ;;                     (:settings
  ;;                      (:systemverilog.launchConfiguration:
  ;;                       "verilator -sv -Wall --lint-only",
  ;;                       :systemverilog.formatCommand:
  ;;                       "verible-verilog-format"))))
  ;;             )
  ;;           )
  )


;;; init-verilog.el ends here.
