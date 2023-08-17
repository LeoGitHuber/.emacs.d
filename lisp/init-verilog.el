;;; init-verilog --- Init for Verilog ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'verilog-mode
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
					  :major-modes '(verilog-mode)
					  :priority -1)))
  (setq verilog-indent-lists nil)
  )

;; (add-hook 'verilog-mode-hook '(lambda ()
;; (lsp)
;; (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
;; ))

;;; init-verilog.el ends here.
