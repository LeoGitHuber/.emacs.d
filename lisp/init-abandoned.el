;;; init-abandoned --- Init for Abandoned Setting
;;; Commentary:
;;; Code:

(with-eval-after-load 'evil
  (evil-set-undo-system 'undo-redo)
  (setq-default evil-want-abbrev-expand-on-insert-exit nil)
  (setq-default evil-want-keybinding nil)
  (evil-define-key 'normal sort-tab-mode-map (kbd "[ b")
                   'sort-tab-select-prev-tab)
  (evil-define-key 'normal sort-tab-mode-map (kbd "] b")
                   'sort-tab-select-next-tab)
  (evil-define-key 'normal sort-tab-mode-map (kbd "[ B")
                   'sort-tab-select-first-tab)
  (evil-define-key 'normal sort-tab-mode-map (kbd "] B")
                   'sort-tab-select-last-tab)
  (require 'evil-nerd-commenter)
  (keymap-global-set "M-/" 'evil-comment-or-uncomment-lines)
  ;; (evil-define-key 'insert 'prog-mode-map (kbd "TAB") 'insert-tab-char)
  (evil-define-key '(insert normal) org-mode-map (kbd "TAB")
                   'org-cycle)
  (require 'evil-nerd-commenter))

;; (require 'gcmh)
;; (with-eval-after-load 'gcmh (setq gcmh-auto-idle-delay-factor 10
;; 								  gcmh-high-cons-threshold (* 16 1024 1024)))
;; (gcmh-mode t)

;;; Abandoned Setting

;; (dolist (hook '(prog-mode-hook text-mode-hook cuda-mode-hook))
;;   (add-hook hook 'rainbow-mode))

;;; Emacs-application-framework

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework")
;; (require 'eaf)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (with-eval-after-load 'evil-mode (require 'eaf-evil))
;; (setq-default eaf-webengine-default-zoom 2.0)
;; (add-hook 'eaf-mode-hook '(lambda() ((local-set-key (kbd "\C-h") 'backward-delete-char) (local-set-key (kbd "\M-?") 'help-command))))

;;; Eshell

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/aweshell")
;; (require 'aweshell)

;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-14"))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'python-mode-hook 'eglot-ensure)

;;; Lsp-mode
;; (add-hook 'c-mode-hook #'lsp)
;; (add-hook 'c-mode-hook (lambda () (require 'ccls) (lsp-mode)))
;; (setq ccls-executable "ccls")
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

;; (global-company-mode)
;; (add-hook 'prog-mode-hook 'company-mode)
;; (add-hook 'emacs-lisp-mode 'company-mode)
;; (add-hook 'company-mode-hook 'company-box-mode)
;; (add-hook 'lsp-mode-hook 'company-mode)
;; (add-hook 'company-mode-hook
;;           (lambda ()
;;             (setq company-tooltip-align-annotations t
;;                   company-tooltip-limit 12
;;                   company-idle-delay 0
;;                   company-echo-delay (if (display-graphic-p) nil 0)
;;                   company-minimum-prefix-length 1
;;                   company-icon-margin 3
;;                   company-require-match nil
;;                   company-dabbrev-ignore-case nil
;;                   company-dabbrev-downcase nil
;;                   company-global-modes '(not erc-mode message-mode help-mode
;;                                              gud-mode eshell-mode shell-mode)
;;                   company-backends '((company-capf :with company-yasnippet)
;;                                      (company-dabbrev-code company-keywords company-files)
;;                                      company-dabbrev))
;;             (define-key company-active-map (kbd "C-h") 'delete-backward-char)
;;             (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
;;             ;; (setq company-box-icons-alist 'company-box-icons-idea)
;;             (setq company-box-scrollbar 'inherit)
;;             (company-box-mode t)
;;             (advice-add 'company-capf :around #'company-completion-styles)))

;; (defun company-completion-styles (capf-fn &rest args)
;;   (let ((completion-styles '(basic partial-completion)))
;;     (apply capf-fn args)))


;; (add-to-list 'load-path "~/.emacs.d/site-lisp/awesome-tab")
;; (require 'awesome-tab)
;; (awesome-tab-mode t)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/sort-tab")
;; (require 'sort-tab)
;; (with-eval-after-load 'sort-tab
;; (sort-tab-mode t)
;; (define-key sort-tab-mode-map (kbd "C-]") 'sort-tab-select-next-tab)
;; (keymap-set sort-tab-mode-map "M-[" 'sort-tab-select-prev-tab)
;; (centaur-tabs-mode t)
;; (setq centaur-tabs-cycle-scope 'tabs)
;; (setq centaur-tabs-style "bar")
;; (setq centaur-tabs-set-icons t)
;; (setq centaur-tabs-set-bar 'left)
;;  )

(provide 'init-abandoned)
;;; init-abandoned.el ends here.
