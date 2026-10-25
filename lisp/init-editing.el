;;; init-editing.el --- Shared editing behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep buffer policy in named functions so it can be tested and reloaded.

;;; Code:

(require 'hl-line)

(defun my/hl-line-buffer-p (buffer)
  "Return non-nil when BUFFER should highlight its current line."
  (with-current-buffer buffer
    (not (or (minibufferp)
             (string-prefix-p " " (buffer-name))
             (bound-and-true-p cursor-face-highlight-mode)
             (derived-mode-p 'eshell-mode 'shell-mode 'term-mode 'eat-mode
                             'messages-buffer-mode 'org-mode 'markdown-mode
                             'markdown-view-mode 'markdown-ts-mode
                             'markdown-ts-view-mode 'etaf-manual-reader-mode
                             'nov-mode 'tex-mode
                             'TeX-mode 'LaTeX-mode)))))

;; Emacs 31 provides a public buffer predicate for the global mode.
(when (boundp 'global-hl-line-buffers)
  (setq global-hl-line-buffers #'my/hl-line-buffer-p))
(global-hl-line-mode 1)

(defun enable-buffer-local-trailing-whitespace-cleanup ()
  "Clean trailing whitespace except where it has Markdown semantics."
  (unless (derived-mode-p 'markdown-mode 'markdown-view-mode
                          'markdown-ts-mode 'markdown-ts-view-mode)
    (delete-trailing-whitespace-mode 1)))

(dolist (hook '(text-mode-hook conf-mode-hook yaml-ts-mode-hook
                toml-ts-mode-hook json-ts-mode-hook emacs-lisp-mode-hook))
  (add-hook hook #'enable-buffer-local-trailing-whitespace-cleanup))

(defun my/scala-mode ()
  "Use Scala Tree-sitter when its grammar is ready, else use scala-mode."
  (interactive)
  (if (and (fboundp 'treesit-ready-p) (treesit-ready-p 'scala t))
      (scala-ts-mode)
    (scala-mode)))

;; Keep file completion flexible with Emacs 31's partial-completion matcher.
(when (boundp 'completion-pcm-leading-wildcard)
  (setq completion-pcm-leading-wildcard t))

;; Limit costly display features when opening files with very long lines.
(global-so-long-mode 1)

(provide 'init-editing)
;;; init-editing.el ends here
