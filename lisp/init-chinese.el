;;; init-chinese --- Init for Chinese Environment
;;; Commentary:
;;; Code:

;;; Emacs-Chinese

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation")
(setq cns-prog "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cnws"
      cns-dict-directory "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cppjieba/dict"
      ;; To use other program for word segmentation, set cns-process-shell-command:
      ;; cns-process-shell-command "word_segmentation_program arg1 arg2..."
      ;; disable debug output, default is t
      cns-recent-segmentation-limit 20
      cns-debug nil)

;; (require 'cns nil t)

(when (featurep 'cns)
  (add-hook 'find-file-hook 'cns-auto-enable))

(provide 'init-chinese)
;;; init-chinese.el ends here.
