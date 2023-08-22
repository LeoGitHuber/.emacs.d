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

(require 'cns nil t)

(when (featurep 'cns)
  (add-hook 'find-file-hook 'cns-auto-enable)
  (when (bound-and-true-p meow-mode)
    (defun meow-cns-back-word (n)
      "Select to the beginning the previous Nth word.

A non-expandable word selection will be created.
This command works similar to `meow-next-word'."
      (interactive "p")
      (unless (equal 'word (cdr (meow--selection-type)))
        (meow--cancel-selection))
      (let* ((expand (equal '(expand . word) (meow--selection-type)))
             (_ (when expand (meow--direction-backward)))
             (type (if expand '(expand . word) '(select . word)))
             (m (point))
             (p (save-mark-and-excursion
                  (when (cns-backward-word n)
                    (point)))))
        (when p
          (thread-first
            (meow--make-selection type (meow--fix-word-selection-mark p m) p expand)
            (meow--select))
          (meow--maybe-highlight-num-positions '(meow--backward-word-1 . meow--forward-word-1)))))

    (defun meow-cns-next-word (n)
      "Select to the end of the next Nth word.

A non-expandable, word selection will be created.

To select continuous words, use following approaches:

1. start the selection with `meow-mark-word'.

2. use prefix digit arguments.

3. use `meow-expand' after this command.
"
      (interactive "p")
      (unless (equal 'word (cdr (meow--selection-type)))
        (meow--cancel-selection))
      (let* ((expand (equal '(expand . word) (meow--selection-type)))
             (_ (when expand (meow--direction-forward)))
             (type (if expand '(expand . word) '(select . word)))
             (m (point))
             (p (save-mark-and-excursion
                  (when (cns-forward-word n)
                    (point)))))
        (when p
          (thread-first
            (meow--make-selection type (meow--fix-word-selection-mark p m) p expand)
            (meow--select))
          (meow--maybe-highlight-num-positions '(meow--backward-word-1 . meow--forward-word-1)))))


    ;; (add-hook 'cns-mode-hook
    ;;           (lambda ()
    ;;             (meow-normal-define-key
    ;;              '("b" . meow-cns-back-word)
    ;;              '("e" . meow-cns-next-word)
    ;;              )))
    ))

(provide 'init-chinese)
;;; init-chinese.el ends here.
