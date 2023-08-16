;;; init-gc --- Init for GC

;;; Commentary:

;;; Code:

(defvar better-gc-cons-threshold (* 32 1024 1024) ;; 128mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.
If you experience stuttering, increase this.")

;; 开启 minibuffer 的时候不要 gc
(defun gc-minibuffer-setup-hook ()
  "Turn off garbage collection during setup minibuffer."
  (setq-default gc-cons-threshold (* better-gc-cons-threshold 2)))

(defun gc-minibuffer-exit-hook ()
  "Turn on garbage collection after minibuffer exit."
  (garbage-collect)
  (setq-default gc-cons-threshold better-gc-cons-threshold))

;;; init-gc.el ends here.
