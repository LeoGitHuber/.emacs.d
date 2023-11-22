;;; init-dired --- Init for Dired
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish")
(add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish/extensions")
(add-hook 'dired-mode-hook
          (lambda ()
            (or (boundp 'diredfl-mode)
                (load "~/.emacs.d/site-lisp/diredfl/diredfl.el"))
            (toggle-truncate-lines)
            (diredfl-mode)
            ;; (require 'image-dired)
            ;; (require 'dirvish)
            ))
(with-eval-after-load 'dired
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group"
        dired-dwim-target t
        dired-mouse-drag-files t
        dired-auto-revert-buffer t
        dired-do-revert-buffer t
        mouse-drag-and-drop-region-cross-program t
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-copies 'always
        ;; dired-recursive-deletes 'always
        delete-by-moving-to-trash t
        image-dired-thumb-size 256
        image-dired-marking-shows-next nil)
  (defun dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (embark-open-externally (dired-get-filename))
     arg))
  (keymap-set dired-mode-map "e" 'dired-open-externally))

(with-eval-after-load 'dirvish
  ;; (dirvish-peek-mode)
  ;; (require 'dirvish-side)
  (dirvish-override-dired-mode)
  (dirvish-side-follow-mode)
  ;; (add-hook 'dirvish-setup-hook 'dirvish-emerge-mode)
  (setq dirvish-attributes '(vc-state nerd-icons file-size subtree-state collapse file-time)
		dirvish-side-width 35
        dirvish-emerge-groups '(("Recent files" (predicate . recent-files-2h))
                                ("Video" (extensions "mp4" "mkv" "webm"))
                                ("Pictures" (extensions "jpg" "png" "jpeg" "svg" "gif"))
                                ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
                                ("Archives" (extensions "gz" "rar" "zip")))
        dirvish-path-separators '(" ~" " /" "/")
        ;; dirvish-hide-details nil
        dirvish-mode-line-height 20
        ;; dirvish-show-media-properties t
        ;; Turn off media cache, but it will slow down the speed of media preview
        dirvish-media-auto-cache-threshold nil
        ;; dirvish-preview-dispatch (remove 'epub dirvish-preview-dispatch)
        )
  (keymap-set dirvish-mode-map "TAB" #'dirvish-toggle-subtree))

(provide 'init-dired)
;;; init-dired.el ends here.
