;;; LaTeX-Node --- Create LaTeX Nodes  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar latex-node-directory "/run/media/kunh/Elements/Personal/LaTeX-nodes/"
  "Directory to store LaTeX files.")

(defvar latex-node-template "~/.emacs.d/lisp/template.tex"
  "Template for LaTeX node.")

(defun latex-node-find-file ()
  "Create or open LaTeX node file."
  (interactive)
  (if (not (file-exists-p latex-node-directory))
      (if (y-or-n-p (format "Whether to create directory %s?" latex-node-directory))
          (make-directory latex-node-directory)))
  (let ((value (find-file-noselect
                (format "%s%s.tex" latex-node-directory
                        (completing-read "Node: "
                                         (let ((files (directory-files latex-node-directory))
                                               (texs ()))
                                           (dolist (file files)
                                             (let ((fname (string-match "\\.tex" file))
                                                   (temp (string-match "_region_.tex" file)))
                                               (when (and fname
                                                          (not temp))
                                                 (setq texs (cons (substring file 0 fname) texs)))))
                                           texs))))))
    (with-current-buffer value
      (save-excursion
        (goto-char (point-min))
        (and (or (= (point-min) (point-max))
                 (string-match-p "^%%%"
                                 (buffer-substring-no-properties
                                  (line-beginning-position 2)
                                  (line-end-position 2))))
             (insert-file-contents latex-node-template))))
    (pop-to-buffer-same-window value)))

(provide 'latex-node)
;;; latex-node.el ends here.
