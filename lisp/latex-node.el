;;; LaTeX-Node --- Create LaTeX Nodes  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar latex-node-directory "~/Personal/LaTeX-nodes/"
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
                                             (let ((fname (string-match "\.tex" file)))
                                               (when fname
                                                 (setq texs (cons (substring file 0 fname) texs)))))
                                           texs))))))
    (pop-to-buffer-same-window value)
    (unless (file-exists-p (buffer-file-name value))
      (insert-file-contents latex-node-template))))

(provide 'latex-node)
;;; latex-node.el ends here.
