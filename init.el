(add-to-list 'load-path "~/.emacs.d/site-lisp/benchmark-init-el")
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
(setq custom-file "~/.emacs.d/custom.el")
(load "~/.emacs.d/custom.el")
(load "~/.emacs.d/config.el")
