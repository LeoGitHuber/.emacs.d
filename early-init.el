;; -*- lexical-binding: t; -*-
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
;; 以下方式与上面相同
;; (push '(undecorated . t) default-frame-alist)
;; (push '(vertical-scroll-bar) default-frame-alist)
;; (push '(alpha-background . 85) default-frame-alist)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)

;; (load-theme 'modus-operandi t)

(setq gc-cons-threshold most-positive-fixnum
      package-quickstart nil
      package-enable-at-startup nil
      ;; inhibit-startup-message t
      ;; auto-mode-case-fold nil
      command-line-x-option-alist nil ; Remove irreleant command line options for faster startup
	  )
(setq-default mode-line-format nil)
