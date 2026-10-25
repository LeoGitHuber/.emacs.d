;;; init-navigation.el --- Project trees and file tabs -*- lexical-binding: t; -*-

;;; Commentary:
;; Tabspaces owns workspace membership; tab-line owns each editor's file order.
;; A single native tab-bar displays files and the current workspace menu.
;; Dirvish already scopes its side sessions to native tab-bar tabs.

;;; Code:

(require 'project)
(require 'color)
(require 'tab-line)
(require 'tabspaces)
(require 'consult)
(require 'dirvish-side)

(defun my/dirvish-process-buffer-name (arguments)
  "Give Dirvish a buffer name instead of asking for a temporary file name."
  (unless (nth 2 arguments)
    (setf (nth 2 arguments) (generate-new-buffer-name " *dirvish-batch*")))
  arguments)

;; NeoEmacs on Windows validates make-temp-name as a disk path, rejecting `*'.
(when (and (eq system-type 'windows-nt) (string-match-p "neo" invocation-name))
  (advice-add 'dirvish--make-proc :filter-args #'my/dirvish-process-buffer-name))

(setq project-vc-extra-root-markers '("Cargo.toml" ".dir-locals.el" "build.mill" ".project")
      tabspaces-initialize-project-with-todo nil
      tabspaces-initialize-project-with-vc nil
      tabspaces-remove-to-default nil
      ;; Desktop already owns persistence across Emacs restarts.
      tabspaces-session nil
      tabspaces-session-auto-restore nil
      tab-bar-show t
      ;; Reserve Ctrl+Tab for files, rather than tab-bar's workspace switching.
      tab-bar-define-keys 'numeric
      tab-bar-auto-width nil
      tab-bar-new-tab-choice "*scratch*")

(tabspaces-mode 1)
(tab-bar-mode 1)
(when (= (length (tab-bar-tabs)) 1)
  (tab-bar-rename-tab tabspaces-default-tab))
(tab-bar-history-mode 1)

(defun my/workspace-root (&optional prompt)
  "Return the selected workspace's root, falling back to project.el.
With PROMPT, ask for a project if no context is available."
  (or (car (rassoc (alist-get 'name (assq 'current-tab (tab-bar-tabs)))
                   tabspaces-project-tab-map))
      (when-let* ((project (project-current prompt)))
        (project-root project))))

(defun my/workspace-file-buffers (&optional frame)
  "Return the file buffers belonging to FRAME's current workspace."
  (seq-filter #'buffer-file-name (tabspaces-local-buffer-list frame)))

(defun my/tab-line-buffers (buffers)
  "Keep workspace files in BUFFERS, preserving their window-local order."
  (let ((files (my/workspace-file-buffers)))
    (seq-filter (lambda (buffer) (memq buffer files))
                (tab-line-tabs-non-excluded buffers))))

(setq tab-line-tabs-function #'tab-line-tabs-fixed-window-buffers
      tab-line-tabs-window-buffers-filter-function #'my/tab-line-buffers
      tab-line-exclude-buffers (lambda (buffer) (not (buffer-file-name (get-buffer buffer))))
      tab-line-close-tab-function #'kill-buffer
      tab-line-close-button-show 'selected
      tab-line-new-button-show nil
      tab-line-switch-cycling t
      consult-buffer-list-function #'my/workspace-file-buffers
      consult-buffer-sources '(consult-source-buffer)
      consult-project-function #'my/workspace-root
      popper-group-function #'my/workspace-root)
(global-tab-line-mode -1)
;; Desktop restores buffer-local modes after init, including old tab lines.
;; Ignore the old entries on restore and omit this mode from future saves.
(with-eval-after-load 'desktop
  (add-to-list 'desktop-minor-mode-handlers '(tab-line-mode . ignore))
  (add-to-list 'desktop-minor-mode-table '(tab-line-mode nil)))

(defconst my/navigation-padding
  " "
  "Horizontal padding that leaves navigation height to the font.")

(defun my/navigation-truncate (name width)
  "Fit NAME in WIDTH columns, keeping a recognizable beginning and end."
  (let ((columns (string-width name))
        (tail (min 12 (- width 4))))
    (if (> columns width)
        (concat (truncate-string-to-width name (- width tail 1))
                "…" (truncate-string-to-width name columns (- columns tail)))
      name)))

(defun my/navigation-workspace-label ()
  "Return the compact current workspace label for the right-hand menu."
  (propertize
   (concat " " (my/navigation-truncate
                (alist-get 'name (assq 'current-tab (tab-bar-tabs)))
                (max 6 (min 24 (/ (frame-width) 3))))
           " ▾ ")
   'face 'tab-bar-tab-inactive))

(defun my/navigation-workspace-menu ()
  "Select a workspace using the native popup menu."
  (interactive)
  (popup-menu
   (cons "Workspaces"
         (mapcar (lambda (tab)
                   (let ((name (alist-get 'name tab)))
                     (vector name
                             (lambda () (interactive)
                               (tab-bar-select-tab-by-name name))
                             :style 'radio :selected
                             (eq (car tab) 'current-tab))))
                 (tab-bar-tabs)))))

(defun my/tab-bar-format-workspace ()
  "Show only the current workspace, with all workspaces in its menu."
  `((workspace menu-item ,(my/navigation-workspace-label)
               my/navigation-workspace-menu :help "Switch workspace")))

(defun my/navigation-file-name (buffer &optional _buffers)
  "Fit BUFFER's label to the frame, keeping the distinguishing tail."
  (concat " " (my/navigation-truncate
               (buffer-name buffer)
               (max 6 (min 28 (- (frame-width)
                                 (string-width (my/navigation-workspace-label))
                                 10))))
          (if (buffer-modified-p buffer) " • " " ")))

(defun my/navigation-select-file (window buffer)
  "Show BUFFER in its captured editor WINDOW."
  (when (and (my/editor-window-p window) (buffer-live-p buffer))
    (select-window window)
    (switch-to-buffer buffer)
    (my/remember-editor-window)))

(defun my/tab-bar-format-files ()
  "Display the recent editor's file tabs, keeping its current file visible."
  (when-let* ((window (my/navigation-editor-window)))
    (with-selected-window window
      (let* ((buffers (funcall tab-line-tabs-function))
             (current (window-buffer window))
             (entries
              (mapcar
               (lambda (buffer)
                 (let* ((face (if (eq buffer current)
                                  'tab-line-tab-current 'tab-line-tab-inactive))
                        (label (propertize (my/navigation-file-name buffer) 'face face))
                        (items
                         `((,(make-symbol "file") menu-item ,label
                            ,(lambda () (interactive)
                               (my/navigation-select-file window buffer))
                            :help ,(buffer-file-name buffer)))))
                   (when (eq buffer current)
                     (setq items
                           (append items
                                   `((file-close menu-item ,(propertize "× " 'face face)
                                      ,(lambda () (interactive) (kill-buffer buffer))
                                      :help "Close file buffer")))))
                   (list buffer items
                         (string-pixel-width (mapconcat (lambda (item) (nth 2 item))
                                                       items)))))
               buffers))
             (visible (copy-sequence entries))
             (overflow (propertize " … " 'face 'tab-bar-tab-inactive))
             ;; Leave room for the workspace, overflow menu and native edges.
             (budget (- (frame-inner-width)
                        (string-pixel-width (my/navigation-workspace-label))
                        (string-pixel-width (concat overflow "  "))))
             (width (apply #'+ (mapcar #'caddr visible))))
        (while (and (> width budget) (cdr visible))
          (let* ((index (or (seq-position (mapcar #'car visible) current) 0))
                 (drop-first (> index (/ (1- (length visible)) 2)))
                 (removed (if drop-first (car visible) (car (last visible)))))
            (setq visible (if drop-first (cdr visible) (butlast visible))
                  width (- width (nth 2 removed)))))
        (append
         (apply #'append (mapcar #'cadr visible))
         (when (< (length visible) (length entries))
           `((files-overflow menu-item ,overflow
              ,(lambda ()
                 (interactive)
                 (popup-menu
                  (cons "Files"
                        (mapcar
                         (lambda (buffer)
                           (vector (buffer-name buffer)
                                   (lambda () (interactive)
                                     (my/navigation-select-file window buffer))
                                   :style 'radio :selected (eq buffer current)))
                         buffers))))
              :help "All files in this editor"))))))))

(defun my/navigation-blend (foreground background alpha)
  "Blend theme FOREGROUND into BACKGROUND by ALPHA, tolerating terminal colors."
  (let ((fg (color-name-to-rgb foreground))
        (bg (color-name-to-rgb background)))
    (if (and fg bg)
        (apply #'color-rgb-to-hex (color-blend fg bg alpha))
      foreground)))

(setq tab-bar-separator " "
      tab-bar-format '(my/tab-bar-format-files tab-bar-format-align-right
                      my/tab-bar-format-workspace)
      window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)

(defun my/navigation-refresh-faces (&rest _)
  "Give native navigation flat, padded faces using the active theme's colors."
  (unless (bound-and-true-p my/theme-refresh-suppressed)
    (let* ((background (face-background 'default nil t))
           (foreground (face-foreground 'default nil t))
           (surface (my/navigation-blend foreground background 0.06))
           (muted (my/navigation-blend foreground background 0.78))
           (accent (face-foreground 'font-lock-keyword-face nil t)))
      (dolist (face '(tab-bar tab-bar-tab tab-bar-tab-inactive tab-bar-tab-highlight
                      tab-line tab-line-active tab-line-inactive tab-line-tab
                      tab-line-tab-current tab-line-tab-inactive tab-line-highlight))
        (when (facep face)
          (set-face-attribute face nil :inherit 'fixed-pitch
                              :family (face-attribute 'fixed-pitch :family nil t)
                              :foreground muted :background surface
                              :height (face-attribute 'fixed-pitch :height nil t)
                              :weight 'normal :slant 'normal
                              :underline nil :overline nil :inverse-video nil :box nil)))
      (set-face-attribute 'tab-bar-tab nil :foreground foreground
                          :background background :weight 'semibold)
      (set-face-attribute 'tab-line-tab-current nil :foreground accent
                          :background background :weight 'semibold)
      (set-face-attribute 'tab-line-tab nil :foreground foreground
                          :background background)
      (set-face-attribute 'tab-line nil :foreground foreground)
      (dolist (face '(window-divider window-divider-first-pixel window-divider-last-pixel))
        (set-face-attribute face nil
                            :foreground (my/navigation-blend foreground background 0.22)))
      (dolist (face '(tab-bar-tab-highlight tab-line-highlight))
        (set-face-attribute face nil :foreground accent))
      ;; Native SVG buttons carry their own `shadow' face, which some themes
      ;; make too dark even when their surrounding label is readable.
      (dolist (button (list tab-bar-close-button tab-line-close-button
                            tab-line-close-modified-button
                            tab-line-left-button tab-line-right-button))
        (put-text-property 0 (length button) 'face
                           (list :foreground muted) button))
      ;; Some themes disable this mode through their custom variables.
      (window-divider-mode 1)
      (tab-line-force-update t)
      (force-mode-line-update t))))

(add-hook 'enable-theme-functions #'my/navigation-refresh-faces)
(add-hook 'disable-theme-functions #'my/navigation-refresh-faces)
(my/navigation-refresh-faces)

(defun my/consult-all-buffers ()
  "Search all buffers, recent files and bookmarks across workspaces."
  (interactive)
  (let ((consult-buffer-list-function #'buffer-list))
    (consult-buffer '(consult-source-buffer consult-source-recent-file
                      consult-source-bookmark))))

(defun my/project-find-file ()
  "Find a file in the workspace project, even from a reference file."
  (interactive)
  (let* ((default-directory (or (my/workspace-root t) default-directory))
         (project-current-directory-override default-directory))
    (call-interactively #'project-find-file)))

(defun my/editor-window-p (window)
  "Whether WINDOW is a live ordinary editor in the selected frame."
  (and (window-live-p window)
       (eq (window-frame window) (selected-frame))
       (not (window-minibuffer-p window))
       (not (window-parameter window 'window-side))
       (not (window-dedicated-p window))
       (not (window-no-other-p window))))

(defun my/remember-editor-window (&rest _)
  "Remember editor focus before Dirvish temporarily selects other windows."
  (when (my/editor-window-p (selected-window))
    (set-frame-parameter nil 'my/editor-window (selected-window))))

(defun my/navigation-editor-window ()
  "Return the current or most recently used ordinary editor in this frame."
  (let ((remembered (frame-parameter nil 'my/editor-window))
        (windows (seq-filter #'my/editor-window-p (window-list nil 'nomini))))
    (cond ((my/editor-window-p (selected-window)) (selected-window))
          ((my/editor-window-p remembered) remembered)
          (t (car (sort windows (lambda (a b)
                                 (> (window-use-time a) (window-use-time b)))))))))

(defun my/dirvish-select-editor ()
  "Select the most recently used ordinary editor in the current frame."
  ;; Dirvish can temporarily select another editor before invoking this action.
  (let ((remembered (frame-parameter nil 'my/editor-window)))
    (select-window (or (and (my/editor-window-p remembered) remembered)
                       (my/navigation-editor-window)
                       (user-error "No editor window available")))))

(defun my/dirvish-toggle ()
  "Toggle the sidebar rooted at the current workspace's project."
  (interactive)
  (if-let* ((window (dirvish-side--session-visible-p)))
      (with-selected-window window (dirvish-quit))
    (let* ((root (or (my/workspace-root) default-directory))
           (dirvish-side-width (min 32 (max 20 (/ (frame-width) 4))))
           ;; A reference outside this root must not redirect the tree.
           (buffer-file-name (and buffer-file-name
                                  (file-in-directory-p buffer-file-name root)
                                  buffer-file-name)))
      (dirvish-side root))))

(defun my/workspace-open-tree ()
  "Open the new workspace's tree and leave focus in its editor."
  (interactive)
  (unless (dirvish-side--session-visible-p)
    (my/dirvish-toggle))
  (my/dirvish-select-editor))

(defun my/project-workspace (directory)
  "Switch to DIRECTORY's workspace, creating it if needed.
Plain folders are remembered as transient projects without creating files."
  (interactive (list (project-prompt-project-dir)))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (project (or (let ((tabspaces-project-fallback-to-tab nil))
                        (project-current nil directory))
                      (cons 'transient directory)))
         (tabspaces-project-switch-commands #'my/workspace-open-tree))
    (project-remember-project project)
    (tabspaces-open-or-create-project-and-workspace (project-root project))))

(defun my/dirvish-follow-file (&rest _)
  "Locate the selected file inside the visible tree without changing roots."
  (when-let* (((not (active-minibuffer-window)))
              (file (buffer-file-name (window-buffer (selected-window))))
              ((not (file-remote-p file)))
              (window (dirvish-side--session-visible-p)))
    (with-selected-window window
      (when (and (file-in-directory-p file default-directory)
                 (not (equal file (dired-get-filename nil t))))
        (dirvish-subtree-expand-to file)))))

;; Upstream follow mode also changes roots using vc-root-dir.  Use its
;; subtree command on selection changes so reference files keep this tree intact.
(dirvish-side-follow-mode -1)
(setq dirvish-side-open-file-action #'my/dirvish-select-editor
      dirvish-side-width 32
      dirvish-side-header-line-format '(:left (my-workspace))
      dirvish-side-attributes (if my/use-nerd-icons-p
                                  '(nerd-icons subtree-state)
                                '(subtree-state)))
(dirvish-define-mode-line my-workspace
  "Show the tree's root name, including non-Git projects."
  (propertize (concat my/navigation-padding (file-name-nondirectory
                           (directory-file-name default-directory)))
              'face '(:weight semibold)))

(defvar-keymap my/dirvish-side-map
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'dirvish-subtree-toggle-or-open
  "<mouse-2>" #'dirvish-subtree-toggle-or-open
  "<double-mouse-1>" #'ignore)

(defun my/dirvish-side-entry (&rest _)
  "Use single-click navigation only in Dirvish side sessions."
  (when-let* ((session (dirvish-curr)) ((eq (dv-type session) 'side)))
    ;; Dired's follow-link property otherwise converts mouse-1 to mouse-2,
    ;; invoking dired-mouse-find-file-other-window and bypassing this binding.
    (setq-local mouse-1-click-follows-link nil
                line-spacing 0.15
                dirvish-use-mode-line nil
                mode-line-format nil
                dirvish-header-line-height 1)
    (face-remap-set-base 'header-line 'tab-line)
    (face-remap-set-base 'header-line-active 'tab-line)
    (face-remap-set-base 'header-line-inactive 'tab-line)
    (use-local-map (make-composed-keymap my/dirvish-side-map dirvish-mode-map))))

(add-hook 'dirvish-find-entry-hook #'my/dirvish-side-entry)
(add-hook 'window-selection-change-functions #'my/dirvish-follow-file)
(add-hook 'window-buffer-change-functions #'my/dirvish-follow-file)
(add-hook 'post-command-hook #'my/remember-editor-window)
(add-hook 'tab-bar-tab-post-select-functions #'my/remember-editor-window)

(keymap-global-set "<f8>" #'my/dirvish-toggle)
(keymap-global-set "C-x B" #'my/consult-all-buffers)
(keymap-global-set "C-<tab>" #'tab-line-switch-to-next-tab)
(keymap-global-set "C-S-<tab>" #'tab-line-switch-to-prev-tab)
(keymap-global-set "C-<iso-lefttab>" #'tab-line-switch-to-prev-tab)
(keymap-global-set "C-S-<iso-lefttab>" #'tab-line-switch-to-prev-tab)
(keymap-set project-prefix-map "p" #'my/project-workspace)
(keymap-set project-prefix-map "f" #'my/project-find-file)
(keymap-set project-prefix-map "s" #'consult-ripgrep)
(keymap-set tabspaces-command-map "o" #'my/project-workspace)

(provide 'init-navigation)
;;; init-navigation.el ends here
