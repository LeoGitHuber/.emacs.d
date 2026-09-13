;;; vim-bar.el --- frame-local tabs with named persistent window configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defcustom vim-bar-show t
  "Defines when to show the tab bar.
If t, the default, enable `tab-bar-mode' automatically upon using
the commands that create new window configurations (e.g., `tab-new').
If a non-negative integer, show the tab bar only if the number of
the tabs exceeds the value of this variable.  In particular,
if the value is 1, hide the tab bar when it has only one tab, and
show it again once more tabs are created.  A value that is a
non-negative integer also makes the tab bar appearance be different
on different frames: the tab bar can be shown on some frames and
hidden on others, depending on how many tab-bar tabs are on that
frame, and whether that number is greater than the numerical value
of this variable.
If nil, always keep the tab bar hidden.  In this case it's still
possible to use persistent named window configurations by relying on
keyboard commands `tab-new', `tab-close', `tab-next', `tab-switcher', etc.

Setting this variable directly does not take effect; please customize
it (see the info node `Easy Customization'), then it will automatically
update the tab bar on all frames according to the new value.

To enable or disable the tab bar individually on each frame,
you can use the command `toggle-frame-tab-bar'."
  :type '(choice (const :tag "Always" t)
                 (const :tag "When more than one tab" 1)
                 (const :tag "Never" nil))
  :initialize #'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (vim-bar-mode 1)
           (tab-bar--update-tab-bar-lines t)))
  :group 'vim-bar
  :version "27.1")

(defvar tab-bar-tabs-function #'vim-bar-tabs
  "Function to get a list of tabs to display in the tab bar.
This function should have one optional argument FRAME,
defaulting to the selected frame when nil.
It should return a list of alists with parameters
that include at least the element (name . TAB-NAME).
For example, \\='((tab (name . \"Tab 1\")) (current-tab (name . \"Tab 2\")))
By default, use function `tab-bar-tabs'.")

(defun vim-bar--load-buttons ()
  "Load the icons for the tab buttons."
  (require 'icons)
  (declare-function icon-string "icons" (name))
  (declare-function iconp "icons" (object))
  (declare-function icons--register "icons")
  (unless (iconp 'tab-bar-new)
    (define-icon tab-bar-new nil
      `((image "symbols/plus_16.svg" "tabs/new.xpm"
               :face shadow
               :height (1.0 . em)
               :margin ,tab-bar-button-margin
               :ascent center)
        ;; (emoji "➕")
        ;; (symbol "＋")
        (text " + "))
      "Icon for creating a new tab."
      :version "29.1"
      :help-echo "New tab"))
  (setq tab-bar-new-button (icon-string 'tab-bar-new))

  (unless (iconp 'tab-bar-close)
    (define-icon tab-bar-close nil
      `((image "symbols/cross_16.svg" "tabs/close.xpm"
               :face shadow
               :height (1.0 . em)
               :margin ,tab-bar-button-margin
               :ascent center)
        ;; (emoji " ❌")
        ;; (symbol "✕") ;; "ⓧ"
        (text " x"))
      "Icon for closing the clicked tab."
      :version "29.1"
      :help-echo "Click to close tab"))
  (setq tab-bar-close-button (propertize (icon-string 'tab-bar-close)
                                         'close-tab t))

  (unless (iconp 'tab-bar-menu-bar)
    (define-icon tab-bar-menu-bar nil
      `((image "symbols/menu_16.svg"
               :height (1.0 . em)
               :margin ,tab-bar-button-margin
               :ascent center)
        ;; (emoji "🍔")
        (symbol "☰")
        (text "Menu" :face tab-bar-tab-inactive))
      "Icon for the menu bar."
      :version "29.1"
      :help-echo "Menu bar"))

(defun tab-bar-tabs (&optional frame)
  "Return a list of tabs belonging to the FRAME.
Ensure the frame parameter `tabs' is pre-populated.
Update the current tab name when it exists.
Return its existing value or a new value."
  (let ((tabs (frame-parameter frame 'tabs)))
    (if tabs
        (let* ((current-tab (tab-bar--current-tab-find tabs))
               (current-tab-name (assq 'name current-tab))
               (current-tab-explicit-name (assq 'explicit-name current-tab)))
          (when (and current-tab-name
                     current-tab-explicit-name
                     (not (cdr current-tab-explicit-name)))
            (setf (cdr current-tab-name)
                  (funcall tab-bar-tab-name-function))))
      ;; Create default tabs
      (setq tabs (list (tab-bar--current-tab-make)))
      (tab-bar-tabs-set tabs frame))
    tabs))

(defun tab-bar-tabs-set (tabs &optional frame)
  "Set a list of TABS on the FRAME."
  (set-frame-parameter frame 'tabs tabs))

(defun tab-bar--current-tab-find (&optional tabs frame)
  ;; Find the current tab as a pointer to its data structure.
  (assq 'current-tab (or tabs (funcall tab-bar-tabs-function frame))))

(defun tab-bar--current-tab-make (&optional tab)
  "Make the current tab data structure from TAB.
TAB here is an argument meaning \"use tab as template\",
i.e. the tab is created using data from TAB.  This is
necessary when switching tabs, otherwise the destination tab
inherits the current tab's `explicit-name' parameter."
  (let* ((tab-explicit-name (alist-get 'explicit-name tab))
         (tab-group (if tab
                        (alist-get 'group tab)
                      (pcase tab-bar-new-tab-group
                        ((pred stringp) tab-bar-new-tab-group)
                        ((pred functionp) (funcall tab-bar-new-tab-group))))))
    `(current-tab
      (name . ,(if tab-explicit-name
                   (alist-get 'name tab)
                 (funcall tab-bar-tab-name-function)))
      (explicit-name . ,tab-explicit-name)
      ,@(if tab-group `((group . ,tab-group)))
      ;; Copy other possible parameters
      ,@(mapcan (lambda (param)
                  (unless (memq (car param)
                                '(name explicit-name group time
                                       ws wc wc-point wc-bl wc-bbl
                                       wc-history-back wc-history-forward))
                    (list param)))
                (cdr tab)))))

(defun vim-bar--vim-bar-lines-for-frame (frame)
  "Determine and return the value of `tab-bar-lines' for FRAME.
Return 0 if `tab-bar-mode' is not enabled.  Otherwise return
either 1 or 0 depending on the value of the customizable variable
`tab-bar-show', which see."
  (cond
   ((not vim-bar-mode) 0)
   ((not vim-bar-show) 0)
   ((eq vim-bar-show t) 1)
   ((natnump vim-bar-show)
    (if (> (length (funcall tab-bar-tabs-function frame)) tab-bar-show) 1 0))))

(defun vim-bar--update-vim-bar-lines (&optional frames)
  "Update the `tab-bar-lines' frame parameter in FRAMES.
If the optional parameter FRAMES is omitted, update only
the currently selected frame.  If it is t, update all frames
as well as the default for new frames.  Otherwise FRAMES should be
a list of frames to update."
  (let ((frame-lst (cond ((null frames)
                          (list (selected-frame)))
                         ((eq frames t)
                          (frame-list))
                         (t frames))))
    ;; Loop over all frames and update `tab-bar-lines'
    (dolist (frame frame-lst)
      (unless (or (frame-parameter frame 'tab-bar-lines-keep-state)
                  (and (eq auto-resize-tab-bars 'grow-only)
                       (> (frame-parameter frame 'tab-bar-lines) 1)))
        (set-frame-parameter frame 'tab-bar-lines
                             (tab-bar--tab-bar-lines-for-frame frame)))))
  ;; Update `default-frame-alist'
  (when (eq frames t)
    (setq default-frame-alist
          (cons (cons 'tab-bar-lines
                      (if (and tab-bar-mode (eq tab-bar-show t)) 1 0))
                (assq-delete-all 'tab-bar-lines default-frame-alist)))))

(define-minor-mode vim-bar-mode
  "Toggle the tab bar in all graphical frames (Vim Bar mode)."
  :global t
  ;; It's defined in C/cus-start, this stops the d-m-m macro defining it again.
  :variable vim-bar-mode

  ;; Recalculate `vim-bar-lines' for all frames
  (vim-bar--update-vim-bar-lines t)

  (when vim-bar-mode
    (tab-bar--load-buttons))
  (if vim-bar-mode
      (tab-bar--define-keys)
    (tab-bar--undefine-keys)))

(frame-parameter (car (frame-list)) 'tab-bar-lines-keep-state)

(provide 'vim-bar)
;;; vim-bar.el ends here.
