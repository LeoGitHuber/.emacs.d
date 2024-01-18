;;; ultimate-tab --- Ultimate Tab  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(tab-bar-mode)

(defvar tab-bar-format-tab-line-cache nil
  "Tab line cache for tab-bar.")

(defvar tab-bar-tab-line-hidden-filter
  (mapconcat (lambda (x)
               (concat "\\(?:" x "\\)"))
             '("\\` "
               ;; "\\`\\*Completions\\*\\'"
               ;; "\\`\\*Chinese-word-segmentation\\*\\'"
               ;; "\\`\\*Flymake log\\*\\'"
               ;; "\\`\\*Semantic SymRef\\*\\'"
               ;; "\\`\\*tramp/.*\\*\\'"
               ;; "\\`\\*help.*\\*\\'"
               "\\`\\*.*\\*\\'"
               )
             "\\|")
  "Filter buffer for tab-bar's hidden tab line.")

(defvar tab-bar-tab-line-windows-filter
  (mapconcat (lambda (x)
               (concat "\\(?:" x "\\)"))
             '("\\` \\*acm-buffer\\*\\'"
               "\\` \\*acm-doc-buffer\\*\\'"
               "\\` \\*corfu\\*\\'")
             "\\|")
  "Filter buffer for tab-bar's windows tab line.")

(defvar tab-bar-tab-line-close-button
  (propertize "󰅖 " :help-echo "Close tab"
              'face `(:inherit tab-bar-tab)
              'display '(space-width 0.5)
              'mouse-face 'highlight
              'pointer 'hand
              )
  "Close button for tab-bar's tab."
  )

(defvar tab-bar-tab-line-indicator
  (propertize "▎" 'face
              `(:inherit tab-bar-tab
                         :foreground
                         ,(or (face-foreground 'mode-line-emphasis)
                              "#4B535D")))
  "Indicator for tab-bar's current tab.")

(defvar tab-bar-tab-line-strings nil
  "tab line string for tab-bar.")

(defvar-local tab-bar-tab-line-diagnostic-icon nil
  "Diagnostic icon for tab-bar's tab-line.")

(defvar tab-bar-tab-project nil
  "Project of current tab-bar.")

(defun tab-bar-tab-line-format-lsp-diagnostic (&rest _)
  "Show lsp diagnostic info icons."
  (and (bound-and-true-p flymake-mode)
       (let* ((known (hash-table-keys flymake--state))
              (running (flymake-running-backends))
              (disabled (flymake-disabled-backends))
              (reported (flymake-reporting-backends))
              (all-disabled (and disabled (null running)))
              (some-waiting (not (equal reported running))))
         (or some-waiting (null known) all-disabled
             (setq tab-bar-tab-line-diagnostic-icon
                   (let ((.error 0)
                         (.warning 0)
                         (.note 0)
                         (warning-level (warning-numeric-level :warning))
                         (note-level (warning-numeric-level :debug)))
                     (dolist (d (flymake-diagnostics))
                       (let ((severity (flymake--severity (flymake-diagnostic-type d))))
                         (cond ((> severity warning-level) (setq .error (+ .error 1)))
                               ((> severity note-level) (setq .warning (+ .warning 1)))
                               (t (setq .note (+ .note 1)))))
                       )
                     (let ((cache (mapconcat
                                   (lambda (c)
                                     (unless (eq (car c) 0)
                                       (propertize
                                        (concat (number-to-string (car c))
                                                (nth 2 c))
                                        'face `(:inherit ,(nth 1 c)
                                                         :background ,(face-background 'tab-bar-tab)
                                                         :slant italic)
                                        )))
                                   `((,.error
                                      flymake-error-echo
                                      " "
                                      )
                                     (,.warning
                                      flymake-warning-echo
                                      " ")
                                     (,.note
                                      flymake-note-echo  ;; ""
                                      "")))))
                       (if (equal cache "")
                           cache
                         (concat cache
                                 (propertize " "
                                             'face
                                             '(:inherit tab-bar-tab)
                                             'display
                                             '(space-width 0.5)))))
                     ))))))

(defun tab-bar-generate-tab-line-cache (&rest _)
  "Produce buffer tabs for the tab bar."
  (or tab-bar-tab-line-diagnostic-icon
      (tab-bar-tab-line-format-lsp-diagnostic))
  (let* ((buffers (buffer-list))
         (current (current-buffer))
         (project (project-current))
         hidden
         project-hidden
         windows-buffer)
    (if (or (string-match-p "\\` " (buffer-name current))
            (memq current tab-bar-format-tab-line-cache))
        (setq tab-bar-format-tab-line-cache
              (mapcan
               (lambda (b)
                 (when (or (get-buffer-window b 'visible)
                           (not (string-match-p
                                 tab-bar-tab-line-hidden-filter
                                 (buffer-name b))))
                   `(,b)))
               tab-bar-format-tab-line-cache))
      (dolist (b buffers)
        (or (eq b current)
            (if (get-buffer-window b 'visible)
                (and (not (string-match-p tab-bar-tab-line-windows-filter
                                          (buffer-name b)))
                     (push b windows-buffer))
              (push b hidden))))
      (setq tab-bar-format-tab-line-cache
            (if (or (not project)
                    (and tab-bar-tab-project
                         (eq tab-bar-tab-project project)))
                (if tab-bar-format-tab-line-cache
                    (nconc tab-bar-format-tab-line-cache (list current))
                  (nconc (list current) windows-buffer))
              (let ((pr (expand-file-name (project-root project))))
                (setq tab-bar-tab-project project)
                (dolist (b hidden)
                  (let ((dir (buffer-local-value 'default-directory b)))
                    (and (string-prefix-p pr
                                          (if (and (/= 0 (length dir))
                                                   (eq (aref dir 0) ?/))
                                              dir
                                            (expand-file-name dir)))
                         (not (string-match-p
                               tab-bar-tab-line-hidden-filter
                               (buffer-name b)))
                         (push b project-hidden)
                         )))
                (nconc project-hidden (list current) windows-buffer)))
            )
      ))
  (force-mode-line-update)
  )

(defun tab-bar-format-tab-line ()
  "Concatenate tab-line strings."
  (if tab-bar-format-tab-line-cache
      (setq tab-bar-format-tab-line-cache
            (mapcan
             (lambda (b)
               (if (buffer-live-p b)
                   `(,b)))
             tab-bar-format-tab-line-cache))
    (tab-bar-generate-tab-line-cache))
  (let ((current (current-buffer))
        (i 0))
    (setq tab-bar-tab-line-strings
          (mapcan (lambda (b)
                    (let ((buf-name (buffer-name b)))
                      (setq i (+ i 1))
                      (if (eq b current)
                          (append
                           `((tab-bar-tab-line-indicator
                              menu-item
                              ,tab-bar-tab-line-indicator
                              ignore
                              :help nil
                              ))
                           `((,(intern buf-name)
                              menu-item
                              ,(propertize
                                (concat
                                 (format "%d." i)
                                 " "
                                 buf-name
                                 " "
                                 )
                                'face '(:inherit tab-bar-tab))
                              ignore
                              :help ,buf-name
                              ))
                           (and (bound-and-true-p flymake-mode)
                                `((,(intern "flymake-diagnostic")
                                   menu-item
                                   tab-bar-tab-line-diagnostic-icon
                                   flymake-show-buffer-diagnostics
                                   :help "flymake-diagnostic")))
                           `((,(intern "buffer-close")
                              menu-item
                              tab-bar-tab-line-close-button
                              kill-this-buffer
                              :help "Close Buffer")))
                        `((,(intern buf-name)
                           menu-item
                           ,(propertize (concat
                                         (format " %d." i)
                                         " "
                                         buf-name
                                         " ")
                                        'face '(:inherit tab-bar-tab-inactive)
                                        )
                           switch-to-buffer
                           :help ,buf-name
                           )))))
                  tab-bar-format-tab-line-cache)))
  tab-bar-tab-line-strings)

(defun tab-bar--format-tab (tab i)
  "Format TAB using its index I and return the result as a keymap."
  (append
   `((,(intern (format "sep-%i" i)) menu-item ,tab-bar-separator ignore))
   (cond
    ((eq (car tab) 'current-tab)
     `((current-tab
        menu-item
        ,(propertize (format " %d " i) 'face '(:inherit tab-bar-tab)
                     'display '(space-width 0.5))
        ignore
        :help ,(format "tab-%i" i))))
    (t
     `((,(intern (format "tab-%i" i))
        menu-item
        ,(propertize (format " %d " i) 'display '(space-width 0.5))
        ,(alist-get 'binding tab)
        :help ,(format "tab-%i" i)))))
   (when (alist-get 'close-binding tab)
     `((,(if (eq (car tab) 'current-tab) 'C-current-tab
           (intern (format "C-tab-%i" i)))
        menu-item ""
        ,(alist-get 'close-binding tab))))))

(defun tab-bar-mouse-down-1 (event)
  "Select the tab at mouse click, or add a new tab on the tab bar.
  Whether this command adds a new tab or selects an existing tab
  depends on whether the click is on the \"+\" button or on an
  existing tab."
  (interactive "e")
  (let* ((item (tab-bar--event-to-item (event-start event)))
         (tab-number (tab-bar--key-to-number (nth 0 item))))
    (setq tab-bar--dragging-in-progress t)
    ;; Don't close the tab when clicked on the close button.  Also
    ;; don't add new tab on down-mouse.  Let `tab-bar-mouse-1' do this.
    (unless (or (memq (car item) '(add-tab history-back history-forward))
                (nth 2 item))
      (if (functionp (nth 1 item))
          (if (eq (nth 1 item) 'switch-to-buffer)
              (switch-to-buffer (symbol-name (nth 0 item)))
            (call-interactively (nth 1 item)))
        (unless (eq tab-number t)
          (tab-bar-select-tab tab-number))))))

(defun tab-bar-mouse-1 (event)
  "Close the tab whose \"x\" close button you click.
  See also `tab-bar-mouse-close-tab', which closes the tab
  regardless of where you click on it.  Also add a new tab."
  (interactive "e")
  (let* ((item (tab-bar--event-to-item (event-start event)))
         (tab-number (tab-bar--key-to-number (nth 0 item))))
    (cond
     ((and (memq (car item) '(add-tab history-back history-forward))
           (functionp (nth 1 item)))
      (call-interactively (nth 1 item)))
     ((and (nth 2 item) (not (eq tab-number t)))
      (tab-bar-close-tab tab-number)))))

(defun tab-bar--tab (&optional frame)
  "Make a new tab data structure that can be added to tabs on the FRAME."
  (let* ((tab (tab-bar--current-tab-find nil frame))
         (tab-explicit-name (alist-get 'explicit-name tab))
         (tab-group (alist-get 'group tab))
         (tab-line tab-bar-format-tab-line-cache)
         (bl  (seq-filter #'buffer-live-p (frame-parameter
                                           frame 'buffer-list)))
         (bbl (seq-filter #'buffer-live-p (frame-parameter
                                           frame 'buried-buffer-list))))
    `(tab
      (name . ,(if tab-explicit-name
                   (alist-get 'name tab)
                 (funcall tab-bar-tab-name-function)))
      (explicit-name . ,tab-explicit-name)
      ,@(if tab-group `((group . ,tab-group)))
      (time . ,(float-time))
      (tab-line . ,tab-line)
      (ws . ,(window-state-get
              (frame-root-window (or frame (selected-frame))) 'writable))
      (wc . ,(current-window-configuration))
      (wc-point . ,(point-marker))
      (wc-bl . ,bl)
      (wc-bbl . ,bbl)
      ,@(when tab-bar-history-mode
          `((wc-history-back . ,(gethash (or frame (selected-frame))
                                         tab-bar-history-back))
            (wc-history-forward . ,(gethash (or frame (selected-frame))
                                            tab-bar-history-forward))))
      ;; Copy other possible parameters
      ,@(mapcan (lambda (param)
                  (unless (memq (car param)
                                '(name explicit-name group time
                                       ws wc wc-point wc-bl wc-bbl
                                       wc-history-back wc-history-forward))
                    (list param)))
                (cdr tab)))))

(defun tab-bar-new-tab-to (&optional tab-number)
  "Add a new tab at the absolute position TAB-NUMBER.
  TAB-NUMBER counts from 1.  If no TAB-NUMBER is specified, then add
  a new tab at the position specified by `tab-bar-new-tab-to'.
  Negative TAB-NUMBER counts tabs from the end of the tab bar,
  and -1 means the new tab will become the last one.
  Argument addressing is absolute in contrast to `tab-bar-new-tab',
  where argument addressing is relative.
  After the tab is created, the hooks in
  `tab-bar-tab-post-open-functions' are run."
  (interactive "P")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (from-tab (tab-bar--tab)))

    (setq tab-line-format-tab-line-cache nil)
    (when tab-bar-new-tab-choice
      ;; Handle the case when it's called in the active minibuffer.
      (when (window-minibuffer-p)
        (select-window (get-mru-window)))
      (let ((ignore-window-parameters t)
            (window--sides-inhibit-check t))
        (if (eq tab-bar-new-tab-choice 'clone)
            ;; Create new unique windows with the same layout
            (window-state-put (window-state-get))
          ;; Remove window parameters that can cause problems
          ;; with `delete-other-windows' and `split-window'.
          (set-window-parameter nil 'window-atom nil)
          (delete-other-windows)
          (if (eq tab-bar-new-tab-choice 'window)
              ;; Create new unique window from remaining window
              (progn
                (set-window-parameter nil 'window-side nil)
                (window-state-put (window-state-get)))
            ;; Create a new window to get rid of old window parameters
            ;; (e.g. prev/next buffers) of old window.
            (split-window nil window-safe-min-width t)
            (delete-window))))

      (let ((buffer
             (if (and (functionp tab-bar-new-tab-choice)
                      (not (memq tab-bar-new-tab-choice '(clone window))))
                 (funcall tab-bar-new-tab-choice)
               (if (stringp tab-bar-new-tab-choice)
                   (or (get-buffer tab-bar-new-tab-choice)
                       (find-file-noselect tab-bar-new-tab-choice))))))
        (when (buffer-live-p buffer)
          (switch-to-buffer buffer))))

    (when from-index
      (setf (nth from-index tabs) from-tab))

    (let* ((to-tab (tab-bar--current-tab-make
                    (when (eq tab-bar-new-tab-group t)
                      `((group . ,(alist-get 'group from-tab))))))
           (to-number (and tab-number (prefix-numeric-value tab-number)))
           (to-index (or (if to-number
                             (if (< to-number 0)
                                 (+ (length tabs) (1+ to-number))
                               (1- to-number)))
                         (pcase tab-bar-new-tab-to
                           ('leftmost 0)
                           ('rightmost (length tabs))
                           ('left (or from-index 1))
                           ('right (1+ (or from-index 0)))
                           ((pred functionp)
                            (funcall tab-bar-new-tab-to))))))
      (setq to-index (max 0 (min (or to-index 0) (length tabs))))
      (cl-pushnew to-tab (nthcdr to-index tabs))

      (when (eq to-index 0)
        ;; `pushnew' handles the head of tabs but not frame-parameter
        (tab-bar-tabs-set tabs))

      (when tab-bar-history-mode
        (puthash (selected-frame) nil tab-bar-history-back)
        (puthash (selected-frame) nil tab-bar-history-forward)
        (setq tab-bar-history-omit t))

      (run-hook-with-args 'tab-bar-tab-post-open-functions
                          (nth to-index tabs))
      )

    (when tab-bar-show
      (if (not tab-bar-mode)
          ;; Turn on `tab-bar-mode' since a tab was created.
          ;; Note: this also updates `tab-bar-lines'.
          (tab-bar-mode 1)
        (tab-bar--update-tab-bar-lines)))

    (force-mode-line-update)
    (unless tab-bar-mode
      (message "Added new tab at %s" tab-bar-new-tab-to))
    )
  )

(defun tab-bar-select-tab (&optional tab-number)
  "Switch to the tab by its absolute position TAB-NUMBER in the tab bar.
  When this command is bound to a numeric key (with a key prefix or modifier key
                                                    using `tab-bar-select-tab-modifiers'), calling it without an argument
  will translate its bound numeric key to the numeric argument.
  Also the prefix argument TAB-NUMBER can be used to override
  the numeric key, so it takes precedence over the bound digit key.
  For example, `<MODIFIER>-2' will select the second tab, but `C-u 15
  <MODIFIER>-2' will select the 15th tab.  TAB-NUMBER counts from 1.
  Negative TAB-NUMBER counts tabs from the end of the tab bar."
  (interactive "P")
  (unless (integerp tab-number)
    (let ((key (event-basic-type last-command-event)))
      (setq tab-number (if (and (characterp key) (>= key ?1) (<= key ?9))
                           (- key ?0)
                         0))))

  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (to-number (cond ((< tab-number 0) (+ (length tabs) (1+ tab-number)))
                          ((zerop tab-number) (1+ from-index))
                          (t tab-number)))
         (to-index (1- (max 1 (min to-number (length tabs)))))
         (minibuffer-was-active (minibuffer-window-active-p (selected-window))))

    (when (and read-minibuffer-restore-windows minibuffer-was-active
               (not tab-bar-minibuffer-restore-tab))
      (setq-local tab-bar-minibuffer-restore-tab (1+ from-index))
      (add-hook 'minibuffer-exit-hook 'tab-bar-minibuffer-restore-tab nil t))

    (unless (eq from-index to-index)
      (let* ((from-tab (tab-bar--tab))
             (to-tab (nth to-index tabs))
             (wc (alist-get 'wc to-tab))
             (ws (alist-get 'ws to-tab)))

        ;; During the same session, use window-configuration to switch
        ;; tabs, because window-configurations are more reliable
        ;; (they keep references to live buffers) than window-states.
        ;; But after restoring tabs from a previously saved session,
        ;; its value of window-configuration is unreadable,
        ;; so restore its saved window-state.
        (cond
         ((and (window-configuration-p wc)
               ;; Check for such cases as cloning a frame with tabs.
               ;; When tabs were cloned to another frame, then fall back
               ;; to using `window-state-put' below.
               (eq (window-configuration-frame wc) (selected-frame)))
          (let ((wc-point (alist-get 'wc-point to-tab))
                (wc-bl  (seq-filter #'buffer-live-p (alist-get 'wc-bl to-tab)))
                (wc-bbl (seq-filter #'buffer-live-p (alist-get 'wc-bbl to-tab)))
                (tab-line (seq-filter #'buffer-live-p (alist-get 'tab-line to-tab)))
                (wc-history-back (alist-get 'wc-history-back to-tab))
                (wc-history-forward (alist-get 'wc-history-forward to-tab)))

            (set-window-configuration wc nil t)

            ;; set-window-configuration does not restore the value of
            ;; point in the current buffer, so restore it separately.
            (when (and (markerp wc-point)
                       (marker-buffer wc-point)
                       ;; FIXME: After dired-revert, marker relocates to 1.
                       ;; window-configuration restores point to global point
                       ;; in this dired buffer, not to its window point,
                       ;; but this is slightly better than 1.
                       ;; Maybe better to save dired-filename in each window?
                       (not (eq 1 (marker-position wc-point))))
              (goto-char wc-point))

            (when wc-bl  (set-frame-parameter nil 'buffer-list wc-bl))
            (when wc-bbl (set-frame-parameter nil 'buried-buffer-list wc-bbl))
            (when tab-line (setq tab-bar-format-tab-line-cache tab-line))

            (when tab-bar-history-mode
              (puthash (selected-frame)
                       (and (window-configuration-p
                             (alist-get 'wc (car wc-history-back)))
                            wc-history-back)
                       tab-bar-history-back)
              (puthash (selected-frame)
                       (and (window-configuration-p
                             (alist-get 'wc (car wc-history-forward)))
                            wc-history-forward)
                       tab-bar-history-forward))))

         (ws
          ;; `window-state-put' fails when called in the minibuffer
          (when (window-minibuffer-p)
            (select-window (get-mru-window)))
          (window-state-put ws nil 'safe)))

        ;; Select the minibuffer when it was active before switching tabs
        (when (and minibuffer-was-active (active-minibuffer-window))
          (select-window (active-minibuffer-window)))

        ;; When the minibuffer was activated in one tab, but exited in
        ;; another tab, then after going back to the first tab, it has
        ;; such inconsistent state that the current buffer is the minibuffer,
        ;; but its window is not active.  So try to undo this mess.
        (when (and (window-minibuffer-p) (not (active-minibuffer-window)))
          (select-window (get-mru-window)))

        (when tab-bar-history-mode
          (setq tab-bar-history-omit t))

        (when from-index
          (setf (nth from-index tabs) from-tab))
        (setf (nth to-index tabs)
              (tab-bar--current-tab-make (nth to-index tabs)))

        (unless tab-bar-mode
          (message "Selected tab '%s'" (alist-get 'name to-tab))))

      (force-mode-line-update))))

(defun find-file (filename &optional wildcards)
  "Edit file FILENAME.
  \\<minibuffer-local-map>Switch to a buffer visiting file FILENAME, creating one if none
  already exists.
  Interactively, the default if you just type RET is the current directory,
  but the visited file name is available through the minibuffer history:
  type \\[next-history-element] to pull it into the minibuffer.

  The first time \\[next-history-element] is used after Emacs prompts for the file name,
  the result is affected by `file-name-at-point-functions', which by
  default try to guess the file name by looking at point in the current
  buffer.  Customize the value of `file-name-at-point-functions' or set
  it to nil, if you want only the visited file name and the current
  directory to be available on first \\[next-history-element] request.

  You can visit files on remote machines by specifying something
  like /ssh:SOME_REMOTE_MACHINE:FILE for the file name.  You can
  also visit local files as a different user by specifying
  /sudo::FILE for the file name.
  See the Info node `(tramp)File name Syntax' in the Tramp Info
  manual, for more about this.

  Interactively, or if WILDCARDS is non-nil in a call from Lisp,
  expand wildcards (if any) and visit multiple files.  You can
  suppress wildcard expansion by setting `find-file-wildcards' to nil.

  \\<global-map>To visit a file without any kind of conversion and without
  automatically choosing a major mode, use \\[find-file-literally]."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	    (mapcar 'pop-to-buffer-same-window (nreverse value))
      (if (or (not tab-bar-tab-project)
              (not (buffer-file-name value))
              (equal (project--find-in-directory (file-name-directory (buffer-file-name value)))
                     tab-bar-tab-project))
          (pop-to-buffer-same-window value)
        (let ((tab-bar-new-tab-choice (buffer-name value)))
          (tab-bar-new-tab-to))))))

(with-eval-after-load 'tab-bar
  (advice-add #'flymake--handle-report :after
              #'tab-bar-tab-line-format-lsp-diagnostic)

  (add-hook 'window-state-change-hook #'tab-bar-generate-tab-line-cache)
  ;; (add-to-list 'window-size-change-functions 'tab-bar-generate-tab-line-cache)

  (setq tab-bar-format
        '(tab-bar-format-tab-line
          tab-bar-format-align-right
          tab-bar-format-history
          tab-bar-format-tabs
          ))
  (set-face-attribute 'tab-bar nil
                      :inherit nil)
  (set-face-attribute 'tab-bar-tab nil
                      :background (face-background 'default)
                      :box nil
                      :slant 'italic
                      :weight 'bold)
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :slant 'normal :weight 'normal :underline nil)

  (defvar tab-bar-name-hints-format " %d " ;; " %d. "
    "Set format for tab-bar-name-hints.")

  (defface tab-bar-name-hints-face
    '((t :slant italic))
    "Face for tab-bar-hints."
    :group 'tab-bar-faces)

  ;;   (defun tab-bar-tab-name-format-hints (name _tab i)
  ;;     "Show absolute numbers on tabs in the tab bar before the tab name.
  ;; It has effect when `tab-bar-tab-hints' is non-nil."
  ;;     (if tab-bar-tab-hints
  ;;         (concat
  ;;          (propertize (format tab-bar-name-hints-format i)
  ;;                      'face '(:inherit tab-bar-name-hints-face)
  ;;                      'display '(space-width 0.5))
  ;;          name)
  ;;       name))

  (setq tab-bar-separator ""
        ;; (propertize " " 'face '(:inherit variable-pitch :background "black"))
        tab-bar-tab-name-format-functions
        '(tab-bar-tab-name-format-face)
        tab-bar-tab-name-function (lambda () "")
        tab-bar-auto-width nil
        tab-bar-tab-name-truncated-max 25
        tab-bar-tab-hints t
        tab-bar-new-button-show nil
        tab-bar-close-button-show nil
        )
  ;; (add-hook 'tab-bar-mode-hook
  ;;           (lambda ()
  ;;             (setq tab-bar-close-button
  ;;                   ;; (propertize " 󰅖 " :help-echo "Close tab"
  ;;                   ;;             'close-tab t
  ;;                   ;;             'mouse-face 'highlight
  ;;                   ;;             'pointer 'vdrag
  ;;                   ;;             )
  ;;                   (propertize " ✗ " :help-echo "Close tab"
  ;;                               'close-tab
  ;;                               'mouse-face 'error
  ;;                               'pointer 'hand
  ;;                               )
  ;;                   ;; (propertize "  " :help-echo "Close tab"
  ;;                   ;;             'close-tab t
  ;;                   ;;             'mouse-face 'highlight
  ;;                   ;;             ;; 'pointer 'vdrag
  ;;                   ;;             )
  ;;                   ))
  ;;           ;; modified_icon = '●'
  ;;           ;; close_icon = ''
  ;;           )
  )

(provide 'ultimate-tab)
;;; ultimate-tab.el ends here.
