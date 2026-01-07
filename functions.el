(defvar-local hidden-mode-line-mode nil
  "Non-nil if Hidden-Mode-Line mode is enabled.")

(defvar hidden-mode-line-format nil
  "Store the current mode-line-format.")

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hidden-mode-line-format mode-line-format
            mode-line-format nil)
    (setq mode-line-format hidden-mode-line-format
          hidden-mode-line-format nil))
  (force-mode-line-update)
  (redraw-display))

(defun luyangliuable/toggle-mode-line ()
  "Toggle the modeline on and off."
  (interactive)
  (if hidden-mode-line-mode
      (hidden-mode-line-mode -1)
    (hidden-mode-line-mode 1)))

(defun luyangliuable/toggle-absolute-line-numbers ()
  "Toggle between absolute line numbers and no line numbers."
  (interactive)
  (if (eq display-line-numbers t)
      (setq display-line-numbers nil)
    (setq display-line-numbers t))
  (redraw-display))

(defun luyangliuable/toggle-relative-line-numbers ()
  "Toggle between relative line numbers and no line numbers."
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers nil)
    (setq display-line-numbers 'relative))
  (redraw-display))

(defun luyangliuable/goto-scratch-buffer ()
  "Switch to the *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun luyangliuable/split-window-right-and-run-callback (callback)
  "Split the window vertically and run the CALLBACK function in the new window.
   Handles side windows (like Treemacs) by using a regular window instead."
  (interactive "aFunction to run in new window: ")
  ;; Check if we're in a side window
  (let ((current-window (selected-window)))
    (if (window-parameter current-window 'window-side)
        ;; If in a side window, find a regular window to split
        (let ((main-window (get-mru-window nil nil t))) ; Get main regular window
          (if main-window
              (progn
                (select-window main-window)
                (split-window-right)
                (other-window 1)
                (funcall callback))
            ;; If no main window, just call the callback in current window
            (funcall callback)))
      ;; If not in a side window, proceed normally
      (split-window-right)
      (other-window 1)
      (funcall callback))))

(defun luyangliuable/magit ()
  "Smart magit function that refreshes if already in magit-status-mode, otherwise opens magit in a split window."
  (interactive)
  (if (eq major-mode 'magit-status-mode)
      ;; If already in magit-status-mode, just refresh
      (magit-refresh)
    ;; Otherwise, open magit in a split window and mark it for cleanup
    (let ((original-window (selected-window)))
      (luyangliuable/split-window-right-and-run-callback #'magit)
      ;; Store the original window for cleanup purposes
      (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
        (setq-local luyangliuable--magit-original-window original-window)))))

(defun luyangliuable/magit-quit ()
  "Custom magit quit function that properly handles split window cleanup."
  (interactive)
  (let ((magit-window (selected-window))
        (magit-buffer (current-buffer))
        (original-window (when (local-variable-p 'luyangliuable--magit-original-window)
                          luyangliuable--magit-original-window)))

    ;; Store values before calling +magit/quit which might change the buffer context
    (let ((should-cleanup (and original-window
                               (window-live-p original-window)
                               (not (eq magit-window original-window))
                               (> (length (window-list)) 1)))) ;; Don't delete if it's the only window

      ;; Call the standard Doom magit quit function
      (+magit/quit)

      ;; If we should clean up and the magit window still exists, do the cleanup
      (when (and should-cleanup (window-live-p magit-window))
        (delete-window magit-window)
        (select-window original-window)))))

(defhydra hydra-window-management (:color amaranth :hint nil)
  "
Movement^^        ^Split^         ^Delete^        ^Other^
----------------------------------------------------------------
_h_: left         _v_: vertical   _d_: delete     _u_: undo
_j_: down         _s_: horizontal _o_: other      _r_: redo
_k_: up           _m_: maximize   _D_: delete     _b_: balance
_l_: right        _}_: minimize
_f_: follow       _{_: enlarge    _F_: fullscreen
_o_: other        _w_: ace-window
"
  ("h" windmove-left)               ; Move focus to the left window
  ("j" windmove-down)               ; Move focus to the window below
  ("k" windmove-up)                 ; Move focus to the window above
  ("l" windmove-right)              ; Move focus to the right window
  ("v" split-window-right)          ; Split the window vertically
  ("s" split-window-below)          ; Split the window horizontally
  ("d" delete-window)               ; Delete the current window
  ("D" delete-other-windows)        ; Delete all other windows
  ("m" delete-other-windows)        ; Maximize the current window
  ("M" minimize-window)             ; Minimize the current window
  ("]" enlarge-window-horizontally) ; Enlarge the window horizontally
  ("[" shrink-window-horizontally)  ; Shrink the window horizontally
  ("{" shrink-window)               ; Shrink the window horizontally
  ("}" enlarge-window)              ; Enlarge the window horizontally
  ("f" follow-mode)                 ; Toggle follow mode
  ("o" other-window)                ; Switch to the other window
  ("b" balance-windows)             ; Balance the sizes of all windows
  ("F" toggle-frame-fullscreen)     ; Toggle fullscreen mode
  ("u" winner-undo)                 ; Undo window configuration change
  ("r" winner-redo)                 ; Redo window configuration change
  ("w" ace-window)                  ; Select window with ace-window
  ("q" nil "quit" :color blue))     ; Quit the hydra

(defun luyangliuable/switch-to-last-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window.

If `doom-workspaces-restrict-spc-tab' is `t' then this only switches between
the current workspace's buffers."
  (interactive)
  (let ((window (or window (selected-window))))
    (cl-destructuring-bind (buf start pos)
        (if (bound-and-true-p doom-workspaces-restrict-spc-tab)
            (let ((buffer-list (doom-buffer-list))
                  (current-buffer (window-buffer window)))
              ;; Find buffer of the same workspace in window
              (seq-find (lambda (it) ;; Predicate
                          (and (not (eq (car it) current-buffer))
                               (member (car it) buffer-list)))
                        (window-prev-buffers window)
                        ;; Default if none found
                        (list nil nil nil)))
          (or (cl-find (window-buffer window) (window-prev-buffers window)
                       :key #'car :test-not #'eq)
              (list (other-buffer) nil nil)))
      (if (not buf)
          (message "Last buffer not found.")
        (set-window-buffer-start-and-point window buf start pos)))))

(defun luyangliuable/split-window-below-and-run-callback (callback)
  "Split the window horizontally and run the CALLBACK function in the new window.
   Handles side windows (like Treemacs) by using a regular window instead."
  (interactive "aFunction to run in new window: ")
  ;; Check if we're in a side window
  (let ((current-window (selected-window)))
    (if (window-parameter current-window 'window-side)
        ;; If in a side window, find a regular window to split
        (let ((main-window (get-mru-window nil nil t))) ; Get main regular window
          (if main-window
              (progn
                (select-window main-window)
                (split-window-below)
                (other-window 1)
                (funcall callback))
            ;; If no main window, just call the callback in current window
            (funcall callback)))
      ;; If not in a side window, proceed normally
      (split-window-below)
      (other-window 1)
      (funcall callback))))

(defun luyangliuable/wrap-with-char (char)
  "Wrap the selected region with the corresponding CHAR pair."
  (interactive "Wrap with char: ")
  (let ((pairs '((?` . ?`)
                 (?\" . ?\")
                 (?' . ?')
                 (?\( . ?\))
                 (?\[ . ?\])
                 (?{ . ?})
                 (?* . ?*)))
        (beg (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char end)
      (insert (or (cdr (assoc char pairs)) char))
      (goto-char beg)
      (insert char))))

(defun luyangliuable/copy-directory-path ()
  "Copy the directory path of the current buffer to the clipboard."
  (interactive)
  (let ((directory-path (if buffer-file-name
                            (file-name-directory buffer-file-name)
                          default-directory)))
    (when directory-path
      (kill-new directory-path)
      (message "Copied directory path: %s" directory-path))))

(defun luyangliuable/copy-file-path ()
  "Copy the directory path of the current buffer to the clipboard."
  (interactive)
  (let ((directory-path (if buffer-file-name buffer-file-name default-directory)))
    (when directory-path
      (kill-new directory-path)
      (message "Copied directory path: %s" directory-path))))

(defun luyangliuable/copy-file-path-with-line ()
  "Copy and show the file path of the current buffer, including the line number."
  (interactive)
  (if-let* ((file-path (buffer-file-name))
            (line-number (line-number-at-pos))
            (full-path (format "%s:%d" file-path line-number)))
      (progn
        (kill-new full-path)
        (message "Copied: %s" full-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun luyangliuable/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let* ((file-path (buffer-file-name))
            (file-name (file-name-nondirectory file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun luyangliuable/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

(defun luyangliuable/yank-active-minor-modes ()
  "Yank the names of all active minor modes into the kill ring."
  (interactive)
  (let ((active-minor-modes '()))
    (mapc (lambda (mode)
            (when (and (boundp mode) (symbol-value mode))
              (push (symbol-name mode) active-minor-modes)))
          minor-mode-list)
    (let ((modes-string (string-join active-minor-modes ", ")))
      (kill-new modes-string)
      (message "Yanked active minor modes: %s" modes-string))))

(defun luyangliuable/yank-major-mode ()
  "Yank the name of the current major mode into the kill ring."
  (interactive)
  (let ((major-mode-name (symbol-name major-mode)))
    (kill-new major-mode-name)
    (message "Yanked major mode: %s" major-mode-name)))

;; https://stackoverflow.com/a/10216338
(defun luyangliuable/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Yanked entire buffer"))

(defun luyangliuable/shell-clear-buffer ()
  "Clear the shell buffer content, similar to 'clear' command."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer))
  (goto-char (point-max))
  (comint-send-input))

(defun luyangliuable/shell-kill-current-command ()
  "Kill the current command being typed in shell."
  (interactive)
  (comint-kill-input))

(defun luyangliuable/shell-send-eof ()
  "Send EOF (Ctrl-D) to shell process."
  (interactive)
  (comint-send-eof))

(defun luyangliuable/shell-interrupt-process ()
  "Send interrupt signal (Ctrl-C) to shell process."
  (interactive)
  (comint-interrupt-subjob))

(defun luyangliuable/shell-copy-last-output ()
  "Copy the last command output to kill ring."
  (interactive)
  (let ((start (save-excursion
                 (comint-previous-prompt 1)
                 (forward-line 1)
                 (point)))
        (end (save-excursion
               (comint-next-prompt 1)
               (forward-line -1)
               (end-of-line)
               (point))))
    (when (< start end)
      (kill-ring-save start end)
      (message "Copied last output to kill ring"))))

(defun luyangliuable/treemacs-shell-here ()
  "Open shell in the directory of the current treemacs node, or project root if not in treemacs."
  (interactive)
  (let ((target-dir
         (condition-case err
             (cond
              ;; If we're in treemacs, get the directory of the current node
              ((and (eq major-mode 'treemacs-mode)
                    (treemacs-current-button))
               (let* ((button (treemacs-current-button))
                      (node (when button (treemacs-button-get button :path))))
                 (when node
                   (if (file-directory-p node)
                       node
                     (file-name-directory node)))))
              ;; If we have a project root, use that
              ((doom-project-root) (doom-project-root))
              ;; Otherwise use current directory
              (t default-directory))
           ;; If there's any error with treemacs, fall back to project root or default
           (error
            (message "Treemacs error, using fallback directory: %s" (error-message-string err))
            (or (doom-project-root) default-directory)))))

    ;; Ensure we have a valid directory
    (setq target-dir (or target-dir default-directory))
    (message "Opening shell in: %s" target-dir)

    ;; Use safe window splitting and open shell
    (luyangliuable/split-window-right-and-run-callback
     (lambda ()
       (let ((default-directory target-dir))
         (shell))))))

(defun luyangliuable/treemacs-shell-here-horizontal ()
  "Open shell horizontally in the directory of the current treemacs node, or project root if not in treemacs."
  (interactive)
  (let ((target-dir
         (condition-case err
             (cond
              ;; If we're in treemacs, get the directory of the current node
              ((and (eq major-mode 'treemacs-mode)
                    (treemacs-current-button))
               (let* ((button (treemacs-current-button))
                      (node (when button (treemacs-button-get button :path))))
                 (when node
                   (if (file-directory-p node)
                       node
                     (file-name-directory node)))))
              ;; If we have a project root, use that
              ((doom-project-root) (doom-project-root))
              ;; Otherwise use current directory
              (t default-directory))
           ;; If there's any error with treemacs, fall back to project root or default
           (error
            (message "Treemacs error, using fallback directory: %s" (error-message-string err))
            (or (doom-project-root) default-directory)))))

    ;; Ensure we have a valid directory
    (setq target-dir (or target-dir default-directory))
    (message "Opening shell in: %s" target-dir)

    ;; Use safe window splitting and open shell
    (luyangliuable/split-window-below-and-run-callback
     (lambda ()
       (let ((default-directory target-dir))
         (shell))))))

(defun luyangliuable/drag-stuff-up-repeatable ()
  "Drag stuff up with repeatable key."
  (interactive)
  (drag-stuff-up 1)
  (message "Press K to move up, J to move down, any other key to exit")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "K") #'luyangliuable/drag-stuff-up-repeatable)
     (define-key map (kbd "J") #'luyangliuable/drag-stuff-down-repeatable)
     map)
   nil)) ; Simplified - just use nil without the message parameter

(defun luyangliuable/drag-stuff-down-repeatable ()
  "Drag stuff down with repeatable key."
  (interactive)
  (drag-stuff-down 1)
  (message "Press J to move down, K to move up, any other key to exit")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "J") #'luyangliuable/drag-stuff-down-repeatable)
     (define-key map (kbd "K") #'luyangliuable/drag-stuff-up-repeatable)
     map)
   nil)) ; Simplified - just use nil without the message parameter
