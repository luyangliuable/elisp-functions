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

(defun luyangliuable/modeline-scroll-bar-segment ()
  "Return a custom modeline scroll bar segment.
Archived implementation; currently unused in favor of Doom modeline's bar."
  (let* ((width 8)
         (max-pos (max 1 (point-max)))
         (point-pos (min max-pos (point)))
         (ratio (/ (float point-pos) max-pos))
         (pos (min (1- width)
                   (max 0 (floor (* width ratio))))))
    (concat
     (propertize " " 'display '(space :width 2))
     (propertize (make-string pos ?▁) 'face 'mode-line-inactive)
     (propertize " " 'face 'doom-modeline-bar 'display '(space :width 4))
     (propertize (make-string (- (1- width) pos) ?▁)
                 'face 'mode-line-inactive)
     (propertize " " 'display '(space :width 2)))))

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

(defun luyangliuable/treemacs-magit-here ()
  "Open magit in the directory of the current treemacs node, or project root if not in treemacs."
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
    (message "Opening magit in: %s" target-dir)

    ;; Check if already in magit-status-mode in the target directory
    (if (and (eq major-mode 'magit-status-mode)
             (let ((current-repo (magit-toplevel))
                   (target-repo (magit-toplevel target-dir)))
               (and current-repo target-repo
                    (string= (file-truename current-repo) (file-truename target-repo)))))
        ;; If already in magit-status-mode in the same repository, just refresh
        (magit-refresh)
      ;; Otherwise, open magit in a split window for the target directory
      (let ((original-window (selected-window)))
        (luyangliuable/split-window-right-and-run-callback
         (lambda () (magit-status target-dir)))
        ;; Store the original window for cleanup purposes
        (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
          (setq-local luyangliuable--magit-original-window original-window))))))

(defhydra hydra-window-management (:color amaranth :hint nil)
	  "
Movement^^        ^Split^         ^Delete^        ^Other^
----------------------------------------------------------------
_h_: left         _v_: vertical   _d_: delete     _u_: undo
_j_: down         _s_: horizontal _o_: other      _r_: redo
_k_: up           _m_: maximize   _D_: delete     _=_: balance
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
	  ("=" balance-windows)             ; Balance the sizes of all windows
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

(declare-function +fold/close "fold")
(declare-function magit-current-section "magit-section")
(declare-function magit-section-hide "magit-section")
(declare-function origami-close-node-recursively "origami")
(declare-function origami-mode "origami")
(declare-function org-back-to-heading "org")
(declare-function org-element-at-point "org-element")
(declare-function org-element-lineage "org-element-ast")
(declare-function org-element-post-affiliated "org-element")
(declare-function org-element-type "org-element-ast")
(declare-function org-fold-hide-block-toggle "org-fold")
(declare-function org-fold-hide-drawer-toggle "org-fold")
(declare-function org-fold-hide-subtree "org-fold")
(declare-function org-at-heading-p "org")
(declare-function org-in-item-p "org-list")
(declare-function org-up-heading-safe "org")
(declare-function org-list-struct "org-list")
(declare-function org-list-get-parent "org-list")
(declare-function org-list-has-child-p "org-list")
(declare-function org-list-parents-alist "org-list")
(declare-function org-list-set-item-visibility "org-list")
(declare-function outline-back-to-heading "outline")
(declare-function outline-hide-subtree "outline")
(declare-function org-get-next-sibling "org")
(declare-function org-get-previous-sibling "org")
(declare-function org-move-subtree-down "org")
(declare-function org-move-subtree-up "org")

(declare-function origami-apply-new-tree "origami")
(declare-function origami-fold-assoc "origami")
(declare-function origami-fold-beg "origami")
(declare-function origami-fold-end "origami")
(declare-function origami-fold-find-path-with-range "origami")
(declare-function origami-fold-is-root-node? "origami")
(declare-function origami-fold-map "origami")
(declare-function origami-fold-open-set "origami")
(declare-function origami-fold-open? "origami")
(declare-function origami-get-fold-tree "origami")
(declare-function origami-reset "origami")
(declare-function origami-search-forward-for-path "origami")
(declare-function origami-store-cached-tree "origami")

(defun luyangliuable/origami-closed-node-at-point ()
  "Return the closed Origami node at point, or nil when there is none."
  (when (and (not (region-active-p))
             (bound-and-true-p origami-mode)
             (fboundp 'origami-search-forward-for-path))
    (when-let* ((path (origami-search-forward-for-path (current-buffer) (point)))
                (node (car (last path))))
      (unless (or (origami-fold-is-root-node? node)
                  (origami-fold-open? node))
        node))))

(defun luyangliuable/origami-close-exact-range (beg end)
  "Recursively close the Origami node whose exact range is BEG through END."
  (let* ((buffer (current-buffer))
         (old-tree (origami-get-fold-tree buffer))
         (path (origami-fold-find-path-with-range old-tree beg end)))
    (unless path
      (user-error "Origami could not find moved fold at %d-%d" beg end))
    (let ((new-tree
           (origami-fold-assoc
            path
            (lambda (node)
              (origami-fold-map
               (lambda (child) (origami-fold-open-set child nil))
               node)))))
      (origami-apply-new-tree
       buffer old-tree (origami-store-cached-tree buffer new-tree)))))

(defun luyangliuable/origami-fold-movable-p (beg end drag-function)
  "Return non-nil when BEG through END can move with DRAG-FUNCTION."
  (save-excursion
    (cond ((eq drag-function #'drag-stuff-up)
           (goto-char beg)
           (= (forward-line -1) 0))
          ((eq drag-function #'drag-stuff-down)
           (goto-char end)
           (forward-line 1)
           (not (eobp)))
          (t (error "Unsupported drag function: %S" drag-function)))))

(defun luyangliuable/org-close-item-at-point (item)
  "Fold ITEM's nearest parent list item that has nested items.
Return non-nil when a foldable item was found."
  (save-excursion
    (goto-char item)
    (forward-line 0)
    (let* ((struct (org-list-struct))
           (parents (org-list-parents-alist struct))
           (target item))
      (while (and target (not (org-list-has-child-p target struct)))
        (setq target (org-list-get-parent target struct parents)))
      (when target
        (org-list-set-item-visibility target struct 'folded)
        t))))

(defun luyangliuable/org-close-subtree-at-point ()
  "Close the nearest relevant Org parent fold at point.
Blocks and ordinary drawers take priority over list items.  Property drawers
fold their containing heading.  On a heading, fold its parent heading."
  (interactive)
  (let* ((block-types '(center-block comment-block dynamic-block example-block
                        export-block quote-block special-block src-block
                        verse-block))
         (element (org-element-at-point))
         (block (org-element-lineage element block-types t))
         (item (org-in-item-p))
         (drawer (org-element-lineage element 'drawer t)))
    (cond (block
           (goto-char (org-element-post-affiliated block))
           (org-fold-hide-block-toggle t nil block))
          (drawer
           (goto-char (org-element-post-affiliated drawer))
           (org-fold-hide-drawer-toggle t nil drawer))
          ((and item (luyangliuable/org-close-item-at-point item)))
          ((org-at-heading-p)
           (org-up-heading-safe)
           (org-fold-hide-subtree))
          (t
           (org-back-to-heading t)
           (org-fold-hide-subtree)))))

(defun luyangliuable/magit-close-section-at-point ()
  "Close the enclosing Magit log section when point is on a commit."
  (let ((section (magit-current-section)))
    (while (and (eq (oref section type) 'commit)
                (oref section parent))
      (setq section (oref section parent)))
    (magit-section-hide section)))

(defun luyangliuable/close-fold-at-point ()
  "Close the fold or section at point in the active major mode."
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (luyangliuable/org-close-subtree-at-point))
        ((derived-mode-p 'magit-section-mode)
         (luyangliuable/magit-close-section-at-point))
        ((or (bound-and-true-p origami-mode)
             (derived-mode-p 'prog-mode))
         (unless (bound-and-true-p origami-mode)
           (origami-mode 1))
         (origami-close-node-recursively (current-buffer) (point)))
        ((derived-mode-p 'outline-mode)
         (outline-back-to-heading t)
         (outline-hide-subtree))
        (t (+fold/close))))

(defun luyangliuable/drag-org-subtree (drag-function)
  "Move the Org subtree at point with DRAG-FUNCTION.
Return `moved', `blocked', or `no-fold' to describe the outcome."
  (if (and (derived-mode-p 'org-mode)
           (not (region-active-p))
           (org-at-heading-p))
      (let ((move-up-p (eq drag-function #'drag-stuff-up)))
        (if (save-excursion
              (funcall (if move-up-p
                           #'org-get-previous-sibling
                         #'org-get-next-sibling)))
            (progn
              ;; Org swaps complete sibling subtrees, preserving their children.
              (funcall (if move-up-p
                           #'org-move-subtree-up
                         #'org-move-subtree-down))
              'moved)
          (message "Subtree cannot move further %s"
                   (if move-up-p "up" "down"))
          'blocked))
    'no-fold))

(defun luyangliuable/drag-closed-origami-fold (drag-function)
  "Move a closed Origami fold with DRAG-FUNCTION.
Return `moved', `blocked', or `no-fold' to describe the outcome."
  (if-let ((node (luyangliuable/origami-closed-node-at-point)))
      (let* ((fold-beg (origami-fold-beg node))
             (fold-end (origami-fold-end node))
             (beg (save-excursion
                    (goto-char fold-beg)
                    (line-beginning-position)))
             (end (save-excursion
                    (goto-char fold-end)
                    (line-end-position))))
        (if (not (luyangliuable/origami-fold-movable-p
                  beg end drag-function))
            (progn
              (message "Fold cannot move further %s"
                       (if (eq drag-function #'drag-stuff-up) "up" "down"))
              'blocked)
          (let ((beg-offset (- fold-beg beg))
                (end-offset (- fold-end beg))
                moved-beg)
            ;; Origami overlays cannot survive drag-stuff's delete-and-insert move.
            (origami-reset (current-buffer))
            (let ((deactivate-mark nil))
              (goto-char beg)
              (push-mark end t t)
              (funcall drag-function 1)
              (setq moved-beg (region-beginning)))
            (deactivate-mark)
            (origami-reset (current-buffer))
            (luyangliuable/origami-close-exact-range
             (+ moved-beg beg-offset) (+ moved-beg end-offset))
            (goto-char moved-beg)
            'moved)))
    'no-fold))

(defun luyangliuable/drag-stuff-repeat-map ()
  "Activate the transient J/K map for repeated vertical movement."
  (message "Press J to move down, K to move up, any other key to exit")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "J") #'luyangliuable/drag-stuff-down-repeatable)
     (define-key map (kbd "K") #'luyangliuable/drag-stuff-up-repeatable)
     map)
   nil))

(defun luyangliuable/drag-stuff-up-repeatable ()
  "Move the closed fold or active text upward and enter repeat mode."
  (interactive)
  (pcase (luyangliuable/drag-org-subtree #'drag-stuff-up)
    ('no-fold
     (pcase (luyangliuable/drag-closed-origami-fold #'drag-stuff-up)
       ('no-fold
        (drag-stuff-up 1)
        (luyangliuable/drag-stuff-repeat-map))
       ('moved (luyangliuable/drag-stuff-repeat-map))))
    ('moved (luyangliuable/drag-stuff-repeat-map))))

(defun luyangliuable/drag-stuff-down-repeatable ()
  "Move the closed fold or active text downward and enter repeat mode."
  (interactive)
  (pcase (luyangliuable/drag-org-subtree #'drag-stuff-down)
    ('no-fold
     (pcase (luyangliuable/drag-closed-origami-fold #'drag-stuff-down)
       ('no-fold
        (drag-stuff-down 1)
        (luyangliuable/drag-stuff-repeat-map))
       ('moved (luyangliuable/drag-stuff-repeat-map))))
    ('moved (luyangliuable/drag-stuff-repeat-map))))

(defun luyangliuable/sort-lines (&optional reverse)
  "Sort the lines within a selected region or entire buffer.
When given a non-nil argument, sort in descending order instead."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-lines reverse beg end)))

(defun luyangliuable/new-shell-for-project ()
  "Create a new shell session for the current project.
Always spawns a fresh shell instead of reusing existing ones."
  (interactive)
  (let* ((project-root (or (doom-project-root) default-directory))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (timestamp (format-time-string "%H%M%S"))
         (shell-buffer-name (format "*shell-%s-%s*" project-name timestamp))
         (default-directory project-root))

    ;; Create the new shell buffer with unique name
    (let ((shell-buffer (shell shell-buffer-name)))
      ;; Switch to the new shell buffer
      (switch-to-buffer shell-buffer)
      (message "New shell created for project '%s' in %s" project-name project-root))))

(defun luyangliuable/new-shell-for-project-split ()
  "Create a new shell session for the current project in a split window.
Always spawns a fresh shell instead of reusing existing ones."
  (interactive)
  (let* ((project-root (or (doom-project-root) default-directory))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (timestamp (format-time-string "%H%M%S"))
         (shell-buffer-name (format "*shell-%s-%s*" project-name timestamp)))

    ;; Use the existing split window function but with our new shell logic
    (luyangliuable/split-window-right-and-run-callback
     (lambda ()
       (let ((default-directory project-root))
         (let ((shell-buffer (shell shell-buffer-name)))
           (switch-to-buffer shell-buffer)
           (message "New shell created for project '%s' in %s" project-name project-root)))))))

(defun luyangliuable/browse-at-remote-line (&optional arg)
  "Open the remote URL for the current file at point's line.
With prefix ARG, toggle `browse-at-remote-prefer-symbolic'."
  (interactive "P")
  (require 'browse-at-remote)
  (let ((browse-at-remote-add-line-number-if-no-region-selected t)
        (browse-at-remote-prefer-symbolic
         (if arg
             (not browse-at-remote-prefer-symbolic)
           browse-at-remote-prefer-symbolic))
        (mark-active nil))
    (browse-at-remote)))

(defun luyangliuable/browse-at-remote-region (&optional arg)
  "Open the remote URL for the active region.
With prefix ARG, toggle `browse-at-remote-prefer-symbolic'."
  (interactive "P")
  (unless (use-region-p)
    (user-error "No active region"))
  (require 'browse-at-remote)
  (let ((browse-at-remote-prefer-symbolic
         (if arg
             (not browse-at-remote-prefer-symbolic)
           browse-at-remote-prefer-symbolic)))
    (browse-at-remote)))

(defun luyangliuable/open-in-external-app (file-path)
  "Open FILE-PATH with the system default external application."
  (cond
   ((eq system-type 'darwin)
    (start-process "open-external" nil "open" file-path))
   ((eq system-type 'gnu/linux)
    (let ((process-connection-type nil))
      (start-process "open-external" nil "xdg-open" file-path)))
   ((eq system-type 'windows-nt)
    (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
   (t
    (user-error "Unsupported system type: %s" system-type))))

(defun luyangliuable/open-file-or-directory-in-external-app (arg)
  "Open current file in an external application.
With universal prefix ARG, open the containing folder instead."
  (interactive "P")
  (if arg
      (luyangliuable/open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (luyangliuable/open-in-external-app file-path)
        (message "No file associated to this buffer.")))))

(defvar-local luyangliuable/buffer-font-cookie nil
  "Face remapping cookie for this buffer's custom font.")

(defvar-local luyangliuable/buffer-font-saved-scale nil
  "Text scale amount saved before setting a custom buffer font.")

(defun luyangliuable/buffer-font--default-size ()
  "Return the current effective default face size in points."
  (let* ((height (face-attribute 'default :height nil t))
         (base-size (/ (float height) 10.0))
         (scale (if (bound-and-true-p text-scale-mode)
                    (expt text-scale-mode-step text-scale-mode-amount)
                  1.0)))
    (* base-size scale)))

(defun luyangliuable/buffer-font--system-families ()
  "Return font families known to Emacs and the system font database."
  (require 'subr-x)
  (let ((families (font-family-list)))
    (when (executable-find "fc-list")
      (dolist (line (ignore-errors (process-lines "fc-list" ":family")))
        (dolist (family (split-string line "," t "[[:space:]]+"))
          (push (string-trim family) families))))
    (sort (delete-dups (delq nil families)) #'string<)))

(defun luyangliuable/set-buffer-font (family size)
  "Set FAMILY at SIZE points for the current buffer only."
  (interactive
   (progn
     (unless (display-graphic-p)
       (user-error "Buffer font changes require graphical Emacs"))
     (let* ((families (luyangliuable/buffer-font--system-families))
            (default-family (face-attribute 'default :family nil t)))
       (unless families
         (user-error "No graphical fonts are available"))
       (setq default-family
             (if (member default-family families) default-family (car families)))
       (let* ((family (completing-read "Font family: " families nil nil nil default-family))
              (size (read-number "Font size: "
                                 (luyangliuable/buffer-font--default-size))))
         (list family size)))))
  (unless (display-graphic-p)
    (user-error "Buffer font changes require graphical Emacs"))
  (unless (and (numberp size) (> size 0))
    (user-error "Font size must be positive"))
  (when luyangliuable/buffer-font-cookie
    (face-remap-remove-relative luyangliuable/buffer-font-cookie))
  (unless luyangliuable/buffer-font-saved-scale
    (setq luyangliuable/buffer-font-saved-scale text-scale-mode-amount))
  (text-scale-set 0)
  (setq luyangliuable/buffer-font-cookie
        (face-remap-add-relative
         'default `(:family ,family :height ,(round (* size 10)))))
  (redraw-display)
  (message "Buffer font: %s %.1f" family (float size)))

(defun luyangliuable/reset-buffer-font ()
  "Reset the current buffer's custom font."
  (interactive)
  (if luyangliuable/buffer-font-cookie
      (progn
        (face-remap-remove-relative luyangliuable/buffer-font-cookie)
        (setq luyangliuable/buffer-font-cookie nil)
        (text-scale-set luyangliuable/buffer-font-saved-scale)
        (setq luyangliuable/buffer-font-saved-scale nil)
        (redraw-display)
        (message "Buffer font reset"))
    (message "No custom buffer font is active")))
