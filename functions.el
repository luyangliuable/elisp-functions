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
  "Split the window vertically and run the CALLBACK function in the new window."
  (interactive "aFunction to run in new window: ")
  (split-window-right)
  (other-window 1)
  (funcall callback))

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
  ("{" shrink-window-horizontally)  ; Shrink the window horizontally
  ("}" enlarge-window-horizontally) ; Enlarge the window horizontally
  ("f" follow-mode)                 ; Toggle follow mode
  ("o" other-window)                ; Switch to the other window
  ("b" balance-windows)             ; Balance the sizes of all windows
  ("F" toggle-frame-fullscreen)     ; Toggle fullscreen mode
  ("u" winner-undo)                 ; Undo window configuration change
  ("r" winner-redo)                 ; Redo window configuration change
  ("w" ace-window)                  ; Select window with ace-window
  ("q" nil "quit" :color blue))     ; Quit the hydra

(defvar luyangliuable/last-buffer nil
  "The buffer to switch back to.")

(defun luyangliuable/switch-to-last-buffer ()
  "Switch back and forth between the current and last buffer in the current window."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (if (and luyangliuable/last-buffer
             (buffer-live-p luyangliuable/last-buffer))
        (switch-to-buffer luyangliuable/last-buffer)
      (message "No last buffer to switch to."))
    (setq luyangliuable/last-buffer current-buffer)))

(defun luyangliuable/split-window-below-and-run-callback (callback)
  "Split the window vertically and run the CALLBACK function in the new window."
  (interactive "Function to run in new window: ")
  (split-window-below)
  (other-window 1)
  (funcall callback))

(defun luyangliuable/wrap-with-char (char)
  "Wrap the selected region with the specified CHAR."
  (interactive "cWrap with char: ")
  (let ((beg (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char end)
      (insert char)
      (goto-char beg)
      (insert char))))

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
  (clipboard-kill-ring-save (point-min) (point-max)))
