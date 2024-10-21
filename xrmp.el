
(require 'xref)

(defcustom xrmp-idle-time 5
  "Xrmp marker idle time in seconds."
  :type 'number
  :group 'xrmp)

(defcustom xrmp-target-functions
  '(find-file
    project-find-file
    project-switch-to-buffer
    consult-buffer
    consult-outline
    consult-imenu
    consult-imenu-multi
    consult-ripgrep
    consult-line
    consult-line-multi)
  "List of functions to advise to push the current point marker onto the xref marker stack.
This will record the current cursor point before executing these functions."
  :type '(repeat function)
  :group 'xrmp)

(defvar xrmp--idle-timer nil
  "Timer to track idle time after cursor movement, marks the remaining time at the current cursor point.")

(defun xrmp--push-marker-stack (&rest _)
  "Push the current point marker onto the xref marker stack."
  (xref-push-marker-stack (point-marker)))

(defun xrmp--function ()
  "Execute function if no actions occur within `xrmp-idle-time` seconds."
  (when (and (not (minibufferp)) (not buffer-read-only))
    (xrmp--push-marker-stack)
    )
  )

(defun xrmp--reset-timer ()
  "Reset the idle timer after every action."
  (when xrmp--idle-timer
    (cancel-timer xrmp--idle-timer))
  (setq xrmp--idle-timer
        (run-with-idle-timer xrmp-idle-time nil #'xrmp--function)))

(defun xrmp-enable-advice ()
  "Add advice to specified functions to push markers onto the xref stack."
  (dolist (target xrmp-target-functions)
    (advice-add target :before #'xrmp--push-marker-stack)))

(defun xrmp-mode-enable ()
  "Enable xrmp mode to track cursor movement and idle time."
  (add-hook 'post-command-hook #'xrmp--reset-timer)
  (xrmp-enable-advice))

(defun xrmp-mode-disable ()
  "Disable xrmp mode."
  (remove-hook 'post-command-hook #'xrmp--reset-timer)
  (when xrmp--idle-timer
    (cancel-timer xrmp--idle-timer)
    (setq xrmp--idle-timer nil))
  (dolist (target xrmp-target-functions)
    (advice-remove target #'xrmp--push-marker-stack)))

;;;###autoload
(define-minor-mode xrmp-mode
  "Xrmp records the cursor position for forward/backward navigation."
  :lighter " Xrmp"
  :global t
  (if xrmp-mode
      (xrmp-mode-enable)
    (xrmp-mode-disable)))

(provide 'xrmp)
