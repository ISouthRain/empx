
(require 'xref)

(defcustom xrmp-idle-time 5
  "Idle time in seconds before triggering dictionary lookup."
  :type 'number
  :group 'xrmp)

(defvar xrmp--idle-timer nil
  "Timer to track idle time after cursor movement.")

(defun xrmp--function ()
  "Execute function if no actions occur within X seconds."
  (when (and (not (minibufferp)) (not buffer-read-only))
    (xref-push-marker-stack (point-marker))
    )
  )

(defun xrmp--reset-timer ()
  "Reset the idle timer after every action."
  (when xrmp--idle-timer
    (cancel-timer xrmp--idle-timer))
  (setq xrmp--idle-timer
        (run-with-idle-timer xrmp-idle-time nil #'xrmp--function)))

(defun xrmp-mode-enable ()
  "Enable xrmp mode to track cursor movement and idle time."
  (add-hook 'post-command-hook #'xrmp--reset-timer))

(defun xrmp-mode-disable ()
  "Disable xrmp mode."
  (remove-hook 'post-command-hook #'xrmp--reset-timer)
  (when xrmp--idle-timer
    (cancel-timer xrmp--idle-timer)
    (setq xrmp--idle-timer nil)))

;;;###autoload
(define-minor-mode xrmp-mode
  "Minor mode to display a message after X seconds of inactivity following cursor movement."
  :lighter " Xrmp"
  :global t
  (if xrmp-mode
      (xrmp-mode-enable)
    (xrmp-mode-disable)))

(provide 'xrmp)
