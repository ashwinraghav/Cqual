; functions to manage blocks of code executed when the user clicks or presses control
; sequences

(require 'pam-vars)
(require 'pam-util)
(require 'pam-window)
(require 'pam-copyright)

(defconst pam-executions-default '((pam-overlays . pam-overlay-list)
				   (pam-default-path . pam-set-default-path)
				   (pam-blobs . pam-blob)
				   (pam-clear-window . pam-clear-buffer)
				   (pam-split-window . pam-external-split-buffer)
				   (pam-set-window-up . pam-set-buffer-up)
				   (pam-set-window-down . pam-set-buffer-down)
				   (pam-change-window . pam-change-window)
				   (pam-goto-char . pam-goto-char)
				   (pam-message . pam-message)
				   (pam-debug . pam-debug)
				   (pam-set-access . pam-set-access)
				   (pam-close-window . pam-close-window)
				   (pam-waiting . pam-waiting) ))
(defconst pam-execution-procs-default '((callback . pam-execution-callback)
					(direct . pam-execution-direct)))
(defconst pam-click-default "click\n")
(defconst pam-shift-click "shift-click\n")

(defvar pam-executions pam-executions-default
  "The association list to figure out what to do on the various kinds of child process input")
(defvar pam-execution-procs pam-execution-procs-default
  "The association list to determine what type of thing to do when a user clicks on an overlay")

(defun pam-executions-init () )

(defun pam-process-ok (proc)
  "Is the child process still running?"
  (let ((status (process-status proc)))
    (if (member status '(stop exit signal nil))
	nil
        t)))

(defun pam-execution-callback (val click-message)
  "The user clicked on an overlay that needs to be reported to the child process"
  (let* ((filename (cadr val))
	 (send-val (car val))
	 (buf (pam-retrieve-buffer (list pam-results-buf "buffer"))) )
    (save-excursion
      (if (pam-process-ok pam-process)
	  (progn
	    (set-buffer buf)
	    (process-send-string pam-process click-message)
	    (process-send-string pam-process (concat send-val "\n"))
	    (process-send-string pam-process (concat filename "\n"))
	    (process-send-string pam-process (concat (cadr (assoc filename pam-buffer-table)) "\n")))
	  (progn (setq pam-initialized nil)
		 (message "Child process is no longer running.  Use 'M-x pam-reset' to reset") )))))

(defun pam-execution-direct (val click-message)
  "The user clicked on an overlay that can be processed without talking to the child process"
  (let ((cur-buf (current-buffer)))
    (let* ((filename (car val))
	   (range (cadr val))
	   (buf (pam-retrieve-buffer '(filename "file"))))
      (switch-to-buffer buf)
      (if (> (car range) -1)
	  (goto-char (car range))) )))

(defun pam-execute-returned-command (command click-message)
  "Figure out what to do with the overlay just clicked on"
  (let* ((name (car command))
	 (val (cdr command))
	 (proc (assoc (car val) pam-execution-procs)))
    (if proc
	(eval (list (cdr proc) (quote (cdr val)) click-message)) )))

(provide 'pam-executions)
