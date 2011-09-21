; window maintenance

(require 'pam-vars)
(require 'pam-util)
(require 'pam-overlays)
(require 'pam-copyright)

(defun pam-window-init ()
  (setq pam-buffer-table nil) )

(defun pam-clear-buffer (els)
  "Clear a buffer of all text"
  (let* ((name-info (car els))
	 (filename (pam-name-to-file (pam-get-buffer-name name-info)))
	 (buf (pam-retrieve-buffer name-info)) )
    (pam-delete-from-overlay-list-by-file filename)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (delete-char (- (point-max) (point-min))) )))

(defun pam-external-split-buffer (buffer-name)
  (let ((buf (pam-retrieve-buffer buffer-name)))
    (set-buffer buf)
    (pam-split-buffer) ))

(defun pam-split-buffer ()
  "Split a buffer, but only if it already takes up all of the screen"
  (if (equal (one-window-p) t)
      (let* ((height (window-height))
	     (win (get-largest-window))
	     (proportion (/ (* height 2) 3)))
	(split-window (get-largest-window) proportion)) ))

; setting windows:

(defun pam-set-buffer-up (els)
  "Sets the upper buffer"
  (let* ((name-info (car els))
	 (buf (pam-retrieve-buffer name-info))
	 (win (get-buffer-window buf)) )
    (pam-split-buffer)
    (select-window (get-largest-window))
    (if win
	(progn (select-window win))
        (switch-to-buffer buf) )))

(defun pam-set-buffer-down (els)
  "Sets the lower buffer"
  (let* ((name-info (car els))
	 (buf (pam-retrieve-buffer name-info))
	 (win (get-buffer-window buf)) )
    (pam-split-buffer)
    (select-window (get-largest-window))
    (other-window 1)
    (if win
	(progn (select-window win))
        (switch-to-buffer buf) )))

(defun pam-change-window (els)
  "Changes to the window with the specified buffer, or changes the current window to display that buffer
   if the buffer isn't already displayed"
  (let* ((name-info (car els))
	 (buf (pam-retrieve-buffer name-info))
	 (win (get-buffer-window buf)) )
    (if win
	(select-window win)
        (switch-to-buffer buf) )))

(defun pam-goto-char (els)
  "Goto a character position in the specified file and changes to the window of that file.  If the file is not currently
   displayed, loads the file into a window that is not the current one (assuming there are >= 2 windows at the moment;
   otherwise it sets the current one)."
  (let* ((name-info (car els))
	 (char (cadr els))
	 (buf (pam-retrieve-buffer name-info))
	 (win (get-buffer-window buf)) )
    (if win
	(select-window win)
        (switch-to-buffer-other-window buf) )
    (goto-char char) ))

(defun pam-message (els)
  "Write a message to the mini-buffer"
  (let ((msg (car els)))
    (message msg) ))

(defun pam-debug (els)
  "Write a debugging message to pam-debug-buf"
  (let ((buf (pam-retrieve-buffer pam-debug-buffer))
	(msg (car els)) )
    (with-current-buffer buf
      (let ((moving (= (point) (point-max))))
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (point-max))
	  (insert msg) )
	(if moving (goto-char (point-max))) ))))

(defun pam-close-window (els)
  "Close a window.. if it's already closed, do nothing"
  (let* ((name (pam-get-buffer-name (car els)))
	 (buf (get-buffer name)) )
    (if buf
	(pam-delete-buffer name nil)
      )))

(defun pam-set-access (els)
  "Set whether a buffer is read-only or read-write"
  (let* ((name-info (car els))
	 (access-method (cadr els))
	 (buf (pam-retrieve-buffer name-info))
	 (cur-auto-save-mode (not (auto-save-mode nil))) )
    (cond ((string-equal access-method "read-only")
	   (with-current-buffer buf
	     (if buffer-read-only
		 nil
	       (toggle-read-only) )))
	  ((string-equal access-method "read-write")
	   (with-current-buffer buf
	     (if buffer-read-only
		 (toggle-read-only)
	         nil ))))
    (if cur-auto-save-mode
	(with-current-buffer buf (auto-save-mode 1))
        (with-current-buffer buf
	  (auto-save-mode 0)
	  (delete-auto-save-file-if-necessary t) ))))

(defun pam-waiting (els))

(provide 'pam-window)
