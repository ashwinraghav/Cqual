; utility functions

(require 'easy-mmode)

(require 'pam-vars)
(require 'pam-faces)
(require 'pam-copyright)

(defconst pam-default-path-default "")
(defconst pam-fancy-twirler-counter-default 0)
(defconst pam-fancy-twirler-vector-default ["-" "\\" "|" "/"])
(defconst pam-fancy-twirler-placer-default 0)
(defconst pam-fancy-twirler-counter-speed-default 5)

(defvar pam-default-path pam-default-path-default)
(defvar pam-fancy-twirler-counter pam-fancy-twirler-counter-default)
(defvar pam-fancy-twirler-vector pam-fancy-twirler-vector-default)
(defvar pam-fancy-twirler-placer pam-fancy-twirler-placer-default)
(defvar pam-fancy-twirler-counter-speed pam-fancy-twirler-counter-speed-default)

(defun pam-util-init ()
  (setq pam-default-path pam-default-path-default)
  (setq pam-fancy-twirler-counter pam-fancy-twirler-counter-default)
  (setq pam-fancy-twirler-placer pam-fancy-twirler-placer-default) )

(defvar pam-buffer-table nil)
(defvar pam-debug-buffer (list pam-debug-buf "buffer"))
  
(defun pam-name-to-file (name)
  "To make relative paths into absolute ones...the child process should really be sending the absolute
   ones _only_, since relative paths don't work very well"
  (concat pam-default-path (format "%s" name)))

(defun pam-mode-by-buffer (buf &optional extra)
  "Set a buffer to the PAM mode you desire: the extra argument is sent as an argument to the function
   'pam-mode'"
  (save-excursion
    (set-buffer buf)
    (pam-mode extra) ))

(defun pam-construct-safe-buffer-list ()
  (setq pam-buffers-not-to-kill-on-reset (buffer-list)) )

(defun pam-insert-buffer (name type buf)
  "Add a buffer to the buffer table"
  (if (assoc name pam-buffer-table)
      nil
      (setq pam-buffer-table (append (list (list name type buf))
				     pam-buffer-table ))))

(defun pam-delete-buffer (name ignore-modified)
  "Delete a buffer from the buffer table"
  (save-excursion
    (let* ((buf (pam-retrieve-buffer (list name "file"))))
      (if (not (member buf pam-buffers-not-to-kill-on-reset))
	  (progn
	    (set-buffer buf)
	    (if ignore-modified
		(set-buffer-modified-p nil) )
	    (kill-buffer buf) )
	  (save-excursion
	    (if (bufferp buf)
		(progn
		  (set-buffer buf)
		  (pam-mode nil) ))))))
  (let ((func
	 (function (lambda (el) (not (equal (car el) name)))) ))
    (setq pam-buffer-table (pam-my-map func pam-buffer-table)) ))

(defun pam-delete-all-buffers-helper (table ignore-modified)
  "A helper function..."
  (while (not (null table))
    (let ((buf (pam-retrieve-buffer (list (caar table) (cadr (car table))))))
      (if (not (member buf pam-buffers-not-to-kill-on-reset))
	  (progn
	    (set-buffer buf)
	    (if ignore-modified
		(set-buffer-modified-p nil) )
	    (kill-buffer buf) )
	  (save-excursion
	    (if (bufferp buf)
		(progn
		  (set-buffer buf)
		  (pam-mode nil) ))))
      (setq table (cdr table)) )))


(defun pam-delete-all-buffers (ignore-modified)
  "Delete all buffers in the table"
  (interactive)
  (save-excursion (pam-delete-all-buffers-helper pam-buffer-table ignore-modified))
  (setq pam-buffer-table nil)
  'done)

(defun pam-retrieve-buffer (file)
  "Find a buffer by name"
  (let* ((name (car file))
	 (type (cadr file)) )

    (cond ((string-equal type "file")
	   (let ((val (save-window-excursion (with-temp-buffer (find-file name)))))
	     (save-excursion (set-buffer val)
			     (pam-mode-by-buffer val 1) )
	     (pam-insert-buffer name type val)
	     val ))
	  ((string-equal type "buffer")
	   (let ((val (get-buffer-create name)))
	     (with-current-buffer val
	       (auto-save-mode 0)
	       (delete-auto-save-file-if-necessary t)
	       (pam-mode-by-buffer val 1) )
	     (pam-insert-buffer name type val)
	     val ))
	  (t
	   (save-window-excursion (with-temp-buffer (get-buffer "*Messages*"))) ))))

(defun pam-get-buffer-name (file)
  (car file) )
(defun pam-get-buffer-type (file)
  (cadr file) )

(defun pam-make-default-buf ()
  "Do the default buffer if they don't specify a file"
  (let ((buf (pam-retrieve-buffer '("*default-pam-file*" "buffer"))))
    (set-buffer buf)
    buf))

(defun pam-set-default-path (els)
  "Set the value..."
  (setq pam-default-path (concat (car els) "/")))

(defun pam-my-map (pred lst)
  "A function that returns a list of all elements that pass 'pred', but works better than mapcar"
  (let ((out nil))
    (while (not (null lst))
      (if (funcall pred (car lst))
	 (setq out (append (list (car lst)) out)) )        ; to make the appending faster, do it backwards
      (setq lst (cdr lst)) )
    (nreverse out) ))

(defun pam-fancy-twirler ()
  "A function to iterate (at most once per function call, depending on the speed) through the spinning twirler
   so that the user knows PAM is working and not in an infinite loop"
  (if (> pam-fancy-twirler-counter pam-fancy-twirler-counter-speed)
      (progn (message (elt pam-fancy-twirler-vector pam-fancy-twirler-placer))
	     (if (> pam-fancy-twirler-placer (- (length pam-fancy-twirler-vector) 2))
		 (setq pam-fancy-twirler-placer 0)
	         (setq pam-fancy-twirler-placer (1+ pam-fancy-twirler-placer)) )
	     (setq pam-fancy-twirler-counter 0) )
      (setq pam-fancy-twirler-counter (1+ pam-fancy-twirler-counter)) ))

(defun pam-mode (&optional blah)
  nil )

(defun pam-mode-info ()
  (interactive)
  (info "pam-mode-info") )

(defun pam-mode-info-keybindings ()
  (interactive)
  (info "(pam-mode-info)keybindings"))

(defvar pam-keymap (make-sparse-keymap "PAM"))
(define-key pam-keymap "\C-c\C-f" 'pam-analyze-file)
(define-key pam-keymap "\C-c\C-r" 'pam-reset)
(define-key pam-keymap "\C-c\C-l" 'pam-keyboard-action-at-point)
(define-key pam-keymap "\C-c\C-m" 'pam-mode)
(define-key pam-keymap "\C-c\C-i" 'pam-mode-info)
(define-key pam-keymap [mouse-1]  'pam-mouse-1-action) 
(define-key pam-keymap [mouse-2]  'pam-mouse-2-action-at-point)
(define-key pam-keymap [S-mouse-2] 'pam-S-mouse-2-action-at-point)
(define-key pam-keymap [mouse-3] 'pam-mouse-3-action)
(define-key pam-keymap [menu-bar pam] (cons "PAM" (make-sparse-keymap "pam")))
(define-key pam-keymap [menu-bar pam mode] '("Turn off PAM mode for this buffer" . pam-mode))
(define-key pam-keymap [menu-bar pam reset] '("Reset PAM" . pam-reset))
(define-key pam-keymap [menu-bar pam analyze] '("Analyze new file" . pam-analyze-file))
(define-key pam-keymap [menu-bar pam help] '("Full PAM documentation" . pam-mode-info))
(define-key pam-keymap [menu-bar pam help-keymap] '("Help for PAM keybindings" . pam-mode-info-keybindings))

(defun pam-define-minor-mode ()
  (if pam-also-use-old-mouse-1-binding
      (progn
	(setq pam-old-mouse-1-action (global-key-binding [mouse-1])) ))
  (if pam-also-use-old-mouse-2-binding
      (setq pam-old-mouse-2-action (global-key-binding [mouse-2])) )
  (if pam-also-use-old-mouse-3-binding
      (setq pam-old-mouse-3-action (global-key-binding [mouse-3])) )
  (easy-mmode-define-minor-mode
   pam-mode
   "A procedure to start the minor mode called \"program analysis mode\" (PAM).  Usage:
  no arguments toggles the mode, a nil argument turns it off, and a non-nil one turns it on"
   nil					; initial value
   " PAM"					; mode line indicator
   pam-keymap) )

(provide 'pam-util)


(current-local-map)
