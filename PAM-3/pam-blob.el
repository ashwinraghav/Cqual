; pam-blob implementation

(require 'pam-vars)
(require 'pam-util)
(require 'pam-window)
(require 'pam-overlays)
(require 'pam-hyper)
(require 'pam-copyright)

(defconst pam-blob-types-default '((file . pam-blob-file)
				   (text . pam-blob-text)
				   (hyper . pam-blob-hyper)
				   (markup . pam-blob-markup)) )

(defvar pam-blob-types pam-blob-types-default)

(defun pam-blob-init () )

(defun pam-blob-file (els buf)
  "Set the current filename for a blob and return it"
  (let* ((name (pam-name-to-file (pam-get-buffer-name (car els))))
	 (buf (pam-retrieve-buffer (car els))) )
    (set-buffer buf)
    buf))

(defun pam-insert-text (text buf)
  "Insert text at the end of the buffer, advancing point if necessary"
  (with-current-buffer buf
    (let ((moving (= (point) (point-max))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (point-max))
	(insert text)
	)
      (if moving (goto-char (point-max))) )))
  
(defun pam-blob-text (els buf)
  "Get some raw text to put in the buffer"
  (if (not buf)
      (setq buf (pam-make-default-buf)))
  (pam-insert-text (car els) buf)
  buf)

; <hyper> ::= (hyper "<name>" (<pos> <pos>) (<highlight>) "<text>")
(defun pam-output-link (link range buf)
  "Add the overlay for the link just added"
  (let ((filename (buffer-name buf))
	(callback (car link))
	(target-range (cadr link))
	(color (if (not (null (cddr link)))
		   (caar (cddr link))
		   nil )))
    (pam-add-an-overlay range buf color (list 'callback callback filename)) ))

; <hyper> ::= (hyper "<name>" (<pos> <pos>) (<highlight>) "<text>")
(defun pam-blob-hyper (els buf)
  "Get a link with embedded text to put in the buffer"
  (if (not buf)
      (setq buf (pam-make-default-buf)))
  (let* ((text (cadr (cddr els)))
	 (str-len (length text))
	 (file-len (with-current-buffer buf
		     (buffer-size))) )
    (pam-insert-text text buf)
    (pam-output-link els (cons (+ file-len 1) (+ file-len str-len 1)) buf)
    buf ))

; <markup> ::= (markup (<pos> <pos>) (<highlight>) "<text>")
(defun pam-blob-markup (els buf)
  "Insert new text into the buffer and mark it up"
  (if (not buf)
      (setq buf (pam-make-default-buf)) )
  (let* ((pam-markup (caar (cdr els)))
	 (text (car (cddr els)))
	 (str-len (length text))
	 (file-len (with-current-buffer buf
		     (buffer-size)))
	 (nothing (pam-insert-text text buf))
	 (overlay (make-overlay (+ file-len 1) (+ file-len str-len 1) buf)))
    (pam-insert-in-markup-table overlay)
    (overlay-put overlay 'face (eval pam-markup))
    buf ))

(defun pam-blob-iter (els buf)
  "Go over each element of the blob and run its corresponding procedure"
  (while (not (null els))
    (let ((proc (assoc (caar els) pam-blob-types)))
      (if proc
	  (progn (setq buf (eval (list (cdr proc) (quote (cdar els)) buf)))
		 (setq els (cdr els)) )))))

(defun pam-blob (els)
  "Evaluate a pam-blob"
  (save-excursion (pam-blob-iter els nil)) )

(provide 'pam-blob)
