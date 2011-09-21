; pam-hyperlink functions

(require 'pam-vars)
(require 'pam-util)
(require 'pam-window)
(require 'pam-overlays)
(require 'pam-copyright)

(defconst pam-hyper-or-markup-types-default '((hyper . pam-hyper)
					      (markup . pam-markup)
					      (file . pam-hm-file) ))

(defvar pam-hyper-or-markup-types pam-hyper-or-markup-types-default
  "The association list for the types of pam-hyperlinks/pam-markups in a '(pam-hypers-or-markups ...)'" )

(defvar pam-markup-table nil
  "The table that contains all markup overlays (not hyperlink ones)" )

(defun pam-hyper-init ()
  (setq pam-markup-table nil) )

; <hyper> ::= (hyper "<name>" (<pos> <pos>) (<highlight>) "<text>")
(defun pam-hyper (args buf)
  "How to process a hyperlink"
  (progn (let* ((filename (buffer-name buf))
		(name (car args))
		(color (if (not (null (cddr args)))
			   (caar (cddr args))
			   nil ))
		(range (cons (car (cadr args))
			     (cadr (cadr args)) )))
	   (pam-add-an-overlay range buf color (list 'callback name filename))
	   buf )))
(defvar pam-hyper (lambda (args buf) (pam-hyper args buf)))

(defun pam-insert-in-markup-table (overlay)
  (setq pam-markup-table (append (list overlay) pam-markup-table)) )

(defun pam-mark-help (table)
  (while (not (null table))
    (let ((overlay (car table)))
      (delete-overlay overlay)
      (setq table (cdr table)) )))

(defun pam-delete-all-markups ()
  (pam-mark-help pam-markup-table)
  (setq pam-markup-table nil) )

; <markup> ::= (markup (<pos> <pos>) (<pam-markup>) "<text>")
(defun pam-markup (els buf)
  "Runs a markup operation"
  (if (not buf)
      (setq buf (pam-make-default-buf)) )
  (let* ((a (caar els))
	 (b (cadr (car els)))
	 (pam-markup (caar (cdr els)))
	 (overlay (make-overlay a b buf)) )
    (pam-insert-in-markup-table overlay) 
    (overlay-put overlay 'face (eval pam-markup) ))
  buf )

(defun pam-hm-file (els buf)
  (let ((name-info (car els)))
    (pam-retrieve-buffer name-info) ))

(defun pam-hyper-or-markup-iter (els buf)
  "Go over each element of the blob and run its corresponding procedure"
  (while (not (null els))
    (let ((proc (assoc (caar els) pam-hyper-or-markup-types)))
      (if proc
	  (progn (setq buf (eval (list (cdr proc) (quote (cdar els)) buf)))
		 (setq els (cdr els)) )))))

(defun pam-overlay-list (els)
  "A wrapper for pam-hyper-or-markup-iter"
  (save-excursion (pam-hyper-or-markup-iter els nil)) )

(provide 'pam-hyper)
