; pam!

(require 'cl)

(require 'pam-vars)
(require 'pam-executions)
(require 'pam-window)
(require 'pam-util)
(require 'pam-blob)
(require 'pam-hyper)
(require 'pam-overlays)
(require 'pam-copyright)

(defvar pam-partial-read-2 nil)


(defun pam-concat-list (l)
  (let ((tmp (copy-list l))
	(accum nil))
    (while (not (null (cdr tmp)))
      (while (not (null tmp))
	(if (not (null (cdr tmp)))
	    (progn (setq accum (cons (concat (car tmp) (cadr tmp)) accum))
		   (setq tmp (cddr tmp)) )
	    (progn (setq accum (cons (car tmp) accum))
		   (setq tmp (cdr tmp))) ))
      (setq tmp (reverse accum))
      (setq accum nil) )
    (car tmp) ))

(defun pam-process-substrings-2 (string)
"Split the argument (on newlines) into substrings that are 
 then parsed by pam-parse-input. Notice that pam-partial-read 
 may be non-empty on entry.  This version uses a fancier concatenation scheme."
  (let ((len (length string))
	(last-index 0)
	(index 0))
    (while (< index len)
      (if (= (aref string index) newline)
	  (progn
	    (let ((str (pam-concat-list (reverse (cons (substring string last-index index) pam-partial-read-2)))))
	      (if (not (eq str ""))
		  (pam-parse-input str)
		nil))
	    (setq pam-partial-read-2 nil)
	    (setq last-index (1+ index)) )
	  nil)
      (setq index (1+ index)))
    (setq pam-partial-read-2 (cons (substring string last-index index) pam-partial-read-2)) ))

(defun pam-process-substrings (string)
"Split the argument (on newlines) into substrings that are 
 then parsed by pam-parse-input. Notice that pam-partial-read 
 may be non-empty on entry."
  (let ((len (length string))
	(last-index 0)
	(index 0))
    (while (< index len)
      (if (= (aref string index) newline)
	  (progn
	    (let ((str (concat pam-partial-read (substring string last-index index))))
	      (if (not (eq str ""))
		  (pam-parse-input str)
		nil))
	    (setq pam-partial-read "")
	    (setq last-index (1+ index)) )
	  nil)
      (setq index (1+ index)))
    (setq pam-partial-read (concat pam-partial-read (substring string last-index index))) ))

(defun pam-parse-input (string)
  "Parse the input"
;  (message string)
  (if pam-initialized
      (let* ((expr (read string))
	     (proc (assoc (car expr) pam-executions)))
	(if proc
	    (eval (list (cdr proc) (quote (cdr expr)))) ))
      (if (equal string "///")
	  (setq pam-initialized t) )))
(defvar pam-parse-input (lambda (string) (pam-parse-input string)))

(defun pam-process-input (proc string)
  "Handle the arrival of new text output from the child process"
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	(insert string)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc)))))
  (pam-fancy-twirler)
  (pam-process-substrings-2 string))
(defvar pam-process-input (lambda (process string) (pam-process-input process string)))

(defun pam-init-all ()
  (setq pam-partial-read "")
  (setq pam-initialized nil)
  (pam-executions-init)
  (pam-window-init)
  (pam-util-init)
  (pam-blob-init)
  (pam-hyper-init)
  (pam-overlays-init) )

(defun pam-reset ()
  (interactive)
  (pam-delete-all-overlays)
  (pam-delete-all-buffers t)
  (pam-delete-all-markups)
  (pam-init-all)
  (if pam-prev-buffer
      (let ((buf (get-buffer pam-prev-buffer)))
	(if buf
	    (progn (switch-to-buffer buf)
		   (delete-other-windows) ))
	(setq pam-prev-buffer nil) ))
  (pam-construct-safe-buffer-list))

(defun pam-format-list (in-list)
  (let ((out-list nil))
    (while in-list
      (setq out-list (cons (format "%s" (car in-list)) out-list))
      (setq in-list (cdr in-list)) )
    (reverse out-list) ))

(defun pam-analyze-file-common (analysis-passed-in filename)
  "The common code for the pam-analyze-* functions"
  (let* ((analysis (if analysis-passed-in
		       analysis-passed-in
		     (error "No analysis specified") ))
	 (child-process-name (if (listp analysis)
				 (format "%s" (car analysis))
			       (format "%s" analysis) ))
	 (arg-list (if (listp analysis)
		       (pam-format-list (cdr analysis))
		     (list (format "%s" analysis)) ))
	 (actual-arg-list (append arg-list (list (expand-file-name filename)))) )
    (setq pam-default-analysis analysis-passed-in)
    (setq pam-current-analyze-file filename)
    (pam-reset)
    (setq pam-prev-buffer (buffer-name (current-buffer)))
    (pam-mode t)
    (let ((buf (pam-retrieve-buffer (list pam-results-buf "buffer"))))
      (switch-to-buffer buf))
    (setq pam-process nil)
    (setq pam-process (apply 'start-process
			     "pam-process" pam-results-buf
			     child-process-name
			     actual-arg-list ))
    (set-process-filter pam-process pam-process-input) ))


(defun pam-find-config-in-user-table (sysconf)
  "Part 1 of make-architecture-dependent-sml-load-file"
  (let ((local-tbl pam-sml-system-configuration-table)
	(ret-val nil) )
    (while local-tbl
      (if (string-match (caar local-tbl) sysconf)
	  (progn (setq ret-val (cdar local-tbl))
		 (setq local-tbl nil) )
	  (setq local-tbl (cdr local-tbl)) ))
    ret-val ))

(defun make-architecture-dependent-sml-load-file (string)
  "Trys to put SML/NJ extensions on binary file name prefixes based first on the
   user-defined configuration table, then on built-in heuristics based on
   the Emacs variable 'system-configuration'"
  (let* ((extension "")
	 (ret-val (pam-find-config-in-user-table system-configuration)))
    (if ret-val
	(setq extension ret-val)
        (setq extension
	      (let* ((sysconf system-configuration)
		     (extension
		      (cond ((string-match "hppa" sysconf)
			     "hppa-hpux" )
			    ((string-match "i386" sysconf)
			     (cond ((string-match "linux" sysconf)
				    "x86-linux" )
				   ((string-match "solaris" sysconf)
				    "x86-solaris" )))
			    ((string-match "sparc" sysconf)
			     (cond ((string-match "solaris" sysconf)
				    "sparc-solaris" )))
			    (t
			     "no-architecture-found" )))))))
    (concat "@SMLload=" (format "%s" string) "." extension) ))
		

(defun pam-correct-list-rep (val)
  "Puts string markers around non-string elements in a list.  Doesn't work in all cases though.
   Hack! Hack!"
  (concat "(" (mapconcat (function (lambda (el)
				     (concat "\"" el "\"") ))
			 val " " )
	  ")" ))

(defun pam-sml-analyze-file (analysis-passed-in filename)
  "A shortcut to pam-analyze-file that assumes you're running SML/NJ and guesses at architectures for you."
  (interactive
   (let* ((height (window-height (minibuffer-window)))
	  (nothing (save-excursion
		     (let ((l (get-buffer-window (current-buffer))))
		       (if (< height 3)  ; arbitrary number that seems to work well
			   (progn (select-window (minibuffer-window))
				  (enlarge-window (- 3 height))
				  (select-window l) )))))
	  (string1 (read-from-minibuffer
		    "SML/NJ analysis to perform (enter as a lisp-style list if args are needed): "
		    (if pam-default-sml-analysis
			(pam-correct-list-rep pam-default-sml-analysis)
		        "")                                  ;default value
		    nil                                      ; no custom keymap
		    t                                        ; is in fact a lisp object
		    'pam-sml-analyze-file-history
		    (if pam-default-sml-analysis
			(pam-correct-list-rep pam-default-sml-analysis)
		        "")                                  ; default if they enter nothing
		    nil ))
	  (nothing2 (save-excursion
		      (let ((l (get-buffer-window (current-buffer))))
			(if (< height 3)  ; arbitrary number that seems to work well
			    (progn (select-window (minibuffer-window))
				   (shrink-window (- 3 height))
				   (select-window l) )))))
					; no inheriting of input methods
	  (string2 (read-file-name "File to analyze: "                               ; prompt
				   (file-name-directory pam-current-analyze-file)    ; cur. directory
				   pam-current-analyze-file                          ; default
				   nil                                               ; must match a file
					                                             ; name
				   (file-name-nondirectory pam-current-analyze-file) ; initial value
				   )))
     (list string1 string2) ))
  (setq pam-default-sml-analysis analysis-passed-in)
  (let ((analysis (append (list sml-program-name
				(make-architecture-dependent-sml-load-file (car analysis-passed-in)) )
			  (cdr analysis-passed-in) )))
    (pam-analyze-file-common analysis filename) ))

(defun pam-full-analyze-file (analysis-passed-in filename)
  "Run the analysis given in 'analysis-passed-in' on 'filename'.  See the description for
   the variable pam-default-analysis in pam-vars.el for the format of analysis-passed-in."
  (interactive
   (let* ((height (window-height (minibuffer-window)))
	  (nothing (save-excursion
		     (let ((l (get-buffer-window (current-buffer))))
		       (if (< height 3)  ; arbitrary number that seems to work well
			   (progn (select-window (minibuffer-window))
				  (enlarge-window (- 3 height))
				  (select-window l) )))))
	  (string1 (read-from-minibuffer 
		    "Analysis to perform (enter as a lisp-style list if args are needed): " ; prompt
		    (if pam-default-analysis
			(pam-correct-list-rep pam-default-analysis)
		        "")                               ; default value
		    nil                                   ; no custom keymap
		    t                                     ; is in fact a lisp object
		    'pam-full-analyze-file-history
		    (if pam-default-analysis
			(pam-correct-list-rep pam-default-analysis)
		        "")                               ; default if they enter nothing
		    nil ))                                ; no inheriting of input methods
	  (nothing2 (save-excursion
		      (let ((l (get-buffer-window (current-buffer))))
			(if (< height 3)  ; arbitrary number that seems to work well
			    (progn (select-window (minibuffer-window))
				   (shrink-window (- 3 height))
				   (select-window l) )))))
	  (string2 (read-file-name "File to analyze: "                               ; prompt
				   (file-name-directory pam-current-analyze-file)    ; cur. directory
				   pam-current-analyze-file                          ; default
				   nil                                               ; must match a file
					                                             ; name
				   (file-name-nondirectory pam-current-analyze-file) ; initial value
				   )))
     (list string1 string2) ))
  (pam-analyze-file-common analysis-passed-in filename) )

(defun pam-analyze-file (filename)
  (interactive
   (let ((string (read-file-name "File to analyze: "                              ; prompt
				 (file-name-directory pam-current-analyze-file)    ; cur. directory
				 pam-current-analyze-file                          ; default
				 nil                                               ; must match a file
					; name?
				 (file-name-nondirectory pam-current-analyze-file) ; initial value
				 )))
     (list string) ))
  (pam-analyze-file-common pam-default-analysis filename) )




;;;;;;;;;;;;;;;;;;;;;; end startup stuff

(defun pam-keyboard-action-at-point ()
  (interactive)
  (let* ((pos (point))
	 (window (get-buffer-window (current-buffer)))
	 (buffer (window-buffer window))
	 (filename (buffer-name buffer))
	 (value (pam-lookup-overlay-global filename pos)) )
    ;(set-buffer (window-buffer window))
    ;(select-window window)
    ;(goto-char pos)
    ;(print filename)
    ;(print pos)
    (if value
	(pam-execute-returned-command value pam-click-default))))

(defun pam-mouse-1-action (event)
  (interactive "e")
  (if (overlayp pam-region-overlay)
      (progn (delete-overlay pam-region-overlay)
	     (setq pam-region-overlay nil) ))
  (if pam-also-use-old-mouse-1-binding
      (funcall pam-old-mouse-1-action event) ))

(defun pam-mouse-2-action-at-point (event)
  (interactive "e")
  (let* ((loc (event-end event))
	 (pos (posn-point loc))
	 (window (posn-window loc))
	 (buffer (window-buffer window))
	 (filename (buffer-name buffer))
	 (value (pam-lookup-overlay-global filename pos)) )
    (if (overlayp pam-region-overlay)
	(progn (delete-overlay pam-region-overlay)
	       (setq pam-region-overlay nil) ))
    (set-buffer buffer)
    (select-window window)
    (goto-char pos)
    (if value
	(pam-execute-returned-command value pam-click-default) ))
  (if pam-old-mouse-2-action
      (funcall pam-old-mouse-2-action event) ))

(defun pam-S-mouse-2-action-at-point (event)
  (interactive "e")
  (let* ((loc (event-end event))
	 (pos (posn-point loc))
	 (window (posn-window loc))
	 (buffer (window-buffer window))
	 (filename (buffer-name buffer))
	 (value (pam-lookup-overlay-global filename pos)) )
    (if (overlayp pam-region-overlay)
	(progn (delete-overlay pam-region-overlay)
	       (setq pam-region-overlay nil) ))
    (set-buffer buffer)
    (select-window window)
    (goto-char pos)
    (if value
	(pam-execute-returned-command value pam-shift-click) ))
  (if pam-old-mouse-2-action
      (funcall pam-old-mouse-2-action event) ))

(defun pam-mouse-3-action (event)
  (interactive "e")
  (let* ((lo (if mouse-last-region-beg
		 mouse-last-region-beg
	       -1 ))
	 (hi (if mouse-last-region-end
		 mouse-last-region-end
	       -1 ))
	 (buffer (window-buffer (posn-window (event-end event))))
	 (filename (buffer-name buffer))
	 (value (pam-lookup-range-overlay-global filename lo hi)) )
    (if (overlayp pam-region-overlay)
	(progn (delete-overlay pam-region-overlay)
	       (setq pam-region-overlay nil) ))
    (if value
	(progn
	  (if (overlayp pam-region-overlay)
	      (delete-overlay pam-region-overlay) )
	  (setq pam-region-overlay (make-overlay (caar value) (+ (cdar value) 1) buffer))
	  (if (facep 'region)
	      (overlay-put pam-region-overlay 'face 'region)
	      (overlay-put pam-region-overlay 'face 'pam-highlight-face) )

	  (pam-execute-returned-command (car (cddr value)) pam-click-default))
        (message "PAM-3: no overlays found that overlap with the mouse region") ))

;  (if pam-old-mouse-3-action
;      (funcall pam-old-mouse-3-action event) ))
)

(pam-define-minor-mode)

(provide 'pam)
