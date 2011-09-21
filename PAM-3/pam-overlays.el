; overlay table maintenance

(require 'pam-vars)
(require 'pam-util)
(require 'avltree)
(require 'pam-copyright)

; variables:

(defvar pam-global-overlay-list nil)


; functions:

(defun pam-overlays-init ()
  (setq pam-global-overlay-list nil) )

(defun pam-compare-fun (A B)
  "Returns non-nil if A is less than B, and nil otherwise"
  (if (not (cadr A))                   ; if the first overlay value is nil
      (if (not (cadr B))               ; they're both nil
	  (or (< (caar A) (caar B))
	      (< (cdar A) (cdar B)) )
      nil )
      (if (not (cadr B))
	  t
	  (< (cdar A) (caar B)) )))

(defun pam-ov-list-find-element (list name)
  (let ((val (assoc name list)))
    (if val
	(cdr val)
        nil )))

(defun pam-insert-overlay-global (file range overlay value)
  (let* ((avltree (pam-ov-list-find-element pam-global-overlay-list file))
	 (new-range-lo (car range))
	 (new-range-hi (- (cdr range) 1))   ; accout for the one-off error of using the range (16 . 17) to
					    ; represent only an overlay on character 16
	 (new-range (cons new-range-lo new-range-hi)) )
    (if avltree
	(avltree-enter avltree (list new-range overlay value))
        (let ((avltree (avltree-create (lambda (pos data) (pam-compare-fun pos data)))))
	  (avltree-enter avltree (list new-range overlay value))
	  (setq pam-global-overlay-list (cons (cons file avltree)
					      pam-global-overlay-list ))))))


(defun pam-lookup-overlay-global (file pos)
  (let* ((avltree (pam-ov-list-find-element pam-global-overlay-list file)) )
    (if avltree
	(car (cddr (avltree-member avltree (list (cons pos pos) t 'nothing))))
        nil )))

(defun pam-lookup-range-overlay-global (file lo hi)
  (let ((avltree (pam-ov-list-find-element pam-global-overlay-list file)))
    (if avltree
	(let ((els-list (avltree-flatten avltree)) 
	      (possible-els nil) )
	  (while els-list
	    (let* ((el (car els-list))
		   (lo-el (caar el))
		   (hi-el (cdar el)) )
	      (if (not (or (< hi lo-el)      ; i.e. the high point of the chosen region is below the low point
					     ; of the overlay region;
					; clearly they cannot overlap
			   (> lo hi-el) ))   ; i.e. the low point of the chosen region is above the high point of the overlay region;
					; again, clearly they cannot overlap
		  (setq possible-els (cons el possible-els)) )
	      (setq els-list (cdr els-list)) ))
	  (if possible-els
	      (return-minimum-distance-element possible-els lo hi)
	      nil ))
        nil )))

(defun return-minimum-distance-element (els lo hi)
  (let ((current-best-value nil)
	(current-best-distance 9999999)) ; a big number to make sure it's less than this number for any element
    (while els
      (let* ((el (car els))
	     (dist (el-distance el lo hi)) )
	(if (< dist current-best-distance)
	    (progn (setq current-best-value el)
		   (setq current-best-distance dist) ))
	(setq els (cdr els)) ))
    current-best-value ))

(defun el-distance (el lo hi)
  (let ((el-lo (caar el))
	(el-hi (cdar el)) )
    (+ (abs (- el-lo lo))
       (abs (- el-hi hi)) )))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pam-make-returned-command (type name)
  "data abstraction..."
  (cons type name))

(defun pam-add-an-overlay (range buf color callback)
  "Add an overlay"
  (if color
      (let ((new-overlay (make-overlay (car range) (cdr range) buf)))
	(overlay-put new-overlay 'face (eval color))
	(overlay-put new-overlay 'mouse-face 'pam-color-mouse)
	(pam-insert-overlay-global (buffer-name buf) range new-overlay
				   (pam-make-returned-command 'clicked callback) ))
      (pam-insert-overlay-global (buffer-name buf) range nil
				 (pam-make-returned-command 'clicked callback) )))

(defun pam-delete-node-func (node)
  (if (overlayp (cadr node))
      (delete-overlay (cadr node)) ))

(defun pam-delete-from-overlay-list-by-file (file)
  "Delete all overlays in the named file"
  (let ((new-list-reverse nil)
	(trav pam-global-overlay-list) )
    (while trav
      (if (string-equal (caar trav) file)
	  (avltree-map (lambda (node) (pam-delete-node-func node))
		       (cdar trav))
	(setq new-list-reverse (cons (car trav) new-list-reverse)) )
      (setq trav (cdr trav)) )
    (setq pam-global-overlay-list (reverse new-list-reverse)) ))
    

(defun pam-delete-all-overlays ()
  "Delete all of them"
  (let ((trav pam-global-overlay-list))
    (while trav
      (avltree-map (lambda (node) (pam-delete-node-func node))
		   (cdar trav))
      (setq trav (cdr trav)) )
    (setq pam-global-overlay-list nil) ))

(provide 'pam-overlays)