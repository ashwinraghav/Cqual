;  This file is part of cqual.
;  Copyright (C) 2000-2001 The Regents of the University of California.
;
; cqual is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2, or (at your option)
; any later version.
;
; cqual is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with cqual; see the file COPYING.  If not, write to
; the Free Software Foundation, 59 Temple Place - Suite 330,
; Boston, MA 02111-1307, USA.

;; The subtype check support stuff

(setq typefield-width 6)
(setq max-type-depth 4)

(setq type-codes (cons (cons nil 0) nil))
(setq next-subtype-code nil)
(setq type-depths (cons (cons nil -1) nil))

(defun get-next-subtype-code (type)
  (let ((code-pair (assoc type next-subtype-code)))
    (if code-pair
	(let ((next-code (rplacd code-pair (1+ (cdr code-pair)))))
	  (if (= next-code (lsh 1 typefield-width))
	      (message (format "Too many subtypes (max %s) for %s"
			       (1- (lsh 1 typefield-width)) type)))
	  next-code)
      (setq next-subtype-code (cons (cons type 1) next-subtype-code))
      1)))

(defun lookup-code (type)
  (let ((cc (assoc type type-codes)))
    (if cc (cdr cc)
      (let* ((super-type (cdr (assoc type parent-types)))
	     (sub-code (get-next-subtype-code super-type))
	     (super-code (lookup-code super-type))
	     (code (+ super-code (lsh sub-code (* typefield-width (lookup-depth type))))))
	(setq type-codes (cons (cons type code) type-codes))
	code))))
      

(defun lookup-depth (type)
  (let ((dd (assoc type type-depths)))
    (if dd (cdr dd)
      (let ((depth (1+ (lookup-depth (cdr (assoc type parent-types))))))
	(setq type-depths (cons (cons type depth) type-depths))
	depth))))

(load-file "build-basics.el")
(load-file "nodetypes.def")
(check-defs)

(setq all-type-names (append (mapcar #'type-name types)
			     (mapcar #'node-name nodes)))
(setq parent-types
      (append
       (mapcar #'(lambda (type) (cons (type-name type) (type-super-type type)))
	       types)
       (mapcar #'(lambda (node) (cons (node-name node) (node-type node)))
	       nodes)))

(defun fill-buffer ()
  (copyright-notice)
  (mapcar #'write-typedefs types)
  (mapcar #'write-node-typedefs nodes)
  (insert "typedef enum {\n");
  (mapcar #'write-kinds all-type-names)
  (backward-delete-char 2)
  (insert "\n} ast_kind;\n")
  (mapcar #'write-is-test all-type-names))

(defun write-typedefs (type)
  (insert (format "typedef struct AST_%s *%s;\n"
		  (type-name type) (type-name type))))

(defun write-node-typedefs (node)
  (insert (format "typedef struct AST_%s *%s;\n"
		  (node-type node) (node-name node))))

(defun write-kinds (name)
  (insert (format "  kind_%s = 0x%x,\n" name (lookup-code name))))

(defun write-is-test (name)
  (insert (format "#define is_%s(x) ((((x)->kind) & 0x%x) == kind_%s)\n"
		  name (1- (lsh 1 (* typefield-width (1+ (lookup-depth name))))) name)))

(build-file "AST_types.h")



(defun fill-buffer ()
  (copyright-notice)
  (mapcar #'(lambda (type) (write-creator (type-name type) type)) types)
  (mapcar #'(lambda (node) (write-creator (node-name node) (assoc (node-type node) types))) nodes)
  (insert "\n\n")
  (mapcar #'write-type types)
  (mapcar #'write-list all-type-names))

(defun write-type (type)
  (insert (format "/* %s */\n" (type-documentation type)))
  (insert (format "struct AST_%s { /* extends %s */\n"
		  (type-name type) (type-super-type type)))
  (write-fields type)
  (insert "};\n\n"))

(defun write-fields (type)
  (if (type-super-type type)
      (write-fields (assoc (type-super-type type) types))
    (insert "  ast_kind kind;\n"))
  (mapcar '(lambda (field-name)
	     (let ((field (assoc field-name fields)))
	       (insert (format "  %s %s%s;\n" (field-c-type field)
			       (if (assoc 'tree (field-attributes field))
				   "sameregion " "")
			       field-name))))
	  (type-fields type)))

(defun write-creator (name type)
  (write-creator-header name type)
  (insert ";\n"))

(defun write-creator-header (name type)
  (insert (format "%s new_%s(region r" name name))
  (let ((write-creator-fields
	 #'(lambda (type)
	     (if (type-super-type type)
		 (funcall write-creator-fields
			  (assoc (type-super-type type) types)))
	     (mapcar #'(lambda (field-name)
			 (let ((field (assoc field-name fields)))
			   (if (assoc 'init (field-attributes field))
			       (insert (format ", %s %s"
					       (field-c-type field)
					       field-name)))))
		       (type-fields type)))))
      (funcall write-creator-fields type))
  (insert ")"))

(defun write-list (name)
  (insert (format "%s %s_chain(%s l1, %s l2);\n" name name name name))
  (insert (format "#define scan_%s(var, list) for (var = (list); var; var = CAST(%s, var->next))\n" name name)))

(build-file "AST_defs.h")

(defun fill-buffer ()
  (copyright-notice)
  (mapcar #'(lambda (type) (write-creator-source (type-name type) type)) types)
  (mapcar #'(lambda (node) (write-creator-source (node-name node) (assoc (node-type node) types))) nodes)
  (insert "\n\n")
  (mapcar #'write-list-source all-type-names))

(defun write-creator-source (name type)
  (write-creator-header name type)
  (insert "\n{\n")
  (insert (format "  %s obj = ralloc(r, struct AST_%s);\n\n" name (type-name type)))
  (insert (format "  obj->kind = kind_%s;\n" name))
  (let ((write-creator-fields
	 #'(lambda (type)
	     (if (type-super-type type)
		 (funcall write-creator-fields
			  (assoc (type-super-type type) types)))
	     (mapcar #'(lambda (field-name)
			 (let ((field (assoc field-name fields)))
			   (cond ((assoc 'init (field-attributes field))
				  (insert (format "  obj->%s = %s;\n" field-name field-name)))
				 ((assoc 'default (field-attributes field))
				  (insert (format "  obj->%s = %s;\n"
						  field-name
						  (cadr (assoc 'default (field-attributes field)))))))))
		     (type-fields type)))))
    (funcall write-creator-fields type))
  (insert "\n  return obj;\n}\n\n"))

  

(defun write-list-source (name)
  (insert (format "%s %s_chain(%s l1, %s l2)\n" name name name name))
  (insert (format "{ return CAST(%s, ast_chain(CAST(node, l1), CAST(node, l2))); }\n\n" name)))

(build-file "AST_defs.c")

(if (rassoc (1+ max-type-depth) type-depths)
    (message (format "There is at least one type (%s) at depth %s"
		     (car (rassoc (1+ max-type-depth) type-depths))
		     (1+ max-type-depth))))

