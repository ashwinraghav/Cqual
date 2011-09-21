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

(defmacro defset (name cmp-fn element-type)
  `(defset* ',name ',cmp-fn ',element-type))

(defmacro defbag (name element-type)
  `(defbag* ',name ',element-type))

(defmacro defmap (name domain-type range-type hash-fn keyeq-fn)
  `(defmap* ',name ',domain-type ',range-type ',hash-fn ',keyeq-fn))

(setq sets nil)
(setq bags nil)
(setq maps nil)

(defun defset* (name cmp-fn element-type)
  (if (or (assoc name sets)
	  (assoc name bags)
	  (assoc name maps))
      (message (format "Set %s already declared" name))
    (setq sets (append sets (list (list name cmp-fn element-type))))))

(defun defbag* (name element-type)
  (if (or (assoc name sets)
	  (assoc name bags)
	  (assoc name maps))
      (message (format "Bag %s already declared" name))
    (setq bags (append bags (list (list name element-type))))))

(defun defmap* (name domain-type range-type hash-fn keyeq-fn)
  (if (or (assoc name sets)
	  (assoc name bags)
	  (assoc name maps))
      (message (format "Map %s already declared" name))
    (setq maps (append maps (list (list name domain-type range-type hash-fn
				  keyeq-fn))))))

(defun build-file (file-name)
  (let ((buffer (create-file-buffer file-name))
	(debug-on-error (or debug-on-error
			    (interactive-p))))
    (if debug-on-error
	(switch-to-buffer buffer)
      (set-buffer buffer))
    (fill-buffer)
    (write-file file-name)
    (unless debug-on-error
      (kill-buffer buffer))))

(defun copyright-notice ()
  (insert "/* Automatically generated file, do not edit. See containters.def */\n\n")
  (insert "/* Copyright (c) 2000-200 The Regents of the University of California. */\n")
  (insert "/* See the copyright notice in nodetypes.def for distribution restrictions */\n"))

(defun write-intro ()
  (copyright-notice)
  (insert "#ifndef CONTAINERS_H\n"
	  "#define CONTAINERS_H\n"
	  "#include \"AST.h\"\n"
	  "#include \"cqual.h\"\n"
	  "#include \"set.h\"\n"))

(defun write-epilogue ()
  (insert "#endif\n"))

(defun fill-set (name cmp-fn element-type)
  (insert (format "typedef struct { set s; } *%s;\n" name)
	  (format "typedef struct { set_scanner ss; } %s_scanner;\n" name)
	  (format "extern int %s(%s, %s);\n" cmp-fn element-type element-type)
	  (format "static inline %s empty_%s(void) { return (%s) empty_set; }\n" name name name)
	  (format "static inline %s %s_copy(region r, %s s) { return (%s) set_copy(r, (set) s); }\n" name name name name)
	  (format "static inline bool %s_empty(%s s) { return set_empty((set) s); }\n" name name)
	  (format "static inline bool %s_member(%s s, %s elt) { return set_member((set_cmp_fn) %s, (set) s, (void *) elt); }\n" name name element-type cmp-fn)
	  (format "static inline int %s_size(%s s) { return set_size((set) s); }\n" name name)
	  (format "static inline void %s_insert(region r, %s *s, %s elt) { set_insert(r, (set_cmp_fn) %s, FALSE, (set *) s, (void *) elt); }\n" name name element-type cmp-fn)
	  (format "static inline void %s_remove(%s *s, %s elt) { set_remove((set_cmp_fn) %s, FALSE, (set *) s, (void *) elt); }\n" name name element-type cmp-fn)
	  (format "static inline bool %s_subset(%s s1, %s s2) { return set_subset((set_cmp_fn) %s, (set) s1, (set) s2); }\n" name name name cmp-fn)
	  (format "static inline %s %s_union(%s s1, %s s2) { return (%s) set_union((set_cmp_fn) %s, FALSE, (set) s1, (set) s2); }\n" name name name name name cmp-fn)
	  (format "static inline bool %s_single(%s s) { return set_single((set) s); }\n" name name)
	  (format "static inline void %s_sort(%s s) { set_sort((set_cmp_fn) %s, (set) s); }\n" name name cmp-fn)
	  (format "static inline void %s_scan(%s s, %s_scanner *ss) { set_scan((set) s, &ss->ss); }\n" name name name)
	  (format "static inline %s %s_next(%s_scanner *ss) { return (%s) set_next(&ss->ss); }\n" element-type name name element-type)
	  (format "#define scan_%s(var, scanner, set) for (%s_scan(set, &scanner), var = %s_next(&scanner); var; var = %s_next(&scanner))\n\n" name name name name)
))

(defun fill-bag (name element-type)
  (insert (format "typedef struct { set s; } *%s;\n" name)
	  (format "typedef struct { set_scanner ss; } %s_scanner;\n" name)
	  (format "static inline %s empty_%s(void) { return (%s) empty_set; }\n" name name name)
	  (format "static inline %s %s_copy(region r, %s s) { return (%s) set_copy(r, (set) s); }\n" name name name name)
	  (format "static inline bool %s_empty(%s s) { return set_empty((set) s); }\n" name name)
	  (format "static inline int %s_size(%s s) { return set_size((set) s); }\n" name name)
	  (format "static inline void %s_insert(region r, %s *s, %s elt) { set_insert(r, NULL, TRUE, (set *) s, (void *) elt); }\n" name name element-type)
	  (format "static inline bool %s_subset(%s s1, %s s2) { return set_subset(NULL, (set) s1, (set) s2); }\n" name name name)
	  (format "static inline %s %s_union(%s s1, %s s2) { return (%s) set_union(NULL, TRUE, (set) s1, (set) s2); }\n" name name name name name)
	  (format "static inline bool %s_single(%s s) { return set_single((set) s); }\n" name name)
	  (format "static inline void %s_sort(int (*cmp)(%s, %s), %s s) { set_sort((set_cmp_fn) cmp, (set) s); }\n" name name name name)
	  (format "static inline void %s_remove_dups(int (*cmp)(%s, %s), %s s) { set_remove_dups((set_cmp_fn) cmp, TRUE, (set) s); }\n" name element-type element-type name)
	  (format "static inline void %s_scan(%s s, %s_scanner *ss) { set_scan((set) s, &ss->ss); }\n" name name name)
	  (format "static inline %s %s_next(%s_scanner *ss) { return (%s) set_next(&ss->ss); }\n" element-type name name element-type)
	  (format "#define scan_%s(var, scanner, set) for (%s_scan(set, &scanner), var = %s_next(&scanner); var; var = %s_next(&scanner))\n\n" name name name name)
))

(defun fill-map (name domain-type range-type hash-fn keyeq-fn)
  (insert (format "typedef struct { hash_table ht; } *%s;\n" name)
	  (format "typedef struct { hash_table_scanner hts; } %s_scanner;\n" name)
	  (format "typedef struct { hash_table_scanner_sorted htss; } %s_scanner_sorted;\n" name)
	  (format "extern int %s(%s);\n" hash-fn domain-type)
	  (format "extern bool %s(%s, %s);\n" keyeq-fn domain-type domain-type)
	  (format "static inline %s make_%s(region r, int size) { return (%s) make_hash_table(r, size, (hash_fn) %s, (keyeq_fn) %s); }\n" name name name hash-fn keyeq-fn)
	  (format "static inline void %s_reset(%s m) { hash_table_reset((hash_table) m); }\n" name name)
	  (format "static inline int %s_size(%s m) { return hash_table_size((hash_table) m); }\n" name name)
	  (format "static inline bool %s_lookup(%s m, %s k, %s *d) { return hash_table_lookup((hash_table) m, (hash_key) k, (hash_data *) d); }\n" name name domain-type range-type)
	  (format "static inline bool %s_insert(%s m, %s k, %s d) { return hash_table_insert((hash_table) m, (hash_key) k, (hash_data) d); }\n" name name domain-type range-type)
	  (format "static inline bool %s_remove(%s m, %s k) { return hash_table_remove((hash_table) m, (hash_key) k); }\n" name name domain-type)
	  (format "static inline %s %s_copy(region r, %s m) { return (%s) hash_table_copy(r, (hash_table) m); }\n" name name name name)
	  (format "static inline %s %s_map(region r, %s m, %s (*f)(%s, %s, void *), void *arg) { return (%s) hash_table_map(r, (hash_table) m, (hash_map_fn) f, arg); }\n" name name name range-type domain-type range-type name)
	  (format "static inline void %s_scan(%s m, %s_scanner *ms) { hash_table_scan((hash_table) m, &ms->hts); }\n" name name name)
	  (format "static inline bool %s_next(%s_scanner *ms, %s *k, %s *d) { return hash_table_next(&ms->hts, (hash_key *) k, (hash_data *) d); }\n" name name domain-type range-type)
	  (format "static inline void %s_scan_sorted(%s m, int (*f)(%s, %s), %s_scanner_sorted *mss) { hash_table_scan_sorted((hash_table) m, (keycmp_fn) f, &mss->htss); }\n" name name domain-type domain-type name)
	  (format "static inline bool %s_next_sorted(%s_scanner_sorted *ms, %s *k, %s *d) { return hash_table_next_sorted(&ms->htss, (hash_key *) k, (hash_data *) d); }\n" name name domain-type range-type)
))

(defun fill-buffer ()
  (write-intro)
  (mapcar (lambda (args) (apply #'fill-set args)) sets)
  (insert "\n\n")
  (mapcar (lambda (args) (apply #'fill-bag args)) bags)
  (insert "\n\n")
  (mapcar (lambda (args) (apply #'fill-map args)) maps)
  (write-epilogue)
)

(load-file "containers.def")
(build-file "containers.h")
