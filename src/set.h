/* This file is part of cqual.
   Copyright (C) 2000-2001 The Regents of the University of California.

cqual is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

cqual is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with cqual; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

#ifndef SET_H
#define SET_H

#include "bool.h"
#include "linkage.h"
#include "regions.h"

EXTERN_C_BEGIN

typedef struct Set *set;
typedef set set_scanner;

/* Fn for comparing set elements.  Should return 0 if e1 = e2, <0 if
   e1 < e2, and >0 if e1 > e2 */
typedef int (*set_cmp_fn)(void *e1, void *e2);

/* The empty set */
extern set empty_set;

/* Return a copy of s in r */
set set_copy(region r, set s);

/* Return TRUE iff s is empty */
bool set_empty(set s);

/* Return TRUE if elt in s */
bool set_member(set_cmp_fn cmp, set s, void *elt);

/* Return the number of elements in s */
int set_size(set s);

/* Add elt to s. */
void set_insert(region r, set_cmp_fn cmp, bool bag, set *s, void *elt);

/* Remove all copies of elt from s. */
void set_remove(set_cmp_fn cmp, bool bag, set *s, void *elt);

/* Return TRUE if s1 <= s2 */
bool set_subset(set_cmp_fn cmp, set s1, set s2);

/* Return the union of s1 and s2.  Destructive operation */
set set_union(set_cmp_fn cmp, bool bag, set s1, set s2);

/* Return TRUE iff s is a singleton set */
bool set_single(set s);

/* Sort s.  If you call set_sort just before using scan_set then
   the set elements will be scanned in sorted order. */
void set_sort(set_cmp_fn cmp, set s);

/* Remove duplicates from s.  Only makes sense if s is a bag. */
void set_remove_dups(set_cmp_fn cmp, bool bag, set s);

/* Iterate over elements of s */
void set_scan(set s, set_scanner *ss);
void *set_next(set_scanner *ss);

#define scan_set(var, scanner, set) \
  for (set_scan(set, &scanner), var = set_next(&scanner); \
       var; var = set_next(&scanner))

EXTERN_C_END

#endif
