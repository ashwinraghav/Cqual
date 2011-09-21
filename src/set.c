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

#include <assert.h>
#include "bool.h"
#include "regions.h"
#include "set.h"

/**************************************************************************
 *                                                                        *
 * Types and Globals                                                      *
 *                                                                        *
 **************************************************************************/

struct Set
{
  void *elt;
  struct Set *next;
};

/**************************************************************************
 *                                                                        *
 * Functions                                                              *
 *                                                                        *
 **************************************************************************/

/* The empty set */
set empty_set = NULL;

/* Return a copy of s in region r */
set set_copy(region r, set s)
{
  set new, old, tmp;

  new = empty_set;
  for (old = s; old; old = old->next)
    {
      tmp = ralloc(r, struct Set);
      tmp->elt = old->elt;
      tmp->next = new;
      new = tmp;
    }
  return new;
}

/* Return TRUE iff s is empty */
bool set_empty(set s)
{
  return s == NULL;
}

/* Return TRUE if elt in s */
bool set_member(set_cmp_fn cmp, set s, void *elt)
{
  for (; s; s = s->next)
    if (!cmp(s->elt, elt))
      return TRUE;
  return FALSE;
}

/* Return the number of elements in s */
int set_size(set s)
{
  int i;

  for (i = 0; s; s = s->next)
    i++;
  return i;
}

/* Add elt to s.  If elt is not in s or s is a bag, return TRUE.
   Otherwise return FALSE. */
void set_insert(region r, set_cmp_fn cmp, bool bag, set *s, void *elt)
{
  struct Set *new_elt;

  if (!bag && set_member(cmp, *s, elt))
    return;

  /* elt was not in s, or s->bag was TRUE */
  new_elt = ralloc(r, struct Set);
  new_elt->elt = elt;
  new_elt->next = *s;
  *s = new_elt;
}

/* Remove all copies of elt from s. Return TRUE if elt was in s. */
void set_remove(set_cmp_fn cmp, bool bag, set *s, void *elt)
{
  while (*s)
    {
      if (!cmp((*s)->elt, elt))
	{
	  *s = (*s)->next;
	  if (!bag)
	    break;
	}
      else
	s = &(*s)->next;
    }
}

/* Return TRUE if s1 <= s2 */
bool set_subset(set_cmp_fn cmp, set s1, set s2)
{
  for (; s1; s1 = s1->next)
    if (!set_member(cmp, s2, s1->elt))
      return FALSE;
  return TRUE;
}

/* Return the union of s1 and s2.  Destructive operation */
set set_union(set_cmp_fn cmp, bool bag, set s1, set s2)
{
  if (bag)
    {
      /* For bags we can just link the lists together */
      set *s;

      for (s = &s2; *s; s = &(*s)->next);
      *s = s1;
      return s2;
    }
  else
    {
      /* For sets we need to watch out for duplicates */
      set next;

      for (; s2; s2 = next)
	{
	  next = s2->next;
	  if (!set_member(cmp, s1, s2->elt))
	    {
	      s2->next = s1;
	      s1 = s2;
	    }
	}
      return s1;
    }
}

/* Return TRUE iff s is a singleton set */
bool set_single(set s)
{
  return s && !s->next;
}

/* Sort s.  If you call set_sort just before using scan_set then
   the set elements will be scanned in sorted order. */
static set_cmp_fn temp_cmp = NULL;

int set_sort_cmp(const void *left, const void *right)
{
  void *left_elt = *(void **) left;
  void *right_elt = *(void **) right;
  return temp_cmp(left_elt, right_elt);
}

/* Sort s.  If you call set_sort just before using scan_set then
   the set elements will be scanned in sorted order. */
void set_sort(set_cmp_fn cmp, set s)
{
  region scratch_region;
  set cur, *sorted;
  int i, count;

  scratch_region = newregion();
  count = set_size(s);
  sorted = rarrayalloc(scratch_region, count, void *);
  for (i = 0, cur = s; i < count; i++, cur = cur->next)
    sorted[i] = cur->elt;
  assert(temp_cmp == NULL);
  temp_cmp = cmp;
  qsort(sorted, count, sizeof(void *), set_sort_cmp);
  temp_cmp = NULL;
  for (i = 0; i < count; i++)
    {
      s->elt = sorted[i];
      s = s->next;
    }
  deleteregion(scratch_region);
}

/* Remove duplicates from s.  Only makes sense if s is a bag. */
void set_remove_dups(set_cmp_fn cmp, bool bag, set s)
{
  set cur;

  assert(bag);
  set_sort(cmp, s);
  cur = s;
  while (cur && cur->next)
    {
      while (cur->next && !cmp(cur->elt, cur->next->elt))
	cur->next = cur->next->next;
      cur = cur->next;
    }
}

/* Iterate over elements of s */
void set_scan(set s, set_scanner *ss)
{
  *ss = s;
}

void *set_next(set_scanner *ss)
{
  void *result;

  if (!*ss)
    return NULL;
  result = (*ss)->elt;
  *ss = (*ss)->next;
  return result;
}
