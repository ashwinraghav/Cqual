/* This file is part of cqual.
   Copyright (C) 2000-2002 The Regents of the University of California.

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

#ifndef BITSET_H
#define BITSET_H

#include <stdlib.h>
#include "linkage.h"
#include "bool.h"
#include "regions.h"

EXTERN_C_BEGIN

typedef unsigned int *bitset; /* An array of ints.  First int is number
				 of bits. */

/* Return the number of unsigned int's to allocate for a bitset of
   size nbits. */
unsigned int sizeof_bitset(unsigned int nbits);

/* Allocate a bit set in region r. */
bitset bitset_new(region r, unsigned int nbits);

#define alloca_bitset(nbits) ((unsigned int *) alloca(sizeof_bitset(nbits)*sizeof(unsigned int)))

/* Make a copy of b */
bitset bitset_copy(region r, bitset b);

/* Replace bitset b1 by b2.  b1 and b2 must be the same size. */
void bitset_assign(bitset b1, bitset b2);

/* Return TRUE iff bitset b is empty */
bool bitset_empty(bitset b);

/* Return TRUE iff elements of b from first to last, inclusive, are
   all absent */
bool bitset_empty_range(bitset b, unsigned int first, unsigned int last);

/* Return TRUE iff elements of b from first to last, inclusive, are
   all present */
bool bitset_full_range(bitset b, unsigned int first, unsigned int last);

/* Add all possible elements to b. */
void bitset_insert_all(bitset b);

/* Add elt to bitset b.  Returns TRUE if elt was not already in the set. */
bool bitset_insert(bitset b, unsigned int elt);

/* Remove elt from bitset b.  Returns TRUE if elt was in the set. */
bool bitset_remove(bitset b, unsigned int elt);

/* Return TRUE iff elt is in b */
bool bitset_member(bitset b, unsigned int elt);

/* Replace b1 by b1 & b2.  Destructively updates b1.  b2 is unchanged.
   Returns TRUE iff b1 was changed. */
bool bitset_intersect(bitset b1, const bitset b2);

/* Print b to stdout */
void bitset_print(bitset b);

/* Return a hash for b */
int bitset_hash(bitset b);

/* Return TRUE iff b1 and b2 are equal */
bool bitset_eq(bitset b1, bitset b2);

EXTERN_C_END

#endif
