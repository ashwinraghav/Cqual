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

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include "bitset.h"
#include "bool.h"
#include "regions.h"

/**************************************************************************
 *                                                                        *
 * A set represented as a bunch of bits                                   *
 *                                                                        *
 * A bit at position i is true if the ith element is in the set, false    *
 * otherwise.                                                             *
 *                                                                        *
 **************************************************************************/

#define BITS_PER_BYTE 8
#define BITS_PER_UINT (sizeof(unsigned int)*BITS_PER_BYTE)
#define UINT_MAX 0xffffffff

/* Compute a/b, rounded up to the nearest integer.  Only works when a
   and b are positive integers. */
static inline unsigned int div_ceil(unsigned int a, unsigned int b)
{
  return ((a/b) + (a%b ? 1 : 0));
}

/* Compute which unsigned int elt would be in in a bitset of nbits
   bits.  Includes skip over length byte. */
static inline unsigned int compute_offset(bitset b, unsigned int elt)
{
  unsigned int i;

  assert(elt < b[0]);
  for (i = 0; elt > BITS_PER_UINT; i++, elt = elt - BITS_PER_UINT);
  return i + 1;
}

/* Construct a bitmask for element elt */
static inline unsigned int compute_mask(bitset b, unsigned int elt)
{
  unsigned int a, d, c;

  assert(elt < b[0]);
  a = BITS_PER_UINT;
  d = elt % BITS_PER_UINT;
  c = 1 << d;
  return 1 << (elt % BITS_PER_UINT);
}

/* Compute the number of elements of b.bits */
static inline unsigned int bitset_length(bitset b)
{
  return div_ceil(b[0], BITS_PER_UINT);
}

/* Return the number of unsigned int's to allocate for a bitset of
   size nbits. */
unsigned int sizeof_bitset(unsigned int nbits)
{
  return div_ceil(nbits, BITS_PER_UINT) + 1;
}

/* Allocate a bit set of size nbits in region r. */
bitset bitset_new(region r, unsigned int nbits)
{
  unsigned int num_uints;
  bitset new_set;

  num_uints = sizeof_bitset(nbits);
  assert(nbits >= 0);
  new_set = rarrayalloc(r, num_uints, unsigned int);
  new_set[0] = nbits;
  return new_set;
}

/* Make a copy of b */
bitset bitset_copy(region r, bitset b)
{
  unsigned int num_uints;
  bitset new_set;

  num_uints = bitset_length(b);
  new_set = rarrayalloc(r, num_uints + 1, unsigned int);
  rarraycopy(new_set, b, num_uints + 1, unsigned int);
  return new_set;
}

/* Replace bitset b1 by b2.  b1 and b2 must be the same size. */
void bitset_assign(bitset b1, bitset b2)
{
  unsigned int num_uints;
  int i;

  assert(bitset_length(b1) == bitset_length(b2));
  num_uints = bitset_length(b1);
  for (i = 0; i < num_uints; i++)
    b1[i+1] = b2[i+1];
}

/* Return TRUE iff bitset b is empty */
bool bitset_empty(bitset b)
{
  int num_uints;
  int i;

  num_uints = bitset_length(b);
  for (i = 0; i < num_uints; i++)
    if (b[i+1])
      return FALSE;
  return TRUE;
}

/* Return TRUE iff elements of b from first to last, inclusive, are
   all empty */
bool bitset_empty_range(bitset b, unsigned int first, unsigned int last)
{
  int i;

  for (i = first; i <= last; i++)
    if (bitset_member(b, i))
      return FALSE;
  return TRUE;
}

/* Return TRUE iff elements of b from first to last, inclusive, are
   all present */
bool bitset_full_range(bitset b, unsigned int first, unsigned int last)
{
  int i;

  for (i = first; i <= last; i++)
    if (!bitset_member(b, i))
      return FALSE;
  return TRUE;
}

/* Add all possible elements to b. */
void bitset_insert_all(bitset b)
{
  int num_ints;
  int i;

  num_ints = bitset_length(b);
  for (i = 0; i < num_ints; i++)
    b[i+1] = UINT_MAX;
}

/* Add elt to bitset b of nbits bits.  Returns TRUE if elt was not
   already in the set. */
bool bitset_insert(bitset b, unsigned int elt)
{
  int offset = compute_offset(b, elt);
  int mask = compute_mask(b, elt);

  if (b[offset] & mask)
    return FALSE;
  else
    {
      b[offset] = b[offset] | mask;
      return TRUE;
    }
}

/* Remove elt from bitset b.  Returns TRUE if elt was in the set. */
bool bitset_remove(bitset b, unsigned int elt)
{
  int offset = compute_offset(b, elt);
  int mask = compute_mask(b, elt);

  if (b[offset] & mask)
    {
      b[offset] = b[offset] & (~mask);
      return TRUE;
    }
  else
    return FALSE;
}

/* Return TRUE iff elt is in b */
bool bitset_member(bitset b, unsigned int elt)
{
  int offset = compute_offset(b, elt);
  int mask = compute_mask(b, elt);

  if (b[offset] & mask)
    return TRUE; /* Prevent truncating conversions to bool */
  return FALSE;
}

/* Replace b1 by b1 & b2.  Destructively updates b1.  b2 is unchanged.
   Returns TRUE iff b1 was changed. */
bool bitset_intersect(bitset b1, const bitset b2)
{
  int num_uints, i;
  bool changed;

  assert(bitset_length(b1) == bitset_length(b2));
  num_uints = bitset_length(b1);
  changed = FALSE;
  for (i = 0; i < num_uints; i++)
    {
      if (b1[i+1] != (b1[i+1] & b2[i+1]))
	{
	  b1[i+1] = (b1[i+1] & b2[i+1]);
	  changed = TRUE;
	}
    }
  return changed;
}

/* Print b to stdout */
void bitset_print(bitset b)
{
  int num_uints, i;

  num_uints = bitset_length(b);
  for (i = 0; i < num_uints; i++)
    printf("%08x", b[i+1]);
}

/* Return a hash for b */
int bitset_hash(bitset b)
{
  int result, num_uints, i;

  result = b[0];
  num_uints = bitset_length(b);
  for (i = 0; i < num_uints; i++)
    result = result*77 + ((int) b[i+1]);
  return result;
}

/* Return TRUE iff b1 and b2 are equal */
bool bitset_eq(bitset b1, bitset b2)
{
  int num_uints, i;

  assert(bitset_length(b1) == bitset_length(b2));
  num_uints = bitset_length(b1);
  for (i = 0; i < num_uints; i++)
    if (b1[i+1] != b2[i+1])
      return FALSE;
  return TRUE;
}
