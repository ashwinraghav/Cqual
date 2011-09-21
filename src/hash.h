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

#ifndef HASH_H
#define HASH_H

#include "bool.h"
#include "linkage.h"

EXTERN_C_BEGIN

typedef void *hash_key;
typedef void *hash_data;

typedef int (*hash_fn)(hash_key k); /* Function to hash a key */
typedef bool (*keyeq_fn)(hash_key k1, hash_key k2);
                                         /* Function returning true iff
					     k1 and k2 are equal */
typedef void (*hash_apply_fn)(hash_key k, hash_data d, void *arg);
                                          /* Function applied to
					     elts in the hash table */
typedef hash_data (*hash_map_fn)(hash_key k, hash_data d, void *arg);
                                          /* Function mapped to
					     elts in the hash table */

typedef struct Hash_table *hash_table;

/* Make a new hash table, with size buckets initially.  Hash table
   elements are allocated in region rhash. */
hash_table make_hash_table(region rhash, int size, hash_fn hash,
			   keyeq_fn cmp);

/* Make a hash table for strings. */
hash_table make_string_hash_table(region rhash, int size);

/* Zero out ht.  Doesn't reclaim bucket space. */
void hash_table_reset(hash_table ht);

/* Return the number of entries in ht */
int hash_table_size(hash_table ht);

/* Lookup k in ht.  If d is not NULL, returns corresponding data in *d.
   Function result is TRUE if the k was in ht, false otherwise. */
bool hash_table_lookup(hash_table ht, hash_key k, hash_data *d);

/* Add k:d to ht.  If k was already in ht, replace old entry by k:d.
   Rehash if necessary.  Returns TRUE if k was not already in ht. */
bool hash_table_insert(hash_table ht, hash_key k, hash_data d);

/* Remove mapping for k in ht.  Returns TRUE if k was in ht. */
bool hash_table_remove(hash_table ht, hash_key k);

/* Return a copy of ht, allocated in rhash */
hash_table hash_table_copy(region rhash, hash_table ht);

/* Map f to all elements on ht, creating a new hash table */
hash_table hash_table_map(region r, hash_table ht, hash_map_fn f, void *arg);

typedef struct bucket *bucket;
typedef struct
{
  hash_table ht;
  int i;
  bucket cur;
} hash_table_scanner; /* Opaque type!  Do not modify fields. */

/* Begin scanning ht */
void hash_table_scan(hash_table ht, hash_table_scanner *);

/* Get next elt in table, storing the elt in *k and *d if k and d are
   non-NULL, respectively.  Returns TRUE if there is a next elt, FALSE
   otherwise. */
bool hash_table_next(hash_table_scanner *, hash_key *k, hash_data *d);

/* Total order on hash table keys, only uesd for hash_table_scan_sorted */
typedef int (*keycmp_fn)(hash_key k1, hash_key k2);

struct sorted_entry
{
  hash_key k;
  hash_data d;
};

typedef struct
{
  region r;
  int i;
  int size;
  struct sorted_entry *entries;
} hash_table_scanner_sorted;

/* Begin scanning ht in sorted order according to f */
void hash_table_scan_sorted(hash_table ht, keycmp_fn f,
			    hash_table_scanner_sorted *htss);

/* Just like hash_table_next, but scans in sorted order */
bool hash_table_next_sorted(hash_table_scanner_sorted *htss, hash_key *k,
			    hash_data *d);


EXTERN_C_END

#endif
