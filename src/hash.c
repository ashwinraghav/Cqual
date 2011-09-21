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

#include "parser.h"
#include "hash.h"
#include "regions.h"
#include "utils.h"

struct bucket
{
  hash_key key;
  hash_data data;
  struct bucket *next;
};

#define scan_bucket(b, var) for (var = b; var; var = var->next)

struct Hash_table
{
  region r;         /* Region for this table */
  hash_fn hash;     /* Function for hashing keys */
  keyeq_fn cmp;     /* Function for comparing keys */

  int size;         /* Number of buckets */
  int elts;         /* Number of elements */
  bucket *table;    /* Array of (size) buckets */
};

static void rehash(hash_table ht);

/* Make a new hash table, with size buckets initially.  Hash table
   elements are allocated in region rhash. */
hash_table make_hash_table(region r, int size, hash_fn hash, keyeq_fn cmp)
{
  hash_table result;

  assert(size > 0);
  result = ralloc(r, struct Hash_table);
  result->r = r;
  result->hash = hash;
  result->cmp = cmp;
  result->size = size;
  result->elts = 0;
  result->table = rarrayalloc(result->r, size, bucket);

  return result;
}

/* Make a hash table for strings. */
hash_table make_string_hash_table(region rhash, int size)
{
  return make_hash_table(rhash, size, (hash_fn) string_hash,
			 (keyeq_fn) string_eq);
}

/* Zero out ht.  Doesn't reclaim bucket space. */
void hash_table_reset(hash_table ht)
{
  int i;

  ht->elts = 0;
  for (i = 0; i < ht->size; i++)
    ht->table[i] = NULL;
}

/* Return the number of entries in ht */
int hash_table_size(hash_table ht)
{
  return ht->elts;
}

/* Return the bucket corresponding to k in ht */
static inline bucket *find_bucket(hash_table ht, hash_key k)
{
  int hash;

  hash = ht->hash(k);
  if (hash < 0)
    hash = -1*hash;
  return &ht->table[hash % ht->size];
}

/* Lookup k in ht.  Returns corresponding data in *d, and function
   result is TRUE if the k was in ht, false otherwise. */
bool hash_table_lookup(hash_table ht, hash_key k, hash_data *d)
{
  bucket cur;

  cur = *find_bucket(ht, k);
  while (cur)
    {
      if (ht->cmp(k, cur->key))
	{
	  if (d)
	    *d = cur->data;
	  return TRUE;
	}
      cur = cur->next;
    }
  return FALSE;
}

/* Add k:d to ht.  If k was already in ht, replace old entry by k:d.
   Rehash if necessary.  Returns TRUE if k was not already in ht. */
bool hash_table_insert(hash_table ht, hash_key k, hash_data d)
{
  bucket *cur;

  if (ht->elts > ht->size*15)
    rehash(ht);
  cur = find_bucket(ht, k);
  while (*cur)
    {
      if (ht->cmp(k, (*cur)->key))
	{
	  (*cur)->data = d;
	  return FALSE; /* Replace */
	}
      cur = &(*cur)->next;
    }
  *cur = ralloc(ht->r, struct bucket);
  (*cur)->key = k;
  (*cur)->data = d;
  (*cur)->next = NULL;
  ht->elts++;
  return TRUE; /* New key */
}

/* Remove mapping for k in ht.  Returns TRUE if k was in ht. */
bool hash_table_remove(hash_table ht, hash_key k) 
{
  bucket *cur;
  bucket prev = NULL;

  cur = find_bucket(ht, k);
  while (*cur)
    {
      if (ht->cmp(k, (*cur)->key))
	{
	  if (prev)
	    prev->next = (*cur)->next;
	  else
	    *cur = NULL;
	  ht->elts--;
	  return TRUE;
	}
      prev = *cur;
      cur = &(*cur)->next;
    }
  return FALSE;
}

/* Return a copy of ht */
hash_table hash_table_copy(region r, hash_table ht)
{
  int i;
  hash_table result;
  bucket cur, newbucket, *prev;

  result = make_hash_table(r, ht->size, ht->hash, ht->cmp);
  result->elts = ht->elts;
  
  for (i = 0; i < ht->size; i++)
    {
      prev = &result->table[i];
      scan_bucket(ht->table[i], cur)
	{
	  newbucket = ralloc(result->r, struct bucket);
	  newbucket->key = cur->key;
	  newbucket->data = cur->data;
	  newbucket->next = NULL;
	  assert(!*prev);
	  *prev = newbucket;
	  prev = &newbucket->next;
	}
    }
  return result;
  /*
  hash_table result;
  hash_table_scanner hts;
  hash_key k;
  hash_data d;
  
  result = make_hash_table(r, ht->size, ht->hash, ht->cmp);
  hash_table_scan(ht, &hts);
  while (hash_table_next(&hts, &k, &d))
    insist(hash_table_insert(result, k, d));
  
  return result;
  */
}

/* Increase size of ht (double it) and reinsert all the elements */
static void rehash(hash_table ht)
{
  int old_table_size, i;
  bucket *old_table, cur;

#ifdef DEBUG
  printf("Rehash table size=%d, elts=%d\n", ht->size, ht->elts);
#endif

  old_table_size = ht->size;
  old_table = ht->table;

  ht->size = ht->size*2;
  ht->elts = 0;
  ht->table = rarrayalloc(ht->r, ht->size, bucket);

  for (i = 0; i < old_table_size; i++)
    scan_bucket(old_table[i], cur)
      insist(hash_table_insert(ht, cur->key, cur->data));
}

/* Begin scanning ht */
void hash_table_scan(hash_table ht, hash_table_scanner *hts)
{
  hts->ht = ht;
  hts->i = 0;
  hts->cur = hts->ht->table[0];
}

/* Get next elt in table, storing the elt in *k and *d if k and d are
   non-NULL, respectively.  Returns TRUE if there is a next elt, FALSE
   otherwise. */
bool hash_table_next(hash_table_scanner *hts, hash_key *k, hash_data *d)
{
  while (hts->cur == NULL)
    {
      hts->i++;
      if (hts->i < hts->ht->size)
	hts->cur = hts->ht->table[hts->i];
      else
	break;
    }

  if (hts->i == hts->ht->size)
    {
      return FALSE;
    }
  else
    {
      if (k)
	*k = hts->cur->key;
      if (d)
	*d = hts->cur->data;
      hts->cur = hts->cur->next;
    }
  return TRUE;
}

/* Apply f to all elements of ht, in some arbitrary order */
void hash_table_apply(hash_table ht, hash_apply_fn f, void *arg)
{
  int i;
  bucket cur;

  for (i = 0; i < ht->size; i++)
    scan_bucket(ht->table[i], cur)
      f(cur->key, cur->data, arg);
}

/* Map f to all elements on ht, creating a new hash table in region r */
hash_table hash_table_map(region r, hash_table ht, hash_map_fn f, void *arg)
{
  int i;
  hash_table result;
  bucket cur, newbucket, *prev;

  result = make_hash_table(r, ht->size, ht->hash, ht->cmp);
  result->elts = ht->elts;
  
  for (i = 0; i < ht->size; i++)
    {
      prev = &result->table[i];
      scan_bucket(ht->table[i], cur)
	{
	  newbucket = ralloc(result->r, struct bucket);
	  newbucket->key = cur->key;
	  newbucket->data = f(cur->key, cur->data, arg);
	  newbucket->next = NULL;
	  assert(!*prev);
	  *prev = newbucket;
	  prev = &newbucket->next;
	}
    }
  return result;
  /*
  hash_table result;
  int i;
  bucket cur;

  result = make_hash_table(ht->r, ht->size, ht->hash, ht->cmp);
  for (i = 0; i < ht->size; i++)
    scan_bucket(ht->table[i], cur)
      insist(hash_table_insert(result, cur->key, f(cur->key, cur->data, arg)));
  return result;
  */
}

static keycmp_fn cur_cmp = NULL;

int entry_cmp(const void *a, const void *b)
{
  struct sorted_entry *ae = (struct sorted_entry *) a;
  struct sorted_entry *be = (struct sorted_entry *) b;
  return cur_cmp(ae->k, be->k);
}

/* Begin scanning ht in sorted order according to f */
void hash_table_scan_sorted(hash_table ht, keycmp_fn f,
			    hash_table_scanner_sorted *htss)
{
  hash_table_scanner hts;
  int i;

  htss->r = newregion();
  htss->size = hash_table_size(ht);
  htss->entries = rarrayalloc(htss->r, htss->size, struct sorted_entry);
  htss->i = 0;

  hash_table_scan(ht, &hts);
  i = 0;
  while (hash_table_next(&hts, &htss->entries[i].k,
			 &htss->entries[i].d))
    i++;
  assert(i == htss->size);
  cur_cmp = f;
  qsort(htss->entries, htss->size, sizeof(struct sorted_entry), entry_cmp);
  cur_cmp = NULL;
}

/* Just like hash_table_next, but scans in sorted order */
bool hash_table_next_sorted(hash_table_scanner_sorted *htss, hash_key *k,
			    hash_data *d)
{
  if (htss->i < htss->size)
    {
      if (k)
	*k = htss->entries[htss->i].k;
      if (d)
	*d = htss->entries[htss->i].d;
      htss->i++;
      return TRUE;
    }
  else
    {
      deleteregion(htss->r);
      htss->r = NULL;
      return FALSE;
    }
}
