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
#include "env.h"
#include "hash.h"

struct env 
{
  env parent;
  region sameregion r;
  hash_table entries;
};

/* Create a new, empty environment with ancestor 'parent'.
   The environment, and any future contents, are allocated in r */
env new_env(region r, env parent)
{
  env e = ralloc(r, struct env);

  e->parent = parent;
  e->r = r;
  e->entries = NULL;

  return e;
}

/* Return parent environment of e */
env env_parent(env e)
{
  return e->parent;
}

/* Return region of e */
region env_region(env e)
{
  return e->r;
}

/* Find entry s in in environment e. If not found, check ancestors
   except if this_level_only is true.
   Returns entry's value if s is found, NULL otherwise */
void *env_lookup(env e, const char *s, bool this_level_only)
{
  for (;;)
    {
      void *value;

      if (e->entries && hash_table_lookup(e->entries, (void *) s, &value))
	return value;
      if (this_level_only || !e->parent) 
	return NULL;
      e = e->parent;
    }
}

/* Add an entry for s, with value 'value' to environment e.
   Behaviour is undefined if e already contains an entry for s.
   Does not copy s. */
void env_add(env e, const char *s, void *value)
{
  if (e->entries == NULL)
    e->entries = make_string_hash_table(e->r, 10);
  hash_table_insert(e->entries, (void *) s, value);
}

void env_scan(env e, env_scanner *scanner)
{
  if (e->entries)
    hash_table_scan(e->entries, scanner);
  else
    scanner->ht = NULL;
}

bool env_next(env_scanner *scanner, const char **name, void **value)
{
  if (!scanner->ht)
    return FALSE;

  return hash_table_next(scanner, (void **) name, value);
}
