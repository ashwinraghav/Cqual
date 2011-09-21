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
#include <stdio.h>
#include <string.h>
#include "AST.h"
#include "c-parse.h"
#include "cqual.h"
#include "flags.h"
#include "hash.h"
#include "analyze.h"
#include "qerror.h"
#include "qtype.h"
#include "quals.h"
#include "utils.h"
#include "effect.h"
#include "regions.h"
#include "containers.h"

/*#define DEBUG*/ /* Print effect constraints */
/*#define STATS*/ /* Print statistics about the constraint graph(s) */
/*#define DEBUGST*/ /* Print store constraints */

/**************************************************************************
 *                                                                        *
 * Globals                                                                *
 *                                                                        *
 **************************************************************************/

static void unify_constrs(effect *c1, effect *c2);
int cmp_store(store s1, store s2);

typedef struct Nin_constraint
{
  aloc aloc;       /* Constraint aloc \not\in eff, generated at loc */
  eff_kind kind;
  effect eff;
  location loc;
} *nin_constraint;

typedef struct Forall_nin_constraint
{
  eff_kind kind;  
  location loc;
  effect eff;
} *forall_nin_constraint;

typedef struct Forall_cond_nin_constraint
{
  location loc;
  eff_kind kind1;  
  effect eff1;
  eff_kind kind2;
  effect eff2;
} *forall_cond_nin_constraint;

effect effect_empty = NULL;                  /* The \emptyset of effects */
dd_list constraints = NULL;  /* The set of generated \not\in constraints */
dd_list forall_constraints = NULL;
dd_list forall_cond_constraints = NULL;
region effect_region = NULL;
int next_effect = 0;
static enum { state_init, state_reach, state_lin } state = state_init;

dd_list aloc_list;
hash_table global_ht;

#ifdef STATS
dd_list effect_list;
void stats(void);
#endif

void init_effects(void)
{
  assert(effect_region == NULL);
  effect_region = newregion();
  next_effect = 0;
  constraints = dd_new_list(effect_region);
  forall_constraints = dd_new_list(effect_region);
  forall_cond_constraints = dd_new_list(effect_region);
  aloc_list = dd_new_list(effect_region);
  state = state_init;
#ifdef STATS
  effect_list = dd_new_list(effect_region);
  if (atexit(stats))
    fprintf(stderr, "Registration of stats at exit failed\n");
#endif
}

/**************************************************************************
 *                                                                        *
 * Abstract locations                                                     *
 *                                                                        *
 **************************************************************************/

struct Abstract_loc
{
  enum { aloc_var, aloc_link } kind;
#ifdef STATS
  bool visited;
#endif
  union
  {
    struct {
      const char *name;
      int num_equiv;
      effect *effects;    /* Effects constructed from this aloc (if any) */
      qtype points_to;    /* The flow-insensitive qtype this aloc points to */
      qtype fs_points_to; /* The flow-sensitive qtype this aloc points to,
			     if this aloc is not interesting */
      bool interesting:1; /* For stores */
      bool reach_computed:1; /* TRUE if we've computed what locations
				this aloc reaches */
    } var;
    aloc link;
  } u;
};

/* Make a new, unique abstract location */
aloc make_aloc(const char *name, qtype points_to)
{
  aloc al;
  
  if (!restrict_qual && !flag_flow_sensitive)
    return NULL;
  
  al = ralloc(effect_region, struct Abstract_loc);
#ifdef DEBUG
  printf("Aloc %s %p points to qtype ", name, al);
  print_qtype_raw(printf, points_to, NULL);
  printf("\n");
#endif
  al->kind = aloc_var;
  al->u.var.name = name;
  al->u.var.num_equiv = 1;
  al->u.var.effects = NULL;
  al->u.var.points_to = points_to;
  al->u.var.fs_points_to = NULL;
  al->u.var.interesting = FALSE;
  al->u.var.reach_computed = FALSE;
  dd_add_last(effect_region, aloc_list, al);
  return al;
}

/* Find equivalence class representative of al */
static aloc ecr_aloc(aloc al)
{
  if (al->kind == aloc_link)
    {
      aloc ecr = al, cur, temp;
      
      /* Find root */
      while (ecr->kind == aloc_link)
	ecr = ecr->u.link;
      
      /* Compress path */
      cur = al;
      while (cur->u.link != ecr)
	{
	  temp = cur->u.link;
	  cur->u.link = ecr;
	  cur = temp;
	}
      return ecr;
    }
  else
    return al;
}

/* Return al's name */
const char *name_aloc(aloc al)
{
  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  return al->u.var.name;
}

/* Hash function on alocs */
int hash_aloc(aloc al)
{
  al = ecr_aloc(al);
  return (int) al;
}

/* Return the qtype al points to */
qtype points_to_aloc(aloc al)
{
  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  return al->u.var.points_to;
}

/* Add constraint that aloc al1 = al2 */
void unify_aloc(location loc, aloc al1, aloc al2)
{
  if (!restrict_qual && !flag_flow_sensitive)
    return;

#ifdef DEBUG
  print_aloc(printf, al1);
  printf(" %p == ", ecr_aloc(al1));
  print_aloc(printf, al2);
  printf(" %p\n", ecr_aloc(al2));
#endif

  al1 = ecr_aloc(al1);
  al2 = ecr_aloc(al2);

  assert(al1->kind == aloc_var && al2->kind == aloc_var);
  /* How should this go? */
  /*  assert(!al1->u.var.fs_points_to && !al2->u.var.fs_points_to);*/
  if (al1 != al2)
    {
      aloc new_ecr, new_link;

      if (al1->u.var.num_equiv <= al2->u.var.num_equiv)
	{
	  new_ecr = al2;
	  new_link = al1;
	}
      else
	{
	  new_ecr = al1;
	  new_link = al2;
	}

      if (new_ecr->u.var.effects && new_link->u.var.effects)
	unify_constrs(new_ecr->u.var.effects, new_link->u.var.effects);
      else if (new_link->u.var.effects)
	new_ecr->u.var.effects = new_link->u.var.effects;

      if (flag_flow_sensitive &&
	  !match_qtype(new_ecr->u.var.points_to, new_link->u.var.points_to,
		       NULL, NULL))
	{
	  report_qerror(loc, sev_err,
			"inconsistent location type for flow-sensitive pass");
	  new_ecr->u.var.points_to = error_qtype;
	}
      new_ecr->u.var.num_equiv += new_link->u.var.num_equiv;
      new_ecr->u.var.interesting =
	new_ecr->u.var.interesting || new_link->u.var.interesting;
      new_link->kind = aloc_link;      
      new_link->u.link = new_ecr;
      /* Don't do anything for `points_to', since we only care about the
         shape, anyhow, and the flow-insensitive analysis will make sure
         they match. */
#ifdef DEBUG
  printf("    result points to qtype ");
  print_qtype_raw(printf, ecr_aloc(new_ecr)->u.var.points_to, NULL);
  printf("\n");
#endif
   }
}

/* A total ordering on alocs */
int cmp_aloc(aloc left, aloc right)
{
  left = ecr_aloc(left);
  right = ecr_aloc(right);
  return (int) left - (int) right;
}

/* Return TRUE iff al1 and al2 are equal */
bool eq_aloc(aloc al1, aloc al2)
{
  return (ecr_aloc(al1) == ecr_aloc(al2));
}

/* Print the abstract location al */
int print_aloc(printf_func pf, aloc al)
{
  int result;

  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  result = 0;
#ifdef DEBUGST
  if (al->u.var.interesting)
    result += pf("*");
#endif
  result += pf(al->u.var.name);
  return result;
}

/* Print the abstract location al */
static int print_unique_aloc(printf_func pf, aloc al)
{
  int result;

  al = ecr_aloc(al);
  result = print_aloc(pf, al);
  if (flag_ugly)
    result += pf(" %s", ptr_to_ascii(al));
  return result;
}

/**************************************************************************
 *                                                                        *
 * Effects                                                                *
 *                                                                        *
 **************************************************************************/

#define MARK_R (1 << (eff_r - 1))
#define MARK_WR (1 << (eff_wr - 1))
#define MARK_ALLOC (1 << (eff_alloc - 1))
#define MARK_RWR (MARK_R | MARK_WR)
#define MARK_ANY (MARK_RWR | MARK_ALLOC)

struct Effect
{
  enum { eff_constr, eff_union, eff_inter, eff_var, eff_link } kind;
  int visited;
  union
  {
    struct {
      eff_kind kind;
      aloc base;      /* This effect is kind(base) */
      effect_set ub;  /* Upper bounds.  Variables only. */
    } constr;
    struct {
      effect e1;
      effect e2;
    } u;              /* e1 \cup e2 */
    struct {
      effect e1;
      effect e2;
      effect_set ub;
      int visited1, visited2;
    } inter;          /* e1 \cap e2 */
    struct {
      const char *name;
      effect_set lb, ub;     /* lb contains variables and constants,
			        ub contains variables and intersections */
      int num_equiv;
      bool interesting;      /* true iff we should fill in reach_cache */
      aloc_int_map reach_cache; /* Cache of reachability computation result */
#ifdef STATS
      int lb_size;      /* Size of lower-bound set (if any) */
      int lowlink;      /* dfs number of root in scc */
#endif
    } var;
    effect link;
  } u;
};

/* Find the equivalence class representative of e */
static effect ecr_effect(effect e)
{
  if (e == NULL)
    return e;
  
  if (e->kind == eff_link)
    {
      effect ecr = e, cur, temp;
      
      /* Find root */
      while (ecr && ecr->kind == eff_link)
	ecr = ecr->u.link;
      
      /* Compress path */
      cur = e;
      while (cur->u.link != ecr)
	{
	  temp = cur->u.link;
	  cur->u.link = ecr;
	  cur = temp;
	}
      return ecr;
    }
  else
    return e;
}

/* mark this aloc as interesting (optimization help for store) */
void mark_aloc_interesting(aloc al)
{
  if (!restrict_qual && !flag_flow_sensitive)
    return;

  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  al->u.var.interesting = TRUE;
}

static bool aloc_interesting(aloc al)
{
  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  return al->u.var.interesting;
}

aloc aloc_effect(effect e)
{
  e = ecr_effect(e);
  assert(e && e->kind == eff_constr);
  return e->u.constr.base;
}

/* The effect k(al) */
effect effect_constr(aloc al, eff_kind k)
{
  int i;
  effect *effects;

  assert(state == state_init);
  if (!restrict_qual && !flag_flow_sensitive)
    return NULL;

  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  assert(0 <= k && k <= eff_last);
  if (!al->u.var.effects)
    {
      al->u.var.effects = rarrayalloc(effect_region, eff_last + 1, effect);
      for (i = 0; i <= eff_last; i++)
	al->u.var.effects[i] = NULL;
    }

  effects = al->u.var.effects;

  if (!effects[k])
    {
      effect e;

      e = ralloc(effect_region, struct Effect);
      e->kind = eff_constr;
      e->visited = 0;
      e->u.constr.kind = k;
      e->u.constr.base = al;
      e->u.constr.ub = empty_effect_set();
      effects[k] = e;
#ifdef STATS
      dd_add_last(effect_region, effect_list, e);
#endif
    }

  return effects[k];
}

/* Return the effect containing {al} */
effect effect_single(aloc al)
{
  return effect_constr(al, eff_any);
}

/* The effect alloc(al), which is different than al itself */
effect effect_alloc(aloc al)
{
  return effect_constr(al, eff_alloc);
}

/* The effect r(al) */
effect effect_r(aloc al)
{
  return effect_constr(al, eff_r);
}

/* The effect wr(al) */
effect effect_wr(aloc al)
{
  return effect_constr(al, eff_wr);
}


/* The effect rwr(al) */
effect effect_rwr(aloc al)
{
  return effect_constr(al, eff_rwr);
}

/* Create a fresh effect variable */
effect effect_var(const char *name)
{
  effect e;

  assert(state == state_init);
  if (!restrict_qual && !flag_flow_sensitive)
    return NULL;

  e = ralloc(effect_region, struct Effect);
  e->kind = eff_var;
  e->u.var.name = name;
  e->u.var.lb = empty_effect_set();
  e->u.var.ub = empty_effect_set();
  e->u.var.interesting = FALSE;
  e->u.var.reach_cache = NULL;
  e->visited = 0;
#ifdef STATS
  dd_add_last(effect_region, effect_list, e);
#endif
  return e;
}

/* Mark this effect as interesting (optimization for caching
   reachability queries) */
void mark_effect_interesting(effect e)
{
  assert(state == state_init);
  if (!restrict_qual && !flag_flow_sensitive)
    return;

  e = ecr_effect(e);
  switch (e->kind)
    {
    case eff_constr:
      break;
    case eff_var:
      e->u.var.interesting = TRUE;
      break;
    case eff_union:
      mark_effect_interesting(e->u.u.e1);
      mark_effect_interesting(e->u.u.e2);
      break;
    case eff_inter:
      mark_effect_interesting(e->u.inter.e1);
      mark_effect_interesting(e->u.inter.e2);
      break;
    default:
      fail("Unexpected effect kind %x\n", e->kind);
    }
}

/* Create a fresh effect variable with a unique name */
effect effect_fresh(void)
{
  const char *name;

  if (!restrict_qual && !flag_flow_sensitive)
    return NULL;

  name = rstrcat(effect_region, "ef",
		 inttostr(effect_region, next_effect++));
  return effect_var(name);
}

/* The effect e1 \cup e2 */
effect effect_union(effect e1, effect e2)
{
  if (!restrict_qual && !flag_flow_sensitive)
    return NULL;

  e1 = ecr_effect(e1);
  e2 = ecr_effect(e2);

  if (e1 == e2)
    return e1;

  if (e1 == NULL)
    return e2;
  else if (e2 == NULL)
    return e1;
  else
    {
      effect e;

      e = ralloc(effect_region, struct Effect);
      e->kind = eff_union;
      e->u.u.e1 = e1;
      e->u.u.e2 = e2;
      e->visited = 0;
#ifdef STATS
      dd_add_last(effect_region, effect_list, e);
#endif
      return e;
    }
}

/* Add to_add to e's ub set */
static void add_to_effect_ub(effect e, effect to_add)
{
  e = ecr_effect(e);
  switch (e->kind)
    {
    case eff_constr:
      effect_set_insert(effect_region, &e->u.constr.ub, to_add);
      break;
    case eff_inter:
      effect_set_insert(effect_region, &e->u.inter.ub, to_add);
      break;
    case eff_var:
      effect_set_insert(effect_region, &e->u.var.ub, to_add);
      break;
    default:
      fail("Unexpected effect kind %x\n", e->kind);
    }
}

/* The effect e1 \cap e2.  e2 cannot be the empty effect. */
effect effect_inter(effect e1, effect e2)
{
  assert(state == state_init);
  if (!restrict_qual && !flag_flow_sensitive)
    return NULL;

  e1 = ecr_effect(e1);
  e2 = ecr_effect(e2);

  assert(e2 != NULL);

  if (e1 == NULL)
    return effect_empty;
  else
    {
      effect e;

      assert(e2 != NULL);

      /* Get rid of any unions in e1 and e2 */
      if (e1->kind == eff_union)
	{
	  effect temp;

	  temp = effect_fresh();
	  mkleq_effect(e1, temp);
	  e1 = temp;
	}
      if (e2->kind == eff_union)
	{
	  effect temp;

	  temp = effect_fresh();
	  mkleq_effect(e2, temp);
	  e2 = temp;
	}

      e = ralloc(effect_region, struct Effect);
      e->kind = eff_inter;
      e->u.inter.e1 = e1;
      e->u.inter.e2 = e2;
      e->u.inter.ub = empty_effect_set();
      e->u.inter.visited1 = 0;
      e->u.inter.visited2 = 0;
      e->visited = 0;

      add_to_effect_ub(e1, e);
      add_to_effect_ub(e2, e);

#ifdef STATS
      dd_add_last(effect_region, effect_list, e);
#endif
      return e;
    }
}

/* Return FALSE if e1 may not be equal to e2.  Does not generate a
   constraint. */
bool eq_effect(effect e1, effect e2)
{
  return (ecr_effect(e1) == ecr_effect(e2));
}

/* A total ordering on effects */
int cmp_effect(effect e1, effect e2)
{
  e1 = ecr_effect(e1);
  e2 = ecr_effect(e2);
  return (int) e1 - (int) e2;
}

/* Add constraint that effect set e1 is a subset of effect set e2. */
void internal_mkleq_effect(effect e1, effect e2)
{
  e1 = ecr_effect(e1);
  e2 = ecr_effect(e2);

  assert(e2->kind == eff_var);

  if (e1 == NULL)
    return;
  else
    switch (e1->kind)
      {
      case eff_union:
	internal_mkleq_effect(e1->u.u.e1, e2);
	internal_mkleq_effect(e1->u.u.e2, e2);
	break;
      case eff_constr:
	effect_set_insert(effect_region, &e1->u.constr.ub, e2);
	effect_set_insert(effect_region, &e2->u.var.lb, e1);
	break;
      case eff_var:
	effect_set_insert(effect_region, &e1->u.var.ub, e2);
	effect_set_insert(effect_region, &e2->u.var.lb, e1);
	break;
      case eff_inter:
	effect_set_insert(effect_region, &e1->u.inter.ub, e2);
	effect_set_insert(effect_region, &e2->u.var.lb, e1);
	break;
      default:
	fail("Unexpected effect kind %x\n", e1->kind);
      }
}

/* Add constraint e1 <= e2 */
void mkleq_effect(effect e1, effect e2)
{
  assert(state == state_init);
  if (!restrict_qual && !flag_flow_sensitive)
    return;

#ifdef DEBUG
  print_effect(printf, e1);
  printf(" <= ");
  print_effect(printf, e2);
  printf("\n");
#endif

  internal_mkleq_effect(e1, e2);
}

/* Add constraint e1 = e2 */
void mkeq_effect(effect e1, effect e2)
{
  assert(state == state_init);
  if (!restrict_qual && !flag_flow_sensitive)
    return;

#ifdef DEBUG
  print_effect(printf, e1);
  printf(" = ");
  print_effect(printf, e2);
  printf("\n");
#endif

  internal_mkleq_effect(e1, e2);
  internal_mkleq_effect(e2, e1);
}

/* Add constraint that effect sets e1 and e2 are equal. */
void unify_effect(effect e1, effect e2)
{
  effect new_ecr, new_link;

  if (!restrict_qual && !flag_flow_sensitive)
    return;

#ifdef DEBUG
  print_effect(printf, e1);
  printf(" == ");
  print_effect(printf, e2);
  printf("\n");
#endif

  e1 = ecr_effect(e1);
  e2 = ecr_effect(e2);

  if (e1 == e2)
    return;

  assert(e1->kind == eff_var && e2->kind == eff_var);
  assert(state == state_init);
  assert(!e1->u.var.reach_cache && !e2->u.var.reach_cache);

  mkeq_effect(e1, e2);

  if (e1->u.var.num_equiv <= e2->u.var.num_equiv)
    {
      new_ecr = e2;
      new_link = e1;
    }
  else
    {
      new_ecr = e1;
      new_link = e2;
    }

  new_ecr->u.var.num_equiv += new_link->u.var.num_equiv;
  new_ecr->u.var.lb = effect_set_union(new_ecr->u.var.lb,
				       new_link->u.var.lb);
  new_ecr->u.var.ub = effect_set_union(new_ecr->u.var.ub,
				       new_link->u.var.ub);
  new_ecr->u.var.interesting =
    new_ecr->u.var.interesting || new_link->u.var.interesting;
  new_link->kind = eff_link;
  new_link->u.link = new_ecr;
}

/* Unify the constructed effects in c1 and c2 */
static void unify_constrs(effect *c1, effect *c2)
{
  int i;

  for (i = 0; i <= eff_last; i++)
    {
      effect e1, e2;

      e1 = ecr_effect(c1[i]);
      e2 = ecr_effect(c2[i]);
      if (!e1)
	c1[i] = e2;
      else if (!e2)
	c2[i] = e1;
      else
	{
	  assert(e1->kind == eff_constr && e2->kind == eff_constr &&
		 e1->u.constr.kind == e2->u.constr.kind);
	  if (e1 != e2)
	    {
	      e1->u.constr.ub = effect_set_union(e1->u.constr.ub,
						 e2->u.constr.ub);
	      e2->kind = eff_link;
	      e2->u.link = e1;
	    }
	}
    }
}

/* Remember the constraint al !<= e2.  The constraint will be checked
   when check_nin is called. */
void mknin_aloc_effect(location loc, aloc al, eff_kind kind, effect e)
{
  nin_constraint c;

  e = ecr_effect(e);

  /* Normalize e */
  if (e == NULL)  /* nothing is in 0 */
    return;
  else if (e->kind != eff_var)
    {
      effect temp;

      temp = effect_fresh();
      mkleq_effect(e, temp);
      e = temp;
    }

  mark_effect_interesting(e);
  
  c = ralloc(effect_region, struct Nin_constraint);
  c->aloc = al;
  c->eff = e;
  c->kind = kind;
  c->loc = loc;
  dd_add_last(effect_region, constraints, c);
}

/* Forall rho.kind(rho) \notin e */
void forall_aloc_mknin_effect(location loc, eff_kind kind, effect e)
{
  forall_nin_constraint c;
  
  e = ecr_effect(e);
  
  if (e == NULL)
    return;
  else if (e->kind != eff_var)
    {
      effect temp;

      temp = effect_fresh();
      mkleq_effect(e, temp);
      e = temp;
    }
  
  mark_effect_interesting(e);
  
  c = ralloc(effect_region, struct Forall_nin_constraint);
  c->kind = kind;
  c->loc = loc;
  c->eff = e;
  dd_add_last(effect_region, forall_constraints, c);
}

/* Forall rho s.t. kind(rho) \in e1.kind(rho) \notin e2 */
void forall_aloc_in_effect_mknin_effect(location loc, eff_kind kind1, 
					effect e1, eff_kind kind2, effect e2)
{
  forall_cond_nin_constraint c;

  e1 = ecr_effect(e1);
  
  if (e1 == NULL)
    return;
  else if (e1->kind != eff_var)
    {
      effect temp;

      temp = effect_fresh();
      mkleq_effect(e1, temp);
      e1 = temp;
    }
  
  mark_effect_interesting(e1);

  if (e2 == NULL)
    return;
  else if (e2->kind != eff_var)
    {
      effect temp;

      temp = effect_fresh();
      mkleq_effect(e2, temp);
      e2 = temp;
    }
  
  mark_effect_interesting(e2);

  c = ralloc(effect_region, struct Forall_cond_nin_constraint);
  c->kind1 = kind1;
  c->kind2 = kind2;
  c->loc = loc;
  c->eff1 = e1;
  c->eff2 = e2;
  dd_add_last(effect_region, forall_cond_constraints, c);
}

/* Remember the constraint e1 !<= e2.  The constraint will be checked
  when check_nin is called.  e1 must not contain any effect variables.  */
void mknin_effect(location loc, effect e1, effect e2)
{
  e1 = ecr_effect(e1);
  e2 = ecr_effect(e2);
  
  if (e1 == NULL || e2 == NULL)     /* Anything is <= !0 and 0 <= anything */
    return;

  if (e1->kind == eff_constr)
    mknin_aloc_effect(loc, e1->u.constr.base, e1->u.constr.kind, e2);
  else if (e1->kind == eff_union)
    {
      /* e1 = u.e1\cup u.e2, so !e1 = !u.e1 \cap !u.e2 */
      mknin_effect(loc, e1->u.u.e1, e2);
      mknin_effect(loc, e1->u.u.e2, e2);
    }
  else
    fail("Unexpected effect kind %x\n", e1->kind);
}

/* Return true if there is a path (respecting intersections) from e1
   to e2.  e2 must be a variable. */
static bool reachable_forward(effect e1, effect e2) {
  effect_set_scanner ss;
  effect b;

  e1 = ecr_effect(e1);
  e2 = ecr_effect(e2);

  assert(e2->kind == eff_var);
  /*
  printf("  ");
  print_effect(printf, e1);
  printf("\n");
  */
  if (e1 == e2)
    return TRUE;
  else
    switch (e1->kind)
      {
      case eff_constr:
	{
	  assert(e1->visited == 0);
	  e1->visited = 1;
	  scan_effect_set(b, ss, e1->u.constr.ub)
	    if (reachable_forward(b, e2))
	      return TRUE;
	}
	break;
      case eff_inter:
	{
	  if (e1->visited == 0)
	    e1->visited = 1;
	  else if (e1->visited == 1)
	    {
	      e1->visited = 2;
	      scan_effect_set(b, ss, e1->u.inter.ub)
		if (reachable_forward(b, e2))
		  return TRUE;
	    }
	}
	break;
      case eff_var:
	{
	  if (!e1->visited)
	    {
	      e1->visited = 1;
	      scan_effect_set(b, ss, e1->u.var.ub)
		if (reachable_forward(b, e2))
		  return TRUE;
	    }
	}
	break;
      default:
	fail("Unexpected effect kind %x\n", e1->kind);
      }
  return FALSE;
}

/* Set visited = 0 for everything reachable forwards from e.  Also
   fills in e's reach_cache (if any) */
static void clean_reachable_forward(aloc al, effect e) {
  effect_set_scanner ss;
  effect b;

  e = ecr_effect(e);

  if (e->visited == 0 &&
      (e->kind != eff_inter || (e->u.inter.visited1 == 0 &&
				e->u.inter.visited2 == 0)))
    return;

  switch (e->kind)
    {
    case eff_constr:
      e->visited = 0;
      scan_effect_set(b, ss, e->u.constr.ub)
	clean_reachable_forward(al, b);
      break;
    case eff_inter:
      e->visited = 0;
      e->u.inter.visited1 = 0;
      e->u.inter.visited2 = 0;
      scan_effect_set(b, ss, e->u.inter.ub)
	clean_reachable_forward(al, b);
      break;
    case eff_var:
      if (e->u.var.interesting)
	{
	  if (!e->u.var.reach_cache)
	    e->u.var.reach_cache = make_aloc_int_map(effect_region, 17);
	  aloc_int_map_insert(e->u.var.reach_cache, al, e->visited);
	}
      e->visited = 0; /* After we insert in the cache */
      scan_effect_set(b, ss, e->u.var.ub)
	clean_reachable_forward(al, b);
      break;
    default:
      fail("Unexpected effect kind %x\n", e->kind);
    }
}

/* Mark ub and everything reachable from ub with mark.  lb is where we
   came from on the traversal. */
static void mark_reachable_forward(effect lb, effect ub, int mark)
{
  effect_set_scanner ss;
  effect b;

  /*
  printf("  reaches ");
  print_effect(printf, ub);
  printf(" as %x\n", mark);
  */
  ub = ecr_effect(ub);
  switch(ub->kind)
    {
    case eff_constr:
      {
	assert(!ub->visited);
	ub->visited = mark;
	scan_effect_set(b, ss, ub->u.constr.ub)
	  mark_reachable_forward(ub, b, mark);
      }
      break;
    case eff_inter:
      {
	int old_mark = ub->visited;

	if (eq_effect(lb, ub->u.inter.e1))
	  ub->u.inter.visited1 |= mark;
	if (eq_effect(lb, ub->u.inter.e2))
	  ub->u.inter.visited2 |= mark;

	ub->visited = ub->u.inter.visited1 & ub->u.inter.visited2;
	if (ub->visited != old_mark)
	  scan_effect_set(b, ss, ub->u.var.ub)
	    mark_reachable_forward(ub, b, ub->visited);
      }
      break;
    case eff_var:
      {
	int old_mark = ub->visited;
	ub->visited |= mark;
	if (ub->visited != old_mark)
	  scan_effect_set(b, ss, ub->u.var.ub)
	    mark_reachable_forward(ub, b, mark);
      }
      break;
    default:
      fail("Unexpected effect kind %x\n", ub->kind);
    }

}

/* Fill in the reach caches for all variables and intersections */
static void compute_reachable_forward(aloc al)
{
  /*
  printf("Reachability of ");
  print_unique_aloc(printf, al);
  printf("\n");
  */
  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  assert(!al->u.var.reach_computed);
  if (al->u.var.effects)
    {
      effect any, rwr, r, wr, alloc;

      any = al->u.var.effects[eff_any];
      rwr = al->u.var.effects[eff_rwr];
      r = al->u.var.effects[eff_r];
      wr = al->u.var.effects[eff_wr];
      alloc = al->u.var.effects[eff_alloc];
      if (any)
	mark_reachable_forward(effect_empty, any, MARK_ANY);
      if (rwr)
	mark_reachable_forward(effect_empty, rwr, MARK_RWR);
      if (r)
	mark_reachable_forward(effect_empty, r, MARK_R);
      if (wr)
	mark_reachable_forward(effect_empty, wr, MARK_WR);
      if (alloc)
	mark_reachable_forward(effect_empty, alloc, MARK_ALLOC);
      if (alloc)
	clean_reachable_forward(al, alloc);
      if (rwr)
	clean_reachable_forward(al, rwr);
      if (r)
	clean_reachable_forward(al, r);
      if (wr)
	clean_reachable_forward(al, wr);
      if (any)
	clean_reachable_forward(al, any);
    }
  al->u.var.reach_computed = TRUE;
}

/* Return the way by which al reaches e in the least solution.
   Possible return values are 0 (unreachable), MARK_ANY, MARK_RWR,
   MARK_R, MARK_WR, and MARK_ALLOC. */
static int aloc_in_effect(aloc al, effect e)
{
  int mark;

  state = state_reach;
  if (!restrict_qual && !flag_flow_sensitive)
    return FALSE;

  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  e = ecr_effect(e);
  mark = 0; /* Assume not reachable */
  switch (e->kind)
    {
    case eff_constr:
      if (e->u.constr.kind == eff_any)
	mark = MARK_ANY;
      else if (e->u.constr.kind == eff_rwr)
	mark = MARK_RWR;
      else if (e->u.constr.kind == eff_r)
	mark = MARK_R;
      else if (e->u.constr.kind == eff_wr)
	mark = MARK_WR;
      else if (e->u.constr.kind == eff_alloc)
	mark = MARK_ALLOC;
      else
	fail("Unexpected constr kind %x\n", e->u.constr.kind);
      break;
    case eff_union:
      mark = aloc_in_effect(al, e->u.u.e1) | aloc_in_effect(al, e->u.u.e2);
      break;
    case eff_inter:
      mark = (aloc_in_effect(al, e->u.inter.e1) &
	      aloc_in_effect(al, e->u.inter.e2));
      break;
    case eff_var:
      assert(e->u.var.interesting); /* can only check interesting effects */
      if (!al->u.var.reach_computed)
	compute_reachable_forward(al);
      if (e->u.var.reach_cache)
	/* If lookup finds nothing, mark is 0 */
	aloc_int_map_lookup(e->u.var.reach_cache, al, &mark);
      break;
    default:
      fail("Unexpected effect kind %x\n", e->kind);
    }
  return mark;
}

static unsigned int get_mask(eff_kind kind)
{
  unsigned int mask;

  switch (kind)
    {
    case eff_any:
      mask = MARK_ANY;
      break;
    case eff_rwr:
      mask = MARK_RWR;
      break;
    case eff_r:
      mask = MARK_R;
      break;
    case eff_wr:
      mask = MARK_WR;
      break;
    case eff_alloc:
      mask = MARK_ALLOC;
      break;
    default:
      fail("Unexpected constraint kind %x\n", kind);
    }

  return mask;
}

/* Call check_nin_aloc_effect on all the constraints generated by
   mknin_aloc_effect and mknin_effect.  Generates error messages with
   calls to report_qerror. */
/* Also checks forall constraints */
void check_nin(void)
{
  dd_list_pos cur;

  /* forall constraints */
  /* This ends up solving reachability for all aloc. */
  if (dd_length(forall_constraints) > 0 ||
      dd_length(forall_cond_constraints) > 0) {
    dd_scan(cur, aloc_list)
      {
	aloc al;
	dd_list_pos curconst;
	al = ecr_aloc(DD_GET(aloc, cur));
      
	assert(al->kind == aloc_var);
	if (!al->u.var.reach_computed)
	  compute_reachable_forward(al);
      
	dd_scan(curconst, forall_constraints)
	  {
	    forall_nin_constraint c = DD_GET(forall_nin_constraint, curconst);
 
	    if (aloc_in_effect(al, c->eff) & get_mask(c->kind))
	      report_qerror(c->loc, sev_err, 
			    "Invalid use of restrict - side effect");
	  }

	dd_scan(curconst, forall_cond_constraints)
	  {
	    forall_cond_nin_constraint c = DD_GET(forall_cond_nin_constraint, 
						  curconst);
	  
	    if ((aloc_in_effect(al, c->eff1) & get_mask(c->kind1)) &&
		(aloc_in_effect(al, c->eff2) & get_mask(c->kind2)))
	      report_qerror(c->loc, sev_err, 
			    "Invalid use of restrict - readonly written");
	  }
      }
  }

  /* The rest */
  dd_scan(cur, constraints)
    {
      nin_constraint c = DD_GET(nin_constraint, cur);
#ifdef DEBUG
      print_aloc(printf, c->aloc);
      printf(" !<= ");
      print_effect(printf, c->eff);
      printf("\n");
#endif
      if (aloc_in_effect(c->aloc, c->eff) & get_mask(c->kind))
	{
#ifdef DEBUG
	  printf("Invalid constraint ");
	  print_aloc(printf, c->aloc);
	  printf(" !<= ");
	  print_effect(printf, c->eff);
	  printf("\n");
#endif
	  report_qerror(c->loc, sev_err,
			"Invalid use of restrict");
	}
    }
}

int print_effect(printf_func pf, effect e)
{
  int result;

  result = 0;
  e = ecr_effect(e);
  if (e == NULL)
    return pf("0");
  switch(e->kind)
    {
    case eff_constr:
      switch (e->u.constr.kind)
	{
	case eff_any:
	  break;
	case eff_alloc:
	  result += pf("alloc(");
	  break;
	case eff_rwr:
	  result += pf("rwr(");
	  break;
	case eff_r:
	  result += pf("r(");
	  break;
	case eff_wr:
	  result += pf("wr(");
	  break;
	default:
	  fail("Unexpected constructor kind %x\n", e->u.constr.kind);
	}
      result += print_unique_aloc(pf, e->u.constr.base);
      switch (e->u.constr.kind)
	{
	case eff_alloc:
	case eff_rwr:
	  result += pf(")");
	  break;
	case eff_r:
	  result += pf(")");
	  break;
	case eff_wr:
	  result += pf(")");
	  break;
	case eff_any:
	  break;
	default:
	  fail("Unexpected constructor kind %x\n", e->u.constr.kind);
	}
      break;
    case eff_union:
      result += print_effect(pf, e->u.u.e1);
      result += pf(" + ");
      result += print_effect(pf, e->u.u.e2);
      break;
    case eff_inter:
      result += pf("[(");
      result += print_effect(pf, e->u.inter.e1);
      result += pf(") & (");
      result += print_effect(pf, e->u.inter.e2);
      result += pf(")]");
      break;
    case eff_var:
      if (flag_ugly)
	result += pf("%s %p", e->u.var.name, e);
      else
	result += pf("%s", e->u.var.name);
      break;
    default:
      fail("Unexpected effect kind %x\n", e->kind);
    }
  return result;
}

/**************************************************************************
 *                                                                        *
 * Stores                                                                 *
 *                                                                        *
 **************************************************************************/

struct Store_edge {
  location loc;
  const char *err_msg;
  store store;
};

struct Cell {
  qtype qtype;
  bool propagated_forward;
  bool propagated_backward;
};

struct Store {
  enum { store_var, store_filter, store_ref,  store_ow, store_assign,
	 store_link } kind;
  location loc;		/* where this store was created */
  int lin;
  int lowlink;
  int visited;
  store_edge_set ub;
  union {
    struct {
      const char *name; 
      int num_equiv;
      store_edge_set lb;
      cell_map cells;
    } var;              /* { cells } */
    struct {
      store s;
      effect e;
    } filter;		/* s|_e, s restricted to e */
    struct {
      store s;
      aloc al;
    } ref;		/* ref(al, s) = s + {al : qt} */
    struct {
      store s1;
      store s2;
      effect e;
    } ow;		/* store_ow(s1, s2, e) = s1|_e + s2|_{!e} */
    struct {
      store s;
      aloc al;
      bool omega_warned:1;
      int lin;          /* linearity of al in s */
      bool strong:1;    /* TRUE if this must be a strong update */
      cell cell;        /* cell = (qt, propagated) */
      const char *err_msg; /* Error to present if this assignment is bad */
    } assign;		/* assign(s, al) = (s - al) + {al : qt}, plus
			   if al \omega then s(al) = qt */
    store link;
  } u;
};

static dd_list assign_stores = NULL;
static region store_region = NULL;
static int store_count = 0; /* total number of stores in the 2nd pass */
#ifdef STATS
static dd_list store_list;
#endif

static bool propagate_cell_backward(location loc, store lb, aloc al, qtype qt);
static bool propagate_cell_forward(location loc, store lb, store ub, aloc al, qtype qt);
static cell_map store_to_cells(region r, store s);
static cell cell_from_store(store s, aloc al, store *store_with_cell, bool ignore_filter);
static int print_cells(printf_func pf, pr_qual_fn pr_qual, cell_map cells,
		       bool print_alocs);

void init_store(void)
{
  assert(store_region == NULL);
  store_region = newregion();
  assign_stores = dd_new_list(store_region);
#ifdef STATS
  store_list = dd_new_list(store_region);
#endif
}

static inline store_edge mkstore_edge(region r, location loc,
				      const char *err_msg, store s)
{
  store_edge result = ralloc(r, struct Store_edge);
  result->loc = loc;
  result->err_msg = err_msg;
  result->store = s;
  return result;
}

static inline cell mkcell(region r, qtype qt, bool propagated_forward, 
			  bool propagated_backward)
{
  cell result = ralloc(r, struct Cell);
  result->qtype = qt;
  result->propagated_forward = propagated_forward;
  result->propagated_backward = propagated_backward;
  return result;
}

static store ecr_store(store s) 
{
  if (!s) return s;

  if (s->kind == store_link)
    {
      store ecr = s, cur, temp;
      
      /* Find root */
      while (ecr->kind == store_link)
	ecr = ecr->u.link;
      
      /* Compress path */
      cur = s;
      while (cur->u.link != ecr)
	{
	  temp = cur->u.link;
	  cur->u.link = ecr;
	  cur = temp;
	}
      return ecr;
    }
  else
    return s;
}

int cmp_store(store s1, store s2)
{
  s1 = ecr_store(s1);
  s2 = ecr_store(s2);
  return s1 - s2;
}

/* Return TRUE iff s1 and s2 are the same store */
bool eq_store(store s1, store s2)
{
  return ecr_store(s1) == ecr_store(s2);
}

/* Make a fresh store variable */
store make_store_var(location loc, const char *name)
{
  store result;
  
  result = ralloc(store_region, struct Store);
  result->kind = store_var;
  result->loc = loc;
  result->u.var.name = name;
  result->u.var.num_equiv = 1;
  result->u.var.lb = empty_store_edge_set();
  result->u.var.cells = make_cell_map(store_region, 7);
  result->ub = empty_store_edge_set();
  result->lin = -1;
  result->lowlink = 0;
  result->visited = 0;
  store_count++;
#ifdef STATS
  dd_add_last(store_region, store_list, result);
#endif
  return result;
}

/* Make the store s|_e, s restricted to e */
store make_store_filter(location loc, store s, effect e)
{
  store result;
  
  s = ecr_store(s);
  result = ralloc(store_region, struct Store);
  result->kind = store_filter;
  result->loc = loc;
  result->u.filter.s = s;
  /*
  e = ecr_effect(e);
  if (e->kind != eff_var)
    {
      effect temp;

      temp = effect_var("s_filter");
      mkleq_effect(e, temp);
      e = temp;
    }
  */
  result->u.filter.e = e;
  result->lin = -1;
  result->lowlink = 0;
  result->visited = 0;
  result->ub = empty_store_edge_set();
  store_edge_set_insert(store_region, &s->ub,
			mkstore_edge(store_region, loc, NULL, result));
  store_count++;
#ifdef STATS
  dd_add_last(store_region, store_list, result);
#endif
  return result;
}

/* Make the store ref(al, s) = s + {al : qt} */
store make_store_ref(location loc, store s, aloc al)
{
  store result;

  al = ecr_aloc(al);
  s = ecr_store(s);
  assert(al->kind == aloc_var);
  result = ralloc(store_region, struct Store);
  result->kind = store_ref;
  result->loc = loc;
  result->u.ref.s = s;
  result->u.ref.al = al;
  result->lin = -1;
  result->lowlink = 0;
  result->visited = 0;
  result->ub = empty_store_edge_set();
  store_edge_set_insert(store_region, &s->ub,
			mkstore_edge(store_region, loc, NULL, result));
  store_count++;
#ifdef STATS
  dd_add_last(store_region, store_list, result);
#endif
  return result;
}

/* Make the store ref(al1, ref(al2, ..., ref(aln, s))) for ali in e.  e
   must be a singleton effect or a union */
store make_store_ref_effect(location loc, store s, effect e)
{
  e = ecr_effect(e);
  switch (e->kind)
    {
    case eff_constr:
      /* assert(e->u.constr.kind == eff_any); */
      return make_store_ref(loc, s, e->u.constr.base);
    case eff_union:
      return make_store_ref_effect(loc,
				   make_store_ref_effect(loc, s, e->u.u.e2),
				   e->u.u.e1);
    default:
      fail("Unexpected effect kind %x\n", e->kind);
    }
}

/* Make the store store_ow(s1, s2, e) = s1|_e + s2|_{!e} */
store make_store_ow(location loc, store s1, store s2, effect e)
{
  store result;
  
  s1 = ecr_store(s1);
  s2 = ecr_store(s2);
  e = ecr_effect(e);
  result = ralloc(store_region, struct Store);
  result->kind = store_ow;
  result->loc = loc;
  result->u.ow.s1 = s1;
  result->u.ow.s2 = s2;
  if (e->kind != eff_var)
    {
      effect temp;
      
      temp = effect_var("s_ow");
      mkleq_effect(e, temp);
      e = temp;
    }
  result->u.ow.e = e;
  result->lin = -1;
  result->lowlink = 0;
  result->visited = 0;
  result->ub = empty_store_edge_set();
  store_edge_set_insert(store_region, &s1->ub,
			mkstore_edge(store_region, loc, NULL, result));
  store_edge_set_insert(store_region, &s2->ub,
			mkstore_edge(store_region, loc, NULL, result));
  store_count++;
#ifdef STATS
  dd_add_last(store_region, store_list, result);
#endif
  return result;
}

/* Make the store assign(s, al, qt) = (s - al) + {al : qt}, plus
   the constraint that if al is \omega, then qt = s(al) .  If strong
   is TRUE then this must be a strong update. */
store make_store_assign(location loc, const char *err_msg, store s, aloc al,
			qtype qt, bool strong)
{
  store result;

  al = ecr_aloc(al);

  assert(al->u.var.interesting);
  s = ecr_store(s);
  result = ralloc(store_region, struct Store);
  result->kind = store_assign;
  result->loc = loc;
  result->u.assign.s = s;
  result->u.assign.al = al;
  result->u.assign.omega_warned = FALSE;
  result->u.assign.lin = -1;
  result->u.assign.strong = strong;
  /* XXX: Should be TRUE if qt is interesting */
  result->u.assign.cell = mkcell(store_region,
				 qt,
				 FALSE, FALSE);
  result->u.assign.err_msg = err_msg;
  result->lin = -1;
  result->lowlink = 0;
  result->visited = 0;
  result->ub = empty_store_edge_set();
  store_edge_set_insert(store_region, &s->ub,
			mkstore_edge(store_region, loc, NULL, result));
  store_count++;
  dd_add_last(store_region, assign_stores, result);
#ifdef STATS
  dd_add_last(store_region, store_list, result);
#endif
  store_aloc_qtype(result->u.assign.cell->qtype, result,
		   result->u.assign.al); /* May or may not trigger a
					    propagation */
  return result;
}

/* Add constraint left <= right */
void mkleq_store(location loc, const char *err_msg, store left, store right)
{
  bool err;
  cell_map_scanner cms;
  cell right_cell, left_cell;
  aloc al;
  region scratch_region;

  assert(state == state_init || state == state_reach);

#ifdef DEBUGST
  print_store(printf, left);
  printf(" ");
  print_store_cells(printf, print_qual, left, TRUE);
  printf(" <= ");
  print_store(printf, right);
  printf(" ");
  print_store_cells(printf, print_qual, right, TRUE);
  printf("\n");
#endif
  right = ecr_store(right);
  left = ecr_store(left);
  assert(right->kind == store_var);
  
  if (right == left)
    return;

  /* Add constraints between elements of left and elements of right */
  err = FALSE;

  /* Make constraints between all elements in right to left */
  cell_map_scan(right->u.var.cells, &cms);
  while (cell_map_next(&cms, &al, &right_cell))
    if (right_cell->propagated_backward)
      {
	store left_store;
	
	left_cell = cell_from_store(left, al, &left_store, FALSE);
	if (left_cell)
	  {
	    /* Could be NULL if lb is a filter store */

	    /* Ensure al is in left */
	    /* propagate_store_cell_backward(left_store, al); */

	    err = mkleq_qtype(loc, left_cell->qtype, right_cell->qtype) || err;
	  }
      }

  /* Find all the elements in left that aren't in right, add them to right,
     and make constraints */
  scratch_region = newregion();
  cell_map_scan(store_to_cells(scratch_region, left), &cms);
  while (cell_map_next(&cms, &al, &left_cell))
    if (left_cell->propagated_forward)
      {
	/* propagate_store_cell_forward(right, al); *//* Add al to right */
	/* insist(cell_map_lookup(right->u.var.cells, al, &right_cell)); */
	
	if (!cell_map_lookup(right->u.var.cells, al, &right_cell))
	  {
	    qtype qt;
	    
	    qt = qtype_to_fs_qtype(right->loc, al->u.var.points_to);
	    insistnot(store_aloc_qtype(qt, right, al));
	    right_cell = mkcell(store_region, qt, FALSE, FALSE);
	    insist(cell_map_insert(right->u.var.cells, al, right_cell));
	  }
	err = mkleq_qtype(loc, left_cell->qtype, right_cell->qtype) || err;
      }
  
  deleteregion(scratch_region);

  if (err)
    report_qerror(loc, sev_err, "%s", err_msg);
  store_edge_set_insert(store_region, &right->u.var.lb,
			mkstore_edge(store_region, loc, err_msg, left));
  store_edge_set_insert(store_region, &left->ub,
			mkstore_edge(store_region, loc, err_msg, right));
}

/* Add constraint left = right */
void mkeq_store(location loc, const char *err_msg, store s1, store s2)
{
  mkleq_store(loc, err_msg, s1, s2);
  mkleq_store(loc, err_msg, s2, s2);
}

/* Return a fresh store s such that s1 <= s and s2 <= s */
store lub_store(location loc, const char *err_msg, store s1, store s2)
{
  store result;

  /* easy case */
  if (eq_store(s1, s2))
    return s1;

  result = make_store_var(loc, "lub");
  mkleq_store(loc, err_msg, s1, result);
  mkleq_store(loc, err_msg, s2, result);
  
  return result;
}

/* Add constraint left == right, and can't distinguish left from right
   afterward. */
void unify_store(location loc, const char *err_msg, store s1, store s2)
{
  store new_ecr, new_link;
  cell_map_scanner cms;
  aloc al;
  cell link_cell, ecr_cell;

#ifdef DEBUGST
  print_store(printf, s1);
  printf(" == ");
  print_store(printf, s2);
  printf("\n");
#endif
  s1 = ecr_store(s1);
  s2 = ecr_store(s2);
  assert(s1->kind == store_var && s2->kind == store_var);

  if (s1 == s2)
    return;

  if (s1->u.var.num_equiv <= s2->u.var.num_equiv)
    {
      new_ecr = s2;
      new_link = s1;
    }
  else
    {
      new_ecr = s1;
      new_link = s2;
    }

  /* Add constraints between propagating elements of left and propagating
     elements of right */
  mkleq_store(loc, err_msg, new_link, new_ecr);
  mkleq_store(loc, err_msg, new_ecr, new_link);

  /* Add non-propagating elements of new_link to new_ecr.  Note that
     because these are non-propagating, we don't need to generate any
     constraints */
  cell_map_scan(new_link->u.var.cells, &cms);
  while (cell_map_next(&cms, &al, &link_cell))
    if (!link_cell->propagated_forward && !link_cell->propagated_backward)
      {
	if (cell_map_lookup(new_ecr->u.var.cells, al, &ecr_cell))
	  {
	    assert(ecr_cell->propagated_forward == link_cell->propagated_forward);
	    assert(ecr_cell->propagated_backward == link_cell->propagated_backward);
	    insistnot(unify_qtype(loc, link_cell->qtype, ecr_cell->qtype));
	  }
	else
	  insist(cell_map_insert(new_ecr->u.var.cells, al, link_cell));
      }


  new_ecr->u.var.lb = store_edge_set_union(new_ecr->u.var.lb,
					   new_link->u.var.lb);
  new_ecr->u.var.num_equiv += new_link->u.var.num_equiv;
  new_ecr->ub = store_edge_set_union(new_ecr->ub, new_link->ub);
  new_link->kind = store_link;
  new_link->u.link = new_ecr;;
}

/**************************************************************************
 *                                                                        *
 * Linearity Computation                                                  *
 *                                                                        *
 **************************************************************************/

/* scc stuff */
static int scc_store_number = 0;
static store *scc_store_stack = NULL;
static int scc_store_size = 0;
static int scc_store_top = 0;
static region scc_store_region = NULL;

/* helper routines for scc */
static void scc_store_stack_push(store s)
{
  assert (scc_store_size > scc_store_top);
  scc_store_stack[scc_store_top] = s;
  scc_store_top++;
}

static store scc_store_stack_pop(void)
{
  assert(scc_store_top > 0);
  scc_store_top--;
  return scc_store_stack[scc_store_top];
}

static void scc_store_stack_clear(void)
{
  scc_store_top = 0;
}

static void scc_store_stack_init(int size)
{
  assert(scc_store_region == NULL);
  scc_store_region = newregion();
  scc_store_stack = rarrayalloc(scc_store_region, size, store);
  scc_store_size = size;
  scc_store_top = 0;
}

static void scc_store_stack_delete(void)
{
  deleteregion(scc_store_region);
  scc_store_region = NULL;
  scc_store_size = 0;
  scc_store_top = 0;
  scc_store_stack = NULL;
}

static bool in_scc_store_stack(store s)
{
  int i;

  for (i=0; i < scc_store_top; i++)
    {
      if (s == scc_store_stack[i])
	return TRUE;
    }
  return FALSE;
}

/* subroutine for compute_lin_scc */
static int compute_lin_scc_sub(aloc al, store parent, store child);

/* computes lin at al for all stores reachable from s */
static int compute_lin_scc(aloc al, store s)
{
  s->lowlink = s->visited = ++scc_store_number;
  scc_store_stack_push(s);

#ifdef DEBUG
  printf("computing lin of ");
  print_aloc(printf, al);
  printf(" at ");
  print_store(printf, s);
  printf("\n");
#endif

  switch (s->kind)
    {
    case store_var:
      {
	store_edge_set_scanner sess;
	store_edge b;
	int lin, max;
	
	max = 0;
	scan_store_edge_set(b, sess, s->u.var.lb)
	  {
	    b->store = ecr_store(b->store);
	    lin = compute_lin_scc_sub(al, s, b->store);

	    max = MAX(max, lin);
	  }
	s->lin = max;
      }
      break;
    case store_filter:
      {
	effect e;
	    
	e = ecr_effect(s->u.filter.e);
	if (e && aloc_in_effect(al, e))
	  s->lin = compute_lin_scc_sub(al, s, ecr_store(s->u.filter.s));
	else
	  s->lin = 0;
      }
      break;
    case store_ref:
      {
	s->lin = compute_lin_scc_sub(al, s, ecr_store(s->u.ref.s));
	
	if (eq_aloc(s->u.ref.al, al))
	  s->lin = MIN(s->lin + 1, 2);
      }
      break;
    case store_ow:
      {
	effect e;
	    
	e = ecr_effect(s->u.ow.e);
	if (e && (aloc_in_effect(al, e) & MARK_ALLOC))
	  s->lin = compute_lin_scc_sub(al, s, ecr_store(s->u.ow.s1));
	else
	  s->lin = compute_lin_scc_sub(al, s, ecr_store(s->u.ow.s2));
      }
      break;
    case store_assign:
      {
	s->lin = compute_lin_scc_sub(al, s, ecr_store(s->u.assign.s));
      }
      break;
    default:
      fail("Unexpected store kind %d\n", s->kind);
    }

  if (s->visited == s->lowlink)
    {
      bool ref = FALSE;
      store c;
      int i, max, count = 0;

      /* look for a ref node in the scc found */
      max = 0;
      for (i = scc_store_top - 1; i >= 0; i--)
	{
	  c = scc_store_stack[i];
	  if (c->kind == store_ref && eq_aloc(c->u.ref.al, al))
	    ref = TRUE;
	  max = MAX(max, c->lin);
	  count++;
	  if (c->visited <= s->visited)
	    break;
	}
      
      /* increasing cycle found */
      if (ref && count > 1)
	max = 2;

      do {
	c = scc_store_stack_pop();
	c->lin = max;
#ifdef DEBUG
  printf("lin of ");
  print_aloc(printf, al);
  printf(" at ");
  print_store(printf, c);
  printf(" is %d\n", c->lin);
#endif
      } while (c->visited > s->visited);
    }

#ifdef DEBUG
  printf("lin of ");
  print_aloc(printf, al);
  printf(" at ");
  print_store(printf, s);
  printf(" is %d\n", s->lin);
#endif
  return s->lin;
}

/* subroutine for compute_lin_scc */
static int compute_lin_scc_sub(aloc al, store parent, store child)
{
  int lin = 0;

  if (!child->visited)
    {
      if (child->lin != -1)
	lin = child->lin;
      else
	{
	  lin = compute_lin_scc(al, child);
	  parent->lowlink = MIN(parent->lowlink, child->lowlink);
	}
    }
  else
    {
      lin = child->lin;
      if (child->visited < parent->visited)
	{
	  if (in_scc_store_stack(child))
	    parent->lowlink = MIN(parent->lowlink, child->visited);
	}
    }

  return lin;
}

/* cleans stores reachable from s.  If cleanlin, resets lin to -1 */
static void clean_store(store s, bool cleanlin)
{
  s = ecr_store(s);
  
  if (cleanlin)
    {
      if (!s->visited && s->lin == -1) 
	return;
      else
	{
	  s->visited = 0;
	  s->lowlink = 0;
	  s->lin = -1;
	}
    }
  else
    {
      if (!s->visited)
	return;
      else
	{
	  s->visited = 0;
	  s->lowlink = 0;
	}
    }

  switch (s->kind)
    {
    case store_var:
      {
	store_edge_set_scanner sess;
	store_edge b;
	
	scan_store_edge_set(b, sess, s->u.var.lb)
	  clean_store(b->store, cleanlin);
	return;
      }
      break;
    case store_filter:
      clean_store(s->u.filter.s, cleanlin);
      break;
    case store_ref:
      clean_store(s->u.ref.s, cleanlin);
      break;
    case store_ow:
      clean_store(s->u.ow.s1, cleanlin);
      clean_store(s->u.ow.s2, cleanlin);
      break;
    case store_assign:
      clean_store(s->u.assign.s, cleanlin);
      break;
    default:
      fail("Unexpected store kind %d\n", s->kind);
    }
}

static int cmp_assign_store_by_al(const void *s1, const void *s2)
{
  store *sl1 = (store *) s1;
  store *sl2 = (store *) s2;
  *sl1 = ecr_store(*sl1);
  *sl2 = ecr_store(*sl2);
  assert((*sl1)->kind == store_assign && (*sl2)->kind == store_assign);
  return cmp_aloc((*sl1)->u.assign.al, (*sl2)->u.assign.al);
}

/* #define LINSTATS */

/* Compute linearities.  Only used if -fstop-after-2p specified. */
void compute_lins(void)
{
  dd_list_pos cur;
  int sched_size, last, processed, i = 0;
  store *sched_array;
  aloc cur_al;
#ifdef LINSTATS
  int count_omega, count_one, count_zero;
  
  count_omega = count_one = count_zero = 0;
#endif

  state = state_lin;

  scc_store_stack_init(store_count);

  sched_size = dd_length(assign_stores);
  sched_array = rarrayalloc(store_region, sched_size, store);
  dd_scan(cur, assign_stores)
    {
      sched_array[i] = DD_GET(store, cur);
      i++;
    }

  assert(i == sched_size);
  qsort(sched_array, i, sizeof(store), cmp_assign_store_by_al);

  do {
    cur_al = NULL;
    last = 0;
    processed = 0;
    for (i = 0; i < sched_size; i++)
      {
	int lin;
	qtype lhs_qtype;

	sched_array[i] = ecr_store(sched_array[i]);
	assert(sched_array[i]->kind == store_assign);

	lhs_qtype = qtype_from_store(sched_array[i]->u.assign.s, 
				     sched_array[i]->u.assign.al);
	if ((!toplvl_qual_fs_qtype(sched_array[i]->u.assign.cell->qtype) &&
	     !toplvl_qual_fs_qtype(lhs_qtype)) ||
	    sched_array[i]->u.assign.lin > -1)
	  continue;

	if (processed == 0)
	  {
	    /* first one in the work list ... */
	    cur_al = sched_array[i]->u.assign.al;
	    scc_store_number = 0;
	    scc_store_stack_clear();
	    lin = compute_lin_scc(cur_al,
				  ecr_store(sched_array[i]->u.assign.s));
	  }
	else if (eq_aloc(cur_al, sched_array[i]->u.assign.al))
	  {
	    /* if same al as the previous one in the work list, do not
	       clean lins.
	    */
	    store cur;

	    clean_store(sched_array[i-1]->u.assign.s, FALSE);
	    scc_store_number = 0;
	    scc_store_stack_clear();
	    cur = ecr_store(sched_array[i]->u.assign.s);
	    if (cur->lin != -1)
	      lin = cur->lin;
	    else
	      lin = compute_lin_scc(cur_al, cur);
	  }
	else
	  {
	    /* otherwise clean lins to make room for the new aloc */
	    int j;

	    for (j = last; j < i; j++)
	      clean_store(sched_array[j]->u.assign.s, TRUE);

	    cur_al = sched_array[i]->u.assign.al;
	    scc_store_number = 0;
	    scc_store_stack_clear();
	    lin = compute_lin_scc(cur_al,
				  ecr_store(sched_array[i]->u.assign.s));
	    last = i;
	  }

	assert(lin > -1);
	sched_array[i]->u.assign.lin = lin;
#ifdef LINSTATS
	if (lin > 1)
	  count_omega++;
	else if (lin > 0)
	  count_one++;
	else
	  count_zero++;
#endif
	processed++;
      }
    
    /* last clean up */
    if (cur_al)
      {
	int j;
	
	for (j = last; j < i; j++)
	  clean_store(sched_array[j]->u.assign.s, TRUE);
      }

    for (i = 0; i < sched_size; i++)
      {
	/* Do this *after* computing all the linearities -- otherwise
	   the marks from qtype_from_store get confused with the marks
	   from the linearity computation */
	store s;

	s = sched_array[i];
	assert(s->kind == store_assign);
	if (s->u.assign.lin == 2 && !s->u.assign.omega_warned)
	{
	  s->u.assign.omega_warned = TRUE;
	  if (s->u.assign.strong)
	    report_qerror(sched_array[i]->loc, sev_warn,
			  "change_type applied to non-unique location");
	  /* XXX: This is more conservative than it needs to be, but
	     straight mkleq is wrong */
	  if (mkeq_qtype(s->loc,
			 s->u.assign.cell->qtype,
			 qtype_from_store(s->u.assign.s,
					  s->u.assign.al)))
	    report_qerror(s->loc, sev_err, "%s", s->u.assign.err_msg);
	}
      }
  } while (processed != 0);

  scc_store_stack_delete();
#ifdef LINSTATS
  printf("omega: %d\n one: %d\n zero: %d\n", count_omega, count_one, count_zero);
#endif
}

/**************************************************************************
 *                                                                        *
 * Qtypes in Stores                                                       *
 *                                                                        *
 **************************************************************************/

/* Add al:qt to s's bounds */
/*
static void propagate_cell(store s, aloc al, qtype qt)
{
  store_edge_set_scanner sess;
  store_edge lb, ub;

  scan_store_edge_set(lb, sess, s->u.var.lb)
    if (propagate_cell_backward(lb->loc, lb->store, al, qt))
      report_qerror(lb->loc, sev_err, "%s", lb->err_msg);

  scan_store_edge_set(ub, sess, s->ub)
    if (propagate_cell_forward(ub->loc, s, ub->store, al, qt))
      report_qerror(ub->loc, sev_err, "%s", ub->err_msg);
}
*/

void propagate_store_cell_backward(store s, aloc al)
{
  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  if (aloc_interesting(al))
    {
      s = ecr_store(s);
      switch (s->kind)
	{
	case store_var:
	  {
	    cell c;

	    if (!cell_map_lookup(s->u.var.cells, al, &c))
	      {
		qtype qt;

		qt = qtype_to_fs_qtype(s->loc, al->u.var.points_to);
		insistnot(store_aloc_qtype(qt, s, al));
		c = mkcell(store_region, qt, FALSE, FALSE);
		insist(cell_map_insert(s->u.var.cells, al, c));
	      }
	    if (!c->propagated_backward)
	      {
		store_edge_set_scanner sess;
		store_edge lb;

		c->propagated_backward = TRUE;
		scan_store_edge_set(lb, sess, s->u.var.lb)
		  if (propagate_cell_backward(lb->loc, lb->store, al,
					      c->qtype))
		    report_qerror(lb->loc, sev_err, "%s", lb->err_msg);
	      }
	  }
	  break;
	case store_assign:
	  /* No-op */
	  break;
	default:
	  fail("Unexpected store kind %x\n", s->kind);
	}
    }
}

void propagate_store_cell_forward(store s, aloc al)
{
  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  if (aloc_interesting(al))
    {
      s = ecr_store(s);
      switch (s->kind)
	{
	case store_var:
	  {
	    cell c;

	    if (!cell_map_lookup(s->u.var.cells, al, &c))
	      {
		qtype qt;

		qt = qtype_to_fs_qtype(s->loc, al->u.var.points_to);
		insistnot(store_aloc_qtype(qt, s, al));
		c = mkcell(store_region, qt, FALSE, FALSE);
		insist(cell_map_insert(s->u.var.cells, al, c));
	      }
	    if (!c->propagated_forward)
	      {
		store_edge_set_scanner sess;
		store_edge ub;

		c->propagated_forward = TRUE;
		scan_store_edge_set(ub, sess, s->ub)
		  if (propagate_cell_forward(ub->loc, s, ub->store, al,
					     c->qtype))
		    report_qerror(ub->loc, sev_err, "%s", ub->err_msg);
	      }
	  }
	  break;
	case store_assign:
	  assert(eq_aloc(al, s->u.assign.al));
	  if (!s->u.assign.cell->propagated_forward)
	    {
	      store_edge_set_scanner sess;
	      store_edge ub;

	      s->u.assign.cell->propagated_forward = TRUE;
	      scan_store_edge_set(ub, sess, s->ub)
		  if (propagate_cell_forward(ub->loc, s, ub->store, al,
					   s->u.assign.cell->qtype))
		  report_qerror(ub->loc, sev_err, "%s", ub->err_msg);
	    }
	  break;
	default:
	  fail("Unexpected store kind %x\n", s->kind);
	}
    }
}

/* Add al:qt as an upper bound on lb, coming from ub */
static bool propagate_cell_backward(location loc, store lb, aloc al, qtype qt)
{
  bool result;

  lb = ecr_store(lb);
#ifdef DEBUGST
  printf("Back propagating ");
  print_aloc(printf, al);
  printf(":");
  print_qtype(printf, qt, NULL);
  printf(" to ");
  print_store(printf, lb);
  printf("\n");
#endif
  result = FALSE;
  switch (lb->kind)
    {
    case store_var:
      {
	cell c;

	if (!cell_map_lookup(lb->u.var.cells, al, &c))
	  {
	    qtype qt_fresh;

	    qt_fresh = qtype_to_fs_qtype(lb->loc, al->u.var.points_to);
	    insistnot(store_aloc_qtype(qt_fresh, lb, al));
	    c = mkcell(store_region, qt_fresh, FALSE, FALSE);
	    insist(cell_map_insert(lb->u.var.cells, al, c));
	  }
	result = mkleq_qtype(loc, c->qtype, qt) || result;
      }
      break;
    case store_filter:
      if (aloc_in_effect(al, lb->u.filter.e))
	result =
	  propagate_cell_backward(loc, lb->u.filter.s, al, qt) ||
	  result;
      break;
    case store_ref:
      result = propagate_cell_backward(loc, lb->u.ref.s, al, qt) || result;
      break;
    case store_ow:
      if (aloc_in_effect(al, lb->u.ow.e))
	result = propagate_cell_backward(loc, lb->u.ow.s1, al, qt) ||
	  result;
      else
	result = propagate_cell_backward(loc, lb->u.ow.s2, al, qt) ||
	  result;
      break;
    case store_assign:
      if (eq_aloc(al, lb->u.assign.al))
	result = mkleq_qtype(loc, lb->u.assign.cell->qtype, qt) || result;
      else
	result =
	  propagate_cell_backward(loc, lb->u.assign.s, al, qt) ||
	  result;
      break;
    default:
      fail("Unexpected store kind %x\n", lb->kind);
    }
  return result;
}

/* Add al:qt, assumed to be in lb, as a lower bound on ub */
static bool propagate_cell_forward(location loc, store lb, store ub,
				   aloc al, qtype qt)
{
  bool result, propagate_to_ub;

#ifdef DEBUGST
  printf("Forward propagating ");
  print_aloc(printf, al);
  printf(":");
  print_qtype(printf, qt, NULL);
  printf(" to ");
  print_store(printf, ub);
  printf("\n");
#endif
  result = FALSE;
  propagate_to_ub = FALSE;
  ub = ecr_store(ub);
  switch (ub->kind)
    {
    case store_var:
      {
	cell c;

	if (!cell_map_lookup(ub->u.var.cells, al, &c))
	  {
	    qtype qt_fresh;

	    qt_fresh = qtype_to_fs_qtype(ub->loc, al->u.var.points_to);
	    insistnot(store_aloc_qtype(qt_fresh, ub, al));
	    c = mkcell(store_region, qt_fresh, FALSE, FALSE);
	    insist(cell_map_insert(ub->u.var.cells, al, c));
	  }
	result = mkleq_qtype(loc, qt, c->qtype) || result;
	propagate_to_ub = FALSE;
      }
      break;
    case store_filter:
      propagate_to_ub = aloc_in_effect(al, ub->u.filter.e);
      break;
    case store_ref:
      propagate_to_ub = TRUE;
      break;
    case store_ow:
      /* propagate if al\in e && ub = ow(lb, *, *), or if al\not\in
	 e && ub = ow(*, lb, *) */
      if (aloc_in_effect(al, ub->u.ow.e))
	propagate_to_ub = eq_store(lb, ub->u.ow.s1);
      else
	propagate_to_ub = eq_store(lb, ub->u.ow.s2);
      break;
    case store_assign:
      if (!eq_aloc(al, ub->u.assign.al))
	/* assign stops forward propagation of al */
	propagate_to_ub = TRUE;
      break;
    default:
      fail("Unexpected store kind %x\n", ub->kind);
    }
  if (propagate_to_ub)
    {
      store_edge_set_scanner sess;
      store_edge ub_ub;

      scan_store_edge_set(ub_ub, sess, ub->ub)
	/* Use old loc, not ub_ub->loc, since ub_ub->loc isn't
	   interesting except for variable-variable edges, and we
	   never propagate past a variable */
	result = propagate_cell_forward(loc, ub, ub_ub->store, al, qt)
	|| result;
    }
  return result;
}

/* Return the cells in s.  propagated_forward == TRUE only! */
cell_map store_to_cells(region r, store s)
{
  cell_map cells;
  cell_map_scanner cms;
  aloc al;
  cell c;

  s = ecr_store(s);
  switch (s->kind)
    {
    case store_var:
       cell_map_scan(s->u.var.cells, &cms);
       cells = make_cell_map(r, 7);
       /* cells = cell_map_copy(r, s->u.var.cells); */
       while (cell_map_next(&cms, &al, &c))
       if (c->propagated_forward)
         insist(cell_map_insert(cells, al, c));
      break;
    case store_filter:
      cells = make_cell_map(r, 7);
      cell_map_scan(store_to_cells(r, s->u.filter.s), &cms);
      while (cell_map_next(&cms, &al, &c))
	if (aloc_in_effect(al, s->u.filter.e))
	  insist(cell_map_insert(cells, al, c));
      break;
    case store_ref:
      cells = store_to_cells(r, s->u.ref.s);
      break;
    case store_ow:
      cells = make_cell_map(r, 7);
      cell_map_scan(store_to_cells(r, s->u.ow.s1), &cms);
      while (cell_map_next(&cms, &al, &c))
	if (aloc_in_effect(al, s->u.ow.e))
	  insist(cell_map_insert(cells, al, c));
      cell_map_scan(store_to_cells(r, s->u.ow.s2), &cms);
      while (cell_map_next(&cms, &al, &c))
	if (!aloc_in_effect(al, s->u.ow.e))
	  insist(cell_map_insert(cells, al, c));
      break;
    case store_assign:
      cells = store_to_cells(r, s->u.assign.s);
      if (aloc_interesting(s->u.assign.al))
      {
	cell_map_remove(cells, s->u.assign.al);
	if (s->u.assign.cell->propagated_forward)
	  insist(cell_map_insert(cells, s->u.assign.al, s->u.assign.cell));
      }
      break;
    default:
      fail("Unexpected store kind %x\n", s->kind);
    }
  return cells;
}

/* Return al's cell in s.  If store_with_cell is non-NULL, set it
   to the store that actually contains al. */
static cell cell_from_store(store s, aloc al, store *store_with_cell, bool ignore_filter)
{
  cell c;

  assert(aloc_interesting(al));
  s = ecr_store(s);
  switch (s->kind)
    {
    case store_var:
      if (!cell_map_lookup(s->u.var.cells, al, &c))
	{
	  qtype qt;
	  qt = qtype_to_fs_qtype(s->loc, al->u.var.points_to);
	  insistnot(store_aloc_qtype(qt, s, al));
	  /* FALSE because it has no interesting bounds */
	  c = mkcell(store_region, qt, FALSE, FALSE);
#ifdef DEBUGST
	  printf("Adding ");
	  print_aloc(printf, al);
	  printf(":");
	  print_qtype(printf, qt, NULL);
	  printf(" to ");
	  print_store(printf, s);
	  printf("\n");
#endif
	  insist(cell_map_insert(s->u.var.cells, al, c));
	}
      if (store_with_cell)
	*store_with_cell = s;
      break;
    case store_filter:
      if (ignore_filter || aloc_in_effect(al, s->u.filter.e))
	c = cell_from_store(s->u.filter.s, al, store_with_cell, ignore_filter);
      else
	c = NULL;
      break;
    case store_ref:
      c = cell_from_store(s->u.ref.s, al, store_with_cell, ignore_filter);
      break;
    case store_ow:
      if (aloc_in_effect(al, s->u.ow.e))
	c = cell_from_store(s->u.ow.s1, al, store_with_cell, ignore_filter);
      else
	c = cell_from_store(s->u.ow.s2, al, store_with_cell, ignore_filter);
      break;
    case store_assign:
      if (eq_aloc(al, s->u.assign.al))
	{
	  c = s->u.assign.cell;
	  if (store_with_cell)
	    *store_with_cell = s;
	}
      else
	c = cell_from_store(s->u.assign.s, al, store_with_cell, ignore_filter);
      break;
    default:
      fail("Unexpected store kind %x\n", s->kind);
    }
  return c;
}

/* Return al's qtype in s */
qtype qtype_from_store(store s, aloc al)
{
  qtype qt;

  al = ecr_aloc(al);
  assert(al->kind == aloc_var);
  /*
  printf("Get ");
  print_aloc(printf, al);
  printf(" from ");
  if (al->u.var.interesting)
    print_store(printf, s);
  else
    printf("global store");
  printf("\n");
  */
  if (al->u.var.interesting)
    {
      cell c;

      c = cell_from_store(s, al, NULL, TRUE);
      if (c)
	return qt = c->qtype;
      else
	return qt = NULL;
    }
  else
    {
      /* Global */
      if (!al->u.var.fs_points_to)
	al->u.var.fs_points_to = qtype_to_fs_qtype(NULL, al->u.var.points_to);
      qt = al->u.var.fs_points_to;
    }
  /*
  printf("  yields ");
  if (qt)
    print_qtype_raw(printf, qt, NULL);
  else
    printf("NULL");
  printf("\n");
  */
  return qt;
}

/* Return the linearity of al in s. compute_lins() must be called first,
   and this function only works on assign stores */
int lin_from_store(store s, aloc al)
{
  s = ecr_store(s);
  assert(s->kind == store_assign);
  assert(eq_aloc(al, s->u.assign.al));
  return s->u.assign.lin;
}

/**************************************************************************
 *                                                                        *
 * Store Printing                                                         *
 *                                                                        *
 **************************************************************************/

int print_store(printf_func pf, store s)
{
  int result = 0;

  s = ecr_store(s);
  switch (s->kind)
    {
    case store_var:
      /* return pf("%s %s", s->u.var.name, ptr_to_ascii(s)); */
      return pf("%s", s->u.var.name);
      break;
    case store_filter:
      result = pf("filter(");
      result += print_store(pf, s->u.filter.s);
      result += pf(", ");
      result += print_effect(pf, s->u.filter.e);
      return result + pf(")");
      break;
    case store_ref:
      result = pf("ref(");
      result += print_aloc(pf, s->u.ref.al);
      result += printf(", ");
      result += print_store(pf, s->u.ref.s);
      return result + pf(")");
      break;
    case store_ow:
      result = pf("ow(");
      result += print_store(pf, s->u.ow.s1);
      result += pf(", ");
      result += print_store(pf, s->u.ow.s2);
      result += pf(", ");
      result += print_effect(pf, s->u.ow.e);
      return result + pf(")");
      break;
    case store_assign:
      result = pf("assign(");
      result += print_store(pf, s->u.assign.s);
      result += pf(", ");
      result += print_aloc(pf, s->u.assign.al);
      result += pf(", ");
      result += print_qtype(pf, s->u.assign.cell->qtype, s);
      return result + pf(")");
      break;
    default:
      fail("Unexpected store of kind %d\n", s->kind);
    }
}

static int print_cells(printf_func pf, pr_qual_fn pr_qual, cell_map cells,
		       bool print_alocs)
{
  int result;
  bool first;
  cell_map_scanner cms;
  aloc al;
  cell c;

  result = 0;
  first = TRUE;
  cell_map_scan(cells, &cms);
  result += pf("{");
  while (cell_map_next(&cms, &al, &c))
    {
      if (!first)
	result += pf(", ");
      else
	first = FALSE;
      result += print_aloc(pf, al);
      result += pf(": ");
      result += print_qtype_qf(pf, pr_qual, c->qtype, NULL, print_alocs);
    }
  result += pf("}");
  return result;
}

int print_store_cells(printf_func pf, pr_qual_fn pr_qual, store s,
		      bool print_alocs)
{
  region scratch_region;
  cell_map cells;
  int result;

  scratch_region = newregion();
  cells = store_to_cells(scratch_region, s);
  result = print_cells(pf, pr_qual, cells, print_alocs);
  deleteregion(scratch_region);
  return result;
}

/**************************************************************************
 *                                                                        *
 * Effect Statistics and Constraint Simplification                        *
 *                                                                        *
 **************************************************************************/

#ifdef STATS

/* Clear the visited marks */
void clear_alocs_visited(void)
{
  dd_list_pos pos;
  aloc al;

  dd_scan(pos, aloc_list)
    {
      al = DD_GET(aloc, pos);
      al = ecr_aloc(al);
      al->visited = FALSE;
    }
}

/* Return the number of unique alocs */
int count_alocs(void)
{
  dd_list_pos pos;
  aloc al;
  int count;

  clear_alocs_visited();
  count = 0;
  dd_scan(pos, aloc_list)
    {
      al = DD_GET(aloc, pos);
      al = ecr_aloc(al);
      if (!al->visited)
	{
	  count++;
	  al->visited = TRUE;
	}
    }

  return count;
}

/* Clear the visited marks */
void clear_effects_visited(void)
{
  dd_list_pos pos;
  effect e;

  dd_scan(pos, effect_list)
    {
      e = DD_GET(effect, pos);
      e = ecr_effect(e);
      if (e)
	e->visited = FALSE;
    }
}

/* Return the number of non-zero effects in s */
int effective_effect_set_size(effect_set s)
{
  effect_set_scanner ess;
  effect e;
  int count;

  count = 0;
  scan_effect_set(e, ess, s)
    if (ecr_effect(e) != effect_empty)
      count++;
  return count;
}

/* Set the lb_size fields for all effects */
void compute_effect_lb_sizes(void)
{
  dd_list_pos pos;
  effect e;

  dd_scan(pos, effect_list)
    {
      e = DD_GET(effect, pos);
      e = ecr_effect(e);

      if (!e)
	continue;
      if (e->kind == eff_var)
	/* Not effective_effect_set_size, since we want it to match
	   the number of times we'll visit this variable */
	e->u.var.lb_size = effect_set_size(e->u.var.lb);
    }
}


typedef struct
{
  int num_constr_any;
  int num_constr_alloc;
  int num_constr_rwr;
  int num_constr_r;
  int num_constr_wr;
  int num_union;
  int num_inter;
  int num_var;
  int num_edges;
} effect_counts;

/* Return the number of unique effects */
effect_counts count_effects(void)
{
  dd_list_pos pos;
  effect e;
  effect_counts ecounts = {0, 0, 0, 0, 0, 0, 0};

  clear_effects_visited();
  dd_scan(pos, effect_list)
    {
      e = DD_GET(effect, pos);
      e = ecr_effect(e);
      if (!e)
	continue;
      if (!e->visited)
	{
	  if (e->kind == eff_constr)
	    {
	      if (e->u.constr.kind == eff_any)
		ecounts.num_constr_any++;
	      else if (e->u.constr.kind == eff_alloc)
		ecounts.num_constr_alloc++;
	      else if (e->u.constr.kind == eff_rwr)
		ecounts.num_constr_rwr++;
	      else if (e->u.constr.kind == eff_r)
		ecounts.num_constr_r++;
	      else if (e->u.constr.kind == eff_wr)
		ecounts.num_constr_wr++;
	      else
		fail("Unexpected constr kind %x\n", e->u.constr.kind);
	      ecounts.num_edges += effective_effect_set_size(e->u.constr.ub);
	    }
	  else if (e->kind == eff_union)
	    ecounts.num_union++;
	  else if (e->kind == eff_inter)
	    {
	      ecounts.num_inter++;
	      ecounts.num_edges += effective_effect_set_size(e->u.inter.ub);
	    }
	  else if (e->kind == eff_var)
	    {
	      ecounts.num_var++;
	      ecounts.num_edges += effective_effect_set_size(e->u.var.ub);
	    }
	  else
	    fail("Unexpected eff kind %x\n", e->kind);
	  e->visited = TRUE;
	}
    }

  return ecounts;
}

/* Print out statistics about the effect constraint graph */
void print_effect_stats(void)
{
  effect_counts ecounts;
  int indent;

  printf("\n\n");
  printf("alocs: %d\n", count_alocs());

  ecounts = count_effects();
  indent = printf("effects: %d", (ecounts.num_constr_any +
				  ecounts.num_constr_alloc +
				  ecounts.num_constr_rwr +
				  ecounts.num_union +
				  ecounts.num_inter +
				  ecounts.num_var));
  printf(" (constr: %d (any: %d, alloc: %d, rwr: %d),\n",
	 (ecounts.num_constr_any +
	  ecounts.num_constr_alloc +
	  ecounts.num_constr_rwr),
	 ecounts.num_constr_any,
	 ecounts.num_constr_alloc,
	 ecounts.num_constr_rwr);
  printf("%*s  union: %d, inter: %d, var: %d)\n", indent, "",
	 ecounts.num_union,
	 ecounts.num_inter,
	 ecounts.num_var);
  printf("effect edges: %d\n", ecounts.num_edges);
}

void minimize_effects(bool emptyni)
{
  region scratch_region;
  dd_list to_minimize;
  dd_list_pos pos;
  effect e;

  clear_effects_visited();
  compute_effect_lb_sizes();

  scratch_region = newregion();
  to_minimize = dd_new_list(scratch_region);

  /* Initialize the to_minimize list with effect variables with empty
     lower bound sets.  Assumes that no effects have been minimized
     yet! */
  dd_scan(pos, effect_list)
    {
      e = DD_GET(effect, pos);
      e = ecr_effect(e);
      if (e->kind == eff_var && effect_set_empty(e->u.var.lb))
	dd_add_last(scratch_region, to_minimize, e);
      else if (emptyni && e->kind == eff_constr)
	{
	  aloc al = ecr_aloc(e->u.constr.base);
	  if (!al->u.var.interesting)
	    dd_add_last(scratch_region, to_minimize, e);
	}
    }

  while (!dd_is_empty(to_minimize))
    {
      effect_set ub;
      effect_set_scanner ess;
      effect b;

      e = DD_GET(effect, dd_first(to_minimize));
      dd_remove(dd_first(to_minimize));
      e = ecr_effect(e);
      if (!e)
	continue;
      if (e->kind == eff_inter)
	ub = e->u.inter.ub;
      else if (e->kind == eff_var)
	ub = e->u.var.ub;
      else if (emptyni && e->kind == eff_constr)
	ub = e->u.constr.ub;
      else
	fail("Unexpected effect kind %x\n", e->kind);

      e->kind = eff_link;
      e->u.link = effect_empty;

      /* Check if any of the upper bounds are now empty */
      scan_effect_set(b, ess, ub)
	{
	  b = ecr_effect(b);
	  if (!b)
	    continue;
	  assert(b->kind == eff_inter || b->kind == eff_var);
	  b->visited++;
	  if ((b->kind == eff_var && b->u.var.lb_size == b->visited) ||
	      b->kind == eff_inter)
	    /* All of b's lower bounds are zero, for a variable, or
	       at least one of b's lower bounds are zero, for an
	       intersection */
	    dd_add_last(scratch_region, to_minimize, b);
	}
    }
  deleteregion(scratch_region);
  clear_effects_visited();
}

static int scc_number = 0;
static region scc_region = NULL;
static dd_list scc_stack = NULL;
static int trivial_cycles = 0;

/* Return TRUE iff e is in scc_stack */
bool in_scc_stack(effect e)
{
  dd_list_pos pos;
  effect f;

  dd_scan(pos, scc_stack)
    {
      f = DD_GET(effect, pos);
      if (eq_effect(f, e))
	return TRUE;
    }
  return FALSE;
}

/* Find any strongly-connected components reachable from e, which much
   be a variable */
void scc_effect(effect e)
{
  effect_set_scanner ess;
  effect b;
  int number;

  assert(e->kind == eff_var && !e->visited);
  number = ++scc_number;
  e->u.var.lowlink = e->visited = number;
  dd_add_first(scc_region, scc_stack, e);

  /* Process all out-edges of e */
  scan_effect_set(b, ess, e->u.var.ub)
    {
      b = ecr_effect(b);
      if (b && b->kind == eff_var)
	{
	  if (!b->visited)
	    {
	      /* edge e->b is in the DFS traversal */
	      scc_effect(b);
	      b = ecr_effect(b);
	      assert(b->kind == eff_var);
	      e->u.var.lowlink = MIN(e->u.var.lowlink, b->u.var.lowlink);
	    }
	  else if (b->visited < number)
	    {
	      /* edge e->b is either a cross or back edge */
	      if (in_scc_stack(b))
		/* edge e->b is a back edge */
		e->u.var.lowlink = MIN(e->u.var.lowlink, b->visited);
	      /* BTW, why is that b->visited and not b->u.var.lowlink? */
	    }
	  /* else edge e->b is a regular forward edge that we didn't
	     happen to take, so do nothing */
	}
    }

  /* Check to see if we found an scc */
  e = ecr_effect(e);
  if (number == e->u.var.lowlink)
    {
      int count = 0;
      do {
	/* e is the root of an scc, so collapse the scc's nodes */
	b = DD_GET(effect, dd_first(scc_stack));
	dd_remove(dd_first(scc_stack));
	count++;
      } while (ecr_effect(b)->visited > number);
      if (count > 1)
	printf("Cycle: %d variables\n", count);
      else
	trivial_cycles++;
    }
}

/* Collapse any cycles among variables in the effect constraint
   graph */
void cycle_eliminate_effects(void)
{
  dd_list_pos pos;

  assert(!scc_region && !scc_stack);
  scc_region = newregion();
  scc_stack = dd_new_list(scc_region);
  scc_number = 0;
  clear_effects_visited();

  dd_scan(pos, effect_list)
    {
      effect e;

      e = DD_GET(effect, pos);
      e = ecr_effect(e);

      if (e && !e->visited && e->kind == eff_var)
	scc_effect(e);
    }

  scc_stack = NULL;
  deleteregion(scc_region);
  scc_region = NULL;
  printf("Trivial cycles: %d\n", trivial_cycles);
}

/* Return TRUE iff there's a path from var e1 to var e2 */
bool effect_path(effect e1, effect e2)
{
  effect_set_scanner ess;
  effect b;
  bool result;

  e1 = ecr_effect(e1);
  e2 = ecr_effect(e2);
  assert(e1->kind == eff_var && e2->kind == eff_var);
  if (e1->visited)
    return FALSE;

  e1->visited = TRUE;
  if (eq_effect(e1, e2))
    return TRUE;

  result = FALSE;
  scan_effect_set(b, ess, e1->u.var.ub)
    if (ecr_effect(b) && ecr_effect(b)->kind == eff_var)
      result = result || effect_path(b, e2);
  return result;
}

/* Return TRUE iff there's a non-trivial path from e to itself */
bool cyclic_var(effect e)
{
  effect_set_scanner ess;
  effect b;
  bool found;

  clear_effects_visited();
  e = ecr_effect(e);
  assert(e->kind == eff_var);
  found = FALSE;
  scan_effect_set(b, ess, e->u.var.ub)
    {
      b = ecr_effect(b);
      if (b && b->kind == eff_var && b != e)
	found = found || effect_path(b, e);
    }

  return found;
}

/* Count variables that can reach themselves on non-trivial cycles */
void count_cyclic_vars(void)
{
  dd_list_pos pos;
  int cyclic_vars;

  cyclic_vars = 0;
  dd_scan(pos, effect_list)
    {
      effect e;

      e = DD_GET(effect, pos);
      e = ecr_effect(e);

      if (e && e->kind == eff_var && cyclic_var(e))
	{
	  printf("Cyclic var: %s %s\n", e->u.var.name, ptr_to_ascii(e));
	  cyclic_vars++;
	}
    }
  printf("Cyclic vars: %d\n", cyclic_vars);
}

void effect_stats(void)
{
  print_effect_stats();
  minimize_effects(FALSE);
  printf("\nAfter minimization:\n");
  print_effect_stats();
  /* count_cyclic_vars(); */
  cycle_eliminate_effects();
  printf("\nAfter cycle elimination:\n");
  print_effect_stats();
}

static FILE *graph_file = NULL;

int gfprintf(const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  return vfprintf(graph_file, fmt, args);
}

void print_effect_graph(void)
{
  dd_list_pos pos;
  int count;
  FILE *f;

  f = fopen("effect.dot", "w");
  assert(!graph_file);
  graph_file = f;
  gfprintf("digraph G {\n");
  count = 0;
  dd_scan(pos, effect_list)
    {
      effect e = ecr_effect(DD_GET(effect, pos));
      effect_set ub = empty_effect_set();
      effect_set_scanner ess;
      effect b;

      switch (e->kind)
	{
	case eff_constr:
	  ub = e->u.constr.ub;
	  break;
	case eff_inter:
	  ub = e->u.inter.ub;
	  break;
	case eff_var:
	  ub = e->u.var.ub;
	  break;
	default:
	}

      scan_effect_set(b, ess, ub)
	{
	  gfprintf("\"");
	  print_effect(gfprintf, e);
	  gfprintf("\"");
	  gfprintf("->");
	  gfprintf("\"");
	  print_effect(gfprintf, b);
	  gfprintf("\"");
	  gfprintf("\n");
	  count++;
	  if (count > 100000)
	    {
	      printf("Too many nodes\n");
	      goto exit;
	    }
	}
    }
 exit:
  gfprintf("}\n");
  fclose(f);
}

/**************************************************************************
 *                                                                        *
 * Store Statistics and Constraint Simplification                         *
 *                                                                        *
 **************************************************************************/

void clear_stores_visited(void)
{
  dd_list_pos pos;
  store s;

  dd_scan(pos, store_list)
    {
      s = DD_GET(store, pos);
      s = ecr_store(s);
      s->visited = 0;
    }
}

typedef struct
{
  int num_var;
  int num_filter;
  int num_ref;
  int num_ow;
  int num_edges;
  int singles;
} store_counts;


static void scc_store_sub(store parent, store child);

static void scc_store(store s)
{
  s->lowlink = s->visited = ++scc_store_number;
  scc_store_stack_push(s);

  switch (s->kind)
    {
    case store_var:
      {
	store_edge_set_scanner sess;
	store_edge b;
	
	scan_store_edge_set(b, sess, s->u.var.lb)
	  {
	    b->store = ecr_store(b->store);
	    scc_store_sub(s, b->store);
	  }
      }
      break;
    case store_filter:
      break;
    case store_ref:
      {
	scc_store_sub(s, ecr_store(s->u.ref.s));
      }
      break;
    case store_ow:
      {
	effect e;
	    
	e = ecr_effect(s->u.ow.e);
	if (!e)
	  scc_store_sub(s, ecr_store(s->u.ow.s2));
      }
      break;
    case store_assign:
      {
	scc_store_sub(s, ecr_store(s->u.assign.s));
      }
      break;
    default:
      fail("Unexpected store kind %d\n", s->kind);
    }

  if (s->visited == s->lowlink)
    {
      store c;
      int count = 0;

      do {
	c = scc_store_stack_pop();
	count++;
      } while (c->visited > s->visited);
      if (count > 1)
	printf("Cycle : %d variables, ow's and ref's\n", count);
    }
}

static void scc_store_sub(store parent, store child)
{
  if (!child->visited)
    {
      scc_store(child);
      parent->lowlink = MIN(parent->lowlink, child->lowlink);
    }
  else
    {
      if (child->visited < parent->visited)
	{
	  if (in_scc_store_stack(child))
	    parent->lowlink = MIN(parent->lowlink, child->visited);
	}
    }
}

store_counts compute_store_stats(void)
{
  dd_list_pos pos;
  store s;
  store_counts scounts = {0, 0, 0, 0, 0};

  clear_stores_visited();
  dd_scan(pos, store_list)
    {
      s = DD_GET(store, pos);
      s = ecr_store(s);

      if (s->kind == store_var)
	{
	  store_edge_set_scanner sess;
	  store_edge b;
	  store ecr;
	  bool single;

	  scounts.num_var++;
	  scounts.num_edges += store_edge_set_size(s->u.var.lb);
	  ecr = NULL;
	  single = TRUE;
	  scan_store_edge_set(b, sess, s->u.var.lb)
	    {
	      if (ecr == NULL)
		ecr = ecr_store(b->store);
	      else if (ecr == ecr_store(b->store))
		continue;
	      else
		single = FALSE;
	    }
	  if (single)
	    scounts.singles++;
	}
      else if (s->kind == store_filter)
	scounts.num_filter++;
      else if (s->kind == store_ref)
	scounts.num_ref++;
      else if (s->kind == store_ow)
	scounts.num_ow++;
      else
	fail("Unexpected store kind %x\n", s->kind);
    }
  return scounts;
}

void cycle_eliminate_stores(void)
{
  dd_list_pos pos;
  store s;

  minimize_effects(TRUE);
  scc_store_stack_init(store_count);
  clear_stores_visited();

  dd_scan(pos, store_list)
    {
      s = DD_GET(store, pos);
      s = ecr_store(s);

      if (!s->visited)
	scc_store(s);
    }
  scc_store_stack_delete();
}

void print_store_stats(void)
{
  store_counts scounts;

  cycle_eliminate_stores();

  scounts = compute_store_stats();
  printf("stores: %d (var: %d, filter: %d, ref: %d, ow: %d singles: %d)\n",
	 (scounts.num_var + scounts.num_filter + scounts.num_ref +
	  scounts.num_ow),
	 scounts.num_var,
	 scounts.num_filter,
	 scounts.num_ref,
	 scounts.num_ow,
	 scounts.singles);
  printf("store edges: %d\n", scounts.num_edges);
}

void print_stores(void)
{
  dd_list_pos pos;
  store s;

  dd_scan(pos, store_list)
    {
      s = DD_GET(store, pos);
      print_store(printf, s);
      printf(" ");
      print_store_cells(printf, print_qual, s, TRUE);
      printf("\n");
    }
}

void stats(void)
{
  printf("---------------------------------------------\n");
  print_stores();
  /*
  print_effect_stats();
  print_store_stats();
  */
}

#endif
