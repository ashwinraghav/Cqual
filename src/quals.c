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

#include <stdio.h>
#include <string.h>
#include "color.h"
#include "containers.h"
#include "effect.h"
#include "analyze.h"
#include "qerror.h"
#include "quals.h"
#include "utils.h"
#include "pam.h"

/*#define DEBUG*/    /* Enable printing of generated constraints */
/*#define BITSET*/   /* Enable bitset code; not fully implemented */

#ifdef BITSET
#include "bitset.h"
#endif

#ifndef AST_H
typedef struct po_info *po_info;
#endif

typedef enum { dir_none = 0, dir_forward = 1, dir_backward = 2} dir;

typedef struct Traverse_edge
{
  location loc;
  qual qual;
  dir dir;
} *traverse_edge;

#ifdef BITSET
struct Bounds
{
  bitset bitset;      /* Qualifier set */
  bitset up;          /* Upward closure */
  bitset down;        /* Downward closure */
};
#endif

struct type_qualifier {
  enum { qk_constant, qk_variable, qk_link } kind;
  bool mark;                       /* For traversing the graph */
  traverse_edge parent;            /* Parent in current spanning tree. */
#ifdef BITSET
  bounds bounds;		   /* Possible values for this qualifier */
#endif
  union {

    /* Constant */
    struct {
      const char *name;		     /* Name of this qualifier */
      const char *color;             /* Color information for PAM mode */
      level_qual_t level;            /* Level information for qtype */
      sign_qual_t sign;              /* Sign information for qtype */
      qual_set lbc, ubc;             /* Upper- and lower-bounds in order */
#ifdef BITSET
      int index;                     /* Position in bitset for this
					qualifier */
#endif
      po_info po;                    /* Partial order this qual is in */
    } elt;

    /* Variable */
    struct {
      const char *name;
      location loc;                  /* where this qualifier came from */
      store store;                   /* for stores */
      loc_set no_qual_loc_set;       /* no_qual locations */
      aloc aloc;
      qual_edge_set lb, ub;          /* lower/upper bounds (generated) */
#ifndef BITSET
      qual_set lbc, ubc;             /* lower/upper bounds (solution) */
#endif
      cond_set cond_set;             /* conditional constraints pending
					on this variable */
      int num_equiv;                 /* # of vars unified with this one */
      bool preferred;                /* True we should try to keep the
				        name of this variable as the ecr */
      int num_errors;                /* # of error paths this variable is
					involved in */
    } var;

    /* Variable that's just been unified with another variable.  In
       this case, mark and parent don't make sense. */
    qual link;
  } u;
};

struct po_info {
  qual_set qualifiers;       /* The qualifiers in this partial order */
  bool flow_sensitive;       /* True if these qualifiers are flow-sensitive */
  bool nonprop;              /* True if these qualifiers don't propagate
				through constraints */
#ifdef BITSET
  int first, last;           /* Index in bitset of first and last element */
#endif
};

typedef enum cond_dir { c_leq_v, v_leq_c} cond_dir;

struct Condition {
  cond_dir dir;
  qual c, left, right;         /* constraint is
				   c <= v ==> left <= right   if dir = c_leq_v
				   v <= c ==> left <= right   if dir = v_leq_c
				  where v is the variable this constraint
				  is associated with */
  location loc;
  bool triggered;              /* TRUE iff this condition has been
				  triggered */
};

/* Some prototypes */

cond mkcond(cond_dir dir, qual c, qual left, qual right, location loc);

static qual ecr_qual(qual q);
static void increment_error_counts(qual left, qual right);
static void update_error_count(qual q);
static bool cond_set_trigger(qual q);
#ifdef BITSET
static void construct_qual_bitset(qual q);
#endif

/**************************************************************************
 *                                                                        *
 * Global Variables and Initialization                                    *
 *                                                                        *
 **************************************************************************/

/* Used to prevent some catastrophic errors from occurring */
enum { state_start, state_init, state_pos_defined } state = state_start;

/* Set to TRUE if after end_define_pos() is called there are any
   calls to find_qual to look up a flow-sensitive qualifier. */
bool found_fs_qual = FALSE;

/* Set (externally) to true if we should print out the qualifier
   constraint graph in dot form. */
int flag_print_quals_graph = 0; 

static region quals_region = NULL;

static int next_qual = 0;
static int max_index = 0;
static FILE *graph_file = NULL;
static qual_set all_quals;
static po_set all_pos;
static po_info current_po;
qual const_qual = NULL;
qual nonconst_qual = NULL;
qual volatile_qual = NULL;
qual restrict_qual = NULL;
qual noreturn_qual = NULL;
qual init_qual = NULL;
qual noninit_qual = NULL;

/* Set (externally) to number of hotspots to track. 0 = track no
   hotspots. */
int num_hotspots = 0;         

qual *hotspots; /* Array of size num_hotspots */

#ifdef BITSET
bounds bounds_any;
static bitset_bounds_map bitset_hash_table = NULL;
#endif

/* Called before any other functions in quals.h are called. */
void init_quals(void)
{
  assert(!quals_region);
  quals_region = newregion();
  next_qual = 0;
  max_index = 0;
  all_quals = empty_qual_set();
  all_pos = empty_po_set();
  current_po = NULL;
  assert(num_hotspots >= 0);
  if (num_hotspots != 0)
    /* rarrayalloc initializes hotspots to 0 */
    {
      int i;

      hotspots = rarrayalloc(quals_region, num_hotspots, qual);
      for (i = 0; i < num_hotspots; i++)
	hotspots[i] = NULL;
    }
  else
    hotspots = NULL;
  if (flag_print_quals_graph)
    {
      if (graph_file)
	fclose(graph_file);
      graph_file = fopen("quals.dot", "w");
      fprintf(graph_file, "digraph G {\n");
    }
  state = state_init;
}

/* Called when no more partial orders will be defined.  You *must*
   call this function before creating any qualifier variables. */
void end_define_pos(void)
{
  assert(state == state_init);
#ifdef BITSET
  {
    qual_set_scanner qs;
    qual q;

    bitset_hash_table = make_bitset_bounds_map(quals_region, 33);
    scan_qual_set(q, qs, all_quals)
      construct_qual_bitset(q);

    bounds_any = ralloc(quals_region, struct Bounds);
    bounds_any->bitset = bitset_new(quals_region, max_index);
    bitset_insert_all(bounds_any->bitset);
    bounds_any->up = bounds_any->bitset;
    bounds_any->down = bounds_any->bitset;
    bitset_bounds_map_insert(bitset_hash_table, bounds_any->bitset,
			     bounds_any);
  }
#endif
  state = state_pos_defined;
}

/* Called when no more constraints will be generated. */
void finish_quals(void)
{
  if (flag_print_quals_graph)
    {
      fprintf(graph_file, "}\n");
      fclose(graph_file);
    }
}

/* Disposes of all memory internally allocated by functions in
   quals.c.  All qualifiers and qualifier variables become invalid
   after calling this function. */
void dispose_quals(void)
{
  assert(quals_region);
  deleteregion(quals_region);
  quals_region = 0;
  state = state_start;
}

#ifdef BITSET
/* Print b, but with names */
void bitset_print_pretty(bitset b)
{
  qual_set_scanner qs;
  qual q;
  bool first;

  first = TRUE;
  scan_qual_set(q, qs, all_quals)
    if (bitset_member(b, q->u.elt.index))
      {
	if (!first)
	  printf(" ");
	first = FALSE;
	printf(q->u.elt.name);
      }
}

/**************************************************************************
 *                                                                        *
 * Bitsets                                                                *
 *                                                                        *
 **************************************************************************/

/* Construct the bitset for q and initailize the bitset hash table
   with entries for q's bitset */
static void construct_qual_bitset(qual q)
{
  qual_set_scanner qs;
  qual p;
  bitset b, up, down;
  bounds bounds;

  assert(q->kind == qk_constant);
  if (nonprop_qual(q))
    return;

  /* Construct bitset for q */

  /* Assume initially q consistent with all qualifiers */
  b = bitset_new(quals_region, max_index);
  up = bitset_new(quals_region, max_index);
  down = bitset_new(quals_region, max_index);
  bitset_insert_all(b);
  bitset_insert_all(up);
  bitset_insert_all(down);

  /* Remove any qualifiers that are inconsistent with q.  Note that
     these can only be qualifiers in q's partial order. */
  scan_qual_set(p, qs, q->u.elt.po->qualifiers)
    {
      if (!eq_qual(q, p))
	insist(bitset_remove(b, p->u.elt.index));
      if (!leq_qual(q, p))
	insist(bitset_remove(up, p->u.elt.index));
      if (!leq_qual(p, q))
	insist(bitset_remove(down, p->u.elt.index));
    }

  /*
  printf("bitset for %s: ", q->u.elt.name);
  bitset_print_pretty(b);
  printf("\n");
  */

  /* Add computed bounds to hash table */
  bounds = ralloc(quals_region, struct Bounds);
  bounds->bitset = b;
  bounds->up = up;
  bounds->down = down;
  q->bounds = bounds;
  insist(bitset_bounds_map_insert(bitset_hash_table, b, bounds));

  /*
  printf("upward closure of %s: ", q->u.elt.name);
  bitset_print_pretty(up);
  printf("\ndownward closure for %s: ", q->u.elt.name);
  bitset_print_pretty(down);
  printf("\n");
  */
}

/* Return bounds set for b.  b may be safely deallocated after calling
   this function. */
static bounds find_bounds(bitset b)
{
  bounds bounds;

  if (!bitset_bounds_map_lookup(bitset_hash_table, b, &bounds))
    {
      /* Not in table, so make a new entry. */
      qual_set_scanner qs;
      qual p;

      bounds = ralloc(quals_region, struct Bounds);
      bounds->bitset = bitset_copy(quals_region, b);
      bounds->up = bitset_new(quals_region, max_index);
      bounds->down = bitset_new(quals_region, max_index);

      scan_qual_set(p, qs, all_quals)
	if (bitset_member(b, p->u.elt.index))
	  {
	    /* Add any upper/lower bounds of p */
	    qual_set_scanner qs2;
	    qual p2;

	    scan_qual_set(p2, qs2, p->u.elt.po->qualifiers)
	      {
		if (leq_qual(p, p2))
		  bitset_insert(bounds->up, p2->u.elt.index);
		if (leq_qual(p2, p))
		  bitset_insert(bounds->down, p2->u.elt.index);
	      }
	  }
      insist(bitset_bounds_map_insert(bitset_hash_table, bounds->bitset,
				      bounds));

      /*
      printf("constructed new bitset ");
      bitset_print(bounds->bitset);
      printf("\nupward closure: ");
      bitset_print(bounds->up);
      printf("\ndownward closure: ");
      bitset_print(bounds->down);
      printf("\n");
      */
    }
  return bounds;
}
#endif

/**************************************************************************
 *                                                                        *
 * Qualifier Constants                                                    *
 *                                                                        *
 **************************************************************************/

/* Begin and end defining a partial order.  All the qualifiers created
   between calls to begin and end are exclusively related. */
void begin_po_qual(void)
{
  if (current_po)
    fail("begin_po_qual called without previous end_po_qual\n");
  current_po = ralloc(quals_region, struct po_info);
  current_po->qualifiers = empty_qual_set();
  current_po->flow_sensitive = FALSE;
  current_po->nonprop = FALSE;
}

void end_po_qual(void)
{
  if (!current_po)
    fail("end_po_qual called without previous begin_po_qual\n");
  po_set_insert(quals_region, &all_pos, current_po);
  current_po = NULL;
}

/* Mark the current partial order flow-sensitive */
void set_po_flow_sensitive(void)
{
  current_po->flow_sensitive = TRUE;
  current_po->nonprop = TRUE;
}

/* Mark the current partial order as non-propagating */
void set_po_nonprop(void)
{
  current_po->nonprop = TRUE;
#ifdef BITSET
  current_po->first = -1;
  current_po->last = -1;
#endif
}

/* Make a new (constant) qualifier */
qual add_qual(const char *name)
{
  /* qual_set_insert does pointer comparisons, not strcmps, so we need
     to do a special search by name first. */
  qual new_qual;

  assert(state != state_pos_defined);
  if (!current_po)
    fail("add_qual called without previous begin_po_qual\n");
  if ((new_qual = find_qual(name)))
    {
      if (new_qual->u.elt.po != current_po)
	fail("Qualifier %s in two different partial orders\n",
	     new_qual->u.elt.name);
      return new_qual;
    }
  /* We didn't find the qualifier */
  /*  printf("Adding qualifier %s\n", name);*/
  new_qual = ralloc(quals_region, struct type_qualifier);
  new_qual->kind = qk_constant;
  new_qual->mark = FALSE;
  new_qual->u.elt.name = rstrdup(quals_region, name);
  new_qual->u.elt.color = NULL;
  new_qual->u.elt.level = level_value;
  new_qual->u.elt.sign = sign_pos;
  new_qual->u.elt.lbc = empty_qual_set();
  new_qual->u.elt.ubc = empty_qual_set();
  new_qual->u.elt.po = current_po;
#ifdef BITSET
  if (!current_po->nonprop)
    {
      new_qual->u.elt.index = max_index++;
      if (qual_set_empty(current_po->qualifiers))
	current_po->first = new_qual->u.elt.index;
      current_po->last = new_qual->u.elt.index;
    }
#endif
  qual_set_insert(quals_region, &all_quals, new_qual);
  qual_set_insert(quals_region, &current_po->qualifiers, new_qual);
  if (!strcmp(name, "const"))
    const_qual = new_qual;
  else if (!strcmp(name, "$nonconst"))
    nonconst_qual = new_qual;
  else if (!strcmp(name, "volatile"))
    volatile_qual = new_qual;
  else if (!strcmp(name, "restrict"))
    restrict_qual = new_qual;
  else if (!strcmp(name, "noreturn"))
    noreturn_qual = new_qual;
  else if (!strcmp(name, "$init"))
    init_qual = new_qual;
  else if (!strcmp(name, "$noninit"))
    noninit_qual = new_qual;
  return new_qual;
}

/* Add assumption left < right */
void add_qual_lt(qual left, qual right)
{
  assert(left->kind == qk_constant);
  assert(right->kind == qk_constant);
  if (!qual_set_member(left->u.elt.ubc, right))
    {
      qual_set_scanner qs;
      qual q;

      /*
      printf("Adding assumption ");
      print_qual_raw(printf, left);
      printf(" < ");
      print_qual_raw(printf, right);
      printf("\n");
      */

      qual_set_insert(quals_region, &left->u.elt.ubc, right);
      qual_set_insert(quals_region, &right->u.elt.lbc, left);

      /* Add transitively-implied assumptions */
      scan_qual_set(q, qs, right->u.elt.ubc)
	add_qual_lt(left, q);

      scan_qual_set(q, qs, left->u.elt.lbc)
	add_qual_lt(q, right);
    }
}

/* Add color for qualifier */
void add_color_qual(qual q, const char *color)
{
  assert(q->kind == qk_constant);
  q->u.elt.color = rstrdup(quals_region, color);
}

/* Assert that q is a qualifier on level lev */
void add_level_qual(qual q, level_qual_t lev)
{
  assert(q->kind == qk_constant);
  assert(lev == level_value || lev == level_ref);

  q->u.elt.level = lev;
}

/* Assert that q is a qualifier on values */
void add_sign_qual(qual q, sign_qual_t sign)
{
  assert(q->kind == qk_constant);
  assert(sign == sign_pos || sign == sign_neg || sign == sign_eq);
  q->u.elt.sign = sign;
}

/* Look up a qualifier */
qual find_qual(const char *name)
{
  qual_set_scanner qs;
  qual q;

  scan_qual_set(q, qs, all_quals)
    {
      assert(q->kind == qk_constant);
      if (!strcmp(q->u.elt.name, name))
	{
	  if (state == state_pos_defined && q->u.elt.po->flow_sensitive)
	    found_fs_qual = TRUE;
	  return q;
	}
    }
  return NULL;
}

/* Change flow-sensitive quals from nonprop to regular quals for this pass */
void reset_flow_sensitive_quals(void)
{
  po_set_scanner ss;
  po_info po;

  scan_po_set(po, ss, all_pos)
    if (po->flow_sensitive)
      po->nonprop = FALSE;
}

/**************************************************************************
 *                                                                        *
 * Qualifier Variables                                                    *
 *                                                                        *
 **************************************************************************/

/* Make a fresh qualifier variable */
qual make_qvar(const char *name, location loc, bool preferred)
{
  qual fresh;

  assert(state == state_pos_defined);
  fresh = ralloc(quals_region, struct type_qualifier);
  fresh->kind = qk_variable;
  fresh->mark = FALSE;
  fresh->u.var.name = name;
  fresh->u.var.loc = loc;
  fresh->u.var.store = NULL;
  fresh->u.var.no_qual_loc_set = empty_loc_set();
  fresh->u.var.aloc = NULL;
  fresh->u.var.lb = empty_qual_edge_set();
  fresh->u.var.ub = empty_qual_edge_set();
#ifdef BITSET
  fresh->bounds = bounds_any;
#else
  fresh->u.var.lbc = empty_qual_set();
  fresh->u.var.ubc = empty_qual_set();
#endif
  fresh->u.var.cond_set = empty_cond_set();
  fresh->u.var.num_equiv = 1;
  fresh->u.var.preferred = preferred;
  fresh->u.var.num_errors = 0;
  return fresh;
}

/* Make a fresh qvar, with a (somewhat) unique name. */
qual make_fresh_qvar(const char *base_name, location loc)
{
  const char *name;

  name = rstrcat(quals_region, base_name,
		 inttostr(quals_region, next_qual++));
  return make_qvar(name, loc, FALSE);
}

/* Check no qualifier flows into this qual */
void mk_no_qual_qual(location loc, qual q)
{
  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  if (q->u.var.lbc || q->u.var.ubc)
    report_qerror(loc, sev_err, "Qualifier constraint reached a no-qual zone");
  loc_set_insert(quals_region, &q->u.var.no_qual_loc_set, loc);
}

/* Associate (s, al) with q; return TRUE if this triggers a propagation */
bool store_aloc_qual(qual q, store s, aloc al)
{
  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  assert(!q->u.var.store && !q->u.var.aloc);
  q->u.var.store = s;
  q->u.var.aloc = al;
  if (q->u.var.lbc)
    propagate_store_cell_forward(s, al);
  if (q->u.var.ubc)
    propagate_store_cell_backward(s, al);

  if (!q->u.var.lbc && !q->u.var.ubc)
    return FALSE;
  else
    return TRUE;
}

/* Return the ECR for this qualifier */
static qual ecr_qual(qual q)
{
  switch (q->kind)
    {
    case qk_constant:
    case qk_variable:
      return q;
    case qk_link:
      {
	qual ecr = q, cur, temp;
	
	/* Find root */
	while (ecr->kind == qk_link)
	  ecr = ecr->u.link;
	
	/* Compress path */
	cur = q;
	while (cur->u.link != ecr)
	  {
	    temp = cur->u.link;
	    cur->u.link = ecr;
	    cur = temp;
	  }
	return ecr;
      }
      break;
    default:
      fail("Unexpected qual kind %x\n", q->kind);
    }
}

/**************************************************************************
 *                                                                        *
 * Qualifier Edges                                                        *
 *                                                                        *
 **************************************************************************/

static inline qual_edge mkqual_edge(region r, location loc, qual q)
{
  qual_edge result = ralloc(r, struct Qual_edge);
  result->loc = loc;
  result->qual = q;
  if (loc)
    pam_add_overlay_anchor(loc);
  return result;
}

static inline traverse_edge mktraverse_edge(region r, qual q, location loc,
					    dir d)
{
  traverse_edge result = ralloc(r, struct Traverse_edge);
  result->loc = loc;
  result->qual = q;
  result->dir = d;
  return result;
}

/**************************************************************************
 *                                                                        *
 * Predicates and Queries                                                 *
 *                                                                        *
 **************************************************************************/

/* Return the level of qualifier q */
level_qual_t level_qual(qual q)
{
  assert(q->kind == qk_constant);
  return q->u.elt.level;
}

/* Return the sign of qualifier q */
sign_qual_t sign_qual(qual q)
{
  assert(q->kind == qk_constant);
  return q->u.elt.sign;
}

/* Return TRUE iff q is flow-sensitive */
bool flow_sensitive_qual(qual q)
{
  assert(q->kind == qk_constant);
  return q->u.elt.po->flow_sensitive;
}

/* Return TRUE iff q is non-propagating */
bool nonprop_qual(qual q)
{
  assert(q->kind == qk_constant);
  return q->u.elt.po->nonprop;
}

/* Return TRUE iff q is a variable (or a link to a variable) */
bool variable_qual(qual q)
{
  q = ecr_qual(q);
  return q->kind == qk_variable;
}

/* Return TRUE iff q is a constant */
bool constant_qual(qual q)
{
  q = ecr_qual(q);
  return q->kind == qk_constant;
}

/* Return TRUE iff q is preferred */
bool preferred_qual(qual q)
{
  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  return q->u.var.preferred;
}

/* Return the location a qualifier variable came from */
location location_qual(qual q)
{
  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  return q->u.var.loc;
}

/* Lower-bound variables of q */
qual_edge_set lb_qual(qual q)
{
  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  return q->u.var.lb;
}

/* Upper-bound variables of q */
qual_edge_set ub_qual(qual q)
{
  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  return q->u.var.ub;
}

/* Number of error paths q is involved in */
int error_count_qual(qual q)
{
  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  return q->u.var.num_errors;
}

/**************************************************************************
 *                                                                        *
 * Constraint Generation                                                  *
 *                                                                        *
 **************************************************************************/

int print_graph_file(const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  return vfprintf(graph_file, fmt, args);
}

#ifdef BITSET
static bool update_var_bounds_left(qual q, bounds bounds);
static bool update_var_bounds_right(qual q, bounds bounds);

/* Return TRUE if there exists a partial order po such that b contains
   no elements of po. */
static bool invalid_bounds(bitset b)
{
  po_set_scanner ss;
  po_info po;
  bool result;

  result = FALSE;
  scan_po_set(po, ss, all_pos)
    if (!po->nonprop)
      result = result || bitset_empty_range(b, po->first, po->last);
  return result;
}

/* Call update_var_bounds_X for all bounds of variable q */
static bool propagate_bounds_change(qual q)
{
  bool result;
  qual_edge_set_scanner qess;
  qual_edge qe;
  cond_set_scanner css;
  cond cond;

  assert(q->kind == qk_variable);
  result = FALSE;

  /* Propagate to neighbors in constraint graph */
  scan_qual_edge_set(qe, qess, q->u.var.lb)
    if (variable_qual(qe->qual))
      result = update_var_bounds_left(qe->qual, q->bounds) || result;
  scan_qual_edge_set(qe, qess, q->u.var.ub)
    if (variable_qual(qe->qual))
      result = update_var_bounds_right(qe->qual, q->bounds) || result;

  /* Trigger any conditional constraints */
  result = cond_set_trigger(q) || result;

  return result;
}

/* Constrain variable q to be <= bounds */
static bool update_var_bounds_left(qual q, bounds bounds)
{
  bitset new_bitset;
  bool result;

  q = ecr_qual(q);
  assert(q->kind == qk_variable);

  if (q->bounds == bounds)
    return FALSE;

  /*
  printf("Bitset ");
  bitset_print_pretty(q->bounds->bitset);
  printf(" <= ");
  bitset_print_pretty(bounds->bitset);
  printf("\n");
  */

  result = FALSE;
  new_bitset = alloca_bitset(max_index);
  /*new_bitset = bitset_new(quals_region, max_index);*/
  bitset_assign(new_bitset, q->bounds->bitset);
  if (bitset_intersect(new_bitset, bounds->down))
    {
      /* We changed the set for q
      printf("  Result: ");
      bitset_print(new_bitset);
      printf("\n");
      */

      result = invalid_bounds(new_bitset) || result;
      q->bounds = find_bounds(new_bitset);
      result = propagate_bounds_change(q) || result;
    }
  /*
  else
    printf("  Result: (no change)\n");
  */

  return result;
}

/* Constrain variable q to be >= bounds */
static bool update_var_bounds_right(qual q, bounds bounds)
{
  bitset new_bitset;
  bool result;

  q = ecr_qual(q);
  assert(q->kind == qk_variable);

  if (q->bounds == bounds)
    return FALSE;

  /*
  printf("Bitset ");
  bitset_print_pretty(bounds->bitset);
  printf(" <= ");
  bitset_print_pretty(q->bounds->bitset);
  printf("\n");
  */

  result = FALSE;
  new_bitset = alloca_bitset(max_index);
  /*new_bitset = bitset_new(quals_region, max_index);*/
  bitset_assign(new_bitset, q->bounds->bitset);
  if (bitset_intersect(new_bitset, bounds->up))
    {
      /* We changed the set for q
      printf("  Result: ");
      bitset_print_pretty(new_bitset);
      printf("\n");
      `*/

      result = invalid_bounds(new_bitset) || result;
      q->bounds = find_bounds(new_bitset);
      result = propagate_bounds_change(q) || result;
    }
  /*
  else
    printf("  Result: (no change)\n");
  */

  return result;
}

/* Add constraint left <= right to the system and solve.  Returns TRUE
   iff an error occurred. */
bool mkleq_qual(location loc, qual left, qual right)
{
  bool result;

  result = FALSE;
  if (flag_print_quals_graph)
    {
      /*
      fprintf(graph_file, "%p %p\n", left, right);
      */
      fprintf(graph_file, "\"");
      print_qual_raw(print_graph_file, left);
      if (flag_ugly)
	fprintf(graph_file, " %p\" -> \"", left);
      else
	fprintf(graph_file, "\"->\"");
      print_qual_raw(print_graph_file, right);
      if (flag_ugly)
	fprintf(graph_file, " %p\"\n", right);
      else
	fprintf(graph_file, "\"\n");
    }
#ifdef DEBUG
  print_qual_raw(printf, left);
  printf(" %p <= ", left);
  print_qual_raw(printf, right);
  printf(" %p\n", right);
#endif
  left = ecr_qual(left);
  right = ecr_qual(right);
  if (left == right)
    /* Reflexivity */
    return FALSE;

  if (left->kind == qk_constant && right->kind == qk_constant)
    result = !leq_qual(left, right) || result;
  else
    {
      if (left->kind == qk_variable)
	{
	  qual_edge qe;

	  qe = mkqual_edge(quals_region, loc, right);
	  /*printf("insert in %s: %p\n", name_qual(left), qe);*/
	  qual_edge_set_insert(quals_region, &left->u.var.ub, qe);
	  if (right->bounds) /* Right may be non-prop qual constant */
	    result = update_var_bounds_left(left, right->bounds) || result;
	}

      if (right->kind == qk_variable)
	{
	  qual_edge qe;

	  qe = mkqual_edge(quals_region, loc, left);
	  /*printf("insert in %s: %p\n", name_qual(right), qe);*/
	  qual_edge_set_insert(quals_region, &right->u.var.lb, qe);
	  if (left->bounds)  /* Left may be non-prop qual constant */
	    result = update_var_bounds_right(right, left->bounds) || result;
	}
    }

  if (result)
    {
      fflush(NULL);
      printf("Unsatisfiable ");
      print_qual_raw(printf, left);
      printf(" <= ");
      print_qual_raw(printf, right);
      printf("\n");
    }

  if (result)
    increment_error_counts(left, right);

  return result;
}

/* left == right */
bool mkeq_qual(location loc, qual left, qual right)
{
  bool result;
  result = mkleq_qual(loc, left, right);
  result = mkleq_qual(loc, right, left) || result;
  return result;
}

/* Returns TRUE iff left <= right.  Requires that at least one of
   left, right is a constant.  By defn, returns TRUE if left and right
   are in distinct partial orders. */
bool leq_qual(qual left, qual right)
{
  left = ecr_qual(left);
  right = ecr_qual(right);

  assert(constant_qual(left) || constant_qual(right));
  if (left == right)
    return TRUE;     /* Reflexivity */

  /* XXX Fix for transitive partial order constraints */
  if (left->kind == qk_constant && right->kind == qk_constant)
    return (qual_set_member(left->u.elt.ubc, right) ||
	    !qual_set_member(left->u.elt.po->qualifiers, right));
  else if (left->kind == qk_variable)
    {
      bitset b;
      bool result;

      b = alloca_bitset(max_index);
      /*b = bitset_new(quals_region, max_index);*/
      bitset_assign(b, left->bounds->bitset);

      /* If adding the constraint that left <= right doesn't change
	 anything, then return TRUE, else return FALSE. */
      result = !bitset_intersect(b, right->bounds->down);

      return result;
    }
  else
    {
      bitset b;
      bool result;

      assert (right->kind == qk_variable);
      b = alloca_bitset(max_index);
      /*b = bitset_new(quals_region, max_index);*/
      bitset_assign(b, right->bounds->bitset);

      /* If adding the constraint that left <= right doesn't change
	 anything, then return TRUE, else return FALSE. */
      result = !bitset_intersect(b, left->bounds->up);

      return result;
    }
}

#else /* #IFDEF BITSET */

/* left <= right.  orig is true iff this constraint was generated
   directly instead of added through transitivity. */
bool _mkleq_qual(location loc, qual left, qual right, bool orig)
{
  if (flag_print_quals_graph && orig)
    {
      /*
      fprintf(graph_file, "%p %p\n", left, right);
      */
      fprintf(graph_file, "\"");
      print_qual_raw(print_graph_file, left);
      if (flag_ugly)
	fprintf(graph_file, " %p\" -> \"", left);
      else
	fprintf(graph_file, "\" -> \"");
      print_qual_raw(print_graph_file, right);
      if (flag_ugly)
	fprintf(graph_file, " %p\"\n", right);
      else
	fprintf(graph_file, "\"\n");
    }
#ifdef DEBUG
  print_qual_raw(printf, left);
  printf(" %p <= ", left);
  print_qual_raw(printf, right);
  printf(" %p\n", right);
#endif
  left = ecr_qual(left);
  right = ecr_qual(right);
  if (left == right)
    /* Reflexivity */
    return FALSE;
  if (left->kind == qk_constant && right->kind == qk_constant)
    {
      /* Do comparison in lattice. */
      bool result = !leq_qual(left, right);

      if (result && growbuf_empty(qual_error_detail))
	{
	  qual_ed_printf("unsatisfiable qualifier constraint %s <= %s",
			 left->u.elt.name, right->u.elt.name);
	}

#ifdef DEBUG
      if (result)
	{
	  fflush(NULL);
	  printf("Unsatisfiable ");
	  print_qual_raw(printf, left);
	  printf(" <= ");
	  print_qual_raw(printf, right);
	  printf("\n");
	}
#endif
	
      return result;
    }
  else if (left->kind == qk_constant && right->kind == qk_variable)
    {
      bool result = FALSE;

      if (!nonprop_qual(left) && !qual_set_member(right->u.var.lbc, left))
	if (right->u.var.store)
	  propagate_store_cell_forward(right->u.var.store, 
				       right->u.var.aloc);

      if (orig) /* Add no matter what */
	{
	  qual_edge qe;

	  qe = mkqual_edge(quals_region, loc, left);
	  /*printf("insert in %s: %p\n", name_qual(right), qe);*/
	  qual_edge_set_insert(quals_region, &right->u.var.lb, qe);
	  if (qual_set_empty(left->u.elt.lbc) &&
	      qual_set_empty(left->u.elt.ubc))
	    {
	      qual_edge qe;

	      qe = mkqual_edge(quals_region, loc, left);
	      /*printf("insert in %s: %p\n", name_qual(right), qe);*/
	      qual_edge_set_insert(quals_region, &right->u.var.ub, qe);
	    }
	}
      if (!nonprop_qual(left) && !qual_set_member(right->u.var.lbc, left))
	{
	  /* Propagate if we haven't already */
	  qual q;
	  qual_set_scanner qs;
	  qual_edge qe;
	  qual_edge_set_scanner qess;

	  qual_set_insert(quals_region, &right->u.var.lbc, left);

	  {
	    loc_set_scanner ls;
	    location loc;
	    
	    scan_loc_set(loc, ls, right->u.var.no_qual_loc_set)
	      report_qerror(loc, sev_err, 
			    "Qualifier constraint reached a no-qual zone");
	  }

	  scan_qual_set(q, qs, right->u.var.ubc)
	    result = _mkleq_qual(loc, left, q, FALSE) || result;
	  scan_qual_edge_set(qe, qess, right->u.var.ub)
	    result = _mkleq_qual(loc, left, qe->qual, FALSE) || result;
	  if (qual_set_empty(left->u.elt.ubc))
	    /* If left has no upper bounds then left <= right
	       ==> left = right. */
	    result = _mkleq_qual(loc, right, left, orig) || result;
	  result = cond_set_trigger(right) || result;
	}
      if (orig && result)
	increment_error_counts(left, right);

      return result;
    }
  else if (left->kind == qk_variable && right->kind == qk_constant)
    {
      bool result = FALSE;

      if (!nonprop_qual(right) && !qual_set_member(left->u.var.ubc, right))
	{
	  if (left->u.var.store)
	    propagate_store_cell_backward(left->u.var.store, 
					  left->u.var.aloc);
	}

      if (orig) /* Add no matter what */
	{
	  qual_edge qe;

	  qe = mkqual_edge(quals_region, loc, right);
	  /*printf("insert in %s: %p\n", name_qual(left), qe);*/
	  qual_edge_set_insert(quals_region, &left->u.var.ub, qe);
	  if (qual_set_empty(right->u.elt.lbc) &&
	      qual_set_empty(right->u.elt.ubc))
	    {
	      qual_edge qe;

	      qe = mkqual_edge(quals_region, loc, right);
	      /*printf("insert in %s: %p\n", name_qual(left), qe);*/
	      qual_edge_set_insert(quals_region, &left->u.var.lb, qe);
	    }
	}
      if (!nonprop_qual(right) && !qual_set_member(left->u.var.ubc, right))
	{
	  /* Propagate if we haven't already */
	  qual q;
	  qual_set_scanner qs;
	  qual_edge qe;
	  qual_edge_set_scanner qess;

	  qual_set_insert(quals_region, &left->u.var.ubc, right);

	  {
	    loc_set_scanner ls;
	    location loc;

	    scan_loc_set(loc, ls, left->u.var.no_qual_loc_set)
	      report_qerror(loc, sev_err, 
			    "Qualifier constraint reached a no-qual zone");
	  }

	  scan_qual_set(q, qs, left->u.var.lbc)
	    result = _mkleq_qual(loc, q, right, FALSE) || result;
	  scan_qual_edge_set(qe, qess, left->u.var.lb)
	    result = _mkleq_qual(loc, qe->qual, right, FALSE) || result;
	  if (qual_set_empty(right->u.elt.lbc))
	    /* If right has no lower bounds then left <= right
	       ==> left = right. */
	    result = _mkleq_qual(loc, right, left, orig) || result;
	  result = cond_set_trigger(left) || result;
	}
      if (orig && result)
	increment_error_counts(left, right);

      return result;
    }
  else
    {
      /* Both variables -- Propagate constants in both directions */
      bool result = FALSE;
      qual q1, q2;
      qual_set_scanner qs;
      qual_edge qe_left, qe_right;

      qe_left = mkqual_edge(quals_region, loc, right);
      /*printf("insert in %s: %p\n", name_qual(left), qe_left);*/
      qual_edge_set_insert(quals_region, &left->u.var.ub, qe_left);

      qe_right = mkqual_edge(quals_region, loc, left);
      /*printf("insert in %s: %p\n", name_qual(right), qe_right);*/
      qual_edge_set_insert(quals_region, &right->u.var.lb, qe_right);

      scan_qual_set(q1, qs, left->u.var.lbc)
	result = _mkleq_qual(loc, q1, right, FALSE) || result;
      scan_qual_set(q2, qs, right->u.var.ubc)
	result = _mkleq_qual(loc, left, q2, FALSE) || result;

      if (orig && result)
	increment_error_counts(left, right);

      return result;
    }
}

bool mkleq_qual(location loc, qual left, qual right)
{
  return _mkleq_qual(loc, left, right, TRUE);
}

/* left == right */
bool mkeq_qual(location loc, qual left, qual right)
{
  bool result;
  result = mkleq_qual(loc, left, right);
  result = mkleq_qual(loc, right, left) || result;
  return result;
}

/* Returns TRUE iff left <= right.  Requires that at least one of
   left, right is a constant.  By defn, returns TRUE if left and right
   are in distinct partial orders. */
bool leq_qual(qual left, qual right)
{
  left = ecr_qual(left);
  right = ecr_qual(right);

  assert(constant_qual(left) || constant_qual(right));
  if (left == right)
    return TRUE;     /* Reflexivity */

  /* XXX Fix for transitive partial order constraints */
  if (left->kind == qk_constant && right->kind == qk_constant)
    return (qual_set_member(left->u.elt.ubc, right) ||
	    !qual_set_member(left->u.elt.po->qualifiers, right));
  else if (left->kind == qk_variable)
    {
      qual_set_scanner qss;
      qual q;

      scan_qual_set(q, qss, left->u.var.ubc)
	if (leq_qual(q, right))
	  return TRUE;
      return FALSE;
    }
  else
    {
      qual_set_scanner qss;
      qual q;

      assert (right->kind == qk_variable);
      scan_qual_set(q, qss, right->u.var.lbc)
	if (leq_qual(left, q))
	  return TRUE;
      return FALSE;
    }
}
#endif /* #IFDEF BITSET else */

/* left == right, plus cannot distinguish left and right anymore */
bool unify_qual(location loc, qual left, qual right)
{
  bool result;

  left = ecr_qual(left);
  right = ecr_qual(right);
  if (left == right) /* Short circuit to prevent loops in ecr_qual */
    return FALSE;
#ifdef DEBUG
  print_qual_raw(printf, left);
  printf(" %p = ", left);
  print_qual_raw(printf, right);
  printf(" %p\n", right);
#endif

  result = mkeq_qual(loc, left, right);
  if (left->kind == qk_variable && right->kind == qk_variable)
    {
      qual new_ecr, new_link;

      if (right->u.var.preferred ||
	  (left->u.var.num_equiv <= right->u.var.num_equiv))
	{
	  new_ecr = right;
	  new_link = left;
	}
      else
	{
	  new_ecr = left;
	  new_link = right;
	}

#ifdef BITSET
      assert(new_ecr->bounds == new_link->bounds);
#endif
      new_ecr->u.var.num_equiv += new_link->u.var.num_equiv;
      new_ecr->u.var.num_errors += new_link->u.var.num_errors;
      new_ecr->u.var.lb = qual_edge_set_union(new_ecr->u.var.lb,
					      new_link->u.var.lb);
      new_ecr->u.var.ub = qual_edge_set_union(new_ecr->u.var.ub,
					      new_link->u.var.ub);
      new_ecr->u.var.cond_set = cond_set_union(new_ecr->u.var.cond_set,
					       new_link->u.var.cond_set);
      new_link->kind = qk_link;
      new_link->u.link = new_ecr;
    }
  return result;
}

/* Compute q such that left <= q and right <= q */
/*
bool lub_qual(qual left, qual right, qual *lub, location loc)
{
  bool result = FALSE;

  *lub = make_fresh_qvar(dummy_location);
  result = mkleq_qual(left, *lub);
  assert(!result);
  result = mkleq_qual(right, *lub) || result;
  return result;
}
*/

/* Returns TRUE iff left and right are the same.  Does not generate a
   constraint. */
bool eq_qual(qual left, qual right)
{
  return (ecr_qual(left) == ecr_qual(right));
}

/* A total ordering on qualifiers.  Returns 0 if left = right, a value
   <0 if left < right, or a value >0 if left > right */
int cmp_qual(qual left, qual right)
{
  left = ecr_qual(left);
  right = ecr_qual(right);
  return (int) left - (int) right;
}

/* Return TRUE iff q has bound as an originally-specified lower-bound */
bool has_lb_qual(qual q, qual bound)
{
  qual_edge_set_scanner qess;
  qual_edge qe;

  scan_qual_edge_set(qe, qess, lb_qual(q))
    if (eq_qual(bound, qe->qual))
      return TRUE;
  return FALSE;
}

/* Return TRUE iff q has bound as an originally-specified upper-bound */
bool has_ub_qual(qual q, qual bound)
{
  qual_edge_set_scanner qess;
  qual_edge qe;

  scan_qual_edge_set(qe, qess, ub_qual(q))
    if (eq_qual(bound, qe->qual))
      return TRUE;
  return FALSE;
}

/* Return TRUE iff q has bound as an originally-specified bound */
bool has_qual(qual q, qual bound)
{
  return has_lb_qual(q, bound) || has_ub_qual(q, bound);
}

/* Return TRUE iff q has any flow-sensitive qualifier as an
   originally-specified bound. */
bool has_fs_qual(qual q)
{
  qual_edge_set_scanner qess;
  qual_edge qe;

  scan_qual_edge_set(qe, qess, ub_qual(q))
    if (constant_qual(qe->qual) &&
	ecr_qual(qe->qual)->u.elt.po->flow_sensitive)
      return TRUE;

  scan_qual_edge_set(qe, qess, lb_qual(q))
    if (constant_qual(qe->qual) &&
	ecr_qual(qe->qual)->u.elt.po->flow_sensitive)
      return TRUE;

  return FALSE;
}

/**************************************************************************
 *                                                                        *
 * Conditional Constraint Sets                                            *
 *                                                                        *
 **************************************************************************/

/* Add condition to conds.  May add duplicates. */
cond mkcond(cond_dir dir, qual c, qual left, qual right, location loc)
{
  cond cond;

  cond = ralloc(quals_region, struct Condition);
  cond->dir = dir;
  cond->c = c;
  cond->left = left;
  cond->right = right;
  cond->loc = loc;
  cond->triggered = FALSE;
  return cond;
}

/* Adds the conditional constraint l1<=r1 ==> l2<=r2.  This constraint
   is allowed only if at least one of {l1,r1} is a constant. */
bool cond_mkleq_qual(location loc, qual l1, qual r1, qual l2, qual r2)
{
  assert(constant_qual(l1) || constant_qual(r1));
#ifdef DEBUG
  print_qual_raw(printf, l1);
  printf(" %p <= ", l1);
  print_qual_raw(printf, r1);
  printf(" %p  ==>  ", r1);
  print_qual_raw(printf, l2);
  printf(" %p <= ", l2);
  print_qual_raw(printf, r2);
  printf(" %p\n", r2);
#endif

  l1 = ecr_qual(l1);
  r1 = ecr_qual(r1);
  l2 = ecr_qual(l2);
  r2 = ecr_qual(r2);
  if (leq_qual(l1, r1))
    /* Condition is true */
    return mkleq_qual(loc, l2, r2);
  else if (constant_qual(l1) && constant_qual(r1))
    /* Conditional will never be true -- ignore constraint */
    return FALSE;
  else
    {
      /* Condition is not true yet; add constraint to pending set */
      qual v, c;
      cond_dir dir;

      if (constant_qual(l1))
	{ c = l1; v = ecr_qual(r1); dir = c_leq_v; }
      else
	{ c = r1; v = ecr_qual(l1); dir = v_leq_c; }
      assert (constant_qual(c) && variable_qual(v));
      cond_set_insert(quals_region, &v->u.var.cond_set,
		      mkcond(dir, c, l2, r2, loc));
      return FALSE;
    }
}

/* If there are any conditional constraint depending on q, trigger
   any newly satisfied ones */
static bool cond_set_trigger(qual q)
{
  bool result;
  cond_set_scanner css;
  cond cond;

  result = FALSE;
  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  scan_cond_set(cond, css, q->u.var.cond_set)
    if (!cond->triggered)
      {
	switch (cond->dir)
	  {
	  case c_leq_v:
	    cond->triggered = leq_qual(cond->c, q);
	    break;
	  case v_leq_c:
	    cond->triggered = leq_qual(q, cond->c);
	    break;
	  default:
	    fail("Unexpected condition dir %x\n", cond->dir);
	  }
	if (cond->triggered)
	  result = mkleq_qual(cond->loc, cond->left, cond->right) || result;
      }

  return result;
}

/**************************************************************************
 *                                                                        *
 * Print/View Qualifiers                                                  *
 *                                                                        *
 **************************************************************************/

/* Apply f to all possible values of q.  f is applied to NULL in
   the error case. */
void scan_qual_bounds(qual q, qual_traverse_fn f, void *arg)
{
#ifdef BITSET
  qual_set_scanner qs;
  qual p;
  po_set_scanner ss;
  po_info po;

  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  scan_po_set(po, ss, all_pos)
    {
      if (!po->nonprop &&
	  bitset_full_range(q->bounds->bitset, po->first, po->last))
	/* Don't do anything if all quals in the partial order
	   are possible */
	;
      else if (!po->nonprop &&
	       bitset_empty_range(q->bounds->bitset, po->first, po->last))
	/* If none of the quals in the p.o. are possible, it's
	   an error */
	/* XXX: Hack to make look like lattice case */
	scan_qual_set(p, qs, po->qualifiers)
	  f(p, arg);
      else if (!po->nonprop)
	{
	  scan_qual_set(p, qs, po->qualifiers)
	    if (bitset_member(q->bounds->bitset, p->u.elt.index))
	      f(p, arg);
	}
      else
	{
	  assert(po->nonprop);
	  scan_qual_set(p, qs, po->qualifiers)
	    if (has_qual(q, p))
	      f(p, arg);
	}
    }

#else /* #ifdef BITSET */
  qual_set_scanner qs;
  qual p;
  qual_edge_set_scanner qess;
  qual_edge qe;

  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  scan_qual_set(p, qs, q->u.var.lbc)
    if ((sign_qual(p) == sign_pos) || (sign_qual(p) == sign_eq))
      f(p, arg);

  scan_qual_set(p, qs, q->u.var.ubc)
    if (sign_qual(p) == sign_neg)    /* eq handled above */
      f(p, arg);

  /* Handle non-propagating qualifiers */
  scan_qual_edge_set(qe, qess, q->u.var.lb)
    if (constant_qual(qe->qual) &&
	nonprop_qual(qe->qual) &&
	((sign_qual(qe->qual) == sign_pos) ||
	 (sign_qual(qe->qual) == sign_eq)))
      f(qe->qual, arg);

  scan_qual_edge_set(qe, qess, q->u.var.ub)
    if (constant_qual(qe->qual) &&
	nonprop_qual(qe->qual) &&
	sign_qual(qe->qual) == sign_neg) /* eq handled above */
      f(qe->qual, arg);
#endif
}

/* Compare constant qualifiers by name */
int cmp_qual_byname(qual left, qual right)
{
  assert(left->kind == qk_constant && right->kind == qk_constant);
  return strcmp(left->u.elt.name, right->u.elt.name);
}

struct add_info
{
  region r;
  qual_set_byname *b;
};

/* Add q to bounds */
static void add_bound(qual q, struct add_info *ai)
{
  qual_set_byname_insert(ai->r, ai->b, q);
}

/* Apply f to all possible values of q in sorted order. */
void scan_qual_bounds_sorted(qual q, qual_traverse_fn f, void *arg)
{
  region scratch_region;
  qual_set_byname bounds;
  qual_set_byname_scanner qsbn;
  struct add_info ai;
  qual p;

  scratch_region = newregion();
  bounds = empty_qual_set_byname();
  ai.r = scratch_region;
  ai.b = &bounds;
  scan_qual_bounds(q, (qual_traverse_fn) add_bound, &ai);
  qual_set_byname_sort(bounds);
  scan_qual_set_byname(p, qsbn, bounds)
    f(p, arg);
  deleteregion(scratch_region);
}

typedef struct print_info {
  printf_func pf;
  bool first;
  int size;
} *print_info;

void print_qual_fn(qual q, void *arg)
{
  print_info pi = (print_info) arg;

  assert(q->kind == qk_constant);
  if (!pi->first)
    pi->size += pi->pf(" ");
  pi->size += pi->pf("%s", q->u.elt.name);
  pi->first = FALSE;
}

/* Print the qualifiers, nicely */
int print_qual(printf_func pf, qual q)
{
  q = ecr_qual(q);
  switch(q->kind)
    {
    case qk_constant:
      {
	return pf("%s", q->u.elt.name);
      }
    case qk_variable:
      {
	struct print_info pi = {0, TRUE, 0};
	pi.pf = pf;

	scan_qual_bounds_sorted(q, print_qual_fn, &pi);
	return pi.size;
      };
      break;
    default:
      fail("Unexpected kind %d for qualifiers\n", q->kind);
    }
}

/* Print the raw qualifiers */
int print_qual_raw(printf_func pf, qual q)
{
  if (!q)
    fail("Null qualifier in print_qual_raw\n");
  switch(q->kind)
    {
    case qk_constant: return pf("%s", q->u.elt.name);
    case qk_variable: return pf("%s", q->u.var.name);
    case qk_link:     return print_qual_raw(pf, q->u.link);
    default: fail("Unexpected kind %d for qualifier\n", q->kind);
    }
}

/* Returns color we should use for lub of colors 1 and 2 */
const char *combine_colors_pam(const char *color1, const char *color2)
{
  if (!color1) return color2;
  if (!color2) return color1;
  if (strcmp(color1, color2) == 0)
    return color1;
  return pam_color_multiqual;
}

void color_qual_fn(qual q, void *arg)
{
  const char **color = (const char **) arg;

  *color = combine_colors_pam(*color, color_qual(q));
}

/* Return the color of qualifier q.  q may be a constant or a variable */
const char *color_qual(qual q)
{
  q = ecr_qual(q);
  switch (q->kind)
    {
    case qk_constant:
      return q->u.elt.color;
    case qk_variable:
      {
	const char *color = NULL;

	scan_qual_bounds(q, color_qual_fn, &color);
	return color;
    }
      break;
    default:
      fail("Unexpected kind %d for qualifier\n", q->kind);
    }
}

/* Return the name of qualifier q.  q may be a constant or a variable. */
const char *name_qual(qual q)
{
  q = ecr_qual(q);
  switch (q->kind)
    {
    case qk_constant: return q->u.elt.name;
    case qk_variable: return q->u.var.name;
    default:
      fail("Unexpected kind %d for qualifier\n", q->kind);
    }
}

/* Return the unique internal name used to identify q (q's address) */
const char *unique_name_qual(qual q)
{
  q = ecr_qual(q);
  return ptr_to_ascii(q);
}

/**************************************************************************
 *                                                                        *
 * Constraint Graph Traversal                                             *
 *                                                                        *
 **************************************************************************/

/* Called for each bound of the qualifier we're interested in.  arg is
   a pointer to the current dir. */
static void interesting_scan_dir_fn(qual q, void *arg)
{
  dir *d = (dir *) arg;

  switch (sign_qual(q))
    {
    case sign_pos:
      *d |= dir_backward;
      break;
    case sign_neg:
      *d |= dir_forward;
      break;
    case sign_eq:
      *d |= dir_forward | dir_backward;
      break;
    default:
      fail("Unexpected sign %x\n", sign_qual(q));
    }
}

/* Given a qualifier q, return in which directions to look for interesting
   explicit qualifiers.  If c is given, only looks for directions to c. */
static dir interesting_scan_dir(qual q, qual c)
{
  dir result;

  result = dir_none;
  if (!c)
    /* Look for any interesting qualifier */
    scan_qual_bounds(q, interesting_scan_dir_fn, &result);
  else
    {
      /* See if q has c has a bound */
      if (!nonprop_qual(c))
	{
	  switch (sign_qual(c))
	    {
	    case sign_pos:
	      if (leq_qual(c, q))
		result = dir_backward;
	      break;
	    case sign_neg:
	      if (leq_qual(q, c))
		result = dir_forward;
	      break;
	    case sign_eq:
	      if (leq_qual(c, q)) /* Should be eq_qual, but that's something
				     else */
		result = dir_backward | dir_forward;
	      break;
	    default:
	      fail("Unexpected sign %x\n", sign_qual(c));
	    }
	}
      else
	{
	  /* Handle non-propagating qualifiers */
	  qual_edge_set_scanner qess;
	  qual_edge qe;

	  switch (sign_qual(c))
	    {
	    case sign_pos:
	      scan_qual_edge_set(qe, qess, q->u.var.lb)
		if (leq_qual(c, qe->qual))
		  result = dir_backward;
	      break;
	    case sign_neg:
	      scan_qual_edge_set(qe, qess, q->u.var.ub)
		if (leq_qual(qe->qual, c))
		  result = dir_forward;
	      break;
	    case sign_eq:
	      scan_qual_edge_set(qe, qess, q->u.var.lb)
		if (leq_qual(c, qe->qual))
		  result = dir_backward;
	      break;
	    default:
	      fail("Unexpected sign %x\n", sign_qual(c));
	    }
	}
    }
  return result;
}

/* Erase all the marks marked as TRUE reachable via TRUE marked nodes
   from q. */
static void traverse_clear_marks(qual q)
{
  qual_edge qe;
  qual_edge_set_scanner qess;

  q = ecr_qual(q);
  if (!q->mark)
    return;
  q->mark = FALSE;

  if (q->kind == qk_variable)
    {
      scan_qual_edge_set(qe, qess, q->u.var.lb)
	traverse_clear_marks(qe->qual);
      
      scan_qual_edge_set(qe, qess, q->u.var.ub)
	traverse_clear_marks(qe->qual);
    }
  else
    assert(q->kind == qk_constant);
}

/* Traverse the constraint graph starting from qualifier variable q,
   looking for constant qualifier c in direction d.  If such a path
   exists, set the parent pointers and return c.  Otherwise return
   NULL.  If c is NULL, look for any path to a constant qualifier, and
   return that qualifier. */
qual find_shortest_path(region r, qual q, qual c, dir d)
{
  dd_list queue;
  qual path_end;

  q = ecr_qual(q);
  assert(q->kind == qk_variable && (!c || constant_qual(c)));
  queue = dd_new_list(r);
  dd_add_last(r, queue, q);
  q->parent = NULL;                /* Mark as root */
  path_end = NULL;

  while (!dd_is_empty(queue))
    {
      qual q;

      q = DD_GET(qual, dd_first(queue));
      dd_remove(dd_first(queue));
      q = ecr_qual(q);
      if (q->mark)
	continue;

      q->mark = TRUE;

      if (constant_qual(q))
	{
	  if (q == c || !c)
	    {
	      path_end = q;
	      break;
	    }
	  else
	    continue;
	}
      else
	{
	  qual_edge_set_scanner qess;
	  qual_edge qe;
	  dir dir_to_look;

	  assert(q->kind == qk_variable);
	  dir_to_look = interesting_scan_dir(q, c);
	  if (d & dir_to_look & dir_backward)
	    {
	      scan_qual_edge_set(qe, qess, q->u.var.lb)
		{
		  qual b;
		  location loc;

		  b = ecr_qual(qe->qual);
		  loc = qe->loc;
		  if (!b->mark)
		    {
		      /* Dir of edge is opposite of dir of traversal */
		      b->parent = mktraverse_edge(r, q, loc, dir_forward);
		      dd_add_last(r, queue, b);
		    }
		}
	    }

	  if (d & dir_to_look & dir_forward)
	    {
	      scan_qual_edge_set(qe, qess, q->u.var.ub)
		{
		  qual b;
		  location loc;
		      
		  b = ecr_qual(qe->qual);
		  loc = qe->loc;
		  if (!b->mark)
		    {
		      /* Dir of edge is opposite of dir of traversal */
		      b->parent = mktraverse_edge(r, q, loc, dir_backward);
		      dd_add_last(r, queue, b);
		    }
		}
	    }
	}
    }
  
  traverse_clear_marks(q);
  return path_end;
}

/* Traverse parent pointer chain from q */
static void traverse_parent_edges_forward(qual q, edge_traverse_fn f,
					  void *arg)
{
  qual b;

  q = ecr_qual(q);
  for (b = q; b->parent != NULL; b = ecr_qual(b->parent->qual))
    {
      if (b->parent->dir == dir_forward)
	f(b, b->parent->qual, b->parent->loc, arg);
      else
	{
	  assert(b->parent->dir == dir_backward);
	  f(b->parent->qual, b, b->parent->loc, arg);
	}
    }
}

/* Traverse parent pointer chain from q in reverse */
static void traverse_parent_edges_reverse(qual q, edge_traverse_fn f,
					  void *arg) {
  if (q->parent != NULL)
    {
      traverse_parent_edges_reverse(q->parent->qual, f, arg);
      if (q->parent->dir == dir_backward)
	f(q->parent->qual, q, q->parent->loc, arg);
      else
	{
	  assert(q->parent->dir == dir_forward);
	  f(q, q->parent->qual, q->parent->loc, arg);
	}
    }
}

/* Call f(q1, q2, arg) for every edge q1->q2 on a shortest
   unidirectional path from qualifier variable q to constant qualifier
   c.  If c == NULL, finds shortest path to any constant qualifier.
   If there are paths in both directions, traverses both. */
void traverse_shortest_path_edges(qual q, qual c, edge_traverse_fn f,
				  void *arg)
{
  region scratch_region;

  scratch_region = newregion();

  if (!c)
    {
      qual left_end, right_end;

      if ((left_end = find_shortest_path(scratch_region, q, c, dir_backward)))
	traverse_parent_edges_forward(left_end, f, arg);
      
      if ((right_end = find_shortest_path(scratch_region, q, c,
					  dir_forward)) &&
	  (!left_end || !eq_qual(right_end, left_end)))
	traverse_parent_edges_reverse(right_end, f, arg);
    }
  else
    {
      qual end;
      dir d;

      d = interesting_scan_dir(q, c);
      assert(d != dir_none);

      if (d & (dir_forward | dir_backward))
	{
	  /* Could look in both directions.  Try finding a
	     unidirectional path first, since they're easier to understand */
	  if ((end = find_shortest_path(scratch_region, q, c, dir_backward)))
	    ;
	  else if ((end = find_shortest_path(scratch_region, q, c,
					     dir_forward)))
	    ;
	  else
	    end = find_shortest_path(scratch_region, q, c, d);

	  if (end)
	    traverse_parent_edges_forward(end, f, arg);
	}
      else
	{
	  /* Look only in one direction */
	  if ((end = find_shortest_path(scratch_region, q, c, d)))
	    traverse_parent_edges_forward(end, f, arg);
	}
    }

  deleteregion(scratch_region);
}

/* Call f(q', arg) for every node q' on all paths from qualifier q to
   constant qualifier c.  Does nothing if there is no such path. */
void __traverse_qual_graph_nodes(qual q, qual c, qual_traverse_fn f,
			       void *arg)
{
  q = ecr_qual(q);
  assert(constant_qual(c));

  if (q->mark)
    return;
  q->mark = TRUE;

  if (q->kind == qk_variable)
    {
      bool traverse_upper, traverse_lower;
      qual_edge_set_scanner qess;
      qual_edge qe;

      traverse_upper = has_ub_qual(q, c);
      traverse_lower = has_lb_qual(q, c);

      if (traverse_upper || traverse_lower)
	f(q, arg);

      if (traverse_upper)
	scan_qual_edge_set(qe, qess, q->u.var.ub)
	  __traverse_qual_graph_nodes(qe->qual, c, f, arg);

      if (traverse_lower)
	scan_qual_edge_set(qe, qess, q->u.var.lb)
	  __traverse_qual_graph_nodes(qe->qual, c, f, arg);
    }
  else
    {
      assert(q->kind == qk_constant);
      f(q, arg);
    }
}

/* Apply qtf to every *node* in the quals graph reachable in direction
   dir from q. */
static void _traverse_qual_graph_nodes(qual q, dir d,
				       qual_traverse_fn qtf,
				       void *arg)
{
  q = ecr_qual(q);

  if (q->mark)
    return;
  q->mark = TRUE;
  qtf(q, arg);
  if (q->kind == qk_variable)
    {
      qual_edge_set_scanner qess;
      qual_edge qe;

      if (d & dir_backward)
	scan_qual_edge_set(qe, qess, q->u.var.lb)
	  _traverse_qual_graph_nodes(qe->qual, d, qtf, arg);
      if (d & dir_forward)
	scan_qual_edge_set(qe, qess, q->u.var.ub)
	  _traverse_qual_graph_nodes(qe->qual, d, qtf, arg);
    }
  else
    assert(q->kind == qk_constant);
}

/* Traverse the edges in the qualifier graph.  po_bounds_only as above.
   arg is passed to f. */
struct traverse_node_data {
  qual_traverse_fn f;
  void *arg;
  bool po_bounds_only;
};

void traverse_qual_graph_nodes_lower(qual q, void *arg)
{
  struct traverse_node_data *data = (struct traverse_node_data *) arg;
  if (q->kind == qk_constant ||
      !data->po_bounds_only ||
      (interesting_scan_dir(q, NULL) & dir_backward))
    data->f(q, data->arg);
}

void traverse_qual_graph_nodes_upper(qual q, void *arg)
{
  struct traverse_node_data *data = (struct traverse_node_data *) arg;
  if (q->kind == qk_constant ||
      !data->po_bounds_only ||
      (interesting_scan_dir(q, NULL) & dir_forward))
    data->f(q, data->arg);
}

void traverse_qual_graph_nodes_both(qual q, void *arg)
{
  struct traverse_node_data *data = (struct traverse_node_data *) arg;
  if (q->kind == qk_constant ||
      !data->po_bounds_only ||
      (interesting_scan_dir(q, NULL) & (dir_forward | dir_backward)))
    data->f(q, data->arg);
}

void traverse_qual_graph_nodes(qual q, bool po_bounds_only,
			       qual_traverse_fn f, void *arg)
{
  struct traverse_node_data data;

  data.f = f;
  data.po_bounds_only = po_bounds_only;
  data.arg = arg;

  q = ecr_qual(q);
  assert(q->kind == qk_variable);

  if (po_bounds_only &&
      (interesting_scan_dir(q, NULL) == (dir_forward | dir_backward)))
      {
	_traverse_qual_graph_nodes(q,
				   dir_forward | dir_backward,
				   traverse_qual_graph_nodes_both,
				   &data);
	traverse_clear_marks(q);
      }
  else
    {
      _traverse_qual_graph_nodes(q,
				 dir_backward,
				 traverse_qual_graph_nodes_lower,
				 &data);
      traverse_clear_marks(q);
      _traverse_qual_graph_nodes(q,
				 dir_forward,
				 traverse_qual_graph_nodes_upper,
				 &data);
      traverse_clear_marks(q);
    }

}

/* Apply qtf to every *non-transitive edge* in the quals graph
   reachable in direction dir from q. */
static void _traverse_qual_graph_edges(qual q, dir d,
				       edge_traverse_fn qtf,
				       void *arg)
{
  qual_edge_set_scanner qess;
  qual_edge qe;

  q = ecr_qual(q);

  if (q->kind == qk_constant)
    return;
  if (q->mark)
    return;
  q->mark = TRUE;

  if (d & dir_backward)
    {
      scan_qual_edge_set(qe, qess, q->u.var.lb)
	{
	  qtf(qe->qual, q, qe->loc, arg);
	  _traverse_qual_graph_edges(qe->qual, d, qtf, arg);
	}
    }

  if (d & dir_forward)
    {
      scan_qual_edge_set(qe, qess, q->u.var.ub)
	{
	  qtf(q, qe->qual, qe->loc, arg);
	  _traverse_qual_graph_edges(qe->qual, d, qtf, arg);
	}
    }
}

/* Traverse the edges in the qualifier graph.  po_bounds_only as above.
   arg is passed to f. */
struct traverse_edge_data {
  edge_traverse_fn f;
  void *arg;
  bool po_bounds_only;
};

void traverse_qual_graph_edges_lower(qual left, qual right, 
				     location loc, void *arg)
{
  struct traverse_edge_data *data = (struct traverse_edge_data *) arg;
  if (left->kind == qk_constant || !data->po_bounds_only ||
      (interesting_scan_dir(left, NULL) & dir_backward))
    data->f(left, right, loc, data->arg);
}

void traverse_qual_graph_edges_upper(qual left, qual right,
				     location loc, void *arg)
{
  struct traverse_edge_data *data = (struct traverse_edge_data *) arg;
  if (right->kind == qk_constant || !data->po_bounds_only ||
      (interesting_scan_dir(right, NULL) & dir_forward))
    data->f(left, right, loc, data->arg);
}

void traverse_qual_graph_edges_both(qual left, qual right,
				    location loc, void *arg)
{
  struct traverse_edge_data *data = (struct traverse_edge_data *) arg;
  if (left->kind == qk_constant || right->kind == qk_constant ||
      !data->po_bounds_only ||
      interesting_scan_dir(left, NULL) ||
      interesting_scan_dir(right, NULL))
    data->f(left, right, loc, data->arg);
}

void traverse_qual_graph_edges(qual q, bool po_bounds_only,
			       edge_traverse_fn f, void *arg)
{
  struct traverse_edge_data data;

  data.f = f;
  data.po_bounds_only = po_bounds_only;
  data.arg = arg;

  q = ecr_qual(q);
  assert(q->kind == qk_variable);

  if (po_bounds_only &&
      (interesting_scan_dir(q, NULL) == (dir_forward | dir_backward)))
      {
	_traverse_qual_graph_edges(q,
				   dir_forward | dir_backward,
				   traverse_qual_graph_edges_both,
				   &data);
	traverse_clear_marks(q);
      }
  else
    {
      _traverse_qual_graph_edges(q,
				 dir_backward,
				 traverse_qual_graph_edges_lower,
				 &data);
      traverse_clear_marks(q);
      _traverse_qual_graph_edges(q,
				 dir_forward,
				 traverse_qual_graph_edges_upper,
				 &data);
      traverse_clear_marks(q);
    }
}

/* Print the graph of qualifiers for q to file.  If po_bounds_only is
   true, then only print the graph showing how partial order elements
   affect q; otherwise print all qualifiers reachable from q. */
void print_qual_graph_edge(qual left, qual right, location loc, void *arg)
{
  FILE *graph_file = (FILE *) arg;
  fprintf(graph_file, "%s -> %s\n", name_qual(left), name_qual(right));
}

void print_qual_graph(qual q, const char *file, bool po_bounds_only)
{
  FILE *graph_file;

  if (!(graph_file = fopen(file, "w")))
    {
      perror(file);
      return;
    }
  fprintf(graph_file, "digraph G {\n");
  traverse_qual_graph_edges(q, po_bounds_only, print_qual_graph_edge,
			    graph_file);
  fprintf(graph_file, "}\n");
  fclose(graph_file);
}

/**************************************************************************
 *                                                                        *
 * Hotspots                                                               *
 *                                                                        *
 **************************************************************************/

/* Called when q's error count has changed. */
static void update_error_count(qual q)
{
  int i;

  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  if (q->u.var.num_errors == 0)
    return;

  /* See if q is in hotspots list */
  for (i = 0; i < num_hotspots && hotspots[i]; i++)
    if (eq_qual(hotspots[i], q))
      break;

  if (i == num_hotspots)
    {
      /* List is full, but q is not in the list */
      int j;

      /* Find smallest element with greater count than q */
      for (j = num_hotspots - 1; j >= 0; j--)
	if (error_count_qual(hotspots[j]) > error_count_qual(q))
	  break;

      if (j != num_hotspots)
	{
	  /* Move q to position j */
	  int k;

	  for (k = num_hotspots - 1; k > j; k--)
	    hotspots[k] = hotspots[k-1];
	  hotspots[j] = q;
	}
    }
  else if (hotspots[i] != NULL && i != 0)
    {
      /* In the list at position i, and it may need to be moved up
         the list. */
      int j;

      for (j = i; j > 0; j--)
	if (error_count_qual(hotspots[j-1]) < error_count_qual(hotspots[j]))
	  {
	    qual tmp;
	    
	    tmp = hotspots[j-1];
	    hotspots[j-1] = q;
	    hotspots[j] = tmp;
	  }
    }
  else
    /* Empty space on the hotspots list -- just add */
    hotspots[i] = q;
}

/* Increment q's error count and update the hotspots list */
static void increment_error_count(qual q)
{

  if (!hotspots)
    return;

  q = ecr_qual(q);
  assert(q->kind == qk_variable);
  q->u.var.num_errors++;

  update_error_count(q);
}

/* Called during graph traversal of increment_error_counts */
static void increment_error_count_func(qual q, void *arg)
{
  q = ecr_qual(q);
  if (q->kind == qk_variable)
    increment_error_count(q);
}

/* Called when an error occurs for the constraint left <= right */
static void increment_error_counts(qual left, qual right)
{
  if (num_hotspots == 0)
    return;

  left = ecr_qual(left);
  right = ecr_qual(right);

  if (left->kind == qk_constant && right->kind == qk_constant)
    ;
  else if (left->kind == qk_variable)
    traverse_qual_graph_nodes(left, TRUE, increment_error_count_func, NULL);
  else
    {
      assert(right->kind == qk_variable);
      traverse_qual_graph_nodes(right, TRUE, increment_error_count_func, NULL);
    }
}

void print_qual_edges(qual q)
{
  qual_edge_set_scanner qes;
  qual_edge qe;

  scan_qual_edge_set(qe, qes, lb_qual(q))
    printf("%s %p <= %s %p\n", name_qual(qe->qual), ecr_qual(qe->qual),
	   name_qual(q), ecr_qual(q));
  scan_qual_edge_set(qe, qes, ub_qual(q))
    printf("%s %p <= %s %p\n", name_qual(q), ecr_qual(q),
	   name_qual(qe->qual), ecr_qual(qe->qual));
}
