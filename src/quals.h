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

#ifndef QUALS_H
#define QUALS_H

#include <location.h>
#include "linkage.h"
#include "cqual.h"
#include "containers.h"
#include "buffer.h"
#include "regions.h"

/**************************************************************************
 *                                                                        *
 * Types and globals                                                      *
 *                                                                        *
 **************************************************************************/

EXTERN_C_BEGIN

/* True if we should print out the qualifier constraint graph in dot
   form. */
extern int flag_print_quals_graph;

typedef enum { level_value, level_ref } level_qual_t;
typedef enum { sign_pos, sign_neg, sign_eq } sign_qual_t;

/* C qualifiers */
extern qual const_qual;
extern qual nonconst_qual;
extern qual volatile_qual;
extern qual restrict_qual;
extern qual noreturn_qual; /* __attribute__((noreturn)) */
extern qual init_qual;     /* __init */
extern qual noninit_qual;

/* Set to TRUE if after end_define_pos() is called there are any
   calls to find_qual to look up a flow-sensitive qualifier. */
extern bool found_fs_qual;

struct Qual_edge {
  location loc;
  qual qual;
};

typedef void (*qual_traverse_fn)(qual q, void *arg);
typedef void (*edge_traverse_fn)(qual left, qual right, location loc,
				 void *arg);

/**************************************************************************
 *                                                                        *
 * State transitions                                                      *
 *                                                                        *
 * These functions are in general called in the order listed in this      *
 * file.                                                                  *
 *                                                                        *
 **************************************************************************/

void init_quals(void);        /* Called before any other functions in
				 quals.h are called. */
void end_define_pos(void);    /* Called when no more partial orders will
				 be defined.  You *must* call this function
			         before creating any qualifier variables. */
void finish_quals(void);      /* Called when no more constraints will
				 be generated. */
void dispose_quals(void);     /* Disposes of all memory internally
			         allocated by functions in quals.c.
			         All qualifiers and qualifier
			         variables become invalid after
			         calling this function. */

/**************************************************************************
 *                                                                        *
 * Partial order elements                                                 *
 *                                                                        *
 **************************************************************************/

/* Begin and end defining a partial order.  All the qualifiers created
   between calls to begin and end are exclusively related. */
void begin_po_qual(void);
void end_po_qual(void);

void set_po_flow_sensitive(void);         /* Marks the current partial order
					     as flow-sensitive */
void set_po_nonprop(void);                /* Marks the current partial order
					     as non-propagating */

qual add_qual(const char *name);          /* Make a new (constant) qualifier */
void add_qual_lt(qual left, qual right);  /* Add assumption left < right */
void add_color_qual(qual q, const char *color); /* Add color for qualifier */
void add_level_qual(qual q, level_qual_t lev);
                                          /* Assert that q qualifies lev */
void add_sign_qual(qual q, sign_qual_t sign);
                                          /* Assert that q has variance sign */

qual find_qual(const char *name);        /* Look up a qualifier */
level_qual_t level_qual(qual q);         /* Return the level of qualifier q */
sign_qual_t sign_qual(qual q);           /* Return the sign of qualifier q */
bool flow_sensitive_qual(qual q);        /* Return TRUE iff q is flow-
					    sensitive */
bool nonprop_qual(qual q);               /* Return TRUE iff q is
					    non-propagating */
bool variable_qual(qual q);              /* Return TRUE iff q is a variable */
bool constant_qual(qual q);              /* Return TRUE iff q is a constant */

/**************************************************************************
 *                                                                        *
 * Qualifier variables                                                    *
 *                                                                        *
 **************************************************************************/

/* Make a new, distinct qualifier variable */
qual make_qvar(const char *name, location loc, bool preferred);

/* Make a fresh qualifier variable with a unique name */
qual make_fresh_qvar(const char *base_name, location loc);

/* Associate (s, al) with q; return TRUE if this triggers a propagation */
bool store_aloc_qual(qual q, store s, aloc al);

/* Check if qualifier flows into this qual at cast */
void mk_no_qual_qual(location loc, qual q);

bool preferred_qual(qual q);         /* Return TRUE iff q is preferred */
location location_qual(qual q);      /* Where q came from */
qual_edge_set lb_qual(qual q);       /* Lower-bounds (generated) of q */
qual_edge_set ub_qual(qual q);       /* Upper-bounds (generated) of q */
int error_count_qual(qual q);        /* Number of error paths q is
					involved in */

/* Apply f to each possible value of q */
void scan_qual_bounds(qual q, qual_traverse_fn f, void *arg);

/* Apply f to each possible value of q in sorted order */
void scan_qual_bounds_sorted(qual q, qual_traverse_fn f, void *arg);

/**************************************************************************
 *                                                                        *
 * Generate/solve constraints                                             *
 *                                                                        *
 **************************************************************************/

bool mkleq_qual(location loc, qual left, qual right);
bool mkeq_qual(location loc, qual left, qual right);
bool unify_qual(location loc, qual left, qual right);

/* Adds the conditional constraint l1<=r1 ==> l2<=r2.  This constraint
   is allowed only if at least one of {l1,r1} is a constant. */
bool cond_mkleq_qual(location loc, qual l1, qual r1, qual l2, qual r2);

/**************************************************************************
 *                                                                        *
 * Queries (no generated constraints)                                     *
 *                                                                        *
 **************************************************************************/

bool eq_qual(qual left, qual right);     /* Returns TRUE iff left and
					    right are the same.  Does
					    not generate a constraint. */
bool leq_qual(qual left, qual right);    /* Returns TRUE iff left <=
					    right.  Requires that at
					    least one of left, right
					    is a constant.  By defn,
					    returns TRUE if left and
					    right are in distinct
					    partial orders. */
int cmp_qual(qual left, qual right);     /* Total ordering on qualifiers */

/* Return TRUE iff q has bound as an originally-specified lower-bound */
bool has_lb_qual(qual q, qual bound);

/* Return TRUE iff q has bound as an originally-specified upper-bound */
bool has_ub_qual(qual q, qual bound);

/* Return TRUE iff q has bound as an originally-specified bound */
bool has_qual(qual q, qual bound);

/* Return TRUE iff q has any flow-sensitive qualifier as an
   originally-specified bound. */
bool has_fs_qual(qual q);

typedef int (*pr_qual_fn)(printf_func, qual);
int print_qual(printf_func, qual);       /* Print a qualifier, nice. */
int print_qual_raw(printf_func, qual);   /* Print a qualifier, ugly */
            /* Both functions return the number of characters printed. */
const char *name_qual(qual q);           /* Return the name of
					    qualifier q.  q may be a
					    constant or a variable. */
const char *unique_name_qual(qual q);    /* Return the unique internal
					    name used to identify q
					    (q's address) */
const char *color_qual(qual q);          /* Return the color of
					    qualifier q.  q may be a
					    constant or a variable. */

/* Traverse the qualifier graph */

/* Print the graph of qualifiers for q to file.  If po_bounds_only is
   true, then only print the graph showing how partial order elements
   affect q; otherwise print all qualifiers reachable from q. */
void print_qual_graph(qual q, const char *file, bool po_bounds_only);

/* Call f(q1, q2, arg) for every edge q1->q2 on a shortest
   unidirectional path from qualifier variable q to constant qualifier
   c.  If c == NULL, finds shortest path to any constant qualifier.
   If there are paths in both directions, traverses both. */
void traverse_shortest_path_edges(qual q, qual c, edge_traverse_fn f,
				  void *arg);

extern int num_hotspots;   /* Number of hotspots to track */
extern qual *hotspots;     /* Array of num_hotspots qualifiers
                              involved in error paths */


/* Hacks */

/* Change flow-sensitive quals from nonprop to regular quals for this pass */
void reset_flow_sensitive_quals(void);

EXTERN_C_END

#endif
