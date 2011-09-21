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

#ifndef QTYPE_H
#define QTYPE_H

#include "AST.h"
#include "cqual.h"
#include "quals.h"
#include "effect.h"
#include "linkage.h"

EXTERN_C_BEGIN

typedef struct Qtypelist *qtypelist;
typedef qtypelist qtypelist_scanner;
typedef void *field_scanner;

void init_qtype(void);

/**************************************************************************
 *                                                                        *
 * qtypelists                                                             *
 *                                                                        *
 **************************************************************************/

void qtypelist_scan(qtypelist l, qtypelist_scanner *); /* Begin scanning */
qtype qtypelist_next(qtypelist_scanner *); /* continue scanning */

/**************************************************************************
 *                                                                        *
 * Predicates                                                             *
 *                                                                        *
 **************************************************************************/

bool qtype_error(qtype qt);
bool qtype_primitive(qtype qt);
bool qtype_char(qtype qt);
bool qtype_signed_char(qtype qt);
bool qtype_unsigned_char(qtype qt);
bool qtype_int(qtype qt);
bool qtype_unsigned_int(qtype qt);
bool qtype_float(qtype qt);
bool qtype_complex(qtype qt);
bool qtype_aggregate(qtype qt);
bool qtype_struct(qtype qt);
bool qtype_union(qtype qt);
bool qtype_transparent(qtype qt);
bool qtype_void(qtype qt);
bool qtype_pointer(qtype qt);
bool qtype_pointer_loc(qtype qt);
bool qtype_pointer_or_loc(qtype qt);
bool qtype_function(qtype qt);
bool qtype_varargs(qtype qt);
bool qtype_oldstyle(qtype qt);
bool qtype_array(qtype qt);
bool qtype_unsigned(qtype qt);
bool qtype_smallerthanint(qtype qt);
bool qtype_integral(qtype qt); /* Does not include enums */
bool qtype_string(qtype qt);
bool qtype_var(qtype qt);
bool qtype_scheme(qtype qt);

bool qtype_void_pointer(qtype qt);
bool qtype_char_pointer(qtype qt);

/**************************************************************************
 *                                                                        *
 * Constructors                                                           *
 *                                                                        *
 **************************************************************************/

extern qtype error_qtype;
qtype mkqtype_void(qual);
qtype mkqtype_bool(qual);
qtype mkqtype_char(qual);
qtype mkqtype_int(qual);
qtype mkqtype_unsigned_int(qual);
qtype mkqtype_long(qual);
qtype mkqtype_unsigned_long(qual);
qtype mkqtype_long_long(qual);
qtype mkqtype_unsigned_long_long(qual);
qtype mkqtype_float(qual);
qtype mkqtype_double(qual);
qtype mkqtype_size_t(qual);
qtype mkqtype_ptrdiff_t(qual);
qtype mkqtype_pointer(qual, qtype, aloc);
qtype mkqtype_pointer_loc(qual, aloc);
qtype mkqtype_function(qual q, bool varargs, qual vquals, bool oldstyle,
		       qtype ret, qtypelist args, store store_in,
		       store store_out, aloc al, effect eff);
qtype mkqtype_var(qual q, const char *name);
qtype mkqtype_fresh(void); /* Make a fresh, uniquely named qtype variable */

/**************************************************************************
 *                                                                        *
 * Accessors                                                              *
 *                                                                        *
 **************************************************************************/

/* Operations on qualified types */
qtype complex_base_qtype(qtype qt);    /* Return the base type of a complex
				          qtype */
qtype points_to_qtype(qtype qt);       /* Remove one level of ptr from
                                          a qtype */
aloc  aloc_qtype(qtype qt);            /* Return the abstract location of a
					  pointer/array/fn qtype */
qtype array_of_qtype(qtype qt);        /* Remove one level of array from
                                          a qtype */
expression array_size_qtype(qtype qt); /* Get the size of the array */
aloc array_aloc_qtype(qtype qt);       /* Return the abstract location of
					  an array (not its contents) */
qtype return_qtype(qtype qt);          /* Get return type of function
                                          qtype */
qtypelist arg_qtypes(qtype qt);        /* Get argument types of
                                          function qtype */
effect effect_qtype(qtype qt);         /* Get effect of a function
                                          qtype */
qual vqual_qtype(qtype qt);            /* Get varargs qualifier, if
                                          any */
store store_in_qtype(qtype qt);        /* Get initial store */
store store_out_qtype(qtype qt);       /* Get final store */

const char *tag_name_qtype(qtype qt);  /* The name of a struct or
                                          union (if any) */

/* Get the type of a field of an aggregate.  Returns NULL if no such
   field exists. */
qtype field_qtype(qtype qt, cstring field);

/* Iterate over the fields of an aggregate.  Returns NULL if no such
   field exists. */
void field_scan(qtype qt, field_scanner *);

/* Iterate over the fields of an aggregate, starting at field */
void field_scan_at(qtype qt, cstring field, field_scanner *);     

/* Iterate to the next field */
qtype field_next(field_scanner *);

/* Get the top-level qualifier of qt */
qual qual_qtype(qtype qt);

 /* TRUE iff qt's top-level qualifier has bound as an originally-
    specified qualifier */
bool has_qual_qtype(qtype qt, qual bound);

/* Return an effect containing all the locations in qt */
effect alocs_qtype(qtype qt);

/* Return an effect containing all the locations in qt as rwr effects */
effect rwr_alocs_qtype(qtype qt);

/* The effect of defining a variable of type qt */
effect defn_effect_qtype(qtype qt);

/* The effect \cup k(al) for all al in qt */ 
effect constr_effect_qtype(qtype qt, eff_kind k);

/**************************************************************************
 *                                                                        *
 * Qtype manipulation                                                     *
 *                                                                        *
 **************************************************************************/

/* Given a data declaration, assign it a qualified type if it doesn't
   have one already.  Return true iff it didn't already have a
   qualified type.  If generalize is true, make a polymorphic type
   if possible.  Otherwise always make a monomorphic type. */
bool add_ddecl_qtype(data_declaration ddecl, bool generalize);

/* Convert a field declaration to a qtype */
qtype get_fdecl_qtype(const char *tag_name, field_declaration fdecl);

/* Make a qtype from a regular type. */
qtype type_to_qtype(type, const char *, location loc);

/* Copy qt, making fresh qualifiers everywhere. */
qtype copy_qtype(qtype qt, location loc);

/* Turn qt into a type scheme, generalizing over all qualifier
   variables in qt.  No checking is done that this is sound. */
qtype generalize_qtype(qtype qt);

/* Make a copy of qt, making fresh qualifiers everywhere, but also
   copying the constraints.  All variables in reachable constraints
   are also copied. */
qtype instantiate_qtype(qtype qt, location loc);

/* Return TRUE iff qt1 and qt2 have the same shape.  If match_quals is
   not NULL, calls match_quals on every pair of corresponding
   qualifiers (q1 from qt1, q2 from qt2), with extra arg. */
bool match_qtype(qtype qt1, qtype qt2,
		 void (*match_quals)(qual q1, qual q2, void *arg), void *arg);

/* Return TRUE iff t1 and t2 match exactly, i.e., if converting t1 and
   t2 to qtypes would yield exactly the same qtype */
bool match_type(type t1, type t2);

/**************************************************************************
 *                                                                        *
 * Constraint generation                                                  *
 *                                                                        *
 * These functions return FALSE if adding the constraint was              *
 * successful (there was no error) and TRUE if adding the constraint      *
 * was unsuccessful (there was an inconsistency).                         *
 *                                                                        *
 **************************************************************************/

/* qt1 <= qt2 */
bool mkleq_qtype(location loc, qtype qt1, qtype qt2);

/* qt1 = qt2 */
bool mkeq_qtype(location loc, qtype qt1, qtype qt2);

/* qt1 = qt2, plus cannot distinguish qt1 and qt2 afterwards */
bool unify_qtype(location loc, qtype qt1, qtype qt2);

/* Equate the qualifiers on types qt1 and qt2.  For the levels on
   which qt1 and qt2 have the same shape, the qualifiers are equated
   level by level.  After that point, all qualifiers on the remains
   levels are equated. */
bool mkeq_qtype_cast(location loc, qtype qt1, qtype qt2);

/* Make fresh qtype lub and have qt1 <= lub, qt2 <= lub. */
bool lub_qtype(location loc, qtype qt1, qtype qt2, qtype *lub);

/* Return TRUE iff left and right are the same.  Does not generate
   constraints. */
bool eq_qtype(qtype qt1, qtype qt2);

/* Add constraints bewteen all qualifiers of qt and the varargs
   qualifier q.  For each qualifier q' of qt, a copy of q is made
   sharing q's constraints, and then the copy is equated to q'.
   Returns TRUE if an error occurs. */
bool varargs_constrain_quals_qtype(location loc, qtype qt, qual q);

/* Add constraint e \not\in qt */
void mknin_effect_qtype(location loc, effect e, qtype qt);

/**************************************************************************
 *                                                                        *
 * Conversions                                                            *
 *                                                                        *
 **************************************************************************/

/* Convert the qtype as appropriate for appearing in a generic expr
   context.  The logic is duplicated from types.c. */
qtype default_conversion_qtype(qtype qt);

/* rhs_qtype <= lhs_qtype, plus any necessary conversions */
bool mkleq_assign_convert(location loc, qtype rhs_qtype, qtype lhs_qtype);

/**************************************************************************
 *                                                                        *
 * Printing/Output                                                        *
 *                                                                        *
 **************************************************************************/

/* Print a qualified type */
int print_qtype(printf_func, qtype, store);
int print_qtype_raw(printf_func, qtype, store);
int print_qtype_qf(printf_func, pr_qual_fn, qtype, store, bool);

/* Return the color markup for a qualified type */
const char *color_qtype(qtype, store);

/* TRUE iff at least one of qt's qualifiers has a non-trivial
   solution */
bool nontriv_soln_qtype(qtype qt, store s);

/**************************************************************************
 *                                                                        *
 * Second Pass                                                            *
 *                                                                        *
 **************************************************************************/

/* Create a ref() store corresponding to allocating qt in store in */
store ref_qtype_store(location loc, qtype qt, store in);

/**************************************************************************
 *                                                                        *
 * Third Pass                                                             *
 *                                                                        *
 **************************************************************************/

/* Convert qt to a flow-sensitive qtype.  Transfer any constant
   flow-sensitive qualifiers over. */
qtype qtype_to_fs_qtype_with_quals(location loc, qtype qt, store s);

/* Convert qt to a flow-sensitive qtype, generating fresh qualifiers */
qtype qtype_to_fs_qtype(location loc, qtype qt);

/* Associate (s, al) with all the quals in qt; return TRUE if this triggers
   a propagation */
bool store_aloc_qtype(qtype qt, store s, aloc al);

/* TRUE iff this qtype contains a qualifier that can be passed by
   a weak update */
bool toplvl_qual_fs_qtype(qtype qt);

/* Check no qualifiers flows into a fs qtype qt */
void mk_no_qual_qtype_fs(location loc, qtype qt, store s);

/* Return the loc qt points to in s */
qtype points_to_fs_qtype(qtype qt, store s);

/* Same as default_conversion_qtype, but for flow-sensitive qtypes */
qtype default_conversion_fs_qtype(qtype fs_qt);

/* qt1 <= qt2.  Right now s is only used in printing error
   messages. */
bool mkleq_fs_qtype(location loc, qtype qt1, qtype qt2, store s);

/* rhs_qtype <= lhs_qtype, plus any necessary conversions */
bool mkleq_fs_assign_convert(location loc, qtype rhs_qtype, qtype lhs_qtype,
			     store s);

/* Assign rhs_qtype to lhs_qtype.  lhs_qype should be an l-type, and
   rhs_qtype should be an r-type.  s is the current store, and strong
   is TRUE if this must be a strong update.  Returns the store after
   the assignment. */
store assign_flow_sensitive(location loc, const char *err_msg,
			    qtype rhs_qtype, qtype lhs_qtype,
			    store s, bool strong);

EXTERN_C_END

#endif
