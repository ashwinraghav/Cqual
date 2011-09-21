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

#include <ctype.h>
#include "parser.h"
#include "c-parse.h"
#include "types.h"
#include "color.h"
#include "cqual.h"
#include "analyze.h"
#include "qerror.h"
#include "qtype.h"
#include "quals.h"
#include "effect.h"
#include "containers.h"
#include "utils.h"
#include "hash.h"

/*#define DEBUG*/  /* Enable printing of generated constraints */

/**************************************************************************
 *                                                                        *
 * Type declarations                                                      *
 *                                                                        *
 **************************************************************************/

struct Qtypelist {
  qtype qt;
  struct Qtypelist *next;
};

typedef struct Field {
  const char *name;
  qtype qtype;
  struct Field *next;
} *field;

/* We basically reuse the type structure from the parser.  We use a two
   level design, with the type structure explicitly factored out, to make
   variables that range over types possible. */
struct Ty
{
  enum { tk_primitive, tk_complex, tk_struct, tk_union, tk_error, tk_void,
	 tk_pointer, tk_pointer_loc, tk_function, tk_array, tk_var,
	 tk_scheme, tk_link } kind;
  effect *alocs;      /* An array of effects containing the alocs in
			 this qtype */

  union {
    /* tk_primtive and tk_complex.
       The order reflects promotion order (for an arbitrary machine,
       see common_primitive_type) */
    enum { tp_char, tp_signed_char, tp_unsigned_char,
	   tp_short, tp_unsigned_short,
	   tp_int, tp_unsigned_int,
	   tp_long, tp_unsigned_long,
	   tp_long_long, tp_unsigned_long_long,
	   tp_first_floating,
	   tp_float = tp_first_floating, tp_double, tp_long_double
    } primitive;

    /* tk_struct and tk_union */
    struct {
      const char *name;
      bool mark:1;              /* For walking over recursive types */
      bool defined:1;           /* TRUE iff fields have been declared */
      bool transparent_union:1;
      field fields;
      int num_equiv;
      ty fs_ty;               /* Flow-sensitive conversion of this
				 structure (if any) */
    } tag;

    /* tk_pointer */
    struct {
      qtype pointsto;
      aloc aloc; /* For restrict */
    } ptr;

    /* tk_pointer_loc */
    struct {
      aloc aloc;
    } pointer_loc;

    /* tk_function */
    struct {
      qtype returns;
      qtypelist argtypes;
      bool varargs:1;
      bool oldstyle:1;
      qual vqual;          /* Qualifier on ... */
      store in, out;       /* For flow-sensitive qualifiers */
      aloc aloc;           /* For restrict */
      effect effect;       /* For restrict */
    } fn;

    /* tk_array */
    struct {
      qtype arrayof;
      expression size;
      aloc aloc; /* For restrict */
      aloc addr_aloc; /* Location of the array itself (not its contents) */
    } array;

    /* tk_var */
    struct {
      const char *name;
      ty_set lb, ub;
    } var;

    /* tk_link */
    ty link;  /* Notice the link is a struct Type, not a qtype */

    /* tk_scheme */
    struct {
      int num_inst;    /* Number of instantiations of this type */
      ty_set tyvars;   /* Bound type variables */
      qual_set qvars;  /* Bound qualifier variables */
      qtype body;      /* Type that has been generalized.  By convention,
			  all qualifiers and type variables in the type
			  are considered quantified. */
    } scheme;
  } u;
};

struct Qtype
{
  qual qual;       /* Qualifier */
  ty type;         /* Base type */
  qtype conv;      /* Default conversion */
};


/**************************************************************************
 *                                                                        *
 * Prototypes                                                             *
 *                                                                        *
 **************************************************************************/

static qtype ecr_qtype(qtype);
static field mkfield(const char *name, qtype qt);
bool add_constraints_level(qual target, type_quals quals,
			   user_qual_list user_quals, level_qual_t lev,
			   location loc);
bool make_qvar_name_constrained(const char *name, location loc, bool pref,
				qual *q, user_qual_list uql,
				hash_table name_map);
static effect enforce_param_effects(qtypelist_scanner qte, location loc);
static effect param_qual_effect(qtype qt);

tag_declaration find_tag_declaration(tag_declaration td);
tag_declaration find_tag_declaration_nocompress(tag_declaration td);
void union_tag_declaration(tag_declaration td1, tag_declaration td2);
void union_tag_declaration_forceleftecr(tag_declaration td1,
					tag_declaration td2);
bool equate_all_quals_qtype(location loc, qtype qt, qual q);
bool is_qualifier_constraint(const char *s);
static ty copy_ty(ty t, location loc);
ty substitute_ty(ty t, qual (*f)(qual, void *arg), void *f_arg,
		 ty (*g)(ty, void *arg), void *g_arg);
qtype substitute_qtype(qtype qt, qual (*f)(qual, void *arg), void *f_arg,
		       ty (*g)(ty, void *arg), void *g_arg);
qtypelist substitute_qtypelist(qtypelist qt, qual (*f)(qual, void *arg),
			       void *f_arg, ty (*g)(ty, void *arg),
			       void *g_arg);
bool eq_ty(ty t1, ty t2);
int cmp_ty(ty t1, ty t2);
static ty ecr_ty(ty t);
int print_ty_structure(printf_func pf, ty t, store s);
static qtypelist qtypelist_to_fs_qtypelist(location loc, qtypelist qtl);
static bool transfer_fs_qtypelist(location loc, qtypelist old_qtl,
				  qtypelist new_qtl, store s);

/**************************************************************************
 *                                                                        *
 * Global variables                                                       *
 *                                                                        *
 **************************************************************************/

qtype error_qtype = NULL;
int next_qtype = 0;
static ty void_ty, error_ty, bool_ty, char_ty, signed_char_ty,
  unsigned_char_ty, short_ty, unsigned_short_ty, int_ty, unsigned_int_ty,
  long_ty, unsigned_long_ty, long_long_ty, unsigned_long_long_ty, float_ty,
  double_ty, long_double_ty, size_t_ty, ptrdiff_t_ty;
region scratch_region = NULL;   /* A region that can be periodically flushed */
hash_table name_map = NULL; /* Map for translating type variables.  Just
			       reset instead of reallocating all over the
			       place. */

#define mkty_primitive(typ) \
  ({ ty new_ty = ralloc(parse_region, struct Ty); \
     new_ty->kind = tk_primitive; \
     new_ty->alocs = NULL; \
     new_ty->u.primitive = tp_ ## typ; \
     new_ty; })

void init_qtype(void)
{
  scratch_region = newregion();
  name_map = make_string_hash_table(scratch_region, 10);

  error_ty = ralloc(parse_region, struct Ty);
  error_ty->kind = tk_error;
  error_ty->alocs = NULL;

  error_qtype = ralloc(parse_region, struct Qtype);
  error_qtype->qual = NULL;
  error_qtype->type = error_ty;
  error_qtype->conv = NULL;

  void_ty = ralloc(parse_region, struct Ty);
  void_ty->kind = tk_void;
  void_ty->alocs = NULL;

  bool_ty = mkty_primitive(int);
  char_ty = mkty_primitive(char);
  signed_char_ty = mkty_primitive(signed_char);
  unsigned_char_ty = mkty_primitive(unsigned_char);
  short_ty = mkty_primitive(short);
  unsigned_short_ty = mkty_primitive(unsigned_short);
  int_ty = mkty_primitive(int);
  unsigned_int_ty = mkty_primitive(unsigned_int);
  long_ty = mkty_primitive(long);
  unsigned_long_ty = mkty_primitive(unsigned_long);
  long_long_ty = mkty_primitive(long_long);
  unsigned_long_long_ty = mkty_primitive(unsigned_long_long);
  float_ty = mkty_primitive(float);
  double_ty = mkty_primitive(double);
  long_double_ty = mkty_primitive(long_double);
  size_t_ty = mkty_primitive(unsigned_int);
  ptrdiff_t_ty = mkty_primitive(int);
  next_qtype = 0;
}

/**************************************************************************
 *                                                                        *
 * Predicates                                                             *
 *                                                                        *
 **************************************************************************/

bool qtype_error(qtype qt) {
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_error;
}
bool qtype_primitive(qtype qt) {
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_primitive;
}
bool qtype_char(qtype qt)
{
  qt = ecr_qtype(qt);
  return qtype_primitive(qt) && qt->type->u.primitive == tp_char;
}
bool qtype_signed_char(qtype qt)
{
  qt = ecr_qtype(qt);
  return qtype_primitive(qt) && qt->type->u.primitive == tp_signed_char;
}
bool qtype_unsigned_char(qtype qt)
{
  qt = ecr_qtype(qt);
  return qtype_primitive(qt) &&	qt->type->u.primitive == tp_unsigned_char;
}
bool qtype_int(qtype qt)
{
  qt = ecr_qtype(qt);
  return qtype_primitive(qt) &&	qt->type->u.primitive == tp_int;
}
bool qtype_unsigned_int(qtype qt)
{
  qt = ecr_qtype(qt);
  return qtype_primitive(qt) &&	qt->type->u.primitive == tp_unsigned_int;
}
bool qtype_float(qtype qt)
{
  qt = ecr_qtype(qt);
  return qtype_primitive(qt) && qt->type->u.primitive == tp_float;
}
bool qtype_complex(qtype qt)  {
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_complex;
}
bool qtype_aggregate(qtype qt) {
  qt = ecr_qtype(qt);
  return qtype_struct(qt) || qtype_union(qt);
}
bool qtype_struct(qtype qt)
{
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_struct;
}
bool qtype_union(qtype qt)
{
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_union;
}
bool qtype_transparent(qtype qt)
{
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_union && qt->type->u.tag.transparent_union;
}
bool qtype_void(qtype qt)
{
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_void;
}
bool qtype_pointer(qtype qt)  {
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_pointer;
}
bool qtype_pointer_loc(qtype qt) {
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_pointer_loc;
}
bool qtype_pointer_or_loc(qtype qt) {
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_pointer || qt->type->kind == tk_pointer_loc;
}
bool qtype_function(qtype qt) {
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_function;
}
bool qtype_varargs(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_function);
  return qt->type->u.fn.varargs;
}
bool qtype_oldstyle(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_function);
  return qt->type->u.fn.oldstyle;
}
bool qtype_array(qtype qt)    {
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_array;
}
bool qtype_unsigned(qtype qt)
{
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_primitive &&
    (qt->type->u.primitive == tp_unsigned_char  ||
     qt->type->u.primitive == tp_unsigned_short ||
     qt->type->u.primitive == tp_unsigned_int   ||
     qt->type->u.primitive == tp_unsigned_long  ||
     qt->type->u.primitive == tp_unsigned_long_long);
}
bool qtype_smallerthanint(qtype qt)
{
  qt = ecr_qtype(qt);
  return qt->type->kind == tk_primitive && qt->type->u.primitive < tp_int;
}
bool qtype_integral(qtype qt)
{
  qt = ecr_qtype(qt);
  return (qt->type->kind == tk_primitive &&
	  qt->type->u.primitive < tp_first_floating);
}
bool qtype_string(qtype qt)
{
  qtype array_of;

  qt = ecr_qtype(qt);
  if (!qtype_array(qt))
    return FALSE;
  array_of = array_of_qtype(qt);
  return (qtype_char(array_of) ||
	  qtype_unsigned_char(array_of) ||
	  qtype_signed_char(array_of));
}

bool qtype_var(qtype qt)
{
  qt = ecr_qtype(qt);
  return (qt->type->kind == tk_var);
}

bool qtype_scheme(qtype qt)
{
  /* Shouldn't need to do ecr:  type schemes can't be unified. */
  return qt->type->kind == tk_scheme;
}


bool qtype_void_pointer(qtype qt)
{
  return qtype_pointer(qt) && qtype_void(points_to_qtype(qt));
}
bool qtype_char_pointer(qtype qt)
{
  return qtype_pointer(qt) && qtype_char(points_to_qtype(qt));
}
bool qtype_non_void_pointer(qtype qt)
{
  return qtype_pointer(qt) && !qtype_void(points_to_qtype(qt));
}

bool qtype_void_pointer_loc(qtype qt)
{
  return (qtype_pointer_loc(qt) &&
	  qtype_void(points_to_aloc(qt->type->u.pointer_loc.aloc)));
}
bool qtype_char_pointer_loc(qtype qt)
{
  return (qtype_pointer_loc(qt) &&
	  qtype_char(points_to_aloc(qt->type->u.pointer_loc.aloc)));
}
bool qtype_non_void_pointer_loc(qtype qt)
{
  return (qtype_pointer_loc(qt) &&
	  !qtype_void(points_to_aloc(qt->type->u.pointer_loc.aloc)));
}

/**************************************************************************
 *                                                                        *
 * Constructors                                                           *
 *                                                                        *
 **************************************************************************/

qtype mkqtype_void(qual q)
{
  qtype new_qtype = ralloc(parse_region, struct Qtype);
  new_qtype->qual = q;
  new_qtype->type = void_ty;
  new_qtype->conv = NULL;
  return new_qtype;
}

#define mkqtype(q, typ) \
{ \
  qtype new_qtype = ralloc(parse_region, struct Qtype); \
  new_qtype->qual = q; \
  new_qtype->type = typ ## _ty; \
  new_qtype->conv = NULL; \
  return new_qtype; \
}

qtype mkqtype_bool(qual q)               { mkqtype(q, int); }
qtype mkqtype_char(qual q)               { mkqtype(q, char); }
qtype mkqtype_int(qual q)                { mkqtype(q, int); }
qtype mkqtype_unsigned_int(qual q)       { mkqtype(q, unsigned_int); }
qtype mkqtype_long(qual q)               { mkqtype(q, long); }
qtype mkqtype_unsigned_long(qual q)      { mkqtype(q, unsigned_long); }
qtype mkqtype_long_long(qual q)          { mkqtype(q, long_long); }
qtype mkqtype_unsigned_long_long(qual q) { mkqtype(q, unsigned_long_long); }
qtype mkqtype_float(qual q)              { mkqtype(q, float); }
qtype mkqtype_double(qual q)             { mkqtype(q, double); }
qtype mkqtype_size_t(qual q)             { mkqtype(q, unsigned_int); }
qtype mkqtype_ptrdiff_t(qual q)          { mkqtype(q, int); }

static ty mkty_tagged(int kind, const char *name, bool defined,
		      bool transparent_union, field fields)
{
  ty new_ty = ralloc(parse_region, struct Ty);
  assert(kind == tk_struct || kind == tk_union);
  new_ty->kind = kind;
  new_ty->alocs = NULL;
  new_ty->u.tag.name = name;
  new_ty->u.tag.mark = FALSE;
  new_ty->u.tag.defined = defined;
  new_ty->u.tag.transparent_union = transparent_union;
  new_ty->u.tag.fields = fields;
  new_ty->u.tag.num_equiv = 1;
  return new_ty;
}

/* This function should probably not be used outside of qtype.c */
static qtype mkqtype_tagged(qual q, int kind, const char *name, bool defined,
			    bool transparent_union, field fields)
{
  qtype new_qtype = ralloc(parse_region, struct Qtype);
  new_qtype->qual = q;
  new_qtype->type = mkty_tagged(kind, name, defined, transparent_union,
				fields);
  new_qtype->conv = NULL;
  return new_qtype;
}
static ty mkty_pointer(qtype pointsto, aloc al)
{
  ty new_ty = ralloc(parse_region, struct Ty);
  new_ty->kind = tk_pointer;
  new_ty->alocs = NULL;
  new_ty->u.ptr.pointsto = pointsto;
  new_ty->u.ptr.aloc = al;
  return new_ty;
}

qtype mkqtype_pointer(qual q, qtype pointsto, aloc al)
{
  qtype new_qtype = ralloc(parse_region, struct Qtype);
  new_qtype->qual = q;
  new_qtype->type = mkty_pointer(pointsto, al);
  new_qtype->conv = NULL;
  return new_qtype;
}

static ty mkty_pointer_loc(aloc al)
{
  ty new_ty = ralloc(parse_region, struct Ty);
  new_ty->kind = tk_pointer_loc;
  new_ty->alocs = NULL;
  new_ty->u.pointer_loc.aloc = al;
  return new_ty;
}

qtype mkqtype_pointer_loc(qual q, aloc al)
{
  qtype new_qtype = ralloc(parse_region, struct Qtype);
  new_qtype->qual = q;
  new_qtype->type = mkty_pointer_loc(al);
  new_qtype->conv = NULL;
  return new_qtype;
}

static ty mkty_array(qtype arrayof, expression size, aloc al, aloc addr_aloc)
{
  ty new_ty = ralloc(parse_region, struct Ty);
  new_ty->kind = tk_array;
  new_ty->alocs = NULL;
  new_ty->u.array.arrayof = arrayof;
  new_ty->u.array.size = size;
  new_ty->u.array.aloc = al;
  new_ty->u.array.addr_aloc = addr_aloc;
  return new_ty;
}

qtype mkqtype_array(qual q, qtype arrayof, expression size, aloc al,
		    aloc addr_aloc)
{
  qtype new_qtype = ralloc(parse_region, struct Qtype);
  new_qtype->qual = q;
  new_qtype->type = mkty_array(arrayof, size, al, addr_aloc);
  new_qtype->conv = NULL;
  return new_qtype;
}

static ty mkty_function(bool varargs, qual vqual, bool oldstyle, qtype ret,
			qtypelist args, store in, store out, aloc al,
			effect eff)
{
  ty new_ty = ralloc(parse_region, struct Ty);

  new_ty->kind = tk_function;
  new_ty->alocs = NULL;
  new_ty->u.fn.varargs = varargs;
  new_ty->u.fn.vqual = vqual;
  new_ty->u.fn.oldstyle = oldstyle;
  new_ty->u.fn.returns = ret;
  new_ty->u.fn.argtypes = args;
  new_ty->u.fn.in = in;
  new_ty->u.fn.out = out;
  new_ty->u.fn.aloc = al;
  new_ty->u.fn.effect = eff;
  return new_ty;
}

qtype mkqtype_function(qual q, bool varargs, qual vqual, bool oldstyle,
		       qtype ret, qtypelist args, store in, store out, 
		       aloc al, effect eff)
{
  qtype new_qtype = ralloc(parse_region, struct Qtype);
  new_qtype->qual = q;
  new_qtype->type = mkty_function(varargs, vqual, oldstyle, ret, args,
				  in, out, al, eff);
  new_qtype->conv = NULL;
  return new_qtype;
}

static ty mkty_var(const char *name)
{
  ty new_ty = ralloc(parse_region, struct Ty);
  new_ty->kind = tk_var;
  new_ty->alocs = NULL;
  new_ty->u.var.name = name;
  new_ty->u.var.lb = empty_ty_set();
  new_ty->u.var.ub = empty_ty_set();
  return new_ty;
}

static ty mkty_fresh(void)
{
  const char *name;

  name = rstrcat(parse_region, "'t",
		 inttostr(parse_region, next_qtype++));
  return mkty_var(name);
}

qtype mkqtype_var(qual q, const char *name)
{
  qtype new_qtype = ralloc(parse_region, struct Qtype);
  new_qtype->qual = q;
  new_qtype->type = mkty_var(name);
  new_qtype->conv = NULL;
  return new_qtype;
}

/* Make a fresh, uniquely named qtype variable */
qtype mkqtype_fresh(void)
{
  qual q;
  const char *name;

  q = make_fresh_qvar("q", NULL);
  name = rstrcat(parse_region, "t",
		 inttostr(parse_region, next_qtype++));
  return mkqtype_var(q, name);
}

/* Generalize a qtype over tyvars and qvars.  Right now, does no
   checks. */
static ty mkty_scheme(qtype qt, ty_set tyvars, qual_set qvars)
{
  ty new_ty = ralloc(parse_region, struct Ty);
  new_ty->kind = tk_scheme;
  new_ty->alocs = NULL;
  new_ty->u.scheme.num_inst = 0;
  new_ty->u.scheme.body = qt;
  new_ty->u.scheme.tyvars = tyvars;
  new_ty->u.scheme.qvars = qvars;
  return new_ty;
}

/* Generalize a qtype over tyvars and qvars.  Right now, does no
   checks. */
qtype mkqtype_scheme(qtype qt, ty_set tyvars, qual_set qvars)
{
  qtype new_qtype = ralloc(parse_region, struct Qtype);
  new_qtype->qual = NULL; /* Shouldn't touch this */
  new_qtype->type = mkty_scheme(qt, tyvars, qvars);
  new_qtype->conv = NULL;
  return new_qtype;
}

/**************************************************************************
 *                                                                        *
 * Destructors                                                            *
 *                                                                        *
 **************************************************************************/

static inline ty ty_qtype(qtype qt)
{
  return ecr_ty(qt->type);
}

/* Return the top-level qualifier of qtype qt */
qual qual_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  return qt->qual;
}

/* Return TRUE iff qt's top-level qualifier has bound as an
   originally-specified qualifier */
bool has_qual_qtype(qtype qt, qual bound)
{
  return has_qual(qual_qtype(qt), bound);
}

/* Return an effect containing all the abstract locations in qt as kind k */
static effect alocs_qtype_as_effect(qtype qt, eff_kind k)
{
  effect *alocs;

  if (!restrict_qual && !flag_flow_sensitive)
    return NULL;

  assert (eff_any <= k && k <= eff_last);
  qt = ecr_qtype(qt);
  alocs = qt->type->alocs;
  if (alocs && alocs[k])
    return alocs[k];

  switch (qt->type->kind)
    {
    case tk_primitive:
    case tk_complex:
    case tk_error:
    case tk_void:
    case tk_var:
      return effect_empty;
    default:;
    }

  if (!alocs)
    {
      qt->type->alocs = rarrayalloc(parse_region, eff_last + 1, effect);
      alocs = qt->type->alocs;
    }
  alocs[k] = effect_fresh();

  switch(qt->type->kind)
    {
    case tk_struct:
    case tk_union:
      {
	field f;

	for (f = qt->type->u.tag.fields; f; f = f->next)
	  mkleq_effect(constr_effect_qtype(f->qtype, k), alocs[k]);
      }
      break;
    case tk_pointer:
      mkleq_effect(effect_constr(qt->type->u.ptr.aloc, k), alocs[k]);
      if (k != eff_alloc || qtype_aggregate(qt->type->u.ptr.pointsto))
	mkleq_effect(constr_effect_qtype(qt->type->u.ptr.pointsto, k),
		     alocs[k]);
      break;
    case tk_pointer_loc:
      mkleq_effect(effect_constr(qt->type->u.pointer_loc.aloc, k), 
		   alocs[k]);
      break;
    case tk_function:
      {
	mkleq_effect(effect_constr(qt->type->u.fn.aloc, k), 
		     alocs[k]);
	/* Ignore effects in function parameters/return
	  {
	    qtypelist_scanner qtls;
	    qtype argqtype;

	    mkleq_effect(constr_effect_qtype(qt->type->u.fn.returns, k),
			 alocs[k]);
	    qtypelist_scan(qt->type->u.fn.argtypes, &qtls);
	    while ((argqtype = qtypelist_next(&qtls)))
	      mkleq_effect(constr_effect_qtype(argqtype, k), alocs[k]);
	  }
	*/
      }
      break;
    case tk_array:
      mkleq_effect(effect_constr(qt->type->u.array.aloc, k), 
		   alocs[k]);
      if (k != eff_alloc || qtype_array(qt->type->u.array.arrayof) ||
	  qtype_aggregate(qt->type->u.array.arrayof))
	mkleq_effect(constr_effect_qtype(qt->type->u.array.arrayof, k),
		     alocs[k]);
      break;
    case tk_scheme:
      alocs[k] = constr_effect_qtype(qt->type->u.scheme.body, k);
      break;
    default:
      fail("Unexpected qtype kind %x\n", qt->type->kind);
    }
  return alocs[k];
}

/* Return an effect containing all the locations in qt */
effect alocs_qtype(qtype qt)
{
  return alocs_qtype_as_effect(qt, eff_any);
}

/* Return an effect containing all the locations in qt as rwr effects */
effect rwr_alocs_qtype(qtype qt)
{
  return alocs_qtype_as_effect(qt, eff_rwr);
}

/* Return an effect containing all the locations in qt as r effects */
effect r_alocs_qtype(qtype qt)
{
  return alocs_qtype_as_effect(qt, eff_r);
}

/* Return an effect containing all the locations in qt as wr effects */
effect wr_alocs_qtype(qtype qt)
{
  return alocs_qtype_as_effect(qt, eff_wr);
}

/* The effect of defining a variable of type qt */
effect defn_effect_qtype(qtype qt)
{
  return alocs_qtype_as_effect(qt, eff_alloc);
}

/* The effect \cup k(al) for all al in qt */ 
effect constr_effect_qtype(qtype qt, eff_kind k)
{
  switch (k)
    {
    case eff_any: return alocs_qtype(qt);
    case eff_rwr: return rwr_alocs_qtype(qt);
    case eff_r: return r_alocs_qtype(qt);
    case eff_wr: return wr_alocs_qtype(qt);
    case eff_alloc: return defn_effect_qtype(qt);
    default: fail("Unexpected constr kind %x\n", k);
    }
}

/**************************************************************************
 *                                                                        *
 * Qtypelists and Fields                                                  *
 *                                                                        *
 **************************************************************************/

void qtypelist_append(qtypelist *l, qtype qt)
{
  qtypelist new_elt = ralloc(parse_region, struct Qtypelist);

  new_elt->qt = qt;
  while (*l)
    l = &(*l)->next;
  *l = new_elt;
}

void qtypelist_scan(qtypelist l, qtypelist_scanner *s)
{
  *s = l;
}

qtype qtypelist_next(qtypelist_scanner *s)
{
  qtype qt;

  if (!*s)
    return NULL;
  qt = (*s)->qt;
  *s = (*s)->next;
  return qt;
}

static field mkfield(const char *name, qtype qt)
{
  field f;

  f = ralloc(parse_region, struct Field);
  f->name = name;
  f->qtype = qt;
  f->next = NULL;
  return f;
}

static void field_append(field *fs, field f)
{
  while (*fs)
    fs = &(*fs)->next;
  *fs = f;
}

/**************************************************************************
 *                                                                        *
 * Setting up the AST                                                     *
 *                                                                        *
 * Code to munge the qualifiers and types on the AST into the correct     *
 * form.                                                                  *
 *                                                                        *
 **************************************************************************/

typedef struct ctq_info {
  location loc;
  hash_table name_map;
  bool generalize; /* True if we might generalize this type */
} *ctq_info;

static bool convert_type_qtype(type t, const char *name, bool pref, qtype *qt,
			       ctq_info ctqi);

/* Given a data declaration, assign it a qualified type if it didn't
   have one already.  Since we're reusing the type structure from the
   parser, this means walking over the type structure, making a fresh
   copy, and putting in qualifier variables.  Return TRUE iff it
   didn't already have a qualified type. If generalize is true, make a
   polymorphic type if possible.  Otherwise always make a monomorphic
   type.

   We also add a level of indirection for pointers, so that the type
   system will have some sanity.  This may requires pulling consts
   and volatiles up one level of ptrs.  For example, a const int
   becomes a ptr(const ptr(int)).  See [FFA99].  */
bool add_ddecl_qtype(data_declaration ddecl, bool generalize)
{
  assert(ddecl->kind != decl_typedef);
  if (!ddecl->qtype)
    {
      /*      printf("Adding qualifiers to %s\n", ddecl->name);*/
      type t, lifted_t;
      struct ctq_info ctqi;
      location loc;
      bool preferred;

      t = ddecl->type;
      /* For most types, add one level of indirection to the type to
	 make these ML-style ref types (sensible subtyping!).
	 
	 Since functions and arrays can only be used as rtypes, we only
	 make rtypes for those.  Enumerators (decl_constants) can
	 also only be used as rtypes.  Note that in ANSI C and gcc,
	 structs can be used as lvalues. */
      if (type_function(t) || type_array(t)|| ddecl->kind == decl_constant)
	lifted_t = t;
      else
	lifted_t = make_pointer_type(t);

      /* Pick loc and preferred flags */
      if (ddecl->definition)
	{
	  loc = location_declaration(ddecl->definition);
	  preferred = TRUE;
	}
      else
	{
	  loc = location_declaration(ddecl->ast);
	  preferred = FALSE;
	}
	    
      /* Make the qtypes */
      hash_table_reset(name_map);
      ctqi.loc = loc;
      ctqi.name_map = name_map;
      ctqi.generalize = generalize;
      if (convert_type_qtype(lifted_t, ddecl->name, preferred,
			     &ddecl->qtype, &ctqi))
	report_qerror(loc, sev_err, "invalid qualifier annotations");

      if (ddecl->noreturn && noreturn_qual)
	{
	  /* Add noreturn as a qualifier */
	  if (qtype_function(ddecl->qtype))
	    insistnot(mkeq_qual(loc, qual_qtype(ddecl->qtype), noreturn_qual));
	  else
	    report_qerror(loc, sev_err,
			  "noreturn attribute on non-function");
	}

      if (ddecl->__init && init_qual)
	{
	  /* Add init as a qualifier */
	  if (qtype_function(ddecl->qtype))
	    {
	      if (mkleq_qual(loc, qual_qtype(ddecl->qtype), init_qual))
		report_qerror(loc, sev_err,
			      "inconsistent __init declaration");
	    }
	  else
	    report_qerror(loc, sev_err,
			  "__init declared for non-function");
	}

      if (generalize && qtype_function(ddecl->qtype) &&
	  hash_table_size(name_map) > 0)
	{
	  ty_set tyvars;
	  qual_set qvars;
	  hash_table_scanner hts;
	  const char *vname;
	  void *vp;

	  /* Generalize the type */
	  tyvars = empty_ty_set();
	  qvars = empty_qual_set();

	  hash_table_scan(name_map, &hts);
	  while (hash_table_next(&hts, (hash_key *) &vname, &vp))
	    {
	      if (is_qualifier_constraint(vname))
		{
		  assert(!qual_set_member(qvars, vp));
		  qual_set_insert(parse_region, &qvars, vp);
		}
	      else
		{
		  assert(!ty_set_member(tyvars, vp));
		  ty_set_insert(parse_region, &tyvars, vp);
		}
	    }

	  ddecl->qtype = mkqtype_scheme(ddecl->qtype, tyvars, qvars);
	}

      /*	  printf("|- %s : ", ddecl->name);
	  	  print_qtype_raw(printf, ddecl->qtype);
	  	  printf("\n");*/
      /* XXX: Hack!  Add magic effects of fopen */
      if (qtype_function(ddecl->qtype) &&
	  (!strcmp(ddecl->name, "fopen") ||
	   !strcmp(ddecl->name, "fdopen") ||
	   !strcmp(ddecl->name, "safefopen")))
	{
	  effect fn_eff;
	  aloc al;

	  al = aloc_qtype(return_qtype(ddecl->qtype));
	  fn_eff = effect_qtype(ddecl->qtype);

	  /* Allocates and changes qtype */
	  mark_aloc_interesting(al);
	  mkleq_effect(effect_wr(al), fn_eff);
	  mkleq_effect(effect_alloc(al), fn_eff);
	}
      return TRUE;
    }
  else
    return FALSE;
}

/* Convert a field declaration to a qtype */
qtype get_fdecl_qtype(const char *tag_name, field_declaration fdecl)
{
  type t, lifted_t;
  struct ctq_info ctqi;
  const char *name;
  qtype result;

  t = fdecl->type;
  name = fdecl->name ? fdecl->name : "(anonymous)";
  
  /*      printf("Computing qualifiers for field %s.%s\n", tag_name,
	  fdecl->name);*/
  /* Use same logic as above.
     
     Why do we lift field types to ptr types (in general)?  Because
     they can have difference reference properties than their
     containing structure.  E.g., a single field can be const without
     the structure itself being const.  (The converse is not true.) */
  if (type_function(t) || type_array(t))
    lifted_t = t;
  else
    lifted_t = make_pointer_type(t);

  /* XXX: name_map is NULL for now... */
  ctqi.loc = location_declaration(CAST(declaration, fdecl->ast));
  ctqi.name_map = NULL;
  ctqi.generalize = FALSE;
  if (convert_type_qtype(lifted_t, name, FALSE, &result, &ctqi))
    report_qerror(fdecl->ast->loc, sev_err,
		  "invalid qualifier annotations");
  /*      printf("|- %s.%s : ", tag_name, fdecl->name);
	  print_qtype_raw(printf, result);
	  printf("\n");*/
  return result;
}

/* Internal function to convert a type into a qtype.  We basically
   just make a copy, except we use qualifier variables at every level,
   which may be constrained by the already given qualifiers (if any).
   loc is the location where this type is defined, and pref is true if
   we should mark the qualifiers we create as preferred.  Returns the
   new qtype in *qt.  Returns TRUE if an error occurred, false
   otherwise.  */
static bool convert_type_qtype(type t, const char *name, bool pref, qtype *qt,
			       ctq_info ctqi)
{
  qtype new_qtype;
  bool result;

  assert(t && qt);
  new_qtype = ralloc(parse_region, struct Qtype);
  new_qtype->conv = NULL;
  result = FALSE;
  result = make_qvar_name_constrained(name,
				      ctqi->loc,
				      pref,
				      &new_qtype->qual,
				      type_user_quals(t),
				      ctqi->name_map) || result;

  if (type_atomic(t))
    {
      if (type_void(t))
	new_qtype->type = void_ty;
      else
	{
	  if (type_complex(t))
	    fail("Unimplemented\n");

	  if (type_char(t))
	    new_qtype->type = char_ty;
	  else if (type_signed_char(t))
	    new_qtype->type = signed_char_ty;
	  else if (type_unsigned_char(t))
	    new_qtype->type = unsigned_char_ty;
	  else if (type_short(t))
	    new_qtype->type = short_ty;
	  else if (type_unsigned_short(t))
	    new_qtype->type = unsigned_short_ty;
	  else if (type_int(t))
	    new_qtype->type = int_ty;
	  else if (type_unsigned_int(t))
	    new_qtype->type = unsigned_int_ty;
	  else if (type_long(t))
	    new_qtype->type = long_ty;
	  else if (type_unsigned_long(t))
	    new_qtype->type = unsigned_long_ty;
	  else if (type_long_long(t))
	    new_qtype->type = long_long_ty;
	  else if (type_unsigned_long_long(t))
	    new_qtype->type = unsigned_long_long_ty;
	  else if (type_float(t))
	    new_qtype->type = float_ty;
	  else if (type_double(t))
	    new_qtype->type = double_ty;
	  else if (type_long_double(t))
	    new_qtype->type = long_double_ty;
	  else
	    fail("Unknown primitive type in type_to_qtype\n");
	}

      if (type_restrict(t))
	report_qerror(ctqi->loc, sev_err,
		      "ignoring restrict qualifier on non-pointer");

      /* Construct alocs and rwr_alocs fields */
      new_qtype->type->alocs = NULL;
      result = add_constraints_level(new_qtype->qual,
				     type_qualifiers(t)&(~restrict_qualifier),
				     type_user_quals(t),
				     level_value,
				     ctqi->loc) || result;
    }
  else if (type_function(t))
    {
      /* Make a function qtype.  Note that in the function's type, the
	 params and return value have value types (r-types). */
      effect fn_eff;
      
      new_qtype->type = ralloc(parse_region, struct Ty);
      new_qtype->type->alocs = NULL;
      new_qtype->type->kind = tk_function;
      new_qtype->type->u.fn.oldstyle = type_function_oldstyle(t);
      new_qtype->type->u.fn.varargs = type_function_varargs(t);
      fn_eff = effect_var(rstrcat(parse_region, name, "_eff"));
      mark_effect_interesting(fn_eff);
      new_qtype->type->u.fn.effect = fn_eff;

      /* Handle varargs qualifier */
      if (new_qtype->type->u.fn.varargs &&
	  (type_function_varargs_quals(t) ||
	   type_function_varargs_user_quals(t)))
	{
	  if (ctqi->generalize)
	    {
	      const char *vq_name = rstrcat(parse_region, name, "_v");
	      user_qual_list vuql;
	      type_quals vqs;

	      if ((type_function_varargs_quals(t) & restrict_qualifier) != 0)
		report_qerror(ctqi->loc, sev_err,
			      "ignoring restrict qualifier on ...");
	      vqs = type_function_varargs_quals(t) & (~restrict_qualifier);
	      vuql = type_function_varargs_user_quals(t);
	      result = make_qvar_name_constrained(vq_name,
						  ctqi->loc,
						  pref,
						  &new_qtype->type->u.fn.vqual,
						  vuql,
						  ctqi->name_map) || result;
	      result = add_constraints_level(new_qtype->type->u.fn.vqual,
					     vqs,
					     vuql,
					     level_value,
					     ctqi->loc) || result;
	    }
	  else
	    /* Error here because otherwise our instantiation of varargs
	       constraints in varargs_constrain_quals_qtype is not safe */
	    report_qerror(ctqi->loc, sev_err,
			  "Ignoring varargs qualifier in non-generalizable "
			  "context");
	}
      else
	new_qtype->type->u.fn.vqual = NULL;

      /* Convert return type */
      {
	struct ctq_info ctqi_ret;
	qtype returns;

	ctqi_ret.loc = ctqi->loc;
	ctqi_ret.name_map = ctqi->name_map;
	ctqi_ret.generalize = ctqi->generalize;
	result = convert_type_qtype(type_function_return_type(t),
				    rstrcat(parse_region, name, "_ret"),
				    FALSE,
				    &returns,
				    &ctqi_ret) || result;
	new_qtype->type->u.fn.returns = returns;
	mkleq_effect(param_qual_effect(returns), fn_eff);
      }

      /* Convert argument types */
      {
	typelist args;
	typelist_scanner scanargs;
	type argt;
	struct ctq_info ctqi_args;
	int i;

	new_qtype->type->u.fn.argtypes = NULL;
	args = type_function_arguments(t);
	if (args)
	  {
	    ctqi_args.loc = ctqi->loc;
	    ctqi_args.name_map = ctqi->name_map;
	    ctqi_args.generalize = ctqi->generalize;
	    typelist_scan(args, &scanargs);
	    i = 0;
	    while ((argt = typelist_next(&scanargs)))
	      {
		const char *arg_name;
		qtype argqt;
		
		arg_name = rstrscat(parse_region, name, "_arg",
				    inttostr(parse_region, i++), 0);
		result = convert_type_qtype(argt,
					    arg_name,
					    FALSE,
					    &argqt,
					    &ctqi_args) || result;
		qtypelist_append(&new_qtype->type->u.fn.argtypes, argqt);
		mkleq_effect(param_qual_effect(argqt), fn_eff);
	      }
	  }
	mkleq_effect(enforce_param_effects(new_qtype->type->u.fn.argtypes,
					   ctqi->loc),
		     fn_eff);
      }

      new_qtype->type->u.fn.in =
	make_store_var(ctqi->loc,
		       rstrcat(parse_region, name, "_in"));
      new_qtype->type->u.fn.out =
	make_store_var(ctqi->loc,
		       rstrcat(parse_region, name, "_out"));

      new_qtype->type->u.fn.aloc =
	make_aloc(rstrcat(parse_region, name, "_loc"), new_qtype);

      if (type_restrict(t))
	report_qerror(ctqi->loc, sev_err,
		      "ignoring restrict qualifier on non-pointer");
      result = add_constraints_level(new_qtype->qual,
				    type_qualifiers(t) & (~restrict_qualifier),
				     type_user_quals(t),
				     level_value,
				     ctqi->loc) || result;
    }
  else if (type_pointer(t))
    {
      const char *new_name;
      type pt;
      qtype qpt;
      bool constpt, volatilept;
      type_quals pt_quals;
      aloc al;

      new_name = rstrcat(parse_region, name, "_p");
      pt = type_points_to(t);
      constpt = type_const(pt);
      volatilept = type_volatile(pt);

      result = convert_type_qtype(pt, new_name, pref, &qpt, ctqi)
	|| result;

      /* o|-< make function pointers const */
      if(const_qual && qtype_function(qpt))
	{
	  mkleq_qual(ctqi->loc, const_qual, new_qtype->qual);
	  pt_quals = force_qual_const(type_qualifiers(pt));
	}
      else
	pt_quals = type_qualifiers(pt);

      result = add_constraints_level(new_qtype->qual,
				     pt_quals,
				     type_user_quals(pt),
				     level_ref,
				     ctqi->loc) || result;
      result = add_constraints_level(new_qtype->qual,
				     type_qualifiers(t),
				     type_user_quals(t),
				     level_value,
				     ctqi->loc) || result;

      al = make_aloc(rstrcat(parse_region, name, "_loc"), qpt);

      new_qtype->type = mkty_pointer(qpt, al);
    }
  else if (type_array(t))
    {
      /* Treat arrays just like pointers.  The decl const int a[5]
         gets type const array('a int)); the const on the array means
         the same as for pointers: We can't assign through *a. */
      const char *new_name;
      type contents;
      qtype qpt;
      aloc al;

      new_name = rstrcat(parse_region, name, "\'");
      contents = type_array_of(t);

      result = convert_type_qtype(contents, new_name, pref, &qpt,
				  ctqi) || result;
      result =  add_constraints_level(new_qtype->qual,
				      type_qualifiers(contents),
				      type_user_quals(contents),
				      level_ref,
				      ctqi->loc) || result;
      result = add_constraints_level(new_qtype->qual,
				     type_qualifiers(t),
				     type_user_quals(t),
				     level_value,
				     ctqi->loc) || result;
      /* Multi-dimensional arrays do *not* share locations, because if they
	 do then we'll have a problem with the third pass (the same location
	 will have different types). */
      /*
      if (qtype_array(qpt))
	new_qtype->type = mkty_array(qpt,
				     type_array_size(t),
				     make_aloc(rstrcat(parse_region,
						       name, "_loc")));
      else
      */
      al = make_aloc(rstrcat(parse_region, name, "_loc"), qpt);
      new_qtype->type = mkty_array(qpt, type_array_size(t), al, NULL);

      /* Set addr_aloc here so debug printing in effect.c works */
      new_qtype->type->u.array.addr_aloc =
	make_aloc(rstrcat(parse_region, name, "_array_loc"), 
		  new_qtype);
    }
  else if (type_enum(t))
    {
      /* These are really just integers */
      new_qtype->type = int_ty;
      new_qtype->type->alocs = NULL;
      result = add_constraints_level(new_qtype->qual,
				     type_qualifiers(t),
				     type_user_quals(t),
				     level_value,
				     ctqi->loc) || result;
      /* Also give qtypes to the enumerators */
      analyze_tag_ref(type_tag(t)->ast);
    }
  else if (type_struct(t) || type_union(t))
    {
      tag_declaration td = type_tag(t);

      if (td->qtype)
	new_qtype->type = td->qtype->type;
      else
	{
	  field_declaration fd;

	  new_qtype->type = ralloc(parse_region, struct Ty);
	  if (type_struct(t))
	    new_qtype->type->kind = tk_struct;
	  else
	    new_qtype->type->kind = tk_union;

	  if (td->name)
	    new_qtype->type->u.tag.name = rstrdup(parse_region, td->name);
	  else
	    new_qtype->type->u.tag.name = NULL;
	  new_qtype->type->u.tag.mark = FALSE;
	  new_qtype->type->u.tag.defined = td->defined;
	  new_qtype->type->u.tag.transparent_union = type_transparent(t);
	  new_qtype->type->u.tag.num_equiv = 1;
	  new_qtype->type->u.tag.fs_ty = NULL;
	  new_qtype->type->alocs = NULL;
	  td->qtype = new_qtype;

	  new_qtype->type->u.tag.fields = NULL;
	  for (fd = td->fieldlist; fd; fd = fd->next)
	    {
	      field f;

	      f = mkfield(fd->name, analyze_field_declaration(td->name, fd));
	      field_append(&new_qtype->type->u.tag.fields, f);
	    }
	}
      result =  add_constraints_level(new_qtype->qual,
				      type_qualifiers(t),
				      type_user_quals(t),
				      level_value,
				      ctqi->loc) || result;
    }
  else if (type_var(t))
    {
      ty newt;

      if (hash_table_lookup(ctqi->name_map,
			    (hash_key) type_name(t),
			    (hash_data *) &newt))
	new_qtype->type = newt;
      else
	{
	  newt = mkty_var(type_name(t));
	  insist(hash_table_insert(ctqi->name_map,
				   (hash_key) type_name(t),
				   newt));
	  new_qtype->type = newt;
	}

      result = add_constraints_level(new_qtype->qual,
				     type_qualifiers(t),
				     type_user_quals(t),
				     level_value,
				     ctqi->loc) || result;
    }
  else
    fail("Unknown type in type_to_qtype\n");

  *qt = new_qtype;
  return result;
}

/* Makes a qtype from a regular type. */
qtype type_to_qtype(type t, const char *name, location loc)
{
  qtype result;
  struct ctq_info ctqi;

  hash_table_reset(name_map);
  ctqi.loc = loc;
  ctqi.name_map = name_map;
  ctqi.generalize = FALSE;
  if (convert_type_qtype(t, name, FALSE, &result, &ctqi))
    report_qerror(loc, sev_err, "inconsistent qualifier annotations");
  return result;
}

/* Constrain target by q according to the variance of q */
bool add_constraint_qual(qual target, qual q, location loc)
{
  switch (sign_qual(q))
    {
    case sign_pos:
      return mkleq_qual(loc, q, target);
      break;
    case sign_neg:
      return mkleq_qual(loc, target, q);
      break;
    case sign_eq:
      return mkeq_qual(loc, q, target);
      break;
    default:
      fail("Unexpected variance %d", sign_qual(q));
    }
}

/* Add constraints that the qualifier target by the qualifiers
   const, volatile, and user_quals.  This function only makes constraints
   if the level of the qualifiers match lev. */
bool add_constraints_level(qual target, type_quals quals,
			   user_qual_list user_quals, level_qual_t lev,
			   location loc)
{
  user_qual_list uql;
  qual q;
  bool result = FALSE;

  assert(lev == level_value || lev == level_ref);
  scan_user_qual_list(uql, user_quals)
    {
      if (is_qualifier_constraint(uql->qual->cstring.data))
	continue;
      q = find_qual(uql->qual->cstring.data);
      if (!q)
	{
	  report_qerror(uql->qual->loc, sev_err,
			"unknown qualifier %s", uql->qual->cstring.data);
	  result = TRUE;

	}
      else if (level_qual(q) == lev)
	/* Only add qualifier if the level property matches */
	result = add_constraint_qual(target, q, loc) || result;
    }

#define Q(name, kind, tq, val) \
  if (qual_ ## name(quals) && \
      name ## _qual && \
      level_qual(name ## _qual) == lev) \
    result = add_constraint_qual(target, name ## _qual, loc) || result;
#include "qualifiers.h"
#undef Q

  if (flag_strict_const && !qual_const(quals) && nonconst_qual &&
      level_qual(nonconst_qual) == lev)
    result = add_constraint_qual(target, nonconst_qual, loc) || result;

  return result;
}

/* Treat n1 and n2 as a string of the form $(_(n|v|w))*, where n is
   any number.  Return TRUE if the n's in n1 are a subset of the n's
   in n2. */
/* Extract the next number, ignoring _'s */
static inline int digits_to_num(const char **p) {
  int num;

  while (**p == '_') *p = *p + 1; /* Advance over initial _'s */
  num = strtol(*p, (char **) p, 10);
  if (**p != '\0' && **p != '_')
    return -1;
  else
    return num;
}
bool subset_str(const char *n1, const char *n2, location loc)
{
  const char *p1;

  assert(is_qualifier_constraint(n1) && is_qualifier_constraint(n2));
  n1++; n2++; /* Advance n1 and n2 beyond $ */

  for (p1 = n1; *p1;)
    {
      int num1, num2;
      const char *p2;

      num1 = digits_to_num(&p1); /* Extract number */
      num2 = 0;
      if (num1 < 0)
	{
	  report_qerror(loc, sev_err, "Bad qualifier constraint");
	  return FALSE;
	}

      for (p2 = n2; *p2;)
	{
	  num2 = digits_to_num(&p2); /* Extract number */
	  if (num2 < 0)
	    {
	      report_qerror(loc, sev_err, "Bad qualifier constraint");
	      return FALSE;
	    }
	  if (num1 == num2)
	    break;
	}
      if (num1 != num2)
	/* Found a number in n1 not in n2 */
	return FALSE;
    }
  return TRUE;
}

/* Generate constraints between q1 and q2 according to their names */
bool constrain_quals_by_name(qual q1, const char *n1, qual q2, const char *n2,
			     location loc)
{
  bool result;

  result = FALSE;
  if (subset_str(n1, n2, loc))
    result = mkleq_qual(loc, q1, q2) || result;
  if (subset_str(n2, n1, loc))
    result = mkleq_qual(loc, q2, q1) || result;
  return result;
}

/* Given a user_qual_list and a mapping of already-constructed
   name-constrained qualifiers, create a new qualifier in *q and
   constraint it according to its name and the qualifiers already in
   name_map.  Return TRUE if there is an error. */
bool make_qvar_name_constrained(const char *name, location loc, bool pref,
				qual *q, user_qual_list uql,
				hash_table name_map)
{
  user_qual_list uq;
  bool named;
  bool result;

  assert(q);
  result = FALSE;
  named = FALSE; /* Set to true when we find a name for q */
  scan_user_qual_list(uq, uql)
    {
      const char *uq_name;

      uq_name = uq->qual->cstring.data;
      if (is_qualifier_constraint(uq_name))
	{
	  /* Found a named qualifier */
	  if (!named)
	    {
	      hash_table_scanner hts;
	      qual q2;
	      const char *q2_name;

	      named = TRUE;

	      if (hash_table_lookup(name_map, (hash_key) uq_name,
				    (hash_data *) &q2))
		/* If the qual was already in the table, retrieve it */
		*q = q2;
	      else
		{
		  /* The qual was not in the table.  Make a fresh
                     qualifiers and add constraints between it and any
                     other constrained qualifiers in name_map, then
                     add the new qualifier to name_map .*/
		  *q = make_qvar(name, loc, pref);
		  named = TRUE;
		  hash_table_scan(name_map, &hts);
		  while (hash_table_next(&hts, (hash_key *) &q2_name,
					 (hash_data *) &q2))
		    {
		      if (is_qualifier_constraint(q2_name))
			/* Generate any constraint between q and q2 */
			result = constrain_quals_by_name(*q, uq_name,
							 q2, q2_name,
						uq->qual->loc) || result;
		    }
		  insist(hash_table_insert(name_map, (hash_key) uq_name, *q));
		}
	    }
	  else
	    {
	      report_qerror(uql->qual->loc, sev_err,
			    "duplicate declared qualifier constraints");
	      result = TRUE;
	    }
	}
    }
  if (!named)
    *q = make_qvar(name, loc, pref);
  return result;
}

/* Given an argument qtypelist qte, return the effects of restrict
   qualifiers in the argument list.  Also add constraints so that
   restricts of the same location are an error. */
static effect enforce_param_effects(qtypelist_scanner qte, location loc)
{
  effect eff;

  if (qte == NULL)
    return effect_empty;

  eff = enforce_param_effects(qte->next, loc);
  if (restrict_qual && has_qual_qtype(qte->qt, restrict_qual))
    {
      aloc al;

      /* Don't worry about const here */
      al = aloc_qtype(qte->qt);
      mknin_aloc_effect(loc, al, eff_rwr, eff); /* Can't restrict this same
                                                   param later on */
      eff = effect_union(effect_rwr(al), eff);/* Add restrict as an effect*/
    }
  return eff;
}

/* Return an effect containing any locations whose contents have
   flow-sensitive qualifiers.  This will prevent the flow-sensitive
   pass from flowing these locations around the function. */
static effect param_qual_effect(qtype qt)
{
  qt = ecr_qtype(qt);

  switch (qt->type->kind)
    {
    case tk_primitive:
    case tk_complex:
    case tk_error:
    case tk_void:
    case tk_var:
    case tk_struct:  /* XXX: Ignore inside of structs and unions */
    case tk_union:
      return effect_empty;
    case tk_pointer:
      {
	effect pte;

	pte = param_qual_effect(qt->type->u.ptr.pointsto);
	if (has_fs_qual(qt->type->u.ptr.pointsto->qual))
	  return effect_union(effect_r(qt->type->u.ptr.aloc),
			      pte);
	else
	  return pte;
      }
      break;
    case tk_function:
      {
	effect eff;
	qtypelist_scanner qtls;
	qtype argqtype;

	eff = param_qual_effect(qt->type->u.fn.returns);
	qtypelist_scan(qt->type->u.fn.argtypes, &qtls);
	while ((argqtype = qtypelist_next(&qtls)))
	  eff = effect_union(eff, param_qual_effect(argqtype));
	return eff;
      }
      break;
    case tk_array:
      {
	effect aoe;

	aoe = param_qual_effect(qt->type->u.array.arrayof);
	if (has_fs_qual(qt->type->u.array.arrayof->qual))
	  return effect_union(effect_r(qt->type->u.array.aloc),
			      aoe);
	else
	  return aoe;
      }
      break;
    case tk_pointer_loc:
    case tk_scheme:
      /* These should never show up -- fail */
    default:
      fail("Unexpected qtype kind %x\n", qt->type->kind);
    }
}

/* Return TRUE iff t1 and t2 match exactly, i.e., if converting t1 and
   t2 to qtypes would yield exactly the same qtype */
bool match_type(type t1, type t2)
{
  user_qual_list uql1, uql2;

  if (type_qualifiers(t1) != type_qualifiers(t2))
    return FALSE;

  uql1 = type_user_quals(t1);
  uql2 = type_user_quals(t2);
  while (uql1 && uql2)
    {
      if (strcmp(uql1->qual->cstring.data, uql2->qual->cstring.data) != 0)
	/* Very conservative -- must match textually */
	return FALSE;
      uql1 = uql1->next;
      uql2 = uql2->next;
    }
  if (uql1 != uql2)
    return FALSE;

  if ((type_void(t1) && type_void(t2)) ||
      (type_char(t1) && type_char(t2)) ||
      (type_signed_char(t1) && type_signed_char(t2)) ||
      (type_unsigned_char(t1) && type_unsigned_char(t2)) ||
      (type_short(t1) && type_short(t2)) ||
      (type_unsigned_short(t1) && type_unsigned_short(t2)) ||
      (type_int(t1) && type_int(t2)) ||
      (type_unsigned_int(t1) && type_unsigned_int(t2)) ||
      (type_long(t1) && type_long(t2)) ||
      (type_unsigned_long(t1) && type_unsigned_long(t2)) ||
      (type_long_long(t1) && type_long_long(t2)) ||
      (type_unsigned_long_long(t1) && type_unsigned_long_long(t2)) ||
      (type_float(t1) && type_float(t2)) ||
      (type_double(t1) && type_double(t2)) ||
      (type_long_double(t1) && type_long_double(t2)))
    return TRUE;
  else if (type_function(t1) && type_function(t2))
    {
      typelist args1, args2;
      typelist_scanner scanargs1, scanargs2;
      type arg1, arg2;

      if ((type_function_varargs(t1) != type_function_varargs(t2)) ||
	  (type_function_oldstyle(t1) != type_function_oldstyle(t2)) ||
	  (type_function_varargs_quals(t1) != type_function_varargs_quals(t2)) ||
	  (type_function_varargs_user_quals(t1) != type_function_varargs_user_quals(t2)))
	return FALSE;

      if (!match_type(type_function_return_type(t1),
		      type_function_return_type(t2)))
	return FALSE;

      args1 = type_function_arguments(t1);
      args2 = type_function_arguments(t2);
      if (args1 == NULL && args2 == NULL)
	return TRUE;
      else
	{
	  typelist_scan(args1, &scanargs1);
	  typelist_scan(args2, &scanargs2);
	  while ((arg1 = typelist_next(&scanargs1)) &&
		 (arg2 = typelist_next(&scanargs2)))
	    if (!match_type(arg1, arg2))
	      return FALSE;
	}

      return TRUE;
    }
  else if (type_pointer(t1) && type_pointer(t2))
    return match_type(type_points_to(t1), type_points_to(t2));
  else if (type_array(t1) && type_array(t2))
    return match_type(type_array_of(t1), type_array_of(t2));
  else if ((type_struct(t1) && type_struct(t2)) ||
	   (type_union(t1) && type_union(t2)))
    {
      /* This code relies on the fact that as structures are declared
	 they are annotated.  If that's not true then using this code
	 will change the -fprint-results and PAM output. */

      tag_declaration td1, td2;

      td1 = type_tag(t1);
      td2 = type_tag(t2);

      if (td1->qtype && td2->qtype && eq_qtype(td1->qtype, td2->qtype))
	return TRUE;

      /*

      union_tag_declaration(td1, td2);
      fd1 = td1->fieldlist;
      fd2 = td2->fieldlist;

      result = TRUE;
      while (result && fd1 && fd2)
	{
	  result = result && match_type(fd1->type, fd2->type);
	  fd1 = fd1->next;
	  fd2 = fd2->next;
	}
      result = result && (fd1 == NULL) && (fd2 == NULL);
      */
    }
  else if (type_enum(t1) && type_enum(t2))
    {
      return TRUE; /* All enums considered equal */
    }
  return FALSE;
}

/**************************************************************************
 *                                                                        *
 * Copying qtypes and polymorphic qtypes                                  *
 *                                                                        *
 **************************************************************************/

/* Return TRUE iff s is a special qualifier name indicating a
   constraint on the qualifier. */
bool is_qualifier_constraint(const char *s)
{
  return strncmp(s, "$_", 2) == 0;
}

/* Make a hash table whose keys are qualifiers */
hash_table make_qual_hash_table(region r, int size)
{
  return make_hash_table(r, size, ptr_hash, (keyeq_fn) eq_qual);
}

hash_table make_ty_hash_table(region r, int size)
{
  return make_hash_table(r, size, ptr_hash, (keyeq_fn) eq_ty);
}

/* Make a copy of t, replacing qualifiers according to f and type
   variables according to g */
ty substitute_ty(ty t, qual (*f)(qual, void *arg), void *f_arg,
		 ty (*g)(ty, void *arg), void *g_arg)
{
  ty newt;

  switch (t->kind)
    {
    case tk_primitive:
    case tk_complex:
    case tk_void:
    case tk_error:
    case tk_struct:
    case tk_union:
    case tk_pointer_loc:
      newt = t;
      break;
    case tk_pointer:
      newt = mkty_pointer(substitute_qtype(t->u.ptr.pointsto,
					   f, f_arg, g, g_arg),
			  t->u.ptr.aloc);
      break;
    case tk_array:
      newt = mkty_array(substitute_qtype(t->u.array.arrayof,
					 f, f_arg, g, g_arg),
			t->u.array.size,
			t->u.array.aloc,
			t->u.array.addr_aloc);
      break;
    case tk_function:
      newt = mkty_function(t->u.fn.varargs,
			   t->u.fn.vqual
			     ? f(t->u.fn.vqual, f_arg)
			     : NULL,
			   t->u.fn.oldstyle,
			   substitute_qtype(t->u.fn.returns,
					    f, f_arg, g, g_arg),
			   substitute_qtypelist(t->u.fn.argtypes,
						f, f_arg, g, g_arg),
			   t->u.fn.in,
			   t->u.fn.out,
			   t->u.fn.aloc,
			   t->u.fn.effect);
      break;
    case tk_var:
      newt = g(t, g_arg);
      break;
    default:
      fail("Unexpected qtype kind %x\n", t->kind);
    }
  return newt;

}

qtype substitute_qtype(qtype qt, qual (*f)(qual, void *arg), void *f_arg,
		       ty (*g)(ty, void *arg), void *g_arg)
{
  qtype new_qtype;

  qt = ecr_qtype(qt);
  new_qtype = ralloc(parse_region, struct Qtype);
  new_qtype->qual = f(qt->qual, f_arg);
  new_qtype->type = substitute_ty(qt->type, f, f_arg, g, g_arg);
  new_qtype->conv = NULL;
  return new_qtype;
}

/* Make a copy of qtlist, replacing qualifiers according to f and type
   variables according to g */
qtypelist substitute_qtypelist(qtypelist qtlist, qual (*f)(qual, void *arg),
			       void *f_arg, ty (*g)(ty, void *arg),
			       void *g_arg)
{
  qtypelist_scanner qs;
  qtype qt;
  qtypelist result = NULL;

  qtypelist_scan(qtlist, &qs);
  while ((qt = qtypelist_next(&qs)))
    qtypelist_append(&result, substitute_qtype(qt, f, f_arg, g, g_arg));
  return result;
}

/* Info needed for copy_qtype */
typedef struct copy_info
{
  location loc;    /* Where to say new qualifiers came from */
  hash_table qmap; /* Maintains mapping from old quals to new quals */
  hash_table vmap; /* Maintains mapping from old type vars to new type vars */
} *copy_info;

/* Copy qt, making fresh qualifiers everywhere. */
qual copy_qtype_qsubs(qual q, void *arg)
{
  copy_info ci = (copy_info) arg;
  qual newq;

  if (hash_table_lookup(ci->qmap, q, (hash_data *) &newq))
    return newq;
  else
    {
      int base_len;
      const char *orig_name;
      char *base_name;
      char *index;

      /* Compute the base name of q by removing any @ suffix, so
	 that copies of copies look like x@0, x@1, etc instead of
	 x@0, x@0@1, ... */
      orig_name = name_qual(q);
      if ((index = strstr(orig_name, "@")))
	base_len = index - orig_name;
      else
	base_len = strlen(orig_name);
      base_name = alloca((base_len + 2)*sizeof(char));
      strncpy(base_name, orig_name, base_len);
      base_name[base_len] = '@';
      base_name[base_len+1] = '\0';


      newq = make_fresh_qvar(base_name, ci->loc);
      insist(hash_table_insert(ci->qmap, q, newq));
      return newq;
    }
}

ty copy_qtype_tsubs(ty t, void *arg)
{
  copy_info ci = (copy_info) arg;
  ty newt;

  if (hash_table_lookup(ci->vmap, t, (hash_data *) &newt))
    return newt;
  else
    {
      newt = mkty_fresh();
      insist(hash_table_insert(ci->vmap, t, newt));
      return newt;
    }
}

qtype copy_qtype(qtype qt, location loc)
{
  struct copy_info ci;

  ci.loc = loc;
  ci.qmap = make_qual_hash_table(scratch_region, 10);
  ci.vmap = make_ty_hash_table(scratch_region, 10);
  return substitute_qtype(qt, copy_qtype_qsubs, &ci, copy_qtype_tsubs, &ci);
}

static ty copy_ty(ty t, location loc)
{
  struct copy_info ci;

  ci.loc = loc;
  ci.qmap = make_qual_hash_table(scratch_region, 10);
  ci.vmap = make_ty_hash_table(scratch_region, 10);
  return substitute_ty(t, copy_qtype_qsubs, &ci, copy_qtype_tsubs, &ci);
}

/* Info needed for instantiate_qtype */
typedef struct inst_info
{
  location loc;     /* Where to say new qualifiers came from */
  ty_set tyvars;    /* The bound type variables */
  qual_set qvars;   /* The bound qualifier variables */
  hash_table qmap;  /* Maintains mapping from old quals to new quals */
  hash_table vmap;  /* Maintains mapping from old type vars to new type vars */
  const char *suffix; /* Add this to the end of the names of newly
			 instantiated vars */
} *inst_info;


/* Helper function for instantiate_qtype.  Given a qual q and
   hash_table arg, if q is in arg return the qual it maps to,
   otherwise make a fresh copy of q preserving constraints. */
qual instantiate_qtype_qsubs(qual q, void *arg)
{
  inst_info ii = (inst_info) arg;
  qual newq;

  if (!qual_set_member(ii->qvars, q))
    return q;    /* Don't copy q if it's not bound */
  else if (hash_table_lookup(ii->qmap, q, (hash_data *) &newq))
    return newq;
  else
    {
      const char *newq_name;
      qual_edge_set_scanner qess;
      qual_edge qe;

      newq_name = rstrcat(parse_region, name_qual(q), ii->suffix);
      newq = make_qvar(newq_name, ii->loc, FALSE);
      insist(hash_table_insert(ii->qmap, q, newq));

      /* Copy constraints to newq.  Notice that we also copy variables,
	 but we ignore the results of mkleq_qual. */
      scan_qual_edge_set(qe, qess, lb_qual(q))
	{
	  qual b;
	  location loc;

	  b = qe->qual;
	  loc = qe->loc;
	  if (constant_qual(b))
	    mkleq_qual(loc, b, newq);
	  else if (variable_qual(b))
	    mkleq_qual(loc, instantiate_qtype_qsubs(b, arg), newq);
	  else
	    fail("Unexpected qualifier\n");
	}
      scan_qual_edge_set(qe, qess, ub_qual(q))
	{
	  qual b;
	  location loc;

	  b = qe->qual;
	  loc = qe->loc;
	  if (constant_qual(b))
	    mkleq_qual(loc, newq, b);
	  else if (variable_qual(b))
	    mkleq_qual(loc, newq, instantiate_qtype_qsubs(b, arg));
	  else
	    fail("Unexpected qualifier\n");
	}

      return newq;
    }
}

ty instantiate_qtype_tsubs(ty t, void *arg)
{
  inst_info ii = (inst_info) arg;
  ty newt;

  if (!ty_set_member(ii->tyvars, t))
    return t;     /* Don't copy t if it's not bound */
  else if (hash_table_lookup(ii->vmap, t, (hash_data *) &newt))
    return newt;
  else
    {
      newt = mkty_fresh();
      insist(hash_table_insert(ii->vmap, t, newt));
      return newt;
    }
}

/* Instantiate a fresh copy of qt, also copying the constraints on qt. */
qtype instantiate_qtype(qtype qt, location loc)
{
  struct inst_info ii;

  assert(qtype_scheme(qt));
  ii.loc = loc;
  ii.tyvars = qt->type->u.scheme.tyvars;
  ii.qvars = qt->type->u.scheme.qvars;
  ii.qmap = make_qual_hash_table(scratch_region, 10);
  ii.vmap = make_ty_hash_table(scratch_region, 10);
  ii.suffix = rstrcat(parse_region,
		      "_inst",
		      inttostr(parse_region,
			       qt->type->u.scheme.num_inst++));

  return substitute_qtype(qt->type->u.scheme.body,
			  instantiate_qtype_qsubs, &ii,
			  instantiate_qtype_tsubs, &ii);
}

/* Add q and all qualifiers reachable from q to qvars. */
void qual_gather_reachable_qvars(qual q, qual_set *qvars)
{
  if (variable_qual(q) && !qual_set_member(*qvars, q))
    {
      /* We added q.  Add all qualifiers reachable from q */
      qual_edge_set_scanner qess;
      qual_edge qe;

      qual_set_insert(parse_region, qvars, q);
      scan_qual_edge_set(qe, qess, lb_qual(q))
	qual_gather_reachable_qvars(qe->qual, qvars);
      scan_qual_edge_set(qe, qess, ub_qual(q))
	qual_gather_reachable_qvars(qe->qual, qvars);
    }
}

/* Traverse qt, gather up all variables in and reachable from qt */
void qtype_gather_vars(qtype qt, ty_set *tyvars, qual_set *qvars)
{
  if (variable_qual(qt->qual))
    qual_gather_reachable_qvars(qt->qual, qvars);
  switch (qt->type->kind)
    {
    case tk_primitive:
    case tk_complex:
    case tk_void:
    case tk_error:
    case tk_struct:
    case tk_union:
    case tk_pointer_loc:
      return;
    case tk_pointer:
      qtype_gather_vars(qt->type->u.ptr.pointsto, tyvars, qvars);
      return;
    case tk_array:
      qtype_gather_vars(qt->type->u.array.arrayof, tyvars, qvars);
      return;
    case tk_function:
      {
	qtypelist_scanner qs;
	qtype argqt;

	qtype_gather_vars(qt->type->u.fn.returns, tyvars, qvars);
	if (qt->type->u.fn.vqual)
	  qual_gather_reachable_qvars(qt->type->u.fn.vqual, qvars);

	qtypelist_scan(qt->type->u.fn.argtypes, &qs);
	while ((argqt = qtypelist_next(&qs)))
	  qtype_gather_vars(argqt, tyvars, qvars);
	return;
      }
    case tk_var:
      ty_set_insert(parse_region, tyvars, qt->type);
      return;
    default:
      fail("Unexpected qtype kind %x\n", qt->type->kind);
    }
}

/* Turn qt into a type scheme, generalizing over all qualifier
   variables in qt.  No checking is done that this is sound. */
qtype generalize_qtype(qtype qt)
{
  ty_set tyvars;
  qual_set qvars;

  assert(!qtype_scheme(qt));
  tyvars = empty_ty_set();
  qvars = empty_qual_set();

  qtype_gather_vars(qt, &tyvars, &qvars);

  return mkqtype_scheme(qt, tyvars, qvars);
}

/**************************************************************************
 *                                                                        *
 * Qualified type manipulation                                            *
 *                                                                        *
 **************************************************************************/

/* Return TRUE iff qt1 and qt2 have the same shape.  If match_quals is
   not NULL, calls match_quals on every pair of corresponding
   qualifiers (q1 from qt1, q2 from qt2), with extra arg. */
bool match_qtype(qtype qt1, qtype qt2,
		 void (*match_quals)(qual q1, qual q2, void *arg), void *arg)
{
  qt1 = ecr_qtype(qt1);
  qt2 = ecr_qtype(qt2);
  if (match_quals)
    match_quals(qt1->qual, qt2->qual, arg);
  if (qt1->type->kind == tk_primitive && qt2->type->kind == tk_primitive)
    return TRUE;
  else if (qt1->type->kind == tk_void && qt2->type->kind == tk_void)
    return TRUE;
  else if (qt1->type->kind == tk_pointer && qt2->type->kind == tk_pointer)
      return match_qtype(qt1->type->u.ptr.pointsto, qt2->type->u.ptr.pointsto,
			 match_quals, arg);
  else if (qt1->type->kind == tk_pointer_loc &&
	   qt2->type->kind == tk_pointer_loc)
    return eq_aloc(qt1->type->u.pointer_loc.aloc,
		   qt2->type->u.pointer_loc.aloc);
  else if (qt1->type->kind == tk_array && qt2->type->kind == tk_array)
      return match_qtype(qt1->type->u.array.arrayof, qt2->type->u.array.arrayof,
			 match_quals, arg);
  else if ((qt1->type->kind == tk_struct && qt2->type->kind == tk_struct) ||
	   (qt1->type->kind == tk_union && qt2->type->kind == tk_union))
    /* Don't match up fields -- only succeed if they're equal */
    return (qt1->type == qt2->type);
  else if (qt1->type->kind == tk_function && qt2->type->kind == tk_function)
    {
      /* For function types, the issue is what to do when their numbers
	 of arguments don't match.  We follow the logic of
	 types.c/function_compatible. */

      qtypelist_scanner qs1, qs2;
      qtype arg1_qtype, arg2_qtype;
      bool result;

      result = match_qtype(return_qtype(qt1), return_qtype(qt2),
			   match_quals, arg);

      qtypelist_scan(arg_qtypes(qt1), &qs1);
      qtypelist_scan(arg_qtypes(qt2), &qs2);

      /* Avoid short circuiting && */
      while ((arg1_qtype = qtypelist_next(&qs1),
	      arg2_qtype = qtypelist_next(&qs2),
	      arg1_qtype && arg2_qtype))
	result = result && match_qtype(arg2_qtype, arg1_qtype, match_quals,
				       arg);


      result = result && (qtype_oldstyle(qt1) == qtype_oldstyle(qt2));
      result = result && (!arg1_qtype && !arg2_qtype);
      result = result && (qtype_varargs(qt1) == qtype_varargs(qt2));
      if (match_quals && qtype_varargs(qt1) && qtype_varargs(qt2) &&
	  vqual_qtype(qt1) && vqual_qtype(qt2))
	match_quals(vqual_qtype(qt1), vqual_qtype(qt2), arg);

      return result;
    }
  else
    return FALSE;
}

/* Return the base type of a complex qtype */
qtype complex_base_qtype(qtype qt)
{
  qtype base_qtype;

  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_complex);
  base_qtype = ralloc(parse_region, struct Qtype);
  base_qtype->type->kind = tk_primitive;
  base_qtype->qual = qt->qual;
  base_qtype->type->u.primitive = qt->type->u.primitive;
  base_qtype->conv = NULL;
  return base_qtype;
}

/* Remove one level of ptr from a qtype */
qtype points_to_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_pointer);
  return qt->type->u.ptr.pointsto;
}

/* Return the abstract location of a pointer qtype */
aloc aloc_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  switch (qt->type->kind)
    {
    case tk_pointer:
      return qt->type->u.ptr.aloc;
    case tk_pointer_loc:
      return qt->type->u.pointer_loc.aloc;
    case tk_array:
      return qt->type->u.array.aloc;
    case tk_function:
      return qt->type->u.fn.aloc;
    case tk_scheme:
      return aloc_qtype(qt->type->u.scheme.body); /* Assume locs not
						     generalized */
    default:
      fail("Unexpected qtype kind %x in aloc_qtype\n", qt->type->kind);
    }
}

/* Remove one level of array from a qtype */
qtype array_of_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_array);
  return qt->type->u.array.arrayof;
}

/* Get the size of the array */
expression array_size_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_array);
  return qt->type->u.array.size;
}

/* Return the abstract location of an array (not its contents) */
aloc array_aloc_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_array);
  return qt->type->u.array.addr_aloc;
}

/* Get return type of function qtype */
qtype return_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_function);
  return qt->type->u.fn.returns;
}

/* Get argument types of function qtype */
qtypelist arg_qtypes(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_function);
  return qt->type->u.fn.argtypes;
}

/* Get effect of a function qtype */
effect effect_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_function);
  return qt->type->u.fn.effect;
}

/* Get varargs qualifier of function qtype, if any */
qual vqual_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_function &&
	 qt->type->u.fn.varargs);
  return qt->type->u.fn.vqual;
}

/* Get initial flow-sensitive qualifier environment */
store store_in_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_function);
  return qt->type->u.fn.in;
}

/* Get final flow-sensitive qualifier environment */
store store_out_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_function);
  return qt->type->u.fn.out;
}

/* The name of a struct or union (if any) */
const char *tag_name_qtype(qtype qt)
{
  qt = ecr_qtype(qt);
  assert(qt->type->kind == tk_struct ||
	 qt->type->kind == tk_union);
  return qt->type->u.tag.name;
}

/* Get the type of a field of an aggregate */
qtype field_qtype(qtype qt, cstring name)
{
  field f;

  qt = ecr_qtype(qt);
  assert(qtype_aggregate(qt));
  for (f = qt->type->u.tag.fields; f; f = f->next)
    if (f->name && !strcmp(name.data, f->name))
      return f->qtype;
  fail("Request for field not in aggregate\n");
}

/* Start scanning the fields of an aggregate */
void field_scan(qtype qt, field_scanner *fs)
{
  qt = ecr_qtype(qt);
  assert(qtype_aggregate(qt));
  *fs = qt->type->u.tag.fields;
}

/* Start scanning the fields of an aggregate.  Returns NULL if no such
   field exists. */
void field_scan_at(qtype qt, cstring name, field_scanner *fs)
{
  field f;

  qt = ecr_qtype(qt);
  assert(qtype_aggregate(qt));
  for (f = qt->type->u.tag.fields; f; f = f->next)
    if (!strcmp(name.data, f->name))
      {
	*fs = f;
	break;
      }
}

/* Get the next field of an aggregate */
qtype field_next(field_scanner *fs)
{
  field f = (field) *fs;
  qtype result;

  if (!f)
    return NULL;
  result = f->qtype;
  *fs = f->next;
  return result;
}

/**************************************************************************
 *                                                                        *
 * Constraint generation                                                  *
 *                                                                        *
 **************************************************************************/

/* Return the ECR for qt */
static ty ecr_ty(ty t)
{
  if (t->kind == tk_link)
    {
      ty ecr = t, cur, temp;
	
      /* Find root */
      while (ecr->kind == tk_link)
	ecr = ecr->u.link;
      
      /* Compress path */
      cur = t;
      while (cur->u.link != ecr)
	{
	  temp = cur->u.link;
	  cur->u.link = ecr;
	  cur = temp;
	}
      return ecr;
    }
  else
    return t;
}

static qtype ecr_qtype(qtype qt)
{
  qt->type = ecr_ty(qt->type);    /* Compress exactly to ecr */
  return qt;
}

typedef enum {mkleq, mkeq, unify} mkXeq;

static mkXeq toEq(mkXeq which)
{
  switch (which)
    {
    case mkleq: return mkeq;
    case mkeq: return mkeq;
    case unify: return unify;
    default:
      fail("Unexpected mkXeq kind %x\n", which);
    }
}

/* Constrain q1 and q2 according to which.  Return TRUE iff an error
   occurs. */
static bool mkXeq_qual(qual q1, qual q2, mkXeq which, location loc)
{
  switch (which)
    {
    case mkleq:
      return mkleq_qual(loc, q1, q2);
    case mkeq:
      return mkeq_qual(loc, q1, q2);
    case unify:
      return unify_qual(loc, q1, q2);
    default:
      fail("Unexpected mkXeq kind %x\n", which);
    }
}

/* Constrain e1 and e2 according to which. */
static void mkXeq_effect(effect e1, effect e2, mkXeq which)
{
  switch (which)
    {
    case mkleq:
      mkleq_effect(e1, e2);
      break;
    case mkeq:
      mkeq_effect(e1, e2);
      break;
    case unify:
      unify_effect(e1, e2);
      break;
    default:
      fail("Unexpected mkXeq kind %x\n", which);
    }
}

/* Constrain s1 and s2 according to which. */
static void mkXeq_store(location loc, store s1, store s2, mkXeq which)
{
  switch (which)
    {
    case mkleq:
      mkleq_store(loc, "inconsistent flow-sensitive qualifiers", s1, s2);
      break;
    case mkeq:
      mkeq_store(loc, "inconsistent flow-sensitive qualifiers", s1, s2);
      break;
    case unify:
      unify_store(loc, "inconsistent flow-sensitive qualifiers", s1, s2);
      break;
    default:
      fail("Unexpected mkXeq kind %x\n", which);
    }
}


/* Unify the alocs array of t1 and t2 */
static bool unify_ty_alocs(ty t1, ty t2)
{
  bool result;
  int i;

  result = FALSE;
  if (t1->alocs && t2->alocs)
    for (i = eff_any; i <= eff_last; i++)
      {
	if (!eq_effect(t1->alocs[i], effect_empty) &&
	    !eq_effect(t2->alocs[i], effect_empty))
	  unify_effect(t1->alocs[i], t2->alocs[i]);
      }
  return result; /* No possible errors, actually */
}

/* Add a constraint between qt1 and qt2 according to which.  If
   collapse is FALSE, report an error if qt1 and qt2's shapes are
   different.  If collapse is TRUE, then don't report an error if qt1
   and qt2's shapes are different, but instead equate all qualifiers
   from the erroneous level of qt1 (qt2) down. */
static bool mkXeq_qtype(location loc, qtype qt1, qtype qt2, mkXeq which,
			bool collapse);

/* Hacky function so we can work with type variables directly.  This
   function is never called if there are no constraints between type
   variables. */
static bool mkXeq_ty(location loc, ty t1, ty t2, mkXeq which, bool collapse)
{
  struct Qtype qt1, qt2;
  static qual missing_qual = NULL; /* Use so we can call mkXeq_qtype */

  if (missing_qual == NULL)
    missing_qual = make_fresh_qvar("q", NULL);
  qt1.qual = missing_qual;
  qt1.type = t1;
  qt1.conv = NULL;
  qt2.qual = missing_qual;
  qt2.type = t2;
  qt2.conv = NULL;
  return mkXeq_qtype(loc, &qt1, &qt2, which, collapse);
}

static bool mkXeq_qtype(location loc, qtype qt1, qtype qt2, mkXeq which,
			bool collapse)
{
  bool result = FALSE;
  ty t1, t2;

  qt1 = ecr_qtype(qt1);
  qt2 = ecr_qtype(qt2);
  if (qtype_error(qt1) || qtype_error(qt2))
    return FALSE;

  result = mkXeq_qual(qual_qtype(qt1), qual_qtype(qt2), which, loc) || result;

  t1 = ty_qtype(qt1);
  t2 = ty_qtype(qt2);

  if (t1 == t2)
    ;

  else if (t1->kind == tk_primitive && t2->kind == tk_primitive)
    ;

  else if (t1->kind == tk_void && t2->kind == tk_void)
    ;

  else if (t1->kind == tk_pointer && t2->kind == tk_pointer)
    {
      mkXeq new_which;

      if (flag_const_subtyping && const_qual &&
	  has_lb_qual(qual_qtype(qt2), const_qual))
	new_which = which;
      else
	new_which = toEq(which);

      /* Don't short-circuit */
      result = mkXeq_qtype(loc, t1->u.ptr.pointsto, t2->u.ptr.pointsto,
			   new_which, collapse) || result;

      /* Unify the aloc, *after* unifying pt qtype so the match passes if
	 possible */
      unify_aloc(loc, t1->u.ptr.aloc, t2->u.ptr.aloc);

      if (which == unify)
	result = unify_ty_alocs(t1, t2) || result;
    }

  else if (t1->kind == tk_pointer_loc && t2->kind == tk_pointer_loc)
    {
      assert(eq_aloc(t1->u.pointer_loc.aloc, t2->u.pointer_loc.aloc));
      if (which == unify)
	result = unify_ty_alocs(t1, t2) || result;
    }

  else if (t1->kind == tk_array && t2->kind == tk_array)
    {
      result = mkXeq_qtype(loc, t1->u.array.arrayof, t2->u.array.arrayof,
			   toEq(which), collapse) || result;

      /* Unify the aloc, *after* unifying arrayof qtype so the match passes if
	 possible */
      unify_aloc(loc, t1->u.array.aloc, t2->u.array.aloc);

      if (which == unify)
	result = unify_ty_alocs(t1, t2) || result;
    }

  else if ((t1->kind == tk_struct && t2->kind == tk_struct) ||
	   (t1->kind == tk_union && t2->kind == tk_union))
    {
      assert(!t1->u.tag.fs_ty && !t2->u.tag.fs_ty);
      if (collapse)
	{
	  /* Don't unify the structures.  Instead equate all the
	     qualiifers, which for now does nothing.  This loses
	     conversions between structures with common prefixes. */
	  result = (equate_all_quals_qtype(loc, qt1, qt1->qual) ||
		    equate_all_quals_qtype(loc, qt2, qt2->qual) ||
		    mkeq_qual(loc, qt1->qual, qt2->qual) ||
		    result);
	}
      else
	{
	  /* Both are well-defined structures */
	  ty new_ecr, new_link;
	  field ecr_fields, link_fields;
	  bool link_defined;
	  const char *kind, *ecr_name, *link_name;

	  if (!t2->u.tag.defined)
	    {
	      new_ecr = t1;
	      new_link = t2;
	    }
	  else if (!t1->u.tag.defined)
	    {
	      new_ecr = t2;
	      new_link = t1;
	    }
	  else if (t1->u.tag.num_equiv > t2->u.tag.num_equiv)
	    {
	      new_ecr = t1;
	      new_link = t2;
	    }
	  else
	    {
	      new_ecr = t2;
	      new_link = t1;
	    }

	  new_ecr->u.tag.defined |= new_link->u.tag.defined;
	  new_ecr->u.tag.transparent_union |= new_link->u.tag.transparent_union;
	  ecr_fields = new_ecr->u.tag.fields;
	  ecr_name = new_ecr->u.tag.name;
	  kind = (t1->kind == tk_struct) ? "struct" : "union";
	  link_fields = new_link->u.tag.fields;
	  link_defined = new_link->u.tag.defined;
	  link_name = new_link->u.tag.name;
	  new_ecr->u.tag.num_equiv += new_link->u.tag.num_equiv;
	  new_link->kind = tk_link;
	  new_link->u.link = new_ecr;

	  if (new_link->u.tag.defined)
	    {
	      while (ecr_fields && link_fields)
		{
		  /* XXX: Make this unify instead of mkeq? */
		  if (mkXeq_qtype(loc, ecr_fields->qtype, link_fields->qtype,
				  toEq(which), collapse))
		    {
		      result = TRUE;
		      if (growbuf_empty(qtype_error_detail))
			qtype_ed_printf(
                                "field `%s' of `%s %s' incompatible with "
				"field `%s' of `%s %s'",
				ecr_fields->name,
				kind,
				ecr_name,
				link_fields->name,
				kind,
				link_name);
		    }
		  ecr_fields = ecr_fields->next;
		  link_fields = link_fields->next;
		}
	      if ((ecr_fields || link_fields) && !collapse)
	      /* If there are any fields left over, then fail.  Exception:
	         if we're in collapse mode, we interpret that as meaning
	         the constraint comes from a cast.  In this case, be
	         liberal and allow the structures to not match so we
	         get some subtyping.  Note that this also allows unsound
	         supertyping, but that's life. */
		{
		  result = TRUE;
		  if (growbuf_empty(qtype_error_detail))
		    qtype_ed_printf("`%s %s' has %s fields than `%s %s'",
				    kind,
				    ecr_name,
				    ecr_fields ? "more" : "fewer",
				    kind,
				    link_name);
		}
	    }
	}
    }
  else if (t1->kind == tk_function && t2->kind == tk_function)
    {
      /* For function types, the issue is what to do when their numbers
	 of arguments don't match.  We follow the logic of
	 types.c/function_compatible. */

      qtypelist_scanner qs1, qs2;
      qtype arg1_qtype, arg2_qtype;

      /* If we're doing a flow-sensitive analysis, we need to always
	 unify the effects, since we unified the alocs (hence the
	 functions must have the same type) */
      mkXeq_effect(t1->u.fn.effect, t2->u.fn.effect,
		   flag_flow_sensitive ? unify : which);

      /* Covariant return */
      result = mkXeq_qtype(loc, t1->u.fn.returns, t2->u.fn.returns,
			   which, collapse) || result;

      qtypelist_scan(t1->u.fn.argtypes, &qs1);
      qtypelist_scan(t2->u.fn.argtypes, &qs2);

      /* Avoid short circuiting && */
      while ((arg1_qtype = qtypelist_next(&qs1),
	      arg2_qtype = qtypelist_next(&qs2),
	      arg1_qtype && arg2_qtype))
	/* Contravariant parameters */
	result = mkXeq_qtype(loc, arg2_qtype, arg1_qtype, which,
			     collapse) || result;

      if (!t1->u.fn.oldstyle && !t2->u.fn.oldstyle)
	{
	  if (collapse)
	    {
	      /* If we're collapsing, ignore the left-over arguments --
		 it doesn't really make sense to collapse arguments, since
	         you're not supposed to just from one to the other */
	      if (t1->u.fn.vqual && t2->u.fn.vqual)
		result = mkXeq_qual(t1->u.fn.vqual, t2->u.fn.vqual,
				    which, loc) || result;
	    }
	  else
	    {
	      /* For non old-style functions, the # of parameters must
		 be the same, and they both must be or not be varargs. */
	      result = (arg1_qtype || arg2_qtype) || result;
	      result = (t1->u.fn.varargs != t2->u.fn.varargs) || result;
	      if (t1->u.fn.vqual && t2->u.fn.vqual)
		result = mkXeq_qual(t1->u.fn.vqual, t2->u.fn.vqual,
				    which, loc) || result;
	      else
		/* Error if one of them is qualified but the other isn't */
		result = (t1->u.fn.vqual || t1->u.fn.vqual) || result;
	    }
	}

      /* o|-< */
      if (restrict_qual || flag_flow_sensitive)
	{
	  mkXeq_store(loc, t2->u.fn.in, t1->u.fn.in, which);
	  mkXeq_store(loc, t1->u.fn.out, t2->u.fn.out, which);
	}

      /* Unify the aloc, *after* unifying function qtypes so the match
	 passes if possible */
      unify_aloc(loc, t1->u.fn.aloc, t2->u.fn.aloc);

      if (which == unify)
	result = unify_ty_alocs(t1, t2) || result;
    }

  else if (t1->kind == tk_var && t2->kind != tk_var)
    {
      assert(!collapse);
      if (which == unify)
	{
	  ty_set lb, ub;
	  ty_set_scanner ss;
	  ty t;

	  lb = t1->u.var.lb;
	  ub = t1->u.var.ub;

	  /* Make qt1 an alias for qt2 */
	  t1->kind = tk_link;
	  t1->u.link = t2;

	  /* And add qt1's bounds to qt2. */
	  scan_ty_set(t, ss, lb)
	    result = mkXeq_ty(loc, t1, t2, mkleq, collapse) || result;
	  scan_ty_set(t, ss, ub)
	    result = mkXeq_ty(loc, t2, t1, mkleq, collapse) || result;
	  result = unify_ty_alocs(t1, t2) || result;
	}
      else
	{
	  ty newt;

	  newt = copy_ty(t2, NULL);
	  result = mkXeq_ty(loc, newt, t2, which, collapse) || result;
	  result = mkXeq_ty(loc, t1, newt, unify, collapse) || result;
	}
    }

  else if (t1->kind != tk_var && t2->kind == tk_var)
    {
      assert(!collapse);
      if (which == unify)
	/* Switch order, calling code above */
	result = mkXeq_ty(loc, t2, t1, unify, collapse) || result;
      else
	{
	  ty newt;

	  newt = copy_ty(t1, NULL);
	  result = mkXeq_ty(loc, t1, newt, which, collapse) || result;
	  result = mkXeq_ty(loc, t2, newt, unify, collapse) || result;
	}
    }

  else if (t1->kind == tk_var && t2->kind == tk_var)
    {
      /* Add edge.  No transitive constraints, because as soon as a
	 variable has a definite shape we use unify to resolve the
	 transitive constraints. */
      assert(!collapse);
      ty_set_insert(parse_region, &t1->u.var.ub, t2);
      ty_set_insert(parse_region, &t2->u.var.lb, t1);
      insistnot(unify_ty_alocs(t1, t2));
    }

  else if (collapse)
    {
      /* See above case for struct mkXeq */
      result = (equate_all_quals_qtype(loc, qt1, qt1->qual) ||
		equate_all_quals_qtype(loc, qt2, qt2->qual) ||
		mkeq_qual(loc, qt1->qual, qt2->qual) ||
		result);
    }
  else
    /* Invalid constraint */
    result = TRUE;

  /* Else assume detail was added above */

  return result;
}

/* Calls mkXeq_qtype and reports any errors.  s is only used to make
   nicer error messages for third pass. */
bool mkXeq_qtype_err(location loc, qtype qt1, qtype qt2, mkXeq which,
		     bool collapse, store s)
{
  bool result;

#ifdef DEBUG
  print_qtype(printf, qt1, s);
  if (which == mkleq) printf(" <= ");
  else if (which == mkeq) printf(" = ");
  else if (which == unify) printf(" == ");
  else printf(" ??op?? ");
  print_qtype(printf, qt2, s);
  printf("\n");
#endif

  result = mkXeq_qtype(loc, qt1, qt2, which, collapse);

  if (result && growbuf_empty(qual_error_detail) &&
      growbuf_empty(qtype_error_detail))
    {
      qtype_ed_printf("`");
      print_ty_structure(qtype_ed_printf, qt1->type, s);
      qtype_ed_printf("' incompatible with `");
      print_ty_structure(qtype_ed_printf, qt2->type, s);
      qtype_ed_printf("'");
    }

#ifdef DEBUG
  if (result) {
    fflush(NULL);
    printf("\nInvalid constraint ");
    print_qtype_raw(printf, qt1, s);
    if (which == mkleq)
      printf(" <= ");
    else if (which == mkeq && !collapse)
      printf(" = ");
    else if (which == mkeq && collapse)
      printf(" (cast)= ");
    else if (which == unify)
      printf(" == ");
    else
      printf(" \?\?= ");
    print_qtype_raw(printf, qt2, s);
    printf("\n");
  }
#endif


  return result;
}

/* qt1 <= qt2 */
bool mkleq_qtype(location loc, qtype qt1, qtype qt2)
{
  return mkXeq_qtype_err(loc, qt1, qt2, mkleq, FALSE, NULL);
}

/* qt1 = qt2 */
bool mkeq_qtype(location loc, qtype qt1, qtype qt2)
{
  return mkXeq_qtype_err(loc, qt1, qt2, mkeq, FALSE, NULL);
}

/* qt1 = qt2, plus cannot distinguish qt1 and qt2 any more */
bool unify_qtype(location loc, qtype qt1, qtype qt2)
{
  return mkXeq_qtype_err(loc, qt1, qt2, unify, FALSE, NULL);
}

/* Equate the qualifiers on types qt1 and qt2.  For the levels on
   which qt1 and qt2 have the same shape, the qualifiers are equated
   level by level.  After that point, all qualifiers on the remains
   levels are equated. */
bool mkeq_qtype_cast(location loc, qtype qt1, qtype qt2)
{
  return mkXeq_qtype_err(loc, qt1, qt2, mkeq, TRUE, NULL);
}

bool lub_qtype(location loc, qtype qt1, qtype qt2, qtype *lub)
{
  bool result;

  *lub = copy_qtype(qt1, loc);
  result = mkleq_qtype(loc, qt1, *lub);
  result = mkleq_qtype(loc, qt2, *lub) || result;
  return result;
}

/* Return TRUE iff left and right are the same.  Does not generate
   constraints. */
bool eq_ty(ty t1, ty t2)
{
  return (ecr_ty(t1) == ecr_ty(t2));
}

/* A total ordering on types.  Returns 0 if t1 = t2, a value <0
   if t1 < t2, or a value >0 if t1 > t2. */
int cmp_ty(ty t1, ty t2)
{
  t1 = ecr_ty(t1);
  t2 = ecr_ty(t2);
  return (int) t1 - (int) t2;
}

/* Return TRUE iff left and right are the same.  Does not generate
   constraints. */
bool eq_qtype(qtype qt1, qtype qt2)
{
  return (eq_qual(qt1->qual, qt2->qual) &&
	  eq_ty(qt1->type, qt2->type));
}

/* Apply f(q, arg) for all qualifiers q of qt. */
void traverse_quals_qtype(qtype qt, void (*f)(qual q, void *arg), void *arg)
{
  ty t;

  qt = ecr_qtype(qt);
  f(qt->qual, arg);
  t = qt->type;

  switch(t->kind)
    {
    case tk_primitive:
    case tk_void:
      break;
    case tk_pointer:
      traverse_quals_qtype(t->u.ptr.pointsto, f, arg);
      break;
    case tk_pointer_loc:
      fail("Unimplemented\n");
      break;
    case tk_array:
      traverse_quals_qtype(t->u.array.arrayof, f, arg);
      break;
    case tk_struct:
    case tk_union:
      /*
      {
	tag_declaration tdecl;
	field_declaration fd;

	tdecl = find_tag_declaration(t->u.tag.tdecl);
	if (tdecl->defined)
	  {
	    if (!tdecl->being_defined)
	      {
		tdecl->being_defined = TRUE;
		for (fd = tdecl->fieldlist; fd; fd = fd->next)
		  traverse_quals_qtype(fd->qtype, f, arg);
		tdecl->being_defined = FALSE;
	      }
	  }
	else
	  result = TRUE;     Collapse fields of undefined structure
      }
      */
      break;
    case tk_function:
      /* XXX: What to do here? */
      break;
    default:
      fail("Unexpected ty kind %x\n", t->kind);
    }
}

typedef struct eqtq_info
{
  qual q;
  bool result;
  location loc;
} *eqtq_info;

void equate_traverse_fn(qual q, void *arg)
{
  eqtq_info eqtqi = (eqtq_info) arg;
  eqtqi->result = mkeq_qual(eqtqi->loc, q, eqtqi->q) || eqtqi->result;
}

/* Add equality constraints bewteen all qualifiers of qt and qualifier
   q. */
bool equate_all_quals_qtype(location loc, qtype qt, qual q)
{
  struct eqtq_info eqtqi = {q, FALSE, loc};

  traverse_quals_qtype(qt, equate_traverse_fn, &eqtqi);
  return eqtqi.result;
}

typedef struct vtq_info
{
  qual q;
  bool result;
  int num;
  location loc;
} *vtq_info;

/* Make a copy of tqi->q with constraints and equate q with it. */
void varargs_traverse_fn(qual q, void *arg)
{
  vtq_info vtqi = (vtq_info) arg;
  qual newq;
  qual_edge_set_scanner qs;
  qual_edge qe;

  newq = make_fresh_qvar(name_qual(q), location_qual(vtqi->q));

  /* Copy the bounds on ctqi->q over to newq */
  scan_qual_edge_set(qe, qs, lb_qual(vtqi->q))
    vtqi->result = mkleq_qual(qe->loc, qe->qual, newq) || vtqi->result;
  scan_qual_edge_set(qe, qs, ub_qual(vtqi->q))
    vtqi->result = mkleq_qual(qe->loc, newq, qe->qual) || vtqi->result;

  /* Equate newq and q */
  vtqi->result = mkeq_qual(vtqi->loc, newq, q) || vtqi->result;
}

/* Add constraints bewteen all qualifiers of qt and the varargs
   qualifier q.  For each qualifier q' of qt, a copy of q is made
   sharing q's constraints, and then the copy is equated to q'.
   Returns TRUE if an error occurs. */
bool varargs_constrain_quals_qtype(location loc, qtype qt, qual q)
{
  struct vtq_info vtqi = {q, FALSE, 0, loc};
  traverse_quals_qtype(qt, varargs_traverse_fn, &vtqi);
  return vtqi.result;
}

/* Add constraint e \not\in qt */
void mknin_effect_qtype(location loc, effect e, qtype qt)
{
  effect eff;

  eff = alocs_qtype(qt);

  mknin_effect(loc, e, eff);
}

/**************************************************************************
 *                                                                        *
 * Conversions                                                            *
 *                                                                        *
 **************************************************************************/

/* Convert the qtype as appropriate for appearing in a generic expr
   context.  The logic is duplicated from types.c. */
qtype default_conversion_qtype(qtype qt)
{

  qt = ecr_qtype(qt);
  if (!qt->conv)
    {
      /* Don't care about these cases.
	 if (qtype_enum(qt))
	 {
	 qtype new_qtype = ralloc(parse_region, struct Qtype);
	 
	 new_qtype->type->kind = tk_primitive;
	 new_qtype->qual = qt->qual;
	 new_qtype->type->u.primitive = tp_int;
	 return new_qtype;
	 }
	 
	 if (qtype_smallerthanint(qt))
	 {
	 qtype new_qtype = ralloc(parse_region, struct Qtype);
	 
	 new_qtype->type->kind = tk_primitive;
	 new_qtype->qual = qt->qual;
	 
	 if (flag_traditional && qtype_unsigned(qt))
	 new_qtype->type->u.primitive = tp_unsigned_int;
	 else
	 new_qtype->type->u.primitive = tp_int;
	 return new_qtype;
	 }
	 
	 if (flag_traditional && !flag_allow_single_precision &&
	     qtype_float(qt))
	 {
	 qtype new_qtype = ralloc(parse_region, struct Qtype);
	 
	 new_qtype->type->kind = tk_primitive;
	 new_qtype->qual = qt->qual;
	 new_qtype->type->u.primitive = tp_double;
	 return new_qtype;

	 }
      */
      if (qtype_function(qt))
	{
	  qual q;

	  q = make_qvar("<fp>", NULL, FALSE);
	  if (const_qual)
	    mkleq_qual(NULL, const_qual, q);
	  
	  qt->conv = mkqtype_pointer(q, qt, qt->type->u.fn.aloc);
	}
      else if (qtype_array(qt))
	qt->conv = mkqtype_pointer(qt->qual,
				   qt->type->u.array.arrayof,
				   qt->type->u.array.aloc);
      else
	qt->conv = qt;
    }

  return qt->conv;
}

/* rqt <= lqt, plus any necessary conversions

   conversions performed are

   * If either lhs or rhs is a void * and the other is any pointer qtype,
     convert the other to a void *.

   * If either lhs or rhs is an integer and the other is any pointer
     qtype, convert the other to an integer.  (This is for conversion
     from pointer to integer, esp. 0.)

   Note that we do not allow nested conversions.  (E.g., char ** is
   not compatible with void **.)

   We don't worry about converting primitive types, because we don't
   check for that class of errors.

   Returns TRUE if an error occurs.
 */
static bool internal_mkleq_assign_convert(location loc, qtype rqt, qtype lqt,
					  store s)
{
  /* NOTE:  Be sure to also make any changes to mkleq_fs_assign_convert */
  bool result;
  
  if ((qtype_void_pointer(lqt) && qtype_non_void_pointer(rqt)) ||
      (qtype_non_void_pointer(lqt) && qtype_void_pointer(rqt)))
    {
      result = mkleq_qual(loc, qual_qtype(rqt), qual_qtype(lqt));
      if (flag_casts_preserve)
	result = mkeq_qtype_cast(loc, points_to_qtype(rqt),
				 points_to_qtype(lqt)) || result;
    }
  else if ((qtype_void_pointer_loc(lqt) && qtype_non_void_pointer_loc(rqt)) ||
	   (qtype_non_void_pointer_loc(lqt) && qtype_void_pointer_loc(rqt)))
    {
      result = mkleq_qual(loc, qual_qtype(rqt), qual_qtype(lqt));
      if (flag_casts_warn)
	{
	  mk_no_qual_qtype_fs(loc, points_to_fs_qtype(lqt, s), s);
	  mk_no_qual_qtype_fs(loc, points_to_fs_qtype(rqt, s), s);
	}
    }
  else if ((qtype_pointer_or_loc(lqt) && qtype_integral(rqt)) ||
	   (qtype_integral(lqt) && qtype_pointer_or_loc(rqt)))
    {
      if (flag_casts_preserve)
	result = mkeq_qtype_cast(loc, rqt, lqt);
      else
	result = mkleq_qual(loc, qual_qtype(rqt), qual_qtype(lqt));
    }
  else if (qtype_transparent(lqt) && !match_qtype(lqt, rqt, NULL, NULL))
    {
      field_scanner fields;
      qtype fqt;
      qtype candidate, marginal_candidate;

      /* Looks like we're assigning into a transparent union. */
      candidate = NULL;
      marginal_candidate = NULL;
      field_scan(lqt, &fields);
      while ((fqt = field_next(&fields)))
	{
	  qtype frqt;

	  if (qtype_array(fqt) || qtype_function(fqt))
	    frqt = fqt;
	  else if (qtype_pointer(fqt))
	    frqt = points_to_qtype(fqt);
	  else if (qtype_pointer_loc(fqt))
	    frqt = points_to_fs_qtype(fqt, s);
	  else
	    fail("Unexpected field qtype\n");

	  if (match_qtype(frqt, rqt, NULL, NULL))
	    {
	      candidate = frqt;
	      break;
	    }

	  if ((qtype_void_pointer(frqt) && qtype_pointer(rqt)) ||
	      (qtype_pointer(frqt) && qtype_void_pointer(rqt)) ||
	      (qtype_void_pointer_loc(frqt) && qtype_pointer_loc(rqt)) ||
	      (qtype_pointer_loc(frqt) && qtype_void_pointer_loc(rqt)) ||
	      (qtype_pointer_or_loc(frqt) && qtype_integral(rqt)) ||
	      (qtype_integral(frqt) && qtype_pointer_or_loc(rqt)))
	    marginal_candidate = frqt;
	}
      if (!candidate && marginal_candidate)
	result = internal_mkleq_assign_convert(loc,rqt,marginal_candidate, s);
      else if (candidate)
	result = internal_mkleq_assign_convert(loc, rqt, candidate, s);
      else
	result = mkXeq_qtype_err(loc, rqt, lqt, mkleq, FALSE, s);
    }
  else
    result = mkXeq_qtype_err(loc, rqt, lqt, mkleq, FALSE, s);
  
  return result;
}

/* rhs_qtype <= lhs_qtype, plus any necessary conversions */
bool mkleq_assign_convert(location loc, qtype rhs_qtype, qtype lhs_qtype)
{
  return internal_mkleq_assign_convert(loc, rhs_qtype, lhs_qtype, NULL);
}


/**************************************************************************
 *                                                                        *
 * Printing                                                               *
 *                                                                        *
 **************************************************************************/

/* Print a type; pr_qual is the function to use for printing the
   qualifiers. */
int print_ty_qf(printf_func pf, pr_qual_fn pr_qual, ty t, store s,
		bool print_alocs)
{
  int result;
  
  result = 0; /* Count of printed chars */
  t = ecr_ty(t);
  
  switch (t->kind)
    {
    case tk_complex: result += pf("complex "); /* no break */
    case tk_primitive:
      switch (t->u.primitive)
	{
	case tp_char:               result += pf("char"); break;
	case tp_signed_char:        result += pf("signed char"); break;
	case tp_unsigned_char:      result += pf("unsigned char"); break;
	case tp_short:              result += pf("short"); break;
	case tp_unsigned_short:     result += pf("unsigned short"); break;
	case tp_int:                result += pf("int"); break;
	case tp_unsigned_int:       result += pf("unsigned int"); break;
	case tp_long:               result += pf("long"); break;
	case tp_unsigned_long:      result += pf("unsigned long"); break;
	case tp_long_long:          result += pf("long long"); break;
	case tp_unsigned_long_long: result += pf("unsigned long long"); break;
	case tp_float:              result += pf("float"); break;
	case tp_double:             result += pf("double"); break;
	case tp_long_double:        result += pf("long double"); break;
	default: fail("Unexpected primite %d\n", t->u.primitive);
	};
      break;
    case tk_struct:
      result += pf("struct");
      if (t->u.tag.name)
	result += pf(" %s", t->u.tag.name);
      break;
    case tk_union:
      result += pf("union");
      if (t->u.tag.name)
	result += pf(" %s", t->u.tag.name);
      break;
    case tk_void: result += pf("void"); break;
    case tk_pointer:
      result += pf("ptr");
      if (print_alocs)
	{
	  result += pf("@");
	  result += print_aloc(pf, t->u.ptr.aloc);
	}
      result += pf(" (");
      result += print_qtype_qf(pf, pr_qual, t->u.ptr.pointsto, s, print_alocs);
      result += pf(")");
      break;
    case tk_pointer_loc:
      {
	aloc al;
	qtype qpt;

	al = t->u.pointer_loc.aloc;
	if (s)
	  {
	    result += pf("ptr");
	    if (print_alocs)
	      {
		result += pf("@");
		result += print_aloc(pf, al);
	      }
	    qpt = qtype_from_store(s, al);
	    if (qpt)
	      {
		result += pf(" (");
		result += print_qtype_qf(pf, pr_qual, qpt, s, print_alocs);
		result += pf(")");
	      }
	  }
	else
	  {
	    result += pf("ptr@");
	    result += print_aloc(pf, al);
	  }
      }
      break;
    case tk_function: 
      {
	qtypelist_scanner elt;
	qtype qt_arg;
	int first = TRUE;
	
	result += pf("fun (");
	qtypelist_scan(t->u.fn.argtypes, &elt);
	while ((qt_arg = qtypelist_next(&elt)))
	  {
	    if (!first)
	      result += pf(", ");
	    result += print_qtype_qf(pf, pr_qual, qt_arg, t->u.fn.in,
				     print_alocs);
	    first = FALSE;
	  }
	if (!t->u.fn.argtypes && !t->u.fn.oldstyle)
	  result += pf("void");
	
	if (t->u.fn.varargs)
	  {
	    if (!first)
	      result += pf(", ");
	    if (t->u.fn.vqual)
	      {
		int vq_len = pr_qual(pf, t->u.fn.vqual);
		result += vq_len;
		if (vq_len)
		  result += pf(" ");
	      }
	    result += pf("...");
	  }
	result += pf(") -> ");
	result += print_qtype_qf(pf, pr_qual, t->u.fn.returns,
				 t->u.fn.out, print_alocs);

	if (flag_print_stores)
	  {
	    result += pf("  [");
	    result += print_store_cells(pf, pr_qual, t->u.fn.in, print_alocs);
	    result += pf(" -> ");
	    result += print_store_cells(pf, pr_qual, t->u.fn.out, print_alocs);
	    result += pf("]");
	  }
      };
      break;
    case tk_array:
      result += pf("array (");
      result += print_qtype_qf(pf, pr_qual, t->u.array.arrayof, s,
			       print_alocs);
      result += pf(")");
      break;
    case tk_error:
      result += pf("err");
      break;
    case tk_var:
      result += pf(t->u.var.name);
      break;
    case tk_scheme:
      {
	ty_set_scanner ss;
	qual_set_scanner qs;
	ty bound_t;
	qual bound_q;

	/* Print type schemes in raw format; otherwise they don't make
	   sense */
	result += pf("forall ");
	scan_ty_set(bound_t, ss, t->u.scheme.tyvars)
	  {
	    result += print_ty_qf(pf, pr_qual, bound_t, s, print_alocs);
	    result += pf(" ");
	  }
	scan_qual_set(bound_q, qs, t->u.scheme.qvars)
	  {
	    result += print_qual_raw(pf, bound_q);
	    result += pf(" ");
	  }
	result += pf(". ");
	result += print_qtype_qf(pf, print_qual_raw, t->u.scheme.body, s,
				 FALSE);
      }
      break;
    default:
      fail("Unexpected kind %d for qtype\n", t->kind);
    }
  return result;
}

/* Print a qualified type; pr_qual is the function to use for printing the
   qualifiers. */
int print_qtype_qf(printf_func pf, pr_qual_fn pr_qual, qtype qt, store s,
		   bool print_alocs)
{
  int result;
  
  result = 0; /* Count of printed chars */
  qt = ecr_qtype(qt);
  
  /* Print the qualifiers first, followed by a space if something was
     printed.  There's no qualifier on a type scheme (make sure it's
     NULL) */
  if (qtype_error(qt) || qtype_scheme(qt))
    assert(qt->qual == NULL);
  else if ((result += pr_qual(pf, qt->qual)))
    result += pf(" ");
  result += print_ty_qf(pf, pr_qual, qt->type, s, print_alocs);
  return result;
}

/* Print a qualified type, but make the qualifiers ugly. */
int print_qtype_raw(printf_func pf, qtype qt, store s)
{
  return print_qtype_qf(pf, print_qual_raw, qt, s, TRUE);
}

/* Print a qualified type, and make the qualifiers look nice. */
int print_qtype(printf_func pf, qtype qt, store s)
{
  return print_qtype_qf(pf, print_qual, qt, s, FALSE);
}

/* Print t without qualifiers */
int print_ty_structure(printf_func pf, ty t, store s)
{
  int result;

  result = 0; /* Count of printed chars */
  t = ecr_ty(t);
  
  switch (t->kind)
    {
    case tk_complex:
    case tk_primitive:
    case tk_struct:
    case tk_union:
    case tk_void:
    case tk_error:
    case tk_var:
      result += print_ty_qf(pf, NULL, t, s, FALSE);
      break;
      result += pf("struct");
      if (t->u.tag.name)
	result += pf(" %s", t->u.tag.name);
      break;
    case tk_pointer:
      result += pf("ptr (");
      result += print_ty_structure(pf, t->u.ptr.pointsto->type, s);
      result += pf(")");
      break;
    case tk_pointer_loc:
      if (s)
	{
	  result += pf("ptr (");
	  result +=
	    print_ty_structure(pf,
			       qtype_from_store(s, t->u.pointer_loc.aloc)->type,
			       s);
	  result += pf(")");
	}
      else
	{
	  result += pf("ptr@");
	  result += print_aloc(pf, t->u.pointer_loc.aloc);
	}
      break;
    case tk_function: 
      {
	qtypelist_scanner elt;
	qtype qt_arg;
	int first = TRUE;
	
	result += pf("fun (");
	qtypelist_scan(t->u.fn.argtypes, &elt);
	while ((qt_arg = qtypelist_next(&elt)))
	  {
	    if (!first)
	      result += pf(", ");
	    result += print_ty_structure(pf, qt_arg->type, t->u.fn.in);
	    first = FALSE;
	  }

	if (t->u.fn.varargs)
	  {
	    if (!first)
	      result += pf(", ");
	    result += pf("...");
	  }
	result += pf(") -> ");
	result += print_ty_structure(pf, t->u.fn.returns->type,
				     t->u.fn.out);
      };
      break;
    case tk_array:
      result += pf("array (");
      result += print_ty_structure(pf, t->u.array.arrayof->type, s);
      result += pf(")");
      break;
    default:
      fail("Unexpected kind %d for qtype\n", t->kind);
    }
  return result;
}

/* Return the color markup for a qualified type */
const char *color_qtype(qtype qt, store s)
{
  const char *cur_color;
  
  qt = ecr_qtype(qt);
  if (qtype_scheme(qt) || qtype_error(qt))
    {
      assert(qt->qual == NULL); /* No qualifier on type scheme */
      return NULL;
    }
  else
    cur_color = color_qual(qt->qual);
  switch (qt->type->kind)
    {
    case tk_complex:
    case tk_primitive:
    case tk_void:
    case tk_error:
    case tk_var:
      break;
    case tk_struct:
    case tk_union:
      /* Don't color based on structure fields */
      break;
    case tk_pointer:
      cur_color = combine_colors_pam(cur_color,
				     color_qtype(qt->type->u.ptr.pointsto, s));
      break;
    case tk_pointer_loc:
      if (s)
	{
	  qtype points_to;

	  points_to = points_to_fs_qtype(qt, s);
	  if (points_to)
	    cur_color = combine_colors_pam(cur_color,
					   color_qtype(points_to, s));
	}
      break;
    case tk_function:
      /* Don't color based on function parameters/return */
      break;
    case tk_array:
      cur_color = combine_colors_pam(cur_color,
				     color_qtype(qt->type->u.array.arrayof,s));
      break;
    default:
      fail("Unexpected kind %d for qtype\n", qt->type->kind);
    }

  return cur_color;
}

/* Used by nontriv_soln_qtype */
void nontriv_soln_fn(qual q, bool *result)
{
  *result = TRUE;
}

/* TRUE iff at least one of qt's qualifiers has a non-trivial
   solution */
bool nontriv_soln_qtype(qtype qt, store s)
{
  bool result;

  result = FALSE;
  qt = ecr_qtype(qt);
  if (qtype_error(qt) || qtype_scheme(qt))
    return TRUE;
  scan_qual_bounds(qt->qual, (qual_traverse_fn) nontriv_soln_fn, &result);
  switch (qt->type->kind)
    {
    case tk_complex:
    case tk_primitive:
    case tk_void:
    case tk_error:
    case tk_var:
      break;
    case tk_struct:
    case tk_union:
      if (!qt->type->u.tag.mark)
	{
	  field f;
	  
	  qt->type->u.tag.mark = TRUE;
	  for (f = qt->type->u.tag.fields; f; f = f->next)
	    result = result || nontriv_soln_qtype(f->qtype, s);
	  qt->type->u.tag.mark = FALSE;
	}
      break;
    case tk_pointer:
      result = result || nontriv_soln_qtype(points_to_qtype(qt), s);
      break;
    case tk_pointer_loc:
      if (s)
	{
	  qtype points_to;

	  points_to = points_to_fs_qtype(qt, s);
	  if (points_to)
	    result = result || nontriv_soln_qtype(points_to, s);
	}
      break;
    case tk_function:
      {
	qtypelist_scanner qtls;
	qtype arg_qt;

	qtypelist_scan(arg_qtypes(qt), &qtls);
	while ((arg_qt = qtypelist_next(&qtls)))
	  result = result || nontriv_soln_qtype(arg_qt, store_in_qtype(qt));
	result = result || nontriv_soln_qtype(return_qtype(qt),
					      store_out_qtype(qt));
	if (!result && qtype_varargs(qt))
	  {
	    qual vqual = vqual_qtype(qt);

	    if (vqual)
	      scan_qual_bounds(vqual,
			       (qual_traverse_fn) nontriv_soln_fn,
			       &result);
	  }
      }
      break;
    case tk_array:
      result = result || nontriv_soln_qtype(array_of_qtype(qt), s);
      break;
    default:
      fail("Unexpected kind %d for qtype\n", qt->type->kind);
    }
  return result;
}


/**************************************************************************
 *                                                                        *
 * For second analysis pass for flow-sensitive qualifiers                 *
 *                                                                        *
 **************************************************************************/

/* Create a ref() store corresponding to allocating qt in store in */
store ref_qtype_store(location loc, qtype qt, store in)
{
  qt = ecr_qtype(qt);
  switch (qt->type->kind)
    {
    case tk_pointer:
      {
	qtype pto;
	store cur_store;

	cur_store = in;
	pto = ecr_qtype(qt->type->u.ptr.pointsto);
	cur_store = make_store_ref(loc, cur_store, qt->type->u.ptr.aloc);

	if (qtype_aggregate(qt->type->u.ptr.pointsto))
	  {
	    field_scanner fs;
	    qtype qt_field;
	    field_scan(qt->type->u.ptr.pointsto, &fs);
	    while ((qt_field = field_next(&fs)))
	      cur_store = ref_qtype_store(loc, qt_field, cur_store);
	  }

	/* checks for restrict */
	/* Don't do this here -- do it in flow.c explicitly.
	if (restrict_qual &&
	    pto->type->kind == tk_pointer &&
	    has_qual_qtype(pto, restrict_qual) &&
	    const_qual &&
	    has_qual_qtype(qt, const_qual))
	  in = make_store_ref(loc, in, pto->type->u.ptr.aloc);
	*/
	return cur_store;
      }
    case tk_pointer_loc:
      return make_store_ref(loc, in, qt->type->u.pointer_loc.aloc);
    case tk_array:
      {
	store cur_store;
	
	cur_store = in;
	cur_store = make_store_ref(loc, cur_store, qt->type->u.array.aloc);
	cur_store = make_store_ref(loc, cur_store, qt->type->u.array.aloc);
	if (qtype_array(qt->type->u.array.arrayof))
	  cur_store = ref_qtype_store(loc, qt->type->u.array.arrayof, cur_store);
	else if (qtype_aggregate(qt->type->u.array.arrayof))
	  {
	    field_scanner fs;
	    qtype qt_field;
	    field_scan(qt->type->u.array.arrayof, &fs);
	    while ((qt_field = field_next(&fs)))
	      cur_store = ref_qtype_store(loc, qt_field, cur_store);
	  }
	return cur_store;
      }
    case tk_function:
      return make_store_ref(loc, in, qt->type->u.fn.aloc);
    default:
      return in;
    }
}


/**************************************************************************
 *                                                                        *
 * For third analysis pass for flow-sensitive qualifiers                  *
 *                                                                        *
 **************************************************************************/

/* Convert qt to a new flow-sensitive qtype, this level only. */
qtype qtype_to_fs_qtype(location loc, qtype qt)
{
  const char *new_name;
  qual new_qual;
  qtype result;

  qt = ecr_qtype(qt);
  if (qtype_error(qt))
    return error_qtype;
  if (qtype_scheme(qt)) {
    report_qerror(loc, sev_warn,
		  "warning: ignoring polymorphic quals in flow-sensitive analysis");
    return qtype_to_fs_qtype(loc, qt->type->u.scheme.body);
  }

  /*
  if (loc)
    new_name = rstrscat(parse_region, name_qual(qt->qual),
			"_line_",
			inttostr(parse_region, loc->lineno),
			NULL);
  else
  */
  new_name = name_qual(qt->qual);
  new_qual = make_qvar(new_name, loc, preferred_qual(qt->qual));

  /* Compute new qtype */
  switch (qt->type->kind)
    {
    case tk_primitive:
    case tk_complex:
    case tk_void:
    case tk_error:
      result = ralloc(parse_region, struct Qtype);
      result->qual = new_qual;
      result->type = qt->type;
      result->conv = NULL;
      break;
    case tk_pointer:
      result = mkqtype_pointer_loc(new_qual, aloc_qtype(qt));
      break;
    case tk_array:
      result = mkqtype_array(new_qual,
			     qtype_to_fs_qtype(loc, array_of_qtype(qt)),
			     array_size_qtype(qt),
			     aloc_qtype(qt),
			     array_aloc_qtype(qt));
      break;
    case tk_struct:
    case tk_union:
      {
	/* XXX: This is weird -- all structure types share fields
	   qualifiers, even though the qualifiers on the field contents
	   can differ, since this is a flow-sensitive analysis. ?! */
	if (qt->type->u.tag.fs_ty)
	  {
	    result = ralloc(parse_region, struct Qtype);
	    result->qual = new_qual;
	    result->type = qt->type->u.tag.fs_ty;
	    result->conv = NULL;
	  }
	else
	  {
	    field f;
	
	    /* Make a qtype with empty fields */
	    result = mkqtype_tagged(new_qual,
				    qt->type->kind,
				    qt->type->u.tag.name,
				    qt->type->u.tag.defined,
				    qt->type->u.tag.transparent_union,
				    NULL);

	    assert(!result->type->u.tag.fields);
	    for (f = qt->type->u.tag.fields; f; f = f->next)
	      {
		field fs_f;

		/* Don't need to worry about recursion, since that can
		   only be through pointers. */
		fs_f = mkfield(f->name, qtype_to_fs_qtype(loc, f->qtype));
		field_append(&result->type->u.tag.fields, fs_f);
	      }
	    qt->type->u.tag.fs_ty = result->type;
	  }
      }
      break;
    case tk_function:
      {
	qtype fs_qtype_returns;
	qtypelist fs_qtype_argtypes;

	fs_qtype_returns = qtype_to_fs_qtype(loc, qt->type->u.fn.returns);
	fs_qtype_argtypes = qtypelist_to_fs_qtypelist(loc,
						      qt->type->u.fn.argtypes);

	result = mkqtype_function(new_qual,
				  qt->type->u.fn.varargs,
				  NULL, /* XXX: no vquals for now */
				  qt->type->u.fn.oldstyle,
				  fs_qtype_returns,
				  fs_qtype_argtypes,
				  qt->type->u.fn.in,
				  qt->type->u.fn.out,
				  qt->type->u.fn.aloc,
	                          qt->type->u.fn.effect);
      }
      break;
    default:
      fail("Unexpected qtype %x in qtype_to_fs_qtype\n", qt->type->kind);
    }

  return result;
}

/* As above */
static qtypelist qtypelist_to_fs_qtypelist(location loc, qtypelist qtl)
{
  qtypelist_scanner qs;
  qtype qt;
  qtypelist result;

  result = NULL;
  qtypelist_scan(qtl, &qs);
  while ((qt = qtypelist_next(&qs)))
    qtypelist_append(&result, qtype_to_fs_qtype(loc, qt));

  return result;
}

/* Transfer any constant fs qual bounds from old_qual to new_qual */
static void transfer_fs_qual(qual old_qual, qual new_qual)
{
  qual_edge_set_scanner qes;
  qual_edge qe;

  scan_qual_edge_set(qe, qes, lb_qual(old_qual))
    if (constant_qual(qe->qual) && flow_sensitive_qual(qe->qual))
      {
	if (mkleq_qual(qe->loc, qe->qual, new_qual))
	  report_qerror(qe->loc, sev_err,
			"inconsistent qualifier annotations");
      }
  scan_qual_edge_set(qe, qes, ub_qual(old_qual))
    if (constant_qual(qe->qual) && flow_sensitive_qual(qe->qual))
      {
	if (mkleq_qual(qe->loc, new_qual, qe->qual))
	  report_qerror(qe->loc, sev_err,
			"inconsistent qualifier annotations");
      }
}

/* Transfer any constant fs qual bounds from old_qt to new_qt */
static bool transfer_fs_qtype(location loc, qtype old_qt, qtype new_qt,
			      store s)
{
  bool result;
  qual old_qual, new_qual;

  /* Skip over any type schemes */
  if (qtype_scheme(old_qt))
    return transfer_fs_qtype(loc, old_qt->type->u.scheme.body, new_qt, s);

  result = FALSE;
  if (qtype_error(old_qt) || qtype_error(new_qt))
    return result;

  old_qt = ecr_qtype(old_qt);
  old_qual = old_qt->qual;

  /* HACK: It's really an error if new_qt is NULL, but we can let it slide
     as long as new_qt's qualifier isn't actually used anywhere. */
  if (new_qt)
    {
      new_qt = ecr_qtype(new_qt);
      new_qual = new_qt->qual;
    }
  else
    new_qual = NULL;

  /* Transfer over top-level qualifier */
  transfer_fs_qual(old_qual, new_qual);

  switch (old_qt->type->kind)
    {
    case tk_primitive:
    case tk_complex:
    case tk_void:
    case tk_error:
      break;
    case tk_pointer:
      {
        /* HACK:  If new_qt is NULL (see above), then assume it was
           actually a pointer_loc */
	if (!new_qt || qtype_pointer_loc(new_qt))
	  /* This should always hold, unless their was an error in the
	     first pass */
	  {
	    aloc al;
	    
	    al = aloc_qtype(old_qt);
	    result = transfer_fs_qtype(loc, points_to_qtype(old_qt),
				       qtype_from_store(s, al), s) || result;
	  }
      }
      break;
    case tk_array:
      {
	/* XXX: Don't use unify? */
	qtype qtype_in_store;

	result = transfer_fs_qtype(loc, array_of_qtype(old_qt),
				   array_of_qtype(new_qt), s) || result;
	qtype_in_store = qtype_from_store(s, aloc_qtype(new_qt));
        if (qtype_in_store)
	  /* HACK: might be NULL (see above), in which case there's no
	     work to do */
	  result = unify_qtype(loc, qtype_in_store, array_of_qtype(new_qt))
	    || result;
      }
      break;
    case tk_struct:
    case tk_union:
      {
	/* DO NOTHING 
	field new_f, old_f;

	if (new_qt->type->u.tag.mark)
	  return;

	new_qt->type->u.tag.mark = TRUE;
	new_f = new_qt->type->u.tag.fields;
	old_f = old_qt->type->u.tag.fields;
	while (new_f && old_f)
	  {
	    transfer_fs_qtype(old_f->qtype, new_f->qtype, s);
	    new_f = new_f->next;
	    old_f = old_f->next;
	  }
	assert(!new_f && !old_f);
	new_qt->type->u.tag.mark = FALSE;
	*/
      }
      break;
    case tk_function:
      {
	result = transfer_fs_qtype(loc, return_qtype(old_qt),
				   return_qtype(new_qt),
				   store_out_qtype(new_qt)) || result;
	result = transfer_fs_qtypelist(loc, arg_qtypes(old_qt), 
				       arg_qtypes(new_qt),
				       store_in_qtype(new_qt)) || result;
	/* Don't need -- new_qt was from the store
	qtype_in_store = qtype_from_store(s, aloc_qtype(new_qt));
	result = unify_qtype(loc, qtype_in_store, new_qt) || result;
	*/
      }
      break;
    default:
      fail("Unexpected qtype %x in transfer_fs_qtype\n", old_qt->type->kind);
    }
  return result;
}

/* As above. */
static bool transfer_fs_qtypelist(location loc, qtypelist old_qtl,
				  qtypelist new_qtl, store s)
{
  bool result;
  qtypelist_scanner old_qs, new_qs;
  qtype old_qt, new_qt;

  result = FALSE;
  qtypelist_scan(old_qtl, &old_qs);
  qtypelist_scan(new_qtl, &new_qs);
  while ((old_qt = qtypelist_next(&old_qs)) &&
	 (new_qt = qtypelist_next(&new_qs)))
    result = transfer_fs_qtype(loc, old_qt, new_qt, s) || result;

  /* Error if any leftover arguments */
  result = qtypelist_next(&old_qs) || qtypelist_next(&new_qs) || result;
  return result;
}

/* Convert qt to a flow-sensitive qtype.  Transfer any constant
   flow-sensitive qualifiers over. */
qtype qtype_to_fs_qtype_with_quals(location loc, qtype qt, store s)
{
  qtype result;

  qt = ecr_qtype(qt);

  if (qtype_function(qt))
    result = qtype_from_store(s, aloc_qtype(qt));
  else
    result = qtype_to_fs_qtype(loc, qt);
  if (transfer_fs_qtype(loc, qt, result, s))
    report_qerror(loc, sev_err,
		  "inconsistent location type for flow-sensitive pass");
  return result;
}

int count = 0;

static void mk_no_qual_qtype_fs_sub(location loc, qtype qt, store s);
static void clean_reachable_tags_fs(qtype qt, store s);

/* Check no qualifiers flows into qt */
void mk_no_qual_qtype_fs(location loc, qtype qt, store s)
{
  mk_no_qual_qtype_fs_sub(loc, qt, s);
  clean_reachable_tags_fs(qt, s);
}

static void clean_reachable_tags_fs(qtype qt, store s)
{
  qt = ecr_qtype(qt);

  switch (qt->type->kind)
    {
    case tk_primitive:
    case tk_complex:
    case tk_void:
    case tk_error:
      break;
    case tk_pointer_loc:
      clean_reachable_tags_fs(points_to_fs_qtype(qt, s), s);
      break;
    case tk_array:
      clean_reachable_tags_fs(qt->type->u.array.arrayof, s);
      break;
    case tk_struct:
    case tk_union:
      if (qt->type->u.tag.mark)
	{
	  field f;

	  qt->type->u.tag.mark = FALSE;
	  for (f = qt->type->u.tag.fields; f; f = f->next)
	    clean_reachable_tags_fs(f->qtype, s);
	}
      break;
    case tk_function:
      {
	qtypelist_scanner qs;
	qtype argqt;

	clean_reachable_tags_fs(return_qtype(qt), store_out_qtype(qt));
	qtypelist_scan(arg_qtypes(qt), &qs);
	while ((argqt = qtypelist_next(&qs)))
	  clean_reachable_tags_fs(argqt, store_in_qtype(qt));
      }
      break;
    default:
      fail("Unexpected qtype kind %x\n", qt->type->kind);
    }
}

static void mk_no_qual_qtype_fs_sub(location loc, qtype qt, store s)
{
  count++;
  //printf("%d ", count);
  qt = ecr_qtype(qt);
  if (!qtype_error(qt))
    mk_no_qual_qual(loc, qt->qual);

  switch (qt->type->kind)
    {
    case tk_primitive:
    case tk_complex:
    case tk_void:
    case tk_error:
      break;
    case tk_pointer_loc:
      mk_no_qual_qtype_fs_sub(loc, points_to_fs_qtype(qt, s), s);
      break;
    case tk_array:
      mk_no_qual_qtype_fs_sub(loc, qt->type->u.array.arrayof, s);
      break;
    case tk_struct:
    case tk_union:
      if (!qt->type->u.tag.mark)
	{
	  field f;
	  //print_qtype_raw(printf, qt, s);
	  qt->type->u.tag.mark = TRUE;
	  for (f = qt->type->u.tag.fields; f; f = f->next)
	    mk_no_qual_qtype_fs_sub(loc, f->qtype, s);
	}
      break;
    case tk_function:
      {
	qtypelist_scanner qs;
	qtype argqt;

	mk_no_qual_qtype_fs_sub(loc, return_qtype(qt), store_out_qtype(qt));
	qtypelist_scan(arg_qtypes(qt), &qs);
	while ((argqt = qtypelist_next(&qs)))
	  mk_no_qual_qtype_fs_sub(loc, argqt, store_in_qtype(qt));
      }
      break;
    default:
      fail("Unexpected qtype kind %x\n", qt->type->kind);
    }
  count--;
}

/* TRUE iff this qtype contains a qualifier that can be passed by
   a weak update */
bool toplvl_qual_fs_qtype(qtype qt)
{
  bool result;

  result = FALSE;
  qt = ecr_qtype(qt);
  if (qtype_error(qt))
    return TRUE;
  scan_qual_bounds(qt->qual, (qual_traverse_fn) nontriv_soln_fn, &result);

  switch (qt->type->kind)
    {
    case tk_primitive:
    case tk_complex:
    case tk_void:
    case tk_error:
    case tk_pointer_loc:
      break;
    case tk_array:
      result = toplvl_qual_fs_qtype(qt->type->u.array.arrayof) || result;
      break;
    case tk_struct:
    case tk_union:
      break;
    case tk_function:
      {
	qtypelist_scanner qs;
	qtype argqt;

	result = toplvl_qual_fs_qtype(return_qtype(qt)) || result;
	qtypelist_scan(arg_qtypes(qt), &qs);
	while ((argqt = qtypelist_next(&qs)))
	  result = toplvl_qual_fs_qtype(argqt) || result;
      }
    default:
      fail("Unexpected qtype kind %x\n", qt->type->kind);
    }
  return result;
}

/* Associate (s, al) with all the quals in qt */
bool store_aloc_qtype(qtype qt, store s, aloc al)
{
  bool result;

  result = FALSE;
  qt = ecr_qtype(qt);
  if (!qtype_error(qt))
    result = store_aloc_qual(qt->qual, s, al) || result;
  switch (qt->type->kind)
    {
    case tk_primitive:
    case tk_complex:
    case tk_void:
    case tk_error:
    case tk_pointer_loc:
      break;
    case tk_array:
      result = store_aloc_qtype(qt->type->u.array.arrayof, s, al) || result;
      break;
    case tk_struct:
    case tk_union:
      /* XXX: These are shared!  Gack!
      {
	field f;

	for (f = qt->type->u.tag.fields; f; f = f->next)
	  store_aloc_qtype(f->qtype, s, al);
      }
      */
      break;
    case tk_function:
      {
	qtypelist_scanner qs;
	qtype argqt;

	result = store_aloc_qtype(return_qtype(qt), s, al) || result;
	qtypelist_scan(arg_qtypes(qt), &qs);
	while ((argqt = qtypelist_next(&qs)))
	  result = store_aloc_qtype(argqt, s, al) || result;
      }
    default:
      fail("Unexpected qtype kind %x\n", qt->type->kind);
    }
  return result;
}


/* Return the loc qt points to in s */
qtype points_to_fs_qtype(qtype qt, store s)
{
  assert(qtype_pointer_loc(qt));
  return qtype_from_store(s, aloc_qtype(qt));
}

/* Same as default_conversion_qtype, but for flow-sensitive qtypes */
qtype default_conversion_fs_qtype(qtype fs_qt)
{
  fs_qt = ecr_qtype(fs_qt);

  if (!fs_qt->conv)
    {
      if (qtype_function(fs_qt))
	{
	  qual q;

	  q = make_qvar("<fp>", NULL, FALSE);
	  fs_qt->conv = mkqtype_pointer_loc(q, fs_qt->type->u.fn.aloc);
	}
      else if (qtype_array(fs_qt))
	fs_qt->conv = mkqtype_pointer_loc(fs_qt->qual,
					  fs_qt->type->u.array.aloc);
      else
	fs_qt->conv = fs_qt;
    }
  return fs_qt->conv;
}

/* qt1 <= qt2.  Right now s is only used in printing error
   messages. */
bool mkleq_fs_qtype(location loc, qtype qt1, qtype qt2, store s)
{
  return mkXeq_qtype_err(loc, qt1, qt2, mkleq, FALSE, s);
}

/* Same as mkleq_assign_convert, but for flow-sensitive qtypes */
bool mkleq_fs_assign_convert(location loc, qtype rhs_qtype, qtype lhs_qtype,
			     store s)
{
  return internal_mkleq_assign_convert(loc, rhs_qtype, lhs_qtype, s);
}

/* Assign rhs_qtype to lhs_qtype.  lhs_qype should be an l-type, and
   rhs_qtype should be an r-type.  s is the current store, and strong
   is TRUE if this must be a strong update.  Returns the store after
   the assignment. */
store assign_flow_sensitive(location loc, const char *err_msg,
			    qtype rhs_qtype, qtype lhs_qtype,
			    store s, bool strong)
{
  qtype new_lhs_points_to;
  aloc al;

  assert(qtype_pointer_loc(lhs_qtype));
  new_lhs_points_to = copy_qtype(points_to_fs_qtype(lhs_qtype, s), loc);
  al = aloc_qtype(lhs_qtype);
  insistnot(mkleq_fs_assign_convert(loc, rhs_qtype, new_lhs_points_to, s));
  /* Use new_lhs_points_to instead of rhs_qtype in case the lhs and
     rhs match but not in primitives, e.g., int vs. long */
  return make_store_assign(loc, err_msg, s, al, new_lhs_points_to, strong);
}
