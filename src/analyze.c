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

#include "parser.h"
#include "AST_utils.h"
#include "expr.h"
#include "c-parse.h"
#include "constants.h"
#include "analyze.h"
#include "cqual.h"
#include "hash.h"
#include "qerror.h"
#include "qtype.h"
#include "utils.h"
#include "pam.h"
#include "effect.h"

typedef enum { lpos, rpos, apos } context;
/* What kind of position we're in. apos = & context */

typedef struct einfo {
  qtype qt;    /* Standard qtype */
  effect eff;     /* Locations read, written, or restricted */
  bool ismalloc;
} einfo;

typedef struct sinfo {
  effect eff;     /* Locations read, written, or restricted */
} sinfo;

typedef struct dinfo {
  effect eff;    /* Locations read, written, or restricted */
  effect alocs;  /* Locations bound in environment by this decl */
  dd_list drinfolist;
} dinfo;

static void reset_operators(void);
static void init_magic(void);

static void add_declarator_overlay(declarator, qtype qt);
void analyze(declaration);
static dinfo analyze_declaration(declaration, effect env, 
				 dd_list drinfolist);
static dinfo analyze_operator_declaration(variable_decl);
void analyze_tag_ref(tag_ref);
static sinfo analyze_init(location loc, qtype lhs_qtype, expression rhs,
			  effect env, dd_list drinfolist);
static sinfo analyze_statement(statement, effect env, dd_list drinfolist);
static einfo analyze_expression(expression, context, effect env,
				dd_list drinfolist);
static einfo analyze_unary_expression(unary, context, effect env, 
				      dd_list drinfolist);
static einfo analyze_binary_expression(binary, context, effect env,
				       dd_list drinfolist);
static einfo analyze_magic_function_call(function_call, qtype, effect env,
					 dd_list drinfolist);
static einfo put_id_in_context(location loc, einfo ei, context c);
void qtype_mklhs_nonconst(location, qtype);
bool mkNonConst_qual(location, qual);
bool mkNonConst_pointer(location, qtype);
bool mkNonConst_aggregate(location, qtype);
bool mkConst_pointer(location, qtype);
ast_kind assign_op_kind(binary);
bool restrict_variable_decl(variable_decl vd, bool isglobal);
effect enforce_restricts(declaration d, effect eff, qtype qt);
void enforce_var_decl_readonly(variable_decl vd, effect eff);
void enforce_readonly(declaration d, effect eff);
effect filter_effects(qtype qt, effect alocs);

/* Some handy functions defined elsewhere */
void unparse_start(FILE *);                  /* unparse.c */
void prt_variable_decl(variable_decl);
void prt_expression(expression, int);
const char *binary_op_name(ast_kind);
bool is_void_parms(declaration);             /* semantics.c */
bool equal_expressions(expression e1, expression e2); /* eq_expressions.c */

/**************************************************************************
 *                                                                        *
 * Globals                                                                *
 *                                                                        *
 **************************************************************************/

static hash_table globals = NULL;   /* Extern stuff that should match
                                       across files */
static effect global_effect = NULL; /* Effect containing all the effects
				       of every function */
static effect global_env = NULL;    /* Locat<ions bound in global scope */

static compound_stmt cur_cs = NULL; /* current enclosing compound stmt */
static dd_list definitions = NULL;  /* Defined functions and variables */

/* The current analysis state */
static enum {state_orig, state_init, state_finish} state = state_orig;


void init_analyze(void)
{
  globals = make_string_hash_table(parse_region, 100);
  cur_cs = NULL;

  current_function_decl = NULL;
  global_effect = effect_var("<global>");
  /*mark_effect_interesting(global_effect);*/
  global_env = effect_var("<global-alocs>");
  definitions = dd_new_list(parse_region);
  if (restrict_qual && !global_effect)
    fail("Must load config file before init_analyze\n");
  reset_operators();
  state = state_init;
  init_magic();
}

void finish_analyze(void)
{
  if (!flag_pam_mode && !flag_flow_sensitive)
    /* This may be delayed until after second pass */
    traverse_globals((traverse_global_fn) warn_if_dangerous_global, NULL);

  if (init_qual && noninit_qual)
    {
      dd_list_pos pos;

      dd_scan(pos, definitions)
	{
	  data_declaration root;
	  bool may_be_init;
	  bool declared_init;
	  
	  root = root_ddecl(DD_GET(data_declaration, pos));
	  may_be_init = !leq_qual(noninit_qual, qual_qtype(root->qtype));
	  declared_init = has_ub_qual(qual_qtype(root->qtype), init_qual);

	  if (may_be_init && !declared_init && !root->isinline &&
	      !root->addresstaken)
	    report_qerror(location_declaration(root->ast), sev_info,
			  "Found undeclared %s %s\n",
			  qtype_function(root->qtype)
			      ? "__init function"
			      : "__initdata variable",
			  root->name);
	}
    }

  check_nin(); /* Check the \rho\not\in\varepsilon constraints for
                  restrict */

  state = state_finish;
}

/**************************************************************************
 *                                                                        *
 * Utilities                                                              *
 *                                                                        *
 **************************************************************************/

const char *context_tostring(context c)
{
  switch (c)
    {
    case lpos: return "lpos";
    case apos: return "&pos";
    case rpos: return "rpos";
    default: return "<context?>";
    }
}

/* Follow the shadow links to the outermost ddecl this ddecl shadows. */
data_declaration root_ddecl(data_declaration ddecl)
{
  assert(ddecl);
  while (ddecl->shadowed)
    ddecl = ddecl->shadowed;
  return ddecl;
}

/* Return TRUE iff ddecl declares a static variable or function */
bool static_ddecl(data_declaration ddecl)
{
  ddecl = root_ddecl(ddecl);
  return ((ddecl->kind == decl_variable && ddecl->vtype == variable_static) ||
	  (ddecl->kind == decl_function && ddecl->ftype == function_static));
}

/* Report an error at location l */
void vreport_qerror(location loc, severity sev, const char *format,
		    va_list args)
{
  if (sev == sev_err || (sev == sev_warn && warnings_are_errors))
    qerrors = TRUE;
  if (flag_pam_mode)
    {
      if (current_function_decl)
	pam_add_error(root_ddecl(current_function_decl->ddecl)->name,
		      loc, sev, format, args);
      else
	pam_add_error("top level", loc, sev, format, args);
    }
  else
    {
      fflush(NULL);
      if (loc)
	fprintf(stderr, "%s:%ld ", loc->filename, loc->lineno);
      vfprintf(stderr, format, args);
      if (!growbuf_empty(qual_error_detail) ||
	  !growbuf_empty(qtype_error_detail))
	{
	  fprintf(stderr, "\n");
	  if (loc)
	    fprintf(stderr, "%s:%ld ", loc->filename, loc->lineno);
	  if (!growbuf_empty(qual_error_detail))
	    fprintf(stderr, "%s", growbuf_contents(qual_error_detail));
	  else
	    fprintf(stderr, "%s", growbuf_contents(qtype_error_detail));
	}
      fprintf(stderr, "\n");
    }
  if (!growbuf_empty(qual_error_detail))
    growbuf_reset(qual_error_detail);
  if (!growbuf_empty(qtype_error_detail))
    growbuf_reset(qtype_error_detail);
}

/* Report an error at loc */
void report_qerror(location loc, severity sev, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  vreport_qerror(loc, sev, format, args);
}

identifier_declarator get_id_declarator(declarator d)
{
  switch (d->kind)
    {
    case kind_function_declarator:
      {
	function_declarator fd = CAST(function_declarator, d);
	return get_id_declarator(fd->declarator);
      }
      break;
    case kind_pointer_declarator:
      {
	pointer_declarator pd = CAST(pointer_declarator, d);
	return get_id_declarator(pd->declarator);
      }
      break;
    case kind_array_declarator:
      {
	array_declarator ad = CAST(array_declarator, d);
	return get_id_declarator(ad->declarator);
      }
      break;
    case kind_identifier_declarator:
      {
	identifier_declarator id = CAST(identifier_declarator, d);
	return id;
      }
      break;
    default:
      fail_loc(d->loc, "Unexpected declarator kind 0x%x\n", d->kind);
    }
}

/* Given a declarator d, find the identifier its defining and add
   an overlay for that id for qtype qt.  Assumes d has a name. */
static void add_declarator_overlay(declarator d, qtype qt)
{
  identifier_declarator id = get_id_declarator(d);
  cstring name = id->cstring;
  pam_add_overlay_file(id->loc, name.data, qt);

}

/* Given a declarator, return a good location for it, i.e., the
   location of the identifier it declares (if any). */
location location_declarator(declarator d)
{
  identifier_declarator id = get_id_declarator(d);
  return id->loc;
}

/* Given a declaration, return a good location for it, i.e., the
   location of the identifier it declares (if any). */
location location_declaration(declaration d)
{
  switch(d->kind)
    {
    case kind_variable_decl:
      {
	variable_decl vd = CAST(variable_decl, d);
	return location_declarator(vd->declarator);
      }
    case kind_oldidentifier_decl:
      {
	return d->loc;
      }
    case kind_function_decl:
      {
	function_decl fd = CAST(function_decl, d);
	return location_declarator(fd->declarator);
      }
    case kind_field_decl:
      {
	field_decl fd = CAST(field_decl, d);
	if (!fd->declarator)
	  /* Anonymous field */
	  return fd->loc;
	else
	  return location_declarator(fd->declarator);
      }
    case kind_enumerator:
      {
	enumerator e = CAST(enumerator, d);
	return e->loc;
      }
    case kind_implicit_decl:
      {
	implicit_decl id = CAST(implicit_decl, d);
	return id->loc;
      }
    case kind_error_decl:
      {
	return d->loc;
      }
    default:
      fail_loc(d->loc,
	       "Unexpected decl kind 0x%x\n", d->kind);
    }
}

/* Return the range qtype of the current function */
static qtype cur_return_qtype(void)
{
  qtype fun_qtype, ret_qtype;

  assert(current_function_decl);
  fun_qtype = root_ddecl(current_function_decl->ddecl)->qtype;
  ret_qtype = return_qtype(fun_qtype);
  return ret_qtype;
}

/* Return the qualifier on the current function */
/*static*/ qual cur_function_qual(void)
{
  qtype fun_qtype;
  assert(current_function_decl);
  fun_qtype = root_ddecl(current_function_decl->ddecl)->qtype;
  return qual_qtype(fun_qtype);
}

/* Return the kind of operation used in an assignment statement */
ast_kind assign_op_kind(binary e)
{
  switch(e->kind)
    {
    case kind_plus_assign: return kind_plus;
    case kind_minus_assign: return kind_minus;
    case kind_times_assign: return kind_times; 
    case kind_divide_assign: return kind_divide; 
    case kind_modulo_assign: return kind_modulo; 
    case kind_lshift_assign: return kind_lshift; 
    case kind_rshift_assign: return kind_rshift; 
    case kind_bitand_assign: return kind_bitand; 
    case kind_bitor_assign: return kind_bitor; 
    case kind_bitxor_assign: return kind_bitxor; 
    default:
      fail_loc(e->loc, "Unexpected assign_op_kind kind 0x%x\n", e->kind);
    }
}

bool file_pointer_qtype(qtype qt)
{
  if (qtype_pointer(qt))
    {
      qtype points_to;

      points_to = points_to_qtype(qt);
      return (qtype_struct(points_to) &&
	      tag_name_qtype(points_to) &&
	      !strcmp(tag_name_qtype(points_to), "_IO_FILE"));
    }
  return FALSE;
}

/**************************************************************************
 *                                                                        *
 * Manipulating einfo, sinfo, and dinfo                                   *
 *                                                                        *
 **************************************************************************/

static inline einfo mkeinfo(qtype qt, effect eff, bool ismalloc)
{
  struct einfo result = {qt: qt, eff: eff, ismalloc: ismalloc};
  return result;
}

static inline sinfo mksinfo(effect eff)
{
  struct sinfo result = {eff: eff};
  return result;
}

static inline dinfo mkdinfo(effect eff, effect alocs)
{
  struct dinfo result = {eff: eff, alocs: alocs};
  return result;
}

static einfo dummy_einfo = {qt: NULL, eff: NULL};

/**************************************************************************
 *                                                                        *
 * Operator signatures                                                    *
 *                                                                        *
 **************************************************************************/

typedef struct operator
{
  const char *name;
  enum sig_kind { sig_deref, sig_addr, sig_unop, sig_binop, sig_assign } sig;
  qtype qt;
} *operator;

/* A list of all the function names used to define signatures for
   operators */
static struct operator operators[] =
{
 {"_op_uminus", sig_unop, NULL},
 {"_op_uplus", sig_unop, NULL},
 {"_op_bitnot", sig_unop, NULL},
 {"_op_not", sig_unop, NULL},
 {"_op_times", sig_binop, NULL},
 {"_op_div", sig_binop, NULL},
 {"_op_mod", sig_binop, NULL},
 {"_op_lshift", sig_binop, NULL},
 {"_op_rshift", sig_binop, NULL},
 {"_op_lt", sig_binop, NULL},
 {"_op_gt", sig_binop, NULL},
 {"_op_leq", sig_binop, NULL},
 {"_op_geq", sig_binop, NULL},
 {"_op_eq", sig_binop, NULL},
 {"_op_neq", sig_binop, NULL},
 {"_op_bitand", sig_binop, NULL},
 {"_op_bitor", sig_binop, NULL},
 {"_op_bitxor", sig_binop, NULL},
 {"_op_andand", sig_binop, NULL},
 {"_op_oror", sig_binop, NULL},
 {"_op_deref", sig_deref, NULL},
 /*{"_op_addr", sig_addr, NULL},*/
 /*{"_op_assign", sig_assign, NULL},*/
 /*{"_op_plus", sig_binop, NULL},*/
 /*{"_op_minus", sig_binop, NULL},*/
 {0, 0, NULL}};

/* Given an operator kind, return its signature */
static inline operator find_op_kind(ast_kind k)
{
  operator op;

  switch (k)
    {
    case kind_unary_minus: op = operators;    break;
    case kind_unary_plus:  op = operators+1;  break;
    case kind_bitnot:      op = operators+2;  break;
    case kind_not:         op = operators+3;  break;
    case kind_times:       op = operators+4;  break;
    case kind_divide:      op = operators+5;  break;
    case kind_modulo:      op = operators+6;  break;
    case kind_lshift:      op = operators+7;  break;
    case kind_rshift:      op = operators+8;  break;
    case kind_lt:          op = operators+9;  break;
    case kind_gt:          op = operators+10; break;
    case kind_leq:         op = operators+11; break;
    case kind_geq:         op = operators+12; break;
    case kind_eq:          op = operators+13; break;
    case kind_ne:          op = operators+14; break;
    case kind_bitand:      op = operators+15; break;
    case kind_bitor:       op = operators+16; break;
    case kind_bitxor:      op = operators+17; break;
    case kind_andand:      op = operators+18; break;
    case kind_oror:        op = operators+19; break;
    case kind_dereference: op = operators+20; break;
      /*    case kind_address_of:  return operators+1; break;*/
      /*    case kind_assign:      return operators+24; break;*/
      /*    case kind_plus:        return operators+9; break;*/
      /*    case kind_minus:       return operators+10; break;*/
    default:               op = NULL;
    }

  if (op && op->qt)
    return op;
  else
    return NULL;
}

/* Given an operator function name, return its signature */
static inline operator find_op_name(const char *name)
{
  int i;

  for (i = 0; operators[i].name; i++)
    {
      if (!strcmp(name, operators[i].name))
	return operators+i;
    }
  return NULL;
}

#define is_op_name(name) (!strncmp("_op_", name, 4) && strlen(name) >= 4)

#define exists_op_kind(k) (find_op_kind(k) != NULL)

/* Reset the operator qtypes to NULL (nonexistant) */
static void reset_operators(void)
{
  int i;

  for (i = 0; operators[i].name; i++)
    operators[i].qt = NULL;
}

/* Return TRUE if qt matches the signature s, false otherwise */
bool match_signature(enum sig_kind k, qtype qt, location loc)
{
  if (qtype_function(qt))
    {
      qtype ret, arg1, arg2;
      store in, out;
      qtypelist_scanner s;
      bool fmatch; /* Formals match */

      ret = return_qtype(qt);
      in = store_in_qtype(qt);
      out = store_out_qtype(qt);
      qtypelist_scan(arg_qtypes(qt), &s);
      arg1 = qtypelist_next(&s);
      arg2 = qtypelist_next(&s);
      if (qtypelist_next(&s) || qtype_varargs(qt) || qtype_oldstyle(qt))
	/* Too many arguments */
	return FALSE;
      if (arg2 && (k == sig_deref || k == sig_addr || k == sig_unop))
	/* Too many arguments */
	return FALSE;

      switch (k)
	{
	case sig_deref:
	  /* Note last condition only true if arg1 is a variable */
	  /* XXX: Check, don't force qtypes to be equal! */
	  fmatch = (qtype_var(ret) && qtype_pointer(arg1) &&
		    !mkeq_qtype(loc, points_to_qtype(arg1), ret));
	  break;
	case sig_addr:
	  /* Note last condition only true if ret is a variable */
	  fmatch = (qtype_pointer(arg1) &&
		    qtype_var(points_to_qtype(arg1)) &&
		    qtype_pointer(ret) &&
		    !mkeq_qtype(loc,
				points_to_qtype(arg1),
				points_to_qtype(ret)));
	  break;
	case sig_unop:
	  fmatch = qtype_int(ret) && qtype_int(arg1);
	  break;
	case sig_binop:
	  fmatch = qtype_int(ret) && qtype_int(arg1) && qtype_int(arg2);
	  break;
	default:
	  fail("Unexpected signature kind %x\n", k);
	}
      return fmatch;
    }
  else
    return FALSE;

  /* Matches */
  return TRUE;
}

/* Add qt and dqt to op, checking first that they match op's stated
   signature kind.  Return TRUE iff an error occurs. */
bool add_op_signature(operator op, qtype qt, location loc)
{
  /* If qt or dqt were generalized, instantiate them because we're
     going to add some constraints and then generalize them again */
  if (qtype_scheme(qt))
    qt = instantiate_qtype(qt, loc);
  if (match_signature(op->sig, qt, loc))
    {
      op->qt = generalize_qtype(qt);
      return FALSE;
    }
  else
    return TRUE;
}

/* Add the constraints implied by operator signature op to ret, arg1,
   and arg2. */
bool add_op_constraints(operator op, einfo ret, einfo arg1, einfo arg2,
			location loc)
{
  qtype op_inst;
  qtypelist_scanner s;
  bool result;

  result = FALSE;

  op_inst = instantiate_qtype(op->qt, NULL);
  result = mkeq_qtype(loc, return_qtype(op_inst), ret.qt) || result;
  qtypelist_scan(arg_qtypes(op_inst), &s);
  result = mkeq_qtype(loc, qtypelist_next(&s), arg1.qt) || result;
  if (arg2.qt)
    result = mkeq_qtype(loc, qtypelist_next(&s), arg2.qt) || result;
  else
    assert(!qtypelist_next(&s));

  return result;
}

/**************************************************************************
 *                                                                        *
 * Multi-file stuff                                                       *
 *                                                                        *
 * ``globals'' is a hash table mapping extern-scoped identifiers to their *
 * declarations.  If an identifier has an entry in globals, that ddecl    *
 * should not be used -- we would need to resolve all oldstyle and        *
 * otherwise mismatching function decls ahead of time if we wanted to do  *
 * that.                                                                  *
 *                                                                        *
 **************************************************************************/

/* Initialize a data declaration's qtype fields.  If ddecl was already
   declared in a prelude file, use the types from the prelude file.
   Otherwise verify that ddecl can be unified with what has already
   been declared.  Display an error if ddecl was already declared in
   another file but with a different type. */
void add_global_ddecl_qtype(data_declaration ddecl, bool generalize)
{
  data_declaration old_ddecl;
  location loc;

  ddecl = root_ddecl(ddecl);
  loc = location_declaration(ddecl->ast);
  assert(ddecl->isexternalscope);

  if (ddecl->qtype)
    return;

  if (hash_table_lookup(globals, (hash_key) ddecl->name,
			(hash_data *) &old_ddecl))
    {
      /* If ddecl was previously declared in a prelude file, that
	 declaration completely overrides this one */
      if (old_ddecl->in_prelude && !ddecl->in_prelude)
	{
	  ddecl->qtype = old_ddecl->qtype;
	  ddecl->global = old_ddecl;
	  return;
	}

      /* Multiple declarations in prelude files are allowed; neither
	 overrides the other
	 assert(old_ddecl->in_prelude || !ddecl->in_prelude); */

      /* Otherwise, check whether the types match */
      if (match_type(ddecl->type, old_ddecl->type))
	/* They did -- just reuse old qtype */
	ddecl->qtype = old_ddecl->qtype;
      else
	{
	  /* The types differ in some way.  Create new qtypes for
	     ddecl and make sure they match the old declaration */
	  insist(add_ddecl_qtype(ddecl, generalize));

	  if (qtype_scheme(old_ddecl->qtype) || qtype_scheme(ddecl->qtype))
	    {
	      report_qerror(loc, sev_warn,
			    "multiple polymorphic types for ``%s''"
			    " -- not checking for consistency", ddecl->name);
	    }
	  else if (unify_qtype(loc, old_ddecl->qtype, ddecl->qtype))
	    {
	      report_qerror(loc, sev_err, "conflicting types for ``%s''",
			    ddecl->name);
	      report_qerror(old_ddecl->ast->loc, sev_info,
			    "-- previous declaration of ``%s''", ddecl->name);
	    }
	}

      /* Special case: If you redeclare an oldstyle function with a
	 newstyle declaration, replcae the old ddecl with the new one */
      if (qtype_function(ddecl->qtype) && !qtype_oldstyle(ddecl->qtype) &&
	  qtype_function(old_ddecl->qtype) && qtype_oldstyle(old_ddecl->qtype))
	{
	  insistnot(hash_table_insert(globals, (hash_key) ddecl->name, ddecl));
	  old_ddecl->global = ddecl;
	  old_ddecl = ddecl;
	}

      /* Check whether ddecl has been defined anywhere */
      /* Don't do this check -- too annoying
      if (old_ddecl->definition && ddecl->definition)
	report_qerror(loc, sev_warn,
		      "redefinition of extern ``%s''", ddecl->name);
      else */

      /* Update flags */
      old_ddecl->defined = ddecl->defined =
	(old_ddecl->defined || ddecl->defined);

      old_ddecl->isused = ddecl->isused =
	(old_ddecl->isused || ddecl->isused);

      old_ddecl->addresstaken = ddecl->addresstaken =
	(old_ddecl->addresstaken || ddecl->addresstaken);

      /* New declaration is shadowed by the old one. */
      ddecl->global = old_ddecl;
    }
  else
    {
      insist(add_ddecl_qtype(ddecl, generalize));
      if (ddecl->in_prelude && !ddecl->defined)
	ddecl->defined = TRUE;
      insist(hash_table_insert(globals, (hash_key) ddecl->name, ddecl));
    }
}

/* Add a qtype to a static ddecl.  We allow static declarations/definitions
   in prelude files to supersede static declarations/definitions in files,
   so we need to do a special check. */
void add_static_ddecl_qtype(data_declaration ddecl, bool generalize)
{
  data_declaration old_ddecl;

  ddecl = root_ddecl(ddecl);
  assert(static_ddecl(ddecl));
  if (ddecl->qtype)
    return;

  if (hash_table_lookup(globals, (hash_key) ddecl->name,
			(hash_data *) &old_ddecl))
    {
      if (old_ddecl->in_prelude && static_ddecl(old_ddecl) &&
	  !ddecl->in_prelude)
	{
	  ddecl->qtype = old_ddecl->qtype;
	  ddecl->global = old_ddecl;
	  return;
	}
      if (ddecl->in_prelude)
	report_qerror(location_declaration(ddecl->ast), sev_err,
		      "ignoring static prelude declaration -- conflict with "
		      "non-static prelude declaration");
    }
  else if (ddecl->in_prelude)
    {
      add_ddecl_qtype(ddecl, generalize);
      assert(ddecl->qtype);
      ddecl->defined = TRUE;
      insist(hash_table_insert(globals, (hash_key) ddecl->name, ddecl));
    }

  /* Fall through case */
  add_ddecl_qtype(ddecl, generalize);
}

/* Add a qtype to ddecl.  The logic from qtype.c should really be
   moved here. */
void __add_ddecl_qtype(data_declaration ddecl, bool generalize)
{
  ddecl = root_ddecl(ddecl);

  if (ddecl->qtype)
    return;

  if (ddecl->isexternalscope)
    add_global_ddecl_qtype(ddecl, generalize);
  else if (static_ddecl(ddecl))
    add_static_ddecl_qtype(ddecl, generalize);
  else
    /* You can declare things many times, so this may return either
       true or false. */
    add_ddecl_qtype(ddecl, generalize);
}

/* Mark that ddecl is used.  If ddecl is global, then the mark is also
   set in the corresponding ddecl in globals. */
void mark_used(data_declaration ddecl)
{
  ddecl = root_ddecl(ddecl);
  ddecl->isused = TRUE;
  if (ddecl->isexternalscope && ddecl->global)
    ddecl->global->isused = TRUE;
}

/* Mark that ddecl has its address taken.  If ddecl is global, then
   the mark is also set in the corresponding ddecl in globals. */
void mark_address_taken(data_declaration ddecl)
{
  ddecl = root_ddecl(ddecl);
  ddecl->addresstaken = TRUE;
  if (ddecl->isexternalscope && ddecl->global)
    ddecl->global->addresstaken = TRUE;
}

/* Here's an example of why we can't just pick one ddecl to stand for
   all of them:

   file1: void foo(void) { extern char lower(); }
   file2: char lower(char c) { return c; }

   If we analyze file1 before file2 and set lower's ddecl to be the
   oldstyle function, we'll get messed up at the definition.  On the
   other hand, if we change unify_qtype to merge extra parameters, we
   can't handle varargs oldstyle functions. */

/* Apply f to one ddecl for each global */
void traverse_globals(void (*f)(data_declaration, void *),
				void *arg)
{
  hash_table_scanner hts;
  data_declaration ddecl;

  hash_table_scan(globals, &hts);
  while (hash_table_next(&hts, NULL, (hash_data *) &ddecl))
    {
      assert(!ddecl->shadowed);
      f(ddecl, arg);
    }
}
/* Return TRUE iff ddecl is used but undefined */
bool is_undefined_global(data_declaration ddecl, store s)
{
  ddecl = root_ddecl(ddecl);
  return (ddecl->isused && !ddecl->defined &&
	  (nontriv_soln_qtype(ddecl->qtype, NULL) ||
	   flag_flow_sensitive));
  /* The follow test doesn't work, because of the filtering w.r.t. effects
     we do
	    (s ? nontriv_soln_qtype(ddecl->fs_qtype, NULL) : FALSE))));
  */
}

/* Return TRUE iff ddecl is declares a varargs function that was
   defined and used but not given a type in a prelude file */
bool is_unannotated_global(data_declaration ddecl, store s)
{
  /* XXX: Also check isused flag here? */
  ddecl = root_ddecl(ddecl);
  return (!is_undefined_global(ddecl, s) &&
	  ddecl->isused &&
	  qtype_function(ddecl->qtype) &&
	  qtype_varargs(ddecl->qtype) &&
	  !ddecl->in_prelude);
}

/* Display an error if ddecl is used but not defined, or if it's
   a varargs function not given a type in a prelude file */
void warn_if_dangerous_global(data_declaration ddecl, store s)
{
  if (is_undefined_global(ddecl, s))
    report_qerror(location_declaration(ddecl->ast), sev_err,
		  "``%s'' used but not defined", ddecl->name);
  if (is_unannotated_global(ddecl, s))
    report_qerror(location_declaration(ddecl->ast), sev_info,
		  "vararg function ``%s'' does not have polymorphic type",
		  ddecl->name);
}

/* Compute the top-level effect */
effect get_global_effect(void)
{
  assert(state == state_finish);
  return global_effect;
}

/* Return the root data declaration for main */
data_declaration get_main_ddecl(void)
{
  data_declaration main_ddecl;

  assert(state == state_finish);
  if (hash_table_lookup(globals, (hash_key) "main", (hash_data *) &main_ddecl))
    return root_ddecl(main_ddecl);
  else
    return NULL;
}

/**************************************************************************
 *                                                                        *
 * Tree Traversal                                                         *
 *                                                                        *
 * The code that walks over the AST and generates the constraint          *
 * graph, plus whatever info we'll need to do linking.  Think of this     *
 * as a big pattern-match.                                                *
 *                                                                        *
 **************************************************************************/

void analyze(declaration program)
{
  declaration d;
  dinfo next_info;
  dd_list global_drinfolist;

  global_drinfolist = dd_new_list(parse_region);

  /* Set this fresh for each file - sleepy qual */
  interrupt_status_qtype = NULL;

  unparse_start(stdout);
  pam_add_file(program->loc->filename);
  AST_set_parents(CAST(node, program));
  scan_declaration(d, program)
    {
      /* Assume left-to-right order of evaluation */
      next_info = analyze_declaration(d, global_env, global_drinfolist);
      mkleq_effect(next_info.eff, global_effect);
      mkleq_effect(next_info.alocs, global_env);
      /* report_qerror(d->loc, sev_err,
		      "Allocation or initialization accesses globally-"
		      "restricted location");
      */
    }
}

static dinfo analyze_declaration(declaration d, effect env, dd_list drinfolist)
{
  switch (d->kind)
    {
    case kind_asm_decl:
      {
	/*	report_qerror(d->loc, sev_warn,
		"warning: ignoring in-line assembly code");*/
	return mkdinfo(effect_empty, effect_empty);
      };
    case kind_data_decl:
      {
	data_decl dd = CAST(data_decl, d);
	declaration decl;
	effect eff, local_env, alocs;

	eff = effect_empty;
	local_env = env;
	alocs = effect_empty;

	scan_declaration(decl, dd->decls) {
	  dinfo next;

	  next = analyze_declaration(decl, local_env, drinfolist);
	  local_env = effect_union(local_env, next.alocs);
	  alocs = effect_union(alocs, next.alocs);
	  eff = effect_union(eff, next.eff);
	}

	if (!dd->decls && dd->modifiers && is_tag_ref(dd->modifiers))
	  /* If we only declared a struct/union/enum, add types for
	     the fields */
	  analyze_tag_ref(CAST(tag_ref, dd->modifiers));

	return mkdinfo(eff, alocs);
      };
    case kind_variable_decl:
      {
	variable_decl vd = CAST(variable_decl, d);
	data_declaration root;
	effect eff, alocs;
	bool isglobal;

	/* Ignore asm_stmt
	if (vd->asm_stmt)
	  fail_loc(vd->loc,
		   "Unimplemented: asm in variable_decl\n", 0);
	*/

	root = root_ddecl(vd->ddecl);

	if (is_op_name(root->name))
	  return analyze_operator_declaration(vd);

	if (root->kind == decl_typedef)
	  {
	    /* Skip typedefs -- except enums, since we have to give
	       qtypes to those */
	    if (type_enum(root->type))
	      analyze_tag_ref(type_tag(root->type)->ast);
	    return mkdinfo(effect_empty, effect_empty);
	  }

	__add_ddecl_qtype(root, TRUE);

	eff = effect_empty;
	alocs = effect_empty;
	isglobal = (root->kind == decl_function ||
		    root->isexternalscope ||
		    root->vtype == variable_static);

	/* Add the alocs from this qtype to the appropriate effects */
	if (root->qtype)
	  {
	    if (isglobal)
	      {
		mkleq_effect(alocs_qtype(root->qtype), global_env);
		mkleq_effect(defn_effect_qtype(root->qtype), global_effect);
	      }
	    else
	      {
		alocs = effect_union(alocs_qtype(root->qtype), alocs);
		eff = effect_union(defn_effect_qtype(root->qtype), eff);
	      }
	  }

	/* Handle restrict */
	if (restrict_variable_decl(vd, isglobal))
	  {
	    if (isglobal)
	      {
		/* Forbid use of unrestricted loc anywhere */
		mknin_effect(vd->loc, vd->rinfo->flocs, global_effect);

		/* 2p:  Add effect of allocating the new loc */
		if (flag_flow_sensitive)
		  mkleq_effect(vd->rinfo->flocs_alloc, global_effect);
	      }
	    else
	      {
		/* Forbid escape */
		mknin_effect(vd->loc, vd->rinfo->rlocs, env);

		/* Add rloc to environment */
		alocs = effect_union(vd->rinfo->rlocs, alocs); 

		/* 2p:  Add effect of allocating the new loc */
		if (flag_flow_sensitive)
		  eff = effect_union(vd->rinfo->rlocs_alloc, eff);
	      }
	  }
	
	/* If readonly is on global, forbit write everywhere */
	if (isglobal)
	  enforce_var_decl_readonly(vd, global_effect);

	if (vd->arg1)
	  {
	    qtype init_qt;
	    sinfo arg1;

	    /* If we are initializing a global restricted loc, then
	       root->qtype contains the restricted loc */
	    /* XXX: This doesn't change anything if we only have
	       top-level restricts */
	    if (vd->rinfo && vd->rinfo->isglobal)
	      init_qt = vd->rinfo->fqtype;
	    else
	      init_qt = root->qtype;

	    if (!qtype_array(init_qt))
	      init_qt = points_to_qtype(init_qt);

	    arg1 = analyze_init(vd->loc, init_qt, vd->arg1, env, drinfolist);
	    vd->arg1_eff = arg1.eff;

	    if (isglobal)
	      /* enforce_restricts is never called for globals */
	      eff = effect_union(eff, arg1.eff);
	  }

	/* If we restricted this loc, replace its qtype with its
	   restricted qtype, which we'll see from now on */
	if (vd->rinfo)
	  root->qtype = vd->rinfo->rqtype;

	/* If attribute alias, unify aliased qtype */
	if (root->alias != NULL)
	  {
	    data_declaration alias_ddecl;

	    if (hash_table_lookup(globals, (hash_key) root->alias, (hash_data *) &alias_ddecl))
	      {
		alias_ddecl = root_ddecl(alias_ddecl);
		unify_qtype(vd->loc, alias_ddecl->qtype, root->qtype);
	      }
	  }

	/* Set interrupt status qtype - sleepy qual */
	if (have_sleepy_quals && 
	    !strcmp(root->name, "fake_interrupt_status"))
	  {
	    if (interrupt_status_qtype != NULL)
	      assert(interrupt_status_qtype == root->qtype);
	    else
	      interrupt_status_qtype = root->qtype;
	  }

	/* Add PAM markup */
	if ((flag_pam_mode || flag_print_results) &&
	    root->kind != decl_typedef)
	  add_declarator_overlay(vd->declarator, root->qtype);

	/*
	printf("Effect of declaring %s: ", root->name);
	print_effect(printf, eff);
	printf("\n");
	*/
	return mkdinfo(eff, alocs);
      }
      break;
    case kind_function_decl:
      {
	function_decl fd = CAST(function_decl, d), old_function_decl;
	data_declaration root;
	qtype ret_qtype;
	effect param_rtype_alocs; /* alocs in parameters */
	effect alocs, local_env, eff;

	root = root_ddecl(fd->ddecl);
	dd_add_last(parse_region, definitions, root);

	__add_ddecl_qtype(root, TRUE);
	if (root->global && root->global->in_prelude)
	  /* Ignore definition of something from a prelude file */
	  return mkdinfo(effect_empty, effect_empty);

	if (qtype_scheme(root->qtype))
	  {
	    /* If you change this, also change the case in flow.c for
	       ignoring the definition. */
	    report_qerror(fd->loc, sev_warn,
			  "Ignoring definition of polymorphic function");
	    return mkdinfo(effect_empty, effect_empty);
	  }

	alocs = alocs_qtype(root->qtype);

	if (flag_pam_mode || flag_print_results)
	  add_declarator_overlay(fd->declarator, root->qtype);

	old_function_decl = current_function_decl;
	current_function_decl = fd;
	ret_qtype = return_qtype(root->qtype);
	mkleq_effect(effect_qtype(root->qtype), global_effect);
	/*  report_qerror(fd->loc, sev_err,
	    "Function accesses globally-restricted location"); */

	/* 2p: Add effect of allocation the function to the global effect */
	mkleq_effect(defn_effect_qtype(root->qtype), global_effect);

	local_env = env;
	eff = effect_empty;
	param_rtype_alocs = effect_empty;

	/*** Scan argument list ***/

	{
	  declaration arg;
	  qtypelist_scanner old_qtypes;
	  int i;

	  i = 1;
	  qtypelist_scan(arg_qtypes(root->qtype), &old_qtypes);

	  if (!is_void_parms(fd->fdeclarator->parms))
	    scan_declaration(arg, fd->fdeclarator->parms)
	      /* Ignore ...'s */
	      if (!is_ellipsis_decl(arg))
		{
		  data_declaration root_vd;
		  variable_decl argvd;
		  oldidentifier_decl oid;
		  qtype old_qtype, arg_qt;
		  
		  /* Construct type of parameter */
		  if (arg->kind == kind_data_decl)
		    {
		      data_decl argd = CAST(data_decl, arg);

		      argvd = CAST(variable_decl, argd->decls);
		      assert(!argvd->next);    /* Only var_decl per data_decl.
						   multi var_decls are for
						   things like int a, b. */
		      oid = NULL;
		      root_vd = root_ddecl(argvd->ddecl);
		    }
		  else
		    {
		      oid = CAST(oldidentifier_decl, arg);
		      argvd = NULL;
		      root_vd = root_ddecl(oid->ddecl);
		    }

		  __add_ddecl_qtype(root_vd, FALSE);
		  assert(root_vd->qtype);
	
		  /* Match up parameter type with function type */
		  old_qtype = qtypelist_next(&old_qtypes);
		  arg_qt = points_to_qtype(root_vd->qtype);

		  if (old_qtype && unify_qtype(arg->loc, old_qtype, arg_qt))
		    report_qerror(arg->loc, sev_err,
				  "type of parameter %d doesn't match "
				  "previous uses", i);
		  else if (!old_qtype && arg->kind == kind_data_decl)
		    report_qerror(arg->loc, sev_err,
				  "parameter %d doesn't appear in "
				  "previous uses", i);

		  /* NOTE:  No checking done for oldstyle identifiers!
		     This matches gcc's beavhior. */

		  if (old_qtype)
		    param_rtype_alocs = effect_union(param_rtype_alocs,
						     alocs_qtype(old_qtype));

		  /* 2p:  Add the effect of allocating space for parameter */
		  eff = effect_union(eff, defn_effect_qtype(root_vd->qtype));

		  /* Assume the function touches all its parameters */
		  /* Don't assume this for now */
		  /*eff = effect_union(eff, alocs_qtype(root_vd->qtype));*/

		  local_env = effect_union(local_env,
					   alocs_qtype(root_vd->qtype));

		  if (arg->kind == kind_data_decl &&
		      restrict_variable_decl(argvd, FALSE))
		    {
		      mknin_effect(argvd->loc, argvd->rinfo->rlocs, local_env);
		      local_env = effect_union(argvd->rinfo->rlocs, local_env);

		      /* 2p:  Add the location from restrict_variable_decl */
		      if (flag_flow_sensitive)
			eff = effect_union(eff, argvd->rinfo->rlocs_alloc);
		      root_vd->qtype = argvd->rinfo->rqtype;
		    }

		  if (flag_pam_mode || flag_print_results)
		    {
		      if (arg->kind == kind_data_decl)
			add_declarator_overlay(argvd->declarator,
					       root_vd->qtype);
		      else
			pam_add_overlay_file(oid->loc,
					     oid->cstring.data,
					     root_vd->qtype);
		    }
		  
		  i++;
		}
	  if (qtypelist_next(&old_qtypes))
	    report_qerror(fd->loc, sev_err,
			  "inconsistent number of arguments for %s",
			  root->name);
	}

	/*** Evaluate body ***/

	{
	  sinfo body;

	  assert(is_compound_stmt(fd->stmt));
	  body = analyze_statement(fd->stmt, local_env, drinfolist);
	  eff = effect_union(eff,
			     enforce_restricts(fd->fdeclarator->parms,
					       body.eff,
					       ret_qtype));
	  enforce_readonly(fd->fdeclarator->parms, eff);
	}

	/* Apply (Down) once more to filter out locations neither in
	   parameters (param_rtype_alocs) or return value (added by
	   filter_effects) */
	fd->scope_env = effect_union(param_rtype_alocs,
				     filter_effects(NULL, env));
	mark_effect_interesting(fd->scope_env);
	eff = effect_inter(eff, fd->scope_env);
	mkleq_effect(eff, effect_qtype(root->qtype));

	current_function_decl = old_function_decl;
	return mkdinfo(effect_empty, alocs);
      };
    case kind_extension_decl:
      {
	extension_decl ed = CAST(extension_decl, d);
	return analyze_declaration(ed->decl, env, drinfolist); 
	/* Ignore __extension__ */
      };
    default:
      fail_loc(d->loc,
	       "Unexpected decl kind 0x%x\n", d->kind);
    }
}

static dinfo analyze_operator_declaration(variable_decl vd)
{
  data_declaration root;
  operator op;

  root = root_ddecl(vd->ddecl);
  assert(is_op_name(root->name));
  if (!root->isexternalscope)
    {
      report_qerror(vd->loc, sev_err,
		    "Operator not declared at top level");
      return mkdinfo(effect_empty, effect_empty);
    }

  add_global_ddecl_qtype(root, TRUE);

  op = find_op_name(root->name);
  if (op && op->qt)
    report_qerror(vd->loc, sev_err,
		  "redefinition of operator signature");
  else if (op)
    {
      /* It's an operator definition */
      if (add_op_signature(op, root->qtype, vd->loc))
	report_qerror(vd->loc, sev_err,
		      "definition of operator with incorrect signature");
      
      /* Reset qtype to fully constrained qtype */
      root->qtype = op->qt;      
    }
  else
    report_qerror(vd->loc, sev_err,
		  "definition of non-existent operator signature");
  
  /* Add PAM markup */
  if ((flag_pam_mode || flag_print_results) &&
      root->kind != decl_typedef)
    add_declarator_overlay(vd->declarator, root->qtype);
  
  return mkdinfo(effect_empty, effect_empty);
}

/* Analyze a field declaration */
qtype analyze_field_declaration(const char *name, field_declaration fd)
{
  qtype result;

  result = get_fdecl_qtype(name, fd);
  if ((flag_pam_mode || flag_print_results) && fd->name)
    /* fields can be anonymous */
    add_declarator_overlay(fd->ast->declarator, result);

  mkleq_effect(defn_effect_qtype(result), global_effect);
  mkleq_effect(effect_single(aloc_qtype(result)), global_env);
  return result;
}

/* Analyze a struct/union/enum { ... } declaration. */
void analyze_tag_ref(tag_ref tr)
{
  if (is_struct_ref(tr) || is_union_ref(tr))
    /* XXX: Enabled the next line to give types to all structure
       fields to get types, no matter if they're actually used or
       not. */
    /*type_to_qtype(make_tagged_type(tr->tdecl), NULL, NULL);*/
    ;
  else
    {
      declaration d;

      scan_declaration(d, tr->fields)
	{
	  enumerator e = CAST(enumerator, d);
	  data_declaration root;

	  root = root_ddecl(e->ddecl);
	  if (!root->qtype)
	    {
	      __add_ddecl_qtype(root, FALSE);
	      if (flag_pam_mode || flag_print_results)
		pam_add_overlay_file(e->loc,
				     e->cstring.data,
				     root->qtype);
	    }
	}
    }
}

static bool init_string(location loc, qtype lhs_qtype, qtype rhs_qtype)
{
  bool result = FALSE;
  qtype lhs_contents_qtype, rhs_contents_qtype;

  lhs_contents_qtype = array_of_qtype(lhs_qtype);
  rhs_contents_qtype = points_to_qtype(rhs_qtype);
  result = mkleq_qual(loc,
		      qual_qtype(rhs_contents_qtype),
		      qual_qtype(lhs_contents_qtype)) || result;
  result = mkleq_qual(loc,
		      qual_qtype(rhs_qtype),
		      qual_qtype(lhs_qtype)) || result;
  return result;

}

/* XXX Init */
/* Analyze the initialization of lhs_qtype and lhs_dqtype (r-types)
   with rhs, in initial store in. */
static sinfo analyze_init(location loc, qtype lhs_qtype, expression rhs,
			  effect env, dd_list drinfolist)
{
  if (rhs->kind == kind_string && (qtype_string(lhs_qtype)))
    {
      /* Special case for strings.  The rhs is a char ptr, but the
	 quals should flow into the array quals, anyhow. */

      einfo arg1;
      assert(qtype_string(lhs_qtype));
      /* assert(!rhs->next); not always true -- init arrays of strings */
      arg1 = analyze_expression(rhs, rpos, env, drinfolist);
      /* Hack:  rhs is a string, so no side effects. */
      if (init_string(rhs->loc, lhs_qtype, arg1.qt))
	report_qerror(rhs->loc, sev_err, "incompatible initializer");
      
      return mksinfo(arg1.eff);
    }
  else if (rhs->kind == kind_init_list && (qtype_array(lhs_qtype)))
    {
      /* Initialize the array contents qtype with each rhs */
      init_list il = CAST(init_list, rhs);
      qtype lhs_contents_qtype;
      expression e;
      sinfo init;
      effect eff;

      assert(qtype_array(lhs_qtype));

      lhs_contents_qtype = array_of_qtype(lhs_qtype);
      eff = effect_empty;

      scan_expression(e, il->args)
	if (e->kind == kind_init_index)
	  {
	    /* Handle this case here -- only arrays can be initialized
	       this way. */
	    init_index ii = CAST(init_index, e);
	    einfo arg1, arg2;

	    arg1 = analyze_expression(ii->arg1, rpos, env, drinfolist);
	    eff = effect_union(eff, arg1.eff);
	    if (ii->arg2)
	      {
		arg2 = analyze_expression(ii->arg2, rpos, env, drinfolist);
		eff = effect_union(eff, arg2.eff);
	      }
	    init = analyze_init(loc, lhs_contents_qtype, ii->init_expr, env,
				drinfolist);
	    eff = effect_union(eff, init.eff);
	  }
	else
	  {
	    init = analyze_init(loc, lhs_contents_qtype, e, env, drinfolist);
	    eff = effect_union(eff, init.eff);
	  }
      return mksinfo(eff);
     }
   else if (rhs->kind == kind_init_list && (qtype_struct(lhs_qtype) ||
					    qtype_union(lhs_qtype)))
     {
       /* Initialize each of the field qtypes with each rhs */
       init_list il = CAST(init_list, rhs);
       field_scanner fs;
       qtype fqt;
       expression e;
       effect eff;

       fqt = NULL;
       field_scan(lhs_qtype, &fs);
       eff = effect_empty;

       scan_expression(e, il->args)
	 {
	   expression init_expr;
	   sinfo init;
	   
	   if (e->kind == kind_init_field)
	     {
	       /* Handle specific field initialization here -- only
		  structures/unions can be initialized this way.
		  Adding a label changes where we start initializing
		  the struct/union. */
	       init_field ifd = CAST(init_field, e);
	       field_scan_at(lhs_qtype, ifd->word1->cstring, &fs);
	       if (!fs)
		 {
		   report_qerror(loc, sev_err,
			      "unknown field `%s' in aggregate initialization",
				 ifd->word1->cstring);
		   break;
		 }
	       init_expr = ifd->init_expr;
	     }
	   else
	     init_expr = e;

	   fqt = field_next(&fs);
	   if (!fqt)
	     {
	       report_qerror(loc, sev_err,
			  "aggregate initailized with wrong number of fields");
	       break;
	     }

	   /* Unless we're initializing an array, get the contents of the
	      ref.  Note we can't be initializing a function -- not C++. */
	   if (!qtype_array(fqt))
	     fqt = points_to_qtype(fqt);
	   
	   init = analyze_init(loc, fqt, init_expr, env, drinfolist);
	   eff = effect_union(eff, init.eff);
	 }
      /* OK to initialize aggregate with too few fields
	 err = (field_next(&fs) || err); */
       return mksinfo(eff);
     }
  else
    {
      /* Default case.  Note we don't do a substitute_store, because variables
	 can't be used before they're initialized. */
      einfo arg1;

      arg1 = analyze_expression(rhs, rpos, env, drinfolist);
      
      if (mkleq_assign_convert(loc, arg1.qt, lhs_qtype))
	report_qerror(loc, sev_err,
		      "incompatible right-hand side in initialization");
      return mksinfo(arg1.eff);
    }
}

static sinfo analyze_statement(statement s, effect env, dd_list drinfolist)
{
  switch (s->kind)
    {
    case kind_asm_stmt:
      /*      report_qerror(s->loc, sev_warn,
	      "warning: ignoring in-line assembly code");*/
      return mksinfo(effect_empty);
      break;
    case kind_compound_stmt:
      {
	compound_stmt cs = CAST(compound_stmt, s);
	declaration decl;
	statement stmt;
	effect eff, local_env;
	dd_list new_drinfolist, old_drinfolist;

	old_drinfolist = drinfolist;
	new_drinfolist = dd_copy(parse_region, drinfolist);

	cs->enclosing_cs = cur_cs;
	cur_cs = cs;

	eff = effect_empty;
	local_env = env;

	/* Analyze the declarations. */
	scan_declaration(decl, cs->decls)
	  {
	    dinfo next;

	    assert(decl->kind != kind_asm_decl); /*asm_decl only at toplevel */
	    next = analyze_declaration(decl, local_env, new_drinfolist);
	    eff = effect_union(eff, next.eff);
	    local_env = effect_union(local_env, next.alocs);
	  }

	cs->scope_env = local_env;
	mark_effect_interesting(cs->scope_env);

	/* Analyze the body, gathering up the effects. */
	scan_statement(stmt, cs->stmts)
	  {
	    sinfo next = analyze_statement(stmt, local_env, new_drinfolist);
	    eff = effect_union(eff, next.eff);
	  }

	/* Add constraints from effects */
	eff = enforce_restricts(cs->decls, eff, NULL);
	enforce_readonly(cs->decls, eff);

	/* Handle deep restricts */

	/* Filter out non-escaping effects */
	cs->filter_env = filter_effects(NULL, env);
	mark_effect_interesting(cs->filter_env);
	eff = effect_inter(eff, cs->filter_env);

	/*
	printf("Filter line %ld is", cs->loc.lineno);
	print_effect(printf, eff);
	printf("\n");
	*/

	cur_cs = cs->enclosing_cs;

	return mksinfo(eff);
      };
      break;
    case kind_if_stmt:
      {
	if_stmt is = CAST(if_stmt, s);
	einfo guard;
	sinfo then;
	effect eff;

	guard = analyze_expression(is->condition, rpos, env, drinfolist);
	then = analyze_statement(is->stmt1, env, drinfolist);
	eff = effect_union(guard.eff, then.eff);

	if (is->stmt2)
	  {
	    sinfo els;

	    els = analyze_statement(is->stmt2, env, drinfolist);
	    eff = effect_union(eff, els.eff);
	  }

	return mksinfo(eff);
      };
      break;
    case kind_labeled_stmt:
      {
	labeled_stmt ls = CAST(labeled_stmt, s);

	ls->label->enclosing_cs = cur_cs;

	return analyze_statement(ls->stmt, env, drinfolist);
      };
      break;
    case kind_expression_stmt:
      {
	expression_stmt es = CAST(expression_stmt, s);
	einfo ei;

	ei = analyze_expression(es->arg1, rpos, env, drinfolist);
	return mksinfo(ei.eff);
      };
      break;
    case kind_while_stmt:
      {
	while_stmt ws = CAST(while_stmt, s);
	einfo guard;
	sinfo body;
	effect eff;

	ws->enclosing_cs = cur_cs;

	guard = analyze_expression(ws->condition, rpos, env, drinfolist);
	body = analyze_statement(ws->stmt, env, drinfolist);
	eff = effect_union(guard.eff, body.eff);
	return mksinfo(eff);
      };
      break;
    case kind_dowhile_stmt:
      {
	dowhile_stmt dws = CAST(dowhile_stmt, s);
	sinfo body;
	effect eff;

	dws->enclosing_cs = cur_cs;

	body = analyze_statement(dws->stmt, env, drinfolist);
	eff = body.eff;

	if (!definite_zero(dws->condition))
	  {
	    /* Catch do { x } while(0); case -- used in macro expansions */
	    einfo guard;

	    guard = analyze_expression(dws->condition, rpos, env, drinfolist);
	    eff = effect_union(eff, guard.eff);
	  }
	return mksinfo(eff);
      };
      break;
    case kind_switch_stmt:
      {
	switch_stmt ss = CAST(switch_stmt, s);
	einfo guard;
	sinfo body;
	effect eff;

	ss->enclosing_cs = cur_cs;

	guard = analyze_expression(ss->condition, rpos, env, drinfolist);
	body = analyze_statement(ss->stmt, env, drinfolist);
	eff = effect_union(guard.eff, body.eff);

	return mksinfo(eff);
      };
      break;
    case kind_for_stmt:
      {
	for_stmt fs = CAST(for_stmt, s);
	sinfo body;
	effect eff;

	fs->enclosing_cs = cur_cs;

	eff = effect_empty;

	if (fs->arg1)
	  {
	    einfo arg1;

	    arg1 = analyze_expression(fs->arg1, rpos, env, drinfolist);
	    eff = effect_union(eff, arg1.eff);
	  }

	if (fs->arg2)
	  {
	    einfo arg2;

	    arg2 = analyze_expression(fs->arg2, rpos, env, drinfolist);
	    eff = effect_union(eff, arg2.eff);
	  }

	body = analyze_statement(fs->stmt, env, drinfolist);
	eff = effect_union(eff, body.eff);

	if (fs->arg3)
	  {
	    einfo arg3;

	    arg3 = analyze_expression(fs->arg3, rpos, env, drinfolist);
	    eff = effect_union(eff, arg3.eff);
	  }

	return mksinfo(eff);
      };
      break;
    case kind_return_stmt:
      {
	return_stmt rs = CAST(return_stmt, s);
	qtype ret_qtype;
	effect eff;

	ret_qtype = cur_return_qtype();

	eff = effect_empty;
	if (rs->arg1)
	  {
	    einfo arg1;

	    arg1 = analyze_expression(rs->arg1, rpos, env, drinfolist);
	    eff = effect_union(eff, arg1.eff);
	    /* ret_qtype is an r-type, so no deref */
	    if (mkleq_assign_convert(rs->loc, arg1.qt, ret_qtype))
	      report_qerror(rs->loc, sev_err,
			    "return type incompatible with function type");
	  }
	/* Don't raise an error here -- will already be flagged by
	   parser if warn_return_type is enabled
	else
	  {
	    if (warn_return_type && !qtype_void(ret_qtype))
	      report_qerror(rs->loc, sev_err,
		    "`return' with no value, in function returning non-void");
	  }
	*/

	return mksinfo(eff);
      };
      break;
    case kind_computed_goto_stmt:
      {
	computed_goto_stmt cgs = CAST(computed_goto_stmt, s);
	einfo arg1;

	/* stmt is goto *arg1 */
	arg1 = analyze_expression(cgs->arg1, rpos, env, drinfolist);
	return mksinfo(arg1.eff);
      };
      break;
    case kind_break_stmt:
      {
	break_stmt bs = CAST(break_stmt, s);
	bs->enclosing_cs = cur_cs;
	
	return mksinfo(effect_empty);
      }
    case kind_continue_stmt:
      {
	continue_stmt cs = CAST(continue_stmt, s);
	cs->enclosing_cs = cur_cs;
	
	return mksinfo(effect_empty);
      }
    case kind_goto_stmt:
      {
	goto_stmt gs = CAST(goto_stmt, s);
	gs->enclosing_cs = cur_cs;

	return mksinfo(effect_empty);
      }
    case kind_empty_stmt:
      return mksinfo(effect_empty);
    case kind_change_type_stmt:
      {
	change_type_stmt ct = CAST(change_type_stmt, s);
	qtype new_qt;
	einfo arg1;
	aloc al;
	
	arg1 = analyze_expression(ct->arg1, lpos, env, drinfolist);
	qtype_mklhs_nonconst(ct->loc, arg1.qt);
	
	new_qt = type_to_qtype(ct->asttype->type, "__change", ct->loc);
	assert(!ct->qtype);
	ct->qtype = new_qt;
	al = aloc_qtype(arg1.qt);
	
	mark_aloc_interesting(al);
	
	assert(qtype_pointer(arg1.qt));
	if (mkleq_qtype(ct->loc, points_to_qtype(arg1.qt), new_qt))
	  report_qerror(ct->loc, sev_err,
			"new type incompatible with previous type");
	return mksinfo(effect_union(arg1.eff, effect_wr(al)));
      }
      break;
    case kind_assert_type_stmt:
      {
	assert_type_stmt at = CAST(assert_type_stmt, s);
	einfo arg1;
	type t;
	qtype assert_qt;
	
	/*arg1 = analyze_expression(at->arg1, rpos);*/
	arg1 = analyze_expression(at->arg1, rpos, env, drinfolist);
	t = at->asttype->type;
	assert_qt = type_to_qtype(t, "assert", at->asttype->loc);
	assert(!at->qtype);
	at->qtype = assert_qt;
	
	if (mkleq_qtype(at->loc, arg1.qt, assert_qt))
	  report_qerror(at->loc, sev_err,
			"unsatisfiable assertion");
	
	/* Pretend we have the effect of arg1, so that in 3rd pass
	   we'll be able to retrieve arg1 from the store, if necessary */
	return mksinfo(arg1.eff);
      }
      break;
    case kind_deep_restrict_stmt:
      {
	deep_restrict_stmt dr = CAST(deep_restrict_stmt, s);
	drinfo dri;
	aloc old_aloc, new_aloc;
	einfo arg1;
	sinfo stmt;
	effect eff = effect_empty;

	arg1 = analyze_expression(dr->arg1, lpos, env, drinfolist);
	assert(qtype_pointer(arg1.qt));
	old_aloc = aloc_qtype(arg1.qt);
	new_aloc = make_aloc(rstrcat(parse_region,name_aloc(old_aloc),
				     "_r"), points_to_qtype(arg1.qt));

	mark_aloc_interesting(old_aloc);
	mark_aloc_interesting(new_aloc);

	dri = ralloc(parse_region, struct Drinfo);
	dri->qtype = arg1.qt;
	dri->rqtype = mkqtype_pointer(qual_qtype(arg1.qt), 
				      points_to_qtype(arg1.qt), new_aloc);
	dri->effect = arg1.eff;
	dri->expression = dr->arg1;
	dri->location = dr->loc;
	dr->drinfo = dri;
	    
	/* Constraints for restrict */
	mknin_effect_qtype(dri->location, effect_rwr(new_aloc),
			   points_to_qtype(arg1.qt));
	mknin_effect(dri->location, effect_single(new_aloc), env);
	env = effect_union(effect_single(new_aloc), env);
	if (flag_flow_sensitive)
	  eff = effect_union(effect_alloc(new_aloc), eff);

	/* Constraints for referential transparency */
	forall_aloc_mknin_effect(dri->location, eff_wr, dri->effect);
	forall_aloc_mknin_effect(dri->location, eff_alloc, dri->effect);

	dd_add_first(parse_region, drinfolist, dri);

	/* Analyze the stmt */
	stmt = analyze_statement(dr->stmt, env, drinfolist);
	
	/* Constraints for restrict */
	mknin_effect(dri->location, effect_rwr(aloc_qtype(dri->qtype)), 
		     stmt.eff);

	/* Constraints for referential transparency */
	forall_aloc_in_effect_mknin_effect(dri->location,
					   eff_r, dri->effect, eff_wr, 
					   stmt.eff);

	eff = effect_union(eff, stmt.eff);
	eff = effect_union(eff, effect_rwr(aloc_qtype(dri->qtype)));

	return mksinfo(eff);
      }
    default:
      fail_loc(s->loc, "Unexpected statement kind 0x%x\n", s->kind);
      break;
    }
}

static einfo analyze_expression(expression e, context context, effect env,
				dd_list drinfolist)
{
  einfo result;
  dd_list_pos cur;

  /*  printf("Analyzing ");
      prt_expression(e, 0);
      printf(" in %s\n", context_tostring(context));*/

  /* deep restrict check */
  dd_scan(cur, drinfolist)
    {
      drinfo dri = DD_GET(drinfo, cur);
      if (equal_expressions(e, dri->expression))
	{
	  /* if r context, then must dereference once. */
	  if (context == rpos)
	    {
	      assert(qtype_pointer(dri->rqtype));
	      e->drinfo = dri;
	      e->qtype = points_to_qtype(dri->rqtype);
	      return mkeinfo(e->qtype,
			     effect_r(aloc_qtype(dri->rqtype)),
			     FALSE);
	    }
	  else
	    {
	      e->drinfo = dri;
	      e->qtype = dri->rqtype;
	      return mkeinfo(e->qtype, effect_empty, FALSE);
	    }
	}
    }
  e->drinfo = NULL;

  switch(e->kind) {
  case kind_comma:
    {
      comma c = CAST(comma, e);
      expression e2;
      effect eff;

      eff = effect_empty;
      scan_expression (e2, c->arg1)
	{
	  result = analyze_expression(e2, e2->next ? rpos : context, env,
				      drinfolist);
	  eff = effect_union(eff, result.eff);
	}
      if (!result.qt)
	fail_loc(c->loc, "Empty comma expression\n", 0);
      /* XXX: cparser has weird rule here, but I don't think it's right:
	 if (qtype_array(result))
	   result = default_conversion_qtype(result); */
      result = mkeinfo(result.qt, eff, FALSE);
    };
    break;
  case kind_sizeof_type:
    {
      qtype qt;

      assert(context == rpos);
      qt = mkqtype_size_t(make_qvar("sizeof_type", e->loc, FALSE));
      result = mkeinfo(qt, effect_empty, FALSE);
    }
    break;
  case kind_alignof_type:
    {
      qtype qt;

      assert(context == rpos);
      qt = mkqtype_size_t(make_qvar("alignof_type", e->loc, FALSE));
      result = mkeinfo(qt, effect_empty, FALSE);
    }
    break;
  case kind_label_address:
    {
      qtype points_to, qt;
      aloc loc;

      assert(context == rpos);
      points_to = mkqtype_void(make_qvar("&&_p", e->loc,FALSE));
      loc = make_aloc("&&", points_to);
      mkleq_effect(effect_single(loc), global_env);

      qt = mkqtype_pointer(make_qvar("&&", e->loc, FALSE),
			   points_to,
			   loc);

      /* Add && location to global effect. */
      mkleq_effect(effect_alloc(aloc_qtype(qt)), global_effect);

      result = mkeinfo(qt, effect_empty, FALSE);
    }
    break;
  case kind_cast:
    {
      cast c = CAST(cast, e);
      einfo arg1;
      type cast_t;
      qtype qt;
      effect eff;
      
      arg1 = analyze_expression(c->arg1, context, env, drinfolist);
      eff = arg1.eff;
      cast_t = c->asttype->type;
      if (context == lpos || context == apos)
	{
	  type pt;

	  assert(!type_function(cast_t));
	  assert(!type_array(cast_t));

	  pt = make_pointer_type(cast_t);
	  qt = type_to_qtype(pt, "cast", c->loc);
	}
      else
	qt = type_to_qtype(cast_t, "cast", c->loc);

      /* ad-hoc malloc handling in action */
      if (arg1.ismalloc)
	eff = effect_union(defn_effect_qtype(qt), eff);

      if (flag_casts_preserve &&
	  !type_void(cast_t) && /* cast to void is special -- it means,
				   ``ignore this result'' */
	  !type_contains_user_quals(cast_t))
	/* If cast_t doesn't have explicit user-defined qualifiers, */
	if (mkeq_qtype_cast(c->loc, qt, arg1.qt))
	  report_qerror(c->loc, sev_err,
			"Incompatible qualifiers at cast");

      result = mkeinfo(qt, eff, FALSE);
    };
    break;
  case kind_cast_list:
    {
      /* XXX Fix! */
      cast_list cl = CAST(cast_list, e);
      qtype qt;
      sinfo init;

      assert(context == rpos);
      qt = type_to_qtype(cl->type, "castlist", cl->loc);
      assert(!cl->astqtype);
      cl->astqtype = qt;
      init = analyze_init(cl->loc, qt, cl->init_expr, env, drinfolist);

      qt = default_conversion_qtype(qt);
      result = mkeinfo(qt, init.eff, FALSE);
    };
    break;
  case kind_conditional:
    {
      conditional c = CAST(conditional, e);
      einfo cond, arg1, arg2;
      effect eff;
      qtype qt;

      cond = analyze_expression(c->condition, rpos, env, drinfolist);
      eff = cond.eff;
      if (c->arg1)
	{
	  arg1 = analyze_expression(c->arg1, context, env, drinfolist);
	  eff = effect_union(eff, arg1.eff);
	}
      else
	/* gcc extension:  if you omit the middle op, evaluates to
	   guard when guard is true, and guard is not reevaluated */
	arg1 = cond;
      arg2 = analyze_expression(c->arg2, context, env, drinfolist);
      eff = effect_union(eff, arg2.eff);

      if (context == rpos)
	qt = type_to_qtype(c->type, "lub", c->loc);
      else
	qt = type_to_qtype(make_pointer_type(c->type), "lub", c->loc);

      if (qtype_void(arg1.qt))
	mkleq_assign_convert(c->loc, arg1.qt, qt);
      else if (qtype_void(arg2.qt))
	mkleq_assign_convert(c->loc, arg2.qt, qt);
      else if (mkleq_assign_convert(c->loc, arg1.qt, qt) ||
	       mkleq_assign_convert(c->loc, arg2.qt, qt))
	report_qerror(c->loc, sev_err,
		      "incompatible branches of conditional");
      result = mkeinfo(qt, eff, FALSE);
    };
    break;
  case kind_identifier:
    {
      identifier id = CAST(identifier, e);
      data_declaration root;
      qtype qt;

      root = root_ddecl(id->ddecl);
      if (is_op_name(root->name))
	report_qerror(id->loc, sev_err,
		      "use of built-in operator name");
      mark_used(root); /* We need this because we're marking the *root* */

      if (id->parent->kind != kind_function_call ||
	  CAST(function_call, id->parent)->arg1 != e)
	/* Mark if not in the callee position of a function call */
	mark_address_taken(root);

      qt = root->qtype;

      /*printf("Aloc of %s is ",id->cstring.data);
        print_aloc(printf,aloc_qtype(qt));
        printf("\n");*/

      if (!qt)
	{
	  /* Only functions can be implicitly declared.  But there are also
	     some special gcc identifiers, notably __FUNCTION__ and
	     __PRETTY_FUNCTION__, that also don't need to be declared. */
	  if ((type_function(id->ddecl->type) &&
	       id->ddecl->ftype == function_implicit) ||
	      !strcmp(id->ddecl->name, "__FUNCTION__") ||
	      !strcmp(id->ddecl->name, "__PRETTY_FUNCTION__") ||
	      !strcmp(id->ddecl->name, "=va_arg"))
	    {
	      __add_ddecl_qtype(root, TRUE);
	      qt = root->qtype;

	      /* 2p: These are gcc artifacts.  Add the effect of their
		 allocation to the global effect. */
	      mkleq_effect(defn_effect_qtype(qt), global_effect);
	    }
	  else
	    fail_loc(id->loc, "Use before declaration\n", 0);
	}

      if (qtype_scheme(qt))
	qt = instantiate_qtype(qt, id->loc);
      if (flag_pam_mode || flag_print_results)
	pam_add_overlay_file(id->loc, root->name, qt);
      if (root->kind == decl_constant)
	{
	  /* Constants are always rtypes */
	  if (context != rpos)
	    {
	      /* The front end should catch this */
	      report_qerror(id->loc, sev_err, "invalid lvalue");
	      /*qt = mkqtype_pointer(make_qvar("__err", id->loc, FALSE),
		qt);*/
	      qt = error_qtype;
	    }
	  result = mkeinfo(qt, effect_empty, FALSE);
	}
      else
	{
	  result = put_id_in_context(id->loc,
				     mkeinfo(qt, effect_empty, FALSE),
				     context);
	}
    };
    break;
  case kind_compound_expr:
    {
      compound_expr ce = CAST(compound_expr, e);
      compound_stmt cs = CAST(compound_stmt, ce->stmt);
      statement cur_stmt;
      declaration d;
      qtype qt;
      effect eff, local_env;
      dd_list new_drinfolist, old_drinfolist;

      old_drinfolist = drinfolist;
      new_drinfolist = dd_copy(parse_region, drinfolist);



      cs->enclosing_cs = cur_cs;
      cur_cs = cs;

      if (cs->id_labels)
	fail_loc(cs->loc, "Unimplemented: id_labels\n", 0);

      eff = effect_empty;
      local_env = env;

      /* Analyze the declarations in the block */
      scan_declaration(d, cs->decls)
	{
	  dinfo next;

	  assert(d->kind != kind_asm_decl); /*asm_decl only at toplevel */
	  next = analyze_declaration(d, local_env, new_drinfolist);
	  eff = effect_union(eff, next.eff);
	  local_env = effect_union(local_env, next.alocs);
	}

      cs->scope_env = local_env;
      mark_effect_interesting(cs->scope_env);

      /* Analyze the statements in the block.  Analyze all but the
         last one. */
      cur_stmt = cs->stmts;
      while (cur_stmt && cur_stmt->next)
	{
	  sinfo next = analyze_statement(cur_stmt, local_env, new_drinfolist);
	  eff = effect_union(eff, next.eff);
	  cur_stmt = CAST(statement, cur_stmt->next);
	}

      /* Now analyze the last statement (if there is one), and
         compute the type of the expression. */
      if (cur_stmt && is_expression_stmt(cur_stmt))
	{
	  einfo next;

	  next = analyze_expression(CAST(expression_stmt, cur_stmt)->arg1,
				    context,
				    local_env, new_drinfolist);

	  eff = effect_union(eff, next.eff);
	  qt = next.qt;
	}
      else
	{
	  /* Type is void */
	  qt = mkqtype_void(make_qvar("compound", ce->loc, FALSE));
	  if (cur_stmt)
	    {
	      sinfo next = analyze_statement(cur_stmt, local_env, 
					     new_drinfolist);
	      eff = effect_union(eff, next.eff);
	    }
	}

      /* Add constraints from effects */
      eff = enforce_restricts(cs->decls, eff, qt);
      enforce_readonly(cs->decls, eff);

      /* Filter out non-escaping effects */
      ce->filter_env = filter_effects(qt, env);
      mark_effect_interesting(ce->filter_env);
      eff = effect_inter(eff, ce->filter_env);

      cur_cs = cs->enclosing_cs;

      result = mkeinfo(qt, eff, FALSE);
    };
    break;
  case kind_function_call:
    {
      function_call fc = CAST(function_call, e);
      einfo f_info, magic_info;
      qtype fqt;
      qtypelist args;
      expression arg;
      qtypelist_scanner args_qs;
      int i;
      effect eff;

      /* Extract the function type of the expr in fun posn.  Note
         that arg1 must be a pointer to a function.  From C9X:

	 [#1]  The  expression  that  denotes  the called function
	 shall have  type  pointer  to  function  returning  void  or
	 returning an object type other than an array type. */

      assert(context == rpos);
      if (fc->va_arg_call)
	{
	  qtype va_arg_qtype;

	  /* Magic call to __builtin_va_arg(args, fc->va_arg_call) */
	  va_arg_qtype = type_to_qtype(fc->va_arg_call->type,
				       "_va_arg",
				       fc->loc);

	  result = mkeinfo(va_arg_qtype, effect_empty, FALSE);
	  break;
	}

      f_info = analyze_expression(fc->arg1, rpos, env, drinfolist);
      fqt = points_to_qtype(f_info.qt);
      args = arg_qtypes(fqt);

      magic_info = analyze_magic_function_call(fc, fqt, env, drinfolist);
      if (magic_info.qt)
	{
	  magic_info.eff = effect_union(f_info.eff, magic_info.eff);
	  result = magic_info;
	  break;
	}

      eff = effect_union(f_info.eff, effect_qtype(fqt));

      /* Analyze each argument, making a constrant with the
	 corresponding formal paramter */
      qtypelist_scan(args, &args_qs);
      i = 1;
      scan_expression(arg, fc->args)
	{
	  einfo arg_info;
	  qtype formal_qt;
	  bool warned_oldstyle;

	  arg_info = analyze_expression(arg, rpos, env, drinfolist);
	  eff = effect_union(eff, arg_info.eff);
	  formal_qt = qtypelist_next(&args_qs);
	  warned_oldstyle = FALSE;

	  if (!formal_qt)
	    {
	      if (!qtype_varargs(fqt) && !qtype_oldstyle(fqt))
		report_qerror(arg->loc, sev_err,
			      "too many actual arguments for function "
			      "call.\n");
	      else if (qtype_oldstyle(fqt) && !warned_oldstyle)
		{
		  /* Warn about calls to oldstyle functions, but
		     not __builtins */
		  if (fc->arg1->kind == kind_identifier)
		    {
		      identifier fid = CAST(identifier, fc->arg1);
		      if (strstr(fid->cstring.data, "__builtin") !=
			  fid->cstring.data)
			report_qerror(fc->loc, sev_warn,
				      "warning: ignoring argument types "
				      "in call to oldstyle function");
		    }
		  warned_oldstyle = TRUE;
		}

	      /* If func is varargs, then only add constraints if func
	         has a varargs qualifier. */
	      if (qtype_varargs(fqt) && vqual_qtype(fqt))
		{
		  qual vq;

		  vq = vqual_qtype(fqt);
		  if (varargs_constrain_quals_qtype(arg->loc,
						    arg_info.qt, vq))
		    report_qerror(arg->loc, sev_err,
				  "type of actual argument %d doesn't match "
				  "varargs qualifier", i);
		}
	    }
	  else
	    if (mkleq_assign_convert(arg->loc, arg_info.qt, formal_qt))
	      report_qerror(arg->loc, sev_err,
			    "type of actual argument %d doesn't match "
			    "type of formal", i);
	  i++;
	}
      if (qtypelist_next(&args_qs))
	fail_loc(e->loc, "Not enough actual arguments.\n", 0);

      result = mkeinfo(return_qtype(fqt), eff, FALSE);
    };
    break;
  case kind_array_ref:
    {
      array_ref ar = CAST(array_ref, e);
      expression array, plus, star_plus;

      if (type_array(ar->arg1->type))
	array = ar->arg1;
      else
	array = ar->arg2;

      array->lvalue = TRUE; /* XXX: Hack to fix problem
				  w/default_conversion */
      array->cst = NULL; /* XXX: Hack to fix problem w/default_conversion */

      plus = make_binary(ar->loc, kind_plus, ar->arg1, ar->arg2);
      star_plus = make_dereference(ar->loc, plus);
      assert(!ar->alt);
      ar->alt = star_plus;

      result = analyze_expression(star_plus, context, env, drinfolist);
    };
    break;
  case kind_field_ref:
    {
      /* We unify the qualifiers on the structure and the qualifiers
	 on the field.  For now.  See previous versions for other
	 approaches. */
      field_ref fr = CAST(field_ref, e);
      einfo arg1;
      qtype field_qt;
      effect eff;

      arg1 = analyze_expression(fr->arg1, context, env, drinfolist);
      eff = arg1.eff;

      /*** Compute field qtypes ***/

      if (qtype_error(arg1.qt))
	field_qt = error_qtype;
      else
	{
	  qtype qt, ag_qt;

	  qt = arg1.qt;
	  if (context == lpos || context == apos)
	    {
	      ag_qt = points_to_qtype(qt);
	      eff = effect_union(eff, effect_r(aloc_qtype(qt)));
	    }
	  else
	    ag_qt = qt;
	  assert(qtype_aggregate(ag_qt));

	  field_qt = field_qtype(ag_qt, fr->cstring);
	}

      if (flag_pam_mode || flag_print_results)
	pam_add_overlay_file(fr->cstring_loc, fr->cstring.data, field_qt);

      /* unify the qualifiers on the field contents and the structure value */
      /*
      if (mkeq_qual(qual_qtype(field_rqt), qual_qtype(ag_qt)))
	report_qerror(e->loc,
		      "Qualifiers on field ``%s'' incomptabile with "
		      "access path qualifiers (in r-pos)", fr->cstring);
      */
	  /*
	    if (context == lpos)
	    {
	    if (mkeq_qual(qual_qtype(qt), qual_qtype(field_qt)))
	    report_qerror(e->loc,
		      "Qualifiers on field ``%s'' incomptabile with "
		      "access path qualifiers (in l-pos)", fr->cstring);
	  */
      /* XXX: arg1.eff ok? */
      result = put_id_in_context(fr->loc,
				 mkeinfo(field_qt, eff, FALSE),
				 context);
    };
    break;
  case kind_init_list:
    {
      /*    init_list il = CAST(init_list, e);*/
      fail_loc(e->loc, "Unexpected init list\n", 0);
    };
    break;
  case kind_init_index:
    {
      /*    init_index ii = CAST(init_index, e);*/
      fail_loc(e->loc, "Unexpected init index\n", 0);
    };
    break;
  case kind_init_field:
    {
      fail_loc(e->loc, "Unexpected init field\n", 0);
    };
  case kind_known_cst:
    {
      /* Should check size */
      /*    signed_cst sc = CAST(signed_cst, e);*/
      qtype qt;

      assert(context == rpos);
      qt = type_to_qtype(e->type, "__cst", e->loc);
      result = mkeinfo(qt, effect_empty, FALSE);
    };
    break;
  case kind_lexical_cst:
    {
      lexical_cst lc = CAST(lexical_cst, e);
      qtype qt;

      assert(context == rpos);

      /* If you assign a special type to the constant ``1'', also
	 modify pre/post inc/decrement.
	 If you assign a special type to the constant ``0'', also
	 modify mkleq_assign_convert.
      */

      qt = type_to_qtype(lc->type, "__cst", lc->loc);
      result = mkeinfo(qt, effect_empty, FALSE);
    };
    break;
  case kind_string:
    {
      /* Could use string name -- see string_to_charp */
      qtype points_to, qt;
      aloc loc;

      assert(context == rpos);
      points_to = mkqtype_char(make_qvar("__string_p", e->loc, FALSE));
      loc = make_aloc("__string", points_to);
      mkleq_effect(effect_single(loc), global_env);
      qt = mkqtype_pointer(make_qvar("__string", e->loc, FALSE),
			   points_to,
			   loc);

      /* Add effect of allocating this string to the global effect. */
      mkleq_effect(effect_alloc(loc), global_effect);

      result = mkeinfo(qt, effect_empty, FALSE);
    };
    break;
  default:
    if (is_unary(e))
      result = analyze_unary_expression(CAST(unary, e), context, env, 
					drinfolist);
    else if (is_binary(e))
      result = analyze_binary_expression(CAST(binary, e), context, env, 
					 drinfolist);
    else
      fail_loc(e->loc, "Unexpected expr kind 0x%x\n", e->kind);
  }

  e->qtype = result.qt;
  return result;
}

static einfo analyze_unary_expression(unary e, context context, effect env,
				      dd_list drinfolist)
{
  operator op;

  op = find_op_kind(e->kind);  /* Look up the signature of this operator */

  switch (e->kind)
    {
    case kind_dereference:
      {
	/* Analyze the subexpression in an r-context; this will return
	   an l-type, so either return that if we're in an l-context or
	   deref it if we're in an r-context.  Function are special --
	   *f is allowed even if f is a function and not a function
	   pointer type. */
	einfo arg1, result;
	effect eff;

	arg1 = analyze_expression(e->arg1, rpos, env, drinfolist);
	if (!qtype_pointer(arg1.qt))
	  {
	    report_qerror(e->loc, sev_err,
			  "dereference of non-pointer type (probably a "
			  "type incompatibility across files)");
	    return mkeinfo(error_qtype, effect_empty, FALSE);
	  }
	assert(qtype_pointer(arg1.qt));
	eff = effect_union(arg1.eff, effect_r(aloc_qtype(arg1.qt)));
	if (context == lpos || context == apos)
	  /* *e in an l-context silently converts a (pointer) r-type
	     to an l-type */
	  result = mkeinfo(arg1.qt, eff, FALSE);
	else if (context == rpos)
	  {
	    if (qtype_function(arg1.qt))
	      /*return arg1_qtype;*/
	      fail_loc(e->loc, "Unexpected function in rpos", 0);
	    else
	     result = mkeinfo(default_conversion_qtype(points_to_qtype(arg1.qt)),
			      eff, FALSE);
	  }
	else
	  fail_loc(e->loc, "Unexpected context %d\n", context);

	if (op)
	  {
	    einfo result_rpos;

	    if (context == rpos)
	      result_rpos = result;
	    else
	      result_rpos = mkeinfo(default_conversion_qtype(points_to_qtype(arg1.qt)),
				    eff, FALSE);

	    if (add_op_constraints(op, result_rpos, arg1, dummy_einfo,
				   e->loc))
	      report_qerror(e->loc, sev_err,
			    "Operands incompatible with operator definition");

	  }

	return result;
      }
      break;
    case kind_address_of:
      {
	/* &e can only appear in an r-context.  We analyze the
	   subexpression in an l-context (thus the subexpression type
	   with have a ptr at the outside) and silently convert the
	   l-type to an r-type.  On the other hand, taking the address
	   of a function type is a no-op. */
	if (context != rpos)
	  fail_loc(e->loc, "Unexpected & in non rpos\n", 0);
	if (type_function(e->arg1->type))
	  return analyze_expression(e->arg1, context, env, drinfolist);
	else
	  return analyze_expression(e->arg1, apos, env, drinfolist);
      }
      break;
    case kind_extension_expr:
      {
	return analyze_expression(e->arg1, context, env, drinfolist);
      }
      break;
    case kind_sizeof_expr:
      {
	qtype qt;

	assert(context == rpos);
	/*analyze_expression(e->arg1, rpos); (arg1 not evaluated) */
	qt = mkqtype_size_t(make_qvar("sizeof", e->loc, FALSE));
	return mkeinfo(qt, effect_empty, FALSE);
      }
      break;
    case kind_alignof_expr:
      {
	qtype qt;

	assert(context == rpos);
	/*analyze_expression(e->arg1, rpos); (arg1 not evaluated) */
	qt = mkqtype_size_t(make_qvar("alignof", e->loc, FALSE));
	return mkeinfo(qt, effect_empty, FALSE);
      }
      break;
    case kind_realpart:
    case kind_imagpart:
      {
	einfo arg1;

	assert(context == rpos);
	arg1 = analyze_expression(e->arg1, rpos, env, drinfolist);
	assert(qtype_complex(arg1.qt));
	return mkeinfo(complex_base_qtype(arg1.qt), arg1.eff, FALSE);
      }
      break;
    case kind_unary_minus:
    case kind_unary_plus:
    case kind_conjugate:
    case kind_bitnot:
      {
	/* These operations don't change types.  Note we assume
	   that compatibility with these ops has already been
	   tested. */
	assert(context == rpos);
	return analyze_expression(e->arg1, rpos, env, drinfolist);
      }
      break;
    case kind_not:
      {
	einfo arg1;
	qtype qt;

	assert(context == rpos);
	arg1 = analyze_expression(e->arg1, rpos, env, drinfolist);
	qt = mkqtype_bool(make_qvar("!", e->loc, FALSE));
	return mkeinfo(qt, arg1.eff, FALSE);
      }
      break;
    case kind_preincrement:
    case kind_postincrement:
    case kind_predecrement:
    case kind_postdecrement:
      {
	einfo arg1;
	effect eff;

	assert(context == rpos);
	arg1 = analyze_expression(e->arg1, lpos, env, drinfolist);
	qtype_mklhs_nonconst(e->loc, arg1.qt);
	eff = effect_union(arg1.eff, effect_wr(aloc_qtype(arg1.qt)));
	return mkeinfo(points_to_qtype(arg1.qt),
		       eff, FALSE);
      }
      break;
    default:
      fail_loc(e->loc, "Unexpected unary op kind 0x%x\n", e->kind);
    }
}

/* Return TRUE iff e is a form of assignment */
bool isassignment(binary e)
{
  return (e->kind == kind_plus_assign ||
	  e->kind == kind_minus_assign ||
	  e->kind == kind_modulo_assign ||
	  e->kind == kind_lshift_assign ||
	  e->kind == kind_rshift_assign ||
	  e->kind == kind_times_assign ||
	  e->kind == kind_divide_assign ||
	  e->kind == kind_bitand_assign ||
	  e->kind == kind_bitor_assign ||
	  e->kind == kind_bitxor_assign ||
	  e->kind == kind_assign);
}

static einfo analyze_binary_expression(binary e, context context, effect env,
				       dd_list drinfolist)
{
  einfo arg1, arg2;
  operator op;
  effect eff;

  assert(context == rpos);
  op = find_op_kind(e->kind);  /* Look up the signature of this operator */
  if (isassignment(e))
    arg1 = analyze_expression(e->arg1, lpos, env, drinfolist);
  else
    arg1 = analyze_expression(e->arg1, rpos, env, drinfolist);
  arg2 = analyze_expression(e->arg2, rpos, env, drinfolist);
  eff = effect_union(arg1.eff, arg2.eff);

  if (qtype_error(arg1.qt) || qtype_error(arg2.qt))
    return mkeinfo(error_qtype, eff, FALSE);

  switch (e->kind)
    {
    case kind_plus:
      {
	qtype qt;

	if (qtype_pointer(arg1.qt) && qtype_pointer(arg2.qt))
	  fail_loc(e->loc, "Unexpected pointer addition\n", 0);

	if (qtype_pointer(arg1.qt) && !qtype_pointer(arg2.qt))
	  qt = arg1.qt;
	else if (!qtype_pointer(arg1.qt) && qtype_pointer(arg2.qt))
	  qt = arg2.qt;
	else
	  {
	    /* No lub here -- when would lub ever make sense? */
	    if (mkeq_qtype(e->loc, arg1.qt, arg2.qt))
	      report_qerror(e->loc, sev_err,
			    "incompatible operands of ``%s''",
			    binary_op_name(e->kind));
	    qt = arg1.qt;
	  }

	return mkeinfo(qt, eff, FALSE);
      }
      break;
    case kind_minus:
      {
	qtype qt;

	if (qtype_pointer(arg1.qt) && qtype_pointer(arg2.qt))
	  qt = mkqtype_ptrdiff_t(make_qvar("__ptr diff", e->loc, FALSE));
	else if (qtype_pointer(arg1.qt) && !qtype_pointer(arg2.qt))
	  qt = arg1.qt;
	else if (!qtype_pointer(arg1.qt) && qtype_pointer(arg2.qt))
	  qt = arg2.qt;
	else
	  {
	    /* No lub here -- when would lub ever make sense? */
	    if (mkeq_qtype(e->loc, arg1.qt, arg2.qt))
	      report_qerror(e->loc, sev_err,
			    "incompatible operands of ``%s''",
			    binary_op_name(e->kind));
	    qt = arg1.qt;
	  }

	return mkeinfo(qt, eff, FALSE);
      }
      break;
    case kind_modulo:
    case kind_lshift:
    case kind_rshift:
      {
	return mkeinfo(arg1.qt, eff, FALSE);
      }
    case kind_modulo_assign:
    case kind_lshift_assign:
    case kind_rshift_assign:
      {
	qtype_mklhs_nonconst(e->loc, arg1.qt);
	return mkeinfo(points_to_qtype(arg1.qt),
		       effect_union(eff, effect_wr(aloc_qtype(arg1.qt))), 
		       FALSE);
      }
    case kind_times:
    case kind_divide:
    case kind_bitand:
    case kind_bitor:
    case kind_bitxor:
      {
	/* Got rid of lub -- when would lub make sense? */
	if (mkeq_qtype(e->loc, arg1.qt, arg2.qt))
	  report_qerror(e->loc, sev_err,
			"incompatible operands of ``%s''",
			binary_op_name(e->kind));
	return mkeinfo(arg1.qt, eff, FALSE);
      }
      break;
    case kind_leq:
    case kind_geq:
    case kind_lt:
    case kind_gt:
    case kind_eq:
    case kind_ne:
    case kind_andand:
    case kind_oror:
      {
	qtype qt;
	einfo result;

	qt = mkqtype_bool(make_qvar("__boolop", e->loc, FALSE));

	/* XXX: Ick! */
	if ((e->kind == kind_eq || e->kind == kind_ne) &&
	    ((file_pointer_qtype(arg1.qt) && definite_null(e->arg2)) ||
	     (definite_null(e->arg1) && file_pointer_qtype(arg2.qt))))
	  /* We'll do an assign in the flow-sensitive pass, so add an
	     effect here */
	  {
	    aloc al;

	    if (file_pointer_qtype(arg1.qt))
	      al = aloc_qtype(arg1.qt);
	    else
	      al = aloc_qtype(arg2.qt);

	    mark_aloc_interesting(al);
	    eff = effect_union(eff, effect_wr(al));
	  }

	result = mkeinfo(qt, eff, FALSE);
	if (op && add_op_constraints(op, result, arg1, arg2, e->loc))
	  report_qerror(e->loc, sev_err,
			"Operands incompatible with operator definition");
	return result;
      }
    case kind_plus_assign:
    case kind_minus_assign:
      {
	qtype arg1_pt;

	arg1_pt = points_to_qtype(arg1.qt);

	if ((qtype_pointer(arg1_pt) && qtype_pointer(arg2.qt)))
	  fail_loc(e->loc, "Unexpected pointer addition\n", 0);

	if (qtype_pointer(arg1_pt) && !qtype_pointer(arg2.qt))
	  {
	    /* In p += count, no interaction between p and count's
	       qualifiers */
	    qtype_mklhs_nonconst(e->loc, arg1.qt);
	    return mkeinfo(arg1_pt,
			   effect_union(eff, effect_wr(aloc_qtype(arg1.qt))),
			   FALSE);
	  }
      }
      /* else fall through to next case */
    case kind_times_assign:
    case kind_divide_assign:
    case kind_bitand_assign:
    case kind_bitor_assign:
    case kind_bitxor_assign:
    case kind_assign:
      {
	if (arg2.ismalloc)
	  eff = effect_union(defn_effect_qtype(points_to_qtype(arg1.qt)), eff);

	qtype_mklhs_nonconst(e->loc, arg1.qt);
	mark_aloc_interesting(aloc_qtype(arg1.qt));
	if (mkleq_assign_convert(e->loc, arg2.qt, points_to_qtype(arg1.qt)))
	  report_qerror(e->loc, sev_err,
			"incompatible types in assignment");

	return mkeinfo(points_to_qtype(arg1.qt),
		       effect_union(eff, effect_wr(aloc_qtype(arg1.qt))),
		       FALSE);
      }
    default:
      fail_loc(e->loc, "Unexpected binary op kind 0x%x\n", e->kind);
    }
}

/* Given the type of an identifier, convert it to the right type in
   the current context. */
einfo put_id_in_context(location loc, einfo ei, context c)
{
  qtype result_qt;
  effect result_eff;

  result_eff = ei.eff;
  if (qtype_error(ei.qt))
    result_qt = error_qtype;
  else if (qtype_array(ei.qt)) {
    /* Arrays are always in r-positions, and when they're used,
       they're actually pointers.  Well, not when they're used in
       sizeof and as the target of &, but you can't take & of an
       array, and in the other places we don't care (see C9X).
       EXCEPT you can take the address of an array, so that
       is considered an apos.  Yuck. */
    if (c == rpos)
      result_qt = default_conversion_qtype(ei.qt);
    else if (c == apos)
      result_qt = mkqtype_pointer(make_fresh_qvar("q", loc),
				  ei.qt,
				  array_aloc_qtype(ei.qt));
    else
      fail("Unexpected context %d\n", c);
  }
  else if (qtype_function(ei.qt)) {
    assert(c == rpos);
    /* Function are promoted to function pointers.  Hack: If we
       are in an & context, then still return a pointer.  The &
       only changes an l-type to an r-type by doing nothing, thus
       it will not change the fun ptr type! */
    result_qt = default_conversion_qtype(ei.qt);
  }
  else if (c == lpos || c == apos)
    result_qt = ei.qt;
  else if (c == rpos)
    {
      if (qtype_pointer(ei.qt))
	{
	  result_qt = points_to_qtype(ei.qt);
	  result_eff = effect_union(result_eff, /* Implicit deref */
				    effect_r(aloc_qtype(ei.qt)));
	}
      else
	fail("Unexpected qtype\n");

    }
  else fail_loc(loc, "Unexpected context %d\n", c);

  return mkeinfo(result_qt, result_eff, FALSE);
}

/**************************************************************************
 *                                                                        *
 * Qtype manipulation routines                                            *
 *                                                                        *
 **************************************************************************/

/* Make the left-hand side of an assignment non-const.  Prints
   an error message if this causes an error. */
void qtype_mklhs_nonconst(location loc, qtype lhs_qtype)
{
  if (mkNonConst_pointer(loc, lhs_qtype))
    report_qerror(loc, sev_err,
		  "left-hand side of assignment is const");
}

/* Make q nonconst, if nonconst is a qualifier */
bool mkNonConst_qual(location loc, qual q)
{
  if (!nonconst_qual)
    return FALSE;
  return mkleq_qual(loc, q, nonconst_qual);
}

/* Make a pointer (l-) type non-const.  Returns true if there was an
   error, false if the constraint is currently valid. */
bool mkNonConst_pointer(location loc, qtype ptr_qtype)
{
  bool result = FALSE;
  qtype contents_qtype;

  if (!nonconst_qual)
    return FALSE;
  assert(qtype_pointer(ptr_qtype));
  result = mkNonConst_qual(loc, qual_qtype(ptr_qtype)) || result;
  contents_qtype = points_to_qtype(ptr_qtype);
  if (qtype_aggregate(contents_qtype))
    result = mkNonConst_aggregate(loc, contents_qtype) || result;
  return result;
}

/* Make a pointer (l-)type const.  Returns true if there was an error. */
bool mkConst_pointer(location loc, qtype ptr_qtype)
{
  bool result = FALSE;

  if (!const_qual)
    return FALSE;
  assert(qtype_pointer(ptr_qtype));
  result = mkleq_qual(loc, const_qual, qual_qtype(ptr_qtype));
  /* XXX: Do something for aggregate */
  return result;
}

/* Make all the fields of an aggregate non-const, but not through pointers
   (but through arrays!).  Returns true if there was an error, false
   if the constraint is currently valid. */
bool mkNonConst_aggregate(location loc, qtype ag_qtype)
{
  field_scanner fields;
  qtype field_qtype, contents_qtype;
  bool result = FALSE;
  
  if (!nonconst_qual)
    return FALSE;

  field_scan(ag_qtype, &fields);
  while ((field_qtype = field_next(&fields)))
    {
      /* const *does* propagate through arrays.  This makes sense
  	 because arrays nested in structs are part of the same
  	 struct object, whereas pointers point to other objects.
	 
	 So if we have an array(t) type, we move through the type
  	 until t is not an array. */
      if (qtype_array(field_qtype))
  	{
  	  contents_qtype = array_of_qtype(field_qtype);
  	  while (qtype_array(contents_qtype))
  	    {
  	      field_qtype = contents_qtype;
  	      contents_qtype = array_of_qtype(field_qtype);
  	    }
  	}
      else
  	contents_qtype = points_to_qtype(field_qtype);
      
      /* Now we make field_qtype (the most deeply nested array qtype,
  	 or the ptr qtype) non-const, and then if the contents is
  	 a struct/union we recurse. */
      result = mkNonConst_qual(loc, qual_qtype(field_qtype)) || result;
      if (qtype_aggregate(contents_qtype))
  	result = mkNonConst_aggregate(loc, contents_qtype) || result;
    }
  return result;
}

/**************************************************************************
 *                                                                        *
 * Functions for restrict                                                 *
 *                                                                        *
 **************************************************************************/

/* If vd's type has a restrict, then generate an rinfo record for vd
   and add the appropriate constraints.  Returns TRUE if vd's type has
   a valid restrict and an rinfo record was generated. */
bool restrict_variable_decl(variable_decl vd, bool isglobal)
{
  data_declaration root;
  qtype rqtype, lqtype;

  root = root_ddecl(vd->ddecl);

  if (!root->qtype)
    return FALSE;

  lqtype = root->qtype;
  if (!qtype_pointer(lqtype))
    return FALSE;       /* Only pointers can be annotated with restrict */

  rqtype = points_to_qtype(lqtype);

  if (restrict_qual &&
      has_qual_qtype(rqtype, restrict_qual))
    {
      assert(qtype_pointer(rqtype));  /* Enforced by add_ddecl_qtype */

      /* If value of pointer is restrict, the lvalue itself
	 must be const (well, or restrict, but we have no
	 notation for that) */
      if (const_qual && has_qual_qtype(lqtype, const_qual))
	{
	  rinfo ri;
	  qtype rqtype_points_to, new_rqtype, new_lqtype;
	  aloc old_aloc, old_toplevel_aloc, new_aloc, new_toplevel_aloc;

	  assert(!vd->rinfo);
	  old_toplevel_aloc = aloc_qtype(lqtype);
	  old_aloc = aloc_qtype(rqtype);

	  /* Construct new qtype.  We construct a new aloc for the pointed-to
	     location, and also for the container location */
	  rqtype_points_to = points_to_qtype(rqtype);
	  new_aloc = make_aloc(rstrcat(parse_region,name_aloc(old_aloc),"_r"),
			       rqtype_points_to);

	  new_rqtype = mkqtype_pointer(qual_qtype(rqtype),
				       rqtype_points_to,
				       new_aloc);

	  new_toplevel_aloc = make_aloc(rstrcat(parse_region,
						name_aloc(old_toplevel_aloc),
						"_rtl"),
					new_rqtype);

	  new_lqtype = mkqtype_pointer(qual_qtype(lqtype),
				       new_rqtype,
				       new_toplevel_aloc);

	  ri = ralloc(parse_region, struct Rinfo);
	  ri->isglobal = isglobal;
	  ri->fs_fqtype = NULL;
	  ri->fs_rqtype = NULL;
	  ri->lin = NULL;

	  if (!isglobal)
	    {
	      ri->flocs = effect_rwr(old_aloc);
	      ri->flocs_alloc = effect_alloc(old_aloc);
	      ri->fqtype = lqtype;
	      ri->rlocs = effect_rwr(new_aloc);
	      ri->rlocs_alloc = effect_alloc(new_aloc);
	      ri->rqtype = new_lqtype;
	    }
	  else
	    {
	      ri->flocs = effect_rwr(new_aloc);
	      ri->flocs_alloc = effect_alloc(new_aloc);
	      ri->fqtype = new_lqtype;
	      ri->rlocs = effect_rwr(old_aloc);
	      ri->rlocs_alloc = effect_alloc(old_aloc);
	      ri->rqtype = lqtype;
	    }

	  vd->rinfo = ri;
	  
	  mark_aloc_interesting(old_aloc);
	  mark_aloc_interesting(new_aloc);

	  /* Add constraints on new_aloc */
	  /* insist(!mkneq_aloc(new_aloc, old_aloc)); -- unnecessary */
	  mknin_effect_qtype(vd->loc, effect_rwr(new_aloc),
			     points_to_qtype(rqtype));
	  /* mknin_effect(vd->rlocs, env, vd->loc); */
	  /* env = effect_union(vd->rlocs, env); */
	  return TRUE;
	}
      else
	report_qerror(vd->loc, sev_err,
		      "lvalue of restrict-qualified pointer is "
		      "non-const -- ignoring restrict");
    }
  return FALSE;
}

/* Given the effect eff and qtype qt of code in the scope of vd (a
   list of variable declarations), verify that none of the conditions
   imposed by restricts in dls are violated.  qt can be NULL,
   indicating the code has type void. */
effect enforce_var_decl_restrict(variable_decl vd, effect eff, qtype qt)
{
  if (vd && vd->rinfo)
    {
      mknin_effect(vd->loc, vd->rinfo->flocs, eff);
      /* report_qerror(vd->loc, sev_err,
		      "restricted location accessed within scope of "
		      "declaration"); */

      if (qt != NULL && !(eq_effect(vd->rinfo->rlocs, effect_empty)))
	mknin_effect_qtype(vd->loc, vd->rinfo->rlocs, qt);
     
      /* report_qerror(vd->loc, sev_err,
	 "restricted location escapes scope of declaration"); */

      /* We could also reset vd's qtype back to its unrestricted qtype,
	 but what's the point? */

      eff = effect_union(eff, vd->rinfo->flocs);
    }

  eff = effect_union(eff, vd->arg1_eff);
  return eff;
}

/* Given the effect eff and qtype qt of code in the scope of d (a list
   of declarations), verify that none of the conditions imposed by
   restricts in dls are violated.  qt can be NULL, indicating the code
   has type void. */
effect enforce_restricts(declaration d, effect eff, qtype qt)
{
  if (d == NULL)
    return eff;

  eff = enforce_restricts(CAST(declaration, d->next), eff, qt);
  switch (d->kind)
    {
    case kind_asm_decl:
    case kind_function_decl:
    case kind_ellipsis_decl:
    case kind_oldidentifier_decl:
      return eff;
    case kind_extension_decl:
      {
	extension_decl ed = CAST(extension_decl, d);

	assert(ed->decl->next == NULL);
	return enforce_restricts(ed->decl, eff, qt);
      }
    case kind_data_decl:
      {
	declaration vd;

	scan_declaration(vd, CAST(data_decl, d)->decls)
	  eff = enforce_var_decl_restrict(CAST(variable_decl, vd), eff, qt);
	return eff;
      }
    default:
      fail_loc(d->loc, "Unexpected decl kind %x\n", d->kind);
    }
}

/* Return effects that appear in qt and alocs */
effect filter_effects(qtype qt, effect alocs)
{
  /* This implements (Down).  To get rid of (Down), simply replace the
     body of this function by ``return eff;''. */
  effect filter;

  filter = alocs;
  if (qt)
    filter = effect_union(filter, alocs_qtype(qt));
  if (current_function_decl)
    filter = effect_union(filter, alocs_qtype(cur_return_qtype()));
  return filter;
}

/**************************************************************************
 *                                                                        *
 * Magic -- special interpretations of certain syntax                     *
 *                                                                        *
 **************************************************************************/

static int fopen_count = 0;
static int alloc_count = 0;
bool have_stream_quals = FALSE;
bool have_sleepy_quals = FALSE;
qual open_unchecked_qual = NULL;
qual read_unchecked_qual = NULL;
qual write_unchecked_qual = NULL;
qual readwrite_unchecked_qual = NULL;
qual open_qual = NULL;
qual read_qual = NULL;
qual write_qual = NULL;
qual readwrite_qual = NULL;
qual closed_qual = NULL;
qual enabled_qual = NULL;
qual disabled_qual = NULL;
qual readonly_qual = NULL;
qtype interrupt_status_qtype = NULL;

static void init_magic(void)
{
  fopen_count = 0;
  alloc_count = 0;
  open_unchecked_qual = find_qual("$open_unchecked");
  read_unchecked_qual = find_qual("$read_unchecked");
  write_unchecked_qual = find_qual("$write_unchecked");
  readwrite_unchecked_qual = find_qual("$readwrite_unchecked");
  open_qual = find_qual("$open");
  read_qual = find_qual("$read");
  write_qual = find_qual("$write");
  readwrite_qual = find_qual("$readwrite");
  closed_qual = find_qual("$closed");
  have_stream_quals = (open_unchecked_qual &&
		       read_unchecked_qual &&
		       write_unchecked_qual &&
		       readwrite_unchecked_qual &&
		       open_qual &&
		       read_qual &&
		       write_qual &&
		       readwrite_qual &&
		       closed_qual);
  enabled_qual = find_qual("$enabled");
  disabled_qual = find_qual("$disabled");
  have_sleepy_quals = (enabled_qual && disabled_qual);
  readonly_qual = find_qual("$readonly");
}

static einfo analyze_magic_function_call(function_call fc, qtype fqt,
					 effect env, dd_list drinfolist)
{
  if (fc->arg1->kind == kind_identifier)
    {
      identifier id = CAST(identifier, fc->arg1);
      if ((!strcmp(id->cstring.data, "fopen") ||
	   !strcmp(id->cstring.data, "fdopen") ||
	   !strcmp(id->cstring.data, "tmpfile") ||
	   !strcmp(id->cstring.data, "safefopen") ||
	   !strcmp(id->cstring.data, "popen") ||
	   !strcmp(id->cstring.data, "my_popen")) &&
	  have_stream_quals)
      {
	expression arg;
	effect eff;
	qtype ret_qt, file_qt, result_qt;
	const char *count, *new_al_name, *new_qual_name;
	qual new_qual;
	aloc new_al;

	eff = effect_empty;
	scan_expression(arg, fc->args)
	  {
	    einfo arg_info = analyze_expression(arg, rpos, env, drinfolist);
	    eff = effect_union(eff, arg_info.eff);
	  }

	ret_qt = return_qtype(fqt);
	file_qt = points_to_qtype(ret_qt);
	count = inttostr(parse_region, fopen_count++);
	new_al_name = rstrscat(parse_region, name_aloc(aloc_qtype(ret_qt)),
			       "_inst", count, NULL);
	new_qual_name = rstrscat(parse_region, name_qual(qual_qtype(ret_qt)),
				 "_inst", count, NULL);
	new_al = make_aloc(new_al_name, file_qt);
	mark_aloc_interesting(new_al);
	eff = effect_union(eff, effect_alloc(new_al));
	new_qual = make_qvar(new_qual_name, id->loc, TRUE);
	result_qt = mkqtype_pointer(new_qual, file_qt, new_al);
	return mkeinfo(result_qt, eff, FALSE);
      }

      if (!strcmp(id->cstring.data, "malloc") ||
	  !strcmp(id->cstring.data, "xmalloc") ||
	  !strcmp(id->cstring.data, "vmalloc") ||
	  !strcmp(id->cstring.data, "kmalloc") ||
	  !strcmp(id->cstring.data, "ioremap") ||
	  !strcmp(id->cstring.data, "kmem_cache_alloc"))
	{
	  qtype ret_qt, pointsto;
	  aloc loc;
	  expression arg;
	  effect eff;
	  const char *count;
	  const char *new_void_name, *new_aloc_name, *new_void_pointer_name;

	  eff = effect_empty;
	  scan_expression(arg, fc->args)
	    {
	      einfo arg_info = analyze_expression(arg, rpos, env, drinfolist);
	      eff = effect_union(eff, arg_info.eff);
	    }

	  count = inttostr(parse_region, alloc_count++);
	  new_void_name = rstrcat(parse_region, "alloc_p_inst", count);
	  new_aloc_name = rstrcat(parse_region, "alloc_inst", count);
	  new_void_pointer_name = rstrcat(parse_region, "alloc_inst", count);
	  pointsto = mkqtype_void(make_qvar(new_void_name, fc->loc, FALSE));
	  loc = make_aloc(new_aloc_name, pointsto);
	  ret_qt = mkqtype_pointer(make_qvar(new_void_pointer_name, fc->loc,
					     FALSE),
				   pointsto,
				   loc);
				      
	  eff = effect_union(effect_alloc(loc), eff);

	  if (!strcmp(id->cstring.data, "kmalloc") && have_sleepy_quals)
	    {
	      assert(interrupt_status_qtype != NULL);
	      eff = effect_union(effect_wr(aloc_qtype(interrupt_status_qtype)),
				 eff);
	    }

	  return mkeinfo(ret_qt, eff, TRUE);
	}
      else
	return mkeinfo(NULL, NULL, FALSE);
    }
  else
    return mkeinfo(NULL, NULL, FALSE);
}

/**************************************************************************
 *                                                                        *
 * Functions for readonly                                                 *
 *                                                                        *
 **************************************************************************/

/* Given the effect eff in the scope of vd (a list of variable
   declarations), verify that none of the readonly locations are
   written */
void enforce_var_decl_readonly(variable_decl vd, effect eff)
{
  data_declaration root;
  qtype pqtype;

  if (!vd || !vd->ddecl) 
    return;

  root = root_ddecl(vd->ddecl);
  if (!root->qtype || !qtype_pointer(root->qtype) || 
      !qtype_pointer((pqtype = points_to_qtype(root->qtype))))
    return;

  if (readonly_qual && has_qual_qtype(pqtype, readonly_qual))
    mknin_effect(vd->loc, effect_wr(aloc_qtype(pqtype)), eff);
}

/* Given the effect eff in the scope of d (a list of declarations),
   verify that none of the readonly locations are written */
void enforce_readonly(declaration d, effect eff)
{
  if (d == NULL)
    return;

  enforce_readonly(CAST(declaration, d->next), eff);
  switch (d->kind)
    {
    case kind_asm_decl:
    case kind_function_decl:
    case kind_ellipsis_decl:
    case kind_oldidentifier_decl:
      return;
    case kind_extension_decl:
      {
	extension_decl ed = CAST(extension_decl, d);

	assert(ed->decl->next == NULL);
	enforce_readonly(ed->decl, eff);
	return;
      }
    case kind_data_decl:
      {
	declaration vd;

	scan_declaration(vd, CAST(data_decl, d)->decls)
	  enforce_var_decl_readonly(CAST(variable_decl, vd), eff);
	return;
      }
    default:
      fail_loc(d->loc, "Unexpected decl kind %x\n", d->kind);
    }
}
