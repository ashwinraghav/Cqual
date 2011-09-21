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

#include <fcntl.h>
#include "parser.h"
#include "AST_utils.h"
#include "expr.h"
#include "c-parse.h"
#include "constants.h"
#include "analyze.h"
#include "cqual.h"
#include "flow.h"
#include "hash.h"
#include "qerror.h"
#include "qtype.h"
#include "utils.h"
#include "pam.h"
#include "effect.h"

/*#define DEBUG*/

/**************************************************************************
 *                                                                        *
 * Second analysis pass for flow-sensitive qualifiers                     *
 *                                                                        *
 **************************************************************************/

typedef enum { lpos, /* lvalue */
	       rpos, /* rvalue */
	       apos, /* operand of & */
	       cpos, /* tested condition */} context;

typedef struct einfo {
  qtype qt;
  store out;            /* store when not on branch */
  store true_out;       /* store on branch where expr is TRUE */
  store false_out;      /* store on branch where expr is FALSE */
  /* true_out and false_out are only ever set in cpos.  If they're
     not set in cpos, use out */
  bool ismalloc;
} einfo;

typedef struct sinfo {
  store out;
} sinfo;

typedef struct dinfo {
  store out;
} dinfo;

void analyze_flow_sensitive(declaration);
static dinfo analyze_declaration(declaration, store);
static sinfo analyze_init(location, qtype, expression, store);
static sinfo analyze_statement(statement, store);
static einfo analyze_expression(expression, context, store);
static einfo analyze_unary_expression(unary, context, store);
static einfo analyze_binary_expression(binary, context, store);
static einfo analyze_magic_function_call(function_call, qtype, store);
static einfo split_file_pointer_context(location loc, qtype qt, store s);
bool file_pointer_fs_qtype(qtype qt, store s);
static store allocate_qtype(location loc, qtype qt, store in);
static einfo put_id_in_fs_context(location, qtype, context, store);

static void print_queue(void);
static void queue_print_exp(expression expr, store s, aloc al);
static void queue_print_stmt(statement stmt, store s, aloc al);
void prt_expression(expression e, int context_priority);
void prt_statement(statement s);
static store tfr_jumped_blocks(store s, compound_stmt from, compound_stmt to);
static store allocate_ddecl(data_declaration ddecl, store s);
static store merge_restricts(declaration, store);

bool is_void_parms(declaration);             /* semantics.c */
bool isassignment(binary e);                 /* analyze.c */
const char *binary_op_name(ast_kind);        /* unparse.c */

/**************************************************************************
 *                                                                        *
 * Globals and initialization                                             *
 *                                                                        *
 **************************************************************************/

store global_store = NULL;
static store toplvl_store = NULL;
static store global_store_ext = NULL;
static dd_list function_defns = NULL;
static dd_list printqueue = NULL;

void init_flow_sensitive(void)
{
  current_function_decl = NULL;
  global_store = make_store_var(NULL, "global");
  global_store_ext = make_store_var(NULL, "global");
  function_defns = dd_new_list(parse_region);
  printqueue = dd_new_list(parse_region);
  reset_flow_sensitive_quals();
}

void finish_flow_sensitive(void)
{
  data_declaration main_ddecl;

  mkleq_store(NULL, "inconsistent flow-sensitive qualifiers",
	      global_store_ext, global_store);
  main_ddecl = get_main_ddecl(); /* does root_ddecl */
  if (main_ddecl && main_ddecl->definition)
    {
      qtype fqt;
      store main_store_in;
      effect main_effect;

      fqt = main_ddecl->qtype;
      main_store_in = store_in_qtype(fqt);
      main_effect = effect_qtype(fqt);
      mkleq_store(location_declaration(main_ddecl->ast),
		  "inconsistent stores at main entry",
		  make_store_filter(NULL, toplvl_store, main_effect),
		  main_store_in);
    }
  else if (flag_driver)
    {
      bool found;
      dd_list_pos cur;

      found = FALSE;
      dd_scan(cur, function_defns)
	{
	  function_decl fd;
	  data_declaration ddecl;

	  fd = DD_GET(function_decl, cur);
	  ddecl = root_ddecl(fd->ddecl);
	  if (is_externally_visible_fn(ddecl))
	    {
	      qtype fqt;
	      store store_in, store_out;
	      effect eff;
	      
	      found = TRUE;
	      fprintf(stderr, "Assuming %s is an interface function\n",
		      ddecl->name);
	      fqt = ddecl->qtype;
	      store_in = store_in_qtype(fqt);
	      store_out = store_out_qtype(fqt);
	      eff = effect_qtype(fqt);

	      /* Add constraints global store <= f's store <= global store */
	      mkleq_store(location_declaration(ddecl->ast),
			  "inconsistent stores at interface function entry",
			  make_store_filter(NULL, toplvl_store, eff),
			  store_in);
	      mkleq_store(location_declaration(ddecl->ast),
			  "inconsistent stores at interface function exit",
			  make_store_ow(NULL, store_out, toplvl_store, eff),
			  toplvl_store);
	    }
	}
      if (!found)
	report_qerror(NULL, sev_warn,
		      "No interface functions found\n");
    }
  else
    /* Otherwise we'll report after third pass */
    report_qerror(NULL, sev_warn,
		  "No main function -- initial environment discarded");

  compute_lins();

  if (!flag_pam_mode)
    traverse_globals((traverse_global_fn) warn_if_dangerous_global,
		     global_store);

  if (flag_print_lin)
    print_queue();
}

/**************************************************************************
 *                                                                        *
 * Utilities                                                              *
 *                                                                        *
 **************************************************************************/

static inline einfo mkeinfo(qtype qt, store out, store true_out,
			    store false_out, bool ismalloc)
{
  struct einfo result = {qt: qt,
			 out: out,
			 true_out: true_out,
			 false_out: false_out,
                         ismalloc: ismalloc};
  return result;
}

static inline store true_out_einfo(einfo ei)
{
  if (ei.true_out)
    return ei.true_out;
  else
    {
      assert(ei.out);
      return ei.out;
    }
}

static inline store false_out_einfo(einfo ei)
{
  if (ei.false_out)
    return ei.false_out;
  else
    {
      assert(ei.out);
      return ei.out;
    }
}

static inline sinfo mksinfo(store out)
{
  struct sinfo result = {out: out};
  return result;
}

static inline dinfo mkdinfo(store out)
{
  struct dinfo result = {out: out};
  return result;
}

/* Return TRUE iff ddecl may be visible to the outside world */
bool is_externally_visible_fn(data_declaration ddecl)
{
  ddecl = root_ddecl(ddecl);
  return (ddecl->kind == decl_function &&
	  !ddecl->isexterninline &&
	  (ddecl->ftype != function_static || ddecl->addresstaken));
}

/* Given a declarator d, find the identifier its defining and add a
   flow-sensitive overlay with fs_qtype in store s.  Assumes d has a
   name. */
static void add_flow_sensitive_declarator_overlay(declarator d, qtype fs_qtype,
						  store s)
{
  identifier_declarator id = get_id_declarator(d);
  pam_add_overlay_flow_sensitive(id->loc, fs_qtype, s);
}

/* Initialize restricted location from non-restricted location.  This
   is slightly messy because their alocs don't match, so we have to do
   this manually. */
static bool init_restrict(variable_decl vd, store s)
{
  bool result;
  qtype fs_fqtype, fs_rqtype;

  assert(vd->rinfo);
  result = FALSE;
  fs_fqtype = vd->rinfo->fs_fqtype;
  fs_rqtype = vd->rinfo->fs_rqtype;

  /* Top-level qualifier */
  result = mkleq_qual(vd->loc, qual_qtype(fs_fqtype), qual_qtype(fs_rqtype))
    || result;

  /* One level down */
  fs_fqtype = points_to_fs_qtype(fs_fqtype, s);
  fs_rqtype = points_to_fs_qtype(fs_rqtype, s);
  result = mkleq_qual(vd->loc, qual_qtype(fs_fqtype), qual_qtype(fs_rqtype))
    || result;

  /* Below this they must in fact match */
  fs_fqtype = points_to_fs_qtype(fs_fqtype, s);
  fs_rqtype = points_to_fs_qtype(fs_rqtype, s);
  result = mkleq_qtype(vd->loc, fs_fqtype, fs_rqtype) || result;

  return result;
}

/* Initialize the fs_qtype field of ddecl */
static void add_ddecl_fs_qtype(data_declaration ddecl, store s)
{
  location loc;

  if (!ddecl->qtype || ddecl->fs_qtype)
    /* Do nothing if we're already assigned ddecl an fs_qtype */
    return;

  ddecl = root_ddecl(ddecl);
  loc = location_declaration(ddecl->ast);

  if (ddecl->global)
    {
      data_declaration global;

      global = ddecl->global;

      /* Doesn't shadow anything */
      if (!global)
	{
	  ddecl->fs_qtype = qtype_to_fs_qtype_with_quals(loc,
							 ddecl->qtype,
							 global_store);
	  return;
	}

      /* Add fs_qtype to global if necessary */
      if (global && !global->fs_qtype)
	  global->fs_qtype =
	    qtype_to_fs_qtype_with_quals(location_declaration(global->ast),
					 global->qtype,
					 global_store);

      /* Shadows a global from a prelude file */
      if (global->in_prelude && !ddecl->in_prelude)
	{
	  ddecl->fs_qtype = global->fs_qtype;
	  return;
	}

      /* Shadows some other global */
      if (match_type(ddecl->type, global->type))
	ddecl->fs_qtype = global->fs_qtype;
      else
	{
	  ddecl->fs_qtype = qtype_to_fs_qtype_with_quals(loc,
							 ddecl->qtype,
							 global_store);
	  if (unify_qtype(loc, global->fs_qtype, ddecl->fs_qtype))
	    {
	      report_qerror(loc, sev_err, "conflicting types for ``%s''",
			    ddecl->name);
	      report_qerror(global->ast->loc, sev_info,
			    "-- previous declaration of ``%s''", ddecl->name);
	    }
	}
    }
  else
    /* Local */
    ddecl->fs_qtype = qtype_to_fs_qtype_with_quals(loc,
						   ddecl->qtype,
						   s);
}

/**************************************************************************
 *                                                                        *
 * Tree Traversal                                                         *
 *                                                                        *
 **************************************************************************/

void analyze_flow_sensitive(declaration program)
{
  declaration d;
  dinfo next_info;

  toplvl_store = global_store;
  scan_declaration(d, program)
    {
      /* Assume left-to-right order of evaluation */
      next_info = analyze_declaration(d, toplvl_store);
      toplvl_store = next_info.out;
    }
}

/* Declaration */
static dinfo analyze_declaration(declaration d, store in)
{
  switch (d->kind)
    {
    case kind_asm_decl:
      return mkdinfo(in);
    case kind_data_decl:
      {
	data_decl dd = CAST(data_decl, d);
	declaration decl;
	store cur_store;

	cur_store = in;

	scan_declaration(decl, dd->decls) {
	  dinfo next;

	  next = analyze_declaration(decl, cur_store);
	  cur_store = next.out;
	}
	return mkdinfo(cur_store);
      }
    case kind_variable_decl:
      {
	variable_decl vd = CAST(variable_decl, d);
	store cur_store;
	data_declaration root;

	/* Ignore asm_stmt
	   assert(!vd->asm_stmt); */

	cur_store = in;
	root = root_ddecl(vd->ddecl);

	if (vd->rinfo)
	  /* This var was restricted.  Restore its original qtype */
	  root->qtype = vd->rinfo->fqtype;

	/* Allocate memory for any definitions */
	cur_store = allocate_ddecl(root, cur_store);

	add_ddecl_fs_qtype(root, cur_store);
	
	if (vd->arg1)
	  {
	    qtype init_qt;
	    sinfo arg1;

	    init_qt = root->fs_qtype;
	    if (!qtype_error(init_qt) && !qtype_array(init_qt))
	      init_qt = points_to_fs_qtype(init_qt, cur_store);

	    /* XXX: enter blocks without going through init ?! */
	    arg1 = analyze_init(vd->loc, init_qt, vd->arg1, cur_store);
	    cur_store = arg1.out;
	  }

	/* Handle restrict */
	if (vd->rinfo)
	  {
	    assert(!vd->rinfo->fs_rqtype && !vd->rinfo->fs_fqtype);
	    assert(qtype_pointer(vd->rinfo->rqtype));

	    /* Add rtl location (hack to make consistent stores) and
	       restricted location to store */
	    cur_store = ref_qtype_store(vd->loc, vd->rinfo->rqtype, cur_store);
	    cur_store = make_store_ref_effect(vd->loc, cur_store,
					      vd->rinfo->rlocs);

	    vd->rinfo->fs_fqtype = root->fs_qtype;
	    vd->rinfo->fs_rqtype =
	      qtype_to_fs_qtype_with_quals(vd->loc, vd->rinfo->rqtype,
					   cur_store);

	    /* Initialize restricted location from non-restricted
	       location */
	    if (init_restrict(vd, cur_store))
	      report_qerror(vd->loc, sev_err,
			    "incompatible right-hand side in restricted "
			    "location initialization");

	    /* Switch to restricted qtype, which we'll see from now
	       on */
	    root->fs_qtype = vd->rinfo->fs_rqtype;
	  }

	if ((flag_pam_mode || flag_print_results) &&
	    root->kind != decl_typedef)
	  add_flow_sensitive_declarator_overlay(vd->declarator,
						root->fs_qtype,
						cur_store);

	/* XXX: Operators */

	return mkdinfo(cur_store);
      }
    case kind_function_decl:
      {
	function_decl fd = CAST(function_decl, d), old_function_decl;
	data_declaration root;
	store cur_store;

	dd_add_last(parse_region, function_defns, fd);

	root = root_ddecl(fd->ddecl);

	/* Ignore definition of polymorphic function, just like in
	   analyze.c. */
	if (qtype_scheme(root->qtype))
	  return mkdinfo(in);

	add_ddecl_fs_qtype(root, in);
	if (root->global && root->global->in_prelude)
	  /* Ignore definition of something from a prelude file */
	  return mkdinfo(in);

	/* Add location to global store */
	global_store_ext = allocate_qtype(location_declaration(root->ast),
					  root->qtype,
					  global_store_ext);

	/* Add PAM overlay */
	if (flag_pam_mode || flag_print_results)
	  add_flow_sensitive_declarator_overlay(fd->declarator,
						root->fs_qtype,
						in);

	old_function_decl = current_function_decl;
	current_function_decl = fd;
	if (qtype_error(root->fs_qtype))
	  cur_store = make_store_var(fd->loc, "error_store");
	else
	  cur_store = store_in_qtype(root->fs_qtype);

	/*** Scan argument list ***/

	{
	  declaration arg;
	  qtypelist_scanner old_qtypes;
	  int i;

	  i = 1;
	  if (qtype_error(root->fs_qtype))
	    old_qtypes = NULL;
	  else
	    qtypelist_scan(arg_qtypes(root->fs_qtype), &old_qtypes);

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
		      root_vd = root_ddecl(argvd->ddecl);
		      oid = NULL;
		    }
		  else
		    {
		      oid = CAST(oldidentifier_decl, arg);
		      argvd = NULL;
		      root_vd = root_ddecl(oid->ddecl);
		    }

		  /* Build flow sensitive qtype.  root_vd->qtype is
		     always a pointer qtype. */
		  assert(!root_vd->fs_qtype);
		  assert(qtype_pointer(root_vd->qtype));
		  if (argvd && argvd->rinfo)
		    /* This parameter was restricted.  Restore its
                       original qtype */
		    root_vd->qtype = argvd->rinfo->fqtype;

		  cur_store =
		    allocate_qtype(location_declaration(root_vd->ast),
				   root_vd->qtype,
				   cur_store);

		  root_vd->fs_qtype =
		    qtype_to_fs_qtype_with_quals(arg->loc,
						 root_vd->qtype,
						 cur_store);

		  /* Unify with qtype for parameter.  Unfortunately
		     we need to do this, because the previous unification
		     in analyze.c unified the qual and the ty, but *not*
		     the qtype itself */
		  arg_qt = points_to_fs_qtype(root_vd->fs_qtype, cur_store);
		  if (!qtype_error(root->fs_qtype))
		    {
		      old_qtype = qtypelist_next(&old_qtypes);
		      if (old_qtype &&
			  unify_qtype(arg->loc, old_qtype, arg_qt))
			report_qerror(arg->loc, sev_err,
				      "type of parameter %d doesn't match "
				      "previous uses",
				      i);
		      else if (!old_qtype && arg->kind == kind_data_decl)
			report_qerror(arg->loc, sev_err,
				      "parameter %d doesn't appear in "
				      "previous uses", i);
		    }

		  /* Handle restrict */
		  if (argvd && argvd->rinfo)
		    {
		      assert(!argvd->rinfo->fs_rqtype &&
			     !argvd->rinfo->fs_fqtype);
		      assert(qtype_pointer(argvd->rinfo->rqtype));

		      /* Add rtl location (hack to make consistent stores) and
			 restricted location to store */
		      cur_store = ref_qtype_store(arg->loc,
						  argvd->rinfo->rqtype,
						  cur_store);
		      cur_store = make_store_ref_effect(arg->loc,
							cur_store,
							argvd->rinfo->rlocs);

		      argvd->rinfo->fs_fqtype = root_vd->fs_qtype;
		      argvd->rinfo->fs_rqtype =
			qtype_to_fs_qtype_with_quals(arg->loc,
						     argvd->rinfo->rqtype,
						     cur_store);

		      /* Initialize restricted location from non-restricted
			 location */
		      if (init_restrict(argvd, cur_store))
			report_qerror(argvd->loc, sev_err,
				      "type of restricted parameter %d "
				      "doesn't match previous uses",
				      i);

		      /* Switch to restricted qtype, which we'll see
			 from now on */
		      root_vd->fs_qtype = argvd->rinfo->fs_rqtype;
		    }

		  i++;

		  if (flag_pam_mode || flag_print_results)
		    {
		      if (arg->kind == kind_data_decl)
		       add_flow_sensitive_declarator_overlay(argvd->declarator,
							     root_vd->fs_qtype,
							     cur_store);
		      else
			pam_add_overlay_flow_sensitive(oid->loc,
						       root_vd->fs_qtype,
						       cur_store);
		    }
		}
	}

	/*** Evaluate body ***/

	{
	  sinfo body;

	  assert(is_compound_stmt(fd->stmt));
	  body = analyze_statement(fd->stmt, cur_store);
	  cur_store = body.out;

	  cur_store = merge_restricts(fd->fdeclarator->parms, body.out);

	  cur_store = make_store_filter(fd->loc, cur_store, fd->scope_env);
	  if (!qtype_error(root->fs_qtype))
	    mkleq_store(fd->loc,
			"inconsitent stores at function exit",
			cur_store, store_out_qtype(root->fs_qtype));
	}

	current_function_decl = old_function_decl;
	return mkdinfo(in);
      }
    case kind_extension_decl:
      {
	dinfo result;
	extension_decl ed = CAST(extension_decl, d);
        result =  analyze_declaration(ed->decl, in); /* Ignore __extnesion__ */
	return result;
      }
    default:
      fail("Unexpected decl kind %x\n", d->kind);
    }
}

static bool init_string(location loc, qtype lhs_qtype, qtype rhs_qtype,
			store in)
{
  bool result = FALSE;
  qtype lhs_contents_qtype, rhs_contents_qtype;

  lhs_contents_qtype = array_of_qtype(lhs_qtype);
  rhs_contents_qtype = points_to_fs_qtype(rhs_qtype, in);
  result = mkleq_qual(loc,
		      qual_qtype(rhs_contents_qtype),
		      qual_qtype(lhs_contents_qtype)) || result;
  result = mkleq_qual(loc,
		      qual_qtype(rhs_qtype),
		      qual_qtype(lhs_qtype)) || result;
  return result;
}

static sinfo analyze_init(location loc, qtype lhs_qtype, expression rhs,
			  store in)
{
  if (rhs->kind == kind_string && qtype_string(lhs_qtype))
    {
      einfo arg1;
      
      arg1 = analyze_expression(rhs, rpos, in);
      if (init_string(rhs->loc, lhs_qtype, arg1.qt, in))
	report_qerror(rhs->loc, sev_err, "incompatibile initializer");

      return mksinfo(arg1.out);
    }

  if (rhs->kind == kind_init_list && (qtype_array(lhs_qtype)))
    {
      /* Initialize the array contents qtype with each rhs */
      init_list il = CAST(init_list, rhs);
      store cur_store;
      expression e;
      sinfo init;

      cur_store = in;

      scan_expression(e, il->args)
	if (e->kind == kind_init_index)
	  {
	    init_index ii = CAST(init_index, e);
	    einfo arg1, arg2;

	    arg1 = analyze_expression(ii->arg1, rpos, cur_store);
	    cur_store = arg1.out;
	    if (ii->arg2)
	      {
		arg2 = analyze_expression(ii->arg2, rpos, cur_store);
		cur_store = arg2.out;
	      }
	    init = analyze_init(loc, array_of_qtype(lhs_qtype),
				ii->init_expr, cur_store);
	    cur_store = init.out;
	  }
	else
	  {
	    init = analyze_init(loc, array_of_qtype(lhs_qtype),
				e, cur_store);
	    cur_store = init.out;
	  }
      return mksinfo(cur_store);
     }

  if (rhs->kind == kind_init_list && (qtype_struct(lhs_qtype) ||
				      qtype_union(lhs_qtype)))
     {
       /* Initialize each of the field qtypes with each rhs */
       init_list il = CAST(init_list, rhs);
       field_scanner fs;
       qtype fqt;
       expression e;
       store cur_store;

       fqt = NULL;
       field_scan(lhs_qtype, &fs);
       cur_store = in;

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
		 /* Error -- but reported in first pass */
		 break;
	       init_expr = ifd->init_expr;
	     }
	   else
	     init_expr = e;

	   fqt = field_next(&fs);

	   if (!fqt)
	     /* Error -- but reported in first pass */
	     break;

	   /* Unless we're initializing an array, get the contents of the
	      ref.  Note we can't be initializing a function -- not C++. */
	   if (!qtype_array(fqt))
	     fqt = points_to_fs_qtype(fqt, cur_store);
	   
	   init = analyze_init(loc, fqt, init_expr, cur_store);
	   cur_store = init.out;
	 }
       return mksinfo(cur_store);
     }

  /* Default case. */
  {
    einfo arg1;

    arg1 = analyze_expression(rhs, rpos, in);
    if (mkleq_fs_assign_convert(loc, arg1.qt, lhs_qtype, arg1.out))
      report_qerror(loc, sev_err,
		    "incompatible right-hand side in initialization");
    
    return mksinfo(arg1.out);
  }
}

static sinfo analyze_statement(statement s, store in)
{
#ifdef DEBUG
  printf("Store before line %ld: ", s->loc->lineno);
  print_store(printf, in);
  printf("\n");
#endif
  switch (s->kind)
    {
    case kind_asm_stmt:
      /*      report_qerror(s->loc, sev_warn,
	      "warning: ignoring in-line assembly code");*/
      return mksinfo(in);
      break;
    case kind_compound_stmt:
      {
	compound_stmt cs = CAST(compound_stmt, s);	
	declaration decl;
	statement stmt;
	store cur_store, out;
	
	cur_store = in;
	/* cur_store = make_store_filter(in, cs->filter_env); */

	/* Analyze the declarations. */
	scan_declaration(decl, cs->decls)
	  {
	    dinfo next;

	    next = analyze_declaration(decl, cur_store);
	    cur_store = next.out;
	  }

	/* Analyze the body, gathering up the effects. */
	scan_statement(stmt, cs->stmts)
	  {
	    sinfo next = analyze_statement(stmt, cur_store);
	    cur_store = next.out;
	  }

	cur_store = merge_restricts(cs->decls, cur_store);
	out = make_store_filter(cs->loc, cur_store, cs->filter_env);
	/*
	cur_store = make_store_filter(cs->loc, cur_store, cs->filter_env);
	out = make_store_var(cs->loc, "compound_out");
	mkleq_store(cs->loc,
		    "inconsistent stores at compound statement exit",
		    cur_store, out);
	*/

	return mksinfo(out);
      };
      break;
    case kind_if_stmt:
      {
	if_stmt is = CAST(if_stmt, s);
	einfo guard;
	sinfo then, els;
	store result;

	guard = analyze_expression(is->condition, cpos, in);
	then = analyze_statement(is->stmt1, true_out_einfo(guard));

	if (is->stmt2)
	  {
	    els = analyze_statement(is->stmt2, false_out_einfo(guard));
	    result = lub_store(is->loc,
			       "inconsistent then and else branch stores",
			       els.out, then.out);
	  }
	else
	  {
	    /* if (false) w/no else */
	    result = lub_store(is->loc,
			       "inconsistent then and fall-through stores",
			       then.out, false_out_einfo(guard));
	  }

	return mksinfo(result);
      };
      break;
    case kind_labeled_stmt:
      {
	labeled_stmt ls = CAST(labeled_stmt, s);
	sinfo body;

	if (ls->label->store_in)
	  mkleq_store(ls->loc,
		      "inconsistent stores at label entry",
		      in, ls->label->store_in);
	else
	  {
	    ls->label->store_in = make_store_var(ls->loc, "label");
	    mkleq_store(ls->loc,
			"inconsitent stores at label entry",
			in, ls->label->store_in);
	  }
	body = analyze_statement(ls->stmt, ls->label->store_in);
	return body;
      };
      break;
    case kind_expression_stmt:
      {
	expression_stmt es = CAST(expression_stmt, s);
	einfo ei;

	ei = analyze_expression(es->arg1, rpos, in);
	return mksinfo(ei.out);
      };
      break;
    case kind_while_stmt:
      {
	while_stmt ws = CAST(while_stmt, s);
	einfo guard;
	sinfo body;

	ws->break_dest = make_store_var(ws->loc, "ws_break");
	ws->continue_dest = make_store_var(ws->loc, "ws_continue");
	mkleq_store(ws->loc, "inconsitent stores at loop entry", in,
		    ws->continue_dest);
	guard = analyze_expression(ws->condition, rpos, ws->continue_dest);

	body = analyze_statement(ws->stmt, guard.out);

	/* 1+ times through loop */
	mkleq_store(ws->loc, "inconsistent stores at loop body exit",
		    body.out, ws->continue_dest);

	if (!definite_one(ws->condition))
	  /* Non-infinite loop, so may exit from guard */
	  mkleq_store(ws->loc, "inconsistent stores at loop exit",
		      guard.out, ws->break_dest);

	return mksinfo(ws->break_dest);
      };
      break;
    case kind_dowhile_stmt:
      {
	dowhile_stmt dws = CAST(dowhile_stmt, s);
	einfo guard;
	sinfo body;
	store before_body_store;

	dws->break_dest = make_store_var(dws->loc, "dws_break");
	dws->continue_dest = make_store_var(dws->loc, "dws_continue");
	before_body_store = make_store_var(dws->loc, "dws_body");
	mkleq_store(dws->loc, "inconsistent stores at loop entry",
		    in, before_body_store);
	body = analyze_statement(dws->stmt, before_body_store);
	mkleq_store(dws->loc, "inconsistent stores at loop body entry",
		    body.out, dws->continue_dest);

	if (definite_zero(dws->condition))
	  /* Catch do { x } while (0); case -- used in macro expansions */
	  mkleq_store(dws->loc, "inconsistent stores at loop exit",
		      dws->continue_dest, dws->break_dest);
	else if (definite_one(dws->condition))
	  {
	    /* 1 time through loop */
	    guard = analyze_expression(dws->condition, rpos,
				       dws->continue_dest);
	  
	    /* 2+ times through loop */
	    mkleq_store(dws->loc, "inconsistent stores at loop body entry",
			guard.out, before_body_store); 
	  }
	else
	  {
	    /* 1 time through loop */
	    guard = analyze_expression(dws->condition, rpos,
				       dws->continue_dest);
	  
	    /* 2+ times through loop */
	    mkleq_store(dws->loc, "inconsistent stores at loop body entry",
			guard.out, before_body_store);

	    /* May exit after guard */
	    mkleq_store(dws->loc, "inconsistent stores at loop exit",
			guard.out, dws->break_dest);
	  }

	return mksinfo(dws->break_dest);
      };
      break;
    case kind_switch_stmt:
      {
	switch_stmt ss = CAST(switch_stmt, s);
	einfo guard;
	sinfo body;
	label cases;
	bool has_default;

	ss->break_dest = make_store_var(ss->loc, "break_dest");

	guard = analyze_expression(ss->condition, rpos, in);
	has_default = FALSE;
	for (cases = ss->next_label; cases; cases = cases->next_label)
	  {
	    switch (cases->kind)
	      {
	      case kind_default_label:
		has_default = TRUE;
	      case kind_case_label:
		/* this is a jump, but target (case stmts) are
		   guaranteed to be included in this scope.  So we
		   don't need to apply Down */
 		cases->store_in = make_store_var(cases->loc, "ss_case");
	 	mkleq_store(ss->loc, "inconsistent stores at case entry",
			    guard.out, cases->store_in);
		break;
 	      default:
	 	fail("Unexpected label kind %x in switch\n", cases->kind);
	      }
 	  }

	/* Note:  no connection from guard to body.  Only can jump
	   to a case. */
	body = analyze_statement(ss->stmt,
				 make_store_var(ss->loc, "ss_start"));
	/* body may end without a break */
	mkleq_store(ss->loc, "inconsistent stores at body exit",
		    body.out, ss->break_dest); 

	if (!has_default)
	  /* There may be no matching case */
	  mkleq_store(ss->loc, "inconsistent stores at body exit",
		      guard.out, ss->break_dest);

	return mksinfo(ss->break_dest);
      };
      break;
    case kind_for_stmt:
      {
	for_stmt fs = CAST(for_stmt, s);
	sinfo body;
	store cur_store;

	fs->break_dest = make_store_var(fs->loc, "fs_break");
	fs->continue_dest = make_store_var(fs->loc, "fs_continue");

	cur_store = in;

	if (fs->arg1)
	  {
	    einfo arg1;
	    arg1 = analyze_expression(fs->arg1, rpos, cur_store);
	    cur_store = arg1.out;
	  }

	mkleq_store(fs->loc, "inconsistent stores at loop entry",
		    cur_store, fs->continue_dest);
	cur_store = fs->continue_dest;

	if (fs->arg2)
	  {
	    einfo arg2;
	    arg2 = analyze_expression(fs->arg2, rpos, cur_store);
	    cur_store = arg2.out;

	    /* Might exit loop */
	    mkleq_store(fs->loc, "inconsistent stores at loop exit",
			cur_store, fs->break_dest);
	  }

	body = analyze_statement(fs->stmt, cur_store);
	cur_store = body.out;

	if (fs->arg3)
	  {
	    einfo arg3;
	    arg3 = analyze_expression(fs->arg3, rpos, cur_store);
	    cur_store = arg3.out;
	  }

	mkleq_store(fs->loc, "inconsistent stores at loop entry",
		    cur_store, fs->continue_dest);

	return mksinfo(fs->break_dest);
      };
      break;
    case kind_return_stmt:
      {
	return_stmt rs = CAST(return_stmt, s);
	qtype fun_qtype;
	store cur_store;

	assert(current_function_decl);
	fun_qtype = root_ddecl(current_function_decl->ddecl)->fs_qtype;

	cur_store = in;
	if (rs->arg1)
	  {
	    einfo arg1;

	    arg1 = analyze_expression(rs->arg1, rpos, in);
	    cur_store = arg1.out;
	    if (!qtype_error(fun_qtype) &&
		mkleq_fs_assign_convert(rs->loc, arg1.qt,
					return_qtype(fun_qtype), arg1.out))
	      report_qerror(rs->loc, sev_err,
			    "return type incompatible with function type");
	  }

	cur_store = merge_restricts(current_function_decl->fdeclarator->parms,
				    cur_store); 

	/* XXX: Could get rid of this here and use a variable as the
	   join point for returns. */
	cur_store = make_store_filter(rs->loc,
				      cur_store,
				      current_function_decl->scope_env);
	if (!qtype_error(fun_qtype))
	  mkleq_store(rs->loc, "inconsistent stores at function exit",
		      cur_store, store_out_qtype(fun_qtype));

	return mksinfo(make_store_var(rs->loc, "return"));
      };
      break;
    case kind_computed_goto_stmt:
      {
	/* o|-< UNSOUND */
	computed_goto_stmt cgs = CAST(computed_goto_stmt, s);
	einfo arg1;

	arg1 = analyze_expression(cgs->arg1, rpos, in);/* stmt is goto *arg1 */
	return mksinfo(make_store_var(cgs->loc, "computed_goto"));
      };
      break;
    case kind_break_stmt:
      {
	break_stmt bs = CAST(break_stmt, s);
	store cur_store;

	cur_store = tfr_jumped_blocks(in, bs->enclosing_cs, 
				      bs->parent_loop->enclosing_cs);

	cur_store = make_store_filter(bs->loc, cur_store,
				   bs->parent_loop->enclosing_cs->scope_env);

	mkleq_store(bs->loc,
		    (bs->parent_loop->kind == kind_switch_stmt ?
		     "inconsistent stores at switch exit" :
		     "inconsistent stores at loop exit"),
		    cur_store, bs->parent_loop->break_dest);
	return mksinfo(make_store_var(bs->loc, "break_stmt"));
      }
      break;
    case kind_continue_stmt:
      {
	continue_stmt cs = CAST(continue_stmt, s);
	store cur_store;

	cur_store = tfr_jumped_blocks(in, cs->enclosing_cs, 
				      cs->parent_loop->enclosing_cs);

	cur_store = make_store_filter(cs->loc, in,
				   cs->parent_loop->enclosing_cs->scope_env);

	mkleq_store(cs->loc,
		    (cs->parent_loop->kind == kind_switch_stmt ?
		     "inconsistent stores at switch exit" :
		     "inconsistent stores at loop exit"),
		    cur_store, cs->parent_loop->continue_dest);
	return mksinfo(make_store_var(cs->loc, "continue_stmt"));
      }
      break;
    case kind_goto_stmt:
      {
	goto_stmt gs = CAST(goto_stmt, s);
	label_declaration ldecl;
	id_label target;
	store cur_store;

	ldecl = gs->id_label->ldecl;
	target = ldecl->definition;
	if (!target->store_in)
	  target->store_in = make_store_var(target->loc, "label");
	cur_store = tfr_jumped_blocks(in, gs->enclosing_cs,
				      target->enclosing_cs);
	cur_store = make_store_filter(gs->loc, cur_store,
				      target->enclosing_cs->scope_env);
	mkleq_store(gs->loc, "inconsistent stores at unconditional branch",
		    cur_store, target->store_in);

	return mksinfo(make_store_var(gs->loc, "goto_stmt"));
      }
      break;
    case kind_empty_stmt:
      return mksinfo(in);
    case kind_change_type_stmt:
      {
	change_type_stmt ct = CAST(change_type_stmt, s);
	einfo arg1;
	aloc al;
	store out;
	qtype new_qt;

	arg1 = analyze_expression(ct->arg1, lpos, in);
	out = arg1.out;
	al = aloc_qtype(arg1.qt);
	new_qt = qtype_to_fs_qtype_with_quals(ct->loc, ct->qtype, out);
	out = assign_flow_sensitive(ct->loc,
				    "incompatible types in change_type",
				    new_qt, arg1.qt, out, TRUE);

	if (flag_pam_mode || flag_print_results)
	  pam_add_overlay_lin(ct->loc, out, al);

	if (flag_print_lin)
	  queue_print_stmt((statement) s, out, al);

      return mksinfo(out);
      }
      break;
    case kind_assert_type_stmt:
      {
	assert_type_stmt at = CAST(assert_type_stmt, s);
	einfo arg1;
	qtype assert_qt;
	
	arg1 = analyze_expression(at->arg1, rpos, in);
	/* Get declared qtype; ignore any effect of arg1 */
	assert_qt = qtype_to_fs_qtype_with_quals(at->loc, at->qtype, in);
	if (mkleq_fs_qtype(at->loc, arg1.qt, assert_qt, in))
	  report_qerror(at->loc, sev_err,
			"unsatisfiable assert_type");
	return mksinfo(arg1.out);
      }
      break;
    case kind_deep_restrict_stmt:
      {
	deep_restrict_stmt dr = CAST(deep_restrict_stmt, s);
	einfo arg1;
	sinfo stmt;
	store cur_store;
	bool result;

	assert(dr->drinfo);

	arg1 = analyze_expression(dr->arg1, lpos, in);
	cur_store = arg1.out;
	dr->drinfo->fs_qtype = arg1.qt;

	/* Allocated the deep restricted location */
	cur_store = ref_qtype_store(dr->loc, dr->drinfo->rqtype, cur_store);

	/* initialize the deep restrict type */
	dr->drinfo->fs_rqtype = 
	  qtype_to_fs_qtype_with_quals(dr->loc, dr->drinfo->rqtype, cur_store);
	
	result = mkleq_qual(dr->loc, qual_qtype(dr->drinfo->fs_qtype),
			    qual_qtype(dr->drinfo->fs_rqtype));
	assert(qtype_pointer_loc(dr->drinfo->fs_rqtype));
	result = mkleq_qtype(dr->loc, 
			     points_to_fs_qtype(dr->drinfo->fs_qtype, 
						cur_store),
			     points_to_fs_qtype(dr->drinfo->fs_rqtype,
						cur_store));

	if (result)
	  report_qerror(dr->loc, sev_err,
			"incompatible right-hand side in deeprestricted "
			"location initialization");
	
	stmt = analyze_statement(dr->stmt, cur_store);
	cur_store = stmt.out;
	
	/* Merge at the exit */
	{
	  qtype rqtype, fqtype;

	  fqtype = dr->drinfo->fs_qtype;
	  rqtype = points_to_fs_qtype(dr->drinfo->fs_rqtype, in);
      
	  cur_store = assign_flow_sensitive(dr->loc,
			   "incompatible types at deep-restrict scope end",
				     rqtype, fqtype, cur_store, FALSE);
	}
	return mksinfo(cur_store);
      }
    break;
    default:
      fail("Unexpected statement kind %x\n", s->kind);
    }
}

static einfo analyze_expression(expression e, context context, store in)
{
#ifdef DEBUG
  printf("Store before ");
  prt_expression(e, 0);
  printf(": ");
  print_store(printf, in);
  printf("\n");
#endif
  
  /* deep restrict */
  if (e->drinfo)
    {
      qtype qt;

      qt = e->drinfo->fs_rqtype;
      assert(qt);
      assert(qt && qtype_pointer_loc(qt));

      if (context == rpos)
	return mkeinfo(points_to_fs_qtype(qt, in), in, NULL, NULL, FALSE);
      else 
	return mkeinfo(qt, in, NULL, NULL, FALSE);
    }

  /*  printf("Analyzing ");
      prt_expression(e, 0);
      printf(" in %s\n", context_tostring(context));*/
  switch(e->kind) {
  case kind_comma:
    {
      comma c = CAST(comma, e);
      einfo last;
      expression e2;

      last = mkeinfo(NULL, in, NULL, NULL, FALSE);
      scan_expression (e2, c->arg1)
	last = analyze_expression(e2, context, last.out);
      return last;
    };
    break;
  case kind_sizeof_type:
  case kind_alignof_type:
  case kind_known_cst:
  case kind_lexical_cst:
  case kind_sizeof_expr:
  case kind_alignof_expr:
    {
      qtype qt;

      qt = qtype_to_fs_qtype_with_quals(e->loc, e->qtype, in);
      return mkeinfo(qt, in, NULL, NULL, FALSE);
    }
    break;
  case kind_label_address:
  case kind_string:
    {
      qtype qt;

      qt = qtype_to_fs_qtype_with_quals(e->loc, e->qtype, in);

      global_store_ext = allocate_qtype(e->loc, e->qtype, global_store_ext);
      return mkeinfo(qt, in, NULL, NULL, FALSE);
    }
    break;
  case kind_cast:
    {
      cast c = CAST(cast, e);
      einfo arg1;
      type cast_t;
      qtype qt;
      store cur_store;

      arg1 = analyze_expression(c->arg1, context, in);
      cur_store = arg1.out;
      cast_t = c->asttype->type;

      qt = qtype_to_fs_qtype_with_quals(c->loc, e->qtype, in);

      /* ad-hoc malloc handling in action */
      if (arg1.ismalloc)
	cur_store = allocate_qtype(c->loc, e->qtype, cur_store);

      /*
      if (flag_casts_preserve && !type_contains_quals(cast_t))
	{
	  if (mkeq_qtype_cast(c->loc, qt, arg1.qt))
	    report_qerror(c->loc, sev_err,
			  "Incompatible qualifiers at cast");
	}
      */      

      if (flag_casts_warn && !arg1.ismalloc && !type_contains_quals(cast_t))
	{
	  mk_no_qual_qtype_fs(c->loc, arg1.qt, cur_store);
	  mk_no_qual_qtype_fs(c->loc, qt, cur_store);
	}

      return mkeinfo(qt, cur_store, NULL, NULL, FALSE);
    };
    break;
  case kind_cast_list:
    {
      cast_list cl = CAST(cast_list, e);
      qtype qt;
      sinfo init;

      qt = qtype_to_fs_qtype_with_quals(cl->loc, cl->astqtype, in);
      init = analyze_init(cl->loc, qt, cl->init_expr, in);

      qt = default_conversion_fs_qtype(qt);
      return mkeinfo(qt, init.out, NULL, NULL, FALSE);
    };
    break;
  case kind_conditional:
    {
      conditional c = CAST(conditional, e);
      einfo cond, arg1, arg2;
      store out;
      qtype fs_qt;

      /* XXX: cpos not handled yet, so changed to rpos */
      cond = analyze_expression(c->condition, rpos, in);
      if (c->arg1)
	arg1 = analyze_expression(c->arg1,
				  context == cpos ? rpos : context,
				  cond.out);
      else
	/* gcc extension:  if you omit the middle op, evaluates to
	   guard when guard is true, and guard is not reevaluated */
	arg1 = cond;
      arg2 = analyze_expression(c->arg2,
				context == cpos ? rpos : context,
				cond.out);
      out = lub_store(c->loc, "inconsistent ? and : stores",
		      arg1.out, arg2.out);

      fs_qt = qtype_to_fs_qtype(c->loc, c->qtype);
      if (qtype_void(arg1.qt))
	mkleq_fs_assign_convert(c->loc, arg1.qt, fs_qt, out);
      else if (qtype_void(arg2.qt))
	mkleq_fs_assign_convert(c->loc, arg2.qt, fs_qt, out);
      else if (mkleq_fs_assign_convert(c->loc, arg1.qt, fs_qt, out) ||
	       mkleq_fs_assign_convert(c->loc, arg2.qt, fs_qt, out))
	report_qerror(c->loc, sev_err,
		      "incompatible branches of conditional");

      return mkeinfo(fs_qt, out, NULL, NULL, FALSE);
    };
    break;
  case kind_identifier:
    {
      identifier id = CAST(identifier, e);
      data_declaration root;
      qtype qt;

      root = root_ddecl(id->ddecl);

      qt = root->fs_qtype;
      if (!qt)
	{
	  add_ddecl_fs_qtype(root, in);
	  qt = root->fs_qtype;
	}
      if ((type_function(id->ddecl->type) &&
          id->ddecl->ftype == function_implicit) ||
         !strcmp(id->ddecl->name, "__FUNCTION__") ||
         !strcmp(id->ddecl->name, "__PRETTY_FUNCTION__"))
        global_store_ext = allocate_qtype(id->loc, qt, global_store_ext);

      if (flag_pam_mode || flag_print_results)
	pam_add_overlay_flow_sensitive(id->loc, qt, in);

      if (root->kind != decl_constant)
	/* Put non-enums in context */
	return put_id_in_fs_context(id->loc, qt, context, in);
      else
	return mkeinfo(qt, in, NULL, NULL, FALSE);
    };
    break;
  case kind_compound_expr:
    {
      compound_expr ce = CAST(compound_expr, e);
      compound_stmt cs = CAST(compound_stmt, ce->stmt);
      statement cur_stmt;
      declaration d;
      qtype qt;
      store cur_store, out;

      assert(!cs->id_labels);
      
      cur_store = in;
      /* cur_store = make_store_filter(in, ce->filter_env); */

      /* Analyze the declarations in the block */
      scan_declaration(d, cs->decls)
	{
	  dinfo next;

	  assert(d->kind != kind_asm_decl); /*asm_decl only at toplevel */
	  next = analyze_declaration(d, cur_store);
	  cur_store = next.out;
	}
      /* Analyze the statements in the block.  Analyze all but the
         last one. */
      cur_stmt = cs->stmts;
      while (cur_stmt && cur_stmt->next)
	{
	  sinfo next = analyze_statement(cur_stmt, cur_store);
	  cur_store = next.out;
	  cur_stmt = CAST(statement, cur_stmt->next);
	}

      /* Now analyze the last statement (if there is one), and
         compute the type of the expression. */
      if (cur_stmt && is_expression_stmt(cur_stmt))
	{
	  einfo next;
	  next = analyze_expression(CAST(expression_stmt,cur_stmt)->arg1,
				    context,
				    cur_store);
	  qt = next.qt;
	  cur_store = next.out;
	}
      else
	{
	  /* Type is void */
	  qt = mkqtype_void(make_qvar("compound", ce->loc, FALSE));
	  if (cur_stmt)
	    {
	      sinfo next = analyze_statement(cur_stmt, cur_store);
	      cur_store = next.out;
	    }
	}

      cur_store = merge_restricts(cs->decls, cur_store);
      out = make_store_filter(ce->loc, cur_store, ce->filter_env);
      /*
      cur_store = make_store_filter(ce->loc, cur_store, ce->filter_env);
      out = make_store_var(ce->loc, "compound_out");
      mkleq_store(ce->loc,
		  "inconsistent stores at compount expression exit",
		  cur_store, out);
      */

      return mkeinfo(qt, out, NULL, NULL, FALSE);
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
      store cur_store, out;

      if (fc->va_arg_call)
	{
	  qtype va_arg_fs_qtype;

	  /* Magic call to __builtin_va_arg(args, fc->va_arg_call) */
	  va_arg_fs_qtype = qtype_to_fs_qtype(fc->loc, fc->qtype);
	  return mkeinfo(va_arg_fs_qtype, in, NULL, NULL, FALSE);
	}

      cur_store = in;
      f_info = analyze_expression(fc->arg1, rpos, cur_store);
      cur_store = f_info.out; /* Assume function evaluated first */

      fqt = NULL;
      if (!qtype_error(f_info.qt))
	fqt = points_to_fs_qtype(f_info.qt, cur_store);

      if (qtype_error(f_info.qt) || qtype_error(fqt))
	{
	  /* Evaluate arguments */
	  scan_expression(arg, fc->args)
	    {
	      einfo arg_info;

	      arg_info = analyze_expression(arg, rpos, cur_store);
	      cur_store = arg_info.out;
	    }

	  return mkeinfo(error_qtype, cur_store, NULL, NULL, FALSE);
	}


      magic_info = analyze_magic_function_call(fc, fqt, cur_store);
      if (magic_info.out)
	return magic_info;

      /* Analyze each argument, making a constraint with the
	 corresponding formal paramter */
      args = arg_qtypes(fqt);
      qtypelist_scan(args, &args_qs);
      i = 1;
      scan_expression(arg, fc->args)
	{
	  einfo arg_info;
	  qtype formal_qt;

	  arg_info = analyze_expression(arg, rpos, cur_store);
	  cur_store = arg_info.out; /* Assume left-to-right order
				      of evaluation of arguments */
	  formal_qt = qtypelist_next(&args_qs);

	  if (formal_qt)
	    {
	      /* Flow arg qtype into function.  No flow-sensitivity
		 here, because this is an initialization. */
	      if (mkleq_fs_assign_convert(arg->loc, arg_info.qt, formal_qt,
					  cur_store))
		report_qerror(arg->loc, sev_err,
			      "type of actual argument %d doesn't match "
			      "type of formal", i);
	    }
	  
	  i++;
	}

      mkleq_store(fc->loc,
		  "inconsistent stores at function entry",
		  make_store_filter(fc->loc, cur_store, effect_qtype(fqt)),
		  store_in_qtype(fqt));
      
      if (noreturn_qual &&
	  has_qual_qtype(points_to_qtype(fc->arg1->qtype), noreturn_qual))
	out = make_store_var(fc->loc, "noreturn");
      else
	{
	  /*
	  out = make_store_ow(fc->loc, store_out_qtype(fqt), cur_store,
			      effect_qtype(fqt));
	  */
	  cur_store = make_store_ow(fc->loc, store_out_qtype(fqt), cur_store,
				    effect_qtype(fqt));
	  out = make_store_var(fc->loc, "func_out");
	  mkleq_store(fc->loc, "inconsistent stores at function return",
		      cur_store, out);
	}

      return mkeinfo(return_qtype(fqt), out, NULL, NULL, FALSE);
    };
    break;
  case kind_array_ref:
    {
      array_ref ar = CAST(array_ref, e);

      assert(ar->alt);
      return analyze_expression(ar->alt, context, in);
    };
    break;
  case kind_field_ref:
    {
      field_ref fr = CAST(field_ref, e);
      einfo arg1;
      qtype field_qt;

      arg1 = analyze_expression(fr->arg1, context, in);
      if (qtype_error(arg1.qt))
	return arg1;

      /*** Compute field qtypes ***/
      
      {
	qtype qt, ag_qt;

	qt = arg1.qt;
	if (context == lpos || context == apos)
	  ag_qt = points_to_fs_qtype(qt, in);
	else
	  ag_qt = qt;

	if (qtype_error(ag_qt))
	  field_qt = error_qtype;
	else
	  {
	    assert(qtype_aggregate(ag_qt));
	    field_qt = field_qtype(ag_qt, fr->cstring);
	  }
      }

      if (flag_pam_mode || flag_print_results)
	pam_add_overlay_flow_sensitive(fr->cstring_loc, field_qt, arg1.out);

      return put_id_in_fs_context(fr->loc, field_qt, context, arg1.out);
    };
    break;
  case kind_init_list:
  case kind_init_index:
  case kind_init_field:
    fail("Unexpected expression kind %d\n", e->kind);
    break;
  default:
    if (is_unary(e))
      return analyze_unary_expression(CAST(unary, e), context, in);
    else if (is_binary(e))
      return analyze_binary_expression(CAST(binary, e), context, in);
    else
      fail("Unexpected expression kind %x\n", e->kind);
  }
}

static einfo analyze_unary_expression(unary e, context context, store in)
{
  switch (e->kind)
    {
    case kind_dereference:
      {
	/* Analyze the subexpression in an r-context; this will return
	   an l-type, so either return that if we're in an l-context or
	   deref it if we're in an r-context.  Function are special --
	   *f is allowed even if f is a function and not a function
	   pointer type. */
	einfo arg1;

	arg1 = analyze_expression(e->arg1, rpos, in);
	if (qtype_error(arg1.qt))
	  return arg1;
	assert(qtype_pointer_loc(arg1.qt));
	if (context == lpos || context == apos)
	  /* *e in an l-context silently converts a (pointer) r-type
	     to an l-type */
	    return mkeinfo(arg1.qt, arg1.out, NULL, NULL, FALSE);
	else if (context == rpos || context == cpos || context == apos)
	  {
	    qtype qt;

	    assert(!qtype_function(arg1.qt));
	    qt = points_to_fs_qtype(arg1.qt, arg1.out);
	    qt = default_conversion_fs_qtype(qt);

	    return mkeinfo(qt, arg1.out, NULL, NULL, FALSE);
	  }
	else
	  fail("Unexpected context %d\n", context);

	/* o|-< ignore op for now */
      }
    case kind_address_of:
      {
	/* &e can only appear in an r-context.  We analyze the
	   subexpression in an l-context (thus the subexpression type
	   with have a ptr at the outside) and silently convert the
	   l-type to an r-type.  On the other hand, taking the address
	   of a function type is a no-op. */
	if (type_function(e->arg1->type))
	  return analyze_expression(e->arg1, context, in);
	else
	  return analyze_expression(e->arg1, apos, in);
      }
    case kind_extension_expr:
      return analyze_expression(e->arg1, context, in);
    case kind_realpart:
    case kind_imagpart:
      {
	/* o|-< ??? */
	einfo arg1;
	qtype qt;

	arg1 = analyze_expression(e->arg1, rpos, in);
	assert(qtype_complex(arg1.qt));
	qt = complex_base_qtype(arg1.qt);

	return mkeinfo(qt, arg1.out, NULL, NULL, FALSE);
      }
    case kind_unary_minus:
    case kind_unary_plus:
    case kind_conjugate:
    case kind_bitnot:
      /* These operations don't change types.  Note we assume
	 that compatibility with these ops has already been
	 tested. */
      assert(context == rpos || context == cpos);
      return analyze_expression(e->arg1, context, in);
    case kind_not:
      {
	/* not doesn't change types, but it swaps true/false branches */
	einfo arg1;

	arg1 = analyze_expression(e->arg1, context, in);
	if (context == cpos)
	  return mkeinfo(arg1.qt, arg1.out, arg1.false_out, arg1.true_out, FALSE);
	else
	  return arg1;
      }
    case kind_preincrement:
    case kind_postincrement:
    case kind_predecrement:
    case kind_postdecrement:
      {
	einfo result;
	result = analyze_expression(e->arg1, lpos, in);
	result.qt = points_to_fs_qtype(result.qt, in);
	if (context == cpos)
	  {
	    result.true_out = result.out;
	    result.false_out = result.out;
	    result.out = NULL;
	  }
	return result;
      }
    default:
      fail("Unexpected unary expression kind %x\n", e->kind);
    }
}

static einfo analyze_binary_expression(binary e, context context, store in)
{
  assert(context == rpos || context == cpos);
  switch (e->kind)
    {
    case kind_plus:
      {
	einfo arg1, arg2;
	qtype qt;

	arg1 = analyze_expression(e->arg1, rpos, in);
	arg2 = analyze_expression(e->arg2, rpos, arg1.out);

	if (qtype_pointer_loc(arg1.qt) && qtype_pointer_loc(arg2.qt))
	  fail_loc(e->loc, "Unexpected pointer addition\n", 0);
	else if (qtype_pointer_loc(arg1.qt) && !qtype_pointer_loc(arg2.qt))
	  qt = arg1.qt;
	else if (!qtype_pointer_loc(arg1.qt) && qtype_pointer_loc(arg2.qt))
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
	return mkeinfo(qt, arg2.out, NULL, NULL, FALSE);
      }
      break;
    case kind_minus:
      {
	einfo arg1, arg2;
	qtype qt;

	arg1 = analyze_expression(e->arg1, rpos, in);
	arg2 = analyze_expression(e->arg2, rpos, arg1.out);

	if (qtype_pointer_loc(arg1.qt) && qtype_pointer_loc(arg2.qt))
	  qt = qtype_to_fs_qtype(e->loc, e->qtype);
	else if (qtype_pointer_loc(arg1.qt) && !qtype_pointer_loc(arg2.qt))
	  qt = arg1.qt;
	else if (!qtype_pointer_loc(arg1.qt) && qtype_pointer_loc(arg2.qt))
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
	return mkeinfo(qt, arg2.out, NULL, NULL, FALSE);
      }
      break;
    case kind_modulo:
    case kind_lshift:
    case kind_rshift:
      {
	einfo arg1, arg2;

	arg1 = analyze_expression(e->arg1, rpos, in);
	arg2 = analyze_expression(e->arg2, rpos, arg1.out);

	return mkeinfo(arg1.qt, arg2.out, NULL, NULL, FALSE);
      }
      break;
    case kind_times:
    case kind_divide:
    case kind_bitand:
    case kind_bitor:
    case kind_bitxor:
      {
	einfo arg1, arg2;

	arg1 = analyze_expression(e->arg1, rpos, in);
	arg2 = analyze_expression(e->arg2, rpos, arg1.out);

	/* Got rid of lub -- when would lub make sense? */
	if (mkeq_qtype(e->loc, arg1.qt, arg2.qt))
	  report_qerror(e->loc, sev_err,
			"incompatible operands of ``%s''",
			binary_op_name(e->kind));
	return mkeinfo(arg1.qt, arg2.out, NULL, NULL, FALSE);
      }
      break;
    case kind_andand:
      {
	einfo arg1, arg2;
	qtype qt;

	/* arg1 always evaluated */
	arg1 = analyze_expression(e->arg1, cpos, in);

	/* arg2 only evaluated if arg1 true, and it's evaluated in
	   our context */
	arg2 = analyze_expression(e->arg2, context, true_out_einfo(arg1));

	qt = mkqtype_bool(make_qvar("__boolop", e->loc, FALSE));

	if (context == cpos)
	  return mkeinfo(qt, NULL,
			 true_out_einfo(arg2),
			 lub_store(e->loc,
				   "inconsistent stores for false &&",
				   false_out_einfo(arg1),
				   false_out_einfo(arg2)),
			 FALSE);
	else
	  return mkeinfo(qt,
			 lub_store(e->loc,
				   "inconsistent stores for &&",
				   false_out_einfo(arg1),
				   arg2.out),
			 NULL, NULL, FALSE);
      }
      break;
    case kind_oror:
      {
	einfo arg1, arg2;
	qtype qt;

	/* arg1 always evaluated */
	arg1 = analyze_expression(e->arg1, cpos, in);

	/* arg2 only evaluated if arg1 false, and it's evaluated in
	   our context */
	arg2 = analyze_expression(e->arg2, context, false_out_einfo(arg1));

	qt = mkqtype_bool(make_qvar("__boolop", e->loc, FALSE));

	if (context == cpos)
	  return mkeinfo(qt, NULL,
			 lub_store(e->loc,
				   "inconsistent stores for true ||",
				   true_out_einfo(arg1),
				   true_out_einfo(arg2)),
			 false_out_einfo(arg2),
			 FALSE);
	else
	  return mkeinfo(qt,
			 lub_store(e->loc,
				   "inconsistent stores for ||",
				   true_out_einfo(arg1),
				   arg2.out),
			 NULL, NULL, FALSE);
      }
      break;
    case kind_eq:
    case kind_ne:
      {
	qtype qt;

	qt = mkqtype_bool(make_qvar("__boolop", e->loc, FALSE));
	if (context == cpos && have_stream_quals &&
	    ((file_pointer_qtype(e->arg1->qtype) && definite_null(e->arg2)) ||
	     (definite_null(e->arg1) && file_pointer_qtype(e->arg2->qtype))))
	  {
	    /* May be comparison of file pointer to NULL */
	    einfo fp, null;

	    /* XXX: Analyze null operand first; this changes the
	       left-to-right order of evaluation */
	    if (file_pointer_qtype(e->arg1->qtype))
	      {
		null = analyze_expression(e->arg2, rpos, in);
		fp = analyze_expression(e->arg1, cpos, null.out);
	      }
	    else
	      {
		null = analyze_expression(e->arg1, rpos, in);
		fp = analyze_expression(e->arg2, cpos, null.out);
	      }

	    if (e->kind == kind_eq)
	      /* NULL on TRUE branch */
	      return mkeinfo(qt, NULL, false_out_einfo(fp),
			     true_out_einfo(fp), FALSE);
	    else
	      /* NULL on FALSE branch */
	      return mkeinfo(qt, NULL, true_out_einfo(fp),
			     false_out_einfo(fp), FALSE);
	  }
	else
	  {
	    /* Ordinary comparsion */
	    einfo arg1, arg2;

	    arg1 = analyze_expression(e->arg1, rpos, in);
	    arg2 = analyze_expression(e->arg2, rpos, arg1.out);
	    return mkeinfo(qt, arg2.out, NULL, NULL, FALSE);
	  }
      }
      break;
    case kind_leq:
    case kind_geq:
    case kind_lt:
    case kind_gt:
      {
	einfo arg1, arg2;

	arg1 = analyze_expression(e->arg1, rpos, in);
	arg2 = analyze_expression(e->arg2, rpos, arg1.out);
	return mkeinfo(mkqtype_bool(make_qvar("__boolop", e->loc, FALSE)),
		       arg2.out, NULL, NULL, FALSE);
      }
      break;
    case kind_modulo_assign:
    case kind_lshift_assign:
    case kind_rshift_assign:
      {
	einfo arg1, arg2;
	qtype qt;

	arg1 = analyze_expression(e->arg1, lpos, in);
	arg2 = analyze_expression(e->arg2, rpos, arg1.out);
	if (!qtype_error(arg1.qt))
	  qt = points_to_fs_qtype(arg1.qt, arg2.out);
	else
	  qt = error_qtype;
	return mkeinfo(qt, arg2.out, NULL, NULL, FALSE);
      }
      break;
    case kind_plus_assign:
    case kind_minus_assign:
    case kind_times_assign:
    case kind_divide_assign:
    case kind_bitand_assign:
    case kind_bitor_assign:
    case kind_bitxor_assign:
    case kind_assign:
      {
	einfo arg1, arg2;
	qtype qt;
	store out;

	arg1 = analyze_expression(e->arg1, lpos, in);
	arg2 = analyze_expression(e->arg2, rpos, arg1.out);
	out = arg2.out;

	if (arg2.ismalloc)
	  {
	    out = allocate_qtype(e->loc, points_to_qtype(e->arg1->qtype), out);
	    if (flag_casts_warn)
	      /* No interaction.  This prevents no-qual zone warning for
		 x = malloc(...).  Also, this has the side effect of
		 qualifier on magic_malloc being untouched, but it
		 doesn't matter because magic_malloc is unique. */
	      {
		qt = points_to_fs_qtype(arg1.qt, out);
		return mkeinfo(qt, out, NULL, NULL, FALSE);
	      }
	  }

	if (!qtype_error(arg1.qt))
	  {
	    qt = points_to_fs_qtype(arg1.qt, out);
	    if ((e->kind == kind_plus_assign ||
		 e->kind == kind_minus_assign) &&
		qtype_pointer_loc(qt) && !qtype_pointer_loc(arg2.qt))
	      /* In p +/-= count, no interaction between p and count's
		 qualifiers */
	      return mkeinfo(qt, out, NULL, NULL, FALSE);

	    out = assign_flow_sensitive(e->loc,
					"incompatible types in assignment",
					arg2.qt, arg1.qt, out, FALSE);

	    if (flag_print_lin)
	      queue_print_exp((expression) e, out, aloc_qtype(arg1.qt));
	  }
	else
	  qt = error_qtype;

	if (have_stream_quals && context == cpos &&
	    file_pointer_fs_qtype(qt, out))
	  return split_file_pointer_context(e->loc, qt, out);
	else
	  return mkeinfo(qt, out, NULL, NULL, FALSE);
      }
      break;
    default:
      fail_loc(e->loc, "Unexpected binary op kind 0x%x\n", e->kind);
    }
}

/**************************************************************************
 *                                                                        *
 * Utilities for traversal                                                *
 *                                                                        *
 **************************************************************************/

einfo put_id_in_fs_context(location loc, qtype qt, context c, store s)
{
  if (qtype_error(qt))
    /* Do nothing */;
  else if (qtype_array(qt)) 
    {
      if (c == rpos || c == cpos)
	qt = default_conversion_fs_qtype(qt);
      else if (c == apos)
	qt = mkqtype_pointer_loc(make_fresh_qvar("q", loc),
				 array_aloc_qtype(qt));
      else
	fail("Unexpected context %d\n", c);
    }
  else if (qtype_function(qt)) 
    {
      assert(c == rpos || c == cpos);
      /* Aloc was associated with qt in qtype_to_fs_qtype */
      qt = mkqtype_pointer_loc(make_fresh_qvar("q", loc),
			       aloc_qtype(qt));
    }
  else if (c == lpos || c == apos)
    /* Do nothing */;
  else if (c == rpos || c == cpos)
    {
      if (qtype_pointer_loc(qt))
	qt = points_to_fs_qtype(qt, s);
      else
	fail("Unexpected qtype\n");
    }
  else
    fail("Unexpected context %d\n", c);

  if (have_stream_quals && c == cpos && file_pointer_fs_qtype(qt, s))
    return split_file_pointer_context(loc, qt, s);
  else
    return mkeinfo(qt, s, NULL, NULL, FALSE);
}

/* Handles allocation of this qtype. */
static store allocate_qtype(location loc, qtype qt, store in)
{
  /* Do this in ref_qtype_store
  if (qtype_pointer(qt) && qtype_aggregate(points_to_qtype(qt)))
    {
      field_scanner fs;
      qtype qt_field;
      field_scan(points_to_qtype(qt), &fs);
      while ((qt_field = field_next(&fs)))
	global_store_ext = allocate_qtype(loc, qt_field, global_store_ext);
    }
  */

  return ref_qtype_store(loc, qt, in);
}

/* finds the smallest enclosing scope */
static compound_stmt get_enclosing_scope(compound_stmt cs1, compound_stmt cs2)
{
  compound_stmt temp;

  if (cs1 == cs2) return cs1; /* get the easy case out of the door */
  
  temp = cs1;
  while (temp != NULL)
    {
      temp->visited = TRUE;
      temp = temp->enclosing_cs;
    }

  temp = cs2;
  while (temp != NULL)
    {
      if (temp->visited == TRUE)
	/* found */
	{
	  /* clean up */
	  while (cs1 != NULL) 
	    { cs1->visited = FALSE; cs1 = cs1->enclosing_cs; }
	  while (cs2->visited) 
	    { cs2->visited = FALSE; cs2 = cs2->enclosing_cs; }
	  
	  return temp;
	}
      temp = temp->enclosing_cs;
    }

  /* toplvl match...this is ok since there should be no inter-procedural
     jumps */
  /* clean up */
  while (cs1 != NULL) 
    { cs1->visited = FALSE; cs1 = cs1->enclosing_cs; }
  while (cs2->visited) 
    { cs2->visited = FALSE; cs2 = cs2->enclosing_cs; }

  return NULL;
}

/* Allocates locations for the variables in d */
static store ref_decls(store s, declaration d)
{
  if (!d)
    return s;

  s = ref_decls(s, CAST(declaration, d->next));
  switch (d->kind)
    {
    case kind_asm_decl:
    case kind_function_decl:
    case kind_ellipsis_decl:
    case kind_oldidentifier_decl:
      return s;
    case kind_extension_decl:
      {
	extension_decl ed = CAST(extension_decl, d);

	assert(ed->decl->next == NULL);
	return ref_decls(s, ed->decl);
      }
    case kind_data_decl:
      {
	data_decl dd = CAST(data_decl, d);
	declaration decl;

	scan_declaration(decl, dd->decls)
	  {
	    variable_decl vd = CAST(variable_decl, decl);
	    data_declaration root;
	    
	    assert(!vd->asm_stmt);
	    
	    root = root_ddecl(vd->ddecl);

	    if (root->qtype &&
		root->definition &&
		root->kind == decl_variable &&
		!root->isexternalscope &&
		root->vtype != variable_static)
	      /* If we defined a local variable, allocate it, ignoring
		 root->isallocated flag */
	      s = allocate_qtype(location_declaration(root->ast),
				 root->qtype,
				 s);
	  }
	return s;
      }
    default:
      fail_loc(d->loc, "Unexpected decl kind %x\n", d->kind);
    }
}

/* Cross-block transfer routine.
   Allocates locations in blocks jumping into, and 
   matches any restricts.
*/
static store tfr_jumped_blocks(store s, compound_stmt from, compound_stmt to)
{
  compound_stmt enclosing_cs;

  enclosing_cs = get_enclosing_scope(from, to);

  while (enclosing_cs != to)
    {
      assert(to);
      s = ref_decls(s, to->decls);
      to = to->enclosing_cs;
    }

  while (enclosing_cs != from)
    {
      assert(from);
      s = merge_restricts(from->decls, s);
      from = from->enclosing_cs;
    }
  
  return s;
}

/* If ddecl is a definition, add memory for ddecl to either s or to the
   global store */
static store allocate_ddecl(data_declaration ddecl, store s)
{
  location loc;

  ddecl = root_ddecl(ddecl);
  loc = location_declaration(ddecl->ast);
  if (ddecl->qtype && ddecl->definition && !ddecl->isallocated)
    {
      if (ddecl->kind == decl_variable)
	{
	  if (ddecl->isexternalscope || ddecl->vtype == variable_static)
	      global_store_ext =
		allocate_qtype(loc,
			       ddecl->qtype,
			       global_store_ext);
	  else
	    /* local variable */
	    s = allocate_qtype(loc, ddecl->qtype, s);
	}
      else if (ddecl->kind == decl_function)
	/* function */
	global_store_ext = allocate_qtype(loc, ddecl->qtype,
					  global_store_ext);
      ddecl->isallocated = TRUE;
    }
  return s;
}

/**************************************************************************
 *                                                                        *
 * Routines to handle restricted locations                                *
 *                                                                        *
 **************************************************************************/

/* Called when a restricted location may go out of scope.  in is the
   current store.  Returns a new store, in which the resticted
   locations from d are strong/weakly updated */
static store merge_var_decl_restrict(variable_decl vd, store in)
{
  if (vd && vd->rinfo)
    {
      qtype rqtype, fqtype;

      /* Update one level down, since vd's qtype is an lvalue pointer,
	 and it's the pointer which is restricted */
      fqtype = points_to_fs_qtype(vd->rinfo->fs_fqtype, in);
      rqtype = points_to_fs_qtype(vd->rinfo->fs_rqtype, in);
      rqtype = points_to_fs_qtype(rqtype, in);

      in = assign_flow_sensitive(vd->loc,
				 "incompatible types at restrict scope end",
				 rqtype, fqtype, in, FALSE);
      return in;
    }
  else
    return in;
}

/* Called when a restricted location may go out of scope.  in is the
   current store.  Returns a new store, in which the resticted
   locations from d are strong/weakly updated */
static store merge_restricts(declaration d, store in)
{
  if (!d)
    return in;

  in = merge_restricts(CAST(declaration, d->next), in);
  switch (d->kind)
    {
    case kind_asm_decl:
    case kind_function_decl:
    case kind_ellipsis_decl:
    case kind_oldidentifier_decl:
      return in;
    case kind_extension_decl:
      {
	extension_decl ed = CAST(extension_decl, d);

	assert(ed->decl->next == NULL);
	return merge_restricts(ed->decl, in);
      }
    case kind_data_decl:
      {
	declaration vd;

	scan_declaration(vd, CAST(data_decl, d)->decls)
	  in = merge_var_decl_restrict(CAST(variable_decl, vd), in);
	return in;
      }

    default:
      fail_loc(d->loc, "Unexpected decl kind %x\n", d->kind);
    }
}

/**************************************************************************
 *                                                                        *
 * Magic -- special interpretations of certain syntax                     *
 *                                                                        *
 **************************************************************************/

static qual string_mode_to_qual(const char *mode)
{
  if (!strcmp(mode, "r") ||
      !strcmp(mode, "rb"))
    return read_unchecked_qual;
  else if (!strcmp(mode, "w") ||
	   !strcmp(mode, "wb") ||
	   !strcmp(mode, "a") ||
	   !strcmp(mode, "ab"))
    return write_unchecked_qual;
  else if (!strcmp(mode, "r+") ||
	   !strcmp(mode, "rb+") ||
	   !strcmp(mode, "r+b") ||
	   !strcmp(mode, "w+") ||
	   !strcmp(mode, "wb+") ||
	   !strcmp(mode, "w+b") ||
	   !strcmp(mode, "a+") ||
	   !strcmp(mode, "ab+") ||
	   !strcmp(mode, "a+b"))
    return readwrite_unchecked_qual;

  return NULL;
}

static qual int_mode_to_qual(known_cst cst)
{
  unsigned int mode;

  mode = constant_uint_value(cst);
  switch (mode & O_ACCMODE)
    {
    case O_RDONLY: return read_unchecked_qual;
    case O_WRONLY: return write_unchecked_qual;
    case O_RDWR: return readwrite_unchecked_qual;
    default: return NULL;
    }
}

/* If fc is a magic function, do a special analysis.  Otherwise return
   an einfo with NULL as out */
static einfo analyze_magic_function_call(function_call fc, qtype fqt, store in)
{
  if (fc->arg1->kind == kind_identifier)
    {
      identifier id = CAST(identifier, fc->arg1);
      if ((!strcmp(id->cstring.data, "fopen") ||
	   !strcmp(id->cstring.data, "fdopen") ||
	   !strcmp(id->cstring.data, "tmpfile") ||
	   !strcmp(id->cstring.data, "safefopen") ||
	   !strcmp(id->cstring.data, "popen") ||
	   !strcmp(id->cstring.data, "my_popen"))
	  && have_stream_quals)
	{
	  store cur_store;
	  qual mode_qual;
	  expression e;
	  qtype ret_qtype;
	  int num_args;

	  cur_store = in;
	  mode_qual = NULL;

	  /* Process the function arguments for any side effects */
	  num_args = 0;
	  scan_expression(e, fc->args)
	    {
	      einfo arg_info = analyze_expression(e, rpos, cur_store);
	      cur_store = arg_info.out;
	      num_args++;
	    }

	  /* Extract the mode */
	  if (!strcmp(id->cstring.data, "fopen") ||
	      !strcmp(id->cstring.data, "fdopen") ||
	      !strcmp(id->cstring.data, "popen") ||
	      !strcmp(id->cstring.data, "my_popen"))
	    {
	      if (num_args != 2)
		report_qerror(fc->loc, sev_err,
			      "wrong number of arguments to fopen");

	      if (num_args >= 2 && fc->args->next->kind == kind_string)
		mode_qual =
		  string_mode_to_qual(string_to_charp(parse_region,
						CAST(string, fc->args->next)));
	      if (!mode_qual)
		{
		  mode_qual = open_unchecked_qual;
		  report_qerror(fc->args->next->loc, sev_info,
				"unable to determine mode for fopen");
		}
	      else if (!strcmp(id->cstring.data, "popen") &&
		       mode_qual == readwrite_unchecked_qual)
		report_qerror(fc->args->next->loc, sev_err,
			      "pipes can only be opened for reading or "
			      "writing, not both");

	    }
	  else if (!strcmp(id->cstring.data, "safefopen"))
	    {
	      if (num_args != 4)
		report_qerror(fc->loc, sev_err,
			      "wrong number of arguments to safefopen");

	      if (num_args >= 2 && CAST(expression, fc->args->next)->cst)
		mode_qual =
		  int_mode_to_qual(CAST(expression, fc->args->next)->cst);
	      if (!mode_qual)
		{
		  mode_qual = open_unchecked_qual;
		  report_qerror(fc->args->next->loc, sev_info,
				"unable to determine mode for fopen");
		}
	    }
	  else
	    fail("Unexpected id %s\n", id->cstring.data);

	  /* Construct the result type */

	  /* Get from fc to mimic polymorphism from first pass */
	  ret_qtype = qtype_to_fs_qtype(fc->loc, fc->qtype); 
	  if (mode_qual != readwrite_qual)
	    {
	      /* Don't add constraint for readwrite, since it's
		 useless */
	      if (qtype_pointer_loc(ret_qtype))
		{
		  aloc al;
		  qtype ret_ptqtype;

		  al = aloc_qtype(ret_qtype);
		  ret_ptqtype = copy_qtype(points_to_fs_qtype(ret_qtype,
							      cur_store),
					   fc->loc);
		  insistnot(mkleq_qual(fc->loc, mode_qual,
				       qual_qtype(ret_ptqtype)));

		  /* fopen allocates the FILE */
		  cur_store =
		    make_store_ref(fc->loc, cur_store, al);
		  cur_store =
		    make_store_assign(fc->loc,
				      "strong update failure in fopen result",
				      cur_store, al, ret_ptqtype, TRUE);
		}
	      else
		report_qerror(fc->loc, sev_err,
			      "return type of fopen isn't a pointer type");
	    }

	  return mkeinfo(ret_qtype, cur_store, NULL, NULL, FALSE);
	}
      else if (!strcmp(id->cstring.data, "malloc") ||
	       !strcmp(id->cstring.data, "xmalloc") ||
	       !strcmp(id->cstring.data, "vmalloc") ||
	       !strcmp(id->cstring.data, "kmalloc") ||
	       !strcmp(id->cstring.data, "ioremap") ||
	       !strcmp(id->cstring.data, "kmem_cache_alloc"))
	{
	  qtype ret_qtype;
	  store cur_store;
	  expression e;
	  int num_args = 0;

	  cur_store = in;
	  scan_expression(e, fc->args)
	    {
	      einfo arg_info = analyze_expression(e, rpos, cur_store);
	      cur_store = arg_info.out;
	      num_args++;
	    }

	  assert(qtype_pointer(fc->qtype));
	  cur_store = allocate_qtype(fc->loc, fc->qtype, cur_store);

	  ret_qtype = qtype_to_fs_qtype(fc->loc, fc->qtype);
	  if (!strcmp(id->cstring.data, "kmalloc") && have_sleepy_quals)
	    {
	      if (num_args != 2)
		report_qerror(fc->loc, sev_err,
			      "wrong number of arguments to kmalloc");
	      else if (fc->args->next->kind == kind_lexical_cst)
		{
		  lexical_cst cst = (lexical_cst)(fc->args->next);
		  if(strcmp(cst->cstring.data, "0x20"))
		    /* Not GFP_ATOMIC */
		    /* Assert interrupts are enabled */
		    {
		      aloc al;
		      qtype qt;
		      assert(interrupt_status_qtype != NULL);
		      al = aloc_qtype(interrupt_status_qtype);
		      qt = qtype_from_store(cur_store, al);
		      if (mkleq_qual(fc->loc, qual_qtype(qt), enabled_qual))
			report_qerror(fc->loc, sev_err,
				      "kmalloc (non-ATOMIC) called with "
				      "interrupts enabled");
		    }
		}
	    }

	  return mkeinfo(ret_qtype, cur_store, NULL, NULL, TRUE);
	}
      else
	return mkeinfo(NULL, NULL, NULL, NULL, FALSE);
    }
  else
    return mkeinfo(NULL, NULL, NULL, NULL, FALSE);
}

bool file_pointer_fs_qtype(qtype qt, store s)
{
  if (qtype_pointer_loc(qt))
    {
      qtype points_to;

      points_to = points_to_fs_qtype(qt, s);
      return (qtype_struct(points_to) &&
	      tag_name_qtype(points_to) &&
	      !strcmp(tag_name_qtype(points_to), "_IO_FILE"));
    }
  return FALSE;
}

/* Give a file pointer qtype, return an einfo in whose true store qt is
   non-NULL and in whose false store qt in NULL */
static einfo split_file_pointer_context(location loc, qtype qt, store s)
{
  aloc al;
  qtype old_qt, new_non_null_qt, new_null_qt;
  store non_null_store, null_store;

  assert(have_stream_quals && file_pointer_fs_qtype(qt, s));
  al = aloc_qtype(qt);
  old_qt = qtype_from_store(s, al);
  new_non_null_qt = copy_qtype(old_qt, loc);
  new_null_qt = copy_qtype(old_qt, loc);

  insistnot(cond_mkleq_qual(loc,
			    read_unchecked_qual, qual_qtype(old_qt),
			    read_qual, qual_qtype(new_non_null_qt)));
  insistnot(cond_mkleq_qual(loc,
			    write_unchecked_qual, qual_qtype(old_qt),
			    write_qual, qual_qtype(new_non_null_qt)));
  insistnot(mkleq_qual(loc, closed_qual,
		       qual_qtype(new_null_qt)));

  non_null_store = 
    make_store_assign(loc,
		      "strong update failure in fopen result test",
		      s, al, new_non_null_qt, TRUE);

  null_store =
    make_store_assign(loc,
		      "strong update failure in fopen result test",
		      s, al, new_null_qt, TRUE);

  return mkeinfo(qt, NULL, non_null_store, null_store, FALSE);
}

/**************************************************************************
 *                                                                        *
 * Printing                                                               *
 *                                                                        *
 **************************************************************************/

typedef struct PrintQueue_elt {
  enum { kind_exp, kind_stmt } kind;
  union {
    statement stmt;
    expression expr;
  } u;
  store store;
  aloc al;
} * printqueue_elt;

static void print_queue(void)
{
  dd_list_pos cur;
  
  dd_scan(cur, printqueue)
    {
      printqueue_elt elt = DD_GET(printqueue_elt, cur);
      if (elt->kind == kind_exp)
	{
	  printf("\n Expression: ");
	  prt_expression(elt->u.expr, 0);
	}
      else if (elt->kind == kind_stmt)
	{
	  printf("\n Stmt: ");
	  prt_statement(elt->u.stmt);
	}
      else
	fail("Unexpected print queue elt kind %d\n", elt->kind);

      printf("\n");
      print_aloc(printf, elt->al);
      printf("^");
      switch (lin_from_store(elt->store, elt->al))
	{
	case 0: printf("0"); break;
	case 1: printf("1"); break;
	case 2: printf("w"); break;
	default: printf("?");
	}
      printf("\n---------------------------------------------------\n");
    }
}

static void queue_print_stmt(statement stmt, store s, aloc al)
{
  printqueue_elt elt;
  
  elt = ralloc(parse_region, struct PrintQueue_elt);
  elt->kind = kind_stmt;
  elt->u.stmt = stmt;
  elt->store = s;
  elt->al = al;
  dd_add_last(parse_region, printqueue, elt);
}

static void queue_print_exp(expression expr, store s, aloc al)
{
  printqueue_elt elt;
  
  elt = ralloc(parse_region, struct PrintQueue_elt);
  elt->kind = kind_exp;
  elt->u.expr = expr;
  elt->store = s;
  elt->al = al;
  dd_add_last(parse_region, printqueue, elt);
}

