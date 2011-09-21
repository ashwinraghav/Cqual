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
#include "AST_utils.h"
#include "constants.h"

bool oldstyle_function(function_decl fn)
{
  return !fn->fdeclarator->parms || is_oldidentifier_decl(fn->fdeclarator->parms);
}

function_declarator get_fdeclarator(declarator d)
{
  for (;;)
    switch (d->kind)
      {
      case kind_function_declarator:
	return CAST(function_declarator, d);
      case kind_identifier_declarator:
	return NULL;
      case kind_array_declarator:
	d = CAST(array_declarator, d)->declarator;
	break;
      case kind_pointer_declarator:
	d = CAST(pointer_declarator, d)->declarator;
	break;
      default:
	assert(0);
      }
}

data_declaration get_parameter(declaration d)
{
  switch (d->kind)
    {
    case kind_ellipsis_decl:
      return NULL;
    case kind_oldidentifier_decl:
      return CAST(oldidentifier_decl, d)->ddecl;
    case kind_data_decl:
      return CAST(variable_decl, CAST(data_decl, d)->decls)->ddecl;
    default:
      assert(0);
      return NULL;
    }
}

data_declaration base_identifier(data_declaration d)
{
  while (d->isfilescoperef && d->shadowed)
    d = d->shadowed;

  return d;
}

bool same_function(data_declaration d1, data_declaration d2)
{
  d1 = base_identifier(d1);
  d2 = base_identifier(d2);
  return d1->kind == decl_function && d1 == d2;
}

bool call_to(data_declaration fnd, function_call fce)
{
  return is_identifier(fce->arg1) &&
    fnd && same_function(CAST(identifier, fce->arg1)->ddecl, fnd);
}

bool is_localvar(expression e)
{
  data_declaration decl;

  if (!is_identifier(e))
    return FALSE;

  decl = CAST(identifier, e)->ddecl;
  return decl->kind == decl_variable && decl->islocal;
}

bool is_assignment(void *e)
{
  int kind = ((node)e)->kind;

  /* XXX: Yuck */
  return kind == kind_assign || kind == kind_plus_assign ||
    kind == kind_minus_assign || kind == kind_times_assign ||
    kind == kind_divide_assign || kind == kind_modulo_assign ||
    kind == kind_lshift_assign || kind == kind_rshift_assign ||
    kind == kind_bitand_assign || kind == kind_bitor_assign ||
    kind == kind_bitxor_assign;
}

bool is_increment(void *e)
{
  int kind = ((node)e)->kind;

  /* XXX: Yuck */
  return kind == kind_preincrement || kind == kind_predecrement ||
    kind == kind_postincrement || kind == kind_postdecrement;
}

expression ignore_fields(expression e)
{
  while (e->kind == kind_field_ref)
    e = CAST(field_ref, e)->arg1;

  return e;
}

expression expression_of_stmt(compound_expr ce)
{
  compound_stmt blk = CAST(compound_stmt, ce->stmt);
  statement last_stmt = CAST(statement, last_node(CAST(node, blk->stmts)));

  if (last_stmt && is_expression_stmt(last_stmt))
    {
      expression_stmt es = CAST(expression_stmt, last_stmt);

      return es->arg1;
    }
  return NULL;
}

bool zero_expression(expression e)
{
  /* a = 0 is not a constant expression but it's value is known to be 0,
     and other similar cases */
  for (;;)
    {
      if (is_assign(e))
	{
	  e = CAST(assign, e)->arg2;
	  continue;
	}
      if (is_cast(e))
	{
	  e = CAST(cast, e)->arg1;
	  continue;
	}
      if (is_comma(e))
	{
	  e = CAST(expression, last_node(CAST(node, CAST(comma, e)->arg1)));
	  continue;
	}
      break;
    }

  return definite_zero(e);
}

expression build_int_constant(region r, location loc, type t, largest_int c)
{
  char cstbuf[64];
  cstring csts;
  lexical_cst cst;

  snprintf(cstbuf, sizeof cstbuf, "%lld", c);
  csts.data = rstrdup(r, cstbuf);
  csts.length = strlen(cstbuf);
  cst = new_lexical_cst(r, loc, csts);
  cst->type = t;
  cst->cst = make_cst(make_cval_signed(c, t), t);

  return CAST(expression, cst);
}

expression build_uint_constant(region r, location loc, type t, largest_uint c)
{
  char cstbuf[64];
  cstring csts;
  lexical_cst cst;

  snprintf(cstbuf, sizeof cstbuf, "%llu", c);
  csts.data = rstrdup(r, cstbuf);
  csts.length = strlen(cstbuf);
  cst = new_lexical_cst(r, loc, csts);
  cst->type = t;
  cst->cst = make_cst(make_cval_unsigned(c, t), t);

  return CAST(expression, cst);
}

cval value_of_enumerator(enumerator e)
{
  return e->ddecl->value->cval;
}

expression build_identifier(region r, location loc, data_declaration id)
{
  identifier e = new_identifier(r, loc, str2cstring(r, id->name), id);

  assert(id->kind == decl_variable || id->kind == decl_function);
  e->type = id->type;
  e->cst = fold_identifier(CAST(expression, e), id);

  return CAST(expression, e);
}

/* Convert an AST string into a char * */
char *string_to_charp(region r, string s)
{
  expression e;
  int len = 0;
  char *str;

  scan_expression(e, s->strings)
    if (e->kind == kind_string_cst)
      len += CAST(string_cst, e)->cstring.length - 2; /* for begin/end "s */
    else if (e->kind == kind_identifier)
      len += CAST(identifier, e)->cstring.length - 2;
    else
      fail("Unexpected string component kind %x\n", e->kind);
  str = rarrayalloc(r, len + 1, char);
  str[0] = '\0';
  scan_expression(e, s->strings)
    if (e->kind == kind_string_cst)
      strncat(str,
	      CAST(string_cst, e)->cstring.data + 1,
	      CAST(string_cst, e)->cstring.length - 2);
    else if (e->kind == kind_identifier)
      strncat(str,
	      CAST(identifier, e)->cstring.data + 1,
	      CAST(identifier, e)->cstring.length - 2);
    else
      fail("Unexpected string component kind %x\n", e->kind);
  return str;
}
