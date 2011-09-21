/* This file is part of cqual.
   Copyright (C) 2002 The Regents of the University of California.

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

/* Routines to find syntactic equivalences of two expressions modulo
 easily recognizable semantic equivalenes */

#include "parser.h"
#include "AST_utils.h"
#include "expr.h"
#include "c-parse.h"
#include "constants.h"
#include "analyze.h"
#include "cqual.h"
#include "utils.h"

bool equal_unary_expressions(unary e1, unary e2);
bool equal_binary_expressions(binary e1, binary e2);

bool equal_expressions(expression e1, expression e2)
{
  if (e1->kind != e2->kind)
    return FALSE;

  switch (e1->kind) {
  case kind_cast:
    {
      /* Ignore cast syntax */
      cast c1 = CAST(cast, e1);
      cast c2 = CAST(cast, e2);
      return equal_expressions(c1->arg1, c2->arg1);
    }
    break;
  case kind_conditional:
    {
      conditional c1 = CAST(conditional, e1);
      conditional c2 = CAST(conditional, e2);
      
      if ((c1->arg1 && !c2->arg1) || (!c1->arg1 && c2->arg1))
	return FALSE;
      if (!equal_expressions(c1->condition, c2->condition))
	return FALSE;
      if (c1->arg1)
	{
	  assert(c2->arg1);
	  if (!equal_expressions(c1->arg1, c2->arg1))
	    return FALSE;
	}
      return equal_expressions(c1->arg2, c2->arg2);
    }
    break;
  case kind_identifier:
    {
      identifier id1 = CAST(identifier, e1);
      identifier id2 = CAST(identifier, e2);
      return root_ddecl(id1->ddecl) == root_ddecl(id2->ddecl);
    }
    break;
  case kind_function_call:
    {
      function_call fc1 = CAST(function_call, e1);
      function_call fc2 = CAST(function_call, e2);
      expression arg1, arg2;

      if (!equal_expressions(fc1->arg1, fc2->arg1)) 
	return FALSE;

      arg2 = fc2->args;
      scan_expression(arg1, fc1->args)
	{
	  if (!arg2 || !equal_expressions(arg1, arg2))
	    return FALSE;
	  
	  arg2 = CAST(expression, arg2->next);
	}
      return arg2 ? FALSE : TRUE;
    }
  case kind_field_ref:
    {
      field_ref fr1 = CAST(field_ref, e1);
      field_ref fr2 = CAST(field_ref, e2);

      return equal_expressions(fr1->arg1, fr2->arg1) &&
	!strcmp(fr1->cstring.data, fr2->cstring.data);
    }
  case kind_array_ref:
    {
      array_ref ar1 = CAST(array_ref, e1);
      array_ref ar2 = CAST(array_ref, e2);
      
      return equal_expressions(ar1->arg1, ar2->arg1) &&
	equal_expressions(ar1->arg2, ar2->arg2);
    }
  case kind_lexical_cst:
    {
      lexical_cst lc1 = CAST(lexical_cst, e1);
      lexical_cst lc2 = CAST(lexical_cst, e2);

      return !strcmp(lc1->cstring.data, lc2->cstring.data);
    }
  default:
    if (is_unary(e1) && is_unary(e2))
      return equal_unary_expressions(CAST(unary, e1), CAST(unary, e2));
    else if (is_binary(e1) && is_binary(e2))
      return equal_binary_expressions(CAST(binary, e1), CAST(binary, e2));
    else
      return FALSE;
  }
}

bool equal_unary_expressions(unary e1, unary e2)
{
  if (e1->kind != e2->kind)
    return FALSE;  

  return equal_expressions(e1->arg1, e2->arg1);
}

bool equal_binary_expressions(binary e1, binary e2)
{
  if (e1->kind != e2->kind)
    return FALSE;

  return equal_expressions(e1->arg1, e2->arg1) &&
    equal_expressions(e1->arg2, e2->arg2);      
}
