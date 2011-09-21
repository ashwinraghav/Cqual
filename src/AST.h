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

#ifndef AST_H
#define AST_H

#include "linkage.h"

EXTERN_C_BEGIN

typedef int id_declaration_list;
typedef struct typelist *typelist;
typedef struct type *type;

EXTERN_C_END

#include "AST_types.h"
#include "c-lex.h"
#include "cqual.h"
#include "decls.h"
#include "types.h"
#include "dd_list.h"

EXTERN_C enum { struct_type, union_type, enum_type };

#include "AST_defs.h"

EXTERN_C_BEGIN

typedef struct AST_ast_generic
{
  ast_kind kind;
} *ast_generic;

#ifdef __GNUC__
#define CAST(type, x) ({ast_generic tEmPcast = (ast_generic)(x); if (tEmPcast) assert(is_ ## type(tEmPcast)); (type)(tEmPcast); })
#define CASTPTR(type, x) ({ast_generic *tEmPcast = (ast_generic *)(x); if (tEmPcast && *tEmPcast) assert(is_ ## type(*tEmPcast)); (type *)(tEmPcast); })
#define CASTSRPTR(type, x) ({ast_generic *tEmPcast = (ast_generic *)(x); if (tEmPcast && *tEmPcast) assert(is_ ## type(*tEmPcast)); (type sameregion *)(tEmPcast); })
#else
/* Could also generate some code to make this safe */
#define CAST(type, x) ((type)(x))
#define CASTPTR(type, x) ((type *)(x))
#define CASTSRPTR(type, x) ((type sameregion *)(x))
#endif

extern declaration the_program;

unary newkind_unary(region r, ast_kind kind, location location, expression arg1);
binary newkind_binary(region r, ast_kind kind, location location,
		      expression arg1, expression arg2);
tag_ref newkind_tag_ref(region r, ast_kind kind, location location, word word1, attribute attributes, declaration fields, bool defined);
node last_node(node n);
int chain_length(node n);
node ast_chain(node l1, node l2);
void insert_before(node sameregion *list, node before, node n);
node ast_reverse(node l);

void AST_set_parents(node n);

void AST_print(node n);

#define AST_SET(parent, ptr, value) \
  (*(ptr) = (value), (value) ? (set_parent(CASTSRPTR(node, (ptr)), CAST(node, (parent))), 0) : 0)

#define AST_SET_FIELD(parent, field, value) \
  AST_SET((parent), &(parent)->field, (value))

#define AST_SET_NEXT(previous, value) \
  AST_SET((previous)->parent, &(previous)->next, (value))

#define AST_REPLACE(n, value) \
  AST_SET((n)->parent_ptr, (n)->parent, (value))

EXTERN_C_END

#endif
