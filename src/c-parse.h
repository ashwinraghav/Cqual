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

#ifndef C_PARSE_H
#define C_PARSE_H

#include "linkage.h"

EXTERN_C_BEGIN

struct yystype {
  union {
    void *ptr;
    asm_operand asm_operand;
    asm_stmt asm_stmt;
    attribute attribute;
    lexical_cst constant;
    string_cst string_cst;
    declaration decl;
    declarator declarator;
    expression expr;
    id_label id_label;
    label label;
    node node;
    statement stmt;
    conditional_stmt cstmt;
    for_stmt for_stmt;
    string string;
    type_element telement;
    asttype type;
    word word;

    struct {
      location location;
      int i;
    } itoken;

    struct {
      location location;
      expression expr;
      int i;
    } iexpr;

    struct {
      statement stmt;
      int i;
    } istmt;

  } u;

  struct {
    location location;
    cstring id;
    data_declaration decl;
  } idtoken;
};

#define YYSTYPE struct yystype

int yyparse(void) deletes;

/* Region in which to allocate parse structures. Idea: the AST user can set
   this to different regions at appropriate junctures depending on what's
   being done with the AST */
extern region parse_region;

/* >0 if currently parsing an expression that will not be evaluated (argument
   to alignof, sizeof. Currently not typeof though that could be considered
   a bug) */
extern int unevaluated_expression;

EXTERN_C_END

#endif
