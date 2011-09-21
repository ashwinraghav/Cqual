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

#ifndef DECLS_H
#define DECLS_H

#include "env.h"
#include "linkage.h"

EXTERN_C_BEGIN

/* Types representing declarations */

typedef struct field_declaration {
  struct field_declaration *next; /* Next field in struct/union */
  const char *name;		/* May be NULL for bitfields (if NULL, bitwidth == 0) */
  type type;
  field_decl ast;
  int bitwidth;			/* for bitfields, -1 otherwise */
  size_t offset;		/* in bits, not bytes */
  bool offset_cc:1;		/* TRUE if offset is a compile-time
				   constant (can be false because of
				   variable-size arrays) */
  bool packed:1;		/* if packed attribute specified */
} *field_declaration;

/* A struct, union or enum */
typedef struct tag_declaration {
  int kind; /* One of kind_{struct/union/enum}_ref */
  const char *name; /* NULL for anonynous struct/union/enum */
  type reptype; /* The type used to represent an enum, NULL for struct
		   and unions */
  /* fields and fieldlist are only defined for structs/unions */
  env fields;
  field_declaration fieldlist;
  tag_ref ast;
  struct tag_declaration *shadowed; /* Any struct with the same tag
                                       defined in enclosing scope */
  bool defined:1;
  bool being_defined:1;
  bool fields_const:1;
  bool fields_volatile:1;
  bool transparent_union:1;	/* transparent_union attribute is present */
  bool size_cc:1;		/* TRUE if size is a compile-time
				   constant (can be false because of
				   variable-size arrays) */
  bool packed:1;		/* if packed attribute specified */

  size_t size, alignment;

  /* Type qualifier specific fields */
  qtype qtype;                  /* qtype */
} *tag_declaration;

typedef enum { decl_variable, decl_constant, decl_function,
	       decl_typedef, decl_error, decl_magic_string } data_kind;

typedef struct data_declaration {
  data_kind kind;
  const char *name;
  type type;

  struct data_declaration *shadowed; /* For extern's shadowing globals
                                        in inner scopes */
  struct data_declaration *global;   /* For externs the first
					data_declaration for this name */
  declaration definition;  /* Pointer to actual definition, if any */
  declaration ast;         /* Last declaration */
  expression initialiser;  /* NULL if none */

  bool islimbo:1;          /* TRUE if comes from an extern declaration
		              in an inner scope (also true for implicit
		              function declarations) */


/* (TREE_PUBLIC)
   variables: (means: name is to be accessible from outside this module)
   in global scope: non-static, non-register
   in local scope: DECL_EXTERNAL
   functions: non-static, non-nested fns */
  bool isexternalscope:1;

  /* (DECL_EXTERNAL)
     variables: !initialised, `extern' specified
     functions: undefined non-nested fns (so true for static functions!)
     also true for extern inline functions (which aren't "really" defined) */
  bool isfilescoperef:1;

  /* (TREE_STATIC)
     variables (means: alllocate static storage)
     in global scope: !DECL_EXTERNAL
     in local scope: static
     functions (means: has been defined) */
  bool needsmemory:1;

  /* isused is TRUE if declaration used. For parameters, there is a special
     use during parameter list declaration to support forward parameters:
       - a duplicate parameter declaration is allowed if isused is FALSE
         once a duplicate is seen, isused is set to TRUE
       - parameters are created with isused == TRUE
       - after the forward parameters are seen, they have their isused field
         set to FALSE */
  bool isused:1;
  bool in_system_header:1;

  bool in_prelude:1;            /* Declared in a prelude file -- override
				   later declarations */
  bool defined:1;               /* TRUE if defined somewhere, in any
				   file */
  bool isallocated:1;           /* TRUE if 2p allocated it */
  bool addresstaken:1;          /* TRUE if this function's address is
			           taken in a non-function call
			           context */
  bool __init:1;                /* Declared __init or __initdata */

  /* For functions */
  enum { function_implicit, function_normal, function_static, function_nested }
    ftype:2;
  bool isinline:1;
  bool isexterninline:1;
  bool noreturn:1;        /* Noreturn attribute specified */
  typelist oldstyle_args; /* Type of arguments from old-style declaration */
  char * alias;           /* Alias attribute */

  /* For variables */
  enum { variable_register, variable_static, variable_normal } vtype:2;
  bool islocal:1;     /* belongs to local scope; could be
			 auto, static, or register */
  bool isparameter:1; /* implies islocal */

  /* For constants */
  known_cst value;

  /* For magic_strings */
  const wchar_t *chars;
  size_t chars_length;

  qtype qtype;                  /* Qualified type */
  qtype fs_qtype;               /* Flow-sensitive qualified type */

  alocreftype alref;            /* Pointsto */
} *data_declaration;

typedef struct label_declaration {
  const char *name;
  bool explicitly_declared:1;
  bool used:1;
  id_label firstuse; /* Never NULL */
  id_label definition; /* NULL until actually defined */
  function_decl containing_function;
} *label_declaration;

typedef struct environment
{
  struct environment *sameregion parent;
  function_decl fdecl;
  bool parm_level;
  env sameregion id_env;
  env sameregion tag_env;
} *environment;

extern data_declaration bad_decl;

EXTERN_C_END

#endif
