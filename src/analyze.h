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

#ifndef ANALYZE_H
#define ANALYZE_H

#include "linkage.h"
#include "parser.h"
#include "c-lex.h"
#include "cqual.h"
#include "buffer.h"
#include "effect.h"
#include "qerror.h"

EXTERN_C_BEGIN

/**************************************************************************
 *                                                                        *
 * Analysis Interface                                                     *
 *                                                                        *
 **************************************************************************/

struct Rinfo {
  bool isglobal;     /* TRUE if this is a gloal */
  effect flocs;      /* locations that were restricted */
  effect flocs_alloc;/* flocs as alloc effect */ 
  qtype fqtype;      /* qtype containing rlocs */
  qtype fs_fqtype;   /* flow-sensitive qtype containing rlocs */
  effect rlocs;      /* new names for the restricted locations */
  effect rlocs_alloc;/* rlocs as alloc effect */
  qtype rqtype;      /* qtype containing flocs */
  qtype fs_rqtype;   /* flow-sensitive qtype containing flocs */
  effect arg1_eff;   /* effect of the initialization (if any) */
  qual lin;          /* linearity of fqtype at decl */
};

struct Drinfo {
  effect effect;
  expression expression;
  qtype qtype;
  qtype rqtype;
  qtype fs_qtype;
  qtype fs_rqtype;
  location location;
};

void analyze(declaration program);
qtype analyze_field_declaration(const char *name, field_declaration fd);
void analyze_tag_ref(tag_ref);
void init_analyze(void);
void finish_analyze(void);

extern function_decl current_function_decl;

/**************************************************************************
 *                                                                        *
 * Utilities                                                              *
 *                                                                        *
 **************************************************************************/

/* Report an error at loc */
void report_qerror(location loc, severity sev, const char *format, ...);

location location_declaration(declaration);
data_declaration root_ddecl(data_declaration ddecl);
bool static_ddecl(data_declaration ddecl);
identifier_declarator get_id_declarator(declarator d);

/* Return TRUE iff ddecl is used but undefined */
bool is_undefined_global(data_declaration ddecl, store s);

/* Return TRUE iff ddecl is declares a varargs function that
   was not given a type in a prelude file */
bool is_unannotated_global(data_declaration ddecl, store s);

/* Display an error if ddecl is used but not defined, or if it's
   a varargs function not given a type in a prelude file */
void warn_if_dangerous_global(data_declaration ddecl, store s);

typedef void (*traverse_global_fn)(data_declaration, void *);

/* Apply f to one ddecl for each global */
void traverse_globals(traverse_global_fn f, void *arg);

/* Compute the top-level effect */
effect get_global_effect(void);

/* Return the root data declaration for main */
data_declaration get_main_ddecl(void);

/**************************************************************************
 *                                                                        *
 * File open/close                                                        *
 *                                                                        *
 **************************************************************************/

/* For magic stuff */
extern bool have_stream_quals;
extern qual open_unchecked_qual;
extern qual read_unchecked_qual;
extern qual write_unchecked_qual;
extern qual readwrite_unchecked_qual;
extern qual open_qual;
extern qual read_qual;
extern qual write_qual;
extern qual readwrite_qual;
extern qual closed_qual;
extern bool have_sleepy_quals;
extern qual enabled_qual;
extern qual disabled_qual;
extern qtype interrupt_status_qtype;

bool file_pointer_qtype(qtype qt);

EXTERN_C_END

#endif
