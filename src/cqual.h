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

#ifndef CQUAL_H
#define CQUAL_H

#include "bitset.h"
#include "bool.h"
#include "buffer.h"
#include "regions.h"
#include "linkage.h"

EXTERN_C_BEGIN

/* Qualifiers and qualified types */
typedef struct Ty *ty;
typedef struct Qtype *qtype;
typedef struct type_qualifier *qual;
typedef struct Qual_edge *qual_edge;
typedef struct Condition *cond;
typedef struct Bounds *bounds;

/* Stores and effects */
typedef struct Abstract_loc *aloc;
typedef struct Effect *effect;
typedef struct Store *store;
typedef struct Store_edge *store_edge;
typedef struct Cell *cell;

/* Aloctypes */
typedef struct Aloctype *aloctype;
typedef struct Alocreftype *alocreftype;
typedef struct Alocfntype *alocfntype;
typedef struct Effecttype *effecttype;
typedef struct Rinf_const *rinf_const;

/* PAM */
typedef struct po_info *po_info;
typedef struct File *pam_file;
typedef struct Click *pam_click;
typedef struct Overlay *pam_overlay;

/* Other */
typedef struct Rinfo *rinfo; /* Info for restrict */
typedef struct Drinfo *drinfo; /* Info for deep restrict */
typedef int (*printf_func)(const char *fmt, ...);

/* Possible effect constructors */
typedef enum {
  eff_any = 0,
  eff_rwr,
  eff_r,
  eff_wr,
  eff_alloc,
  eff_last = eff_alloc } eff_kind;

/* Load in a lattice configuration file */
void load_config_file_quals(const char *);

/* If you use fail_loc, you must give a 3rd argument!  Pass 0 if you
   don't need anything there. */
#define fail_loc(l, s, args...) fail("(at %s:%ld) " s, ((l)->filename), ((l)->lineno), args)

EXTERN_C_END

#endif
