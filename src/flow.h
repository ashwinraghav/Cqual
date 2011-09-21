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

#ifndef FLOW_SENSITIVE_H
#define FLOW_SENSITIVE_H

#include "parser.h"
#include "c-lex.h"
#include "cqual.h"
#include "effect.h"
#include "linkage.h"

EXTERN_C_BEGIN

/**************************************************************************
 *                                                                        *
 * Analysis Interface                                                     *
 *                                                                        *
 **************************************************************************/

void analyze_flow_sensitive(declaration program);
void init_flow_sensitive(void);
void finish_flow_sensitive(void);

/* Return TRUE iff ddecl may be visible to the outside world */
bool is_externally_visible_fn(data_declaration ddecl);

extern store global_store;

EXTERN_C_END

#endif
