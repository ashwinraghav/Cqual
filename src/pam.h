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

#ifndef PAM_H
#define PAM_H

#include <stdarg.h>
#include "linkage.h"
#include "AST.h"
#include "c-lex.h"
#include "qerror.h"

EXTERN_C_BEGIN

/* Switch to PAM mode */
void enter_pam_mode(void);

/* Add a file to the analysis */
void pam_add_file(const char *name);

/* Report an error */
void pam_add_error(const char *func, location loc, severity s,
		   const char *fmt, va_list args);

/* Add an overlay to a file.  Adds the file if it doesn't already
   exist. */
void pam_add_overlay_file(location loc, const char *idname, qtype qt);

/* Add an anchor, i.e., something we might want to jump to */
void pam_add_overlay_anchor(location loc);

/* Add a linearity overlay to a file */
void pam_add_overlay_lin(location loc, store store, aloc al);

/* Tell PAM that qt is the flow-sensitive qtype at location loc, and
   the store at that location is s. */
void pam_add_overlay_flow_sensitive(location loc, qtype qt, store s);

/* Dump out the analysis results */
void print_results(void);

void init_pam(void);

EXTERN_C_END

#endif
