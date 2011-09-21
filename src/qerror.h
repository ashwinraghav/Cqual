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

#ifndef QERROR_H
#define QERROR_H

#include <location.h>
#include "buffer.h"

typedef enum {sev_err, sev_warn, sev_info} severity;

void init_qerror(void);

/* Set to TRUE if any qualifier errors happened (unsatisfiable
   constraints) */
extern bool qerrors;

/* Detailed information about the last invalid constraint */
extern growbuf qual_error_detail; 
extern growbuf qtype_error_detail;

/* printf into X_error_detail */
int qual_ed_printf(const char *fmt, ...);
int qtype_ed_printf(const char *fmt, ...);

#endif
