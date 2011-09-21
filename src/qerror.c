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

#include <assert.h>
#include <stdio.h>
#include "qerror.h"

static region qerror_region = NULL;

/* Set to TRUE if any qualifier errors happened (unsatisfiable
   constraints) */
bool qerrors = FALSE;

/* Detailed information about the last invalid constraint */
growbuf qual_error_detail = NULL; 
growbuf qtype_error_detail = NULL; 

void init_qerror(void)
{
  assert(!qerror_region);
  qerror_region = newregion();
  qual_error_detail = growbuf_new(qerror_region, 256);
  qtype_error_detail = growbuf_new(qerror_region, 256);
  assert(!qerrors);
}

/* printf into qual_error_detail */
int qual_ed_printf(const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  return gvprintf(qual_error_detail, fmt, args);
}

/* printf into qtype_error_detail */
int qtype_ed_printf(const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  return gvprintf(qtype_error_detail, fmt, args);
}
