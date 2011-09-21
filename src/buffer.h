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

#ifndef BUFFER_H
#define BUFFER_H

/*
 * Growable buffers
 *
 * Always null terminated.
 */

#include <stdarg.h>
#include "bool.h"
#include "regions.h"
#include "linkage.h"

EXTERN_C_BEGIN

typedef struct growbuf *growbuf;

growbuf growbuf_new(region, int);   /* Make a new buffer with initial size */
void growbuf_reset(growbuf);        /* Empty a buffer */
int gprintf(growbuf, const char *, ...);      /* Print to a buffer */
int gvprintf(growbuf, const char *, va_list); /* Print to a buffer */
char *growbuf_contents(growbuf);    /* Get the contents of a buffer */
bool growbuf_empty(growbuf);        /* Return true iff buffer is empty */

EXTERN_C_END

#endif
