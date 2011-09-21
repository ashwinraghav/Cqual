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

#ifndef CSTRING_H
#define CSTRING_H

#include "linkage.h"
#include "regions.h"

EXTERN_C_BEGIN

/* A C string is like a regular C string, but with a length. A null byte is
   added AFTER the string for convenience */
typedef struct {
  char *data;
  int length;
} cstring;

/* Make a new cstring with a copy of s, length l */
cstring make_cstring(region r, const char *s, int l);

/* Make a new cstring with a copy of regular C string s */
cstring str2cstring(region r, const char *s);

EXTERN_C_END

#endif
